      SUBROUTINE OFSTS_MULT_REF ( CLKPOL_FLG, CLKPOL_DEG, NOFST, MODE, ISTAD )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OFSTS PROGRAM SPECIFICATION
!
! 1.1  Determine offset epochs
!
! 1.2 REFERENCES:
!
! 2.  OFSTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2     CLKPOL_DEG, ISTAD, NOFST
      CHARACTER     CLKPOL_FLG*(*), MODE*(*) 
!
! ISTAD - Station data flag
! MODE  - A = auto; F = force
! NOFST - Number of offsets
!
! 2.3 OUTPUT Variables:
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: autcl
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  I, J, STA, JCLOCK, NUM_SELSTA, BM_REF_CL_ORIG
      INTEGER*2  TICLSTA(ARC_STA_BIT_WORDS,MAX_CLK), &
     &           TLCLK(MAX_CLK), INDX(MAX_CLK)
      REAL*8     EARLY, DATE1, TFJDCL(MAX_CLK), SORTRA(MAX_CLK)
      LOGICAL*4  CHECK_STABIT
      LOGICAL*2  KBIT, WAS_BREAK, FL_ELIG(MAX_ARC_STA)
      CHARACTER  FILCLO*128, STR*80, C_CLS(MAX_STA)*8
      INTEGER*2  L_CLS, K, J1, J2, J3, ISTA
      INTEGER*4  IOS, UNIT_CLS
      INTEGER*4, EXTERNAL ::  I_LEN, GET_UNIT
!
! 4.  HISTORY
!   WHO  WHEN        WHAT
!                   
!   pet  10-DEC-97   Change a bit logic for choosing reference clock station
!                   
!   pet  16-DEC-97   Corrected spelling errors in error messages
!                   
!   pet  12-JAN-98   Corrected bug: in the case when no clock reference station
!                    was chosen or the clock reference station appeared to
!                    be deselected, SOLVE will select the new clock reference
!                    station as the first station in the station list which
!                    is 1) not deselected; 2) don't contain clock breaks.
!                   
!   pet  22-JAN-98   Corrected bug: the routine firstly tries to find the
!                    previous clock reference station and only then sets
!                    clock reference stations anew.
!                   
!   pet  05-JUL-99   Corrected a bug: the previous version erroneously
!                    set all stations as clock reference and therefore did
!                    not estimate clock for any station if the original clock
!                    reference station was deselected. Changed logic:
!                    if the clock reference station(s) was changed then
!                    baseline depended clocks are not estimated for any
!                    baselines with clock reference stations to avoid
!                    singularity of normal matrix.
!                   
!   pet  2001.09.19  Changed the logic of selection of clock reference station
!                    when Solve must do it since the clock reference station
!                    was deselected. Solve  first reads file
!                    $SAVE_DIR/CLOCK_PICK (which keeps the stations list)
!                    and starts to check each station of this list,
!                    from the first to the last, a) did this station
!                    participated in the session, b) was this station
!                    delesected; c) did this station has clock breaks. If
!                    (a) is TRUE (b), (c) are FALSE, then this station become
!                    the clock reference station. If not, then Solve checks
!                    the next station from the CLOCK_PICK list. If this file
!                    was not found or no station from the list satisfied
!                    above, Solve makes the last attempt to set clock
!                    reference station: it selects the first station among
!                    participated stations which do not have clock breaks
!                    and was not deselected.
!                   
!   pet  2001.09.20  Added support the case when all stations have clock breaks.
!                    In this case the first selected station is treated
!                    as clock refrence station
!                   
!   pet  2004.03.16  Removed ba2cm.i and made CLKPOL_FLG, CLKPOL_DEG formal 
!                    arguments
!                   
!   pet  2004.06.26  Fixed a bug. The previous version did not work in
!                    the simulation ode in MODE = 'N' -- by another words when
!                    only global clock polynomials are to be estimated.
!                    
!   pet  2004.10.06  Fixed a bug. The previous version did not set correctly
!                    flags for global polynomial flags when no segements were
!                    requested
!                    
!   pet  2004.11.26  Fixed a bug. The previous version did not set correctly
!                    global polynomial flags.
!                    
!                    
!                   
! 5.  OFSTS PROGRAM STRUCTURE
!
!CCCC
!
! --- Initialize temp arrays
!
      DO I=1,MAX_CLK
         TLCLK(I)  = 0
         TFJDCL(I) = 0.0
         DO J=1,ARC_STA_BIT_WORDS
            TICLSTA(J,I) = 0
         ENDDO
         SORTRA(I) = 0.0
         INDX(I)   = 0
      ENDDO
!
! --- Find earliest offset first epoc, set up as first element in ICLSTA, ect.
!
      EARLY =FJDCL(ICLSTR(1)+1)
      STA = 1
      DO I=2,NUMSTA
          DATE1 = FJDCL(ICLSTR(I)+1)
          IF ( DATE1 .LT. EARLY ) THEN
               EARLY = DATE1
               STA = I
          ENDIF
      ENDDO
!
! --- Setup maximum degree for global clock polinomials
!
      IF ( CLKPOL_FLG .EQ. 'D' ) THEN
!
! -------- Default -- 2
!
           CALL SBIT ( TLCLK(1), INT2(1), INT2(1) )
           CALL SBIT ( TLCLK(1), INT2(2), INT2(1) )
           CALL SBIT ( TLCLK(1), INT2(3), INT2(1) )
         ELSE IF ( CLKPOL_FLG .EQ. 'A' .OR. CLKPOL_FLG .EQ. 'F' .OR. &
     &             CLKPOL_FLG .EQ. 'P' ) THEN
!
! -------- Auto -- Use exactly the value specified in BATCH file
!
           IF ( CLKPOL_DEG .GE. 0 ) CALL SBIT ( TLCLK(1), INT2(1), INT2(1) )
           IF ( CLKPOL_DEG .GE. 1 ) CALL SBIT ( TLCLK(1), INT2(2), INT2(1) )
           IF ( CLKPOL_DEG .GE. 2 ) CALL SBIT ( TLCLK(1), INT2(3), INT2(1) )
         ELSE IF ( CLKPOL_FLG .EQ. 'M' ) THEN
!
! -------- MOST -- Use value to be max of specified in BATCH file and superfile
!
           IF ( CLKPOL_DEG .GE. 0 .OR. KBIT( LCLK(1), INT2(1) ) ) THEN
                CALL SBIT (TLCLK(1), INT2(1), INT2(1) )
           END IF
!
           IF ( CLKPOL_DEG .GE. 1 .OR. KBIT( LCLK(1), INT2(2) ) ) THEN
                CALL SBIT ( TLCLK(1), INT2(2), INT2(1) )
           END IF
!         
           IF ( CLKPOL_DEG .GE. 2 .OR. KBIT( LCLK(1), INT2(3) ) ) THEN
                CALL SBIT ( TLCLK(1), INT2(3), INT2(1) )
           END IF
      END IF
!
      TFJDCL(1) = FJDCL(ICLSTR(STA)+1)
!
! --- If the stations which was a clock reference station in superfile
! --- appeared to be deselected then we set temporary status
! --- "no clock reference station" is chosen and we will select a new
! --- reference station among stations which are in solution
!
      BM_REF_CL_ORIG = BM_REF_CL
      IF ( BM_REF_CL .GE. 1 .AND.  BM_REF_CL .LE. NUMSTA ) THEN
           IF ( .NOT. CHECK_STABIT ( BM_REF_CL ) ) BM_REF_CL = 0
      END IF
      IF ( BM_REF_CL .EQ. 0 ) THEN
           L_CLS = 0
!
! -------- Open the file with clock stations
!
           FILCLO = PRE_SAV_DIR(1:PRE_SV_LEN)//'CLOCK_PICK'
           UNIT_CLS = GET_UNIT()
           OPEN ( UNIT=UNIT_CLS, FILE=FILCLO, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                IF ( G_WARNING ) THEN
                     WRITE (  6, '(A,I5,A)' ) 'Warning: error ',IOS, &
     &                     ' in attempt to open file '//FILCLO(1:I_LEN(FILCLO))
                     WRITE ( 23, '(A,I5,A)' ) 'Warning: error ',IOS, &
     &                     ' in attempt to open file '//FILCLO(1:I_LEN(FILCLO))
                END IF
             ELSE
!
! ------------- Read the file with clock stations
!
                DO 410 J1=1,1024
                   READ ( UNIT=UNIT_CLS, FMT='(A)', IOSTAT=IOS ) STR
                   IF ( IOS .NE. 0 ) GOTO 810
                   IF ( STR(1:1) .NE. '*'  .AND.  &
     &                  STR(1:1) .NE. '#'  .AND.  &
     &                  STR(1:1) .NE. '$'         ) THEN
!
! --------------------- Increment the list of potential clock reference stations
!
                        L_CLS = L_CLS + 1
                        C_CLS(L_CLS) = STR(1:8)
                   END IF
 410            CONTINUE
 810            CONTINUE
                CLOSE ( UNIT=UNIT_CLS )
           END IF
      END IF
!
! --- Loop through looking for BM_REF_CL saved in superfile, offset epocs
!
      NOFST=1
      NUM_SELSTA = 0 ! Counter of selected stations
      DO I=1,NUMSTA
         FL_ELIG(I) = .TRUE.  ! Set eligibility flag
         IF ( CHECK_STABIT(I) ) THEN
!
! ----------- This station is not deselected from solution
!
              NUM_SELSTA = NUM_SELSTA + 1 ! counter of selected stations
              WAS_BREAK  = .FALSE.
              DO J=2,NUMCLK(I)
                 JCLOCK=ICLSTR(I)+J
!
! -------------- Check: was real clock break detected for the I-th station?
!
                 IF ( .NOT. KBIT( LCLK(JCLOCK), INT2(13))    .AND. &
     &                KBIT(ICLSTA(1,JCLOCK),I)       ) THEN
                      WAS_BREAK = .TRUE.
                      FL_ELIG(I) = .FALSE. ! lift eligibility flag
                 END IF
                 IF (       KBIT( LCLK(JCLOCK), INT2(1) ) .AND. &
     &                      KBIT(ICLSTA(1,JCLOCK),I)      .AND. &
     &                .NOT. KBIT( LCLK(JCLOCK), INT2(13))       ) THEN
!
                      IF ( MODE(1:1) .EQ. 'A'  .OR.  MODE(1:1) .EQ. 'N' ) THEN
                           NOFST=NOFST+1
                           CALL SBIT ( TLCLK(NOFST), INT2(1), INT2(1) )
                           CALL SBIT ( TLCLK(NOFST), INT2(2), INT2(1) )
                           CALL SBIT ( TLCLK(NOFST), INT2(3), INT2(1) )
                           TFJDCL(NOFST) = FJDCL(JCLOCK)
                           CALL SBIT ( TICLSTA(1,NOFST), I, INT2(1) )
                      ENDIF
                 ENDIF
              ENDDO ! j
              IF ( MODE .EQ. 'N' ) NUMCLK(I) = NOFST
!
              IF ( BM_REF_CL .EQ. 0   .AND.   .NOT. WAS_BREAK ) THEN
!
! ---------------- Attempt to set a clock reference station by crude force
!
                   JCLOCK    = ICLSTR(I) + 1
                   IF ( .NOT. KBIT( LCLK(JCLOCK), INT2(1) )     .OR..NOT. &
     &                  KBIT(ICLSTA(1,JCLOCK),I)      ) THEN
!
! --------------------- Setting "clock reference station" flag for this stations
! --------------------- since it
! --------------------- 1) was chosen before as a clock reference station;
! --------------------- 2) selected in solution;
! --------------------- 3) doesn't have clock breaks
!
                        BM_REF_CL = I
                        CALL SBIT ( CLOCK_REF_BITS, I, INT2(1) )
!
                        CALL SBIT ( LCLK(JCLOCK), INT2(1), INT2(0) )
                        CALL SBIT ( ICLSTA(1,JCLOCK), I, INT2(0) )
                  ENDIF
              ENDIF
           ELSE
!
! ----------- Station was deselected
!
              FL_ELIG(I) = .FALSE. ! lift eligibility flag
         ENDIF
      ENDDO ! i
!
      IF ( BM_REF_CL .EQ. 0  .AND. L_CLS .GT. 0 ) THEN
!
! -------- Scan the list of prefereabe clock station from top to bottom.
! -------- We stop this process as soon as we chose the first eligible station
!
           DO 420 J2=1,L_CLS
              DO 430 J3=1,NUMSTA
                 JCLOCK = ICLSTR(J3) + 1
                 IF ( C_CLS(J2) .EQ. ISITN_CHR(J3)  .AND.  FL_ELIG(J3) ) THEN
!
! ------------------- Well! This station is at the top of the station clock
! ------------------- pick up list and it is eligible for becoming clock
! ------------------- reference station.
!
                      BM_REF_CL = J3
                      CALL SBIT ( CLOCK_REF_BITS, J3, INT2(1) )
                      CALL SBIT ( LCLK(JCLOCK), INT2(1), INT2(0) )
                      CALL SBIT ( ICLSTA(1,JCLOCK), J3, INT2(0) )
                      GOTO 820
                 END IF
 430          CONTINUE
 420       CONTINUE
 820       CONTINUE
      END IF
!
      IF ( BM_REF_CL .EQ. 0 ) THEN
!
! -------- Gentle way for setting clock reference station failed. Well.
!
           DO I=1,NUMSTA
              IF ( BM_REF_CL .EQ. 0 ) THEN
                   JCLOCK    = ICLSTR(I) + 1
!
! ---------------- Then we mercilessly set the first a) selected;
! ---------------- b) clock breaks free station as a clock reference station
!
                   IF ( FL_ELIG(I) ) THEN
!
! --------------------- If no reference station was chosen and the clock for
! --------------------- this station doesn't have break then we forcible set
! --------------------- flag "clock reference station" for this station
!
                        BM_REF_CL = I
                        CALL SBIT ( CLOCK_REF_BITS, I, INT2(1) )
                        CALL SBIT ( LCLK(JCLOCK), INT2(1), INT2(0) )
                        CALL SBIT ( ICLSTA(1,JCLOCK), I, INT2(0) )
                  ENDIF
              END IF
           END DO
      END IF
!
      IF ( BM_REF_CL .EQ. 0 ) THEN
!
! -------- We still did not set clock refernce clocks??? U-u-u-u-u-u!
! -------- It means that ALL stations has clock breaks. It is is a disaster.
! -------- But we have to set clock refernce stations anyhow. Well, let's take
! -------- the first selected stations. Results will be poor, but we don't
! -------- have other choice
!
           DO I=1,NUMSTA
              IF ( BM_REF_CL .EQ. 0 ) THEN
                   JCLOCK    = ICLSTR(I) + 1
!
! ---------------- Then we mercilessly set the first station which is selected
!
                   IF ( CHECK_STABIT(I) ) THEN
!
! --------------------- If no reference station was chosen and the clock for
! --------------------- this station doesn't have break then we forcible set
! --------------------- flag "clock reference station" for this station
!
                        BM_REF_CL = I
                        CALL SBIT ( CLOCK_REF_BITS, I, INT2(1) )
                        CALL SBIT ( LCLK(JCLOCK), INT2(1), INT2(0) )
                        CALL SBIT ( ICLSTA(1,JCLOCK), I, INT2(0) )
                  ENDIF
              END IF
           END DO
!
           IF ( G_WARNING ) THEN
                WRITE (  6, '(A)' ) 'Warning: the station with clock break '// &
     &                            'was selected as the clock reference '// &
     &                             DBNAME_CH
                WRITE ( 23, '(A)' ) 'Warning: the station with clock break '// &
     &                            'was selected as the clock reference in '// &
     &                             DBNAME_CH
           END IF
      END IF
!
      IF ( BM_REF_CL .NE. BM_REF_CL_ORIG ) THEN
!
! -------- If the clock reference stations was changed then we disable
! -------- estimation of baseline clock at any baselines with a clock
! -------- reference station(s) to avoid singularity of normal matrix.
!
           DO I=1,NUMSTA
              DO J=1,NUMSTA
                 IF ( KBIT(CLOCK_REF_BITS,I) .OR. KBIT(CLOCK_REF_BITS,J) ) THEN
                      CALL SBIT( ICLOCK(1,I), J, INT2(0) )
                      CALL SBIT( ICLOCK(1,J), I, INT2(0) )
                 END IF
              ENDDO
           ENDDO
      END IF
!
      IF ( NUM_SELSTA .EQ. 0 ) THEN
           CALL FERR ( INT2(141), &
     &         'BATCH(ofsts_mult_ref): No one station was '// &
     &         'selected in solution for the session '//DBNAME_CH, INT2(0), &
     &          INT2(0) )
      END IF
!
      IF ( NUM_SELSTA .EQ. 1 ) THEN
           CALL FERR ( INT2(142), 'BATCH(ofsts_mult_ref): Only one station '// &
     &         'appeared to be selected in solution for the session '// &
     &          DBNAME_CH, INT2(0), INT2(0) )
      END IF
!
! --- Test for a reference clock station
!
      IF ( CLOCK_REF_BITS(1) .EQ. 0  .AND. &
     &     CLOCK_REF_BITS(2) .EQ. 0        ) THEN
!
           CALL FERR ( INT2(143), &
     &         'BATCH(ofsts_mult_ref): No clock reference '// &
     &         'station(s) for the session '//DBNAME_CH, INT2(0), INT2(0) )
           STOP 'BATCH (ofsts_mult_ref) Abnoramal ermination'
      END IF
!
      DO I=1,NUMSTA
         CALL SBIT ( TICLSTA(1,1), I, INT2(1) )
      ENDDO
      DO I = 1, NUMSTA
         IF ( KBIT(CLOCK_REF_BITS,I) ) CALL SBIT ( TICLSTA(1,1), I, INT2(0) )
      ENDDO
!
! --- Set up and sort the FJDCL array in time order:  use INDX array
! --- to move around elements of LCLK, FJDCL arrays
!
      IF ( SIMULATION_TEST .AND. MODE .EQ. 'N' ) THEN
!
! -------- Special case of simulation. There is not information about clocks
! -------- in the super file. The logic above does not work. We wet aside
! -------- all this bullshit and sets the flags. For some reasons which 
! -------- are bejond me, it is not needed in MODE = 'A'
!
           DO ISTA=1,NUMSTA
              IF (       CHECK_STABIT ( ISTA )     .AND. &
     &             .NOT. KBIT(CLOCK_REF_BITS,ISTA)       ) THEN
!
                   FJDCL(ISTA) = TFJDCL ( 1 )
                   LCLK(ISTA)  = TLCLK(ISTA)
                   IF ( CLKPOL_DEG .GE. 0 ) CALL SBIT ( LCLK(ISTA), INT2(1), INT2(1) )
                   IF ( CLKPOL_DEG .GE. 1 ) CALL SBIT ( LCLK(ISTA), INT2(2), INT2(1) )
                   IF ( CLKPOL_DEG .GE. 2 ) CALL SBIT ( LCLK(ISTA), INT2(3), INT2(1) )
                   DO J=1,ARC_STA_BIT_WORDS
                      ICLSTA(J,ISTA) = TICLSTA(J,1)
                   ENDDO
              END IF
           ENDDO
         ELSE 
!
! -------- Not a simulation
!
           IF ( MODE(1:1) .EQ. 'A'  .OR.  MODE .EQ. 'N' ) THEN
                DO I=1,NOFST
                   SORTRA(I) = TFJDCL(I)
                   INDX(I)   = I
                ENDDO
                CALL HSORT ( SORTRA, INDX, NOFST )
!
! ------------- Move temp arrays to common
!
                DO I=1,NOFST
                   FJDCL(I) = TFJDCL ( INDX(I) )
                   LCLK(I)  = TLCLK  ( INDX(I) )
                   DO J=1,ARC_STA_BIT_WORDS
                      ICLSTA(J,I) = TICLSTA ( J, INDX(I) )
                   ENDDO
                ENDDO
              ELSE
                DO ISTA=1,NUMSTA
                   FJDCL(ISTA)=TFJDCL(1)
                   LCLK(ISTA) =TLCLK(1)
                   DO J=1,ARC_STA_BIT_WORDS
                      ICLSTA(J,ISTA) = TICLSTA(J,1)
                   END DO
                ENDDO
           ENDIF
!
!@           IF ( MODE .EQ. 'N' ) THEN
!@                DO ISTA=1,NUMSTA
!@                   IF (       CHECK_STABIT ( ISTA )     .AND. &
!@     &                   .NOT. KBIT(CLOCK_REF_BITS,ISTA)       ) THEN
!@                        LCLK(ISTA) =TLCLK(1)
!@                   END IF
!@                END DO
!@           ENDIF
      ENDIF
!
      RETURN
      END  !#!  OFSTS_MULT_REF  #!#
