      SUBROUTINE SET_BASCLK ( U_TRI, UIS_TRI, U_BAS, UIS_BAS, U_STA, UIS_STA, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SET_BASCLK  resets up flags of estimation of baselines    *
! *   dependent clocks. It finds such a combination of baselines         *
! *   dependent clocks which doesn't make normal matrix singular.        *
! *                                                                      *
! *   Old setup of baseline-dependent clocks is lost.                    *
! *                                                                      *
! *   Algorithm is straitforward in the case when all possible baselines *
! *   were in use: all baselines except baseline with clock reference    *
! *   baseline are set up.                                               *
! *                                                                      *
! *   In the case when not all baselines were are in solution the list   *
! *   of linearly independent closed triangles is used. The number of    *
! *   baseline dependent clocks is equal to the number of linearly       *
! *   independent closed triangles. Setup of baseline dependent clocks   *
! *   which provides the maximal number of baselines and doesn't make    *
! *   normal matrix singular is that when each baseline from the set     *
! *   enters the list of closed triangles once and only once.            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     U_TRI ( INTEGER*4 ) -- Number of closed triangles.               *
! *                            length of LIS_TRI list.                   *
! *   UIS_TRI ( INTEGER*4 ) -- List of closed triangles dimension of     *
! *                            U_TRI*3. Each row of the list contains    *
! *                            the index in the list of baselines for    *
! *                            those three baselines, which close        *
! *                            triangle. Indices of the baselines are in *
! *                            increasing of baselines codes.            *
! *     U_STA ( INTEGER*4 ) -- The number of stations used in solution.  *
! *   UIS_STA ( INTEGER*4 ) -- List of the stations used in solutions    *
! *                            containing codes of the stations          *
! *                            ( sorted in increasing of the codes ).    *
! *                            These codes are indices in the list of    *
! *                            station names ISITN_CHR from prfil.i      *
! *     U_BAS ( INTEGER*4 ) -- The number of baselines used in solution. *
! *   UIS_BAS ( INTEGER*4 ) -- List of the stations containing codes     *
! *                            of the baselines( sorted in increasing    *
! *                            of the codes ). Codes of the baselines    *
! *                            are produced by routine NSTBA.            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  pet 2001.12.13  Added setting flags to ICLOCK_P (for phase delay    *
! *                  solution) in parallel to ICLOCK (for group delay    *
! *                  solution).                                          *
! *  pet 2007.10.17  Restored a line which accidentally fell on          *
! *                  2007.08.07.                                         *
! *                                                                      *
! *  ###  20-JUL-98  SET_BASCLK   v1.1 (c)  L. Petrov  13-DEC-2001  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'solve.i'
      INCLUDE    'socom.i'
      INCLUDE    'obser.i'
!
      INTEGER*4  U_TRI, UIS_TRI(3,*), U_BAS, UIS_BAS(*), U_STA, UIS_STA(*), &
     &           IUER
      LOGICAL*4  SEL_BAS(MO_BAS), SEL_TRI(MO_TRI)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, K_TRI, &
     &           N_STA, NST1, NST2, IMB
      CHARACTER  STR*80
      INTEGER*4  INT4
      INTEGER*2  INT2_ARG
      LOGICAL*2, EXTERNAL :: KBIT
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      IF ( CGM_TYPE ) THEN
           CALL ERR_LOG ( 8451, IUER, 'SET_BASCLK', 'CGM-type socom block '// &
     &         'is in use' )
           RETURN
      END IF
      IF ( U_BAS .LE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL INCH    ( U_BAS, STR )
           CALL ERR_LOG ( 8452, IUER, 'SET_BASCLK', 'Parameter U_BAS has '// &
     &                   'wrong value: '//STR )
           RETURN
      END IF
!
      IF ( U_STA .LE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL INCH    ( U_STA, STR )
           CALL ERR_LOG ( 8453, IUER, 'SET_BASCLK', 'Parameter U_STA has '// &
     &                   'wrong value: '//STR )
           RETURN
      END IF
      N_STA = NUMSTA
!
! --- Initialization of the ICLOCK array which keeps baseline-dependent clock
! --- estimation flags
!
      DO 410 J1=1,N_STA
         DO 420 J2=1,ARC_STA_BIT_WORDS
            ICLOCK  (J2,J1) = 0
            ICLOCK_P(J2,J1) = 0
 420     CONTINUE
 410  CONTINUE
      IF ( U_TRI .LE. 0 ) THEN
!
! -------- No closed triangle -- nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Set status "not used" for all closed triangles
!
      DO 430 J3=1,U_TRI
         SEL_TRI(J3) = .FALSE.
 430  CONTINUE
!
! --- Set status "used" or "not used" for all participated baselines.
! --- Status "used" will be set only for the baselines which have common
! --- stations with clock reference stations
!
      DO 440 J4=1,U_BAS
         SEL_BAS(J4) = .FALSE.
!
         CALL NBAST ( UIS_BAS(J4), NST1, NST2 )
         IF ( NST1 .EQ. INT4(BM_REF_CL)  .OR.   NST2 .EQ. INT4(BM_REF_CL) ) THEN
              SEL_BAS(J4) = .TRUE.
         END IF
!
         IF ( KBIT ( CLOCK_REF_BITS, INT2(NST1) )  .OR. &
     &        KBIT ( CLOCK_REF_BITS, INT2(NST2) )         ) THEN
              SEL_BAS(J4) = .TRUE.
         END IF
 440  CONTINUE
!
      IF ( U_TRI .EQ. ((N_STA-1)*(N_STA-2))/2 ) THEN
!
! -------- The simpliest case: all possible baselines were used
!
           DO 450 J5=1,U_BAS
              IF ( .NOT. SEL_BAS(J5) ) THEN
                   CALL NBAST ( UIS_BAS(J5), NST1, NST2 )
                   CALL SBIT  ( ICLOCK(1,INT2(NST1)),   INT2(NST2), INT2(1) )
                   CALL SBIT  ( ICLOCK(1,INT2(NST2)),   INT2(NST1), INT2(1) )
                   CALL SBIT  ( ICLOCK_P(1,INT2(NST1)), INT2(NST2), INT2(1) )
                   CALL SBIT  ( ICLOCK_P(1,INT2(NST2)), INT2(NST1), INT2(1) )
              END IF
 450       CONTINUE
         ELSE
!
! -------- Not all possible baselines were used.
!
           K_TRI = 0
           DO 460 J6=2,0,-1
!
! ----------- First try triangles with 2 baselines have been already used,
! ----------- then  try triangles with 1 baselines have been already used,
! ----------- and then triangles with no baselines used.
!
              DO 470 J7=1,U_TRI
                 IF ( .NOT. SEL_TRI(J7) ) THEN
!
! ------------------- We take into consideration only triangles which have
! ------------------- not been used earlier
!
! ------------------- Count how many times baselines from this triangle have
! ------------------- been used earlier
!
                      IMB = 0
                      DO 480 J8=1,3
                         IF ( SEL_BAS(UIS_TRI(J8,J7)) ) IMB = IMB + 1
 480                  CONTINUE
!
! ------------------- They were used exact number of times we expect
!
                      IF ( IMB .GE. J6  .AND.  IMB .LT. 3 ) THEN
!
! ------------------------ Search for a baseline form this triangle which have
! ------------------------ not been yet used
!
                           DO 490 J9=1,3
                              IF ( .NOT. SEL_BAS(UIS_TRI(J9,J7)) ) THEN
!
! -------------------------------- OK. This baseline has not been used yet.
!
                                   CALL NBAST ( UIS_BAS(UIS_TRI(J9,J7)), NST1, &
     &                                                                   NST2 )
!
! -------------------------------- Set flags of estimation of baseline
! -------------------------------- dependent clocks
!
                                   CALL SBIT  ( ICLOCK(1,INT2(NST1)), &
     &                                                   INT2(NST2),   INT2(1) )
                                   CALL SBIT  ( ICLOCK(1,INT2(NST2)), &
     &                                                   INT2(NST1),   INT2(1) )
                                   CALL SBIT  ( ICLOCK_P(1,INT2(NST1)), &
     &                                                   INT2(NST2),   INT2(1) )
                                   CALL SBIT  ( ICLOCK_P(1,INT2(NST2)), &
     &                                                   INT2(NST1),   INT2(1) )
!
                                   SEL_BAS(UIS_TRI(J9,J7)) = .TRUE.
                                   SEL_TRI(J7)             = .TRUE.
                                   K_TRI = K_TRI + 1 ! Triangle counter
                                   IF ( K_TRI .EQ. U_TRI ) GOTO 860
                                   GOTO 470
                              END IF
 490                       CONTINUE
                      END IF
                 END IF
 470          CONTINUE
 460       CONTINUE
!
 860       CONTINUE
           IF ( K_TRI .LT. U_TRI ) THEN
                CALL ERR_LOG ( 8454, IUER, 'SET_BASCLK', 'Trap of internal '// &
     &              'control: not all possible baseline-dependent clocks '// &
     &              'have been set up. Please send verbose bug-report to '// &
     &              'Leonid Petrov (pet@leo.gsfc.nasa.gov) ' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SET_BASCLK  #!#
