      SUBROUTINE WRITE_EOP_MENU ( IP, NLINE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1   Display polar motion and UT1 sections of 'last page'
      INCLUDE 'solve.i'
!
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
      INTEGER*2 JKONT, ICHR, JCHAR,IDUM2, IROTF, &
     &          J, KK, K, JJ, II, ILOC, KBITN, KERR, &
     &          LEPS, LPSI, L, NLTIDE, KKK, IIROT, JERR, NLCOR, ICOD, NLINE, &
     &          III, ICOUNT, LP, ITYP, IMIN, IH, ID, IM, IYR, IROTT, &
     &          IDIR, IREGB, IREGA, IEOP, IPOS
      INTEGER*4 IX, IY, IXD, IYD, IDUM, ICH, ILLOC, NCNT, IPOS4, NLREL, &
     &          IONF(3), IMINX_CON, IMINX_INT, IOVER, IDOWN
      CHARACTER*4 CCH
      CHARACTER*2 CCHAR
      EQUIVALENCE ( ICHR,CCHAR)
      EQUIVALENCE ( ICH,CCH)
!
      LOGICAL*2   IP, KBIT, TOG_OK, KSTON
      CHARACTER*3 RES_FLAG,COR_FLAG,norm_out_flag,norm_zero_flag
      CHARACTER   BUFSTR*79
!
      INTEGER*2 IDSP(16),KKBUF(188)
      INTEGER*2 NSTPLT(3),NSOLVE(3)
      INTEGER*2 IRC(2,3)
      INTEGER*2 IFIRST_LINE(3), ILAST_LINE(3), IDISP, ICHOICE, &
     &          IMINX_TOG(3), IMAXX_CON, IMAXX_INT, IMAXX_TOG(3)
      CHARACTER CDISP(3)*3, EOP_DISP_A1(2)*10 
!
      REAL*8    REG, FIRST_TIME, LAST_TIME
      INTEGER*2 IREG(2), TRIMLEN
      EQUIVALENCE (REG,IREG,IREGA),(IREG(2),IREGB)
      REAL*8 FJDOBS,LJDOBS,RANGE_LEFT,TOLERANCE
      INTEGER*2 IBIT,ITYP_START(2),ITYP_STOP(2), I
!
      DATA NSTPLT/2HST,2HPL,2HT /,NSOLVE/2HSO,2HLV,2HE /
      DATA IRC/2HXW,2HOB,2HYW,2HOB,2HUT,2H1 /
      DATA EOP_DISP_A1 /"X/Y WOBBLE","UT1-TAI   "/
      CHARACTER PM_STRING*17, UT_STRING*17 
      DATA ITYP_START / 1, 3 /, ITYP_STOP / 2, 3 /
      CHARACTER  STR*54, GET_VERSION*54
!
!     Partial soft coding for the location of the earth orientation fields
!
      DATA IONF / 35, 52, 74 /
      DATA IMINX_TOG / 13, 40, 57 /, IMAXX_TOG / 37, 54, 76 /
      DATA IMINX_INT / 13 /, IMAXX_INT / 48 /
      DATA IMINX_CON / 51 /, IMAXX_CON / 79 /
      INTEGER*4   I_LEN
!
! 4.  HISTORY
!
!     97.09.02:jwr: New code to support changing the eop epoch.
!     97.11.05:pet: Removed hard coded SEOCNST values (now they are installed
!                   by blkcl)
!
      EOP_STYLE(2) = EOP_STYLE(1)
!
! --- Set up the crt display.
! --- Clear the screen.
!
      IF ( IP ) THEN  !setting up the display
           CALL SETCR_MN ( 0, 0 )
           CALL CLEAR_MN()
      END IF  !setting up the display
!
! --- Set up polar motion and UT1 flags.
!
      IF(IP) THEN
!
        IF (     EOP_STYLE(1) .EQ. EOP__POLY          ) THEN
           CALL ADDSTR_F( 'Polar motion and UT1: Polynomial Parameterization' )
        ELSE IF(EOP_STYLE(1) .EQ. EOP__RATES_AND_SEGS) THEN
           CALL ADDSTR_F( 'Polar motion and UT1: Global Rate and Segments   ' )
        ELSE IF(EOP_STYLE(1) .EQ. EOP__SEGS_ONLY     ) THEN
           CALL ADDSTR_F( 'Polar motion and UT1: Segmented Offsets Only     ' )
        ELSE IF(EOP_STYLE(1) .EQ. EOP__SINE          ) THEN
           CALL ADDSTR_F( 'Polar motion and UT1 Parameterization            ' )
        ENDIF
!
        STR = GET_VERSION()
        CALL SETCR_MN ( 79-I_LEN(STR), 0 )
        CALL REVERSE_ON_MN()
        CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
        CALL REVERSE_OFF_MN()
        CALL NL_MN()
      ENDIF
!
      CALL SETCR_MN ( 0, 1 )
      CALL REFRESH_MN()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!write ( 6, * ) ' eop_style(1) = ', eop_style(1), &
!     &         ' eop_style(2) = ', eop_style(2)  ! %%%%%%%%%%%%%%%%%%
!write ( 6, * ) ' ITYP_START = ', ITYP_START   ! %%%%%%%%%%%%%%%%%%%%%%
!write ( 6, * ) ' ITYP_STOP  = ', ITYP_STOP    ! %%%%%%%%%%%%%%%%%%%%%%
!write ( 6, * ) ' NROT = ', NROT
!write ( 6, * ) ' ITYP_STOP = ', ITYP_STOP
!write ( 6, * ) ' TROT(1) = ', TROT(1), ' TROT(2) = ', TROT(2)
!CALL PAUSE ( 'WRITE_EOP_MENU' ) ! %%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO IEOP = 1,2 !runing over x&y pole (1) and ut1 (2)
         IF ( EOP_STYLE(IEOP) .EQ. EOP__POLY) THEN !old style of polynomial parameterization
!                                                  (individually specified epochs and settings)
            DO ITYP = ITYP_START(IEOP), ITYP_STOP(IEOP) ! X and Y pole, or UT1
               DO I=1,NROT ! running over rotation epochs
                  CALL EPOC(IM,ID,IYR,IH,IMIN,TROT(I) )
                  DO IPOS =1,4 !running over 0 to 3 order
                     IDSP(IPOS) = IROTT(I,ITYP,IPOS,LROT)
                  END DO  !running over 0 to 3 order
!
                  IF ( IP ) THEN
                       CALL CLRCH ( BUFSTR )
                       WRITE ( BUFSTR, 80 ) IYR,IM,ID,IH,IMIN, &
     &                         (IRC(LP,ITYP),LP=1,2),(IDSP(LP),LP=1,4)
   80                  FORMAT(2(I2,"/"),I2,1X,I2,":",I2,1X,2A2, &
     &                        ' Coefficients',2X,4I2)
                       CALL BLANK_TO_ZERO ( BUFSTR(1:8) )
                       CALL BLANK_TO_ZERO ( BUFSTR(10:14) )
                       CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
                  ENDIF
               END DO !running over 0 to 3 order
            END DO !running over x&y or UT1
            IF ( IEOP .EQ. 2 ) THEN
                 BUFSTR = 'Select:(/)G.Rate & Segments (%)Only Segments '// &
     &                    '(|)Sine Style (@)Reset Poly Epoch'
                 CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
            ENDIF
          ENDIF  !old style of parameterization
!
          IF (EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS .OR. &
     &        EOP_STYLE(IEOP) .EQ. EOP__SEGS_ONLY     ) THEN ! global rate and segments
              IF ( IEOP.EQ.2 ) THEN
                 IF ( NROT_A1(1)      .LE.   2             .OR.  &
     &                NROT_A1(1)      .GT. INT2(MAX4_EOP)  .OR.  &
     &                ROT_INTERVAL(1) .LT. 0.25D0/24.0D0         ) THEN
!
! ------------------- Wrong intervals. Reset them
!
                      ROT_INTERVAL(1) = 1.0D0/24.0D0
                      ROT_INTERVAL(2) = ROT_INTERVAL(1)
                      CALL OBSTM ( FIRST_TIME, LAST_TIME )
                      NROT_A1(1) = (LAST_TIME - TROT_A1)/ROT_INTERVAL(1) + 1
                      IF ( TROT_A1+ROT_INTERVAL(1)*(NROT_A1(1)-1) .LT. &
     &                     LAST_TIME ) THEN
                           NROT_A1(1) = NROT_A1(1)+1
                      END IF
!
                      ROT_INTERVAL(2) = ROT_INTERVAL(1)
                      NROT_A1   (2) = NROT_A1   (1)
                      CALL SBIT ( CONSTRAINT_BITS, INT2(4), INT2(1) )
                      CALL SBIT ( CONSTRAINT_BITS, INT2(5), INT2(1) )
                 ENDIF
!
                 WRITE ( BUFSTR, '(5x,"Number of Epochs       :",I4, &
     &                   17X, " Interval:",F9.3," hours")') &
     &                   NROT_A1(IEOP), ROT_INTERVAL(IEOP)*24.D0
                 CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
                 IF ( SEOCNST(1) .LE. 1.D-5 ) CALL SBIT( CONSTRAINT_BITS, &
     &                                                   INT2(4), INT2(0) )
                 IF ( SEOCNST(2) .LE. 1.D-5 ) CALL SBIT( CONSTRAINT_BITS, &
     &                                                   INT2(5), INT2(0) )
                 WRITE ( PM_STRING, '(f7.2," mas/day  ")' ) SEOCNST(1)
                 WRITE ( UT_STRING, '(f7.2," ms/day   ")' ) SEOCNST(2)
                 IF ( .NOT. KBIT( INT2( CONSTRAINT_BITS), INT2(4) )) &
     &                PM_STRING(1:17)= ' None            '
                 IF ( .NOT. KBIT( INT2( CONSTRAINT_BITS), INT2(5) )) &
     &                UT_STRING(1:17)= ' None            '
!
                 WRITE ( BUFSTR, &
     &                   '(5X,"Polar Motion Constraint:",a17,'// &
     &                   '"UT1 Constraint:",a17)') pM_STRING, UT_STRING
                 CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
                 BUFSTR = '     Reset : (>)Interval, ($) Polar '// &
     &                    'Motion Constraint, (&) UT1 Constraint'
                 CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
                 IF ( EOP_STYLE(1) .EQ. EOP__RATES_AND_SEGS ) THEN
                      BUFSTR = '     Select: (#)Polynomial (%)Only '// &
     &                         'Segments (|) Sine Style'
                    ELSE
                      BUFSTR = '     Select: (#)Polynomial (/)Rates and '// &
     &                         'Segments (|) Sine Style'
                 ENDIF
                 CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
            ENDIF
          ENDIF ! global rate and segments
!
          IF ( EOP_STYLE(IEOP) .EQ. EOP__SINE ) THEN ! sine wave style
               IF ( IEOP .EQ. 2 ) THEN
                    BUFSTR = 'Gobal.Rate, Offset, 24hr Sine, 12hr Sine'
                    CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
                    BUFSTR = 'Select: (#)Polynomial (/)G.Rate & '// &
     &                       'Segments (%)Only Segments'
                    CALL WRITE_SCREEN ( BUFSTR(1:I_LEN(BUFSTR)) )
                    CALL WRITE_SCREEN ( ' ' )
                    CALL WRITE_SCREEN ( ' ' )
               ENDIF
          ENDIF ! sine wave style
        ENDDO ! runing over x&y pole (1) and ut1 (2)
!
        NLINE = 4
!
        CALL REFRESH_MN()
        RETURN
        END  !#!  WRITE_EOP_MENU  #!#
