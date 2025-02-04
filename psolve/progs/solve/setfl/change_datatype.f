      SUBROUTINE CHANGE_DATATYPE ( FL_EXPORT_SUPPRESSION )
! ************************************************************************
! *                                                                      *
! *   Routine  CHANGE_DATATYPE  changes solution type in interactive     *
! *   mode.                                                              *
! *                                                                      *
! * ### 17-FEB-1998  CHANGE_DATATYPE  v2.0 (c) L. Petrov 08-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'oborg.i'
      INTEGER*4  ML
      PARAMETER  ( ML = 30 )
      LOGICAL*4  FL_EXPORT_SUPPRESSION 
      CHARACTER  UPLINE*(ML), DOWNLINE*(ML), LINE*(ML), CCH*4, STRLET*36, &
     &           STA(2)*8, STA_USE(2)*8
      REAL*8     WEI_ARR(4)
      INTEGER*2  INEW_DATYP
      INTEGER*4  IHT, IH_FR, IH_SC, IW_SC, IFL, ILL, ILB
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IOS, IX, IY, ICH, IER
      LOGICAL*4  ALLOWED, FL_SUPR
      INTEGER*2  IFL2, IER2
      CHARACTER  JBUF(MAX_ARC_BSL)*80
      LOGICAL*2, EXTERNAL :: KBIT
      EQUIVALENCE ( ICH, CCH )
      DATA STRLET / '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      PARAMETER  ( IH_SC = 23 ) ! min acceptable height of the screen
      PARAMETER  ( IW_SC = 79 ) ! min acceptable width  of the screen
      LOGICAL*4  DATYP_INQ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      DO 410 J1=1,LEN(UPLINE)
         UPLINE(J1:J1)   = CHAR(176)
         DOWNLINE(J1:J1) = '_'
 410  CONTINUE
      UPLINE(1:1) = '|'
      UPLINE(LEN(UPLINE):LEN(UPLINE)) = '|'
      DOWNLINE(1:1) = '|'
      DOWNLINE(LEN(DOWNLINE):LEN(DOWNLINE)) = '|'
!
      ILB   = (IW_SC - ML)/2
!
      IHT = LAST__DTP - FIRST__DTP + 1
      IH_FR = IHT + 2
      IFL = IH_SC - IH_FR + 1
      ILL = IH_SC
!
 910  CONTINUE
      CALL SETCR_MN ( ILB, IFL )
      CALL ADDSTR_F ( UPLINE )
!
      DO 420 J2=1,IHT
         CALL CLRCH ( LINE )
         LINE(3:3) = STRLET(J2:J2)
         CALL DATYP_SHOW ( INT2(J2-1), LINE(6:26) )
         LINE(1:1) = '|'
         LINE(LEN(LINE):LEN(LINE)) = '|'
         ALLOWED = .TRUE.
         IF ( .NOT. KBIT ( OPP_STATUS, OPP_SET2__BIT ) ) ALLOWED = .FALSE.
         IF ( INT2(J2-1) .EQ. GRPRAT__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ. PHSRAT__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ. SNBRAT__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ. GRPONL__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ. PHSONL__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ. SNBONL__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ. RATONL__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ.     GX__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ.  SNG_X__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ.     PX__DTP ) ALLOWED = .TRUE.
         IF ( INT2(J2-1) .EQ.  FUSED__DTP .AND. FUSED_STATUS ==   IONOV__UNDF ) ALLOWED = .FALSE.
!
         IF ( .NOT. ALLOWED ) LINE(LEN(LINE)-2:LEN(LINE)-2) = '*'
         CALL SETCR_MN ( ILB, IFL+J2 )
         IF ( J2-1 .EQ. IDATYP ) CALL REVERSE_ON_MN
         CALL ADDSTR_F ( LINE )
         IF ( J2-1 .EQ. IDATYP ) CALL REVERSE_OFF_MN
 420  CONTINUE
!
      CALL SETCR_MN ( ILB, ILL )
      CALL ADDSTR_F ( DOWNLINE )
!
      IX = ILB-1
      IY = IFL+IDATYP+1
      CALL SETCR_MN ( IX, IY      )
      CALL SENKR_MN ( IX, IY, ICH )
!
      INEW_DATYP = INDEX ( STRLET, CCH(4:4) ) - 1
      IF ( INEW_DATYP .LT. 0 ) THEN
           INEW_DATYP = IY - (IFL+1)
      END IF
!
      IF ( INEW_DATYP .LT. FIRST__DTP  .OR. INEW_DATYP .GT. LAST__DTP ) RETURN
!
      ALLOWED = .TRUE.
      IF ( .NOT. KBIT ( OPP_STATUS, OPP_SET2__BIT ) ) ALLOWED = .FALSE.
      IF ( INEW_DATYP .EQ. GRPRAT__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ. PHSRAT__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ. SNBRAT__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ. GRPONL__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ. PHSONL__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ. SNBONL__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ. RATONL__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ.     GX__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ.  SNG_X__DTP ) ALLOWED = .TRUE.
      IF ( INEW_DATYP .EQ.     PX__DTP ) ALLOWED = .TRUE.
!
      IF ( ALLOWED ) THEN
!
! -------- Set new solution type
!
           IF ( SUPMET == SUPMET__META  .AND.  FL_EXPORT_SUPPRESSION ) THEN
                CALL ACS_OBSFIL ( 'O' )
                DO 430 J3=1,NUMOBS
                   CALL USE_OBSFIL ( IOBSFIL, J3, 'R' )
!
                   FL_SUPR = BTEST ( USER_SUP, INT4(IDATYP) )
                   IF ( FL_SUPR ) THEN
                        USER_SUP = IBSET ( USER_SUP, INT4(INEW_DATYP) )
                      ELSE
                        USER_SUP = IBCLR ( USER_SUP, INT4(INEW_DATYP) )
                   END IF
                   CALL USE_OBSFIL ( IOBSFIL, J3, 'W' )
 430            CONTINUE 
                CALL ACS_OBSFIL ( 'C' )
           END IF
           IDATYP = INEW_DATYP
!
           IF ( SUPMET == SUPMET__META  ) THEN
                CALL USE_GLBFIL_3 ( 'ORC' )
                CALL OPENNAMFIL()
                DO 440 J4=1,(INT4(NUMSTA)*(INT4(NUMSTA)-1))/2
                   IF ( J4 .EQ. 1 ) THEN
                        IFL2 = INT2(1)
                     ELSE
                        IFL2 = INT2(0)
                   END IF
!
! ---------------- Reading REWT card to the arrays of cards
!
                   CALL GETCARD ( INT2(1), 'REWT', IFL2, JBUF(J4), IER2 )
                   IF ( JBUF(J4)(44:52) == '*********' ) JBUF(J4)(44:52) = '     0.00'
                   IF ( JBUF(J4)(54:62) == '*********' ) JBUF(J4)(54:62) = '     0.00'
                   IF ( JBUF(J4)(49:52) == ' NaN' ) JBUF(J4)(49:52) = '0.00'
                   IF ( JBUF(J4)(59:62) == ' NaN' ) JBUF(J4)(59:62) = '0.00'
                   READ ( JBUF(J4), '(5X, A8, 1X, A8, 4F10.2, 8X )', IOSTAT=IOS ) &
     &                    STA(1), STA(2), WEI_ARR
                   STA_USE(1) = STA(1)
                   STA_USE(2) = STA(2)
                   CALL VTD_NAME_REPAIR ( STA_USE(1) )
                   CALL VTD_NAME_REPAIR ( STA_USE(2) )
!                
                   DO 450 J5=1,META_N_BAS
                      IF ( STA_USE(1) == META_RW_BAS(1,J5) .AND. &
     &                     STA_USE(2) == META_RW_BAS(2,J5)       ) THEN
                           WEI_ARR(1) = META_RW_DEL(IDATYP,J5) 
                      END IF
                      IF ( STA_USE(2) == META_RW_BAS(1,J5) .AND. &
     &                     STA_USE(1) == META_RW_BAS(2,J5)       ) THEN
                           WEI_ARR(1) = META_RW_DEL(IDATYP,J5) 
                      END IF
 450               CONTINUE 
                   WRITE ( JBUF(J4), '("REWT ", A8, 1X, A8, 4F10.2, 8X )', IOSTAT=IOS ) &
     &                     STA(1), STA(2), WEI_ARR
 440            CONTINUE 
!
                DO 460 J6=1,(INT4(NUMSTA)*(INT4(NUMSTA)-1))/2
                   IF ( J6 .EQ. 1 ) THEN
                        IFL2 = INT2(1)
                      ELSE
                        IFL2 = INT2(0)
                   END IF
!
                   CALL PUTCARD ( INT2(1), 'REWT', IFL2, JBUF(J6), IER2 )
 460            CONTINUE 
           END IF
!
! -------- Update of ionosphere calibration
!
           CALL SET_IONOFLAG ( IDBSEL, IDATYP )
!
! -------- Update usage status bits
! 
           CALL UPDATE_SUPSTAT ()
         ELSE
!
           DO 470 J7=1,IHT
              CALL CLRCH    ( LINE )
              LINE(1:1) = '|'
              IF ( J7 .EQ. 1 ) THEN
                   LINE(3:)  = 'Solution type '
                 ELSE IF ( J7 .EQ. 3 ) THEN
                   CALL DATYP_SHOW ( INEW_DATYP, LINE(6:26) )
                 ELSE IF ( J7 .EQ. 5 ) THEN
                   LINE(3:)  = 'cannot be set up since'
                 ELSE IF ( J7 .EQ. 6 ) THEN
                   LINE(3:)  = 'there is no information'
                 ELSE IF ( J7 .EQ. 7 ) THEN
                   LINE(3:)  = 'enough in the database'
              END IF
              LINE(LEN(LINE):LEN(LINE)) = '|'
              CALL SETCR_MN ( ILB, IFL+J7 )
              IF ( J7 .NE. 3 ) CALL REVERSE_ON_MN
              CALL ADDSTR_F ( LINE )
              CALL REVERSE_OFF_MN()
 470       CONTINUE
!
           CALL SETCR_MN ( ILB+10, IFL + LEN(LINE)/2 )
           CALL SENKR_MN ( IX, IY, ICH )
           GOTO 910
      END IF
!
      RETURN
      END  !#!  CHANGE_DATATYPE  #!#
