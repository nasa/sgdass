      SUBROUTINE MODCOR  ( L_CLM, MCALNAMS, MCAVL, MCAPL, PROGCOM, CDBNAM, &
     &                     NVER )
! ************************************************************************
! *                                                                      *
! *   Routine  MODCOR
! *                                                                      *
! *  ###  16-NOV-99     MODCOR     v1.0  (c)  L. Petrov  16-NOV-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INTEGER*4  L_CLM, NVER
      INTEGER*2  MCAVL, MCAPL
      CHARACTER  MCALNAMS(M_CLM)*8, CDBNAM*10, PROGCOM*1
      CHARACTER  STR*128, LET*9
      CHARACTER  GET_VERSION*54
      INTEGER*4  J1, J2, J3, J4, IP, IOS, IX, IY, ICH
      INTEGER*4  IYSH, IYCOM
      INTEGER*2  LUN, LCTYP, LCNUM, LDISP(8), LCORC(7), LCSTA(7), LCFAC(2)
      PARAMETER  ( IYSH  =  3 )
      PARAMETER  ( IYCOM = 12 )
      CHARACTER   CCH*4, CH_CODE, CBUF*80, LCORF*128
      EQUIVALENCE (ICH,CCH)
      LOGICAL*2  KBIT
      DATA       LET  / '123456789' /
      INTEGER*4  I_LEN, ILEN
!
 910  CONTINUE
        CALL SETCR_MN ( 0, 0 )
        CALL CLEAR_MN()
        WRITE ( STR, FMT='(A,A,"  <",I2,">")' ) 'Mode calibration status  ', &
     &          CDBNAM, NVER
        CALL ADDSTR_F ( STR )
        STR = GET_VERSION()
        CALL SETCR_MN ( 79-I_LEN(STR), 0 )
        CALL REVERSE_ON_MN()
        CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
        CALL REVERSE_OFF_MN()
!
        IF ( L_CLM .EQ. 0 ) THEN
             CALL SETCR_MN ( 0, 2 )
             CALL ADDSTR_F ( 'No mode calibration is specified in section 50 '// &
     &                       'of CORFxx file' )
             CALL SETCR_MN ( 0, 4 )
             CALL SENKR_MN ( IX, IY, ICH )
             PROGCOM = '+'
             RETURN
        END IF
!
        CALL SETCR_MN ( 0, 2 )
        CALL ADDSTR_F ( '   Code  Calibration    Status      |   Legend:' )
        CALL NL_MN()
        DO 410 J1=0,M_CLM+1
           CALL CLRCH ( STR )
           IF ( J1 .GE. 1  .AND.  J1 .LE. L_CLM ) THEN
                STR = '   ('//LET(J1:J1)//')   '//MCALNAMS(J1)
                IF ( KBIT ( MCAVL, INT2(J1) ) ) THEN
                     IF ( KBIT ( MCAPL, INT2(J1) ) ) THEN
                          STR(23:33) = '  Applied  '
                       ELSE
                          STR(23:33) = 'NOT Applied'
                     END IF
                  ELSE
                     STR(23:33) = '-----------'
                END IF
           END IF
!
           STR(37:37) = '|'
           CALL SETCR_MN ( 0, J1+IYSH )
           CALL ADDSTR_F ( STR )
           CALL NL_MN()
 410    CONTINUE
!
        IX = 40
        IY = 4
        CALL SETCR_MN ( IX, IY )
        CALL ADDSTR_F ( "-----------  :" )
        CALL ADDSTR_F ( " not available" )
        IY = IY + 2
        CALL SETCR_MN ( IX, IY )
        CALL ADDSTR_F ( "NOT APPLIED  :" )
        CALL ADDSTR_F ( " available but not applied" )
        IY = IY + 2
        CALL SETCR_MN ( IX, IY )
        CALL ADDSTR_F ( "APPLIED      :" )
        CALL ADDSTR_F ( " available and applied" )
!
!
        CALL SETCR_MN ( 0, 11 )
        CALL ADDSTR_F ( '----------------------------------------'// &
     &                  '---------------------------------------' )
!
        CALL SETCR_MN ( 0, IYCOM )
        CALL ADDSTR_F ( '(O)Return Options  (+)Return to obs. dep. '// &
     &                  'calibrations  (I)nitialize' )
!
        IX = 0
        IY = 15
        CALL SETCR_MN ( IX, IY )
        STR = '=== A calibration is either applied to all observations in '// &
     &        'the data base or ==='
        CALL ADDSTR_F ( STR )
        IY = IY+1
        CALL SETCR_MN (IX, IY )
        STR = '=== else not applied to any observation in the data base   '// &
     &        '                 ==='
        CALL ADDSTR_F ( STR )
!
        IX = 1
        IY = 12
!
        CALL SETCR_MN ( IX, IY )
        CALL SENKR_MN ( IX, IY, ICH )
        CH_CODE = CCH(4:4)
        IF ( CH_CODE .EQ. CHAR(13) ) CH_CODE = ' '
        IF ( CH_CODE .EQ. ' ' ) THEN
!
! ---------- Transform the command from coordinate to code
!
             IF ( IY .GE. IYSH+1  .AND.  IY .LE. IYSH+L_CLM ) THEN
                  IF ( IX .LE. 3  .AND.  IX .LE. 32 ) THEN
                       CH_CODE = LET(IY-IYSH:IY-IYSH)
                  END IF
             END IF
!
             IF ( IY .GE. IYCOM ) THEN
                  IF ( IX .GE.  0  .AND.  IX .LE. 16 ) CH_CODE = 'O'
                  IF ( IX .GE. 19  .AND.  IX .LE. 53 ) CH_CODE = '+'
                  IF ( IX .GE. 56  .AND.  IX .LE. 67 ) CH_CODE = 'I'
             END IF
        END IF
!
        IP = INDEX ( LET, CH_CODE )
        IF ( IP .GT. 0 ) THEN
             IF ( KBIT ( MCAVL, INT2(IP) ) ) THEN
                  IF ( KBIT ( MCAPL, INT2(IP) ) ) THEN
                       CALL SBIT ( MCAPL, INT2(IP), INT2(0) )
                     ELSE
                       CALL SBIT ( MCAPL, INT2(IP), INT2(1) )
                  END IF
             END IF
           ELSE IF ( CH_CODE .EQ. 'O' ) THEN
             PROGCOM = 'O'
             GOTO 810
           ELSE IF ( CH_CODE .EQ. '+' ) THEN
             PROGCOM = '+'
             GOTO 810
           ELSE IF ( CH_CODE .EQ. 'I' ) THEN
!
! ---------- Operation: setting initial observation calibration setup.
! ---------- First clear off all calibrations
!
             DO 420 J2=1,L_CLM
                CALL SBIT ( MCAPL, INT2(J2), INT2(0) )
 420         CONTINUE
!
! ---------- Form the name of a corfil file
!
             LCORF = PRE_SCR_DIR(1:PRE_SD_LEN)//'CORF'//PRE_LETRS
!
! ---------- Look: is the environment variable CORFIL specified?
!
             CALL GETENVAR ( 'CORFIL', STR )
             IF ( ILEN(STR) .NE. 0 ) LCORF = STR
!
! ---------- Open CORFIL file
!
             LUN = 301
             OPEN ( UNIT=LUN, FILE=LCORF, IOSTAT=IOS, STATUS='OLD' )
             IF ( IOS .NE. 0 ) THEN
                  CALL SETCR_MN ( 4, 20 )
                  CALL ADDSTR_F ( 'ERROR in openning CORFIL' )
                  IX = 0
                  IY = 20
                  CALL SENKR_MN ( IX, IY, ICH )
                  CALL SETCR_MN ( 4, 20 )
                  CALL ADDSTR_F ( '                        ' )
                  GOTO 910
             END IF
!
! ---------- Read CORFIL file. We are looking for the section 51
!
             DO 430 J3=1,1024
                CALL REACO ( LUN, LCTYP, LCNUM, LDISP, LCORC, LCSTA, LCFAC, &
     &                       CBUF )
                IF ( LCTYP .EQ. 51 ) THEN
!
! ------------------ Well. Check does tha calibration specified in this line of
! ------------------ the section 51 of the CORFIL available
!
                     DO 440 J4=1,L_CLM
!
! --------------------- Replace underscores with blanks
!
                        CALL UNDSCR ( CBUF(8:15) )
                        IF ( CBUF(8:15) .EQ. MCALNAMS(J4)  .AND. &
     &                       KBIT ( MCAVL, INT2(J4) )              ) THEN
!
! -------------------------- ... if available -- apply it!
!
                             CALL SBIT ( MCAPL, INT2(J4), INT2(1) )
                        END IF
 440                 CONTINUE
                  ELSE IF ( LCTYP .EQ. 99 ) THEN
!
! ------------------ End of CORFIL has been detected
!
                     GOTO 830
                END IF
 430         CONTINUE
 830         CONTINUE
!
! ---------- Close CORFIL file
!
             CLOSE ( UNIT=LUN )
        END IF  ! cycle over commands
      GOTO 910
!
 810  CONTINUE
      RETURN
      END  !#!  MODCOR   #!#
