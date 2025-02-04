      SUBROUTINE MALO_EPHEDISP_WRITE ( MALO, DSP_ARR, PRGNAM, FILOUT, &
     &                                 FILDSC, FILCOM, FILFMT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_EPHEDISP_WRITE writes down the file with atmosphere   *
! *   pressure loading displacements.                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MALO ( MALO__TYPE ) -- Object that holds internal data structure  *
! *                           of package malo.                           *
! * DSP_ARR ( REAL*8     ) -- Displacement array. Units: meters.         *
! *                           Dimension: (3,MALO%NSTA).                  *
! *  PRGNAM ( CHARACTER  ) -- Program name and version                   *
! *  FILOUT ( CHARACTER  ) -- The name of the output file.               *
! *  FILDSC ( CHARACTER  ) -- The name of description file. If           *
! *                           FILDSC == 'NO' then no dsecription file    *
! *                           is read and the description block in the   *
! *                           output file is left blank.                 *
! *  FILCOM ( CHARACTER  ) -- The name of comment file. If               *
! *                           FILCOM == NO then no comment file          *
! *                           is read and the comments block in the      *
! *                           output file is left blank.                 *
! *  FILFMT ( CHARACTER  ) -- The name of the file with EPHEDISP format  *
! *                           description. If FILFMT == NO then no       *
! *                           format description is put in the output    *
! *                           file.                                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 05-DEC-2002 MALO_EPHEDISP_WRITE v3.2 (c) L. Petrov 24-JUL-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      INCLUDE   'astro_constants.i'
      INCLUDE   'ephedisp.i'
      TYPE ( MALO__TYPE         ) :: MALO
      CHARACTER  PRGNAM*(*), FILOUT*(*), FILDSC*(*), FILCOM*(*), FILFMT*(*)
      REAL*8     DSP_ARR(3,MALO%NSTA,MALO%NTIM)
      INTEGER*4  IUER
      TYPE ( EPHEDISP__P_RECORD ) :: P_REC
      TYPE ( EPHEDISP__S_RECORD ) :: S_REC
      TYPE ( EPHEDISP__D_RECORD ) :: D_REC
      TYPE ( EPHEDISP__T_RECORD_SAMPLE ) ::  TS_REC
!
      CHARACTER  STR*128, STR_DATE*32
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  SYSNAME*128, NODENAME*128, HARDWARE*128
      CHARACTER  BEG_COM_LINE*80, END_COM_LINE*80, &
     &           BEG_DSC_LINE*80, END_DSC_LINE*80, &
     &           BEG_FMT_LINE*80, END_FMT_LINE*80
      REAL*8     SEC_EPC, DSP_INTERVAL
      LOGICAL*4  LEX
      INTEGER*4  LUN, LUN_DSC, LUN_COM, LUN_FMT, IOS, MJD_EPC, &
     &           J1, J2, J3, J4, J5, J6
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, GET_UNIT, LOC__SUN$$_STR
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD
!
      BEG_DSC_LINE = '#============================ Beginning of description: ========================'
      END_DSC_LINE = '#============================ End of description: =============================='
      BEG_COM_LINE = '#============================ Beginning of comments: ==========================='
      END_COM_LINE = '#============================ End of comments: ================================='
      BEG_FMT_LINE = '#============================ Beginning of format description: ================='
      END_FMT_LINE = '#============================ End of format description: ======================='
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2721, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open the output file '// &
     &          FILOUT )
           RETURN
      END IF
!
! --- Write the label
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) EPHEDISP__LABEL
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2722, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write in the output '// &
     &         'file '//FILOUT )
           RETURN
      END IF
!
! --- Write down the string with program name and program version
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# Created by '// &
     &        PRGNAM(1:I_LEN(PRGNAM))
!
! --- Get information about user name and system name
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- Write information about user and system
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#         run by '// &
     &        USER_REALNAME(1:I_LEN(USER_REALNAME))//' ( '// &
     &        USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#             on '// &
     &        NODENAME(1:I_LEN(NODENAME))//' at '//GET_CDATE()//' local time'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#'
!
      IF ( FILDSC(1:2) .EQ. '  ' .OR. FILDSC(1:2) .EQ. 'NO' .OR. &
     &     FILDSC(1:2) .EQ. 'No' .OR. FILDSC(1:2) .EQ. 'no'      ) THEN
           CONTINUE
         ELSE
!
! -------- Well, we have to put the file with description in the output file
!
           INQUIRE ( FILE=FILDSC, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 2723, IUER, 'MALO_EPHEDISP_WRITE', 'File with '// &
     &              'comments '//FILDSC(1:I_LEN(FILDSC))//' has not been '// &
     &              'found' )
                RETURN
           END IF
!
           LUN_DSC = GET_UNIT ()
!
! -------- Open file with comments
!
           OPEN ( UNIT=LUN_DSC, FILE=FILDSC, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 2724, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'comments '//FILDSC )
                RETURN
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) BEG_DSC_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           DO 410 J1=1,1024*1024
!
! ----------- Read  a line
!
              READ ( UNIT=LUN_DSC, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) GOTO 810
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2725, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &                  'with description '//FILDSC )
                   RETURN
              END IF
!
! ----------- Write a line
!
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2726, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 410       CONTINUE
 810       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) END_DSC_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           CLOSE ( UNIT=LUN_DSC )
      END IF
!
      IF ( FILCOM(1:2) .EQ. '  ' .OR. FILCOM(1:2) .EQ. 'NO' .OR. &
     &     FILCOM(1:2) .EQ. 'No' .OR. FILCOM(1:2) .EQ. 'no'      ) THEN
           CONTINUE
         ELSE
!
! -------- Well, we have to put the file with comments in the output file
!
           INQUIRE ( FILE=FILCOM, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 2727, IUER, 'MALO_EPHEDISP_WRITE', 'File '// &
     &              'with comments '//FILCOM(1:I_LEN(FILCOM))//' has '// &
     &              'not been found' )
                RETURN
           END IF
!
           LUN_COM = GET_UNIT ()
!
! -------- Open file with comments
!
           OPEN ( UNIT=LUN_COM, FILE=FILCOM, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 2728, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'comments '//FILCOM )
                RETURN
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) BEG_COM_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           DO 420 J2=1,1024*1024
!
! ----------- Read  a line
!
              READ ( UNIT=LUN_COM, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) GOTO 820
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2729, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &                  'with comments '//FILCOM )
                   RETURN
              END IF
!
! ----------- Write a line
!
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2730, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 420       CONTINUE
 820       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) END_COM_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           CLOSE ( UNIT=LUN_COM )
      END IF
!
      IF ( FILFMT(1:2) .EQ. '  '  .OR.  FILFMT(1:2) .EQ. 'NO'  .OR. &
     &     FILFMT(1:2) .EQ. 'No'  .OR.  FILFMT(1:2) .EQ. 'no'       ) THEN
           CONTINUE
         ELSE
!
! -------- Well, we have to put file with format description in the output file
!
           INQUIRE ( FILE=FILFMT, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 2731, IUER, 'MALO_EPHEDISP_WRITE', 'File with '// &
     &              'format description '//FILFMT(1:I_LEN(FILFMT))//' has '// &
     &              'not been found' )
                RETURN
           END IF
!
           LUN_FMT = GET_UNIT ()
           OPEN ( UNIT=LUN_FMT, FILE=FILFMT, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 2732, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'format description '//FILFMT )
                RETURN
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) BEG_FMT_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           DO 430 J3=1,1024*1024
!
! ----------- Read line
!
              READ ( UNIT=LUN_FMT, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) GOTO 830
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2733, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &                  'with format description '//FILFMT )
                   RETURN
              END IF
!
! ----------- Write line
!
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2734, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 430       CONTINUE
 830       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) END_FMT_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           CLOSE ( UNIT=LUN_FMT )
      END IF
!
! --- Write P-record
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#     # sites # epochs # points'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
! 
#ifdef SUN
      CALL LIB$MOVC3 (  LEN(EPHEDISP__P_RECORD_TEMPLATE), &
     &                  %VAL(LOC__SUN$$_STR(EPHEDISP__P_RECORD_TEMPLATE)), &
     &                  P_REC )
#else
      CALL LIB$MOVC3 (  LEN(EPHEDISP__P_RECORD_TEMPLATE), &
     &                 %REF(EPHEDISP__P_RECORD_TEMPLATE), P_REC )
#endif
!
      P_REC%NUMB_T_REC = '3'
      WRITE ( UNIT=P_REC%NUMB_S_REC,  FMT='(I10)', IOSTAT=IOS ) MALO%NSTA
      WRITE ( UNIT=P_REC%NUMB_EPOCHS, FMT='(I6)',  IOSTAT=IOS ) MALO%NTIM
      WRITE ( UNIT=P_REC%NUMB_D_REC,  FMT='(I10)', IOSTAT=IOS ) MALO%NSTA*MALO%NTIM
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2735, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
      CALL LIB$MOVC3 (  LEN(EPHEDISP__P_RECORD_TEMPLATE), P_REC, STR(1:40) )
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:40)
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Radius of displacements validity (in meters)'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A,1X,F14.6)', IOSTAT=IOS ) 'A', MALO__RD_AREA
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2736, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
! --- Write T-beg record
!
      STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( MALO%MJD_BEG, MALO%TAI_BEG ), -2 )
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# First epoch of site displacements'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT=120, IOSTAT=IOS ) 'T begin ', MALO%MJD_BEG, &
     &                                         MALO%TAI_BEG, STR_DATE(1:19)
 120  FORMAT ( A8, 2X, I5, 1X, F7.1, 2X, A )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2737, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
! --- Write T-end record
!
      STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( MALO%MJD_END, MALO%TAI_END ), -2 )
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Last epoch of site displacements'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      IF ( MALO%NTIM > 1 ) THEN
           STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( MALO%MJD_END, MALO%TAI_END ), -2 )
           WRITE ( UNIT=LUN, FMT=120, IOSTAT=IOS ) 'T end   ', MALO%MJD_END, &
     &                                             MALO%TAI_END, STR_DATE(1:19)
         ELSE 
           STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( MALO%MJD_BEG, MALO%TAI_BEG ), -2 )
           WRITE ( UNIT=LUN, FMT=120, IOSTAT=IOS ) 'T end   ', &
     &             MALO%MJD_BEG, MALO%TAI_BEG, STR_DATE(1:19)
      END IF
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2738, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
! --- Write T-sample record
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Sampling interval in days'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
#ifdef SUN
      CALL LIB$MOVC3 (  LEN(EPHEDISP__T_RECORD_TEMPLATE), &
     &                 %VAL(LOC__SUN$$_STR(EPHEDISP__T_RECORD_SAMPLE_TEMPL)), &
     &                  TS_REC )
#else
      CALL LIB$MOVC3 (  LEN(EPHEDISP__T_RECORD_TEMPLATE), &
     &                 %REF(EPHEDISP__T_RECORD_TEMPLATE), TS_REC )
#endif
      IF ( MALO%NTIM > 1 ) THEN
           DSP_INTERVAL = ( MALO%MJD_END - MALO%MJD_BEG + &
     &                    ( MALO%TAI_END - MALO%TAI_BEG )/86400.0 )/(MALO%NTIM-1)
           WRITE ( UNIT=TS_REC%SAMPLE_INTERVAL, FMT='(F16.11)', IOSTAT=IOS ) DSP_INTERVAL
         ELSE 
           DSP_INTERVAL = 0.0D0
           WRITE ( UNIT=TS_REC%SAMPLE_INTERVAL, FMT='(F16.11)', IOSTAT=IOS ) &
     &              MALO%MJD_END - MALO%MJD_BEG + &
     &            ( MALO%TAI_END - MALO%TAI_BEG )/86400.0 
      END IF
#ifdef SUN
      CALL LIB$MOVC3 (  LEN(EPHEDISP__T_RECORD_TEMPLATE), &
     &                  TS_REC, %VAL(LOC__SUN$$_STR(STR)) )
#else
      CALL LIB$MOVC3 (  LEN(EPHEDISP__T_RECORD_TEMPLATE), &
     &                  TS_REC, %REF(STR) )
#endif
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) &
     &        STR(1:LEN(EPHEDISP__T_RECORD_TEMPLATE))
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2739, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Site ID    X-coord.      Y-coord.      Z-coord.     phi-geoc.  longit. height'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
! --- Cycle over stations
!
      DO 440 J4=1,MALO%NSTA
         CALL CLRCH ( STR )
!
! ------ Prepare S-record
!
#ifdef SUN
         CALL LIB$MOVC3 (  LEN(EPHEDISP__S_RECORD_TEMPLATE), &
     &                    %VAL(LOC__SUN$$_STR(EPHEDISP__S_RECORD_TEMPLATE)), &
     &                     S_REC )
#else
         CALL LIB$MOVC3 (  LEN(EPHEDISP__S_RECORD_TEMPLATE), &
     &                    %REF(EPHEDISP__S_RECORD_TEMPLATE), S_REC )
#endif
!
         S_REC%SITE_ID = MALO%STA(J4)%NAME
         WRITE ( UNIT=S_REC%X_COORD,   FMT='(F13.4)' ) MALO%STA(J4)%COO(1)
         WRITE ( UNIT=S_REC%Y_COORD,   FMT='(F13.4)' ) MALO%STA(J4)%COO(2)
         WRITE ( UNIT=S_REC%Z_COORD,   FMT='(F13.4)' ) MALO%STA(J4)%COO(3)
         WRITE ( UNIT=S_REC%GEOC_LAT,  FMT='(F8.4)'  ) MALO%STA(J4)%LAT_GDT/DEG__TO__RAD
         WRITE ( UNIT=S_REC%LONGITUDE, FMT='(F8.4)'  ) MALO%STA(J4)%LON/DEG__TO__RAD
         WRITE ( UNIT=S_REC%HEIGHT,    FMT='(F6.1)'  ) MALO%STA(J4)%HEI_ELL
!
#ifdef SUN
         CALL LIB$MOVC3 ( LEN(EPHEDISP__S_RECORD_TEMPLATE), S_REC, &
     &                    %VAL(LOC__SUN$$_STR(STR)) )
#else
         CALL LIB$MOVC3 ( LEN(EPHEDISP__S_RECORD_TEMPLATE), S_REC, %REF(STR) )
#endif
!
! ------ Write S-record
!
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR(1:I_LEN(STR))
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 2740, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing in the output file '// &
     &             FILOUT )
              RETURN
         END IF
 440  CONTINUE
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#                                                       Loading Displacement'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Epoch  MJD     Time   Calendar date        Site ID    Up       East     North'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
! --- Cycle over epochs
!
      DO 450 J5=1,MALO%NTIM
         SEC_EPC = MALO%MJD_BEG*86400.0D0 + MALO%TAI_BEG + &
     &             (J5-1)*DSP_INTERVAL*86400.0D0
         MJD_EPC = IDINT ( SEC_EPC/86400.D0 + 1.D-5 )
         SEC_EPC = SEC_EPC - MJD_EPC*86400.D0
         STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( MJD_EPC, SEC_EPC), -2 )
!
! ------ ... and over stations
!
         DO 460 J6=1,MALO%NSTA
            CALL CLRCH ( STR )
!
! --------- Site displacement has been computed for this station.
! --------- Prepare D-record
!
#ifdef SUN
            CALL LIB$MOVC3 ( LEN(EPHEDISP__D_RECORD_TEMPLATE), &
     &                       %VAL(LOC__SUN$$_STR(EPHEDISP__D_RECORD_TEMPLATE)), D_REC )
#else
            CALL LIB$MOVC3 ( LEN(EPHEDISP__D_RECORD_TEMPLATE), &
     &                       %REF(EPHEDISP__D_RECORD_TEMPLATE), D_REC )
#endif
            WRITE ( UNIT=D_REC%IND_EPOCH, FMT='(I5)'   ) J5
            WRITE ( UNIT=D_REC%MJD,       FMT='(I5)'   ) MJD_EPC
            WRITE ( UNIT=D_REC%TAI,       FMT='(F7.1)' ) SEC_EPC
            WRITE ( UNIT=D_REC%DATE,      FMT='(A19)'  ) STR_DATE(1:19)
            WRITE ( UNIT=D_REC%SITE_ID,   FMT='(A8)'   ) MALO%STA(J6)%NAME
            WRITE ( UNIT=D_REC%U_DSPL,    FMT='(F8.5)' ) DSP_ARR(1,J6,J5)
            WRITE ( UNIT=D_REC%E_DSPL,    FMT='(F8.5)' ) DSP_ARR(2,J6,J5)
            WRITE ( UNIT=D_REC%N_DSPL,    FMT='(F8.5)' ) DSP_ARR(3,J6,J5)
#ifdef SUN
            CALL LIB$MOVC3 ( LEN(EPHEDISP__D_RECORD_TEMPLATE), D_REC, &
     &                       %VAL(LOC__SUN$$_STR(STR)) )
#else
            CALL LIB$MOVC3 ( LEN(EPHEDISP__D_RECORD_TEMPLATE), D_REC, &
     &                       %REF(STR) )
#endif
!
! --------- Write D-record
!
            WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR(1:I_LEN(STR))
            IF ( IOS .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IOS, STR )
                 CALL ERR_LOG ( 2741, IUER, 'MALO_EPHEDISP_WRITE', 'Error '// &
     &                STR(1:I_LEN(STR))//' in writing in the output '// &
     &               'file '//FILOUT )
                 RETURN
            END IF
 460     CONTINUE
 450  CONTINUE
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) EPHEDISP__LABEL
!
      CLOSE ( UNIT=LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_EPHEDISP_WRITE  !#!#
