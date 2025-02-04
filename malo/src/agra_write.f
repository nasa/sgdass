      SUBROUTINE AGRA_WRITE ( PRGNAM, FILFMT, FILDSC, FILCOM, AGRA, &
     &                        FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  AGRA_WRITE  writes the time series of Stokes            *
! *   coefficients of the expansion of the atmosphere contribution to    *
! *   the geopotential into a series of spherical harmonics. The output  *
! *   file is in AGRA format. Optionally AGRA_WRITE includes in the      *
! *   comment section of the output file the format description, the     *
! *   description of the file conents and arbitrary comments. These      *
! *   three sections are copied from the files FILFMT, FILDSC, FILCOM.
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  PRGNAM ( CHARACTER  ) -- Program name and version                   *
! *  FILOUT ( CHARACTER  ) -- The name of the output file.               *
! *  FILFMT ( CHARACTER  ) -- The name of the file with AGRA format      *
! *                           description. If FILFMT == NO then no       *
! *                           format description is put in the output    *
! *                           file.                                      *
! *  FILDSC ( CHARACTER  ) -- The name of description file. If           *
! *                           FILDSC == 'NO' then no dsecription file    *
! *                           is read and the description block in the   *
! *                           output file is left blank.                 *
! *  FILCOM ( CHARACTER  ) -- The name of comment file. If               *
! *                           FILCOM == NO then no comment file          *
! *                           is read and the comments block in the      *
! *                           output file is left blank.                 *
! *    AGRA ( RECORD     ) -- Data structure which keeps site related    *
! *                           information: maximal degree, time range,   *
! *                           time epochs of Stokes coefficients, and    *
! *                           the Stokes coefficients themselves.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 06-JAN-2005   AGRA_WRITE  v1.0 (c)  L. Petrov  06-JAN-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'agra.i'
      CHARACTER  PRGNAM*(*), FILOUT*(*), FILFMT*(*), FILDSC*(*), FILCOM*(*)
      INTEGER*4  IDEG, IUER
      TYPE     ( AGRA__D_RECORD        ) :: D_REC
      TYPE     ( AGRA__T_RECORD_SAMPLE ) :: TS_REC
      TYPE     ( AGRA__TYPE            ) :: AGRA
!
      REAL*8     PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2.D0 ) ! PI number
      CHARACTER  STR*128, STR_DATE*32
      CHARACTER  JD_TO_DATE*23, GET_CDATE*19
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  SYSNAME*128, NODENAME*128, HARDWARE*128
      CHARACTER  BEG_COM_LINE*80, END_COM_LINE*80, &
     &           BEG_DSC_LINE*80, END_DSC_LINE*80, &
     &           BEG_FMT_LINE*80, END_FMT_LINE*80
      REAL*8     SEC_EPC
      LOGICAL*4  LEX
      INTEGER*4  LUN, LUN_DSC, LUN_COM, LUN_FMT, IOS, MJD_EPC, &
     &           J1, J2, J3, J4, J5, J6, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
      REAL*8     MJD_SEC_TO_JD
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
! --- Define section delimeters
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
           CALL ERR_LOG ( 2921, IUER, 'AGRA_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open the output file '// &
     &          FILOUT )
           RETURN
      END IF
!
! --- Write the label in the output file
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) AGRA__LABEL
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2722, IUER, 'AGRA_WRITE', 'Error '// &
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
! -------- Well, we have to put the file with solution description 
! -------- in the output file. First check, whether the description file
! -------- exists
!
           INQUIRE ( FILE=FILDSC, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 2723, IUER, 'AGRA_WRITE', 'File with '// &
     &              'comments '//FILDSC(1:I_LEN(FILDSC))//' has not been '// &
     &              'found' )
                RETURN
           END IF
!
           LUN_DSC = 77; ! LUN_DSC = GET_UNIT ()
!
! -------- Open file with solution description
!
           OPEN ( UNIT=LUN_DSC, FILE=FILDSC, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 2724, IUER, 'AGRA_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'comments '//FILDSC )
                RETURN
           END IF
!
! -------- Write tyhe section delimiter
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
                   CALL ERR_LOG ( 2725, IUER, 'AGRA_WRITE', 'Error '// &
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
                   CALL ERR_LOG ( 2726, IUER, 'AGRA_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 410       CONTINUE
 810       CONTINUE
!
! -------- Write the section deliminter and close the solution desciption
! -------- file
!
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
                CALL ERR_LOG ( 2727, IUER, 'AGRA_WRITE', 'File with '// &
     &              'comments '//FILCOM(1:I_LEN(FILCOM))//' has not been '// &
     &              'found' )
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
                CALL ERR_LOG ( 2728, IUER, 'AGRA_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'comments '//FILCOM )
                RETURN
           END IF
!
! -------- Write the section delimeter
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
                   CALL ERR_LOG ( 2729, IUER, 'AGRA_WRITE', 'Error '// &
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
                   CALL ERR_LOG ( 2730, IUER, 'AGRA_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 420       CONTINUE
 820       CONTINUE
!
! -------- Write the section delimeter and close the comment file
!
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
                CALL ERR_LOG ( 2731, IUER, 'AGRA_WRITE', 'File with '// &
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
                CALL ERR_LOG ( 2732, IUER, 'AGRA_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'format description '//FILFMT )
                RETURN
           END IF
!
! -------- Write the section delimeter
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
                   CALL ERR_LOG ( 2733, IUER, 'AGRA_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &                  'with format description '//FILFMT )
                   RETURN
              END IF
!
! ----------- Write the line
!
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2734, IUER, 'AGRA_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 430       CONTINUE
 830       CONTINUE
!
! -------- Write the section delimeter and close the format description file
!
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) END_FMT_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           CLOSE ( UNIT=LUN_FMT )
      END IF
!
! --- Write the P-record
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#     # sites # epochs # points'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT=110, IOSTAT=IOS ) AGRA%L_DEG, AGRA%L_EPC, AGRA%L_COE
 110  FORMAT ( 'P T 3 M ',I4,' E ',I5,' D ',I7 )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2735, IUER, 'AGRA_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
! --- Write the T-beg record
!
      CALL ERR_PASS ( IUER, IER )
      STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( AGRA%MJD_BEG, &
     &                                        AGRA%SEC_BEG ), IER )
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# First epoch of Stokes coefficients'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT=120, IOSTAT=IOS ) 'T begin ', AGRA%MJD_BEG, &
     &                                         AGRA%SEC_BEG, STR_DATE(1:19)
 120  FORMAT ( A8, 2X, I5, 1X, F7.1, 2X, A )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2736, IUER, 'AGRA_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
! --- Write T-end record
!
      IF ( AGRA%SEC_END .LT. -0.001D0 ) THEN
!
! -------- Fix a bug in atm3_create
!
           AGRA%MJD_END = AGRA%MJD_END - 1
           AGRA%SEC_END = AGRA%SEC_END + 86400.0D0
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( AGRA%MJD_END, &
     &                                        AGRA%SEC_END ), IER )
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Last epoch of Stokes coefficients'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT=120, IOSTAT=IOS ) 'T end   ', AGRA%MJD_END, &
     &        AGRA%SEC_END, STR_DATE(1:19)
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2737, IUER, 'AGRA_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
! --- Write the T-sample record
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Sampling interval in days'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
#ifdef SUN
      CALL LIB$MOVC3 (  LEN(AGRA__T_RECORD_SAMPLE_TEMPL), &
     &                 %VAL(LOC__SUN$$_STR(AGRA__T_RECORD_SAMPLE_TEMPL)), TS_REC )
#else
      CALL LIB$MOVC3 (  LEN(AGRA__T_RECORD_SAMPLE_TEMPL), &
     &                 %REF(AGRA__T_RECORD_SAMPLE_TEMPL), TS_REC )
#endif
      IF ( AGRA%L_EPC .GT. 1 ) THEN
           WRITE ( UNIT=TS_REC%SAMPLE_INTERVAL, FMT='(F16.11)', IOSTAT=IOS ) &
     &              ( ( AGRA%MJD_END - AGRA%MJD_BEG ) +          &
     &                ( AGRA%SEC_END - AGRA%SEC_BEG )/86400.0D0 &
     &              )/(AGRA%L_EPC-1)
         ELSE 
           TS_REC%SAMPLE_INTERVAL = '0.0             '
      END IF
#ifdef SUN
      CALL LIB$MOVC3 (  LEN(AGRA__T_RECORD_SAMPLE_TEMPL), &
     &                  TS_REC, %VAL(LOC__SUN$$_STR(STR)) )
#else
      CALL LIB$MOVC3 (  LEN(AGRA__T_RECORD_SAMPLE_TEMPL), &
     &                  TS_REC, %REF(STR) )
#endif
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) &
     &        STR(1:LEN(AGRA__T_RECORD_SAMPLE_TEMPL))
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2738, IUER, 'AGRA_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in the output file '//FILOUT )
           RETURN
      END IF
!
! --- Put some comments
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Epoch  MJD     Time   Calendar date        Deg Order Stokes_cos  Stokes_sin'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
! --- Cycle over epochs
!
      DO 440 J4=1,AGRA%L_EPC
         SEC_EPC = AGRA%MJD_BEG*86400.0D0 + AGRA%SEC_BEG + &
     &             (J4-1)*AGRA%INTERVAL
         MJD_EPC = IDINT ( SEC_EPC/86400.D0 + 1.D-5 )
         SEC_EPC = SEC_EPC - MJD_EPC*86400.D0
         STR_DATE = JD_TO_DATE ( MJD_SEC_TO_JD ( MJD_EPC, SEC_EPC), -3 )
!
! ------ ... and over stations
!
         DO 450 J5=0,AGRA%L_DEG  !
            DO 460 J6=J5,AGRA%L_DEG !
               CALL CLRCH ( STR )
!
! ------------ Prepare D-record
!
#ifdef SUN
               CALL LIB$MOVC3 ( LEN(AGRA__D_RECORD_TEMPLATE), &
     &                          %VAL(LOC__SUN$$_STR(AGRA__D_RECORD_TEMPLATE)), D_REC )
#else
               CALL LIB$MOVC3 ( LEN(AGRA__D_RECORD_TEMPLATE), &
     &                          %REF(AGRA__D_RECORD_TEMPLATE), D_REC )
#endif

               WRITE ( UNIT=D_REC%IND_EPOCH, FMT='(I5)'   ) J4
               WRITE ( UNIT=D_REC%MJD,       FMT='(I5)'   ) MJD_EPC
               WRITE ( UNIT=D_REC%TAI,       FMT='(F7.1)' ) SEC_EPC
               WRITE ( UNIT=D_REC%DATE,      FMT='(A19)'  ) STR_DATE(1:19)
               WRITE ( UNIT=D_REC%DEGREE,    FMT='(I3)'   ) J6
               WRITE ( UNIT=D_REC%ORDER,     FMT='(I3)'   ) J5
               WRITE ( UNIT=D_REC%STOKES_C,  FMT='(1PD12.5)' ) &
     &                                       AGRA%STOKES(C__COEF,J6,J5,J4)
               WRITE ( UNIT=D_REC%STOKES_S,  FMT='(1PD12.5)' ) &
     &                                       AGRA%STOKES(S__COEF,J6,J5,J4)
#ifdef SUN
               CALL LIB$MOVC3 ( LEN(AGRA__D_RECORD_TEMPLATE), D_REC, &
     &                          %VAL(LOC__SUN$$_STR(STR)) )
#else
               CALL LIB$MOVC3 ( LEN(AGRA__D_RECORD_TEMPLATE), D_REC, &
     &                          %REF(STR) )
#endif
!
! ------------ Write the D-record
!
               WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR(1:I_LEN(STR))
               IF ( IOS .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IOS, STR )
                     CALL ERR_LOG ( 2740, IUER, 'AGRA_WRITE', 'Error '// &
     &                    STR(1:I_LEN(STR))//' in writing in the output '// &
     &                   'file '//FILOUT )
                     RETURN
               END IF
 460        CONTINUE
 450     CONTINUE
 440  CONTINUE
!
! --- Wrrite the trailer record and eventually close the output file.
! --- Deal done
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) AGRA__LABEL
!
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  AGRA_WRITE
