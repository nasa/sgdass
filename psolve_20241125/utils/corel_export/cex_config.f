      SUBROUTINE CEX_CONFIG ( CONFIG_FILE, CEX, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  CEX_CONFIG 
! *                                                                      *
! *  ### 07-APR-2005   CEX_CONFIG  v1.0 (c)  L. Petrov  07-APR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'corel_export.i'
      CHARACTER  CONFIG_FILE*(*)
      INTEGER*4  IUER
      TYPE ( CEX__TYPE ) :: CEX
      INTEGER*4  M_BUF, MIND
      PARAMETER  ( M_BUF = 256 )
      PARAMETER  ( MIND  = 32  )
      CHARACTER  BUF(M_BUF)*128, PID_STR*5, STR*128, STR1*128, DELIM*3
      PARAMETER  ( DELIM = CHAR(0)//CHAR(9)//CHAR(32) )
      INTEGER*4  I_PAR, IND(2,MIND), IL, J1, J2, LIND, N_BUF, PID, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GETPID
!
! --- Clean record CEX
!
      CALL NOUT ( SIZEOF(CEX), CEX )
!
! --- Save name of the configuration file
!
      CEX%CONFIG_FILE = CONFIG_FILE
!
! --- Read configuration file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONFIG_FILE, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4821, IUER, 'CEX_CONFIG', 'Error in attempt '// &
     &         'to open geo_export configuration file '//CONFIG_FILE )
           RETURN
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      I_PAR = 0
      DO 410 J1=1,N_BUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .NE. '#'  ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '#!' ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '##' ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, -3 )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ It should be exactly three words. Let's check it
!
         IF ( LIND .LT. 3 ) THEN
              CALL ERR_LOG ( 4822, IUER, 'CEX_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': too few words' )
              RETURN
         END IF
!
! ------ Well. Three words. Parse them
!
! ------ Transform the keyword to the letters of upper case
!
         CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                   BUF(J1)(IND(1,2):IND(2,2))  )
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'OUTCOMING_LOCAL_DIR:' ) THEN
              CEX%OUTCOMING_LOCAL_DIR  = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'OUTCOMING_URL_DIR:' ) THEN
              CEX%OUTCOMING_URL_DIR = BUF(J1)(IND(1,3):IND(2,LIND))
              IL = I_LEN(CEX%OUTCOMING_URL_DIR)
              IF ( CEX%OUTCOMING_URL_DIR(IL:IL) .NE. '/' ) THEN
                   CEX%OUTCOMING_URL_DIR = &
     &                           CEX%OUTCOMING_URL_DIR(1:IL)//'/'
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DSERVER_E_MAIL:' ) THEN
              CEX%DSERVER_E_MAIL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CONFIRM_E_MAIL:' ) THEN
              CEX%CONFIRM_E_MAIL = BUF(J1)(IND(1,3):IND(2,LIND))
              IF ( CEX%CONFIRM_E_MAIL ==  'NONE' ) CEX%CONFIRM_E_MAIL = ' '
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SUCCESS_E_MAIL:' ) THEN
              CEX%SUCCESS_E_MAIL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MASTER_DIR:' ) THEN
              CEX%MASTER_DIR = BUF(J1)(IND(1,3):IND(2,LIND))
              IL = I_LEN(CEX%MASTER_DIR)
              IF ( CEX%MASTER_DIR(IL:IL) .NE. '/' ) THEN
                   CEX%MASTER_DIR = CEX%MASTER_DIR(1:IL)//'/'
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'URL_IVSCONTROL:' ) THEN
              CEX%URL_IVSCONTROL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MAIL_COMMAND:' ) THEN
              CEX%MAIL_COMMAND = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'BZIP2_EXE:' ) THEN
              CEX%BZIP2_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WGET_EXE:' ) THEN
              CEX%WGET_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE
              CALL ERR_LOG ( 4823, IUER, 'CEX_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': unsupported keyword '//BUF(J1)(IND(1,2):IND(2,2)) )
              RETURN
         END IF
         I_PAR = I_PAR + 1
 410  CONTINUE
!
! --- Check: did we define all fields of geo_export?
!
      IF ( I_PAR .LT. CEX__M_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( I_PAR, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( CEX__M_PAR, STR1 )
           CALL ERR_LOG ( 4824, IUER, 'CEX_CONFIG', 'Not all parameters '// &
     &         'were found in geo_export configuration file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' --  '//STR(1:I_LEN(STR))// &
     &         ' instead of '//STR1 )
           RETURN
      END IF
!
      PID = GETPID()
      CALL INCH   ( PID,   PID_STR )
      CALL CHASHR (        PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
      CEX%TMP_COM_FILE = '/tmp/__corel_config__'//PID_STR//'.com'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CEX_CONFIG  #!#
