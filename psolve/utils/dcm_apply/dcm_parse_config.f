      SUBROUTINE DCM_PARSE_CONFIG ( DCM_CONFIG, DCM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DCM_PARSE_CONFIG
! *                                                                      *
! * ### 26-OCT-2007 DCM_PARSE_CONFIG v1.1 (c) L. Petrov  03-SEP-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      CHARACTER  DCM_CONFIG*(*)
      TYPE     ( DCM__TYPE     ) :: DCM
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 256 )
      PARAMETER  ( MIND =  32 )
      CHARACTER    BUF(MBUF)*128
      LOGICAL*4  LEX
      CHARACTER  DELIM*3, STR*80, STR1*80
      PARAMETER  ( DELIM = CHAR(0)//CHAR(9)//CHAR(32) )
      INTEGER*4  LIND, IND(2,MIND), J1, J2, J3, J4, J5, &
     &           I_CNF, NBUF, IER
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
!
! --- Check whether configuration file exists
!
      INQUIRE ( FILE=DCM_CONFIG, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6311, IUER, 'DCM_PARSE_CONFIG', 'Configuration '// &
     &         'file '//DCM_CONFIG(1:I_LEN(DCM_CONFIG))//' was not found' )
           RETURN
      END IF
!
! --- Read configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( DCM_CONFIG, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6312, IUER, 'DCM_PARSE_CONFIG', 'Error reading '// &
     &         'configuration file '//DCM_CONFIG(1:I_LEN(DCM_CONFIG)) )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(DCM__CNF_LABEL)) == DCM__CNF_LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 6313, IUER, 'DCM_PARSE_CONFIG', 'Wrong file '// &
     &         'format: the first line of the file used as DCM '// &
     &         'configuration is '//BUF(1)(1:I_LEN(BUF(1)))//' while '// &
     &         'the label line '//DCM__CNF_LABEL//' was expected. '// &
     &         'Please check the file!' )
           RETURN
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      I_CNF = 0
      DO 410 J1=2,NBUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#'  ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ It should be exactly two words. Let's check it
!
         IF ( LIND .LT. 2 ) THEN
              CALL ERR_LOG ( 6314, IUER, 'DCM_PARSE_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &            ' : too few words' )
              RETURN
         END IF
!
! ------ Transform the keyword to the letters of upper case
!
         CALL TRAN ( 11, BUF(J1)(IND(1,1):IND(2,1)), &
     &                   BUF(J1)(IND(1,1):IND(2,1))  )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PROCEDURE_NAME:' ) THEN
              DCM%PRC_NAME = BUF(J1)(IND(1,2):IND(2,2))
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'OBJECT:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == DCM__SOU ) THEN
                   DCM%OBJECT = DCM__SOU 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == DCM__STA ) THEN
                   DCM%OBJECT = DCM__STA 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == DCM__BAS ) THEN
                   DCM%OBJECT = DCM__BAS 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == DCM__ALL ) THEN
                   DCM%OBJECT = DCM__ALL
                 ELSE 
                   CALL ERR_LOG ( 6315, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : file unsupported object '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'INPUT_FILE_LIST:' ) THEN
              DCM%FILIN_LIST = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 6316, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' was not found' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'OUTPUT_DIRECTORY:' ) THEN
              DCM%OUTDIR = BUF(J1)(IND(1,2):IND(2,2))
              DIR_DESC = OPENDIR ( BUF(J1)(IND(1,2):IND(2,2))//CHAR(0) )
              IF ( DIR_DESC .LE. 0 ) THEN
                   CALL GERROR ( STR1 )
                   CALL ERR_LOG ( 6317, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : directory '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' cannot be opened: '//STR1 )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'INCLUDE_NAMES_FILE:' ) THEN
              DCM%FIL_INC = BUF(J1)(IND(1,2):IND(2,2))
              IF ( DCM%FIL_INC(1:2) == 'NO'   .OR. &
     &             DCM%FIL_INC(1:4) == 'NONE'      ) THEN  
                    CALL CLRCH ( DCM%FIL_INC )
                 ELSE 
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 6318, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                      'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EXCLUDE_NAMES_FILE:' ) THEN
              DCM%FIL_EXC = BUF(J1)(IND(1,2):IND(2,2))
              IF ( DCM%FIL_EXC(1:2) == 'NO'   .OR. &
     &             DCM%FIL_EXC(1:4) == 'NONE'      ) THEN  
                    CALL CLRCH ( DCM%FIL_EXC )
                 ELSE 
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 6319, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                      'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SESSION_RESET:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'TRUE' ) THEN
                   DCM%SESS_RESET = .TRUE.
                ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'YES' ) THEN
                   DCM%SESS_RESET = .TRUE.
                ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'FALSE' ) THEN
                   DCM%SESS_RESET = .FALSE.
                ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NO' ) THEN
                   DCM%SESS_RESET = .FALSE.
                ELSE 
                   CALL ERR_LOG ( 6320, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' TRUE or FALSE were '// &
     &                 'expected ' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SELECT_DCM:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I)', IOSTAT=IER ) &
     &               DCM%SELECT_DCM
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6321, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' an integer number '// &
     &                 'was expected' )
                   RETURN
              END IF
              IF ( DCM%SELECT_DCM < 0 ) THEN
                   CALL ERR_LOG ( 6322, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' -- a positive '// &
     &                 'integer number was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'TOTAL_DCM:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I)', IOSTAT=IER ) &
     &               DCM%TOTAL_DCM
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6323, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' an integer number '// &
     &                 'was expected' )
                   RETURN
              END IF
              IF ( DCM%TOTAL_DCM < 0 ) THEN
                   CALL ERR_LOG ( 6324, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' -- a positive '// &
     &                 'integer number was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EDC_TYP:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'ASCII' ) THEN
                   DCM%EDC_TYP = EDC__ASC
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'BINARY' ) THEN
                   DCM%EDC_TYP = EDC__BIN
                 ELSE
                   CALL ERR_LOG ( 6325, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' -- ASCII or BINARY '// &
     &                 'was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EDC_PAR:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I)', IOSTAT=IER ) &
     &               DCM%EDC_PAR
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6326, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' in integer number '// &
     &                 'was expected' )
                   RETURN
              END IF
              IF ( DCM%EDC_PAR < -127 .OR. DCM%EDC_PAR > 127 ) THEN
                   CALL ERR_LOG ( 6327, IUER, 'DCM_PARSE_CONFIG', 'Error '// &
     &                 'in parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  DCM_CONFIG(1:I_LEN(DCM_CONFIG))// &
     &                 ' : error in decoding value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' in integer number '// &
     &                 'was in the range of [-127, 127] was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
           ELSE 
         END IF
 410  CONTINUE 
!
! --- Check whether all keywords were specified
!
      IF ( I_CNF .NE. DCM__M_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( I_CNF, STR )
           CALL INCH  ( DCM__M_PAR, STR1 )
           CALL ERR_LOG ( 6328, IUER, 'DCM_PARSE_CONFIG', 'Wrong number of '// &
     &         'keywords in the control file '// &
     &          DCM_CONFIG(1:I_LEN(DCM_CONFIG))//' -- '//STR(1:I_LEN(STR))// &
     &         ' while '//STR1(1:I_LEN(STR1))//' were expected' )
           RETURN
      END IF
!
      IF ( DCM%SELECT_DCM .GE. DCM%TOTAL_DCM ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( DCM%SELECT_DCM, STR  )
           CALL INCH  ( DCM%TOTAL_DCM,  STR1 )
           CALL ERR_LOG ( 6329, IUER, 'DCM_PARSE_CONFIG', 'Wrong value of '// &
     &         'SELECT_DCM: '//STR(1:I_LEN(STR))//' -- it should be less '// &
     &         'than TOTAL_DCM: '//STR1(1:I_LEN(STR1))// &
     &         ' in the control file '//DCM_CONFIG )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DCM_PARSE_CONFIG  !#!#
