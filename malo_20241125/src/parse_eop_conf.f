      SUBROUTINE PARSE_EOP_CONF ( EOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_EOP_CONF
! *                                                                      *
! * ### 05-MAR-2016 PARSE_EOP_CONF  v1.3 (c)  L. Petrov  04-DEC-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
      TYPE     ( MALO__EOP_TYPE ) :: EOP
      INTEGER*4  IUER 
      LOGICAL*1  LEX, FL_EOP_FILE 
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF  = 256 )
      PARAMETER  ( MIND  =  32 )
      CHARACTER  BUF(MBUF)*128, EOP_DIR*128, STR*128, STR1*128
      INTEGER*4  J1, J2, J3, NBUF, LIND, IND(2,MIND), L_EOP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=EOP%CNF_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6611, IUER, 'PARSE_EOP_CONF', 'EOP_FCS '// &
     &          'configuration file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &          ' does not exist' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( EOP%CNF_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6612, IUER, 'PARSE_EOP_CONF', 'Error in reading '// &
     &         'configuration file '//EOP%CNF_FILE )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(EOP_CONF__LABEL)) == EOP_CONF__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 6613, IUER, 'PARSE_EOP_CONF', 'Error in parsing '// &
     &         'configuration file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &         ' -- the first line is '//STR(1:I_LEN(STR))//' while label '// &
     &         EOP_CONF__LABEL//' was expected' )
           RETURN 
      END IF
!
      FL_EOP_FILE = .FALSE.
      L_EOP = 0
      CALL CLRCH ( EOP_DIR )
      DO 410 J1=1,NBUF
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'eop_file:' ) THEN
              IF ( LIND < 5 ) THEN
                   CALL ERR_LOG ( 6614, IUER, 'PARSE_EOP_CONF', 'Error in parsing '// &
     &                  'configuration file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &                  ' -- the '//STR(1:I_LEN(STR))//'-th line has less than 5'// &
     &                  ' words' )
                   RETURN 
              END IF
              IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_r' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__R) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__R) = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_u' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__U) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__U) = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_i' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__I) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__I) = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_j' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__J) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__J) = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_s' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__S) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__S) = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_f' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__F) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__F) = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_l' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__L) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__L) = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '_c' ) > 0 ) THEN
                   EOP%CONF%FIL_EOP(EOPS__C) = BUF(J1)(IND(1,4):IND(2,4))//'.eop'
                   EOP%CONF%URL_EOP(EOPS__C) = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              IF ( .NOT. FL_EOP_FILE ) L_EOP = L_EOP + 1
              FL_EOP_FILE = .TRUE.
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'eop_dir:' ) THEN
              EOP_DIR = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'heo_file:' ) THEN
              EOP%CONF%FIL_HEO = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=EOP%CONF%FIL_HEO, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 6615, IUER, 'PARSE_EOP_CONF', 'Error during '// &
     &                 'parsing control file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &                 ' -- the Harmonic Earth Orienation Parameter file '// &
     &                 EOP%CONF%FIL_HEO(1:I_LEN(EOP%CONF%FIL_HEO))// &
     &                 ' defined there is not found' )
                   RETURN 
              END IF
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'leapsec_file:' ) THEN
              EOP%CONF%FIL_LEAPSEC = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=EOP%CONF%FIL_LEAPSEC, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 6616, IUER, 'PARSE_EOP_CONF', 'Error during '// &
     &                 'parsing control file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &                 ' -- the leapsecond file '// &
     &                 EOP%CONF%FIL_LEAPSEC(1:I_LEN(EOP%CONF%FIL_LEAPSEC))// &
     &                 ' defined there is not found' )
                   RETURN 
              END IF
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'aam_ser_file:' ) THEN
              EOP%CONF%FIL_AAM_SER = BUF(J1)(IND(1,3):IND(2,3))
              INQUIRE ( FILE=EOP%CONF%FIL_AAM_SER, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 6617, IUER, 'PARSE_EOP_CONF', 'Error during '// &
     &                 'parsing control file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &                 ' -- the AAM series file '// &
     &                 EOP%CONF%FIL_AAM_SER(1:I_LEN(EOP%CONF%FIL_AAM_SER))// &
     &                 ' defined there is not found' )
                   RETURN 
              END IF
              EOP%CONF%URL_AAM_SER = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'eop_fcs_pref:' ) THEN
              EOP%CONF%EOP_FCS_PREF = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ners_url:' ) THEN
              EOP%CONF%NERS_URL = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'nut_apr_mod:' ) THEN
              EOP%CONF%NUT_APR_MOD = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'prc_apr_mod:' ) THEN
              EOP%CONF%PRC_APR_MOD = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'heo_mod:'     ) THEN
              EOP%CONF%HEO_MOD = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'e3z_mod:'     ) THEN
              EOP%CONF%E3Z_MOD = BUF(J1)(IND(1,2):IND(2,LIND))
              IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_D93 ) THEN
                   CONTINUE 
                 ELSE IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_RE2014 ) THEN
                   CONTINUE 
                 ELSE IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_NONE   ) THEN
                   CONTINUE 
                ELSE
                   CALL ERR_LOG ( 6618, IUER, 'PARSE_EOP_CONF', 'Error in parsing '// &
     &                 'configuration file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &                 ' unknown E3Z model '//EOP%CONF%E3Z_MOD )
                   RETURN 
              END IF
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'heo_id:'      ) THEN
              EOP%CONF%HEO_ID = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ltp_mod:'     ) THEN
              EOP%CONF%LTP_MOD = BUF(J1)(IND(1,2):IND(2,2))
              L_EOP = L_EOP + 1
         END IF
 410  CONTINUE 
!
      IF ( L_EOP .NE. MALO__MEOP ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_EOP,      STR  )
           CALL INCH  ( MALO__MEOP, STR1 )
           CALL ERR_LOG ( 6618, IUER, 'PARSE_EOP_CONF', 'Error in parsing '// &
     &         'configuration file '//EOP%CNF_FILE(1:I_LEN(EOP%CNF_FILE))// &
     &         ' -- found '//STR(1:I_LEN(STR))//' keyword of '//STR1 )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__R)= EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__R)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__R), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6619, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_R file '//EOP%CONF%FIL_EOP(EOPS__R) )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__U)= EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__U)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__U), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6620, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_R file '//EOP%CONF%FIL_EOP(EOPS__U) )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__F) = EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__F)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__F), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6621, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_F file '//EOP%CONF%FIL_EOP(EOPS__F) )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__I) = EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__I)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__I), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6622, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_I file '//EOP%CONF%FIL_EOP(EOPS__I) )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__J) = EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__J)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__J), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6623, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_J file '//EOP%CONF%FIL_EOP(EOPS__J) )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__S) = EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__S)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__S), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6624, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_F file '//EOP%CONF%FIL_EOP(EOPS__S) )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__L) = EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__L)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__L), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6625, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_L file '//EOP%CONF%FIL_EOP(EOPS__L) )
           RETURN 
      END IF
!
      EOP%CONF%FIL_EOP(EOPS__C) = EOP_DIR(1:I_LEN(EOP_DIR))//'/'//EOP%CONF%FIL_EOP(EOPS__C)
      INQUIRE ( FILE=EOP%CONF%FIL_EOP(EOPS__C), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6626, IUER, 'PARSE_EOP_CONF', 'Cannot find '// &
     &          'EOPS_C file '//EOP%CONF%FIL_EOP(EOPS__C) )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_EOP_CONF  !#!#
