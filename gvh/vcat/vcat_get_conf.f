#include <mk5_preprocessor_directives.inc>
      SUBROUTINE VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VCAT_GET_CONF 
! GVF_DB_DIR, GVF_ENV_DIR, VTD_CONF_SES, 
! *                                                                      *
! * ###  18-FEB-2006 VCAT_GET_CONF  v2.0 (c) L. Petrov  08-JUN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vcat.i'
      TYPE     ( VCAT__TYPE ) :: VCAT
      CHARACTER  VCAT_CONF_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 512 )
      PARAMETER  ( MIND = 32  )
      CHARACTER  BUF(MBUF)*128, STR*128, REG*5
      PARAMETER  ( REG = CHAR(32)//CHAR(0)//CHAR(9)//',;' )
      INTEGER*4  J1, J2, IP, IND(2,MIND), LIND, NBUF, IND_REP, FMT_VER, IER
      LOGICAL*1  LEX
      ADDRESS__TYPE :: DIR_DESC 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, LTM_DIF
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, FUNC_READDIR
!
      VCAT%STATUS = VCAT__UNDF
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VCAT_CONF_FILE, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4811, IUER, 'VCAT_GET_CONF', 'Error in an '// &
     &         'attempt to read VCAT configuration file '//VCAT_CONF_FILE )
           RETURN 
      END IF
!
      CALL NOUT ( SIZEOF(VCAT), VCAT )
      IF ( BUF(1)(1:LEN(VCAT__LABEL)) == VCAT__LABEL ) THEN
           FMT_VER = 0
         ELSE IF ( BUF(1)(1:LEN(VCAT__LABEL_V1)) == VCAT__LABEL_V1 ) THEN
           FMT_VER = 1
         ELSE
           CALL CLRCH ( STR ) 
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4812, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &         'parsing VCAT configuration file '// &
     &          VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))//' unrecognized '// &
     &         ' format label at the first line: '//STR(1:64) )
           RETURN 
      END IF
!
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 4813, IUER, 'VCAT_GET_CONF', 'The last and the '// &
     &         'first line of the VCAT configration file '// &
     &          VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))//' are not the same' )
           RETURN 
      END IF
      IF ( FMT_VER == 1 ) THEN
           VCAT%NREPS = 1
           VCAT%GVF_REP_NAME = 'SYS'
      END IF
!
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
         CALL ERR_PASS ( IUER, IER )
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND < 2 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4814, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the VCAT '// &
     &            'configuration file '// &
     &            VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))//' "'// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//'" -- to few words' )
              RETURN 
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GVF_REP_NAMES:' ) THEN
              VCAT%NREPS = LIND - 1
              DO 420 J2=1,VCAT%NREPS
                 IF ( IND(2,J2+1) .NE. IND(1,J2+1)+2 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J1, STR )
                      CALL ERR_LOG ( 4815, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &                    'parsing line '//STR(1:I_LEN(STR))//' of the VCAT '// &
     &                    'configuration file '// &
     &                     VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))//' "'// &
     &                     BUF(J1)(1:I_LEN(BUF(J1)))//'" -- the repository name '// &
     &                    'should have exactly three characters' )
                      RETURN 
                 END IF 
                 VCAT%GVF_REP_NAME(J2) = BUF(J1)(IND(1,J2+1):IND(2,J2+1)) 
 420          CONTINUE 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GVF_ENV_DIR:' ) THEN
              IF ( FMT_VER == 1 ) THEN
                   IND_REP = 1
                   IND(1,3) = IND(1,2)
                   IND(2,3) = IND(2,2)
                 ELSE
                   IND_REP = LTM_DIF ( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, BUF(J1)(IND(1,2):IND(2,2)) )
                   IF ( IND_REP < 1 ) THEN
                        CALL ERR_LOG ( 4816, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &                      'parsing line '//STR(1:I_LEN(STR))//' of the VCAT '// &
     &                      'configuration file '// &
     &                       VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))//' "'// &
     &                       BUF(J1)(1:I_LEN(BUF(J1)))//'" -- the repository name '// &
     &                       BUF(J1)(IND(1,2):IND(2,2))//' was not defined ' )
                        RETURN 
                   END IF
              END IF
              VCAT%GVF_ENV_DIR(IND_REP) = BUF(J1)(IND(1,3):IND(2,3)) 
!
! ----------- Check whether the envelope file directory exists and readable
!
              DIR_DESC = FUNC_OPENDIR ( TRIM(VCAT%GVF_ENV_DIR(IND_REP))//CHAR(0) )
              IF ( DIR_DESC .EQ. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 4817, IUER, 'VCAT_GET_CONF', 'Error in parsing VCAT '// &
     &                      'configuration file '//VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))// &
     &                      ' -- cannot open directory '//VCAT%GVF_ENV_DIR(IND_REP) )
                   RETURN 
              END IF
              IP = CLOSEDIR ( %VAL(DIR_DESC) )
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GVF_DB_DIR:' ) THEN
              IF ( FMT_VER == 1 ) THEN
                   IND_REP = 1
                   IND(1,3) = IND(1,2)
                   IND(2,3) = IND(2,2)
                 ELSE
!
! ---------------- Search for the repository index
!
                   IND_REP = LTM_DIF ( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, BUF(J1)(IND(1,2):IND(2,2)) )
                   IF ( IND_REP < 1 ) THEN
                        CALL ERR_LOG ( 4818, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &                      'parsing line '//STR(1:I_LEN(STR))//' of the VCAT '// &
     &                      'configuration file '// &
     &                       VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))//' "'// &
     &                       BUF(J1)(1:I_LEN(BUF(J1)))//'" -- the repository name '// &
     &                       BUF(J1)(IND(1,2):IND(2,2))//' was not defined in GVF_REP_NAMES:' )
                        RETURN 
                   END IF
              END IF
              VCAT%GVF_DB_DIR(IND_REP)  = BUF(J1)(IND(1,3):IND(2,3)) 
!
! ----------- Check whether the database file directory exists and readable
!
              DIR_DESC = FUNC_OPENDIR ( TRIM(VCAT%GVF_DB_DIR(IND_REP))//CHAR(0) )
              IF ( DIR_DESC .EQ. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 4819, IUER, 'VCAT_GET_CONF', 'Error in parsing VCAT '// &
     &                      'configuration file '//VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))// &
     &                      ' -- cannot open directory '//VCAT%GVF_DB_DIR(IND_REP) )
                   RETURN 
              END IF
              IP = CLOSEDIR ( %VAL(DIR_DESC) )
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'VTD_CONF_FILE:' ) THEN
              VCAT%VTD_CONF_SES_FILE = BUF(J1)(IND(1,2):IND(2,2)) 
              INQUIRE ( FILE=VCAT%VTD_CONF_SES_FILE, EXIST=LEX ) 
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 4820, IUER, 'VCAT_GET_CONF', 'Error in parsing VCAT '// &
     &                 'configuration file '//VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))// &
     &                 ' -- session VTD configuration file '//TRIM(VCAT%VTD_CONF_SES_FILE)// &
     &                 ' does not exist' )
                   RETURN 
              END IF
            ELSE 
              CALL ERR_LOG ( 4821, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the VCAT '// &
     &            'configuration file '// &
     &            VCAT_CONF_FILE(1:ILEN(VCAT_CONF_FILE))//' "'// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//'" -- unrecognized keyword' )
              RETURN 
         END IF
 410  CONTINUE 
!
      IF ( VCAT%NREPS == 0 ) THEN
           CALL ERR_LOG ( 4822, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &         'parsing VCAT configuration file '//TRIM(VCAT_CONF_FILE)// &
     &         ' -- keyword GVF_REP_NAMES was not found' )
           RETURN 
      END IF
!
      IF ( ILEN(VCAT%VTD_CONF_SES_FILE) == 0 ) THEN
           CALL ERR_LOG ( 4823, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &         'parsing VCAT configuration file '// &
     &          TRIM(VCAT_CONF_FILE)//' -- keyword '// &
     &         'VTD_CONF_FILE: was not found' )
           RETURN 
      END IF
!
      VCAT%CONF_FILE = VCAT_CONF_FILE
      VCAT%STATUS = VCAT__LOADED
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VCAT_GET_CONF  !#!#
