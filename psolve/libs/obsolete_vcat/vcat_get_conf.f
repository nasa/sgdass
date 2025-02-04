      SUBROUTINE VCAT_GET_CONF ( VCAT_CONF_FILE, GVF_DB_DIR, &
     &                           GVF_ENV_DIR, VTD_CONF_SES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VCAT_GET_CONF 
! *                                                                      *
! * ###  18-FEB-2006 VCAT_GET_CONF  v1.0 (c) L. Petrov  18-FEB-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  VTD_CONF_SES*(*), GVF_DB_DIR*(*), GVF_ENV_DIR*(*), &
     &           VCAT_CONF_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 512 )
      PARAMETER  ( MIND = 32  )
      CHARACTER  BUF(MBUF)*128, VCAT__LABEL*50, STR*128, REG*3
      PARAMETER  ( REG = CHAR(32)//CHAR(0)//CHAR(9) )
      PARAMETER  ( VCAT__LABEL = '# VCAT  Configuration file. Version of 2006.02.18 ' )
      INTEGER*4  J1, J2, J3, J4, IND(2,MIND), LIND, NBUF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VCAT_CONF_FILE, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4811, IUER, 'VCAT_GET_CONF', 'Error in an '// &
     &         'attempt to read VCAT configuration file '//VCAT_CONF_FILE )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(VCAT__LABEL)) == VCAT__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR ) 
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4812, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &         'parsing VCAT configuration file '// &
     &          VCAT_CONF_FILE(1:LEN(VCAT_CONF_FILE))//' unrecognized '// &
     &         ' format label at the first line: '//STR(1:64) )
           RETURN 
      END IF
!
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 4813, IUER, 'VCAT_GET_CONF', 'The last and the '// &
     &         'first line of the VCAT configration file '// &
     &          VCAT_CONF_FILE(1:LEN(VCAT_CONF_FILE))//' are not the same' )
           RETURN 
      END IF
!
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         CALL ERR_PASS ( IUER, IER )
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND < 2 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4814, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the VCAT '// &
     &            'configuration file '// &
     &            VCAT_CONF_FILE(1:LEN(VCAT_CONF_FILE))//' "'// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//'" -- to few words' )
              RETURN 
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GVF_DB_DIR:' ) THEN
              GVF_DB_DIR   = BUF(J1)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GVF_ENV_DIR:' ) THEN
              GVF_ENV_DIR   = BUF(J1)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'VTD_CONF_FILE:' ) THEN
              VTD_CONF_SES = BUF(J1)(IND(1,2):IND(2,2)) 
            ELSE 
              CALL ERR_LOG ( 4815, IUER, 'VCAT_GET_CONF', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the VCAT '// &
     &            'configuration file '// &
     &            VCAT_CONF_FILE(1:LEN(VCAT_CONF_FILE))//' "'// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//'" -- unrecognized keyword' )
              RETURN 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VCAT_GET_CONF  !#!#
