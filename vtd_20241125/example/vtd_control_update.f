      PROGRAM    VTD_CONTROL_UPDATE
! ************************************************************************
! *                                                                      *
! *   PRogram VTD_CONTROL_UPDATE
! *                                                                      *
! * ## 19-AUG-2014  VTD_CONTROL_UPDATE v1.0 (c) L. Petrov 19-AUG-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4    MBUF
      PARAMETER  ( MBUF = 2048 )
      CHARACTER  CONF_FILE*128, FILOUT*128
      CHARACTER  BUF(MBUF)*256, OUT(MBUF)*256, WMF_VAL*128, WZD_VAL*128, &
     &           SPD_VAL*128
      INTEGER*4    MIND
      PARAMETER  ( MIND = 128 )
      CHARACTER  REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  J1, J2, J3, J4, LIND, IND(2,MIND), NB, NOUT, IL, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: vtd_control_update  input_vtd_control_file' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, CONF_FILE ) 
      END IF
      FILOUT = CONF_FILE(1:I_LEN(CONF_FILE))//'.new'
!
!
      CALL RD_TEXT  ( CONF_FILE, MBUF, BUF, NB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2001, IUER, 'VTD_CONTROL_UPDATE', 'Failure in '// &
     &         'reading configuration file '//CONF_FILE )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,NB
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'WET_MAPPING_FUNCTION:' ) THEN
              WMF_VAL = BUF(J1)(IND(1,2):IND(2,2)) 
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'WET_ZENITH_DELAY:' ) THEN
              WZD_VAL = BUF(J1)(IND(1,2):IND(2,2)) 
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SLANTED_PATH_DELAY_MODEL:' ) THEN
              SPD_VAL = BUF(J1)(IND(1,2):IND(2,2)) 
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SLANT_PATH_DELAY_MODEL:' ) THEN
              SPD_VAL = BUF(J1)(IND(1,2):IND(2,2)) 
         END IF
 410  CONTINUE 
!
      NOUT = 0
      IF ( BUF(1)(1:50) == '# VTD Control file.   Format version of 2010.05.18' ) THEN
           DO 420 J2=1,NB
              CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, -2 )
              IF ( J2 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = VTD_CNF__LABEL
                   GOTO 420
                ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'WET_MAPPING_FUNCTION:' ) THEN
                   IF ( WZD_VAL == 'NONE' ) THEN
                        BUF(J2)(IND(1,2):) = 'NONE'
                   END IF
                ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'SLANTED_PATH_DELAY_MODEL:' .OR.  &
     &                    BUF(J2)(IND(1,1):IND(2,1)) == 'SLANT_PATH_DELAY_MODEL:'         ) THEN
                   IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'SLANTED_PATH_DELAY_MODEL:' ) THEN
                        BUF(J2)(IND(1,1):IND(2,1)) = 'SLANT_PATH_DELAY_MODEL:  ' 
                   END IF
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J2)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ATMOSPHERE_PATH_DELAY_PARTIAL:  '//WMF_VAL
                   GOTO 420
                ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'SLANTED_PATH_DELAY_BIAS_FILE:' ) THEN
                   BUF(J2)(IND(1,1):IND(2,1)) = 'SLANT_PATH_DELAY_BIAS_FILE:  ' 
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J2)
 420       CONTINUE 
         ELSE 
           WRITE ( 6, * ) 'Label >>'//BUF(1)(1:I_LEN(BUF(1)))//'<<  '
           IUER  = -1
           CALL ERR_LOG ( 2002, IUER, 'VTD_CONTROL_UPDATE', 'Label of the '// &
     &         'configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' is not supported' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL WR_TEXT ( NOUT, OUT, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER  = -1
           CALL ERR_LOG ( 2003, IUER, 'VTD_CONTROL_UPDATE', 'Failure in '// &
     &         'writing new configuration file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, '(A)' ) ' '
      WRITE ( 6, '(A)' ) 'Upgraded VTD configuration file is written in '
      WRITE ( 6, '(A)' ) '   '//FILOUT(1:I_LEN(FILOUT))
!
      END  PROGRAM  VTD_CONTROL_UPDATE  !#!#
