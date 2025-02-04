      PROGRAM    SPD_CLI_EXAMPLE
! ************************************************************************
! *                                                                      *
! *   Program  SPD_CLI_EXAMPLE
! *                                                                      *
! * ### 09-JAN-2015 SPD_CLI_EXAMPLE v1.0 (c)  L. Petrov  09-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      CHARACTER  SPD_CLI_CONF*128, MAP_FILE*128
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      TYPE     ( SPD_2P__TYPE ), POINTER :: SPD_2P(:)
      INTEGER*4  M_BUF
      PARAMETER  ( M_BUF = 2*1024*1024 )
      REAL*8     DIST, TAI_BEG, TAI_END
      INTEGER*8  MEL
      CHARACTER  VERB*8, BUF(M_BUF)*256, STR*128, FILOUT*128
      INTEGER*4  J1, J2, J3, NF, NB, IFMT, ISTL, N_DEL, MJD_BEG, MJD_END, &
     &           IVRB, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: spd_client example ping|shutdown|get_2pd|send'
           CALL EXIT ( 1 )
      END IF 
      CALL GETARG ( 1, VERB )
      IVRB = 1
!
      SPD_CLI_CONF = SPD__SHARE//'/spd_cli_example.cnf'
      MAP_FILE     = SPD__SHARE//'/mab_example.txt'
      IF ( IARGC() .GE. 2 ) THEN
           CALL GETARG ( 2, MAP_FILE )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( MAP_FILE, M_BUF, BUF, NB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4401, IUER, 'PD_2POINT', 'Error in an attempt '// &
     &         'to read input file '//MAP_FILE )
           CALL EXIT ( 0 )
      END IF
!
      IF ( BUF(1)(1:60) == SPD__RESP_LABEL ) THEN
           IFMT = 1
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 4402, IUER, 'PD_2POINT', 'Unsupported type of '// &
     &         'the input file '//MAP_FILE(1:I_LEN(MAP_FILE))//' . Expected '// &
     &         'format label has not been found in the first line' )
           CALL EXIT ( 1 )
      END IF
!
      MJD_BEG = 0
      N_DEL = 0
      DO 410 J1=1,NB
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( MJD_BEG == 0 ) THEN
              READ ( UNIT=BUF(J1)(11:15), FMT='(I5)'   ) MJD_BEG
              READ ( UNIT=BUF(J1)(17:23), FMT='(F7.1)' ) TAI_BEG
         END IF 
         READ ( UNIT=BUF(J1)(11:15), FMT='(I5)'   ) MJD_END
         READ ( UNIT=BUF(J1)(17:23), FMT='(F7.1)' ) TAI_END
         N_DEL = N_DEL + 1
 410  CONTINUE 
!
      ALLOCATE ( SPD_2P(N_DEL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           MEL = N_DEL*SIZEOF(SPD_2P(1))
           CALL IINCH8 ( MEL, STR )
           IUER = -2
           CALL ERR_LOG ( 4403, IUER, 'PD_2POINT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array SPD_2P' )
           CALL EXIT ( 1 )
      END IF
!
      N_DEL = 0
      DO 420 J2=1,NB
         IF ( BUF(J2)(1:1)  == '#' ) GOTO 420
         IF ( ILEN(BUF(J2)) ==  0  ) GOTO 420
         IF ( N_DEL == 0 ) NF = J2
         N_DEL = N_DEL + 1
         READ ( UNIT=BUF(J2)(11:15),   FMT='(I5)'    ) SPD_2P(N_DEL)%MJD
         READ ( UNIT=BUF(J2)(17:23),   FMT='(F7.1)'  ) SPD_2P(N_DEL)%TAI
         READ ( UNIT=BUF(J2)(26:38),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_EMI(1)
         READ ( UNIT=BUF(J2)(40:52),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_EMI(2)
         READ ( UNIT=BUF(J2)(54:66),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_EMI(3)
         READ ( UNIT=BUF(J2)(69:81),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_REC(1)
         READ ( UNIT=BUF(J2)(83:95),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_REC(2)
         READ ( UNIT=BUF(J2)(97:109),  FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_REC(3)
         READ ( UNIT=BUF(J2)(112:117), FMT='(F6.4)'  ) SPD_2P(N_DEL)%DEL(1)
         READ ( UNIT=BUF(J2)(120:125), FMT='(F6.4)'  ) SPD_2P(N_DEL)%DEL(2)
         READ ( UNIT=BUF(J2)(128:138), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_RDER(1)
         READ ( UNIT=BUF(J2)(140:150), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_RDER(2)
         READ ( UNIT=BUF(J2)(152:162), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_EDER(1)
         READ ( UNIT=BUF(J2)(164:174), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_EDER(2)
!
         DIST= DSQRT ( (SPD_2P(N_DEL)%COO_EMI(1) - SPD_2P(N_DEL)%COO_REC(1))**2 + &
     &                 (SPD_2P(N_DEL)%COO_EMI(2) - SPD_2P(N_DEL)%COO_REC(2))**2 + &
     &                 (SPD_2P(N_DEL)%COO_EMI(3) - SPD_2P(N_DEL)%COO_REC(3))**2   )
         IF ( DIST > 1.5*SPD__U_MAX ) THEN
              ISTL = SPD__SAT
            ELSE 
              ISTL = SPD__2P
         END IF
 420  CONTINUE 
!
      IUER = -1
      CALL SPD_CLI_INIT ( SPD_CLI_CONF, SPD_CLI, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT (  1 )
!
      IF ( VERB == 'ping' ) THEN
           CALL SPD_CLI_PING ( SPD_CLI, IVRB, IUER )
         ELSE IF ( VERB == 'shutdown' ) THEN
           CALL SPD_CLI_SHUTDOWN ( SPD_CLI, IUER )
         ELSE IF ( VERB == 'get_2pd ' ) THEN
           CALL SPD_CLI_GET_2PD ( SPD_CLI, N_DEL, SPD_2P, IVRB, IUER )
!           WRITE ( 6, * ) 'SPD_2P(N_DEL)%DEL = ', SPD_2P(N_DEL)%DEL
!           WRITE ( 6, * ) 'SPD_2P(N_DEL)%DEL = ', SPD_2P(N_DEL)%DEL_RDER
!           WRITE ( 6, * ) 'SPD_2P(N_DEL)%DEL = ', SPD_2P(N_DEL)%DEL_EDER
           DO 430 J3=1,N_DEL
              WRITE ( BUF(J3+NF-1), 110 ) J3, SPD_2P(J3)%MJD, SPD_2P(J3)%TAI, &
     &                              SPD_2P(J3)%COO_EMI, SPD_2P(J3)%COO_REC, &
     &                              SPD_2P(J3)%DEL, SPD_2P(J3)%DEL_RDER, &
     &                              SPD_2P(J3)%DEL_EDER
 110          FORMAT ( I9, 1X, I5, 1X, F7.1, 1X, 3(1X,F13.4), 1X,3(1X,F13.4), 2(2X,F6.4), &
     &                 1X,2(1X,1PD11.4), 1X,2(1X,1PD11.4) )
 430       CONTINUE 
           FILOUT = MAP_FILE(1:I_LEN(MAP_FILE))//'.out'
           IUER = -1
           CALL WR_TEXT ( NB, BUF, FILOUT, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, '(A)' ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
         ELSE 
           IUER =-1
           CALL ERR_LOG ( 9000, IUER, 'SPD_CLI_EXAMPLE', 'Unknown '// &
     &          'verb '//VERB )
           CALL EXIT ( 1 )
      END IF
      IF ( IUER .NE. 0 ) WRITE ( 6, * ) 'IUER= ', IUER 
      END  !#!  
