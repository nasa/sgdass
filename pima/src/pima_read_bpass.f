      SUBROUTINE PIMA_READ_BPASS ( FILIN, PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_READ_BPASS 
! *                                                                      *
! * ### 24-MAY-2006 PIMA_READ_BPASS  v1.8 (c) L. Petrov  11-DEC-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  FILIN*(*)
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  MBUF 
      PARAMETER  ( MBUF = 512*1024 )
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      LOGICAL*4  LEX
      INTEGER*4  NBUF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=FILIN, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4011, IUER, 'PIMA_READ_BPASS', 'Bandpass file '// &
     &                    FILIN(1:I_LEN(FILIN))//' was not found' )
           RETURN 
      END IF
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 4012, IUER, 'PIMA_READ_BPASS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4013, IUER, 'PIMA_READ_BPASS', 'Failure to read '// &
     &         'bandpass file '//FILIN )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__BPASS_STA_LABEL))  .EQ. PIMA__BPASS_STA_LABEL  .OR. &
     &     BUF(1)(1:LEN(PIMA__BPASS_STA_LAB_01)) .EQ. PIMA__BPASS_STA_LAB_01 .OR. &
     &     BUF(1)(1:LEN(PIMA__BPASS_STA_LAB_01)) .EQ. PIMA__BPASS_STA_LAB_02      ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_READ_BPASS_STA ( PIM, NBUF, BUF, FILIN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4015, IUER, 'PIMA_READ_BPASS', 'Failure to '// &
     &              'parse station-dependent bandpass file '//FILIN )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
         ELSE 
           CALL CLRCH  ( STR ) 
           CALL TRAN   ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4016, IUER, 'PIMA_READ_BPASS', 'Wrong format of '// &
     &         'the input bandpass file '//FILIN(1:I_LEN(FILIN))// &
     &         ' file -- the first line is "'//STR(1:I_LEN(STR))//'" while '// &
     &         PIMA__BPASS_STA_LABEL//' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_READ_BPASS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_READ_BPASS_STA ( PIM, NBUF, BUF, FILIN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_READ_BPASS_BAS
! *                                                                      *
! * ## 27-JAN-2009 PIMA_READ_BPASS_STA v2.4 (c) L. Petrov 11-DEC-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  NBUF, IUER
      CHARACTER  BUF(NBUF)*(*), FILIN*(*)
      CHARACTER  STR*128, STR1*128, STR2*128, STA_REF*8, OUT*512, &
     &           STA_NAM*8, BPASS_METHOD*8, REG*3
      INTEGER*4  MBUF, MIND
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//':' )
      PARAMETER  ( MBUF = 128*1024 )
      PARAMETER  ( MIND = 32 )
      INTEGER*2  STA_IND(2)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           NCHN, NFRQ, IND(2,MIND), LIND, ICHN_MAX, &
     &           IFRQ_MAX, I_CHN, I_FRQ, I_CHN_2ND, I_FRQ_2ND, L_STA, &
     &           IND_STA_REF, I_STA, L_FRQ(PIM__MSTA), PIMA_VERS, &
     &           L_CHN(PIM__MSTA), BEG_FRQ, END_FRQ, IND_FRQ, IND_CHN, IER
      REAL*8     FREQ_VAL, RATE_VAL
      CHARACTER  POLAR*8, BPS_VERS*10
      LOGICAL*4  FL_PIMA
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      REAL*4     AMPL_R4, PHAS_R4, AMPL_INTG(PIM__MSTA), BPS_MB_GRDEL(PIM__MSTA), &
     &           PCAL_SB_GRDEL, PCAL_MB_GRDEL
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      REAL*8,    EXTERNAL :: PIMA_BPASS_RENRML, PIMA_BPASS_RENRML_PRE20170303
      REAL*4,    EXTERNAL :: ATAN_CS_R4 
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
!
      ICHN_MAX = 0
      IFRQ_MAX = 0
      IF ( PIM%NSTA == 0 ) THEN
           FL_PIMA = .FALSE.
         ELSE 
           FL_PIMA = .TRUE.
      END IF
!
      BPS_VERS = BUF(1)(31:40)
!
      PIMA_VERS = 20090909 ! Default pima version for the case if it is not defined in the bandpass file
      IND_STA_REF = -1
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         IER = 0
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         CALL CLRCH  ( STR ) 
         CALL INCH   ( J1, STR )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPASS_STA' .AND. &
     &        BUF(J1)(IND(1,2):IND(2,2)) == 'INFO'            ) THEN
!
              IF ( LIND < 3 ) THEN
                   CALL CLRCH (    STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4051, IUER, 'PIMA_READ_BPASS_STA', 'Format '// &
     &                 'violation: line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'bandbass file '//FILIN(1:I_LEN(FILIN))//' has '// &
     &                 'too few words' )
                   RETURN 
              END IF
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'NUMBER_OF_STATIONS' ) THEN
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), L_STA  )
                   PIM%L_BASBPS = (L_STA*(L_STA+1))/2
!
! ---------------- A special trick for the case when PIM is not initialized
!
                   IF ( .NOT. FL_PIMA ) PIM%NSTA = L_STA
!
                   IF ( ASSOCIATED ( PIM%BPASS ) ) THEN
                        DEALLOCATE ( PIM%BPASS )
                   END IF
!
                   ALLOCATE ( PIM%BPASS(PIM%NSTA ), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( PIM%NSTA *SIZEOF(PIM%BPASS(1)), STR )
                        CALL ERR_LOG ( 4052, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                      ' bytes of dynamic memory for PIM%BPASS' )
                        RETURN 
                   END IF
!
                   IF ( .NOT. FL_PIMA ) THEN
                        IFRQ_MAX = 0
                        ICHN_MAX = 0
                        DO 420 J2=J1+1,NBUF
                           CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, IER )
                           IF ( BUF(J2)(IND(1,3):IND(2,3)) == 'PHAS_RATE_WITH_FREQUENCY' ) THEN
                                CALL CHIN ( BUF(J2)(IND(1,4):IND(2,4)), I_FRQ )
                                IF ( I_FRQ > IFRQ_MAX ) IFRQ_MAX = I_FRQ
                              ELSE IF ( BUF(J2)(IND(1,3):IND(2,3)) == 'NUMBER_OF_SPECTRAL_CHANNELS' ) THEN
                                CALL CHIN ( BUF(J2)(IND(1,4):IND(2,4)), I_CHN )
                                IF ( I_CHN > ICHN_MAX ) ICHN_MAX = I_CHN
                           END IF
 420                    CONTINUE 
                   END IF
!
                   DO 430 J3=1,PIM%NSTA
                      IF ( FL_PIMA ) THEN
                           NCHN = PIM%NCHN
                           NFRQ = PIM%NFRQ
                         ELSE 
                           NCHN = ICHN_MAX
                           NFRQ = IFRQ_MAX
                           PIM%NCHN = NCHN 
                           PIM%NFRQ = NFRQ 
                      END IF
                      PIM%BPASS(J3)%L_CHN = NCHN 
                      PIM%BPASS(J3)%L_FRQ = NFRQ
                      PIM%BPASS(J3)%STA_NAM = PIM%C_STA(J3)
                      PIM%BPASS(J3)%PIMA_VERS = PIMA_VERS
!
                      ALLOCATE ( PIM%BPASS(J3)%AMP_RMS_FRQ(NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL IINCH ( 8*NFRQ, STR )
                           CALL ERR_LOG ( 4053, IUER, 'PIMA_READ_BPASS_STA', &
     &                         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                         ' bytes of dynamic memory for '// &
     &                         ' BPASS()%AMP_RMS_FRQ for station '//PIM%C_STA(J3) )
                          RETURN 
                      END IF
!
                      ALLOCATE ( PIM%BPASS(J3)%PHS_RMS_FRQ(NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL IINCH ( 8*NFRQ, STR )
                           CALL ERR_LOG ( 4054, IUER, 'PIMA_READ_BPASS_STA', &
     &                         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                         ' bytes of dynamic memory for '// &
     &                         ' BPASS()%PHS_RMS_FRQ for station '//PIM%C_STA(J3) )
                          RETURN 
                      END IF
!
                      ALLOCATE ( PIM%BPASS(J3)%FREQ(NCHN,NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL IINCH ( 8*NFRQ, STR )
                           CALL ERR_LOG ( 4055, IUER, 'PIMA_READ_BPASS_STA', &
     &                         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                         ' bytes of dynamic memory for '// &
     &                         ' BPASS()%FREQ for station '//PIM%C_STA(J3) )
                          RETURN 
                      END IF
!
                      ALLOCATE ( PIM%BPASS(J3)%BPS(NCHN,NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL IINCH ( 8*NCHN*NFRQ, STR )
                           CALL ERR_LOG ( 4056, IUER, 'PIMA_READ_BPASS_STA', &
     &                         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                         ' bytes of dynamic memory for '// &
     &                         ' BPASS()%BPS for station '//PIM%C_STA(J3) )
                          RETURN 
                      END IF
!
! ------------------- Initialization of the bandpass
!
                      PIM%BPASS(J3)%BPS = CMPLX ( 1.0, 0.0 )
!
                      ALLOCATE ( PIM%BPASS(J3)%PHS_RATE(NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL IINCH ( 4*NFRQ, STR )
                           CALL ERR_LOG ( 4057, IUER, 'PIMA_READ_BPASS_STA', &
     &                         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                         ' bytes of dynamic memory for '// &
     &                         ' BPASS()%PHS_DTE for station '//PIM%C_STA(J3) )
                          RETURN 
                      END IF
!
                      PIM%BPASS(J3)%PCAL_MB_GRDEL = 0.0D0
                      PIM%BPASS(J3)%PCAL_SB_GRDEL = 0.0D0
 430               CONTINUE 
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'GENERATED_BY_PIMA_VERSION' ) THEN
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), PIMA_VERS )
                   IF ( PIMA_VERS < 20090909 ) THEN
                        CALL ERR_LOG ( 4058, IUER, 'PIMA_READ_BPASS_STA', 'Wrong '// &
     &                      'PIMA version is specified in the '// &
     &                      'bandpass file '//FILIN(1:I_LEN(FILIN))//' line '// &
     &                       STR1(1:I_LEN(STR1))//' -- '//BUF(J1)(IND(1,4):IND(2,4))// &
     &                      ' shile a date in YYYYMMDD format after 20090909 was expected'  )
                        RETURN 
                   END IF
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'REFERENCE_STATION' ) THEN
                   STA_REF = BUF(J1)(IND(1,4):IND(2,4))
                   IF ( .NOT. FL_PIMA ) THEN
                        PIM%NSTA = 0
                        IND_STA_REF = ADD_CLIST ( PIM__MSTA, PIM%NSTA, PIM%C_STA, STA_REF, -2 ) 
                      ELSE 
                        IND_STA_REF = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_REF )
                   END IF
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'STATION' ) THEN
                   STA_NAM = BUF(J1)(IND(1,4):IND(2,4))
                   IF ( FL_PIMA ) THEN
                        I_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
                      ELSE 
                        I_STA = ADD_CLIST ( PIM__MSTA, PIM%NSTA, PIM%C_STA, STA_NAM, -2 ) 
                   END IF
                   IF ( I_STA .LE. 0 ) GOTO 410
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'NUMBER_OF_FREQUENCY_CHANNELS' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), L_FRQ(I_STA) )
                   IF ( L_FRQ(I_STA) < 1  .OR. L_FRQ(I_STA)  > PIM__MFRQ ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( PIM__MFRQ, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4059, IUER, 'PIMA_READ_BPASS_STA', 'Wrong '// &
     &                      'number of frequency channels found in the '// &
     &                      'bandpass file '//FILIN(1:I_LEN(FILIN))//' line '// &
     &                      STR1(1:I_LEN(STR1))//' -- '// &
     &                      BUF(J1)(IND(1,3):IND(2,3))//' -- an integer '// &
     &                      'number in range [1, '//STR(1:I_LEN(STR))//']' )
                        RETURN 
                   END IF
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'NUMBER_OF_SPECTRAL_CHANNELS' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), L_CHN(I_STA) )
                   IF ( L_CHN(I_STA) < 1  .OR. L_CHN(I_STA) > PIM__MCHN ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( PIM__MCHN, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4060, IUER, 'PIMA_READ_BPASS_STA', 'Wrong '// &
     &                      'number of spectral channels found in the '// &
     &                      'bandpass file '//FILIN(1:I_LEN(FILIN))//' line '// &
     &                      STR1(1:I_LEN(STR1))//' -- '// &
     &                      BUF(J1)(IND(1,4):IND(2,4))//' -- an integer '// &
     &                      'number in range [1, '//STR(1:I_LEN(STR))//']' )
                        RETURN 
                   END IF
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'NUMBER_OF_USED_OBSERVATIONS' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), PIM%BPASS(I_STA)%L_OBS )
                   IF ( PIM%BPASS(I_STA)%L_OBS < 0  .OR.  PIM%BPASS(I_STA)%L_OBS  > PIM__MOBS ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( PIM__MOBS, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4061, IUER, 'PIMA_READ_BPASS_STA', 'Wrong '// &
     &                      'number of used observations found in the '// &
     &                      'bandpass file '//FILIN(1:I_LEN(FILIN))//' line '// &
     &                      STR1(1:I_LEN(STR1))//' -- '// &
     &                      BUF(J1)(IND(1,4):IND(2,4))//' -- an integer '// &
     &                      'number in range [1, '//STR(1:I_LEN(STR))//']' )
                        RETURN 
                   END IF
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'INTEGRAL_FREQ_CHAN_AMPLITUDE' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(E22.10)', IOSTAT=IER ) &
     &                    AMPL_INTG(I_STA)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4062, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
                   PIM%BPASS(I_STA)%INTG_AMPL = AMPL_INTG(I_STA)
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'BANDPASS_GROUP_DELAY' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(E22.10)', IOSTAT=IER ) &
     &                    BPS_MB_GRDEL(I_STA)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4063, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
                   PIM%BPASS(I_STA)%BPS_MB_GRDEL = BPS_MB_GRDEL(I_STA)
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'PCAL_MB_GROUP_DELAY' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(E22.10)', IOSTAT=IER ) &
     &                    PCAL_MB_GRDEL
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4064, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
                   IF ( IS_R4_NAN ( PCAL_MB_GRDEL ) ) THEN
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4064, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error happened in decoding file '// &
     &                       TRIM(FILIN)//' line '//TRIM(STR1)//' -- '//BUF(J1)  )
                        RETURN 
                   END IF
                   PIM%BPASS(I_STA)%PCAL_MB_GRDEL = PCAL_MB_GRDEL
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'PHAS_RATE_WITH_FREQUENCY' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I8)', IOSTAT=IER ) &
     &                    I_FRQ
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4065, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
!
                   READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F13.6)', IOSTAT=IER ) &
     &                    FREQ_VAL
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )

                        CALL ERR_LOG ( 4066, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
                   FREQ_VAL = FREQ_VAL*1.D6 
!                 
                   READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)), FMT='(F13.6)', IOSTAT=IER ) &
     &                    RATE_VAL
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4067, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
                   IF ( FL_PIMA ) THEN
!
! --------------------- Search the frequency among the frequencies known to PIMA
!
                        I_FRQ     = 0
                        I_FRQ_2ND = 0
                        DO 450 J5=1,PIM%NFRQ
                           IF ( DABS( FREQ_VAL - PIM%FREQ_ARR(1,J5,PIM%CONF%FRQ_GRP) ) < &
     &                          0.5D0*PIM%FRQ(J5,PIM%CONF%FRQ_GRP)%CHAN_WIDTH ) THEN
                                IF ( I_FRQ == 0 ) THEN
                                     I_FRQ = J5
                                   ELSE 
                                     I_FRQ_2ND = J5
                                END IF
                           END IF
 450                    CONTINUE 
                        IF ( I_FRQ .LE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             WRITE ( 6, * ) ' J5= ', J5, ' FREQ_VAL= ', FREQ_VAL
!!                             WRITE ( 6, * ) ' FREQ_ARR_ALL: ', PIM%FREQ_ARR(1:PIM%NCHN,1:PIM%NFRQ,PIM%CONF%FRQ_GRP) 
                             WRITE ( 6, * ) ' FREQ_ARR: ', PIM%FREQ_ARR(1,1:PIM%NFRQ,PIM%CONF%FRQ_GRP) 
                             CALL ERR_LOG ( 4068, IUER, 'PIMA_READ_BPASS_STA', &
     &                           'Failure in processing line '//STR(1:I_LEN(STR))// &
     &                           ' of the band pass file '//FILIN(1:I_LEN(FILIN))// &
     &                           ': the frequency specified is not found among '// &
     &                           'known frequencies: '//BUF(J1) )
                             RETURN 
                        END IF
                   END IF
!
                   PIM%BPASS(I_STA)%PHS_RATE(I_FRQ) = RATE_VAL
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'PCAL_SB_GROUP_DELAY' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I8)', IOSTAT=IER ) &
     &                    I_FRQ
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4069, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
!
                   READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F13.6)', IOSTAT=IER ) &
     &                    FREQ_VAL
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )

                        CALL ERR_LOG ( 4070, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
                   FREQ_VAL = FREQ_VAL*1.D6 
!                 
                   READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)), FMT='(F13.6)', IOSTAT=IER ) &
     &                    PCAL_SB_GRDEL
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (      STR )
                        CALL INCH  ( IER, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4071, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Error '//STR(1:I_LEN(STR))//' happened in '// &
     &                      'decoding file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' line '//STR1 )
                        RETURN 
                   END IF
                   IF ( FL_PIMA ) THEN
!
! --------------------- Search the frequency among the frequencies known to PIMA
!
                        I_FRQ     = 0
                        I_FRQ_2ND = 0
                        DO 460 J6=1,PIM%NFRQ
                           IF ( DABS( FREQ_VAL - PIM%FREQ_ARR(1,J6,PIM%CONF%FRQ_GRP) ) < &
     &                          0.5D0*PIM%FRQ(J6,PIM%CONF%FRQ_GRP)%CHAN_WIDTH ) THEN
                                IF ( I_FRQ == 0 ) THEN
                                     I_FRQ = J6
                                   ELSE 
                                     I_FRQ_2ND = J6
                                END IF
                           END IF
 460                    CONTINUE 
                        IF ( I_FRQ .LE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             WRITE ( 6, * ) ' J6= ', J6, ' FREQ_VAL= ', FREQ_VAL
!!                             WRITE ( 6, * ) ' FREQ_ARR_ALL: ', PIM%FREQ_ARR(1:PIM%NCHN,1:PIM%NFRQ,PIM%CONF%FRQ_GRP) 
                             WRITE ( 6, * ) ' FREQ_ARR: ', PIM%FREQ_ARR(1,1:PIM%NFRQ,PIM%CONF%FRQ_GRP) 
                             CALL ERR_LOG ( 4072, IUER, 'PIMA_READ_BPASS_STA', &
     &                           'Failure in processing line '//STR(1:I_LEN(STR))// &
     &                           ' of the band pass file '//FILIN(1:I_LEN(FILIN))// &
     &                           ': the frequency specified is not found among '// &
     &                           'known frequencies: '//BUF(J1) )
                             RETURN 
                        END IF
                   END IF
!
                   PIM%BPASS(I_STA)%PCAL_SB_GRDEL(I_FRQ) = PCAL_SB_GRDEL
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'METHOD' ) THEN
                   BPASS_METHOD = BUF(J1)(IND(1,4):IND(2,4)) 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPASS_STA' .AND. &
     &                BUF(J1)(IND(1,2):IND(2,2)) == 'VAL'             ) THEN
              STA_NAM = BUF(J1)(IND(1,3):IND(2,3))
              I_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
              IF ( I_STA .LE. 0 ) GOTO 410
!
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I5)', IOSTAT=IER ) I_CHN
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4073, IUER, 'PIMA_READ_BPASS_STA', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding channel '// &
     &                 'index at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(I5)', IOSTAT=IER ) I_FRQ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (      STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4074, IUER, 'PIMA_READ_BPASS_STA', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding frequency '// &
     &                 'index at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F13.6)', IOSTAT=IER ) &
     &               FREQ_VAL
              FREQ_VAL = FREQ_VAL*1.0D6
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (      STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4075, IUER, 'PIMA_READ_BPASS_STA', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding frequency '// &
     &                  BUF(J1)(IND(1,6):IND(2,6))//' at line '// &
     &                  STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)), FMT='(F7.5)', IOSTAT=IER ) &
     &               AMPL_R4 
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (      STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4076, IUER, 'PIMA_READ_BPASS_STA', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding amplitude '// &
     &                  BUF(J1)(IND(1,8):IND(2,8))//' at line '// &
     &                  STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
              IF ( AMPL_R4 < PIMA__BPS_AMP_SPLN_MIN ) AMPL_R4 = PIMA__BPS_AMP_SPLN_MIN
!
              READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(F8.5)', IOSTAT=IER ) &
     &               PHAS_R4 
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (      STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4077, IUER, 'PIMA_READ_BPASS_STA', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding phase '// &
     &                  BUF(J1)(IND(1,10):IND(2,10))//' at line '// &
     &                  STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
              IF ( FL_PIMA ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I6)' ) IND_CHN
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(I6)' ) IND_FRQ
                   IF ( IND_CHN < 1 .OR. IND_CHN > PIM%NCHN ) THEN
                        CALL CLRCH (           STR  )
                        CALL INCH  ( J1,       STR  )
                        CALL CLRCH (           STR1  )
                        CALL INCH  ( IND_CHN,  STR1  )
                        CALL CLRCH (           STR2 )
                        CALL INCH  ( PIM%NCHN, STR2 )
                        CALL ERR_LOG ( 4078, IUER, 'PIMA_READ_BPASS_STA', 'Error '// &
     &                      'in decoding the '//STR(1:I_LEN(STR))//' the line '// &
     &                      'of the bandpass file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- wring channel index '//BUF(J1)(IND(1,4):IND(2,4))// &
     &                      '. Should be in range [1, '//STR2(1:I_LEN(STR2))//']' )
                        RETURN 
                   END IF
                   IF ( IND_FRQ < 1 .OR. IND_FRQ > PIM%NFRQ ) THEN
                        CALL CLRCH (           STR  )
                        CALL INCH  ( J1,       STR  )
                        CALL CLRCH (           STR1  )
                        CALL INCH  ( IND_FRQ,  STR1  )
                        CALL CLRCH (           STR2 )
                        CALL INCH  ( PIM%NFRQ, STR2 )
                        CALL ERR_LOG ( 4079, IUER, 'PIMA_READ_BPASS_STA', 'Error '// &
     &                      'in decoding the '//STR(1:I_LEN(STR))//' the line '// &
     &                      'of the bandpass file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- wring frequency index '//BUF(J1)(IND(1,5):IND(2,5))// &
     &                      '. Should be in range [1, '//STR2(1:I_LEN(STR2))//']' )
                        RETURN 
                   END IF
                   IF ( DABS( FREQ_VAL - PIM%FREQ_ARR(IND_CHN,IND_FRQ,PIM%CONF%FRQ_GRP) ) < &
       &                      0.5D0*PIM%FRQ(IND_FRQ,PIM%CONF%FRQ_GRP)%CHAN_WIDTH ) THEN
                        I_CHN = IND_CHN
                        I_FRQ = IND_FRQ
                     ELSE
!
! --------------------- Search the frequency among the frequencies known to PIMA.
! --------------------- This is an old slow algorithm of the last resort
!
                        I_CHN     = 0
                        I_FRQ     = 0
                        I_FRQ_2ND = 0
                        I_CHN_2ND = 0
                        DO 470 J7=1,PIM%NFRQ
                           DO 480 J8=1,PIM%NCHN
                              IF ( DABS( FREQ_VAL - PIM%FREQ_ARR(J8,J7,PIM%CONF%FRQ_GRP) ) < &
       &                           0.5D0*PIM%FRQ(J7,PIM%CONF%FRQ_GRP)%CHAN_WIDTH ) THEN
                                   IF ( I_CHN == 0 ) THEN
                                        I_CHN = J8
                                      ELSE
                                        I_CHN_2ND = J8
                                   END IF
                                   IF ( I_FRQ == 0 ) THEN
                                        I_FRQ = J7
                                      ELSE
                                        I_FRQ_2ND = J7
                                   END IF
                              END IF
 480                       CONTINUE 
 470                    CONTINUE 
                   END IF
                   IF ( I_FRQ .LE. 0  .OR.  I_CHN .LE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        WRITE ( 6, * ) ' FREQ_VAL = ', FREQ_VAL
                        CALL ERR_LOG ( 4080, IUER, 'PIMA_READ_BPASS_STA', &
     &                      'Failure in processing line '//STR(1:I_LEN(STR))// &
     &                      ' of the band pass file: the frequency '// &
     &                      'specified is not found among known frequencies' )
                        RETURN 
                   END IF
!
                   PIM%BPASS(I_STA)%STA_NAM = PIM%C_STA(I_STA)
                   IF ( I_FRQ_2ND > 0  .AND. &
     &                  PHAS_R4 == 0.0 .AND. &
     &                  AMPL_R4 == PIMA__BPS_AMP_SPLN_MIN ) THEN
                        CONTINUE 
                      ELSE
                        PIM%BPASS(I_STA)%BPS(I_CHN,I_FRQ) = &
     &                            CMPLX( AMPL_R4*COS(PHAS_R4), AMPL_R4*SIN(PHAS_R4) )
                   END IF
                   PIM%BPASS(I_STA)%FREQ(I_CHN,I_FRQ) = FREQ_VAL 
                   PIM%BPASS(I_STA)%STATUS = BPASS_METHOD
                   IF ( I_FRQ_2ND > 0 ) THEN
                        PIM%BPASS(I_STA)%BPS(I_CHN,I_FRQ_2ND) = &
     &                            CMPLX( AMPL_R4*COS(PHAS_R4), AMPL_R4*SIN(PHAS_R4) )
                        PIM%BPASS(I_STA)%FREQ(I_CHN,I_FRQ_2ND) = FREQ_VAL 
                   END IF
                   PIM%BPASS(I_STA)%IND_STA_REF = IND_STA_REF
              END IF
         END IF
 410  CONTINUE 
!
      IF ( PIM%CONF%BEG_FRQ == 0 ) THEN
           BEG_FRQ = 1
         ELSE 
           BEG_FRQ = PIM%CONF%BEG_FRQ 
      END IF
!
      IF ( PIM%CONF%BEG_FRQ == 0 ) THEN
           END_FRQ = IFRQ_MAX 
         ELSE 
           END_FRQ = PIM%CONF%END_FRQ  
      END IF
!
      DO 490 J9=1,PIM%NSTA
         IF ( PIM%CONF%SPLT_BPASS_NRML_METHOD .EQ. PIMA__WEIGHTED ) THEN
              ALLOCATE ( PIM%BPASS(J9)%SPLT_NRML_FRQ(PIM%NFRQ), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%NFRQ, STR )
                   CALL ERR_LOG ( 4081, IUER, 'PIMA_READ_BPASS_STA', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPLT_NRML_FRQ' )
                   RETURN 
              END IF
!            
              DO 4100 J10=1,PIM%NFRQ
                 STA_IND(1) = J9
                 STA_IND(2) = 0
                 IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      PIM%BPASS(J9)%SPLT_NRML_FRQ(J10) = PIMA_BPASS_RENRML ( PIM, &
     &                                   J9, J10, PIMA__POLAR_1ST, PIMA__MASK_FRNG, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4082, IUER, 'PIMA_READ_BPASS_STA', &
     &                         'Error in bandpass renormalization. Are you sure '// &
     &                         'you are using a correct bandpass file?' )
                           RETURN 
                      END IF
                    ELSE
                      PIM%BPASS(J9)%SPLT_NRML_FRQ(J10) = PIMA_BPASS_RENRML_PRE20170303 ( PIM, &
     &                                   J10, STA_IND, PIMA__MASK_FRNG )
                 END IF
 4100         CONTINUE 
         END IF
         IF ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
     &        PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
!
! ----------- Copy group delays in phase calibration from bnandpass object to 
! ----------- the phase calibration object
!
              PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_MB_GRDEL = PIM%BPASS(J9)%PCAL_MB_GRDEL
              DO 4110 J11=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SB_GRDEL(J11) = PIM%BPASS(J9)%PCAL_SB_GRDEL(J11)
 4110         CONTINUE 
              PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_GRDEL_STATUS = PIMA__LOADED
         END IF
         PIM%BPASS(J9)%FINAM = FILIN
 490  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  PIMA_READ_BPASS_STA  !#!#
