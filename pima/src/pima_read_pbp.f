      SUBROUTINE PIMA_READ_PBP ( FILIN, PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_READ_PBP 
! *                                                                      *
! * ### 19-AUG-2010  PIMA_READ_PBP   v2.3 (c) L. Petrov  08-MAY-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  FILIN*(*)
      INTEGER*4  IUER
      LOGICAL*4  LEX
      CHARACTER  STR*128, STR1*128, STA_REF*8, OUT*512, STA_NAM*8, &
     &           PBP_METHOD*8, REG*3
      INTEGER*4  MBUF, MIND
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//':' )
      PARAMETER  ( MBUF = 128*1024 )
      PARAMETER  ( MIND = 32 )
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, NCHN, NFRQ, &
     &           IND(2,MIND), LIND, I_BAS, ICHN_MAX, IFRQ_MAX, &
     &           I_CHN, I_FRQ, I_CHN_2ND, I_FRQ_2ND, L_STA, &
     &           IND_STA_REF, I_STA, L_FRQ(PIM__MSTA), &
     &           L_CHN(PIM__MSTA), BEG_FRQ, END_FRQ, NBUF, PIMA_VERS, IER
      REAL*8     FREQ_VAL, RATE_VAL
      LOGICAL*1  FL_PIMA, FL_AMPL_ONE
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      REAL*4     AMPL_R4, PHAS_R4, AMPL_INTG(PIM__MSTA)
      REAL*8,    EXTERNAL :: PIMA_BPASS_RENRML
      REAL*4,    EXTERNAL :: ATAN_CS_R4 
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
!
      CALL GETENVAR ( 'PIMAVAR_PBP_AMPL_ONE', STR ) 
      IF ( STR == 'YES'  .OR.  STR == 'yes' ) THEN
           FL_AMPL_ONE = .TRUE.
         ELSE 
           FL_AMPL_ONE = .FALSE.
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4371, IUER, 'PIMA_READ_PBP', 'Bandpass file '// &
     &                    FILIN(1:I_LEN(FILIN))//' was not found' )
           RETURN 
      END IF
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 4372, IUER, 'PIMA_READ_PBP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4373, IUER, 'PIMA_READ_PBP', 'Failure to read '// &
     &         'bandpass file '//FILIN )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__POL_BPASS_LABEL)) .EQ. PIMA__POL_BPASS_LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH  ( STR ) 
           CALL TRAN   ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4376, IUER, 'PIMA_READ_PBP', 'Wrong format of '// &
     &         'the input polarization bandpass file '//FILIN(1:I_LEN(FILIN))// &
     &         ' file -- the first line is "'//STR(1:I_LEN(STR))//'" while '// &
     &         PIMA__POL_BPASS_LABEL//' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      ICHN_MAX = 0
      IFRQ_MAX = 0
      IF ( PIM%NSTA == 0 ) THEN
           FL_PIMA = .FALSE.
         ELSE 
           FL_PIMA = .TRUE.
      END IF
!
      PIMA_VERS = 20090909 ! Default pima version for the case if it is not defined in the bandpass file
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         IER = 0
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         CALL CLRCH  ( STR ) 
         CALL INCH   ( J1, STR )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PBP'  .AND. &
     &        BUF(J1)(IND(1,2):IND(2,2)) == 'INFO'        ) THEN
!
              IF ( LIND < 3 ) THEN
                   CALL CLRCH (    STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4377, IUER, 'PIMA_READ_PBP', 'Format '// &
     &                 'violation: line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'bandbass file '//FILIN(1:I_LEN(FILIN))//' has '// &
     &                 'too few words' )
                   RETURN 
              END IF
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'NUMBER_OF_STATIONS' ) THEN
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), L_STA  )
                   PIM%L_STA_PBP = L_STA
!
! ---------------- A special trick for the case when PIM is not initialized
!
                   IF ( .NOT. FL_PIMA ) PIM%NSTA = L_STA
!
                   IF ( ASSOCIATED ( PIM%PBP ) ) THEN
                        DEALLOCATE ( PIM%PBP )
                   END IF
!
                   ALLOCATE ( PIM%PBP(PIM%NSTA), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( PIM%NSTA*SIZEOF(PIM%PBP(1)), STR )
                        CALL ERR_LOG ( 4378, IUER, 'PIMA_READ_PBP', &
     &                      'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                      ' bytes of dynamic memory for PIM%PBP' )
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
                   I_BAS = 0
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
!
                      ALLOCATE ( PIM%PBP(J3)%CMPL(NCHN,NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL IINCH ( 8*NFRQ, STR )
                           CALL ERR_LOG ( 4379, IUER, 'PIMA_READ_PBP', &
     &                         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                         ' bytes of dynamic memory for '// &
     &                         ' PBP()%AMP_RMS_FRQ for station '//PIM%C_STA(J3) )
                          RETURN 
                      END IF
!
! ------------------- Initialization of the bandpass
!
                      PIM%PBP(J3)%CMPL = CMPLX ( 1.0, 0.0 )
 430               CONTINUE 
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'GENERATED_BY_PIMA_VERSION' ) THEN
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), PIMA_VERS )
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
                   PIM%PBP(I_STA)%IND_STA_REF = -1
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'BANDPASS_TYPE' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'NO' ) THEN
                        PIM%PBP(I_STA)%TYP = PIMA__POLARCAL_NO
                      ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == PBP__LL_RR_REF ) THEN
                        PIM%PBP(I_STA)%TYP = PBP__LL_RR_REF 
                      ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == PBP__RR_LL_REF ) THEN
                        PIM%PBP(I_STA)%TYP = PBP__RR_LL_REF 
                      ELSE 
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 4380, IUER, 'PIMA_READ_PBP', &
     &                      'Unsupported polarization type '//BUF(J1)(IND(1,4):IND(2,4))// &
     &                      ' was found while parsing the '//STR(1:I_LEN(STR))// &
     &                      ' -th line of the bandpass polariztion file '// &
     &                      FILIN )
                        RETURN 
                   END IF 
                   PIM%PBP(I_STA)%IND_STA_REF = IND_STA_REF 
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'NUMBER_OF_FREQUENCY_CHANNELS' ) THEN
                   IF ( I_STA .LE. 0 ) GOTO 410
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), L_FRQ(I_STA) )
                   IF ( L_FRQ(I_STA) < 1  .OR. L_FRQ(I_STA)  > PIM__MFRQ ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( PIM__MFRQ, STR )
                        CALL CLRCH (     STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4381, IUER, 'PIMA_READ_PBP', 'Wrong '// &
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
                        CALL ERR_LOG ( 4382, IUER, 'PIMA_READ_PBP', 'Wrong '// &
     &                      'number of spectral channels found in the '// &
     &                      'bandpass file '//FILIN(1:I_LEN(FILIN))//' line '// &
     &                      STR1(1:I_LEN(STR1))//' -- '// &
     &                      BUF(J1)(IND(1,4):IND(2,4))//' -- an integer '// &
     &                      'number in range [1, '//STR(1:I_LEN(STR))//']' )
                        RETURN 
                   END IF
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'METHOD' ) THEN
                   PBP_METHOD = BUF(J1)(IND(1,4):IND(2,4)) 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PBP' .AND. &
     &                BUF(J1)(IND(1,2):IND(2,2)) == 'VAL'             ) THEN
              STA_NAM = BUF(J1)(IND(1,3):IND(2,3))
              I_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
              IF ( I_STA .LE. 0 ) GOTO 410
              PIM%PBP(I_STA)%PIMA_VERS = PIMA_VERS
!
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I5)', IOSTAT=IER ) I_CHN
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4383, IUER, 'PIMA_READ_PBP', 'Error '// &
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
                   CALL ERR_LOG ( 4384, IUER, 'PIMA_READ_PBP', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding frequency '// &
     &                 'index at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F22.10)', IOSTAT=IER ) &
     &               FREQ_VAL
              FREQ_VAL = FREQ_VAL*1.0D6
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (      STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4385, IUER, 'PIMA_READ_PBP', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding frequency '// &
     &                  BUF(J1)(IND(1,6):IND(2,6))//' at line '// &
     &                  STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)), FMT='(F8.5)', IOSTAT=IER ) &
     &               AMPL_R4 
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (      STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4386, IUER, 'PIMA_READ_PBP', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding amplitude '// &
     &                  BUF(J1)(IND(1,8):IND(2,8))//' at line '// &
     &                  STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
              IF ( FL_AMPL_ONE ) AMPL_R4 = 1.0
              IF ( AMPL_R4 < PIMA__BPS_AMP_SPLN_MIN ) AMPL_R4 = PIMA__BPS_AMP_SPLN_MIN 
!
              READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(F8.5)', IOSTAT=IER ) &
     &               PHAS_R4 
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (      STR1 )
                   CALL INCH  ( IER, STR1 )
                   CALL ERR_LOG ( 4387, IUER, 'PIMA_READ_PBP', 'Error '// &
     &                  STR1(1:I_LEN(STR1))//' in decoding phase '// &
     &                  BUF(J1)(IND(1,10):IND(2,10))//' at line '// &
     &                  STR(1:I_LEN(STR))//' of the '// &
     &                 'bandpass file '//FILIN )
                   RETURN 
              END IF
              IF ( FL_PIMA ) THEN
!
! ---------------- Search the frequency among the frequencies known to PIMA
!
                   I_CHN     = 0
                   I_FRQ     = 0
                   I_FRQ_2ND = 0
                   DO 460 J6=1,PIM%NFRQ
                      DO 470 J7=1,PIM%NCHN
                         IF ( DABS( FREQ_VAL - PIM%FREQ_ARR(J7,J6,PIM%CONF%FRQ_GRP) ) < &
       &                      0.5D0*PIM%FRQ(J6,PIM%CONF%FRQ_GRP)%CHAN_WIDTH ) THEN
                              I_CHN = J7
                              IF ( I_FRQ == 0 ) THEN
                                   I_FRQ = J6
                                 ELSE
                                   I_FRQ_2ND = J6
                              END IF
                         END IF
 470                  CONTINUE 
 460               CONTINUE 
                   IF ( I_FRQ .LE. 0  .OR.  I_CHN .LE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        WRITE ( 6, * ) ' FREQ_VAL = ', FREQ_VAL
                        CALL ERR_LOG ( 4388, IUER, 'PIMA_READ_PBP', &
     &                      'Failure in processing line '//STR(1:I_LEN(STR))// &
     &                      ' of the band pass file: the frequency '// &
     &                      'specified is not found among known frequencies' )
                        RETURN 
                   END IF
!
                   IF ( I_FRQ_2ND > 0  .AND. &
     &                  PHAS_R4 == 0.0 .AND. &
     &                  AMPL_R4 == PIMA__BPS_AMP_SPLN_MIN  ) THEN
                      ELSE
                        PIM%PBP(I_STA)%CMPL(I_CHN,I_FRQ) = &
     &                            CMPLX( AMPL_R4*COS(PHAS_R4), AMPL_R4*SIN(PHAS_R4) )
                   END IF
                   PIM%PBP(I_STA)%STATUS = PBP_METHOD
                   IF ( I_FRQ_2ND > 0 ) THEN
                        PIM%PBP(I_STA)%CMPL(I_CHN,I_FRQ_2ND) = &
     &                            CMPLX( AMPL_R4*COS(PHAS_R4), AMPL_R4*SIN(PHAS_R4) )
                   END IF
              END IF
         END IF
 410  CONTINUE 
!
      IF ( .NOT. ASSOCIATED ( PIM%BPASS ) ) THEN
           CALL ERR_LOG ( 4389, IUER, 'PIMA_READ_PBP', &
     &         'Trap of internal control: data strucuture PIM%BPASS '// &
     &         'has not been associated' )
           RETURN 
      END IF
!
      DO 480 J8=1,PIM%NSTA
         ALLOCATE ( PIM%PBP(J8)%SPLT_NRML_FRQ(PIM%NFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%NFRQ, STR )
              CALL ERR_LOG ( 4390, IUER, 'PIMA_READ_PBP', &
     &            'Failure to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for array SPLT_NRML_FRQ' )
              RETURN 
         END IF
         DO 490 J9=1,PIM%NFRQ
            CALL ERR_PASS ( IUER, IER )
            PIM%PBP(J8)%SPLT_NRML_FRQ(J9) = PIMA_BPASS_RENRML ( PIM, &
     &                                           J8, J9, PIMA__POLAR_2ND, PIMA__MASK_FRNG, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4391, IUER, 'PIMA_READ_PBP', &
     &                          'Error in bandpass renormalization' )
            END IF
 490     CONTINUE 
         PIM%PBP(J8)%FINAM = FILIN
 480  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  PIMA_READ_PBP  !#!#
