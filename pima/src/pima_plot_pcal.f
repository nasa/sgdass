      SUBROUTINE PIMA_PLOT_PCAL ( PIM, PCAL_TYPE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PLOT_PCAL 
! *                                                                      *
! * ### 07-AUG-2006  PIMA_PLOT_PCAL  v3.9 (c)  L. Petrov 14-DEC-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  PCAL_TYPE, IUER
      INTEGER*4    MPB, MP, NN
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      TYPE     ( DIAGI_STRU ) :: DIA(PIM__MFRQ)
      REAL*8,    ALLOCATABLE :: T8(:,:,:), X8(:,:,:), Y8(:,:)
      REAL*8     FRQ_DIF, FREQS(PIM__MTON,PIM__MFRQ), PHS_DIF
      CHARACTER  COMMON_TIT*80, TITS(PIM__MFRQ)*32, BUTTON_NAME(MPB)*32, &
     &           BUTTON_LET(MPB)*2, PREF_NAME*128, STR*32, STR1*32, &
     &           MES_SELSTA*80, STA_STR*8
      INTEGER*4  MODE, ISTA, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ, &
     &           J1, J2, J3, J4, J5, J6, NP, IAMB, IND_POL, IND_PLT, &
     &           M_TONES, IND_TONE, N_TONES, I_TONE, IND(2,32), LIND, &
     &           IND_SCA, IER 
      REAL*4     TIME_VAL, AMPL_VAL, PHAS_VAL
      CHARACTER  ZAG*128, UNIT*128
      LOGICAL*4  FL_STA, FL_PCAL_AMBIG 
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, &
     &           ICL1, ICL2, ICL3, IND_FREQ, K1, K2
      LOGICAL*4, EXTERNAL :: PIMA_SELSTA
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_PCAL  !#!  
#else
!
      FL_PCAL_AMBIG = .FALSE.
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_PCAL_AMBIG_RESOLVE', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:3) == 'YES' ) FL_PCAL_AMBIG = .TRUE.
      IF ( STR(1:2) == 'NO'  ) FL_PCAL_AMBIG = .FALSE.
!
      IND_FREQ = PIM%CONF%BEG_FRQ 
!
      ALLOCATE ( T8(MP,MAX(1,PIM%NPCT),PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           write ( 6, * ) 'mp, PIM%NPCT, pim%nfrq= ', mp, PIM%NPCT, pim%nfrq ! %%%%
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(MP)*INT8(PIM%NPCT)*INT8(PIM%NFRQ)*INT8(8), STR )
           CALL ERR_LOG ( 5711, IUER, 'PIMA_PLOT_PCAL', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array T8' )
           RETURN 
      END IF 
!
      ALLOCATE ( X8(MP,MAX(1,PIM%NPCT),PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*8, STR )
           CALL ERR_LOG ( 5712, IUER, 'PIMA_PLOT_PCAL', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array X8' )
           RETURN 
      END IF 
!
      ALLOCATE ( Y8(MP,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*PIM%NFRQ*8, STR )
           CALL ERR_LOG ( 5713, IUER, 'PIMA_PLOT_PCAL', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array Y8' )
           RETURN 
      END IF 
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5714, IUER, 'PIMA_PLOT_PCAL', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
      MES_SELSTA = 'Select a station for plots of phase calibration'
!
      MODE = 1
      ISTA_LAST = 1
      CALL ERR_PASS ( IUER, IER )
      FL_STA = PIMA_SELSTA ( MES_SELSTA, PIM, ISTA_LAST, ISTA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5715, IUER, 'PIMA_PLOT_PCAL', 'Error in PIMA_SELSTA' )
           RETURN 
      END IF
!
      M_TONES = MIN ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES, MCLR )
      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE )
      IF ( IND_TONE > 0 ) THEN
           N_TONES = 1
        ELSE IF ( IND_TONE < 1 ) THEN
           IF ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE ) THEN
                IND_TONE = 1
                N_TONES  = 1
             ELSE IF ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ONE ) THEN
                IND_TONE = 1
                N_TONES  = 1
             ELSE IF ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ALL) THEN
                IND_TONE = 0
                N_TONES  = M_TONES
           END IF 
      END IF
 910  CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
              WRITE ( 6, * ) 'PIMA_PLOT_PCAL: M_TONES= ', INT2(M_TONES), ' N_TONES= ', INT2(N_TONES), &
     &                       ' IND_TONE = ', INT2(IND_TONE), ' code: ', PIM%CONF%PHAS_CAL_CODE, &
     &                       ' MODE = ', MODE, ' PCAL_TYPE = ', PCAL_TYPE, ' PCAL_SCA= ', &
     &                       PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA, &
     &                       ' NPOI= ', PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%NPOI
         END IF
         NFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
         STA_STR = PIM%STA(ISTA)%IVS_NAME
         CALL BLANK_TO_UNDERSCORE ( STA_STR )
         IF ( MODE == 1 ) THEN
              PREF_NAME = '/tmp/pcal_phas_'//STA_STR//'_'// &
     &                     PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_'
            ELSE IF ( MODE == 2 ) THEN
              PREF_NAME = '/tmp/pcal_ampl_'//STA_STR//'_'// &
     &                     PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_'
            ELSE IF ( MODE == 3 ) THEN
              PREF_NAME = '/tmp/pcal_frat_'//STA_STR//'_'// &
     &                     PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_'
            ELSE IF ( MODE == 4 ) THEN
              PREF_NAME = '/tmp/pcal_rel_'//STA_STR//'_'// &
     &                     PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_'
            ELSE IF ( MODE == 5 ) THEN
              PREF_NAME = '/tmp/pcal_amph_'//STA_STR//'_'// &
     &                     PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_'
         END IF
!                   
         T8    = 0.0D0
         X8    = 0.0D0
         Y8    = 0.0D0
         FREQS = 0.0D0
!                   
         IF ( ( PIM%CONF%POLAR == PIMA__POLAR_RR  .OR. &
     &          PIM%CONF%POLAR == PIMA__POLAR_HH       ) .OR. &
     &        PIM%CONF%ACT_CODE == PIMA__PDPL_CODE            ) THEN 
              IND_POL = 1
            ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &                  PIM%CONF%POLAR == PIMA__POLAR_VV      ) .AND. &
     &                PIM%NPOL == 2 ) THEN
              IND_POL = 2
            ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &                PIM%NPOL == 1 ) THEN
              IND_POL = 1
            ELSE 
              CALL ERR_LOG ( 5716, IUER, 'PIMA_PLOT_PCAL', 'Polarization code '// &
     &             TRIM(PIM%CONF%POLAR)//' is not supported for plotting pcal. '//&
     &            'Supported codes: RR, LL, HH, VV' )  
              RETURN 
         END IF
!                   
         IF ( PIM%CONF%END_FRQ > PIM%NFRQ ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%END_FRQ, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%NFRQ, STR1 )
              CALL ERR_LOG ( 5717, IUER, 'PIMA_PLOT_PCAL', 'Wrong parameter '// &
     &            'PIM%CONF%END_FRQ: '//STR(1:I_LEN(STR))//' -- more than '// &
     &            'PIM%NFRQ: '//STR1 )
              RETURN
         END IF
!       
         IFRQ = 0
         DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            IF ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
     &           PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
                 DO 420 J2=1,M_TONES
                    IF ( IND_TONE > 0 .AND. IND_TONE .NE. J2 ) GOTO 420
                    NP = 0
                    IF ( N_TONES == 1 ) THEN
                         I_TONE = 1
                       ELSE 
                         I_TONE = J2
                    END IF
                    DO 430 J3=1,PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%NPOI
                       NP = NP + 1
                       IF ( FREQS(J2,J1) < PIMA__MIN_FRQ ) THEN
                            FREQS(J2,J1) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(J2,J1,J3)
                       END IF
                       IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                            IND_SCA  = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%IPOI_SCA(J3)
                          ELSE
                            IND_SCA  = 0
                       END IF
                       TIME_VAL = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J3)
                       IF ( MODE == 1 ) THEN
                            T8(NP,I_TONE,IFRQ) = TIME_VAL
                            IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                 X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(J2,J1,J3,IND_POL)
                               ELSE
                                 X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(J2,J1,J3,IND_POL)
                            END IF
                            IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                                 IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                      X8(NP,I_TONE,IFRQ) = X8(NP,I_TONE,IFRQ) - &
     &                                                     PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(J2,J1,J3,2)
                                    ELSE
                                      X8(NP,I_TONE,IFRQ) = X8(NP,I_TONE,IFRQ) - &
     &                                                     PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(J2,J1,J3,2)
                                 END IF
                            END IF
                            IF ( PIM%FRQ(J1,PIM%CONF%FRQ_GRP)%SIDE_BAND == -1 ) THEN
                                 X8(NP,I_TONE,IFRQ) = -X8(NP,I_TONE,IFRQ) 
                            END IF
                         ELSE IF ( MODE == 2  ) THEN
                            T8(NP,I_TONE,IFRQ) = TIME_VAL
                            IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                 X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL_RGR(J2,J1,J3,IND_POL)
                               ELSE
                                 X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J2,J1,J3,IND_POL)
                            END IF
                            IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                                 IF ( X8(NP,I_TONE,IFRQ) > PIMA__PCAL_AMP_MIN ) THEN
                                      IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                           X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL_RGR(J2,J1,J3,2)/X8(NP,I_TONE,IFRQ)
                                         ELSE
                                           X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J2,J1,J3,2)/X8(NP,I_TONE,IFRQ)
                                      END IF
                                 END IF
                            END IF
                         ELSE IF ( MODE == 3  .AND.  M_TONES > 1 ) THEN
                            T8(NP,1,IFRQ) = TIME_VAL
                            FRQ_DIF = ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(2,J1,J3) - &
     &                                  PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(1,J1,J3) )
                            IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                 Y8(NP,IFRQ) = (PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(2,J1,J3,IND_POL) - &
     &                                          PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(1,J1,J3,IND_POL))/FRQ_DIF
                               ELSE
                                 Y8(NP,IFRQ) = (PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(2,J1,J3,IND_POL) - &
     &                                          PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(1,J1,J3,IND_POL))/FRQ_DIF
                            END IF
                            IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                                 IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                      Y8(NP,IFRQ) = Y8(NP,IFRQ) - &
     &                                      ( (PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(2,J1,J3,2) - &
     &                                         PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(1,J1,J3,2))/FRQ_DIF )
                                    ELSE
                                      Y8(NP,IFRQ) = Y8(NP,IFRQ) - &
     &                                      ( (PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(2,J1,J3,2) - &
     &                                         PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(1,J1,J3,2))/FRQ_DIF )
                                 END IF
                            END IF
                            IF ( NP > 1 ) THEN
                                 IAMB = NINT ( FRQ_DIF*(Y8(NP,IFRQ) - Y8(NP-1,IFRQ))/PI2 )
                                 Y8(NP,IFRQ) =  Y8(NP,IFRQ) - IAMB*PI2/FRQ_DIF
                            END IF
                            IF ( PIM%FRQ(J1,PIM%CONF%FRQ_GRP)%SIDE_BAND == -1 ) THEN
                                 Y8(NP,IFRQ) = -Y8(NP,IFRQ) 
                            END IF
                          ELSE IF ( MODE == 4  .AND.  J2 == I_TONE ) THEN
                            IF ( J1 < IND_FREQ ) THEN
                                 T8(NP,1,IFRQ) = TIME_VAL
                                 IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                      Y8(NP,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,J1,J3,IND_POL) - &
     &                                              PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,IND_FREQ,J3,IND_POL)
                                   ELSE
                                      Y8(NP,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,J1,J3,IND_POL) - &
     &                                              PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,IND_FREQ,J3,IND_POL)
                                 END IF
                                 IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                                      IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                           Y8(NP,IFRQ) = Y8(NP,IFRQ) - &
     &                                         ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,J1,J3,2) - &
     &                                           PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,IND_FREQ,J3,2) )
                                         ELSE
                                           Y8(NP,IFRQ) = Y8(NP,IFRQ) - &
     &                                         ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,J1,J3,2) - &
     &                                           PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,IND_FREQ,J3,2) )
                                      END IF
                                 END IF
                                 IF ( NP > 1 ) THEN
                                      PHS_DIF = Y8(NP,IFRQ) - Y8(NP-1,IFRQ) 
                                      PHS_DIF = PHS_DIF - PI2*IDNINT(PHS_DIF/PI2)
                                      Y8(NP,IFRQ) = Y8(NP-1,IFRQ) + PHS_DIF
                                 END IF
                               ELSE IF ( J1 > IND_FREQ ) THEN
                                 T8(NP,1,IFRQ-1) = TIME_VAL
                                 IF ( IND_TONE > 0 ) THEN
                                      IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                           Y8(NP,IFRQ-1) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,J1,J3,IND_POL) - &
     &                                                     PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,IND_FREQ,J3,IND_POL)
                                         ELSE
                                           Y8(NP,IFRQ-1) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,J1,J3,IND_POL) - &
     &                                                     PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,IND_FREQ,J3,IND_POL)
                                      END IF
                                      IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                                           IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                                Y8(NP,IFRQ-1) = Y8(NP,IFRQ-1) - &
     &                                               ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,J1,J3,2) - &
     &                                                 PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(IND_TONE,IND_FREQ,J3,2) )
                                              ELSE
                                                Y8(NP,IFRQ-1) = Y8(NP,IFRQ-1) - &
     &                                               ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,J1,J3,2) - &
     &                                                 PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(IND_TONE,IND_FREQ,J3,2) )
                                           END IF
                                      END IF
                                    ELSE
                                      IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                           Y8(NP,IFRQ-1) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(1,J1,J3,IND_POL) - &
     &                                                     PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(1,IND_FREQ,J3,IND_POL)
                                         ELSE
                                           Y8(NP,IFRQ-1) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(1,J1,J3,IND_POL) - &
     &                                                     PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(1,IND_FREQ,J3,IND_POL)
                                      END IF
                                      IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                                          IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                               Y8(NP,IFRQ-1) = Y8(NP,IFRQ-1) - &
     &                                              ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(1,J1,J3,2) - &
     &                                                PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(1,IND_FREQ,J3,2) )
                                             ELSE
                                               Y8(NP,IFRQ-1) = Y8(NP,IFRQ-1) - &
     &                                              ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(1,J1,J3,2) - &
     &                                                PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(1,IND_FREQ,J3,2) )
                                          END IF
                                      END IF
                                 END IF
                                 IF ( NP > 1 ) THEN
                                      PHS_DIF = Y8(NP,IFRQ-1) - Y8(NP-1,IFRQ-1) 
                                      PHS_DIF = PHS_DIF - PI2*IDNINT(PHS_DIF/PI2)
                                      Y8(NP,IFRQ-1) = Y8(NP-1,IFRQ-1) + PHS_DIF
                                 END IF
                            END IF
                          ELSE IF ( MODE == 5 ) THEN
                             IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                  T8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(J2,J1,J3,IND_POL)
                                  X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL_RGR(J2,J1,J3,IND_POL)
                                ELSE
                                  T8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(J2,J1,J3,IND_POL)
                                  X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J2,J1,J3,IND_POL)
                             END IF
                             IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                                  IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                       T8(NP,I_TONE,IFRQ) = T8(NP,I_TONE,IFRQ) - &
     &                                                      PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS_RGR(J2,J1,J3,2)
                                     ELSE
                                       T8(NP,I_TONE,IFRQ) = T8(NP,I_TONE,IFRQ) - &
     &                                                      PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(J2,J1,J3,2)
                                  END IF
                                  IF ( X8(NP,I_TONE,IFRQ) > PIMA__PCAL_AMP_MIN ) THEN
                                       IF ( PCAL_TYPE == PIMA__AVR .AND. PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_SCA ) THEN
                                            X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL_RGR(J2,J1,J3,2)/ &
     &                                                           X8(NP,I_TONE,IFRQ) 
                                          ELSE
                                            X8(NP,I_TONE,IFRQ) = PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J2,J1,J3,2)/ &
     &                                                           X8(NP,I_TONE,IFRQ) 
                                       END IF
                                  END IF
                             END IF
                       END IF
                       IF ( J1 .EQ. PIM%CONF%BEG_FRQ .AND. &
     &                      J2 .EQ. 1                .AND. &
     &                      PIM%CONF%DEBUG_LEVEL .GE. 4    ) THEN
!
                            STR = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                           PIM%TAI_0 + PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J3), -2 )
                            IF ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%SOU_IND(J3) > 0 ) THEN
                                 STR1 = PIM%C_SOU(PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%SOU_IND(J3))
                            END IF
                            WRITE ( 6, 210 ) PIM%STA(ISTA)%IVS_NAME, STR(1:24), IND_POL, STR1(1:8), &
     &                        (( K1, K2, 1.D-6*PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(K2,K1,J3), &
     &                                         PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(K2,K1,J3,IND_POL), &
     &                           K2=1,M_TONES ), &
     &                           K1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ )
 210                        FORMAT ( 'Pcal: ', A, 1X, A, ' Pol_ind: ', I1, &
     &                               ' Sou: ', A, &
     &                               64(' Ind_frq: ', I2, ' Ind_tone: ', I1, &
     &                                  ' Fr_pcal: ', F7.1, ' Ph_pcal: ', F8.5) )
                       END IF
 430                CONTINUE 
                    IF ( FL_PCAL_AMBIG .AND. MODE == 4 .AND.  &
     &                   J2 == IND_TONE .AND. NP > 3 ) THEN
!
                         CALL ERR_PASS ( IUER, IER )
                         IF ( J1 < IND_FREQ ) THEN
                              CALL AMBIG_RESOLVE ( ARA3__PHD, NP, T8(1,1,IFRQ), &
     &                                             Y8(1,IFRQ), IER )
                            ELSE IF ( J1 > IND_FREQ ) THEN
                              CALL AMBIG_RESOLVE ( ARA3__PHD, NP, T8(1,1,IFRQ-1), &
     &                                             Y8(1,IFRQ-1), IER )
                         END IF
                         IF ( J1 .NE. IND_FREQ .AND. IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 5718, IUER, 'PIMA_PLOT_PCAL', &
     &                            'Failure in an attempt to resolve '// &
     &                            'phase delay ambiguites' )
                              RETURN 
                         END IF
                    END IF
                    IF ( FL_PCAL_AMBIG .AND. MODE == 1 .AND. NP > 3 ) THEN
!
                         CALL ERR_PASS ( IUER, IER )
                         CALL AMBIG_RESOLVE ( ARA1__PHD, NP, T8(1,J2,IFRQ), &
     &                                        X8(1,J2,IFRQ), IER )
                         IF ( J1 .NE. IND_FREQ .AND. IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 5719, IUER, 'PIMA_PLOT_PCAL', &
     &                            'Failure in an attempt to resolve '// &
     &                            'phase delay ambiguites' )
                              RETURN 
                         END IF
                    END IF
 420             CONTINUE 
 830             CONTINUE 
              ELSE
                 NP = 2
                 T8(1:NP,1,IFRQ) = 0.0D0
                 X8(1:NP,1,IFRQ) = 0.0D0
                 Y8(1:NP,IFRQ)   = 0.0D0
            END IF
            CALL NOUT ( SIZEOF(DIA(IFRQ)), DIA(IFRQ) )
 410     CONTINUE 
!
         IFRQ = 0
         DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DIA(IFRQ)%IDEV = IDEV
!
            IF ( MODE == 1 .OR. MODE == 2 .OR. MODE == 5 ) THEN
                 DIA(IFRQ)%NCLR = N_TONES
               ELSE IF ( MODE == 3 ) THEN
                 DIA(IFRQ)%NCLR = 1
            END IF
            IF ( MODE == 4 ) THEN
                 DIA(1)%NCLR = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ
                 IF ( J4 < IND_FREQ ) THEN
                      IND_PLT = IFRQ
                    ELSE IF ( J4 > IND_FREQ ) THEN
                      IND_PLT = IFRQ - 1
                    ELSE IF ( J4 == IND_FREQ ) THEN
                      GOTO 840
                 END IF 
                 DIA(1)%NPOI(IND_PLT)   = NP
                 DIA(1)%ADR_X8(IND_PLT) = LOC(T8(1,1,IND_PLT))
                 DIA(1)%ADR_Y8(IND_PLT) = LOC(Y8(1,IND_PLT))
                 DIA(1)%ADR_E8(IND_PLT) = 0
                 DIA(1)%LER(IND_PLT)    = .FALSE.
                 DIA(1)%ICOL(IND_PLT)   = ICL1 + IND_PLT - 1
                 DIA(1)%IBST(IND_PLT)   = 0
                 DIA(1)%ILST(IND_PLT)   = 1
                 DIA(1)%IPST(IND_PLT)   = 4
                 DIA(1)%IOST(IND_PLT)   = 1
                 DIA(1)%IWST(IND_PLT)   = 1
                 DIA(1)%STATUS = DIA__DEF
 840             CONTINUE 
               ELSE 
                 DO 460 J6=1,N_TONES
                    DIA(IFRQ)%NPOI(J6)   = NP 
                    DIA(IFRQ)%ADR_X8(J6) = LOC(T8(1,J6,IFRQ))
                    IF ( MODE == 1 .OR. MODE == 2 .OR. MODE == 5 ) THEN
                         DIA(IFRQ)%ADR_Y8(J6) = LOC(X8(1,J6,IFRQ))
                       ELSE IF ( MODE == 3 ) THEN
                         DIA(IFRQ)%ADR_Y8(J6) = LOC(Y8(1,IFRQ))
                    END IF
                    DIA(IFRQ)%ADR_E8(J6) = 0
                    DIA(IFRQ)%LER(J6)    = .FALSE.
                    IF ( J6 == 1 ) THEN
                         DIA(IFRQ)%ICOL(J6) = ICL1
                       ELSE IF ( J6 == 2 ) THEN
                         DIA(IFRQ)%ICOL(J6) = ICL2
                       ELSE 
                         DIA(IFRQ)%ICOL(J6) = ICL2 + J6
                    END IF
                    DIA(IFRQ)%IBST(J6)   = 0
                    DIA(IFRQ)%ILST(J6)   = 1
                    DIA(IFRQ)%IPST(J6)   = 4
                    DIA(IFRQ)%IOST(J6)   = 1
                    DIA(IFRQ)%IWST(J6)   = 1
 460             CONTINUE 
            END IF
!
            DIA(IFRQ)%XMIN  =  1.0
            DIA(IFRQ)%XMAX  = -1.0
            DIA(IFRQ)%ARG_UNITS = 'Time in seconds'
            DIA(IFRQ)%YMIN   =  1.0
            DIA(IFRQ)%YMAX   = -1.0
            IF ( MODE == 1 ) THEN
                 DIA(IFRQ)%ZAG = 'Phase of phase-cal signal at '// &
     &                          PIM%STA(ISTA)%IVS_NAME// &
     &                         ' during '//TRIM(PIM%CONF%SESS_CODE)//' '// &
     &                                     TRIM(PIM%CONF%POLAR)
               ELSE IF ( MODE == 2 ) THEN
                 DIA(IFRQ)%ZAG = 'Amplitude of phase-cal signal at '// &
     &                          PIM%STA(ISTA)%IVS_NAME// &
     &                         ' during '//TRIM(PIM%CONF%SESS_CODE)//' '// &
     &                                     TRIM(PIM%CONF%POLAR)
               ELSE IF ( MODE == 3 ) THEN
                 DIA(IFRQ)%ZAG = 'Frequency phas-cal rate at '// &
     &                          PIM%STA(ISTA)%IVS_NAME// &
     &                         ' during '//TRIM(PIM%CONF%SESS_CODE)//' '// &
     &                                     TRIM(PIM%CONF%POLAR)
               ELSE IF ( MODE == 4 ) THEN
                 DIA(1)%ZAG = 'Pcal phase relative '// &
     &                        'reference frequency at '// &
     &                         PIM%STA(ISTA)%IVS_NAME// &
     &                        ' during '//TRIM(PIM%CONF%SESS_CODE)//' '// &
     &                                     TRIM(PIM%CONF%POLAR)
               ELSE IF ( MODE == 5 ) THEN
                 DIA(IFRQ)%ZAG = 'Phase cal ampl versus phase cal phase at '// &
     &                            PIM%STA(ISTA)%IVS_NAME// &
     &                           ' during '//TRIM(PIM%CONF%SESS_CODE)//' '// &
     &                                       TRIM(PIM%CONF%POLAR)
                 DIA(IFRQ)%ARG_UNITS = 'Phase in rad'
            END IF
            DIA(IFRQ)%NAME   = '/tmp/pcal_'//PIM%STA(ISTA)%IVS_NAME
            IF ( PIM%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
                 DIA(IFRQ)%ZAG = 'Diff '//DIA(IFRQ)%ZAG 
                 DIA(IFRQ)%NAME   = '/tmp/diff_pcal_'//PIM%STA(ISTA)%IVS_NAME
            END IF
            DIA(IFRQ)%ITRM   = 0
            DIA(IFRQ)%IBATCH = 0
            DIA(IFRQ)%STATUS = DIA__DEF
            CALL CLRCH ( STR )
            IF ( ASSOCIATED ( PIM%STA(ISTA)%PCAL(PIM%CONF%FRQ_GRP)%FREQ ) ) THEN
                 IF ( MODE == 4 ) THEN
                      IF ( IND_TONE > 0 ) THEN
                           WRITE ( UNIT=STR(1:8), FMT='(F6.3)' ) FREQS(IND_TONE,IND_FREQ)*1.D-9
                         ELSE
                           WRITE ( UNIT=STR(1:8), FMT='(F6.3)' ) FREQS(1,IND_FREQ)*1.D-9
                      END IF
                    ELSE 
                      IF ( IND_TONE > 0 ) THEN
                           WRITE ( UNIT=STR(1:8), FMT='(F6.3)' ) FREQS(IND_TONE,J4)*1.D-9
                         ELSE
                           WRITE ( UNIT=STR(1:8), FMT='(F6.3)' ) FREQS(1,J4)*1.D-9
                      END IF
                 END IF
               ELSE
                 STR(1:8) = '     0.0'
            END IF
            IF ( MODE == 1 ) THEN
                 TITS(IFRQ) = PIM%STA(ISTA)%IVS_NAME//' PC-phs '//TRIM(STR)//' '// &
     &                        TRIM(PIM%CONF%POLAR)
               ELSE IF ( MODE == 2 ) THEN
                 TITS(IFRQ) = PIM%STA(ISTA)%IVS_NAME//' PC-amp '//TRIM(STR)//' '// &
     &                        TRIM(PIM%CONF%POLAR)
               ELSE IF ( MODE == 3 ) THEN
                 TITS(IFRQ) = PIM%STA(ISTA)%IVS_NAME//' PC-frat '//TRIM(STR)//' '// &
     &                        TRIM(PIM%CONF%POLAR)
               ELSE IF ( MODE == 4 ) THEN
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( IND_TONE, STR1)
                 TITS(1) = PIM%STA(ISTA)%IVS_NAME//' PC-rel '//TRIM(STR)//' '// &
     &                        TRIM(PIM%CONF%POLAR)//' Tone '//TRIM(STR1)
               ELSE IF ( MODE == 5 ) THEN
                 TITS(IFRQ) = PIM%STA(ISTA)%IVS_NAME//' PC-AmPh '//TRIM(STR)//' '// &
     &                        TRIM(PIM%CONF%POLAR)
            END IF
 440     CONTINUE 
!
         IF ( NFRQ < 3 ) THEN
              NC = 1
              NR = NFRQ
            ELSE IF ( NFRQ .LE. 4 ) THEN
              NC = 2
              NR = 2
            ELSE IF ( NFRQ .LE. 9 ) THEN
              NC = 3
              NR = 3
            ELSE IF ( NFRQ .LE. 16 ) THEN
              NC = 4
              NR = 4
            ELSE 
              NC = 5
              NR = 5
         END IF
         IF ( MODE == 4 ) THEN
              NC = 1
              NR = 1
              NFRQ = 1
         END IF 
!
         IF ( MODE == 1 ) THEN
              COMMON_TIT = 'Phase cal phase / time for '// &
     &                      PIM%STA(ISTA)%IVS_NAME//' in '// &
                            TRIM(PIM%CONF%SESS_CODE)//' '// &
     &                      TRIM(PIM%CONF%POLAR)
              BUTTON_LET(1)  = 'Ss'
              BUTTON_NAME(1) = 'Station select'
              BUTTON_LET(2)  = 'Mm'
              BUTTON_NAME(2) = 'Phase cal|amplitude'
              BUTTON_LET(3)  = 'Ff'
              BUTTON_NAME(3) = 'Phase cal|freq rate'
              BUTTON_LET(4)  = 'Rr'
              BUTTON_NAME(4) = 'Phase cal|relative f0'
              BUTTON_LET(5)  = 'Vv'
              BUTTON_NAME(5) = 'Phase amp|versus phase'
              BUTTON_LET(6)  = 'Nn'
              BUTTON_NAME(6) = 'Enter reference|frequency'
              BUTTON_NAME(7) = 'Quit'
              BUTTON_LET(7)  = 'Qq'
            ELSE IF ( MODE == 2 ) THEN
              COMMON_TIT = 'Phase cal amp / time for '// &
     &                      PIM%STA(ISTA)%IVS_NAME//' in '// &
                            PIM%CONF%SESS_CODE//' '// &
     &                      TRIM(PIM%CONF%POLAR)
              BUTTON_LET(1)  = 'Ss'
              BUTTON_NAME(1) = 'Station select'
              BUTTON_LET(2)  = 'Pp'
              BUTTON_NAME(2) = 'Phase cal|Phase'
              BUTTON_LET(3)  = 'Ff'
              BUTTON_NAME(3) = 'Phase cal|freq rate'
              BUTTON_LET(4)  = 'Rr'
              BUTTON_NAME(4) = 'Phase cal|relative f0'
              BUTTON_LET(5)  = 'Vv'
              BUTTON_NAME(5) = 'Phase amp|versus phase'
              BUTTON_LET(6)  = 'Nn'
              BUTTON_NAME(6) = 'Enter reference|frequency'
              BUTTON_NAME(7) = 'Quit'
              BUTTON_LET(7)  = 'Qq'
            ELSE IF ( MODE == 3 ) THEN
              COMMON_TIT = 'Phase cal fr.rat / time for '// &
     &                      PIM%STA(ISTA)%IVS_NAME//' in '// &
                            PIM%CONF%SESS_CODE//' '// &
     &                      TRIM(PIM%CONF%POLAR)
              BUTTON_LET(1)  = 'Ss'
              BUTTON_NAME(1) = 'Station select'
              BUTTON_LET(2)  = 'Pp'
              BUTTON_NAME(2) = 'Phase cal|Phase'
              BUTTON_LET(3)  = 'Mm'
              BUTTON_NAME(3) = 'Phase cal|amplitude'
              BUTTON_LET(4)  = 'Rr'
              BUTTON_NAME(4) = 'Phase cal|relative f0'
              BUTTON_LET(5)  = 'Vv'
              BUTTON_NAME(5) = 'Phase amp|versus phase'
              BUTTON_LET(6)  = 'Nn'
              BUTTON_NAME(6) = 'Enter reference|frequency'
              BUTTON_NAME(7) = 'Quit'
              BUTTON_LET(7)  = 'Qq'
            ELSE IF ( MODE == 4 ) THEN
              COMMON_TIT = 'Pcal phase relative ref freq '// &
     &                     'for '//PIM%STA(ISTA)%IVS_NAME//' in '// &
                            PIM%CONF%SESS_CODE//' '// &
     &                      TRIM(PIM%CONF%POLAR)
              BUTTON_LET(1)  = 'Ss'
              BUTTON_NAME(1) = 'Station select'
              BUTTON_LET(2)  = 'Pp'
              BUTTON_NAME(2) = 'Phase cal|Phase'
              BUTTON_LET(3)  = 'Mm'
              BUTTON_NAME(3) = 'Phase cal|amplitude'
              BUTTON_LET(4)  = 'Ff'
              BUTTON_NAME(4) = 'Phase cal|freq rate'
              BUTTON_LET(5)  = 'Vv'
              BUTTON_NAME(5) = 'Phase amp|versus phase'
              BUTTON_LET(6)  = 'Nn'
              BUTTON_NAME(6) = 'Enter reference|frequency'
              BUTTON_NAME(7) = 'Quit'
              BUTTON_LET(7)  = 'Qq'
            ELSE IF ( MODE == 5 ) THEN
              COMMON_TIT = 'Pcal ampl versus pcal phase for '// &
     &                      PIM%STA(ISTA)%IVS_NAME//' in '// &
                            PIM%CONF%SESS_CODE//' '// &
     &                      TRIM(PIM%CONF%POLAR)
              BUTTON_LET(1)  = 'Ss'
              BUTTON_NAME(1) = 'Station select'
              BUTTON_LET(2)  = 'Pp'
              BUTTON_NAME(2) = 'Phase cal|Phase'
              BUTTON_LET(3)  = 'Mm'
              BUTTON_NAME(3) = 'Phase cal|amplitude'
              BUTTON_LET(4)  = 'Ff'
              BUTTON_NAME(4) = 'Phase cal|freq rate'
              BUTTON_LET(5)  = 'Rr'
              BUTTON_NAME(5) = 'Phase cal|relative f0'
              BUTTON_LET(6)  = 'Nn'
              BUTTON_NAME(6) = 'Enter reference|frequency'
              BUTTON_NAME(7) = 'Quit'
              BUTTON_LET(7)  = 'Qq'
         END IF
!
         IF ( MODE .NE. 6 ) THEN
              ICODE = 0
              CALL ERR_PASS ( IUER, IER )
              CALL MULTI_DIAGI ( COMMON_TIT, NFRQ, NC, NR, TITS, MPB, &
     &                           BUTTON_NAME, BUTTON_LET, PREF_NAME, DIA, &
     &                           ICODE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5720, IUER, 'PIMA_PLOT_PCAL', 'Failure '// &
     &                 'to make a plot of system temperature' )
                   RETURN 
              END IF
         END IF
!
         IF ( MODE == 1 ) THEN
              IF ( ICODE == 0  .OR. ICODE == 1  .OR. ICODE == 7 ) THEN
                   ISTA_LAST = ISTA
                   CALL ERR_PASS ( IUER, IER )
                   FL_STA = PIMA_SELSTA ( MES_SELSTA, PIM, ISTA_LAST, ISTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5721, IUER, 'PIMA_PLOT_PCAL', &
     &                      'Error in PIMA_SELSTA' )
                        RETURN 
                   END IF
                   IF ( .NOT. FL_STA ) GOTO 810
                   MODE = 1
                 ELSE IF ( ICODE == 2 ) THEN
                   MODE = 2
                 ELSE IF ( ICODE == 3 ) THEN
                   MODE = 3
                 ELSE IF ( ICODE == 4 ) THEN
                   MODE = 4
                 ELSE IF ( ICODE == 5 ) THEN
                   MODE = 5
                 ELSE IF ( ICODE == 6 ) THEN
                   MODE = 6
              END IF
              GOTO 910
           ELSE IF ( MODE == 2 ) THEN
              IF ( ICODE == 0  .OR. ICODE == 1  .OR. ICODE == 7 ) THEN
                   ISTA_LAST = ISTA
                   CALL ERR_PASS ( IUER, IER )
                   FL_STA = PIMA_SELSTA ( MES_SELSTA, PIM, ISTA_LAST, ISTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5722, IUER, 'PIMA_PLOT_PCAL', &
     &                      'Error in PIMA_SELSTA' )
                        RETURN 
                   END IF
                   IF ( .NOT. FL_STA ) GOTO 810
                   MODE = 2
                 ELSE IF ( ICODE == 2 ) THEN
                   MODE = 1
                 ELSE IF ( ICODE == 3 ) THEN
                   MODE = 3
                 ELSE IF ( ICODE == 4 ) THEN
                   MODE = 4
                 ELSE IF ( ICODE == 5 ) THEN
                   MODE = 5
                 ELSE IF ( ICODE == 6 ) THEN
                   MODE = 6
              END IF
              GOTO 910
           ELSE IF ( MODE == 3 ) THEN
              IF ( ICODE == 0  .OR. ICODE == 1  .OR. ICODE == 7 ) THEN
                   ISTA_LAST = ISTA
                   CALL ERR_PASS ( IUER, IER )
                   FL_STA = PIMA_SELSTA ( MES_SELSTA, PIM, ISTA_LAST, ISTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5723, IUER, 'PIMA_PLOT_PCAL', &
     &                      'Error in PIMA_SELSTA' )
                        RETURN 
                   END IF
                   IF ( .NOT. FL_STA ) GOTO 810
                   MODE = 3
                 ELSE IF ( ICODE == 2 ) THEN
                   MODE = 1
                 ELSE IF ( ICODE == 3 ) THEN
                   MODE = 2
                 ELSE IF ( ICODE == 4 ) THEN
                   MODE = 4
                 ELSE IF ( ICODE == 5 ) THEN
                   MODE = 5
              END IF
              GOTO 910
           ELSE IF ( MODE == 4 ) THEN
              IF ( ICODE == 0  .OR. ICODE == 1  .OR. ICODE == 7 ) THEN
                   ISTA_LAST = ISTA
                   CALL ERR_PASS ( IUER, IER )
                   FL_STA = PIMA_SELSTA ( MES_SELSTA, PIM, ISTA_LAST, ISTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5724, IUER, 'PIMA_PLOT_PCAL', &
     &                      'Error in PIMA_SELSTA' )
                        RETURN 
                   END IF
                   IF ( .NOT. FL_STA ) GOTO 810
                   MODE = 4
                 ELSE IF ( ICODE == 2 ) THEN
                   MODE = 1
                 ELSE IF ( ICODE == 3 ) THEN
                   MODE = 2
                 ELSE IF ( ICODE == 4 ) THEN
                   MODE = 3
                 ELSE IF ( ICODE == 5 ) THEN
                   MODE = 5
                 ELSE IF ( ICODE == 6 ) THEN
                   MODE = 6
              END IF
              GOTO 910
           ELSE IF ( MODE == 5 ) THEN
              IF ( ICODE == 0  .OR. ICODE == 1  .OR. ICODE == 7 ) THEN
                   ISTA_LAST = ISTA
                   CALL ERR_PASS ( IUER, IER )
                   FL_STA = PIMA_SELSTA ( MES_SELSTA, PIM, ISTA_LAST, ISTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5725, IUER, 'PIMA_PLOT_PCAL', &
     &                      'Error in PIMA_SELSTA' )
                        RETURN 
                   END IF
                   IF ( .NOT. FL_STA ) GOTO 810
                   MODE = 5
                 ELSE IF ( ICODE == 2 ) THEN
                   MODE = 1
                 ELSE IF ( ICODE == 3 ) THEN
                   MODE = 2
                 ELSE IF ( ICODE == 4 ) THEN
                   MODE = 3
                 ELSE IF ( ICODE == 5 ) THEN
                   MODE = 4
              END IF
              GOTO 910
           ELSE IF ( MODE == 6 ) THEN
 920          CONTINUE 
              MODE = 4
              WRITE ( 6, 110 ) PIM%CONF%BEG_FRQ, PIM%CONF%END_FRQ, &
     &                         1, M_TONES
 110          FORMAT ( 'Enter the IF index and tone index in range (',I2, &
     &                  ', ',I2,')  (',I2,', ',I2,')  >> '$ )
              READ ( UNIT=5, FMT='(A)' ) STR
              CALL EXWORD ( STR, 8, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9)//',', IER )
              IF ( LIND < 2 ) THEN
                   WRITE ( 6, * ) 'Wrong input: entered 1 word but 2 are needed, please try again'
                   GOTO 920
              END IF
              CALL CHIN ( STR(IND(1,1):IND(2,1)), IND_FREQ )
              CALL CHIN ( STR(IND(1,2):IND(2,2)), IND_TONE )
              IF ( IND_FREQ < PIM%CONF%BEG_FRQ .OR. IND_FREQ > PIM%CONF%END_FRQ ) THEN
                   WRITE ( 6, * ) 'Wrong input: the IF index is out of range, please try again'
                   GOTO 920
              END IF
              IF ( IND_TONE < 1 .OR. IND_TONE > M_TONES ) THEN
                   WRITE ( 6, * ) 'Wrong input: the tone index is out of range, please try again'
                   GOTO 920
              END IF
              GOTO 910
         END IF
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_PCAL  !#!  
#endif
