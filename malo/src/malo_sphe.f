#define SPHE_NEW
      SUBROUTINE MALO_SPHE ( FILIN, MALO, HEB_LS, HEB_MOD, MALO_TASK, FSH, &
     &                       MALO_OUTPUT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_SPHE
! *                                                                      *
! *  ### 24-OCT-2012   MALO_SPHE  v4.2 (c)  L. Petrov  13-JAN-2024  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      INCLUDE   'heb.i'
      TYPE     ( SPHE_TYPE  ) :: FSH
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_LS, HEB_MOD
      CHARACTER  FILIN*128, MALO_TASK*(*), MALO_OUTPUT*(*)
      TYPE     ( HEB__TYPE  ) :: HEB_SPR
      INTEGER*4  IVRB, MJD_BEG, MJD_END, IUER
      REAL*8     TIM, TIM_BEG, TIM_END
      CHARACTER  STR*128, SGN_LAT*1, SHC_FORMAT*10, &
     &           C_TXT(MALO__SHC_LTXT)*128, FILOUT*128, PREF*128, &
     &           FILTMP*128, INP_EXT*8, OUT_EXT*8, DATE_STR*30, &
     &           INTERNET_HOSTNAME*64, SYSNAME*128, HARDWARE*128, &
     &           TMP_DIR*128
      LOGICAL*1  FL_LAT_LON_VAL 
      REAL*8,    ALLOCATABLE :: SPR_R8(:,:,:), SPH_HAR(:,:,:,:)
      REAL*4,    ALLOCATABLE :: SPR_R4(:,:)
      REAL*8,    ALLOCATABLE :: DSP_ARR3(:,:,:)
      REAL*8     LAT_VAL, LON_VAL, DSP(8192), RES(3), SPR_VAL(4)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, DEG, &
     &           NORM, IPHS, IER, IDEV, NLAT, NLON, L_TXT, ID, IE, IP, &
     &           NUM_SETS, IND_WC, IND_PAR, N_FRQ, IND_FRQ(2*MALO__MFRQ), &
     &           M_FRQ, KLON, KLAT, IND3_SECT(2), IND4_SECT(2)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      REAL*8,    EXTERNAL :: SPHE_COMP_VAL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
!
      IF ( IVRB .GE. 2 ) WRITE ( 6, '(A)' ) 'MALO_SPHE: '//GET_CDATE()// &
     &                   ' Read input file '//FILIN(1:I_LEN(FILIN))
!
      FL_LAT_LON_VAL = .FALSE. 
      CALL ERR_PASS ( IUER, IER ) 
      CALL READ_HEB ( FILIN, HEB_SPR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6111, IUER, 'MALO_SPHE', 'Error in '// &
     &         'an attempt to read heb-file '//FILIN )
           RETURN 
      END IF   
      MALO%NLON = HEB_SPR%DIMS(1)
      MALO%NLAT = HEB_SPR%DIMS(2)
      MALO%NTIM = HEB_SPR%DIMS(4)
      MALO%MJD_BEG = HEB_SPR%MJD
      MALO%TAI_BEG = HEB_SPR%UTC
      IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
           M_FRQ = MALO%MODC_NHAR_SUBTR
         ELSE 
           M_FRQ = 1
      END IF
!
      IF ( MALO%NLAT == MALO%NLON/2 + 1 ) THEN
           MALO%NLAT  = MALO%NLAT - 1
      END IF
!      IF ( DABS ( DLOG(MALO%NLAT-1.D0)/DLOG(2.0D0) - IDINT(DLOG(MALO%NLAT-1.D0)/DLOG(2.0D0)) ) < 1.0D-5 ) THEN
!           MALO%NLAT = MALO%NLAT - 1
!      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      ALLOCATE ( MALO%MJD_ARR(1), STAT=IER )
!
      CALL ERR_PASS ( IUER, IER ) 
      ALLOCATE ( MALO%TAI_ARR(1), STAT=IER )
      MALO%MJD_ARR(1) = MALO%MJD_BEG 
      MALO%TAI_ARR(1) = MALO%TAI_BEG 
!
      DEG = MALO%NLAT/2 - 1
      NORM = 1
      IPHS = 1
!
      IF ( ASSOCIATED ( MALO%SPH ) ) THEN
           DEALLOCATE ( MALO%SPH )
      END IF
!
      ALLOCATE ( MALO%SPH(2,0:DEG,0:DEG,2,1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*2*(DEG+1)**2*2, STR )
           CALL ERR_LOG ( 6112, IUER, 'MALO_SPHE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for spherical harmonic '// &
     &         'coefficients' )
           RETURN 
      END IF
      MALO%SPH_STATUS = MALO__ALLO
!
      ALLOCATE ( SPR_R8(MALO%NLON,MALO%NLAT,M_FRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MALO%NLON*MALO%NLAT*M_FRQ, STR )
           CALL ERR_LOG ( 6113, IUER, 'MALO_SPHE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPR_R8' )
           RETURN 
      END IF
!
      ALLOCATE ( SPH_HAR(2,0:DEG,0:DEG,M_FRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*(DEG+1)**2*M_FRQ, STR )
           CALL ERR_LOG ( 6114, IUER, 'MALO_SPHE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPH_HAR' )
           RETURN 
      END IF
!
! --- Check whether the name has astrisk
!
      IND_WC = INDEX ( MALO_OUTPUT, '*' ) 
      IF ( IND_WC > 1 ) THEN
           PREF    = MALO_OUTPUT(1:IND_WC-1)
           OUT_EXT =  MALO_OUTPUT(IND_WC+1:)
         ELSE 
           CALL CLRCH ( PREF )
           CALL CLRCH ( OUT_EXT  )
           IP = LINDEX ( MALO_OUTPUT, '.' ) 
           IF ( IP > 1 ) THEN
                OUT_EXT = MALO_OUTPUT(IP:)
           END IF
           IF ( OUT_EXT == '.shc' ) THEN
                CONTINUE 
              ELSE 
                CALL ERR_LOG ( 6115, IUER, 'MALO_SPHE', 'Unsupported '// &
     &              'output file name extension '//OUT_EXT(1:I_LEN(OUT_EXT))// &
     &              ' . Supported extnesions: .shc . Malo output: '// &
     &              MALO_OUTPUT )
                RETURN 
           END IF
      END IF 
!
      SHC_FORMAT = 'REAL*4'
!
! --- Create comments put into the file
!
      L_TXT    = 1
      C_TXT(L_TXT) = 'Spherical transform of the surface pressure field'
      IF ( MALO%CONF%FINAM_LS_MASK .NE. 'NONE' ) THEN
           L_TXT    = L_TXT + 1
           C_TXT(L_TXT) = 'with land-sea mask applied'
      END IF
      IF ( MALO_TASK == 'sphe_love_create' .OR. MALO_TASK == 'sphe_love_update'  ) THEN
           L_TXT    = L_TXT + 1
           C_TXT(L_TXT) = 'with Love numbers multiplied its haramonics'
      END IF
      L_TXT    = L_TXT + 1
      C_TXT(L_TXT) = 'MALO_SURFACE_TYPE   = '//MALO%CONF%SURFACE_TYPE   
      L_TXT    = L_TXT + 1
      C_TXT(L_TXT) = 'MALO_FINAM_LS_MASK  = '//MALO%CONF%FINAM_LS_MASK
      L_TXT    = L_TXT + 1
      C_TXT(L_TXT) = 'MALO_FINAM_MODEL    = '//MALO%CONF%FINAM_MODEL
      IF ( MALO_TASK == 'sphe_love_create' .OR. MALO_TASK == 'sphe_love_update'  ) THEN
           C_TXT(L_TXT) = 'LOVE_FILE           = '//MALO%CONF%LOVE_FILE
      END IF
!
      IF ( IVRB .GE. 2 ) WRITE ( 6, '(A)' ) 'MALO_SPHE: '//GET_CDATE()// &
     &                   ' Started detrending the surface pressure' 
      DO 410 J1=1,MALO%NTIM
         IF ( (MALO%MJD_ARR(J1)*86400.0D0 + MALO%TAI_ARR(J1)) < &
     &        (MJD_BEG*86400.D0 + TIM_BEG - MALO__TIME_EPS )    ) GOTO 410
         IF ( (MALO%MJD_ARR(J1)*86400.0D0 + MALO%TAI_ARR(J1)) > &
     &        (MJD_END*86400.D0 + TIM_END + MALO__TIME_EPS )    ) GOTO 410
         TIM = (MALO%MJD_BEG - MALO%MJD_DEF)*86400.0D0 + &
     &         (MALO%TAI_BEG - MALO%TAI_DEF)
!
         IF ( FL_LAT_LON_VAL ) THEN
              LON_VAL = 20.0
              LAT_VAL = 50.0
              KLON = NINT(LON_VAL/360.*MALO%NLON) + 1
              KLAT = NINT((LAT_VAL+90.0)/180.*(MALO%NLAT-1))  + 1
              SPR_VAL(1) = HEB_SPR%VAL(KLON,KLAT,1,1) 
              SPR_VAL(2) = HEB_MOD%VAL(KLON,KLAT,MALO%IND_MOD_CNST,1)
              SPR_VAL(3) = SPR_VAL(1) - SPR_VAL(2) 
         END IF
!!!!!!!!!
#ifdef SPHE_OLD
!
! ====== OBSOLETE approach
!
         DO 420 J2=1,MALO%NLAT
            DO 430 J3=1,MALO%NLON
               IND_PAR = 0
               IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
!
! ----------------- Remove the average surface pressure
!
                    SPR_R8(J3,J2,1) = HEB_SPR%VAL(J3,J2,1,1) - HEB_MOD%VAL(J3,J2,MALO%CONF%MEAN_PAR,1)
                  ELSE IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_IGNORE ) THEN
                    SPR_R8(J3,J2,1) = HEB_SPR%VAL(J3,J2,1,1)
               END IF
!
! ------------ Remove the contribution of the harmonic model in surface pressure
!
               IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                    N_FRQ = 0
                  ELSE 
                    N_FRQ = 1
               END IF
               DO 440 J4=1,MALO__MFRQ
                  IND_PAR = IND_PAR + 1
                  IF ( IND_PAR .GE. MALO%CONF%MIN_HAR_PAR  .AND. &
     &                 IND_PAR .LE. MALO%CONF%MAX_HAR_PAR        ) THEN
                       IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
                            SPR_R8(J3,J2,1) = SPR_R8(J3,J2,1) - HEB_MOD%VAL(J3,J2,IND_PAR,1)* &
     &                                                          DCOS(FREQ(J4)*TIM + PHAS(J4))
                          ELSE IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                            N_FRQ = N_FRQ + 1
                            IND_FRQ(N_FRQ) = IND_PAR
                            SPR_R8(J3,J2,N_FRQ) = HEB_MOD%VAL(J3,J2,IND_PAR,1)
                       END IF
                  END IF
!
                  IND_PAR = IND_PAR + 1
                  IF ( IND_PAR .GE. MALO%CONF%MIN_HAR_PAR  .AND. &
     &                 IND_PAR .LE. MALO%CONF%MAX_HAR_PAR        ) THEN
                       IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
                            SPR_R8(J3,J2,1) = SPR_R8(J3,J2,1) - HEB_MOD%VAL(J3,J2,IND_PAR,1)* &
     &                                                          DSIN(FREQ(J4)*TIM + PHAS(J4))
                          ELSE IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                            N_FRQ = N_FRQ + 1
                            IND_FRQ(N_FRQ) = IND_PAR
                            SPR_R8(J3,J2,N_FRQ) = HEB_MOD%VAL(J3,J2,IND_PAR,1)
                       END IF
                  END IF
 440           CONTINUE 
!
! ------------ Apply land-sea mask
!
               IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND ) THEN
                    SPR_R8(J3,J2,1:N_FRQ) = SPR_R8(J3,J2,1:N_FRQ)*HEB_LS%VAL(J3,J2,1,1)
                 ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__OCEAN ) THEN
                    SPR_R8(J3,J2,1:N_FRQ) = SPR_R8(J3,J2,1:N_FRQ)*(1.D0 - HEB_LS%VAL(J3,J2,1,1))
               END IF
 430        CONTINUE 
 420     CONTINUE 
#else
!
! ====== MODERN approach
!
         IND4_SECT = 1
         N_FRQ = 0
         IF ( MALO%NMDC == 0 .AND. MALO%CONF%MODEL_USE .EQ. MALO__MOD_IGNORE ) THEN
              N_FRQ = N_FRQ + 1
              SPR_R8(1:MALO%NLON,1:MALO%NLAT,N_FRQ) = HEB_SPR%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
         END IF
         DO 420 J2=1,MALO%NMDC
            IF ( MALO%MODC(J2)%TYP == MALO__CNST ) THEN
                 IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT  .OR. &
     &                MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUB_NOHAR      ) THEN
!
! ------------------- Get the J2-th component of the regression model
!
                      IND3_SECT = J2
                      CALL ERR_PASS ( IUER, IER )
                      CALL READ_HEB_SECT ( MALO%CONF%FINAM_MODEL, &
     &                                     HEB_MOD, IND3_SECT, IND4_SECT, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL IINCH ( J2, STR )
                           CALL ERR_LOG ( 6116, IUER, 'MALO_SPHE', 'Error in '// &
     &                         'reading the '//STR(1:I_LEN(STR))// &
     &                         ' component of the model file '//MALO%CONF%FINAM_MODEL )
                           RETURN 
                      END IF
                      N_FRQ = N_FRQ + 1
!
! ------------------- Remove the average surface pressure
!
                      SPR_R8(1:MALO%NLON,1:MALO%NLAT,1) = HEB_SPR%VAL(1:MALO%NLON,1:MALO%NLAT,1,1) - HEB_MOD%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
                    ELSE IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_IGNORE ) THEN
!
! ------------------- Just copy surface pressure
!
                      N_FRQ = N_FRQ + 1
                      SPR_R8(1:MALO%NLON,1:MALO%NLAT,N_FRQ) = HEB_SPR%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
                 END IF
            END IF
!
            IF ( MALO%MODC(J2)%TYP == MALO__JMP ) THEN
                 IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT .OR. &
     &                MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUB_NOHAR     ) THEN
                      IF ( TIM .GE. MALO%MODC(J2)%TIM .AND. MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
!
! ------------------------ Get the J2-th component of the regression model
!
                           CALL ERR_PASS ( IUER, IER )
                           IND3_SECT = J2
                           CALL ERR_PASS ( IUER, IER )
                           CALL READ_HEB_SECT ( MALO%CONF%FINAM_MODEL, &
     &                                          HEB_MOD, IND3_SECT, IND4_SECT, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR )
                                CALL IINCH ( J2, STR )
                                CALL ERR_LOG ( 6117, IUER, 'MALO_SPHE', 'Error in '// &
     &                              'reading the '//STR(1:I_LEN(STR))// &
     &                              ' component of the model file '//MALO%CONF%FINAM_MODEL )
                                RETURN 
                           END IF
!
! ------------------------ Remove the jump in the surface pressure

                           SPR_R8(1:MALO%NLON,1:MALO%NLAT,1) =   SPR_R8(1:MALO%NLON,1:MALO%NLAT,1) &
     &                                                         - HEB_MOD%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
                      END IF
                 END IF
            END IF
 420     CONTINUE 
!
         DO 430 J3=1,MALO_HFS(MALO%CONF%MODEL_CODE)
            IF ( ( MALO%MODC(J3)%TYP == MALO__COS .OR. &
     &             MALO%MODC(J3)%TYP == MALO__SIN      ) .AND. &
     &           ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT .OR. &
     &             MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY      ) ) THEN
!
! --------------- Get the J3-th component of the regression model
!
                  CALL ERR_PASS ( IUER, IER )
                  IND3_SECT = J3
                  CALL ERR_PASS ( IUER, IER )
                  CALL READ_HEB_SECT ( MALO%CONF%FINAM_MODEL, &
     &                                 HEB_MOD, IND3_SECT, IND4_SECT, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL IINCH ( J3, STR )
                       CALL ERR_LOG ( 6118, IUER, 'MALO_SPHE', 'Error in '// &
     &                     'reading the '//STR(1:I_LEN(STR))// &
     &                     ' component of the model file '//MALO%CONF%FINAM_MODEL )
                       RETURN 
                  END IF
            END IF
!
            IF ( MALO%MODC(J3)%TYP == MALO__COS ) THEN
                 IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
!
! ------------------- Remove the contribution of the cos-part of the harmonic model in surface pressure
!
                      SPR_R8(1:MALO%NLON,1:MALO%NLAT,1) = &
     &                       SPR_R8(1:MALO%NLON,1:MALO%NLAT,1) - &
     &                           DCOS(MALO%MODC(J3)%FRQ*TIM + MALO%MODC(J3)%PHS)*  &
     &                                HEB_MOD%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
                    ELSE IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                      N_FRQ = N_FRQ + 1
                      SPR_R8(1:MALO%NLON,1:MALO%NLAT,N_FRQ) = &
     &                       HEB_MOD%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
                 END IF
              ELSE IF ( MALO%MODC(J3)%TYP == MALO__SIN ) THEN
                 IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
!
! ------------------- Remove the contribution of the sin-part of the harmonic model in surface pressure
!
                      SPR_R8(1:MALO%NLON,1:MALO%NLAT,1) = &
     &                       SPR_R8(1:MALO%NLON,1:MALO%NLAT,1) - &
     &                           DSIN(MALO%MODC(J3)%FRQ*TIM + MALO%MODC(J3)%PHS)*  &
     &                                HEB_MOD%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
                    ELSE IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                      N_FRQ = N_FRQ + 1
                      SPR_R8(1:MALO%NLON,1:MALO%NLAT,N_FRQ) = &
     &                       HEB_MOD%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
                 END IF
            END IF
 430     CONTINUE 
         IF ( ASSOCIATED ( HEB_MOD%VAL ) ) THEN
              DEALLOCATE ( HEB_MOD%VAL )
         END IF
!
! ------ Apply the land-sea mask
!
         DO 440 J4=1,N_FRQ
            IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND ) THEN
                 SPR_R8(1:MALO%NLON,1:MALO%NLAT,J4) = SPR_R8(1:MALO%NLON,1:MALO%NLAT,J4)* &
     &               HEB_LS%VAL(1:MALO%NLON,1:MALO%NLAT,1,1)
              ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__OCEAN ) THEN
                 SPR_R8(1:MALO%NLON,1:MALO%NLAT,J4) = SPR_R8(1:MALO%NLON,1:MALO%NLAT,J4)* &
     &              (1.D0 - HEB_LS%VAL(1:MALO%NLON,1:MALO%NLAT,1,1))
            END IF
 440     CONTINUE 
#endif
!
         IF ( IVRB .EQ. 8 .OR. IVRB .EQ. 9 ) THEN
!
! ----------- Ploting in test modes
!
              DO 450 J5=1,N_FRQ
                  ALLOCATE ( SPR_R4(MALO%NLON,MALO%NLAT) )
                  STR = '/tmp/foo'
                  SPR_R4(1:MALO%NLON,1:MALO%NLAT) = SPR_R8(1:MALO%NLON,1:MALO%NLAT,J5)
!
                  IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                       CALL CLRCH ( STR(11:12) )
                       CALL INCH  ( IND_FRQ(J5), STR(11:12) )
                       CALL CHASHR ( STR(11:12) )
                       STR(11:16) = 'frq='//STR(11:12)
                     ELSE
                       CALL CLRCH ( STR(11:12) )
                  END IF
                  IF ( IVRB == 8 ) THEN
                       IDEV = 1
                       WRITE ( 6, * ) 'nlon/nlat: ', MALO%NLON, MALO%NLAT, ' n_frq= ', n_frq
                       CALL PLOT_GRID_R4 ( IDEV, 7, 0, 1, MALO%NLON, MALO%NLAT, SPR_R4, &
     &                                    'Pressure '//STR(11:16), 'Pa', 1.0, -1.0, &
     &                                    STR(1:8), IER  )
!                       IF ( MALO%CONF%MODEL_USE .NE. MALO__MOD_HAR_ONLY ) THEN
!                            CALL PLOT_GRID_R4 ( IDEV, 7, 10, 1, MALO%NLON, MALO%NLAT, HEB_LS%VAL, &
!     &                          'Land-sea mask', 'd/l', 1.0, -1.0, STR(1:8), IER )
!                       END IF
                    ELSE IF ( IVRB == 9 ) THEN
                       DATE_STR = MJDSEC_TO_DATE ( HEB_SPR%MJD, HEB_SPR%UTC, IER )
                       CALL GRID_2D_SHIFT_180_R4 ( MALO%NLON, MALO%NLAT, SPR_R4 )
                       ID = LINDEX ( FILIN, '/' )
                       IE = LINDEX ( FILIN, '.heb' )
                       FILTMP = '/tmp'//FILIN(ID:IE-1)
!!                     WRITE ( 6, * ) 'FILTMP= ', FILTMP(1:I_LEN(FILTMP))
                       IF ( FL_LAT_LON_VAL ) THEN
                            SPR_VAL(4) = SPR_R8(KLON,KLAT,1) 
                            WRITE ( 6, 210 ) DATE_STR(1:16), -90.0 + (180.0*(KLAT-1))/(MALO%NLAT-1), &
     &                                       (360.*(KLON-1))/MALO%NLON, SPR_VAL
 210                        FORMAT ( 'RRR ', A, ' Lat= ', F6.2, ' Lon= ', F7.2, &
     &                               ' Pres1= ', F8.1, ' Pres2= ', F8.1, &
     &                               ' Pres3= ', F7.1, ' Pres4= ', F7.1  )
                       END IF
                       IDEV = 3
                       CALL PLOT_GRID_R4 ( IDEV, 7, 44, 2, MALO%NLON, MALO%NLAT, SPR_R4, &
     &                                     'Pressure anomaly '//DATE_STR(1:16), 'Pa', &
     &                                     1.0, -1.0, FILTMP, IER  )
                       CALL EXIT ( 0 )
                  END IF
                  DEALLOCATE ( SPR_R4 )
 450          CONTINUE 
           ELSE IF ( IVRB .EQ. 10 ) THEN
!
! ----------- Special test mode: we write down the surface presssure after
! ----------- removing the mean value and harmonic variations
!
              HEB_SPR%DATA_FORMAT  = HEB__R8
              HEB_SPR%OFFSET       = 0.0
              HEB_SPR%SCALE_FACTOR = 1.0
              HEB_SPR%DIMS(3) = 1
              HEB_SPR%DIMS(4) = 1
              CALL HEB_MINMAX_R8 ( HEB_SPR, SPR_R8, 100.0D0*1000.0D0 )
!
              DATE_STR = MJDSEC_TO_DATE ( HEB_SPR%MJD, HEB_SPR%UTC, IER )
              CALL GETINFO_HOST ( INTERNET_HOSTNAME )
              IF ( INTERNET_HOSTNAME == 'localhost' ) THEN
                   CALL GETINFO_SYSTEM ( SYSNAME, INTERNET_HOSTNAME, HARDWARE )
              END IF
              IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &             INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &             INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &             INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &             INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'        ) THEN
                   TMP_DIR = '/dev/shm'
                ELSE IF ( INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a' ) THEN
                   TMP_DIR = '/imls/oper_temp'
                ELSE 
                   TMP_DIR = '/tmp'
              END IF
!
              FILOUT = TRIM(TMP_DIR)//'/spr_anomaly_'//DATE_STR(1:4)//DATE_STR(6:7)// &
     &                  DATE_STR(9:10)//'_'//DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
              IER = -1
              CALL WRITE_HEB ( HEB_SPR, SPR_R8, FILOUT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6119, IUER, 'MALO_SPHE', 'Error in writing '// &
     &                 'output file '//FILOUT )
                   CALL EXIT ( 1 )
              END IF
              WRITE ( 6, '(A)' ) 'Surface pressure was written in '//TRIM(FILOUT)
              CALL EXIT ( 0 )
         END IF
!
         IF ( IVRB .GE. 2 ) WRITE ( 6, '(A,I5)' ) 'MALO_SPHE: '//GET_CDATE()// &
     &                     ' Started transform of degree ', DEG
!
! ------ Spherical harmonics transform
!
         DO 460 J6=1,N_FRQ
            CALL ERR_PASS ( IUER, IER )
            CALL SPHE_DIR_2NN ( FSH, MALO%NLAT, SPR_R8(1,1,J6), DEG, DEG, NORM, &
     &                          IPHS, SPH_HAR(1,0,0,J6), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6120, IUER, 'MALO_SPHE', 'Error in '// &
     &               'an attempt to perform spherical transform of the '// &
     &               'pressure field' )
                 RETURN 
            END IF
 460     CONTINUE 
         IF ( IVRB .GE. 2 ) WRITE ( 6, '(A,I5)' ) 'MALO_SPHE: '//GET_CDATE()// &
     &                     ' Finished spherical transform'
         IF ( IVRB .GE. 8 ) THEN
              WRITE ( 6, * ) ' NLON/NLAT/N_FRQ= ', MALO%NLON, MALO%NLAT,  N_FRQ
              IER = -1
              ALLOCATE ( DSP_ARR3(MALO%NLON,MALO%NLAT+1,3)  )
              CALL SPHE_INV_2NN_VEC ( FSH, DEG, DEG, SPH_HAR, MALO%NLAT+1, DSP_ARR3, IER )
              ALLOCATE ( SPR_R4(MALO%NLON,MALO%NLAT+1) )
              SPR_R4(1:MALO%NLON,1:MALO%NLAT+1) = DSP_ARR3(1:MALO%NLON,1:MALO%NLAT+1,1) 
              CALL PLOT_GRID_R4 ( IDEV, 7, 0, 1, MALO%NLON, MALO%NLAT+1, SPR_R4, &
     &                           'Loading displacelemnt', 'mm', 1.0, -1.0, FILTMP, IER  )
              DEALLOCATE ( SPR_R4   ) 
              DEALLOCATE ( DSP_ARR3 ) 
         END IF
         MALO%SPH_STATUS = MALO__LOAD
!
! ------ Scaling the spherical harmonics by Love numbers
!
         IF ( N_FRQ > 0 ) THEN
              DO 470 J7=1,N_FRQ
                 IF ( MALO_TASK == 'sphe_love_create' .OR. &
     &                MALO_TASK == 'sphe_love_update'      ) THEN
!
                      DO 480 J8=0,DEG ! order
                         DO 490 J9=J8,DEG ! degree
                            DO 4100 J10=1,2
                               MALO%SPH(J10,J9,J8,1,1) = SPH_HAR(J10,J9,J8,J7)* &
     &                                                   3.0D0*MALO%LOVE(J9,MALO__H)/(2*J9+1)/ &
     &                                                  (MALO__DENS*MALO__GRAV)
                               MALO%SPH(J10,J9,J8,2,1) = SPH_HAR(J10,J9,J8,J7)* &
     &                                                   3.0D0*MALO%LOVE(J9,MALO__L)/(2*J9+1)/ &
     &                                                  (MALO__DENS*MALO__GRAV)
 4100                       CONTINUE 
 490                    CONTINUE 
 480                  CONTINUE 
                      NUM_SETS = 2
                    ELSE IF ( MALO_TASK == 'sphe_create' .OR. &
     &                        MALO_TASK == 'sphe_update'      ) THEN
                      MALO%SPH(1:2,0:DEG,0:DEG,1,1) = SPH_HAR(1:2,0:DEG,0:DEG,1) 
                      NUM_SETS = 1
                 END IF
!
! -------------- Build the output file name
!
                 IF ( IND_WC > 1 ) THEN
                      STR = MJDSEC_TO_DATE ( MALO%MJD_ARR(J1), MALO%TAI_ARR(J1), -2 )
                      IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                           CALL INCH ( IND_FRQ(J7), STR(1:2) )
                           CALL CHASHR            ( STR(1:2) )
                           CALL BLANK_TO_ZERO     ( STR(1:2) )
                           FILOUT = PREF(1:I_LEN(PREF))//'frq'//STR(1:2)//OUT_EXT
                         ELSE 
                           FILOUT = PREF(1:I_LEN(PREF))//STR(1:4)//STR(6:7)//STR(9:10)//'_'// &
     &                              STR(12:13)//STR(15:16)//OUT_EXT
                      END IF
                    ELSE 
                       FILOUT = MALO_OUTPUT
                 END IF 
!
                 IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
                      MALO%MJD_ARR(J1) = J2000__MJD
                      MALO%TAI_ARR(J1) = 0.0D0
                 END IF
!
! -------------- Write scaled spherical harmonics in the output file
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL SHC_WRITE ( DEG, SHC_FORMAT, NUM_SETS, MALO%SPH, MALO%MJD_ARR(J1), &
     &                            MALO%TAI_ARR(J1), L_TXT, C_TXT, FILOUT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6121, IUER, 'MALO_SPHE', 'Failure in an attempt '// &
     &                    'to write the spherical harmonics into output file '// &
     &                     FILOUT )
                      RETURN 
                 END IF
 470          CONTINUE
            ELSE
!
! ----------- Write scaled spherical harmonics in the output file
!
              NUM_SETS = 1
              FILOUT = MALO_OUTPUT
              CALL ERR_PASS ( IUER, IER )
              CALL SHC_WRITE ( DEG, SHC_FORMAT, NUM_SETS, MALO%SPH, MALO%MJD_ARR(J1), &
     &                         MALO%TAI_ARR(J1), L_TXT, C_TXT, FILOUT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6121, IUER, 'MALO_SPHE', 'Failure in an attempt '// &
     &                 'to write the spherical harmonics into output file '// &
     &                  FILOUT )
                   RETURN 
              END IF
         END IF
         IF ( IVRB .GE. 1 ) THEN
              WRITE ( 6, '(A)' ) 'MALO_SPHE: '//GET_CDATE()// &
     &                           ' Wrote output file '//FILOUT(1:I_LEN(FILOUT))
              CALL FLUSH ( 6 )
         END IF
 410  CONTINUE 
!
! --- Release allocated memory
!
      CALL HEB_QUIT ( HEB_SPR )
      DEALLOCATE    ( SPH_HAR )
      DEALLOCATE    ( SPR_R8 )
      IF ( ASSOCIATED ( MALO%MJD_ARR ) ) DEALLOCATE ( MALO%MJD_ARR )
      IF ( ASSOCIATED ( MALO%TAI_ARR ) ) DEALLOCATE ( MALO%TAI_ARR )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_SPHE  !#!  
