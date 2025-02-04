      PROGRAM    TEST_01
! ************************************************************************
! *                                                                      *
! *   Routine TEST_01 
! *                                                                      *
! *   Usage: test_01 heb-dir date inp_grid_height deg output_grid_height *
! *                                                                      *
! *  ### 19-NOV-2013    TEST_01   v1.1 (c)  L. Petrov  19-NOV-2013  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_H, HEB_W, &
     &                           HEB_IGH
      REAL*8       SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72 
      PARAMETER  ( SPD__U1_GMAO72 =    20.25319 )
      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000 )
      PARAMETER  ( SPD__U3_GMAO72 =  -169.30782 )
      CHARACTER  DIR_HEB*128, OBS_DATE*128, FIL_IGH*128, &
     &           DIR_OUT*128, FIL_OUT*128, STR*128, CAL_DATE*19, &
     &           FINAM_DELP*128, FINAM_W*128, DATA_SRC*128, GEN_SPR__LABEL*32
      REAL*8     LAT_GDT(2), LON(2), HEI(2), P(2), PW(2), TEM(2), REF(2), DEL
      PARAMETER  ( GEN_SPR__LABEL = 'GEN_SPR  Vers  1.2 of 2013.08.01' )
      INTEGER*8  DIR_DESC
      REAL*4     ARGS(3)
      INTEGER*4  ID, DEG, J1, J2, INDS(3), DIMS(3), IUER
      LOGICAL*1  EX_DELP, EX_W
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN4 
      REAL*8,    EXTERNAL :: REFRA_CIDDOR
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, LINDEX
!
!!      IF ( IARGC() < 3 ) THEN
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: test_01 heb-dir date inp_grid_height '
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIR_HEB  )
           CALL GETARG ( 2, OBS_DATE )
           CALL GETARG ( 3, FIL_IGH  )
      END IF
!
      IF ( DIR_HEB == "fix_nc_date" ) THEN
           IUER = -1
           CALL FIX_NC_DATE ( OBS_DATE, IUER )
           CALL EXIT ( 0 )
        ELSE IF ( DIR_HEB == "fix_spl_date" ) THEN
           IUER = -1
           CALL FIX_SPL_DATE ( OBS_DATE, IUER )
           CALL EXIT ( 0 )
      END IF
!
      FINAM_DELP = DIR_HEB(1:I_LEN(DIR_HEB))//'/'//OBS_DATE(1:4)// &
     &            '/d/d_'//OBS_DATE(1:I_LEN(OBS_DATE))//'.heb.bz2'
      FINAM_W    = DIR_HEB(1:I_LEN(DIR_HEB))//'/'//OBS_DATE(1:4)// &
     &            '/w/w_'//OBS_DATE(1:I_LEN(OBS_DATE))//'.heb.bz2'
      INQUIRE ( FILE=FINAM_DELP, EXIST=EX_DELP )
      INQUIRE ( FILE=FINAM_W,    EXIST=EX_W    )
      IF ( EX_DELP .AND. .NOT. EX_W ) THEN
           IUER = -1
           CALL READ_DQT_HEB ( DIR_HEB, OBS_DATE, HEB_DELP, HEB_Q, HEB_T, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6702, IUER, 'TEST_01', 'Error in an attempt '// &
     &                        'to read numerical weather model for date '//OBS_DATE )
                CALL EXIT ( 1 )
           END IF
           DATA_SRC = HEB_DELP%SOURCE
         ELSE IF ( .NOT. EX_DELP .AND. EX_W ) THEN
           IUER = -1
           CALL READ_HTW_HEB ( DIR_HEB, OBS_DATE, HEB_H, HEB_T, HEB_W, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6703, IUER, 'TEST_01', 'Error in an attempt '// &
     &                        'to read numerical weather model for date '//OBS_DATE )
                CALL EXIT ( 1 )
           END IF
           DATA_SRC = HEB_W%SOURCE
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'TEST_01', 'Did not find neither delp file '// &
     &                    FINAM_DELP(1:I_LEN(FINAM_DELP))//' nor w file '// &
     &                    FINAM_W(1:I_LEN(FINAM_W)) )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_IGH, HEB_IGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6705, IUER, 'TEST_01', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_COMP_PPWTEM ( 1, HEB_DELP, HEB_T, HEB_Q, HEB_IGH, &
     &                        MALO, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL ERR_LOG ( 6707, IUER, 'TEST_01', 'Error in an attempt '// &
     &         'to compute P, PW, TEM' )
           CALL EXIT ( 1 )
      END IF
!!      WRITE ( 6, * ) ' lev/lon/lat/tim = ', MALO%NLEV, MALO%NLON, MALO%NLAT, MALO%NTIM
!
      LAT_GDT(1) = -0.45186116212885435D0
      LON(1)     = 0.48318160438050523D0
      HEI(1)     = 1409.7094442229718D0
!
      LAT_GDT(2) = -0.45186141345288428D0
      LON(2)     = 0.48320125682682646D0
      HEI(2)     = 1416.0132307205349D0
!
      DIMS(1) = MALO%NLEV
      DIMS(2) = MALO%NLON
      DIMS(3) = MALO%NLAT
!
      DO 410 J1=1,2
         ARGS(1) = LOG(HEI(J1) + SPD__U2_GMAO72)*SPD__U1_GMAO72 + SPD__U3_GMAO72
         ARGS(2) = LON(J1)
         ARGS(3) = LAT_GDT(J1)
!
         INDS(1) = IXMN4 ( MALO%NLEV, MALO%LEV, ARGS(1) )
         INDS(2) = IXMN4 ( MALO%NLON, MALO%LON, ARGS(2) )
         INDS(3) = IXMN4 ( MALO%NLAT, MALO%LAT, ARGS(3) )
!
         P(J1)   = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                            MALO%LEV, MALO%LON, MALO%LAT, &
     &                            MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1,MALO__P) )
         PW(J1)  = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                            MALO%LEV, MALO%LON, MALO%LAT, &
     &                            MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1,MALO__PW) )
         TEM(J1) = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                            MALO%LEV, MALO%LON, MALO%LAT, &
     &                            MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1,MALO__TEM) )
         REF(J1) = REFRA_CIDDOR ( P(J1), PW(J1), TEM(J1) ) - 1.0D0
 410  CONTINUE 
      DEL = (REF(2) + REF(1))/2.0D0*(HEI(2) - HEI(1) + 6.6907*DCOS(LAT_GDT(1)))/299792458.D0
      WRITE ( 6, 110 ) OBS_DATE(1:I_LEN(OBS_DATE)), DEL*1.D12
 110  FORMAT ( A, 2X, F5.2, ' ps' )
!
      END  PROGRAM  TEST_01  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   REFRA_CIDDOR ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine I1TD_REFRA_CIDDOR
! *                                                                      *
! * ## 16-SEP-2013 I1TD_REFRA_CIDDOR v1.0 (c) L. Petrov  16-SEP-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     REFRA_CIDDOR
      REAL*8     P, PW, TEM
!
! --- Thermodynamic constants
!
      REAL*8     SPD__R, SPD__MA, SPD__H2O, SPD__CO2, SPD__NOM_CO2, &
     &           SPD__ABS_TEMP, SPD__H2O_DRY_RAT
      PARAMETER  ( SPD__R        = 8.314472D0    ) ! CIPM-2007 J/(K mol)
      PARAMETER  ( SPD__MA       = 0.02896546D0  ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__H2O      = 0.01801528    ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__CO2      = 0.012011D0    ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__NOM_CO2  = 0.004D0       ) ! CIPM-2007
      PARAMETER  ( SPD__ABS_TEMP = 273.15D0      )
      REAL*8     SPD__ENH_ALPHA, SPD__ENH_BETA, SPD__ENH_GAMMA
      PARAMETER  ( SPD__ENH_ALPHA = 1.00062D0 )
      PARAMETER  ( SPD__ENH_BETA  = 3.14D-8   )
      PARAMETER  ( SPD__ENH_GAMMA = 5.6D-7    )
      REAL*8     SPD__PSV_A, SPD__PSV_B, SPD__PSV_C, SPD__PSV_D
      PARAMETER  ( SPD__PSV_A =  1.2378847D-5  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_B = -1.9121316D-2  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_C = 33.93711047D0  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_D = -6.3431645D3  ) ! CIPM-2007
      REAL*8     SPD__COMP_A0, SPD__COMP_A1, SPD__COMP_A2, SPD__COMP_B0, SPD__COMP_B1, &
     &           SPD__COMP_C0, SPD__COMP_C1, SPD__COMP_D0, SPD__COMP_E0
      PARAMETER  ( SPD__COMP_A0 =  1.58123D-6 ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A1 = -2.9331D-8  ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A2 =  1.1043D-10 ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B0 =  5.707D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B1 = -2.051D-8   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C0 =  1.9898D-4  ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C1 = -2.376D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_D0 =  1.83D-11   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_E0 = -0.765D-8   ) ! CIPM-2007
      REAL*8       SPD__CO2_CONC
      PARAMETER  ( SPD__CO2_CONC = 0.00375D0 ) 
      PARAMETER  ( SPD__H2O_DRY_RAT = SPD__H2O/(SPD__MA + SPD__CO2*(SPD__CO2_CONC - SPD__NOM_CO2)) )
!
      REAL*8     SPD__IR_K0, SPD__IR_K1, SPD__IR_K2, SPD__IR_K3
      REAL*8     SPD__IR_W0, SPD__IR_W1, SPD__IR_W2, SPD__IR_W3, SPD__CF
      PARAMETER  ( SPD__IR_K0 =  2.380185D+14 ) ! d/l
      PARAMETER  ( SPD__IR_K1 =  5.792105D+10 ) ! 1/m^2
      PARAMETER  ( SPD__IR_K2 =  5.7362D+13   ) ! 1/m^2
      PARAMETER  ( SPD__IR_K3 =  1.67917D+09  ) ! 1/m^2
      PARAMETER  ( SPD__CF    =  1.022D0      ) ! 1/m^2
!
      PARAMETER  ( SPD__IR_W0 =  2.95235D-6 ) ! d/l
      PARAMETER  ( SPD__IR_W1 =  2.6422D-20 ) ! 1/m^2
      PARAMETER  ( SPD__IR_W2 = -3.2380D-34 ) ! 1/m^4
      PARAMETER  ( SPD__IR_W3 =  4.028D-47  ) ! 1/m^6
      REAL*8     PD__REF, PW__REF, TEM__D_REF, TEM__W_REF
      PARAMETER  ( PD__REF = 101325.0D0 )
      PARAMETER  ( PW__REF =   1333.0D0 )
      PARAMETER  ( TEM__D_REF = 288.15D0 )
      PARAMETER  ( TEM__W_REF = 293.15D0 )
      REAL*8     SVP, F, ZD_REF, ZW_REF, RHOD_REF, RHOW_REF, RHO_D, RHO_W, &
     &           ZM, ZD, WN, NDS, NDW, C1, C2
      PARAMETER  ( C1 = 2.2432028D-04 )
      PARAMETER  ( C2 = 3.1172776D-04 )
!
! --- Refractivity constants
!
! --- Jean Roeger, "Refractivity Index Formulae for Radio Waves",
! --- JS28 Interaction of Techniques and Corrections to Achieve
! --- Accurate Engeneering, FIGS XXII International congress, 
! --- Washington D.C. April 19-26, 2002
!
      REAL*8     SPD__K1R, SPD__K2R, SPD__K3R
      PARAMETER  ( SPD__K1R = 7.76890D-7  ) ! K/Pa
      PARAMETER  ( SPD__K2R = 7.129520D-7 ) ! K/Pa
      PARAMETER  ( SPD__K3R = 3.754630D-3 ) ! K**2/Pa
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM+SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM+SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM+SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM+SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      RHO_D = (P - PW)/TEM * (SPD__MA /SPD__R)*ZM
      RHO_W =      PW /TEM * (SPD__H2O/SPD__R)*ZM
      REFRA_CIDDOR = 1.D0 + RHO_D*C1 + RHO_W*C2
!
      RETURN
      END  FUNCTION   REFRA_CIDDOR  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FIX_NC_DATE ( FILIN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auzilliary program for fixing TAI time tag in the loading          *
! *   displacement field in netCDF format.                               *
! *                                                                      *
! *  ### 07-FEB-2017   FIX_NC_DATE   v1.0 (c) L. Petrov 07-FEB-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      CHARACTER  FILIN*128
      INTEGER*4  IUER
!
      INTEGER*4  NLON, NLAT, MJD, IVEC, ICMP, IFRQ, IER
      REAL*4,    ALLOCATABLE :: DSP_ARR_R4(:)
      REAL*8,    ALLOCATABLE :: DSP_ARR_R8(:)
      CHARACTER  WAV_NAM(MALO__MWAV)*4, NC_FILE*128, TIM_STR*4, FILOUT*128
      CHARACTER  FILDSC*128, FILCOM*128, COM*512
      INTEGER*8  MEL
      INTEGER*4  IMN, IHR, IL, IS
      REAL*8     TAI
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SYSTEM
!
      IL = ILEN(FILIN)
      TIM_STR = FILIN(IL-10:IL-7)
      READ ( UNIT=TIM_STR, FMT='(I2,I2)' ) IHR, IMN
      TAI = IHR*3600.0D0 + IMN*60.0D0
!
      NC_FILE = FILIN(1:IL-4)
!!      FILOUT = NC_FILE(1:14)//'GRID'//NC_FILE(19:)
      FILOUT = NC_FILE(1:6)//'moad'//NC_FILE(11:)
!
      CALL ERR_PASS ( IUER, IER )
      CALL INQ_LOADING_NC ( FILIN, NLON, NLAT, MJD, TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6031, IUER, 'FIX_NC_DATE', 'Error in inq_loading_nc' ) 
           RETURN 
      END IF
      MEL = INT8(3)*INT8(NLON)*INT8(NLAT)
      ALLOCATE ( DSP_ARR_R4(MEL) )
      ALLOCATE ( DSP_ARR_R8(MEL) )
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_LOADING_NC ( FILIN, MEL, NLON, NLAT, IVEC, ICMP, &
     &                       IFRQ, MJD, TAI, DSP_ARR_R4, WAV_NAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6032, IUER, 'FIX_NC_DATE', 'Error in '// &
     &         'reading file '//FILIN )
           RETURN 
      END IF
      TAI = IHR*3600.0D0 + IMN*60.0D0
      DSP_ARR_R8 = DSP_ARR_R4
      IF ( INDEX ( FILIN, 'atm/merra2' ) > 0 ) THEN
           FILDSC = MALO_SHARE//'/atm_merra2_data_source.txt'
           FILCOM = MALO_SHARE//'/atm_description.txt'
        ELSE IF ( INDEX ( FILIN, 'atm/geosfpit' ) > 0 ) THEN
           FILDSC = MALO_SHARE//'/atm_geosfpit_data_source.txt'
           FILCOM = MALO_SHARE//'/atm_description.txt'
        ELSE IF ( INDEX ( FILIN, 'lws/merra2'   ) > 0 ) THEN
           FILDSC = MALO_SHARE//'/lws_merra2_data_source.txt'
           FILCOM = MALO_SHARE//'/lws_description.txt'
        ELSE IF ( INDEX ( FILIN, 'lws/geosfpit' ) > 0 ) THEN
           FILDSC = MALO_SHARE//'/lws_geosfpit_data_source.txt'
           FILCOM = MALO_SHARE//'/lws_description.txt'
        ELSE IF ( INDEX ( FILIN, 'nto/omct'     ) > 0 ) THEN
           FILDSC = MALO_SHARE//'/nto_omct_data_source.txt'
           FILCOM = MALO_SHARE//'/nto_description.txt'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRITE_LOADING_NC ( NLON, NLAT, 1, 0, &
     &                        MJD, TAI, %VAL(0), DSP_ARR_R4, &
     &                        MALO__LABEL, 'NO', FILDSC, FILCOM, &
     &                        FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6033, IUER, 'FIX_NC_DATE', 'Error in '// &
     &         'writing into file '//FILOUT )
           RETURN 
      END IF
      COM = 'lbzip2 -f -n 1 '//FILOUT
      IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
!
      RETURN
      END  SUBROUTINE  FIX_NC_DATE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FIX_SPL_DATE ( FILIN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auzilliary program for fixing TAI time tag in the loading          *
! *   displacement field in netCDF format.                               *
! *                                                                      *
! *  ### 07-FEB-2017   FIX_SPL_DATE  v1.0 (c) L. Petrov 07-FEB-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      CHARACTER  FILIN*128
      INTEGER*4  IUER
!
      TYPE     ( HEB__TYPE  ) :: HEB
      INTEGER*4  NLON, NLAT, MJD, IVEC, ICMP, IFRQ, IER
      REAL*4,    ALLOCATABLE :: DSP_ARR_R4(:)
      REAL*8,    ALLOCATABLE :: DSP_ARR_R8(:)
      CHARACTER  WAV_NAM(MALO__MWAV)*4, TIM_STR*4, FILOUT*128
      CHARACTER  FILDSC*128, FILCOM*128, COM*512
      INTEGER*8  MEL
      INTEGER*4  IMN, IHR, IL, IS, ID, IB
      REAL*8     TAI
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, SYSTEM
!
      IL = ILEN(FILIN)
      TIM_STR = FILIN(IL-11:IL-8)
      READ ( UNIT=TIM_STR, FMT='(I2,I2)' ) IHR, IMN
      TAI = IHR*3600.0D0 + IMN*60.0D0
!
      ID = LINDEX ( FILIN, '/' ) + 1
      IB = LINDEX ( FILIN, '.bz2' ) - 1
      FILOUT = '/f0/'//FILIN(ID:IB)
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6644, IUER , 'FIX_SPL_DATE', 'Failure to read '// &
     &         'input file '// FILIN )
           RETURN 
      END IF
      HEB%UTC = IHR*3600.0D0 + IMN*60.0D0
      HEB%TAI = IHR*3600.0D0 + IMN*60.0D0
      HEB%DATA_COMPRESSION = 'none'
      CALL CLRCH ( HEB%COMMENT(4) )
      IUER = -1
      CALL WRITE_HEB ( HEB, HEB%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6645, IUER , 'FIX_SPL_DATE', 'Failure to WRITE '// &
     &         'ouput file '// FILIN )
           RETURN 
      END IF
      COM = 'lbzip2 -f -n 1 '//FILOUT
      IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
      COM = 'mv '//TRIM(FILOUT)//'.bz2'//' '//FILIN
      IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FIX_SPL_DATE  !#!#
