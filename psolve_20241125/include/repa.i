!
! >>>>> Include block for package REPA ( Residuals Plotting and Amboguities )
! >>>>> 2004.12.01 (c)  L. Petrov  v 2.6  2021.03.20_20:14:35
!
      INTEGER*4   REPA__M_CNF, REPA__M_STS
      PARAMETER ( REPA__M_CNF =   12 )
      PARAMETER ( REPA__M_STS =    9 )
      INTEGER*4   REPA__M_SOU, REPA__M_STA, REPA__M_BAS, REPA__M_COM
      PARAMETER ( REPA__M_SOU = 1024  )
      PARAMETER ( REPA__M_STA =  256  )
      PARAMETER ( REPA__M_BAS = 32000 )
      PARAMETER ( REPA__M_COM = 32768 )
      CHARACTER   REPA__CNF_LABEL*54,  REPA__STS_LABEL*42, &
     &            REPA__CNF_2004*54,   REPA__STS_2004*42 
      PARAMETER ( REPA__CNF_LABEL = &
     &           'REPA Configuration file. Format version of 2006.06.01 ' )
      PARAMETER ( REPA__STS_LABEL = &
     &           'REPA_STATUS  Format version of 2006.06.01 ' )
      PARAMETER ( REPA__CNF_2004 = &
     &           'REPA Configuration file. Format version of 2004.12.01 ' )
      PARAMETER ( REPA__STS_2004 = &
     &           'REPA_STATUS  Format version of 2004.12.01 ' )
!
      INTEGER*4  N1$REP
      INTEGER*4  REP__M_ARG, REP__M_VAL, REP__M_MOD
      PARAMETER  ( REP__M_ARG = 12 ) 
      PARAMETER  ( REP__M_VAL = 30 ) 
      PARAMETER  ( REP__M_MOD =  4 ) 
      CHARACTER  REP__CH_ARG(REP__M_ARG)*8, REP__CH_VAL(REP__M_VAL)*8, &
     &           REP__CH_MOD(REP__M_MOD)*6, REP__CH_KEY(REP__M_MOD)*3
      DATA       REP__CH_ARG &
     &     /                 &
     &           'Time    ', & !  1
     &           'Del_Err ', & !  2
     &           'Delay   ', & !  3
     &           'Rate    ', & !  4
     &           'Elev_St1', & !  5
     &           'Elev_St2', & !  6
     &           'Azim_St1', & !  7
     &           'Azim_St2', & !  7
     &           'Feed_St1', & !  8
     &           'Feed_St2', & ! 10
     &           'Temp_St1', & ! 11
     &           'Temp_St2'  & ! 12
     &     /
      DATA       REP__CH_VAL &
     &     /                 &
     &           'Delay   ', & !  1
     &           'Rate    ', & !  2
     &           'SNR_X   ', & !  3
     &           'SNR_S   ', & !  4
     &           'Temp_St1', & !  5
     &           'Temp_St2', & !  6
     &           'Pres_St1', & !  7
     &           'Pres_St2', & !  8
     &           'GrIon_Dl', & !  9
     &           'PhIon_Dl', & ! 10
     &           'SpAmb_Gx', & ! 11
     &           'SpAmb_Gs', & ! 12
     &           'GrAmb_Gx', & ! 13
     &           'GrAmb_Gs', & ! 14
     &           'Cal1_St1', & ! 15
     &           'Cal1_St2', & ! 16
     &           'Cal2_St1', & ! 17
     &           'Cal2_St2', & ! 18
     &           'Cal3_St1', & ! 19
     &           'Cal3_St2', & ! 20
     &           'Cal4_St1', & ! 21
     &           'Cal4_St2', & ! 22
     &           'Cal5_St1', & ! 23
     &           'Cal5_St2', & ! 24
     &           'Cal6_St1', & ! 25
     &           'Cal6_St2', & ! 26
     &           'Cal7_St1', & ! 27
     &           'Cal7_St2', & ! 28
     &           'Cal8_St1', & ! 29
     &           'Cal8_St2'  & ! 30
     &     /
      DATA    ( REP__CH_MOD(N1$REP), REP__CH_KEY(N1$REP), N1$REP=1,REP__M_MOD ) &
     &     /                 &
     &           'DiaGi ',  'ESC',  & !  1
     &           'SngAmb',  'F1 ',  & !  2
     &           'GrpAmb',  'F2 ',  & !  3
     &           'GrpTgl',  'F3 '   & !  4
     &     /
!
      TYPE      REP__CNF__TYPE 
	  CHARACTER  CONF_FILE*128
	  INTEGER*4  GOOD_CLR
	  INTEGER*4  BAD_CLR
	  INTEGER*4  UNRC_CLR
	  INTEGER*4  MARKED_CLR
	  CHARACTER  STAT_FILE*128
	  CHARACTER  REPI_FILE*128
	  CHARACTER  BASELINE*16
	  CHARACTER  BOX_SYMMETRIC*4
	  CHARACTER  INQUIRY_DATA*128
	  CHARACTER  INQUIRY_FORMAT*128
	  CHARACTER  MARKED_SOURCE*8
	  INTEGER*4  ARG_IND
	  INTEGER*4  VAL_IND
	  INTEGER*4  MOD_IND
	  INTEGER*4  PAGE
	  INTEGER*4  LUN_REPI
	  INTEGER*4  BOU_IND
	  CHARACTER  BOU_BOX*1
	  LOGICAL*1  SHOW_CBAD
	  LOGICAL*1  SHOW_UNRC
	  CHARACTER  FILLER1*3
      END TYPE  REP__CNF__TYPE 
!
      TYPE      REP__LIS__TYPE 
	  INTEGER*4  L_SOU
	  INTEGER*4  L_STA
	  INTEGER*4  L_BAS
	  CHARACTER  C_SOU(REPA__M_SOU)*8
	  CHARACTER  C_STA(REPA__M_STA)*8
	  CHARACTER  C_BAS(REPA__M_BAS)*16
	  INTEGER*4  KA_BAS(REPA__M_BAS)
	  INTEGER*4  KG_BAS(REPA__M_BAS)
	  INTEGER*4  KB_BAS(REPA__M_BAS)
	  INTEGER*4  KU_BAS(REPA__M_BAS)
	  INTEGER*4  KG_SOU(REPA__M_SOU)
	  INTEGER*4  KB_SOU(REPA__M_SOU)
	  INTEGER*4  KU_SOU(REPA__M_SOU)
          LOGICAL*4  EF_SOU(REPA__M_SOU) ! Flag whether coordinates are estimated
      END TYPE  REP__LIS__TYPE 
!
      TYPE      REP__COM__TYPE 
	  INTEGER*4  IND_COM
	  INTEGER*4  IND_SBC
	  INTEGER*4  IND_BAS
	  INTEGER*4  IND_OBS
	  INTEGER*4  IVAL
      END TYPE  REP__COM__TYPE 
!
      TYPE     REP__PLT__TYPE 
	  INTEGER*4  N_GOO
	  INTEGER*4  N_BAD
	  INTEGER*4  N_UNR
!
	  INTEGER*4  BOU_IND
	  REAL*8     ARG_SCL
	  REAL*8     VAL_SCL
!
	  REAL*8,    POINTER :: ARG_GOO(:)
	  REAL*8,    POINTER :: VAL_GOO(:)
	  REAL*8,    POINTER :: ERR_GOO(:)
	  INTEGER*4, POINTER :: IND_GOO(:)
!
	  REAL*8,    POINTER :: ARG_BAD(:)
	  REAL*8,    POINTER :: VAL_BAD(:)
	  REAL*8,    POINTER :: ERR_BAD(:)
	  INTEGER*4, POINTER :: IND_BAD(:)
!
	  REAL*8,    POINTER :: ARG_UNR(:)
	  REAL*8,    POINTER :: VAL_UNR(:)
	  REAL*8,    POINTER :: ERR_UNR(:)
	  INTEGER*4, POINTER :: IND_UNR(:)
      END TYPE REP__PLT__TYPE 
!
      TYPE     REP__OBS__TYPE
	  REAL*8     FJD
	  REAL*8     FRACT
	  REAL*8     TIM_SEC
	  REAL*8     TAU_GR_X
	  REAL*8     TAU_GR_S
	  REAL*8     TAU_PH_X
	  REAL*8     TAU_PH_S
	  REAL*8     TAU_SB_X
	  REAL*8     TAU_SB_S
	  REAL*8     TAU_GR_X_ORIG
	  REAL*8     TAU_GR_S_ORIG
	  REAL*8     TAU_PH_X_ORIG
	  REAL*8     TAU_PH_S_ORIG
	  REAL*8     ERR_GR_X
	  REAL*8     ERR_GR_S
	  REAL*8     ERR_SB_X
	  REAL*8     ERR_SB_S
	  REAL*8     ERR_PH_X
	  REAL*8     ERR_PH_S
	  REAL*8     SPAMB_GR_X
	  REAL*8     SPAMB_GR_S
	  REAL*8     SPAMB_PH_X
	  REAL*8     SPAMB_PH_S
	  REAL*8     FRQEFF_GR_X
	  REAL*8     FRQEFF_GR_S
	  REAL*8     FRQEFF_PH_X
	  REAL*8     FRQEFF_PH_S
	  INTEGER*4  NAMB_GR_X
	  INTEGER*4  NAMB_GR_S
	  INTEGER*4  NAMB_PH_X
	  INTEGER*4  NAMB_PH_S
	  REAL*8     EL(2)
	  REAL*8     AZ(2)
	  REAL*8     FEED_ANG(2)
	  REAL*8     AIR_TEMP(2)
	  REAL*8     AIR_PRES(2)
	  REAL*8     SCAL(8,2)
	  REAL*8     SNR_X
	  REAL*8     SNR_S
	  INTEGER*2  SUPSTAT(2)
	  INTEGER*2  UACSUP
	  INTEGER*4  AUTO_SUP
	  INTEGER*4  USER_SUP
	  INTEGER*4  USER_REC
	  INTEGER*4  IND_SOU
	  INTEGER*4  IND_STA(2)
	  INTEGER*4  IND_BAS
	  CHARACTER  QUAL_X*2
	  CHARACTER  QUAL_S*2
	  CHARACTER  SCAN_NAME*10
	  CHARACTER  SCAN_PIMA*10
	  CHARACTER  FRINGE_X_FINAM*16
	  CHARACTER  FRINGE_S_FINAM*16
      END TYPE REP__OBS__TYPE
!
      TYPE     REP__RES__TYPE
	  REAL*8     RES_DEL
	  REAL*8     RES_RAT
	  REAL*8     ERR_DEL
	  REAL*8     ERR_RAT
	  REAL*8     TAU_C
	  REAL*8     RATE_C
      END TYPE REP__RES__TYPE
!
      TYPE     REP__TYPE
	  INTEGER*4  N_OBS
	  INTEGER*4  N_BAS
	  INTEGER*4  N_STA
	  INTEGER*4  N_SOU
	  CHARACTER, POINTER ::  C_BAS(:)*17
	  CHARACTER, POINTER ::  C_STA(:)*8
	  CHARACTER, POINTER ::  C_SOU(:)*8
          CHARACTER  BAND_NAM(MAX_BND)*1
	  CHARACTER  DBNAME_STR*16
	  CHARACTER  FPL_DIR*128
	  CHARACTER  GIF_VIEWER*128
          TYPE ( REP__CNF__TYPE )           :: CNF
          TYPE ( REP__LIS__TYPE )           :: LIS
          TYPE ( REP__COM__TYPE ), POINTER  :: COM(:)
          TYPE ( REP__PLT__TYPE ), POINTER  :: PLT(:)
          TYPE ( REP__RES__TYPE ), POINTER  :: RES(:)
          TYPE ( REP__OBS__TYPE ), POINTER  :: OBS(:)
	  TYPE ( DIAGI_STRU     ), POINTER  :: DIAGI(:)
	  LOGICAL*4  LSEL_SOU(REPA__M_SOU)
	  INTEGER*4  IND_SOU_SEL
	  INTEGER*4  IND_SOU_SEL_LAST
	  CHARACTER  CH_ARG(REP__M_ARG)*32
	  CHARACTER  CH_VAL(REP__M_VAL)*32
	  CHARACTER  TITLE*128
	  CHARACTER  TITS(REPA__M_BAS)*17
	  CHARACTER  PREF(REPA__M_BAS)*128
	  CHARACTER  FRINGE_ROOT_DIR*128
	  CHARACTER  SOLVE_PS_VIEWER*128
	  CHARACTER  EXPSERNO_STR*8
	  INTEGER*4  N_COM
	  INTEGER*2  DATYP_I2
	  INTEGER*2  FILLER_1
	  INTEGER*4  STATUS
      END TYPE REP__TYPE
      INTEGER*4  REPA__LINQ
      PARAMETER  ( REPA__LINQ = 11 )
      CHARACTER  REPA__CINQ(REPA__LINQ)*12
      DATA         REPA__CINQ &
     &           / &
     &             'OBS_TYPE    ', &
     &             'OBS_INDEX   ', &
     &             'BAS_NAME    ', &
     &             'SOUR_NAME   ', &
     &             'TIME_SHORT  ', &
     &             'TIME_LONG   ', &
     &             'OBSERVABLE  ', &
     &             'QUAL_CODE   ', &
     &             'SNR         ', &
     &             'ELEV_DEG    ', &
     &             'AZIM_DEG    ' &
     &           / 
!
      INTEGER*4  REPA__INIT, REPA__LOADED, REPA__PLOT, REPA__QUIT
      PARAMETER  ( REPA__INIT   = 1823543232 )
      PARAMETER  ( REPA__LOADED = 1723102321 )
      PARAMETER  ( REPA__PLOT   = 1932827023 )
      PARAMETER  ( REPA__QUIT   = 1648203923 )
      REAL*4       REPA__LAB_RSC
      PARAMETER  ( REPA__LAB_RSC = 1.0/1.5 ) ! Rescaling factor for labels
      INTEGER*4    REPA__M_NC, REPA__M_NR, REPA__M_BUT, REPA__M_BOX
      PARAMETER  ( REPA__M_NC  = 5  )
      PARAMETER  ( REPA__M_NR  = 5  )
      PARAMETER  ( REPA__M_BUT = 11 )
      PARAMETER  ( REPA__M_BOX = 3  )
      CHARACTER   REPA__BUTLET(REPA__M_BUT)*3, REPA__BUTNAM(REPA__M_BUT)*32
!
! --- NB: Some REPA__BUTLET definitions are redefined in repa.f 
! ---     It is done this stupid way due to a bug in HP Fortran 90 compiler
!
      DATA        ( REPA__BUTLET(N1$REP), REPA__BUTNAM(N1$REP), &
     &              N1$REP=1,REPA__M_BUT ) &
     &            / &
     &               'Ttt',  'Change |argument                ', & !  1
     &               'Vvv',  'Change value                    ', & !  2
     &               'Ggg',  'Frame|good points               ', & !  3
     &               'Bbb',  'Frame|good&bad pts              ', & !  4
     &               'Lll',  'Frame|all points                ', & !  5
     &               'Sss',  'Set box|symmetric               ', & !  6
     &               'Rrr',  'Set box|around all              ', & !  7
     &               'Mmm',  'Mark |a source                  ', & !  8
     &               'Xxx',  'Exit                            ', & !  9
     &               'Ppp',  'Prior Page                      ', & ! 10
     &               'Nnn',  'Next Page                       '  & ! 11
     &            /
      CHARACTER  REPA__LET_BOX(REPA__M_BOX)*1, REPA__NAME_BOX(REPA__M_BOX)*8
      DATA       ( REPA__LET_BOX(N1$REP), REPA__NAME_BOX(N1$REP), &
     &             N1$REP=1,REPA__M_BOX ) &
     &            / &
     &               'G',  'Good    ', & ! 1
     &               'B',  'Bad&Good', & ! 2
     &               'L',  'All     '  & ! 3
     &            / 
!
      REAL*8       REPA__OVR_ARG
      PARAMETER  ( REPA__OVR_ARG = 0.02D0 )
!
! --- Maximal distance to the point which the cursor is pointing at
! --- (as a share of entire screen) necessary for selection the point
!
      REAL*4     REPA__M_DIST_R4, REPA__ASPECT
      PARAMETER  ( REPA__M_DIST_R4 = 0.05 )
      PARAMETER  ( REPA__ASPECT    = 1.5  ) ! Plot's aspect ratio
!
      REAL*8       REPA__M_AMBSP 
      PARAMETER  ( REPA__M_AMBSP = 1.D-12 ) ! Minimal ambiguity spacing
!
      INTEGER*4    REPA__M_AMB
      PARAMETER  ( REPA__M_AMB = 32000) ! Maximal ambiguity
!
! --- Repa commands symbpolic names
!
      INTEGER*4  REP__COM_SNGTGL, REP__COM_SNGAMB, &
     &           REP__COM_GRPAMB, REP__COM_GRPTGL
      PARAMETER  ( REP__COM_SNGTGL = 1 )
      PARAMETER  ( REP__COM_SNGAMB = 2 )
      PARAMETER  ( REP__COM_GRPAMB = 3 )
      PARAMETER  ( REP__COM_GRPTGL = 4 )
!
      INTEGER*4  REPA__I_GOO, REPA__I_BAD, REPA__I_UNR
      PARAMETER  ( REPA__I_GOO = 1 )
      PARAMETER  ( REPA__I_BAD = 2 )
      PARAMETER  ( REPA__I_UNR = 3 )
!
      INTEGER*4   REPA__M_PRG
      PARAMETER ( REPA__M_PRG = 3000 ) ! Progress counter step
!
      REAL*8      REPA__RES_DEL_MAX, REPA__RES_RAT_MAX, &
     &            REPA__ERR_DEL_MAX, REPA__ERR_RAT_MAX
      PARAMETER ( REPA__RES_DEL_MAX = 1.D0  ) 
      PARAMETER ( REPA__RES_RAT_MAX = 1.D0  ) 
      PARAMETER ( REPA__ERR_DEL_MAX = 1.D-6 ) 
      PARAMETER ( REPA__ERR_RAT_MAX = 1.D-9 ) 
!
! >>>>>
