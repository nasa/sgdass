!
! >>>>> INCLUDE-BLOCK with descriptions data structures used for phase delay
!       ambiguity resolution algorithm incorporated to SOLVE
!
!       include block solve.i SHOULD BE declared before!
!
!       pamb.i  07-NOV-1997  v3.22  (c)  L. Petrov  --  2022.03.26_07:25:39
!
      INTEGER*4    MG_SOU, MG_STA, MG_BAS, MG_TRI, MG_OBS, MG_SCA, M_FAM
      PARAMETER  ( MG_SOU = MAX_ARC_SRC )
      PARAMETER  ( MG_STA = MAX_ARC_STA )
      PARAMETER  ( MG_BAS = (MG_STA*(MG_STA-1))/2 )
      PARAMETER  ( MG_TRI = (MG_STA*(MG_STA-1)*(MG_STA-2))/6 )
      PARAMETER  ( MG_OBS = MAX_OBS )
      PARAMETER  ( MG_SCA = 2048    )
      PARAMETER  ( M_FAM  = 8       )
!
      INTEGER*4   NPAMB$ARG, &
     &            PAMB_VRB_MIN,  PAMB_VRB_MAX, &
     &            PAMB_BAND_MIN, PAMB_BAND_MAX, &
     &            PAMB_PTP_MIN,  PAMB_PTP_MAX, &
     &            PAMB__XBAND,   PAMB__SBAND
      PARAMETER  ( PAMB_VRB_MIN  = 0, PAMB_VRB_MAX  = 4  )
      PARAMETER  ( PAMB_BAND_MIN = 1, PAMB_BAND_MAX = 2  )
      PARAMETER  ( PAMB_PTP_MIN  = 1, PAMB_PTP_MAX  = 19 )
      PARAMETER  ( PAMB__XBAND   = 1, PAMB__SBAND   = 2  )
      CHARACTER    BAND_STR(PAMB_BAND_MAX)*6, BAND_ABR(PAMB_BAND_MAX)*1, &
     &              PTP_STR(PAMB_PTP_MAX)*17,  PTP_ABR(PAMB_PTP_MAX)*3
      DATA       ( BAND_STR(NPAMB$ARG), BAND_ABR(NPAMB$ARG), &
     &                                  NPAMB$ARG=1,PAMB_BAND_MAX ) &
     &           / &
     &             'X-band', 'X', &
     &             'S-band', 'S' &
     &           /
      DATA       ( PTP_STR(NPAMB$ARG),  PTP_ABR(NPAMB$ARG), &
     &                                  NPAMB$ARG=1,PAMB_PTP_MAX ) &
     &           / &
     &             'resid_gr         ', 'Ggg', &
     &             'resid_ph+pg_iono ', 'Ppg', &
     &             'B1 phase         ', 'B1p', &
     &             'B2 phase         ', 'B2p', &
     &             'resid_ph+pp_iono ', 'Ppp', &
     &             'Pion_Gxs         ', 'PGi', &
     &             'Pion_Pxs         ', 'PPi', &
     &             'Pion_Pxs-Gxs     ', 'PDi', &
     &             'Raw_ambiguity    ', 'Pra', &
     &             'Gr-del misclosure', 'Gdc', &
     &             'Ph-del misclosure', 'Pdc', &
     &             'Ambig. misclosure', 'Amc', &
     &             'Phase  misclosure', 'Phc', &
     &             'Ppg misclosure   ', 'Pgc', &
     &             'S1 phase         ', 'S1p', &
     &             'S2 phase         ', 'S2p', &
     &             'S3 phase         ', 'S3p', &
     &             'S4 phase         ', 'S4p', &
     &             'Gr-Ph difference ', 'GPd' &
!!     #             'APA-value-1      ', 'AP1',
!!     #             'APA-value-2      ', 'AP2',
!!     #             'APA-value-3      ', 'AP3',
!!     #             'APA-value-4      ', 'AP4'
     &           /
!
        INTEGER*4    PSL__ALL, PSL__GDL, PSL__PDL, PSL__MIN, PSL__MAX
        PARAMETER  ( PSL__ALL = 1        )  ! all data
        PARAMETER  ( PSL__GDL = 2        )  ! good for group delay solution
        PARAMETER  ( PSL__PDL = 3        )  ! good for group delay solution
        PARAMETER  ( PSL__MIN = PSL__ALL )  ! min code
        PARAMETER  ( PSL__MAX = PSL__PDL )  ! max code
        CHARACTER  PSL_STR(PSL__MAX)*22
        DATA       ( PSL_STR(NPAMB$ARG), NPAMB$ARG=PSL__MIN,PSL__MAX ) &
     &           / &
     &             'Plot all data         ', &
     &             'Plot good group delays', &
     &             'Plot good phase delays' &
     &           /
!
      INTEGER*4  PARU_MPRG
      PARAMETER  ( PARU_MPRG = 4 )
!
      INTEGER*2     OBORG_XBAND__BIT,  OBORG_SBAND__BIT, &
     &             OBSBAS_XBAND__BIT, OBSBAS_SBAND__BIT
      PARAMETER  ( OBORG_XBAND__BIT  = 1 )
      PARAMETER  ( OBORG_SBAND__BIT  = 2 )
      PARAMETER  ( OBSBAS_XBAND__BIT = 3 )
      PARAMETER  ( OBSBAS_SBAND__BIT = 4 )
!
      INTEGER*4     LEN_AOB
      PARAMETER   ( LEN_AOB = 272 )
      TYPE      AOB__STRU
         REAL*8     HORN_COR
!
         REAL*8     FREQPHA_X
         REAL*8     GRFREQ_X
         REAL*8     PHFREQ_X
         REAL*8     TAU_GR_X
         REAL*8     PHAS_X
         REAL*8     TAU_GR_ERR_X
         REAL*8     TAU_PH_ERR_X
         REAL*8     SNR_X
         REAL*8     AMPL_X
         REAL*8     GRAMB_SP_X
         REAL*8     DNB_X
         REAL*8     DNBER_X
         INTEGER*4  NGRAMB_X
         INTEGER*4  NPHAMB_X
	 CHARACTER  FRINGE_X_FINAM*16
!
         REAL*8     FREQPHA_S
         REAL*8     GRFREQ_S
         REAL*8     PHFREQ_S
         REAL*8     TAU_GR_S
         REAL*8     PHAS_S
         REAL*8     TAU_GR_ERR_S
         REAL*8     TAU_PH_ERR_S
         REAL*8     SNR_S
         REAL*8     AMPL_S
         REAL*8     GRAMB_SP_S
         REAL*8     DNB_S
         REAL*8     DNBER_S
         REAL*8     ROBS_S
         REAL*8     RERR_S
         INTEGER*4  NGRAMB_S
         INTEGER*4  NPHAMB_S
         INTEGER*2  LQUAL_S
         INTEGER*2  FILL_AOB_1
         INTEGER*2  FILL_AOB_2
         INTEGER*2  FILL_AOB_3
	 CHARACTER  FRINGE_S_FINAM*16
!
      END TYPE  AOB__STRU ! AOB__STRU !
      INTEGER*4    ML_AOB
      PARAMETER  ( ML_AOB = LEN_AOB )
!
      INTEGER*4     LEN_APA, M_APA
      PARAMETER   ( M_APA   = 4         )
      PARAMETER   ( LEN_APA = 8*M_APA*2 )
      TYPE      APA__STRU
         REAL*8     X_PARAM(M_APA)
         REAL*8     S_PARAM(M_APA)
      END TYPE  APA__STRU ! APA__STRU !
      INTEGER*4    ML_APA
      PARAMETER  ( ML_APA = LEN_APA )
!
      TYPE      OBSI__STRU
         CHARACTER*8  STA1_NAM
         CHARACTER*8  STA2_NAM
         CHARACTER*8  SOUR_NAM
         INTEGER*4    IDT
         REAL*8       UTC
      END TYPE  OBSI__STRU ! OBSI__STRU !
!
      TYPE      PAMBI__STRU
         REAL*8     RES_PX_GXS
         REAL*8     ERR_PX_GXS
         REAL*8     RES_PS_GXS
         REAL*8     ERR_PS_GXS
         REAL*8     RES_P_PXS
         REAL*8     ERR_P_PXS
         REAL*8     TAU_CA     ! Theoretical TAU corrected for adjustment
         REAL*8     PSF_DEL    ! Postfit residual of delay
!
         INTEGER*4  STATUS_X
         INTEGER*4  STATUS_S
         INTEGER*4  NPHAMB_X
         INTEGER*4  NPHAMB_S
      END TYPE  PAMBI__STRU ! PAMBI__STRU !
      INTEGER*4    ML_PAM
      PARAMETER  ( ML_PAM = 80 )
!CCCCC
!
      INTEGER*4  PAR__CTYP, PAR__ITYP, PAR__RTYP, PAR__UTYP, &
     &           PAR__INCOMPLETE, PAR__COMPLETE
      PARAMETER  ( PAR__CTYP       = 101 )
      PARAMETER  ( PAR__ITYP       = 102 )
      PARAMETER  ( PAR__RTYP       = 103 )
      PARAMETER  ( PAR__UTYP       = 104 )
      PARAMETER  ( PAR__INCOMPLETE = 201 )
      PARAMETER  ( PAR__COMPLETE   = 202 )
!
      INTEGER*4  MPAR_FUN, M_KWD, M_LKW, M_VAL, L_VAL, L_IVL, L_RVL
      PARAMETER  ( MPAR_FUN = 18 ) ! The number of PAR-functiuons
      PARAMETER  ( M_KWD    = 13 ) ! The max number of keywords in procedure
      PARAMETER  ( M_LKW    = 41 ) ! Total number of supported keywords
      PARAMETER  ( M_VAL    = 20 ) ! max number of different values for
!                                  ! CHARACTER keywords
      PARAMETER  ( L_VAL    = 36 ) ! Total number of supported CHARACTER values
!                                  ! for keywords
      PARAMETER  ( L_IVL    =  8 ) ! Total number of supported INTEGER*4
!                                  ! restrictions
      PARAMETER  ( L_RVL    =  8 ) ! Total number of supported REAL*8
!                                  ! restrictions
!
      CHARACTER  PAR_FUN(MPAR_FUN)*12
      INTEGER*4  PAR_NUK(MPAR_FUN), PAR_SHK(M_KWD,MPAR_FUN)
      INTEGER*4  N$PAMB, N1$PAMB, N2$PAMB
      DATA ( PAR_FUN(N1$PAMB), PAR_NUK(N1$PAMB), &
     &       ( PAR_SHK(N2$PAMB,N1$PAMB), N2$PAMB=1,M_KWD), &
     &       N1$PAMB=1,MPAR_FUN ) &
     &     / &  !  name           N     1   2   3   4   5   6   7   8   9
     & &        !--------------------------------------------------------
     &        'INITIALIZE  ',  4,   19, 20, 21, 11,  0,  0,  0,  0,  0, &   !  1
     &                               0,  0,  0,  0, &
     &        'SET_SOLTYP  ',  3,    1,  4, 38,  0,  0,  0,  0,  0,  0, &   !  2
     &                               0,  0,  0,  0, &
     &        'CLOPA       ',  2,    2,  4,  0,  0,  0,  0,  0,  0,  0, &   !  3
     &                               0,  0,  0,  0, &
     &        'MIXOB       ',  0,    0,  0,  0,  0,  0,  0,  0,  0,  0, &   !  4
     &                               0,  0,  0,  0, &
     &        'OSCRO       ',  1,    2,  0,  0,  0,  0,  0,  0,  0,  0, &   !  5
     &                               0,  0,  0,  0, &
     &        'SAVE        ',  4,    3, 19, 20, 41,  0,  0,  0,  0,  0, &   !  6
     &                               0,  0,  0,  0, &
     &        'UPWEI       ',  7,    7,  8,  9, 10, 11, 12,  4,  0,  0, &   !  7
     &                               0,  0,  0,  0, &
     &        'ELIM        ',  7,    7, 13, 14, 15, 16,  4, 22,  0,  0, &   !  8
     &                               0,  0,  0,  0, &
     &        'MILE        ',  9,    7, 13, 14, 15, 16,  4, 17, 18, 22, &   !  9
     &                               0,  0,  0,  0, &
     &        'IONO_OBSER  ',  0,    0,  0,  0,  0,  0,  0,  0,  0,  0, &   ! 10
     &                               0,  0,  0,  0, &
     &        'SCATIE      ',  0,    0,  0,  0,  0,  0,  0,  0,  0,  0, &   ! 11
     &                               0,  0,  0,  0, &
     &        'STATIONS    ',  2,   35, 36,  0,  0,  0,  0,  0,  0,  0, &   ! 12
     &                               0,  0,  0,  0, &
     &        'SCADAM      ', 13,    4, 23, 24, 25, 26, 27, 28, 29, 30, &   ! 13
     &                              31, 32, 33, 34, &
     &        'FREEZE_AMB  ',  1,    3,  0,  0,  0,  0,  0,  0,  0,  0, &   ! 14
     &                               0,  0,  0,  0, &
     &        'UNFREEZE_AMB',  1,    3,  0,  0,  0,  0,  0,  0,  0,  0, &   ! 15
     &                               0,  0,  0,  0, &
     &        'UPDATE_DB   ',  1,   37,  0,  0,  0,  0,  0,  0,  0,  0, &   ! 16
     &                               0,  0,  0,  0, &
     &        'GROUP_AMB   ',  4,    4, 37, 39, 40,  0,  0,  0,  0,  0, &   ! 17
     &                               0,  0,  0,  0, &
     &        'SET_GR_WEI  ',  1,   11,  0,  0,  0,  0,  0,  0,  0,  0, &   ! 18
     &                               0,  0,  0,  0  &
     &     /
!
      CHARACTER  PAR_KWD(M_LKW)*12
      INTEGER*4  PAR_NUV(M_LKW), PAR_SHV(M_VAL,M_LKW)
      DATA ( PAR_KWD(N1$PAMB), PAR_NUV(N1$PAMB), &
     &       ( PAR_SHV(N2$PAMB, N1$PAMB), N2$PAMB = 1,M_VAL ), &
     &         N1$PAMB=1,M_LKW ) &
     &     / &
     &         'SOLTYP      ', 20,   1,  2,  3,  4,  5,  6,  7,  8,  9, 10, &  !  1
     &                              11, 12, 13, 14, 15, 16, 17, 18, 19, 34, &
     &         'BAND        ',  2,  20, 21,  0,  0,  0,  0,  0,  0,  0,  0, &  !  2
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'BAND        ',  3,  20, 21, 22,  0,  0,  0,  0,  0,  0,  0, &  !  3
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'VERBOSITY   ', -1,   1,  8,  0,  0,  0,  0,  0,  0,  0,  0, &  !  4
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'IONO_CALC   ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  !  5
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'CUTOFF      ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  !  6
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'TYPE        ',  2,  25, 26,  0,  0,  0,  0,  0,  0,  0,  0, &  !  7
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'MAX_ITER    ', -1,   3,  4,  0,  0,  0,  0,  0,  0,  0,  0, &  !  8
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'TOLERANCE   ', -2,   3,  4,  0,  0,  0,  0,  0,  0,  0,  0, &  !  9
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'INITIALIZE  ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 10
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'INIT_WEI    ', -2,   1,  5,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 11
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'FLOOR       ', -2,   1,  6,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 12
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'CUTOFF      ', -2,   1,  0,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 13
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'THRESHOLD   ', -2,   8,  5,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 14
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'QUALCODE    ', -1,   3,  5,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 15
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'ACCELERATION', -1,   3,  6,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 16
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'AMB_RESOLVE ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 17
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'AMB_IONO    ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 18
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'AMBIGUITY   ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 19
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'SUPPRESSION ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 20
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'NEW_WEIGHTS ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 21
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'MAX_SIGMA   ', -2,   1,  0,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 22
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'XGR_LIMIT   ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 23
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'SGR_LIMIT   ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 24
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'XPH_LIMIT   ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 25
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'SPH_LIMIT   ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 26
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'DEFRAG_LIMIT', -2,   1,  4,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 27
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'MAX_ARF_SIG ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 28
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'FROZEN_TRANZ', -2,   1,  7,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 29
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'ARF_FLOOR   ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 30
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'SPL_SPAN    ', -2,   1,  7,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 31
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'SPL_CONSTR  ', -2,   1,  2,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 32
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'MSC_CHECK   ',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 33
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'ARF_TYPE    ', -1,   1,  8,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 34
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'PARTICIPATED', -3,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 35
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'FIDUCIAL    ', -3,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 36
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'OPERATION   ',  5,  27, 28, 29, 33, 35,  0,  0,  0,  0,  0, &  ! 37
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'SUP_TRANSFER',  2,  23, 24,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 38
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'DATA_USE    ',  3,  30, 31, 32,  0,  0,  0,  0,  0,  0,  0, &  ! 39
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'MARGIN      ', -2,   1,  4,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 40
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
     &         'VERSION     ',  2,  27, 36,  0,  0,  0,  0,  0,  0,  0,  0, &  ! 41
     &                               0,  0,  0,  0,  0,  0,  0,  0,  0,  0  &
     &     /
      CHARACTER  PAR_VAL(L_VAL)*12
      DATA ( PAR_VAL(N$PAMB), N$PAMB=1, L_VAL ) &
     &     / &
     &         'GRPRAT      ', &   !  1
     &         'PHSRAT      ', &   !  2
     &         'SNBRAT      ', &   !  3
     &         'GRPONL      ', &   !  4
     &         'PHSONL      ', &   !  5
     &         'SNBONL      ', &   !  6
     &         'RATONL      ', &   !  7
     &         'G_GXS       ', &   !  8
     &         'PX_GXS      ', &   !  9
     &         'PS_GXS      ', &   ! 10
     &         'PX_GX       ', &   ! 11
     &         'PX_GS       ', &   ! 12
     &         'PS_GX       ', &   ! 13
     &         'PS_GS       ', &   ! 14
     &         'P_PXS       ', &   ! 15
     &         'GX          ', &   ! 16
     &         'GS          ', &   ! 17
     &         'PX          ', &   ! 18
     &         'PS          ', &   ! 19
     &         'X-BAND      ', &   ! 20
     &         'S-BAND      ', &   ! 21
     &         'BOTH-BAND   ', &   ! 22
     &         'YES         ', &   ! 23
     &         'NO          ', &   ! 24
     &         'BASELINE    ', &   ! 25
     &         'GLOBAL      ', &   ! 26
     &         'THIS_VERSION', &   ! 27
     &         'NEXT_VERSION', &   ! 28
     &         'MAKE_SUPER  ', &   ! 29
     &         'GOOD_ONLY   ', &   ! 30
     &         'BAD_ONLY    ', &   ! 31
     &         'ALL_DATA    ', &   ! 32
     &         'RESTORE     ', &   ! 33
     &         'FUSED       ', &   ! 34
     &         'NEXT_IF_1ST ', &   ! 35
     &         'NULL_VERSION'  &   ! 36
     &     /
      INTEGER*4  PAR_IVL(L_IVL)
      DATA ( PAR_IVL(N$PAMB), N$PAMB=1,L_IVL ) &
     &     / &
     &         0, &   ! 1
     &         2, &   ! 2
     &         1, &   ! 3
     &       128, &   ! 4
     &         9, &   ! 5
     &     32000, &   ! 6
     &        24, &   ! 7
     &         4  &   ! 8
     &     /
      REAL*8     PAR_RVL(L_RVL)
      DATA ( PAR_RVL(N$PAMB), N$PAMB=1,L_RVL ) &
     &     / &
     &           0.0D0, &     ! 1
     &           1.D8, &      ! 2
     &           0.0001D0, &  ! 3
     &           1.0D0, &     ! 4
     &           1.0D6, &     ! 5
     &         300.0D0, &     ! 6
     &          50.0D0, &     ! 7
     &          -1.0D6  &     ! 8
     &     /
!CCCC
      INTEGER*4    MPAR_PRC
      PARAMETER  ( MPAR_PRC = 64 )
      TYPE      PAR__STRU
         INTEGER*4  N_PRC
         INTEGER*4  IFUN(MPAR_PRC)
         INTEGER*4  NKWD(MPAR_PRC)
         INTEGER*4  IKWD(M_KWD,MPAR_PRC)
         CHARACTER   KWD(M_KWD,MPAR_PRC)*12
         CHARACTER  CKWD(M_KWD,MPAR_PRC)*384
         REAL*8     RKWD(M_KWD,MPAR_PRC)
         INTEGER*4  ITYP(M_KWD,MPAR_PRC)
         INTEGER*4  ISTS(MPAR_PRC)
         CHARACTER  FINAM*256
         INTEGER*4  STATUS
      END TYPE  PAR__STRU ! PAR__STRU /
!
! --- Minimal and maximal values of SCAINF plotting altermnatives
!
      INTEGER*4    SCAINF_PLOT_MIN, SCAINF_PLOT_MAX, &
     &             ARFTYPE__MIN,    ARFTYPE__MAX
      PARAMETER  ( SCAINF_PLOT_MIN = 0 )
      PARAMETER  ( SCAINF_PLOT_MAX = 1 )
      PARAMETER  ( ARFTYPE__MIN    = 1 )
      PARAMETER  ( ARFTYPE__MAX    = 4 )
!
      INTEGER*4  ARFTYPE__COMM, ARFTYPE__EXPR, &
     &           ARFTYPE__PXGS, ARFTYPE__PSGS
      PARAMETER  ( ARFTYPE__COMM = 1 )
      PARAMETER  ( ARFTYPE__EXPR = 2 )
      PARAMETER  ( ARFTYPE__PXGS = 3 )
      PARAMETER  ( ARFTYPE__PSGS = 4 )
!
      CHARACTER  ARFNAME(4)*8
      DATA  ( ARFNAME(NPAMB$ARG), NPAMB$ARG=ARFTYPE__MIN, ARFTYPE__MAX ) &
     &      / &
     &        'ARF-Comm', &
     &        'ARF-Expr', &
     &        'ARF-PxGs', &
     &        'ARF-PsGs' &
     &      /
!
      TYPE      SCAINF__STRU
         REAL*8     XGR_LIM  ! It is the first element
         REAL*8     SGR_LIM
         REAL*8     XPH_LIM
         REAL*8     SPH_LIM
         REAL*8     DEFRG    ! Limit (in turns) for first search
         REAL*8     ARFMS    ! Limit (in turns) for max ARF uncertainty
         REAL*8     FRZTR    ! Limit in sec for frozen transitions
         REAL*8     ARFFLO   ! Floor for ARF sigma
         REAL*8     SPL_SPAN ! Time span for ARF spline
         REAL*8     SPL_CNST ! Value of ARF spline constraint
!
         LOGICAL*4  MSC_CONTROL
         LOGICAL*4  P_STA(MG_STA)
         INTEGER*4  FID_STA
!
         INTEGER*4  ARF_TYPE
         INTEGER*4  PLOT_INI
         INTEGER*4  PLOT_FIN
!C
         REAL*8     TIM_SCA(MG_SCA)
!
         REAL*8     TAU_SCA(MG_STA,MG_SCA)
         REAL*8     ION_SCA(MG_STA,MG_SCA)
         REAL*8     XPA_SCA(MG_STA,MG_SCA)
         REAL*8     SPA_SCA(MG_STA,MG_SCA)
!
         REAL*8     TAU_SCA_SIG(MG_STA,MG_SCA)
         REAL*8     ION_SCA_SIG(MG_STA,MG_SCA)
         REAL*8     XPA_SCA_SIG(MG_STA,MG_SCA)
         REAL*8     SPA_SCA_SIG(MG_STA,MG_SCA)
!C
         INTEGER*4  IUSE(2,MG_STA)
         INTEGER*4  ISUC(2,MG_STA)
         REAL*8     SHF(2,MG_STA)
         REAL*8     WRMS(2,MG_STA)
!
         LOGICAL*1  USE_SCA(MG_STA,MG_SCA)
         LOGICAL*1  SUC_SCA(2,MG_STA,MG_SCA)
         LOGICAL*1  UPD_OBS(2,MAX_OBS)
!
         INTEGER*4  SCA_TOT(MG_STA)
!
         INTEGER*4  BAS_TOT(MG_BAS)
         INTEGER*4  BAS_USE(2,MG_BAS)
         INTEGER*4  BAS_SUC(2,MG_BAS)
!
         INTEGER*4  LAST_FIELD  ! really the last field in the data structure
      END TYPE  SCAINF__STRU ! SCAINF__STRU /
!
! <<<<< end of INCLUDE-BLOCK  pamb.i
!
