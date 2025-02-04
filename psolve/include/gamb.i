!
! >>>>> INCLUDE-BLOCK with descriptions data structures used for group delay
!       ambiguity resolution algorithm incorporated to SOLVE
!
!       include block solve.i SHOULD BE declared before!
!
!       gamb.i  29-JUL-97  v1.52  (c)  L. Petrov  --  20-JUL-2000 17:14:55
!
      INTEGER*4    MG_SOU, MG_STA, MG_BAS, MG_TRI, MG_OBS, M_FAM
      PARAMETER  ( MG_SOU = MAX_ARC_SRC )
      PARAMETER  ( MG_STA = MAX_ARC_STA )
      PARAMETER  ( MG_BAS = (MG_STA*(MG_STA-1))/2 )
      PARAMETER  ( MG_TRI = (MG_STA*(MG_STA-1)*(MG_STA-2))/6 )
      PARAMETER  ( MG_OBS = MAX_OBS )
      PARAMETER  ( M_FAM  = 8 )
      INTEGER*4  OBS__SIZE, GAMB__SIZE
      PARAMETER  (  OBS__SIZE =  8534628 ) ! Size of  OBS structure (bytes)
      PARAMETER  ( GAMB__SIZE = 57551108 ) ! Size of GAMB structure (bytes)
      INTEGER*4   GAMB__UNF, GAMB__GET, GAMB__DONE, GAMB__ERROR, &
     &            GAMB__IONO_0, GAMB__IONO_1,  GAMB__IONO_2, &
     &            GAMB__BAS_OK, GAMB__BAS_SUS, GAMB__BAS_BAD, &
     &            GAMB__NZ_OK,  GAMB__NZ_SUS,  GAMB__NZ_BAD, &
     &            GAMB__CLS_OK, GAMB__CLS_SUS, &
     &            GAMB__WHL_OK, GAMB__WHL_SUS, GAMB__WHL_BAD, GAMB__WHL_FAI, &
     &            GAMB__STS_OK, GAMB__STS_SUS, GAMB__STS_BAD, GAMB__STS_FAI
      PARAMETER  ( GAMB__UNF       = -1   )
      PARAMETER  ( GAMB__GET       =  101 )
      PARAMETER  ( GAMB__DONE      =  102 )
      PARAMETER  ( GAMB__ERROR     =  103 )
      PARAMETER  ( GAMB__IONO_0    =  200 )
      PARAMETER  ( GAMB__IONO_1    =  201 )
      PARAMETER  ( GAMB__IONO_2    =  202 )
      PARAMETER  ( GAMB__BAS_OK    =  301 )
      PARAMETER  ( GAMB__BAS_SUS   =  302 )
      PARAMETER  ( GAMB__BAS_BAD   =  303 )
      PARAMETER  ( GAMB__NZ_OK     =  401 )
      PARAMETER  ( GAMB__NZ_SUS    =  402 )
      PARAMETER  ( GAMB__NZ_BAD    =  403 )
      PARAMETER  ( GAMB__CLS_OK    =  501 )
      PARAMETER  ( GAMB__CLS_SUS   =  502 )
      PARAMETER  ( GAMB__WHL_OK    =  601 )
      PARAMETER  ( GAMB__WHL_SUS   =  602 )
      PARAMETER  ( GAMB__WHL_BAD   =  603 )
      PARAMETER  ( GAMB__WHL_FAI   =  604 )
      PARAMETER  ( GAMB__STS_OK    =  701 )
      PARAMETER  ( GAMB__STS_SUS   =  702 )
      PARAMETER  ( GAMB__STS_BAD   =  703 )
      PARAMETER  ( GAMB__STS_FAI   =  704 )
!
! --- Constants of solution criteries
!
      REAL*8       GAMB__TOL_SUS, GAMB__TOL_BAD, &
     &             GAMB__RMS_SUS, GAMB__RMS_BAD, GAMB__RMS_FAI, &
     &             GAMB__CLT_SUS
      PARAMETER  ( GAMB__TOL_SUS = 0.80 ) ! Tolerance criteria. If less than
!                                         ! this value observations at the
!                                         ! baseline remained -- session marked
!                                         ! as "suspicious"
      PARAMETER  ( GAMB__TOL_BAD = 0.50 ) ! The same but session marked "bad"
!
      PARAMETER  ( GAMB__RMS_SUS = 0.10 ) ! RMS criteria. If RMS of whole
!                                         ! GAMB solution exceeded this value*
!                                         ! ambiguity_spacing then session
!                                         ! marked as "suspicious"
      PARAMETER  ( GAMB__RMS_BAD = 0.30 ) ! The same but session marked "bad"
      PARAMETER  ( GAMB__RMS_FAI = 0.80 ) ! The same but session marked "failure
      PARAMETER  ( GAMB__CLT_SUS = 0.30 ) ! Toleratnce for closure test. If
!                                         ! closure for clock shift exceed
!                                         ! GAMB__CLT_SUS*GAMBC then session
!                                         ! marked as suspicious
      INTEGER*4    GAMB__SHORT
      PARAMETER  ( GAMB__SHORT = 24   )   ! Minimal number of observations to
!                                         ! apply GAMB_TOL_SUS criteria
!
      REAL*8       ETAU_DEF, EFRE_DEF
      PARAMETER  ( ETAU_DEF = 8.D-11 ) ! Dafault formal error of group delay
      PARAMETER  ( EFRE_DEF = 1.D-14 ) ! Dafault formal error of fringe rate
      INTEGER*4  X__BAND, S__BAND
      PARAMETER  ( X__BAND = 1 )
      PARAMETER  ( S__BAND = 2 )
!
      CHARACTER  OBS__IDE*4, GAMB__IDE*4
      PARAMETER  (  OBS__IDE = 'OBS ' )
      PARAMETER  ( GAMB__IDE = 'GAMB' )
!
      TYPE      OBS__STRU
          CHARACTER*4 FIRST_FIELD   ! Filler.
!
          INTEGER*4   NOBS
!
          INTEGER*4   L_SOU
          INTEGER*4   L_STA
          INTEGER*4   L_BAS
          INTEGER*4   LIS_SOU(MG_SOU)
          INTEGER*4   LIS_STA(MG_STA)
          INTEGER*4   LIS_BAS(MG_BAS)
!
          CHARACTER   C_SOU(MG_SOU)*8
          CHARACTER   C_STA(MG_STA)*8
!
          INTEGER*4   IBA(MG_OBS)
          INTEGER*4   ISO(MG_OBS)
          REAL*8      TT(MG_OBS)
!
          REAL*8      CUTOFF
          INTEGER*4   MINOBS
          INTEGER*4   IT
          REAL*8      TAVALL
          REAL*8      TLNALL
!
          INTEGER*4   STATUS_GET_X
          INTEGER*4   STATUS_GET_S
!
          INTEGER*4   STATUS_GAMB_X
          INTEGER*4   STATUS_GAMB_S
!
          INTEGER*4   NB_X
          INTEGER*4   NE_X
          INTEGER*4   NB_S
          INTEGER*4   NE_S
!
          INTEGER*4   IDB_X
          INTEGER*4   IDB_S
!
          INTEGER*4   LAST_FIELD   ! Filler
      END TYPE  OBS__STRU   !  OBS__STRU    !
!C
      TYPE      GAMB__STRU
          CHARACTER*4 FIRST_FIELD   ! Filler.
          CHARACTER   DBASE*16
          INTEGER*4   UOBS
!
          INTEGER*4   L_SOU
          INTEGER*4   L_STA
          INTEGER*4   L_BAS
          INTEGER*4   L_TRI
          INTEGER*4   LIS_SOU(MG_SOU)
          INTEGER*4   LIS_STA(MG_STA)
          INTEGER*4   LIS_BAS(MG_BAS)
          INTEGER*4   LIS_TRI(3,MG_TRI)
!
          INTEGER*4   L_FAM
          REAL*8      FAM_LIS(M_FAM)
          INTEGER*4   FAMBAS_LIS(M_FAM)
!
          INTEGER*4   K_SOU(MG_SOU)
          INTEGER*4   K_STA(MG_STA)
          INTEGER*4   K_BAS(MG_BAS)
!
          REAL*8      GAMB_SP     ! Group delay ambiguity constant from database
          REAL*8      PAMB_SP     ! Phase delay ambiguity constant from database
          REAL*8      FREQ_GR
          REAL*8      FREQ_RF
          REAL*8      FREQ_PH
!
          LOGICAL*1   USE(MG_OBS)
          INTEGER*2   OBS_IND(MG_OBS)
!
          REAL*8      GAMBC       ! Group delay ambiguity constant in use
!
          REAL*8      OCT(MG_OBS)
          REAL*8      OCF(MG_OBS)
          REAL*8      GION_TAU(MG_OBS)
          REAL*8      GION_FRE(MG_OBS)
!
          REAL*8      JMP(MG_OBS)
!
          INTEGER*4   IREF_STA
          REAL*8      OB
          REAL*8      SIG
          REAL*8      RMS
!
          REAL*8      SH_STA(MG_STA)
          REAL*8      DR_STA(MG_STA)
          REAL*8      SQ_STA(MG_STA)
!
          REAL*8      SH_BAS(MG_BAS)
          REAL*8      DR_BAS(MG_BAS)
          REAL*8      SQ_BAS(MG_BAS)
!
          REAL*8      AV_BAS(MG_BAS)
          REAL*8      DS_BAS(MG_BAS)
!
          INTEGER*4   NZ_BAS(MG_BAS)
          LOGICAL*4   SUC_BAS(MG_BAS)
!
          INTEGER*4   STATUS_GAMB
          INTEGER*4   STATUS_ION
          INTEGER*4   STATUS_NZ
          INTEGER*4   STATUS_BAS
          INTEGER*4   STATUS_CLS
          INTEGER*4   STATUS_WHL
          INTEGER*4   NEW_L_BAS
          INTEGER*4   NEW_NZ
!
          INTEGER*4   LAST_FIELD   ! Filler
      END TYPE  GAMB__STRU   !  GAMB__STRU    !
!
! <<<<< end of INCLUDE-BLOCK  gamb.i
!
