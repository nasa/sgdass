!
! >>>>> INCLUDE-BLOCK with data additional data structure.
!
!CCCCCCC
!       This common block and constants makes extension of socom.i
!       They were not included to socom due to the reason that any changes
!       of socom are very painful since they required remaking all superfiles.
!       So this trick were implemented as temporary measure. It is assumed
!       that after reading socom.i special routine SOCOM_EXT will be called
!       which will fill fields of socom_plus
!
!       Common block solve.i should precede to socom_plus. The following
!       constans from solve.i are used:
!
!       MAX_STA_ARC
!       MAX4_BRK
!       MAX4_EOP
!       MAX_CLK
!       MAX_ATM
!       MAX_STA
!
!       and definitions
!
!       HPESOL__TYPE
!       SPESOL__TYPE
!
! pet  2012.03.12  icreased its size from 5507268 to 5511364 
! pet  2020.09.09  icreased its size from 5690052 to 5864772
! pet  2021.12.31  icreased its size to   9797348  
! pet  2023.07.31  icreased its size to   9819772
!
!       socom_plus.i  21-APR-97  v4.51 (c)  L. Petrov  --  2007.10.25_09:04:16
!
!CCCCCCC
        INTEGER*4  SOCOM_PLUS_FIRST  ! The first field. Keeps status of
!                                    ! socom_plus
        INTEGER*4  SOCOM_PLUS_LAST   ! Filler for the last field
        INTEGER*4  SOCOM_LENGTH      ! The lentgh of comon area (in bytes)
        PARAMETER  ( SOCOM_LENGTH = 9819772 )
!
        INTEGER*4  MAX4_STA, MAX4_CLO, MAX4_ATM, MIN_NSEG
        PARAMETER  ( MAX4_STA = MAX_ARC_STA )
        PARAMETER  ( MAX4_CLO = MAX_CLK     )
        PARAMETER  ( MAX4_ATM = MAX_ATM     )
        PARAMETER  ( MIN_NSEG = 3           ) ! min number of segments when
!                                             ! fast mode is eligibile
        LOGICAL*4  UNF_CLO, UNF_ATM, UNF_EOP ! Flags of uniformity
!                  ! parametrizations clocks, atmosphere, EOP. Nonuniformity
!                  ! means that for different stations (or components for EOP)
!                  ! the number of intervals is different
        LOGICAL*4  FAST_ELIG  !  eligibility of fast mode
!
        INTEGER*4  NUM_CLO, NUM_ATM, NUM_EOP, &
     &             NUM_BRK(MAX4_STA), &   !  Num. of clock breaks for each station
     &             NPL_CLO             !  Max degree of polynom for clocks
!
        REAL*8     CLO_INTERVAL       !  length of clock    interval (in days)
        REAL*8     ATM_INTERVAL       !  length of atmosph. interval (in days)
        REAL*8     EOP_INTERVAL       !  length of EOP interval (in days)
        REAL*8     JDATE_CLO(MAX4_CLO)  ! Array of epoch for clocks
        REAL*8     JDATE_ATM(MAX4_ATM)  ! Array of epoch for atmosphere
        REAL*8     JDATE_EOP(MAX4_EOP)  ! Array of boundaries of intervals for
!                                       ! segmented EOP (in Julian days)
        REAL*8     JDATE_BRK(MAX4_BRK,MAX4_STA) ! Two-dimensional array of
!                                       ! clock breacks for all stations
        INTEGER*4  STATUS_HFE
        INTEGER*4  NUM_HFE, &              ! Actually used number of hf-EOP epoch
     &             IXMN_HFE             ! Node counter
        REAL*8     EPOCH_HFE(MAX4_HFE), &  ! Epochs for interpolation hf-EOP
     &             UT_HFE(MAX4_HFE), &     ! array values hf-UT1 in nodes
     &             UT_SPL(MAX4_HFE), &     ! array spline coefficients for hf-UT1
     &             XP_HFE(MAX4_HFE), &     ! array values hf-Xp in nodes
     &             XP_SPL(MAX4_HFE), &     ! array spline coefficients for hf-Xp
     &             YP_HFE(MAX4_HFE), &     ! array values hf-Yp in nodes
     &             YP_SPL(MAX4_HFE), &     ! array spline coefficients for hf-Yp
     &             WORK_HFE(MAX4_HFE)   ! working array for making spline
!
! ----- Two arrrays of depositary for bit fields for baseline dependent clocks
! ----- for both phase delay and group delay solution
!
        INTEGER*2  BASCL_G(ARC_STA_BIT_WORDS,MAX_ARC_STA)
        INTEGER*2  BASCL_P(ARC_STA_BIT_WORDS,MAX_ARC_STA)
	INTEGER*2  BASCL_IND(MAX_ARC_STA,MAX_ARC_STA)
	INTEGER*2  STAR_IND(MAX_ARC_SRC,2)
	INTEGER*2  PROP_IND(MAX_ARC_SRC,2)
!
! ----- Parameters of the status of the SOCOM_PLUS
!
        INTEGER*4    SPL__UNDF, SPL__INIT, SPL__DONE
        PARAMETER  ( SPL__UNDF = -1037984562 ) ! Status: undefined
        PARAMETER  ( SPL__INIT =  1 ) ! Status: initialized
        PARAMETER  ( SPL__DONE =  2 ) ! Status: done (all fields are filled )
!
        INTEGER*4    HFE__UNDF, HFE__NONE, HFE__DONE
        PARAMETER  ( HFE__UNDF =  -1037984562 ) ! Status: undefined
        PARAMETER  ( HFE__NONE =  1 ) ! Status: disabled
        PARAMETER  ( HFE__DONE =  2 ) ! Status: done
!
	LOGICAL*4  FL_HPESOL, FL_SPESOL, FL_EERM, FL_EHEO
	TYPE ( HPESOL__TYPE ) HPESOL(MAX_STA,M__HPE)
	TYPE ( SPESOL__TYPE ) SPESOL(M__SPE)
	TYPE ( ERM__TYPE    ) EERM
	TYPE ( EHEO__TYPE   ) HEOSOL(M__EHEO)
!
	INTEGER*4  IND_EERM_NOD(3), & ! index of the pivotal element for ERM
     & 	           EERM_OVR(3)        ! overdraft parameter. It tells how many
!                                     ! node changes occured during a session
        COMMON   / SOCOM_PLUS /      &
     &             SOCOM_PLUS_FIRST, &
!
     &             UNF_CLO,   &
     &             UNF_ATM,   &
     &             UNF_EOP,   &
!
     &             FAST_ELIG, &
!
     &             NUM_CLO,   &
     &             NUM_ATM,   &
     &             NUM_EOP,   &
     &             NUM_BRK,   &
     &             NPL_CLO,   &
!
!
     &             CLO_INTERVAL, &
     &             ATM_INTERVAL, &
     &             EOP_INTERVAL, &
!
     &             JDATE_CLO,  &
     &             JDATE_ATM,  &
     &             JDATE_EOP,  &
     &             JDATE_BRK,  &
!
     &             STATUS_HFE, &
     &             NUM_HFE,    &
     &             EPOCH_HFE,  &
     &             UT_HFE,     &
     &             UT_SPL,     &
     &             XP_HFE,     &
     &             XP_SPL,     &
     &             YP_HFE,     &
     &             YP_SPL,     &
     &             WORK_HFE,   &
     &             IXMN_HFE,   &
!
     &             BASCL_G,    &
     &             BASCL_P,    &
     &             BASCL_IND,  &
     &             STAR_IND,   &
     &             PROP_IND,   &
!
     &             FL_HPESOL, FL_SPESOL, FL_EERM, FL_EHEO, &
     &             IND_EERM_NOD, EERM_OVR, &
     &             HPESOL, SPESOL, EERM, HEOSOL, &
!
     &             SOCOM_PLUS_LAST
!
! >>>>>
