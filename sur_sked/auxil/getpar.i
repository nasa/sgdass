!
! >>>>> INCLUDE-BLOCK with descriptions of the data structures generated
!       and/or derived getpar utility for parsing Solve listings in spool
!       format
!
!       pet  2005.07.29  Rasied M_SOU from 3072 to 4096
!       pet  2006.10.03  Rasied M_SOU from 4096 to 8192
!       pet  2007.08.29  Added constants MA_STA and MA_BAS
!       pet  2010.05.03  Increased M_LST from 24*1024 to 32*1024
!       pet  2011.01.12  Rasied M_SOU from 8192 to 16384
!
!       getpar.i  06-MAR-2002 v 2.10 (c) L. Petrov  2007.09.07_16:29:50
!
      INTEGER*4  M_SES, M_SOU, M_STA, M_COMP, M_LSO, M_LST, M_BAS, &
     &           M_LS, M_TRP, M_HEAD_EOB, M_FLD_EOPS, MA_STA, MA_BAS
      PARAMETER  ( M_SES = 8192 )
      PARAMETER  ( M_SOU = 16384 )
      PARAMETER  ( M_STA =  256 )
      PARAMETER  ( M_COMP =   6 )
      PARAMETER  ( M_LSO = 192*1024  )
      PARAMETER  ( M_LST = 32*1024   )
      PARAMETER  ( M_BAS = M_SES*50  )
      PARAMETER  ( M_TRP = 2048*1024 )
      PARAMETER  ( M_LS = 512        )
      PARAMETER  ( M_HEAD_EOB = 1024 )
      PARAMETER  ( M_FLD_EOPS = 29   )
      PARAMETER  ( MA_STA = 32 )
      PARAMETER  ( MA_BAS = (MA_STA*(MA_STA+1))/2 )
      REAL*8       GTP__EPS
      PARAMETER  ( GTP__EPS = 1.D-20 )
      CHARACTER    EOPS__HELP_FILE*32
      PARAMETER  ( EOPS__HELP_FILE = 'eops_format.txt                 ' )
      INTEGER*1    BLANK__B1
      PARAMETER  ( BLANK__B1 = 32 )
!
! --- Status EOP bits
!
      INTEGER*2  XPL__GTP, YPL__GTP, U1__GTP, DPSI__GTP, DEPS__GTP, &
     &           XPR__GTP, YPR__GTP, UTR__GTP, NPAR__GTP
      PARAMETER  ( XPL__GTP  = 1 )
      PARAMETER  ( YPL__GTP  = 2 )
      PARAMETER  ( U1__GTP   = 3 )
      PARAMETER  ( DPSI__GTP = 4 )
      PARAMETER  ( DEPS__GTP = 5 )
      PARAMETER  ( XPR__GTP  = 6 )
      PARAMETER  ( YPR__GTP  = 7 )
      PARAMETER  ( UTR__GTP  = 8 )
      PARAMETER  ( NPAR__GTP = UTR__GTP )
!
      CHARACTER  SIG_STA*46, SIG_VEL*46,   SIG_SOU*46,  &
     &           SIG_EOP*46, SIG_NUT*46,   SIG_CRL*46,  &
     &           SIG_LSO*46, SIG_LST*46,   SIG_BAS*46,  &
     &           SIG_EOB*46, SIG_TRP*46,   SIG_RMS1*46, &
     &           SIG_EOP1*46, SIG_EOB1*46, SIG_RMS2*46
      PARAMETER ( SIG_STA = '# GETPAR_STA format version 1.0  of 2001.05.25' )
      PARAMETER ( SIG_VEL = '# GETPAR_VEL format version 1.0  of 2001.05.25' )
      PARAMETER ( SIG_SOU = '# GETPAR_SOU format version 1.0  of 2001.05.25' )
      PARAMETER ( SIG_EOP = '# GETPAR_EOP format version 2.1  of 2007.08.30' )
      PARAMETER ( SIG_EOP1= '# GETPAR_EOP format version 2.0  of 2002.05.31' )
      PARAMETER ( SIG_NUT = '# GETPAR_NUT format version 1.1  of 2002.06.05' )
      PARAMETER ( SIG_CRL = '# GETPAR_CRL format version 1.0  of 2001.05.25' )
      PARAMETER ( SIG_LSO = '# GETPAR_LSO format version 1.1  of 2001.12.23' )
      PARAMETER ( SIG_LST = '# GETPAR_LST format version 1.0  of 2001.05.25' )
      PARAMETER ( SIG_BAS = '# GETPAR_BAS format version 1.0  of 2001.05.25' )
      PARAMETER ( SIG_EOB1= '# GETPAR_EOB format version 2.0  of 2002.05.31' )
      PARAMETER ( SIG_EOB = '# GETPAR_EOB format version 2.1  of 2007.08.30' )
      PARAMETER ( SIG_TRP = '# GETPAR_TRP format version 1.0  of 2002.03.06' )
      PARAMETER ( SIG_RMS1= '# GETPAR_RMS format version 1.0  of 2001.05.25' )
      PARAMETER ( SIG_RMS2= '# GETPAR_RMS format version 2.0  of 2003.08.12' )
!
      TYPE      EOPS__CHAR
          byte sep_01 !    1  !
          CHARACTER  MJD*12   !  1   2-13     F12.6  days    Modified Julian
          byte sep_02 !   14  !     date of the TDT time tag
          CHARACTER  XPL_V*8  !  2  15-22     F8.6   arcsec  The estimate of
          byte sep_03 !   23  !     X pole coordinate
          CHARACTER  YPL_V*8  !  3  24-31     F8.6   arcsec  The estimate of
          byte sep_04 !   32  !     Y pole coordinate
          CHARACTER  U1_V*10  !  4  33-42     F10.7  sec     The UT1-UTC function
          byte sep_05 !   43  !
          CHARACTER  DPSI_V*8 !  5  44-51     F8.3   mas     Adjustment of the
          byte sep_06 !   52  !     nutation in longitude angle wrt IAU 1980
          CHARACTER  DEPS_V*8 !  6  53-60     F8.3   mas     Adjustment of the
          byte sep_07 !   61  !     nutation in obliquity wrt IAU 1980
          CHARACTER  XPL_E*8  !  7  62-69     F8.6   arcsec  Formal uncertainty
          byte sep_08 !   70  !     of X pole coordinate
          CHARACTER  YPL_E*8  !  8  71-78     F8.6   arcsec  Formal uncertainty
          byte sep_09 !   79  !     of Y pole coordinate
          CHARACTER  U1_E*9   !  9  80-88     F9.7   sec     Formal uncertainty
          byte sep_10 !   89  !     of UT1-UTC function
          CHARACTER  DPSI_E*7 ! 10  90-96     F7.3   mas     Formal uncertainty
          byte sep_11 !   97  !     of nutation in longitude angle
          CHARACTER  DEPS_E*7 ! 11  98-104    F7.3   mas     Formal uncertainty
          byte sep_12 !  105  !     of nutation in obliquity angle
          CHARACTER  WRMS*7   ! 12 106-112    F7.2   psec    Weighted root mean
          byte sep_13 !  113  !       square of postfit residual of the solution
          CHARACTER  C_XY*6   ! 13 114-119    F6.4   --      Correlation
          byte sep_14 !  120  !       between X-pole and Y-pole positions
          CHARACTER  C_XU*6   ! 14 121-126    F6.4   --      Correlation
          byte sep_15 !  127  !       between X-pole and UT1
          CHARACTER  C_YU*6   ! 15 128-133    F6.4   --      Correlation
          byte sep_16 !  134  !       between Y-pole and UT1
          CHARACTER  C_PE*6   ! 16 135-140    F6.4   --      Correlation
          byte sep_17 !  141  !       between nutation in obliquity and longitude
          CHARACTER  NOBS*6   ! 17 142-147    I6     --      Number of used
          byte sep_18 !  148  !       observations in the session
          CHARACTER  SCODE*6  ! 18 149-154    A6     --      IVS session code
          byte sep_19 !  155  !
          CHARACTER  DURA*5   ! 19 156-160    F5.2   hours   Session duration
          byte sep_20 !  161  !
          CHARACTER  XPR_V*9  ! 20 162-170    F9.6   asc/day Estimate of rate
          byte sep_21 !  171  !       change of X pole coordinate
          CHARACTER  YPR_V*9  ! 21 172-180    F9.6   asc/day Estimate of rate
          byte sep_22 !  171  !       change of Y pole coordinate
          CHARACTER  LOD_V*10 ! 22 182-191    F10.7  sec     Length of day
          byte sep_23 !  192  !
          CHARACTER  FILL_1*2 ! 23 193-194    A2             Filler: -0
          byte sep_24 !  195  !
          CHARACTER  FILL_2*2 ! 24 196-197    A2             Filler: -0
          byte sep_25 !  198  !
          CHARACTER  XPR_E*9  ! 25 199-207    F9.6   asc/day Formal uncertainty
          byte sep_26 !  208  !       of X pole coordinate rate
          CHARACTER  YPR_E*9  ! 26 209-217    F9.6   asc/day Formal uncertainty
          byte sep_27 !  218  !       of Y pole coordinate rate
          CHARACTER  LOD_E*10 ! 27 219-228    F10.7  sec     Formal uncertainty
          byte sep_28 !  229  !       of length of day
          CHARACTER  FILL_3*2 ! 28 230-231    A2             Filler: -0
          byte sep_29 !  232  !
          CHARACTER  FILL_4*2 ! 29 233-234    A2             Filler: -0
          byte sep_30 !  235  !
          byte sep_31 !  236  !
          CHARACTER  C_NET*64 ! 30 237-300    A64            Network ID
      END TYPE  EOPS__CHAR  ! EOPS__CHAR !
!
      TYPE      EOP__STRU
          REAL*8     MJD_EOP ! days    Modified Julian date for EOP in TDT
          REAL*8     MJD_NUT ! days    Modified Julian date for nutation in TDT
          REAL*8     XPL_V   ! rad     The estimate of X pole coordinate
          REAL*8     YPL_V   ! rad     The estimate of Y pole coordinate
          REAL*8     U1_V    ! rad     The UT1-TAI angle
          REAL*8     DPSI_V  ! rad     Adjustment of the
          REAL*8     DEPS_V  ! rad     Adjustment of the nutation in obliquity wrt IAU 1980
          REAL*8     XPR_V   ! rad/sec Estimate of rate of change of X pole coordinate
          REAL*8     YPR_V   ! rad/sec Estimate of rate of change of Y pole coordinate
          REAL*8     UTR_V   ! rad/sec Estimate of UT1 rate
          REAL*8     XPL_E   ! rad     Formal uncertainty of X pole coordinate
          REAL*8     YPL_E   ! rad     Formal uncertainty of Y pole coordinate
          REAL*8     U1_E    ! rad     Formal uncertainty
          REAL*8     DPSI_E  ! rad     Formal uncertainty of nutation in longitude angle
          REAL*8     DEPS_E  ! rad     Formal uncertainty of nutation in obliquity angle
          REAL*8     XPR_E   ! rad/sec Formal unceratinty of X pole coordinate rate
          REAL*8     YPR_E   ! rad/sec Formal unceratinty of Y pole coordinate rate
          REAL*8     UTR_E   ! rad/sec Formal uncertainty of length of day
          REAL*8     WRMS    ! sec     Weighted root mean square of postfit residual of the solution
          REAL*8     C_XY    ! --      Correlation between X-pole and Y-pole positions
          REAL*8     C_XU    ! --      Correlation between X-pole and UT1
          REAL*8     C_YU    ! --      Correlation between Y pole and UT1
          REAL*8     C_PE    ! --      Correlation between nutation in obliquity and longitude
          REAL*8     C_URX   ! --      Correlation between UT1 rate and X pole
          REAL*8     C_URY   ! --      Correlation between UT1 rate and Y pole
          REAL*8     C_URU   ! --      Correlation between UT1 rate and UT1
          CHARACTER  SCODE*6 ! --      IVS session code
          CHARACTER  DBNAME*10 ! --    Database name
          REAL*8     DURA    ! sec     Session duration
          INTEGER*4  NOBS    ! --      Number of used observations in the session
          INTEGER*4  STATUS  ! --      Status fit field
	  CHARACTER  C_NET*64! --      Network array string
          CHARACTER*1 FLAG   ! --      Special flag
       END TYPE  EOP__STRU  ! EOP__STRU !
!
      TYPE      EOB__CHAR
          CHARACTER  FLAG*1   !  1    1-1    A1     --      Usage flag
          byte sep_01 !   2   !
          CHARACTER MJD_EOP*12 !   2  3-14   F12.6  days    Modified Julian
          byte sep_02 !   15  !        date for EOP at TDT time scale
          CHARACTER  DBNAME*10 ! 3   16-25   A10     --     Database name
          byte sep_03 !   26  !
          CHARACTER  SCODE*6  !  4   27-32   A6     --      IVS session code
          byte sep_04 !   28  !
          CHARACTER  XPL_V*8  !  5   34-41   F8.6   arcsec  The estimate of
          byte sep_05 !   42  !        X pole coordinate
          CHARACTER  YPL_V*8  !  6   43-50   F8.6   arcsec  The estimate of
          byte sep_06 !   51  !        Y pole coordinate
          CHARACTER  U1_V*11  !  7   52-62   F11.7  sec     The UT1-TAI angle
          byte sep_07 !   63  !
          CHARACTER  DPSI_V*8 !  8   64-71   F8.3   mas     Adjustment of the
          byte sep_08 !   72  !        nutation in longitude angle wrt IAU 1980
          CHARACTER  DEPS_V*8 !  9   73-80   F8.3   mas     Adjustment of the
          byte sep_09 !   62  !        nutation in obliquity wrt IAU 1980
          CHARACTER  XPR_V*9  ! 10   82-90   F9.6   asc/day Estimate of rate
          byte sep_10 !   91  !        change of X pole coordinate
          CHARACTER  YPR_V*9  ! 11   92-100  F9.6   asc/day Estimate of rate
          byte sep_11 !  101  !        change of Y pole coordinate
          CHARACTER  UTR_V*7  ! 12  102-108  F7.4   ms/day  UT1 rate
          byte sep_12 !  109  !
          CHARACTER  XPL_E*8  ! 13  110-117  F8.6   arcsec  Formal uncertainty
          byte sep_13 !  118  !        of X pole coordinate
          CHARACTER  YPL_E*8  ! 14  119-126  F8.6   arcsec  Formal uncertainty
          byte sep_14 !  127  !        of Y pole coordinate
          CHARACTER  U1_E*9   ! 15  128-136  F9.7   sec     Formal uncertainty
          byte sep_15 !  137  !        of UT1-TAI angle
          CHARACTER  DPSI_E*7 ! 16  138-144  F7.3   mas     Formal uncertainty
          byte sep_16 !  145  !        of nutation in longitude angle
          CHARACTER  DEPS_E*7 ! 17  146-152  F7.3   mas     Formal uncertainty
          byte sep_17 !  153  !        of nutation in obliquity angle
          CHARACTER  XPR_E*9  ! 18  154-162  F9.6   asc/day Formal unceratinty
          byte sep_18 !  163  !        of X pole coordinate rate
          CHARACTER  YPR_E*9  ! 19  164-172  F9.6   asc/day Formal unceratinty
          byte sep_19 !  173  !        of Y pole coordinate rate
          CHARACTER  UTR_E*7  ! 20  174-180  F7.4   ms/day  Formal uncertainty
          byte sep_20 !  181  !        of UT1 rate
          CHARACTER  C_XY*6   ! 21  182-187    F6.4   --    Correlation
          byte sep_21 !  188  !        between X-pole and Y-pole positions
          CHARACTER  C_XU*6   ! 22  189-194    F6.4   --    Correlation
          byte sep_22 !  195  !        between X-pole and UT1
          CHARACTER  C_YU*6   ! 23  196-201    F6.4   --    Correlation
          byte sep_23 !  202  !        between Y-pole and UT1
          CHARACTER  C_PE*6   ! 24  203-208    F6.4   --    Correlation
          byte sep_24 !  209  !        between nutation DPSI and DEPS
          CHARACTER  C_URX*6  ! 25  210-215    F6.4   --    Correlation between
          byte sep_25 !  216  !        UT1 rate and X-pole coordinate
          CHARACTER  C_URY*6  ! 26  217-222    F6.4   --    Correlation between
          byte sep_26 !  223  !        the UT1 rate and Y-pole coordinate
          CHARACTER  C_URU*6  ! 27  224-229    F6.4   --    Correlation between
          byte sep_27 !  230  !        the UT1 rate and UT1-TAI angle
          CHARACTER  DURA*5   ! 28  231-235    F5.2   hours Session duration
          byte sep_28 !  236  !
          CHARACTER  WRMS*7   ! 29  237-243    F7.2   psec  Weighted root
          byte sep_29 !  244  !        mean square of postfit residuals
          CHARACTER  NOBS*6   ! 30  245-250    I6     --    Number of used obs
          byte sep_30 !  251  !        mean square of postfit residuals
          CHARACTER MJD_NUT*12 ! 31 252-263    F12.6  days  Modified Julian
          byte sep_31 !  264
          CHARACTER  C_NET*64 ! 31 265-328    A64            Network ID
      END TYPE  EOB__CHAR  ! EOB__CHAR !
!
      TYPE       SOURCE_CAT__TYPE
          CHARACTER  IVS_NAME*8
          CHARACTER  J2000_NAME*10
	  CHARACTER  B1950_NAME*8
	  CHARACTER  DB_NAME*10
          REAL*8     ALP
          REAL*8     DEL
          REAL*8     ALP_ERR
          REAL*8     DEL_ERR
          REAL*8     CORR
          REAL*8     S_VEC(3)
          REAL*8     SOU_ERR
          INTEGER*4  NOBS_TOTAL
          INTEGER*4  NSES_TOTAL
          INTEGER*4  NOBS_USED
          INTEGER*4  NSES_USED
          INTEGER*4  CALIB     !  1 "c", 2 "n", 3 "u", 0 "-"
	  REAL*8     FLUX_TOT_S
	  REAL*8     FLUX_UNR_S
	  REAL*8     FLUX_TOT_C
	  REAL*8     FLUX_UNR_C
	  REAL*8     FLUX_TOT_X
	  REAL*8     FLUX_UNR_X
	  REAL*8     FLUX_TOT_U
	  REAL*8     FLUX_UNR_U
	  REAL*8     FLUX_TOT_K
	  REAL*8     FLUX_UNR_K
          REAL*8     EPOCH_J2000_SEC
          CHARACTER  SESS*10
          CHARACTER  DAT_BEG*10
          CHARACTER  DAT_END*10
	  INTEGER*4  IND_LINE
      END TYPE   SOURCE_CAT__TYPE
!
! >>>>> end of INCLUDE-BLOCK getpar.i
