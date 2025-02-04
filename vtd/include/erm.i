!
! >> include-block for the a priori Earth Rotation Model
! >> L. Petrov  2005.12.10  v 2.3    2022.12.31_14:03:14
!
!    Derived type for the Earth Rotation Model
!
      CHARACTER  ERM_FMT__LABEL*42
      PARAMETER  ( ERM_FMT__LABEL = '# ERM v  2.2  Format version of 2021.10.30' )
!
      INTEGER*4    ERM__MSPL, ERM__MKNOT
      PARAMETER  ( ERM__MSPL  =    5  )
      PARAMETER  ( ERM__MKNOT = 32768 )
      REAL*8       ERM__TIM_EPS
      PARAMETER  ( ERM__TIM_EPS = 1.0D-6 )
!
      TYPE ERM__TYPE
           SEQUENCE
           CHARACTER  FINAM*128
           CHARACTER  SOL_ID*32
           CHARACTER  FIL_APR_EOP*128
           INTEGER*4  DEGREE(3)
           INTEGER*4  MJD_BEG
           INTEGER*4  MJD_END
           INTEGER*4  MJD_BEG_RANGE_CNS
           INTEGER*4  MJD_END_RANGE_CNS
           INTEGER*4  MJD_REF_CNS
           INTEGER*4  UZM
           INTEGER*4  UZU
           REAL*8     TAI_BEG
           REAL*8     TAI_END
           REAL*8     TAI_BEG_RANGE_CNS
           REAL*8     TAI_END_RANGE_CNS
           REAL*8     TAI_REF_CNS
	   REAL*8     TIME_EST_SPAN(3)
	   REAL*8     TIME_CNS_SPAN(3)
           REAL*8     CNS_DER_SIGMA(0:ERM__MSPL,3)
           REAL*8     CNS_MEAN_SIGMA(3)
           REAL*8     CNS_RATE_SIGMA(3)
           REAL*8     CNS_MEAN_RTP(3)
           REAL*8     CNS_RATE_RTP(3)
!
	   INTEGER*4  NKNOTS(3)
           REAL*8     TIM(1-ERM__MSPL:ERM__MKNOT,3)
           REAL*8     APR(1-ERM__MSPL:ERM__MKNOT,3)
           REAL*8     VAL(1-ERM__MSPL:ERM__MKNOT,3)
           REAL*8     ERR(1-ERM__MSPL:ERM__MKNOT,3)
	   REAL*8     COV(1-ERM__MSPL:ERM__MKNOT,ERM__MSPL,3)
	   LOGICAL*1  FL_EQUIDISTANT
	   LOGICAL*1  FL_EST
	   LOGICAL*1  FL_APL
      END  TYPE ERM__TYPE
!
! << end of include block erm.i  for the the Earth Rotation Model
!
