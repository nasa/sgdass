! >>>>> Include block for structure definion related to ionospere maps
! >>>>> 2010.05.06  (c)  L. Petrov  v 1.2  2022.09.27_20:30:01
!
	TYPE IONO_HEADER__TYPE
            CHARACTER  LABEL*48   ! File label
            CHARACTER  MODEL*16   ! Model name
	    CHARACTER  AUTHOR*16  ! Name of the model author
	    INTEGER*4  NLON       ! The number of grid steps along longitude axis
	    INTEGER*4  NLAT       ! The number of grid steps along latitude axis
	    INTEGER*4  NEPC       ! The number of grid steps along time axis
	    INTEGER*4  MJD_BEG    ! MJD at TAI of the first epoch
	    INTEGER*2  MISSING    ! Missing value
	    INTEGER*2  FILLER(3)  ! fillers
	    REAL*8     UTC_BEG    ! UTC Time tag of the first epoch. Units: seconds
	    REAL*8     TIM_STEP   ! Time step along the time axis. Units: seconds
	    REAL*8     SCALE      ! The scaling factor which should be applied to raw TEC values
	    REAL*8     LON_MIN    ! Longitude at the beginning of the longitude axis. Units: radians
	    REAL*8     LAT_MIN    ! Latitude  at the beginning of the latitude  axis. Units: radians
	    REAL*8     LON_STEP   ! Longitude step along the longitude axis. Units: radians
	    REAL*8     LAT_STEP   ! Latitude  step along the latitude  axis. Units: radians
	    REAL*8     HEIGHT     ! Height of the ionosphere. Units: meters
        END TYPE IONO_HEADER__TYPE
!
	TYPE     IONO__TYPE
            TYPE ( IONO_HEADER__TYPE ) :: HEADER
            INTEGER*2, POINTER :: TEC_VAL(:,:,:) => NULL ()
            REAL*4,    POINTER :: TEC_SPL(:,:,:) => NULL ()
            REAL*4,    POINTER :: LON_VAL(:)     => NULL ()
            REAL*4,    POINTER :: LAT_VAL(:)     => NULL ()
            REAL*4,    POINTER :: TIM_VAL(:)     => NULL ()
            REAL*4,    POINTER :: AVR_STA(:)     => NULL ()
            REAL*4,    POINTER :: COV_STA(:,:)   => NULL ()
	    INTEGER*4  STATUS_VAL
	    INTEGER*4  STATUS_SPL
        END TYPE IONO__TYPE
!
	INTEGER*4    M__ION, M__IOB, VIO__M_DEG
	PARAMETER  ( M__ION = 64*1024  )
	PARAMETER  ( M__IOB = 128*1024 )
	PARAMETER  ( VIO__M_DEG = 3 )
	INTEGER*4    CMPR__ION
	PARAMETER  ( CMPR__ION = 10 ) ! Maximam compression factor
	INTEGER*4    VIO__UNDF, VIO__ALLO, VIO__READ, VIO__COMP
	PARAMETER  ( VIO__UNDF = 0 )
	PARAMETER  ( VIO__ALLO = 1928310223 )
	PARAMETER  ( VIO__READ = 1372092846 )
	PARAMETER  ( VIO__COMP = 1091954382 )
        REAL*8       SHR__VIO, ALPHA__VIO, VIO__CONST
        PARAMETER  ( SHR__VIO = 0.2D0 )
        PARAMETER  ( ALPHA__VIO = 5.308018D10 ) ! TECU/sec
        PARAMETER  ( VIO__CONST = 1.345D9     ) ! TECU/sec
!
	CHARACTER  VIONO__LABEL*48, VIO__GNSS_TEC*12, VIO__GNSS_TEC_MOD*12, &
     &                              VIO__GNSS_TEC_MHI*12
        PARAMETER  ( VIONO__LABEL = 'VIONO Little-Endian  Format of 2010.05.08       ' )
	PARAMETER  ( VIO__GNSS_TEC     = 'GNSS_TEC_MAP' )
	PARAMETER  ( VIO__GNSS_TEC_MOD = 'GNSS_TEC_MOD' )
	PARAMETER  ( VIO__GNSS_TEC_MHI = 'GNSS_TEC_MHI' )
	INTEGER*2    VIONO__MISSING
	PARAMETER  ( VIONO__MISSING = -1 )
!
! >>>>> End of include block for package VTD
!
