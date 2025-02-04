! >>>>> Include block for stp
!
! >>>>> 2020.07.28 (c)  L. Petrov  v 1.02 2020.12.15_09:24:37
!
        INTEGER*4  STP__MEL, STP__MPOL, STP__MSTA, STP__MFIL
        REAL*8	   SBND__RNG(2), XBND__RNG(2)
!
	PARAMETER  ( STP__MEL  =    32 )
	PARAMETER  ( STP__MPOL =    32 )
	PARAMETER  ( STP__MSTA =    64 )
	PARAMETER  ( STP__MFIL =  1024 )
        PARAMETER  ( SBND__RNG = [ 2.1D9, 2.4D9 ] )
	PARAMETER  ( XBND__RNG = [ 7.9D9, 9.1D9 ] )
!
! ----- Data structure for system temperature for a given date range and frequency range
!
	TYPE      TSYS_STP__TYPE 
             INTEGER*4  MJD_RANGE(2)  ! MJD part of the date range validity
             REAL*8     TAI_RANGE(2)  ! TAI part of the date range validity
             REAL*8     FRQ_RANGE(2)  ! Frequency range of validity
             INTEGER*4  NEL	      ! Number of elevation nodes
             INTEGER*4  NPOL          ! Number of polarizations
             CHARACTER  POLS(2)*1     ! Polarization name 
             REAL*8,    POINTER :: TSYS_ELEV(:)   => NULL() ! Array of elevation nodes in rad
             REAL*8,    POINTER :: TSYS_VALS(:,:) => NULL() ! Array of Tsys values in K
             REAL*8     TCAL(2)       ! Calibration load temperature in K
        END  TYPE TSYS_STP__TYPE 
!
! ----- Data structure for antenna gain for a given date range and frequency range
!
	TYPE      GAIN_STP__TYPE 
             INTEGER*4  MJD_RANGE(2)  ! MJD part of the date range validity
             REAL*8     TAI_RANGE(2)  ! TAI part of the date range validity
             REAL*8     FRQ_RANGE(2)  ! Frequency range of validity in Hz
             INTEGER*4  NEL           ! Number of elevation nodes
             INTEGER*4  NPOL          ! Number of polarizations
             CHARACTER  POLS(2)*1     ! Polarization name
             REAL*8,    POINTER :: GAIN_ELEV(:)   => NULL() ! Array of elevation nodes in rad
             REAL*8,    POINTER :: GAIN_VALS(:,:) => NULL()  ! Array of gain in elevation nodes K/Jy
	END  TYPE GAIN_STP__TYPE 
!
! ----- Data structure for bandpass loss for a given date range and given IF bandwidth
!
	TYPE      BPSL_STP__TYPE 
             INTEGER*4  MJD_RANGE(2)  ! MJD part of the date range validity
             REAL*8     TAI_RANGE(2)  ! TAI part of the date range validity
             INTEGER*8  IF_BANDWIDTH  ! Intermediate frequency bandwidth in Hz
             REAL*8     BETA          ! Bandpass loss
	END  TYPE BPSL_STP__TYPE 
!
	TYPE      STP__STA_TYPE
	    CHARACTER  NAME*8         ! IVS name, upper case
	    CHARACTER  SHORT_NAME*2   ! Two character station name, low case
	    CHARACTER  LAST_UPDATE*10 ! Date of last modification in YYYY.MM.DD format
            REAL*8     COO(3)         ! Station coordinates
	    REAL*8     SLEW_RATE_EL   ! Antenna slewing rate on elevation axis in rad/s
	    REAL*8     SLEW_RATE_AZ   ! Antenna slewing rate on aximuth axis in rad/s
	    REAL*8     SLEW_ACCL_EL   ! Antenna slewing acceleration on elevation axis in rad/s**2
	    REAL*8     SLEW_ACCL_AZ   ! Antenna slewing acceleration on azimuth   axis in rad/s**2
	    REAL*8     SLEW_DECE_EL   ! Antenna slewing deceleration on elevation axis in rad/s**2
	    REAL*8     SLEW_DECE_AZ   ! Antenna slewing deceleration on azimuth   axis in rad/s**2
	    REAL*8     TIME_SETTLE_EL ! Antenna settle time over elevation axis in sec
	    REAL*8     TIME_SETTLE_AZ ! Antenna settle time over azimuth axis in sec
	    REAL*8     EL_MIN         ! Minimum elevation angle in rad
	    REAL*8     EL_MAX         ! Maximum elevation angle in rad
	    REAL*8     AZ_RANGE(4)    ! Azimuth sevtors in rad: 1-2: counterclockwise, 2-3: neutral, 3-4: clockwise
	    REAL*8     PREOB          ! Time allocated for PREOB  procedure when antenna is in source before recording
	    REAL*8     POSTOB         ! Time allocated for POSTOB procedure when antenna is in source after  recording
	    CHARACTER  MOUNT_TYPE*8   ! Mount type code
            CHARACTER  RECORDER*8     ! Recorder type
!
            INTEGER*4  NHOR           ! Number of elements for the elevation mask
            INTEGER*4  NTSYS          ! Number of records for Tsys
            INTEGER*4  NGAIN          ! Number of records for Gain
            INTEGER*4  NBPSL          ! Number of records for the bandpass losses
            REAL*8,    POINTER :: HOR_AZ(:) => NULL()    ! Array of azimuths   for defining the elevation mask, in rad
            REAL*8,    POINTER :: HOR_EL(:) => NULL()    ! Array of elevations for defining the elevation mask, in rad
            TYPE ( TSYS_STP__TYPE ), POINTER :: TSYS(:) => NULL() ! Array of Tsys records
       	    TYPE ( GAIN_STP__TYPE ), POINTER :: GAIN(:) => NULL() ! Array of Gain records
            TYPE ( BPSL_STP__TYPE ), POINTER :: BPSL(:) => NULL() ! Array of bandpass losses
	    INTEGER*4  STATUS
	END  TYPE STP__STA_TYPE
!
	TYPE     STP__TYPE  
            INTEGER*4  NSTA
	    CHARACTER  DIR_NAM*128
	    CHARACTER  C_FIL(STP__MFIL)*128
	    CHARACTER  C_STA(STP__MFIL)*8
            TYPE ( STP__STA_TYPE ), POINTER :: STA(:) => NULL()  ! STPs of station files.
	    INTEGER*4 STATUS
	END TYPE STP__TYPE
!
        REAL*8       STP__SNR_MIN
        PARAMETER  ( STP__SNR_MIN = 5.0 )
	INTEGER*4    STP__UNDF, STP__LOAD, STP__INIT
	PARAMETER  ( STP__UNDF  = 0 )
	PARAMETER  ( STP__LOAD  = 1028931 )
	PARAMETER  ( STP__INIT  = 22831221 )
!
        REAL*8       STP__SNR_FLOOR 
        PARAMETER  ( STP__SNR_FLOOR = 200.0D0 )	    
