! >>> Include block for stp_gain
! >>> 2020.08.05 (c)  N. Habana  v 0.1 2020.08.25_12:56:28
!
      INCLUDE    'stp_tsys.i'  ! We do this so that we can use astro_constants
!
      INTEGER*4  GAIN__MCNT, GAIN__MSTA, GAIN__MFRQ, GAIN__MZEN
      INTEGER*4  GAIN__MELV, GAIN__NODES__CNT
      PARAMETER  ( GAIN__MCNT  =  128 )         ! Maximum number of Gains
      PARAMETER  ( GAIN__MSTA  =   64 )
      PARAMETER  ( GAIN__MFRQ  =    8 )
      PARAMETER  ( GAIN__MELV  =   16 )
      PARAMETER  ( GAIN__NODES__CNT =   7 )
      REAL*8     ELV__NODES(GAIN__NODES__CNT)
!
      PARAMETER ( ELV__NODES  = [ 3.D0*DEG__TO__RAD,                    &
     &                   10.D0*DEG__TO__RAD,   20.D0*DEG__TO__RAD,      &
     &                   30.D0*DEG__TO__RAD,   40.D0*DEG__TO__RAD,      &
     &                   80.D0*DEG__TO__RAD,   90.D0*DEG__TO__RAD  ] )
!%%%%!!     
!%%%%!! --- Derived type for the gain as gathered in the file units
!%%%%!!
!%%%%!      TYPE     GAIN_RAW__TYPE
!%%%%!          CHARACTER   TIMERANGE(2)*19
!%%%%!          REAL*8      FREQ(2)
!%%%%!          REAL*8,     POINTER :: POLY(:,:) => NULL() ! Polynomials at Freq.
!%%%%!      END    TYPE
!
! --- Derived type for the gain as a function of elevation
!
      TYPE     GAIN_ELV__TYPE
          INTEGER*4   MJD_RANGE(2)  ! MJD part of the date range validity
          REAL*8      TAI_RANGE(2)  ! TAI part of the date range validity
          REAL*8      FREQ(2)       ! Freq. Range
          REAL*8      DPFU(2)   ! Degree per flux unit ()
!****!          REAL*8,     POINTER :: ELV(:)    => NULL()  !  Degrees ! [*****CONVERT TO RADIANS****]
          REAL*8,     POINTER :: VALS(:,:)   => NULL()  !  DPFU*G(e)
      END      TYPE
!
! --- Derived type for Stations
!
      TYPE   GAIN__STA_TYPE
          CHARACTER   NAME*8             ! IVS name, upper case
          CHARACTER   SHORT_NAME*2       ! Two X-ter station name, low case
          CHARACTER   MOUNT*6
          INTEGER*4   NGAINS
      END TYPE GAIN__STA_TYPE
!
! --- Gain Parsed values
!
      TYPE   GAIN__TYPE
          INTEGER*4   NSTA               ! No. of stations
!%%%%!          INTEGER*4   NFRQ               ! No. of frequency channels
          INTEGER*4   NELV               ! No. of elevation nodes
          TYPE ( GAIN__STA_TYPE ), POINTER :: STA(:) => NULL()
!%%%%!          REAL*8,     POINTER :: FREQ(:)  => NULL() ! Frequency value [Hz]
!%%%%!          TYPE ( GAIN_RAW__TYPE ), POINTER :: GAIN_RAW(:,:) => NULL()
          TYPE ( GAIN_ELV__TYPE ), POINTER :: GAIN_ELV(:,:) => NULL()
!     
          INTEGER*4   STATUS
      END TYPE GAIN__TYPE
!
      INTEGER*4  GAIN__UNDF, GAIN__LOAD, GAIN__INIT
      PARAMETER  ( GAIN__UNDF  = 0 )
      PARAMETER  ( GAIN__LOAD  = 1028931 )
      PARAMETER  ( GAIN__INIT  = 22831221 )

