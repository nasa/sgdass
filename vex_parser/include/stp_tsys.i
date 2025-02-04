! >>> Include block for stp_tsys
! >>> 2020.08.05 (c)  N. Habana  v 0.1 2020.08.05_22:04:07
!
      INCLUDE    'astro_constants.i'
!
      INTEGER*4  TSYS__MSTA, TSYS__MFRQ, TSYS__MZEN, TSYS__MELV
      INTEGER*4  TSYS__MCNT, TSYS__NODES__CNT
      PARAMETER  ( TSYS__MSTA  =    32 )
      PARAMETER  ( TSYS__MCNT  =    64 )         ! Max. No. of Tsys
      PARAMETER  ( TSYS__MFRQ  =    16 )
      PARAMETER  ( TSYS__MZEN  =   128 )
      PARAMETER  ( TSYS__MELV  =    64 )
      PARAMETER  ( TSYS__NODES__CNT = 8 )
      REAL*8     ELV__NODES__TSYS(TSYS__NODES__CNT)
!
      PARAMETER ( ELV__NODES__TSYS  = [                                       &
     &                    3.D0*DEG__TO__RAD,    5.D0*DEG__TO__RAD,      &
     &                   10.D0*DEG__TO__RAD,   20.D0*DEG__TO__RAD,      &
     &                   30.D0*DEG__TO__RAD,   40.D0*DEG__TO__RAD,      &
     &                   80.D0*DEG__TO__RAD,   90.D0*DEG__TO__RAD  ] )
!     
! --- Derived type for system temperature as a function of time
!
      TYPE    TSYS__ZEN_TYPE
          INTEGER*4,  POINTER :: MJD(:)   => NULL()    ! MJD part of obs. time
          REAL*8,     POINTER :: TAI(:)   => NULL()    ! TAI part of obs. time
          REAL*8,     POINTER :: VALS(:)  => NULL()    ! Tsys [K]
      END TYPE TSYS__ZEN_TYPE
!
! --- Derived type for system temperature as a function of elevation
!
      TYPE    TSYS__ELV_TYPE
          REAL*8,     POINTER :: ELV(:)   => NULL()   ! 
          REAL*8,     POINTER :: VALS(:)  => NULL()   !
      END TYPE TSYS__ELV_TYPE
!      
! --- Derived type for Stations
!
      TYPE   TSYS__STA_TYPE
          CHARACTER   NAME*8             ! IVS name, upper case
          CHARACTER   SHORT_NAME*2       ! IVS name, lower case
          REAL*8      COORD(3)
          INTEGER*4   NTSYS
      END TYPE TSYS__STA_TYPE 
!
! --- Derived type for the Tsys file averages
!
      TYPE   TSYS__AVE_TYPE
          CHARACTER   NAME*8
          INTEGER*4   NZEN
          INTEGER*4   NELV
          INTEGER*4,  POINTER :: MJD(:) => NULL()      
          REAL*8,     POINTER :: TAI(:) => NULL()
          REAL*8,     POINTER :: ELV(:) => NULL()
          REAL*8,     POINTER :: AVE_ZEN_S(:) => NULL()
          REAL*8,     POINTER :: AVE_ZEN_X(:) => NULL()
          REAL*8,     POINTER :: AVE_ELV_S(:) => NULL()
          REAL*8,     POINTER :: AVE_ELV_X(:) => NULL()
      END TYPE TSYS__AVE_TYPE
!%%NOKH%%!!
!%%NOKH%%!! --- Summary of the average Derived Type
!%%NOKH%%!!
!%%NOKH%%!      TYPE   TSYS__AVE_SUMMARY_TYPE
!%%NOKH%%!          CHARACTER   NTSYS_RANGES
!%%NOKH%%!          INTEGER*4,  POINTER :: MJD_RANGE(:,:) => NULL()   ! { NTSYS_RANGES-by-2 }
!%%NOKH%%!          REAL*8,     POINTER :: TAI_RANGE(:,:) => NULL()   ! { NTSYS_RANGES-by-2 }
!%%NOKH%%!          REAL*8,     POINTER :: FRQ_RANGE(:,:) => NULL()   ! { NTSYS_RANGES-by-2 }
!%%NOKH%%!!%%%%!          CHARACTER,  POINTER :: POLS(:,:) => NULL()        ! { NTSYS_RANGES-by-2 }
!%%NOKH%%!          REAL*8,     POINTER :: ELV(:,:) => NULL()     ! { NTSYS_RANGES-by-TSYS__NODES__CNT }
!%%NOKH%%!          REAL*8,     POINTER :: VALS(:,:) => NULL()    ! { NTSYS_RANGES-by-TSYS__NODES__CNT } 
!%%NOKH%%!      END TYPE TSYS__AVE_SUMMARY_TYPE      
!%%NOKH%%!!
!%%NOKH%%!! --- Station by Station analyses
!%%NOKH%%!!
!%%NOKH%%!      TYPE TSYS__TBL_TYPE
!%%NOKH%%!          CHARACTER   EXP_NAME*16
!%%NOKH%%!          CHARACTER   STA_NAME*8
!%%NOKH%%!          REAL*8      ELV_RANGE(3,2)     ! [5,40), [40,60), [60,90]
!%%NOKH%%!          REAL*8      STA_MED(3)         ! Station Median
!%%NOKH%%!      END TYPE TSYS__TBL_TYPE
!
! --- TSYS Parsed values
!
      TYPE   TSYS__TYPE
          CHARACTER   EXP_NAME*16
          INTEGER*4   NSTA               ! No. of stations          
          INTEGER*4   NFRQ               ! No. of frequency channels
          INTEGER*4   NZEN               ! No. of time nodes
          INTEGER*4   NELV               ! No. of elevation nodes
          TYPE ( TSYS__STA_TYPE ), POINTER :: STA(:) => NULL()          
          REAL*8,     POINTER :: FREQ_SKY(:)  => NULL()                ! Frequency value [Hz]
          TYPE (TSYS__ELV_TYPE), POINTER  :: TSYS_ELV(:) => NULL()
          TYPE (TSYS__ZEN_TYPE), POINTER  :: TSYS_ZEN(:) => NULL()
          TYPE (TSYS__AVE_TYPE), POINTER  :: TSYS_AVE(:) => NULL()
!%%%%%!          TYPE (TSYS__AVE_SUMMARY_TYPE) :: TSYS_AVE_SUM(:) => NULL()
!%%%% !          TYPE (TSYS_TBL__TYPE), POINTER :: TSYS_TBL(:) => NULL()
!     
          INTEGER*4   STATUS
      END TYPE TSYS__TYPE
!
      TYPE   SLEW__TYPE
          CHARACTER   RAW_DATE*19
          INTEGER*4   MJD
          REAL*8      TAI
          CHARACTER   MOUNT*8
          CHARACTER   RECORDER*8
          REAL*8      SLEW_AZ
          REAL*8      SLEW_EL
          REAL*8      ACCL_AZ
          REAL*8      ACCL_EL
          REAL*8      TSETTLE_AZ
          REAL*8      TSETTLE_EL
          REAL*8      EL_MIN
          REAL*8      EL_MAX
          REAL*8      AZ_RANGE(4)
          REAL*8      PREOB
          REAL*8      POSTOB
          TYPE ( TSYS__STA_TYPE ), POINTER :: STA => NULL()
!     
          INTEGER*4   STATUS
      END TYPE SLEW__TYPE
!
      INTEGER*4  TSYS__UNDF, TSYS__LOAD, TSYS__INIT
      PARAMETER  ( TSYS__UNDF  = 0 )
      PARAMETER  ( TSYS__LOAD  = 1028931 )
      PARAMETER  ( TSYS__INIT  = 22831221 )
            
