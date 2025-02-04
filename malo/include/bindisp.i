!
!  >>>> Include block BINDISP
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with site ephemeris displacements
!  >>>>
!  >>>> 2002.12.10  L. Petrov   2019.12.28
!
        CHARACTER   BINDISP__LABEL*8, &
     &              BINDISP_SUMMARY__LABEL*50,   &
     &              BINDISP_SUMMARY__LABEL_1*50, &
     &              BINDISP_SUMMARY__LABEL_2*50, &
     &              BINDISP_SUMMARY__LABEL_3*50
        PARAMETER ( BINDISP__LABEL = 'BINDISP ' )
        PARAMETER ( BINDISP_SUMMARY__LABEL = &
     &             'BINDISP Summary file. Format version of 2015.07.24' )
        PARAMETER ( BINDISP_SUMMARY__LABEL_1 = &
     &             'BINDISP Summary file. Format version of 2014.06.20' )
        PARAMETER ( BINDISP_SUMMARY__LABEL_2 = &
     &             'BINDISP Summary file. Format version of 2005.03.28' )
        PARAMETER ( BINDISP_SUMMARY__LABEL_3 = &
     &             'BINDISP Summary file. Format version of 2002.12.12' )
        CHARACTER   BINDISP_VERSION_DATE*19
        PARAMETER ( BINDISP_VERSION_DATE = '2019.12.28_23:00:11' )
!
        INTEGER*4    M__POSVAR       ! maximal number of position variation
        PARAMETER  ( M__POSVAR = 8 ) ! mapping files
!
        INTEGER*4    M__BDSLEN          ! maximal number of lines in BINDISP
        PARAMETER  ( M__BDSLEN = 8192 ) ! summary file
!
        INTEGER*4  PSV__HMD, PSV__TSR, PSV__CLS, PSV__LIN, PSV__SPL, &
                   PSV__AVL, PSV__REQ, PSV__ALC, PSV__REA, PSV__BSP
!
        PARAMETER  ( PSV__HMD = 701 ) ! Position variation mode: harmonic model
        PARAMETER  ( PSV__TSR = 702 ) ! Position variation mode: time series
        PARAMETER  ( PSV__CLS = 703 ) ! Position var. interpolation: close point
        PARAMETER  ( PSV__LIN = 704 ) ! Position var. interpolation: linear
        PARAMETER  ( PSV__SPL = 705 ) ! Position var. interpolation: cubic spline
        PARAMETER  ( PSV__AVL = 706 ) ! Position var. usage: use if available
        PARAMETER  ( PSV__REQ = 707 ) ! Position var. usage: usage is required
!
        PARAMETER  ( PSV__ALC = 708 ) ! Memmory for pos/var structure is allocated
        PARAMETER  ( PSV__REA = 709 ) ! Pos/var structure is filled with data
        PARAMETER  ( PSV__BSP = 699 ) ! B-spline model for position variations
!
        INTEGER*4    M__PSV           ! The number of nodes for position
        PARAMETER  ( M__PSV = 32 )    ! variations
        INTEGER*4    M__BDS           ! Maximal number of records from BINDISP
        PARAMETER  ( M__BDS = 64 )    ! files which will be read for one session
        REAL*8       OVH__PSV              ! Interpolation interval for position
        PARAMETER  ( OVH__PSV = 2048.0D0 ) ! variations model starts OVH__PSV sec
!                                        ! earlier session's nominal start and
!                                        ! ends OVH__PSV sec later.
        REAL*8       NEA__PSV            !  Sites considered sufficiently
        PARAMETER  ( NEA__PSV = 2000.0D0 ) !  close to the station if the
!                                     !  distance is less than NEA__PSV meters
        INTEGER*4    M__BDM, M__HDR1, M__HDR, LEN__HDR, LEN__BDS
        PARAMETER  ( M__BDM = 12 )
        PARAMETER  ( M__HDR1  =  8 )
        PARAMETER  ( M__HDR   = M__HDR1 + 3*M__BDM )
        PARAMETER  ( LEN__HDR =  8 )
        PARAMETER  ( LEN__BDS =  8 )
        CHARACTER    VTD__IO_LOCK_NAME*10, VTD__READ_LOCK_NAME*12, VTD__WRITE_LOCK_NAME*13
        PARAMETER  ( VTD__IO_LOCK_NAME    = 'dir_io.lck'    )
        PARAMETER  ( VTD__READ_LOCK_NAME  = 'dir_read.lck'  )
        PARAMETER  ( VTD__WRITE_LOCK_NAME = 'dir_write.lck' )
        REAL*8       VTD__LOCK_TIMEOUT, VTD__BDS_MAX
        PARAMETER  ( VTD__LOCK_TIMEOUT =  8.0D0 )  ! Maximal age in sec for locks
        PARAMETER  ( VTD__BDS_MAX = 0.32D0 ) ! Maximum displacement without exension
        CHARACTER  SUMMARY_BDS_FILE*15
        PARAMETER  ( SUMMARY_BDS_FILE = 'bds_summary.txt' )
!
        TYPE      BINDISP_MODEL
            CHARACTER  LOAD_TYPE*8
            CHARACTER  LOAD_NAME*8
            CHARACTER  LOAD_VERS*8
        END TYPE  BINDISP_MODEL
!
        TYPE      BINDISP_DATA
            INTEGER*2   X_DSP
            INTEGER*2   Y_DSP
            INTEGER*2   Z_DSP
            INTEGER*2   EXT_DSP
        END TYPE  BINDISP_DATA ! BINDISP_DATA
!
        TYPE      BINDISP_HEADER_2
            INTEGER*4   MJD_FMT
            CHARACTER   ENDIAN_FMT*1
            CHARACTER   FLOAT_FMT*1
            INTEGER*2   N_MOD
        END TYPE  BINDISP_HEADER_2 ! BINDISP_HEADER_2
!
        TYPE      BINDISP_HEADER_3
            TYPE  ( BINDISP_MODEL ) :: BIN_MOD(M__BDM)
        END TYPE  BINDISP_HEADER_3
!
        TYPE      BINDISP_HEADER_4
            INTEGER*4   NUM_REC
            REAL*4      SAMPLING_INTERVAL
        END TYPE  BINDISP_HEADER_4 ! BINDISP_HEADER_4
!
        TYPE      BINDISP_HEADER_8
            INTEGER*4   MJD_FIRST
            REAL*4      TAI_FIRST
        END TYPE  BINDISP_HEADER_8 ! BINDISP_HEADER_4
!
        INTEGER*4  LEN__BDSUM
        PARAMETER  ( LEN__BDSUM = 132 )
        TYPE      BDSSUM_STAREC
            CHARACTER*4   REC_ID      !   1:4
            CHARACTER*1   FILL_1      !   5:5
            CHARACTER*4   SITE_IND    !   6:9
            CHARACTER*1   FILL_2      !  10:10
            CHARACTER*8   SITE_ID     !  11:18
            CHARACTER*1   FILL_3      !  19:19
            CHARACTER*19  DATE_BEG    !  20:38
            CHARACTER*3   FILL_4      !  39:41
            CHARACTER*19  DATE_END    !  42:60
            CHARACTER*1   FILL_5      !  61:61
            CHARACTER*9   NUM_PTS     !  62:70
            CHARACTER*1   FILL_6      !  71:71
            CHARACTER*16  SAMPLE_INT  !  72:87
            CHARACTER*1   FILL_7      !  88:88
            CHARACTER*13  X_COORD     !  89:101
            CHARACTER*1   FILL_8      ! 102:102
            CHARACTER*13  Y_COORD     ! 103:115
            CHARACTER*1   FILL_9      ! 116:116
            CHARACTER*13  Z_COORD     ! 117:129
            CHARACTER*1   FILL_10     ! 130:130
            CHARACTER*1   ENDIAN_FMT  ! 131:131
            CHARACTER*1   FLOAT_FMT   ! 132:132
        END TYPE  BDSSUM_STAREC ! BDSSUM_STAREC !
!
!  >>>> end of include block BINDISP
!
