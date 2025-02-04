!
!  >>>> Include block EPHEDISP
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with site ephemeris displacements
!  >>>>
!  >>>> 2002.12.05  L. Petrov   30-JUN-2005 11:09:44
!
        CHARACTER  EPHEDISP__LABEL*38
        PARAMETER ( EPHEDISP__LABEL = 'EPHEDISP  Format version of 2005.06.30' )
!
        CHARACTER    EPHEDISP__P_RECORD_TEMPLATE*40
        CHARACTER    EPHEDISP__A_RECORD_BEGIN_TEMPL*16
        CHARACTER    EPHEDISP__T_RECORD_BEGIN_TEMPL*44
        CHARACTER    EPHEDISP__T_RECORD_END_TEMPLATE*44
        CHARACTER    EPHEDISP__T_RECORD_TEMPLATE*26
        CHARACTER    EPHEDISP__S_RECORD_TEMPLATE*80
        CHARACTER    EPHEDISP__D_RECORD_TEMPLATE*80
!
        PARAMETER  ( EPHEDISP__P_RECORD_TEMPLATE     = &
     &             'P T 3 S            E        D           '  )
        PARAMETER  ( EPHEDISP__T_RECORD_BEGIN_TEMPL  = &
     &             'T begin                                     ' )
        PARAMETER  ( EPHEDISP__T_RECORD_END_TEMPLATE = &
     &             'T end                                       ' )
        PARAMETER  ( EPHEDISP__T_RECORD_TEMPLATE = &
     &             'T sample                  ' )
        PARAMETER  ( EPHEDISP__S_RECORD_TEMPLATE = &
     &             'S                                                                               ' )
        PARAMETER  ( EPHEDISP__D_RECORD_TEMPLATE = &
     &             'D                                                                               ' )
!
        TYPE      EPHEDISP__P_RECORD
            CHARACTER  FILL_1*4      !  1:4
            CHARACTER  NUMB_T_REC*1  !  5:5  The number of T-records
            CHARACTER  FILL_2*3      !  6:8
            CHARACTER  NUMB_S_REC*10 !  9:18 The number of S-records
            CHARACTER  FILL_3*3      ! 19:21
            CHARACTER  NUMB_EPOCHS*6 ! 22:27 The number of epochs
            CHARACTER  FILL_4*3      ! 28:30
            CHARACTER  NUMB_D_REC*10 ! 31:40 The number of D-records
        END TYPE  EPHEDISP__P_RECORD ! EPHEDISP__P_RECORD !
!
        TYPE      EPHEDISP__A_RECORD
            CHARACTER  FILL_1*2      !  1:2
	    CHARACTER  RD_AREA*14    !  3:16
        END TYPE  EPHEDISP__A_RECORD !
!
        TYPE      EPHEDISP__T_RECORD_BEGIN
            CHARACTER  FILL_1*10     !  1:10
            CHARACTER  MJD*5         ! 11:15  ! MJD of the begin epoch
            CHARACTER  FILL_2*1      ! 16:16
            CHARACTER  TAI*7         ! 17:23  ! TAI at midnight of begin epoch
            CHARACTER  FILL_3*2      ! 24:25
            CHARACTER  DATE*19       ! 26:44  ! Data and time of begin epoch
        END TYPE  EPHEDISP__T_RECORD_BEGIN ! EPHEDISP__T_RECORD_BEGIN !
!
        TYPE      EPHEDISP__T_RECORD_SAMPLE
            CHARACTER  FILL_1*10          !  1:10
            CHARACTER  SAMPLE_INTERVAL*16 ! 11:26  Sampling interval in  days
        END TYPE  EPHEDISP__T_RECORD_SAMPLE ! EPHEDISP__T_RECORD_SAMPLE !
!
        TYPE      EPHEDISP__S_RECORD
            CHARACTER  FILL_1*3    !  1:3
            CHARACTER  SITE_ID*8   !  4:11  ! Site ID
            CHARACTER  FILL_2*2    ! 12:13
            CHARACTER  X_COORD*13  ! 14:26
            CHARACTER  FILL_3*1    ! 27:27
            CHARACTER  Y_COORD*13  ! 28:40
            CHARACTER  FILL_4*1    ! 41:41
            CHARACTER  Z_COORD*13  ! 42:54
            CHARACTER  FILL_5*2    ! 55:56
            CHARACTER  GEOC_LAT*8  ! 57:64
            CHARACTER  FILL_6*1    ! 65:65
            CHARACTER  LONGITUDE*8 ! 66:73
            CHARACTER  FILL_7*1    ! 74:74
            CHARACTER  HEIGHT*6    ! 75:80
        END TYPE  EPHEDISP__S_RECORD ! EPHEDISP__S_RECORD !
!
        TYPE      EPHEDISP__D_RECORD
            CHARACTER  FILL_1*2    !  1:2
            CHARACTER  IND_EPOCH*5 !  3:7   ! Epoch's index
            CHARACTER  FILL_2*2    !  8:9
            CHARACTER  MJD*5       ! 10:14
            CHARACTER  FILL_3*1    ! 15:15
            CHARACTER  TAI*7       ! 16:22
            CHARACTER  FILL_4*2    ! 23:24
            CHARACTER  DATE*19     ! 25:43
            CHARACTER  FILL_5*2    ! 44:45
            CHARACTER  SITE_ID*8   ! 46:53
            CHARACTER  FILL_6*1    ! 54:54
            CHARACTER  U_DSPL*8    ! 55:62
            CHARACTER  FILL_7*1    ! 63:63
            CHARACTER  E_DSPL*8    ! 64:71
            CHARACTER  FILL_8*1    ! 72:72
            CHARACTER  N_DSPL*8    ! 73:80
        END TYPE  EPHEDISP__D_RECORD ! EPHEDISP__D_RECORD !
!
!  >>>> end of include block EPHEDISP
!
