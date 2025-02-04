!
!  >>>> Include block EPHEDISP
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with site ephemeris displacements
!  >>>>
!  >>>> 2005.01.06 L. Petrov   07-JAN-2005 10:05:05
!
	CHARACTER  AGRA_CREATE__LABEL*33
	CHARACTER  AGRA_UPDATE__LABEL*33
        CHARACTER  AGRA__LABEL*34
	PARAMETER  ( AGRA_CREATE__LABEL = 'AGRA_CREATE version of 2005.07.07' )
	PARAMETER  ( AGRA_UPDATE__LABEL = 'AGRA_UPDATE version of 2004.02.16' )
        PARAMETER  ( AGRA__LABEL = 'AGRA  Format version of 2004.12.29' )
        CHARACTER  SPHER__NRM_LABEL*78
        PARAMETER  ( SPHER__NRM_LABEL = &
     & '# SPHERICAL HARMONICS NORMALIZATION COEFFICIENTS  Format version of 2005.01.13' )
!        CHARACTER  LOVE_NUMBERS__LABEL*36
!	PARAMETER  ( LOVE_NUMBERS__LABEL = '# LOVE_NUMBERS  Format of 2005.01.10' )
!
	INTEGER*4  C__COEF, S__COEF, MDEG__AGRA
	PARAMETER  ( C__COEF = 1 ) 
	PARAMETER  ( S__COEF = 2 ) 
	PARAMETER  ( MDEG__AGRA = 256 ) 
	REAL*8       STOKES__MISSED
	PARAMETER  ( STOKES__MISSED = 1.0000D20 ) ! Missed value
!
        CHARACTER    AGRA__P_RECORD_TEMPLATE*30
        CHARACTER    AGRA__T_RECORD_BEGIN_TEMPL*44
        CHARACTER    AGRA__T_RECORD_END_TEMPLATE*44
        CHARACTER    AGRA__T_RECORD_SAMPLE_TEMPL*26
        CHARACTER    AGRA__D_RECORD_TEMPLATE*80
!
        DATA AGRA__P_RECORD_TEMPLATE / &
     &             'P T 3 M      E       D        ' /
        PARAMETER  ( AGRA__T_RECORD_BEGIN_TEMPL  = &
     &             'T begin                                     ' )
        PARAMETER  ( AGRA__T_RECORD_END_TEMPLATE = &
     &             'T end                                       ' )
        PARAMETER  ( AGRA__T_RECORD_SAMPLE_TEMPL = &
     &             'T sample                  ' )
        PARAMETER  ( AGRA__D_RECORD_TEMPLATE = &
     &             'D                                                                               ' )
!
        TYPE      AGRA__P_RECORD
            CHARACTER  FILL_1*4      !  1:4
            CHARACTER  NUMB_T_REC*1  !  5:5  The number of T-records
            CHARACTER  FILL_2*3      !  6:8
            CHARACTER  MAX_DEGREE    !  9:11 The maximal degree 
            CHARACTER  FILL_3*4      ! 12:15
            CHARACTER  NUMB_EPOCHS*5 ! 16:20 The number of epochs
            CHARACTER  FILL_4*3      ! 21:23
            CHARACTER  NUMB_D_REC*7  ! 24:30 The number of D-records
        END TYPE  AGRA__P_RECORD ! AGRA__P_RECORD !
!
        TYPE      AGRA__T_RECORD_BEGIN
            CHARACTER  FILL_1*10     !  1:10
            CHARACTER  MJD*5         ! 11:15  ! MJD of the begin epoch
            CHARACTER  FILL_2*1      ! 16:16
            CHARACTER  TAI*7         ! 17:23  ! TAI at midnight of begin epoch
            CHARACTER  FILL_3*2      ! 24:25
            CHARACTER  DATE*19       ! 26:44  ! Data and time of begin epoch
        END TYPE  AGRA__T_RECORD_BEGIN ! AGRA__T_RECORD_BEGIN !
!
        TYPE      AGRA__T_RECORD_SAMPLE
            CHARACTER  FILL_1*10          !  1:10
            CHARACTER  SAMPLE_INTERVAL*16 ! 11:26  Sampling interval in  days
        END TYPE  AGRA__T_RECORD_SAMPLE ! AGRA__T_RECORD_SAMPLE !
!
!
        TYPE      AGRA__D_RECORD
            CHARACTER  FILL_1*2    !  1:2
            CHARACTER  IND_EPOCH*5 !  3:7   ! Epoch's index
            CHARACTER  FILL_2*2    !  8:9
            CHARACTER  MJD*5       ! 10:14
            CHARACTER  FILL_3*1    ! 15:15
            CHARACTER  TAI*7       ! 16:22
            CHARACTER  FILL_4*2    ! 23:24
            CHARACTER  DATE*19     ! 25:43
            CHARACTER  FILL_5*2    ! 44:45
            CHARACTER  DEGREE*3    ! 46:48
            CHARACTER  FILL_6*1    ! 49:49
            CHARACTER  ORDER*3     ! 50:52
            CHARACTER  FILL_7*2    ! 53:54
            CHARACTER  STOKES_C*12 ! 55:66
            CHARACTER  FILL_8*1    ! 67:67
            CHARACTER  STOKES_S*12 ! 68:79
        END TYPE  AGRA__D_RECORD ! AGRA__D_RECORD !
!
	TYPE  AGRA__TYPE
	    INTEGER*4  L_DEG
	    INTEGER*4  L_EPC
	    INTEGER*4  L_COE
	    INTEGER*4  MJD_BEG
	    INTEGER*4  MJD_END
	    REAL*8     SEC_BEG
	    REAL*8     SEC_END
	    REAL*8     INTERVAL
	    REAL*8,    POINTER :: STOKES(:,:,:,:) ! c/s, N, M, date
	END TYPE AGRA__TYPE
!
!  >>>> end of include block AGRA
!
