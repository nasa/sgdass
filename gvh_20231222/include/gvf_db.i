! >>>>> INCLUDE-BLOCK with description of data structures for 
!       VLBI database in GVF ( Geo VLBI Format ) 
!
!       gvf_db.i  2007.10.14  v 3.4  Leonid Petrov  2023.02.02_21:54:11
!
	INTEGER*4    GVF_DB__MFRQ, GVF_DB__MBND, GVF_DB__MDER, GVF_DB__MSTA, &
                     GVF_DB__MCAL, GVF_DB__MBAS, GVF_DB__MSRC, GVF_DB__MSLT
!
	PARAMETER  ( GVF_DB__MFRQ =   32 )
	PARAMETER  ( GVF_DB__MBND =    2 )
	PARAMETER  ( GVF_DB__MDER =   64 )
	PARAMETER  ( GVF_DB__MSTA =   32 )
	PARAMETER  ( GVF_DB__MCAL =   32 )
	PARAMETER  ( GVF_DB__MBAS =  256 )
	PARAMETER  ( GVF_DB__MSRC = 2048 )
	PARAMETER  ( GVF_DB__MSLT =   32 )
!
	TYPE  GVF_DB_SES__TYPE
              INTEGER*4  NUMB_OBS                 ! Number of observations in the session
              INTEGER*4  NUMB_SCA                 ! Number of scans in the session
              INTEGER*4  NUMB_STA                 ! Number of sites 
              REAL*8     APLENGTH                 ! Lenght of an accumulation period (sec) 
              CHARACTER  BAND_NAM(GVF_DB__MBND)*1 ! Band names
              CHARACTER  CORPLACE*32              ! Correlator place name
              CHARACTER  COR_TYPE*8               ! Correlator type: MK3 MK4 K4 S2 VLBA MITAKA-1
              INTEGER*2  EXPSERNO                 ! Experiment serial number at correlator
              CHARACTER  EXP_CODE*32              ! Experiment 
              CHARACTER  EXP_DESC*80              ! Experiment description
              CHARACTER  EXP_NAME*80              ! Experiment program name					
              CHARACTER  MK3_DBNM*10              ! Mark-3 DBH database name					
              INTEGER*4  NUMB_SOU                 ! Number of observed sources                                   
              INTEGER*4  NUM_BAND                 ! Number of frequency bands observed in the experiment         
              INTEGER*4  NUM_CHAN                 ! Number of frequency channels at all bands                    
              INTEGER*4  NUM_CHBN(GVF_DB__MFRQ)   ! Number of frequency channels per band                        
              INTEGER*4  N_AVBAND                 ! Number of frequency bands for which information is availble  
              CHARACTER  PI_NAME*80               ! Name of the principal investigator                           
              CHARACTER  REC_MODE*80              ! Recording mode                                               
              REAL*8     SKYFRQCH(GVF_DB__MFRQ)   ! Sky frequency of channels in Hz
              REAL*8     UTC_MTAI                 ! Difference UTC minus TAI at first time tag of the database (sec)
              INTEGER*2  BITSAMPL                 ! Number of bits per sample
              REAL*8     SAMPLRAT                 ! Sample rate in Hz
              INTEGER*2  CABL_SGN(GVF_DB__MSTA)   ! Cable sign: +1 or -1
              INTEGER*4  CAL_INFO(GVF_DB__MSTA,GVF_DB__MCAL) ! Information about class and type of available calibrations
              CHARACTER  CAL_NAME(GVF_DB__MSTA)*8 ! Name of available calibrations
              REAL*8     MEANCABL(GVF_DB__MSTA)   ! Mean cable delay (sec)
              INTEGER*2  N_CALIB                  ! Number of available calibrations
              REAL*8     ATM_CNS(GVF_DB__MSTA,GVF_DB__MSLT) ! Reciprocal weights of constraints on atm. path delay rate per station, soltype
              REAL*8     ATM_INTR(GVF_DB__MSLT)   ! Lenght of time span between spline nodes for atm. path delay per soltyp (sec)
              INTEGER*4  BAS_USE(GVF_DB__MBAS)    ! Bit field of baseline selection status
              REAL*8     BSCL_CNS(GVF_DB__MBAS,GVF_DB__MSLT) ! Reciprocal weights of constraints on basdep. clock, per baseline, per soltype
              INTEGER*4  BSCL_EST(GVF_DB__MBAS)   ! Estimation status for baseline depedent clock, per baseline
              INTEGER*4  CAL_STS(GVF_DB__MSTA,GVF_DB__MCAL)  ! Bit field of using available calibrations per station, per calibration
              REAL*8     CLO_CNS(GVF_DB__MSTA,GVF_DB__MSLT)  ! Reciprocal weights of constraints on clock rate per station, per solution type
              REAL*8     CLO_INTR(GVF_DB__MSLT)   ! Lenght of time span between spline nodes for clock function per soltyp (sec)
              INTEGER*2  DATYP                    ! Type of the observable or a combination of observables used in the solution
              INTEGER*2  DGCL_EST(GVF_DB__MSTA,GVF_DB__MSLT) ! Degree of global clock function polynomial per station, per solution type
              INTEGER*4  EDIT_STS                 ! Bit field of database editing status for different types of solutions
              REAL*8     EOP_CNS(11,GVF_DB__MSLT) ! Reciprocal weights of constraints on EOP related parameters
              INTEGER*4  EOP_EST(11)              ! Estimation status for EOP-related parameters
              INTEGER*4  NUM_CLBR                 ! Number of clock breaks in the experiment
              INTEGER*4  NUM_CLRF                 ! Number of clock reference stations
              CHARACTER  RWBASNAM(GVF_DB__MBAS)*16 ! Baseline names for additive baseline-dependent reweighting parameters
              REAL*8     RWDELVAL(GVF_DB__MSLT,GVF_DB__MBAS) ! Additive baseline-dependent reweighting parameters for delays (sec)
              REAL*8     RWRATVAL(GVF_DB__MSLT,GVF_DB__MBAS) ! Additive baseline-dependent reweighting parameters for delay rates (d/l)
              REAL*8     SOCO_CNS(GVF_DB__MSRC,GVF_DB__MSLT) ! Reciprocal weights of constraints on source coordinates per object, per soltype
              INTEGER*4  SOCO_EST(2,GVF_DB__MSRC) ! Estimation status for source coordinats per component, per object
              INTEGER*4  SOU_USE(GVF_DB__MSRC)    ! Bit field of source selection status
              CHARACTER  STA_CLRF*8               ! Names of clock reference stations
              INTEGER*4  STA_USE(GVF_DB__MSTA)    ! Bit field of station selection status
              REAL*8     STPS_CNS(GVF_DB__MSTA,GVF_DB__MSLT) ! Reciprocal weights of constraints on site positions per site, per solution type
              INTEGER*4  STPS_EST(3,GVF_DB__MSTA) ! Estimation status for station positions per component, per station
              INTEGER*2  SUPMET                   ! Code of the suppression method used in the solution
              INTEGER*4  TEC_STS(GVF_DB__MSTA)    ! Flag of availability/usage of the external ionosphere calibration
              REAL*8     TIL_INTR(GVF_DB__MSLT)   ! Lenght of time span between spline nodes for atmospphere tilt per soltyp (sec)
              REAL*8     TLOF_CNS(GVF_DB__MSTA,GVF_DB__MSLT) ! Reciprocal weights of constraints on atm. tilt offset per station, per soltype
              REAL*8     TLRT_CNS(GVF_DB__MSTA,GVF_DB__MSLT) ! Reciprocal weights of constraints on atm. tilt rate per station, per soltype
              REAL*8     EOP_TAB(15,3)            ! Table of aprori EOP as Euler angles with frequencies > 2 cpd filtered out
              INTEGER*4  MJD_EOP                  ! Modified Julian date of the first epoch for the table of apriori EOP
              INTEGER*4  N_APREOP                 ! Number of nodes with apriori EOP
              INTEGER*4  STEP_EOP                 ! Step of the EOP table of apriori EOP (sec)
              INTEGER*4  TAI_EOP                  ! TAI time tag of first epoch of the table of apriori EOP (sec)
	      CHARACTER  TH_PROG*64               ! Name and version of the program which computed theoretical path delays
              CHARACTER  TH_RUDAT*24              ! Date and time of theoretical delay compuation
        END   TYPE GVF_DB_SES__TYPE
!
	TYPE      GVF_DB_STA__TYPE
              REAL*8     SIT_COOR(3)
	      CHARACTER  SITNAMES*8
	END TYPE  GVF_DB_STA__TYPE
!
	TYPE      GVF_DB_SOU__TYPE
              REAL*8     SOU_COOR(2)
              CHARACTER  SRCNAMES*8
	END TYPE  GVF_DB_SOU__TYPE
!
	TYPE      GVF_DB_OBS__TYPE
              REAL*8     DEL_RATE(GVF_DB__MBND)   ! Phase delay rate delays per band (d/l)
              REAL*8     GDAMBSP(GVF_DB__MBND)    ! Group delay ambiguity spacings per band (sec)
              REAL*8     GRDELERR(GVF_DB__MBND)   ! Group delay errors per band (sec)
              REAL*8     GR_DELAY(GVF_DB__MBND)   ! Group delay errors per band (sec)
              REAL*8     ION_GDEL                 ! Ionospheric contribution to group delay at the first band (sec)
              REAL*8     ION_GERR                 ! Uncertainty of ionospheric contribution to group delay at the fband (sec)
              REAL*8     ION_PRAT                 ! Ionospheric contribution to phase delay rate at the first band (sec)
              REAL*8     ION_RERR                 ! Uncertainty of ionospheric contribution to phase delay rate at the 1st band (d/l)
              INTEGER*4  MJD_OBS                  ! MJD of fringe reference time at pseudo-UTC timecale for the scan (days)
              REAL*8     PHRATERR(GVF_DB__MBND)   ! Phase delay rate delay errors per band (d/l)
              CHARACTER  QUALCODE(GVF_DB__MBND)*2 ! Quality code as char*2 value: 5-9 is good, 0 -- non-detection, leter -- failure
              REAL*8     REF_FREQ(GVF_DB__MBND)   ! Reference frequency for phase delay per band (Hz)
              REAL*8     SBDELERR(GVF_DB__MBND)   ! Single-band delay errors per band (sec)
              REAL*8     SB_DELAY(GVF_DB__MBND)   ! Single-band delays per band (sec)
              CHARACTER  SCANNAME*16              ! Scan name
              REAL*8     SNRATIO(GVF_DB__MBND)    ! Fringe amplitude signal to noise ratio (d/l)
              INTEGER*4  SOU_IND                  ! Source name index
              INTEGER*4  STA_IND(2)               ! Station names indexes
              REAL*8     STOP_UTC                 ! Stop time tag in UTC of the last processed bit (sec)
              REAL*8     STRT_UTC                 ! Start time tag in UTC of the first processed bit (sec)
              REAL*8     TOTPHASE(GVF_DB__MBND)   ! Total fringe phases at time of arrival singal at station 1 per band (rad)
              REAL*8     UTC_OBS                  ! Pseudo-UTC time tag of fringe reference time for the scan (sec)
              REAL*8     APSTADEL(2,GVF_DB__MBND) ! Apriori correlator station delay: at station X and at station Y (sec)
              REAL*8     APSTARAT(2,GVF_DB__MBND) ! Apriori correlator station delay date: at station X and at station Y (d/l)
              REAL*8     CORCLOFS                 ! Apriori clock offset used by the correlator (sec)
              REAL*8     CORCLRAT                 ! Apriori clock rate used by the correlator (d/l)
              REAL*8     FRN_AMPL(GVF_DB__MBND)   ! Normalzied fringe amplitude in range [0, 1]
              INTEGER*2  IND_CHN1(GVF_DB__MFRQ)   ! Indexes of channels used in bandwidth synthesis in band 1
              INTEGER*2  IND_CHN2(GVF_DB__MFRQ)   ! Indexes of channels used in bandwidth synthesis in band 2
              INTEGER*2  NUM_AP1(GVF_DB__MFRQ,2)  ! Number of accumulation periods used in band 1 per channel per sideband
              INTEGER*2  NUM_AP2(GVF_DB__MFRQ,2)  ! Number of accumulation periods used in band 2 per channel per sideband
              REAL*8     NUM_SAM1(GVF_DB__MFRQ,2) ! Number of samples used in bandwidth synth. in band 1 per freq. chan and sideband
              REAL*8     NUM_SAM2(GVF_DB__MFRQ,2) ! Number of samples used in bandwidth synth. in band 2 per freq. chan and sideband
              INTEGER*2  NUSEDCHN(GVF_DB__MBND)   ! Number of channels used in bandwidth synthesis per band
              REAL*4     PC_CHN1(GVF_DB__MFRQ)    ! Phase cal data, real, image per channel at 1st band (d/l)
              REAL*4     PC_CHN2(GVF_DB__MFRQ)    ! Phase cal data, real, image per channel at 2nd band (d/l)
              REAL*8     SCAN_DUR(GVF_DB__MBND)   ! Scan duration per band (sec)
              REAL*4, POINTER :: UV_CHN1(:)       ! UV data: real and image part per channel at the 1st band (d/l)
              REAL*4, POINTER :: UV_CHN2(:)       ! UV data: real and image part per channel at the 2nd band (d/l)
              REAL*8     AIR_TEMP(2)              ! Air temperature at the station (K)
              REAL*8     ATM_PRES(2)              ! Atmospheric pressure at the station (Pa)
              REAL*8     CABL_DEL(2)              ! Cable delay (sec)
              REAL*8     REL_HUMD(2)              ! Relative humidity at the station (0-1)
              REAL*8     ZDRY_GPS(2)              ! Zenith hydrostatic path delay from GPS (sec)
              REAL*8     ZWET_GPS(2)              ! Zenith wet path delay from GPS (sec)
              INTEGER*4  AUTO_SUP                 ! Bit field of automatic suppression status for combination of observables
              INTEGER*2  BAND_2ND                 ! Bit field with status of information about the second band observations
              REAL*8     EFF_FREQ(3,GVF_DB__MBND) ! Effective ionospheric frequencies for gr.del, ph.del, ph.rate per band (Hz)
              INTEGER*4  N_GRAMB(GVF_DB__MBND)    ! Number of group delay ambiguities to be added to measured group delays per band
              INTEGER*4  N_PHAMB(GVF_DB__MBND)    ! Number of phase delay ambiguities to be added to measured phase delays per band
              INTEGER*4  USER_REC                 ! Bit field of analyst defined recovery status for combination of observables
              INTEGER*4  USER_SUP                 ! Bit field of analyst defined suppression status for combination of observables
	      INTEGER*4  PIND_OBS                 ! Intenral observation index used by PIMA
              REAL*8     APR_EOP(3,2)             ! Aprori EOP array as Euler angles and its derivatives (rad)
              REAL*8     AZIMUTH(2)               ! Apparent source azimuth at both stations of the baseline (rad)
              REAL*8     DER_DEL(GVF_DB__MDER)    ! Array of partial derivatives of theoretical path delay wrt parameters of the mod
              REAL*8     DER_RAT(GVF_DB__MDER)    ! Array of partial derivatives of theoretical delay rate wrt parameters of the mod
              REAL*8     ELEV(2)                  ! Apparent source elevation at both stations of the baseline (rad)
              REAL*8     NUT_DER(2)               ! Partial derivatives wth nutation deaily offset parameters (sec)
              REAL*8     THGR_DEL                 ! Theoretical group delay (sec)
              REAL*8     THPH_DEL                 ! Theoretical phase delay (sec)
              REAL*8     THPH_RAT                 ! Theoretical phase delay date (d/l)
	      REAL*8     UV_COOR(2)               ! UV projections of the baseline vector on the plane normal to the source direction
	      REAL*8     TSYS(2,GVF_DB__MBND)     ! System temprature for both bands in K
	      REAL*8     GAIN(2,GVF_DB__MBND)     ! gain for both bands (d/l)
	END TYPE  GVF_DB_OBS__TYPE
!
	INTEGER*4    GVF_DB__UNDF, GVF_DB__READ, GVF_DB__FILL
	PARAMETER  ( GVF_DB__UNDF =    0  ) ! Undefined
	PARAMETER  ( GVF_DB__READ = 19001 ) ! Input GVH files were read
	PARAMETER  ( GVF_DB__FILL = 19002 ) ! GVF_DB object is filled with data
!
	TYPE  GVF_DB__TYPE
	      INTEGER*4  STATUS
              TYPE ( GVF_DB_SES__TYPE ) ::  SES
              TYPE ( GVF_DB_STA__TYPE ), POINTER :: STA(:)
              TYPE ( GVF_DB_SOU__TYPE ), POINTER :: SOU(:)
              TYPE ( GVF_DB_OBS__TYPE ), POINTER :: OBS(:)
	      CHARACTER  DB_NAME*10
        END   TYPE GVF_DB__TYPE
!
        INTEGER*4   GR__DTP_PAR, GXS__DTP_PAR, GX__DTP_PAR, GS__DTP_PAR, FUSED__DTP_PAR  
        PARAMETER ( GR__DTP_PAR  =  3 )
        PARAMETER ( GXS__DTP_PAR =  7 )
        PARAMETER ( GX__DTP_PAR  = 15 )
        PARAMETER ( GS__DTP_PAR  = 16 )
        PARAMETER ( FUSED__DTP_PAR  = 21 )
!
        INTEGER*4    GVH__NLC
        PARAMETER  ( GVH__NLC = 287 )
        INTEGER*4    GVH__FR1, GVH__FR2, GVH__CL1, GVH__TH1, GVH__SL1, GVH__IGN, GVH__PRC
!
        PARAMETER  ( GVH__FR1 =  1 )
        PARAMETER  ( GVH__FR2 =  2 )
        PARAMETER  ( GVH__CL1 =  3 )
        PARAMETER  ( GVH__SL1 =  4 )
        PARAMETER  ( GVH__TH1 =  5 )
        PARAMETER  ( GVH__IGN = 11 )
        PARAMETER  ( GVH__PRC = 12 )
!
        INTEGER*4    NN$IND
        CHARACTER    GVH__LC(GVH__NLC)*8
        INTEGER*4    GVH__TBI(GVH__NLC)
        DATA         ( GVH__LC(NN$IND), GVH__TBI(NN$IND), NN$IND=1, GVH__NLC ) &
     &     / &
     &     'APLENGTH',  GVH__FR1, & !   1  Length of accumul. period in sec
     &     'BAND_NAM',  GVH__FR1, & !   2  Band names
     &     'CORCLOCK',  GVH__FR1, & !   3  Clock offset ref, rem [sec], rate ref, rem [sec/sec]
     &     'CORPLACE',  GVH__FR1, & !   4  Correlator place name
     &     'COR_TYPE',  GVH__FR1, & !   5  Correlator type: MK3 MK4 K4 S2 VLBA MITAKA-1
     &     'DEL_RATE',  GVH__FR1, & !   6  Phase delay rate delays per band (d/l)
     &     'DTEC    ',  GVH__FR1, & !   7  Difference of the total electron contents, TEC units
     &     'DTEC_SIG',  GVH__FR1, & !   8  Standard deviation of dTec estimation, TEC units
     &     'EXPSERNO',  GVH__FR1, & !   9  Experiment serial number at correlator
     &     'EXP_DESC',  GVH__FR1, & !  10  Experiment description
     &     'EXP_NAME',  GVH__FR1, & !  11  Experiment program name
     &     'FOURFFIL',  GVH__IGN, & !  12  Fourfit output filename.
     &     'FOURFUTC',  GVH__IGN, & !  13  Fourfit processing time YMDHMS.
     &     'FOURFVER',  GVH__FR1, & !  14  Fourfit version number.
     &     'FOURF_CF',  GVH__IGN, & !  15  Control file name for fourfit
     &     'FOURF_CS',  GVH__IGN, & !  16  Command string used for fourfit
     &     'GDAMBSP ',  GVH__FR1, & !  17  Group delay ambiguity spacings per band (sec)
     &     'GRDELERR',  GVH__FR1, & !  18  Group delay errors per band (sec)
     &     'GR_DELAY',  GVH__FR1, & !  19  Group delays per band (sec)
     &     'HOPS_VER',  GVH__FR1, & !  20  HOPS software revision number.
     &     'INTRVAL4',  GVH__FR1, & !  21  First and last UTC time tag in input file.
     &     'ION_BITS',  GVH__FR1, & !  22  ICORR for full ion tracking.
     &     'ION_GDEL',  GVH__FR1, & !  23  Ionospheric contribution to group delay at the first band (sec)
     &     'ION_GERR',  GVH__FR1, & !  24  Uncertainty of ionospheric contribution to group delay at the first band (sec)
     &     'ION_PRAT',  GVH__FR1, & !  25  Ionospheric contribution to phase delay rate at the first band (sec)
     &     'ION_RERR',  GVH__FR1, & !  26  Uncertainty of ionospheric contribution to phase delay rate at the 1st band (d/l)
     &     'MJD_OBS ',  GVH__FR1, & !  27  MJD of fringe reference time at pseudo-UTC timecale for the scan (days)
     &     'MK3_DBNM',  GVH__FR1, & !  28  Mark-3 DBH database name
     &     'NOBS_STA',  GVH__FR1, & !  29  Number of observations per site
     &     'NUMB_OBS',  GVH__FR1, & !  30  Number of observations in the session
     &     'NUMB_SCA',  GVH__FR1, & !  31  Number of scans in the session
     &     'NUMB_SOU',  GVH__FR1, & !  32  Number of observed sources
     &     'NUMB_STA',  GVH__FR1, & !  33  Number of sites
     &     'NUM_CHAN',  GVH__FR1, & !  34  Number of frequency channels at all bands
     &     'NUM_CHBN',  GVH__FR1, & !  35  Number of frequency channels per band
     &     'NUSEDCHN',  GVH__FR1, & !  36  Number of channels used in bandwidth synthesis per band
     &     'N_AVBAND',  GVH__FR1, & !  37  Number of frequency bands for which information is availble
     &     'OBS_TAB ',  GVH__FR1, & !  38  Observation tables: scan index, indices of the first and the second station
     &     'PHRATERR',  GVH__FR1, & !  39  Phase delay rate delay errors per band (d/l)
     &     'PI_NAME ',  GVH__FR1, & !  40  Name of the principal investigator
     &     'REC_MODE',  GVH__FR1, & !  41  Recording mode
     &     'REF_FREQ',  GVH__FR1, & !  42  Reference frequency for phase delay per band (Hz)
     &     'SBDELERR',  GVH__FR1, & !  43  Single-band delay errors per band (sec)
     &     'SB_DELAY',  GVH__FR1, & !  44  Single-band delays per band (sec)
     &     'SITNAMES',  GVH__FR1, & !  45  IVS site names
     &     'SIT_COOR',  GVH__FR1, & !  46  Site coordinates in a crust-fixed terrestrial reference system: X, Y, Z (meters)
     &     'SKYFRQCH',  GVH__FR1, & !  47  Sky frequency of channels in Hz
     &     'SNRATIO ',  GVH__FR1, & !  48  Fringe amplitude signal to noise ratio (d/l)
     &     'SOU_COOR',  GVH__FR1, & !  49  Source coordinates in a baricenteric reference system: right asc. decl. (rad)
     &     'SOU_IND ',  GVH__FR1, & !  50  Source name index
     &     'SRCNAMES',  GVH__FR1, & !  51  Source names
     &     'STA_IND ',  GVH__FR1, & !  52  Station names indexes
     &     'TOTPHASE',  GVH__FR1, & !  53  Total fringe phases at time of arrival singal at station 1 per band (rad)
     &     'UTC_MTAI',  GVH__FR1, & !  54  Difference UTC minus TAI at first time tag of the database (sec)
     &     'UTC_OBS ',  GVH__FR1, & !  55  Pseudo-UTC time tag of fringe reference time for the scan (sec)
!                                                                                                                                  
     &     'BITSAMPL',  GVH__FR2, & !  56  Number of bits per sample
     &     'NUM_AP1 ',  GVH__FR2, & !  57  Number of accumulation periods used in band 1 per channel per sideband (USB, LSB)
     &     'NUM_AP2 ',  GVH__FR2, & !  58  Number of accumulation periods used in band 2 per channel per sideband (USB, LSB)
     &     'SAMPLRAT',  GVH__FR2, & !  59  Sample rate (Hz)
     &     'POLARZ1 ',  GVH__FR2, & !  60  Space separated polarization per sta/chan in band 1
     &     'POLARZ2 ',  GVH__FR2, & !  61  Space separated polarization per sta/chan in band 2
!
     &     'AIR_TEMP',  GVH__CL1, & !  62  Air temperature at the station (K)
     &     'ATM_PRES',  GVH__CL1, & !  63  Atmospheric pressure at the station (Pa)
     &     'CABL_DEL',  GVH__CL1, & !  64  Cable delay (sec)
     &     'CABL_SGN',  GVH__CL1, & !  65  Cable sign: +1, 0 or -1
     &     'CBLS_SET',  GVH__CL1, & !  66  A set of cable correctons from various sources: FS log, CDMS, PCMT; (sec)
     &     'REL_HUMD',  GVH__CL1, & !  67  Relative humidity at the station (0-1)
     &     'METEONTP',  GVH__CL1, & !  68  Meteo data origin type: undef(0), FS log file(1), external(2)
     &     'METEONTX',  GVH__CL1, & !  69  Meteo data origin text
     &     'CABLONTP',  GVH__CL1, & !  70  Cab calibration origin type: FS log file(1), CDMS(2), PCMT(3), ...
     &     'CABLONTX',  GVH__CL1, & !  71  Cable calibration origin text
!
     &     'EFF_FREQ',  GVH__SL1, & !  72  Effective ionospheric frequency
     &     'MJD_CLBR',  GVH__SL1, & !  73  Integer part of MJD of clock break epochs
     &     'NUM_CLBR',  GVH__SL1, & !  74  Number of clock breaks in the experiment
     &     'N_GRAMB ',  GVH__SL1, & !  75  Number of group delay ambiguities to be added to measured group delays per band
     &     'STA_CLBR',  GVH__SL1, & !  76  Names of stations with clock breaks
     &     'SUPMET  ',  GVH__SL1, & !  77  Code of the suppression method used in the solution
     &     'UTC_CLBR',  GVH__SL1, & !  78  UTC time tag of clock break, seconds since 0:0:0
!
     &     'APBYFRQ1',  GVH__PRC, & !  79  Amp(0-1), phs (-180 to 180) by chan in band 1
     &     'APBYFRQ2',  GVH__PRC, & !  80  Amp(0-1), phs (-180 to 180) by chan in band 2
     &     'ATM_CNST',  GVH__PRC, & !  81  Atmosphere constraint. ps/hr
     &     'ATM_INTV',  GVH__PRC, & !  82  Batchmode atmos interval - hours
     &     'BASLSTAT',  GVH__PRC, & !  83  Baseline selection bit maped array. 1=some obs, etc.
     &     'BLDEPCKS',  GVH__PRC, & !  84  Bl-dependent clock list
     &     'CAL_FLGS',  GVH__PRC, & !  85  Bit set indicate that calibration is recommended for stations
     &     'CAL_LIST',  GVH__PRC, & !  86  Station depedendent calibrations (Cable, Phase,  etc?)
     &     'CLK_CNST',  GVH__PRC, & !  87  Clock constraint-Parts in 1.e14
     &     'CLK_INTV',  GVH__PRC, & !  88  Batchmode clock interval - hours
     &     'CLK_SITS',  GVH__PRC, & !  89  List of clock reference stations
     &     'COHERCOR',  GVH__PRC, & !  90  Corr coeff (0 --> 1)
     &     'DELUFLAG',  GVH__PRC, & !  91  Delay unweight flag
     &     'EFF.DURA',  GVH__PRC, & !  92  Effective run duration sec
     &     'ERROR_K ',  GVH__PRC, & !  93  Group delay and rate re-weighting constants
     &     'ERROR_BL',  GVH__PRC, & !  94  B.L.names for formal errors
     &     'EXP_CODE',  GVH__PRC, & !  95  Experiment code
     &     'FRTYPFIT',  GVH__PRC, & !  96  Fringe type. 1-99 reserved for HOPS, 101-199 reserved for PIMA
     &     'FSCANAME',  GVH__PRC, & !  97  Full scan name
     &     'IONRMS  ',  GVH__PRC, & !  98  Ion correction sigma for delay (sec) and rate (unitless) per band
     &     'ION_CORR',  GVH__PRC, & !  99  Ion correction for delay (sec) and rate (unitless) per band
     &     'NSAMPLS1',  GVH__PRC, & ! 100  Number of samples per channel, sideband (USB, LSB) in band 1
     &     'NSAMPLS2',  GVH__PRC, & ! 101  Number of samples per channel, sideband (USB, LSB) in band 1
     &     'NUM_BAND',  GVH__PRC, & ! 102  Number of frequency bands observed in the experiment
     &     'PHSUFLAG',  GVH__PRC, & ! 103  Phase delay unweight flag
     &     'RFREQ1  ',  GVH__PRC, & ! 104  RF freq by channel (MHz) in band 1
     &     'RFREQ2  ',  GVH__PRC, & ! 105  RF freq by channel (MHz) in band 2
     &     'QUALCODE',  GVH__PRC, & ! 106  Quality code as char*2 value: 5-9 is good, 0 -- non-detection, letter -- failure
     &     'SCANNAME',  GVH__PRC, & ! 107  Scan name
     &     'SOURSTAT',  GVH__PRC, & ! 108  Source selection status bit-mapped array
     &     'UACSUP  ',  GVH__PRC, & ! 109  User action for suppression
     &     'UNPHASCL',  GVH__PRC, & ! 110  UnPhaseCal effect, group delay for first and second station
!
     &     'ABASACCE',  GVH__IGN, & ! 111  Corel bas/apr accel (1/sec**2)
     &     'ABASDEL ',  GVH__IGN, & ! 112  Corel bas/apr delay (sec)
     &     'ABASRATE',  GVH__IGN, & ! 113  Corel bas/apr delay rate (s/s)
     &     'ATI_CFLG',  GVH__IGN, & ! 114  ATIME Flow Control Message Def.
     &     'ATI_MESS',  GVH__IGN, & ! 115  ATIME Message Definition
     &     'ATM_CFLG',  GVH__IGN, & ! 116  Atmosphere control flag mess def
     &     'ATM_MESS',  GVH__IGN, & ! 117  Atmosphere message definition
     &     'AXISOFFS',  GVH__IGN, & ! 118  Axis offsets (m).
     &     'AXISTYPS',  GVH__IGN, & ! 119  Axis type (1-eq 2-xy 3-azel 4 5)
     &     'AXO_CFLG',  GVH__IGN, & ! 120  Axis Offset Control flag mes def
     &     'AXO_CONT',  GVH__IGN, & ! 121  New Axis Offset Contributions
     &     'AXO_MESS',  GVH__IGN, & ! 122  Axis Offset Message Definition
     &     'AXO_PART',  GVH__IGN, & ! 123  Axis Offset partial deriv. def.
     &     'AZ-THEO ',  GVH__IGN, & ! 124  Azimuth array definition
     &     'BBC_IDX1',  GVH__IGN, & ! 125  Physical BBC number per channel per station in band 1
     &     'BBC_IDX2',  GVH__IGN, & ! 126  Physical BBC number per channel per station in band 2
     &     'BENDPART',  GVH__IGN, & ! 127  Grav. bend. partial w.r.t. Gamma
     &     'CALCFLGN',  GVH__IGN, & ! 128  CALC flow control flags name def
     &     'CALCFLGV',  GVH__IGN, & ! 129  CALC flow control flags valu def
     &     'CALC_VER',  GVH__IGN, & ! 130  CALC version number
     &     'CF2J2K_0',  GVH__IGN, & ! 131  Crust-fixed to J2000 Rot. Matrix
     &     'CF2J2K_1',  GVH__IGN, & ! 132  Crust-fixed to J2000 Rot. Matrix rate
     &     'CF2J2K_2',  GVH__IGN, & ! 133  Crust-fixed to J2000 Rot. Matrix accel
     &     'CHANID1 ',  GVH__IGN, & ! 134  Space separated one-letter Fourfit channel IDs in band 1
     &     'CHANID2 ',  GVH__IGN, & ! 135  Space separated one-letter Fourfit channel IDs in band 2
     &     'CI_NUM1 ',  GVH__IGN, & ! 136  Corel index numbers in band 1 per channel per sideband (USB, LSB)
     &     'CI_NUM2 ',  GVH__IGN, & ! 137  Corel index numbers in band 2 per channel per sideband (USB, LSB)
     &     'CONSNDEL',  GVH__IGN, & ! 138  Consensus theoretical delay, sec
     &     'CONSNRAT',  GVH__IGN, & ! 139  Consensus theoretical rate, sec/sec
     &     'CONSPART',  GVH__IGN, & ! 140  Consensus partial w.r.t. Gamma
     &     'CON_CONT',  GVH__IGN, & ! 141  Consensus bending contrib. (sec)
     &     'CORBASCD',  GVH__IGN, & ! 142  Correlator baseline code (2 ch).
     &     'CORR_UTC',  GVH__IGN, & ! 143  UTC time tag of correlation.
     &     'CROOTFIL',  GVH__IGN, & ! 144  Correlator root file name
     &     'CTI_CFLG',  GVH__IGN, & ! 145  CTIMG Flow Control Message Def
     &     'CTI_MESS',  GVH__IGN, & ! 146  CTIMG Message Definition
     &     'CT_SITE1',  GVH__IGN, & ! 147  Coordinate time at site 1
     &     'DELRESID',  GVH__IGN, & ! 148  Delay residual (sec).
     &     'DELTAEPO',  GVH__IGN, & ! 149  Offset from center of scan (sec)
     &     'DISCARD ',  GVH__IGN, & ! 150  Percent data discarded by FRNGE
     &     'EARTH_CE',  GVH__IGN, & ! 151  Earth barycentric coordinates, vels and accs
     &     'ECCCOORD',  GVH__IGN, & ! 152  Eccentricity taken from eccentricity file.
     &     'ECCNAMES',  GVH__IGN, & ! 153  Eccentricity monument name
     &     'ECCTYPES',  GVH__IGN, & ! 154  Eccentricity type: XY or NE
     &     'EFF_FREW',  GVH__IGN, & ! 155  Effective equal weighted ionospheric frequencies for gr.del, ph.del, ph.rate per band (Hz)
     &     'EL-THEO ',  GVH__IGN, & ! 156  Elevation array definition
     &     'ERRATE_1',  GVH__IGN, & ! 157  Log err rate per channel per station in band 1
     &     'ERRATE_2',  GVH__IGN, & ! 158  Log err rate per channel per station in band 2
     &     'ETD_CFLG',  GVH__IGN, & ! 159  Earth Tide flow control mess def
     &     'ETD_CONT',  GVH__IGN, & ! 160  Earth tide contributions def.
     &     'ETD_DATA',  GVH__IGN, & ! 161  Earth tide module data (la. h l)
     &     'ETD_MESS',  GVH__IGN, & ! 162  Earth Tide message definition
     &     'FALSEDET',  GVH__IGN, & ! 163  Prob of false det from FRNGE
     &     'FCL_FLGS',  GVH__IGN, & ! 164  Standard flcal configuration for stations
     &     'FCL_LIST',  GVH__IGN, & ! 165  Key to the standard flcal config
     &     'FEED.COR',  GVH__IGN, & ! 166  Feedhorn corr. in CORFIL scheme [per band]
     &     'FRNGERR ',  GVH__IGN, & ! 167  Fourfit error flag blank=OK.
     &     'FUT1TEXT',  GVH__IGN, & ! 168  Final Value TAI-UT1 origin text.
     &     'FUT1_INF',  GVH__IGN, & ! 169  Array: (FJD of start, spacing in days, number points, Scaling (should be 1))
     &     'FUT1_PTS',  GVH__IGN, & ! 170  Final Value TAI-UT1 data points.
     &     'FWOBTEXT',  GVH__IGN, & ! 171  Final Value wobble origin text.
     &     'FWOBX&YT',  GVH__IGN, & ! 172  Final wobble X Y component value
     &     'FWOB_INF',  GVH__IGN, & ! 173  Array: (FJD of start, spacing in days, number points)
     &     'GCRESPHS',  GVH__IGN, & ! 174  Resid phs corrected to cen of E.
     &     'GC_MBD  ',  GVH__IGN, & ! 175  Tot geocenter group delay (sec)
     &     'GC_PHASE',  GVH__IGN, & ! 176  Tot phase ref to cen of Earth
     &     'GC_RATE ',  GVH__IGN, & ! 177  Tot geocenter delay rate (s/s)
     &     'GC_SBD  ',  GVH__IGN, & ! 178  Tot geocenter sbd delay (sec)
     &     'IDELAY  ',  GVH__IGN, & ! 179  Corel instrumental delay (sec)
     &     'INCOH2  ',  GVH__IGN, & ! 180  Incoh amp from FRNGE plot segs.
     &     'INCOHAMP',  GVH__IGN, & ! 181  Fr. amp from incoh int of chan.
     &     'IONDTFLG',  GVH__IGN, & ! 182  Ion correction data flag per band. 0=OK, -1=Missing, -2=bad
     &     'LO_FREQ1',  GVH__IGN, & ! 183  LO frequencies per cha/sta MHz in band 1
     &     'LO_FREQ2',  GVH__IGN, & ! 184  LO frequencies per cha/sta MHz in band 2
     &     'MOONDATA',  GVH__IGN, & ! 185  Lunar geocentric coordinates and velocities
     &     'NDRYCONT',  GVH__IGN, & ! 186  Nhmf (dry) atm. contribution
     &     'NDRYPART',  GVH__IGN, & ! 187  Nhmf2 dry partial deriv. def.
     &     'NGRADPAR',  GVH__IGN, & ! 188  Niell dry atm. gradient partials
     &     'NLAGS   ',  GVH__IGN, & ! 189  Num of lags used for correlation
     &     'NUT06XYP',  GVH__IGN, & ! 190  2000/2006 Nut/Prec X,Y Partials
     &     'NUT06XYS',  GVH__IGN, & ! 191  2000/2006 Nut/Prec X,Y, S & Rates
     &     'NUT2006A',  GVH__IGN, & ! 192  IAU2006A Nut. - Dpsi  Deps  Rates
     &     'NUT_CFLG',  GVH__IGN, & ! 193  Nutation message definition
     &     'NUT_MESS',  GVH__IGN, & ! 194  Nutation flow control mess def.
     &     'NUT_WAHR',  GVH__IGN, & ! 195  Wahr nut vals  - Dpsi Deps&rates
     &     'NWETCONT',  GVH__IGN, & ! 196  Whmf (wet) atm. contribution
     &     'NWETPART',  GVH__IGN, & ! 197  Whmf2 wet partial deriv. def.
     &     'OBCLFLGS',  GVH__IGN, & ! 198  Bit set indicate that calibration is recommended for observations
     &     'OBCLLIST',  GVH__IGN, & ! 199  Available obs dependent calibrations (poletide, earthdide, ?)
     &     'OCE_CFLG',  GVH__IGN, & ! 200  Ocean load flow control mess def
     &     'OCE_CONT',  GVH__IGN, & ! 201  Obs dependent ocean loading
     &     'OCE_DELD',  GVH__IGN, & ! 202  Ocean load site dependent displace
     &     'OCE_HORZ',  GVH__IGN, & ! 203  Site-dep ocean cont - horizontal
     &     'OCE_MESS',  GVH__IGN, & ! 204  Ocean loading message definition
     &     'OCE_OLD ',  GVH__IGN, & ! 205  Add to Cal-OceanLoad to get Cal10 OceanLoading
     &     'OCE_STAT',  GVH__IGN, & ! 206  Ocean loading station status.
     &     'OCE_VERT',  GVH__IGN, & ! 207  Site-dep ocean cont - vertical
     &     'OPTLCOEF',  GVH__IGN, & ! 208  Ocean Pole Tide loading Coefficients
     &     'OPTLCONT',  GVH__IGN, & ! 209  Ocean Pole Tide Load Contribution
     &     'PAN_MESS',  GVH__IGN, & ! 210  Feedhorn rot. angle mod. ident.
     &     'PARANGLE',  GVH__IGN, & ! 211  Feedhorn rot. angle
     &     'PEP_MESS',  GVH__IGN, & ! 212  PEP Utility Message Definition
     &     'PHCAMP_1',  GVH__IGN, & ! 213  Phase cal amplitudes by channel by station in band 1
     &     'PHCAMP_2',  GVH__IGN, & ! 214  Phase cal amplitudes by channel by station in band 2
     &     'PHCFRQ_1',  GVH__IGN, & ! 215  Phase cal freqs by channel by station in band 1
     &     'PHCFRQ_2',  GVH__IGN, & ! 216  Phase cal freqs by channel by station in band 2
     &     'PHCOFF_1',  GVH__IGN, & ! 217  Phase cal offsets (-18000 to 18000) by channel by station in band 1
     &     'PHCOFF_2',  GVH__IGN, & ! 218  Phase cal offsets (-18000 to 18000) by channel by station in band 2
     &     'PHCPHS_1',  GVH__IGN, & ! 219  Phase cal phases (-18000 to 18000) by channel by station in band 1
     &     'PHCPHS_2',  GVH__IGN, & ! 220  Phase cal phases (-18000 to 18000) by channel by station in band 2
     &     'PHC_RATE',  GVH__IGN, & ! 221  PC rate by sta ( us per s)
     &     'PLX1PSEC',  GVH__IGN, & ! 222  Parallax partial/contr  1 parsec
     &     'PLX_CFLG',  GVH__IGN, & ! 223  Parallax flow control mess def
     &     'PLX_MESS',  GVH__IGN, & ! 224  Parallax message definition
     &     'PLX_PART',  GVH__IGN, & ! 225  Parallax partial deriv. def.
     &     'POLAR_XY',  GVH__IGN, & ! 226  Polar motion X & Y for obs (rad)
     &     'PRE_DATA',  GVH__IGN, & ! 227  Precession constant (asec/cent).
     &     'PTDXYPAR',  GVH__IGN, & ! 228  Pole Tide Partials w.r.t. X & Y
     &     'PTD_CFLG',  GVH__IGN, & ! 229  Pole tide flow control mess def
     &     'PTD_CONT',  GVH__IGN, & ! 230  Pole tide contributions def.
     &     'PTD_MESS',  GVH__IGN, & ! 231  Pole tide message definition
     &     'PTOLDCON',  GVH__IGN, & ! 232  Old Pole Tide Restorer Contrib.
     &     'QBFACTOR',  GVH__IGN, & ! 233  Measure of uniformity of data.
     &     'RATRESID',  GVH__IGN, & ! 234  Rate resid (sec per sec)
     &     'RATUFLAG',  GVH__IGN, & ! 235  Delay rate unweight flag
     &     'REL_CFLG',  GVH__IGN, & ! 236  Relativisitc bending use status
     &     'REL_DATA',  GVH__IGN, & ! 237  Relativity mod data (gamma).
     &     'SBRESID ',  GVH__IGN, & ! 238  Single band delay residual
     &     'SCAN_UTC',  GVH__IGN, & ! 239  Nominal scan time YMDHMS.
     &     'SITEZENS',  GVH__IGN, & ! 240  Site zenith path delays (nsec).
     &     'SITHSOAM',  GVH__IGN, & ! 241  Horz south ocean loading ampltudes (m)
     &     'SITHSOPH',  GVH__IGN, & ! 242  Horz south ocean loading phases (rad).
     &     'SITHWOAM',  GVH__IGN, & ! 243  Horz west ocean loading ampltudes (m)
     &     'SITHWOPH',  GVH__IGN, & ! 244  Horz west ocean loading phases (rad).
     &     'SITOCAMP',  GVH__IGN, & ! 245  Vert ocean loading ampltudes (m)
     &     'SITOCPHS',  GVH__IGN, & ! 246  Vert ocean loading phases (rad).
     &     'SIT_MESS',  GVH__IGN, & ! 247  Site Module Message Definition
     &     'SIT_PART',  GVH__IGN, & ! 248  Site partials: dtau/dr_1=-dtau/dr_2
     &     'SRCHPAR ',  GVH__IGN, & ! 249  FRNGE/Fourfit search parameters
     &     'STARELEV',  GVH__IGN, & ! 250  Elev angles calc by COREL
     &     'STARTOFF',  GVH__IGN, & ! 251  Offset nominal start time (sec).
     &     'STARTSEC',  GVH__IGN, & ! 252  Start time in sec past hour
     &     'STAR_REF',  GVH__IGN, & ! 253  Source a priori coordinates reference
     &     'STOP_OFF',  GVH__IGN, & ! 254  Offset nominal stop time (sec).
     &     'STOP_SEC',  GVH__IGN, & ! 255  Stop  time in sec past hour
     &     'STR_CFLG',  GVH__IGN, & ! 256  Parallax flow control mess def
     &     'STR_MESS',  GVH__IGN, & ! 257  Star module message definition
     &     'STR_PART',  GVH__IGN, & ! 258  Star partial derivatives def.
     &     'SUN2CONT',  GVH__IGN, & ! 259  High order bending contrib.(sec)
     &     'SUN_CONT',  GVH__IGN, & ! 260  Consensus bending contrib. (sec)
     &     'SUN_DATA',  GVH__IGN, & ! 261  Solar geocentric coordinates and velocities
     &     'TAPQCODE',  GVH__IGN, & ! 262  Tape quality code
     &     'TECTPLNM',  GVH__IGN, & ! 263  4-char tectonic plate names
     &     'THE_MESS',  GVH__IGN, & ! 264  Theory module identification
     &     'TIDALUT1',  GVH__IGN, & ! 265  Flag for tidal terms in UT1 sers
     &     'TILTRMVR',  GVH__IGN, & ! 266  Axis Tilt Contribution Remover
     &     'URVR    ',  GVH__IGN, & ! 267  Rate derivatives mHz per asec
     &     'UT1EPOCH',  GVH__IGN, & ! 268  TAI - UT1 epoch value definition
     &     'UT1INTRP',  GVH__IGN, & ! 269  Message for UT1 interp. scheme
     &     'UT1LIBRA',  GVH__IGN, & ! 270  Hi Freq UT1 Libration Contribution
     &     'UT1ORTHO',  GVH__IGN, & ! 271  ORTHO_EOP Tidal UT1 contribution
     &     'UT1_-TAI',  GVH__IGN, & ! 272  UT1 time of day for this obsvr.
     &     'UT1_CFLG',  GVH__IGN, & ! 273  UT1 control flag message def.
     &     'UT1_MESS',  GVH__IGN, & ! 274  UT1 Module message definition
     &     'UT1_PART',  GVH__IGN, & ! 275  UT1 partial derivatives def.
     &     'UTCM_TAG',  GVH__IGN, & ! 276  UTC at central epoch YMDHMS.
     &     'UVF/ASEC',  GVH__IGN, & ! 277  U V in FR per arcsec from CALC per band
     &     'WOBEPOCH',  GVH__IGN, & ! 278  Interpolated wobble array def
     &     'WOBINTRP',  GVH__IGN, & ! 279  Interp. scheme for polar motion.
     &     'WOBLIBRA',  GVH__IGN, & ! 280  Hi Freq Wobble Libration Contribution
     &     'WOBORTHO',  GVH__IGN, & ! 281  ORTHO_EOP tidal wobble contribtn
     &     'WOBXCONT',  GVH__IGN, & ! 282  X Wobble contribution definition
     &     'WOBYCONT',  GVH__IGN, & ! 283  Y Wobble contribution definition
     &     'WOB_CFLG',  GVH__IGN, & ! 284  Wobble flow control mess def.
     &     'WOB_MESS',  GVH__IGN, & ! 285  Wobble message definition.
     &     'WOB_PART',  GVH__IGN, & ! 286  Wobble partial derivatives def.
     &     'ZDELAY  ',  GVH__IGN  & ! 287  Corel zenith atmos. delay (sec).
     &     /
!
	INTEGER*4    GVH__PRC_NUS
	PARAMETER  ( GVH__PRC_NUS = 30 )
        CHARACTER    GVH__LC_NUS(GVH__PRC_NUS)*8, GVH__LC_PSO(2,GVH__PRC_NUS)*8
        INTEGER*4    GVH__PRC_TBI(GVH__PRC_NUS)
        DATA         ( GVH__LC_NUS(NN$IND), GVH__LC_PSO(1,NN$IND), &
     &                 GVH__LC_PSO(2,NN$IND), GVH__PRC_TBI(NN$IND),  &
     &                 NN$IND=1,GVH__PRC_NUS ) &
     &     / &
     &     'APBYFRQ1',  'UV_CHN1 ', '        ', GVH__FR2, & !   1
     &     'APBYFRQ2',  'UV_CHN2 ', '        ', GVH__FR2, & !   2
     &     'ATM_CNST',  'ATM_CNS ', '        ', GVH__SL1, & !   3
     &     'ATM_INTV',  'ATM_INTR', '        ', GVH__SL1, & !   4
     &     'BASLSTAT',  'BAS_USE ', 'STA_USE ', GVH__SL1, & !   5
     &     'BLDEPCKS',  'BSCL_EST', '        ', GVH__SL1, & !   6
     &     'CAL_FLGS',  'CAL_STS ', '        ', GVH__SL1, & !   7
     &     'CAL_LIST',  'CAL_NAME', 'CAL_INFO', GVH__CL1, & !   8
     &     'CLK_CNST',  'CLO_CNS ', '        ', GVH__SL1, & !   9
     &     'CLK_INTV',  'CLO_INTR', '        ', GVH__SL1, & !  10
     &     'CLK_SITS',  'STA_CLRF', '        ', GVH__SL1, & !  11
     &     'COHERCOR',  'FRN_AMPL', '        ', GVH__FR2, & !  12
     &     'DELUFLAG',  'USER_REC', '        ', GVH__SL1, & !  13
     &     'EFF.DURA',  'SCAN_DUR', '        ', GVH__FR2, & !  14
     &     'ERROR_BL',  'RWBASNAM', '        ', GVH__SL1, & !  15
     &     'ERROR_K ',  'RWDELVAL', 'RWRATVAL', GVH__SL1, & !  16
     &     'EXP_CODE',  'EXP_CODE', '        ', GVH__FR1, & !  14
     &     'SCANNAME',  'SCANNAME', '        ', GVH__FR1, & !  17
     &     'IONRMS  ',  'ION_GERR', 'ION_RERR', GVH__FR1, & !  18
     &     'ION_CORR',  'ION_GDEL', 'ION_PRAT', GVH__FR1, & !  19
     &     'NSAMPLS1',  'NUM_SAM1', '        ', GVH__FR2, & !  20
     &     'NSAMPLS2',  'NUM_SAM2', '        ', GVH__FR2, & !  21
     &     'NUM_BAND',  'N_AVBAND', '        ', GVH__FR1, & !  22
     &     'PHSUFLAG',  '        ', '        ', GVH__PRC, & !  23
     &     'RFREQ1  ',  'IND_CHN1', 'NUSEDCHN', GVH__FR2, & !  24
     &     'RFREQ2  ',  'IND_CHN2', 'NUSEDCHN', GVH__FR2, & !  25
     &     'QUALCODE',  'QUALCODE', '        ', GVH__FR1, & !  26
     &     'SOURSTAT',  'SOU_USE ', 'SOCO_EST', GVH__SL1, & !  27
     &     'UACSUP  ',  'USER_SUP', '        ', GVH__SL1, & !  28
     &     'UNPHASCL',  'UNPHASCL', '        ', GVH__CL1  & !  29
     &     /
!
	INTEGER*4    GVH__NEW_NUS
	PARAMETER  ( GVH__NEW_NUS = 13 )
	INTEGER*4    GVH__CL_NEW(GVH__NEW_NUS),   GVH__TY_NEW(GVH__NEW_NUS), &
     &               GVH__DM_NEW(2,GVH__NEW_NUS), GVH__SG_NEW(GVH__NEW_NUS)
        CHARACTER    GVH__LC_NEW(GVH__NEW_NUS)*8, GVH__DS_NEW(GVH__NEW_NUS)*128
        DATA         ( GVH__LC_NEW(NN$IND),   GVH__CL_NEW(NN$IND), &
     &                 GVH__TY_NEW(NN$IND),   GVH__DM_NEW(1,NN$IND), &
     &                 GVH__DM_NEW(2,NN$IND), GVH__SG_NEW(NN$IND), &
     &                 GVH__DS_NEW(NN$IND),    NN$IND=1,GVH__NEW_NUS ) &
     &     / &
     &     'AUTO_SUP', GVH__BAS,  GVH__I4,   1,   1,  GVH__SL1,  'Bit field of automatic suppression status for combination of observables',      & !  1   
     &     'DATYP   ', GVH__SES,  GVH__I2,   1,   1,  GVH__SL1,  'Type of the observable or a combination of observables used in the solution',   & !  2
     &     'DGCL_EST', GVH__SES,  GVH__I2, -83,  32,  GVH__SL1,  'Degree of global clock function polynomial per station, per solution type',     & !  3
     &     'EDIT_STS', GVH__SES,  GVH__I4,   1,   1,  GVH__SL1,  'Bit field of database editing status for different types of solutions',         & !  4
     &     'EXPSERNO', GVH__SES,  GVH__I2,   1,   1,  GVH__FR1,  'Experiment serial number at correlator',                                        & !  5
     &     'FRTYPFIT', GVH__SES,  GVH__I4,   1,   1,  GVH__FR1,  'Fringe type. 1-99 reserved for HOPS, 101-199 reserved for PIMA',                & !  6
     &     'NUM_CLRF', GVH__SES,  GVH__I4,   1,   1,  GVH__SL1,  'Number of clock reference stations',                                            & !  7
     &     'REC_MODE', GVH__SES,  GVH__C1,  80,   1,  GVH__FR1,  'Recording mode',                                                                & !  8
     &     'SUPMET  ', GVH__SES,  GVH__I2,   1,   1,  GVH__SL1,  'Code of the suppression method used in the solution',                           & !  9
     &     'TEC_STS ', GVH__SES,  GVH__I4, -83,   1,  GVH__SL1,  'Flag of availability/usage of the external ionosphere calibration',             & ! 10
     &     'TH_PROG ', GVH__SES,  GVH__C1,  64,   1,  GVH__SL1,  'Name and version of the program which computed theoretical path delays',        & ! 11
     &     'UTC_MTAI', GVH__SES,  GVH__R8,   1,   1,  GVH__FR1,  'Difference UTC minus TAI at first time tag of the database (sec)',              & ! 12
     &     'DB_VERS ', GVH__SES,  GVH__I2,   1,   1,  GVH__SL1,  'Database version'                                                               & ! 13
     &     /
!
! <<<<< end of INCLUDE-BOCK  gvf_db.i
