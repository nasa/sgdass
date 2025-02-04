!@This is the start of file &SOCOM
!
!  This is a representation of CURRENT a-900 socom
!
       COMMON/A_SOCOM/ &
     &A_PI            ,A_LSITEC(A_STA_BIT_WORDS,3)     , &
     &A_LAXOF(A_STA_BIT_WORDS)         ,A_LSTAR(A_SRC_BIT_WORDS,2)     , &
     &A_LCLK(A_MAX_CLK) ,A_LATM(A_ATM_BIT_WORDS,3)       , &
     &A_LROT(A_ROT_BIT_WORDS,3)        , &
     &A_LREL          ,A_IBATCH       , &
     &A_CALCV         ,A_LTIDE(A_STA_BIT_WORDS,3)      ,A_LPREC        , &
     &A_LNUT(3)       ,A_IPSTP        ,A_NUMSTA       ,A_NUMSTR        , &
     &A_ICLSTR(A_MAX_ARC_STA)          ,A_NUMCLK(A_MAX_ARC_STA)        , &
     &A_FJDCL(A_MAX_CLK),A_ICLMAX       , &
     &A_IATSTR(A_MAX_ARC_STA)          ,A_NUMATM(A_MAX_ARC_STA)        , &
     &A_TATM(A_MAX_ATM) , &
     &A_NSPARM(A_MAX_ARC_STA)          ,A_NSLAST       ,A_NSOURC       , &
     &A_IDBEND(15)    ,A_IDBSEL       ,A_NDB          ,A_IDCSEL        , &
     &A_IBLSEL(A_ARC_STA_BIT_WORDS,A_MAX_ARC_STA)       ,A_NROT        , &
     &A_TROT(A_MAX_ROT) , &
     &A_ROTAP(A_MAX_ROT,4)             ,A_ELMIN        ,A_IDATYP       , &
     &A_NPARAM        ,A_ITDGLB       ,A_IRNCD(2)     ,A_IPRES         , &
     &A_NUMOBS        ,A_VLIGHT       ,A_WRMS(3)      ,A_ICRT          , &
     &A_IPRNT         ,A_ISCR         ,A_IDNWT        ,A_IDISP         , &
     &A_IUEN          ,A_IARCSOC      , &
     &A_ICLOCK(A_ARC_STA_BIT_WORDS,A_MAX_ARC_STA)       ,A_LOGBCL      , &
     &A_LENAM(5)      ,A_LMNAM(5)                    ,A_ISBEOP         , &
     &A_UT1INB(3)     ,A_UT1PTB(6)    ,A_WOBINB(3)    ,A_WOBXXB(6)     , &
     &A_WOBYYB(6)     ,A_NSATLL       ,A_NPOLD        ,A_LLNAM(18)     , &
     &A_ISUBST        ,A_NRECD        ,A_ISRSEL(A_SRC_BIT_WORDS)       , &
     &A_NFLPSI        ,A_FLPSI(14)   ,A_NFLEPS        ,A_FLEPS(14)     , &
     &A_NDPNUT        ,A_IDPNUT(7)   ,A_FCNPER        , &
     &A_LSITEV(A_STA_BIT_WORDS,3)     ,A_DBNAMC(6)     , &
     &A_ICLSTA(A_MAX_CLK)             ,A_SCCNST(A_MAX_ARC_STA)         , &
     &A_SACNST(A_MAX_ARC_STA)         , &
     &A_ISPREC       ,A_ISPPOS        ,A_CONSTRAINT_BITS, &
     &A_BMODE_CL     ,A_BMODE_AT    ,A_ATMOS_INTERVAL,A_CLOCK_INTERVAL , &
     &A_CLK_BRK_STAT ,A_FLYBY_WARNING ,A_INDL          , &
     &A_ELVCUT(A_MAX_ARC_STA)       ,A_SITE_DEP_CONST,A_SIMULATION_TEST, &
     &A_SITE_DEP_EL_CUT             ,A_WVMASK(A_MAX_ARC_STA)           , &
     &A_EOPCONS(3)                  ,A_SHORT_UT1_IN, &
     &A_IFREE1(437)
!
!      REAL*8
       CHARACTER*8 &
     &A_PI          ,A_FJDCL        ,A_FRCL        ,A_TATM          , &
     &A_ELMIN       ,A_VLIGHT       ,A_DBINBF      ,A_TROT          , &
     &A_WRMS        ,A_ROTAP        ,A_UT1INB      ,A_UT1PTB        , &
     &A_WOBINB      ,A_WOBXXB       ,A_WOBYYB      ,A_CALCV         , &
     &A_ATMOS_INTERVAL              ,A_CLOCK_INTERVAL
!
!      REAL*4
       CHARACTER*4 &
     &       A_FCNPER,A_SACNST,A_SCCNST,A_ELVCUT,A_EOPCONS
!
!      LOGICAL*2
       CHARACTER*2 &
     &          A_LOGBCL            ,A_BMODE_CL      ,A_BMODE_AT       , &
     &          A_CLK_BRK_STAT      ,A_FLYBY_WARNING ,A_SITE_DEP_CONST , &
     &          A_SIMULATION_TEST   ,A_SITE_DEP_EL_CUT,A_SHORT_UT1_IN
!
      INTEGER*2 &
     &A_NUMSTR,A_NUMSTA,A_IPRNT,A_NRECD,A_NAMSPL,A_ISUBST, &
     &A_LLNAM, &
     &A_NPOLD,A_NSATLL,A_ISBEOP,A_IFREE1,A_LMNAM,A_LENAM,A_ICLOCK, &
     &A_IDISP,A_IDNWT,A_ISCR,A_ICRT,A_IPRES,A_IRNCD,A_ITDGLB,A_NPARAM, &
     &A_IDATYP, &
     &A_NROT,A_NSOURC,A_NSPARM,A_NUMATM,A_IATSTR,A_ICLMAX,A_NUMCLK, &
     &A_ICLSTR, &
     &A_IPSTP,A_LNUT,A_LPREC,A_LTIDE,A_LREL,A_LROT,A_LATM,A_LCLK, &
     &A_LSTAR, &
     &A_LAXOF,A_LSITEC,A_ICOMDC,A_IBATCH,A_ISRSEL,A_IFREE2,A_IUEN, &
     &A_ICLSTA, &
     &A_NFLEPS,A_FLEPS,A_NFLPSI,A_FLPSI,A_IDPNUT,A_NDPNUT,A_LSITEV, &
     &A_DBNAMC, &
     &A_IARCSOC ,A_NSLAST, A_IDBSEL,A_NDB,A_IDCSEL,A_IBLSEL, &
     &A_ISOCOM(A_SOCOM_WORDS),A_CONSTRAINT_BITS,A_INDL,A_WVMASK
!
      INTEGER*4 &
     &          A_NUMOBS, A_IDBEND, A_ISPREC, A_ISPPOS, A_DBNAME_VER, &
     &          A_EDIT_STS, &
     &          A_NPAR
!
      EQUIVALENCE (A_ISOCOM(1),A_PI)
!
!
!  All variables declared so that IMPLICIT NONE will yield no errors.
!
!      Explanations:
!      ICOMDC       DCB for file holding the SOLVE master common.
!      PI           Pi in 11-digit precision.
!      LSITEC(4,3)  Bit array for station solution status. First
!                   index spans 64 sites, second spans X,Y,Z.
!      LAXOF        Bit array for axis offset solution status. 64 sites.
!      LSTAR(13,2,3)Bit array for source solution status. First index
!                   spans 208 sources, second RA and DEC, and third is
!                   not used. (Third will be used for GPS processing.)
!      LCLK         Bit Array for 100 clock epochs.
!      LATM(7,3)    Bit array for atmosphere solution status for 100
!                   atmosphere epochs. 3 bits per epoch.
!      LROT         Bit array for earth orientation solution. First index
!                   runs over 25 epochs, second over X-pole, Y -pole, and
!                   UT1.
!      LREL         Flag word for relativity solution status.
!      IBATCH       Batch Control Flag. = 0 if interactive,
!                   various nonzero values may occur if in Batch Mode.
!      IUEN         UEN PARTIAL CONTROL AND RAIDIAL COMPONENTS
!      NFLPSI       NUMBER OF BITS ON IN FLPSI
!      NFLEPS       NUMBER OF BITS ON IN FLEPS
!      FLPSI        ADDITIONAL PSI FLAG BITS, SEE NUT_PARTS
!      FLEPS        ADDITIONAL EPS FLAG BITS, SEE NUT_PARTS
!      IDPNUT       NUTATION PERIOD FLAGS SEE NUTW
!      NDPNUT       NUMBER OF NUTATION PERIOD FLAGS SEE NUTW
!      FCNPER       FREE CORE NUTATION PERIOD
!      LSITEV(4,3)  SITE VELOCITIES ARRANGED AS IN LSITEC
!      IFREEx       Filler.
!      LTIDE(4,3)   Bit array of earth tide solution status.  If set
!                   earth tide global solution (ITDGLB = 0), the first
!                   3 bits hold status of Love l, Love h, and lag angle.
!                   If site dependent solution (ITDGLB = 1), the first index
!                   holds status for 64 stations and second runs over Love l,
!                   Love h, and lag angle.
!      LPREC        Precession constant solution status word.
!      LNUT         Bit array for nutation solution status. First two bits
!                   are used when solving for daily nutation and correspond
!                   to dpsi and deps. The remaining bits used for solving
!                   for terms in the nutation series. Their exact use is
!                   defined by the terms selected in CALC.
!      IPSTP        Number of non-station-dependent parameters estimated.
!      NUMSTA       Total number of stations SOLVE knows about.
!      NUMSTR       Total number of sources SOLVE knows about.
!      ICLSTR       For the Ith station ICLSTR(I) is the number of the
!                   position in LCLK immediately before the position
!                   where the clock flags for the Ith station begin.
!      NUMCLK       The number of clock epochs for each station.
!                   ICLSTR, NUMCLK, and LCLK must be understood together.
!      FJDCL        Contains the Julian dates at midnight of the clock
!                   epochs.
!      FRCL         Contains the fraction of day since midnight for the
!                   clock epochs.
!      ICLMAX       The maximum degree of any clock parameter.
!      IATSTR       IATSTR, and NUMATM do for atmospheres what ICLSTR and
!                   NUMCLK do for clocks.
!      TATM         Complete Julian date, including fraction of day, for
!                   the atmosphere epochs.
!      NSPARM       For station I NSPARM(I) contains the parameter number
!                   for the last parameter before the parameters for station
!                   I. For example NSPARM(1) is alway zero. Computed
!                   in PARCN. NSPARM(51) contains the total number of
!                   site parameters.
!      NSOURC       Number of source parameters estimated.
!      DBINBF       ?? & IBLSEL is equivalenced to part of this.
!      NROT         Number of rotation epochs currently in use.
!      TROT         Compete Julian Date for up to 20 rotation epochs.
!      ROTAP        Rotation information. First index runs over 20
!                   epochs. Second over UT1, tidal correction to UT1,
!                   x-wobble, and y-wobble values. Filled by BLKCL.
!      ELMIN        Minimum elevation for current solution. Radians.
!      IDATYP       Delay data type for current solution. See SETFL code.
!      NPARAM       Total number of parameters estimated in current solution.
!                   Computed by PARCN.
!      ITDGLB       Earth-tide global/site-dependent flag. See LTIDE.
!      IRNCD        Solution run code. Computed from date/time of solution.
!      IPRES        Bit array of printing status. Bit 1 on = do print
!                   residuals; bit 2 on = write solution library tape (use
!                   only by NGS; not related to the STERN solution archiving
!                   system); bit 3 on = print correlation matrix.
!      NUMOBS       Total number of observations in the scratch files.
!      VLIGHT       Speed of light (m/s).
!      WRMS         Weighted rms delay, weighed rms rate, combined weighted
!                   rms delay and rate.
!      ICRT         Number of the interactive terminal.
!      IPRNT        Number of the print LU. Not used straightforwardly.
!      ISCR         Scratch file LU.
!      IDNWT        If = 1, increase delay weights by 1.E9.
!      IDISP        Plot LU.
!      ISPREC       LOCF/APOSN data for spool file.
!      ISPPOS       LOCF/APOSN data for spool file.
!      NAMSPL       NAMR of the spool file.
!      ICLOCK       Bit array for baseline-dependent clocks.
!      LOGBCL       Set true if any baseline-dependent clocks are estimated.
!      LENAM        NAMR for eccentricty data file.
!      LMNAM        NAMR for offset monument file.
!      ISBEOP       Flag indicating whether VLBI EOP has been mapped
!                   into theoreticals, 0 = no, 1 = yes.
!      UT1INB       UT1 information from the data base (i.e., either
!                   Rapid Service or Circular D data) as follows:
!                   first J.D., interval between epochs, and number
!                   of epochs.
!      UT1PTB       UT1 data corresponding to UT1NB epochs, i.e., the
!                   values of TAI minus UT1 in seconds.
!      WOBINB       Polar motion information from the data base (i.e.,
!                   either Rapid Service or Circular D data) as
!                   follows: first J.D., interval between epochs, and
!                   number of epochs.
!      WOBXXB       X component of polar motion corresponding to WOBINB
!                   epochs, given in milliarcseconds.
!      WOBYYB       Y component of polar motion corresponding to WOBINB
!                   epochs, given in milliarcseconds.
!      NSATLL       Not used. For future GPS code.
!      NPOLD        A holding pen for the number of parameters estimated
!                   the last time PARCN was run.  In general, the number
!                   of parameters in the last solution.
!      LLNAM        NAMR of the substitute site and source file.
!      ISUBST       Not equal zero, if substitute site and source file used.
!      NRECD        Number of observations the user's scratch area holds.
!      CALCV        Calc version.
!      ISRSEL       Source selection array.
!      DBNAMC       Data base name
!      ICLSTA       used to determine if a clock epoch applies
!                   only to this station (= station number), or
!                   to every station but this station (= - station number)
!                   this array is meaningful only if the twelfeth bit
!                   is set in the LCLK word
!     IARCSOC       NUMBER OF ARC PARAMETERS IN THIS ARC, ONLY USED IN
!                   SAVED ARC FILES
!     CONSTRAINT_BITS One 16-bit word used to store clock and atm constraint
!                   use information.
!     CLK_BRK_STAT  Set to .true. if some site has actual clock break.
!                   Otherwise .false.
!     FLYBY_WARNING If .true., then if some sites or sources are not
!                   in flyby files warning is issured. (Does not operate
!                   in batch mode.)
!     INDL          A variable used by STFLG to keep track of the clock
!                   and atmosphere insert/delete/automatic mode status.
!     SITE_DEP_CONST If true, then different clock and atmosphere
!                   for different sites.
!     SIMULATION_TEST Set to .true. only for simulation data bases.
!     SITE_DEP_EL_CUT If true, the elevation cutoff is site dependent.
!     WVMASK        Mask for site dependent data selection based on WVR
!                   quality.  -1 means accept all data with with WVR
!                   calibration; 0 means do not test WVR data at all.
!     SHORT_UT1_IN  True means the universal time values extracted from the
!                   database header DO! contain the effects of fortnightly
!                   tides, i.e., they are real UT1! (not UT1R). False
!                   is the opposite, that is, the tides have been modeled
!                   out or smoothed out. In this case the universal time
!                   information is UT1R.  (BIH and Bulliten B values are
!                   UT1R.)
