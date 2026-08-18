!@This is the start of file &OOCOM
!
!  this is a representation of the OLD A-900 socom
!
       COMMON/OOCOM/ &
     &OICOMDC(16)    ,OPI           ,OLSITEC(4,3)  ,OLAXOF(4)       , &
     &OLSTAR(13,2,3) ,OLCLK(100)    ,OLATM(7,3)    ,OLROT(25,3)     , &
     &OLREL          ,OIBATCH        , &
     &                OCALCV        ,OLTIDE(4,3)   ,OLPREC          , &
     &OLNUT(3)       ,OIPSTP        ,ONUMSTA       ,ONUMSTR         , &
     &OICLSTR(50)    ,ONUMCLK(50)   ,OFJDCL(100)   ,OFRCL(100)      , &
     &OICLMAX        ,OIATSTR(50)   ,ONUMATM(50)   ,OTATM(100)      , &
     &ONSPARM(51)    ,ONSOURC       ,ODBINBF(14)   ,ONROT           , &
     &OTROT(20)      ,OROTAP(20,4)  ,OELMIN        ,OIDATYP         , &
     &ONPARAM        ,OITDGLB       ,OIRNCD(2)     ,OIPRES          , &
     &ONUMOBS        ,OVLIGHT       ,OWRMS(3)      ,OICRT           , &
     &OIPRNT         ,OISCR         ,OIDNWT        ,OIDISP          , &
     &OIUEN          ,OIARCSOC       , &
     &OIFREE1        ,ONAMSPL(10)   ,OICLOCK(4,50) ,OLOGBCL         , &
     &OLENAM(5)      ,OLMNAM(5)                    ,OISBEOP         , &
     &OUT1INB(3)     ,OUT1PTB(6)    ,OWOBINB(3)    ,OWOBXXB(6)      , &
     &OWOBYYB(6)     ,ONSATLL       ,ONPOLD        ,OLLNAM(18)      , &
     &OISUBST        ,ONRECD         , &
     &OISRSEL(13)    ,OISPREC       ,OISPPOS        , &
     &ONFLPSI        ,OFLPSI(14)   ,ONFLEPS        ,OFLEPS(14)      , &
     &ONDPNUT        ,OIDPNUT(7)   ,OFCNPER        ,OLSITEV(4,3)    , &
     &ODBNAMC(6)     ,OICLSTA(70)
!
!      REAL*6
      CHARACTER*6 &
     &OPI           ,OFJDCL         ,OFRCL         ,OTATM           , &
     &OELMIN        ,OVLIGHT        ,ODBINBF       ,OTROT           , &
     &OWRMS         ,OROTAP         ,OUT1INB       ,OUT1PTB         , &
     &OWOBINB       ,OWOBXXB        ,OWOBYYB       ,OCALCV
!
!      REAL*4
       CHARACTER*4 &
     &       OFCNPER
!
!      LOGICAL*2
       CHARACTER*2 &
     &          OLOGBCL
!
      INTEGER*2 &
     &ONUMSTR,ONUMSTA,ONUMOBS,OIPRNT,ONRECD,ONAMSPL,OISUBST,OLLNAM, &
     &ONPOLD,ONSATLL,OISBEOP,OIFREE1,OLMNAM,OLENAM,OICLOCK, &
     &OIDISP,OIDNWT,OISCR,OICRT,OIPRES,OIRNCD,OITDGLB,ONPARAM,OIDATYP, &
     &ONROT,ONSOURC,ONSPARM,ONUMATM,OIATSTR,OICLMAX,ONUMCLK,OICLSTR, &
     &OIPSTP,OLNUT,OLPREC,OLTIDE,OLREL,OLROT,OLATM,OLCLK,OLSTAR, &
     &OLAXOF,OLSITEC,OICOMDC,OIBATCH,OISRSEL,OIFREE2,OIUEN,OICLSTA, &
     &ONFLEPS,OFLEPS,ONFLPSI,OFLPSI,OIDPNUT,ONDPNUT,OLSITEV,ODBNAMC, &
     &OIARCSOC
!
      INTEGER*4 &
     &         OISPREC,OISPPOS
!
!@This is the start of file &IDBEQ
!
      INTEGER*2 OIDBEND(18),OIBLSEL(16),ONDB,OIDCSEL,OIDBSEL
      EQUIVALENCE (OIBLSEL,ODBINBF(7))
      EQUIVALENCE (OIDBEND(1),ODBINBF(1)),(OIDBSEL,OIDBEND(16)), &
     &            (     ONDB,OIDBEND(17)),(OIDCSEL,OIDBEND(18))
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
!
