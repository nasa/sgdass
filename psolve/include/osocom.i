!@This is the start of file OSOCOM
!
!
       Real*8 &
     &OPI           ,OFJDCL(100)  ,OTATM(160)  , &
     &OELMIN        ,OVLIGHT          ,OTROT(12)  , &
     &OWRMS(3)      ,OROTAP(12,4)                 ,OUT1INB(3)  , &
     &OUT1PTB(6)    , &
     &OWOBINB(3)    ,OWOBXXB(6)       ,OWOBYYB(6)      ,OCALCV      , &
     &OATMOS_INTERVAL               ,OCLOCK_INTERVAL   ,OINT_ROT_A1(2), &
     &OTROT_A1
!
       Real*4 &
     &OFCNPER       ,OSACNST(16)          , &
     &OSCCNST(16)          ,OELVCUT(16)      , &
     &OEOPCONS(3)   ,Oeoprcons(3)   ,OSEOCNST(2)
!
       Logical*2 &
     &OLOGBCL           ,OBMODE_CL       ,OBMODE_AT        , &
     &OCLK_BRK_STAT     ,OFLYBY_WARNING  ,OSITE_DEP_CONST  , &
     &OSIMULATION_TEST  ,OSITE_DEP_EL_CUT  ,OSHORT_UT1_IN  , &
     &OSOL_AVAIL        ,oold_clocks     ,oold_atms
!
       Integer*2 &
     &ONUMSTR       ,ONUMSTA        ,ONUMOBS       , &
     &ONPOLD        , &
     &OICLOCK(1,16)       , &
     &OIDNWT        ,OIPRES         , &
     &OIRNCD(2)     ,OITDGLB        ,ONPARAM       ,OIDATYP     , &
     &ONROT         ,ONSOURC        ,ONSPARM(16)       , &
     &ONUMATM(16)          , &
     &OIATSTR(16)          ,OICLMAX       , &
     &ONUMCLK(16)          ,OICLSTR(16)       , &
     &OIPSTP        ,OLNUT(3)       ,OLPREC        , &
     &OLTIDE(8,3)       , &
     &OLREL         ,OLROT(3,3)        , &
     &OLATM(10,3)        ,OLCLK(100)             , &
     &OLSTAR(48,2)       ,OLAXOF(8)      , &
     &OLSITEC(8,3)      , &
     &OISRSEL(48)        ,OIUEN       , &
     &OICLSTA(100)              ,ONFLEPS       ,OFLEPS(14)  , &
     &ONFLPSI       , &
     &OFLPSI(14)    ,OIDPNUT(7)     ,ONDPNUT       , &
     &OLSITEV(8,3)      , &
     &OIARCSOC      ,ONSLAST        ,OIDBEND(15)  , &
     &OIDBSEL       ,ONDB           ,OIDCSEL       , &
     &OIBLSEL(1,16)       , &
     &OCONSTRAINT_BITS              ,OINDL         , &
     &OWVMASK(16)          ,OBM_REF_CL    ,ONROT_A1(2), &
     &OEOP_STYLE(2)                 ,OEOPA1_CHOICE(2)          , &
     &OIEOPL                        ,Onumstax      , &
     &Ointerpolation_UT1            ,ointerpolation_pm, &
     &OBGROUND      ,OIFREE_SOCOM(198)
!
      INTEGER*2 OISOCOM(2432),oiso1(1665),oiso2(66)
!
!
       Equivalence (OISOCOM, OPI)
       Equivalence (oiso1,OPI)
       Equivalence (oiso2,oiarcsoc)
!
!  common
!
       Common /OSOCOM/ &
!
!  real*8
!
     &OPI           ,OFJDCL         ,OTATM         , &
     &OELMIN        ,OVLIGHT        ,OTROT         , &
     &OWRMS         ,OROTAP         ,OUT1INB       ,OUT1PTB  , &
     &OWOBINB       ,OWOBXXB        ,OWOBYYB       ,OCALCV   , &
     &OATMOS_INTERVAL               ,OCLOCK_INTERVAL         , &
!
!  real*4
!
     &OFCNPER       ,OSACNST        ,OSCCNST       ,OELVCUT  , &
     &OEOPCONS      , &
!
!  logical*2
!
     &OLOGBCL           ,OBMODE_CL       ,OBMODE_AT        , &
     &OCLK_BRK_STAT     ,OFLYBY_WARNING  ,OSITE_DEP_CONST  , &
     &OSIMULATION_TEST  ,OSITE_DEP_EL_CUT ,OSHORT_UT1_IN   , &
!
!  integer*2
!
     &ONUMSTR       ,ONUMSTA        ,ONUMOBS       , &
     &ONPOLD        , &
     &OICLOCK       , &
     &OIDNWT        ,OIPRES         , &
     &OIRNCD        ,OITDGLB        ,ONPARAM       ,OIDATYP         , &
     &ONROT         ,ONSOURC        ,ONSPARM       ,ONUMATM         , &
     &OIATSTR       ,OICLMAX        ,ONUMCLK       ,OICLSTR         , &
     &OIPSTP        ,OLNUT          ,OLPREC        ,OLTIDE          , &
     &OLREL         ,OLROT          ,OLATM         ,OLCLK           , &
     &OLSTAR        ,OLAXOF         ,OLSITEC       , &
     &OISRSEL       ,OIUEN          , &
     &OICLSTA       ,ONFLEPS        ,OFLEPS        ,ONFLPSI         , &
     &OFLPSI        ,OIDPNUT        ,ONDPNUT       ,OLSITEV         , &
     &OIARCSOC      ,ONSLAST        ,OIDBEND       , &
     &OIDBSEL       ,ONDB           ,OIDCSEL       ,OIBLSEL         , &
     &OCONSTRAINT_BITS              ,OINDL         ,OWVMASK         , &
     &OBM_REF_CL    , &
!
!    Additions
!
     &OEOP_STYLE    ,OEOPA1_CHOICE ,ONROT_A1       ,OINT_ROT_A1   , &
     &OTROT_A1      ,OSEOCNST      ,Oieopl         ,Onumstax      , &
     &OSOL_AVAIL    ,Ointerpolation_ut1       ,ointerpolation_pm, &
     &OBGROUND      ,Oold_clocks   ,Oold_atms ,oeoprcons, &
     &OIFREE_SOCOM
!
!  All variables declared so that IMPLICIT NONE will yield no errors.
!
