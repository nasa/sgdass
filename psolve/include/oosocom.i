!@This is the start of file OOSOCOM
!
!
       Real*8 &
     & ooPI           , ooFJDCL(100)  , ooTATM(160)  , &
     & ooELMIN        , ooVLIGHT          , ooTROT(12)  , &
     & ooWRMS(3)      , ooROTAP(12,4)    , ooUT1INB(3)  , &
     & ooxUT1PTB(6)    , &
     & ooWOBINB(3)    , ooxWOBXXB(6)   , ooxWOBYYB(6)  , ooCALCV      , &
     & ooATMOS_INTERVAL        , ooCLOCK_INTERVAL   , ooINT_ROT_A1(2), &
     & ooTROT_A1,oout1ptb(15),oowobxxb(15),oowobyyb(15)
!
       Real*4 &
     & ooFCNPER       , ooSACNST(16)          , &
     & ooSCCNST(16)          , ooELVCUT(16)   , &
     & ooEOPCONS(3) , ooeoprcons(3), ooSEOCNST(2),oopwccnst
!
       Logical*2 &
     & ooLOGBCL           , ooBMODE_CL       , ooBMODE_AT        , &
     & ooCLK_BRK_STAT     , ooFLYBY_WARNING  , ooSITE_DEP_CONST  , &
     & ooSIMULATION_TEST  , ooSITE_DEP_EL_CUT  , ooSHORT_UT1_IN  , &
     & ooSOL_AVAIL  , ooold_clocks  , ooold_atms, ooskip_eop_off
!
       Character*64 oouser_pro
       Character*80 oouser_buf
!
       Integer*2 &
     & ooNUMSTR       , ooNUMSTA        , ooNUMOBS       , &
     & ooNPOLD        , &
     & ooICLOCK(1,16)       , &
     & ooIDNWT        , ooIPRES         , &
     & ooIRNCD(2)     , ooITDGLB     , ooNPARAM       , ooIDATYP  , &
     & ooNROT         , ooNSOURC        , ooNSPARM(16)   , &
     & ooNUMATM(16)          , &
     & ooIATSTR(16)          , ooICLMAX       , &
     & ooNUMCLK(16)          , ooICLSTR(16)    , &
     & ooIPSTP        , ooLNUT(3)       , ooLPREC        , &
     & ooLTIDE(32,3)       , &
     & ooLREL         , ooLROT(3,3)        , &
     & ooLATM(10,3)        , ooLCLK(100)         , &
     & ooLSTAR(48,2)       , ooLAXOF(32)   , &
     & ooLSITEC(32,3)      , &
     & ooISRSEL(48)        , ooIUEN       , &
     & ooICLSTA(100)              , ooNFLEPS    , ooFLEPS(14)  , &
     & ooNFLPSI       , &
     & ooFLPSI(14)    , ooIDPNUT(7)     , ooNDPNUT       , &
     & ooLSITEV(32,3)      , &
     & ooIARCSOC      , ooNSLAST        , ooIDBEND(15)  , &
     & ooIDBSEL       , ooNDB           , ooIDCSEL       , &
     & ooIBLSEL(1,16)       , &
     & ooCONSTRAINT_BITS              , ooINDL         , &
     & ooWVMASK(16)    , ooBM_REF_CL    , ooNROT_A1(2), &
     & ooEOP_STYLE(2)           , ooEOPA1_CHOICE(2)          , &
     & ooIEOPL              , oonumstax      , &
     & oointerpolation_UT1            ,oointerpolation_pm, &
     & ooBGROUND    , ooLPROP(48,2)    ,oototsta, &
     & ooIFREE_SOCOM(108)
!
      INTEGER*2 ooISOCOM(2816),ooiso3(960)
      INTEGER*2 ooiso1(1665),ooiso2(66)
!
!
       Equivalence (ooISOCOM, ooPI)
       Equivalence (ooISO1, oopi)
       Equivalence (ooISO2, ooiarcsoc)
       Equivalence (ooISO3, oologbcl)
!
!  common
!
       Common /OOSOCOM/ &
!
!  real*8
!
     & ooPI           , ooFJDCL         , ooTATM         , &
     & ooELMIN        , ooVLIGHT        , ooTROT         , &
     & ooWRMS       , ooROTAP         , ooUT1INB    , ooxUT1PTB  , &
     & ooWOBINB     , ooxWOBXXB        , ooxWOBYYB   , ooCALCV   , &
     & ooATMOS_INTERVAL               , ooCLOCK_INTERVAL         , &
!
!  real*4
!
     & ooFCNPER     , ooSACNST        , ooSCCNST       , ooELVCUT  , &
     & ooEOPCONS      , &
!
!  logical*2
!
     & ooLOGBCL           ,ooBMODE_CL       ,ooBMODE_AT        , &
     & ooCLK_BRK_STAT     ,ooFLYBY_WARNING  ,ooSITE_DEP_CONST  , &
     & ooSIMULATION_TEST  ,ooSITE_DEP_EL_CUT ,ooSHORT_UT1_IN   , &
!
!  integer*2
!
     & ooNUMSTR       , ooNUMSTA        , ooNUMOBS       , &
     & ooNPOLD        , &
     & ooICLOCK       , &
     & ooIDNWT        , ooIPRES         , &
     & ooIRNCD        , ooITDGLB        , ooNPARAM       , ooIDATYP  , &
     & ooNROT         , ooNSOURC        , ooNSPARM       , ooNUMATM  , &
     & ooIATSTR       , ooICLMAX        , ooNUMCLK       , ooICLSTR  , &
     & ooIPSTP        , ooLNUT          , ooLPREC        , ooLTIDE   , &
     & ooLREL         , ooLROT          , ooLATM         , ooLCLK    , &
     & ooLSTAR        , ooLAXOF         , ooLSITEC       , &
     & ooISRSEL       , ooIUEN          , &
     & ooICLSTA       , ooNFLEPS        , ooFLEPS        , ooNFLPSI  , &
     & ooFLPSI        , ooIDPNUT        , ooNDPNUT       , ooLSITEV  , &
     & ooIARCSOC      , ooNSLAST        , ooIDBEND       , &
     & ooIDBSEL       , ooNDB           , ooIDCSEL       , ooIBLSEL  , &
     & ooCONSTRAINT_BITS              , ooINDL         , ooWVMASK    , &
     & ooBM_REF_CL    , &
!
!    Additions
!
     & ooEOP_STYLE    , ooEOPA1_CHOICE , ooNROT_A1   , ooINT_ROT_A1   , &
     & ooTROT_A1      , ooSEOCNST      ,ooieopl  , oonumstax      , &
     & ooSOL_AVAIL    , oointerpolation_ut1     ,oointerpolation_pm, &
     & ooBGROUND      , ooold_clocks , ooold_atms ,  ooeoprcons, &
     & oout1ptb,oowobxxb,oowobyyb,oolprop,oopwccnst,oototsta, &
     & ooskip_eop_off, oouser_pro,oouser_buf,ooIFREE_SOCOM
!
!  All variables declared so that IMPLICIT NONE will yield no errors.
!
