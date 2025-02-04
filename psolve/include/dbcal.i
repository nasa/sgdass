!
!@dbcal.i
!
!*** Please see notes below on Max_Stat, Scr_Points,
!*** Luin, Luout, and Luerr.
!
!***Modifications:
!     All real variables converted to Double Precision
!        for precision. 9-30-91 -BDM-
!     91.11.01  DGG  Max_Stat set to 8 and Wvr_Points set to 1000
!                      for GSFC use.
!     92.07.27  BA   Max_Stat set back to maximum of 10.
!     93.02.25  MSW  All previous include files ( hdata.i, hold1.i,
!                    & hwvr.i) combined into one include file.
!                    Hard coded "10" changed to variable "Max_Stat".
!     93.03.12  BA   Max_Stat set to 20 stations.
!     93.07.16  MSW  "numold" added to "hwx" common, so adding
!                    information for stations in later runs would work
!                    correctly.
!     93.09.20  BA   (This list updated.)
!     93.10.15  BA   "cblmn" added to common.
!   2005.05.18  DG   Typed all variables. 
!   2008.02.14  PET  Increzsed suze of WX_POINTS, CBL_POINTS and SCR_pOINTS
!                    from 2000 to 8192
!
      Integer*2 Max_Stat
      Integer*4 Wx_Points, Cbl_Points, Wvr_Points, &
     &          Scr_Points, Luin, Luout, Luerr
!
!*** The maximum number of stations is set in the next line.  This
!*** is the only change necessary.
!
      Parameter(Max_Stat  =  24)      ! Maximum number of stations
!                                       in the database
!
      Parameter(Wx_Points  = 8192)    ! Maximum weather data points
      Parameter(Cbl_Points = 8192)    ! Maximum cable data points
      Parameter(Wvr_Points = 1000)    ! Maximum WVR data points
!
!*** Please set the variable Scr_Points to a value that is the larger
!*** if Cbl_Points or Wx_Points.
!
      Parameter(Scr_Points = 8192)    ! Maximum number of data points
!                                       in the scratch arrays
!
!*** Standard lu numbers are set for Luin, Luout, and Luerr.
!
      Parameter(Luin  = 5)      ! Standard input lu number
      Parameter(Luout = 6)      ! Standard output lu number
      Parameter(Luerr = 7)      ! Standard error lu number
!-------------------------------------------------------------------
!
! Variables:
!  cblmn  R*8     Mean value of cable calibration for each station
!  (Max_Stat)     during experiment.  Can be added to "cbl" values
!                 to get true (scaled) cable cal reading.
!                 Units: microseconds.
!
!-------------------------------------------------------------------
!
      Real*8 TCBL(CBL_Points,Max_Stat), CBL(CBL_Points,Max_Stat),    &
     &       CBLMN(Max_Stat), A(Scr_Points), V(Scr_Points),          &
     &       TWVR(wvr_Points,Max_Stat),  WVR(wvr_Points,Max_Stat),   &
     &       EWVR(wvr_Points,Max_Stat), AWVR(wvr_Points,Max_Stat),   &
     &       TWX(wx_Points,Max_Stat), STEMP(wx_Points,Max_Stat),     &
     &       SPRES(wx_Points,Max_Stat), SRH(wx_Points,Max_Stat),     &
     &       QTEMP(2,Max_Stat),QPRES(2,Max_Stat),                    &
     &       QRH(2,Max_Stat), TEMP(wx_Points,Max_Stat),              &
     &       PRES(wx_Points,Max_Stat), RH(wx_Points,Max_Stat) 
      Integer*4 NCBL(Max_Stat), NWVR(Max_Stat), NWX(Max_Stat)
!
      COMMON /HOLD1/ TCBL, CBL, CBLMN, A, TWVR, WVR, EWVR, AWVR, TWX,    &
     &               STEMP, SPRES, SRH, QTEMP, QPRES, QRH, TEMP, PRES,   &
     &               RH, NCBL, NWX, NWVR
!
!------------------------------------------------------------------
!
      Character*8   LSPHC(Max_Stat)
      Integer*4     LDOPC, NUMPCL, IPSTAT(2)
!
      COMMON /HPHC/ LSPHC, LDOPC, NUMPCL, IPSTAT
!
!------------------------------------------------------------------
!
      Character*8   LSWVRa(Max_Stat),LSWVRb(Max_Stat),LSWVR(2,Max_Stat)
      Real*8        WVR_STO_DAT(Max_Stat,2,2), WVR_STO_LEN(Max_Stat)
      Integer*4     NUMWVR, NWVST, LLWDE, LLWCD, LLWVD
!
      COMMON /HWVR/ LSWVRa, LSWVRb, LSWVR, NUMWVR, NWVST, LLWDE, LLWCD, &
     &              LLWVD, WVR_STO_DAT, WVR_STO_LEN
!
!------------------------------------------------------------------
!
      Character*8  LSWXA(Max_Stat),LSWXB(Max_Stat),LSWX(2,Max_Stat)
      integer*4    NUMOLD, NUMWX, NWXST, LLTEM, LLPRE, LLREL, LLATM,    &
     &             LLDRY, LCFDRY, LCFWET, LCFPRT, LNYDRY, LNYWET, LNYINF
!
      COMMON /HWX/ LSWXA, LSWXB, LSWX, NUMWX, NWXST, LLTEM, LLPRE,      &
     &             LLREL 
!
!    &             LLATM, LLDRY,LCFDRY,LCFWET,LCFPRT, &
!    &             LNYDRY, LNYWET, LNYINF, NUMOLD
!
!------------------------------------------------------------------
!
      Character*2   LSIGN(Max_Stat)
      Character*8   LSCBL(Max_Stat)
      Integer*4     NUMCBL,NCBST, ISIGN(Max_Stat),LLCBD
!
      COMMON /HCBL/ LSCBL,NUMCBL,NCBST,LSIGN, ISIGN,LLCBD
!
!------------------------------------------------------------------
!
      Character      KYNM*10
      Character*2    LOSIG(Max_Stat)
      Character*8    LSDB(Max_Stat), LOSDB(Max_Stat), LBASE(2)
      Character*34   LHISTX
      Integer*4      IGDATA
      Integer*2      IUTC(5), RLDCB, RDDCB, NDO(3), NSITE
!     Real*8         ELEV(2,2), AZIM(2,2), SEC, VLIGHT, RADDEG, AMARI(2,2,2), &
!    &               AMARID(2,2,2), PID2, SLAT(Max_Stat), ELEVX(Max_Stat),    &
!    &               CFADRY(2,2,2),CFAWET(2,2,2), CFAPRT(2,2)
      Real*8         ELEV(2,2), AZIM(2,2), SEC, VLIGHT, RADDEG,               &
     &               PID2, SLAT(Max_Stat), ELEVX(Max_Stat)
!
      COMMON /HDATA/ KYNM, IUTC, LOSIG, LSDB, LOSDB, LBASE, IGDATA,       &
     &               Elev, AZIM, SEC, VLIGHT, RADDEG, Slat, ELEVX,        &
     &                      PiD2,                                         &
     &                      NDO,lhistx,rldcb,rddcb,nsite
!
