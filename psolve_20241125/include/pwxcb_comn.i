      Integer*4 max_points, max_pts
      Parameter (max_pts = 5000)    ! Maximun # of cable and weather points
!
      REAL*8 XC(max_pts), XW(max_pts), YC1(max_pts), YW1(max_pts), &
     &       YW2(max_pts), YW3(max_pts), XCLO(50), YCLO(50)
      COMMON/WXCB/XC, YC1, XW, YW1, YW2, YW3, XCLO, YCLO
!
      REAL*8 FJD
      COMMON/TIMJD/FJD
!
      Integer*4    ICRT,KBRD,LUP,IDT,KIN
      INTEGER*2    IODLEN,IODES(5)
      Character*2 xtype, ytype, ztype
      COMMON/LYUS/ ICRT,KBRD,LUP,IDT,IODLEN,IODES, xtype, &
     &             ytype, ztype
!
      Integer*4    NCB,NWX,IDOY,IYR,Iverflag,NCBLO
      Real*8       FS
      CHARACTER*1  CSAVE,WSAVE 
      CHARACTER*14 IEXP
      CHARACTER*80 IBFR 
      LOGICAL      BATCH,XGO
      COMMON/INOUT/FS,NCB,NWX,IBFR,IEXP,CSAVE,WSAVE,IDOY,IYR, &
     &             Iverflag,NCBLO,BATCH,XGO
!
      CHARACTER*1  LSIGN,IDUM 
      CHARACTER*2  MATCH2
      Integer*4    Iexx
      Real*8       XMIN, XMAX
      CHARACTER*8  ISTN 
      CHARACTER*60 KWX,NCABL 
      COMMON/CALS/ NCABL,KWX,ISTN,XMIN,XMAX,IEXX,MATCH2,LSIGN,IDUM
!
      REAL*4       ycmin,ycmax,ytmin,ytmax,ypmin,ypmax,yhmin,yhmax
      CHARACTER*10 Dbname
      COMMON/AUTOP/ycmin,ycmax,ytmin,ytmax,ypmin,ypmax,yhmin,yhmax,  &
     &             Dbname
