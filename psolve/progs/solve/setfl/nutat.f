      SUBROUTINE NUTAT ( )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  NUTAT PROGRAM SPECIFICATION
!
! 1.1 Build and then use the nutation time series menu to allow
!     the user to select nutation time series parameters.
!
! 1.2 REFERENCES:
!
! 2.  NUTAT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rmflg
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*3 CMP(2),YN(2)
      INTEGER*2 IDI(6),IDO(6),KBITN,IADD(15),I,ITERM,ISTAT,IYN,ICMP
      INTEGER*2 IDISPV,ICTERM,ICHAR
      integer*4 ix,iy,ich
      character*4 cch
      character*2 cchar
      equivalence (ich,cch)
      equivalence (ichar,cchar)
      CHARACTER   BUFSTR*80, STR*54, GET_VERSION*54
      LOGICAL*2 KBIT
      INTEGER*4 I4P0, I4P1, I4P4, I4P10, I4P12, I4P20, I4P22, I4P43
      DATA  I4P0, I4P1, I4P4, I4P10, I4P12, I4P20, I4P22, &
     &   I4P43/     0,    1,    4,    10,    12,    20,    22,    43 /
!
      DATA CMP/'XYZ','UEN'/,YN/'NO ','YES'/
      INTEGER*2   INT2_ARG
      INTEGER*4   I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  850108  Created
!   JWR  2004:05:05   I_LEN added to buffer reads.
!
! 5.  NUTAT PROGRAM STRUCTURE
!
!     Make certain nutation offsets are not selected.
!
      ICTERM=1
      ISTAT=1
!
    1 CALL setcr_mn(I4P0,I4P0)
      CALL clear_mn()
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      call nl_mn()
      call nl_mn()
      WRITE(bufstr,1020)
      call addstr_f(bufstr )
      call nl_mn()
 1000 FORMAT("Nutation Time Series Parameter Selection ", &
     &       10x,'SETFL Ver. ')
 1010 FORMAT(4A2)
 1020 FORMAT('Term:',16X,'Longitude',13X,'Obliquity')
!
      DO  I=1,6
!
!     running over the 6 selected terms
!
          IDI(I) = KBITN(LNUT(2),I)
          IDO(I) = KBITN(LNUT(3),I)
          END DO  !running over the 6 selected terms
!
      ICMP=KBITN( IUEN, INT2(1) )+1
!
      IYN=KBITN( IUEN, INT2(2) )+1
!
      IADD(1)=KBITN( FLPSI, INT2(1) )
      IADD(2)=KBITN( FLEPS, INT2(1) )
      IADD(3)=KBITN( FLPSI, INT2(2) )
      IADD(4)=KBITN( FLEPS, INT2(2) )
      IADD(5)=KBITN( FLPSI, INT2(3) )
      IADD(6)=KBITN( FLPSI, INT2(4) )
      IADD(7)=KBITN( FLEPS, INT2(3) )
      IADD(8)=KBITN( FLEPS, INT2(4) )
      IADD(9)= KBITN( FLPSI, INT2((ICTERM+2-1)*2+1))
      IADD(10)=KBITN( FLPSI, INT2((ICTERM+2-1)*2+2))
      IADD(11)=KBITN( FLEPS, INT2((ICTERM+2-1)*2+1))
      IADD(12)=KBITN( FLEPS, INT2((ICTERM+2-1)*2+2))
      IADD(13)=KBITN(LSITEV(1,1),ISTAT)
      IADD(14)=KBITN(LSITEV(1,2),ISTAT)
      IADD(15)=KBITN(LSITEV(1,3),ISTAT)
!
      WRITE(bufstr,1001) IDI(1),IDO(1)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1011) IDI(2),IDO(2)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1012) IDI(3),IDO(3)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1013) IDI(4),IDO(4)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1014) IDI(5),IDO(5)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1015) IDI(6),IDO(6)
      call addstr_f(bufstr )
      call nl_mn()
      call nl_mn()
      WRITE(bufstr,1016) CMP(ICMP)
      call addstr_f(bufstr )
      call nl_mn()
      call nl_mn()
      WRITE(bufstr,1017) YN(IYN)
      call addstr_f(bufstr )
      call nl_mn()
      call nl_mn()
      WRITE(bufstr,1018) (IADD(I),I=1,2)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1019) (IADD(I),I=3,4)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1021) FCNPER,(IADD(I),I=5,8)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1022) ICTERM,(IADD(I),I=9,12)
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,1023) ISTAT,(IADD(I),I=13,15)
      call addstr_f(bufstr )
      call nl_mn()
      call nl_mn()
 1001 FORMAT("Principal  ",10X,I5,11X,I11)
 1011 FORMAT('9.3 years  ',10X,I5,11X,I11)
 1012 FORMAT('Annual     ',10X,I5,11X,I11)
 1013 FORMAT('Semi-annual',10X,I5,11X,I11)
 1014 FORMAT('122  days  ',10X,I5,11X,I11)
 1015 FORMAT('13.7 days  ',10X,I5,11X,I11)
 1016 FORMAT('Use ',A3,' Components')
 1017 FORMAT('Diurnal Radial Station Amplitude and Phase ',A3)
 1018 FORMAT('Constant   ',10X,I5,11X,I11)
 1019 FORMAT('Slope/year ',10X,I5,11X,I11)
 1021 FORMAT('FCN',F8.2   ,10X,I5,3I11)
 1022 FORMAT('CALC',I4,3X ,10X,I5,3I11)
 1023 FORMAT('Velocity Station ',I4,I5,2I11)
      call &
     &     addstr_f("Retur(N) to Last Page  FCN (P)eriod  CALC (T)erm  (S)tation" )
!
!     See what the user wants to do
!
      CALL setcr_mn(I4P1,I4P20 )
   2  CALL senkr_mn(IX,IY,ICH)
      cchar(1:1) = cch(4:4)
      IF(CCHAR(1:1).EQ.'R') THEN
        GO TO 1
      ELSE IF(CCHAR(1:1).EQ.'N' .OR. &
     &       (CCHAR(1:1).EQ.' '.AND.IY.EQ.20.AND.IX.LE.20)) THEN
        NFLPSI=0
        NFLEPS=0
        NDPNUT=0
        DO I=1,216
          NFLPSI=NFLPSI+KBITN(FLPSI,I)
          NFLEPS=NFLEPS+KBITN(FLEPS,I)
        ENDDO
        DO I=1,106
          CALL SBIT( IDPNUT, I, INT2(0) )
          IF(KBIT( FLPSI, INT2(4+(I-1)*2+1)).OR.KBIT( FLPSI, INT2(4+(I-1)*2+ &
     &       2)).OR.KBIT( FLEPS, INT2(4+(I-1)*2+1)).OR.KBIT( FLEPS, INT2(4+(I-1)*2+ &
     &       2) )) THEN
            CALL SBIT( IDPNUT, I, INT2(1) )
            NDPNUT=NDPNUT+1
           ENDIF
         ENDDO
        IF(LNUT(2).NE.0.OR.LNUT(3).NE.0.OR.NFLPSI.NE.0.OR.NFLEPS.NE. &
     &     0)LNUT(1)=0
        RETURN
      ELSE IF(CCHAR(1:1).EQ.'P'.OR. &
     & (CCHAR(1:1).EQ.' '.AND.IY.EQ.20.AND.IX.GE.23.AND.IX.LE.34)) THEN
24       CONTINUE
         CALL setcr_mn(I4P1,I4P22 )
         call addstr_f("New FCN Frequency " )
         call getstr_f(bufstr )
         READ ( BUFSTR(1:I_LEN(BUFSTR)), *, ERR=24 ) FCNPER
         GO TO 1
      ELSE IF(CCHAR(1:1).EQ.'T'.OR. &
     & (CCHAR(1:1).EQ.' '.AND.IY.EQ.20.AND.IX.GE.37.AND.IX.LE.47)) THEN
25       CONTINUE
         CALL setcr_mn(I4P1,I4P22 )
         call addstr_f("New CALC Term to display " )
         call getstr_f(bufstr )
         READ ( BUFSTR(1:I_LEN(BUFSTR)), *, ERR=25 ) ICTERM
         GO TO 1
      ELSE IF(CCHAR(1:1).EQ.'S'.OR. &
     & (CCHAR(1:1).EQ.' '.AND.IY.EQ.20.AND.IX.GE.50.AND.IX.LE.58)) THEN
26       CONTINUE
         CALL setcr_mn(I4P1,I4P22 )
         call addstr_f("New Station Velocity to display " )
         call getstr_f(bufstr )
         READ ( BUFSTR(1:I_LEN(BUFSTR)), *, ERR=26 ) ISTAT
         GO TO 1
      ELSE IF (IX.EQ.25.AND.IY.LT.9.AND.IY.GT.3) THEN !flip on a longitude ter
          ITERM = IY-2
          CALL    SWBIT(LNUT(2),ITERM )
          IDISPV = KBITN(LNUT(2),ITERM)
          WRITE ( BUFSTR(1:I_LEN(BUFSTR)), 1003 ) IDISPV
 1003      FORMAT(I1)
          call addstr_f(bufstr(:1) )
      ELSE IF (IX.EQ.47.AND.IY.LT.9.AND.IY.GT.3) THEN !flip on a obliquity ter
          ITERM = IY-2
          CALL    SWBIT(LNUT(3),ITERM )
          IDISPV = KBITN(LNUT(3),ITERM)
          WRITE(bufstr,1003) IDISPV
          call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.10.AND.IX.LT.18) THEN
        CALL SWBIT( IUEN, INT2(1) )
        ICMP=KBITN( IUEN, INT2(1) )+1
        CALL setcr_mn(I4P4,I4P10 )
        WRITE(bufstr,1004) CMP(ICMP)
        call addstr_f(bufstr(:3) )
        CALL setcr_mn(IX,IY )
1004    FORMAT(A)
      ELSE IF(IY.EQ.12.AND.IX.LT.45) THEN
        CALL SWBIT( IUEN, INT2(2) )
        IYN=KBITN( IUEN, INT2(2) )+1
        CALL setcr_mn(I4P43,I4P12 )
        WRITE(bufstr,1004) YN(IYN)
        call addstr_f(bufstr(:3) )
        CALL setcr_mn(IX,IY )
      ELSE IF(IY.EQ.14.AND.IX.EQ.25) THEN
        CALL SWBIT( FLPSI, INT2(1) )
        IDISPV=KBITN( FLPSI, INT2(1) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.14.AND.IX.EQ.47) THEN
        CALL SWBIT( FLEPS, INT2(1) )
        IDISPV=KBITN( FLEPS, INT2(1) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.15.AND.IX.EQ.25) THEN
        CALL SWBIT( FLPSI, INT2(2) )
        IDISPV=KBITN( FLPSI, INT2(2) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.15.AND.IX.EQ.47) THEN
        CALL SWBIT( FLEPS, INT2(2) )
        IDISPV=KBITN( FLEPS, INT2(2) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.16.AND.IX.EQ.25) THEN
        CALL SWBIT( FLPSI, INT2(3) )
        IDISPV=KBITN( FLPSI, INT2(3) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.16.AND.IX.EQ.36) THEN
        CALL SWBIT( FLPSI, INT2(4) )
        IDISPV=KBITN( FLPSI, INT2(4) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.16.AND.IX.EQ.47) THEN
        CALL SWBIT( FLEPS, INT2(3) )
        IDISPV=KBITN( FLEPS, INT2(3) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.16.AND.IX.EQ.58) THEN
        CALL SWBIT( FLEPS, INT2(4) )
        IDISPV=KBITN( FLEPS, INT2(4) )
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.17.AND.IX.EQ.25) THEN
        CALL SWBIT( FLPSI, INT2((ICTERM+2-1)*2+1) )
        IDISPV=KBITN( FLPSI, INT2((ICTERM+2-1)*2+1))
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.17.AND.IX.EQ.36) THEN
        CALL SWBIT( FLPSI, INT2((ICTERM+2-1)*2+2) )
        IDISPV=KBITN( FLPSI, INT2((ICTERM+2-1)*2+2))
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.17.AND.IX.EQ.47) THEN
        CALL SWBIT( FLEPS, INT2((ICTERM+2-1)*2+1) )
        IDISPV=KBITN( FLEPS, INT2((ICTERM+2-1)*2+1))
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.17.AND.IX.EQ.58) THEN
        CALL SWBIT( FLEPS, INT2((ICTERM+2-1)*2+2) )
        IDISPV=KBITN( FLEPS, INT2((ICTERM+2-1)*2+2))
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.18.AND.IX.EQ.25) THEN
        CALL SWBIT(LSITEV(1,1),ISTAT )
        IDISPV=KBITN(LSITEV(1,1),ISTAT)
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.18.AND.IX.EQ.36) THEN
        CALL SWBIT(LSITEV(1,2),ISTAT )
        IDISPV=KBITN(LSITEV(1,2),ISTAT)
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ELSE IF(IY.EQ.18.AND.IX.EQ.47) THEN
        CALL SWBIT(LSITEV(1,3),ISTAT )
        IDISPV=KBITN(LSITEV(1,3),ISTAT)
        WRITE(bufstr,1003) IDISPV
        call addstr_f(bufstr(:1) )
      ENDIF
      GO TO 2
!
      END
