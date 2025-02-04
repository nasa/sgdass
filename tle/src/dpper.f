! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE DPPER
! *
! *  This Subroutine provides deep space long period periodic contributions
! *    to the mean elements.  by design, these periodics are zero at epoch.
! *    this used to be dscom which included initialization, but it's really a
! *    recurring function.
! *
! *  author        : david vallado                  719-573-2600   28 jun 2005
! *
! *  inputs        :
! *    e3          -
! *    ee2         -
! *    peo         -
! *    pgho        -
! *    pho         -
! *    pinco       -
! *    plo         -
! *    se2 , se3 , Sgh2, Sgh3, Sgh4, Sh2, Sh3, Si2, Si3, Sl2, Sl3, Sl4 -
! *    t           -
! *    xh2, xh3, xi2, xi3, xl2, xl3, xl4 -
! *    zmol        -
! *    zmos        -
! *    ep          - eccentricity                           0.0 - 1.0
! *    inclo       - inclination - needed for lyddane modification
! *    nodep       - right ascension of ascending node
! *    argpp       - argument of perigee
! *    mp          - mean anomaly
! *
! *  outputs       :
! *    ep          - eccentricity                           0.0 - 1.0
! *    inclp       - inclination
! *    nodep       - right ascension of ascending node
! *    argpp       - argument of perigee
! *    mp          - mean anomaly
! *
! *  locals        :
! *    alfdp       -
! *    betdp       -
! *    cosip  , sinip  , cosop  , sinop  ,
! *    dalf        -
! *    dbet        -
! *    dls         -
! *    f2, f3      -
! *    pe          -
! *    pgh         -
! *    ph          -
! *    pinc        -
! *    pl          -
! *    sel   , ses   , sghl  , sghs  , shl   , shs   , sil   , sinzf , sis   ,
! *    sll   , sls
! *    xls         -
! *    xnoh        -
! *    zf          -
! *    zm          -
! *
! *  coupling      :
! *    none.
! *
! *  references    :
! *    hoots, roehrich, norad spacetrack report #3 1980
! *    hoots, norad spacetrack report #6 1986
! *    hoots, schumacher and glover 2004
! *    vallado, crawford, hujsak, kelso  2006
! *------------------------------------------------------------------------------

      SUBROUTINE DPPER( e3    , ee2   , peo   , pgho  , pho   , pinco , &
     &                  plo   , se2   , se3   , sgh2  , sgh3  , sgh4  , &
     &                  sh2   , sh3   , si2   , si3   , sl2   , sl3   , &
     &                  sl4   , T     , xgh2  , xgh3  , xgh4  , xh2   , &
     &                  xh3   , xi2   , xi3   , xl2   , xl3   , xl4   , &
     &                  zmol  , zmos  , inclo , init  ,                 &
     &                  Eccp  , Inclp , nodep , Argpp , Mp    ,         &
     &                  operationmode )
        IMPLICIT NONE
        CHARACTER Init, operationmode
        REAL*8  e3    , ee2   , peo   , pgho  , pho   , pinco , plo   , &
     &          se2   , se3   , sgh2  , sgh3  , sgh4  , sh2   , sh3   , &
     &          si2   , si3   , sl2   , sl3   , sl4   , T     , xgh2  , &
     &          xgh3  , xgh4  , xh2   , xh3   , xi2   , xi3   , xl2   , &
     &          xl3   , xl4   , zmol  , zmos  , inclo ,                 &
     &          Eccp  , Inclp , nodep, Argpp , Mp

! * -------------------------- Local Variables --------------------------
        REAL*8  alfdp , betdp , cosip , cosop , dalf  , dbet  , dls   , &
     &          f2    , f3    , pe    , pgh   , ph    , pinc  , pl    , &
     &          sel   , ses   , sghl  , sghs  , shl   , shs   , sil   , &
     &          sinip , sinop , sinzf , sis   , sll   , sls   , xls   , &
     &          xnoh  , zf    , zm
        REAL*8  Zel   , Zes   , Znl   , Zns
        COMMON /DebugHelp/ Help
        CHARACTER Help
        INCLUDE 'astmath.i'

! * ----------------------------- Constants -----------------------------
        ZES  = 0.01675D0
        ZEL  = 0.05490D0
        ZNS  = 1.19459D-5
        ZNL  = 1.5835218D-4

! * ------------------- CALCULATE TIME VARYING PERIODICS ----------------
        ZM   = ZMOS + ZNS*T

        IF (Init.eq.'y') ZM = ZMOS
        ZF   = ZM + 2.0D0*ZES*DSIN(ZM)
        SINZF= DSIN(ZF)
        F2   =  0.5D0*SINZF*SINZF - 0.25D0
        F3   = -0.5D0*SINZF*DCOS(ZF)
        SES  = SE2*F2 + SE3*F3
        SIS  = SI2*F2 + SI3*F3
        SLS  = SL2*F2 + SL3*F3 + SL4*SINZF
        SGHS = SGH2*F2 + SGH3*F3 + SGH4*SINZF
        SHS  = SH2*F2 + SH3*F3
        ZM   = ZMOL + ZNL*T

        IF (Init.eq.'y') ZM = ZMOL
        ZF   = ZM + 2.0D0*ZEL*DSIN(ZM)
        SINZF= DSIN(ZF)
        F2   =  0.5D0*SINZF*SINZF - 0.25D0
        F3   = -0.5D0*SINZF*DCOS(ZF)
        SEL  = EE2*F2 + E3*F3
        SIL  = XI2*F2 + XI3*F3
        SLL  = XL2*F2 + XL3*F3 + XL4*SINZF
        SGHL = XGH2*F2 + XGH3*F3 + XGH4*SINZF
        SHL  = XH2*F2 + XH3*F3
        PE   = SES + SEL
        PINC = SIS + SIL
        PL   = SLS + SLL
        PGH  = SGHS + SGHL
        PH   = SHS + SHL

        IF (Init.eq.'n') THEN
            PE    = PE   - PEO
            PINC  = PINC - PINCO
            PL    = PL   - PLO
            PGH   = PGH  - PGHO
            PH    = PH   - PHO
            Inclp = Inclp  + PINC
            Eccp  = Eccp   + PE
            SINIP = DSIN(Inclp)
            COSIP = DCOS(Inclp)

! * ------------------------- APPLY PERIODICS DIRECTLY ------------------
! c    sgp4fix for lyddane choice
! c    strn3 used original inclination - this is technically feasible
! c    gsfc used perturbed inclination - also technically feasible
! c    probably best to readjust the 0.2 limit value and limit discontinuity
! c    0.2 rad = 11.45916 deg
! c    use next line for original strn3 approach and original inclination
! c            IF (inclo.ge.0.2D0) THEN
! c    use next line for gsfc version and perturbed inclination
            IF (Inclp.ge.0.2D0) THEN

                PH     = PH/SINIP
                PGH    = PGH - COSIP*PH
                Argpp  = Argpp + PGH
                nodep  = nodep + PH
                Mp     = Mp + PL
              ELSE

! * ----------------- APPLY PERIODICS WITH LYDDANE MODIFICATION ---------
                SINOP  = DSIN(nodep)
                COSOP  = DCOS(nodep)
                ALFDP  = SINIP*SINOP
                BETDP  = SINIP*COSOP
                DALF   =  PH*COSOP + PINC*COSIP*SINOP
                DBET   = -PH*SINOP + PINC*COSIP*COSOP
                ALFDP  = ALFDP + DALF
                BETDP  = BETDP + DBET
                nodep = DMOD(nodep,TwoPi)
                ! sgp4fix for afspc written intrinsic functions
                ! nodep used without a trigonometric function ahead
                IF ((nodep .LT. 0.0D0) .and. (operationmode .eq. 'a'))  &
     &                THEN
                    nodep = nodep + twopi
                  ENDIF
                XLS    = Mp + Argpp + COSIP*nodep
                DLS    = PL + PGH - PINC*nodep*SINIP
                XLS    = XLS + DLS
                XNOH   = nodep
                nodep  = DATAN2(ALFDP,BETDP)
                ! sgp4fix for afspc written intrinsic functions
                ! nodep used without a trigonometric function ahead
                IF ((nodep .LT. 0.0D0) .and. (operationmode .eq. 'a'))  &
     &                THEN
                    nodep = nodep + twopi
                  ENDIF
                IF (DABS(XNOH-nodep) .GT. PI) THEN
                    IF(nodep .lt. XNOH) THEN
                        nodep = nodep+TWOPI
                      ELSE
                        nodep = nodep-TWOPI
                      ENDIF
                  ENDIF
                Mp   = Mp + PL
                Argpp=  XLS - Mp - COSIP*nodep
              ENDIF
          ENDIF

! c        INCLUDE 'debug1.f'

      RETURN
      END  !  end dpper

