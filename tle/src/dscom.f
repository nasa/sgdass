! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE DSCOM
! *
! *  This Subroutine provides deep space common items used by both the secular
! *    and periodics subroutines.  input is provided as shown. this routine
! *    used to be called dpper, but the functions inside weren't well organized.
! *
! *  author        : david vallado                  719-573-2600   28 jun 2005
! *
! *  inputs        :
! *    epoch       -
! *    ep          - eccentricity
! *    argpp       - argument of perigee
! *    tc          -
! *    inclp       - inclination
! *    nodep      - right ascension of ascending node
! *    np          - mean motion
! *
! *  outputs       :
! *    sinim  , cosim  , sinomm , cosomm , snodm  , cnodm
! *    day         -
! *    e3          -
! *    ee2         -
! *    em          - eccentricity
! *    emsq        - eccentricity squared
! *    gam         -
! *    peo         -
! *    pgho        -
! *    pho         -
! *    pinco       -
! *    plo         -
! *    rtemsq      -
! *    se2, se3         -
! *    sgh2, sgh3, sgh4        -
! *    sh2, sh3, si2, si3, sl2, sl3, sl4         -
! *    s1, s2, s3, s4, s5, s6, s7          -
! *    ss1, ss2, ss3, ss4, ss5, ss6, ss7, sz1, sz2, sz3         -
! *    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
! *    xgh2, xgh3, xgh4, xh2, xh3, xi2, xi3, xl2, xl3, xl4         -
! *    nm          - mean motion
! *    z1, z2, z3, z11, z12, z13, z21, z22, z23, z31, z32, z33         -
! *    zmol        -
! *    zmos        -
! *
! *  locals        :
! *    a1, a2, a3, a4, a5, a6, a7, a8, a9, a10         -
! *    betasq      -
! *    cc          -
! *    ctem, stem        -
! *    x1, x2, x3, x4, x5, x6, x7, x8          -
! *    xnodce      -
! *    xnoi        -
! *    zcosg  , zsing  , zcosgl , zsingl , zcosh  , zsinh  , zcoshl , zsinhl ,
! *    zcosi  , zsini  , zcosil , zsinil ,
! *    zx          -
! *    zy          -
! *
! *  coupling      :
! *    none.
! *
! *  references    :
! *    hoots, roehrich, norad spacetrack report #3 1980
! *    hoots, norad spacetrack report #6 1986
! *    hoots, schumacher and glover 2004
! *    vallado, crawford, hujsak, kelso  2006
! * ------------------------------------------------------------------------------

      SUBROUTINE DSCOM( EPOCH , Eccp  , Argpp , Tc    , Inclp , nodep , &
     &                  Np    ,                                         &
     &                  SNODM , CNODM , SINIM , COSIM , SINOMM, COSOMM, &
     &                  DAY   , E3    , Ee2   , Eccm  , EMSQ  , GAM   , &
     &                  Peo   , Pgho  , Pho   , PInco , Plo   ,         &
     &                  RTemSq, Se2   , Se3   , Sgh2  , Sgh3  , Sgh4  , &
     &                  Sh2   , Sh3   , Si2   , Si3   , Sl2   , Sl3   , &
     &                  Sl4   , S1    , S2    , S3    , S4    , S5    , &
     &                  S6    , S7    , SS1   , SS2   , SS3   , SS4   , &
     &                  SS5   , SS6   , SS7   , SZ1   , SZ2   , SZ3   , &
     &                  SZ11  , SZ12  , SZ13  , SZ21  , SZ22  , SZ23  , &
     &                  SZ31  , SZ32  , SZ33  , Xgh2  , Xgh3  , Xgh4  , &
     &                  Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   , &
     &                  Xl4   , Xn    , Z1    , Z2    , Z3    , Z11   , &
     &                  Z12   , Z13   , Z21   , Z22   , Z23   , Z31   , &
     &                  Z32   , Z33   , Zmol  , Zmos )
        IMPLICIT NONE
        REAL*8  EPOCH , Eccp  , Argpp , Tc    , Inclp , nodep , Np    , &
     &          SNODM , CNODM , SINIM , COSIM , SINOMM, COSOMM, DAY   , &
     &          E3    , Ee2   , Eccm  , EMSQ  , GAM   , RTemSq, Se2   , &
     &          Peo   , Pgho  , Pho   , PInco , Plo   ,                 &
     &          Se3   , Sgh2  , Sgh3  , Sgh4  , Sh2   , Sh3   , Si2   , &
     &          Si3   , Sl2   , Sl3   , Sl4   , S1    , S2    , S3    , &
     &          S4    , S5    , S6    , S7    , SS1   , SS2   , SS3   , &
     &          SS4   , SS5   , SS6   , SS7   , SZ1   , SZ2   , SZ3   , &
     &          SZ11  , SZ12  , SZ13  , SZ21  , SZ22  , SZ23  , SZ31  , &
     &          SZ32  , SZ33  , Xgh2  , Xgh3  , Xgh4  , Xh2   , Xh3   , &
     &          Xi2   , Xi3   , Xl2   , Xl3   , Xl4   , Xn    , Z1    , &
     &          Z2    , Z3    , Z11   , Z12   , Z13   , Z21   , Z22   , &
     &          Z23   , Z31   , Z32   , Z33   , Zmol  , Zmos

! * -------------------------- Local Variables --------------------------
        REAL*8  c1ss  , c1L   , zcosis, zsinis, zsings, zcosgs,         &
     &          Zes   , zel
        INTEGER LsFlg
        REAL*8  a1    , a2    , a3    , a4    , a5    , a6    , a7    , &
     &          a8    , a9    , a10   , betasq, cc    , ctem  , stem  , &
     &          x1    , x2    , x3    , x4    , x5    , x6    , x7    , &
     &          x8    , xnodce, xnoi  , zcosg , zcosgl, zcosh , zcoshl, &
     &          zcosi , zcosil, zsing , zsingl, zsinh , zsinhl, zsini , &
     &          zsinil, zx    , zy

        COMMON /DebugHelp/ Help
        CHARACTER Help
        INCLUDE 'astmath.i'

! * ------------------------------ Constants ----------------------------
        ZES    =  0.01675D0
        ZEL    =  0.05490D0
        C1SS   =  2.9864797D-6
        C1L    =  4.7968065D-7
        ZSINIS =  0.39785416D0
        ZCOSIS =  0.91744867D0
        ZCOSGS =  0.1945905D0
        ZSINGS = -0.98088458D0

! * ----------------- DEEP SPACE PERIODICS INITIALIZATION ---------------
        XN     = Np
        Eccm   = Eccp
        SNODM  = DSIN(nodep)
        CNODM  = DCOS(nodep)
        SINOMM = DSIN(Argpp)
        COSOMM = DCOS(Argpp)
        SINIM  = DSIN(Inclp)
        COSIM  = DCOS(Inclp)
        EMSQ   = Eccm*Eccm
        BETASQ = 1.0D0-EMSQ
        RTEMSQ = DSQRT(BETASQ)

! * --------------------- INITIALIZE LUNAR SOLAR TERMS ------------------
        PEO    = 0.0D0
        PINCO  = 0.0D0
        PLO    = 0.0D0
        PGHO   = 0.0D0
        PHO    = 0.0D0
        DAY    = EPOCH + 18261.5D0 + TC/1440.0D0
        XNODCE = DMOD(4.5236020D0 - 9.2422029D-4*DAY,TwoPi)
        STEM   = DSIN(XNODCE)
        CTEM   = DCOS(XNODCE)
        ZCOSIL = 0.91375164D0 - 0.03568096D0*CTEM
        ZSINIL = DSQRT(1.0D0 - ZCOSIL*ZCOSIL)
        ZSINHL = 0.089683511D0*STEM / ZSINIL
        ZCOSHL = DSQRT(1.0D0 - ZSINHL*ZSINHL)
        GAM    = 5.8351514D0 + 0.0019443680D0*DAY
        ZX     = 0.39785416D0*STEM/ZSINIL
        ZY     = ZCOSHL*CTEM + 0.91744867D0*ZSINHL*STEM
        ZX     = DATAN2(ZX,ZY)
        ZX     = GAM + ZX - XNODCE
        ZCOSGL = DCOS(ZX)
        ZSINGL = DSIN(ZX)

! * ---------------------------- DO SOLAR TERMS -------------------------
        ZCOSG = ZCOSGS
        ZSING = ZSINGS
        ZCOSI = ZCOSIS
        ZSINI = ZSINIS
        ZCOSH = CNODM
        ZSINH = SNODM
        CC    = C1SS
        XNOI  = 1.0D0 / XN

        DO LSFlg = 1,2
            A1 =   ZCOSG*ZCOSH + ZSING*ZCOSI*ZSINH
            A3 =  -ZSING*ZCOSH + ZCOSG*ZCOSI*ZSINH
            A7 =  -ZCOSG*ZSINH + ZSING*ZCOSI*ZCOSH
            A8 =   ZSING*ZSINI
            A9 =   ZSING*ZSINH + ZCOSG*ZCOSI*ZCOSH
            A10=   ZCOSG*ZSINI
            A2 =   COSIM*A7 + SINIM*A8
            A4 =   COSIM*A9 + SINIM*A10
            A5 =  -SINIM*A7 + COSIM*A8
            A6 =  -SINIM*A9 + COSIM*A10

            X1 =  A1*COSOMM + A2*SINOMM
            X2 =  A3*COSOMM + A4*SINOMM
            X3 = -A1*SINOMM + A2*COSOMM
            X4 = -A3*SINOMM + A4*COSOMM
            X5 =  A5*SINOMM
            X6 =  A6*SINOMM
            X7 =  A5*COSOMM
            X8 =  A6*COSOMM

            Z31= 12.0D0*X1*X1 - 3.0D0*X3*X3
            Z32= 24.0D0*X1*X2 - 6.0D0*X3*X4
            Z33= 12.0D0*X2*X2 - 3.0D0*X4*X4
            Z1 =  3.0D0* (A1*A1 + A2*A2) + Z31*EMSQ
            Z2 =  6.0D0* (A1*A3 + A2*A4) + Z32*EMSQ
            Z3 =  3.0D0* (A3*A3 + A4*A4) + Z33*EMSQ
            Z11= -6.0D0*A1*A5 + EMSQ* (-24.0D0*X1*X7-6.0D0*X3*X5)
            Z12= -6.0D0* (A1*A6 + A3*A5) + EMSQ*                        &
     &           ( -24.0D0*(X2*X7+X1*X8) - 6.0D0*(X3*X6+X4*X5) )
            Z13= -6.0D0*A3*A6 + EMSQ*(-24.0D0*X2*X8 - 6.0D0*X4*X6)
            Z21=  6.0D0*A2*A5 + EMSQ*(24.0D0*X1*X5-6.0D0*X3*X7)
            Z22=  6.0D0* (A4*A5 + A2*A6) + EMSQ*                        &
     &           (  24.0D0*(X2*X5+X1*X6) - 6.0D0*(X4*X7+X3*X8) )
            Z23=  6.0D0*A4*A6 + EMSQ*(24.0D0*X2*X6 - 6.0D0*X4*X8)
            Z1 = Z1 + Z1 + BETASQ*Z31
            Z2 = Z2 + Z2 + BETASQ*Z32
            Z3 = Z3 + Z3 + BETASQ*Z33
            S3 = CC*XNOI
            S2 = -0.5D0*S3 / RTEMSQ
            S4 = S3*RTEMSQ
            S1 = -15.0D0*Eccm*S4
            S5 = X1*X3 + X2*X4
            S6 = X2*X3 + X1*X4
            S7 = X2*X4 - X1*X3

! * ------------------------------ DO LUNAR TERMS -----------------------
            IF (LSFLG.eq.1) THEN
                SS1   = S1
                SS2   = S2
                SS3   = S3
                SS4   = S4
                SS5   = S5
                SS6   = S6
                SS7   = S7
                SZ1   = Z1
                SZ2   = Z2
                SZ3   = Z3
                SZ11  = Z11
                SZ12  = Z12
                SZ13  = Z13
                SZ21  = Z21
                SZ22  = Z22
                SZ23  = Z23
                SZ31  = Z31
                SZ32  = Z32
                SZ33  = Z33
                ZCOSG = ZCOSGL
                ZSING = ZSINGL
                ZCOSI = ZCOSIL
                ZSINI = ZSINIL
                ZCOSH = ZCOSHL*CNODM+ZSINHL*SNODM
                ZSINH = SNODM*ZCOSHL-CNODM*ZSINHL
                CC    = C1L
              ENDIF
          ENDDO

        ZMOL  = DMOD( 4.7199672D0 + 0.22997150D0*DAY-GAM,TwoPi )
        ZMOS  = DMOD( 6.2565837D0 + 0.017201977D0*DAY,TwoPi )

! * ---------------------------- DO SOLAR TERMS -------------------------
        SE2 =   2.0D0*SS1*SS6
        SE3 =   2.0D0*SS1*SS7
        SI2 =   2.0D0*SS2*SZ12
        SI3 =   2.0D0*SS2*(SZ13-SZ11)
        SL2 =  -2.0D0*SS3*SZ2
        SL3 =  -2.0D0*SS3*(SZ3-SZ1)
        SL4 =  -2.0D0*SS3*(-21.0D0-9.0D0*EMSQ)*ZES
        SGH2=   2.0D0*SS4*SZ32
        SGH3=   2.0D0*SS4*(SZ33-SZ31)
        SGH4= -18.0D0*SS4*ZES
        SH2 =  -2.0D0*SS2*SZ22
        SH3 =  -2.0D0*SS2*(SZ23-SZ21)

! * ---------------------------- DO LUNAR TERMS -------------------------
        EE2 =   2.0D0*S1*S6
        E3  =   2.0D0*S1*S7
        XI2 =   2.0D0*S2*Z12
        XI3 =   2.0D0*S2*(Z13-Z11)
        XL2 =  -2.0D0*S3*Z2
        XL3 =  -2.0D0*S3*(Z3-Z1)
        XL4 =  -2.0D0*S3*(-21.0D0-9.0D0*EMSQ)*ZEL
        XGH2=   2.0D0*S4*Z32
        XGH3=   2.0D0*S4*(Z33-Z31)
        XGH4= -18.0D0*S4*ZEL
        XH2 =  -2.0D0*S2*Z22
        XH3 =  -2.0D0*S2*(Z23-Z21)

! c        INCLUDE 'debug2.f'

      RETURN
      END  !  dscom
