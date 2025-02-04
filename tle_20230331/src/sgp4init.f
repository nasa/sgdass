! * -----------------------------------------------------------------------------
! *
! *                             SUBROUTINE SGP4INIT
! *
! *  This subroutine initializes variables for SGP4.
! *
! *  author        : david vallado                  719-573-2600   28 jun 2005
! *
! *  inputs        :
! *    satn        - satellite number
! *    bstar       - sgp4 type drag coefficient              kg/m2er
! *    ecco        - eccentricity
! *    epoch       - epoch time in days from jan 0, 1950. 0 hr
! *    argpo       - argument of perigee (output if ds)
! *    inclo       - inclination
! *    mo          - mean anomaly (output if ds)
! *    no          - mean motion
! *    nodeo      - right ascension of ascending node
! *
! *  outputs       :
! *    satrec      - common block values for subsequent calls
! *    return code - non-zero on error.
! *                   1 - mean elements, ecc >= 1.0 or ecc < -0.001 or a < 0.95 er
! *                   2 - mean motion less than 0.0
! *                   3 - pert elements, ecc < 0.0  or  ecc > 1.0
! *                   4 - semi-latus rectum < 0.0
! *                   5 - epoch elements are sub-orbital
! *                   6 - satellite has decayed
! *
! *  locals        :
! *    CNODM  , SNODM  , COSIM  , SINIM  , COSOMM , SINOMM
! *    Cc1sq  , Cc2    , Cc3
! *    Coef   , Coef1
! *    cosio4      -
! *    day         -
! *    dndt        -
! *    em          - eccentricity
! *    emsq        - eccentricity squared
! *    eeta        -
! *    etasq       -
! *    gam         -
! *    argpm       - argument of perigee
! *    ndem        -
! *    inclm       - inclination
! *    mm          - mean anomaly
! *    nm          - mean motion
! *    perige      - perigee
! *    pinvsq      -
! *    psisq       -
! *    qzms24      -
! *    rtemsq      -
! *    s1, s2, s3, s4, s5, s6, s7          -
! *    sfour       -
! *    ss1, ss2, ss3, ss4, ss5, ss6, ss7         -
! *    sz1, sz2, sz3
! *    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
! *    tc          -
! *    temp        -
! *    temp1, temp2, temp3       -
! *    tsi         -
! *    xpidot      -
! *    xhdot1      -
! *    z1, z2, z3          -
! *    z11, z12, z13, z21, z22, z23, z31, z32, z33         -
! *
! *  coupling      :
! *    getgravconst-
! *    initl       -
! *    dscom       -
! *    dpper       -
! *    dsinit      -
! *
! *  references    :
! *    hoots, roehrich, norad spacetrack report #3 1980
! *    hoots, norad spacetrack report #6 1986
! *    hoots, schumacher and glover 2004
! *    vallado, crawford, hujsak, kelso  2006
! * ---------------------------------------------------------------------------- }

      SUBROUTINE SGP4Init ( whichconst,                                 &
     &                      Satn,   xBStar, xEcco,  Epoch, xArgpo,      &
     &                      xInclo, xMo,    xNo,    xnodeo, Error )
        IMPLICIT NONE
        INTEGER Satn, error, whichconst
        REAL*8  xBStar, xEcco, Epoch, xArgpo, xInclo, xMo, xNo, xnodeo
        REAL*8 T, r(3), v(3)

        INCLUDE 'sgp4.i'

        COMMON /DebugHelp/ Help
        CHARACTER Help

! * -------------------------- Local Variables --------------------------

        REAL*8  Ao,ainv,con42,cosio,sinio,cosio2,Eccsq,omeosq,          &
     &          posq,rp,rteosq, CNODM , SNODM , COSIM , SINIM , COSOMM, &
     &          SINOMM, Cc1sq ,                                         &
     &          Cc2   , Cc3   , Coef  , Coef1 , Cosio4, DAY   , Dndt  , &
     &          Eccm  , EMSQ  , Eeta  , Etasq , GAM   , Argpm , nodem , &
     &          Inclm , Mm    , Xn    , Perige, Pinvsq, Psisq , Qzms24, &
     &          RTEMSQ, S1    , S2    , S3    , S4    , S5    , S6    , &
     &          S7    , SFour , SS1   , SS2   , SS3   , SS4   , SS5   , &
     &          SS6   , SS7   , SZ1   , SZ2   , SZ3   , SZ11  , SZ12  , &
     &          SZ13  , SZ21  , SZ22  , SZ23  , SZ31  , SZ32  , SZ33  , &
     &          Tc    , Temp  , Temp1 , Temp2 , Temp3 , Tsi   , XPIDOT, &
     &          Xhdot1, Z1    , Z2    , Z3    , Z11   , Z12   , Z13   , &
     &          Z21   , Z22   , Z23   , Z31   , Z32   , Z33 
        REAL*8  qzms2t, SS, mu, RadiusEarthKm , J2    , j3oJ2 ,J4,X2o3, &
     &          temp4, j3, xke, tumin
        INCLUDE 'astmath.i'

! * ---------------------------- INITIALIZATION -------------------------
        method = 'n'
! c       clear sgp4 flag
        Error = 0

! c      sgp4fix - note the following variables are also passed directly via sgp4 common. 
! c      it is possible to streamline the sgp4init call by deleting the "x"
! c      variables, but the user would need to set the common values first. we
! c      include the additional assignment in case twoline2rv is not used. 
 
        bstar  = xbstar
        ecco   = xecco
        argpo  = xargpo
        inclo  = xinclo
        mo     = xmo
        no_kozai = xno
        nodeo  = xnodeo

        ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke,   &
     &       j2, j3, j4, j3oj2 )

        SS     = 78.0D0/RadiusEarthKm + 1.0D0
        QZMS2T = ((120.0D0-78.0D0)/RadiusEarthKm) ** 4
        X2o3   =  2.0D0 / 3.0D0
! c     sgp4fix divisor for divide by zero check on inclination
! c     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared it to
! c     1.5D-12, so the threshold was changed to 1.5D-12 for consistency
        temp4    =   1.5D-12

        Init = 'y'
        T = 0.0D0

        CALL INITL( Satn , whichconst, Ecco  , EPOCH , Inclo ,no_kozai, &
     &     Opsmode, Method, AINV  , AO    , CON41 , CON42 , COSIO ,     &
     &     COSIO2 , Eccsq , OMEOSQ, POSQ  , rp    , RTEOSQ, SINIO ,     &
     &     GSTo, no_unkozai )

        IF(rp .lt. 1.0D0) THEN
! c            Write(*,*) '# **! * SATN',Satn,' EPOCH ELTS SUB-ORBITAL **! * '
            Error = 5
          ENDIF

        IF(OMEOSQ .ge. 0.0D0 .OR. no_unkozai .ge. 0.0D0) THEN
            ISIMP = 0
            IF (rp .lt. (220.0D0/RadiusEarthKm+1.0D0)) THEN
                ISIMP = 1
              ENDIF
            SFour  = SS
            QZMS24 = QZMS2T
            PERIGE = (rp-1.0D0)*RadiusEarthKm

! * ----------- For perigees below 156 km, S and Qoms2t are altered -----
            IF(PERIGE .lt. 156.0D0) THEN
                SFour = PERIGE-78.0D0
                IF(PERIGE .le. 98.0D0) THEN
                    SFour = 20.0D0
                  ENDIF
                QZMS24 = ( (120.0D0-SFour)/RadiusEarthKm )**4
                SFour  = SFour/RadiusEarthKm + 1.0D0
              ENDIF
            PINVSQ = 1.0D0/POSQ

            TSI    = 1.0D0/(AO-SFour)
            ETA    = AO*Ecco*TSI
            ETASQ  = ETA*ETA
            EETA   = Ecco*ETA
            PSISQ  = DABS(1.0D0-ETASQ)
            COEF   = QZMS24*TSI**4
            COEF1  = COEF/PSISQ**3.5D0
            CC2    = COEF1*no_unkozai* (AO* (1.0D0+1.5D0*ETASQ+EETA*    &
     &               (4.0D0+ETASQ) )+0.375D0*                           &
     &         J2*TSI/PSISQ*CON41*(8.0D0+3.0D0*ETASQ*(8.0D0+ETASQ)))
            CC1    = BSTAR*CC2
            CC3    = 0.0D0
            IF(Ecco .GT. 1.0D-4) THEN
                CC3 = -2.0D0*COEF*TSI*J3OJ2*no_unkozai*SINIO/Ecco
              ENDIF
            X1MTH2 = 1.0D0-COSIO2
            CC4    = 2.0D0*no_unkozai*COEF1*AO*OMEOSQ*                  &
     &              (ETA*(2.0D0+0.5D0*ETASQ)                            &
     &              +Ecco*(0.5D0 + 2.0D0*ETASQ) - J2*TSI / (AO*PSISQ)*  &
     &              (-3.0D0*CON41*(1.0D0-2.0D0*                         &
     &       EETA+ETASQ*(1.5D0-0.5D0*EETA))+0.75D0*X1MTH2*(2.0D0*ETASQ  &
     &       -EETA*(1.0D0+ETASQ))*DCOS(2.0D0*Argpo)))
            CC5    = 2.0D0*COEF1*AO*OMEOSQ* (1.0D0 + 2.75D0*            &
     &               (ETASQ + EETA) + EETA*ETASQ )
            COSIO4 = COSIO2*COSIO2
            TEMP1  = 1.5D0*J2*PINVSQ*no_unkozai
            TEMP2  = 0.5D0*TEMP1*J2*PINVSQ
            TEMP3  = -0.46875D0*J4*PINVSQ*PINVSQ*no_unkozai
            MDot   = no_unkozai + 0.5D0*TEMP1*RTEOSQ*CON41 +            &
     &               0.0625D0*TEMP2*                                    &
     &               RTEOSQ*(13.0D0 - 78.0D0*COSIO2 + 137.0D0*COSIO4)
            ArgpDot= -0.5D0*TEMP1*CON42 + 0.0625D0*TEMP2*               &
     &               (7.0D0 - 114.0D0*COSIO2 +                          &
     &        395.0D0*COSIO4)+TEMP3*(3.0D0-36.0D0*COSIO2+49.0D0*COSIO4)
            XHDOT1 = -TEMP1*COSIO
            nodeDot = XHDOT1+(0.5D0*TEMP2*(4.0D0-19.0D0*COSIO2)+        &
     &                 2.0D0*TEMP3*(3.0D0 - 7.0D0*COSIO2))*COSIO
            XPIDOT = ArgpDot+nodeDot
            OMGCOF = BSTAR*CC3*DCOS(Argpo)
            XMCOF  = 0.0D0
            IF(Ecco .GT. 1.0D-4) THEN
                XMCOF = -X2O3*COEF*BSTAR/EETA
              ENDIF
            XNODCF = 3.5D0*OMEOSQ*XHDOT1*CC1
            T2COF  = 1.5D0*CC1
! c           sgp4fix for divide by zero with xinco = 180 deg
            if (dabs(cosio+1.0).gt. 1.5d-12) THEN
                XLCOF  = -0.25D0*J3OJ2*SINIO*                           &
     &                   (3.0D0+5.0D0*COSIO)/(1.0D0+COSIO)
              else
                XLCOF  = -0.25D0*J3OJ2*SINIO*                           &
     &                   (3.0D0+5.0D0*COSIO)/temp4
              ENDIF
            AYCOF  = -0.5D0*J3OJ2*SINIO
            DELMO  = (1.0D0+ETA*DCOS(Mo))**3
            SINMAO = DSIN(Mo)
            X7THM1 = 7.0D0*COSIO2-1.0D0

! * ------------------------ Deep Space Initialization ------------------
            IF ((TWOPI/no_unkozai) .ge. 225.0D0) THEN
                METHOD = 'd'
                ISIMP  = 1
                TC     = 0.0D0
                Inclm  = Inclo
                CALL DSCOM( EPOCH     , Ecco  , Argpo , Tc    , Inclo , &
     &                  nodeo, no_unkozai ,                             &
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
                CALL DPPER( e3, ee2   , peo   , pgho  , pho   , pinco , &
     &                  plo   , se2   , se3   , sgh2  , sgh3  , sgh4  , &
     &                  sh2   , sh3   , si2   , si3   , sl2   , sl3   , &
     &                  sl4   , T     , xgh2  , xgh3  , xgh4  , xh2   , &
     &                  xh3   , xi2   , xi3   , xl2   , xl3   , xl4   , &
     &                  zmol  , zmos  , Inclm , init  ,                 &
     &                  Ecco  , Inclo , nodeo, Argpo , Mo, Opsmode )

                Argpm  = 0.0D0 ! add for DS to work initial
                nodem  = 0.0D0
                Mm     = 0.0D0

                CALL DSINIT( whichconst,                                 &
     &                   Cosim ,Emsq   , Argpo , S1    , S2    , S3    , &
     &                   S4    , S5    , Sinim , Ss1   , Ss2   , Ss3   , &
     &                   Ss4   , Ss5   , Sz1   , Sz3   , Sz11  , Sz13  , &
     &                   Sz21  , Sz23  , Sz31  , Sz33  , T     , Tc    , &
     &                   GSTo  , Mo  , MDot , no_unkozai,nodeo, nodeDot, &
     &                   XPIDOT, Z1    , Z3    , Z11   , Z13   , Z21   , &
     &                   Z23   , Z31   , Z33   , ecco  , eccsq,          &
     &                   Eccm  , Argpm , Inclm , Mm    , Xn    , nodem , &
     &                   IREZ  , Atime , D2201 , D2211 , D3210 , D3222 , &
     &                   D4410 , D4422 , D5220 , D5232 , D5421 , D5433 , &
     &                   Dedt  , Didt  , DMDT  , DNDT  , DNODT , DOMDT , &
     &                   Del1  , Del2  , Del3  , Xfact , Xlamo , Xli   , &
     &                   Xni )
            ENDIF

! * ------------ Set variables if not deep space or rp < 220 -------------
            IF (ISIMP .ne. 1) THEN
                CC1SQ = CC1*CC1
                D2    = 4.0D0*AO*TSI*CC1SQ
                TEMP  = D2*TSI*CC1 / 3.0D0
                D3    = (17.0D0*AO + SFour) * TEMP
                D4    = 0.5D0*TEMP*AO*TSI*                              &
     &                  (221.0D0*AO + 31.0D0*SFour)*CC1
                T3COF = D2 + 2.0D0*CC1SQ
                T4COF = 0.25D0* (3.0D0*D3+CC1*(12.0D0*D2+10.0D0*CC1SQ) )
                T5COF = 0.2D0* (3.0D0*D4 + 12.0D0*CC1*D3 + 6.0D0*D2*D2 + &
     &                  15.0D0*CC1SQ* (2.0D0*D2 + CC1SQ) )
              ENDIF

          ENDIF ! ------ if nodeo and No are gtr 0

      init = 'n'

      CALL SGP4(whichconst, 0.0D0, r, v, error)

!@@NH@@!        INCLUDE 'debug6.f'

      RETURN
      END  ! end sgp4init
