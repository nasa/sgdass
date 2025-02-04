! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE DSINIT
! *
! *  This Subroutine provides Deep Space contributions to Mean Motion Dot due
! *    to geopotential resonance with half day and one day orbits.
! *
! *  Inputs        :
! *    Cosim, Sinim-
! *    Emsq        - Eccentricity squared
! *    Argpo       - Argument of Perigee
! *    S1, S2, S3, S4, S5      -
! *    Ss1, Ss2, Ss3, Ss4, Ss5 -
! *    Sz1, Sz3, Sz11, Sz13, Sz21, Sz23, Sz31, Sz33 -
! *    T           - Time
! *    Tc          -
! *    GSTo        - Greenwich sidereal time                   rad
! *    Mo          - Mean Anomaly
! *    MDot        - Mean Anomaly dot (rate)
! *    No          - Mean Motion
! *    nodeo       - right ascension of ascending node
! *    nodeDot     - right ascension of ascending node dot (rate)
! *    XPIDOT      -
! *    Z1, Z3, Z11, Z13, Z21, Z23, Z31, Z33 -
! *    Eccm        - Eccentricity
! *    Argpm       - Argument of perigee
! *    Inclm       - Inclination
! *    Mm          - Mean Anomaly
! *    Xn          - Mean Motion
! *    nodem       - right ascension of ascending node
! *
! *  Outputs       :
! *    Eccm        - Eccentricity
! *    Argpm       - Argument of perigee
! *    Inclm       - Inclination
! *    Mm          - Mean Anomaly
! *    Xn          - Mean motion
! *    nodem       - right ascension of ascending node
! *    IRez        - Resonance flags              0-none, 1-One day,  2-Half day
! *    Atime       -
! *    D2201, D2211, D3210, D3222, D4410, D4422, D5220, D5232, D5421, D5433       -
! *    Dedt        -
! *    Didt        -
! *    DMDT        -
! *    DNDT        -
! *    DNODT       -
! *    DOMDT       -
! *    Del1, Del2, Del3 -
! *    Ses  , Sghl , Sghs , Sgs  , Shl  , Shs  , Sis  , Sls
! *    THETA       -
! *    Xfact       -
! *    Xlamo       -
! *    Xli         -
! *    Xni
! *
! *  Locals        :
! *    ainv2       -
! *    aonv        -
! *    cosisq      -
! *    eoc         -
! *    f220, f221, f311, f321, f322, f330, f441, f442, f522, f523, f542, f543        -
! *    g200, g201, g211, g300, g310, g322, g410, g422, g520, g521, g532, g533        -
! *    sini2       -
! *    temp, temp1 -
! *    Theta       -
! *    xno2        -
! *
! *  Coupling      :
! *    getgravconst-
! *
! *  references    :
! *    hoots, roehrich, norad spacetrack report #3 1980
! *    hoots, norad spacetrack report #6 1986
! *    hoots, schumacher and glover 2004
! *    vallado, crawford, hujsak, kelso  2006
! * ------------------------------------------------------------------------------

      SUBROUTINE DSINIT( whichconst,                                     &
     &                   Cosim , Emsq  , Argpo , S1    , S2    , S3    , &
     &                   S4    , S5    , Sinim , Ss1   , Ss2   , Ss3   , &
     &                   Ss4   , Ss5   , Sz1   , Sz3   , Sz11  , Sz13  , &
     &                   Sz21  , Sz23  , Sz31  , Sz33  , T     , Tc    , &
     &                   GSTo  , Mo    , MDot  ,no_kozai,nodeo ,nodeDot, &
     &                   XPIDOT, Z1    , Z3    , Z11   , Z13   , Z21   , &
     &                   Z23   , Z31   , Z33   , Ecco  , EccSq ,         &
     &                   Eccm  , Argpm , Inclm , Mm    , Xn    , nodem , &
     &                   IREZ  , Atime , D2201 , D2211 , D3210 , D3222 , &
     &                   D4410 , D4422 , D5220 , D5232 , D5421 , D5433 , &
     &                   Dedt  , Didt  , DMDT  , DNDT  , DNODT , DOMDT , &
     &                   Del1  , Del2  , Del3  , Xfact , Xlamo , Xli   , &
     &                   Xni )
        IMPLICIT NONE
        INTEGER  IRez, whichconst
        REAL*8   Cosim , Emsq  , Argpo , S1    , S2    , S3    , S4    , &
     &           S5    , Sinim , Ss1   , Ss2   , Ss3   , Ss4   , Ss5   , &
     &           Sz1   , Sz3   , Sz11  , Sz13  , Sz21  , Sz23  , Sz31  , &
     &           Sz33  , T     , Tc    , GSTo  , Mo    , MDot ,no_kozai, &
     &           nodeo ,nodeDot,XPIDOT , Z1    , Z3    , Z11   , Z13   , &
     &           Z21   , Z23   , Z31   , Z33   , Eccm  , Argpm , Inclm , &
     &           Mm    , Xn    , nodem , Atime , D2201 , D2211 , D3210 , &
     &           D3222 , D4410 , D4422 , D5220 , D5232 , D5421 , D5433 , &
     &           Dedt  , Didt  , DMDT  , DNDT  , DNODT , DOMDT , Del1  , &
     &           Del2  , Del3  , Xfact , Xlamo , Xli   , Xni   , Ecco  , &
     &           Eccsq

! * -------------------------- Local Variables --------------------------
        REAL*8  ainv2 , aonv  , cosisq, eoc   , f220  , f221  , f311  , &
     &          f321  , f322  , f330  , f441  , f442  , f522  , f523  , &
     &          f542  , f543  , g200  , g201  , g211  , g300  , g310  , &
     &          g322  , g410  , g422  , g520  , g521  , g532  , g533  , &
     &          ses   , sgs   , sghl  , sghs  , shs   , shl   , sis   , &
     &          sini2 , sls   , temp  , temp1 , Theta , xno2
        REAL*8  Q22   , Q31   , Q33   , ROOT22, ROOT44, ROOT54,         &
     &          RPTim , Root32, Root52, X2o3  , XKe   , Znl   ,         &
     &          Zns,  Emo, emsqo , tumin, mu, radiusearthkm,j2, j3, j4, &
     &          j3oj2

        COMMON /DebugHelp/ Help
        CHARACTER Help
        INCLUDE 'astmath.i'

        Q22    = 1.7891679D-6
        Q31    = 2.1460748D-6
        Q33    = 2.2123015D-7
        ROOT22 = 1.7891679D-6
        ROOT44 = 7.3636953D-9
        ROOT54 = 2.1765803D-9
        RPTim  = 4.37526908801129966D-3 ! this equates to 7.29211514668855e-5 rad/sec
        Root32 = 3.7393792D-7
        Root52 = 1.1428639D-7
        X2o3   = 2.0D0 / 3.0D0
        ZNL    = 1.5835218D-4
        ZNS    = 1.19459D-5

        ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke,   &
     &       j2, j3, j4, j3oj2 )

! * ------------------------ DEEP SPACE INITIALIZATION ------------------
        IREZ = 0
        IF ((XN.lt.0.0052359877D0).AND.(XN.GT.0.0034906585D0)) THEN
            IREZ = 1
          ENDIF
        IF ((XN.ge.8.26D-3).AND.(XN.LE.9.24D-3).AND.(Eccm.GE.0.5D0))THEN
            IREZ = 2
          ENDIF

! * ---------------------------- DO SOLAR TERMS -------------------------
        SES  =  SS1*ZNS*SS5
        SIS  =  SS2*ZNS*(SZ11 + SZ13)
        SLS  = -ZNS*SS3*(SZ1 + SZ3 - 14.0D0 - 6.0D0*EMSQ)
        SGHS =  SS4*ZNS*(SZ31 + SZ33 - 6.0D0)
        SHS  = -ZNS*SS2*(SZ21 + SZ23)
! c       sgp4fix for 180 deg incl
        IF ((Inclm.lt.5.2359877D-2).or.(Inclm.gt.pi-5.2359877D-2)) THEN
            SHS = 0.0D0
          ENDIF
        IF (SINIM.ne.0.0D0) THEN
            SHS = SHS/SINIM
          ENDIF
        SGS  = SGHS - COSIM*SHS

! * ----------------------------- DO LUNAR TERMS ------------------------
        DEDT = SES + S1*ZNL*S5
        DIDT = SIS + S2*ZNL*(Z11 + Z13)
        DMDT = SLS - ZNL*S3*(Z1 + Z3 - 14.0D0 - 6.0D0*EMSQ)
        SGHL = S4*ZNL*(Z31 + Z33 - 6.0D0)
        SHL  = -ZNL*S2*(Z21 + Z23)
! c       sgp4fix for 180 deg incl
        IF ((Inclm.lt.5.2359877D-2).or.(Inclm.gt.pi-5.2359877D-2)) THEN
            SHL = 0.0D0
          ENDIF
        DOMDT= SGS+SGHL
        DNODT= SHS
        IF (SINIM .ne. 0.0D0) THEN
            DOMDT = DOMDT-COSIM/SINIM*SHL
            DNODT = DNODT+SHL/SINIM
        ENDIF

! * --------------- CALCULATE DEEP SPACE RESONANCE EFFECTS --------------
        DNDT  = 0.0D0
        THETA = DMOD(GSTo + TC*RPTIM,TwoPi)
        Eccm  = Eccm + DEDT*T
        emsq  = eccm**2
        Inclm = Inclm + DIDT*T
        Argpm = Argpm + DOMDT*T
        nodem = nodem + DNODT*T
        Mm    = Mm + DMDT*T
! c   sgp4fix for negative inclinations
! c   the following if statement should be commented out
! c           IF(Inclm .lt. 0.0D0) THEN
! c             Inclm  = -Inclm
! c             Argpm  = Argpm-PI
! c             nodem = nodem+PI
! c           ENDIF

! * ------------------ Initialize the resonance terms -------------------
        IF (IREZ .ne. 0) THEN
            AONV = (XN/XKE)**X2O3

! * -------------- GEOPOTENTIAL RESONANCE FOR 12 HOUR ORBITS ------------
        IF (IREZ .eq. 2) THEN
            COSISQ = COSIM*COSIM
            emo    = Eccm
            emsqo  = emsq
            Eccm   = ecco
            emsq   = eccsq
            EOC    = Eccm*EMSQ
            G201   = -0.306D0-(Eccm-0.64D0)*0.440D0
            IF (Eccm.le.0.65D0) THEN
                G211 =   3.616D0 -  13.2470D0*Eccm +  16.2900D0*EMSQ
                G310 = -19.302D0 + 117.3900D0*Eccm - 228.4190D0*EMSQ +  &
     &                 156.591D0*EOC
                G322 = -18.9068D0+ 109.7927D0*Eccm - 214.6334D0*EMSQ + &
     &                 146.5816D0*EOC
                G410 = -41.122D0 + 242.6940D0*Eccm - 471.0940D0*EMSQ + &
     &                 313.953D0*EOC
                G422 =-146.407D0 + 841.8800D0*Eccm - 1629.014D0*EMSQ + &
     &                1083.435D0*EOC
                G520 =-532.114D0 + 3017.977D0*Eccm - 5740.032D0*EMSQ + &
     &                3708.276D0*EOC
              ELSE
                G211 =  -72.099D0 +  331.819D0*Eccm -  508.738D0*EMSQ + &
     &                  266.724D0*EOC
                G310 = -346.844D0 + 1582.851D0*Eccm - 2415.925D0*EMSQ + &
     &                 1246.113D0*EOC
                G322 = -342.585D0 + 1554.908D0*Eccm - 2366.899D0*EMSQ + &
     &                 1215.972D0*EOC
                G410 =-1052.797D0 + 4758.686D0*Eccm - 7193.992D0*EMSQ + &
     &                 3651.957D0*EOC
                G422 =-3581.690D0 + 16178.11D0*Eccm - 24462.77D0*EMSQ + &
     &                12422.52D0*EOC
                IF (Eccm.gt.0.715D0) THEN
                    G520 =-5149.66D0 + 29936.92D0*Eccm -54087.36D0*EMSQ &
     &                    + 31324.56D0*EOC
                  ELSE
                    G520 = 1464.74D0 -  4664.75D0*Eccm + 3763.64D0*EMSQ
                  ENDIF
              ENDIF
            IF (Eccm.lt.0.7D0) THEN
                G533 = -919.22770D0 + 4988.6100D0*Eccm-9064.7700D0*EMSQ &
     &               + 5542.21D0*EOC
                G521 = -822.71072D0 + 4568.6173D0*Eccm-8491.4146D0*EMSQ &
     &               + 5337.524D0*EOC
                G532 = -853.66600D0 + 4690.2500D0*Eccm-8624.7700D0*EMSQ &
     &               + 5341.4D0*EOC
              ELSE
                G533 =-37995.780D0 + 161616.52D0*Eccm-229838.20D0*EMSQ+ &
     &              109377.94D0*EOC
                G521 =-51752.104D0 + 218913.95D0*Eccm-309468.16D0*EMSQ+ &
     &              146349.42D0*EOC
                G532 =-40023.880D0 + 170470.89D0*Eccm-242699.48D0*EMSQ+ &
     &              115605.82D0*EOC
              ENDIF
            SINI2 =  SINIM*SINIM
            F220  =  0.75D0* (1.0D0+2.0D0*COSIM+COSISQ)
            F221  =  1.5D0*SINI2
            F321  =  1.875D0*SINIM * (1.0D0-2.0D0*COSIM-3.0D0*COSISQ)
            F322  = -1.875D0*SINIM * (1.0D0+2.0D0*COSIM-3.0D0*COSISQ)
            F441  = 35.0D0*SINI2*F220
            F442  = 39.3750D0*SINI2*SINI2
            F522  =  9.84375D0*SINIM * (SINI2* (1.0D0-2.0D0*COSIM-      &
     &               5.0D0*COSISQ)+0.33333333D0 * (-2.0D0+4.0D0*COSIM+  &
     &               6.0D0*COSISQ) )
            F523  =  SINIM * (4.92187512D0*SINI2 * (-2.0D0-4.0D0*COSIM+ &
     &               10.0D0*COSISQ) + 6.56250012D0*                     &
     &               (1.0D0+2.0D0*COSIM-3.0D0*COSISQ))
            F542  =  29.53125D0*SINIM * (2.0D0-8.0D0*COSIM+COSISQ*      &
     &               (-12.0D0+8.0D0*COSIM+10.0D0*COSISQ) )
            F543  = 29.53125D0*SINIM * (-2.0D0-8.0D0*COSIM+COSISQ*      &
     &               (12.0D0+8.0D0*COSIM-10.0D0*COSISQ) )

            XNO2   =  XN * XN
            AINV2  =  AONV * AONV
            TEMP1  =  3.0D0*XNO2*AINV2
            TEMP   =  TEMP1*ROOT22
            D2201  =  TEMP*F220*G201
            D2211  =  TEMP*F221*G211
            TEMP1  =  TEMP1*AONV
            TEMP   =  TEMP1*ROOT32
            D3210  =  TEMP*F321*G310
            D3222  =  TEMP*F322*G322
            TEMP1  =  TEMP1*AONV
            TEMP   =  2.0D0*TEMP1*ROOT44
            D4410  =  TEMP*F441*G410
            D4422  =  TEMP*F442*G422
            TEMP1  =  TEMP1*AONV
            TEMP   =  TEMP1*ROOT52
            D5220  =  TEMP*F522*G520
            D5232  =  TEMP*F523*G532
            TEMP   =  2.0D0*TEMP1*ROOT54
            D5421  =  TEMP*F542*G521
            D5433  =  TEMP*F543*G533
            XLAMO  =  DMOD(Mo+nodeo+nodeo-THETA-THETA,TwoPi)
            XFACT  = MDot + DMDT + 2.0D0 * (nodeDot+DNODT-RPTIM)        &
     &                - no_kozai

            Eccm = emo
            emsq = emsqo
          ENDIF

        IF (Irez .eq. 1) THEN
! * -------------------- SYNCHRONOUS RESONANCE TERMS --------------------
            G200  = 1.0D0 + EMSQ * (-2.5D0+0.8125D0*EMSQ)
            G310  = 1.0D0 + 2.0D0*EMSQ
            G300  = 1.0D0 + EMSQ * (-6.0D0+6.60937D0*EMSQ)
            F220  = 0.75D0 * (1.0D0+COSIM) * (1.0D0+COSIM)
            F311  = 0.9375D0*SINIM*SINIM*                               &
     &               (1.0D0+3.0D0*COSIM) - 0.75D0*(1.0D0+COSIM)
            F330  = 1.0D0+COSIM
            F330  = 1.875D0*F330*F330*F330
            DEL1  = 3.0D0*XN*XN*AONV*AONV
            DEL2  = 2.0D0*DEL1*F220*G200*Q22
            DEL3  = 3.0D0*DEL1*F330*G300*Q33*AONV
            DEL1  = DEL1*F311*G310*Q31*AONV
            XLAMO = DMOD(Mo+nodeo+Argpo-THETA,TwoPi)
            XFACT = MDot + XPIDOT - RPTIM + DMDT + DOMDT + DNODT        &
     &               - no_kozai
          ENDIF

! * ---------------- FOR SGP4, INITIALIZE THE INTEGRATOR ----------------
         XLI   = XLAMO
         XNI   = no_kozai
         ATIME = 0.0D0
         XN    = no_kozai + DNDT
      ENDIF ! Ires non-zero

! c        INCLUDE 'debug3.f'

      RETURN
      END  ! end dsinit
