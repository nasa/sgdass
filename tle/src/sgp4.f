! *   -------------------------------------------------------------------
! *
! *                               sgp4unit.f
! *
! *    this file contains the sgp4 procedures for analytical propagation
! *    of a satellite. the code was originally released in the 1980 and 1986
! *    spacetrack papers. a detailed discussion of the theory and history
! *    may be found in the 2006 aiaa paper by vallado, crawford, hujsak,
! *    and kelso.
! *
! *                            companion code for
! *               fundamentals of astrodynamics and applications
! *                                    2007
! *                              by david vallado
! *
! *       (w) 719-573-2600, email dvallado@agi.com
! *
! *    current :
! *              26 Aug 08  david vallado
! *                           fix atime for faster operation in dspace
! *                           add operationmode for afspc (a) or improved (i)
! *                           performance mode
! *    changes :
! *              16 jun 08  david vallado
! *                           update small eccentricity check
! *              16 nov 07  david vallado
! *                           misc fixes for better compliance
! *               2 apr 07  david vallado
! *                           misc fixes for constants
! *              14 aug 06  david vallado
! *                           chg lyddane choice back to strn3, constants,
! *                           separate debug and writes, misc doc
! *              26 jul 05  david vallado
! *                           fixes for paper
! *                           note that each fix is preceded by a
! *                           comment with "sgp4fix" and an explanation of
! *                           what was changed
! *              10 aug 04  david vallado
! *                           2nd printing baseline working
! *              14 may 01  david vallado
! *                           2nd edition baseline
! *                     80  norad
! *                           original baseline
! *
! *     *****************************************************************
! *  Files         :
! *    Unit 14     - sgp4test.dbg    debug output file






! * -----------------------------------------------------------------------------
! *
! *                             SUBROUTINE SGP4
! *
! *  this procedure is the sgp4 prediction model from space command. this is an
! *    updated and combined version of sgp4 and sdp4, which were originally
! *    published separately in spacetrack report #3. this version follows the
! *    methodology from the aiaa paper (2006) describing the history and
! *    development of the code.
! *
! *  author        : david vallado                  719-573-2600   28 jun 2005
! *
! *  inputs        :
! *    satrec	 - initialised structure from sgp4init() call.
! *    tsince	 - time eince epoch (minutes)
! *
! *  outputs       :
! *    r           - position vector                     km
! *    v           - velocity                            km/sec
! *  return code - non-zero on error.
! *                   1 - mean elements, ecc >= 1.0 or ecc < -0.001 or a < 0.95 er
! *                   2 - mean motion less than 0.0
! *                   3 - pert elements, ecc < 0.0  or  ecc > 1.0
! *                   4 - semi-latus rectum < 0.0
! *                   5 - epoch elements are sub-orbital
! *                   6 - satellite has decayed
! *
! *  locals        :
! *    am          -
! *    axnl, aynl        -
! *    betal       -
! *    COSIM   , SINIM   , COSOMM  , SINOMM  , Cnod    , Snod    , Cos2u   ,
! *    Sin2u   , Coseo1  , Sineo1  , Cosi    , Sini    , Cosip   , Sinip   ,
! *    Cosisq  , Cossu   , Sinsu   , Cosu    , Sinu
! *    Delm        -
! *    Delomg      -
! *    Dndt        -
! *    Eccm        -
! *    EMSQ        -
! *    Ecose       -
! *    El2         -
! *    Eo1         -
! *    Eccp        -
! *    Esine       -
! *    Argpm       -
! *    Argpp       -
! *    Omgadf      -
! *    Pl          -
! *    R           -
! *    RTEMSQ      -
! *    Rdotl       -
! *    Rl          -
! *    Rvdot       -
! *    Rvdotl      -
! *    Su          -
! *    T2  , T3   , T4    , Tc
! *    Tem5, Temp , Temp1 , Temp2  , Tempa  , Tempe  , Templ
! *    U   , Ux   , Uy    , Uz     , Vx     , Vy     , Vz
! *    inclm       - inclination
! *    mm          - mean anomaly
! *    nm          - mean motion
! *    nodem       - longi of ascending node
! *    xinc        -
! *    xincp       -
! *    xl          -
! *    xlm         -
! *    mp          -
! *    xmdf        -
! *    xmx         -
! *    xmy         -
! *    nodedf     -
! *    xnode       -
! *    nodep      -
! *    np          -
! *
! *  coupling      :
! *    getgravconst-
! *    dpper
! *    dpspace
! *
! *  references    :
! *    hoots, roehrich, norad spacetrack report #3 1980
! *    hoots, norad spacetrack report #6 1986
! *    hoots, schumacher and glover 2004
! *    vallado, crawford, hujsak, kelso  2006
! * ------------------------------------------------------------------------------

      SUBROUTINE SGP4 ( whichconst, T, r, v, Error )
        IMPLICIT NONE
        INTEGER  Error, whichconst
        REAL*8   T, r(3), v(3)

        INCLUDE 'sgp4.i'

! * -------------------------- Local Variables --------------------------
        REAL*8 AM    , Axnl  , Aynl  , Betal , COSIM , Cnod  ,          &
     &         Cos2u , Coseo1, Cosi  , Cosip , Cosisq, Cossu , Cosu  ,  &
     &         Delm  , Delomg, Eccm  , EMSQ  , Ecose , El2   , Eo1   ,  &
     &         Eccp  , Esine , Argpm , Argpp , Omgadf, Pl    ,          &
     &         Rdotl , Rl    , Rvdot , Rvdotl, SINIM ,                  &
     &         Sin2u , Sineo1, Sini  , Sinip , Sinsu , Sinu  ,          &
     &         Snod  , Su    , T2    , T3    , T4    , Tem5  , Temp  ,  &
     &         Temp1 , Temp2 , Tempa , Tempe , Templ , U     , Ux    ,  &
     &         Uy    , Uz    , Vx    , Vy    , Vz    , Inclm , Mm    ,  &
     &         XN    , nodem , Xinc  , Xincp , Xl    , Xlm   , Mp    ,  &
     &         Xmdf  , Xmx   , Xmy   , Xnoddf, Xnode , nodep ,          &
     &         Tc    , Dndt

        REAL*8 X2O3, J2,J3,XKE,J3OJ2, mr,mv,                            &
     &         mu, RadiusEarthkm, VKmPerSec, temp4, tumin, j4
	INTEGER iter

        COMMON /DebugHelp/ Help
        CHARACTER Help
        INCLUDE 'astmath.i'

! * ------------------------ WGS-72 EARTH CONSTANTS ---------------------
! * ---------------------- SET MATHEMATICAL CONSTANTS -------------------
      X2O3   = 2.0D0/3.0D0

! c     Keep compiler ok for warnings on uninitialized variables
      mr = 0.0D0
      Coseo1 = 1.0D0
      Sineo1 = 0.0D0

      ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke,   &
     &       j2, j3, j4, j3oj2 )
! c     sgp4fix divisor for divide by zero check on inclination
! c     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared it to
! c     1.5D-12, so the threshold was changed to 1.5D-12 for consistency
      temp4    =   1.5D-12
      VKmPerSec     =  RadiusEarthKm * xke/60.0D0

! * ------------------------- CLEAR SGP4 ERROR FLAG ---------------------
      Error = 0

! * ----------- UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG ---------
      XMDF   = Mo + MDot*T
      OMGADF = Argpo + ArgpDot*T
      XNODDF = nodeo + nodeDot*T
      Argpm  = OMGADF
      Mm     = XMDF
      T2     = T*T
      nodem  = XNODDF + XNODCF*T2
      TEMPA  = 1.0D0 - CC1*T
      TEMPE  = BSTAR*CC4*T
      TEMPL  = T2COF*T2
      IF (ISIMP .ne. 1) THEN
          DELOMG = OMGCOF*T
          DELM   = XMCOF*(( 1.0D0+ETA*DCOS(XMDF) )**3-DELMO)
          TEMP   = DELOMG + DELM
          Mm     = XMDF + TEMP
          Argpm  = OMGADF - TEMP
          T3     = T2*T
          T4     = T3*T
          TEMPA  = TEMPA - D2*T2 - D3*T3 - D4*T4
          TEMPE  = TEMPE + BSTAR*CC5*(DSIN(Mm) - SINMAO)
          TEMPL  = TEMPL + T3COF*T3 + T4*(T4COF + T*T5COF)
        ENDIF
      XN    = no_unkozai
      Eccm  = Ecco
      Inclm = Inclo
      IF ( METHOD .EQ. 'd' ) THEN
          TC = T
          CALL DSPACE ( IRez  , D2201 , D2211 , D3210 , D3222 , D4410 ,  &
     &                  D4422 , D5220 , D5232 , D5421 , D5433 , Dedt  ,  &
     &                  Del1  , Del2  , Del3  , Didt  , Dmdt  , Dnodt ,  &
     &                  Domdt , Argpo , ArgpDot, T    , TC    , GSTo ,   &
     &                  Xfact , Xlamo , no_unkozai,                      &
     &                  Atime , Eccm  , Argpm, Inclm , Xli   , Mm  ,     &
     &                  XNi   , nodem, Dndt  , XN  )
        ENDIF

! ---- mean motion less than 0.0
      IF ( XN .LE. 0.0D0 ) THEN
           ERROR = 2
      ENDIF
      AM = (XKE/XN)**X2O3*TEMPA**2
      XN = XKE/AM**1.5D0
      Eccm = Eccm-TEMPE
! c   fix tolerance for error recognition
      IF (Eccm .GE. 1.0D0 .or. Eccm.lt.-0.001D0 .or. AM .lt. 0.95) THEN
! c	  write(6,*) '# Error 1, Eccm = ',  Eccm, ' AM = ', AM
          Error = 1
        ENDIF
! c   sgp4fix change test condition for eccentricity   
      IF (Eccm .lt. 1.0D-6) Eccm = 1.0D-6
      Mm     = Mm+no_unkozai*TEMPL
      XLM    = Mm+Argpm+nodem
      EMSQ   = Eccm*Eccm
      TEMP   = 1.0D0 - EMSQ
      nodem  = DMOD(nodem,TwoPi)
      Argpm  = DMOD(Argpm,TwoPi)
      XLM    = DMOD(XLM,TwoPi)
      Mm     = DMOD(XLM - Argpm - nodem,TwoPi)

! * --------------------- COMPUTE EXTRA MEAN QUANTITIES -----------------
      SINIM  = DSIN(Inclm)
      COSIM  = DCOS(Inclm)

! * ------------------------ ADD LUNAR-SOLAR PERIODICS ------------------
      Eccp   = Eccm
      XINCP  = Inclm
      Argpp  = Argpm
      nodep = nodem
      Mp     = Mm
      SINIP  = SINIM
      COSIP  = COSIM
      IF(METHOD .EQ. 'd') THEN
          CALL DPPER( e3    , ee2   , peo   , pgho  , pho   , pinco ,   &
     &                plo   , se2   , se3   , sgh2  , sgh3  , sgh4  ,   &
     &                sh2   , sh3   , si2   , si3   , sl2   , sl3   ,   &
     &                sl4   , T     , xgh2  , xgh3  , xgh4  , xh2   ,   &
     &                xh3   , xi2   , xi3   , xl2   , xl3   , xl4   ,   &
     &                zmol  , zmos  , Inclo , 'n'   ,                   &
     &                Eccp  , XIncp , nodep, Argpp, Mp, Opsmode )
          IF(XINCP .lt. 0.0D0) THEN
              XINCP  = -XINCP
              nodep  = nodep + PI
              Argpp  = Argpp - PI
            ENDIF
          IF(Eccp .lt. 0.0D0 .OR. Eccp .GT. 1.0D0) THEN
              Error = 3
            ENDIF
        ENDIF

! * ------------------------ LONG PERIOD PERIODICS ----------------------
      IF(METHOD .EQ. 'd') THEN
          SINIP =  DSIN(XINCP)
          COSIP =  DCOS(XINCP)
          AYCOF = -0.5D0*J3OJ2*SINIP
! c         sgp4fix for divide by zero with xincp = 180 deg
          if (dabs(cosip+1.0).gt. 1.5d-12) THEN
              XLCOF  = -0.25D0*J3OJ2*SINIP*                             &
     &                 (3.0D0+5.0D0*COSIP)/(1.0D0+COSIP)
            else
              XLCOF  = -0.25D0*J3OJ2*SINIP*                             &
     &                 (3.0D0+5.0D0*COSIP)/temp4
            ENDIF
        ENDIF
      AXNL = Eccp*DCOS(Argpp)
      TEMP = 1.0D0 / (AM*(1.0D0-Eccp*Eccp))
      AYNL = Eccp*DSIN(Argpp) + TEMP*AYCOF
      XL   = Mp + Argpp + nodep + TEMP*XLCOF*AXNL

! * ------------------------- SOLVE KEPLER'S EQUATION -------------------
      U    = DMOD(XL-nodep,TwoPi)
      EO1  = U
      ITER=0
! c   sgp4fix for kepler iteration
! c   the following iteration needs better limits on corrections
      Temp = 9999.9D0
      DO WHILE ((Temp.ge.1.0D-12).and.(ITER.lt.10))
          ITER=ITER+1
          SINEO1= DSIN(EO1)
          COSEO1= DCOS(EO1)
          TEM5  = 1.0D0 - COSEO1*AXNL - SINEO1*AYNL
          TEM5  = (U - AYNL*COSEO1 + AXNL*SINEO1 - EO1) / TEM5
          Temp  = DABS(Tem5)
          IF(Temp.gt.1.0D0) Tem5=Tem5/Temp ! Stop excessive correction
          EO1   = EO1+TEM5
        ENDDO

! * ----------------- SHORT PERIOD PRELIMINARY QUANTITIES ---------------
      ECOSE = AXNL*COSEO1+AYNL*SINEO1
      ESINE = AXNL*SINEO1-AYNL*COSEO1
      EL2   = AXNL*AXNL+AYNL*AYNL
      PL    = AM*(1.0D0-EL2)
! c     semi-latus rectum < 0.0
      IF ( PL .lt. 0.0D0 ) THEN
          Error = 4
        ELSE
          RL    = AM*(1.0D0-ECOSE)
          RDOTL = DSQRT(AM)*ESINE/RL
          RVDOTL= DSQRT(PL)/RL
          BETAL = DSQRT(1.0D0-EL2)
          TEMP  = ESINE/(1.0D0+BETAL)
          SINU  = AM/RL*(SINEO1-AYNL-AXNL*TEMP)
          COSU  = AM/RL*(COSEO1-AXNL+AYNL*TEMP)
          SU    = DATAN2(SINU,COSU)
          SIN2U = (COSU+COSU)*SINU
          COS2U = 1.0D0-2.0D0*SINU*SINU
          TEMP  = 1.0D0/PL
          TEMP1 = 0.5D0*J2*TEMP
          TEMP2 = TEMP1*TEMP

! * ------------------ UPDATE FOR SHORT PERIOD PERIODICS ----------------
          IF(METHOD .EQ. 'd') THEN
              COSISQ = COSIP*COSIP
              CON41  = 3.0D0*COSISQ - 1.0D0
              X1MTH2 = 1.0D0 - COSISQ
              X7THM1 = 7.0D0*COSISQ - 1.0D0
            ENDIF
          mr   = RL*(1.0D0 - 1.5D0*TEMP2*BETAL*CON41) +                 &
     &           0.5D0*TEMP1*X1MTH2*COS2U
          SU   = SU - 0.25D0*TEMP2*X7THM1*SIN2U
          XNODE= nodep + 1.5D0*TEMP2*COSIP*SIN2U
          XINC = XINCP + 1.5D0*TEMP2*COSIP*SINIP*COS2U
          mv   = RDOTL - XN*TEMP1*X1MTH2*SIN2U / XKE
          RVDOT= RVDOTL + XN*TEMP1* (X1MTH2*COS2U+1.5D0*CON41) / XKE

! * ------------------------- ORIENTATION VECTORS -----------------------
          SINSU=  DSIN(SU)
          COSSU=  DCOS(SU)
          SNOD =  DSIN(XNODE)
          CNOD =  DCOS(XNODE)
          SINI =  DSIN(XINC)
          COSI =  DCOS(XINC)
          XMX  = -SNOD*COSI
          XMY  =  CNOD*COSI
          UX   =  XMX*SINSU + CNOD*COSSU
          UY   =  XMY*SINSU + SNOD*COSSU
          UZ   =  SINI*SINSU
          VX   =  XMX*COSSU - CNOD*SINSU
          VY   =  XMY*COSSU - SNOD*SINSU
          VZ   =  SINI*COSSU

! * ----------------------- POSITION AND VELOCITY -----------------------
          r(1) = mr*UX * RadiusEarthkm
          r(2) = mr*UY * RadiusEarthkm
          r(3) = mr*UZ * RadiusEarthkm
          v(1) = (mv*UX + RVDOT*VX) * VKmPerSec
          v(2) = (mv*UY + RVDOT*VY) * VKmPerSec
          v(3) = (mv*UZ + RVDOT*VZ) * VKmPerSec
        ENDIF

! * --------------------------- ERROR PROCESSING ------------------------
! c     sgp4fix for decaying satellites
      if (mr .lt. 1.0D0) THEN
! c          write(*,*) '# decay condition ',mr
          error = 6
        ENDIF

! c        INCLUDE 'debug7.f'

      RETURN
      END  ! end sgp4

