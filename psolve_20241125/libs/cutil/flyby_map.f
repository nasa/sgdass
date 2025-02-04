      SUBROUTINE FLYBY_MAP()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FLYBY_MAP PROGRAM SPECIFICATION
!
! 1.1 Perform the mapping of theoretical delays and delay rates
!     of the parameter mapping flyby system.
!
! 1.2 REFERENCES:
!
! 2.  FLYBY_MAP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
      INCLUDE 'heo.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!     CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      REAL*8 T
      REAL*8 DELPSI, DELEPS, FCT, CFCT, TC2000, DXWOB, DYWOB, LG
      PARAMETER  ( LG = 6.969290134D-10 ) ! Value from IERS 2001
      REAL*8 DUTD, DXWOBD, DYWOBD
!
      CHARACTER  STR*32
      REAL*8 UT1_RATE_CALC,  XP_RATE_CALC,  YP_RATE_CALC
      REAL*8 UT1_RATE_FLYBY, XP_RATE_FLYBY, YP_RATE_FLYBY
      REAL*8 CNVRS_UT        
      PARAMETER  ( CNVRS_UT = 1.002737909D0 * 12.0D0 * 60.0D0 * 60.0D0/PI__NUM )
!
      INTEGER*2 I2, I1, J1, J2, J3, J4, J5, J6, I, MODE2
      INTEGER*2 LOWER,UPPER
      REAL*8    FA(5), FAD(5), ARG, T_SEC, VEC_HEO(3), VEC_HEO_RATE(3), &
     &          UT1_M_TDB, TIM_ARG, DER_EROT(3), DER_EROT_RATE(3), &
     &          FA2K(14), FAD2K(14), ADD_DEL, DT_STA_1, DT_STA_2
      INTEGER*4 NODE, IUER
!
      INTEGER*4, EXTERNAL :: ILEN, IXMN8, IXMN8_S
      REAL*8,    EXTERNAL :: FLIN8, FSPL8, DSPL8
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   DG    91JUL01     Changed UT1PM mod file mapping
!   DG    91AUG15     Changed subroutine utpmint to ut1pm_int for
!                     clarity. Added a 'mode' flag to ut1pm_int to
!                     tell how to interpolate. If mode = 1, use linear
!                     interpolation; if mode = 3, use cubic
!                     interpolation.
!   DG    91AUG27     Changed ut1pm_int to compute rates (derivatives),
!                     changed call to ut1pm_int.
!   :93.12.15:jwr:    Very substantial code changes to support cubic spline
!                     interpolation.
!   :94.08.01:jwr:    Bug involved with missing rate information in eop
!                     fixed.
!   :97.04.29:pet:    Added stuff for interpolation high frequency EOP
!                     by cubic spline.
!   :97.08.29:pet:    Change a bit logic to fix "UT1S-rate-AUG97" bug
!   :99.01.08:pet:    Moved all old variables which previousy were at the
!                     right part of the operator save to the common block
!                     flyby_common defined in ../include/flyby.i in order
!                     to make FLYBY_MAP reentrable.
!  1999.05.05:pet:    Fixed a bug: TC2000 was not initialized in some modes.
!  1999.10.15 pet     Added support of mapping new eccentricity
!  1999.10.18 pet     Bug in support of new eccentricity is corrected
!  1999.11.11 pet     Corrected a code related with fixing "UT1S-rate-AUG97" bug
!                     which is not relevant any more with Cacl 9.1 databases
!  1999.11.15 pet     Corrected a bug in the patch of code for compensating
!                     "UT1S-rate-AUG97" : the previous version didn't apply the
!                     compensation when both Cacl 8.2 and SOLVE used UT1S-UT1
!                     interpolation scheme.
!  2000.01.31 pet     Added NUTF96 instead of NUTFA call for Calc 9 databases
!  2000.03.19 pet     Added support of value 'N' of UT1_RS_FLYBY (no zonal tide
!                     interpolation magic for mapping EOP)
!  2000.06.13 pet     Removed support of nuation models
!  2000.09.25 pet     Added support of mean gradient mapping
!  2001.01.12 pet     Added support of metric tensor mapping. Implemented
!                     mapping monumnets name taken from the eccentricty file
!  2002.12.27 pet     Added support of position variation mapping
!  2003.09.02 pet     Added support for harmoinc EOP in heo format.
!  2004.03.11 pet     Fixed an error in sign for harmonic EOP.
!  2005.12.28 DG      Modified to support Calc 10 UT1 mapping both true
!                     UT1 case (normal Calc 10 mode - no smoothing) or
!                     optional UT1 smoothing using subroutine UT1S2K. 
!  2006.01.29 pet     Added support for mapping of the Earth rotation model
!  2006.02.08 pet     Added support of FL_NOFLYBY variable
!  2006.05.04 pet     Added code for support of default formal uncertainties
!                     of input EOP series
!  2008.04.28 pet     Added code for computation of antenna reference point
!                     variations due to thernal expansion
!
! 5.  FLYBY_MAP PROGRAM STRUCTURE
!
      IF ( FL_NOFLYBY ) RETURN ! Nothing to do!
      I1=ISITE(1)
      I2=ISITE(2)
!
! --- Apply pressure loading calibration if requested
!
      IF ( KPLODCAL ) THEN
           CALL DO_PLOD()
      ENDIF
!
      IF ( KSTAM ) THEN
!
! -------- Do the mapping of DT and RT of station subsitutions.
!
           DT = DT + ( &
     &                 BP(1,1,1)*SITDIF(1,I1) + &
     &                 BP(2,1,1)*SITDIF(2,I1) + &
     &                 BP(3,1,1)*SITDIF(3,I1) + &
     &                 BP(1,2,1)*SITDIF(1,I2) + &
     &                 BP(2,2,1)*SITDIF(2,I2) + &
     &                 BP(3,2,1)*SITDIF(3,I2)   )*1.D6
!
           RT = RT + &
     &             BP(1,1,2)*SITDIF(1,I1) + &
     &             BP(2,1,2)*SITDIF(2,I1) + &
     &             BP(3,1,2)*SITDIF(3,I1) + &
     &             BP(1,2,2)*SITDIF(1,I2) + &
     &             BP(2,2,2)*SITDIF(2,I2) + &
     &             BP(3,2,2)*SITDIF(3,I2)
      END IF
!
      IF ( KECC ) THEN
!
! -------- Do the mapping of DT and RT of station eccentricity
!
           DT = DT + &
     &               (   BP(1,1,1)*( ECC_NEW(1,I1) - ECC_OLD(1,I1))  &
     &                 + BP(2,1,1)*( ECC_NEW(2,I1) - ECC_OLD(2,I1))  &
     &                 + BP(3,1,1)*( ECC_NEW(3,I1) - ECC_OLD(3,I1) ) )*1.D6 &
     &              +(   BP(1,2,1)*( ECC_NEW(1,I2) - ECC_OLD(1,I2))  &
     &                 + BP(2,2,1)*( ECC_NEW(2,I2) - ECC_OLD(2,I2))  &
     &                 + BP(3,2,1)*( ECC_NEW(3,I2) - ECC_OLD(3,I2) ) )*1.D6
!
           RT = RT + &
     &               (   BP(1,1,2)*( ECC_NEW(1,I1) - ECC_OLD(1,I1))  &
     &                 + BP(2,1,2)*( ECC_NEW(2,I1) - ECC_OLD(2,I1))  &
     &                 + BP(3,1,2)*( ECC_NEW(3,I1) - ECC_OLD(3,I1) ) ) + &
     &               (   BP(1,2,2)*( ECC_NEW(1,I2) - ECC_OLD(1,I2))  &
     &                 + BP(2,2,2)*( ECC_NEW(2,I2) - ECC_OLD(2,I2))  &
     &                 + BP(3,2,2)*( ECC_NEW(3,I2) - ECC_OLD(3,I2) ) )
!
! -------- Replace monuments names by values from the eccentricity mapping file
!
           DO 410 J1=1,NUMSTA
              MONUMENTS_CHR(J1) = MONU_NAME_NEW(J1)
 410       CONTINUE
      END IF
!
      IF ( KSOUC ) THEN
!
! -------- Do the mapping of DT and RT of sources subsitutions.
!
           DT = DT+ ( SP(1,1)*STRDIF(1,ISTAR) + SP(2,1)*STRDIF(2,ISTAR) )*1.D6
!
           RT = RT+ ( SP(1,2)*STRDIF(1,ISTAR) + SP(2,2)*STRDIF(2,ISTAR) )
      END IF
!
      IF ( KAXOP ) THEN
!
! -------- Do the mapping of DT and RT of axis ofsset subsitutions.
!
           DT = DT+ ( AXOFP(1,1)*AXDIF(I1) + AXOFP(2,1)*AXDIF(I2) )*1.D6
!
           RT = RT+ ( AXOFP(1,2)*AXDIF(I1) + AXOFP(2,2)*AXDIF(I2) )
      ENDIF
!
      IF ( KNUTS ) THEN
           CALL FERR ( INT2(613), &
     &         "FLYBY_MAP: bad news: nutation model mapping "// &
     &         " is not supported after evening 2000.06.13", INT2(0), INT2(0) )
      END IF
!
! --- Do the mapping of DT and RT of the nutation daily value offsets.
!
      IF ( KNUTD ) THEN
           DELPSI=0.0D0
           DELEPS=0.0D0
           T = FJD+FRACTC
           LOWER = 0
           DO I=1,5
              IF (T.GE.DNUT(I)) LOWER=I
           ENDDO
!
           IF ( LOWER .LT. 1  .OR.  LOWER .GT. 4 ) THEN
                CALL FERR ( INT2(123), "FLYBY_MAP: Outside nutation "// &
     &              "interp range", INT2(0), INT2(0) )
           ENDIF
           UPPER  = LOWER+1
           FCT    = ( T - DNUT(LOWER) ) /( DNUT(UPPER) - DNUT(LOWER) )
           CFCT   = 1. - FCT
           DELPSI = DDPSI(LOWER)*CFCT + DDPSI(UPPER)*FCT
           DELEPS = DDEPS(LOWER)*CFCT + DDEPS(UPPER)*FCT
!
           DT  = DT + ( NUTP(1,1)*DELPSI+   NUTP(2,1)*DELEPS )*1.D6
!
           RT  = RT + NUTP(1,2)*DELPSI+ NUTP(2,2)*DELEPS
      END IF
!
! --- Do the mapping of DT and RT of the UT1/PM substitution.
!
!
! --- Compute time of observation
!
      T = FJD + FRACTC
      TC2000 = (T - 2451545.0D0) / 36525.D0
!
      IF ( KEROT ) THEN
!
! ------ Turn on rate computation
!
         MODE2=1
!
! ------ Interpolate UT1 and X/Y wobble from the data base.
! ------ Mod 91AUG15 -DG-. Pass flags 'interpolation_UT1' and
! ------ 'interpolation_pm' so we interpolate the same way as CALC 7.4
! ------ (and later versions). If =1, linear interpolation; if =3,
! ------ cubic interpolation.
!
         CALL UT1PM_INT ( T, UT1INB, UT1PTB, UT1B, UT1_RATE_CALC, &
     &        INTERPOLATION_UT1, MODE2, UC_YP1, UC_YPN, UC_Y2, &
     &        UC_XTABLE, UC_INITIALIZED )
!
         CALL UT1PM_INT ( T, WOBINB, WOBXXB, WOBXB, XP_RATE_CALC, &
     &        INTERPOLATION_PM, MODE2, XC_YP1, XC_YPN, XC_Y2, &
     &        XC_XTABLE, XC_INITIALIZED )
!
         CALL UT1PM_INT ( T, WOBINB, WOBYYB, WOBYB, YP_RATE_CALC, &
     &        INTERPOLATION_PM, MODE2, YC_YP1, YC_YPN, YC_Y2, &
     &        YC_XTABLE, YC_INITIALIZED )
!
! ------ Interpolate UT1 and X/Y wobble from the mod file.
! ------ Mod 91AUG15 -DG-. Check flag 'flyby_interp' to see how the
! ------ user wants to interpolate in the mod file. If =1, linear
! ------ interpolation; if =3, cubic interpolation. For defaults see
! ------ subroutine serot.f.
!
         CALL UT1PM_INT ( T, UT1INV, UT1PTV, UT1V, UT1_RATE_FLYBY, &
     &        FLYBY_INTERP, MODE2, UF_YP1, UF_YPN, UF_Y2, &
     &        UF_XTABLE, UF_INITIALIZED )
!
         CALL UT1PM_INT ( T, WOBINV, WOBXXV, WOBXV, XP_RATE_FLYBY, &
     &        FLYBY_INTERP, MODE2, XF_YP1, XF_YPN, XF_Y2, &
     &        XF_XTABLE, XF_INITIALIZED )
!
         CALL UT1PM_INT ( T, WOBINV, WOBYYV, WOBYV, YP_RATE_FLYBY, &
     &        FLYBY_INTERP, MODE2, YF_YP1, YF_YPN, YF_Y2, YF_XTABLE, &
     &        YF_INITIALIZED )
!
! ------ Convert ut1 rate from sec/day to sec/sec and
! ------ X- and Y- pole rates from milliarcsec/day to radians/sec
!
         UT1_RATE_CALC = UT1_RATE_CALC /86400.D0
         UT1_RATE_FLYBY= UT1_RATE_FLYBY/86400.D0
!
         XP_RATE_CALC  = XP_RATE_CALC /(206264806.2d0*86400.D0)
         XP_RATE_FLYBY = XP_RATE_FLYBY/(206264806.2d0*86400.D0)
!
         YP_RATE_CALC  = YP_RATE_CALC /(206264806.2d0*86400.D0)
         YP_RATE_FLYBY = YP_RATE_FLYBY/(206264806.2d0*86400.D0)
!
! ------ If calc and flyby are not using the same algorithm to remove
! ------ fortnightly terms, then they must be restored before the ut1
! ------ difference can be formed.
! ------ Subroutine UT1ZT or UT1S_82 or UT1S2K returns DOMEGA in units 
! ------ of radians/sec and
! ------ the units must be converted to sec/sec before DOMEGA can be
! ------ added to the interpolated rates.
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) ' '         ! %%%%
!  write ( 6, * ) ' ut1_rs = ', ut1_rs, ' ut1_rs_flyby = ', ut1_rs_flyby ! %%%%%
!  write ( 6, * ) ' '         ! %%%%
!  call pause ( 'flyby_map' ) ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         IF ( UT1_RS .NE. 'R' .AND. UT1_RS .NE. 'S' .AND. &
     &        UT1_RS .NE. 'N' ) THEN
              WRITE ( 6, * ) 'FLYBY_MAP: ut1_rs undefined. Must be R or S or N.'
              CALL EXIT ( 1 ) 
         END IF
!
         IF ( UT1_RS_FLYBY .NE. 'R'  .AND.  UT1_RS_FLYBY .NE. 'S'  .AND. &
     &        UT1_RS_FLYBY .NE. 'N'                                      ) THEN
!
              WRITE ( 6, * ) 'FLYBY_MAP: ut1_rs_fly undefined. Must be R or S or N.'
              CALL EXIT ( 1 ) 
         END IF
!
         IF ( UT1_RS .NE. UT1_RS_FLYBY ) THEN ! This is the hard case
!
! ----------- Do the calc job first
!
              IF ( UT1_RS .EQ. 'R' ) THEN
                   CALL UT1ZT ( TC2000, DUT, DLOD, DOMEGA )
                   UT1B = UT1B - DUT
                   DOMEGA = DOMEGA*(86400.D0/(2.D0*PI__NUM))
                   UT1_RATE_CALC = UT1_RATE_CALC - DOMEGA
                ELSE IF ( UT1_RS .EQ. 'S' ) THEN
                   IF ( CALCV .LE. 8.200001 ) THEN
!
! --------------------- Special trick to fix bug "UT1S-rate-AUG97" in Calc 8.2
! --------------------- and younger
!
                        CALL NUTFA   ( T, 0.D0, TC2000, FA, FAD )
                        CALL UT1S_82 ( FA, DUT, DLOD, DOMEGA )
                     ELSE IF ( CALCV .GE. 9.0 .AND. CALCV .LT. 9.9 ) THEN
                        CALL NUTF96  ( T, 0.D0, TC2000, FA, FAD )
                        CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
                     ELSE IF ( CALCV .GT. 9.99 .AND. CALCV .LT. 99.99 ) THEN
                        CALL NUTFA10 (T, 0.D0, TC2000, FA2K, FAD2K)
                        CALL UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
                   END IF
                   UT1B = UT1B - DUT
                   DOMEGA = DOMEGA*(86400.D0/(2.D0*PI__NUM))
                   UT1_RATE_CALC = UT1_RATE_CALC - DOMEGA
                ELSE IF ( UT1_RS .EQ. 'N' ) THEN
                   DUT = 0.D0
                   DLOD = 0.D0
                   DOMEGA = 0.D0
              ENDIF
!
! ----------- Do the flyby job second
!
              IF ( UT1_RS_FLYBY .EQ. 'R' ) THEN
                   CALL UT1ZT ( TC2000, DUT, DLOD, DOMEGA )
                   UT1V = UT1V - DUT
                   DOMEGA = DOMEGA*(86400.D0/(2.D0*PI__NUM))
                   UT1_RATE_FLYBY = UT1_RATE_FLYBY - DOMEGA
                ELSE IF ( UT1_RS_FLYBY .EQ. 'S' ) THEN
                   IF ( CALCV .LE. 8.200001 ) THEN
                        CALL NUTFA  ( T, 0.D0, TC2000, FA, FAD )
                        CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
                     ELSE IF ( CALCV .GE. 9.0 .AND. CALCV .LT. 9.9 ) THEN
                       CALL NUTF96 ( T, 0.D0, TC2000, FA, FAD )
                       CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
                     ELSE IF ( CALCV .GT. 9.99 ) THEN
                        CALL NUTFA10 (T, 0.D0, TC2000, FA2K, FAD2K)
                        CALL UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
                   END IF
                   UT1V = UT1V - DUT
                   DOMEGA = DOMEGA*(86400.D0/(2.D0*PI__NUM))
                   UT1_RATE_FLYBY = UT1_RATE_FLYBY - DOMEGA
                ELSE IF ( UT1_RS_FLYBY .EQ. 'N' ) THEN
                   CONTINUE
              ENDIF
          ELSE IF ( UT1_RS .EQ. 'S'  .AND.  UT1_RS_FLYBY .EQ. 'S'  .AND. &
     &              CALCV .LE. 8.2 ) THEN !
!
! ----------- special trick for compensation of bug "UT1S-rate-AUG97"
! ----------- in Calc 8.2 and younger
!
              IF ( CALCV .LE. 8.200001 ) THEN
                   CALL NUTFA  ( T, 0.D0, TC2000, FA, FAD )
                 ELSE IF ( CALCV .GE. 9.0 ) THEN
                   CALL NUTF96 ( T, 0.D0, TC2000, FA, FAD )
              END IF
!
! ----------- Compute UT1S-UT1 using old wrong subroutine (but the same as it
! ----------- was used in Calc 8.2)
!
              CALL UT1S_82 ( FA, DUT, DLOD, DOMEGA )
              UT1B = UT1B - DUT
              DOMEGA = DOMEGA*(86400.D0/(2.D0*PI__NUM))
              UT1_RATE_CALC = UT1_RATE_CALC - DOMEGA
!
! ----------- Compute  UT1S-UT1 using up-to-date (15-NOV-99) routine
!
              CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
              UT1V = UT1V - DUT
              DOMEGA = DOMEGA*(86400.D0/(2.D0*PI__NUM))
              UT1_RATE_FLYBY = UT1_RATE_FLYBY - DOMEGA
          ELSE IF ( UT1_RS .EQ. 'N' .AND. UT1_RS_FLYBY .EQ. 'N' ) THEN
!
! ----------- Nothing to do
!
              CONTINUE
        ENDIF ! ut1_rs = ut1_rs_flyby
!
! ----- Difference UT1 and X/Y wobble and convert X/Y wobble from
! ----- milliarcsec to radians.
!
        DELUT1 = UT1V - UT1B
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@ write ( 6, * ) ' ut1v =', ut1v, ' ut1b = ', ut1b           ! %%%%
!@ write ( 6, * ) ' ut1ptv(1) = ', ut1ptv(1), ' ut1v =', ut1v ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        DELX = (WOBXV - WOBXB)/206264806.2D0
        DELY = (WOBYV - WOBYB)/206264806.2D0
!
        DUDOT = UT1_RATE_FLYBY - UT1_RATE_CALC
        DXDOT =  XP_RATE_FLYBY -  XP_RATE_CALC
        DYDOT =  YP_RATE_FLYBY -  YP_RATE_CALC
!
! ----- Do the mapping.
!
        DT = DT + (ROTP(1,1)*DELX + ROTP(2,1)*DELY + ROTP(3,1)*DELUT1)*1.D6
!
! ----- Map the delay rate. Note that 2nd line handles the effect of a change
! ----- in the ut1 offset and the 3rd line handles the effect of change in the
! ----- ut1 rate. We use the useful property the the derivative of the
! ----- delay rate with respect to the ut1 rate is identical to the
! ----- derivative of the delay with respect to ut1.  The same property is
! ----- true for x- and y-pole.
!
        RT = RT + ( ROTP(1,2)*DELX  + ROTP(2,2)*DELY  + ROTP(3,2)*DELUT1 ) + &
     &              ROTP(1,1)*DXDOT + ROTP(2,1)*DYDOT + ROTP(3,1)*DUDOT
        ELSE 
          IF ( ILEN(VTD_CONF_GLB) > 0  .OR.  ILEN(VTD_CONF_SES) > 0 ) THEN
!
! ------------ Store default values of the apriori EOP series
!
               DO 420 J2=1,MAX_FLYBY_EOP_VALUES
                  IF ( EOPCONS(1) > 1.D-30 ) THEN
                       WBXSIG(J2) = 0.D0
                     ELSE 
                       WBXSIG(J2) = DEF__WOBBLE_SIGMA_MAS
                  END IF
                  IF ( EOPCONS(2) > 1.D-30 ) THEN
                       WBYSIG(J2) = 0.D0
                     ELSE 
                       WBYSIG(J2) = DEF__WOBBLE_SIGMA_MAS
                  END IF
                  IF ( EOPCONS(3) > 1.D-30 ) THEN
                       UT1SIG(J2) = 0.D0
                     ELSE 
                       UT1SIG(J2) = DEF__UT1_SIGMA_MSEC
                  END IF
 420           CONTINUE 
          END IF
      ENDIF ! kerot
!
! --- Apply high-frequency EOP correction if called for
!
! --- JMgipson removed code to convert to julian centuries
! --- and call to compute in case of estimation
!
      IF ( KHFEOP.EQ.1  .OR.  KHFEOP.EQ.3  .OR.  STAT_HEO .EQ. HEO__READ ) THEN
         T = FJD + FRACTC
         IF ( STATUS_HFE .NE. HFE__DONE ) THEN
!
! --------- Without interpolation
!
            IF ( KHFEOP.EQ.1  .OR.  KHFEOP.EQ.3 ) THEN
!
! -------------- Get harmonic Earth orientation variations using old-fashioned
! -------------- Doodson arguments
!
                 CALL GET_HF_EOP ( T, DUT, DXWOB, DYWOB, DUTD, DXWOBD, DYWOBD, &
     &                             INT2(1) )
!
! -------------- Units conversion
!
                 DUT = DUT/1000.d0
                 DXWOB = DXWOB/206264806.2d0
                 DYWOB = DYWOB/206264806.2d0
                 DUTD = DUTD/1000.d0
                 DXWOBD = DXWOBD/206264806.2d0
                 DYWOBD = DYWOBD/206264806.2d0
               ELSE IF ( STAT_HEO .EQ. HEO__READ ) THEN
!
! -------------- Get harmonic Earth Orientation variations using modern (2004)
! -------------- approach -- via HEO file
!
                 T_SEC = (T - J2000__JD)*86400.0D0
                 UT1_M_TDB = UT1_M_TAI - 32.184D0
                 IUER = -1
                 CALL GET_HEO ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, &
     &                          %VAL(ADR_HEO), VEC_HEO, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8311, -1, 'FLYBY_MAP', 'Error in '// &
     &                    'an attempt to compute harmonic variations '// &
     &                    'in the Earth Orinetation' )
                      CALL EXIT ( 1 )
                 END IF
!
                 DXWOB  =  VEC_HEO(2)
                 DYWOB  =  VEC_HEO(1)
                 DUT    = -VEC_HEO(3)/(1000.0D0*MSEC__TO__RAD)/1.002737D0
!
! -------------- Get the rate of changes of harmonic Earth Orientation 
! -------------- variations 
!
                 IUER = -1
                 CALL GET_HEO_RATE ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, &
     &                               %VAL(ADR_HEO), VEC_HEO_RATE, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8312, -1, 'FLYBY_MAP', 'Error in '// &
     &                    'an attempt to compute the rate of changes of '// &
     &                    'harmonic variations in the Earth Orinetation' )
                      CALL EXIT ( 1 )
                 END IF
!
                 DXWOBD =  VEC_HEO_RATE(2)
                 DYWOBD =  VEC_HEO_RATE(1)
                 DUTD   = -VEC_HEO_RATE(3)/(1000.0D0*MSEC__TO__RAD)/1.002737D0
            END IF ! khfeop/stat_heo
          ELSE IF ( STATUS_HFE .EQ. HFE__DONE ) THEN
!
! --------- With interpolation
!
            IXMN_HFE = IXMN8_S ( IXMN_HFE, NUM_HFE, EPOCH_HFE, T )
            DUT    = FSPL8 ( T, NUM_HFE, EPOCH_HFE, UT_HFE, IXMN_HFE, UT_SPL )
            DUTD   = DSPL8 ( T, NUM_HFE, EPOCH_HFE, UT_HFE, IXMN_HFE, UT_SPL )/ &
     &                       86400.D0
            DXWOB  = FSPL8 ( T, NUM_HFE, EPOCH_HFE, XP_HFE, IXMN_HFE, XP_SPL )
            DXWOBD = DSPL8 ( T, NUM_HFE, EPOCH_HFE, XP_HFE, IXMN_HFE, XP_SPL )/ &
     &                       86400.D0
            DYWOB  = FSPL8 ( T, NUM_HFE, EPOCH_HFE, YP_HFE, IXMN_HFE, YP_SPL )
            DYWOBD = DSPL8 ( T, NUM_HFE, EPOCH_HFE, YP_HFE, IXMN_HFE, YP_SPL )/ &
     &                       86400.D0
        ENDIF
!
        DT = DT + ( ROTP(1,1)*DXWOB  + ROTP(2,1)*DYWOB  - ROTP(3,1)*DUT  ) *1.D6
        RT = RT + ( ROTP(1,2)*DXWOB  + ROTP(2,2)*DYWOB  - ROTP(3,2)*DUT  ) + &
     &            ( ROTP(1,1)*DXWOBD + ROTP(2,1)*DYWOBD - ROTP(3,1)*DUTD )
      ENDIF
!
      IF ( KMGR ) THEN
!
! -------- Apply mean troposphere gradients
!
           DT = DT + ( - AGRAD_PART(1,1,1)*MGR_NORTH(I1) &
     &                 - AGRAD_PART(1,2,1)*MGR_EAST(I1)  &
     &                 + AGRAD_PART(2,1,1)*MGR_NORTH(I2) &
     &                 + AGRAD_PART(2,2,1)*MGR_EAST(I2)  )/ VLIGHT * 1.D6
!
           RT = RT + ( - AGRAD_PART(1,1,2)*MGR_NORTH(I1) &
     &                 - AGRAD_PART(1,2,2)*MGR_EAST(I1)  &
     &                 + AGRAD_PART(2,1,2)*MGR_NORTH(I2) &
     &                 + AGRAD_PART(2,2,2)*MGR_EAST(I2)  )/ VLIGHT
      END IF
!
      IF ( KMET ) THEN
           IF ( CALCV .GT. 9.19001 ) THEN
                WRITE ( UNIT=STR(1:5), FMT='(F5.2)' ) CALCV
                CALL FERR ( INT2(784), &
     &              'FLYBY_MAP: Attempt to use METRIC_TENSOR '// &
     &              'mapping for Calc version '//STR(1:5)//'  -- flyby_map '// &
     &              'does not which metric tensor Calc '//STR(1:5)// &
     &              ' uses by default. Please update '// &
     &              '$PSOLVE_ROOT/libs/cutil/flyby_map.f Subroutine!', INT2(0), &
     &               INT2(0) )
                WRITE ( 6, * ) 'Abnormal termination'
                CALL EXIT ( 1 ) 
           END IF
!
! -------- Change theoretical delay and delay rate for the specified metric.
! -------- Remind that Calc 9.12 and earlier used the following expression
! -------- for delay: tau = 1/c * (1 - GM_sun/(D_sun*c**2) ) b*S + ...
!
           IF ( METRIC_NEW .EQ. IERS92__MET    ) THEN
!
! ------------- Baseline vector is in a peculiar IERS92-VLBI metric
!
                CONTINUE
             ELSE IF ( METRIC_NEW .EQ. GRS__MET      ) THEN
!
! ------------- Baseline vector is in geocentric metric
!
                DT = (1.D0 - LG)*DT
                RT = (1.D0 - LG)*RT
             ELSE IF ( METRIC_NEW .EQ. TOPOCNTR__MET ) THEN
!
! ------------- Baseline vector is in topocentric metric
!
                DT = (1.D0 - 2.D0*LG)*DT
                RT = (1.D0 - 2.D0*LG)*RT
             ELSE IF ( METRIC_NEW .EQ. NONE__MET     ) THEN
                CONTINUE
             ELSE
                CALL FERR ( INT2(787), &
     &              'FLYBY_MAP: Wrong value of METRIC_MAP: '//METRIC_NEW, &
     &               INT2(0), INT2(0) )
               STOP 'Abnormal termination'
           END IF
      END IF
!
      IF ( FL_PSV_LIN  .OR. FL_PSV_SPL ) THEN
!
! -------- Apply position variations model
!
           IF ( SCA_LAST(I1) .NE. NSCA  .OR.  SCA_LAST(I2) .NE. NSCA ) THEN
!
! ------------- Displacement for at least one station has not yet been
! ------------- computed for this epoch
!
! ------------- Get the argument (time in sec from FJDCT_BEG_PSV)
! ------------- and the node for interpolation
!
                ARG = (FJD + FRACTC - FJDCT_BEG_PSV)*86400.0
                NODE = IXMN8 ( M__PSV, TIM_PSV, ARG )
                IF ( NODE .LT. 1 ) THEN
                     WRITE ( 6, * ) 'NODE=',NODE
                     WRITE ( 6, * ) 'FJD = ',FJD,' FRACTC = ', FRACTC, &
     &                              ' FJDCT_BEG_PSV = ',FJDCT_BEG_PSV
                     WRITE ( 6, * ) 'ARG=',ARG
                     WRITE ( 6, * ) 'TIM_PSV=',TIM_PSV
                     CALL FERR ( INT2(788), &
     &                   'FLYBY_MAP: Trap of internal control: '// &
     &                   'node beyond interpolation range', INT2(0), INT2(0) )
                     WRITE ( 6, * ) 'Abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
!
! ------------- Now let's compute displacement of the station for which it
! ------------- has not been yet computed
!
                DO 430 J3=1,2
                   IF ( SCA_LAST(ISITE(J3)) .NE. NSCA ) THEN
                      CALL NOUT_R8 ( 3, XYZ_PSV(1,ISITE(J3)) )
                      DO 440 J4=1,3
                         IF ( FL_PSV_LIN ) THEN
!
! -------------------------- Linear interpolation
!
                             XYZ_PSV(J4,ISITE(J3)) = XYZ_PSV(J4,ISITE(J3)) + &
     &                               FLIN8 ( ARG, M__PSV, TIM_PSV, &
     &                                       VALLIN_PSV(1,J4,ISITE(J3)), NODE )
                         END IF
!
                         IF ( FL_PSV_SPL ) THEN
!
! -------------------------- Spline interpolation
!
                             XYZ_PSV(J4,ISITE(J3)) = XYZ_PSV(J4,ISITE(J3)) + &
     &                               FSPL8 ( ARG, M__PSV, TIM_PSV, &
     &                                       VALSPL_PSV(1,J4,ISITE(J3)), NODE, &
     &                                       COESPL_PSV(1,J4,ISITE(J3)) )
                         END IF
 440                  CONTINUE
                   END IF
 430            CONTINUE
           END IF ! of computation of site displacement
!
! -------- Now do mapping
!
           DT = DT + &
     &               ( BP(1,1,1)*XYZ_PSV(1,I1) + &
     &                 BP(2,1,1)*XYZ_PSV(2,I1) + &
     &                 BP(3,1,1)*XYZ_PSV(3,I1) + &
     &                 BP(1,2,1)*XYZ_PSV(1,I2) + &
     &                 BP(2,2,1)*XYZ_PSV(2,I2) + &
     &                 BP(3,2,1)*XYZ_PSV(3,I2)   ) * 1.D6
!
           RT = RT + &
     &               ( BP(1,1,2)*XYZ_PSV(1,I1) + &
     &                 BP(2,1,2)*XYZ_PSV(2,I1) + &
     &                 BP(3,1,2)*XYZ_PSV(3,I1) + &
     &                 BP(1,2,2)*XYZ_PSV(1,I2) + &
     &                 BP(2,2,2)*XYZ_PSV(2,I2) + &
     &                 BP(3,2,2)*XYZ_PSV(3,I2)   )
      END IF
!
      IF ( STAUSE_BSP(I1) ) THEN
!
! -------- Apply apriori expansion with the B-spline basis for displacements of
! -------- the first station of the baseline
!
           IF ( SCA_LAST(I1) .NE. NSCA  ) THEN
                ARG = ((FJD - FJDCT_BEG_BSP) + FRACTC) - 32.184D0/86400.0D0
                NODE = IXMN8 ( M__BSP_INT, TIM_BSP, ARG )
                IF ( NODE .LT. 1 ) THEN
                     WRITE ( 6, * ) ' ARG=',ARG, ' NODE=',NODE
                     CALL ERR_LOG ( 8313, -1, 'FLYBY_MAP', 'Trap of '// &
     &                   'internal control' )
                     WRITE ( 6, * ) 'Abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
                DO 450 J5=1,3
                   XYZ_BSP(J5,I1) = FSPL8 ( ARG, M__BSP_INT, TIM_BSP, &
     &                                      VALSPL_BSP(1,J5,I1), NODE, &
     &                                      COESPL_BSP(1,J5,I1) )
 450            CONTINUE 
!% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      WRITE ( 6, * ) 'BSPL_HRAS: ', &                                       ! %
!     &               ( (FJD - J2000__JD) + FRACTC )*86400.0D0 - 32.184D0, & ! %
!     &               ' VAL_Z: ', XYZ_BSP(3,I1)  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           END IF
!
! -------- Now do mapping
!
           DT = DT + &
     &               ( BP(1,1,1)*XYZ_BSP(1,I1) + &
     &                 BP(2,1,1)*XYZ_BSP(2,I1) + &
     &                 BP(3,1,1)*XYZ_BSP(3,I1)   ) * 1.D6
           RT = RT + &
     &               ( BP(1,1,2)*XYZ_BSP(1,I1) + &
     &                 BP(2,1,2)*XYZ_BSP(2,I1) + &
     &                 BP(3,1,2)*XYZ_BSP(3,I1)   )
      END IF
!
      IF ( STAUSE_BSP(I2) ) THEN
!
! -------- Apply apriori expansion with the B-spline basis for displacements of
! -------- the first station of the baseline
!
           IF ( SCA_LAST(I2) .NE. NSCA  ) THEN
                ARG = (FJD + FRACTC - FJDCT_BEG_BSP)
                NODE = IXMN8 ( M__BSP_INT, TIM_BSP, ARG )
                IF ( NODE .LT. 1 ) THEN
                     WRITE ( 6, * ) ' ARG=',ARG, ' NODE=',NODE
                     CALL ERR_LOG ( 8314, -1, 'FLYBY_MAP', 'Trap of '// &
     &                   'internal control' )
                     WRITE ( 6, * ) 'Abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
                DO 460 J6=1,3
                   XYZ_BSP(J6,I2) = FSPL8 ( ARG, M__BSP, TIM_BSP, &
     &                                      VALSPL_BSP(1,J6,I2), NODE, &
     &                                      COESPL_BSP(1,J6,I2) )
 460            CONTINUE 
           END IF
!
! -------- Now do mapping
!
           DT = DT + &
     &               ( BP(1,2,1)*XYZ_BSP(1,I2) + &
     &                 BP(2,2,1)*XYZ_BSP(2,I2) + &
     &                 BP(3,2,1)*XYZ_BSP(3,I2)   ) * 1.D6
           RT = RT + &
     &               ( BP(1,2,2)*XYZ_BSP(1,I2) + &
     &                 BP(2,2,2)*XYZ_BSP(2,I2) + &
     &                 BP(3,2,2)*XYZ_BSP(3,I2)   )
      END IF
!
      IF ( L_MERM > 0  .AND.  ADR_MERM .NE. 0 ) THEN
           DER_EROT(1)      = ROTP(2,1)
           DER_EROT(2)      = ROTP(1,1)
           DER_EROT(3)      = CNVRS_UT*ROTP(3,1)
!
           DER_EROT_RATE(1) = ROTP(2,2)
           DER_EROT_RATE(2) = ROTP(1,2)
           DER_EROT_RATE(3) = CNVRS_UT*ROTP(3,2)
!
           TIM_ARG = ( (FJD - J2000__JD) + FRACTC)*86400.0D0 - 32.184D0
!
           CALL FLYBY_MERM ( %VAL(ADR_MERM), TIM_ARG, DER_EROT, &
     &                       DER_EROT_RATE, DT, RT )
      END IF
!
      IF ( ATD_USE == ATD__AVERAGE  .OR.  ATD_USE == ATD__INSTANT ) THEN
!
! -------- Compute the contrubution due to antenna thermal deformations
!
           CALL APPLY_ATD ( %VAL(ATD_ADR), 1, ATD_USE, TEM_AVR(ISITE(1)),  &
     &                      DT_STA_1 )
           CALL APPLY_ATD ( %VAL(ATD_ADR), 2, ATD_USE, TEM_AVR(ISITE(2)),  &
     &                      DT_STA_2 )
           DT = DT - DT_STA_1*1.D6 + DT_STA_2*1.D6
      END IF
!
      SCA_LAST(I1) = NSCA
      SCA_LAST(I2) = NSCA
!
      RETURN
      END  !#!  FLYBY_MAP  #!#
