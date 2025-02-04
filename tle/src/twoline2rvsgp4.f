! *     ----------------------------------------------------------------
! *
! *                               sgp4io.for
! *
! *    this file contains a function to read two line element sets. while 
! *    not formerly part of the sgp4 mathematical theory, it is 
! *    required for practical implemenation.
! *
! *                             companion code for
! *               fundamentals of astrodynamics and applications
! *                                    2007
! *                              by david vallado
! *
! *        (w) 719-573-2600, email dvallado@agi.com
! *
! *   current :
! *             3 jul 08  david vallado
! *                        add switch for afspc compatibility and improved operation
! *  changes :
! *               14 mar 07  david vallado
! *                            misc fixes and manual operation
! *               15 aug 06  david vallado
! *                            original baseline
! *       ----------------------------------------------------------------


! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE TWOLINE2RVSGP4
! *
! *  this function converts the two line element set character string data to
! *    variables and initializes the sgp4 variables. several intermediate varaibles
! *    and quantities are determined. note that the result is a "structure" so multiple
! *    satellites can be processed simultaneously without having to reinitialize. the
! *    verification mode is an important option that permits quick checks of any
! *    changes to the underlying technical theory. this option works using a
! *    modified tle file in which the start, stop, and delta time values are
! *    included at the end of the second line of data. this only works with the
! *    verification mode. the catalog mode simply propagates from -1440 to 1440 min
! *    from epoch and is useful when performing entire catalog runs.
! *
! *  author        : david vallado                  719-573-2600    1 mar 2001
! *
! *  inputs        :
! *    Numsats     - Number of satellites processed. It also becomes the record
! *                  number for each satellite
! *    typerun     - type of run                    verification 'v', catalog 'c', 
! *                                                 manual 'm'
! *    typeinput   - type of manual input           mfe 'M', epoch 'E', dayofyr 'D'
! *    whichconst  - which set of constants to use  72, 84
! *    opsmode     - type of manual input           afspc 'a', improved 'i'
! *
! *  outputs       :
! *    Code        - EOF indicator. Code = 999 when EOF reached
! *    startmfe    - starttime of simulation,       min from epoch
! *    stopmfe     - stoptime of simulation,        min from epoch
! *    deltamin    - time step                      min
! *
! *  coupling      :
! *    TLE_DAYS2MDHMS  - conversion of days to month, day, hour, minute, second
! *    TLE_JDAY        - convert day month year hour minute second into julian date
! *    sgp4init    - initialize the sgp4 variables
! *
! *  Files         :
! *    Unit 10     - test.elm        input 2-line element set file
! *    Unit 11     - test.bak        output file
! *    Unit 15     - sgp4rec.bak     temporary file of record for 2 line element sets
! *
! *  references    :
! *    norad spacetrack report #3
! *    vallado, crawford, hujsak, kelso  2006
! * ------------------------------------------------------------------------------

      SUBROUTINE TWOLINE2RVSGP4 ( EPH, FL_DEBUG, NumSats, Typerun,      &
     &                            typeinput, whichconst,                &
     &                           startmfe, stopmfe, deltamin, Code )
      IMPLICIT NONE
      INCLUDE     'tle_sgp4.i'
      INCLUDE     'astro_constants.i'
      TYPE ( EPH__TYPE ) :: EPH
      CHARACTER TYPERUN*1, TYPEINPUT*1
      LOGICAL*1 FL_DEBUG
      INTEGER*4 IUER, IER
      INTEGER*4 LUN, CODE, NUMSATS, WHICHCONST
      REAL*8    STARTMFE, STOPMFE, DELTAMIN

! * ----------------------------  Locals  -------------------------------
        REAL*8 J2, mu, RadiusEarthKm,VKmPerSec, xke, tumin
        REAL*8 BC,EPDay, sec, xpdotp, j3, j4, j3oj2 
        REAL*8 startsec, stopsec, startdayofyr, stopdayofyr, jdstart,   &
     &         jdstop, jdf, jdsf
        INTEGER startyear, stopyear, startmon, stopmon, startday,       &
     &          stopday, starthr, stophr, startmin, stopmin 
        INTEGER YR, Mon, Day, Hr, Minute, ICrdno, nexp, bexp, error
        CHARACTER Show
        Character*130 LongStr1,LongStr2

        COMMON /DebugHelp/ Help
        CHARACTER Help

        INCLUDE 'sgp4.i'
        INCLUDE 'astmath.i'

        ! --------------------  Implementation   ----------------------
        Show = 'N'
        xPDOTP =  1440.0D0 / (2.0D0 * PI) ! 229.1831180523293

        CALL GETGRAVCONST( whichconst, tumin, mu, radiusearthkm, xke,   &
     &       j2, j3, j4, j3oj2 );
        VKmPerSec     =  RadiusEarthKm * xke / 60.0D0

!
! --- Parse variables to common block from derived type
!
      CODE = 0
      SATNUM  = EPH%TLE(1)%SAT_CAT
      SATNAME = EPH%TLE(1)%INT_DES
      READ ( UNIT=EPH%TLE(1)%C_EPOCH(1:2), FMT='(I2)', IOSTAT=IER )       &
     &       EPOCHYR
      READ ( UNIT=EPH%TLE(1)%C_EPOCH(3:14), FMT='(F12.8)', IOSTAT=IER )   &
     &       EPDAY
      EPHTYP = EPH%TLE(1)%ET
      ELNO   = EPH%TLE(1)%NTLE
      REVI   = EPH%TLE(1)%NREV
!
! * ---------------------- CONVERT TO INTERNAL UNITS --------------------
! * ---- RADIANS, DISTANCE IN EARTH RADII, AND VELOCITY IN ER/KEMIN) ----
!
      ECCO      = EPH%TLE(1)%ECC 
      NDDOT     = EPH%TLE(1)%MM_DOTDOT
      NDOT      = EPH%TLE(1)%MM_DOT
      BStar     = EPH%TLE(1)%BSTAR
      NO_KOZAI  = EPH%TLE(1)%MM
      A         = (NO_KOZAI*TUMIN)**(-2.D0/3.D0)
      INCLO     = EPH%TLE(1)%INC
      NODEO     = EPH%TLE(1)%RAN
      ARGPO     = EPH%TLE(1)%AOP
      MO        = EPH%TLE(1)%MA
! ---
      IF ( DABS(ECCO-1.0D0) .GT. 0.000001D0 ) THEN
         ALTP= (A*(1.0D0-ECCO))-1.0D0
         ALTA= (A*(1.0D0+ECCO))-1.0D0
      ELSE
         ALTA= 999999.9D0
         ALTP= 2.0D0* (4.0D0/(NO_KOZAI*NO_KOZAI)**(1.0D0/3.0D0))
      ENDIF

      ! ---- Ballistic Coefficient ----
      IF ( DABS(BSTAR) .GT. 0.00000001D0 ) THEN
         BC= 1.0D0/(12.741621D0*BSTAR)
      ELSE
         BC= 1.111111111111111D0
      ENDIF

      ! ----------------------------------------------------------------
      ! find sgp4epoch time of element set
      ! remember that sgp4 uses units of days from 0 jan 1950 (sgp4epoch)
      ! and minutes from the epoch (time)
      ! ----------------------------------------------------------------

      ! Temporary year fix
      IF ( EpochYr .lt. 57 ) THEN
         Yr = EpochYr + 2000
      ELSE
         Yr = EpochYr + 1900
      ENDIF

      CALL TLE_DAYS2MDHMS( Yr,EpDay, Mon,Day,Hr,Minute,Sec )
      CALL TLE_JDAY ( Yr,Mon,Day,Hr,Minute,Sec,  JDSatEpoch,JDSatEpochF )

      ! -------------- input start stop times manually --------------- 
      if ( (typerun .ne. 'v') .and. (typerun .ne. 'c') ) then
         ! --------- enter start/stop ymd hms values ---------------- 
         if ( typeinput .eq. 'E' ) then
            write(*,*) 'input start y m d h m s '
            read(*,*) startyear,startmon,startday,starthr,startmin, &
     &                startsec
            CALL TLE_JDAY( startyear,startmon,startday,starthr,startmin,&
     &                 startsec, jdstart, jdf)

            write(*,*)'input stop  y m d h m s '
            read(*,*) stopyear,stopmon,stopday,stophr,stopmin,      &
     &                stopsec
            CALL TLE_JDAY( stopyear,stopmon,stopday,stophr,stopmin,     &
     &                 stopsec, jdstop, jdsf )

            startmfe = (jdstart +jdf - jdsatepoch +                 &
     &                 JDSatEpochF)*1440.0D0
            stopmfe  = (jdstop +jdsf - jdsatepoch +                 &
     &                 JDSatEpochF)*1440.0D0

            write(*,*)'input time step in minutes '
            read(*,*) deltamin
         ENDIF
         ! ------ enter start/stop year and days of year values -----
         if ( typeinput .eq. 'D' ) then
            write(*,*) 'input start y dayofyr '
            read(*,*) startyear,startdayofyr

            write(*,*) 'input stop  y dayofyr '
            read(*,*) stopyear,stopdayofyr

            CALL TLE_DAYS2MDHMS ( startyear,startdayofyr, mon,day,hr,   &
     &                        minute,sec) 
            CALL TLE_JDAY( startyear,mon,day,hr,minute,sec, jdstart,    &
     &                 jdf )
            CALL TLE_DAYS2MDHMS ( stopyear,stopdayofyr, mon,day,hr,     &
     &                        minute,sec )
            CALL TLE_JDAY( stopyear,mon,day,hr,minute,sec, jdstop,      &
     &                 jdsf )

            startmfe = (jdstart + jdf - jdsatepoch +                &
     &                JDSatEpochF)*1440.0D0
            stopmfe  = (jdstop + jdsf - jdsatepoch +                &
     &                JDSatEpochF)*1440.0D0

            write(*,*)'input time step in minutes '
            read(*,*) deltamin
         ENDIF
         ! -------------- enter start/stop mfe values --------------- 
         IF ( typeinput .eq. 'M') then
            write(*,*) 'input start min from epoch'
            read(*,*) startmfe
            write(*,*) 'input stop min from epoch'
            read(*,*) stopmfe
            write(*,*) 'input time step in minutes'
            read(*,*) deltamin
         ENDIF
      ENDIF

      ! -------- perform complete catalog evaluation, -+ 1 day ------- 
      if ( typerun .eq. 'c') THEN
         startmfe = -1440.0D0
         stopmfe  =  1440.0D0
         deltamin =    10.0D0
      ENDIF
!
! * ------------------- MAKE INITIAL PREDICTION AT EPOCH ----------------
        ! 2433281.5 - 2400000.5 = 33281.0, thus time from 1950
      CALL SGP4INIT( whichconst,                                      &
     &               SatNum, BStar, Ecco, JDSatEpoch+JDSatEpochF       &
     &               -2433281.5D0,                                    &
     &               Argpo,Inclo,Mo,no_kozai, nodeo, Error )
!
! ---- Write common block of data into file of record ----
!
      IF ( FL_DEBUG ) THEN
         WRITE ( 15,Rec=NumSats) SatName,                             &
     &           SatNum, ELNO  , EPHTYP, REVI  , EpochYr,                &
     &           BStar , Ecco  , Inclo , nodeo , Argpo , no_kozai, Mo  , &
     &           NDot  , NDDot ,                                         &
     &           alta  , altp  , a     ,                                 &
     &           DeltaMin, JDSatEpoch, JDSatEpochF,EpochDays,no_unkozai, &
     &           Isimp , Init  , Method, Opsmode,                        &
     &           Aycof , CON41 , Cc1   , Cc4   , Cc5   , D2    , D3    , &
     &           D4    , Delmo , Eta   , ArgpDot,Omgcof, Sinmao,         &
     &           T2cof , T3cof , T4cof , T5cof , X1mth2, X7thm1, MDot  , &
     &           nodeDot,Xlcof , Xmcof , Xnodcf,                         &
     &           D2201 , D2211 , D3210 , D3222 , D4410 , D4422 , D5220 , &
     &           D5232 , D5421 , D5433 , Dedt  , Del1  , Del2  , Del3  , &
     &           Didt  , Dmdt  , Dnodt , Domdt , E3    , Ee2   , Peo   , &
     &           Pgho  , Pho   , Pinco , Plo   , Se2   , Se3   , Sgh2  , &
     &           Sgh3  , Sgh4  , Sh2   , Sh3   , Si2   , Si3   , Sl2   , &
     &           Sl3   , Sl4   , GSTo  , Xfact , Xgh2  , Xgh3  , Xgh4  , &
     &           Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   , Xl4   , &
     &           Xlamo , Zmol  , Zmos  , Atime , Xli   , Xni   , IRez
      END IF
      IF ( ERROR .GT. 0 ) THEN
          WRITE( *,*) '# TWOLINE2RVSGP4: ** SGP4 Model Error ***', ERROR
      ENDIF
!
! c    write tle output details
! c    INCLUDE 'debug8.f'
!
! ----- Fix to indicate end-of-file
!
       GOTO 1000
  999  CODE = 999
 1000  CONTINUE

       RETURN
       END  SUBROUTINE  TWOLINE2RVSGP4  !#!#
