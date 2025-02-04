      SUBROUTINE READ_TLE_FILE ( FIL_TLE, SAT_NAM, SAT_CAT, SAT_CLASS,  &
     &                           LY, LNY, INT_DES, MJD, TAI, EPOCH,     &
     &                           MM_DOT, MM_DOTDOT, BSTAR, ET, NTLE,    &
     &                           ELEM, MM, NREV, IUER )
!
! *******************************************************************************************
! *                                                                                         *
! *   Routine READ_TLE_FILE reads a given TLE file                                          *
! *   Angles are given in radians, displacements in Earth Radii (er), and 
! *                                                                                         *
! *   N.B: This routine assumes 1 TLE per file.                                             * 
! *        We will expand to more TLE's per file later                                      *
! *        The move to multiple files                                                       *
! *                                                                                         *
! *   INPUT:                                                                                *
! *           FIL_TLE    =  Two Line Element File                   { CHAR }                *
! *                                                                                         *
! *           IUER       =  Error Handler                           { INT*4, OPT }          *
! *                         If IUER=0 no error message will be printed,                     *
! *                         even in the event of an error. However, for                     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,                   *
! *                         the error message will print to screen. For                     *
! *                         the latter case, i.e. IUER=-3, after printing                   *
! *                         the program will terminate.                                     *
! *                         Default, IUER = -1                                              *
! *                                                                                         *
! *   OUTPUT:                                                                               *
! *           SAT_NAM    =  Satellite Name                            { CHAR }              *
! *                                                                                         *
! *           SAT_CAT    =  Satellite Catalog Numbre                  { INT*4 }             *
! *                                                                                         *
! *           SAT_CLASS  =  Elset Classification                      { CHAR }              *
! *                         == U: Unclassified                                              *
! *                         == C: Classified                                                *
! *                         == S: Secret                                                    *
! *                                                                                         *
! *           LY         =  Year of Launch                            { INT*4 }             *
! *                                                                                         *
! *           LNY        =  Launch number of the year                 { INT*4 }             *
! *                                                                                         *
! *           INT_DES    = International Designator                   { CHAR }              *
! *                                                                                         *
! *           MJD        =  Mean Julian Date                          { INT*4 }             *
! *                                                                                         *
! *           TAI        =  Universal Coordinated Time                { REAL*8 }            *
! *                                                                                         *
! *           EPOCH      =  Epoch in TLEE format                      { REAL*8 }            *
! *                                                                                         *
! *           MM_DOT     =  First derivativative of Mean Motion       { REAL*8 } [rad/min^2]*
! *                                                                                         *
! *           MM_DOTDOT  =  Second derivative of Mean Motion          { REAL*8 } []         *
! *                         Conventional wisdom is to set this to zero nowadays.            *
! *                                                                                         *
! *           BSTAR      =  Radiation pressure coefficient            { REAL*8 } [1/er]     *
! *                         The drag term, given in units of Earth Radii                    *
! *                                                                                         *
! *           ET         =  Ephemeris Type                            { INT*4 }             *
! *                         Always equal to zero.                                           *
! *                                                                                         *
! *           NTLE       =  Number of TLE's for this object.          { INT*4 }             *
! *                                                                                         *
! *           ELEM       = Keplerian Elements                         { REAL*8 } (6x1)      *
! *		           ELEM(1) == 0 semi-major axis                          [er]       *
! *                        ELEM(2) ==   eccentricity                             []         *
! *                        ELEM(3) ==   inclination                              [rad]      *
! *                        ELEM(4) ==   argument of perigee                      [rad]      *
! *                        ELEM(5) ==   right ascension of node                  [rad]      *
! *                        ELEM(6) ==   mean anomaly                             [rad]      *
! *                                                                                         *
! *           MM         =  Mean Motion                               { REAL*8 } [rads/min] *
! *                                                                                         *
! *           NREV       = Number of full revolutions at epoch        { INT*4 }             *
! *                                                                                         *
! *                                                                                         *
! *  ###   16-NOV-2021    READ_TLE_FILE        v4.0 (c)    N. Habana     08-MAR-2022   ###  *
! *                                                                                         *
! *******************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'ners.i'
      INCLUDE     'ners_local.i'
      INCLUDE     'astro_constants.i'
      TYPE      ( NERS__TYPE ) :: NERS
      CHARACTER   FIL_TLE*(*), STR*128
      CHARACTER   SAT_NAM*24, SAT_CLASS, INT_DES*8
      REAL*8      TAI, MM_DOT, MM_DOTDOT, BSTAR, ELEM(6)
      REAL*8      INC, RAN, ECC, AOP, MA, MM, EPOCH, UTC, UTC_M_TAI
      INTEGER*4   MJD, SAT_CAT, LY, LNY, ET, NTLE, NREV, IUER, IER
      INTEGER*4   IDRAG, IDRAG_EXP
      REAL*8      RDRAG
      CHARACTER   DELIM*5                       ! Deliminator
      INTEGER*4   MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4   MAXL_STRING                   ! Max. String length
      PARAMETER   ( MAXL_STRING = 256 )           
      PARAMETER   ( MP = 16 )             
      PARAMETER   ( MIND = 128 )                 
      PARAMETER   ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' ) 
      CHARACTER   BUF(MP)*(MAXL_STRING)         ! Read File
      INTEGER*4   NP, LIND, IND(2,MIND), LN
      CHARACTER   C_ECC*9, C_ERR*16, NERS_CONFIG*128, HOME_DIR*128
      LOGICAL*1   LEX
      INTEGER*4   J0, J1, J2, J3, J4, J5
!
! --- Read TLE file data
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT(FIL_TLE, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2101, IUER, 'READ_TLE_FILE',                  &
     &                    'Error reading TLE file: '//TRIM(FIL_TLE) )
           RETURN
      END IF
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( NERS_CONFIG == ' ' ) THEN
!
! -------- Second, check $HOME/.ners_config file
!
            CALL GETENVAR ( 'HOME', HOME_DIR )
            NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
            INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
            IF ( .NOT. LEX ) THEN
!
! --------------- Third, check for the system-wide ners configuration file 
!
                  NERS_CONFIG = NERS__CONFIG
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2102, IUER, 'READ_TLE_FILE',                  &
     &                    'Error in initializing NERS data structure' )
           RETURN
      END IF
!
! --- Get through file
!
      CALL CLRCH ( SAT_NAM )
      IF ( (BUF(1)(1:1) .NE. '1') .AND. (BUF(1)(1:1) .NE. '2') ) THEN
         SAT_NAM = TRIM(BUF(1))    ! Satellite name is the first line
      END IF
!
      ELEM = 0.D0
!
      DO 410 J1 = 1, NP
!
! ------ read line 1
!
         IF (  BUF(J1)(1:1) == '1' ) THEN 
!
            READ ( UNIT=BUF(J1)(3:7), FMT='(I7)', IOSTAT=IER )  SAT_CAT  ! Catalogue number
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2102, IUER, 'READ_TLE_FILE',              &
     &                 'Error converting catalogue number. Input not'// &
     &                 ' INT, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
            SAT_CLASS = BUF(J1)(8:8)                         ! Classificiation
!
            INT_DES   = BUF(J1)(10:17)                       ! International Designator
!
! --------- Get the Launch Year
!
            READ ( UNIT=BUF(J1)(10:11), FMT='(I2)', IOSTAT=IER ) LY
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2103, IUER, 'READ_TLE_FILE',              &
     &                 'Error converting Launch Year. Input not'//      &
     &                 ' INT, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
            IF ( LY .LT. 57 ) THEN
               LY = 2000 + LY
            ELSE
               LY = 1900 + LY
            END IF
!
! --------- Get the Launch Number this Year
!
            READ ( UNIT=BUF(J1)(12:14), FMT='(I3)', IOSTAT=IER ) LNY
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2104, IUER, 'READ_TLE_FILE',              &
     &                 'Error converting Launch Number . Input not'//   &
     &                 ' INT, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
! --------- Extract the TLE EPOCH
!
            READ ( UNIT=BUF(J1)(19:32), FMT='(F14.8)', IOSTAT=IER) EPOCH
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2116, IUER, 'READ_TLE_FILE',              &
     &                 'Error converting TLE DATE. Input not REAL, '//  &
     &                 'as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
! --------- Get the MJD and UTC for this epoch
!
            CALL TLEDATE_TO_MJDSEC( BUF(J1)(19:32), MJD, UTC )
            CALL ERR_PASS ( IUER, IER )
            CALL NERS_GET_UTCMTAI ( NERS,                                &
     &                             (MJD - J2000__MJD)*86400 + UTC,      &
     &                             UTC_M_TAI, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2102, IUER, 'READ_TLE',                 &
     &                          'Error in extraction of UTC minus '//   &
     &                          'TAI function' )
               RETURN
            END IF
            TAI = UTC - UTC_M_TAI
!
! --------- First derivative of mean motion [rads/min^2]
!
            READ ( UNIT=BUF(J1)(34:43), FMT='(F10.8)', IOSTAT=IER )     &
     &             MM_DOT
            MM_DOT = MM_DOT*(PI2/(1440.D0*1440.D0))
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2105, IUER, 'READ_TLE_FILE',              &
     &                 'Error converting Mean Motion derivative.'//     &
     &                 'Not REAl, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
! --------- Second derivative of mean motion [revs/day^3]
!
!##@@##!            READ( UNIT=BUF(J1)(45:52), FMT='(F8.6)', IOSTAT=IER ) MM_DOTDOT   ! Second derivative of mean motion
            MM_DOTDOT = 0.D0 
!
! --------- Get the drag term
!
            READ ( UNIT=BUF(J1)(54:59), FMT='(I6)', IOSTAT=IER ) IDRAG
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2106, IUER, 'READ_TLE_FILE',              &
     &                 'Error converting IDRAG. Not INT, as expected.'  &
     &                 //' IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
! --------- Account for the decimal point
! --------- N.B: We assume the drag term is ALWAYS 5 digits long
!
            RDRAG = (REAL(IDRAG,8))*1.D-5
!
            READ (UNIT=BUF(J1)(60:61), FMT='(I2)', IOSTAT=IER) IDRAG_EXP
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2107, IUER, 'READ_TLE_FILE',              &
     &                 'Error converting IDRAG exponential. Not INT, '  &
     &                 //'as expected IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
            BSTAR = RDRAG*(10.D0**(IDRAG_EXP))  ! Earth Radii^-1
!
! --------- Get the Ephemeris Type {Always zero}
!
!##@@##!            READ ( UNIT=BUF(J1)(63:63), FMT='(I1)', IOSTAT=IER ) ET
            ET = 0
!
! --------- Get the Element Set Number
!
            READ ( UNIT=BUF(J1)(65:68), FMT='(I4)', IOSTAT=IER ) NTLE
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2108, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting NTLE's. Not INT, "             &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
         END IF
!
! ------ Read Line 2
!
         IF ( BUF(J1)(1:1) == '2' ) THEN 
!
! --------- Inclination [deg]
!
            READ ( UNIT=BUF(J1)(9:16), FMT='(F8.4)', IOSTAT=IER ) INC
            ELEM(3) = INC*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2109, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting inclination. Not REAL, "       &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Right ascension of ascending node [deg]
!
            READ ( UNIT=BUF(J1)(18:25), FMT='(F8.4)', IOSTAT=IER ) RAN
            ELEM(5) = RAN*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2110, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting right asc. node. Not REAL, "   &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Eccentricity
!
            C_ECC = '0.'//BUF(J1)(27:33)
            READ ( UNIT=C_ECC, FMT='(F9.8)', IOSTAT=IER ) ECC
            ELEM(2) = ECC
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2111, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting eccentricity. Not REAL, "      &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Arg. of perigee [deg]
!
            READ ( UNIT=BUF(J1)(35:42), FMT='(F8.4)', IOSTAT=IER ) AOP
            ELEM(4) = AOP*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2112, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting arg. of perigee. Not REAL, "   &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Mean anomaly [deg]
!
            READ ( UNIT=BUF(J1)(44:51), FMT='(F8.4)', IOSTAT=IER ) MA
            ELEM(6) = MA*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2113, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting mean anomaly. Not REAL, "      &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Mean motion [rads/min]
!
            READ ( UNIT=BUF(J1)(53:63), FMT='(F11.8)', IOSTAT=IER ) MM
            MM = MM*(PI2/1440.D0)   
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2114, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting mean motion. Not REAL, "       &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- No. of revolutions
!
            READ ( UNIT=BUF(J1)(64:68), FMT='(I5)', IOSTAT=IER ) NREV
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2115, IUER, 'READ_TLE_FILE',              &
     &                 "Error converting no. of revs. Not INT, "        &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
         END IF
!
 410  CONTINUE

      RETURN
      END SUBROUTINE !#! 1
