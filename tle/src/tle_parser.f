      SUBROUTINE TLE_PARSER ( FIL_TLE, EPH, IUER )
!
! ***************************************************************************************
! *                                                                                     *
! *   Routine TLE_PARSER reads a given TLE file                                         *
! *   Angles are given in radians, displacements in Earth Radii (er), and
! *   N.B: This routine assumes 1 TLE per file.                                         * 
! *        We will expand to more TLE's per file later                                  *
! *        The move to multiple files                                                   *
! *                                                                                     *
! *   INPUT:                                                                            *
! *           FIL_TLE    =  Two Line Element File                   { CHAR }            *
! *                                                                                     *
! *           IUER       =  Error Handler                           { INT, OPT }        *
! *                         If IUER=0 no error message will be printed,                 *
! *                         even in the event of an error. However, for                 *
! *                         other possible values, i.e. IUER=-1,-2, & -3,               *
! *                         the error message will print to screen. For                 *
! *                         the latter case, i.e. IUER=-3, after printing               *
! *                         the program will terminate.                                 *
! *                         Default, IUER = -1                                          *
! *                                                                                     *
! *   OUTPUT:                                                                           *
! *                                                                                     *
! *           EPH        =  File Ephemiris data                    { DERIVED TYPE }    *
! *                                                                                     *
! *                                                                                     *
! *  ###   16-NOV-2021    TLE_PARSER       v5.0 (c)    N. Habana     30-MAR-2023   ###  *
! *                                                                                     *
! ***************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'tle_sgp4.i'
      INCLUDE     'astro_constants.i'
      INCLUDE     'ners.i'
      INCLUDE     'ners_local.i'
      TYPE ( EPH__TYPE ) :: EPH
      TYPE        ( NERS__TYPE ) :: NERS
      CHARACTER   FIL_TLE*(*), STR*128
      REAL*8      UTC, MM_DOT, MM_DOTDOT, ELEM(6)
      REAL*8      INC, RAN, ECC, AOP, MA, MM
      INTEGER*4   MJD, ET, IUER, IER
      INTEGER*4   IDRAG, IDRAG_EXP, MM_DOTDOT_EXP
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
      CHARACTER   C_ECC*9, C_ERR*16
      INTEGER*4   J0, J1, J2, J3, J4, J5
      CHARACTER   NERS_CONFIG*128, HOME_DIR*128
      LOGICAL*1   LEX
!
! ---
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( NERS_CONFIG == ' ' ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', HOME_DIR )
         NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
         INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF
! ---
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2102, IUER, 'TLE_PARSER',                    &
     &                  'Error in initializing NERS data structure' )
         RETURN
      END IF
!
! --- Read TLE file data
!
      IUER = -1
      CALL RD_TEXT(FIL_TLE, MP, BUF, NP, IUER)
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2101, IUER, 'TLE_PARSER',                     &
     &                    'Error reading TLE file: '//TRIM(FIL_TLE) )
           RETURN
      END IF
!
! --- Allocate the TLE variable
!
      ALLOCATE ( EPH%TLE(1), STAT = IER ) 
!
! --- Get through file
!
      CALL CLRCH ( EPH%TLE(1)%SAT_NAM )
      IF ( (BUF(1)(1:2) .NE. '1 ') .AND. (BUF(1)(1:2) .NE. '2 ') ) THEN
         EPH%TLE(1)%SAT_NAM = TRIM(BUF(1))    ! Satellite name is the first line
      END IF
!
      DO 410 J1 = 1, NP
!
! ------ read line 1
!
         IF ( BUF(J1)(1:2) == '1 ' ) THEN 
!
            READ ( UNIT=BUF(J1)(3:7), FMT='(I7)', IOSTAT=IER )          &
     &           EPH%TLE(1)%SAT_CAT  ! Catalogue number
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2102, IUER, 'TLE_PARSER',                 &
     &                 'Error converting catalogue number. Input not'// &
     &                 ' INT, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
! ---------
            EPH%TLE(1)%SAT_CLASS = BUF(J1)(8:8)                         ! Classificiation
! ---------
            EPH%TLE(1)%INT_DES   = BUF(J1)(10:17)                       ! International Designator
!
! --------- Get the Launch Year
!
            READ ( UNIT=BUF(J1)(10:11), FMT='(I2)', IOSTAT=IER )        &
     &             EPH%TLE(1)%LY
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2103, IUER, 'TLE_PARSER',                 &
     &                 'Error converting Launch Year. Input not'//      &
     &                 ' INT, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
            IF ( EPH%TLE(1)%LY .LT. 57 ) THEN
               EPH%TLE(1)%LY = 2000 + EPH%TLE(1)%LY
            ELSE
               EPH%TLE(1)%LY = 1900 + EPH%TLE(1)%LY
            END IF
!
! --------- Get the Launch Number this Year
!
            READ ( UNIT=BUF(J1)(12:14), FMT='(I3)', IOSTAT=IER )        &
     &             EPH%TLE(1)%LNY
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2104, IUER, 'TLE_PARSER',                 &
     &                 'Error converting Launch Number . Input not'//   &
     &                 ' INT, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
! --------- Extract the TLE EPH%TLE(1)%EPOCH
!
            EPH%TLE(1)%C_EPOCH = BUF(J1)(19:32)
            READ ( UNIT=BUF(J1)(19:32), FMT='(F14.8)', IOSTAT=IER)      &
     &             EPH%TLE(1)%EPOCH
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2116, IUER, 'TLE_PARSER',                 &
     &                 'Error converting TLE DATE. Input not REAL, '//  &
     &                 'as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
! --------- Get the MJD and UTC for this epoch
!
            CALL TLEDATE_TO_MJDSEC( BUF(J1)(19:32), EPH%TLE(1)%MJD,     &
     &                              EPH%TLE(1)%UTC )

        PRINT *, "%%%%%%%%%%% TLE_PARSER - 177 %%%%%%%%%%%%%"
        PRINT *, "str_date=", BUF(J1)(19:32), " mjd= ", EPH%TLE(1)%MJD, "utc=",  EPH%TLE(1)%UTC

!
! --------- convert time to TAI
!
            CALL NERS_GET_UTCMTAI ( NERS,                               &
     &                              (EPH%TLE(1)%MJD - J2000__MJD)*86400 &
     &                              + EPH%TLE(1)%UTC,                   &
     &                              EPH%TLE(1)%UTC_MTAI, IER )
            IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 2102, IUER, 'TLE_PARSER',                   &
     &              'Error in extracting UTC minus TAI function' )
            RETURN
         END IF
!
! --------- First derivative of mean motion [rads/min^2]
!
            READ ( UNIT=BUF(J1)(34:43), FMT='(F10.8)', IOSTAT=IER )     &
     &             EPH%TLE(1)%MM_DOT
            EPH%TLE(1)%MM_DOT = EPH%TLE(1)%MM_DOT*                      &
     &                          (PI2/(1440.D0*1440.D0))
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2105, IUER, 'TLE_PARSER',                 &
     &                 'Error converting Mean Motion derivative.'//     &
     &                 'Not REAl, as expected. IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
! --------- Second derivative of mean motion [revs/min^3]
!
            READ( UNIT=BUF(J1)(45:50), FMT='(F6.5)', IOSTAT=IER )       &
     &            EPH%TLE(1)%MM_DOTDOT                                   ! Second derivative of mean motion
            READ( UNIT=BUF(J1)(51:52), FMT='(I2)', IOSTAT=IER )         &
     &            MM_DOTDOT_EXP                                          ! Exponential of 2nd der. of MM

            EPH%TLE(1)%MM_DOTDOT = EPH%TLE(1)%MM_DOTDOT*                &
     &                             (10.D0**MM_DOTDOT_EXP)
            EPH%TLE(1)%MM_DOTDOT = EPH%TLE(1)%MM_DOTDOT*                &
     &                             (PI2/(1440.D0**3))                
!
! --------- Get the drag term
!
            READ ( UNIT=BUF(J1)(54:59), FMT='(I6)', IOSTAT=IER ) IDRAG
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2106, IUER, 'TLE_PARSER',                 &
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
               CALL ERR_LOG ( 2107, IUER, 'TLE_PARSER',                 &
     &                 'Error converting IDRAG exponential. Not INT, '  &
     &                 //'as expected IOSTAT = '//TRIM(STR) )
               RETURN
            END IF
!
            EPH%TLE(1)%BSTAR = RDRAG*(10.D0**(IDRAG_EXP))  ! Earth Radii^-1
!
! --------- Get the Ephemeris Type {Always zero}
!
            READ ( UNIT=BUF(J1)(63:63), FMT='(I1)', IOSTAT=IER )        &
     &             EPH%TLE(1)%ET
!@@##@@!            EPH%TLE(1)%ET = 0
!
! --------- Get the Element Set Number
!
            READ ( UNIT=BUF(J1)(65:68), FMT='(I4)', IOSTAT=IER )        &
     &             EPH%TLE(1)%NTLE
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2108, IUER, 'TLE_PARSER',                 &
     &                 "Error converting EPH%TLE(1)%NTLE's. Not INT, "  &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
         END IF
!
! ------ Read Line 2
!
         IF ( BUF(J1)(1:2) == '2 ' ) THEN 
!
! --------- Inclination [deg]
!
            READ ( UNIT=BUF(J1)(9:16), FMT='(F8.4)', IOSTAT=IER )       &
     &             EPH%TLE(1)%INC
            EPH%TLE(1)%INC = EPH%TLE(1)%INC*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2109, IUER, 'TLE_PARSER',                 &
     &                 "Error converting inclination. Not REAL, "       &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Right ascension of ascending node [deg]
!
            READ ( UNIT=BUF(J1)(18:25), FMT='(F8.4)', IOSTAT=IER )      &
     &             EPH%TLE(1)%RAN
            EPH%TLE(1)%RAN = EPH%TLE(1)%RAN*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2110, IUER, 'TLE_PARSER',                 &
     &                 "Error converting right asc. node. Not REAL, "   &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Eccentricity
!
            C_ECC = '0.'//BUF(J1)(27:33)
            READ ( UNIT=C_ECC, FMT='(F9.8)', IOSTAT=IER ) EPH%TLE(1)%ECC
!##@@##!            EPH%TLE(1)%ECC = EPH%TLE(1)%ECC
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2111, IUER, 'TLE_PARSER',                 &
     &                 "Error converting eccentricity. Not REAL, "      &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Arg. of perigee [deg]
!
            READ ( UNIT=BUF(J1)(35:42), FMT='(F8.4)', IOSTAT=IER )      &
     &             EPH%TLE(1)%AOP
            EPH%TLE(1)%AOP = EPH%TLE(1)%AOP*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2112, IUER, 'TLE_PARSER',                 &
     &                 "Error converting arg. of perigee. Not REAL, "   &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Mean anomaly [deg]
!
            READ ( UNIT=BUF(J1)(44:51), FMT='(F8.4)', IOSTAT=IER )      &
     &             EPH%TLE(1)%MA
            EPH%TLE(1)%MA = EPH%TLE(1)%MA*DEG__TO__RAD
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2113, IUER, 'TLE_PARSER',                 &
     &                 "Error converting mean anomaly. Not REAL, "      &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- Mean motion [rads/min]
!
            READ ( UNIT=BUF(J1)(53:63), FMT='(F11.8)', IOSTAT=IER )     &
     &             EPH%TLE(1)%MM
            EPH%TLE(1)%MM = EPH%TLE(1)%MM*(PI2/1440.D0)   
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2114, IUER, 'TLE_PARSER',                 &
     &                 "Error converting mean motion. Not REAL, "       &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
!
! --------- No. of revolutions
!
            READ ( UNIT=BUF(J1)(64:68), FMT='(I5)', IOSTAT=IER )        &
     &             EPH%TLE(1)%NREV
            IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL IINCH ( IER, STR)
               CALL ERR_LOG ( 2115, IUER, 'TLE_PARSER',                 &
     &                 "Error converting no. of revs. Not INT, "        &
     &                 //"as expected IOSTAT = "//TRIM(STR) )
               RETURN
            END IF
         END IF
!
 410  CONTINUE
!
      RETURN
      END SUBROUTINE !#! 1
