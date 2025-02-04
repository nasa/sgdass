      SUBROUTINE GMAP()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GMAP PROGRAM SPECIFICATION OF THE CONTROL FILE.
!
! 1.1 PARSE MAPPING SECTION
!
! 1.2 REFERENCES:
!
! 2.  GMAP INTERFACE
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
      INCLUDE 'precm.i'
      INCLUDE 'dmapp.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'vtd.i'
      INCLUDE 'ba2cm.i'
      TYPE   ( VTD__TYPE          ), POINTER :: VTD_PTR
      TYPE   ( TRP__TYPE          ), POINTER :: TRP_PTR
      TYPE   ( ANTENNA_DATA__TYPE ), POINTER :: ATD_PTR
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: cfread,gmapf,cfunrd
!
! 3.  LOCAL VARIABLES
!
      CHARACTER  TOKEN*256, DUMFIL*128, SITPLMAP_DEF*128, STR*128, DEF_DATE*21
      INTEGER*2  LENGTH, IDUM, CFREAD, TRIMLEN, YEAR, MONTH, DAY, &
     &           ITOK(40), J1
      INTEGER*4  IOS
      ADDRESS__TYPE :: DIR_DESC
      EQUIVALENCE  ( TOKEN, ITOK)
      LOGICAL*4  FL_POSVAR, FL_BSP, FL_ERM, FL_VTD, FL_TRP, FL_ANT, LEX
      LOGICAL*2  CFEOF
      INTEGER*8        MEM_LEN(3), SIZEOF_ATD, SIZEOF_TRP, SIZEOF_VTD
      ADDRESS__TYPE :: MEM_ADR(3)
      INTEGER*4  IER, MJD, IUER
      REAL*8     SEC
!
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
      REAL*8     FJLDY
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  900620  pick up site motion reference date, if any
!   AEE  910307  Added code to handle VELOCITIES line of control file
!                and detect if both Plate_model and Station Velocites
!                are used. Only either one of those lines can be present.
!   AEE  910816  Added flag for batch interpolation control: LINear, CUBic,
!                SPLine or default (= not specified).
!   PET  1999.10.13  Added support of a new keyword ECCDAT
!   PET  2000.03.19  Added support of a value NO_ZONAL in keyword
!                    EARTH_ORIENTATION
!   PET  2000.04.12  Added support of the case when UT1R/UT1S/NO_ZONAL
!                    value is omitted in the 'EARTH_ORIENTATION' keyword
!                    UT1S will be used as a default in this case.
!   PET  2000.05.09  Disalbed NUTATION_MODEL keyword since CALC 9.1 doesn't
!                    support parital derivatives wrt the specific terms of
!                    nutation expansion. Added suppot of SIPL NONE clause
!   pet  2000.09.22  Added support of keyword MEAN_GRADIENT
!   pet  2002.12.24  Disabled REF_DATE keyword
!   pet  2002.12.29  Corrected a bug in parsing the keyword VELOCITY: GMAP
!                    mixes keywrods and qualifiers. So, the old version
!                    erroneosly interperted qualifier REF_DATE of the keyword
!                    velocity as a KEYWORD
!   pet  2003.01.14  Fixed a bug: the previous version worked incorrectly
!                    in parsing PLATE_MODEL keyword
!   pet  2003.01.29  Fixed a bug: the previous version worked incorrectly
!                    in parsing PLATE_MODEL keyword
!   pet  2003.05.13  Fixed  a bug: GMAPF returned the second word which was
!                    not supposed to be used, but it was used in NORML and
!                    caused abnormal termination
!   pet  2003.08.04  Fixed a bug: the previous version worked incorrectly
!                    in parsing PLATE_MODEL keyword due to error in
!                    initialization
!   pet  2003.09.02  Added support of keywords for harmonic Earth orientation
!                    parameters and nutation expansion
!   pet  2003.11.13  Replaced ias2b with READ
!   pet  2004.11.24  Added support of 7 additional mapping parameters
!   pet  2006.01.27  Added suport of ERM
!   pet  2008.02.06  Added support of TRP
!   pet  2008.04.23  Added support of atnenna thermal deformations
!   pet  2019.09.03  Changed type of  DIR_DISC and OPENDIR to INTEGER*8
!
! 5. GMAP PROGRAM STRUCTURE
!
! initialize mapping file names to blanks
!
      SITPLMAP_DEF = SITPL_FILE
      DEF_DATE = '2000.01.01_00:00:00.0'
!
! --- Initialize site motion reference date to default (1980 Oct 17  00 UT)
!
      TIMEREF_YR = SOLVE_REF_EPOCH__JD/YEAR__TO__DAY
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 (  'R' )
!
      STAMAP              = ' '
      AXOMAP              = ' '
      PLTMAP              = ' '
      SRCMAP              = ' '
      NTSMAP              = ' '
      NTMMAP              = ' '
      EOPMAP              = ' '
      VELMAP              = ' '
      ESMMAP              = ' '
      PWCMAP              = ' '
      STAMAP2             = ' '
      VELMAP2             = ' '
      SRCMAP2             = ' '
      DUMFIL              = ' '
      PLTMAP2             = ' '
      HFEOPCAL            = ' '
      KHFEOPCAL           = .FALSE.
      PLODCALF            = ' '
      KPLOD               = .FALSE.
      PLATE_SCALE         = 1.D0
      FLYBY_INTERPOLATION = ' '
      UT1RSFLYBY          = ' '
      SITPLMAP            = SITPLMAP_DEF
      NUVEL_FIXED         = ' '
      ECCMAP              = ' '
      MGRMAP              = ' '
      METRIC_MAP          = ' '
      N_POSVAR            = 0
      FL_POSVAR           = .FALSE.
      FL_BSP              = .FALSE.
      FL_VTD              = .FALSE.
      FL_TRP              = .FALSE.
      FL_ANT              = .FALSE.
      L_BSP               = 0
      L_MERM              = 0
      ADR_BSP             = 0
      ADR_MERM            = 0
      ATD_USE             = ATD__UNDF    
      ATD_MEA_USE         = 0 
      ATD_ADR             = 0
      ATD_STS             = 0
      SNR_MIN_X           = 0.0D0
      SNR_MIN_S           = 0.0D0
      CALL CLRCH ( ATD_FIL )
      CALL CLRCH ( ATD_MEA_FIL )
      DO 410 J1=1,M__POSVAR
         CALL CLRCH ( POSVAR_FIL(J1) )
         POSVAR_MOD(J1) = 0
         POSVAR_INT(J1) = 0
         POSVAR_USE(J1) = 0
 410  CONTINUE
      CALL CLRCH ( FINAM_MERM )
      CALL CLRCH ( VTD_CONF_GLB )
      FL_VTD_GLB = .FALSE.
!
! --- Initialization of the variables related to Harmonic Earth Orientation
! --- Parameters
!
      L_HEO     = 0
      ADR_HEO   = 0
      STAT_HEO  = HEO__UNDF
      HEO_EPOCH_SEC = 0.0D0
      CALL CLRCH ( FINAM_HEO )
      CALL CLRCH ( NAME_HEO  )
      CALL CLRCH ( TRP_DIR )
      TRP_USE   = UNDF__TRP
      N_FIL_TRP = 0
      ADR_TRP_FIL_BUF = 0
      STS_TRP_FIL     = UNDF__TRP
      ADR_TRP_SES_BUF = 0
      STS_TRP_SES     = UNDF__TRP
      ADR_TRP = 0
      STS_TRP = UNDF__TRP
!
      LENGTH=CFREAD(STRING)
      DO WHILE ( STRING(1:1) .EQ. ' '  .AND.  .NOT. CFEOF(IDUM) )
         DO WHILE ( TRIMLEN(STRING) .GT. 0 )
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'SOURCES' ) THEN
!
! ------------- 'SOURCES' keyword
!
                CALL GMAPF ( 'SOURCES', STRING, TOKEN, INT2(15010), SRCMAP, &
     &                        SRCMAP2 )
              ELSE IF ( TOKEN .EQ. 'STATIONS' ) THEN
!
! ------------- 'STATIONS' keyword
!
                CALL GMAPF ( 'STATIONS', STRING, TOKEN, INT2(15020), STAMAP, &
     &                        STAMAP2 )
              ELSE IF ( TOKEN .EQ. 'AXIS_OFFSET' ) THEN
!
! ------------- 'AXIS_OFFSET' keyword
!
                CALL GMAPF ( 'AXIS_OFFSET', STRING, TOKEN, INT2(15020), &
     &                        AXOMAP, DUMFIL )
              ELSE IF ( TOKEN .EQ. 'EARTH_ORIENTATION' ) THEN
!
! ------------- 'EARTH_ORIENTATION' keyword
!
                CALL GMAPF ( 'EARTH_ORIENTATION', STRING, TOKEN, INT2(15030), &
     &                        EOPMAP, DUMFIL )
!
! ------------- Pick up flyby interpolation token
! -------------  (LINEAR, CUBIC, SPLINE, DEFAULT=not specified)
!
                IF ( DUMFIL(1:3) .EQ. 'LIN'  .OR. DUMFIL(1:3) .EQ. 'CUB'  .OR. &
     &               DUMFIL(1:3) .EQ. 'SPL' ) THEN
!
                     FLYBY_INTERPOLATION = DUMFIL
                     CALL SPLITSTRING ( STRING, TOKEN, STRING )
                     IF ( TOKEN(1:4) .EQ. 'UT1R' ) THEN
                          UT1RSFLYBY = 'R'
                        ELSE IF ( TOKEN(1:4) .EQ. 'UT1S' ) THEN
                          UT1RSFLYBY = 'S'
                        ELSE IF ( TOKEN(1:8) .EQ. 'NO_ZONAL' ) THEN
                          UT1RSFLYBY = 'N'
                        ELSE IF ( TOKEN(1:4) .EQ. '    ' ) THEN
                          UT1RSFLYBY = 'S'  ! default
                        ELSE
                          CALL FERR ( INT2(7022), &
     &                        'BATCH(gmap) Error in decoding '// &
     &                        'token EARTH_ORIENTATION in $MAPPING section: '// &
     &                         TOKEN//' one of UT1R, UT1S, NO_ZONAL expected', &
     &                         INT2(0), INT2(0) )
                          CALL FLUSH ( 6 ) 
                          STOP 'BATCH(gmap) Abnormal termination'
                     END IF
                   ELSE
                     IF ( DUMFIL(1:4) .EQ. 'UT1R' ) THEN
                          UT1RSFLYBY = 'R'
                        ELSE IF ( DUMFIL(1:4) .EQ. 'UT1S' ) THEN
                          UT1RSFLYBY = 'S'
                        ELSE IF ( DUMFIL(1:8) .EQ. 'NO_ZONAL' ) THEN
                          UT1RSFLYBY = 'N'
                        ELSE IF ( DUMFIL(1:1) .EQ. ' ' ) THEN
                          UT1RSFLYBY = 'N'
                        ELSE
                          CALL FERR ( INT2(7024), &
     &                        'BATCH(gmap) Error in decoding '// &
     &                        'token EARTH_ORIENTATION in $MAPPING section: '// &
     &                         DUMFIL(1:8)//' one of UT1R, UT1S, NO_ZONAL expected', &
     &                         INT2(0), INT2(0) )
                          CALL FLUSH ( 6 ) 
                          STOP 'BATCH(gmap) Abnormal termination'
                     END IF
                ENDIF
             ELSE IF ( TOKEN .EQ. 'HI_FREQ_EOP' ) THEN
!
! ----------- 'HI_FREQ_EOP' KEYWORD
!
               CALL GMAPF ( 'HI_FREQ_EOP', STRING, TOKEN, INT2(15030), &
     &              HFEOPCAL, DUMFIL )
               IF ( HFEOPCAL .NE. ' ' .AND. HFEOPCAL .NE. 'NONE' ) THEN
                    KHFEOPCAL = .TRUE.
               ENDIF
             ELSE IF(TOKEN.EQ.'PRESSURE_LOADING') THEN
!
! ----------- 'PRESSURE_LOADING' KEYWORD
!
               CALL GMAPF ( 'PRESSURE_LOADING', STRING, TOKEN, INT2(15030), &
     &              PLODCALF, DUMFIL )
               KPLOD = .TRUE.
             ELSE IF ( TOKEN .EQ. 'PLATE_MODEL' ) THEN
!
! ----------- 'PLATE_MODEL' KEYWORD
! ----------- Note: Can have either Plate_Model or Station Velocities line,
! ----------- not both
!
              STR = VELMAP
              CALL GMAPF ( 'PLATE_MODEL', STRING, TOKEN, INT2(15040), STR, &
     &             PLTMAP2 )
              VELMAP = 'NONE' ! to indicate plate model is used
              PLTMAP = TOKEN
              IF ( PLTMAP2(1:1) .NE. ' '  ) THEN
!
! --------------- 'SCALE' qualifier was specified
!
                   READ ( UNIT=PLTMAP2, FMT=*, IOSTAT=IOS ) PLATE_SCALE
                   CALL FERR ( INT2(IOS), "GMAP(BATCH) Decoding scale "// &
     &                        "factor: "//PLTMAP2, INT2(0), INT2(0) )
              END IF
              IF ( STR(1:1) .NE. ' ' ) THEN
!
! ---------------- Reference date qualifier
!
                   IF ( INDEX ( PLTMAP, '.' ) .GT. 0 ) THEN
                        READ ( UNIT=STR, FMT='(I4,1X,I2,1X,I2)', IOSTAT=IOS) &
     &                          YEAR, MONTH, DAY
                      ELSE
                        READ ( UNIT=STR, FMT='(I2,I2,I2)', IOSTAT=IOS) &
     &                         YEAR, MONTH, DAY
                   END IF
                   CALL FERR ( INT2(IOS), "GMAP(BATCH) Decoding reference "// &
     &                 "date: "//STR, INT2(0), INT2(0) )
                   TIMEREF_YR = FJLDY ( MONTH, DAY, YEAR )/YEAR__TO__DAY
              END IF
            ELSE IF ( TOKEN .EQ. 'REF_DATE' ) THEN
!
! ----------- 'REF_DATE' keyword
!
               CALL ERR_LOG ( 8681, -2, 'GMAP', 'Keyword '// &
     &             'REF_DATE in $MAPPING section is not supported any more. '// &
     &             'It does not have sense. The reference epoch for site '// &
     &             'positions is specified in $STATION keyword of the $FLAGS '// &
     &             'section. The reference epoch for site position mapping '// &
     &             'file is specified inside the mapping file. Please, '// &
     &             'remove this keyword. Please, check whether you have '// &
     &             'specified the reference epochs for site positions in '// &
     &             'STATION keyword. Although if you omit the reference date '// &
     &             'in STATION keyword, Solve will take a default reference '// &
     &             'date and run, I bet you do not know which default epoch '// &
     &             ' it will pick up.' )
               CALL FLUSH ( 6 ) 
               STOP 'BATCH Abnomral termination'
             ELSE IF ( TOKEN .EQ. 'FIX' ) THEN
!
! ----------- 'FIX' KEYWORD
!
               CALL SPLITSTRING ( STRING, TOKEN, STRING )
               NUVEL_FIXED = TOKEN(1:4)
             ELSE IF(TOKEN.EQ.'NUTATION_SERIES') THEN
!
! ----------- 'NUTATION_SERIES' KEYWORD
!
               CALL GMAPF ( 'NUTATION_SERIES', STRING, TOKEN, INT2(15050), &
     &              NTSMAP, DUMFIL )
             ELSE IF ( TOKEN .EQ. 'NUTATION_MODEL' ) THEN
!
! 'NUTATION_MODEL' KEYWORD
!
!!
!! -- deselected since CALC 9.1 doesn' support partial derivatives wrt specific
!! -- nutation terms
!!
               CALL FERR ( INT2(2831), &
     &             'GMAP(BATCH) Keyword NUTATION_MODEL is not '// &
     &             'supported any more. Please remove this keyword from '// &
     &             'your control file', INT2(0), INT2(0) )
               CALL FLUSH ( 6 ) 
               STOP 'BATCH(gmap) Abonrmal termination'
             ELSE IF ( TOKEN .EQ. 'NUTATION_EXPANSION' ) THEN
               CALL SPLITSTRING ( STRING, TOKEN, STRING )
               IF ( TOKEN .EQ. 'NONE' ) THEN
                    CONTINUE
                  ELSE IF ( TOKEN .EQ. 'WAHR1980' ) THEN
                    CONTINUE
                  ELSE IF ( TOKEN .EQ. 'IERS1996' ) THEN
                    CONTINUE
                  ELSE IF ( TOKEN .EQ. 'REN2000' ) THEN
                    CONTINUE
                  ELSE IF ( TOKEN .EQ. 'MHB2000' ) THEN
                    CONTINUE
                  ELSE
                    CALL FERR ( INT2(2831), 'GMAP(BATCH) Unsupported '// &
     &                  'qualifier in keyword NUTATION_EXPANSION: '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' -- one of NONE, WAHR1980, '// &
     &                  'IERS1996, REN2000, MHB2000 was expected', &
     &                  INT2(0), INT2(0) )
                    CALL FLUSH ( 6 ) 
                    STOP 'BATCH(gmap) Abonrmal termination'
               END IF
             ELSE IF ( TOKEN .EQ. 'VELOCITIES' ) THEN
!
! ------------ 'VELOCITIES' KEYWORD
! ------------ Note: Can have either Station Velocities or Plate_Model line,
! ------------ not both.
!
               CALL GMAPF ( 'VELOCITIES', STRING, TOKEN, INT2(15070), VELMAP, &
     &                       VELMAP2 )
               PLTMAP = 'NONE' ! to indicate Station velocity is used
               IF ( TOKEN .EQ. 'REF_DATE' ) THEN
!
! ----------------- Get the value of the qualifier REF_DATE
!
                    CALL SPLITSTRING ( STRING, TOKEN, STRING )
                    IF ( TOKEN .EQ. 'NONE' ) THEN
                         TIMEREF_YR = J2000__JD/YEAR__TO__DAY
                      ELSE IF ( INDEX ( TOKEN, '.' ) .GT. 0 ) THEN
                         IF ( ILEN(TOKEN) .LT. LEN(DEF_DATE) ) THEN
                              TOKEN(ILEN(TOKEN)+1:) = DEF_DATE(ILEN(TOKEN)+1:)
                         END IF
                         IUER = -1
                         CALL DATE_TO_TIME ( TOKEN, MJD, SEC, IUER )
                         IF ( IUER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8682, -2, 'GMAP', &
     &                            'Wrong qualifier of the REF_DATE keyword: '// &
     &                             TOKEN )
                              CALL FLUSH ( 6 ) 
                              STOP 'GMAP(BATCH) Abnormal termination'
                         END IF
                         TIMEREF_YR = (MJD - J2000__MJD + SEC/86400.0 + &
     &                                 J2000__JD )/YEAR__TO__DAY
                      ELSE IF ( TOKEN .NE. 'NONE' ) THEN
                         READ ( UNIT=TOKEN(1:6), FMT='(3I2)', IOSTAT=IOS ) &
     &                          YEAR, MONTH, DAY
                         IF ( IOS .NE. 0 ) THEN
                              CALL ERR_LOG ( 8683, -2, 'GMAP', &
     &                            'Wrong format of the qualifier of the '// &
     &                            'REF_DATE keyword: '//TOKEN )
                              CALL FLUSH ( 6 ) 
                              STOP 'GMAP(BATCH) Abnormal termination'
                         END IF
                         TIMEREF_YR = FJLDY(MONTH, DAY, YEAR)/YEAR__TO__DAY
                    END IF
                    CALL CLRCH ( VELMAP2 )
                ENDIF
                IF ( TOKEN .EQ. 'SCALE' ) CALL CLRCH ( VELMAP2 )
             ELSE IF ( TOKEN .EQ. 'SPLINE_DISPLACEMENTS' ) THEN
                IF ( FL_BSP ) THEN
                     CALL FERR ( INT2(4040), 'BATCH(gmap) Keyword '// &
     &                   'SPLINE_MOTION used twice', INT2(0), INT2(0) )
                END IF
!
                IUER = -1
                CALL PARSE_BSP ( STRING, L_BSP, ADR_BSP, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8642, -2, 'GFLAGS', 'Error in parsing '// &
     &                   'the keyword SPLINE_POS' )
                     WRITE ( 6, * ) 'BATCH: abnormal termination'
                     CALL EXIT ( 1 )
                END IF
!
                FL_BSP = .TRUE.
             ELSE IF ( TOKEN .EQ. 'EPISODIC_MOTION' ) THEN
!
! ----------- 'EPISODIC_MOTION' keyword
!
               CALL GMAPF ( 'EPISODIC_MOTION', STRING, TOKEN, INT2(15080), &
     &              ESMMAP, DUMFIL )
             ELSE IF ( TOKEN .EQ. 'PIECE_WISE_STA' ) THEN
!
! ------------ 'PIECE_WISE_STA' keyword
!
               CALL GMAPF ( 'PIECE_WISE_STA', STRING, TOKEN, INT2(15090), &
     &              PWCMAP, DUMFIL )
             ELSE IF ( TOKEN .EQ. 'SITPL' ) THEN
!
! ------------ 'SITPL' keyword
!
               SITPLMAP_DEF = SITPLMAP
               CALL GMAPF ( 'SITPL', STRING, TOKEN, INT2(15095), SITPLMAP, &
     &              DUMFIL )
               IF ( SITPLMAP .EQ. 'NONE' .OR. SITPLMAP .EQ. 'none' ) THEN
                    SITPLMAP = SITPLMAP_DEF
               END IF
               IF ( DUMFIL .NE. ' ' ) PLTMAP2 = 'xxx'
             ELSE IF ( TOKEN .EQ. 'ECCENTRICITY' ) THEN
!
! ------------ ECCENTRICITY Keyword
!
               CALL GMAPF ( 'ECCENTRICITY', STRING, TOKEN, INT2(15190), &
     &                       ECCMAP, DUMFIL )
             ELSE IF ( TOKEN .EQ. 'MEAN_GRADIENT' ) THEN
!
! ------------ MEAN_GRADIENT Keyword
!
               CALL GMAPF ( 'MEAN_GRADIENT', STRING, TOKEN, INT2(15190), &
     &                       MGRMAP, DUMFIL )
             ELSE IF ( TOKEN .EQ. 'METRIC_TENSOR' ) THEN
!
! ------------ METRIC_TENSOR Keyword
!
               IF ( METRIC_MAP(1:1) .NE. ' ' ) THEN
                    CALL FERR ( INT2(470), 'gmap(BATCH) Keyword '// &
     &                   TOKEN(1:16)//' was already used', INT2(0), INT2(0) )
                    CALL FLUSH ( 6 ) 
                    STOP 'Abnormal termination'
               END IF
!
! ------------ Read value and check it among allowed values
!
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
               IF ( TOKEN .EQ. IERS92__MET ) THEN
                    METRIC_MAP = IERS92__MET
                  ELSE IF ( TOKEN .EQ. GRS__MET ) THEN
                    METRIC_MAP = GRS__MET
                  ELSE IF ( TOKEN .EQ. TOPOCNTR__MET ) THEN
                    METRIC_MAP = TOPOCNTR__MET
                  ELSE IF ( TOKEN .EQ. NONE__MET ) THEN
                    METRIC_MAP = NONE__MET
                  ELSE
                    CALL FERR ( INT2(480), 'gmap(BATCH) Keyword METRIC_MAP '// &
     &                  'has a wrong value: '//TOKEN(1:16)//' -- one of '// &
     &                   IERS92__MET//', '//GRS__MET//', '//TOPOCNTR__MET//' or '// &
     &                   NONE__MET//' was expected', INT2(0), INT2(0) )
                    CALL FLUSH ( 6 ) 
                    STOP 'Abnormal termination'
                END IF
            ELSE IF ( TOKEN .EQ. 'POSITION_VARIATIONS' ) THEN
!
! ------------ Position variation keyword
!
               CALL SPLIT_STRING ( STRING, TOKEN, STRING )
               IF ( FL_POSVAR ) THEN
                    CALL FERR ( INT2(490), 'gmap(BATCH) Keyword '// &
     &                  'POSITION_VARIATIONS defined twice', INT2(0), INT2(0) )
                    CALL FLUSH ( 6 ) 
                    STOP 'BATCH(gmap) Abnormal termination'
               END IF
               CALL CLRCH ( STR )
               CALL TRAN  ( 11, TOKEN, STR )
               IF ( STR .EQ. 'NONE' ) THEN
                    CONTINUE
                  ELSE
                    IUER = -1
                    CALL GPOSVAR ( STRING, TOKEN, N_POSVAR, POSVAR_FIL, &
     &                             POSVAR_MOD, POSVAR_INT, POSVAR_USE, &
     &                             G_WARNING, FL_WARN_POSVAR, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         CALL FERR ( INT2(495), &
     &                       'gmap(BATCH) Error during parsing '// &
     &                       'keyword POSITION_VARIATIONS in $MAPPING section', &
     &                        INT2(0), INT2(0) )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
               END IF
               FL_POSVAR = .TRUE.
            ELSE IF ( TOKEN .EQ. 'HARMONIC_EOP' ) THEN
               CALL SPLIT_STRING ( STRING, TOKEN, STRING )
               IF ( TOKEN .EQ. 'NONE' ) THEN
                  ELSE
                    IF ( TOKEN .EQ. 'YES' .OR. TOKEN .EQ. 'Yes' .OR. &
     &                   TOKEN .EQ. 'yes' ) THEN
                         CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                    END IF
!
                    STAT_HEO  = HEO__REQUE
                    FINAM_HEO = TOKEN
                    IF ( INDEX ( FINAM_HEO, '/' ) .EQ. 0 ) THEN
                         FINAM_HEO = PRE_SAV_DIR(1:PRE_SV_LEN)//FINAM_HEO
                    END IF
!
! ----------------- Read the file with harmonic Earth orientation variations
!
                    IUER = -1
                    CALL GHEO ( FINAM_HEO, NAME_HEO, L_HEO, STAT_HEO, &
     &                          ADR_HEO, HEO_EPOCH_SEC, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8863, -2, 'GMAP', 'Error in an '// &
     &                       'attempt to read expansion of the harmonic '// &
     &                       'Earth orientation parameters from the file '// &
     &                        FINAM_HEO )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
                    STAT_HEO  = HEO__READ
               END IF
            ELSE IF ( TOKEN .EQ. 'ERM' ) THEN
               CALL SPLIT_STRING ( STRING, TOKEN, STRING )
               IF ( TOKEN .EQ. 'NONE' ) THEN
                    CONTINUE
                  ELSE
                    IF ( TOKEN .EQ. 'YES' .OR. TOKEN .EQ. 'Yes' .OR. &
     &                   TOKEN .EQ. 'yes' ) THEN
                         CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                    END IF
!
                    STAT_MERM  = HEO__REQUE
                    FINAM_MERM = TOKEN
                    IF ( INDEX ( FINAM_MERM, '/' ) .EQ. 0 ) THEN
                         FINAM_MERM = PRE_SAV_DIR(1:PRE_SV_LEN)//FINAM_MERM
                    END IF
!
! ----------------- Read the file with harmonic Earth orientation variations
!
                    IUER = -1
                    CALL GERM ( FINAM_MERM, L_MERM, ADR_MERM, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8864, -2, 'GMAP', 'Error in an '// &
     &                       'attempt to read expansion of the harmonic '// &
     &                       'Earth orientation parameters from the file '// &
     &                        FINAM_MERM )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
                    STAT_MERM  = HEO__READ
               END IF
            ELSE IF ( TOKEN .EQ. 'VTD_CONF' ) THEN
!
! ------------ Position variation keyword
!
               CALL SPLIT_STRING ( STRING, TOKEN, STRING )
               IF ( FL_VTD ) THEN
                    CALL FERR ( INT2(490), 'gmap(BATCH) Keyword '// &
     &                  'VTD_CONF defined twice', INT2(0), INT2(0) )
                    CALL FLUSH ( 6 ) 
                    STOP 'BATCH(gmap) Abnormal termination'
               END IF
               CALL CLRCH ( STR )
               CALL TRAN  ( 11, TOKEN, STR )
               IF ( STR .EQ. 'NONE' ) THEN
                    FL_VTD_GLB = .FALSE.
                    CONTINUE
                  ELSE
                    FL_VTD_GLB = .TRUE.
                    VTD_CONF_GLB = TOKEN
                    INQUIRE ( FILE=VTD_CONF_GLB, EXIST=LEX )
                    IF ( .NOT. LEX ) THEN
                         CALL ERR_LOG ( 8865, -2, 'GMAP', 'VTD '// &
     &                       'configuration file '// &
     &                        VTD_CONF_GLB(1:I_LEN(VTD_CONF_GLB))// &
     &                       ' specified in keyword VTD_CONF of $MAPPING '// &
     &                       'section is not found' )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
               END IF
               FL_VTD = .TRUE.
            ELSE IF ( TOKEN .EQ. 'EXTERNAL_TRP_DELAY' ) THEN
!
! ------------ EXTERNAL_TRP_DELAY keyword
!
               CALL SPLIT_STRING ( STRING, TOKEN, STRING )
               IF ( FL_TRP ) THEN
                    CALL FERR ( INT2(4100), 'gmap(BATCH) Keyword '// &
     &                  'EXTERNAL_TRP_DELAY defined twice', INT2(0), INT2(0) )
                    CALL FLUSH ( 6 ) 
                    STOP 'BATCH(gmap) Abnormal termination'
               END IF
               CALL CLRCH ( STR )
               CALL TRAN  ( 11, TOKEN, STR )
               IF ( STR .EQ. 'NONE'  .OR. STR .EQ. 'NO' ) THEN
                    CALL CLRCH ( TRP_DIR )
                    TRP_USE   = UNDF__TRP
                    N_FIL_TRP = 0
                    ADR_TRP_FIL_BUF = 0
                    STS_TRP_FIL     = 0
                    ADR_TRP_SES_BUF = 0
                    STS_TRP_SES     = 0
                    ADR_TRP = 0
                    STS_TRP = 0
                    CONTINUE
                  ELSE
                    IF ( STR == 'USE' ) THEN
                         TRP_USE = USE__TRP
                      ELSE IF ( STR =='REQUIRE' ) THEN
                         TRP_USE = REQ__TRP
                      ELSE
                         CALL FERR ( INT2(4102), 'gmap(BATCH) Wrong '// &
     &                       'qualifier after the keyword '// &
     &                       'EXTERNAL_TRP_DELAY '//STR(1:I_LEN(STR))// &
     &                       ' while NONE, USE or REQUIRE were expected', &
     &                        INT(0), INT2(0) )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
                    CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                    CALL CLRCH ( STR )
                    CALL TRAN  ( 11, TOKEN, STR )
                    IF ( STR .NE. 'DIRECTORY' ) THEN
                         CALL FERR ( INT2(4102), 'gmap(BATCH) Wrong '// &
     &                       'qualifier after the keyword '// &
     &                       'EXTERNAL_TRP_DELAY '//STR(1:I_LEN(STR))// &
     &                       ' while DIRECTORY was expected', INT(0), INT2(0) )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
                    CALL SPLIT_STRING ( STRING, TRP_DIR, STRING )
                    IF ( TRP_DIR(I_LEN(TRP_DIR):I_LEN(TRP_DIR)) == '/' ) THEN
                         TRP_DIR(I_LEN(TRP_DIR):I_LEN(TRP_DIR)) = ' '
                    END IF
                    DIR_DESC = OPENDIR ( TRP_DIR(1:I_LEN(TRP_DIR))//CHAR(0) )
                    IF ( DIR_DESC == 0 ) THEN
                         CALL GERROR  ( STR )
                         CALL ERR_LOG ( 8866, -2, 'GMAP', 'Cannot read '// &
     &                       'directory for external model of the '// &
     &                       'atmosphere path delay specified in the '// &
     &                       ' keyword EXTERNAL_TRP_DELAY of $MAPPING '// &
     &                       'section '//TRP_DIR(1:I_LEN(TRP_DIR))// &
     &                       ' -- '//STR )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                       ELSE
                         CALL CLOSEDIR ( %VAL(DIR_DESC) )
                    END IF
               END IF
               FL_TRP = .TRUE.
            ELSE IF ( TOKEN .EQ. 'ANTENNA_THERMAL' ) THEN
               CALL SPLIT_STRING ( STRING, TOKEN, STRING )
               IF ( FL_ANT ) THEN
                    CALL FERR ( INT2(4110), 'gmap(BATCH) Keyword '// &
     &                  'ATNENNA_THERMAL defined twice', INT2(0), INT2(0) )
                    CALL FLUSH ( 6 ) 
                    STOP 'BATCH(gmap) Abnormal termination'
               END IF
               CALL CLRCH ( STR )
               CALL TRAN  ( 11, TOKEN, STR )
               IF ( STR .EQ. 'NONE'  .OR. STR .EQ. 'NO' ) THEN
                    ATD_USE     = ATD__UNDF    
                    ATD_MEA_USE = 0 
                    CALL CLRCH ( ATD_FIL )
                    CALL CLRCH ( ATD_MEA_FIL )
                  ELSE
                    IF ( STR .NE. 'MODEL' ) THEN
                         CALL FERR ( INT2(4112), 'gmap(BATCH) Wrong 1st '// &
     &                       'qualifier after the keyword ATNENNA_THERMAL '// &
     &                       'was found: '//STR(1:I_LEN(STR))//' while '// &
     &                       'MODEL was expected', INT2(0), INT2(0) )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
!
                    CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                    CALL TRAN  ( 11, TOKEN, STR )
                    IF ( STR == 'INSTANT' ) THEN
                         ATD_USE = ATD__INSTANT
                       ELSE IF ( STR == 'AVERAGE' ) THEN
                         ATD_USE = ATD__AVERAGE
                       ELSE IF ( STR == 'LAGGED' ) THEN
                         ATD_USE = ATD__LAGGED
                       ELSE 
                         CALL FERR ( INT2(4114), 'gmap(BATCH) Wrong 2nd '// &
     &                       'qualifier after the keyword ATNENNA_THERMAL '// &
     &                       'was found: '//STR(1:I_LEN(STR))//' while '// &
     &                       'INSTANT, AVERAGE or LAGGED were expected', &
     &                        INT2(0), INT2(0)  )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
!
                    CALL SPLIT_STRING ( STRING, ATD_FIL, STRING )
                    INQUIRE ( FILE=ATD_FIL, EXIST=LEX )
                    IF ( .NOT. LEX ) THEN
                         CALL ERR_LOG ( 8870, -2, 'GMAP', 'Wrong 3nd '// &
     &                       'qualifier after the keyword ATNENNA_THERMAL '// &
     &                       '-- file '//ATD_FIL(1:I_LEN(ATD_FIL))// &
     &                       ' does not exist' )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
!
                    CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                    CALL TRAN  ( 11, TOKEN, STR )
!
                    IF ( STR == '\' ) CALL SPLIT_STRING ( STRING, STR, STRING )
                    IF ( STR .NE. 'INSITU' ) THEN
                         CALL FERR ( INT2(4116), 'gmap(BATCH) Wrong 4th '// &
     &                       'qualifier after the keyword ATNENNA_THERMAL '// &
     &                       'was found: '//STR(1:I_LEN(STR))//' while '// &
     &                       'INSITU was expected', INT2(0), INT2(0) )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
!
                    CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                    CALL TRAN  ( 11, TOKEN, STR )
                    IF ( STR == 'NO'  .OR.  STR == 'NONE' ) THEN
                         ATD_MEA_USE = ATD__NO
                       ELSE IF ( STR == 'USE' ) THEN
                         ATD_MEA_USE = ATD__USE
                       ELSE IF ( STR == 'REQUIRED' ) THEN
                         ATD_MEA_USE = ATD__REQ
                       ELSE 
                         CALL FERR ( INT2(4118), 'gmap(BATCH) Wrong 5th '// &
     &                       'qualifier after the keyword ATNENNA_THERMAL '// &
     &                       'was found: '//STR(1:I_LEN(STR))//' while '// &
     &                       'NO, USE or REQUIRED were expected', &
     &                        INT2(0), INT2(0) )
                         CALL FLUSH ( 6 ) 
                         STOP 'BATCH(gmap) Abnormal termination'
                    END IF
!
                    IF ( ATD_MEA_USE .NE. ATD__NO ) THEN
                         CALL SPLIT_STRING ( STRING, ATD_MEA_FIL, STRING )
                         INQUIRE ( FILE=ATD_MEA_FIL, EXIST=LEX )
                         IF ( .NOT. LEX ) THEN
                              CALL ERR_LOG ( 8871, -2, 'GMAP', 'Wrong 6th '// &
     &                            'qualifier after the keyword ATNENNA_THERMAL '// &
     &                            '-- file '//ATD_MEA_FIL(1:I_LEN(ATD_MEA_FIL))// &
     &                            ' does not exist' )
                              CALL FLUSH ( 6 ) 
                              STOP 'BATCH(gmap) Abnormal termination'
                         END IF
                    END IF
               END IF
!               
               FL_ANT = .TRUE.
            ELSE
!
! ----------- Something that isn't suppose to be there
!
              CALL FERR ( INT2(15290), 'UNKNOWN KEYWORD '//TOKEN(1:20), &
     &                    INT2(0), INT2(0) )
          ENDIF
        ENDDO
        LENGTH=CFREAD(STRING)
      ENDDO
!
      IF ( METRIC_MAP(1:1) .EQ. ' ' ) METRIC_MAP = NONE__MET
!
! === Now that this section is finished, what now?
!
      IF ( STAMAP .EQ. ' '  .OR.  PLTMAP .EQ. ' '  .OR. &
     &     SRCMAP .EQ. ' '  .OR.  EOPMAP .EQ. ' '  .OR. &
     &     NTSMAP .EQ. ' '  .OR.  VELMAP .EQ. ' '       ) THEN
           IF ( FL_VTD ) THEN
                CALL CFUNRD ( LENGTH, STRING )
              ELSE
                CALL FERR ( INT2(2095), 'BATCH(gmap): Missing keywords '// &
     &                                  'from $mapping', INT2(0), INT2(0) )
           END IF
         ELSE
!
! -------- Put back the starting line of the next section of control file
!
           CALL CFUNRD ( LENGTH, STRING )
      ENDIF
!
      IF ( N_POSVAR .GT. 0  .AND.  TRAIN ) THEN
           CALL ERR_LOG ( 8867, -2, 'GMAP', 'You cannot apply '// &
     &         'models of site position variations in TRAIN mode. Please, '// &
     &         'specify TRAIN NO in the $SETUP section of your control file' )
           CALL FLUSH ( 6 ) 
           STOP 'BATCH (gmap) abnormal termination'
      END IF
!
      IF ( KHFEOPCAL  .AND.  STAT_HEO .NE. HEO__UNDF ) THEN
           CALL ERR_LOG ( 8868, -2, 'GMAP', 'You cannot apply '// &
     &         'both HI_FREQ_EOP and HARMONIC_EOP in $MAPPING section. '// &
     &         'These two keywords are mutually exclusive' )
           CALL FLUSH ( 6 ) 
           STOP 'BATCH (gmap) abnormal termination'
      END IF
!
      TIME0 = TIMEREF_YR
      STASUB2_CHR = STAMAP2
      VELSUB2_CHR = VELMAP2
      PLTSUB2_CHR = PLTMAP2
      ECCSUB_CHR  = ECCMAP
      MGRSUB_CHR  = MGRMAP
      METSUB_CHR  = METRIC_MAP
      PLATE_SCL   = PLATE_SCALE
!
#ifdef INTEL || GNU
      SIZEOF_VTD = SIZEOF(VTD_PTR)
      SIZEOF_TRP = SIZEOF(TRP_PTR)
      SIZEOF_ATD = SIZEOF(ATD_PTR)
#else
!
! --- HPUX fortran does not support SIZEOF of the pointer which is not
! --- allocated -- it returns zero.
! --- SUN Fortran does not support SIZEOF even if the pointer is allocated.
! --- It returns zero.
! 
      IF ( FL_VTD_GLB ) THEN
           ALLOCATE ( VTD_PTR )
           SIZEOF_VTD = SIZEOF(VTD_PTR)
           DEALLOCATE ( VTD_PTR )
      END IF
!
      IF ( TRP_USE .NE. UNDF__TRP ) THEN
           ALLOCATE ( TRP_PTR )
           SIZEOF_TRP = SIZEOF(TRP_PTR)
           DEALLOCATE ( TRP_PTR )
      END IF
      IF ( ATD_USE .NE. ATD__UNDF ) THEN
           ALLOCATE ( ATD_PTR )
           SIZEOF_ATD = SIZEOF(ATD_PTR)
           DEALLOCATE ( ATD_PTR )
      END IF
#endif
      MEM_LEN = 0 
      MEM_ADR = 0 
!
      IF ( FL_VTD_GLB ) THEN
           IUER = -1
           CALL GRAB_MEM ( IUER, MEM_LEN(1), MEM_ADR(1), 1, SIZEOF_VTD, VTD_ADR )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( SIZEOF_VTD, STR )
                IUER = -1
                CALL ERR_LOG ( 8869, IUER, 'GMAP', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                CALL FLUSH ( 6 ) 
                STOP 'BATCH (gmap) abnormal termination'
           END IF
!
           VTD_STATUS = VTD__ALLC
           IUER = -1
           CALL VTD_INIT ( %VAL(VTD_ADR), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8870, -2, 'GMAP', 'Failure to initialize'// &
     &              'vtd object' )
                CALL FLUSH ( 6 ) 
                STOP 'BATCH (gmap) abnormal termination'
           END IF
           VTD_STATUS = VTD__INIT
         ELSE 
           VTD_STATUS = 0
      END IF
!
      IF ( TRP_USE .NE. UNDF__TRP ) THEN
           IUER = -1
           CALL GRAB_MEM ( IUER, MEM_LEN(2), MEM_ADR(2), 1, SIZEOF_TRP, ADR_TRP )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( SIZEOF_VTD, STR )
                IUER = -1
                CALL ERR_LOG ( 8870, IUER, 'GMAP', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                CALL FLUSH ( 6 ) 
                STOP 'BATCH (gmap) abnormal termination'
           END IF
!
           CALL NOUT8 ( SIZEOF_TRP, %VAL(ADR_TRP) )
           STS_TRP = ALLO__TRP
         ELSE 
           STS_TRP = 0
      END IF
      IF ( ATD_USE .NE. ATD__UNDF ) THEN
           IUER = -1
           CALL GRAB_MEM ( IUER, MEM_LEN(3), MEM_ADR(3), 1, SIZEOF_ATD, ATD_ADR )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( SIZEOF_ATD, STR )
                IUER = -1
                CALL ERR_LOG ( 8871, IUER, 'GMAP', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                CALL FLUSH ( 6 ) 
                STOP 'BATCH (gmap) abnormal termination'
           END IF
!
           CALL NOUT8 ( SIZEOF_ATD, %VAL(ATD_ADR) )
           IUER = -1
           CALL ANTI_PARSE ( ATD_FIL, %VAL(ATD_ADR), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( SIZEOF_ATD, STR )
                IUER = -1
                CALL ERR_LOG ( 8872, IUER, 'GMAP', 'Failure to parse the '// &
     &              'file with antenna information '//ATD_FIL )
                CALL FLUSH ( 6 ) 
                STOP 'BATCH (gmap) abnormal termination'
           END IF
           ATD_STS = ANTI__LOAD 
         ELSE 
           ATD_STS = ANTI__UNDF
      END IF
!
      IF ( EOPMAP(1:1) == ' ' ) EOPMAP = 'NONE'
      CALL USE_GLBFIL   ( 'W'  )
      CALL USE_GLBFIL_4 ( 'WC' )
!
      RETURN
      END  SUBROUTINE  GMAP  !#!#
