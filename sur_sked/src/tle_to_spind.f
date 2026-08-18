      PROGRAM    TLE_TO_SPIND
! ************************************************************************
! *                                                                      *
! *   Program TLE_TO_SPIND generates a file with positions of GNSS       *
! *   Earth orbiting satellites in a form pseudo-sources using orbits    *
! *   in TLE format. A pseudo-source has a name, right ascension,        *
! *   declination, distance, Cartesian positions, epoch and the range    *
! *   of validity. A J2000 source name contains of two parts:            *
! *   three-letter long IGS satellite acronym, underscore and            *
! *   a 6-character long epoch index. A B1950 source name also has two   *
! *   parts: the three-letter long IGS satellite acronym, underscore and *
! *   a 4-character long 26-mal epoch index.                             *
! *                                                                      *
! *   Input ephemeride files are sought in /tle directory.               *
! *   TLE_TO_SPIND uses file with GNSS satellite list that is provided   *
! *   by VTD library.                                                    *
! *                                                                      *
! *   Usage: tle_to_spind sat_list date_beg date_end dur tim_step nsta   *
! *                       output_file                                    *
! *                                                                      *
! *   sat_list    -- comma-separated list of 3-letter long satellite     *
! *                  acronyms which ephemeride will be used.             *
! *   date_beg    -- Start date of the output position file in format    *
! *                  YYYY.MM.DD:hh:dd:ss                                 *
! *   date_end    -- Stop  date of the output position file in format    *
! *                  YYYY.MM.DD:hh:dd:ss                                 *
! *   time_step   -- Time step of the output position file in seconds.   *
! *   dur         -- Field observation duration (in seconds) in  the     *
! *                  output file.                                        *
! *   nsta        -- Field the number of stations in the output file.    *
! *   output_file -- Name of the output file.                            *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 23-JAN-2025  TLE_TO_SPIND  v2.0 (d) L. Petrov  05-OCT-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'sur_sked_local.i'
      INCLUDE   'tle_sgp4.i'
      INCLUDE   'vtd_local.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( EPH__TYPE  ), POINTER :: EPH(:)
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4    M_FIL, MO, M_NOD, M_SAT
      PARAMETER  ( M_FIL = 1024*1024 )
      PARAMETER  ( MO    = 6*1024*1024 )
      PARAMETER  ( M_NOD = 16384       )
      PARAMETER  ( M_SAT = 512         )
      CHARACTER  SAT_LIST*1024, DATE_BEG*32, DATE_END*32, FILOUT*128, &
     &           C_SAT(SUR__M_SAT)*3, C_ACR(SUR__M_SAT)*19, DUR_STR*5, &
     &           STR*128, C_FIL(M_FIL)*128, DIRIN*128, BNAME*8, &
     &           FINAM*128, FIL_GNSS_TABLE*128, BUF_GNSS(2*SUR__M_SAT)*128, &
     &           FIL_TLE(SUR__M_SAT)*128, C_ACR_MIS*1024, C_EPH_MIS*1024, &
     &           SAT_STR*10, COM_LINE*1024, TLE_DATE*19, STR_NSTA*2, &
     &           STR1*128
      CHARACTER   NERS_CONFIG*128, HOME_DIR*128
      CHARACTER, ALLOCATABLE :: OUT(:)*1024
      INTEGER*8  DIR_DESC(16)
      LOGICAL*1  FL_FOUND_ACR, FL_FOUND_EPH, LEX
      REAL*8     TAI_BEG, TAI_END, TIM_STEP, TAI_OBS, COO_SAT(3), VEL_SAT(3), &
     &           DST, RA, DEC, DUR, TLE_TIM, TIM_DIFF, TIM_DIFF_MIN, AZ, EL, &
     &           ARG_NOD(M_NOD), VAL_NOD(M_NOD,3,M_SAT), SPL_NOD(M_NOD,3,M_SAT), &
     &           TMP_ARR(M_NOD),  TIM_SPAN, TIM_ARG
      REAL*8     TIM_NOD_DEF, TIM_EXTRA
      PARAMETER  ( TIM_NOD_DEF =  500.0D0 )
      PARAMETER  ( TIM_EXTRA   = 2000.0D0 )
      INTEGER*4  L_NOD_MIN
      PARAMETER  ( L_NOD_MIN = 11 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           IB, IE, IL, NO, IX, IDAY, IS, N_SAT, &
     &           MJD_BEG, MJD_END, MJD_OBS, LEV, LIND, &
     &           IND(2,SUR__M_SAT), L_FIL, N_TIM, N_GNSS, N_ACR_MIS, &
     &           N_EPH_MIS, TLE_MJD, N_STA, L_NOD, IUER
      LOGICAL*1  FL_INTRP
      REAL*8,    EXTERNAL :: FSPL8
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN, IXMN8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32, GET_CDATE*19
!
      FL_INTRP = .TRUE.
      DIRIN = '/tle'
      ALLOCATE ( OUT(MO) )
!
      IF ( IARGC() < 7 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: tle_to_spind sat_list date_beg date_end tim_step dur nsta output_file'
           CALL EXIT ( 1 )
         ELSE
!
! -------- Parse input arguments
!
           CALL GETARG ( 1, SAT_LIST )
           CALL TRAN   ( 11, SAT_LIST, SAT_LIST ) ! Convert to the upper case
!
           IUER = -1
           CALL EXWORD ( SAT_LIST, SUR__M_SAT, N_SAT, IND, ',', IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4801, IUER, 'TLE_TO_SPIND', 'Error in parsing '// &
     &              'the satellite list' )
                CALL EXIT ( 1 )
           END IF
           DO 410 J1=1,N_SAT
              C_SAT(J1) = SAT_LIST(IND(1,J1):IND(2,J1))
              CALL TRAN ( 11, C_SAT(J1), C_SAT(J1) )
 410       CONTINUE 
!
           CALL GETARG ( 2, DATE_BEG )
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TAI_BEG, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4802, IUER, 'TLE_TO_SPIND', 'Error in parsing '// &
     &              'the start date '//DATE_BEG )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 3, DATE_END )
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END, MJD_END, TAI_END, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4803, IUER, 'TLE_TO_SPIND', 'Error in parsing '// &
     &              'the start date '//DATE_END )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 4, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) THEN
                STR = TRIM(STR)//'.0'
           END IF
           READ ( UNIT=STR, FMT='(F12.3)' ) TIM_STEP
!
           CALL GETARG ( 5, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) THEN
                STR = TRIM(STR)//'.0'
           END IF
           READ ( UNIT=STR, FMT='(F12.3)' ) DUR
!
           CALL GETARG ( 6, STR_NSTA )
           CALL CHASHR (    STR_NSTA )
           CALL CHIN   ( STR, N_STA  )
!           
           CALL GETARG ( 7, FILOUT   )
      END IF
!
! --- Allocate memory for satellute ephemeride
!
      ALLOCATE ( EPH(N_SAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4804, IUER, 'TLE_TO_SPIND', 'Error in an attempt '// &
     &         'to allocate memory for array EPH' )
           CALL EXIT ( 1 )
      END IF
!
! --- Get NERS Environment variables
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
! ------------- Third, check for the system-wide ners configuration file 
!
                NERS_CONFIG = NERS__CONFIG
           END IF
      END IF
!
! --- Initialise NERS
!
      IUER = -1
      CALL NERS_INIT ( 'NERS_CONFIG', NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1930, IUER, 'TLE_TO_SPIND', &
     &         'Error in initializing NERS data structure' )
           CALL EXIT ( 1 )
      END IF
!
! --- Load NERS
!
      IUER = -1
      CALL NERS_LOAD ( NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1931, IUER, 'TLE_TO_SPIND', &
     &         'Error in an attempt to retrieve NERS forecast '//     &
     &         'parameters from the remote server' )
           CALL EXIT ( 1 )
      END IF
!
! --- Traverse the directory with the input ephmeride 
! --- Array C_FIL or L_FIL long will hold all ephemeride files
!
      LEV   = 0
      L_FIL = 0
      DO 420 J2=1,M_FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRIN, FINAM )
         IF ( LEV < 1 ) GOTO 810
         IF ( IS .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4805, IUER, 'TLE_TO_SPIND', 'Error in '// &
     &                 'reading input directory '//DIRIN )
              CALL EXIT ( 1 )
         END IF
         IF ( INDEX ( FINAM, '#' ) .GT. 0 ) GOTO 420
         IF ( INDEX ( FINAM, '!' ) .GT. 0 ) GOTO 420
         IF ( INDEX ( FINAM, '~' ) .GT. 0 ) GOTO 420
         L_FIL = L_FIL + 1
         IF ( L_FIL .GE. M_FIL ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_FIL, STR )
              IUER = -2
              CALL ERR_LOG ( 4806, IUER, 'TLE_TO_SPIND', 'Error in '// &
     &                 'reading input directory '//TRIM(DIRIN)// &
     &                 ' -- that directory has more than '//TRIM(STR)//' files' )
              CALL EXIT ( 1 )
         END IF
         C_FIL(L_FIL) = FINAM
 420  CONTINUE 
 810  CONTINUE 
!
      IF ( L_FIL < 1 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4807, IUER, 'TLE_TO_SPIND', 'Did  not find '// &
     &         'ephemeride files in the input directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Sort these list
!
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
!
! --- Read the file with GNSS satellite list
!
      FIL_GNSS_TABLE = VTD__DATA//'/gnss_sat_table.txt'
      IUER = -1
      CALL RD_TEXT ( FIL_GNSS_TABLE, 2*SUR__M_SAT, BUF_GNSS, N_GNSS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4808, IUER, 'TLE_TO_SPIND', 'Did  not find '// &
     &         'ephemeride files in the input directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
      N_TIM = ((MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG))/TIM_STEP
!
      N_ACR_MIS = 0
      N_EPH_MIS = 0
      CALL CLRCH ( C_ACR_MIS )
      CALL CLRCH ( C_EPH_MIS )
!
! --- Cycle over the satellites which ephemeride we would like 
! --- to compute
!
      DO 430 J3=1,N_SAT
!
! ------ Search for the acronym in the table SUR__GNSS_SAT_TABLE
!
         FL_FOUND_ACR = .FALSE.
         DO 440 J4=1,N_GNSS
            IF ( BUF_GNSS(J4)(1:1) == '#' ) GOTO 440
            IF ( BUF_GNSS(J4)(1:3) == C_SAT(J3) ) THEN
                 FL_FOUND_ACR = .TRUE.
                 C_ACR(J3) = BUF_GNSS(J4)(17:35)
            END IF
 440     CONTINUE 
!
! ------ Search for the ephemeride file
!
         FL_FOUND_EPH = .FALSE.
         IF ( FL_FOUND_ACR ) THEN
              TIM_DIFF_MIN = 1.D12
              DO 450 J5=1,L_FIL
                 IL = ILEN(C_FIL(J5))
                 IF ( IL < 10 ) GOTO 450
                 IF ( C_FIL(J5)(IL-3:IL) .NE. '.tle' ) GOTO 450
!
! -------------- Extract the date from the ephemeride file
!
                 IF ( C_FIL(J5)(IL-8:IL-8) == '_' ) THEN
                      TLE_DATE = C_FIL(J5)(IL-16:IL-13)//'.'//C_FIL(J5)(IL-12:IL-11)//'.'// &
     &                           C_FIL(J5)(IL-10:IL-6)//':'//C_FIL(J5)(IL-5:IL-4)//':'// &
     &                           '00'
                    ELSE
                      TLE_DATE = C_FIL(J5)(IL-18:IL-15)//'.'//C_FIL(J5)(IL-14:IL-13)//'.'// &
     &                           C_FIL(J5)(IL-12:IL-8)//':'//C_FIL(J5)(IL-7:IL-6)//':'// &
     &                           C_FIL(J5)(IL-5:IL-4)
                 END IF
!
! -------------- Parse the date of the ephemeride date
!
                 IUER = -1
                 CALL DATE_TO_TIME ( TLE_DATE, TLE_MJD, TLE_TIM, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 4810, IUER, 'TLE_TO_SPIND', 'Malformed name '// &
     &                    'of epehemerid file '//C_FIL(J5) )
                      CALL EXIT ( 1 )
                 END IF
!
! -------------- Compute the interval between the ephemeride file and the middle
! -------------- epoch of the output file
!
                 TIM_DIFF = DABS( ((MJD_BEG + MJD_END)*86400.0D0 + (TAI_BEG + TAI_END))/2.0D0 - &
     &                             (TLE_MJD*86400.0D0 + TLE_TIM) )
                 IF ( INDEX ( C_FIL(J5), TRIM(C_ACR(J3)) ) > 0 ) THEN
                      IF ( TIM_DIFF < TIM_DIFF_MIN ) THEN
!
! ------------------------ Store the file name that has the epoch that is the closest to 
! ------------------------ the middle epoch of the output file
!
                           TIM_DIFF_MIN = TIM_DIFF  
                           FIL_TLE(J3)  = C_FIL(J5)
                           FL_FOUND_EPH = .TRUE.
                      END IF
                 END IF
 450          CONTINUE 
            ELSE
!
! ----------- Insert this satellite acronym to the list of 
! ----------- missed satellites
!
              N_ACR_MIS = N_ACR_MIS + 1
              C_ACR_MIS = TRIM(C_ACR_MIS)//','//C_SAT(J3)
         END IF
         IF ( .NOT. FL_FOUND_EPH ) THEN
!
! ----------- Hmm! We did not find ephemeride of this satellite.
! ----------- Insert this satellite acronym to the list of 
! ----------- missed satellites
!
              N_EPH_MIS = N_EPH_MIS + 1
              C_EPH_MIS = TRIM(C_EPH_MIS)//','//C_SAT(J3)
         END IF
 430  CONTINUE 
!
! --- Report missing satellites
!
      IF ( N_ACR_MIS > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_ACR_MIS, STR )
           C_ACR_MIS = C_ACR_MIS(2:)
           IUER = -2
           CALL ERR_LOG ( 4809, IUER, 'TLE_TO_SPIND', 'Did  not find '// &
     &         'celestrak acronyms for '//TRIM(STR)//' satellites: '//C_ACR_MIS )
           CALL EXIT ( 1 )
      END IF
!
! --- Report missing ephemerides
!
      IF ( N_EPH_MIS > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_EPH_MIS, STR )
           C_EPH_MIS = C_EPH_MIS(2:)
           IUER = -2
           CALL ERR_LOG ( 4810, IUER, 'TLE_TO_SPIND', 'Did  not find '// &
     &         'ephemeride for '//TRIM(STR)//' satellites: '//C_EPH_MIS )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute the time span of interpolation, consideringt the extra
! --- time at the beginnign and the end of the ephemeride for spinn-off
!
      TIM_SPAN = (MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG) + &
     &           2*TIM_EXTRA
      L_NOD    = TIM_SPAN/TIM_NOD_DEF
      IF ( L_NOD < L_NOD_MIN ) L_NOD = L_NOD_MIN
      IF ( L_NOD > M_NOD ) THEN
           IUER = -2
           CALL ERR_LOG ( 4810, IUER, 'TLE_TO_SPIND', 'Trap of internal control: '// &
     &         'too many splinie nodes. Please increse M_MOD or check requested '// &
     &         'start/stop dates of the output ephemeride' )
           CALL EXIT ( 1 )
      END IF
!
! --- Make the array of nodes for spline interpoplation. 
! --- 0-epoch of ARG_NOD array corresponds to date_beg epoch
!
      DO 460 J6=1,L_NOD
         ARG_NOD(J6) = -TIM_EXTRA + (J6-1)*TIM_SPAN/(L_NOD-1)
 460  CONTINUE 
!
! --- Prepare the header of the output file
!
      WRITE ( UNIT=DUR_STR, FMT='(F5.1)' ) DUR
      CALL GET_COMMAND ( COM_LINE )
      NO = 0
      NO = NO + 1 ; OUT(NO) = '# CATRES Flux and Spectral index file. Format version of 2004.12.18'
      NO = NO + 1 ;  OUT(NO) = '# DURATION, PRIORITY AND NOBS'
      NO = NO + 1 ;  OUT(NO) = '#'
      NO = NO + 1 ;  OUT(NO) = '# Positions of Near Earth Zone objects'
      NO = NO + 1 ;  OUT(NO) = '#'
      NO = NO + 1 ;  OUT(NO) = '# Command '//TRIM(COM_LINE)
      NO = NO + 1 ;  OUT(NO) = '#'
!
      DO 470 J7=1,N_SAT
         NO = NO + 1 ;  OUT(NO) = '# Satellite: '//C_SAT(J7)//' '//C_ACR(J7)// &
     &                            ' Ephemeride: '//TRIM(FIL_TLE(J7))
         IF ( FL_INTRP ) THEN
!
! ----------- Parse tyhe input ephemeride file
!
              IUER = -1
              CALL TLE_PARSER ( NERS, FIL_TLE(J7), EPH(J7), IUER )     
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4811, IUER, 'TLE_TO_SPIND', 'Error in an attempt '// &
     &                'to parse a satellite ephemeride file '//FIL_TLE(J7) )
                   CALL EXIT ( 1 )
              END IF
!
! ----------- Fill array of satellite positions computed from the two-line ephemeride
! ----------- on epochs of ARG_NOD
!
              DO 480 J8=1,L_NOD
!
! -------------- Get time arguments for the J8-th node epoch
!
                 TAI_OBS = TAI_BEG + ARG_NOD(J8)
                 IDAY    = TAI_OBS/86400.0D0
                 TAI_OBS = TAI_OBS - IDAY*86400.D0
                 MJD_OBS = MJD_BEG + IDAY
!
! -------------- Compute satellite Cartesian coordinates 
!
                 IUER = -1
                 CALL TLE_TO_CRS ( NERS, EPH(J7), MJD_OBS, TAI_OBS, COO_SAT, VEL_SAT, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 4812, IUER, 'TLE_TO_SPIND', 'Error in an attempt '// &
     &                    'to compute satellite position from the epehemeride file '// &
     &                    FIL_TLE(J7) )
                      CALL EXIT ( 1 )
                 END IF
!
! -------------- ... and put them in VAL_NOD array
!
                 VAL_NOD(J8,1:3,J7) = COO_SAT(1:3)
 480          CONTINUE 
!
! ----------- Compute interpolating spline for three components of 
! ----------- positinos of the J7-th satellite
!
              DO 490 J9=1,3
                 IUER = -1
                 CALL MAKE_SPLINE ( 3, L_NOD, ARG_NOD, VAL_NOD(1,J9,J7), 0.0D0, 0.0D0, &
     &                              SPL_NOD(1,J9,J7), TMP_ARR, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 4814, IUER, 'TLE_TO_SPIND', 'Error in an attempt '// &
     &                   'to compute interpolating spline' ) 
                      CALL EXIT ( 1 )
                 END IF
 490          CONTINUE 
         END IF
 470  CONTINUE 
!
! --- Continue generation of the output file header
!
      CALL CLRCH ( STR  )
      CALL CLRCH ( STR1 )
      CALL INCH  ( N_SAT, STR )
      CALL INCH  ( N_TIM, STR1 )
      NO = NO + 1 ;  OUT(NO) = '#'
      NO = NO + 1 ;  OUT(NO) = '# Created on '//GET_CDATE()
      NO = NO + 1 ;  OUT(NO) = '#'
      NO = NO + 1 ;  OUT(NO) = '# Number of satellites: '//TRIM(STR)//'  Number of epochs: '//TRIM(STR1)
      NO = NO + 1 ;  OUT(NO) = '#'
      NO = NO + 1 ;  OUT(NO) = '# Format:'
      NO = NO + 1 ;  OUT(NO) = '#'
      NO = NO + 1 ;  OUT(NO) = '#   1:10  A10    J2000-name.'
      NO = NO + 1 ;  OUT(NO) = '#  13:23  A11    Right ascension'
      NO = NO + 1 ;  OUT(NO) = '#  26:36  A11    Declination'
      NO = NO + 1 ;  OUT(NO) = '#  39:48  F10.1  Flux density at 4.8 GHz (mJy)'
      NO = NO + 1 ;  OUT(NO) = '#  52:56  F5.2   Spectral index'
      NO = NO + 1 ;  OUT(NO) = '#  59:62  I4     Number of different frequences used in computations'
      NO = NO + 1 ;  OUT(NO) = '#  65:68  F4.1   Distance from the closest calibrator (in deg)'
      NO = NO + 1 ;  OUT(NO) = '#  71:75  F5.1   Galactic latitude (in deg)'
      NO = NO + 1 ;  OUT(NO) = '#  78:78  A1     Whether the source has been observed with VLBI'
      NO = NO + 1 ;  OUT(NO) = '#  81:88  A8     Bname'
      NO = NO + 1 ;  OUT(NO) = '#  91:96  F6.1   Scan duration in seconds'
      NO = NO + 1 ;  OUT(NO) = '#  98:104 F7.1   Priority'
      NO = NO + 1 ;  OUT(NO) = '# 106:107 I2     Minimal number of stations to observe'
      NO = NO + 1 ;  OUT(NO) = '# 114:115 I2     min number of scans'
      NO = NO + 1 ;  OUT(NO) = '# 117:118 I2     max number of scans'
      NO = NO + 1 ;  OUT(NO) = '# 120:256 A137   Comments'
      NO = NO + 1 ;  OUT(NO) = '#'
      NO = NO + 1 ;  OUT(NO) = '# Name      Right Asc.   Declination        Flux   Sp_ind    #  dist gal.lat D  B-name     Dura      Pri #S El_m  I  X # Comment'
      NO = NO + 1 ;  OUT(NO) = '#'
!
! --- Cycle over satellites
!
      DO 4100 J10=1,N_SAT
         IF ( .NOT. FL_INTRP ) THEN
              IUER = -1
              CALL TLE_PARSER ( NERS, FIL_TLE(J10), EPH(J10), IUER )     
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4811, IUER, 'TLE_TO_SPIND', 'Error in an attempt '// &
     &                'to parse a satellite ephemeride file '//FIL_TLE(J10) )
                   CALL EXIT ( 1 )
              END IF
         END IF
!
! ------ Cycle over epochs
!
         DO 4110 J11=1,N_TIM
!
! --------- Get time argument for the J11-th epoch in the output file
!
            TAI_OBS = TAI_BEG + (J11-1)*TIM_STEP
            IDAY    = TAI_OBS/86400.0D0
            TAI_OBS = TAI_OBS - IDAY*86400.D0
            MJD_OBS = MJD_BEG + IDAY
            IF ( FL_INTRP ) THEN
!
! -------------- Get time argument for spline interpolation
!
                 TIM_ARG = (J11-1)*TIM_STEP
!
! -------------- Find the pivotal node
!
                 IX = IXMN8 ( L_NOD, ARG_NOD, TIM_ARG )
!
! -------------- ... and compute position of the J10-the satellite for the J11-th epoch
! -------------- using spline interpolation
!
                 DO 4120 J12=1,3
                    COO_SAT(J12) = FSPL8 ( TIM_ARG, L_NOD, ARG_NOD, VAL_NOD(1,J12,J10), &
     &                                     IX, SPL_NOD(1,J12,J10) )
 4120            CONTINUE 
              ELSE
                 IUER = -1
                 CALL TLE_TO_CRS ( NERS, EPH(J10), MJD_OBS, TAI_OBS, &
     &                             COO_SAT, VEL_SAT, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 4812, IUER, 'TLE_TO_SPIND', 'Error in an attempt '// &
     &                    'to parse a satellite epehemeride file '//FIL_TLE(J10) )
                      CALL EXIT ( 1 )
                 END IF
            END IF
!
! --------- Connver position to polar coordinates
!
            CALL DECPOL ( 3, COO_SAT, DST, RA, DEC, IUER )
!
! --------- Generate J2000-name of the pseudo-source
!
            CALL INCH ( J11, SAT_STR )
            CALL CHASHR   ( SAT_STR )
            SAT_STR(1:3) = C_SAT(J10)
            SAT_STR(4:4) = '_'
            CALL BLANK_TO_ZERO ( SAT_STR )
!
            NO = NO + 1
            CALL CLRCH ( OUT(NO) )
!
! --------- Put J2000-name of the pseud-source in the output file
!
            OUT(NO)(1:10)  = SAT_STR
!
! --------- Put right ascension and declination
!
            CALL RH_TAT ( RA,  2, OUT(NO)(12:23), IUER )
            CALL RG_TAT ( DEC, 1, OUT(NO)(26:36), IUER )
            IF ( DEC > 0.0 ) OUT(NO)(26:26) = '+'
!
! --------- Generate B-name of the pseudo-sourcee
!
            IB = (J10-1)/26
            IE = J10 - IB*26
            BNAME(1:3) = C_SAT(J10)
!
! --------- using 26-mal notation for its epoch
!
            CALL INT_TO_BASE26 ( J11, BNAME(4:8) )
            IF ( BNAME(4:4) == 'A' ) BNAME(4:4) = '_'
!
! --------- Put B-name
!
            OUT(NO)(81:88)  = BNAME
            OUT(NO)(89:125) = '   '//DUR_STR(1:5)//'     1.0 '//STR_NSTA//' 15.0  1  1 ! Dst:'
!
! --------- Put distasnce, epoch, range, and Carethesian coordinates in the output file
!
            WRITE ( UNIT=OUT(NO)(127:140), FMT='(F13.2)' ) DST
            STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IUER )
            OUT(NO)(142:172) = 'Date: '//STR(1:21)//' TAI'
            WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) TIM_STEP/2.0D0
            OUT(NO)(174:) = 'Range: '//STR(1:8)//' sec'
            WRITE ( UNIT=OUT(NO)(195:207), FMT='(F13.2)' ) COO_SAT(1)
            WRITE ( UNIT=OUT(NO)(209:221), FMT='(F13.2)' ) COO_SAT(2)
            WRITE ( UNIT=OUT(NO)(223:235), FMT='(F13.2)' ) COO_SAT(3)
 4110     CONTINUE 
         DEALLOCATE ( EPH(J10)%TLE )     
 4100 CONTINUE 
!
! --- Write the output file
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4813, IUER, 'TLE_TO_SPIND', 'Error in writing '// &
     &         'into the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Written file '//TRIM(FILOUT)
!
      END  PROGRAM    TLE_TO_SPIND  !#!  
