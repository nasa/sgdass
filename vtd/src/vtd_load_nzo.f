      SUBROUTINE VTD_LOAD_NZO ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD_NZO  parses the input files with an orbit of     *
! *   near zone objects, such as landers or GNSS satellites, and         *
! *   computes arrays with coeffieicnts of the orbit expansion into      *
! *   B-spline basis. For objects that are space radiotelescope or 
! *   a lunar lander array RLT, the differences in geopotential at the   *
! *   oribiting satellite and at the geoid are computed and expanded     *
! *   into B-spine basis. For GNSS satellites clocks are expanded into   *
! *   B-spine basis.                                                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 16-APR-2023   VTD_LOAD_NZO  v2.2 (d) L. Petrov  22-SEP-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INCLUDE   'vtd_local.i'
      INCLUDE   'tle_sgp4.i'
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( EPH__TYPE  ) :: TLE_EPH
      INTEGER*4  IUER
      CHARACTER  STR*128, STR1*128, STR2*128, VTD_DIFX_COMPAT*3, CELE_NAME*20
      ADDRESS__TYPE :: DIR_DESC(16)
      INTEGER*4  M_BUF
      PARAMETER  ( M_BUF = 1024 )
      CHARACTER  FILNAM*128, NZO_NAME*8, CENTER_NAME*32, REF_NAME*32, &
     &           OBJ_TYPE*8, OBJ_CODE*1, OBJ_USED*1, SOU_ANT_NAME*3, &
     &           C_FIL(VTD__M_NZO)*128, C_DIR(VTD__M_NZO)*128, &
     &           CS_FIL(VTD__M_NZO)*128, VTD_GNSS_SAT_TABLE_FILE*128, BUFG(M_BUF)*128, &
     &           SAT_NAM*20
      REAL*8     TIM_ARR(VTD__MEPH), POS_ARR(VTD__MEPH,3), VEL_ARR(VTD__MEPH,3), &
     &           TIM_INT, DST, TIM_INT_INTRP, TAI_BEG, TAI_BEG_1ST, TIM_STEP, &
     &           TIM_KNOT_STEP, RCOND, TIM_EPS, COO_SAT(3), VEL_SAT(3)
      REAL*8     STA_POS(3), AZ, EL
      PARAMETER  ( TIM_EPS = 1.0D0 )
      REAL*8,    ALLOCATABLE :: EQU_MAT(:,:), NOR_VEC(:,:), NOR_MAT(:), TIM_KNOT(:)
      INTEGER*4  TIM_CODE, COO_CODE, NZO_REF
      INTEGER*4  L_DEG, NUM_ANTENNAS, IND, IND_LAST
      LOGICAL*1  FL_SMO_SPLINE, IS_UNIQUE, FL_FOUND, LEX
      INTEGER*4  NZO_FMT_ID, MJD_BEG, L_EPO, L_SAT, IS, MJD_ARR(VTD__MEPH), &
     &           L_ARR, L_KNOT, L_PAR, LL_PAR, MJD_BEG_1ST, LEV, L_FIL, LS_FIL, &
     &           NG_BUF, IL, NORAD_ID, IND_GNSS, IND_NZO, L_PCO, L_DIR, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, J26, IER
      CHARACTER  ANTENNA_TYPES(VTD__M_PCO)*20
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, I_LEN, ILEN, IXMN8, IXMN8_S, LTM_DIF
      REAL*8,    EXTERNAL :: BSPL_VAL
!
      VTD%L_NZO = 0
!
! --- By default an interpolation spline is computed.
! --- If FL_SMO_SPLINE is equale YES, smoothing spline is computed
!
      L_DEG = 3
      FL_SMO_SPLINE = .FALSE.
      CALL GETENVAR ( 'VTD_NZO_SMO', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_SMO_SPLINE = .TRUE.
      END IF
      CALL GETENVAR ( 'VTD_GNSS_SAT_TABLE', STR )
      IF ( ILEN(STR) > 0 ) THEN
           VTD_GNSS_SAT_TABLE_FILE = STR
         ELSE
           VTD_GNSS_SAT_TABLE_FILE = TRIM(VTD__DATA)//'/'//VTD__GNSS_SAT_TABLE
      END IF
!
      LS_FIL = 0
      L_FIL  = 0
      L_DIR  = 0
      DO 410 J1=1,VTD%L_SOU
         IF ( ILEN(VTD%SOU(J1)%NZO_FILE) > 0 ) THEN
              L_FIL = L_FIL + 1
              C_FIL(L_FIL) = VTD%SOU(J1)%NZO_FILE
         END IF 
         IF ( ILEN(VTD%SOU(J1)%NZO_DIR) > 0 ) THEN
              FL_FOUND = .FALSE.
              IF ( L_DIR > 0 ) THEN
                   DO 420 J2=1,L_DIR
                      IF ( VTD%SOU(J1)%NZO_DIR == C_DIR(J2) )  THEN
                           FL_FOUND = .TRUE.
                      END IF
 420               CONTINUE 
              END IF
              IF ( .NOT. FL_FOUND ) THEN
                   L_DIR = L_DIR + 1
                   C_DIR(L_DIR) = VTD%SOU(J1)%NZO_DIR
              END IF
         END IF 
 410  CONTINUE 
!
! --- A cycle over all files in the NZO directory
!
      IF ( ILEN(VTD%CONF%DIR_NZO) .GT. 0 ) THEN
           L_DIR = L_DIR + 1
           C_DIR(L_DIR) = VTD%CONF%DIR_NZO
      END IF
      IF ( L_DIR > 0 ) THEN
           DO 430 J3=1,L_DIR
              LEV = 0
              DO 440 J4=1,1024*1024*1024
                 IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, C_DIR(J3), FILNAM )
                 IF ( LEV == 0 ) GOTO 840
                 IF ( IS .NE. 0 ) THEN
                      CALL ERR_LOG ( 2431, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &                    'reading directory '//TRIM(C_DIR(J3))// &
     &                    ' with the ephemerides of near zone objects: '//FILNAM )
                      RETURN 
                 END IF
                 IF ( INDEX ( FILNAM, '#' ) > 0    ) GOTO 440
                 IF ( INDEX ( FILNAM, '~' ) > 0    ) GOTO 440
                 IL = ILEN(FILNAM) 
                 IF ( IL < 5 ) GOTO 440
                 IF ( FILNAM(IL-3:IL) == '.nzo' .OR. &
     &                FILNAM(IL-3:IL) == '.NZO' .OR. &
     &                FILNAM(IL-3:IL) == '.SP3' .OR. &
     &                FILNAM(IL-3:IL) == '.sp3' .OR. &
     &                FILNAM(IL-3:IL) == '.TLE' .OR. &
     &                FILNAM(IL-3:IL) == '.tle' .OR. &
     &                FILNAM(IL-3:IL) == '.SCF' .OR. &
     &                FILNAM(IL-3:IL) == '.scf' .OR. &
     &                FILNAM(IL-3:IL) == '.ODM' .OR. &
     &                FILNAM(IL-3:IL) == '.odm'      ) THEN
!
                      L_FIL = L_FIL + 1
                      IF ( L_FIL > VTD__M_NZO ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( VTD__M_NZO, STR )
                           CALL ERR_LOG ( 2432, IUER, 'VTD_LOAD_NZO', 'Too many NZO '// &
     &                         'files are in directory '//TRIM(C_DIR(J3))// &
     &                         ' -- more than '//TRIM(STR)//' -- please update parameter '// &
     &                         'VTD__M_NZO' )
                           RETURN 
                      END IF 
                      C_FIL(L_FIL) = FILNAM
                 END IF
 440          CONTINUE 
 840          CONTINUE 
              IF ( L_FIL == 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( VTD__M_NZO, STR )
                   CALL ERR_LOG ( 2433, IUER, 'VTD_LOAD_NZO', 'No NZO '// &
     &                 'files were found in directory '//C_DIR(J3) )
                   RETURN 
              END IF
 430       CONTINUE 
      END IF
!
      IF ( L_FIL == 0 ) THEN
!
! -------- No NZO file has been found. Nothing to do any more
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
!
      INQUIRE ( FILE=VTD_GNSS_SAT_TABLE_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2435, IUER, 'VTD_LOAD_NZO', 'Cannot find a file '// &
     &                   'with GNSS satellite table '//VTD_GNSS_SAT_TABLE_FILE )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( VTD_GNSS_SAT_TABLE_FILE, M_BUF, BUFG, NG_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2436, IUER, 'VTD_LOAD_NZO', 'Failure in '// &
     &         'reading a file with GNSS satellite table '// &
     &         VTD_GNSS_SAT_TABLE_FILE )
           RETURN 
      END IF
!
      IF ( BUFG(1)(1:LEN(VTD__GNS_TAB)) .NE. VTD__GNS_TAB ) THEN
           CALL ERR_LOG ( 2437, IUER, 'VTD_LOAD_NZO', 'Wrong first '// &
     &         'line of file with GNSS satellite table '//TRIM(VTD_GNSS_SAT_TABLE_FILE)// &
     &         ' '//BUFG(1)(1:LEN(VTD__GNS_TAB))//' while '//VTD__GNS_TAB// &
     &         ' was expected' )
           RETURN 
      END IF
!
      MJD_BEG_1ST = -1
      TAI_BEG_1ST = -1.0D0
      DO 450 J5=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_NZO_INQUIRE ( C_FIL(J5), NZO_FMT_ID, MJD_BEG, TAI_BEG, &
     &                          TIM_STEP, L_EPO, L_SAT, SAT_NAM, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2438, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &            'attempt in inquire the satellite ephemeride file '// &
     &             C_FIL(J1) )
              RETURN 
         END IF
         IF ( NZO_FMT_ID == VTD__NZO .OR. NZO_FMT_ID == VTD__ODM ) THEN
!
! =========== NZO or ODM format
!
              CALL ERR_PASS     ( IUER, IER )
              CALL VTD_READ_NZO ( C_FIL(J5), VTD__MEPH, L_ARR, MJD_ARR, &
     &                            TIM_ARR, POS_ARR, VEL_ARR, NZO_NAME, OBJ_TYPE, &
     &                            CENTER_NAME, REF_NAME, TIM_CODE, COO_CODE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2439, IUER, 'VTD_LOAD_NZO', 'Error in an '// &
     &                 'attempt to read NZO data in file '//TRIM(C_FIL(J5))// &
     &                 ' into VTD data structure' )
                   RETURN
              END IF
              IF ( OBJ_TYPE == VTD__GEO_SAT ) THEN
                   OBJ_CODE = VTD__ES
                 ELSE IF ( OBJ_TYPE == VTD__MOO_ORB ) THEN
                   OBJ_CODE = VTD__SS
                 ELSE IF ( OBJ_TYPE == VTD__MOO_LND ) THEN
                   OBJ_CODE = VTD__SS
                 ELSE IF ( OBJ_TYPE == VTD__PLA_ORB ) THEN
                   OBJ_CODE = VTD__SS
                 ELSE IF ( OBJ_TYPE == VTD__PLA_LND ) THEN
                   OBJ_CODE = VTD__SS
                 ELSE IF ( OBJ_TYPE == VTD__SOL_ORB ) THEN
                   OBJ_CODE = VTD__SS
              END IF
!
              TIM_INT = (MJD_ARR(L_ARR) - MJD_ARR(1))*86400.0D0 + (TIM_ARR(L_ARR) - TIM_ARR(1))
              IF ( FL_SMO_SPLINE ) THEN
                   DST = DSQRT ( POS_ARR(1,1)**2 + POS_ARR(1,2)**2 + POS_ARR(1,3)**2 )
                   IF ( DST < 7.D6 ) THEN
                        TIM_INT_INTRP = 50.0D0
                        OBJ_USED = VTD__ES
                      ELSE IF ( DST < 45.D6 ) THEN
                        TIM_INT_INTRP = 100.0D0
                        OBJ_USED = VTD__ES
                      ELSE IF ( DST < 430.D6 ) THEN
                        TIM_INT_INTRP = 500.0D0
                        OBJ_USED = VTD__SS
                      ELSE
                        TIM_INT_INTRP = 3000.0D0
                        OBJ_USED = VTD__SS
                   END IF
                   L_KNOT = IDNINT( TIM_INT/TIM_INT_INTRP ) + 1
                   IF ( L_KNOT  < 8 ) L_KNOT = 8
                 ELSE
                   L_KNOT = L_ARR 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_LOAD_OBJ_NZO ( NZO_NAME, OBJ_CODE, OBJ_USED, NZO_REF, TIM_CODE, &
     &                                VTD, L_ARR, MJD_ARR, TIM_ARR, POS_ARR, L_KNOT, &
     &                                L_DEG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2440, IUER, 'VTD_LOAD_NZO', 'Error in an '// &
     &                 'attempt to load NZO data of object '//TRIM(NZO_NAME)// &
     &                 ' from ephemeride file '//TRIM(FILNAM)//' into VTD '// &
     &                 'data structure ' )
                   RETURN
              END IF
           ELSE IF ( NZO_FMT_ID == VTD__TLE ) THEN
!
! =========== TLE (two line elemment format)
!
              CALL CLRCH ( NZO_NAME )
              DO 460 J6=1,VTD%L_SOU
                 IF ( VTD%SOU(J6)%NORAD_ID > 0 ) THEN
                      CALL INCH ( VTD%SOU(J6)%NORAD_ID, STR(1:5) )
                      IF ( STR(1:5) == SAT_NAM(1:5) ) THEN
                           NZO_NAME = VTD%SOU(J6)%IVS_NAME
                      END IF
                 END IF
 460          CONTINUE 
              IF ( ILEN(NZO_NAME) == 0 ) THEN
!
! ---------------- This file has no associated source. We do not need to process it
!
                   GOTO 450
              END IF
!
              IF ( VTD%MJD_BEG .LT. VTD__MJD_MIN .OR. &
     &             VTD%MJD_BEG .GT. VTD__MJD_MAX      ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( VTD%MJD_BEG, STR )
                   CALL ERR_LOG ( 2441, IUER, 'VTD_LOAD_NZO', 'VTD%MJD_BEG '//TRIM(STR)// &
     &                 ' is out of range' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL TLE_PARSER ( VTD%NERS, C_FIL(J5), TLE_EPH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2442, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &                 'parsing the ephemeride file in TLE format '//C_FIL(J5) )
                   RETURN 
              END IF
!
              IND_NZO = 0              
              IF ( VTD%L_NZO > 0 ) THEN
!
! ---------------- Find the index of the near zone object in the NZO list 
!
                   DO 470 J6=1,VTD%L_SOU
                      IF ( NZO_NAME == VTD%NZO(J5)%NAME ) IND_NZO = J5
 470               CONTINUE 
              END IF
!
! ----------- Did not find?  Add the new object in the list
!
              IF ( IND_NZO == 0 ) THEN
                   VTD%L_NZO = VTD%L_NZO + 1
                   IND_NZO   = VTD%L_NZO 
              END IF
              IF ( VTD%L_NZO + 1 > VTD__M_NZO ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( VTD__M_NZO, STR )
                   CALL ERR_LOG ( 2443, IUER, 'VTD_LOAD_NZO', 'Too many near '// &
     &                 'zone objects -- exceeded the maximum VTD__M_NZO '//STR )
                   RETURN 
              END IF
!
              L_DEG = 3
              L_ARR = ( (VTD%MJD_END - VTD%MJD_BEG)*86400.0D0 + &
     &                  (VTD%TAI_END - VTD%TAI_BEG) )/VTD__TLE_STEP + 1 + &
     &                  2*VTD__TLE_EXTRA_KNOT
              IF ( ASSOCIATED ( VTD%NZO(VTD%L_NZO)%TIM_ARR ) ) DEALLOCATE ( VTD%NZO(VTD%L_NZO)%TIM_ARR )
              IF ( ASSOCIATED ( VTD%NZO(VTD%L_NZO)%SPL_ARR ) ) DEALLOCATE ( VTD%NZO(VTD%L_NZO)%SPL_ARR )
              IF ( ASSOCIATED ( VTD%NZO(VTD%L_NZO)%SPL_RLT_ARR ) ) DEALLOCATE ( VTD%NZO(VTD%L_NZO)%SPL_RLT_ARR )
              IF ( ASSOCIATED ( VTD%NZO(VTD%L_NZO)%PHASE_WINDUP ) ) DEALLOCATE ( VTD%NZO(VTD%L_NZO)%PHASE_WINDUP )
              ALLOCATE ( VTD%NZO(VTD%L_NZO)%TIM_ARR(L_ARR),                    &
     &                   VTD%NZO(VTD%L_NZO)%SPL_ARR(1-L_DEG:L_ARR,3),          &
     &                   VTD%NZO(VTD%L_NZO)%SPL_CLO_ARR(1-L_DEG:L_ARR),        &
     &                   VTD%NZO(VTD%L_NZO)%PHASE_WINDUP(VTD%L_STA,VTD%L_STA), & 
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( VTD__M_NZO, STR )
                   CALL ERR_LOG ( 2444, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &                 'an attempt to allocate dynamic memory for arrays '// &
     &                 'with the ephemeride' )
                   RETURN 
              END IF
              VTD%NZO(VTD%L_NZO)%PHASE_WINDUP = 0.0D0 
              VTD%NZO(VTD%L_NZO)%MJD_BEG  = VTD%MJD_BEG
              VTD%NZO(VTD%L_NZO)%TAI_BEG  = VTD%TAI_BEG - VTD__TLE_EXTRA_KNOT*VTD__TLE_STEP
              IF ( VTD%NZO(VTD%L_NZO)%TAI_BEG < 0.0D0 ) THEN
                   VTD%NZO(VTD%L_NZO)%TAI_BEG = VTD%NZO(VTD%L_NZO)%TAI_BEG + 86400.D0
                   VTD%NZO(VTD%L_NZO)%MJD_BEG = VTD%NZO(VTD%L_NZO)%MJD_BEG - 1
              END IF
              VTD%NZO(VTD%L_NZO)%TIM_CODE = VTD__TAI
              VTD%NZO(VTD%L_NZO)%OBJ_TYPE = VTD__GEO_SAT
              VTD%NZO(VTD%L_NZO)%NZO_COO_ORIGIN = VTD__GEOCENTER 
              VTD%NZO(VTD%L_NZO)%OBJ_USED = VTD__ES 
              VTD%NZO(VTD%L_NZO)%FIL_EPH = C_FIL(J6)
              VTD%NZO(IND_NZO)%STATUS = VTD__ALLC
!
              DO 480 J8=1,L_ARR
                 VTD%NZO(VTD%L_NZO)%TIM_ARR(J8) = (J8-1)*VTD__TLE_STEP
                 STA_POS = 0.0D0
                 CALL TLE_TO_TRS ( VTD%NERS, TLE_EPH, VTD%NZO(VTD%L_NZO)%MJD_BEG, &
     &                             VTD%NZO(VTD%L_NZO)%TAI_BEG + VTD%NZO(VTD%L_NZO)%TIM_ARR(J8), &
     &                             STA_POS, COO_SAT, VEL_SAT, AZ, EL, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 2445, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &                    'computing position using the ephemeride file in TLE format '// &
     &                     C_FIL(J6) )
                      RETURN 
                 END IF
                 VTD%NZO(VTD%L_NZO)%SPL_ARR(J8,1:3) = COO_SAT(1:3)
 480          CONTINUE 
!
              VTD%NZO(VTD%L_NZO)%DEG_SPL = L_DEG
              VTD%NZO(VTD%L_NZO)%NOD_SPL = L_ARR
              VTD%NZO(VTD%L_NZO)%NORAD_ID = TLE_EPH%TLE(1)%SAT_CAT
              VTD%NZO(VTD%L_NZO)%NAME(1:2) = 'N_'
              CALL INCH ( VTD%NZO(VTD%L_NZO)%NORAD_ID, VTD%NZO(VTD%L_NZO)%NAME(3:) ) ! Temporary
!
! ----------- Replace array of ephemeride as input VTD%NZO(J13)%SPL_ARR
! ----------- with the array of spline coefficients as the output of VTD%NZO(J13)%SPL_ARR
!
              DO 490 J9=1,3
                 CALL ERR_PASS ( IUER, IER )
                 CALL BSPL_1D_CMP ( VTD%NZO(VTD%L_NZO)%DEG_SPL, 0, VTD%NZO(VTD%L_NZO)%NOD_SPL, &
     &                              VTD%NZO(VTD%L_NZO)%TIM_ARR, &
     &                              VTD%NZO(VTD%L_NZO)%SPL_ARR(1-VTD%NZO(VTD%L_NZO)%DEG_SPL,J9), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 2446, IUER, 'VTD_LOAD_NZO', 'Trap of internal '// &
     &                        'control: failure in B-spline computation for positions' )
                      RETURN 
                 END IF
 490          CONTINUE 
          ELSE IF ( NZO_FMT_ID == VTD__SP3 ) THEN
!
! ---------- Since data file in SP3 format contain orbits of many satellites,
! ---------- we process them separatery. Just put them in the separate list
!
             LS_FIL = LS_FIL + 1
             CS_FIL(LS_FIL) = C_FIL(J5)
             IF ( MJD_BEG_1ST == -1 ) THEN
                  MJD_BEG_1ST = MJD_BEG
                  TAI_BEG_1ST = TAI_BEG
             END IF
         END IF
 450  CONTINUE 
!
      IF ( LS_FIL > 0 ) THEN
!
! -------- Aga, we have orbits in SP3 format.
! -------- Let us forst sort them, since they should follow in the chronological order
!
           CALL SORT_FAST_CH ( LS_FIL, CS_FIL )
!
! -------- Inquire the first file. Get the date range, time step, number of
! -------- epochs, and the number of satellites
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_NZO_INQUIRE ( CS_FIL(1), NZO_FMT_ID, MJD_BEG, TAI_BEG, &
     &                            TIM_STEP, L_EPO, L_SAT, SAT_NAM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2447, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &              'attempt in inquire the satellite ephemeride file '// &
     &               CS_FIL(1) )
                RETURN 
           END IF
!
! -------- NB: the use of Non-smoothing B-splines of degree 3 resutls in 
! -------- oribit interpolation errors up to 0.3 meters!!!
!
           FL_SMO_SPLINE = .TRUE.
!
! -------- Case of GNSS satellites. The orbit is in SP3 format 
!
           IF ( FL_SMO_SPLINE ) THEN
!
! ------------- Smoothing interpolation with B-spline of the 5th degree
!
                L_DEG = 5
                L_ARR = (L_EPO-1)*LS_FIL + 1
                L_KNOT =  VTD__KNOT_ARR_SHR*L_ARR  ! The mumber of knots is a certaint share of the number of lenght of input data
                L_PAR =  L_KNOT + L_DEG - 1   ! The number of extimated parameters
                LL_PAR = (L_PAR*(L_PAR+1))/2
!
! ------------- Allocate temporary arrays
!
                ALLOCATE ( EQU_MAT(L_PAR,L_ARR), NOR_VEC(L_PAR,4), &
     &                     NOR_MAT(LL_PAR), TIM_KNOT(L_KNOT) )
              ELSE
!
! ------------- The use of B-spline of 3rd degree. NB: accuracy loss!!
! ------------- Interpolation errors may reach 20 cm
!
                L_DEG = 3 
                L_KNOT = (L_EPO-1)*LS_FIL + 1
                L_ARR = L_KNOT
           END IF 
!
! -------- Memory allocation for all satellites defined in SP3 data files
!
           DO 4100 J10=VTD%L_NZO+1,VTD%L_NZO+L_SAT
              IF ( ASSOCIATED ( VTD%NZO(J10)%TIM_ARR ) ) DEALLOCATE ( VTD%NZO(J10)%TIM_ARR )
              IF ( ASSOCIATED ( VTD%NZO(J10)%SPL_ARR ) ) DEALLOCATE ( VTD%NZO(J10)%SPL_ARR )
              IF ( ASSOCIATED ( VTD%NZO(J10)%SPL_RLT_ARR ) ) DEALLOCATE ( VTD%NZO(J10)%SPL_RLT_ARR )
              IF ( ASSOCIATED ( VTD%NZO(J10)%SPL_CLO_ARR ) ) DEALLOCATE ( VTD%NZO(J10)%SPL_CLO_ARR )
              IF ( ASSOCIATED ( VTD%NZO(J10)%PHASE_WINDUP ) ) DEALLOCATE ( VTD%NZO(J10)%PHASE_WINDUP )
!
              ALLOCATE ( VTD%NZO(J10)%TIM_ARR(L_ARR),                      &
     &                   VTD%NZO(J10)%SPL_ARR(1-L_DEG:L_ARR,3),          &
     &                   VTD%NZO(J10)%SPL_CLO_ARR(1-L_DEG:L_ARR),        &
     &                   VTD%NZO(J10)%PHASE_WINDUP(VTD%L_STA,VTD%L_STA),   & 
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( VTD__M_NZO, STR )
                   CALL ERR_LOG ( 2448, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &                 'an attempt to allocate dynamic memory for arrays '// &
     &                 'with the ephemeride' )
                   RETURN 
              END IF
              VTD%NZO(J10)%PHASE_WINDUP = 0.0D0 
              VTD%NZO(J10)%MJD_BEG  = MJD_BEG_1ST  ! epoch of the first file in C_FIL array
              VTD%NZO(J10)%TAI_BEG  = TAI_BEG_1ST  ! epoch of the first file in C_FIL array
              VTD%NZO(J10)%TIM_CODE = VTD__TAI
              VTD%NZO(J10)%OBJ_TYPE = VTD__GEO_SAT
              VTD%NZO(J10)%OBJ_USED = VTD__ES 
              VTD%NZO(J10)%FIL_EPH  = C_FIL(1)
              VTD%NZO(J10)%NZO_COO_ORIGIN = VTD__GEOCENTER 
              VTD%NZO(J10)%FIL_EPH  = C_FIL(1)
              VTD%NZO(J10)%STATUS   = VTD__ALLC
  4100     CONTINUE 
!
! -------- Read the orbit in the array of SP3 files, extract the ephemerid for 
! -------- those satellites tghat are defined in VTDSOU and and put their
! -------- in NSO ojects.
! -------- Routine VTD_INGEST_SP3 concatenates the ephemerid from 
! -------- LS_FIL files into continuous time series. 
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_INGEST_SP3 ( LS_FIL, CS_FIL, VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2449, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &           'attempt in ingest the satellite ephemeride files ' )
                RETURN 
           END IF
!
           IF ( FL_SMO_SPLINE ) THEN 
!
! ------------- Case of smoothing spline.
! ------------- Find the step for the array of knots
!
                TIM_KNOT_STEP = (VTD%NZO(VTD%L_NZO+1)%TIM_ARR(L_ARR) - &
     &                           VTD%NZO(VTD%L_NZO+1)%TIM_ARR(1))/(L_KNOT - 1)
                IF ( .NOT. ASSOCIATED ( VTD%NZO(VTD%L_NZO+1)%TIM_ARR ) ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( VTD%L_NZO+1, STR )
                     CALL ERR_LOG ( 2450, IUER, 'VTD_LOAD_NZO', 'Trap of '// &
     &                   'internal control: TIM_ARR is not associated for '// &
     &                   'the '//TRIM(STR)//' NZO object' )
                     RETURN 
                END IF
                IF ( VTD%NZO(VTD%L_NZO+1)%STATUS .NE. VTD__LOAD ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( VTD%L_NZO+1, STR )
                     CALL ERR_LOG ( 2451, IUER, 'VTD_LOAD_NZO', 'Trap of '// &
     &                   'internal control: the '//TRIM(STR)//' NZO object '// &
     &                   'is not loaded' )
                     RETURN 
                END IF
!
! ------------- Fill array of knots that is 
!
                DO 4110 J11=1,L_KNOT
                   TIM_KNOT(J11) = VTD%NZO(1)%TIM_ARR(1) + TIM_KNOT_STEP*(J11-1)
 4110           CONTINUE 
                TIM_KNOT(1)      = TIM_KNOT(1)      - TIM_EPS
                TIM_KNOT(L_KNOT) = TIM_KNOT(L_KNOT) + TIM_EPS
!
! ------------- Create the equation matrix and the corresponding normal matrix
!
                EQU_MAT  = 0.0D0
                NOR_MAT  = 0.0D0
                IND_LAST = 0
!
! ------------- Cycle over epehemerid points
!
                DO 4120 J12=1,L_ARR
!
! ---------------- Get the low index of VTD%NZO(1)%TIM_ARR(J12) element in array of
! ---------------- knots TIM_KNOT
!
                   IND = IXMN8 ( L_KNOT, TIM_KNOT, VTD%NZO(VTD%L_NZO+1)%TIM_ARR(J12)  )
!
! ---------------- Create an equation of conditions
!
                   DO 4130 J13=IND-L_DEG,IND
                      EQU_MAT(J13+L_DEG,J12) = BSPL_VAL ( L_KNOT, TIM_KNOT, &
     &                                                    L_DEG, J13, VTD%NZO(VTD%L_NZO+1)%TIM_ARR(J12) )
 4130              CONTINUE 
!
! ---------------- Update the normal equation
!
                   CALL DIAD_CVT_S ( 1.D0, L_PAR, EQU_MAT(1,J12), EQU_MAT(1,J12), NOR_MAT )
 4120           CONTINUE 
!
! ------------- Invert the matrix of normal equations
!
                CALL ERR_PASS ( IUER, IER )
                CALL INVS ( L_PAR, NOR_MAT, RCOND, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2452, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &                   'an attempt to invert normal matrix' )
                     RETURN
                END IF
!
! ------------- Cycle over near zone objects
!
                DO 4140 J14=VTD%L_NZO+1,VTD%L_NZO+L_SAT
                   VTD%NZO(J14)%DEG_SPL = L_DEG
                   VTD%NZO(J14)%NOD_SPL = L_KNOT
                   NOR_VEC = 0
!
! ---------------- Build the normal vector
!
                   DO 4150 J15=1,L_ARR
!
! ------------------- Get the low index of VTD%NZO(J14)%TIM_ARR(J15) element in array of
! ------------------- knots TIM_KNOT.
! ------------------- NB: arrays VTD%NZO(J14)%SPL_ARR and VTD%NZO(J14)%SPL_CLO_ARR
! -------------------     contain epeheride on the input
! ------------------- NB: Array VTD%NZO(J14)%TIM_ARR contains array of time
! ----------------     epochs of ephemerides on the input
!
                      IND = IXMN8 ( L_KNOT, TIM_KNOT, VTD%NZO(J14)%TIM_ARR(J15) )
                      DO 4160 J16=IND-L_DEG,IND
!
! ---------------------- First, for X, Y, and Z components of the ephemeride,
!
                         DO 4170 J17=1,3
                            NOR_VEC(J16+L_DEG,J17) = NOR_VEC(J16+L_DEG,J17) + &
     &                                  EQU_MAT(J16+L_DEG,J15)*VTD%NZO(J14)%SPL_ARR(J15,J17)
 4170                    CONTINUE 
!
! ---------------------- Then for clock
!
                         NOR_VEC(J16+L_DEG,4) = NOR_VEC(J16+L_DEG,4) + &
     &                           EQU_MAT(J16+L_DEG,J15)*VTD%NZO(J14)%SPL_CLO_ARR(J15)
 4160                 CONTINUE 
 4150              CONTINUE 
!
! ---------------- Compute spline coefficients
! ---------------- NB: arrays VTD%NZO(J14)%SPL_ARR and VTD%NZO(J14)%SPL_CLO_ARR
! ----------------     contain spline coefficients on the output
!
                   DO 4180 J18=1,3
                      CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC(1,J18), L_PAR, &
     &                                   VTD%NZO(J14)%SPL_ARR(1-L_DEG,J18), IER )
 4180              CONTINUE 
                   CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC(1,4), L_PAR, &
     &                                VTD%NZO(J14)%SPL_CLO_ARR(1-L_DEG), IER )
!
! ---------------- Copy array of knots to VTD%NZO(J14)%TIM_ARR.
! ---------------- NB: Array VTD%NZO(J14)%TIM_ARR contains array of knots 
! ----------------     ont the output
!
                   VTD%NZO(J14)%TIM_ARR = 0.0D0
                   VTD%NZO(J14)%TIM_ARR(1:L_KNOT) = TIM_KNOT(1:L_KNOT)
 4140           CONTINUE 
                DEALLOCATE ( EQU_MAT, NOR_VEC, NOR_MAT, TIM_KNOT )
              ELSE
!
! ============= Interpolating spline of the 3rd degree
!
                DO 4190 J19=VTD%L_NZO+1,VTD%L_NZO+L_SAT
                   VTD%NZO(J19)%DEG_SPL = L_DEG
!
! ---------------- Replace array of ephemeride as input VTD%NZO(J19)%SPL_ARR
! ---------------- with the array of spline coefficients as the output of VTD%NZO(J19)%SPL_ARR
!
                   DO 4200 J20=1,3
                      CALL ERR_PASS ( IUER, IER )
                      CALL BSPL_1D_CMP ( VTD%NZO(J19)%DEG_SPL, 0, VTD%NZO(J19)%NOD_SPL, &
     &                                   VTD%NZO(J19)%TIM_ARR, &
     &                                   VTD%NZO(J19)%SPL_ARR(1-VTD%NZO(J19)%DEG_SPL,J20), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 2453, IUER, 'VTD_LOAD_NZO', 'Trap of internal '// &
     &                         'control: failure in B-spline computation for positions' )
                           RETURN 
                      END IF
  4200             CONTINUE 
!
! ---------------- Replace array of clock as input VTD%NZO(J19)%SPL_CLO_ARR
! ---------------- with the array of spline coefficients as the output of VTD%NZO(J19)%SPL_CLO_ARR
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL BSPL_1D_CMP ( VTD%NZO(J19)%DEG_SPL, 0, VTD%NZO(J19)%NOD_SPL, &
     &                                VTD%NZO(J19)%TIM_ARR, &
     &                                VTD%NZO(J19)%SPL_CLO_ARR(1-VTD%NZO(J19)%DEG_SPL), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2454, IUER, 'VTD_LOAD_NZO', 'Trap of internal '// &
     &                      'control: failure in B-spline computation for GNSS clock' )
                        RETURN 
                   END IF
  4190          CONTINUE 
           END IF
           VTD%L_NZO = VTD%L_NZO + L_SAT
        ELSE IF ( NZO_FMT_ID == VTD__TLE ) THEN
      END IF
!
! --- Check NZO obnjects
!
      DO 4210 J21=1,VTD%L_NZO
         IF ( VTD%NZO(J21)%STATUS .NE. VTD__LOAD ) GOTO 4210
!
! ------ Search for the NORAD ID.
! ------ VTD_GNSS_SAT_TABLE_FILE  relates source name, NORAD ID, and CELESTRACK ID
! ------ We match source name, and CELESTRACK ID against VTD_GNSS_SAT_TABLE_FILE  
!
         IND_GNSS = 0
         DO 4220 J22=1,NG_BUF
            IF ( BUFG(J22)(1:1)  == '#' ) GOTO 4220
            IF ( ILEN(BUFG(J22)) ==  0  ) GOTO 4220
            CALL CHIN ( BUFG(J22)(10:14), NORAD_ID )
            IF ( NORAD_ID == VTD%NZO(J21)%NORAD_ID ) THEN
                 IND_GNSS = J22
            END IF
            IF ( BUFG(J22)(1:8) == VTD%NZO(J21)%NAME ) THEN
                 IND_GNSS = J22
            END IF
            IF ( BUFG(J22)(17:36) == VTD%NZO(J21)%CELESTRACK_NAME ) THEN
                 IND_GNSS = J22
            END IF
 4220    CONTINUE 
!
         IF ( IND_GNSS > 0 ) THEN
              VTD%NZO(J21)%NAME = BUFG(IND_GNSS)(1:8)
              CALL CHIN ( BUFG(IND_GNSS)(10:14), VTD%NZO(J21)%NORAD_ID )
              VTD%NZO(J21)%CELESTRACK_NAME = BUFG(IND_GNSS)(1:8)
              VTD%NZO(J21)%OBJ_TYPE = VTD__GNSS_SAT
         END IF
         IF ( VTD%NZO(J21)%NZO_FMT == VTD__NZO ) THEN
              CONTINUE 
           ELSE IF ( VTD%NZO(J21)%NZO_FMT == VTD__ODM ) THEN
              CONTINUE 
           ELSE IF ( VTD%NZO(J21)%NZO_FMT == VTD__SP3 ) THEN
!
! ----------- If NORAD ID was not found for the object with orbit defined in SP3 
! ----------- data file, we consider it as an error
!
              IF ( IND_GNSS < 1 ) THEN
                   CALL ERR_LOG ( 2455, IUER, 'VTD_LOAD_NZO', 'Cannot of find '// &
     &                 'satellite '//VTD%NZO(J21)%NAME//' defined in file '// &
     &                 TRIM(VTD%NZO(J21)%NAME)//' in the GNSS table file '// &
     &                 VTD_GNSS_SAT_TABLE_FILE )
                   RETURN 
              END IF
           ELSE IF ( VTD%NZO(J21)%NZO_FMT == VTD__TLE ) THEN
!
! ----------- If NORAD ID was not found for the object with orbit defined in TLE
! ----------- data file, we consider it as an error
!
              IF ( IND_GNSS < 1 ) THEN
                   CALL INCH ( VTD%NZO(J21)%NORAD_ID, STR(1:5) )
                   CALL ERR_LOG ( 2456, IUER, 'VTD_LOAD_NZO', 'Cannot of find '// &
     &                 'satellite NORAD ID'//TRIM(STR)//' defined in file '// &
     &                 TRIM(VTD%NZO(J21)%NAME)//' in the GNSS table file '// &
     &                 VTD_GNSS_SAT_TABLE_FILE )
                   RETURN 
              END IF
         END IF
 4210 CONTINUE 
!
! --- Check whether orbits for all near-zone objects have been defined
!
      DO 4230 J23=1,VTD%L_SOU
         IF ( VTD%SOU(J23)%OBJ_TYPE == VTD__ES .OR. &
     &        VTD%SOU(J23)%OBJ_TYPE == VTD__EM .OR. &
     &        VTD%SOU(J23)%OBJ_TYPE == VTD__SS      ) THEN
!
              IF ( VTD%SOU(J23)%IND_NZO == 0 ) THEN
                   CALL ERR_LOG ( 2457, IUER, 'VTD_LOAD_NZO', 'No orbit data '// &
     &                 'were found for source '//VTD%SOU(J23)%IVS_NAME )
                   RETURN 
              END IF
              IF ( VTD%NZO(VTD%SOU(J23)%IND_NZO)%STATUS .NE. VTD__LOAD ) THEN
                   CALL ERR_LOG ( 2458, IUER, 'VTD_LOAD_NZO', 'Orbit data '// &
     &                 'were not loaded for source '//VTD%SOU(J23)%IVS_NAME )
                   RETURN 
              END IF
         END IF         
 4230 CONTINUE 
!
      IF ( VTD%CONF%DIR_BTX .NE. 'NONE' ) THEN
!
! -------- Load Phase Center Offsets (PCO) for all GNSS objects that 
! -------- we can find in directory VTD%CONF%DIR_BTX
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_PCO ( VTD, VTD__GNSS_SAT, VTD%CONF%DIR_BTX, L_PCO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2459, IUER, 'VTD_LOAD_NZO', 'Eror in an attempt '// &
     &              'to load phase center offset for space-born antennas '// &
     &              'that are supposed to be found in directory '// &
     &               VTD%CONF%DIR_BTX )
                RETURN 
           END IF
      END IF
!
      IF ( VTD%CONF%IVRB .GE. 2 ) THEN
           WRITE ( 6, 110 ) L_PCO, VTD%L_NZO, TRIM(VTD%CONF%DIR_BTX)
 110       FORMAT ( 'Phase offsets for ', I4, ' space-born antennas ',  &
     &              'out of ', I4, ' were found in ', A )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_NZO  !#!#
