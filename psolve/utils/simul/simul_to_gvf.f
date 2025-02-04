      SUBROUTINE SIMUL_TO_GVF ( SIMUL, GVH, NERS, VCAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMUL_TO_GVF
! *                                                                      *
! *  ### 10-JUN-2020  SIMUL_TO_GVF v1.0 (c)  L. Petrov  16-JUN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      INCLUDE   'ners.i'
      INCLUDE   'simul.i'
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      TYPE     ( GVH__STRU   ) :: GVH
      TYPE     ( VCAT__TYPE  ) :: VCAT
      TYPE     ( NERS__TYPE  ) :: NERS
      INTEGER*4  IUER
      INTEGER*4  M2G__FR1, M2G__FR2, M2G__CL1, M2G__SL1, M2G__TH1
      PARAMETER  ( M2G__FR1 = 1 )
      PARAMETER  ( M2G__FR2 = 2 )
      PARAMETER  ( M2G__CL1 = 3 )
      PARAMETER  ( M2G__SL1 = 4 )
      PARAMETER  ( M2G__TH1 = 5 )
      REAL*4     NOI_ARR(SIM__MBND)
      REAL*8     SKY_FREQ(8192), UTC_MTAI, TOT_UTC_1ST_OBS
      INTEGER*4  J1, J2, J3, J4, IND_SCA, NUMB_BND, NFRQ, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     do 510 j1=1,simul%nobs
!       write ( 6, * ) ' j1= ', int2(j1), ' obstab= ', simul%obstab(1:3,j1) ! %%%%%%%%%%
! 510 CONTINUE 
!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1411, IUER, 'SIMUL_TO_GVF', 'Error in an attempt to '// &
     &         'initialize GVH' )
           RETURN
      END IF
      GVH%GENERATOR = SIMUL__LABEL
      NUMB_BND = 1
      NFRQ = 1
      SKY_FREQ = 8.2D9
!
! --- Create preamble for the 1st section of the output GVH object
!
      GVH%NSEG = 5
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '1 CHARACTER ASCII', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1412, IUER, 'SIMUL_TO_GVF', 'Error in writing '// &
     &                   'preamlbe of GVH structure' )
           RETURN 
      END IF
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',    '2 INTEGER*2 IEEE-231', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',    '3 INTEGER*4 IEEE-231', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',    '4 REAL*4 IEEE 754-1985', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',    '5 REAL*8 IEEE 754-1985', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',    '6 INTEGER*8 IEEE-231', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:',   '81 Session', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:',   '82 Scan', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:',   '83 Station', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:',   '84 Baseline', IER )
      CALL GVH_PPREA ( GVH, 1, 'GVH_VERSION:', GVH__LABEL, IER )
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PPREA ( GVH, 1, 'GENERATOR:', GVH%GENERATOR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1413, IUER, 'SIMUL_TO_GVF','Error in writing '// &
     &                   'preamlbe of GVH structure' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'COR_TYPE', GVH__C1, GVH__SES, 8, 1, &
     &    'Correlator type: MK3 MK4 K4 S2 VLBA MITAKA-1 KJCC SFXC DiFX Simul', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1414, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EXP_CODE', GVH__C1, GVH__SES, 32, 1, &
     &    'Experiment code', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1415, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EXP_DESC', GVH__C1, GVH__SES, 80, 1, &
     &    'Experiment description', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1416, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PI_NAME ', GVH__C1, GVH__SES, 80, 1, &
     &    'Name of the principal investigator', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1417, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DATYP   ', GVH__I2, GVH__SES, 1, 1, &
     &    'Type of the observable or a combination of observables '// &
     &    'used in the solution', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1418, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SUPMET  ', GVH__I2, GVH__SES, 1, 1, &
     &    'Code of the suppression method used in the solution', &
     &     M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1419, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'UTC_MTAI', GVH__R8, GVH__SES, 1, 1, &
     &    'Difference UTC minus TAI at first time tag of the '// &
     &    'database (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1420, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SIT_COOR', GVH__R8, GVH__SES, 3, SIMUL%NSTA, &
     &    'Site coordinates in a crust-fixed terrestrial reference '// &
     &    'system: X, Y, Z (meters)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           write ( 6, * ) 'SIMUL%NSTA= ', SIMUL%NSTA
           CALL ERR_LOG ( 1421, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUMB_SOU', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of observed sources', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1422, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SRCNAMES', GVH__C1, GVH__SES, 8, SIMUL%NSOU, &
     &    'Source names', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1423, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOU_COOR', GVH__R8, GVH__SES, 2, SIMUL%NSOU, &
     &    'Source coordinates in a barycenteric reference system: '// &
     &    'right asc. decl. (rad)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1424, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_BAND', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of frequency bands observed in the experiment', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1425, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_AVBAND', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of frequency bands for which information is available', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1426, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_CHBN', GVH__I4, GVH__SES, 1, NUMB_BND, &
     &    'Number of frequency channels per band', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1427, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SKYFRQCH', GVH__R8, GVH__SES, NFRQ, 1, &
     &    'Sky frequency of channels in Hz', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1428, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SCANNAME', GVH__C1, GVH__SCA, 16, 1, &
     &    'Scan name', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1429, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'MJD_OBS ', GVH__I4, GVH__SCA, 1, 1, &
     &    'MJD of fringe reference time at pseudo-UTC timescale for the '// &
     &    'scan (days)', M2G__FR1, &
     &     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1430, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'UTC_OBS ', GVH__R8, GVH__SCA, 1, 1, &
     &    'Pseudo-UTC time tag of fringe reference time for the scan (sec)', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1431, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SCAN_DUR', GVH__R8, GVH__BAS, NUMB_BND, &
     &                1, 'Scan duration per band (sec)', M2G__FR2, &
     &                IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1432, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOU_IND ', GVH__I4, GVH__SCA, 1, 1, &
     &    'Source name index', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1433, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STA_IND', GVH__I4, GVH__BAS, 2, 1, &
     &    'Station names indexes', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1434, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GR_DELAY', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Group delays per band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1435, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DEL_RATE', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Phase delay rates per band (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1436, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GRDELERR', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Group delay errors per band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1437, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PHRATERR', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Phase delay rate delay errors per band (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1438, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SNRATIO ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Fringe amplitude signal to noise ratio (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1439, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EFF_FREQ', GVH__R8, GVH__BAS, 3, NUMB_BND, &
     &    'Effective ionospheric frequencies for gr.del, ph.del, '// &
     &    'ph.rate per band (Hz)', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1440, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'REF_FREQ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Reference frequency for phase delay per band (Hz)', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1441, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SAMPLRAT', GVH__R8, GVH__SES, 1, 1, &
     &    'Sample rate in Hz', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1442, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BITSAMPL', GVH__I2, GVH__SES, 1, 1, &
     &    'Number of bits per sample', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1443, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'FRN_AMPL', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Normalized fringe amplitude in range [0, 1]', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1444, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NOISERMS', GVH__R4, GVH__BAS, NUMB_BND, 1, &
     &    'RMS of the fringe amplitude noise (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1445, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'QUALCODE', GVH__C1, GVH__BAS, 1, NUMB_BND, &
     &    'Quality code as a char*1 value: 5-9 is good, '// &
     &    '0 -- non-detection, letter -- failure', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1446, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      GVH%SEG = 2
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'AUTO_SUP', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of automatic suppression status for '// &
     &    'combination of observables', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1447, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'USER_SUP', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of analyst defined suppression status for '// &
     &    'combination of observables', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1448, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'USER_REC', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of analyst defined recovery status for '// &
     &    'combination of observables', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1449, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_CALIB ', GVH__I2, GVH__SES, 1, 1, &
     &    'Number of available calibrations', M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1450, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'AZIMUTH ', GVH__R8, GVH__STA, 1, 1, &
     &                'Apparent source azimuth at both stations of '// &
     &                'the baseline (rad)', M2G__TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1451, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ELEV    ', GVH__R8, GVH__STA, 1, 1, &
     &                'Apparent source elevation at both stations of '// &
     &                'the baseline (rad)', M2G__TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1452, IUER, 'SIMUL_TO_GVF', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! --- Prepare GVH object for putting new lcodes
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PREPUT ( GVH, SIMUL%NOBS, SIMUL%NSCA, SIMUL%NSTA, SIMUL%NOBS_STA, &
     &                  SIMUL%STA_NAM, SIMUL%OBSTAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1453, IUER, 'SIMUL_TO_GVF', 'Error in an '// &
     &         'attempt to load mandatory lcodes to the output database' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'COR_TYPE', 1, 0, 'Simul', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1454, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"COR_TYPE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EXP_CODE', 1, 0, SIMUL%EXPER_NAME, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1455, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"EXP_NAME " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EXP_DESC', 1, 0, SIMUL%EXPER_DESCR(1:80), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1456, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"EXP_DESC" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PI_NAME ', 1, 0, SIMUL%PI_NAME, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1457, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"PI_NAME " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DATYP   ', 1, 0, SIMUL__DATYP__DEF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1458, IUER, 'SIMUL_TO_GVF', 'Error in '// &
     &         'putting "DATYP   " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SUPMET  ', 1, 0, SIMUL__SUPMET__DEF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1459, IUER, 'SIMUL_TO_GVF', 'Error in '// &
     &         'putting "SUPMET   " lcode' )
           RETURN
      END IF
!
      TOT_UTC_1ST_OBS = SIMUL%UTC_OBS(1) + (SIMUL%MJD_OBS(1) - J2000__MJD)*86400.0D0
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_UTCMTAI ( NERS, TOT_UTC_1ST_OBS, UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1460, IUER, 'SIMUL_TO_GVF', 'Error in gettting '// &
     &         'UTC minus TAI function' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'UTC_MTAI', 1, 0, UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1461, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"UTC_MTAI" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SIT_COOR', 1, 0, SIMUL%STA_COO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1462, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"SIT_COOR" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUMB_SOU', 1, 0, SIMUL%NSOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1463, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"NUMB_SOU" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SRCNAMES', 1, 0, SIMUL%SOU_NAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1464, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"NUMB_SOU" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SOU_COOR', 1, 0, SIMUL%SOU_COO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1465, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"SOU_COOR" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_BAND', 1, 0, 1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1466, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"NUM_BAND" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'N_AVBAND', 1, 0, 1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1467, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"N_AVBAND" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_CHBN', 1, 0, 1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1468, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"NUM_CHBN" lcode' )
           RETURN
      END IF
!      
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SKYFRQCH', 1, 0, SKY_FREQ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1469, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"SKYFRQCH" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'N_CALIB ', INT2(1), 0, 0, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1470, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &         '"N_CALIB " lcode' )
           RETURN
      END IF
!
      DO 410 J1=1,SIMUL%NOBS
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'SCANNAME', J1, 0, SIMUL%SCAN_NAME(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1471, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"SCANNAME" lcode' )
             RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'MJD_OBS ', J1, 0, SIMUL%MJD_OBS(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1472, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"MJD_OBS" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'UTC_OBS ', J1, 0, SIMUL%UTC_OBS(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1473, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"UTC_OBS" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'SCAN_DUR', J1, 0, SIMUL%SCAN_DUR(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1474, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"SCAN_DUR" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'SOU_IND ', J1, 0, SIMUL%SOU_IND(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1475, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"STA_IND" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'STA_IND ', J1, 0, SIMUL%STA_IND(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1476, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"STA_IND" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'GR_DELAY', J1, 0, SIMUL%GR_DEL(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1477, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"GR_DELAY" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'DEL_RATE', J1, 0, SIMUL%PH_RAT(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1478, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"DEL_RATE" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'GRDELERR', J1, 0, SIMUL%GR_DEL_ERR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1479, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"GRDELERR" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'PHRATERR', J1, 0, SIMUL%PH_RAT_ERR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1480, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"PHRATERR" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'SNRATIO ', J1, 0, SIMUL%SNR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1481, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"SNRATIO " lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'EFF_FREQ', J1, 0, SIMUL%EFF_FREQ(1,1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1482, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"EFF_FREQ" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'REF_FREQ', J1, 0, SIMUL%REF_FREQ(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1483, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"REF_FREQ" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'SAMPLRAT', 1, 0, SIMUL%SAMPLE_RATE(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1484, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"SAMPLRAT" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'BITSAMPL', 1, 0, SIMUL%BITS_SAMPLE(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1485, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"BITSAMPL" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'FRN_AMPL', J1, 0, SIMUL%AMP(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1486, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"FRN_AMPL" lcode' )
              RETURN
         END IF
!
         NOI_ARR(1:NUMB_BND) = SIMUL%NOI(1:NUMB_BND,J1) ! NB real*8 --> real*4 conversion
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'NOISERMS', J1, 0, NOI_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1487, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"NOISERMS" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'AUTO_SUP', J1, 0, SIMUL%AUTO_SUP(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1488, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"AUTO_SUP" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'USER_SUP', J1, 0, SIMUL%USER_SUP(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1489, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"USER_SUP" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'USER_REC', J1, 0, SIMUL%USER_REC(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1490, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"USER_REC" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'QUALCODE', J1, 0, SIMUL%QUALCODE(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1491, IUER, 'SIMUL_TO_GVF', 'Error in putting '// &
     &            '"QUALCODE" lcode' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'AZIMUTH ', J1, 1, SIMUL%AZ(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1492, IUER, 'SIMUL_TO_GVF', 'Error in '// &
     &             'putting "AZIMUTH" lcode for the first station' )
              RETURN 
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'AZIMUTH ', J1, 2, SIMUL%AZ(2,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1493, IUER, 'SIMUL_TO_GVF', 'Error in '// &
     &             'putting "AZIMUTH" lcode for the second station' )
              RETURN 
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'ELEV    ', J1, 1, SIMUL%EL(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1494, IUER, 'SIMUL_TO_GVF', 'Error in '// &
     &             'putting "ELEV    " lcode for the first station' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'ELEV    ', J1, 2, SIMUL%EL(2,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1495, IUER, 'SIMUL_TO_GVF', 'Error in '// &
     &             'putting "ELEV    " lcode for the second station' )
              RETURN 
         END IF
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SIMUL_TO_GVF  !#!#
