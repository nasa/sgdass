      SUBROUTINE PIMA_GVH_DEFINE ( PIM, GVH, NUMB_OBS, NUMB_STA, NUMB_SOU, &
     &                             NUMB_SCA, NUMB_BAS, NUMB_BND, NUMB_FRQ, &
     &                             OBS_TAB, NOBS_STA, C_STA, L_STA_CAB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GVH_DEFINE
! *                                                                      *
! * ### 10-JUL-2009  PIMA_GVH_DEFINE  v1.4 (c) L. Petrov 25-NOV-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      INCLUDE   'gvh.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( CAL__INFO__TYPE ) :: CAL_INFO(M__CAL)
      INTEGER*4  NUMB_OBS, NUMB_STA, NUMB_SOU, NUMB_SCA, NUMB_BAS, &
     &           NUMB_BND, NUMB_FRQ(NUMB_BND), OBS_TAB(3,NUMB_OBS), &
     &           NOBS_STA(NUMB_STA), L_STA_CAB, J1, IUER
      CHARACTER  C_STA(NUMB_STA)*(*)
      INTEGER*4  N_CALIB, N_NBD, IER
!
      GVH%NSEG = 4
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUMB_SOU', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of observed sources', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8101, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_CHAN', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of frequency channels at all bands', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8102, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_CHBN', GVH__I4, GVH__SES, 1, NUMB_BND, &
     &    'Number of frequency channels per band', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8103, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_BAND', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of frequency bands observed in the experiment', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8104, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_AVBAND', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of frequency bands for which information is available', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8105, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SKYFRQCH', GVH__R8, GVH__SES, PIM%NFRQ, 1, &
     &    'Sky frequency of channels in Hz', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8106, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EFF_FREQ', GVH__R8, GVH__BAS, 3, 2, &
     &    'Effective ionospheric frequencies for gr.del, ph.del, '// &
     &    'ph.rate per band (Hz)', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8107, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SIT_COOR', GVH__R8, GVH__SES, 3, NUMB_STA, &
     &    'Site coordinates in a crust-fixed terrestrial reference '// &
     &    'system: X, Y, Z (meters)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'NUMB_STA= ', NUMB_STA
           CALL ERR_LOG ( 8108, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOU_COOR', GVH__R8, GVH__SES, 2, NUMB_SOU, &
     &    'Source coordinates in a barycenteric reference system: '// &
     &    'right asc. decl. (rad)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8109, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BAND_NAM', GVH__C1, GVH__SES, 1, NUMB_BND, &
     &    'Band names', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8110, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EXPSERNO', GVH__I2, GVH__SES, 1, 1, &
     &    'Experiment serial number at correlator', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8111, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CORPLACE', GVH__C1, GVH__SES, 32, 1, &
     &    'Correlator place name', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8112, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'COR_TYPE', GVH__C1, GVH__SES, 8, 1, &
     &    'Correlator type: MK3 MK4 K4 S2 VLBA MITAKA-1 KJCC SFXC DiFX Simul', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8113, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'COR_VERS', GVH__C1, GVH__SES, 8, 1, &
     &    'Correlator software and/or hardware version', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8114, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EXP_CODE', GVH__C1, GVH__SES, 32, 1, &
     &    'Experiment code', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8115, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'MK3_DBNM', GVH__C1, GVH__SES, 10, 1, &
     &    'Mark-3 DBH database name', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8116, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EXP_NAME', GVH__C1, GVH__SES, 80, 1, &
     &    'Experiment program name', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8117, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EXP_DESC', GVH__C1, GVH__SES, 80, 1, &
     &    'Experiment description', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8118, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PI_NAME', GVH__C1, GVH__SES, 80, 1, &
     &    'Name of the principal investigator', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8119, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'REC_MODE', GVH__C1, GVH__SES, 80, 1, &
     &    'Recording mode', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8120, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SAMPLRAT', GVH__R8, GVH__SES, 1, 1, &
     &    'Sample rate in Hz', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8121, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BITSAMPL', GVH__I2, GVH__SES, 1, 1, &
     &    'Number of bits per sample', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8122, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PIMA_VER', GVH__C1, GVH__SES, 24, 1, &
     &    'Version of PIMA software', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8123, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PIMA_CNT', GVH__C1, GVH__SES, 128, 1, &
     &    'Full path name of pima control file', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8124, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CHAN_WDT', GVH__R8, GVH__SES, PIM%NFRQ, 1, &
     &    'Frequency channel width in Hz per channel', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8125, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CHAN_SDB', GVH__I2, GVH__SES, PIM%NFRQ, 1, &
     &    'Index of lower (-1) or upper (1) sideband per channel', &
     &    M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8126, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SPCH_WDT', GVH__R8, GVH__SES, PIM%NFRQ, 1, &
     &    'Spectral channel width in Hz per channel', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8127, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RWBASNAM', GVH__C1, GVH__SES, 16, &
     &                NUMB_BAS, 'Baseline names for additive '// &
     &                'baseline-dependent reweighting parameters ', &
     &                M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8128, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RWDELVAL', GVH__R8, GVH__SES, SLV__MAX_SOLTYP, &
     &                NUMB_BAS, 'Additive baseline-dependent reweighting '// &
     &               'parameters for delays (sec)', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8129, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RWRATVAL', GVH__R8, GVH__SES, SLV__MAX_SOLTYP, &
     &                NUMB_BAS, 'Additive baseline-dependent reweighting '// &
     &               'parameters for delay rates (d/l)', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8130, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'MEANCABL', GVH__R8, GVH__SES, &
     &                NUMB_STA, 1, 'Mean cable delay (sec)', &
     &                M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8131, IUER, 'PIMA_GVH_DEFINE', &
     &         'Trap of internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CABL_SGN', GVH__I2, GVH__SES, NUMB_STA, &
     &                1, 'Cable sign: +1 or -1', M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8132, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CABL_DEL', GVH__R8, GVH__STA, 1, &
     &                1, 'Cable delay (sec)', M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8133, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SCANNAME', GVH__C1, GVH__SCA, 16, 1, &
     &    'Scan name', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8134, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SCANPIMA', GVH__C1, GVH__SCA, 10, 1, &
     &    'Pima internal scan name', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8135, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SRCNAMES', GVH__C1, GVH__SES, 8, NUMB_SOU, &
     &    'Source names', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8136, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'UTC_MTAI', GVH__R8, GVH__SES, 1, 1, &
     &    'Difference UTC minus TAI at first time tag of the '// &
     &    'database (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8137, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
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
           CALL ERR_LOG ( 8138, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'UTC_OBS ', GVH__R8, GVH__SCA, 1, 1, &
     &    'Pseudo-UTC time tag of fringe reference time for the scan (sec)', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8139, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOU_IND ', GVH__I4, GVH__SCA, 1, 1, &
     &    'Source name index', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8140, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STA_IND', GVH__I4, GVH__BAS, 2, 1, &
     &    'Station names indexes', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8141, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GR_DELAY', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Group delays per band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8142, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DEL_RATE', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Phase delay rates per band (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8143, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SB_DELAY', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Single-band delays per band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8144, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TOTPHASE', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Total fringe phases at time of arrival signal at '// &
     &    'station 1 per band (rad)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8145, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PHDELERR', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Phase delay error (rad)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8146, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GR_RATE ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Group delays rate per band (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8147, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GRDELERR', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Group delay errors per band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8148, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GRRATERR', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Group delay rate errors per band (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8149, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SBDELERR', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Single-band delay errors per band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8150, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SNRATIO ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Fringe amplitude signal to noise ratio (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8151, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PHRATERR', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Phase delay rate delay errors per band (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8152, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GDAMBSP ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Group delay ambiguity spacings per band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8153, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'FRN_AMPL', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Normalized fringe amplitude in range [0, 1]', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8154, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'REF_FREQ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Reference frequency for phase delay per band (Hz)', &
     &     M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8155, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GC_PHASE', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Fringe phase at time of arrival signal at the conventional '// &
     &    'geocenter (rad)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8156, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RESMBDEL', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual multiband group delay (sec)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8157, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RESSBDEL', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual singleband group delay (sec)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8158, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RESPHRAT', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual phase delay rate (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8159, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RESGRRAT', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual group delay rate (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8160, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RESPHAS ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual fringe phase', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8161, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'GCRESPHS', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual fringe phase at time of arrival signal at '// &
     &    'geocenter (rad)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8162, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APR_DEL ', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Apriori delay (sec)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8163, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APR_RATE', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Apriori delay rate (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8164, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APR_PHAS', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Apriori fringe phase (rad)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8165, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APR_PHGC', GVH__R8, GVH__BAS, 2, NUMB_BND, &
     &    'Apriori fringe phase per station at the geocenter (rad)', &
     &     M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8166, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RES_GRDL', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual group delay reported by the post-correlator software (sec)', &
     &     M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8167, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RES_RATE', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Apriori phase delay rate reported by the post-correlator software (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8168, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RES_PHGC', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Residual fringe phase per station at the geocenter (rad)', &
     &     M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8169, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PIND_OBS', GVH__I4, GVH__BAS, 1, 1, &
     &    'Internal index of observation used by PIMA', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8170, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'UVSTAORD', GVH__I2, GVH__BAS, 1, 1, &
     &    'Original station order in the baseline: 1 (ascending) or '// &
     &    '-1 (descending)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8171, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NOISERMS', GVH__R4, GVH__BAS, NUMB_BND, 1, &
     &    'RMS of the fringe amplitude noise (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8172, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'FRT_OFFS', GVH__R8, GVH__SCA, NUMB_BND, 1, &
     &    'Fringe reference time offset relative to the scan start (sec)', &
     &     M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8173, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SRT_OFFS', GVH__R8, GVH__SCA, 1, 1, &
     &    'Scan reference time offset relative to the scan start (sec)', &
     &     M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8174, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APRCLOOF', GVH__R8, GVH__STA, 1, 1, &
     &    'Apriori clock offset used for correlation', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8175, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APRCLORT', GVH__R8, GVH__STA, 1, 1, &
     &    'Apriori clock rate used for correlation', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8176, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ANT_GAIN', GVH__R4, GVH__STA, NUMB_BND, 1, &
     &    'Antenna gain per band K/Jy', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8177, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DELW_CEN', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Delay window center used for fringe search (sec)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8178, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DELW_WDT', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Delay window width used for fringe search (sec)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8179, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RATE_CEN', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Rate window center used for fringe search (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8180, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RATE_WDT', GVH__R8, GVH__BAS, NUMB_BND, 1, &
     &    'Rate window width used for fringe search (d/l)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8181, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'POLARIZ ', GVH__C1, GVH__BAS, NUMB_BND, 2, &
     &    'Polarization label: RR, RL, LR, LL, HH, HV, VH, VV, or I', &
     &     M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8182, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ATM_PRES', GVH__R8, GVH__STA, 1, 1, &
     &    'Atmospheric pressure at the station (Pa)', M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8183, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'AIR_TEMP', GVH__R8, GVH__STA, 1, 1, &
     &    'Air temperature at the station (K)', M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8184, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'REL_HUMD', GVH__R8, GVH__STA, 1, 1, &
     &    'Relative humidity at the station (0-1)', M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8185, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUSEDCHN', GVH__I2, GVH__BAS, NUMB_BND, 1, &
     &    'Number of channels used in bandwidth synthesis per band', &
     &     M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8186, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'IND_CHN1', GVH__I2, GVH__BAS, NUMB_FRQ(1), 1,&
     &     'Indexes of channels used in bandwidth synthesis in band 1', &
     &      M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8187, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_SAM1', GVH__R8, GVH__BAS, NUMB_FRQ(1), 1,&
     &    'Number of samples used in bandwidth synth. in band 1 per '// &
     &    'frequency chan', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8188, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_AP1 ', GVH__I2, GVH__BAS, NUMB_FRQ(1), 2, &
     &    'Number of accumulation periods used in band 1 per channel '//  &
     &    'per sideband', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8189, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'UV_CHN1 ', GVH__R4, GVH__BAS, 2, &
     &                NUMB_FRQ(1), 'UV data: real and image '// &
     &                'part per channel at the 1st band (d/l)', &
     &                M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8190, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &          'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TSYS1   ', GVH__R4, GVH__STA, 1, NUMB_FRQ(1), &
     &                'System temperature per channel at the '// &
     &                '1st band (K)', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8191, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &          'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PCAL_FR1', GVH__R8, GVH__STA, 1, NUMB_FRQ(1), &
     &                'Phase cal frequency per channel at the 1st band', &
     &                M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8192, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &          'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'PCAL_CM1', GVH__R4, GVH__STA, 2, NUMB_FRQ(1), &
     &                'Complex phase cal (real/image) per '// &
     &               'channel at the 1st band', M2G__FR2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8193, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &          'internal control' )
           RETURN
      END IF
!
      IF ( NUMB_BND > 1 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'IND_CHN2', GVH__I2, GVH__BAS, NUMB_FRQ(2), 1, &
     &         'Indexes of channels used in bandwidth synthesis in band 2', &
     &          M2G__FR2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8194, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'NUM_SAM2', GVH__R8, GVH__BAS, NUMB_FRQ(2), 1, &
     &         'Number of samples used in bandwidth synth. in band 2 per '// &
     &         'frequency chan', M2G__FR2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8195, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'NUM_AP2 ', GVH__I2, GVH__BAS, NUMB_FRQ(2), 2, &
     &         'Number of accumulation periods used in band 2 per channel '// &
     &         'per sideband', M2G__FR2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8196, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'UV_CHN2 ', GVH__R4, GVH__BAS, 2, &
     &               NUMB_FRQ(2), 'UV data: real and image part per '// &
     &              'channel at the 2nd band (d/l)', M2G__FR2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8197, IUER, 'PIMA_GVH_DEFINE', 'Trap '// &
     &              'of internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'TSYS2   ', GVH__R4, GVH__STA, 1, &
     &                     NUMB_FRQ(2), 'System temperature per channel '// &
     &                    'at the 2nd band (K)', M2G__FR2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8198, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'PCAL_CM2', GVH__R4, GVH__STA, 2, NUMB_FRQ(2), &
     &                   'Complex phase cal (real/image) per '// &
     &                   'channel at the 2nd band', M2G__FR2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8199, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'PCAL_FR2', GVH__R8, GVH__STA, 1, NUMB_FRQ(2), &
     &                    'Phase cal frequency per channel at the 2nd band', &
     &                     M2G__FR2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8200, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_GRAMB ', GVH__I4, GVH__BAS, NUMB_BND, 1, &
     &    'Number of group delay ambiguities to be added to measured '// &
     &    'group delays per band', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8201, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'QUALCODE', GVH__C1, GVH__BAS, 1, NUMB_BND, &
     &    'Quality code as a char*1 value: 5-9 is good, '// &
     &    '0 -- non-detection, letter -- failure', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8202, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
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
           CALL ERR_LOG ( 8203, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'USER_SUP', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of analyst defined suppression status for '// &
     &    'combination of observables', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8204, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'USER_REC', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of analyst defined recovery status for '// &
     &    'combination of observables', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8205, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ION_GDEL', GVH__R8, GVH__BAS, 1, 1, &
     &    'Ionospheric contribution to group delay at the first '// &
     &    'band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8206, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ION_GERR', GVH__R8, GVH__BAS, 1, 1, &
     &    'Uncertainty of ionospheric contribution to group delay '// &
     &    'at the first band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8207, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ION_PRAT', GVH__R8, GVH__BAS, 1, 1, &
     &    'Ionospheric contribution to phase delay rate at the first '// &
     &    'band (sec)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8208, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ION_RERR', GVH__R8, GVH__BAS, 1, 1, &
          'Uncertainty of ionospheric contribution to phase delay rate '// &
     &    'at the 1st band (d/l)', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8209, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APLENGTH', GVH__R8, GVH__SES, 1, 1, &
     &               'Length of accumul. period in sec', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8210, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_SPCH', GVH__I4, GVH__SES, 1, 1, &
     &               'Number of spectral channels within an intermediate frequency band', M2G__FR1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8211, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BAND_2ND', GVH__I2, GVH__BAS, 1, 1, &
     &    'Bit field with status of information about the second band '// &
     &    'observations', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8212, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_PHAMB ', GVH__I4, GVH__BAS, NUMB_BND, 1, &
     &    'Number of phase delay ambiguities to be added to measured '// &
     &    'phase delays per band', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8213, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_CLBR', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of clock breaks in the experiment', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8214, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SCAN_DUR', GVH__R8, GVH__BAS, NUMB_BND, &
     &                1, 'Scan duration per band (sec)', M2G__FR2, &
     &                IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8215, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EDIT_STS', GVH__I4, GVH__SES, 1, 1, &
     &    'Bit field of database editing status for different types '// &
     &    'of solutions', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8216, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DATYP   ', GVH__I2, GVH__SES, 1, 1, &
     &    'Type of the observable or a combination of observables '// &
     &    'used in the solution', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8217, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SUPMET  ', GVH__I2, GVH__SES, 1, 1, &
     &    'Code of the suppression method used in the solution', &
     &     M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8218, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      IF ( L_STA_CAB > 0 ) THEN
           N_CALIB = 1
         ELSE
           N_CALIB = 0
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_CALIB ', GVH__I2, GVH__SES, 1, 1, &
     &     'Number of available calibrations', M2G__CL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8219, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      IF ( N_CALIB > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'CAL_NAME', GVH__C1, GVH__SES, 8, &
     &          N_CALIB, 'Name of available calibrations', M2G__CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8220, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'CAL_INFO', GVH__I4, GVH__SES, &
     &          SIZEOF(CAL_INFO(1))/4, N_CALIB, 'Information about '// &
     &         'class and type of available calibrations', M2G__CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8221, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'CAL_STS ', GVH__I4, GVH__SES, NUMB_STA, &
     &          N_CALIB, 'Bit field of using available calibrations '//&
     &         'per station, per calibration', M2G__SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8222, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BAS_USE ', GVH__I4, GVH__SES, NUMB_BAS, 1, &
     &                'Bit field of baseline selection status', &
     &                M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8223, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &                   'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BSCL_EST', GVH__I4, GVH__SES, NUMB_BAS, 1, &
     &     'Estimation status for baseline dependent clock, '// &
     &     'per baseline', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8224, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &                   'internal control' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOU_USE ', GVH__I4, GVH__SES, NUMB_SOU, &
     &                1, 'Bit field of source selection status', &
     &                M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8225, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOCO_EST', GVH__I4, GVH__SES, 2, &
     &                NUMB_SOU, 'Estimation status for source '// &
     &               'coordinats per component, per object', &
     &                M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8226, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TEC_STS ', GVH__I4, GVH__SES, NUMB_STA, 1, &
     &    'Flag of availability/usage of the external ionosphere '// &
     &    'calibration', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8227, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
      CALL GVH_PTOC ( GVH, 'CLO_CNS ', GVH__R8, GVH__SES, NUMB_STA, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of '// &
     &               'constraints on clock rate per station, '// &
     &               'per solution type', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8228, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ATM_CNS ', GVH__R8, GVH__SES, NUMB_STA, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of '// &
     &               'constraints on atm. path delay rate per '// &
     &               'station, soltype', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8229, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BSCL_CNS', GVH__R8, GVH__SES, NUMB_BAS, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on basdep. clock, per baseline, per soltype', &
     &                M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8230, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CLO_INTR', GVH__R8, GVH__SES, &
     &                SLV__MAX_SOLTYP, 1, 'Lenght of time span '// &
     &               'between spline nodes for clock function per '// &
     &               'soltyp (sec)', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8231, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_BASCAL', GVH__I4, GVH__SES, 1, 1, &
     &               'Number of external baseline-dependent '// &
     &               'calibrations', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8232, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_STACAL', GVH__I4, GVH__SES, 1, 1, &
     &               'Number of external station-dependent '// &
     &               'calibrations', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8233, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_CLRF', GVH__I4, GVH__SES, 1, 1, &
     &               'Number of clock reference stations', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8234, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STA_CLRF', GVH__C1, GVH__SES, 8, 1, &
     &               'Names of clock reference stations', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8235, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DB_VERS ', GVH__I2, GVH__SES, 1, 1, &
     &               'Database_version', M2G__SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8236, IUER, 'PIMA_GVH_DEFINE', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
! === End of the section for gathering information about lcodes
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  do 410 j1=1,numb_obs
!      write ( 6, * ) ' numb_obs= ', int2(j1), ' obs_sta= ',  obs_tab(1:3,j1)
!  call pause ( 'pima_gvh_define  1307' ) ! %%%%%%%%%%%5
! 410  continue 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      GVH%SEG = 1
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PREPUT ( GVH, NUMB_OBS, NUMB_SCA, NUMB_STA, NOBS_STA, &
     &                  C_STA, OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8237, IUER, 'PIMA_GVH_DEFINE', 'Error in PREPUT' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GVH_DEFINE  !#!
