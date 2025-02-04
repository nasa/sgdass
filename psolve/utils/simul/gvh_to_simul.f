      SUBROUTINE GVH_TO_SIMUL ( GVH, VTD, NERS, SIMUL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVH_TO_SIMUL
! *                                                                      *
! *  ### 16-JUN-2020  GVH_TO_SIMUL  v1.3 (c) L. Petrov  01-DEC-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      INCLUDE   'simul.i'
      INCLUDE   'vtd.i'
      TYPE     ( GVH__STRU   ) :: GVH
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      TYPE     ( NERS__TYPE  ) :: NERS
      TYPE     ( VTD__TYPE   ) :: VTD
      INTEGER*4  IUER
      INTEGER*4  M_PAR
      PARAMETER  ( M_PAR = 64 )
      CHARACTER  DESCR*256, C_SCA(SIM__MSCA)*16
      REAL*8     PARS(M_PAR), ARR_R8(128)
      REAL*4     ARR_R4(128)
      REAL*8     AZ(2), EL(2), HA(2), AZ_RATE(2), EL_RATE(2), HA_RATE(2), TIM_TAI
      ADDRESS__TYPE  ADR_DATA
      INTEGER*4  J1, J2, J3, J4, J5, L_PAR, DIMS(2), CLASS, TYP, &
     &           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, L_SCA, IER
      INTEGER*4, EXTERNAL :: ADD_CLIST
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'EXP_CODE', 0, 0, LEN(SIMUL%EXPER_NAME), &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%EXPER_NAME), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1511, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode EXP_CODE' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'EXP_DESC', 0, 0, LEN(SIMUL%EXPER_DESCR), &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%EXPER_DESCR), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1512, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode EXP_DESC' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'PI_NAME ', 0, 0, LEN(SIMUL%PI_NAME), &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%PI_NAME), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1513, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode PI_NAME' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NOBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1514, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SOU', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NSOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1515, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode NUMB_SOU' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NSTA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1516, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode NUMB_STA' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NSCA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1517, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NOBS_STA', 0, 0, 4*SIMUL%NSTA, DIMS(1), DIMS(2), SIMUL%NOBS_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1518, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 0, 0, 3*4*SIMUL%NOBS, DIMS(1), DIMS(2), &
     &                     SIMUL%OBSTAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1519, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode OBS_TAB' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'N_AVBAND', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NBND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1520, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode N_AVBAND' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( 0, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_MTAI', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  SIMUL%UTC_MTAI, IER )
      IF ( IER .NE. 0 .OR. SIMUL%UTC_MTAI < -90.0 .OR. SIMUL%UTC_MTAI > 200.0 ) THEN
!
           CALL ERR_PASS   ( IUER, IER )
           CALL NERS_GET_EOP ( NERS, -1.0D15, 'utcmtai', NERS__MPAR, L_PAR, PARS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 1521, IUER, 'GVF_TO_SIMUL', 'Error evaluating the '// &
          &         'Earth orientation parameter' )
                RETURN 
           END IF
           SIMUL%UTC_MTAI = PARS(1)
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, 8*SIMUL%NSTA, &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%STA_NAM), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1522, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SRCNAMES', 0, 0, 8*SIMUL%NSOU, &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%SOU_NAM), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1523, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN 
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'SIT_COOR', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1524, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'inquiring lcode N_CALIB ' )
           RETURN
      END IF
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SIT_COOR', 0, 0, 3*8*SIMUL%NSTA, &
     &                       DIMS(1), DIMS(2), SIMUL%STA_COO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1525, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode SIT_COOR' )
                RETURN 
           END IF
         ELSE
           SIMUL%STA_COO = 0.0D0
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'SOU_COOR', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1526, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'inquiring lcode N_CALIB ' )
           RETURN
      END IF
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SOU_COOR', 0, 0, 2*8*SIMUL%NSOU, &
     &                       DIMS(1), DIMS(2), SIMUL%SOU_COO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1527, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode SOU_COOR' )
                RETURN 
           END IF
        ELSE 
           SIMUL%SOU_COO = 0.0D0
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'SAMPLRAT', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1528, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'inquiring lcode SAMPLRAT' )
           RETURN
      END IF
      IF ( CLASS .EQ. GVH__SES ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SAMPLRAT', 0, 0, 8, &
     &                       DIMS(1), DIMS(2), SIMUL%SAMPLE_RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1529, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode SAMPLRAT' )
                RETURN 
           END IF
           SIMUL%SAMPLE_RATE(1:SIMUL%NSTA) = SIMUL%SAMPLE_RATE(1)
        ELSE IF ( CLASS .EQ. GVH__STA ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SAMPLRAT', 0, 0, 8*SIMUL%NSTA, &
     &                       DIMS(1), DIMS(2), SIMUL%SAMPLE_RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1530, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode SAMPLRAT' )
                RETURN 
           END IF
        ELSE
           SIMUL%SAMPLE_RATE = 0.0D0
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'BITSAMPL', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1531, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'inquiring lcode BITS_SAMPLE' )
           RETURN
      END IF
      IF ( CLASS .EQ. GVH__SES ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'BITSAMPL', 0, 0, 8, &
     &                       DIMS(1), DIMS(2), SIMUL%BITS_SAMPLE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1532, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode BITS_SAMPLE' )
                RETURN 
           END IF
           SIMUL%BITS_SAMPLE(1:SIMUL%NSTA) = SIMUL%BITS_SAMPLE(1)
        ELSE IF ( CLASS .EQ. GVH__STA ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SAMPLRAT', 0, 0, 8*SIMUL%NSTA, &
     &                       DIMS(1), DIMS(2), SIMUL%BITS_SAMPLE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1533, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode BITS_SAMPLE' )
                RETURN 
           END IF
        ELSE
           SIMUL%BITS_SAMPLE = 0
      END IF
!
      SIMUL%SCA_IND = 0
      DO 410 J1=1,SIMUL%NOBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SCANNAME', J1, 0, 16, DIMS(1), DIMS(2), &
     &                     %REF(SIMUL%SCAN_NAME(J1)), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1534, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode SCANNAME' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         SIMUL%SCA_IND(J1) = ADD_CLIST ( SIM__MSCA, L_SCA, C_SCA, &
     &                                   SIMUL%SCAN_NAME(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1535, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'adding the scan name to the list' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'MJD_OBS ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     SIMUL%MJD_OBS(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1536, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode MJD_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'UTC_OBS ', J1, 0, 8, DIMS(1), DIMS(2), &
     &                     SIMUL%UTC_OBS(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1537, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode UTC_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SOU_IND ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     SIMUL%SOU_IND(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1538, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode SOU_IND' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'STA_IND ', J1, 0, 2*4, DIMS(1), DIMS(2), &
     &                     SIMUL%STA_IND(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1539, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode STA_IND' )
              RETURN
         END IF
!
         CALL ERR_PASS      ( IUER, IER )
         CALL GVH_INQ_LCODE ( GVH, 'SCAN_DUR', DESCR, CLASS, TYP, DIMS, &
     &                        NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                        IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1540, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'inquiring lcode SCAN_DUR' )
              RETURN
         END IF
!
         CALL ERR_PASS      ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SCAN_DUR', J1, 1, SIMUL%NBND*8, DIMS(1), &
     &                     DIMS(2), ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1541, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode SCAN_DUR' )
              RETURN
         END IF
         SIMUL%SCAN_DUR(J1) = ARR_R8(1)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GR_DELAY', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1542, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode GR_DELAY' )
              RETURN
         END IF
         SIMUL%GR_DEL(1:2,J1) = ARR_R8(1:2)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'DEL_RATE', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1543, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode DEL_RATE' )
              RETURN
         END IF
         SIMUL%PH_RAT(1:2,J1) = ARR_R8(1:2)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GRDELERR', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1544, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode GRDELERR' )
              RETURN
         END IF
         SIMUL%GR_DEL_ERR(1:2,J1) = ARR_R8(1)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'PHRATERR', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1545, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode PHRATERR' )
              RETURN
         END IF
         SIMUL%PH_RAT_ERR(1:2,J1) = ARR_R8(1)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SNRATIO ', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     SIMUL%SNR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1546, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode SNRATIO' )
              RETURN
         END IF
         SIMUL%PH_RAT_ERR(1:2,J1) = SIMUL%SNR(1,J1)
!
         CALL ERR_PASS      ( IUER, IER )
         CALL GVH_INQ_LCODE ( GVH, 'EFF_FREQ', DESCR, CLASS, TYP, DIMS, &
     &                        NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                        IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1547, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'inquiring lcode ION_GDEL' )
              RETURN
         END IF
         IF ( CLASS .NE. 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'EFF_FREQ', J1, 1, 6*8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1548, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &                 'getting lcode ION_RERR for the second station' )
                   RETURN
              END IF
            ELSE
              ARR_R8 = 0.0D0
         END IF
         SIMUL%EFF_FREQ(1:3,1,J1) = ARR_R8(1:3)
         SIMUL%EFF_FREQ(1:3,2,J1) = ARR_R8(4:6)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'REF_FREQ', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1549, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode REF_FREQ' )
              RETURN
         END IF
         SIMUL%REF_FREQ(1:SIMUL%NBND,J1) = ARR_R8(1:SIMUL%NBND)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'FRN_AMPL', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     SIMUL%AMP(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1550, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode FRN_AMPL' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'NOISERMS', J1, 0, SIMUL%NBND*4, DIMS(1), DIMS(2), &
     &                     ARR_R4, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1551, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode NOISERMS' )
              RETURN
         END IF
         SIMUL%NOI(1:SIMUL%NBND,J1) = ARR_R4(1:SIMUL%NBND)
!
         CALL ERR_PASS      ( IUER, IER )
         CALL GVH_INQ_LCODE ( GVH, 'AUTO_SUP', DESCR, CLASS, TYP, DIMS, &
     &                        NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                        IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1552, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'inquiring lcode AUTO_SUP' )
              RETURN
         END IF
         IF ( CLASS .NE. 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'AUTO_SUP', J1, 1, 4, DIMS(1), DIMS(2), &
     &                          SIMUL%AUTO_SUP(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1553, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &                 'getting lcode AUTO_SUP for the second station' )
                   RETURN
              END IF
         END IF
!
         CALL ERR_PASS      ( IUER, IER )
         CALL GVH_INQ_LCODE ( GVH, 'USER_SUP', DESCR, CLASS, TYP, DIMS, &
     &                        NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                        IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1554, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'inquiring lcode USER_SUP' )
              RETURN
         END IF
!
         IF ( CLASS .NE. 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'USER_SUP', J1, 1, 4, DIMS(1), DIMS(2), &
     &                          SIMUL%USER_SUP(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1555, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &                 'getting lcode USER_SUP for the second station' )
                   RETURN
              END IF
         END IF
!
         CALL ERR_PASS      ( IUER, IER )
         CALL GVH_INQ_LCODE ( GVH, 'USER_REC', DESCR, CLASS, TYP, DIMS, &
     &                        NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                        IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1556, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'inquiring lcode USER_REC' )
              RETURN
         END IF
!
         IF ( CLASS .NE. 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'USER_REC', J1, 1, 4, DIMS(1), DIMS(2), &
     &                          SIMUL%USER_REC(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1557, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &                 'getting lcode USER_REC for the second station' )
                   RETURN
              END IF
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'QUALCODE', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     %REF(SIMUL%QUALCODE(J1)), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1558, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &            'getting lcode QUALCODE' )
              RETURN
         END IF
!
         TIM_TAI = (SIMUL%MJD_OBS(J1) - J2000__MJD)*86400.0D0 + SIMUL%UTC_OBS(J1) + SIMUL%UTC_MTAI
         CALL ERR_PASS ( IUER, IER )
         CALL NERS_AZELHA_COMP ( NERS, TIM_TAI, SIMUL%STA_COO(1,SIMUL%STA_IND(1,J1)), &
     &                           SIMUL%SOU_COO(1,SIMUL%SOU_IND(J1)), &
     &                           SIMUL%SOU_COO(2,SIMUL%SOU_IND(J1)), &
     &                          'radio', SIMUL%AZ(1,J1), SIMUL%EL(1,J1), &
     &                           HA(2), AZ_RATE(1), EL_RATE(1), HA_RATE(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1559, IUER, 'VEX_TO_SIMUL', 'Error in '// &
     &            'computing azimuth and elevation for staion '// &
     &             SIMUL%STA_NAM(SIMUL%STA_IND(1,J1)) )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL NERS_AZELHA_COMP ( NERS, TIM_TAI, SIMUL%STA_COO(1,SIMUL%STA_IND(2,J1)), &
     &                           SIMUL%SOU_COO(1,SIMUL%SOU_IND(J1)), &
     &                           SIMUL%SOU_COO(2,SIMUL%SOU_IND(J1)), &
     &                          'radio', SIMUL%AZ(2,J1), SIMUL%EL(2,J1), &
     &                           HA(2), AZ_RATE(2), EL_RATE(2), HA_RATE(2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1560, IUER, 'VEX_TO_SIMUL', 'Error in '// &
     &            'computing azimuth and elevation for staion '// &
     &             SIMUL%STA_NAM(SIMUL%STA_IND(2,J1)) )
              RETURN 
         END IF
 410  CONTINUE 
! 
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_BAND', 1, 1, 4, DIMS(1), DIMS(2), &
     &                  SIMUL%N_BNDS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1561, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode NUM_BAND for the second station' )
           RETURN
      END IF
! 
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_CHBN', 1, 1, SIMUL%N_BNDS*4, DIMS(1), DIMS(2), &
     &                  SIMUL%NUM_IFS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1562, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode NUM_CHBN for the second station' )
           RETURN
      END IF
! 
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'RFREQ1  ', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1563, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'inquiring lcode RFREQ1' )
           RETURN
      END IF
!
      IF ( CLASS .EQ. 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SKYFRQCH', 1, 1, SIM__MIFS*8, DIMS(1), DIMS(2), &
     &                       SIMUL%FRQ_IF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1564, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode SKYFRQCH' )
                RETURN
           END IF
         ELSE
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'RFREQ1  ', 1, 1, SIMUL%NUM_IFS(1)*8, DIMS(1), DIMS(2), &
     &                       SIMUL%FRQ_IF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1565, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode RFREQ1 for the second station' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'IND_CHN1', 1, 1, SIMUL%NUM_IFS(1)*4, DIMS(1), DIMS(2), &
     &                  SIMUL%IND_FRQ_STA(1,1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1566, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &         'getting lcode IND_CHN1 for the second station' )
           RETURN
      END IF
!
      IF ( SIMUL%N_BNDS > 1 ) THEN
           IF ( CLASS .NE. 0 ) THEN
                CALL ERR_PASS   ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'RFREQ2  ', 1, 1, SIMUL%NUM_IFS(2)*8, DIMS(1), DIMS(2), &
     &                            SIMUL%FRQ_IF(SIMUL%NUM_IFS(1)+1), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1567, IUER, 'GVF_TO_SIMUL', 'Error in '// &
          &              'getting lcode RFREQ2 for the second station' )
                     RETURN
                END IF
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'IND_CHN2', 1, 1, SIMUL%NUM_IFS(2)*4, DIMS(1), DIMS(2), &
     &                       SIMUL%IND_FRQ_STA(1,2,1), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1568, IUER, 'GVF_TO_SIMUL', 'Error in '// &
     &              'getting lcode IND_CHN2 for the second station' )
                RETURN
           END IF
      END IF
!
      SIMUL%MJD_BEG = SIMUL%MJD_OBS(1)
      SIMUL%UTC_BEG = SIMUL%UTC_OBS(1)
      SIMUL%MJD_END = SIMUL%MJD_OBS(SIMUL%NOBS)
      SIMUL%UTC_END = SIMUL%UTC_OBS(SIMUL%NOBS)
!
! --- Load stations and soures in the VTD data structure
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_LOAD ( VTD, SIMUL%NSTA, SIMUL%STA_NAM, SIMUL%NSOU, SIMUL%SOU_NAM, &
     &                SIMUL%MJD_BEG, SIMUL%UTC_BEG - SIMUL%UTC_MTAI, &
     &                SIMUL%MJD_END, SIMUL%UTC_END - SIMUL%UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1569, IUER, 'VEX_TO_SIMUL', 'Error in an attempt '// &
     &         'to load the data into VTD data structure' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVH_TO_SIMUL  !#!#
