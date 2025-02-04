      SUBROUTINE GETDB_FILL_OBORG ( GVH, L_CAL, CAL, FL_COMP_THEO, &
     &                              FL_SOU_USE_DB_IGNORE, &
     &                              N_MET, IND_MET, ATM_PRES, AIR_TEMP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GETDB_FILL_OBORG
! *                                                                      *
! *   GET_GLCODE takes 7 mks.                                            *
! *   FL_ALL_READ = .FALSE. readuces read time by 20%.                   *
! *                                                                      *
! * ### 21-NOV-2005 GETDB_FILL_OBORG v2.11 (c) L. Petrov 03-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'gvh.i'
      INCLUDE   'oborg.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc4.i'
      TYPE     ( GVH__STRU ) :: GVH
      TYPE     ( CAL__TYPE ) :: CAL(M__CAL)
      INTEGER*4  L_CAL, IUER
      LOGICAL*4  FL_COMP_THEO, FL_SOU_USE_DB_IGNORE
      INTEGER*4  N_MET(MAX_STA), IND_MET(2,MAX_OBS)
      REAL*8     ATM_PRES(MAX_SCA,MAX_STA), AIR_TEMP(MAX_SCA,MAX_STA)
      REAL*8     UTC_OBS, TDB, ARR_R8(1024), FRQ_SKY(1024), FRQ_BAND(1024), &
     &           SAMPLE_RATE(4), AMP_PHS(2,1024), NOSAMPLE_R8(2,1024), &
     &           REFFR, REFFR_S, TAU_GR(2), FREQ_EFF(2), FRQ_BAND_MIN, &
                 FRQ_BAND_MAX, FRQ_SKY_1(1024), FRQ_SKY_2(1024)
      INTEGER*4, ALLOCATABLE :: OBS_TAB(:,:)
      CHARACTER  STR*80, DESCR*256, STR1_ARR(32)*1
      INTEGER*4  M__ARR
      PARAMETER  ( M__ARR = 1024 )
      INTEGER*2  ARR_I2(M__ARR), IND_CHN_I2(M__ARR), NOAP(2,M__ARR)
      REAL*4     ARR_R4(M__ARR)
      REAL*8     FRQ_DIF_LIM, SNR_MIN_VAL
      REAL*4       AMP_MAX
      PARAMETER  ( FRQ_DIF_LIM = 0.1D0 ) ! MHz
      PARAMETER  ( SNR_MIN_VAL = 0.1D0 )
      PARAMETER  ( AMP_MAX     = 1.0E0 )
      INTEGER*4  CLASS, TYP, DIMS(2), NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &           MJD_UTC_OBS, ARR_I4(M__ARR), N_FRQ(2), N_AVBAND, &
     &           NBIT_SAMPLE, NUM_CHAN, L_FRQ(M__ARR), J1, J2, J3, J4, J5, &
     &           N_EFF, IND_QUALCODE, IER
      INTEGER*4  CLASS_NGRAMB, CLASS_NPHAMB, CLASS_GDAMBSP, CLASS_IONGDEL, CLASS_EFFFRQ
      ADDRESS__TYPE :: ADR_DATA 
      LOGICAL*2  FL_11, FL_SLV, FL_IONO, FL_DTEC, FL_IONO_FREQ, FL_NUSOLVE, FL_ALL_READ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: DATYP_INQ, IS_R8_NAN, IS_R4_NAN
      REAL*8,    EXTERNAL :: ATAN_CS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32
!
      NUMDB = 1
      FRQ_BAND_MIN  = 0.0D0
      FRQ_BAND_MAX  = 0.0D0
      FL_ALL_READ   = .TRUE.
      CLASS_NGRAMB  = 0
      CLASS_NPHAMB  = 0
      CLASS_GDAMBSP = 0
      CLASS_IONGDEL = 0
      CLASS_EFFFRQ  = 0
!
      ALLOCATE ( OBS_TAB(3,NUMOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*NUMOBS, STR )
           CALL ERR_LOG ( 8811, IUER, 'GETDB_FILL_OBORG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for observation table' )
           RETURN
      END IF
      IF ( INDEX ( GVH%GENERATOR, 'nuSolve' ) > 0 ) THEN
           FL_NUSOLVE = .TRUE.
         ELSE
           FL_NUSOLVE = .FALSE.
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 1, 0, 4*3*NUMOBS, DIMS(1), DIMS(2), &
     &                  OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8812, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &         'getting lcode OBS_TAB' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SAMPLRAT', 1, 0, 4*8, DIMS(1), DIMS(2), &
     &                  SAMPLE_RATE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8813, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &         'getting lcode SAMPLRAT' )
           RETURN
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'BITSAMPL', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8814, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &         'inquiring lcode BITSAMPL' )
           RETURN
      END IF
!
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'BITSAMPL', 1, 0, 4*2, DIMS(1), DIMS(2), &
     &                       ARR_I2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8815, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &              'getting lcode BITSAMPL' )
                RETURN
           END IF
         ELSE
!
! -------- A reasonable default in a case of missing LCODE
!
           ARR_I2(1) = 2
      END IF
      NBIT_SAMPLE = ARR_I2(1)
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'N_AVBAND', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  N_AVBAND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8816, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &         'getting lcode N_AVBAND' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'QUALCODE', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IND_QUALCODE = 1
      IF ( DIMS(1) == 1  .AND. DIMS(2) == 2 .AND. N_AVBAND == 1 ) THEN
!
! -------- A fix of a QUALCODE bug
!
           CALL ERR_PASS ( IUER, IER )
           CALL GETDB_QUALCODE_FIX ( GVH, NUMOBS, IND_QUALCODE, IER  )
      END IF
!
      IF ( .NOT. SIMULATION_TEST .AND. FL_ALL_READ ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'NUM_CHAN', 0, 0, 4, DIMS(1), DIMS(2), &
     &                       NUM_CHAN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8817, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &              'getting lcode NUM_CHAN' )
                RETURN
           END IF
!
           L_FRQ = 0
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'NUM_CHBN', 0, 0, 2*4, DIMS(1), DIMS(2), &
     &                       L_FRQ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8818, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &              'getting lcode NUM_CHBN' )
                RETURN
           END IF
!
           IF ( .NOT. FL_NUSOLVE ) THEN
                CALL ERR_PASS   ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'SKYFRQCH', 0, 0, 8*NUM_CHAN, DIMS(1), DIMS(2), &
     &                            FRQ_SKY, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8819, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                   'getting lcode SKYFRQCH' )
                     RETURN
                END IF
           END IF
         ELSE
!
! -------- Simulation case
!
           NUM_CHAN = 1
           L_FRQ    = 1
           FRQ_SKY  = 0.0D0
      END IF
!
! --- Open OBSFIL file
!
      FL_11 = KBIT( PRE_IBATCH, INT2(11) )
      CALL SBIT ( PRE_IBATCH, INT2(11), 0 )
      CALL ACS_OBSFIL ( 'O' )
      IF ( FL_11 ) CALL SBIT ( PRE_IBATCH, INT2(11), 1 )
      META_SUP = .FALSE.
      N_EFF = 0
      MEAN_EFF_FREQ = 0.0D0
!
      DO 410 J1=1,NUMOBS
         CALL NOUT ( JOBSREC_BYTES, IFIRST_OBORG_I2 )
         IF ( .NOT. SIMULATION_TEST ) THEN
              IF ( FL_NUSOLVE ) THEN
                   CALL ERR_PASS   ( IUER, IER )
                   CALL GVH_GLCODE ( GVH, 'RFREQ1  ', J1, 1, 8*L_FRQ(1), &
     &                               DIMS(1), DIMS(2), FRQ_SKY_1, IER )
                   IF ( IER .NE. 0  ) THEN
                        CALL ERR_LOG ( 8820, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                      'getting lcode RFREQ1' )
                        RETURN
                   END IF
              END IF
         END IF
!
         NSCA = OBS_TAB(1,J1)
         ISITE(1) = OBS_TAB(2,J1)
         ISITE(2) = OBS_TAB(3,J1)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SCANNAME', J1, 0, 16, DIMS(1), DIMS(2), &
     &                     %REF(STR), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8821, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode SCANNAME' )
              RETURN
         END IF
         SCAN_NAME = STR
!
         CALL CLRCH ( FRINGE_ROOT_FINAM )
         PIND_OBS = 0
         UV_STA_ORDER = 0
         APR_GR_DEL   = 0.0D0
         APR_PHS_RAT  = 0.0D0
         RES_GR_DEL   = 0.0D0
         RES_PHS_RAT  = 0.0D0
         RES_PHS_GC    = 0.0D0
         IF ( ILEN(PIMA_CNT) > 0 ) THEN
!
! ----------- Get some PIMA specific parameters
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'SCANPIMA', J1, 0, 10, DIMS(1), DIMS(2), &
     &                          %REF(FRINGE_ROOT_FINAM), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8822, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode SCANPIMA' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'PIND_OBS', J1, 0, 4, DIMS(1), DIMS(2), &
     &                          PIND_OBS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8823, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode PIND_OBS' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'UVSTAORD', J1, 0, 2, DIMS(1), DIMS(2), &
     &                          UV_STA_ORDER, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8824, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode UV_STA_ORDER' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'APR_DEL ', J1, 1, N_AVBAND*8, DIMS(1), &
     &                          DIMS(2), APR_GR_DEL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8825, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode APR_DEL  for the second station' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'APR_RATE', J1, 1, N_AVBAND*8, DIMS(1), &
     &                          DIMS(2), APR_PHS_RAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8826, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode APR_RATE  for the second station' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'RES_GRDL', J1, 1, N_AVBAND*8, DIMS(1), &
     &                          DIMS(2), RES_GR_DEL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8827, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode RES_GRDL  for the second station' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'RES_RATE', J1, 1, N_AVBAND*8, DIMS(1), &
     &                          DIMS(2), RES_PHS_RAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8828, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode RES_RATE  for the second station' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'RES_PHGC', J1, 1, N_AVBAND*8, DIMS(1), &
     &                          DIMS(2), RES_PHS_GC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8829, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode RES_PHGC for the second station' )
                   RETURN
              END IF
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SOU_IND ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     ARR_I4 , IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8830, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode SOU_IND' )
              RETURN
         END IF
         ISTAR = ARR_I4(1)
!
         STR1_ARR(1) = '?'
         STR1_ARR(2) = '?'
         STR1_ARR(3) = '?'
         STR1_ARR(4) = '?'
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'QUALCODE', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     STR1_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8831, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode QUALCODE' )
              RETURN
         END IF
         LQUAL_S_CHR = ' 0'
         IF ( DIMS(1) == 1 ) THEN
!
! ----------- QUALCODE takes one character
!
              LQUAL_CHR   = ' '//STR1_ARR(1)
              IF ( DIMS(2) > 1 ) THEN
                   IF ( N_AVBAND == 1 ) THEN
!
! --------------------- This is a pathological case
!
                        LQUAL_CHR   = ' '//STR1_ARR(IND_QUALCODE)
                      ELSE
                        LQUAL_S_CHR = ' '//STR1_ARR(2)
                   END IF
              END IF
            ELSE
              IF ( STR1_ARR(1) == ' ' .OR. STR1_ARR(1) == '_' ) THEN
                   LQUAL_CHR   = ' '//STR1_ARR(2)
                 ELSE
                   LQUAL_CHR   = ' '//STR1_ARR(1)
              END IF
              IF ( DIMS(2) > 1 ) THEN
                   IF ( STR1_ARR(3) == ' ' .OR. STR1_ARR(3) == '_' ) THEN
                        LQUAL_S_CHR = ' '//STR1_ARR(4)
                      ELSE
                        LQUAL_S_CHR = ' '//STR1_ARR(3)
                   END IF
              END IF
         END IF
         IF ( LQUAL_CHR(2:2)   == ' ' ) LQUAL_CHR(2:2)   = '0'
         IF ( LQUAL_S_CHR(2:2) == ' ' ) LQUAL_S_CHR(2:2) = '0'
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'MJD_OBS ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     MJD_UTC_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8832, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode MJD_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'UTC_OBS ', J1, 0, 8, DIMS(1), DIMS(2), &
     &                     UTC_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8833, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode UTC_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         FJD = (J2000__JD + (MJD_UTC_OBS - J2000__MJD -0.5D0))
         FRACT = UTC_OBS/86400.0D0
         CALL TAI_TO_TDB ( MJD_UTC_OBS, UTC_OBS-UTC_M_TAI, TDB )
         FRACTC = TDB/86400.0D0
!
         IF ( CLASS_NGRAMB == 0 ) THEN
              CALL ERR_PASS      ( IUER, IER )
              CALL GVH_INQ_LCODE ( GVH, 'N_GRAMB ', DESCR, CLASS_NGRAMB, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8834, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'inquiring lcode N_GRAMB' )
                   RETURN
              END IF
         END IF
!
         IF ( CLASS_NGRAMB .NE. 0 ) THEN
              FL_SLV = .TRUE.
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'N_GRAMB ', J1, 0, 2*4, DIMS(1), DIMS(2), &
     &                          ARR_I4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8835, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode N_GRAMB' )
                   RETURN
              END IF
              NUMAMB   = ARR_I4(1)
              NUMAMB_S = ARR_I4(2)
            ELSE
              FL_SLV = .FALSE.
              NUMAMB   = 0
              NUMAMB_S = 0
         END IF
!
         IF ( .NOT. FL_NUSOLVE ) THEN
              IF ( CLASS_NPHAMB == 0 ) THEN
                   CALL ERR_PASS      ( IUER, IER )
                   CALL GVH_INQ_LCODE ( GVH, 'N_PHAMB ', DESCR, CLASS_NPHAMB, TYP, DIMS, &
     &                                  NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                                  IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8836, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                      'inquiring lcode N_PHAMB ' )
                        RETURN
                   END IF
              END IF
!
              IF ( CLASS_NPHAMB .NE. 0 ) THEN
                   CALL ERR_PASS   ( IUER, IER )
                   CALL GVH_GLCODE ( GVH, 'N_PHAMB ', J1, 0, 2*4, DIMS(1), DIMS(2), &
     &                               ARR_I4, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8837, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                      'getting lcode N_PHAMB' )
                        RETURN
                   END IF
                 ELSE
                   ARR_I4 = 0
              END IF
            ELSE
              ARR_I4 = 0
         END IF
!
         NPHAM4   = ARR_I4(1)
         NPHAM4_S = ARR_I4(2)
!
         IF ( CLASS_GDAMBSP == 0 ) THEN
              CALL ERR_PASS      ( IUER, IER )
              CALL GVH_INQ_LCODE ( GVH, 'GDAMBSP ', DESCR, CLASS_GDAMBSP, TYP, DIMS, &
     &                             NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                             IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8838, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'inquiring lcode GDAMBSP ' )
                   RETURN
              END IF
         END IF
         IF ( CLASS_GDAMBSP .NE. 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'GDAMBSP ', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8839, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode GDAMBSP' )
                   RETURN
              END IF
            ELSE
              NPHAM4   = 0
              NPHAM4_S = 0
              ARR_R8   = 1.D-7 ! Some reasonable default
         END IF
!
         FAMB   = ARR_R8(1)
         FAMB_S = ARR_R8(2)
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'REF_FREQ', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8840, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode REF_FREQ' )
              RETURN
         END IF
!
         IF ( IS_R8_NAN ( ARR_R8(1) ) ) ARR_R8(1) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(2) ) ) ARR_R8(2) = 0.0D0
         REFFR   = ARR_R8(1)*1.D-6
         REFFR_S = ARR_R8(2)*1.D-6
         IF ( REFFR   < FRQ_DIF_LIM ) REFFR   = FRQ_DIF_LIM
         IF ( REFFR_S < FRQ_DIF_LIM ) REFFR_S = FRQ_DIF_LIM
         FREQ_SKY = REFFR
         IF ( LQUAL_CHR .EQ. ' 0' ) THEN
              PHAMI8    = 0.0D0
            ELSE
              PHAMI8    = 1.D0/REFFR
         END IF
         IF ( LQUAL_S_CHR .EQ. ' 0' ) THEN
              PHAMI8_S  = 0.D0
            ELSE
              PHAMI8_S  = 1.D0/REFFR_S
         END IF
!
! ------ Reading group delays
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GR_DELAY', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8841, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode GR_DELAY' )
              RETURN
         END IF
         IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
         DOBS_ORIG   = ARR_R8(1)*1.D6
         DOBS_ORIG_S = ARR_R8(2)*1.D6
!
! ------ Apply group delay ambiguities
!
         DOBS   = ( DOBS_ORIG*1.D-6 + NUMAMB*FAMB )*1.D6
         DOBS_S = ( DOBS_ORIG_S*1.D-6 + NUMAMB_S*FAMB_S )*1.D6
         TAU_GR(1) = DOBS*1.D-6
         TAU_GR(2) = DOBS_S*1.D-6
!
! ------ Reading phase delay rate
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'DEL_RATE', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8842, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode DEL_RATE' )
              RETURN
         END IF
         IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
         ROBS   = ARR_R8(1)
         ROBS_S = ARR_R8(2)
!
         IF ( .NOT. SIMULATION_TEST ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'TOTPHASE', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8843, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode DEL_RATE' )
                   RETURN
              END IF
              IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
!
! ----------- Solve keeps the total phase in degrees
!
              TOTPH   = ARR_R8(1)/DEG__TO__RAD
              TOTPH_S = ARR_R8(2)/DEG__TO__RAD
!
              IF ( REFFR   < FRQ_DIF_LIM ) REFFR = FRQ_DIF_LIM
              IF ( REFFR_S < FRQ_DIF_LIM ) REFFR_S = FRQ_DIF_LIM
              IF ( LQUAL_CHR .EQ. ' 0' ) THEN
                   DPH_ORIG = 0.0D0
                   DPH      = 0.0D0
                 ELSE
                   DPH_ORIG = ARR_R8(1)/(PI2*REFFR)
                   DPH      = ( ARR_R8(1) + PI2*NPHAM4   )/(PI2*REFFR)
              END IF
              IF ( LQUAL_S_CHR .EQ. ' 0' ) THEN
                   DPH_ORIG_S = 0.0D0
                   DPH_S      = 0.0D0
                 ELSE
                   DPH_ORIG_S = ARR_R8(2)/(PI2*REFFR_S)
                   DPH_S = ( ARR_R8(2) + PI2*NPHAM4_S )/(PI2*REFFR_S)
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'SB_DELAY', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8844, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode SB_DELAY' )
                   RETURN
              END IF
              DNB   = ARR_R8(1)*1.D6
              DNB_S = ARR_R8(2)*1.D6
            ELSE 
              DNB        = 0.0D0
              DNB_S      = 0.0D0
              DPH        = 0.0D0
              DPH_S      = 0.0D0
              DPH_ORIG   = 0.0D0
              DPH_ORIG_S = 0.0D0
              TOTPH      = 0.0D0
              TOTPH_S    = 0.0D0
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GRDELERR', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8845, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode GRDELERR' )
              RETURN
         END IF
         IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
         DERR   = ARR_R8(1)
         DERR_S = ARR_R8(2)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'PHRATERR', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8846, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode PHRATERR' )
              RETURN
         END IF
         IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
         RERR   = ARR_R8(1)
         RERR_S = ARR_R8(2)
!
         IF ( .NOT. SIMULATION_TEST ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'SBDELERR', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8847, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode SBDELERR' )
                   RETURN
              END IF
              IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
              DNBER   = ARR_R8(1)
              DNBER_S = ARR_R8(2)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'FRN_AMPL', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8848, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode FRN_AMPL' )
                  RETURN
              END IF
              IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
              AMPL   = ARR_R8(1)
              AMPL_S = ARR_R8(2)
            ELSE 
              DNBER   = 0.0D0
              DNBER_S = 0.0D0
              AMPL    = 0.0D0
              AMPL_S  = 0.0D0
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SNRATIO ', J1, 0, 2*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8849, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &            'getting lcode SNRATIO' )
              RETURN
         END IF
         IF ( N_AVBAND == 1 ) ARR_R8(2) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(1) ) ) ARR_R8(1) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(2) ) ) ARR_R8(2) = 0.0D0
         SNR   = ARR_R8(1)
         SNR_S = ARR_R8(2)
         IF ( SNR   < SNR_MIN_VAL ) SNR   = 0.0
         IF ( SNR_S < SNR_MIN_VAL ) SNR_S = 0.0
!
         IF ( LQUAL_CHR .EQ. ' 0' ) THEN
              DPHER   = 0.D0
            ELSE
              IF ( REFFR*SNR > 0 ) THEN
                   DPHER   = 1.D0/(PI2*REFFR*1.D6*SNR)
                 ELSE
                   DPHER   = 0.0D0
              END IF
         END IF
         IF ( LQUAL_S_CHR .EQ. ' 0' ) THEN
              DPHER_S = 0.D0
            ELSE
              IF ( REFFR_S*SNR_S > 0 ) THEN
                   DPHER_S = 1.D0/(PI2*REFFR_S*1.D6*SNR_S)
                 ELSE 
                   DPHER_S = 0.0D0
              END IF
         END IF
!
         IF ( IND_MET(1,J1) .NE. 0 .AND. IND_MET(2,J1) .NE. 0 ) THEN
              ATMPR(1) = ATM_PRES(IND_MET(1,J1),ISITE(1))*1.D-2
              ATMPR(2) = ATM_PRES(IND_MET(2,J1),ISITE(2))*1.D-2
              TEMPC(1) = AIR_TEMP(IND_MET(1,J1),ISITE(1)) - 273.16D0
              TEMPC(2) = AIR_TEMP(IND_MET(2,J1),ISITE(2)) - 273.16D0
!
! ----------- Get relative humidity
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'REL_HUMD', J1, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8850, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                  'getting lcode REL_HIMI for the first station' )
                   RETURN
              END IF
              RELHU(1) = ARR_R8(1) ! *1.D-2
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'REL_HUMD', J1, 2, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8851, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode REL_HUMI for the second station' )
                   RETURN
              END IF
              RELHU(2) = ARR_R8(1) ! *1.D-2
            ELSE
              ATMPR(1) = -999.0
              ATMPR(2) = -999.0
              TEMPC(1) = -999.0
              TEMPC(2) = -999.0
              RELHU(1) = -999.0
              RELHU(2) = -999.0
         END IF
!
         IER = 0
         CALL GVH_GLCODE ( GVH, 'SCAN_DUR', J1, 1, 2*8, DIMS(1), &
     &                     DIMS(2), EFF_DUR, IER )
!         IF ( IER .NE. 0 ) THEN
!              CALL ERR_LOG ( 8852, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
!     &            'getting lcode SCAN_DUR' )
!              RETURN
!         END IF
         IF ( IER .NE. 0 ) EFF_DUR(1) = 0.0D0
         IF ( N_AVBAND == 1 ) THEN
              EFF_DUR(2) = 0.0D0
         END IF
!
         IF ( FL_SLV ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'AUTO_SUP', J1, 1, 4, DIMS(1), DIMS(2), &
     &                          AUTO_SUP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8853, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode AUTO_SUP for the second station' )
                   RETURN
              END IF
!
              IF ( BTEST(AUTO_SUP, INT4(INIT__SPS) ) ) META_SUP = .TRUE.
              IF ( FL_SOU_USE_DB_IGNORE ) THEN
                   AUTO_SUP = IBCLR ( AUTO_SUP, INT4(DSSO__SPS) )
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'USER_SUP', J1, 1, 4, DIMS(1), DIMS(2), &
     &                          USER_SUP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8854, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode USER_SUP for the second station' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'USER_REC', J1, 1, 4, DIMS(1), DIMS(2), &
     &                          USER_REC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8855, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode USER_REC for the second station' )
                   RETURN
              END IF
            ELSE
              USER_SUP = 0
              USER_REC = 0
              IF ( LQUAL_CHR == ' 0' .OR. &
     &             LQUAL_CHR == ' A' .OR. &
     &             LQUAL_CHR == ' B' .OR. &
     &             LQUAL_CHR == ' C' .OR. &
     &             LQUAL_CHR == ' D' .OR. &
     &             LQUAL_CHR == ' E' .OR. &
     &             LQUAL_CHR == ' F'      ) THEN
                   USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
                   USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
              END IF
!
              IF ( LQUAL_S_CHR == ' 0' .OR. &
     &             LQUAL_S_CHR == ' A' .OR. &
     &             LQUAL_S_CHR == ' B' .OR. &
     &             LQUAL_S_CHR == ' C' .OR. &
     &             LQUAL_S_CHR == ' D' .OR. &
     &             LQUAL_S_CHR == ' E' .OR. &
     &             LQUAL_S_CHR == ' F'      ) THEN
                   USER_SUP = IBSET ( USER_SUP, NOFS__SPS )
                   USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
              END IF
              AUTO_SUP = USER_SUP
         END IF
!
         IF ( SUPMET == SUPMET__META ) THEN
              AUTO_SUP = IBSET ( AUTO_SUP, INT4(INIT__SPS) )
              USER_SUP = IBSET ( USER_SUP, INT4(INIT__SPS) )
              USER_REC = IBSET ( USER_REC, INT4(INIT__SPS) )
         END IF
!
         IF ( .NOT. FL_COMP_THEO ) THEN
!
! ----------- Set default values. We set elevation 60 deg in order to prevent
! ----------- deselecting "low wlevation obserations". Since the real
! ----------- elevation is not known at this point, let us consider that
! ----------- the source is high enough
!
              AZ(1)      = 0.0D0
              AZ(2)      = 0.0D0
              ELEV(1)    = PI__NUM/3.0D0
              ELEV(2)    = PI__NUM/3.0D0
              UV_COOR(1) = 0.0D0
              UV_COOR(2) = 0.0D0
         END IF
!
         SUPSTAT(1) = 0
         SUPSTAT(2) = 0
         ICORR = 0
         IF ( FL_SOU_USE_DB_IGNORE ) THEN
              IF ( IUNW  == 17 ) IUNW  = 1
              IF ( IUNWP == 17 ) IUNWP = 1
              CALL SBIT ( SUPSTAT, DSSO__SPS, INT2(0) )
         END IF
         CALL SBIT ( ICORR,   INT2(4),   INT2(1) )
         CALL SBIT ( ICORR,   INT2(10),  INT2(1) )
         CALL SBIT ( SUPSTAT, SET1__SPS, INT2(1) )
         CALL SBIT ( SUPSTAT, SET2__SPS, INT2(1) )
         CALL SBIT ( UACSUP,  INIT__UAS, INT2(1) )
         IF ( BTEST ( USER_SUP, INT4(IDATYP) ) ) THEN
              IUNW = 1
              CALL SBIT ( SUPSTAT, IUNW__SPS, INT2(1) )
              CALL SBIT ( UACSUP,  GSUP__UAS, INT2(1) )
            ELSE
              IUNW = 0
         END IF
         IF ( BTEST ( USER_SUP, INT4(PHSRAT__DTP) ) .OR. &
     &        BTEST ( USER_SUP, INT4(PHSONL__DTP) ) .OR. &
     &        BTEST ( USER_SUP, INT4(PX_GXS__DTP) ) .OR. &
     &        BTEST ( USER_SUP, INT4(PS_GXS__DTP) ) .OR. &
     &        BTEST ( USER_SUP, INT4(PX_GX__DTP)  ) .OR. &
     &        BTEST ( USER_SUP, INT4(PX_GS__DTP)  ) .OR. &
     &        BTEST ( USER_SUP, INT4(PS_GX__DTP)  ) .OR. &
     &        BTEST ( USER_SUP, INT4(PS_GS__DTP)  ) .OR. &
     &        BTEST ( USER_SUP, INT4(P_PXS__DTP)  ) .OR. &
     &        BTEST ( USER_SUP, INT4(PX__DTP)     ) .OR. &
     &        BTEST ( USER_SUP, INT4(PS__DTP)     )      ) THEN
!
              IUNWP = 1
              CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
            ELSE
              IUNWP = 0
         END IF
!
         IF ( N_AVBAND > 1 ) THEN
              IF ( LQUAL_S_CHR == ' 0' .OR. &
     &             LQUAL_S_CHR == ' A' .OR. &
     &             LQUAL_S_CHR == ' B' .OR. &
     &             LQUAL_S_CHR == ' C' .OR. &
     &             LQUAL_S_CHR == ' D' .OR. &
     &             LQUAL_S_CHR == ' E' .OR. &
     &             LQUAL_S_CHR == ' F' .OR. &
     &             LQUAL_S_CHR == ' G'      ) THEN
!
                   IF ( ISITN_CHR(ISITE(1)) == 'ONSALA60'  .OR. &
     &                  ISITN_CHR(ISITE(2)) == 'ONSALA60'       ) THEN
                        CONTINUE
                      ELSE
                        CALL SBIT ( SUPSTAT, NOFS__SPS, INT2(1) )
                        CALL SBIT ( UACSUP,  GSUP__UAS, INT2(1) )
                        CALL SBIT ( ICORR, INT2(4),  INT2(0) )
                        CALL SBIT ( ICORR, INT2(10), INT2(0) )
                        CALL SBIT ( ICORR, INT2(6),  INT2(1) )
                        CALL SBIT ( ICORR, INT2(12), INT2(1) )
                        IUNW = 8
                   END IF
              END IF
         END IF
!
         IF ( CLASS_IONGDEL == 0 ) THEN
              CALL ERR_PASS      ( IUER, IER )
              CALL GVH_INQ_LCODE ( GVH, 'ION_GDEL', DESCR, CLASS_IONGDEL, TYP, DIMS, &
     &                             NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                             IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8856, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'inquiring lcode ION_GDEL' )
                   RETURN
              END IF
         END IF
!
         IF ( CLASS_IONGDEL .NE. 0 ) THEN
              FL_IONO = .TRUE.
            ELSE
              FL_IONO = .FALSE.
         END IF
!
         IF ( FL_IONO ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ION_GDEL', J1, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8857, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode ION_GDEL for the second station' )
                   RETURN
              END IF
              GION(1) = ARR_R8(1)*1.D6
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ION_PRAT', J1, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8858, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode ION_PRAT for the second station' )
                   RETURN
              END IF
              GION(2) = ARR_R8(1)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ION_GERR', J1, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8859, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode ION_GERR for the second station' )
                   RETURN
              END IF
              GIONSG(1) = ARR_R8(1)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ION_RERR', J1, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8860, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode ION_RERR for the second station' )
                   RETURN
              END IF
              GIONSG(2) = ARR_R8(1)
            ELSE
              GION = 0.0D0
         END IF
!
! ------ NB: EFF_FREQ presents and has 6 components (3 for each band) 
! ------ if the database was generated by PIMA.
! 
! ------ It is not present, or has only three components for two bands 
! ------ if the database ws generated by MARK3_TO_GVF
!
         CALL NOUT_R8 ( 6, ARR_R8 )
!
         IF ( CLASS_EFFFRQ == 0 ) THEN
              CALL ERR_PASS      ( IUER, IER )
              CALL GVH_INQ_LCODE ( GVH, 'EFF_FREQ', DESCR, CLASS_EFFFRQ, TYP, DIMS, &
     &                             NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                             IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8861, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'inquiring lcode ION_GDEL' )
                   RETURN
              END IF
         END IF
         IF ( N_AVBAND == 1 ) ARR_R8(4:6) = 0.0D0
         IF ( CLASS_EFFFRQ .NE. 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'EFF_FREQ', J1, 1, 6*8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8862, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode EFF_FREQ for the second station' )
                   RETURN
              END IF
         END IF
         IF ( IS_R8_NAN ( ARR_R8(1) ) ) ARR_R8(1) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(2) ) ) ARR_R8(2) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(3) ) ) ARR_R8(3) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(4) ) ) ARR_R8(4) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(5) ) ) ARR_R8(5) = 0.0D0
         IF ( IS_R8_NAN ( ARR_R8(6) ) ) ARR_R8(6) = 0.0D0
!
         IF ( IER .EQ. 0 .AND. &
     &        ARR_R8(1) > 2.0*FRQ_DIF_LIM .AND. &
     &        ARR_R8(4) > 2.0*FRQ_DIF_LIM       ) THEN
              EFFREQ       = ARR_R8(1)*1.0D-6
              PHEFFREQ     = ARR_R8(2)*1.0D-6
              REFFREQ      = ARR_R8(3)*1.0D-6
              EFFREQ_XS    = ARR_R8(4)*1.0D-6
              PHEFFREQ_XS  = ARR_R8(5)*1.0D-6
              REFFREQ_XS   = ARR_R8(6)*1.0D-6
!
              N_EFF = N_EFF + 1
              MEAN_EFF_FREQ(1) = MEAN_EFF_FREQ(1) + EFFREQ       
              MEAN_EFF_FREQ(2) = MEAN_EFF_FREQ(2) + EFFREQ_XS
              MEAN_EFF_FREQ(3) = MEAN_EFF_FREQ(3) + PHEFFREQ 
              MEAN_EFF_FREQ(4) = MEAN_EFF_FREQ(4) + PHEFFREQ_XS 
              MEAN_EFF_FREQ(5) = MEAN_EFF_FREQ(5) + REFFREQ
              MEAN_EFF_FREQ(6) = MEAN_EFF_FREQ(6) + REFFREQ_XS
              FL_IONO_FREQ = .TRUE.
            ELSE
              EFFREQ       = REFFR
              REFFREQ      = REFFR
              PHEFFREQ     = REFFR
              EFFREQ_XS    = REFFR_S
              REFFREQ_XS   = REFFR_S
              PHEFFREQ_XS  = REFFR_S
              FL_IONO_FREQ = .FALSE.
         END IF
!
         IF ( .NOT. SIMULATION_TEST .AND. FL_ALL_READ ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'NUSEDCHN', J1, 1, 4, DIMS(1), DIMS(2), &
     &                          ARR_I2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8863, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode NUSEDCHN for the second station' )
                   RETURN
              END IF
              IF ( N_AVBAND == 1 ) ARR_I2(2) = 0
              N_FRQ(1) = ARR_I2(1)
              N_FRQ(2) = ARR_I2(2)
              IF ( N_FRQ(1) > M__ARR .OR. N_FRQ(1) < 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( N_FRQ(1), STR )
                   CALL ERR_LOG ( 8867, IUER, 'GETDB_FILL_OBORG', 'Wrong value of '// &
     &                 'lcode NUSEDCHN(1): '//STR )
                   RETURN
              END IF
              IF ( N_FRQ(2) > M__ARR .OR. N_FRQ(2) < 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( N_FRQ(2), STR )
                   CALL ERR_LOG ( 8867, IUER, 'GETDB_FILL_OBORG', 'Wrong value of '// &
     &                 'lcode NUSEDCHN(2): '//STR )
                   RETURN
              END IF
!
              ARR_I2 = 0
              ARR_I4 = 0
              ARR_R8 = 0.0D0
!
! ----------- Get lcodes needed for computing effective ionospheric frequencies
! ----------- at the first band
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'NUM_AP1 ', J1, 1, 2*2*L_FRQ(1), &
     &                          DIMS(1), DIMS(2), ARR_I2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8864, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode NUM_AP1' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'UV_CHN1 ', J1, 1, 4*2*L_FRQ(1), &
     &                          DIMS(1), DIMS(2), ARR_R4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8865, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode UV_CHN1 ' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'NUM_SAM1', J1, 1, 8*2*L_FRQ(1), &
     &                          DIMS(1), DIMS(2), ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8866, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode NUM_SAM1' )
                   RETURN
              END IF
!
              IND_CHN_I2 = 0
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'IND_CHN1', J1, 1, 2*L_FRQ(1), &
     &                          DIMS(1), DIMS(2), IND_CHN_I2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8867, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode NUM_CHN1' )
                   RETURN
              END IF
!
              FRQ_BAND_MIN = 0.0D0
              FRQ_BAND_MAX = 0.0D0
              DO 420 J2=1,N_FRQ(1)
                 IF ( FL_NUSOLVE ) THEN
                      IND_CHN_I2(J2) = J2
                 END IF
                 NOAP(1,J2) = ARR_I2(J2)
                 NOAP(2,J2) = ARR_I2(L_FRQ(1)+J2)
                 IF ( IS_R4_NAN ( ARR_R4(1+(J2-1)*2) ) ) ARR_R4(1+(J2-1)*2) = 0.0
                 IF ( IS_R4_NAN ( ARR_R4(2+(J2-1)*2) ) ) ARR_R4(2+(J2-1)*2) = 0.0
                 IF ( ARR_R4(1+(J2-1)*2) > AMP_MAX     ) ARR_R4(1+(J2-1)*2) = 0.0
                 IF ( ARR_R4(2+(J2-1)*2) > AMP_MAX     ) ARR_R4(2+(J2-1)*2) = 0.0
                 AMP_PHS(1,J2) = SQRT ( ARR_R4(1+(J2-1)*2)**2 + ARR_R4(2+(J2-1)*2)**2 )
                 AMP_PHS(2,J2) = ATAN_CS ( 1.0D0*ARR_R4(1+(J2-1)*2), &
     &                                     1.0D0*ARR_R4(2+(J2-1)*2)  )
                 NOSAMPLE_R8(1,J2) = ARR_R8(J2)
                 NOSAMPLE_R8(2,J2) = ARR_R8(L_FRQ(1)+J2)
                 IF ( IS_R8_NAN ( NOSAMPLE_R8(1,J2) ) ) NOSAMPLE_R8(1,J2) = 0.0D0
                 IF ( IS_R8_NAN ( NOSAMPLE_R8(2,J2) ) ) NOSAMPLE_R8(2,J2) = 0.0D0
                 IF ( IND_CHN_I2(J2) > 0 ) THEN
                      IF ( .NOT. FL_NUSOLVE ) THEN
                           FRQ_BAND(J2) = FRQ_SKY(IND_CHN_I2(J2))*1.D-6
                         ELSE
                           FRQ_BAND(J2) = FRQ_SKY_1(J2)
                      END IF
                      IF ( J2 == 1 ) THEN
                           FRQ_BAND_MIN = FRQ_BAND(J2)
                           FRQ_BAND_MAX = FRQ_BAND(J2)
                        ELSE
                           IF ( FRQ_BAND(J2) < FRQ_BAND_MIN ) FRQ_BAND_MIN = FRQ_BAND(J2)
                           IF ( FRQ_BAND(J2) > FRQ_BAND_MAX ) FRQ_BAND_MAX = FRQ_BAND(J2)
                      END IF
                 END IF
 420          CONTINUE
!
! ----------- Compute ionospheric frequencies at the first band
!
              IF ( .NOT. FL_IONO_FREQ ) THEN
                   IF ( FRQ_BAND_MAX - FRQ_BAND_MIN > FRQ_DIF_LIM ) THEN
!
! --------------------- If the frequencies spread is greater than some limit (100 kHz ),
! --------------------- let us compute ionospheric effective frequencies
!
                        CALL IONFR ( J1, IONFR_MODE__USEAP, N_FRQ(1), NOAP, FRQ_BAND, &
     &                               AMP_PHS, NOSAMPLE_R8, SAMPLE_RATE(1), REFFR, &
     &                              .TRUE., .FALSE., EFFREQ, PHEFFREQ, REFFREQ )
                      ELSE
                        EFFREQ   = REFFR
                        PHEFFREQ = REFFR
                        REFFREQ  = REFFR
                   END IF
              END IF
            ELSE
!
! ----------- Simulation case
!
              N_FRQ(1) = 1
              N_FRQ(2) = 0
         END IF
!
         IF ( N_AVBAND > 1 .AND. LQUAL_S_CHR .NE. ' 0' .AND. FL_ALL_READ ) THEN
              ARR_I2 = 0
              ARR_I4 = 0
              ARR_R8 = 0.0D0
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'NUM_AP2 ', J1, 1, 2*2*L_FRQ(2), &
     &                          DIMS(1), DIMS(2), ARR_I2, IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) ' L_FRQ = ', L_FRQ(1:2)
                   CALL ERR_LOG ( 8868, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode NUM_AP2 LQUAL_S_CHR >>'//LQUAL_S_CHR//'<<' )
                   RETURN
              END IF
!
              IF ( .NOT. FL_NUSOLVE ) THEN
                   CALL ERR_PASS   ( IUER, IER )
                   CALL GVH_GLCODE ( GVH, 'UV_CHN2 ', J1, 1, 4*2*L_FRQ(2), &
     &                               DIMS(1), DIMS(2), ARR_R4, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8869, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                      'getting lcode UV_CHN2' )
                        RETURN
                   END IF
              END IF
!
              IF ( FL_NUSOLVE ) THEN
                   CALL ERR_PASS   ( IUER, IER )
                   CALL GVH_GLCODE ( GVH, 'RFREQ2  ', J1, 1, 8*L_FRQ(2), &
     &                               DIMS(1), DIMS(2), FRQ_SKY_2, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8870, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                      'getting lcode RFREQ2' )
                        RETURN
                   END IF
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'NUM_SAM2', J1, 1, 8*2*L_FRQ(2), &
     &                          DIMS(1), DIMS(2), ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8871, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode NUM_SAM2' )
                   RETURN
              END IF
!
              IND_CHN_I2 = 0
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'IND_CHN2', J1, 1, 2*L_FRQ(2), &
     &                          DIMS(1), DIMS(2), IND_CHN_I2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8872, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode IND_CHN2. Observation '//STR )
                   RETURN
              END IF
!
              FRQ_BAND_MIN = 0.0D0
              FRQ_BAND_MAX = 0.0D0
              DO 430 J3=1,N_FRQ(2)
                 NOAP(1,J3) = ARR_I2(J3)
                 NOAP(2,J3) = ARR_I2(L_FRQ(2)+J3)
                 IF ( IS_R4_NAN(ARR_R4(1+(J3-1)*2)) ) ARR_R4(1+(J3-1)*2) = 0.0
                 IF ( IS_R4_NAN(ARR_R4(2+(J3-1)*2)) ) ARR_R4(2+(J3-1)*2) = 0.0
                 AMP_PHS(1,J3) = SQRT ( ARR_R4(1+(J3-1)*2)**2 + ARR_R4(2+(J3-1)*2)**2 )
                 AMP_PHS(2,J3) = ATAN_CS ( 1.0D0*ARR_R4(1+(J3-1)*2), &
     &                                     1.0D0*ARR_R4(2+(J3-1)*2)  )
                 NOSAMPLE_R8(1,J3) = ARR_R8(J3)
                 NOSAMPLE_R8(2,J3) = ARR_R8(L_FRQ(2)+J3)
!
                 IF ( IND_CHN_I2(J3) > 0 ) THEN
                      IF ( .NOT. FL_NUSOLVE ) THEN
                           FRQ_BAND(J3) = FRQ_SKY(IND_CHN_I2(J3))*1.D-6
                         ELSE
                           FRQ_BAND(J3) = FRQ_SKY_2(J3)
                      END IF
!
                      IF ( J3 == 1 ) THEN
                           FRQ_BAND_MIN = FRQ_BAND(J3)
                           FRQ_BAND_MAX = FRQ_BAND(J3)
                         ELSE
                           IF ( FRQ_BAND(J3) < FRQ_BAND_MIN ) FRQ_BAND_MIN = FRQ_BAND(J3)
                           IF ( FRQ_BAND(J3) > FRQ_BAND_MAX ) FRQ_BAND_MAX = FRQ_BAND(J3)
                      END IF
                 END IF
 430          CONTINUE
         END IF
!
         IF ( L_CAL > 0 ) THEN
              DO 440 J4=1,L_CAL
                 IF ( CAL(J4)%INFO%MODE == CAL__DEL   .AND. &
     &                CAL(J4)%INFO%CLASS == GVH__STA  .AND. &
                      CAL(J4)%NAME .NE. 'user+cal'    .AND. &
                      CAL(J4)%NAME .NE. 'user cal'          ) THEN
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL GVH_GLCODE ( GVH, CAL(J4)%NAME, J1, 1, 8, &
     &                     DIMS(1), DIMS(2), ARR_R8(1), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8873, IUER, 'GETDB_FILL_OBORG', &
     &                         'Error in getting lcode '//CAL(J4)%NAME// &
     &                         ' for the first station'  )
                           RETURN
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL GVH_GLCODE ( GVH, CAL(J4)%NAME, J1, 2, 8, &
     &                     DIMS(1), DIMS(2), ARR_R8(2), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8874, IUER, 'GETDB_FILL_OBORG', &
     &                         'Error in getting lcode '//CAL(J4)%NAME// &
     &                         ' for the second station'  )
                           RETURN
                      END IF
!
                      CALIBS(1,1,J4) =  ARR_R8(1)
                      CALIBS(2,1,J4) = -ARR_R8(2)
                 END IF
 440          CONTINUE
         END IF
!
! ------ Compute ionospheric frequencies at the first band
!
         IF ( LQUAL_S_CHR .NE. ' 0'                          .AND.  &
     &        ( FRQ_BAND_MAX - FRQ_BAND_MIN > FRQ_DIF_LIM )         ) THEN
!
              IF ( .NOT. FL_IONO_FREQ ) THEN
                   CALL IONFR ( J1, IONFR_MODE__USEAP, N_FRQ(2), NOAP, FRQ_BAND, &
     &                          AMP_PHS, NOSAMPLE_R8, SAMPLE_RATE(1), REFFR_S, &
     &                         .TRUE., .FALSE., EFFREQ_S, PHEFFREQ_S, REFFREQ_S )
              END IF
              IF ( DABS(GION(1)) < 1.D-14  .AND.  DABS(GIONSG(1))  < 1.D-14 ) THEN
!
! ---------------- If ionospheric contribution was not computed, it is just
! ---------------- time to do it now.
!
                   FREQ_EFF(1) = EFFREQ*1.D6
                   FREQ_EFF(2) = EFFREQ_S*1.D6
                   IF ( DABS(FREQ_EFF(1) - FREQ_EFF(2)) > MIN__FRQ*1.D6 ) THEN
!
! --------------------- Check whether the frequnecies are in range
!
                        GION(1)   = -(TAU_GR(1) - TAU_GR(2))* &
     &                              FREQ_EFF(2)**2/(FREQ_EFF(1)**2 - FREQ_EFF(2)**2)* &
     &                              1.D6
                        GIONSG(1) = DSQRT ( DERR**2 + DERR_S**2) * &
     &                              FREQ_EFF(2)**2/(FREQ_EFF(1)**2 - FREQ_EFF(2)**2)
!
! --------------------- The same for delay rate
!
                        FREQ_EFF(1) = REFFREQ*1.D6
                        FREQ_EFF(2) = REFFREQ_S*1.D6
                        GION(2)   = -(ROBS - ROBS_S)* &
     &                              FREQ_EFF(2)**2/(FREQ_EFF(1)**2 - FREQ_EFF(2)**2)
                        GIONSG(2) = DSQRT ( RERR**2 + RERR_S**2) * &
     &                              FREQ_EFF(2)**2/(FREQ_EFF(1)**2 - FREQ_EFF(2)**2)
                      ELSE
                        GION(1) = 0.0
                        GIONSG(1) = 1.D-6
                        GION(2) = 0.0
                        GIONSG(2) = 1.D-6
                   END IF
              END IF
            ELSE
              IF ( .NOT. FL_IONO_FREQ ) THEN
                   EFFREQ_S   = REFFR_S
                   PHEFFREQ_S = REFFR_S
                   REFFREQ_S  = REFFR_S
              END IF
         END IF
         IF ( REFFR      < FRQ_DIF_LIM ) REFFR      = FRQ_DIF_LIM
         IF ( REFFR_S    < FRQ_DIF_LIM ) REFFR_S    = FRQ_DIF_LIM
         IF ( PHEFFREQ   < FRQ_DIF_LIM ) PHEFFREQ   = FRQ_DIF_LIM
         IF ( PHEFFREQ_S < FRQ_DIF_LIM ) PHEFFREQ_S = FRQ_DIF_LIM
!
         IF ( FUSED_STATUS == IONOV__LOADED ) THEN
              TEC_APR = 0.0D0
              DTEC_ADJ = 0.0D0
              CALL ERR_PASS   ( IUER, IER )
              ier = 0 ! %%%%%%%
              CALL GVH_GLCODE ( GVH, 'TEC_APR ', J1, 1, 16, &
     &                          DIMS(1), DIMS(2), TEC_APR, IER )
              ier = 0 ! %%%%%%%
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8875, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode ... TEC_APR' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              ier = 0 ! %%%%%%%
              CALL GVH_GLCODE ( GVH, 'DTEC_ADJ', J1, 1, 8, &
     &                          DIMS(1), DIMS(2), DTEC_ADJ, IER )
              ier = 0 ! %%%%%%%
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8877, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode DTEC_ADJ' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'DTEC_SIG', J1, 1, 8, &
     &                          DIMS(1), DIMS(2), DTEC_ERR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8879, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode TEC_SIG' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'DEL_BIAS', J1, 1, 8, &
     &                          DIMS(1), DIMS(2), DEL_BIAS_UL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8881, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode DEL_BIAS' )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'DBDT_STS', J1, 1, 2, &
     &                          DIMS(1), DIMS(2), DTEC_FLG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8882, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &                 'getting lcode DEL_BIAS' )
                   RETURN
              END IF
         END IF
!
! ------ Write the current from to OBSFIL
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE
      IF ( N_EFF > 0 ) THEN
           MEAN_EFF_FREQ = MEAN_EFF_FREQ/N_EFF
      END IF
      CALL ACS_OBSFIL ( 'C' )
!
! --- Update usage status bits
! 
      CALL UPDATE_SUPSTAT ()
!
      DEALLOCATE ( OBS_TAB )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETDB_FILL_OBORG  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GETDB_QUALCODE_FIX ( GVH, NUMOBS, IND_QUALCODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliar routine to fix a pathological case when there is only    *
! *   one band, but qualcode has dimension 2 and it is not clear         *
! *   which dimension, the first or the second corresponds to the        *
! *   1st (used) band.                                                   *
! *                                                                      *
! * ## 17-NOV-2019  GETDB_QUALCODE_FIX v1.0 (c) L. Petrov 17-NOV-2019 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4  NUMOBS, IND_QUALCODE, IUER
      CHARACTER  STR1_ARR(32)*1
      INTEGER*4  N1, N2, J1, DIMS(2), IER
!
      N1 = 0
      N2 = 0
      DO 410 J1=1,NUMOBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'QUALCODE', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     STR1_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8899, IUER, 'GETDB_QUALCODE_FIX', 'Error in '// &
     &            'getting lcode QUALCODE' )
              RETURN
         END IF
         IF ( STR1_ARR(1) .NE. '0' ) N1 = N1 + 1
         IF ( STR1_ARR(2) .NE. '0' ) N2 = N2 + 1
 410  CONTINUE 
      IF ( N1 == 0 ) IND_QUALCODE = 2
      IF ( N2 == 0 ) IND_QUALCODE = 1
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE GETDB_QUALCODE_FIX  !#!  
