      SUBROUTINE GETDB_FILL_SOCOM ( GVH, FL_PARAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GETDB_FILL_SOCOM 
! *                                                                      *
! *   It check whether lcode DGCL_EST is present in the database.        *
! *   If yes, GETDB_FILL_SOCOM returns FL_PARAM to .TRUE.                *
! *                                                                      *
! * ### 21-NOV-2005  GETDB_FILL_SOCOM v1.4 (c) L. Petrov 03-JAN-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'ners.i'
      INCLUDE   'socom.i'
      TYPE    ( GVH__STRU  ) :: GVH
      TYPE    ( NERS__TYPE ) :: NERS
      LOGICAL*4  FL_PARAM
      CHARACTER  DESCR*80, STR*80, PIMA_VER*128
      INTEGER*4  I4_VAL, L_PAR, IUER
      INTEGER*2  I2_VAL
      INTEGER*4  CLASS, TYP, DIMS(2), NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &           NUM_BAND, N_AVBAND, IP, J1, J2, J3, J4, J5, J6, FRTYPFIT, IER 
      REAL*8     PARS(NERS__MPAR)
      ADDRESS__TYPE  ADR_DATA
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initializing
!
      CALL NOUT ( JSOCOM_BYTES, PI_VAR )
      CALL SRSET ()
!
      PI_VAR   = PI__NUM
      VLIGHT   = 299792458.0D0
      UT1_RS       = 'N'
      UT1_RS_FLYBY = 'N'
      CGM_TYPE     = .FALSE.
      INIT_INTERACTIVE = 0
      OLD_CLOCKS = .FALSE.
      OLD_ATMS   = .FALSE.
      LOGBCL     = .FALSE.
      BMODE_CL   = .TRUE.
      BMODE_AT   = .TRUE.
      CLK_BRK_STAT    = .FALSE.
      FLYBY_WARNING   = .FALSE.
      SITE_DEP_CONST  = .FALSE.
      SIMULATION_TEST = .FALSE.
      SITE_DEP_EL_CUT = .FALSE.
      SHORT_UT1_IN    = .FALSE.
      SOL_AVAIL       = .FALSE.
      SKIP_EOP_OFF    = .FALSE.
!
      IF ( INDEX ( GVH%GENERATOR, 'simul' ) > 0 ) THEN
           SIMULATION_TEST = .TRUE.
         ELSE 
           SIMULATION_TEST = .FALSE.
      END IF
!
      IER = 0
      CALL GVH_GLCODE ( GVH, 'EXP_DESC', 0, 0, LEN(EXP_DESC), &
     &                  DIMS(1), DIMS(2), %REF(EXP_DESC), IER )
      IF ( IER .NE. 0 ) THEN
           IER = 0
           CALL ERR_LOG ( 8711, IER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode EXP_DESC in processing input GVF experiment '// &
     &         'with the enevlope file '//GVH%FILEENV )
!!           WRITE ( 6, * ) 'Nevertheless, continue'
           EXP_DESC = '??'
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL CLRCH ( STR )
      CALL GVH_GLCODE ( GVH, 'EXP_CODE', 0, 0, LEN(STR), &
     &                  DIMS(1), DIMS(2), %REF(STR), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8712, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode EXP_CODE' )
           RETURN 
      END IF
      EXP_CODE = STR
      IF ( EXP_DESC == '??' ) THEN
           EXP_DESC = EXP_CODE
      ENDIF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'PI_NAME ', 0, 0, LEN(PI_NAME), &
     &                  DIMS(1), DIMS(2), %REF(PI_NAME), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8713, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode PI_NAME' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, DIMS(1), DIMS(2), I4_VAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8714, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode NUMB_STA' )
           RETURN 
      END IF
      NUMSTA = I4_VAL
      TOTSTA = I4_VAL
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SOU', 0, 0, 4, DIMS(1), DIMS(2), I4_VAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8715, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode NUMB_SOU' )
           RETURN 
      END IF
      NUMSTR = I4_VAL
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), NUMOBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8716, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, DIMS(1), DIMS(2), NUMSCA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8717, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( 0, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_MTAI', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  UTC_M_TAI, IER )
      IF ( IER .NE. 0 .OR. UTC_M_TAI < -90.0 .OR. UTC_M_TAI > -2.0 ) THEN
!
! -------- Did not get UTC minus TAI, or got a wrong number
!
           CALL ERR_PASS   ( IUER, IER )
           CALL NERS_INIT ( 'NERS_CONFIG', NERS, -1.0D0, -1.0D0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8718, IUER, 'GETDB_FILL_SOCOM', 'Error in initializing '// &
          &         'NERS data structure' )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL NERS_GET_EOP ( NERS, -1.0D15, 'utcmtai', NERS__MPAR, L_PAR, PARS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8719, IUER, 'GETDB_FILL_SOCOM', 'Error evaluating the '// &
          &         'Earth orientation parameter' )
                RETURN 
           END IF
           UTC_M_TAI = PARS(1)
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_BAND', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  NUM_BAND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8720, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode NUM_BAND' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'N_AVBAND', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  N_AVBAND, IER )
      IF ( IER .NE. 0 ) THEN
!           CALL ERR_LOG ( 8721, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
!     &         'getting lcode N_AVBAND' )
!           RETURN 
!         ELSE 
           N_AVBAND = NUM_BAND
      END IF
!
      OPP_STATUS = 0
      FUSED_STATUS = IONOV__UNDF
      IF ( N_AVBAND == 2 ) THEN
           CALL SBIT  ( OPP_STATUS, OPP_SET1__BIT, INT2(1) ) 
           CALL SBIT  ( OPP_STATUS, OPP_SET2__BIT, INT2(1) ) 
!
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'DBDT_STS', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8722, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &              'inquiring lcode IONOV_GD' )
                RETURN 
           END IF
           IF ( TYP == 0 ) THEN
                CALL SBIT  ( OPP_STATUS, OPP_DUAL__BIT, INT2(0) ) 
             ELSE
                CALL SBIT  ( OPP_STATUS, OPP_DUAL__BIT, INT2(1) ) 
                FUSED_STATUS = IONOV__LOADED
           END IF
      END IF      
!
      IF ( .NOT. SIMULATION_TEST ) THEN
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'MK3_DBNM', 0, 0, LEN(STR), &
     &                       DIMS(1), DIMS(2), %REF(STR), IER )
           IF ( IER .NE. 0 ) THEN
                IER = -1
                CALL ERR_LOG ( 8723, IER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &              'getting lcode MK3_DBNM' )
!@              RETURN 
           END IF
           MK3_DBNM = STR
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'CORPLACE', 0, 0, LEN(CORRELATOR_NAME), &
     &                       DIMS(1), DIMS(2), %REF(CORRELATOR_NAME), IER )
           IF ( IER .NE. 0 ) THEN
!                IER = -1
!                CALL ERR_LOG ( 8724, IER, 'GETDB_FILL_SOCOM', 'Error in '// &
!     &              'getting lcode CORPLACE, nevertheless, continue'
!                RETURN 
              CORRELATOR_NAME = '??????????'
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'REC_MODE', 0, 0, LEN(REC_MODE), &
     &                       DIMS(1), DIMS(2), %REF(REC_MODE), IER )
           IF ( IER .NE. 0 ) THEN
!                CALL ERR_LOG ( 8725, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
!     &              'getting lcode REC_MODE' )
!                RETURN 
              ELSE
                 REC_MODE = '??????????'
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'COR_TYPE', 0, 0, LEN(CORRTYPE), &
     &                       DIMS(1), DIMS(2), %REF(CORRTYPE), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8726, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &              'getting lcode COR_TYPE' )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'EXPSERNO', 0, 0, 2, DIMS(1), DIMS(2), &
     &                       EXPSERNO, IER )
           IF ( IER .NE. 0 ) THEN
!                CALL ERR_LOG ( 8727, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
!     &              'getting lcode EXPSERNO' )
!                RETURN 
              ELSE
                 EXPSERNO = 0
           END IF
!
           IER = 0
           CALL GVH_GLCODE ( GVH, 'PIMA_VER', 0, 0, LEN(PIMA_VER), &
     &                       DIMS(1), DIMS(2), %REF(PIMA_VER), IER )
           IF ( IER == 0 ) THEN
                CALL ERR_PASS   ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'PIMA_CNT', 0, 0, LEN(PIMA_CNT), &
     &                            DIMS(1), DIMS(2), %REF(PIMA_CNT), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8728, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &                   'getting lcode PIMA_CNT' )
                     RETURN 
                END IF
!
              ELSE 
                CALL CLRCH ( PIMA_CNT )
           END IF
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'BAND_NAM', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8729, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &              'inquiring lcode IONOV_GD' )
                RETURN 
           END IF
!
           IF ( CLASS == 0 ) THEN
!
! ------------- The field was not filled. Put reasonable defaults.
!
                BAND_NAM(1) = 'X'
                BAND_NAM(2) = 'S'
             ELSE                
!
! ------------- NB: we may have a pahtologocal case, f.e. 20060623_a, when NUM_BAND = 1,
! ------------- but DIMS(2) = 2. This may be related to a loss of information about the
! ------------- the lower band
!
                IP = DIMS(1)*DIMS(2)
                CALL ERR_PASS   ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'BAND_NAM', 0, 0, IP, DIMS(1), DIMS(2), &
     &                            BAND_NAM, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8730, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &                   'getting lcode BAND_NAM' )
                     RETURN 
                END IF
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'EDIT_STS', 0, 0, 4, DIMS(1), DIMS(2), &
     &                       EDIT_STS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8731, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &              'getting lcode EDIT_STS' )
                RETURN 
           END IF
!
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'TH_PROG ', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8732, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &              'inquiring lcode TH_PROG ' )
                RETURN 
           END IF
           IF ( CLASS == 0 ) THEN
                CALCV =    0.0D0
              ELSE 
                CALCV = -101.0D0
           END IF
!
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'DGCL_EST', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8733, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &              'inquiring lcode DGCL_EST' )
                RETURN 
           END IF
           IF ( CLASS == 0 ) THEN
                FL_PARAM = .FALSE. 
              ELSE 
                FL_PARAM = .TRUE.
           END IF
         ELSE
!
! -------- Simulation case
!
           CORRELATOR_NAME = 'Simulation'
           CORRTYPE        = 'Simul'
           REC_MODE = 'unknown'
           BAND_NAM = 'X'
           FL_PARAM = .FALSE. 
           CALCV    = 0.0D0
           EXPSERNO = 0
           MK3_DBNM = EXP_CODE
           EDIT_STS = 0
      END IF
!
      IDBSEL = IBSET ( IDBSEL, 0 )
      IDCSEL = IBSET ( IDCSEL, 0 )
      NDB = 1
!
! --- By default we select everything
!
      DO 410 J1=1,NUMSTA-1
         DO 420 J2=J1+1,NUMSTA
            CALL SBIT ( IBLSEL_G(1,J1), INT2(J2), INT2(1) )
            CALL SBIT ( IBLSEL_P(1,J1), INT2(J2), INT2(1) )
            CALL SBIT ( IBLSEL_G(1,J2), INT2(J1), INT2(1) )
            CALL SBIT ( IBLSEL_P(1,J2), INT2(J1), INT2(1) )
 420     CONTINUE
 410  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'DATYP   ', 1, 1, 2, DIMS(1), DIMS(2), &
     &                  IDATYP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8734, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting "DATYP   " lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SUPMET  ', 1, 1, 2, DIMS(1), DIMS(2), &
     &                  SUPMET, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8735, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting "SUPMET  " lcode' )
           RETURN 
      END IF
!
      SITE_DEP_CONST = .TRUE.
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETDB_FILL_SOCOM  !#!#
