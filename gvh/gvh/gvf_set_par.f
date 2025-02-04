      PROGRAM    GVF_SET_PAR
! ************************************************************************
! *                                                                      *
! *   Program GVF_SET_PAR inserts or update parameterization in VLBI     *
! *   database using informatino from extenal files. It includes         *
! *   a) clock breaks;                                                   *
! *   b) deselected baselines;                                           *
! *   c) source estimation.                                              *
! *                                                                      *
! * ### 24-MAR-2022  GVF_SET_PAR   v1.1 (c) L. Petrov  25-MAR-2022 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vcat.i'
      INCLUDE   'gvh.i'
      INCLUDE   'gvf_db.i'
      TYPE     ( VCAT__TYPE ) :: VCAT
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4    MP, ME, M_STA, M_SOU, M_BAS, M_OBS
      PARAMETER  ( MP = 32*1024 )
      PARAMETER  ( ME = 32 )
      PARAMETER  ( M_STA =   32 )
      PARAMETER  ( M_SOU = 2048 )
      PARAMETER  ( M_BAS =  512 )
      PARAMETER  ( M_OBS =  512*1024 )
      CHARACTER  FIL_BRK*128, FIL_BAS*128, FIL_SES*128, FIL_SOU*128, &
     &           SESS_NAME*10, EXP_NAME*80, FIL_GVF(ME)*256, STR*128
      CHARACTER  BUF_BRK(MP)*80, BUF_BAS(MP)*80, BUF_SOU(MP)*80
      CHARACTER  C_BAS(MP)*17, C_BRK(MP)*54, C_STA(M_STA)*8, C_SOU(M_SOU)*8, &
     &           SOU_NAM(M_SOU)*8
      CHARACTER  VCAT_CONF_FILE*128, GVF_DB_DIR*128, GVF_ENV_DIR*128, VTD_CONF_SES*128, &
     &           VCAT_REPO_NAME*128, ENV_FILE*128, STA_CLBR(ME)*8
      REAL*8     UTCMTAI, UTC_CLBR(M_BAS), TIM
      INTEGER*4  CLASS_BAS_USE, CLASS_NUM_CLBR, CLASS_STA_CLBR, CLASS_SOU_EST, &
     &           TYP, NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, DIMS(2)
      CHARACTER  DESCR*80, STR_DES*8, STR_BRK*8, STR_SOU*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           K_BAS, ID, IL, REMAINED_BYTES, &
     &           N_BRK, N_BAS, N_SOU, SEG_SL1, IND_SL1, &
     &           L_FIL, L_BAS, L_BRK, L_SOU, &
     &           I_BAS, I_BRK, I_SOU, NE, NUMB_SOU, NUMB_STA, &
     &           NUMB_BAS, NUMB_OBS, NUMB_SCA, NOBS_STA(M_STA), &
     &           OBS_TAB(3,M_OBS), BAS_USE(M_BAS), MJD_CLBR(M_BAS), &
     &           SOCO_EST(2,M_SOU), IUER 
      ADDRESS__TYPE :: ADR_DATA
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, ADD_CLIST, LTM_DIF
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, * ) 'Usage: gvf_put_brk_des fil_sess bas_file break_file prp_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_SES ) 
           CALL GETARG ( 2, FIL_BAS ) 
           CALL GETARG ( 3, FIL_BRK ) 
           CALL GETARG ( 4, FIL_SOU ) 
      END IF
!
! --- Set VCAT repository name
!
      CALL CLRCH    ( VCAT_REPO_NAME )
      CALL GETENVAR ( 'VCAT_CONF', VCAT_CONF_FILE )
      CALL GETENVAR ( 'VCAT_REPO', VCAT_REPO_NAME )
      IF ( ILEN(VCAT_CONF_FILE) > 0 ) THEN
           IUER = -1
           CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
! --- Resolve he sessin name. Get binary database names in L_FIL/FIL_GVF
!
      IUER = -1
      CALL VCAT_RESOLVE_DBNAME ( VCAT, FIL_SES, VCAT_REPO_NAME, &
     &                           ENV_FILE, ME, L_FIL, FIL_GVF, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2811, IUER, 'GVF_SET_PAR', 'Failure in '// &
     &         'resolving database name '//FIL_SES )
           CALL EXIT ( 1 )
      END IF
      FIL_SES = ENV_FILE
!
! --- Get the experiment name 
!
      ID = LINDEX ( FIL_SES, '/' )
      IL = ILEN(FIL_SES)
      SESS_NAME = FIL_SES(ID+1:ID+10)
!
! --- Read the file with deselected baselines
!
      IUER = -1
      CALL RD_TEXT ( FIL_BAS, MP, BUF_BAS, N_BAS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2812, IUER, 'GVF_SET_PAR', 'Error in '// &
     &         'reading file deslected baselines '//FIL_BAS )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the file with clock break station names and clock break epochs
!
      IUER = -1
      CALL RD_TEXT ( FIL_BRK, MP, BUF_BRK, N_BRK, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2813, IUER, 'GVF_SET_PAR', 'Error in '// &
     &         'reading file with clock breaks '//FIL_BRK )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the file with source names which positions should be estimated
!
      IUER = -1
      CALL RD_TEXT ( FIL_SOU, MP, BUF_SOU, N_SOU, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2814, IUER, 'GVF_SET_PAR', 'Error in '// &
     &         'reading file with clock breaks '//FIL_BRK )
           CALL EXIT ( 1 )
      END IF
!
! --- Get the list of baselines that has to be deselected in this experiment
!
      L_BAS = 0
      DO 410 J1=1,N_BAS
         IF ( BUF_BAS(J1)(1:10) == SESS_NAME ) THEN
              I_BAS = ADD_CLIST ( MP, L_BAS, C_BAS, BUF_BAS(J1)(24:40), IUER )
         END IF
 410  CONTINUE 
!
! --- Get the list of clock breaks in this experiment
!
      L_BRK = 0
      DO 420 J2=1,N_BRK
         IF ( BUF_BRK(J2)(1:10) == SESS_NAME ) THEN
              I_BRK = ADD_CLIST ( MP, L_BRK, C_BRK, BUF_BRK(J2)(24:77), IUER )
         END IF
 420  CONTINUE 
!
! --- Get the list of sources with positions to be estimated in this experiment
!
      L_SOU = 0
      DO 430 J3=1,N_SOU
         IF ( BUF_SOU(J3)(1:10) == SESS_NAME ) THEN
              I_SOU = ADD_CLIST ( MP, L_SOU, C_SOU, BUF_SOU(J3)(23:30), IUER )
         END IF
 430  CONTINUE 
!
! --- Initialize GVH object
!
      IUER = -1
      CALL GVH_INIT ( GVH,  IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2815, IUER, 'GVF_SET_PAR', 'Error in an '// &
     &         'attempt to initialize GVH' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read all binary db files
!
      SEG_SL1 = -1
      DO 440 J4=1,L_FIL
         IUER = -1
         CALL GVH_READ_BGV ( GVH, 1, FIL_GVF(J4), REMAINED_BYTES, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 2816, IUER, 'GVF_SET_PAR', 'Error in '// &
     &            'an atttempt to read input database file '// &
     &             FIL_GVF(J2) )
              CALL EXIT ( 1 )
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              IUER = -1
              CALL ERR_LOG ( 2817, IUER, 'GVF_SET_PAR', 'The number '// &
     &            'of remaining bytes after reading input databae file '// &
     &             TRIM(FIL_GVF(J4))//' is not 0, but '//STR )
              CALL EXIT ( 1 )
         END IF
         IF ( INDEX ( FIL_GVF(J4), '_sl1_' ) > 0 ) THEN
!
! ----------- Gget the index os SL1 section
!
              SEG_SL1 = J4
         END IF
 440  CONTINUE 
!
! --- Prepare cashe memory for getting the data
!
      IUER = -1
      CALL GVH_PREGET ( GVH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2818, IUER, 'GVF_SET_PAR', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           CALL EXIT ( 1 )
      END IF
!
! --- Get basic experiment parameter from the database file
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'EXP_NAME', 0, 0, LEN(EXP_NAME), DIMS(1), DIMS(2), &
     &                  EXP_NAME, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2819, IUER, 'GVF_SET_PAR', 'Error '// &
     &         'in getting "EXP_NAME" lcode' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), NUMB_OBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2820, IUER, 'GVF_SET_PAR', 'Error '// &
     &         'in getting "NUMB_OBS" lcode' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, DIMS(1), DIMS(2), NUMB_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2821, IUER, 'GVF_SET_PAR', 'Error '// &
     &         'in getting "NUMB_STA" lcode' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NUMB_SOU', 0, 0, 4, DIMS(1), DIMS(2), NUMB_SOU, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2822, IUER, 'GVF_SET_PAR', 'Error '// &
     &         'in getting "NUMB_SOU" lcode' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, DIMS(1), DIMS(2), NUMB_SCA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2823, IUER, 'GVF_SET_PAR', 'Error '// &
     &         'in getting "NUMB_SCA" lcode' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NOBS_STA', 0, 0, 4*NUMB_STA, DIMS(1), DIMS(2), &
     &                  NOBS_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2824, IUER, 'GVF_SET_PAR', 'Error '// &
     &         'in getting "NOBS_STA" lcode' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 0, 0, 4*3*NUMB_OBS, DIMS(1), DIMS(2), &
     &                  OBS_TAB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2825, IUER, 'GVF_SET_PAR', 'Error '// &
     &         'in getting "OBS_TAB" lcode' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, 8*NUMB_STA, &
     &                  DIMS(1), DIMS(2), %REF(C_STA), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2826, IUER, 'GVF_SET_PAR', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'SRCNAMES', 0, 0, 8*NUMB_SOU, &
     &                  DIMS(1), DIMS(2), %REF(SOU_NAM), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2827, IUER, 'GVF_SET_PAR', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN 
      END IF
      NUMB_BAS = (NUMB_STA*(NUMB_STA-1))/2
!
! --- Check whether some LCODES are present in the database
!
      IUER = -1
      CALL GVH_INQ_LCODE ( GVH, 'BAS_USE ', DESCR, CLASS_BAS_USE, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2828, IUER, 'GVF_SET_PAR', 'Error in '// &
     &        'inquiring lcode BAS_USE ' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GVH_INQ_LCODE ( GVH, 'NUM_CLBR', DESCR, CLASS_NUM_CLBR, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2829, IUER, 'GVF_SET_PAR', 'Error in '// &
     &        'inquiring lcode NUM_CLBR' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GVH_INQ_LCODE ( GVH, 'STA_CLBR', DESCR, CLASS_STA_CLBR, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2830, IUER, 'GVF_SET_PAR', 'Error in '// &
     &        'inquiring lcode NUM_CLBR' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = 1
      CALL GVH_INQ_LCODE ( GVH, 'SOCO_EST', DESCR, CLASS_SOU_EST, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2831, IUER, 'GVF_SET_PAR', 'Error in '// &
     &        'inquiring lcode SOCO_EST' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( CLASS_BAS_USE .NE. 0 ) THEN
!
! -------- Get BAS_USE if present
!
           IUER = -1
           CALL GVH_GLCODE ( GVH, 'BAS_USE ', 1, 1, 4*NUMB_BAS, DIMS(1), DIMS(2), BAS_USE, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2832, IUER, 'GVF_SET_PAR', 'Error in '// &
     &              'getting lcode BAS_USE' )
                RETURN 
           END IF
        ELSE 
!
! -------- Otherwise, initialize
!
           BAS_USE = -1
      END IF
      IF ( L_BAS > 0 ) THEN
!
! -------- Update BAS_USE if some baselines have to be deselected
!
           K_BAS = 0
           DO 450 J5=1,NUMB_STA
              IF ( J5 < NUMB_STA ) THEN
                   DO 460 J6=J5+1,NUMB_STA
                      K_BAS = K_BAS + 1
                      IF ( L_BAS > 0 ) THEN
                           DO 470 J7=1,L_BAS
                              IF ( ( C_BAS(J7)(1:8) == C_STA(J5) .AND. C_BAS(J7)(10:17)  == C_STA(J6) ) .OR. &
     &                             ( C_BAS(J7)(1:8) == C_STA(J6) .AND. C_BAS(J7)(10:17)  == C_STA(J5) )      ) THEN
                                   BAS_USE(K_BAS) = IBCLR ( BAS_USE(K_BAS), GR__DTP_PAR    ) 
                                   BAS_USE(K_BAS) = IBCLR ( BAS_USE(K_BAS), GX__DTP_PAR    ) 
                                   BAS_USE(K_BAS) = IBCLR ( BAS_USE(K_BAS), GS__DTP_PAR    ) 
                                   BAS_USE(K_BAS) = IBCLR ( BAS_USE(K_BAS), GXS__DTP_PAR   ) 
                                   BAS_USE(K_BAS) = IBCLR ( BAS_USE(K_BAS), FUSED__DTP_PAR ) 
                              END IF
 470                       CONTINUE 
                      END IF
 460               CONTINUE 
             END IF
 450       CONTINUE 
!
           IF ( CLASS_BAS_USE == 0 ) THEN
                IUER = -1
                CALL GVH_PTOC ( GVH, 'BAS_USE ', GVH__I4, GVH__SES, NUMB_BAS, 1, &
     &                         'Bit field of baseline selection status', SEG_SL1, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 2833, IUER, 'GVF_SET_PAR', 'Trap of '// &
     &                   'internal control' )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
           IUER = -1
           CALL GVH_PLCODE ( GVH, 'BAS_USE ', 1, 0, BAS_USE, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2834, IUER, 'GVF_SET_PAR', 'Error in '// &
     &              'putting "BAS_USE" lcode' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( L_BRK > 0 ) THEN
!
! -------- Extract MJD/UTC of clock breaks
!
           DO 480 J8=1,L_BRK
              STA_CLBR(J8) = C_BRK(J8)(1:8)
              IUER = -1
              CALL DATE_TO_TIME ( C_BRK(J8)(32:54), MJD_CLBR(J8), TIM, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 2836, IUER, 'GVF_SET_PAR', 'Error in '// &
     &                 'parsing date '//C_BRK(J8)(32:54) )
                   CALL EXIT ( 1 )
              END IF
              UTC_CLBR(J8) = TIM
 480       CONTINUE 
!
           IF ( CLASS_NUM_CLBR == 0 ) THEN
!
! ------------- Create lcode NUM_CLBR is not present
!
                IUER = -1
                CALL GVH_PTOC ( GVH, 'NUM_CLBR', GVH__I4, GVH__SES, 1, 1, &
     &               'Number of clock breaks in the experiment', SEG_SL1, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2837, IUER, 'GVF_SET_PAR', 'Trap of '// &
     &                             'internal control' )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
           IF ( CLASS_STA_CLBR == 0 ) THEN
!
! ------------- Create lcode STA_CLBR is not present
!
                IUER = -1
                CALL GVH_PTOC ( GVH, 'STA_CLBR', GVH__C1, GVH__SES, 8, &
     &               L_BRK, 'Names of stations with clock breaks', &
     &               SEG_SL1, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 2838, IUER, 'GVF_SET_PAR', 'Trap of '// &
     &                   'internal control' )
                     CALL EXIT ( 1 )
                END IF
!
                IUER = -1
                CALL GVH_PTOC ( GVH, 'MJD_CLBR', GVH__I4, GVH__SES, L_BRK, &
     &               1, 'Modified Julian date of clock break epochs', &
     &               SEG_SL1, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 2839, IUER, 'GVF_SET_PAR', 'Trap of '// &
     &                   'internal control' )
                     CALL EXIT ( 1 )
                END IF
!
                IUER = -1
                CALL GVH_PTOC ( GVH, 'UTC_CLBR', GVH__R8, GVH__SES, L_BRK, &
     &               1, 'UTC time tag of clock break epochs', SEG_SL1, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 2840, IUER, 'GVF_SET_PAR', 'Trap of '// &
     &                   'internal control' )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
! -------- Update cache to putting new lcodes
!
           IUER = -1
           CALL GVH_PREPUT ( GVH, NUMB_OBS, NUMB_SCA, NUMB_STA, NOBS_STA, C_STA, &
     &                       OBS_TAB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2841, IUER, 'GVF_SET_PAR', 'Error in '// &
     &              'running GVH_PREPUT' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Put there four lcodes related to clock breaks
!
           IUER = -1
           CALL GVH_PLCODE ( GVH, 'NUM_CLBR', 1, 0, L_BRK, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2842, IUER, 'GVF_SET_PAR', 'Error '// &
     &              'in putting "NUM_CLBR" lcode' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Put into the database information about clock breaks
!
           IUER = -1
           CALL GVH_PLCODE ( GVH, 'STA_CLBR', 1, 0, STA_CLBR, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2843, IUER, 'GVF_SET_PAR', 'Error '// &
     &              'in putting "STA_CLBR" lcode' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL GVH_PLCODE ( GVH, 'MJD_CLBR', 1, 0, MJD_CLBR, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2844, IUER, 'GVF_SET_PAR', 'Error '// &
     &              'in putting "MJD_CLBR" lcode' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL GVH_PLCODE ( GVH, 'UTC_CLBR', 1, 0, UTC_CLBR, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2845, IUER, 'GVF_SET_PAR', 'Error '// &
     &              'in putting "UTC_CLBR" lcode' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( L_SOU > 0 ) THEN
!
! -------- Processing source position estimation
!
           IF ( CLASS_SOU_EST == 0 ) THEN
!
! ------------- Put lcode SOCO_EST if not defined
!
                IUER = -1
                CALL GVH_PTOC ( GVH, 'SOCO_EST', GVH__I4, GVH__SES, 2, NUMB_SOU, &
     &              'Estimation status for source coordinates per '// &
     &              'component, per object', SEG_SL1, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2846, IUER, 'GVF_SET_PAR', 'Trap of '// &
     &                  'internal control' )
                     CALL EXIT ( 1 )
                END IF
                SOCO_EST = 0
           END IF
!
! -------- Put lcode SOCO_ESTdefined
!
           IUER = -1
           CALL GVH_GLCODE ( GVH, 'SOCO_EST', 1, 1, 4*2*NUMB_SOU, &
     &                       DIMS(1), DIMS(2), SOCO_EST, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2847, IUER, 'GVF_SET_PAR', 'Error in '// &
     &              'getting "SOCO_EST" lcode' )
                RETURN
           END IF
!
! -------- Update cache to putting new lcodes
!
           IUER = -1
           CALL GVH_PREPUT ( GVH, NUMB_OBS, NUMB_SCA, NUMB_STA, NOBS_STA, C_STA, &
     &                       OBS_TAB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2841, IUER, 'GVF_SET_PAR', 'Error in '// &
     &              'running GVH_PREPUT' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Set SOCO_EST bits
!
           DO 490 J9=1,NUMB_SOU
              IF ( LTM_DIF ( 0, L_SOU, C_SOU, SOU_NAM(J9) ) > 0 ) THEN
                   DO 4100 J10=1,2
                      SOCO_EST(J10,J9) = IBSET ( SOCO_EST(J10,J9), GR__DTP_PAR    ) 
                      SOCO_EST(J10,J9) = IBSET ( SOCO_EST(J10,J9), GX__DTP_PAR    ) 
                      SOCO_EST(J10,J9) = IBSET ( SOCO_EST(J10,J9), GS__DTP_PAR    ) 
                      SOCO_EST(J10,J9) = IBSET ( SOCO_EST(J10,J9), GXS__DTP_PAR   ) 
                      SOCO_EST(J10,J9) = IBSET ( SOCO_EST(J10,J9), FUSED__DTP_PAR ) 
 4100              CONTINUE 
              END IF
 490       CONTINUE 
           IUER = -1
           CALL GVH_PLCODE ( GVH, 'SOCO_EST', 1, 0, SOCO_EST, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2848, IUER, 'GVF_SET_PAR', 'Error '// &
     &              'in putting "SOCO_EST" lcode' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( L_BAS > 0 .OR. L_BRK > 0 .OR. L_SOU > 0 ) THEN
!
! -------- Write the database file. NB: version counter is not updated.
!
           IUER = -1
           CALL GVH_WRITE_BGV ( GVH, SEG_SL1, GVH__CRT, FIL_GVF(SEG_SL1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 2849, IUER, 'GVF_SET_PAR', 'Error in '// &
     &              'attempt to write output database file '//FIL_GVF(SEG_SL1) )
                CALL EXIT ( 1 )
           END IF
           IF ( L_BAS > 0 ) THEN
                STR_DES = "des_bas"
              ELSE
                CALL CLRCH ( STR_DES )
           END IF
           IF ( L_BRK > 0 ) THEN
                STR_BRK = "clk_brk"
              ELSE
                CALL CLRCH ( STR_BRK )
           END IF
           IF ( L_SOU > 0 ) THEN
                STR_SOU = "sou_est"
              ELSE
                CALL CLRCH ( STR_SOU )
           END IF
           WRITE ( 6, '(A)' ) 'Updated database file '//TRIM(FIL_SES)// &
     &                        ' for experiment '//EXP_NAME(1:8)//' '// &
     &                        STR_DES(1:I_LEN(STR_DES))//' '// &
     &                        STR_BRK(1:I_LEN(STR_BRK))//' '// &
     &                        STR_SOU(1:I_LEN(STR_SOU))
      END IF
      CALL EXIT ( 0 ) 
      END  PROGRAM   GVF_SET_PAR  !#!  
