      PROGRAM    GVF_TO_SUPER
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vtd.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      INCLUDE   'vcat.i'
      INTEGER*2  IPASS(64)
      INTEGER*4  IUER
      CHARACTER  DBNAME*64
      CHARACTER  VCAT_CONF_FILE*128, GVF_DB_DIR*128, GVF_ENV_DIR*128
      LOGICAL*4  FL_BATCH
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( VCAT__TYPE ) :: VCAT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gvf_to_super {input_dbname} {init}'
           CALL EXIT ( 1 )
        ELSE 
           CALL GETARG (  1, DBNAME )
           CALL GETARG (  2, PRE_LETRS )
           CALL TRAN   ( 11, PRE_LETRS, PRE_LETRS )
      END IF
      FL_BATCH = .TRUE.
!
      CALL GETENVAR ( 'PSOLVE_SAVE_DIR', PRE_SAV_DIR )
      IF ( ILEN(PRE_SAV_DIR) == 0 ) PRE_SAV_DIR = SOLVE_SAVE_DIR
      PRE_SV_LEN = I_LEN(PRE_SAV_DIR)
      IF ( PRE_SAV_DIR(PRE_SV_LEN:PRE_SV_LEN) .NE. '/' ) THEN
           PRE_SV_LEN = PRE_SV_LEN+1
           PRE_SAV_DIR(PRE_SV_LEN:PRE_SV_LEN) = '/'
      ENDIF
!
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', PRE_SCR_DIR )
      IF ( ILEN(PRE_SCR_DIR) == 0 ) PRE_SCR_DIR = SOLVE_WORK_DIR
      PRE_SD_LEN = I_LEN(PRE_SCR_DIR)
      IF ( PRE_SCR_DIR(PRE_SD_LEN:PRE_SD_LEN) .NE. '/' ) THEN
           PRE_SD_LEN = PRE_SD_LEN+1
           PRE_SCR_DIR(PRE_SD_LEN:PRE_SD_LEN) = '/'
      ENDIF
!
      VCAT_CONF_FILE = PRE_SAV_DIR(1:PRE_SV_LEN)//'vcat.conf'
!
      IUER = -1
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GETDB_DO ( VCAT, GVH, VTD, DBNAME, DBNAME, &
     &                FL_BATCH, FL_COMP_THEO, FL_SOU_USE_DB_IGNORE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      IUER = -1
      CALL UPTDB_SUP ( GVH, IUER )
      CALL EXIT ( IUER )
!
      END  PROGRAM   GVF_TO_SUPER  !#!#
