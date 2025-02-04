      PROGRAM    GVF_DB_MAIN
! ************************************************************************
! *                                                                      *
! *   Routine  GVF_DB_MAIN
! *                                                                      *
! *  ### 14-OCT-2007  GVF_DB_MAIN  v1.4 (c)  L. Petrov  30-AUG-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INCLUDE   'gvf_db.i'
      INCLUDE   'vcat.i'
#ifdef GVH_STANDALONE
      INCLUDE   'gvh_solve.i'
#else
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
#endif
      TYPE     ( GVF_DB__TYPE ) :: GVF_DB
      TYPE     ( GVH__STRU    ) :: GVH
      TYPE     ( VCAT__TYPE   ) :: VCAT
      CHARACTER  DB_NAME*128, VCAT_CONF_FILE*128, GVF_DB_DIR*128, GVF_ENV_DIR*128, &
     &           VTD_CONF_FILE*128, VCAT_REPO_NAME*128, SAVE_DIR*128, STR*128
      CHARACTER, ALLOCATABLE :: F_ENV(:)*128
      INTEGER*4  MODE, M_ENV, L_ENV, IUER
      PARAMETER  ( M_ENV = 128*1024 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL SET_SIGNAL_CTRLC ( 1 )
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: gvf_db {db_name} {mode}'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DB_NAME )
           CALL GETARG ( 2, STR     )
      END IF
!
      CALL CHIN ( STR, MODE )
!
      IF ( MODE < 1  .OR.  MODE > 30 ) THEN
           CALL ERR_LOG ( 2711, -2, 'GVF_DB_MAIN', 'Wrong mode: '// &
     &          STR(1:I_LEN(STR))//' only mode in range [1, 5], 10, 20, and 30 are supported' )
           CALL EXIT ( 1 )
      END IF
      GVF_DB%STATUS = GVF_DB__UNDF
!
!
      CALL GETENVAR  ( 'PSOLVE_SAVE_DIR', STR )
      IF ( ILEN(STR) == 0 ) THEN
#ifdef GVH_STANDALONE
           WRITE ( 6, '(A)' ) 'Please define environemnetr variable PSOLVE_SAVE_DIR'
           CALL EXIT ( 1 )
#else 
           STR = SOLVE_SAVE_DIR
#endif
      END IF
      IF ( STR(I_LEN(STR):I_LEN(STR)) .NE. '/' ) THEN
           STR = STR(1:I_LEN(STR))//'/'
      END IF
!
! --- First check the environment variable VCAT_CONF
!
      CALL GETENVAR  ( 'VCAT_CONF', VCAT_CONF_FILE )
      IF ( ILEN(VCAT_CONF_FILE) == 0 ) THEN
           VCAT_CONF_FILE = STR(1:I_LEN(STR))//'vcat.conf'
      END IF
      CALL GETENVAR ( 'VCAT_REPO', VCAT_REPO_NAME )
      IF ( ILEN(VCAT_REPO_NAME) == 0 ) THEN
           VCAT_REPO_NAME = GVH__DEF_REPO 
      END IF
!
      IUER = -1
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1901, IUER, 'GVF_DB_MAIN', 'Failure in '// &
     &         'parsing VCAT configuration file '//VCAT_CONF_FILE )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the database into GVH
!
      IUER = -1
      CALL GVF_DB_READ ( DB_NAME, VCAT_REPO_NAME, VCAT, GVH, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1901, IUER, 'GVF_DB_MAIN', 'Failure in '// &
     &         'parsing VCAT configuration file '//VCAT_CONF_FILE )
           CALL EXIT ( 1 )
      END IF
      GVF_DB%STATUS = GVF_DB__READ
!
      IUER = -1
      CALL GVF_DB_GET ( GVH, GVF_DB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2717, -2, 'GVF_DB_MAIN', 'Error in an attempt '// &
     &         'to transform contents of the GVH object into GVF_DB '// &
     &         'object' )
           CALL EXIT ( 1 )
      END IF
      GVF_DB%STATUS = GVF_DB__FILL
!
      IUER = -1
      CALL GVF_DB_WRITE ( MODE, GVF_DB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2718, -2, 'GVF_DB_MAIN', 'Error in an attempt '// &
     &         'to print contants of the GVF_DB object' )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  GVF_DB_MAIN  !#!#
