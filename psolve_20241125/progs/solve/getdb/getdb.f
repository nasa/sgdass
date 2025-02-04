      PROGRAM    GETDB
! ************************************************************************
! *                                                                      *
! *   Program GETDB
! *                                                                      *
! *  ### 14-AUG-2003     GETDB     v1.4 (c)  L. Petrov  25-MAR-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vtd.i'
      INCLUDE   'vcat.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      INTEGER*2  IPASS(64)
      CHARACTER  DBNAME*64
      CHARACTER  ENV_NAME*128, VCAT_REPO_NAME*3, &
     &           STR_EQUAL_EFF_FREQ*4, STR*128
      LOGICAL*4  FL_BATCH, FL_COMP_THEO, FL_SOU_USE_DB_IGNORE
      INTEGER*4  IUER
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( VCAT__TYPE ) :: VCAT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      CALL PRE_PROG()
      CALL ERR_MODE ( 'NO_PROBE' ) 
!        
      INCLUDE   'getdb_version.i' ! Set revision date of the current version
      CALL USE_BUFFER ( IPASS, INT2(64), 'ORC' )
      CALL USE_GLBFIL_4 ( 'OR' )
!
      CALL GETENVAR ( 'EQUAL_EFF_FREQ', STR_EQUAL_EFF_FREQ )
      IF ( ILEN(STR_EQUAL_EFF_FREQ) == 0 ) STR_EQUAL_EFF_FREQ = 'NO'
      IF ( STR_EQUAL_EFF_FREQ(1:3) == 'YES' .OR. STR_EQUAL_EFF_FREQ(1:3) == 'yes' ) THEN
!
           FL_EQUAL_EFF_FREQ = .TRUE.
           CALL USE_GLBFIL_4 ( 'WC'  )
         ELSE IF ( STR_EQUAL_EFF_FREQ(1:2) == 'NO' .OR. STR_EQUAL_EFF_FREQ(1:2) == 'no' ) THEN
!
           FL_EQUAL_EFF_FREQ = .FALSE.
           CALL USE_GLBFIL_4 ( 'WC'  )
      END IF
!
      IF ( IPASS(1) == 1 ) THEN
           CALL UN_CURSES () 
           CALL CLRCH ( DBNAME ) 
           FL_BATCH = .FALSE.
         ELSE 
           CALL LIB$MOVC3 ( 64, IPASS(9), DBNAME ) 
           FL_BATCH = .TRUE.
      END IF
      CALL SET_SIGNAL_CTRLC ( 1 )
!
! --- Read and parse VCAT configuration
!
      IUER = -1
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7601, IUER, 'GETDB', 'Error in parsing VCAT '// &
     &         'configuration file '//VCAT_CONF_FILE )
           CALL EXIT ( 1 ) 
      END IF
      CALL GETENVAR ( 'PSOLVE_CHECK_VCAT', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:1) == 'Y' ) THEN
           WRITE ( 6, * ) 'GETDB: VCAT%VTD_CONF_SES_FILE= ', TRIM(VCAT%VTD_CONF_SES_FILE)
           CALL HIT_CONT ( 'Hit any key to continue', 0 ) 
      END IF
!
      IUER = -1
      FL_COMP_THEO         = .TRUE.
      FL_SOU_USE_DB_IGNORE = .FALSE.
      CALL GETDB_DO ( VCAT, GVH, VTD, DBNAME, ENV_NAME, &
     &                FL_BATCH, FL_COMP_THEO, FL_SOU_USE_DB_IGNORE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
      CALL END_PROG()
      END  PROGRAM   GETDB  !#!#
