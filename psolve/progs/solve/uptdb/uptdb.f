      PROGRAM    UPTDB
! ************************************************************************
! *                                                                      *
! *   Program UPTDB
! *                                                                      *
! *  ### 14-AUG-2003     UPTDB     v1.4 (c)  L. Petrov  22-DEC-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      INTEGER*2  IPASS(64)
      INTEGER*4  CODE, IUER
      CHARACTER  DBNAME*64
      CHARACTER  STR*128
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( VCAT__TYPE ) :: VCAT
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
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
      INCLUDE   'uptdb_version.i' ! Set revision date of the current version
      CALL USE_BUFFER ( IPASS, INT2(64), 'ORC' )
!
      CALL USE_COMMON   ( 'ORC' ) 
      CALL USE_GLBFIL   ( 'OR' ) 
      CALL USE_GLBFIL_4 ( 'RC' ) 
      CALL SOCOM_EXT()
      CALL OPENNAMFIL()
!
      IF ( IPASS(1) == 1 ) THEN
           IUER = -1
           CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7701, IUER, 'UPTDB', 'Error in parsing VCAT '// &
     &              'configuration file '//VCAT_CONF_FILE )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL CLRCH ( DBNAME ) 
           IUER = -1
           CALL UPTDB_MENU ( GVH, VCAT_REPO, DBNAME_CH, DBNAME_VER, CODE, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
           IUER = -1
           IF ( CODE == 1  .OR.  CODE == 2 .OR.  CODE == 3 ) THEN
                CALL UPTDB_DO  ( GVH, VCAT, CODE, IUER )
             ELSE IF ( CODE == -1 ) THEN
                CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
                CALL END_PROG()
           END IF
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
           IF ( CODE == 1  ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( DBNAME_VER, STR )
                CALL HIT_CONT ( 'The old version '//TRIM(STR)//' of the database '//DBNAME_CH// &
     &                          ' was updated. Hit any key to continue', 0 ) 
              ELSE IF ( CODE == 2 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( DBNAME_VER+1, STR )
                CALL HIT_CONT ( 'The new version '//TRIM(STR)//' of the database is '// &
     &                          'created. Hit any key to continue', 0 ) 
              ELSE IF ( CODE == 3 ) THEN
                CALL CLRCH ( STR )
                IF ( DBNAME_VER == 1 ) THEN
                     CALL INCH  ( DBNAME_VER+1, STR )
                     CALL HIT_CONT ( 'The new version '//TRIM(STR)//' of the database is '// &
     &                               'created. Hit any key to continue', 0 ) 
                   ELSE
                     CALL HIT_CONT ( 'The old version '//TRIM(STR)//' of the database '//DBNAME_CH// &
     &                               ' was updated. Hit any key to continue', 0 ) 
                END IF
           END IF
           CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
           CALL END_PROG()
         ELSE 
           CALL LIB$MOVC3 ( 64, IPASS(9), DBNAME ) 
      END IF
!
      END  !#!  UPTDB  #!#
