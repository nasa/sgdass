#define FUNC_OPENDIR       OPENDIR
      SUBROUTINE SIMUL_DO ( SIMUL, FMT_IN, FIL_IN, FMT_OUT, FIL_OUT, &
     &                      IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMUL
! *                                                                      *
! *  ### 08-JUN-2020    SIMUL_DO   v1.2  (c)  L. Petrov 07-SEP-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'vex.i'
      INCLUDE   'vtd.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      INCLUDE   'stp.i'
      INCLUDE   'simul.i'
      INCLUDE   'ners_local.i'
      TYPE     ( VEX_TYPE    ) :: VEX
      TYPE     ( STP__TYPE   ) :: STP
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      TYPE     ( GVH__STRU   ) :: GVH
      TYPE     ( VCAT__TYPE  ) :: VCAT
      TYPE     ( NERS__TYPE  ) :: NERS
      CHARACTER  FMT_IN*(*), FIL_IN*(*), FMT_OUT*(*), FIL_OUT*(*)
      REAL*8     WHT_NOISE, DIL_FACTOR
      INTEGER*4  ISEED, IVRB, IUER
!
      INTEGER*4    M_FIL, MT
      PARAMETER  ( M_FIL = 32   )
      PARAMETER  ( MT    = 1024 )
      CHARACTER  FIL_ENV*128, DB_FILE(M_FIL)*128, RENAME_TAB(MT)*128, &
     &           STR*128, STR1*128
      INTEGER*8  SIZE_I8
      LOGICAL*1  LEX
      INTEGER*4  IS, J1, IND_REP_IN, IND_REP_OUT, L_FIL, LT, UNIX_DATE, IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, LTM_DIF, TIME
      INTEGER*8  DIR_DESC
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR 
      INTEGER*4, EXTERNAL :: CLOSEDIR
!
      IF ( SIMUL%CNF%SBT_TABLE(1:2) .NE. 'NO' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( SIMUL%CNF%SBT_TABLE, MT, RENAME_TAB, LT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1411, IUER, 'SIMUL_DO', 'Failure in reading '// &
          &         'the station/source name substitution table '//SIMUL%CNF%SBT_TABLE )
                RETURN 
           END IF
         ELSE
           LT = 0
      END IF
!
! --- Read and parse VCAT configuration
!
      CALL ERR_PASS ( IUER, IER )
      CALL VCAT_GET_CONF ( SIMUL%CNF%VCAT_CONF, VCAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1412, IUER, 'SIMUL_DO', 'Error in parsing '// &
     &         'VCAT configuration file '//SIMUL%CNF%VCAT_CONF )
           RETURN 
      END IF
      IND_REP_IN  = LTM_DIF( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, SIMUL%CNF%REPO_IN )
      IF ( FMT_IN .EQ. '-i-gvf' .AND. IND_REP_IN < 1 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL ERR_LOG ( 1413, IUER, 'SIMUL_DO', 'Input GVF repository '// &
     &          TRIM(SIMUL%CNF%REPO_IN)//' is not defined in the VCAT configuration '// &
     &         'file '//SIMUL%CNF%VCAT_CONF )
           RETURN 
      END IF
!
      IND_REP_OUT = LTM_DIF( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, SIMUL%CNF%REPO_OUT )
      IF ( IND_REP_OUT < 1 .AND. ILEN(SIMUL%CNF%REPO_OUT) >  0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL ERR_LOG ( 1414, IUER, 'SIMUL_DO', 'Output GVF repository '// &
     &          TRIM(SIMUL%CNF%REPO_OUT)//' is not defined in the VCAT configuration '// &
     &         'file '//SIMUL%CNF%VCAT_CONF )
           RETURN 
      END IF
!
! --- Initialization of NERS structures, reading and parsing NERS configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_INIT ( SIMUL%CNF%NERS_CONF, NERS, -1.0D0, -1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1415, IUER, 'SIMUL_DO', 'Error in initializing '// &
     &         'NERS data structure' )
           RETURN 
      END IF
!
! --- Initialize VTD
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1416, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &         'to initialize VTD object' )
           RETURN 
      END IF
!
! --- Read and parse configuration file
! 
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( SIMUL%CNF%VTD_CONF, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1417, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &         'to read configuration file '//SIMUL%CNF%VTD_CONF )
           RETURN 
      END IF
      IF ( IVRB .GE. 5 ) THEN
           VTD%CONF%IVRB = IVRB
         ELSE
           VTD%CONF%IVRB = 0
      END IF
!
      IF ( FMT_IN == '-i-vex' ) THEN
!
! -------- Parse VLBI scheduile in vex format
!
           CALL ERR_PASS ( IUER, IER )
           CALL VEX_PARSER ( VEX, FIL_IN, MIN(2,IVRB), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1418, IUER, 'SIMUL_DO', 'Error in parsing '// &
     &              'vex files '//FIL_IN )
                RETURN 
           END IF
!
! -------- Generate the simulation database from the parsed vex file schedle
!
           CALL ERR_PASS ( IUER, IER )
           CALL VEX_TO_SIMUL ( VEX, VTD, NERS, SIMUL, LT, RENAME_TAB, &
     &                         IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1419, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &              'to generate the simulated VLBI dataset' )
                RETURN 
           END IF
           SIMUL%ITYP = SIM__VEX
        ELSE IF ( FMT_IN == '-i-gvf' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VCAT_RESOLVE_DBNAME ( VCAT, FIL_IN, SIMUL%CNF%REPO_IN, &
     &                                FIL_ENV, M_FIL, L_FIL, DB_FILE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1420, IUER, 'SIMUL_DO', 'Cannot resolve '// &
     &              'input database name '//TRIM(FIL_IN)//' from '// &
     &               SIMUL%CNF%REPO_IN//' input repository' )
                RETURN 
           END IF
!
! -------- Read input database file in GVF format
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_READ_DB ( FIL_ENV, VCAT%GVF_DB_DIR(IND_REP_IN), GVH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1421, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &              'to read in inpuit VLBI database '//TRIM(FIL_IN)// &
     &              ' from '//SIMUL%CNF%REPO_IN//' repository' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_TO_SIMUL ( GVH, VTD, NERS, SIMUL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1422, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &              'to transform GVF object to SIMUL' )
                RETURN
           END IF
           SIMUL%ITYP = SIM__GVF
        ELSE IF ( FMT_IN == '-i-vda' ) THEN
!
! -------- Read input database file in GVF format
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_READ_DB ( FIL_IN, '-vda', GVH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1423, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &              'to read in inpuit VLBI database '//FIL_IN )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_TO_SIMUL ( GVH, VTD, NERS, SIMUL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1424, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &              'to transform GVF object to SIMUL' )
                RETURN
           END IF
           SIMUL%ITYP = SIM__VDA
      END IF
!
      IF ( ILEN(SIMUL%CNF%STP_DIR) > 0 ) THEN
           DIR_DESC = FUNC_OPENDIR ( TRIM(SIMUL%CNF%STP_DIR)//CHAR(0) )
           IF ( DIR_DESC .EQ. 0 ) THEN
                CALL ERR_LOG ( 1425, IUER, 'SIMUL_DO', 'Station '// &
     &              'parameter file directory '//TRIM(SIMUL%CNF%STP_DIR)// &
     &              ' specified in the simulation configuration file '// &
     &              TRIM(SIMUL%CNF%CONF_FILE)//' was not found' )
                RETURN
              ELSE
                IP = CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
      END IF
!
! --- Parse Station Parameter files
!
      CALL ERR_PASS ( IUER, IER )
      CALL STP_DIR_PARSER ( SIMUL%CNF%STP_DIR, STP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1426, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &         'to parse STP object' )
           RETURN
      END IF
!
      IF ( SIMUL%CNF%RH_MODE == 'cov' .OR. SIMUL%CNF%RH_MODE == 'covest' ) THEN
!
! -------- Parse file with covariances of the zenith path delay
!
           CALL ERR_PASS ( IUER, IER )
           CALL SIMUL_PARSE_COVZEN ( SIMUL, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1427, IUER, 'SIMUL_DO', 'Error in an '// &
     &              'attempt to compute right hand sites of the output dataset' )
                RETURN
           END IF
      END IF
!
! --- Create simulation right hand side
!
      CALL ERR_PASS ( IUER, IER )
      CALL SIMUL_RH ( SIMUL, VEX, STP, VTD, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1428, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &         'to compute right hand sites of the output dataset' )
           RETURN
      END IF
      IF ( FMT_IN == '-i-gvf' .OR. FMT_IN == '-i-vda' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_RELEASE ( GVH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1429, IUER, 'SIMUL_DO', 'Error in an attempt '// &
     &              'to release dynamic memory allocated by input GVH' )
                RETURN
           END IF
      END IF
!
! --- Transform the simulation to the the GVF database
!
      CALL ERR_PASS ( IUER, IER )
      CALL SIMUL_TO_GVF ( SIMUL, GVH, NERS, VCAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1430, IUER, 'SIMUL_DO', 'Error in attempt '// &
     &         'to convert the simulated VLBI dataset in to the GVF database' )
           RETURN
      END IF
!
      IF ( FMT_OUT == '-o-vda' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_WRI_DB ( 'a', GVH, VCAT, SIMUL%CNF%REPO_OUT, FIL_OUT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1431, IUER, 'SIMUL_DO', 'Error in '// &
     &              'an attempt to write the simulation database in '// &
     &              'plain ascii VDA format to the output file '//FIL_OUT )
                RETURN 
           END IF
        ELSE IF ( FMT_OUT == '-o-gvf' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_WRI_DB ( 'b', GVH, VCAT, SIMUL%CNF%REPO_OUT, FIL_OUT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1432, IUER, 'SIMUL_DO', 'Error in '// &
     &              'an attempt to write the simulation database '// &
     &               TRIM(FIL_OUT)//' in binary GVF format' )
                RETURN
           END IF
      ENDIF
      WRITE ( 6, '(A)' ) 'Written database file '//TRIM(GVH%FILEENV)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  SIMUL_DO  !#!#
