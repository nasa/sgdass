      PROGRAM    SIMUL_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = SIMUL__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL SIMOBS_MAIN()
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SIMOBS_MAIN()
! ************************************************************************
! *                                                                      *
! *   Program SIMOBS_MAIN compares simulated and observed schedules      *
! *                                                                      *
! *  ### 14-DEC-2020  SIMOBS_MAIN  v1.0 (c)  L. Petrov  14-DEC-2020 ###  *
! *                                                                      *
! ************************************************************************
!
! simobs /s0/simul/vda/rv117_simul.vda /l2/gvf/obs/env/20160419_s_v002.env | tee /tmp/x.1
!
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      INCLUDE   'simul.i'
      TYPE     ( GVH__STRU   ) :: GVH_SIM,   GVH_OBS
      TYPE     ( SIMUL__TYPE ) :: SIMUL_SIM, SIMUL_OBS
      CHARACTER  FILSIM*128, FILOBS*128, GVF_DB_DIR*128, GVH_DB_OBS*128
      INTEGER*4  IL, ID, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: simobs sim_file obs_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILSIM )
           CALL GETARG ( 2, FILOBS )
      END IF
!
      IF ( ILEN(FILSIM) < 6 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3901, IUER, 'SIMOBS_MAIN', 'Simulation file name '// &
     &          TRIM(FILSIM)//' is too short' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(FILOBS) < 6 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3902, IUER, 'SIMOBS_MAIN', 'Observation file name '// &
     &          TRIM(FILOBS)//' is too short' )
           CALL EXIT ( 1 )
      END IF
      CALL GETENVAR ( 'GVH_DB_OBS', GVH_DB_OBS )
      IF ( ILEN(GVH_DB_OBS) == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3903, IUER, 'SIMOBS_MAIN', 'To run simul, you need define '// &
     &         'enviroment variable GVH_DB_OBS with the name of the binary data directory '// &
     &         'of the database files in gvf format' )
           CALL EXIT ( 1 )
      END IF 
!
      IL = ILEN(FILSIM)
      ID = LINDEX ( FILSIM, '/' ) 
      IF ( FILSIM(IL-3:IL) == '.vda' ) THEN
           IUER = -1
           CALL GVH_READ_DB ( FILSIM, 'vda', GVH_SIM, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 3904, IUER, 'SIMOBS_MAIN', 'Error in an attempt '// &
     &              'to read simulation database '//FILSIM )
                CALL EXIT ( 1 )
           END IF
         ELSE IF ( FILSIM(IL-3:IL) == '.env' ) THEN
           IUER = -1
           CALL GVH_READ_DB ( FILSIM, FILSIM(1:ID+1), GVH_SIM, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 3905, IUER, 'SIMOBS_MAIN', 'Error in an attempt '// &
     &              'to read simulation database '//FILSIM )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IL = ILEN(FILOBS)
      ID = LINDEX ( FILOBS, '/' ) 
      IF ( FILOBS(IL-3:IL) == '.vda' ) THEN
           IUER = -1
           CALL GVH_READ_DB ( FILOBS, 'vda', GVH_OBS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 3906, IUER, 'SIMOBS_MAIN', 'Error in an attempt '// &
     &              'to read simulation database '//FILOBS )
                CALL EXIT ( 1 )
           END IF
         ELSE IF ( FILOBS(IL-3:IL) == '.env' ) THEN
           IUER = -1
           CALL GVH_READ_DB ( FILOBS, GVH_DB_OBS, GVH_OBS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 3907, IUER, 'SIMOBS_MAIN', 'Error in an attempt '// &
     &              'to read simulation database '//FILOBS )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL SIMOBS_GET ( GVH_SIM, SIMUL_SIM, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3908, IUER, 'SIMOBS_MAIN', 'Error in procesing simulated '// &
     &         'database' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL SIMOBS_GET ( GVH_OBS, SIMUL_OBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3909, IUER, 'SIMOBS_MAIN', 'Error in procesing observation '// &
     &         'database' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL SIMOBS_COMP ( SIMUL_OBS, SIMUL_SIM, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3910, IUER, 'SIMOBS_MAIN', 'Error in comparision of '// &
     &         'the simulated and observed databases' )
           CALL EXIT ( 1 )
      END IF
!
      END  SUBROUTINE  SIMOBS_MAIN  !#!#
