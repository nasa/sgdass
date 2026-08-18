#include <mk5_preprocessor_directives.inc>
      PROGRAM    BPSD_UTIL
! ************************************************************************
! *                                                                      *
! *   Program BSPD_UTIL
! *                                                                      *
! *  ### 19-NOV-2024   BSPD_UTIL   v1.0 (c)  L. Petrov  19-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      CHARACTER  FIL_BSPD*128, MODE*128, PAR*128
      LOGICAL*1  LEX, ISDIR
      INTEGER*8  DIR_DESC
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: CLOSEDIR
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4  IUER
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: bspd_util check|truncate|extract|glue bspd_file|bspd_dir param'
           CALL  EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, MODE     )
           CALL GETARG ( 2, FIL_BSPD )
           CALL GETARG ( 3, PAR      )
      END IF
!
      IUER= -1
      IF ( MODE == 'check' ) THEN
           CALL BSPD_CHECK ( FIL_BSPD, 1, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE IF ( MODE == 'info' ) THEN
           CALL BSPD_INFO ( FIL_BSPD, PAR, IUER )
         ELSE IF ( MODE == 'truncate' ) THEN
           INQUIRE ( FILE=FIL_BSPD, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                IUER = -1
                CALL ERR_LOG ( 5401, IUER, 'BSPD_UTIL', 'Input '// &
     &              'file/directory '//TRIM(FIL_BSPD)//' does not exist' )
                CALL EXIT ( 1 )
           END IF
!
           DIR_DESC = FUNC_OPENDIR ( TRIM(FIL_BSPD)//CHAR(0) )
           IF ( DIR_DESC == 0 ) THEN
                ISDIR =.FALSE.
              ELSE
                ISDIR = .TRUE.
                IS = CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
           IF ( ISDIR ) THEN
                CALL BSPDIR_TRUNCATE ( FIL_BSPD, PAR, IUER )
              ELSE 
                CALL BSPD_TRUNCATE ( FIL_BSPD, PAR, IUER )
           END IF
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE IF ( MODE == 'extract' ) THEN
           CALL BSPD_EXTRACT ( FIL_BSPD, PAR, IUER )
         ELSE IF ( MODE == 'extend' ) THEN
           CALL BSPD_EXTEND ( FIL_BSPD, PAR, .FALSE., IUER )
         ELSE IF ( MODE == 'extend_in' ) THEN
           CALL BSPD_EXTEND ( FIL_BSPD, PAR, .TRUE., IUER )
         ELSE 
           CALL ERR_LOG ( 5402, IUER, 'BSPD_UTIL', 'Unspported mode '// &
     &          TRIM(MODE)//' . Supported modes: check, info, truncate, '// &
     &         'extract, extend_in, extend' )
           CALL EXIT ( 1 )
      END IF
      IF ( IUER .NE. 0 ) THEN
           CALL EXIT ( 1 )
        ELSE 
           CALL EXIT ( 0 )
      END IF
      END  PROGRAM  BPSD_UTIL  !#!#
