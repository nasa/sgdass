#include <mk5_preprocessor_directives.inc>
      PROGRAM    BDS_UTIL_MAIN
! ************************************************************************
! *                                                                      *
! *   Program BDS_UTIL
! *                                                                      *
! *  ### 28-NOV-2024 BDS_UTIL_MAIN v1.0 (c)  L. Petrov  28-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      CHARACTER  FIL_BDS*128, MODE*128, PAR*128
      LOGICAL*1  LEX, ISDIR
      INTEGER*8  DIR_DESC
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: CLOSEDIR
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4  IUER
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: bspd_util extract|extend|extend_in bspd_file param'
           CALL  EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, MODE     )
           CALL GETARG ( 2, FIL_BDS )
           CALL GETARG ( 3, PAR      )
      END IF
!
      IUER= -1
      IF ( MODE == 'check' ) THEN
            CALL BDS_CHECK ( FIL_BDS, IUER )
            IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE IF ( MODE == 'info' ) THEN
            IF ( IARGC() < 3 ) THEN
                 WRITE ( 6, * ) 'The third argument, mode, is required'
                 CALL EXIT ( 1 )
            END IF
            CALL BDS_INFO ( FIL_BDS, PAR, IUER )
         ELSE IF ( MODE == 'truncate' ) THEN
            IF ( IARGC() < 3 ) THEN
                 WRITE ( 6, * ) 'The third argument, truncation date, is required'
                 CALL EXIT ( 1 )
            END IF
            INQUIRE ( FILE=FIL_BDS, EXIST=LEX )
            IF ( .NOT. LEX ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 5401, IUER, 'BDS_UTIL', 'Input '// &
     &               'file/directory '//TRIM(FIL_BDS)//' does not exist' )
                 CALL EXIT ( 1 )
            END IF
!
            DIR_DESC = FUNC_OPENDIR ( TRIM(FIL_BDS)//CHAR(0) )
            IF ( DIR_DESC == 0 ) THEN
                 ISDIR =.FALSE.
               ELSE
                 ISDIR = .TRUE.
                 IS = CLOSEDIR ( %VAL(DIR_DESC) )
            END IF
            IF ( ISDIR ) THEN
                 CALL BDS_DIR_TRUNCATE ( FIL_BDS, PAR, IUER )
               ELSE 
                 CALL BDS_TRUNCATE ( FIL_BDS, PAR, IUER )
            END IF
            IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE IF ( MODE == 'extract' ) THEN
            IF ( IARGC() < 3 ) THEN
                 WRITE ( 6, * ) 'The third argument, date, is required'
                 CALL EXIT ( 1 )
            END IF
            CALL BDS_EXTRACT ( FIL_BDS, PAR, IUER )
         ELSE IF ( MODE == 'extend' ) THEN
            IF ( IARGC() < 3 ) THEN
                 WRITE ( 6, * ) 'The third argument, date, is required'
                 CALL EXIT ( 1 )
            END IF
            CALL BDS_EXTEND ( FIL_BDS, PAR, .FALSE., IUER )
         ELSE IF ( MODE == 'extend_in' ) THEN
            IF ( IARGC() < 3 ) THEN
                 WRITE ( 6, * ) 'The third argument, date, is required'
                 CALL EXIT ( 1 )
            END IF
            CONTINUE 
            CALL BDS_EXTEND ( FIL_BDS, PAR, .TRUE., IUER )
         ELSE IF ( MODE == 'summary' ) THEN
            CALL BDS_GEN_SUMMARY ( FIL_BDS, IUER )
         ELSE 
           CALL ERR_LOG ( 5402, IUER, 'BDS_UTIL', 'Unspported mode '// &
     &          TRIM(MODE)//' . Supported modes: check, info, truncate, '// &
     &         'extract, extend_in, extend, summary' )
           CALL EXIT ( 1 )
      END IF
      IF ( IUER .NE. 0 ) THEN
           CALL EXIT ( 1 )
        ELSE 
           CALL EXIT ( 0 )
      END IF
      END  PROGRAM  BDS_UTIL_MAIN  !#!#
