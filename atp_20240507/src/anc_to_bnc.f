#include <mk5_preprocessor_directives.inc>
      PROGRAM    ANC_TO_BNC
! ************************************************************************
! *                                                                      *
! *   Program  ANC_TO_BNC
! *                                                                      *
! *  ### 09-MAY-2021   ANC_TO_BNC  v1.2 (c)  L. Petrov  03-AUG-2021 ###  *
! *                                                                      *
! ************************************************************************
      INCLUDE   'atp.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      LOGICAL*1  LEX
      CHARACTER  NERS_CONFIG*128
      CHARACTER  FILANC_OG*128, FILANC_AV*128, FILBNC*128
      INTEGER*4  IUER
!
! ---
!
      CALL CLRCH ( FILANC_OG )
      CALL CLRCH ( FILANC_AV )
      CALL CLRCH ( FILBNC )
! ---
      IF ( IARGC() == 3 ) THEN 
         CALL GETARG ( 1, FILANC_OG )
         CALL GETARG ( 2, FILANC_AV )
         CALL GETARG ( 3, FILBNC)
      ELSEIF ( IARGC() == 2 ) THEN
         CALL GETARG ( 1, FILANC_OG )
         CALL GETARG ( 2, FILBNC)
         FILANC_AV = 'UNDF'
      ELSE
         WRITE (6, '(A)') 'Usage: anc_to_bnc '//                        &
     &                       'anc_orig_file [anc_scav_file] bnc_file'
         CALL EXIT ( 1 )
      END IF
!
! --- Get NERS_CONFIG file
! --- First, check environment variable NERS_CONFIG
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( ILEN(NERS_CONFIG) == 0 ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', NERS_CONFIG )
         NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
         INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF
!
! --- Innitialization of NERS structures, reading andparsing NERS configuration file
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 4001, IUER, 'ANC_TO_BNC',                       &
     &           'Error in initializing NERS data structure' )
         CALL EXIT ( 1 )
      END IF
! ---
      write (6,'(A)') 'running ANC_PARSE {anc_to_bnc.f}'
! ---
      
      IUER = -1
      CALL ANC_PARSE ( FILANC_OG, FILANC_AV, ANC, NERS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
! ---
      write (6,'(A)') 'parsed anc file to derived type {anc_to_bnc.f}'
      write (6,'(A)') 'running BNC_WRITE {anc_to_bnc.f}'

      IUER = -1
      CALL BNC_WRITE ( ANC, FILBNC, IUER )
! ---
      WRITE ( 6, '(A)' ) 'Written output bnc file: '//TRIM(FILBNC)
      IF ( FILANC_AV == 'UNDF' ) THEN
         WRITE ( 6, '(A)' ) 'Written output anc scav file: UNDEFINED'
      ELSE
         WRITE ( 6, '(A)' ) 'Written output anc scav file: '//TRIM(FILANC_AV)
      END IF
!
      END  PROGRAM   ANC_TO_BNC  !#!#!
