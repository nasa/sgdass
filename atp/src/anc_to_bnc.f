#include <mk5_preprocessor_directives.inc>
      PROGRAM    ANC_TO_BNC
! ************************************************************************
! *                                                                      *
! *   Program  ANC_TO_BNC
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 09-MAY-2021   ANC_TO_BNC  v1.2 (d)  L. Petrov  13-AUG-2025 ###  *
! *                                                                      *
! ************************************************************************
      INCLUDE   'atp.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      LOGICAL*1  LEX
      CHARACTER  NERS_CONFIG*128
      CHARACTER  FILANC*128, FILBNC*128
      INTEGER*4  IUER
!
! ---
!
      CALL CLRCH ( FILANC )
      CALL CLRCH ( FILBNC )
! ---
      IF ( IARGC() == 2 ) THEN
         CALL GETARG ( 1, FILANC )
         CALL GETARG ( 2, FILBNC)
      ELSE
         WRITE (6, '(A)') 'Usage: anc_to_bnc in_anc_file  out_bnc_file'
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
      CALL ANC_PARSE ( FILANC, ANC, NERS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
! ---
      write (6,'(A)') 'parsed anc file to derived type {anc_to_bnc.f}'
      write (6,'(A)') 'running BNC_WRITE {anc_to_bnc.f}'

      IUER = -1
      CALL BNC_WRITE ( ANC, FILBNC, IUER )
! ---
      WRITE ( 6, '(A)' ) 'Written output bnc file: '//TRIM(FILBNC)
!
      END  PROGRAM   ANC_TO_BNC  !#!#!
