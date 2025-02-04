#include <mk5_preprocessor_directives.inc>
      PROGRAM    ANC_TO_BNC_SIM
! ************************************************************************
! *                                                                      *
! *   Program  ANC_TO_BNC_SIM
! *                                                                      *
! *  ### 09-MAY-2021   ANC_TO_BNC_SIM  v1.2 (c)  L. Petrov  03-AUG-2021 ###  *
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
      REAL*8     TS1, TF1, TS2, TF2
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
         WRITE (6, '(A)') 'Usage: anc_to_bnc_sim '//                    &
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
! --- Initialization of NERS structures, reading andparsing NERS configuration file
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 4001, IUER, 'ANC_TO_BNC_SIM',                   &
     &           'Error in initializing NERS data structure' )
         CALL EXIT ( 1 )
      END IF
! ---
      write (6,'(A)') 'running ANC_PARSE_SIM {anc_to_bnc_sim.f}'
! ---
      CALL CPU_TIME ( TS1 );
! ---      
      IUER = -1
      CALL ANC_PARSE_SIM ( FILANC_OG, FILANC_AV, ANC, NERS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )

      CALL CPU_TIME ( TF1 );
      CALL CPU_TIME ( TS2 )
! ---
      WRITE ( 6, 101   )  TRIM(FILANC_OG), TF1 - TS1       
      write ( 6, '(A)' ) 'running BNC_WRITE '
! ---
      IUER = -1
      CALL BNC_WRITE ( ANC, FILBNC, IUER )
! ---
      CALL CPU_TIME ( TF2 );
! ---
      WRITE ( 6, 102   )  TF2 - TS2
      WRITE ( 6, '(A)' ) 'Written output bnc file: '//TRIM(FILBNC)
! ---
      IF ( FILANC_AV == 'UNDF' ) THEN
         WRITE ( 6, '(A)' ) 'Written output anc scav file: UNDEFINED'
      ELSE
         WRITE ( 6, '(A)' ) 'Written output anc scav file: '//TRIM(FILANC_AV)
      END IF
!
! --- Run time for both write ups
!
      WRITE ( 6, '(A)' )  '---------'
! ---
 101  FORMAT ( A, ' parsed to ANC derived type in ', F25.10, ' sec' )
 102  FORMAT ( 'bnc_write runtime ', F25.10, ' sec' )
! ---
      END  PROGRAM   ANC_TO_BNC_SIM  !#!#!
