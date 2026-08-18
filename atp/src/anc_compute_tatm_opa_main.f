      PROGRAM ANC_COMPUTE_TATM_OPA_MAIN
! ************************************************************************
! *                                                                      *
! *   Program ANC_COMPUTE_TATM_OPA_MAIN
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * # 08-AUG-2025 ANC_COMPUTE_TATM_OPA_MAIN v1.0 (d) L. Petrov 15-AUG-2025 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      INCLUDE   'spd.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( ANC__TYP      ) :: ANC      
      TYPE     ( NERS__TYPE    ) :: NERS
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL
      CHARACTER  FILIN*128, DIRNAM*128, FILOUT*128, SPD_URL*128, &
     &           TMP_DIR*128, NERS_CONFIG*128, C_PRV(ANC__MPRV)*256, &
     &           STR*128, COM_LINE*1024
      LOGICAL*1  LEX
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*4  IVRB, L_PRV, IUER, IER, IV_PRV
      REAL*8     TS1, TF1
!     
! anc_compute_tatm_opa /anc/vlbi/scav/vo5204gs_scav.anc /tmp/1.anc https://atmospheric-propagation.smce.nasa.gov/spd/asc/geosit /tmp 3
!
      IVRB = 1
      CALL GET_COMMAND ( COM_LINE )
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: anc_compute_tatm_opa '//            &
     &                               'filin '//                           &
     &                               'filout '//                          &
     &                               'spd_url '//                         &
     &                               'tmp_dir '//                         &
     &                               '[ivrb]'
           CALL EXIT ( 1 )
        ELSE
           CALL GETARG ( 1, FILIN   )
           CALL GETARG ( 2, FILOUT  )
           CALL GETARG ( 3, SPD_URL )
           CALL GETARG ( 4, TMP_DIR )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                CALL CHIN ( STR, IVRB )
           END IF
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 5701, IUER, 'ANC_COMPUTE_TATM_OPA_MAIN',        &
     &             'Cannot find input file with ancilliary '//            &
     &             'telemetry data '//TRIM(FILIN) )
           CALL EXIT ( 1 )
      END IF
!
      IF ( .NOT. IS_DIR_EXIST ( TMP_DIR, STR ) ) THEN
           IUER = -1
           CALL ERR_LOG ( 5702, IUER, 'ANC_COMPUTE_TATM_OPA_MAIN',        &
     &             'Cannot find output temporary directry with slant '//  &
     &             'path delay data '//TRIM(TMP_DIR)//' -- '//STR )
           CALL EXIT ( 1 )
      END IF
!
! --- Get NERS_CONFIG file
! --- First, check environment variable NERS_CONFIG
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( ILEN(NERS_CONFIG) == 0 ) THEN
!
! -------- Second, check $HOME/.ners_config file
!
           CALL GETENVAR ( 'HOME', NERS_CONFIG )
           NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
           INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
!
! ----------- Third, check for the system-wide ners configuration file 
!
              NERS_CONFIG = NERS__CONFIG
           END IF
      END IF
!
! --- Initialization of NERS structures, reading and parsing NERS configuration file
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4201, IUER, 'ANC_COMPUTE_TATM_OPA_MAIN',        &
     &             'Error in initializing NERS data structure' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IVRB > 2 ) THEN
           CALL CPU_TIME ( TS1 )
      END IF
!
! --- Compute the opacity and atmospheric brighntess temperature
!
      IUER = -1
      CALL ANC_COMPUTE_TATM_OPA (FILIN, FILOUT, SPD_URL, TMP_DIR, IVRB,  &
     &                          ANC, NERS, SPD_DEL, L_PRV, C_PRV, IUER)
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4202, IUER, 'ANC_COMPUTE_TATM_OPA_MAIN',        &
     &             'Error in an attempt to inserve atmospheric '//        &
     &             'opacity and brightness temperature' )
             CALL EXIT ( 1 )
      END IF
!
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'ANC%NUM_PRV = ' , ANC%NUM_PRV
      END IF
      IF ( ANC%NUM_PRV == 1 ) THEN
           IV_PRV = 2
         ELSE 
           IV_PRV = 3
      END IF
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'IV_PRV= ', IV_PRV
      END IF
!
      IUER = -1
      CALL ANC_WRITE ( ANC, IV_PRV, L_PRV, C_PRV, NERS, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4203, IUER, 'ANC_COMPUTE_TATM_OPA_MAIN',        &
     &         'Error in writing output ancialliary telemtry file '// &
     &          FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IVRB > 2 ) THEN
           CALL CPU_TIME ( TF1 )
           WRITE( 6, 1001 ) TS1-TF1
 1001      FORMAT ( F12.5, ' sec to compute TATM ' )
      END IF
      END  PROGRAM  ANC_COMPUTE_TATM_OPA_MAIN !#!#
