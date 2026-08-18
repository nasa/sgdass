#include <mk5_preprocessor_directives.inc>
      PROGRAM ANC_SCAV_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  ANC_SCAV_MAIN
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 11-SEP-2025  ANC_SCAV_MAIN  v1.2 (d)  L. Petrov  22-SEP-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      INCLUDE    'ners.i'
      INCLUDE    'ners_local.i'
      TYPE       ( ANC__TYP   ) :: ANC      
      TYPE       ( NERS__TYPE ) :: NERS
      CHARACTER  STR*128
      INTEGER*8  STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE
      CHARACTER  FILANC*256, FILSCAV*256, NERS_CONFIG*128
      CHARACTER  C_PRV(ANC__MPRV)*(256), FILSDE*256
      INTEGER*4  IUER, IER, L_PRV, IV_PRV, IVRB, LN
      REAL*8     TS1, TS2, TS3, TF1, TF2, TF3
      REAL*8     TIM_DIF_MAX
      CHARACTER  CH_DT*8, CH_IVRB, CHA*4
      CHARACTER  ANC_SCAV__LABEL*17
      PARAMETER  ( ANC_SCAV__LABEL = 'anc_scav 20251120' )
      LOGICAL*1  LEX
      INTEGER*4, EXTERNAL :: I_LEN
      CHARACTER  CDATE*8, CTIME*10, CZONE*5
      INTEGER*4  IVALUES(8)
!     
      CALL CLRCH ( FILANC  )
      CALL CLRCH ( FILSCAV )
      CH_IVRB = '1'
      IVRB = 1
      IF ( IARGC() .LT. 4 ) THEN
           WRITE (6, '(A)') 'Usage: anc_scav  anc_file|bnc_file '//     &
     &                      'sde_source_file '//                        &
     &                      'time_dif_max '//                           &
     &                      'scav_file '//                              &
     &                      '[verbosity]'
           CALL EXIT ( 1 )
        ELSE
!
! ------ Get the original anc or binary file
!
         CALL GETARG ( 1, FILANC )
         INQUIRE     ( FILE=FILANC, EXIST=LEX )
         IF ( .NOT. LEX ) THEN ! 
            IUER = -1
            CALL ERR_LOG ( 5001, IUER, 'ANC_SCAV_MAIN',                 &
     &              'Cannot find anc file '//TRIM(FILANC) )
            CALL EXIT ( 1 )
         END IF ! lex
!
! ------ Get the file with SDE sources
!
         CALL GETARG ( 2, FILSDE )
         INQUIRE     ( FILE=FILSDE, EXIST=LEX )
         IF ( .NOT. LEX ) THEN ! 
            IUER = -1
            CALL ERR_LOG ( 5004, IUER, 'ANC_SCAV_MAIN',                 &
     &              'Cannot find sde source list file '//TRIM(FILANC)// &
     &              ' try atp_YYYYMMDD/share/sde_source_names.txt' )
            CALL EXIT ( 1 )
         END IF ! lex
!     
! ------ Get the maximum time difference
!
         CALL CLRCH  ( CH_DT )
         CALL GETARG ( 3, CH_DT )
!
! ------ Check if the number has a decimal point?
!
         READ (UNIT=CH_DT, FMT='(F8.2)', IOSTAT=IER) TIM_DIF_MAX
         IF ( IER .NE. 0 ) THEN
            IUER = -1
            CALL ERR_LOG ( 5002, IUER, 'ANC_SCAV_MAIN',                 &
     &              'Expected real number for scan diff. '//CH_DT )
         END IF
!
! ------ Get the output scan average file.
!         
         CALL GETARG ( 4, FILSCAV )
!
! ------ Verbosity
!
         IF ( IARGC() .GE. 5 ) THEN
              CALL CLRCH  ( CH_IVRB )
              CALL GETARG ( 5, CH_IVRB )
              CALL CHIN ( CH_IVRB, IVRB )
         END IF
      END IF
!
! --- Get the date this was run
!
      CALL DATE_AND_TIME ( DATE = CDATE, TIME   = CTIME,                  &
     &                     ZONE = CZONE, VALUES = IVALUES )      

!
      IF ( IVRB .GT. 2 )  THEN
         CALL CPU_TIME (TS1 )
         WRITE( 6, '(A)') 'Parsing anc file '//TRIM(FILANC)
      END IF
!     
! --- check if this an ascii or binary file
!
      LN  = I_LEN ( FILANC )
      CHA = FILANC(LN-3:LN)
      IF ( (CHA == '.bnc') .OR. (CHA == '.bts') ) THEN
         IUER = -1
         CALL BNC_PARSE (FILANC, ANC, IUER)
!
      ELSEIF ( (CHA == '.anc') .OR. (CHA == '.ant') ) THEN
!
! ------ Initialization of NERS structures, reading and
!        parsing NERS configuration file
!
         IUER = -1
         CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
         IF ( IUER .NE. 0 ) THEN
            IUER = -1
            CALL ERR_LOG ( 5002, IUER, 'ANC_SCAV_MAIN',                 &
     &                 'Error in initializing NERS data structure' )
            CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL ANC_PARSE ( FILANC, ANC, NERS, IUER )
      ELSE
         IUER = -1
         CALL ERR_LOG ( 5003, IUER, 'ANC_SCAV_MAIN',                    &
     &           'Expected '//TRIM(FILANC)//' to end in '//             &
     &           '.anc, ant, .bnc or .bts' )
         CALL EXIT(1)
      END IF         

      IF ( IVRB .GT. 2 ) THEN
         CALL CPU_TIME (TF1 )
         WRITE( 6, 1001 ) TF1 - TS1
         WRITE( 6, '(A)') 'Computing scan averages'
      END IF
!
! --- generate the provenance block for this
!
      CALL CLRCH ( C_PRV )
      C_PRV(1)  = 'GENERATOR:      '//ANC_SCAV__LABEL
      C_PRV(2)  = 'COMMAND_LINE:   anc_scav '//           &
     &                                           TRIM(FILANC)//' '//    &
     &                                           TRIM(CH_DT)//' '//     &
     &                                           TRIM(FILSCAV)//' '//   &
     &                                           TRIM(CH_IVRB)
      C_PRV(3)  = 'UPDATED_VARS:   TSYS TPI PCAL'
      C_PRV(4)  = 'CREATION_DATE:  20'//                  &
     &                                           CDATE(3:4)//'.'//      &
     &                                           CDATE(5:6)//'.'//      &
     &                                           CDATE(7:8)//'-'//      &
     &                                           CTIME(1:2)//':'//      &
     &                                           CTIME(3:4)//':'//      &
     &                                           CTIME(5:6)
      C_PRV(5)  = 'NUM_FILES:      1 '
      C_PRV(6)  = 'NUM_COMMENTS:   1 '
      C_PRV(7)  = 'COMMENTS:       Filtered and averaged for scans'
!
! ------ Update the Experiment type
!
      IUER = -1
      CALL ANC_TYPE (FILSDE, ANC, IUER)
!
      IF ( IVRB .GT. 1 ) THEN
         WRITE( 6, '(A)') ANC%EXP_CODE//' is a '//ANC%EXP_TYPE//        &
     &                    ' experiment'
      END IF
!     
! --- Compute the scan average file
!
      L_PRV = 7
      IV_PRV = 2
      CALL ANC_SCAV ( ANC, TIM_DIF_MAX, L_PRV, C_PRV, FILSCAV, IUER )
!
      IF ( IVRB .GT. 2 ) THEN
         CALL CPU_TIME ( TF2 )
         WRITE(6,1001) TF2 - TF1
         WRITE( 6, '(A)') 'Writting scan average file '//TRIM(FILSCAV)
      END IF
!     
! --- Write updated anc file to file
!
      IV_PRV = 2
      IUER = -1
      ANC%NUM_PRV = ANC%NUM_PRV + 1
      CALL ANC_WRITE ( ANC, IV_PRV, L_PRV, C_PRV, NERS, FILSCAV, IUER )
! ---
      IF ( IVRB .GT. 2 ) THEN
         CALL CPU_TIME ( TF3 )
         WRITE( 6, 1001 ) TF3 - TF2
      END IF
! ---
 1001 FORMAT ( F16.2, ' seconds' )
!
      END PROGRAM
