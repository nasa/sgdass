      PROGRAM     SUR_SKEQ_INQ_MAIN
! ************************************************************************
! *                                                                      *
! *   Prgoram SUR_SKEQ_INQ_MAIN prints some parameters of SUR_SKED       *
! *   installation.                                                      *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 06-OCT-2025 SUR_SKEQ_INQ_MAIN v1.0 (d) L. Petrov 19-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE 
      INCLUDE    'sur_sked_local.i'
      CHARACTER  PAR*32, ANS*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: sur_sked_inq  --version|--prefix|--bindir|--root|--data|--doc'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
      CALL SUR_SKED_INQ ( PAR, ANS )
      WRITE ( 6, '(A)' ) TRIM(ANS)
!
      END PROGRAM  SUR_SKEQ_INQ_MAIN  !#!#
