      PROGRAM     VTD_INQ_MAIN
! ************************************************************************
! *                                                                      *
! *   Prgoram VTD_INQ_MAIN prints some parameters of VTD installation.   *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 23-DEC-2023    VTD_INQ    v1.1 (d)  L. Petrov  19-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE 
      INCLUDE    'vtd_local.i'
      CHARACTER  PAR*32, ANS*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: vtd_inq  --version|--prefix|--bindir|--root|--data|--doc'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
      CALL VTD_INQ ( PAR, ANS )
      WRITE ( 6, '(A)' ) TRIM(ANS)
!
      END PROGRAM  VTD_INQ_MAIN  !#!#
