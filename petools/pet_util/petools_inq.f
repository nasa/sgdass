      PROGRAM    PETOOLS_INQ_MAIN
! ************************************************************************
! *                                                                      *
! *   Program PETOOLS_INQ prints some parameters of petools library      *
! *   installtion.                                                       *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 15-MAY-2026  PETOOLS_INQ v1.0 (d)  L. Petrov  15-MAY-2026  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  PAR*128, ANS*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: petools_inq  --version|--label|--prefix|--bindir|--root|--doc'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
      ANS = '???'
      CALL PETOOLS_INQ ( PAR, ANS )
      IF ( ANS == '???' ) THEN
           WRITE ( 6, '(A)' ) 'Unsupported argument '//TRIM(PAR)
           WRITE ( 6, '(A)' ) 'Supported arguments: --version, --label, --prefix, --bindir, --root, --doc'
         ELSE
           WRITE ( 6, '(A)' ) TRIM(ANS)
      END IF
      END  PROGRAM  PETOOLS_INQ_MAIN  !#!  
