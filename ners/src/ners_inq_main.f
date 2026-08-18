      PROGRAM NERS_INQ_MAIN
! ************************************************************************
! *                                                                      *
! *   Routine NERS_INQ_MAIN returns information about package ners       *
! *   (Network Earth Rotation Serivce).                                  *
! *                                                                      *
! *   Usage: ners_inq  query                                             *
! *          where query is one of                                       *
! *                                                                      *
! *          --version     -- Version of NERS library.                   *
! *          --prefix      -- Directlry where NERS is installed.         *
! *          --include     -- Directory with NERS include files.         *
! *          --libdir      -- Directory that contains NERS library.      *
! *          --bindir      -- Directory that contains NERS binary        *
! *                           executatble files.                         *
! *          --root        -- Root directroy with NERS distribution.     *
! *          --share       -- Directory that contains NERS data.         *
! *          --script      -- Directory that contains NERS python script.*
! *          --doc         -- Directory that contains NERS documentation.*
! *          --config      -- File name with default NERS configuration. *
! *          --standalone  -- string whether NERS was conifgured in      *
! *                           a standalone mode (YES or NO)              *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ###  08-AUG-2025  NERS_INQ_MAIN v1.0 (d)  L. Petrov  08-AUG-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners_local.i'
      CHARACTER  QUE*32, ANS*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: ners_inq  --version|--prefix|--include|--libdir|--bindir|--root|--share|--script|--config|--standalone|--doc'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, QUE )
      END IF
!
      IF ( QUE == 'version'   .OR. &
     &     QUE == '--version'      ) THEN
           ANS = NERS__VERSION
        ELSE IF ( QUE == 'prefix'    .OR. &
     &            QUE == '--prefix'  .OR. &
     &            QUE == 'ners_prefix'     ) THEN
           ANS = NERS__PREFIX
        ELSE IF ( QUE == 'bindir'    .OR. &
     &            QUE == '--bindir'  .OR. &
     &            QUE == 'ners_bindir'     ) THEN
           ANS = NERS__PREFIX//'/bin'
        ELSE IF ( QUE == 'libdir'    .OR. &
     &            QUE == '--libdir'  .OR. &
     &            QUE == 'ners_libdir'     ) THEN
           ANS = NERS__PREFIX//'/lib'
        ELSE IF ( QUE == 'include'    .OR. &
     &            QUE == '--include'  .OR. &
     &            QUE == 'ners_include'     ) THEN
           ANS = NERS__PREFIX//'/include'
        ELSE IF ( QUE == 'root'    .OR. &
     &            QUE == '--root'  .OR. &
     &            QUE == 'ners_root'     ) THEN
           ANS = NERS__ROOT
        ELSE IF ( QUE == 'share'    .OR. &
     &            QUE == '--share'  .OR. &
     &            QUE == 'ners_share'     ) THEN
           ANS = NERS__PREFIX//'/share'
        ELSE IF ( QUE == 'scrpit'    .OR. &
     &            QUE == '--script'  .OR. &
     &            QUE == 'ners_script'    ) THEN
           ANS = NERS__PREFIX//'/script'
        ELSE IF ( QUE == 'config'    .OR. &
     &            QUE == '--config'  .OR. &
     &            QUE == 'ners_config'     ) THEN
           ANS = NERS__CONFIG
        ELSE IF ( QUE == 'standalone'    .OR. &
     &            QUE == '--standalone'  .OR. &
     &            QUE == 'ners_standalone'     ) THEN
           ANS = NERS__STANDALONE
        ELSE IF ( QUE == 'doc'    .OR. &
     &            QUE == '--doc'  .OR. &
     &            QUE == 'ners_doc'     ) THEN
           ANS = NERS__PREFIX//'/doc'
        ELSE
           ANS = 'Unsupported argument '//TRIM(QUE)//'-- Supported arguments: --version, '// &
     &           '--prefix, --include, --libdir, --bindir, --root, --share, --script, --config, --standalone, --doc'
      END IF
      WRITE ( 6, '(A)' ) TRIM(ANS)
!
      RETURN
      END  PROGRAM  NERS_INQ_MAIN  !#!#
