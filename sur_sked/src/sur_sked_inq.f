      SUBROUTINE SUR_SKED_INQ ( QUE, ANS )
! ************************************************************************
! *                                                                      *
! *   Rouytine SUR_SKED_INQ
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 23-DEC-2023    SUR_SKED_INQ    v1.1 (d)  L. Petrov  15-FEB-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'sur_sked_local.i'
      CHARACTER  QUE*(*), ANS*(*)
      REAL*8     MAT(3,3)
!
      IF ( QUE == 'version'   .OR. &
     &     QUE == '--version'      ) THEN
           ANS = SUR_SKED__VERSION
        ELSE IF ( QUE == 'prefix'    .OR. &
     &            QUE == '--prefix'  .OR. &
     &            QUE == 'sur_sked_prefix'     ) THEN
           ANS = SUR_SKED__PREFIX
        ELSE IF ( QUE == 'bindir'    .OR. &
     &            QUE == '--bindir'  .OR. &
     &            QUE == 'sur_sked_bindir'     ) THEN
           ANS = SUR_SKED__PREFIX//'/bin'
        ELSE IF ( QUE == 'root'    .OR. &
     &            QUE == '--root'  .OR. &
     &            QUE == 'sur_sked_root'     ) THEN
           ANS = SUR_SKED__ROOT
        ELSE IF ( QUE == 'data'    .OR. &
     &            QUE == '--data'  .OR. &
     &            QUE == 'sur_sked_data'     ) THEN
           ANS = SUR_SKED__PREFIX//'/share'
        ELSE IF ( QUE == 'doc'    .OR. &
     &            QUE == '--doc'  .OR. &
     &            QUE == 'sur_sked_doc'     ) THEN
           ANS = SUR_SKED__PREFIX//'/doc'
        ELSE
           ANS = 'Unsupported argument '//TRIM(QUE)//'-- Supported arguments: --version, --prefix, --bindir, --root, --data, --doc'
      END IF
      CALL VTD_ROTMAT ( 1, 1.0D0, MAT )
!
      RETURN
      END SUBROUTINE  SUR_SKED_INQ  !#!#
