      SUBROUTINE VTD_INQ ( QUE, ANS )
! ************************************************************************
! *                                                                      *
! *   Rouytine VTD_INQ
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 23-DEC-2023    VTD_INQ    v1.1 (d)  L. Petrov  15-FEB-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd_local.i'
      CHARACTER  QUE*(*), ANS*(*)
      REAL*8     MAT(3,3)
!
      IF ( QUE == 'version'   .OR. &
     &     QUE == '--version'      ) THEN
           ANS = VTD__VERSION
        ELSE IF ( QUE == 'prefix'    .OR. &
     &            QUE == '--prefix'  .OR. &
     &            QUE == 'vtd_prefix'     ) THEN
           ANS = VTD__PREFIX
        ELSE IF ( QUE == 'bindir'    .OR. &
     &            QUE == '--bindir'  .OR. &
     &            QUE == 'vtd_bindir'     ) THEN
           ANS = VTD__PREFIX//'/bin'
        ELSE IF ( QUE == 'root'    .OR. &
     &            QUE == '--root'  .OR. &
     &            QUE == 'vtd_root'     ) THEN
           ANS = VTD__ROOT
        ELSE IF ( QUE == 'data'    .OR. &
     &            QUE == '--data'  .OR. &
     &            QUE == 'vtd_data'     ) THEN
           ANS = VTD__DATA
        ELSE IF ( QUE == 'doc'    .OR. &
     &            QUE == '--doc'  .OR. &
     &            QUE == 'vtd_doc'     ) THEN
           ANS = VTD__DOC
        ELSE
           ANS = 'Unsupported argument '//TRIM(QUE)//'-- Supported arguments: --version, --prefix, --bindir, --root, --data, --doc'
      END IF
      CALL VTD_ROTMAT ( 1, 1.0D0, MAT )
!
      RETURN
      END SUBROUTINE  VTD_INQ  !#!#
