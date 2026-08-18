      SUBROUTINE ATP_INQ ( QUE, ANS )
! ************************************************************************
! *                                                                      *
! *   Routine ATP_INQ returns intermal parameters of TLE instalation,    *
! *   such as prefix, root, bin.                                         *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 15-FEB-2025     ATP_INQ   v1.0 (d)  L. Petrov  15-FEB-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'atp_local.i'
      CHARACTER  QUE*(*), ANS*(*)
      INTEGER*4  A1, A2, A3, IUER
!
      IF ( QUE == "root" .OR. QUE == "--root" ) THEN
           ANS = ATP__ROOT
        ELSE IF ( QUE == 'prefix' .OR. QUE == '--prefix' ) THEN
           ANS = ATP__PREFIX
        ELSE IF ( QUE == 'version' .OR. QUE == '--version' ) THEN
           ANS = ATP__VERSION
        ELSE IF ( QUE == 'bin' .OR. QUE == '--bin' ) THEN
           ANS = ATP__PREFIX//'/bin'
        ELSE
           ANS = 'Wrong argument '//TRIM(QUE)//' -- supported modes: '// &
     &           'root prefix bin version'
      END IF
      A1 = 1
      A2 = 2
      IUER = 0
      CALL ATP_ARRAY_COMPARE ( 1, 1, A1, A2, A3, IUER )
!
      RETURN
      END  SUBROUTINE  ATP_INQ  !#!#
