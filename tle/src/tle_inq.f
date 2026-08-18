      SUBROUTINE TLE_INQ ( MODE, ANS )
! ************************************************************************
! *                                                                      *
! *   Proram TLE_INQ returns intermal parameters of TLE instalation,     *
! *   such as prefix, root, bin.                                         *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 15-FEB-2025     TLE_INQ   v1.0 (d)  L. Petrov  15-FEB-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'tle_local.i'
      INCLUDE   'tle_sgp4.i'
      CHARACTER  MODE*(*), ANS*(*)
      REAL*8     VAL
      REAL*8,    EXTERNAL :: TLE_ASINH
!
      IF ( MODE == "root" .OR. MODE == "--root" ) THEN
           ANS = TLE__ROOT
        ELSE IF ( MODE == 'prefix' .OR. MODE == '--prefix' ) THEN
           ANS = TLE__PREFIX
        ELSE IF ( MODE == 'version' .OR. MODE == '--version' ) THEN
           ANS = TLE__VERSION
        ELSE IF ( MODE == 'bin' .OR. MODE == '--bin' ) THEN
           ANS = TLE__PREFIX//'/bin'
        ELSE
           ANS = 'Wrong argument '//TRIM(MODE)//' -- supported modes: '// &
     &           'root prefix bin'
      END IF
      VAL = TLE_ASINH ( 1.0D0 )
!
      RETURN
      END  SUBROUTINE  TLE_INQ  !#!#
