      SUBROUTINE SUPMET_SHOW ( SUPMET, OUT )
! ************************************************************************
! *                                                                      *
! *   Routine  SUPMET_SHOW  generates output line with short description *
! *   of applied suppression method (which is kept in socom.i). The      *
! *   output line left-adjusted may have up to 16 symbols.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *         OUT ( CHARACTER ) -- Output line.                            *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  30-APR-98  SUPMET_SHOW   v1.3  (d) L. Petrov  05-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INTEGER*2  SUPMET
      CHARACTER  OUT*(*)
!
      IF ( SUPMET .EQ. SUPMET__PRE98 ) THEN
           OUT = 'SUPMET__PRE98   '
         ELSE IF ( SUPMET .EQ. SUPMET__PRE91  ) THEN
           OUT = 'SUPMET__PRE91 * '
         ELSE IF ( SUPMET .EQ. SUPMET__COMB1  ) THEN
           OUT = 'SUPMET__COMB1   '
         ELSE IF ( SUPMET .EQ. SUPMET__SNGBA  ) THEN
           OUT = 'SUPMET__SNGBA   '
         ELSE IF ( SUPMET .EQ. SUPMET__META   ) THEN
           OUT = 'SUPMET__META    '
        ELSE
           OUT = 'Undefined       '
      END IF
!
      RETURN
      END  !#!  SUPMET_SHOW  #!#
