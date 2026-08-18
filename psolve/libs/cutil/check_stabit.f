      FUNCTION CHECK_STABIT ( ISTA )
! ************************************************************************
! *                                                                      *
! *   Routine CHECK_STABIT  checks bit fields of participation of the    *
! *   stations in the solutions.                                         *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  02-DEC-97   CHECK_STABIT  v1.1  (d) L. Petrov  03-FEB-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      LOGICAL*4  CHECK_STABIT, DATYP_INQ
      INTEGER*2  ISTA
      LOGICAL*2  KBIT
!
      CHECK_STABIT = .TRUE.
      IF ( ISTA .GE. 1            .AND. &
     &     ISTA .LE. MAX_ARC_STA  .AND. &
     &     .NOT. CGM_TYPE                ) THEN
!
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Check bit field STABIT_P for the phase delay solution
!
              IF ( .NOT. KBIT ( STABIT_P, ISTA ) ) CHECK_STABIT = .FALSE.
             ELSE
!
! ------------- Check bit field STABIT_G for the group delay solution
!
              IF ( .NOT. KBIT ( STABIT_G, ISTA ) ) CHECK_STABIT = .FALSE.
         END IF
      END IF
!
      RETURN
      END  !#!  CHECK_STABIT  #!#
