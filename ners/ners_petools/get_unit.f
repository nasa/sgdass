      FUNCTION   GET_UNIT ()
! ************************************************************************
! *                                                                      *
! *   Function GET_UNIT  returns the Fortran input/output unit which     *
! *   is free (not used now).                                            *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 15-SEP-2000    GET_UNIT   v1.2 (d)  L. Petrov  04-DEC-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  GET_UNIT
      LOGICAL*4  FLAG
      INTEGER*4  J1
!
      GET_UNIT = -1
      DO 410 J1=41,127
         INQUIRE ( UNIT=J1, OPENED=FLAG )
         IF ( .NOT. FLAG ) THEN
              GET_UNIT = J1
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE 
      RETURN
      END  !#!  GET_UNIT  #!#
