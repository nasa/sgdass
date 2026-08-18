        SUBROUTINE RS_TAT ( RAD, SEC )
! ************************************************************************
! *                                                                      *
! *     Routine RS_TAT transforms the angles from radians to seconds     *
! *   of time.                                                           *
! *                                                                      *
! * ______________________ Input parameters: ___________________________ *
! *                                                                      *
! *      RAD  (  REAL*8  )  --  Angles in radians.                       *
! *                                                                      *
! * _____________________ Output parameters: ___________________________ *
! *                                                                      *
! *      SEC  (  REAL*8  )  --  Angles in secnonds of time.              *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 30-JAN-1989      RS_TAT   v1.0 (d)  L. Petrov  11-SEP-2001 ###  *
! *                                                                      *
! ************************************************************************
        REAL*8 RAD, SEC, PI
        PI=3.141592653589793D0
!!        CALL VER$ARG ( 2 )
        SEC=RAD*43200.D0/PI
        RETURN
        END  !#!  RS_TAT  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SR_TAT ( SEC, RAD )
! ************************************************************************
! *                                                                      *
! *     Routine RS_TAT transforms the angles from seconds of time to     *
! *   radians.                                                           *
! *                                                                      *
! * ______________________ Input parameters: ___________________________ *
! *                                                                      *
! *      SEC  (  REAL*8  )  --  Angles in secnonds of time.              *
! *                                                                      *
! * _____________________ Output parameters: ___________________________ *
! *                                                                      *
! *      RAD  (  REAL*8  )  --  Angles in radians.                       *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 30-JAN-1989      SR_TAT   v1.0 (d)  L. Petrov  11-SEP-2001 ###  *
! *                                                                      *
! ************************************************************************
        REAL*8 RAD, SEC, PI
        PI=3.141592653589793D0
!!        CALL VER$ARG ( 2 )
        RAD=SEC*PI/43200.D0
        RETURN
        END  !#!  SR_TAT  #!#
