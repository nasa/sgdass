        SUBROUTINE DECPOL ( N, S, RD, ALPHA, DELTA, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  DECPOL transforms 3-dimensional vector S from Decart     *
! *   coordinates to spherical coordinates: R -- radius vector, ALPHA    *
! *   -- right ascension, DELTA -- declination.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      N ( INTEGERE*4 ) -- Dimsnesion of vector S. (Should be three).  *
! *      S ( REAL*8     ) -- Three-dimensional vector in Decart          *
! *                          coordinates.                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      R ( REAL*8     ) -- Length of radius-vector.                    *
! *  ALPHA ( REAL*8     ) -- Right ascesion (in rad).                    *
! *  DELTA ( REAL*8     ) -- Declination (in rad).                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 18-APR-1991     DECPOL    v1.1 (d)  L. Petrov  15-APR-2025 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  N, IUER
        REAL*8     S(N), ALPHA, DELTA, PI, DARSIN, RD, SS(3), SD, EPS
        PARAMETER ( PI=3.141592653589793D0 )
        PARAMETER ( EPS = 1.D-15 )
!
        RD = DSQRT ( S(1)**2 + S(2)**2 + S(3)**2 )
        IF ( RD < EPS ) THEN
             CALL ERR_LOG ( 5811, IUER, 'DECPOL', 'Input vector '// &
     &           'has zero norm' )
             RETURN 
        END IF
!
        SS = S/RD
        IF ( SS(3) > 1.0D0 - EPS ) THEN
             DELTA = PI/2.0D0
           ELSE IF ( SS(3) < -1.0D0 + EPS ) THEN
             DELTA = -PI/2.0D0
           ELSE
             DELTA=DASIN ( SS(3) )
        END IF
!
        SD =  SS(2)/( DSQRT ( 1.D0 - SS(3)*SS(3) ) ) 
        IF ( SD > 1.0D0 - EPS ) THEN
             ALPHA =  PI/2.0D0
          ELSE IF ( SD < -1.0D0 + EPS ) THEN
             ALPHA = -PI/2.0D0
          ELSE
             ALPHA = DASIN ( SD )
        END IF
!
        IF ( SS(1) .LT. 0.D0 ) ALPHA = PI - ALPHA
        IF ( ALPHA .LT. 0.D0 ) ALPHA = 2.D0*PI + ALPHA
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  SUBROUTINE  DECPOL !#!#
