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
! *  ### 18-APR-1991     DECPOL    v1.0 (c)  L. Petrov  18-APR-1991 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  N, IER, IUER
        REAL*8     S(N), ALPHA, DELTA, PI, DARSIN, RD, SS(3)
        PARAMETER ( PI=3.141592653589793D0 )
        IER=0
!
        CALL COPY_V   ( 3, S,  SS )
        CALL NORM_VEC ( 3, SS, RD )
        DELTA=DASIN ( SS(3) )
        ALPHA=DASIN ( SS(2)/( DSQRT ( 1.D0 - SS(3)*SS(3) ) ) )
        IF ( SS(1) .LT. 0.D0 ) ALPHA=PI-ALPHA
        IF ( ALPHA .LT. 0.D0 ) ALPHA=2.D0*PI+ALPHA
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  SUBROUTINE  DECPOL  !#!#
