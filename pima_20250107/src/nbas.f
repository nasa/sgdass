        FUNCTION NSTBA ( NST1, NST2 )
! ************************************************************************
! *                                                                      *
! *     Function  NSTBA  calculates the unique number of the baseline    *
! *     using the numbers of the stations entering in this baseline.     *
! *                                                                      *
! *     NSTBA is calculated using the expression:                        *
! *                                                                      *
! *           NSTBA = ISIG * ( (MX*(MX-1))/2 + MN )                      *
! *                                                                      *
! *           where  MN   --  min ( NST1, NST2 ) ;                       *
! *                  MX   --  max ( NST1, NST2 ) ;                       *
! *                  ISIG --  +1, if NST1 < NST1 ;                       *
! *                           -1, if NST1 > NST1 .                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   NST1 ( INTEGER*4 ) -- Number of the first station of the baseline. *
! *   NST2 ( INTEGER*4 ) -- Number of the second station of the baseline.*
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  <NSTBA> ( INTEGER*4 ) -- Number of the baseline.                    *
! *                                                                      *
! *  Comment:                                                            *
! *     If parameters NST1, NST2  are not correct, then <NSTBA>=-999999  *
! *                                                                      *
! * ###  02-NOV-1991    NSTBA     v3.0  (c) L. Petrov  31-JUL-1997  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INCLUDE   'sou_map.i'
        INTEGER*4   NSTBA, NST1, NST2, NL_LIM, NH_LIM, MN, MX, ISIG
        PARAMETER ( NL_LIM = 1    )  !  Minimal number of the station
        PARAMETER ( NH_LIM = MSTA )  !  Maximal number of the station
!
        NSTBA = -999999
        MN = MIN ( NST1, NST2 )
        IF ( NST1 .LT. NL_LIM ) RETURN
        IF ( NST2 .LT. NL_LIM ) RETURN
        IF ( NST1 .GT. NH_LIM ) RETURN
        IF ( NST2 .GT. NH_LIM ) RETURN
        IF ( NST1 .LT. NST2   ) THEN
             MN = NST1
             MX = NST2
             ISIG=1
          ELSE IF ( NST1 .EQ. NST2 ) THEN
             RETURN
          ELSE
             MN = NST2
             MX = NST1
             ISIG=-1
        END IF
!
        NSTBA = ISIG *( (MX*(MX-1))/2 + MN )
!
        RETURN
        END  !#!  NSTBA  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE NBAST ( NB, NST1, NST2 )
! ************************************************************************
! *                                                                      *
! *     Routine  NBAST  calculates the numbers of the stations entering  *
! *     baseline, using the number of the baseline (which is assumed to  *
! *     be calculated by NSTBA ). The maximal number of the stations     *
! *     is being taken from gamb.i                                       *
! *                                                                      *
! *     NST1, NST2  are calculated using the expression:                 *
! *                                                                      *
! *     If NB>0:                                                         *
! *                                                                      *
! *              NST2 = INT( (1. + SQRT(1. + 8.*NB))/2. )                *
! *              NST1 = NB - NST2                                        *
! *                                                                      *
! *        Else if NB<0:                                                 *
! *                                                                      *
! *              NST1 = INT( (1. + SQRT(1. - 8.*NB))/2. )                *
! *              NST1 = -NB - NST1                                       *
! *                                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       NB ( INTEGER*4 ) -- Number of the baseline.                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   NST1 ( INTEGER*4 ) -- Number of the first station of the baseline. *
! *   NST2 ( INTEGER*4 ) -- Number of the second station of the baseline *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     If parameter  NB  is not correct then NST1=NST2=-999 .           *
! *                                                                      *
! * ###  02-NOV-1991      NBAST     v3.0  (c) L. Petrov 31-JUL-1997  ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INCLUDE   'sou_map.i'
        INTEGER*4   NB, NST1, NST2, NL_LIM, NH_LIM, K
        PARAMETER ( NL_LIM = 1    )  !  Minimal number of the station
        PARAMETER ( NH_LIM = MSTA )  !  Maximal number of the station
        REAL*8      D
!
        NST1 = -999
        NST2 = -999
!
        D = 1 + 8*ABS(NB)
        K = INT( (SQRT(D) + 1.0)/2.0 + 0.0001D0 )
        IF ( NB.GT.0 ) THEN
             NST2 = K
             NST1 = NB - (K*(K-1))/2
          ELSE
             NST1 =  K
             NST2 = -NB - (K*(K-1))/2
        END IF
        IF ( NST1 .LT. NL_LIM  .OR.  NST2 .LT. NL_LIM  .OR. &
     &       NST1 .GT. NH_LIM  .OR.  NST2 .GT. NH_LIM        ) THEN
!
             NST1 = -999
             NST2 = -999
        END IF
        RETURN
        END  !#!  NBAST  #!#
