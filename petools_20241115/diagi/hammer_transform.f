      SUBROUTINE HAMMER_TRANS ( PHI, LAM, XCOEF, XP, YP ) 
! ************************************************************************
! *                                                                      *
! *   This routine makes a Hammer transformation: it puts the point with *
! *   latitude PHI and longitude LAM on a map in a equi-rea Hammer       *
! *   projection.                                                        *
! *                                                                      *
! *  ### 26-AUG-2003  HAMMER_TRANS  v1.1 (c) L. Petrov  27-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*4     PHI, LAM, XCOEF, XP, YP
      REAL*4     LAT, LON
      REAL*4      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi number
!
      LAT = PHI
      IF ( LAT .GT. P2I  .OR.  LAT .LT. P2I ) THEN
           LAT = LAT - PI2*NINT((LAT+PI)/PI2)
      END IF
!
      LON = LAM
      IF ( LON .GT. PI  .OR.  LON .LT. -PI ) THEN
           LON = LON - PI2*NINT(LON/PI2)
      END IF
!
      IF ( 1.0 + COS(LAT)*COS(LON/2.0) .GT. 1.E-5 ) THEN
           XP = 2.0*COS(LAT)*SIN(LON/2.0)/SQRT( 1.0 + COS(LAT)*COS(LON/2.0) )
           YP = SIN(LAT)/SQRT( 1.0 + COS(LAT)*COS(LON/2.0) )
        ELSE 
           XP =  0.0
           YP =  0.0
      END IF
!
      XP = XP*XCOEF
!
      RETURN
      END  !#!  HAMMER_TRANS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INV_HAMMER_TRANS ( XP, YP, XCOEF, PHI, LAM )
! ************************************************************************
! *                                                                      *
! *   This routine makes an inverse Hammer transformation: it computes   *
! *   latitude and longitude for the point with coordinates XP, YP.      *
! *                                                                      *
! * ### 27-AUG-2003 INV_HAMMER_TRANS v1.1 (c) L. Petrov  27-AUG-2003 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*4     XP, YP, PHI, LAM, XCOEF
      REAL*4      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi number
      REAL*4     Z, EPS
      PARAMETER  ( EPS = 1.0E-6 )
!
      IF ( 1.0 - ((XP/XCOEF))**2/4 - YP**2 .LT. EPS ) THEN
           PHI = -999.0
           LAM = -999.0
           RETURN 
      END IF
      Z = 2.0 - ((XP/XCOEF))**2/4 - YP**2 
      PHI = ASIN ( YP*SQRT(Z) )
      IF ( COS(PHI) .LT. EPS  .AND.  COS(PHI) .GT. -EPS ) THEN
           LAM = PI
         ELSE
           IF ( (1.0 - ((XP/XCOEF))**2/4 - YP**2)/COS(PHI) .LT.  1.-EPS .AND. &
     &          (1.0 - ((XP/XCOEF))**2/4 - YP**2)/COS(PHI) .GT. -1.+EPS ) THEN
                LAM = 2.0 * ACOS ( (1.0 - ((XP/XCOEF))**2/4 - YP**2)/COS(PHI) )
              ELSE 
                LAM = 0.0
           END IF
      END IF
      IF ( XP .LT. 0.0 ) LAM = -LAM
!
      RETURN
      END  !#!  INV_HAMMER_TRANS  #!#
