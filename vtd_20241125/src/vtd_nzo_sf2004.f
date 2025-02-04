      SUBROUTINE VTD_NZO_FS2004 ( VTD, ISTA1, ISTA2, ISOU, TAU_GEOM, RATE_GEOM, &
     &                            TAU_DER_EOP,  TAU_DER_STA1, TAU_DER_STA2, &
     &                            TAU_DER_POS,  TAU_DER_VEL, & 
     &                            RATE_DER_EOP, RATE_DER_STA1, RATE_DER_STA2, &
     &                            RATE_DER_POS, RATE_DER_VEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_NZO_FS2004 computes for the near zone object VLBI time *
! *   delay, its first time derivative and derivatives with respect to   *
! *   some parameters by using analytical algirithm Sekido, Fukushima    *
! *   (2004).                                                            *
! *                                                                      *
! *   VLBI Time delay is defined as the difference of two intervals of   *
! *   proper time: 1) the interval of proper time of station #1 between  *
! *   events: coming the wave front to the reference point on the moving *
! *   axis and clock synchronization; 2) the interval of proper time of  *
! *   station #2 between events: coming the wave front to the reference  *
! *   point on the moving axis and clock synchronization. The time delay *
! *   is referred to the moment of coming the wave front to the          *
! *   reference point on the moving axis of the first antenna at time    *
! *   measured by the timescale TAI. The reference point of the station  *
! *   for which modeling is done is defined as the point on the moving   *
! *   axis which has the minimal distance to the fixed axis. In the      *
! *   case if axes intersect, this is the point of intersection.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISTA1 ( INTEGER*4 ) -- Index in the list of station of the first *
! *                            station of the baseline.                  *
! *     ISTA2 ( INTEGER*4 ) -- Index in the list of station of the       *
! *                            second station of the baseline.           *
! *      ISOU ( INTEGER*4 ) -- Index in the list of sources of the       *
! *                            source under consideration.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  TAU_GEOM ( REAL*8    ) -- Geometric delay. Units: seconds.          *
! * RATE_GEOM ( REAL*8    ) -- First time derivative of geometric time  *
! *                            delay. Units: dimensionless.              *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
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
! * ### 31-DEC-2005  VTD_NZO_FS2004  v1.0 (c) L. Petrov  31-DEC-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INCLUDE   'astro_constants.i'
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER
      REAL*8     TAU_GEOM, RATE_GEOM, TAU_DER_EOP, TAU_DER_STA1, &
     &           TAU_DER_STA2, TAU_DER_POS(3),  TAU_DER_VEL(3), &
     &           RATE_DER_EOP, RATE_DER_STA1, RATE_DER_STA2, &
     &           RATE_DER_POS(3), RATE_DER_VEL(3)
      TYPE     ( VTD__TYPE ) :: VTD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_NZO_FS2004  !#!#
