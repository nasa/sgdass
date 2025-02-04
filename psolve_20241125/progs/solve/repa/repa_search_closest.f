      SUBROUTINE REPA_SEARCH_CLOSEST ( XC, YC, NCLR,           &      
                                       N1, XARR1, YARR1,       &
     &                                 N2, XARR2, YARR2,       &
     &                                 N3, XARR3, YARR3,       &
     &                                 XMIN, XMAX, YMIN, YMAX, &
     &                                 IND_CLR, IND_PT )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine REPA_SEARCH_CLOSEST searches in the arrays of   *
! *   points the point whcih is the closest to the world coordinates     *
! *   (XC, YC) among the points within the box [XMIN, XMAX, YMIN, YMAX], *
! *   among categories (colors) specicied by parameter NCLR. In fact     *
! *   REPA_SEARCH_CLOSEST searches no in the entire box, but in the      *
! *   curcle of REPA__M_DIST_R4 the size of the box. So, if the cursor   *
! *   is too far from any point, no point will be reported. The index    *
! *   of the point found and the category (color) are IND_CLR and        *
! *   IND_PT.                                                            *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       XC ( REAL*4    ) -- X coordinate of the cursor.                *
! *       YC ( REAL*4    ) -- Y coordinate of the cursor.                *
! *     NCLR ( INTEGER*4 ) -- The total number of categories (colors),   *
! *                           amonge which to search:                    *
! *                        REPA__I_GOOD -- to search only among good     *
! *                                        points.                       *
! *                        REPA__I_BAD  -- to search among good and bad  *
! *                                        points.                       *
! *                        REPA__I_UNR  -- to search only among all:     *
! *                                        good, bad and unrecoverable   *
! *                                        points.                       *
! *       N1 ( INTEGER*4 ) -- The number of good points.                 *
! *    XARR1 ( REAL*4    ) -- Array of good point arguments.             *
! *                           Dimension: N1.                             *
! *    EARR1 ( REAL*4    ) -- Array of good point errors.                *
! *                           Dimension: N1.                             *
! *       N2 ( INTEGER*4 ) -- The number of bad points.                  *
! *    XARR2 ( REAL*4    ) -- Array of bad point arguments.              *
! *                           Dimension: N2.                             *
! *    EARR2 ( REAL*4    ) -- Array of bad point errors.                 *
! *                           Dimension: N2.                             *
! *       N3 ( INTEGER*4 ) -- The number of bad points.                  *
! *    XARR3 ( REAL*4    ) -- Array of unrecoverable point arguments.    *
! *                           Dimension: N3.                             *
! *    EARR3 ( REAL*4    ) -- Array of unrecoverable point errors.       *
! *                           Dimension: N3.                             *
! *     XMIN ( REAL*4    ) -- The minimal argument in the search box.    *
! *     XMAX ( REAL*4    ) -- The maximal argument in the search box.    *
! *     YMIN ( REAL*4    ) -- The minimal value in the search box.       *
! *     YMAX ( REAL*4    ) -- The maximal value in the search box.       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  IND_CLR ( INTEGER*4 ) -- Caterory index. One of                     *
! *                           REPA__I_GOO -- Good point.                 *
! *                           REPA__I_BAD -- Bad, but recoverable point. *
! *                           REPA__I_UNR -- Bad, unrecoverable point.   *
! *   IND_PT ( INTEGER*4 ) -- point index **within** the categroy.       *
! *                                                                      *
! * ## 08-DEC-2004 REPA_SEARCH_CLOSEST v1.0 (c) L. Petrov 08-DEC-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      REAL*4     XC, YC
      INTEGER*4  NCLR, N1, N2, N3, IND_CLR, IND_PT
      REAL*4     XARR1(*), YARR1(*), &
     &           XARR2(*), YARR2(*), &
     &           XARR3(*), YARR3(*)
      REAL*4     XMIN, XMAX, YMIN, YMAX
!
      REAL*4     DIST, DIST_MIN
      INTEGER*4  J1, J2, J3
!
! --- Scanning all points and searching the point to be at the minimal distance
! --- from the current cursor coordinates
!
      DIST_MIN = REPA__M_DIST_R4**2 ! Square of the maximal distance
      IND_PT  = 0
      IND_CLR = 0
      IF ( NCLR .GE. 1  .AND.  N1 .GT. 0 ) THEN
           DO 410 J1=1,N1
!
! ----------- Look through all points of the first category
!
              IF ( XARR1(J1) .GE. XMIN  .AND.  XARR1(J1) .LE. XMAX  .AND. &
     &             YARR1(J1) .GE. YMIN  .AND.  YARR1(J1) .LE. YMAX        ) THEN
!
! ---------------- Only points within plotting area are being taken into account
!
                   DIST = ( (XARR1(J1) - XC)/(XMAX - XMIN)*REPA__ASPECT )**2 + &
     &                    ( (YARR1(J1) - YC)/(YMAX - YMIN)              )**2
                   IF ( DIST .LT. DIST_MIN ) THEN
                        IND_PT   = J1
                        IND_CLR  = REPA__I_GOO 
                        DIST_MIN = DIST
                   END IF
              END IF
 410       CONTINUE
      END IF
!
      IF ( NCLR .GE. 2  .AND.  N2 .GT. 0 ) THEN
!
! -------- Look through all points of the second category
!
           DO 420 J2=1,N2
              IF ( XARR2(J2) .GE. XMIN  .AND.  XARR2(J2) .LE. XMAX  .AND. &
     &             YARR2(J2) .GE. YMIN  .AND.  YARR2(J2) .LE. YMAX        ) THEN
!
! ---------------- Only points within plotting area are being taken into account
!
                   DIST = ( (XARR2(J2) - XC)/(XMAX - XMIN)*REPA__ASPECT )**2 + &
     &                    ( (YARR2(J2) - YC)/(YMAX - YMIN)              )**2
                   IF ( DIST .LT. DIST_MIN ) THEN
                        IND_PT   = J2
                        IND_CLR  = REPA__I_BAD
                        DIST_MIN = DIST
                   END IF
              END IF
 420       CONTINUE
      END IF
!
      IF ( NCLR .GE. 3  .AND.  N3 .GT. 0 ) THEN
!
! -------- Look through all points of the first category
!
           DO 430 J3=1,N3
              IF ( XARR3(J3) .GE. XMIN  .AND.  XARR3(J3) .LE. XMAX  .AND. &
     &             YARR3(J3) .GE. YMIN  .AND.  YARR3(J3) .LE. YMAX        ) THEN
!
! ---------------- Only points within plotting area are being taken into account
!
                   DIST = ( (XARR3(J3) - XC)/(XMAX - XMIN)*REPA__ASPECT )**2 + &
     &                    ( (YARR3(J3) - YC)/(YMAX - YMIN)              )**2
                   IF ( DIST .LT. DIST_MIN ) THEN
                        IND_PT   = J3
                        IND_CLR  = REPA__I_UNR
                        DIST_MIN = DIST
                   END IF
              END IF
 430       CONTINUE
      END IF
!
      RETURN
      END  SUBROUTINE  REPA_SEARCH_CLOSEST 
