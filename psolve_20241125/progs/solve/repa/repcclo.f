      SUBROUTINE REPCCLO ( NP_ARR1, X4_ARR1, Y4_ARR1, &
     &                     NP_ARR2, X4_ARR2, Y4_ARR2, &
     &                     NP_ARR3, X4_ARR3, Y4_ARR3, &
     &                     XMIN, XMAX, YMIN, YMAX, &
     &                     XC, YC, DIST_X, DIST_Y, ICLR, IPQ )
!
! ************************************************************************
! *                                                                      *
! *   REPCCLO searches for the point closest to XC,YC which is           *
! *   1) within [XMIN, XMAX], [YMIN, YMAX] range                         *
! *   2) within [XC-DIST_X, XC+DIST_X], [YC-DIST_Y, YC+DIST_Y] range.    *
! *                                                                      *
! *   The search is done over three sets (colors) of points. If such a   *
! *   point is found, then IPQ keeps the return index of the point in    *
! *   the array ICLR -- the set index (color index). Otherwize IPQ=0,    *
! *   ICLR=0 .                                                           *
! *                                                                      *
! *  called subroutines: none                                            *
! *  calling routines: REPINFO, REPPTSH, REPPTSU                         *
! *                                                                      *
! *  ### 08-AUG-2002   UD_SEARCH   v1.0 (c)  L. Petrov  08-AUG-2002 ###  *
! *  02-08-28 VT - three point sets (colors)                             *
! *  02-10-28 VT - name changed: REPCCLO                                 *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4  NP_ARR1, NP_ARR2, NP_ARR3, ICLR, IPQ
      REAL*4     X4_ARR1(NP_ARR1), Y4_ARR1(NP_ARR1)
      REAL*4     X4_ARR2(NP_ARR2), Y4_ARR2(NP_ARR2)
      REAL*4     X4_ARR3(NP_ARR3), Y4_ARR3(NP_ARR3)
      REAL*4     XC, YC, DIST_X, DIST_Y
      REAL*4     DX, DY
      REAL*4     XMIN, XMAX, YMIN, YMAX
      REAL*4     DIST_MIN
      INTEGER*4  J1, J2, J3
!
      ICLR = 0
      IPQ = 0
!
      DIST_MIN = 2.0
!
! --- 1st array
      DO 410 J1=1,NP_ARR1
! ------ ignore the points which are beyond plotting area
         IF ( X4_ARR1(J1) .LT. XMIN ) GOTO 410
         IF ( X4_ARR1(J1) .GT. XMAX ) GOTO 410
         IF ( Y4_ARR1(J1) .LT. YMIN ) GOTO 410
         IF ( Y4_ARR1(J1) .GT. YMAX ) GOTO 410
! ------ compute the distance on X and Y axis
         DX = (XC - X4_ARR1(J1))/DIST_X
         DY = (YC - Y4_ARR1(J1))/DIST_Y
! ------ reject points which are too far
         IF ( ABS(DX) .GT. 1.0 ) GOTO 410
         IF ( ABS(DY) .GT. 1.0 ) GOTO 410
! ------ check whether the J1-th point is the closest to the cursor
         IF ( SQRT(DX**2 + DY**2) .LT. DIST_MIN ) THEN
              DIST_MIN = SQRT(DX**2 + DY**2)
              ICLR = 1
              IPQ = J1
         END IF
 410  CONTINUE
!
! --- the same for 2nd array
!
      DO 420 J2=1,NP_ARR2
         IF ( X4_ARR2(J2) .LT. XMIN ) GOTO 420
         IF ( X4_ARR2(J2) .GT. XMAX ) GOTO 420
         IF ( Y4_ARR2(J2) .LT. YMIN ) GOTO 420
         IF ( Y4_ARR2(J2) .GT. YMAX ) GOTO 420
         DX = (XC - X4_ARR2(J2))/DIST_X
         DY = (YC - Y4_ARR2(J2))/DIST_Y
         IF ( ABS(DX) .GT. 1.0 ) GOTO 420
         IF ( ABS(DY) .GT. 1.0 ) GOTO 420
         IF ( SQRT(DX**2 + DY**2) .LT. DIST_MIN ) THEN
              DIST_MIN = SQRT(DX**2 + DY**2)
              ICLR = 2
              IPQ = J2
         END IF
 420  CONTINUE
!
! --- the same for 3rd array
!
      DO 430 J3=1,NP_ARR3
         IF ( X4_ARR3(J3) .LT. XMIN ) GOTO 430
         IF ( X4_ARR3(J3) .GT. XMAX ) GOTO 430
         IF ( Y4_ARR3(J3) .LT. YMIN ) GOTO 430
         IF ( Y4_ARR3(J3) .GT. YMAX ) GOTO 430
         DX = (XC - X4_ARR3(J3))/DIST_X
         DY = (YC - Y4_ARR3(J3))/DIST_Y
         IF ( ABS(DX) .GT. 1.0 ) GOTO 430
         IF ( ABS(DY) .GT. 1.0 ) GOTO 430
         IF ( SQRT(DX**2 + DY**2) .LT. DIST_MIN ) THEN
              DIST_MIN = SQRT(DX**2 + DY**2)
              ICLR = 3
              IPQ = J3
         END IF
 430  CONTINUE
!
      RETURN
      END  !#!  REPCCLO  #!#
