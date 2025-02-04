      SUBROUTINE YMDAY ( IYEAR, IDAYR, MONTH, IDAY )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 iyear, idayr, month, iday, m
!-----END of imp added lines.
!
      Integer*2 iday0
!     Convert day-of-year to month and day TAC 760207
!
!     IF IDAYR>0,
!         THIS ROUTINE WILL CONVERT IDAYR = ELAPSED DAY-OF-THE-YEAR
!         INTO MONTH AND IDAY. IF IDAYR>365 (OR >366 FOR A LEAP YEAR)
!         THE YEAR WILL BE INCREMENTED ACCORDINGLY, AND IDAYR WILL BE
!         'CORRECTED'.
!
!     IF IDAYR <=0,
!         THE MONTH AND IDAY FIELDS WILL BE CONVERTED INTO DAY-OF-THE-
!         YEAR AND RETURNED IN IDAYR.
!
!     THIS ROUTINE CALLS IDAY0 ( IYEAR , 0 ) TO GET NUMBER OF DAYS
!     IN IYEAR AND IDAY0 ( IYEAR , MONTH ) TO GET THE DAY NUMBER OF
!     THE ZEROTH DAY OF THE MONTH.
!
!          T.A.CLARK     02 JAN '76       REVISED 07 FEB '76
!
!-----WAS THE ENTRY MONTH AND DAY OR DAY-OF-YEAR?
!
      IF ( IDAYR .LE. 0 ) GO TO 3
!
!-----FIRST, CHECK TO SEE IF IDAYR IS VALID:
!1     IF ( IDAYR .LE. IDAY0 ( IYEAR, INT2(0) ) ) GO TO 2
!      IDAYR = IDAYR - IDAY0 ( IYEAR, INT2(0) )
!      IYEAR = IYEAR + 1
!      GO TO 1
!
!-----NOW, FIND THE MAGIC MONTH:
!
2       DO 21 M = 1 , 12
        MONTH = M
        IF ((IDAY0( IYEAR, INT2(M+1)) .GE. IDAYR) .OR. (M .GE. 12)) GO TO 22
21      CONTINUE
!
!-----AND NOW FIND THE DAY-OF-THE-MONTH AND RETURN:
!
22    IDAY = IDAYR - IDAY0 ( IYEAR , MONTH )
      RETURN
!
!-----GET DAY-OF-THE-YEAR IF MONTH AND IDAY WAS ENTERED:
!
3     IDAYR = IDAY + IDAY0 ( IYEAR , MONTH )
!
      RETURN
      END
