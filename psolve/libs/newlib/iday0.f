      FUNCTION IDAY0 ( IYEAR, MONTH )
! ************************************************************************
! *                                                                      *
! *    Get # elapsed days in year to date  TAC 760102                    *
! *
! *  ### 02-JAN-1976     IDAY0      T. Clark, L. Petrov 16-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2  IDAY0, IYEAR, MONTH, TAB(12,4), YEAR_USE, MON_USE
      DATA TAB / &
     &           0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, &   !
     &           0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, &   !
     &           0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, &   !
     &           0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334  /
!
      YEAR_USE = MOD ( IYEAR, INT2(4) ) + 1
      MON_USE = MONTH
      IF ( MON_USE .LT. 1  ) MON_USE = 1
      IF ( MON_USE .GT. 12 ) MON_USE = 12
      IDAY0 = TAB(MON_USE,YEAR_USE)
!
      RETURN
      END  !#!  IDAY0 #!#
