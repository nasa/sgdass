      SUBROUTINE KEYJL (IKEY, FKEY)
      implicit none
!
! 1.  KEYJL PROGRAM SPECIFICATION
!
! 1.1 Convert a database name into a Julian date.
!
! 1.2 REFERENCES:
!
! 2.  KEYJL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IKEY(5)
!
! IKEY - Data base name
!
! 2.3 OUTPUT Variables:
!
      REAL*8 FKEY
!
! FKEY - Julian date corresponding to database name
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ymd,fjldy
!
! 3.  LOCAL VARIABLES
!
      REAL*8 FJLDY
      INTEGER*2 IDA(3), JKEY(5), i
      CHARACTER*10 QKEY
      EQUIVALENCE (QKEY, JKEY)
!
! IDA - Array holding year,month,day of date
! JKEY,QKEY - Character version of database name
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   IS   850702  A much simpler version
!
! 5.  KEYJL PROGRAM STRUCTURE
!
      DO 100 I = 1, 5
        JKEY(I) = IKEY(I)
  100 CONTINUE
      CALL YMD (QKEY, IDA)
      FKEY = FJLDY (IDA(2), IDA(3), IDA(1))
      RETURN
      END
