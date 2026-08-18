      SUBROUTINE sencr_char(ix,iy,char)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  SENCR_CHAR PROGRAM SPECIFICATION
!
! 1.1 Cursor sensing function with plain int and character args.
!
! 1.2 REFERENCES:
!
! 2.  SENCR_CHAR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      character*(*) char
      INTEGER*2 ix,iy
!
! CHAR - Single character that user struck at position IX,IY
! IX - Absolute cursor position left-to-right, left edge = 0
! IY - Absolute cursor position top-to-bottom, top line = 0
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: sencr
!
! 3.  LOCAL VARIABLES
!
      integer*4 ix2,iy2,icha1
      character*4 cicha1
      equivalence (cicha1,icha1)
!
! CICHA1,ICHA1 - Two-character return from SENCR
! IX2,IY2 - X and Y returns from SENCR
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
! Modified by P. Tomasi July 2 1999
! ichar is an intrinsic function, modified into icha1
!
! 5.  SENCR_CHAR PROGRAM STRUCTURE
!
      call senkr_mn(ix2,iy2,icha1 )
      ix=ix2
      iy=iy2
      char=cicha1(4:4)
!
      return
      end
