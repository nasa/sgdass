!@This is the start of file &curlib.i
!
! --- Created 12-APR-99
!
      INTEGER*4         CURLIB_FLAG
      COMMON / CURLIB / CURLIB_FLAG
      INTEGER*4  CRS__UND, CRS__ON, CRS__OFF
      PARAMETER  ( CRS__UND  = 0          ) ! Undefined
      PARAMETER  ( CRS__ON   = 1022734822 ) ! Curses is ON
      PARAMETER  ( CRS__OFF  = 2103749203 ) ! Curses is OFF
