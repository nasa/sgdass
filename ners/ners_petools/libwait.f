      SUBROUTINE LIB$WAIT ( RTIME )
      REAL*8     RTIME
      INTEGER*4  ITIME, IS
      ADDRESS__TYPE  ARR1(2), ARR2(2)
#if defined LINUX || defined DARWIN
      ARR1(1) = RTIME
      ARR1(2) = (RTIME - ARR1(1))*1.E9
      IS = NANOSLEEP ( ARR1, ARR2 )
#else
      ITIME = RTIME
      IF ( RTIME .LT. 1.0D0 ) ITIME = 1
      CALL FUNC_SLEEP ( ITIME )
#endif
      RETURN
      END  !#!  LIB$WAIT  #!#
