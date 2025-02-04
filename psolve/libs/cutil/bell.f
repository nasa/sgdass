      SUBROUTINE BELL(KBELL)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  BELL PROGRAM SPECIFICATION
!
! 1.1 BELL rings the bell KBELL times on LU if KBELL > 0, rings it
!     10 times in an accelerating rate pattern if KBELL = 0, or in
!     a decelerating pattern if KBELL = -1, or in "shave and a haircut,
!     2 bits" if KBELL = -2, or in "didit,didit,didit" if KBELL = -3.
!     If KBELL < -3, its absolute value is
!     assumed to be the number of times to beep, and the delay after
!     each beep is assumed to be in array elements KBELL(2 thru N+1).
!     Note that the last delay follows the last note, and prevents
!     any further output to the LU until the beep is finished.  It
!     is recommended that the last delay be at least .20 sec.
!
! 1.2 REFERENCES:
!
! 2.  BELL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 KBELL(*)
!
! KBELL - specifies pattern of beeps requested (see above). Delays are
!         in units of .01 sec.  In an empty system, .20 sec is sufficient
!         to hear distinct beeps, and .15 sec causes them to run together.
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: susp
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IBELL,LIMIT,IT,I
      INTEGER*2 I2BITS(8),M3(7)
!
      DATA IBELL /o'3400'/
      DATA I2BITS /7,40,20,20,40,60,40,40/
      DATA M3 /6,20,40,20,40,20,40/
!
! I - Loop index
! IBELL - Word containing ASCII character BELL (o'3400') in high order byte
! IT - Delay, in units of .01 sec
! I2BITS - Array for "shave and a haircut, two bits"
! LIMIT - Number of beeps to do
! M3 - Array for "didit,didit,didit"(dot-dash,dot-dash,dot-dash)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  BELL PROGRAM STRUCTURE
!
!     1. Set limit on do loop.
!
      IF(KBELL(1).GT.0) THEN
         LIMIT = KBELL(1)
      ELSE IF(KBELL(1).EQ.-2) THEN
        LIMIT = I2BITS(1)
      ELSE IF(KBELL(1).EQ.-3) THEN
        LIMIT = M3(1)
      ELSE IF(KBELL(1).LT.-3) THEN
        LIMIT = -KBELL(1)
      ELSE
        LIMIT = 10
      ENDIF
!
!     2. WRITE IBELL IN LOOP, SUSPENDING FOR A FEW MILLISEC EACH TIME
!
      IT = 20
      DO I=1,LIMIT
        call beep_mn()
        IF(KBELL(1).EQ.0) THEN
          IT = 25 - 2 * I
        ELSE IF(KBELL(1).EQ.-1) THEN
          IT =  5 + 2 * I
        ELSE IF(KBELL(1).EQ.-2) THEN
          IT = I2BITS(I+1)
        ELSE IF(KBELL(1).EQ.-3) THEN
          IT = M3(I+1)
        ELSE IF(KBELL(1).LT.-3) THEN
          IT = KBELL(I+1)
        ENDIF
        IF(LIMIT.GT.1) CALL SUSP( INT2(2), INT2(1) )
      ENDDO
!
!     3. RETURN TO CALLING PROGRAM
!
      RETURN
      END
