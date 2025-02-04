      SUBROUTINE ADQL ( N, QLIN, ADD )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ADQL PROGRAM SPECIFICATION
!
! 1.1 Insert (or append) one string into another at the specified position.
!     Program execution pauses in case of overflow.
!
! 1.2 REFERENCES:
!
! 2.  ADQL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 N
      CHARACTER*(*) QLIN,ADD
!
! ADD - The string to be added to the buffer
! N - First position in buffer available for appended string
! QLIN - The buffer holding the string to be appended
!
! 2.3 OUTPUT Variables:
!
! N - The new next available position in QLIN
! QLIN - Buffer with ADD appended
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: first
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 LQLIN,LADD
!
! LADD - Length of string to be added
! LQLIN - Length of buffer
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ADQL PROGRAM STRUCTURE
!
      LQLIN=LEN(QLIN)
      LADD=LEN(ADD)
!
      IF ( N+LADD-1 .GT. LQLIN ) THEN
!           CALL PAUSE ( 'ADQL - Overflow of the line in attempt to add text' )
           CALL FERR ( INT2(136), 'ADQL -- Overflow of the line in attempt '// &
     &         'to add text', INT2(0), INT2(0) )
        ELSE
           QLIN(N:N+LADD-1)=ADD
           N=N+LADD
      END IF
!
      RETURN
      END  !#!  ADQL  #!#
