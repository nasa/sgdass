      SUBROUTINE CFUNRD(LENGTH,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CFUNRD PROGRAM SPECIFICATION
!
! 1.1 Low-level I/O utility to put one string back on the input stream
!     from the control file. Sort of 'UN-READ', like 'UNGETC()' function
!     in languages which shall remain letterless.
!
! 1.2 REFERENCES:
!
! 2.  CFUNRD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
      INTEGER*2 LENGTH
!
! LENGTH - Length of string to be unread (characters)
! STRING - String to be unread
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'batcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset,garc,ctrlfl
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CFUNRD PROGRAM STRUCTURE
!
! Make sure we haven't already unread a record
!
      IF(KGOT) THEN
        CALL FERR( INT2(-32000), 'SECOND UNREAD ON CONTROL FILE', INT2(0), &
     &       INT2(0) )
      ELSE IF(.NOT.KEOF) THEN
!
! Keep internal copy of record to be "read" next time
!
        LENRD=LENGTH
        INTERN=STRING
        KGOT=.TRUE.
      ENDIF
!
      RETURN
      END
