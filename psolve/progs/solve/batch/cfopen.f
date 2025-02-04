      INTEGER*2 FUNCTION CFOPEN(NAMR)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CFOPEN PROGRAM SPECIFICATION
!
! 1.1 Low-level I/O utility to open the control file.
!
! 1.2 REFERENCES:
!
! 2.  CFOPEN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) NAMR
!
! NAMR - Name of the control file.
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'batcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrls
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 IOS
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb  980515  print out name of control file if it can't be opened
!
! 5.  CFOPEN PROGRAM STRUCTURE
!
! Open the file;display message if error return
!
      OPEN ( 92, FILE=NAMR, IOSTAT=IOS, STATUS='OLD' )
      CALL FERR ( INT2(IOS), ' OPENING: '//NAMR, INT2(0), INT2(0) )
      KGOT=.FALSE.
      KEOF=.FALSE.
      SAVREC=1
      CFOPEN=0
!
      RETURN
      END
