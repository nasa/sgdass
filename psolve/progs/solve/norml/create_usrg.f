      SUBROUTINE CREATE_USRG ( NP1, NP2 )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CREATE_USRG PROGRAM SPECIFICATION
!
! 1.1 Create the file containing user-defined global parameters
!
! 1.2 REFERENCES:
!
! 2.  CREATE_USRGP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
      INTEGER*4 NP1, NP2
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'plist.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: adder
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I, J, IERR, NUP, IFIRST
      INTEGER*4 IOS
      character*(NAME_SIZE) fn1
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH   950324 Created
!   JMG   951103 Fix userpartial error occurrring when more than 999 partials.
!   jmg   960610 Remove holleriths.
!
! 5.  CREATE_USRG PROGRAM STRUCTURE
!
! Update user-defined global parameter list file
!
!
      IFIRST = NP1-NP2+1
      FN1 = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
      CALL BIN_UNLINK ( FN1, IERR  )
      OPEN( 66, FILE=FN1, IOSTAT=IOS )
      CALL FERR( INT2(ios), "Opening user partial file "//FN1, INT2(0), INT2(0) )
      WRITE ( 66, '(I5)' ) NP2
      DO I=IFIRST,IFIRST+NP2-1
         WRITE ( 66, '(A)', IOSTAT=IOS ) CPARM_NAMES(I)//" G"
         CALL FERR( INT2(IOS), "Writing user partial file", INT2(0), INT2(0) )
      ENDDO
      CLOSE ( 66, IOSTAT=IOS )
      CALL FERR( INT2(IOS), "Closing user partial file", INT2(0), INT2(0) )
!
      RETURN
      END
