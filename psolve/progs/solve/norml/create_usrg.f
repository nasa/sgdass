      SUBROUTINE create_usrg(np1,np2)
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
      integer*2 np1,np2
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
      INTEGER*2 I,J,ierr,nup,ifirst
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
      ifirst = np1-np2+1
      fn1 = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
      call bin_unlink(fn1,ierr )
      open(66,file=fn1,IOSTAT=ios)
      call ferr( INT2(ios), "Opening user partial file"//fn1, INT2(0), INT2(0) )
      write(66,'(I5)')np2
      do i=ifirst,ifirst+np2-1
        write(66,'(a)',IOSTAT=ios) cparm_names(i)//" G"
        call ferr( INT2(ios), "Writing user partial file", INT2(0), INT2(0) )
      enddo
      close(66,IOSTAT=ios)
      call ferr( INT2(ios), "Closing user partial file", INT2(0), INT2(0) )
!
      RETURN
      END
