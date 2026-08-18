      SUBROUTINE PSFATAL_FILE ( IERR4, STR, FNAME, WHO, NFTREAT )
      IMPLICIT NONE
!
! 1.  psfatal_file PROGRAM SPECIFICATION
!
! 1.1 Display the provided message in case of a file access error.
!
!     expanded version of subroutine fatal_file; the error is possibly fatal
!     (as determined by the input argument, nftreat).
!
! 1.2 REFERENCES:
!
! 2.  psfatal_file INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      character*(*) str,fname,who
      character*1 nftreat
      integer*4 ierr4
!
! FNAME - File name
! IERR4 - Error type (>= 0 means no error)
! STR - Message to be displayed
! WHO - Name of routine in which error is detected
! nftreat - treat the error as fatal (F) or non-fatal (N)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_perror,psfatal_w
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 TRIMLEN
      CHARACTER OUTPUT*1024, CHR*1
      INTEGER*4 IERRIN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB   960207 Created from fatal_file.f.
!
! 5.  psfatal_file PROGRAM STRUCTURE
!
! First check whether we have an error
!
      IF ( IERR4 .GE. 0 ) RETURN
      IERRIN = NULL_TERM ( CHR, ' ' )
      IF ( IERRIN .EQ. 0 ) THEN
           CALL FC_PERROR ( PTR_CH(CHR) )
         ELSE
           WRITE ( *, '("null_term failed in psfatal_file")' )
      ENDIF
      OUTPUT=STR(:TRIMLEN(STR))//' '//FNAME(:TRIMLEN(FNAME))
      CALL PSFATAL_W ( OUTPUT, WHO, NFTREAT )
!
      RETURN
      END  !#!  PSFATAL_FILE  #!#
