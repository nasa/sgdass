      SUBROUTINE BINN_WRITE(FNAME,FILDES,IARR,JCOUNT,NFTREAT,JERR)
      IMPLICIT NONE
!
! 1.  BINN_WRITE PROGRAM SPECIFICATION
!
!     More flexible version of bin_write.  Bin_write treated errors as fatal.
!     Binn_write allows the errors to be returned to the caller for higher
!     level error handling.
!
! 1.1 Write an array to the specified binary file
!
! 1.2 REFERENCES:
!
! 2.  BINN_WRITE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 FILDES,JCOUNT
      INTEGER*2 IARR(*)
      CHARACTER*(*) FNAME
      CHARACTER*1 NFTREAT
!
! FILDES - File descriptor of file to be written to
! FNAME - Name of file to be written to
! IARR - The array to be written
! JCOUNT - Number of blocks to be written
! nftreat - treat the error as fatal (F) or non-fatal (N)
!
! 2.3 OUTPUT Variables:
!
!     JERR - error return or number of bytes written, if successful
!
      INTEGER*4 JERR
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_write,psfatal_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      INTEGER*4 JBYTE
      DATA ME/'BINN_WRITE'/
!
! JBYTE - Number of bytes to be written
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB   960207 Created from bin_write.
!
! 5.  BINN_WRITE PROGRAM STRUCTURE
!
! Calculate number of bytes to write
!
      JBYTE=JCOUNT*BLOCK_WORDS*WORD_BYTES
!
! Write array to file
!
      JERR=FC_WRITE(FILDES,ptr_nc(IARR),JBYTE)
!
! Report any problems encountered
!
      call psfatal_file(jerr,'writing',fname,me,nftreat)
!
      IF(JERR.NE.JBYTE.AND.JERR.GE.0) THEN
        jerr=-1
        call psfatal_file(jerr,'incorrect transfer',fname,me,nftreat)
      ENDIF
!
      END
