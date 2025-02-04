      SUBROUTINE FTN_SEEK(LU,FNAME,OFFSET)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FTN_SEEK PROGRAM SPECIFICATION
!
! 1.1 Seek a position relative to the beginning of file for
!     a fortran unit. The program pauses if an error is
!     detected.
!
! 1.2 REFERENCES:
!
! 2.  FTN_SEEK INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 LU
      INTEGER*4 OFFSET
      CHARACTER*(*) FNAME
!
! FNAME - File name
! LU - Fortran unit number to be positioned
! OFFSET - Result of FTN_TELL for the desired position
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fcfseek
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 LU4,FCFSEEK,STREAM,IERR4
      INTEGER*2 IL,TRIMLEN
      character*80 bufstr
      INTEGER*4  DUM
      INTEGER*4 I4P0
      DATA  I4P0 / 0 /
      INTEGER*4, EXTERNAL :: FSTREAM, UNIT_TO_FILDESC
!
! IERR4 - Return from FCFSEEK
! IL - Length of file name
! LU4 - I*4 version of LU
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   Alan L. Fey: 12/17/92 - change to use fc_lseek and fnum
!
! 5.  FTN_SEEK PROGRAM STRUCTURE
!
1     CONTINUE
      LU4=LU
      STREAM=UNIT_TO_FILDESC(LU4)
#ifdef GNU
      CALL FSEEK ( LU4, OFFSET, 0, DUM )
#else
      IERR4 = FC_LSEEK ( STREAM, OFFSET, 0 )
#endif
      IF(IERR4.EQ.-1) THEN
        IL=TRIMLEN(FNAME)
99      FORMAT(' Bad Seek on ',A)
        call ferr( INT2(177), 'ftn_seek', INT2(0), INT2(0) )
        GOTO 1
      ENDIF
!
      RETURN
      END
