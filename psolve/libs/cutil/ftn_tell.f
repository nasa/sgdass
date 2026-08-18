      SUBROUTINE FTN_TELL ( LU, FNAME, OFFSET )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FTN_TELL PROGRAM SPECIFICATION
!
! 1.1 Tell the position relative to the beginning of file
!     for a fortran unit.  The program pauses if an error
!     is detected.
!
! 1.2 REFERENCES:
!
! 2.  FTN_TELL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 LU
      CHARACTER*(*) FNAME
!
! FNAME - File name
! LU - Fortran unit number assigned to file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 OFFSET
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! OFFSET - Position relative to beginning of file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fcftell
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IL, TRIMLEN
      INTEGER*4 FTELL
!
! IL - Length of file name
! LU4 - I*4 version of LU
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   Alan L. Fey: 12/17/92 - change to use fc_tell and fnum
!   L. Petrov    2003.07.11 -- replaced fc_tell with FTELL. The difference is
!                              that fc_tell called tell() which return position
!                              of the stream (unbuffered file), while FTELL
!                              returned position in a buffered file. If the
!                              buffer is not flushed, then tell() gave not the
!                              actuial position in the file, but position of the
!                              end of the current buffer (which is ahead of the
!                              current position).
!   L. Petrov    2003.12.09 -- It turned out that the previous fix did not work
!                              on HP-UX 10.20, HP Fortran 2.4 due to a bug
!                              in system libraries. In order to curcuvent it
!                              FC_TELL is called, but the buffer is flushed 
!                              before it. It is unclear how well it should 
!                              work, but at least the application does not
!                              crash
!
! 5.  FTN_TELL PROGRAM STRUCTURE
!
!
#ifdef BUG01
      CALL FLUSH ( INT4(LU) )
      OFFSET = FC_TELL ( FNUM ( INT4(LU) ) ) 
#else
      OFFSET = FTELL ( INT4(LU) )
#endif
      IF ( OFFSET .EQ. -1 ) THEN
           IL=TRIMLEN(FNAME)
           CALL FERR ( INT2(178), 'FTN_TELL', INT2(0), INT2(0) )
           CALL EXIT ( 1 )
      ENDIF
!
      RETURN
      END  !#!  FTN_TELL  #!#
