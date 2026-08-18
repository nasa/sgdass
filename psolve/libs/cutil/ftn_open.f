      SUBROUTINE FTN_OPEN(LU,FNAME,MODE)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FTN_OPEN PROGRAM SPECIFICATION
!
! 1.1 Open a file as a fortran logical unit (open for append
!     is supported). Program pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  FTN_OPEN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 LU
      CHARACTER*(*) FNAME,MODE
!
! FNAME - Name of file to open
! LU - Fortran unit number to use
! MODE - 'A' for append; anything else is ignored
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_freopen
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 dum,fc_lseek,offset,whence,fd
      INTEGER*4 UNIT_TO_FILDESC
      INTEGER*2 IL,TRIMLEN
      INTEGER*4  IOS
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! IL - Length of file name, in characters
! IOS - IOSTAT return from open call
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  910912  Use lseek instead of freopen to position at end of file
!
! 5.  FTN_OPEN PROGRAM STRUCTURE
!
1     CONTINUE
!
! Open the file
!
      OPEN(UNIT=LU,FILE=FNAME,IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        CALL FERR( INT2(IOS), 'opening '//FNAME, INT2(0), INT2(0) )
        GO TO 1
      ENDIF
!
! set up for append if requested
!
      IF(INDEX(MODE,'A').NE.0) THEN
        fd = UNIT_TO_FILDESC(int4(lu))
        offset=0
        whence=2
#ifdef GNU
           CALL FSEEK ( int4(lu), 0, 2, DUM )
#else
        dum=fc_lseek(fd,offset,whence)
#endif
        call fatal_file(dum,'seeking',fname,'ftn_open' )
      ENDIF
!
      RETURN
      END
