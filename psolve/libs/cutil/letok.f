      FUNCTION LETOK (LETRS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  LETOK PROGRAM SPECIFICATION
!
! 1.1 Check whether initials are valid.  File /solve/save_files/letok
!     contains the list of valid initials.
!
! 1.2 REFERENCES:
!
! 2.  LETOK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*2 LETRS
!
! LETRS - Initials to be checked for validity
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 LETOK
!
! LETOK - TRUE if given initials are found in the valid list
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ferr
!
! 3.  LOCAL VARIABLES
!
      Character*2 LETRY
      INTEGER*2   IERR
      INTEGER*4   IERR4
      character*50 fname, cletrs*2
      character*64 cbuf
      CHARACTER    STR*128
      integer*2 ibuf(32),trimlen
      equivalence (ibuf,cbuf)
!
! IERR - Error return from OPEN call
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   IS   851107  Created
!   MWH  891229  Added IMPLICIT NONE and fixed bug in error reporting
!   MWH  910326  Corrected file path in error message
!
! 5.  LETOK PROGRAM STRUCTURE
!
! Accept blanks
!
      CALL LIB$MOVC3 ( 2, LETRS, CLETRS )
      IF ( CLETRS == '  ' ) THEN 
           LETOK = .TRUE.
           RETURN
      END IF
!
! Open file with allowed initials
!
!      
      ierr=fc_getenv(ptr_ch('PSOLVE_SAVE_DIR'//char(0)),ptr_nc(ibuf))
      if (ierr.gt.0) then
        pre_sav_dir=cbuf(1:ierr)
      else
        PRE_SAV_DIR=SOLVE_SAVE_DIR
      endif
      PRE_SV_LEN=TRIMLEN(PRE_SAV_DIR)
      if (pre_sav_DIR(pre_sv_len:pre_sv_len).ne.'/') then
        pre_sv_len=pre_sv_len+1
        pre_sav_dir(pre_sv_len:pre_sv_len) = '/'
      endif
      fname=PRE_SAV_DIR(:PRE_SV_LEN)//LETOK_FILE
      OPEN(101,FILE=fname,STATUS='OLD', &
     &     IOSTAT=IERR4,ERR=300,ACCESS='SEQUENTIAL',FORM='FORMATTED')
!
! Read through file looking for a match
!
  100 CONTINUE
        READ (101, '(A2)', END=200,IOSTAT=ierr4) LETRY
        call ferr( INT2(ierr4), "Reading letters file", INT2(0), INT2(0) )
        IF (LETRY(1:2) .NE. LETRS(1:2)) GO TO 100
!
! We found a match
!
      LETOK = .TRUE.
      CLOSE (101)
      RETURN
!
! No match
!
  200 CONTINUE
      LETOK = .FALSE.
      CLOSE (101)
      RETURN
!
! Error encountered opening file
!
  300 CONTINUE
      write(*,'("Cannot access letters file ",A)') fname
      STOP
      END
