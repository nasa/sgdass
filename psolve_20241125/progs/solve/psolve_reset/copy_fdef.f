      SUBROUTINE COPY_FDEF(CORFI,CORFO)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COPY_FDEF PROGRAM SPECIFICATION
!
! 1.1 Copy the flyby/default standard  into the current scratch copy.
!
! 1.2 REFERENCES:
!
! 2.  COPY_FDEF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) CORFI
!
! CORFI - Version of FDEF to be copied *from*
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) CORFO
!
! CORFO - Version of FDEF to be copied *to*
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: solve
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  ILAST, ITYPE, TRIMLEN
      INTEGER*4  MIN_LINES, NLINES, IERR
      CHARACTER  CBUF*160, STR*160
      PARAMETER  ( MIN_LINES = 12 )
      INTEGER*4  I_LEN
!
! 4.  HISTORY
!   WHO     WHEN     WHAT
!
!   MWH  95/02/28    Created, based on cpoy_corfil
!   pet  2003.08.27  Added check, whether FDEFxx has at list the minimum number &
!                    of lines
!   pet  2004.12.17  Reduced MIN_NLINES from 16 to 12
!
! 5.  COPY_FDEF PROGRAM STRUCTURE
!
! --- Open the input file
!
      OPEN ( 301, FILE=CORFI, IOSTAT=IERR, STATUS='OLD' )
      CALL FERR ( INT2(IERR), 'opening '//CORFI, INT2(0), INT2(0) )
!
! --- Open the output file
!
      OPEN(302,FILE=CORFO,IOSTAT=IERR,STATUS='OLD')
      CALL FERR( INT2(IERR), 'opening '//CORFO, INT2(0), INT2(0) )
!
! --- Loop to read input and write output
!
      NLINES = 0
      DO WHILE(.TRUE.)
         READ(301,'(A)',IOSTAT=IERR,END=900) CBUF
         CALL FERR( INT2(IERR), 'reading '//CORFI, INT2(0), INT2(0) )
         WRITE(302,'(A)',IOSTAT=IERR) CBUF(:TRIMLEN(CBUF))
         CALL FERR( INT2(IERR), 'writing '//CORFO, INT2(0), INT2(0) )
         NLINES = NLINES + 1
      ENDDO
!
! --- Close files and exit; display error message if appropriate
!
900   CONTINUE
      CLOSE(301,IOSTAT=IERR)
      CALL FERR( INT2(IERR), 'closing '//CORFI, INT2(0), INT2(0) )
      CLOSE(302,IOSTAT=IERR)
      CALL FERR( INT2(IERR), 'closing '//CORFO, INT2(0), INT2(0) )
      IF ( NLINES .LT. MIN_LINES ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NLINES, STR )
           CALL ERR_LOG ( 2223, -1, 'COPY_FDDEF', 'File '// &
     &          CORFI(1:I_LEN(CORFI))//' is too short: only '// &
     &          STR(1:I_LEN(STR))//' lines' )
           CALL EXIT ( 1 )
      END IF
!
      RETURN
      END
