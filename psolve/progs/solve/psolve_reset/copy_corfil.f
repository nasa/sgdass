      SUBROUTINE COPY_CORFIL(CORFI,CORFO)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COPY_CORFIL PROGRAM SPECIFICATION
!
! 1.1 Copy the standard CORFIL into the current scratch copy.
!
! 1.2 REFERENCES:
!
! 2.  COPY_CORFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) CORFI
!
! CORFI - Version of CORFIL to be copied *from*
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) CORFO
!
! CORFO - Version of CORFIL to be copied *to*
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
      INTEGER*2    ILAST, ITYPE, ICOUNT, TRIMLEN
      INTEGER*4    IERR
      CHARACTER*80 CBUF
!
! 4.  HISTORY
!   WHO     WHEN     WHAT
!
!   KDB  91/10/23    revised for current corfil.  (current corfil separates
!                    flyby and non-flyby calibrations)  Biggest effect on
!                    this subroutine:  comments now labelled with 2000,
!                    not section number.
!   jwr  04/04/09    Added logic to error exit if the CORFIL template is empty.
!
! 5.  COPY_CORFIL PROGRAM STRUCTURE
!
! Open the input file
!
      OPEN(301,FILE=CORFI,IOSTAT=IERR,STATUS='OLD')
      CALL FERR( INT2(IERR), 'opening '//CORFI, INT2(0), INT2(0) )
!
! Open the output file
!
      OPEN(302,FILE=CORFO,IOSTAT=IERR,STATUS='OLD')
      CALL FERR( INT2(IERR), 'opening '//CORFO, INT2(0), INT2(0) )
!
! Loop to read input and write output, omitting all comment
!  lines and blank lines.  (Section header lines get copied.)
!
      ICOUNT = 0
      DO WHILE(.TRUE.)
         READ(301,'(A)',IOSTAT=IERR,END=900) CBUF
         CALL FERR( INT2(IERR), 'reading '//CORFI, INT2(0), INT2(0) )
         READ(CBUF,'(I5)',IOSTAT=IERR) ITYPE
         CALL FERR( INT2(IERR), 'decoding CORFIL record', INT2(0), INT2(0) )
         ICOUNT = ICOUNT+1
         IF ( ITYPE.NE.2000 .AND. ITYPE.NE.0 ) THEN
              WRITE(302,'(A)',IOSTAT=IERR) CBUF(:TRIMLEN(CBUF))
              CALL FERR( INT2(IERR), 'writing '//CORFO, INT2(0), INT2(0) )
         ENDIF
      ENDDO
!
! Close files and exit; display error message if appropriate
!
900   CONTINUE
      CLOSE(301,IOSTAT=IERR)
      CALL FERR( INT2(IERR), 'closing '//CORFI, INT2(0), INT2(0) )
      CLOSE(302,IOSTAT=IERR)
      CALL FERR( INT2(IERR), 'closing '//CORFO, INT2(0), INT2(0) )
!
      IF ( ICOUNT .EQ. 0 ) THEN
           CALL FERR( INT2(1), 'empty '//CORFO, INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END

