      SUBROUTINE READ_TO_LIMF (IUNIT,FNAME,LIMIT, &
     &                       BUFFERS,NUM_BUFF,KERR)
!
      implicit none
!
!
! 1.  READ_TO_LIMF PROGRAM SPECIFICATION
!
! 1.1 Read the appropriate file to get an array of character buffers.
!     Reads up to LIMIT number of buffers.
!
! 1.2 REFERENCES:
!
! 2.  READ_TO_LIMF INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IUNIT,LIMIT
      CHARACTER*(*) FNAME
!
!     iunit - unit number the calling program wants to use to read
!             the file.
!     fname - name of file to be read
!     limit - maximum number of buffers the user wants to read
!             from the file
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) BUFFERS(*)
      INTEGER*2 NUM_BUFF,KERR
!
! BUFFERS - array of buffers read from file
! NUM_BUFF - number of file lines read into the buffer array
! KERR - error return
!        0 = ok (read the entire availability file)
!       -1 = open error
!       -2 = read error, when still trying to get requested number of calibs
!        1 = more in file than requested
!        2 = read error, when doing final read to see if more in file than
!            requested (may or may not be more in file than requested)
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  IERR
      INTEGER*2  TRIMLEN
      character*255 buf
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  980318  Created based on cutil/flyavl_n.f
!
! 5.  READ_TO_LIMF PROGRAM STRUCTURE
!
!     Just read the input file and pull out each line, up to the
!     limit the user requested.
!
      OPEN (IUNIT,FILE=fname,IOSTAT=IERR,ERR=100, &
     &      STATUS='OLD',ACCESS='SEQUENTIAL',FORM='FORMATTED')
 100  IF (IERR.NE.0) THEN
        KERR = -1
        GO TO 900
      END IF
      NUM_BUFF = 0
      DO WHILE (NUM_BUFF.LT.LIMIT)
        READ (IUNIT,'(A)',IOSTAT=IERR,ERR=200,END=300) buf
        NUM_BUFF = NUM_BUFF + 1
        BUFFERS(NUM_BUFF) = BUF(1:TRIMLEN(BUF))
      END DO
!
!     The subroutine stopped reading because it reached the limit requested
!     by the user.  See if there were more left, to warn the user.
!
      READ(IUNIT,'(A)',IOSTAT=IERR,ERR=201,END=301) BUF
      CLOSE(IUNIT)
      KERR = 1
      GO TO 900
 201  CLOSE(IUNIT)
      KERR = 2
      GO TO 900
 301  CLOSE(IUNIT)
      KERR = 0
      GO TO 900
!
!     error and eof from normal read to pull off list
!
 200  CLOSE(IUNIT)
      KERR = -2
      GO TO 900
 300  CLOSE (IUNIT)
      KERR = 0
!
 900  RETURN
      END
