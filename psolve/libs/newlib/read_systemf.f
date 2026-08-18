      SUBROUTINE READ_SYSTEMF ( COMMAND, MXOUT, OUTPUT, NOUT, MESSAGE, KERR )
!
!     READ_SYSTEMF
!
      implicit none
!
! 1.  READ_SYSTEMF PROGRAM SPECIFICATION
!
! 1.1.   READ_SYSTEMF executes the given command via a system call
!        and returns the command's output to the calling sub.
!
! 1.2.   RESTRICTIONS - This sub was developed to run shell commands
!        (e.g., grep), which produce one or more lines of output,
!        without requiring further input from the user.  The sub will
!        return the output as a set of lines.  It is up to the caller
!        to parse the lines into fields, if necessary.
!
!
!
!
!
!
!
!
!
!
! 1.3.   REFERENCES - none
!
! 2.  READ_SYSTEMF INTERFACE
!
! 2.1.   CALLING SEQUENCE:
!
!     INPUT VARIABLES:
!
!     COMMAND - the command to be executed.
!                 MUST NOT BE ZERO TERMINATED
!     MXOUT - maximum number of lines of output to be
!             returned.
!
      CHARACTER*(*) COMMAND
      INTEGER*2 MXOUT
!
!     OUTPUT VARIABLES:
!
!     OUTPUT - output from the command
!     NOUT - number of pieces of output
!     MESSAGE  - error message
!     KERR - error return
!
      CHARACTER*(*) MESSAGE
      CHARACTER*(*) OUTPUT(1)
      INTEGER*2 KERR,NOUT
      INTEGER*4  I_LEN
!
! 2.2.   COMMON BLOCKS USED: none
!
! 2.3.   DATA BASE ACCESSES: none
!
! 2.4.   EXTERNAL INPUT/OUTPUT: none
!
! 2.5.   SUBROUTINE INTERFACE:
!
!     CALLING SUBROUTINES: utility
!
!     CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 IPID,GETPID
      CHARACTER*5 CPID,CNUM1,CNUM2,CNUM
      CHARACTER*10 CNUM10
      CHARACTER*22 RESULTFILE
      CHARACTER*255 RESULTBUF,NEW_COMMAND
      INTEGER*2     IERR, TRIMLEN,IELEN
      INTEGER*4     IERR4
      INTEGER*4     SYSTEM
!
! 4.  CONSTANTS USED:
!
! 5.  INITIALIZED VARIABLES:
!
! 6.  PROGRAMMER: K. Baver 12/18/92
!
!     LAST MODIFIED:
!
!     PROGRAM STRUCTURE
!
!     This sub will redirect the results of the command to a
!     file, then read the file into the output line array,
!     line by line.
!
!     pet  2003.08.06  Improved error messages. Corrected type of SYSTEM &
!                      routine: it should be integer*4
!
!     Initialize.
!
      KERR = -999
      IPID = GETPID()
      WRITE(CPID,"(I5.5)") IPID
      RESULTFILE = '/tmp/read_systemf_'//CPID
      IELEN = TRIMLEN(COMMAND)
      NEW_COMMAND = COMMAND(1:IELEN) // ' > '// RESULTFILE
!
      CALL ZTERM(NEW_COMMAND,IERR4)
      IF ( IERR4 .NE. 0 ) THEN
           WRITE(CNUM10,"(I10)") IERR4
           MESSAGE = ':READ_SYSTEMF *1* Error in ZTERM '//CNUM10// &
     &         ' Setting up command "'//NEW_COMMAND(1:I_LEN(NEW_COMMAND))//'"'
           KERR = -1
           RETURN
      END IF
!
      IERR4 = SYSTEM ( NEW_COMMAND )
      IERR = IERR4
      IF ( IERR .NE.  0) THEN
           WRITE(CNUM1,"(I5)") IERR
           WRITE(CNUM2,"(I5)") IERR
           MESSAGE = ':READ_SYSTEMF *1* Error in SYSTEM '//CNUM1//' & '// &
     &               CNUM2//' Performing command '//NEW_COMMAND 
        KERR = -2
        RETURN
      END IF
!
!     Read the results of the command from the file to which
!     they were sent.  Place the results in the output
!     line array.
!
      OPEN ( 82, FILE=RESULTFILE, IOSTAT=IERR4, ERR=101, STATUS='OLD', &
     &       ACCESS='SEQUENTIAL', FORM='FORMATTED' )
 101  IF ( IERR4 .NE. 0 ) THEN
           WRITE(CNUM,"(I5)") IERR4
           MESSAGE = ':READ_SYSTEMF *3* Error '//CNUM// &
     &               ' Opening result file '//RESULTFILE
           KERR = -3
           RETURN
      END IF
      NOUT = 0
      DO WHILE (NOUT .GE. 0)
         READ ( 82, "(A)", IOSTAT=IERR4, ERR=102, END=103 ) RESULTBUF
         NOUT = NOUT + 1
         IF ( NOUT .GT. MXOUT ) THEN
              WRITE(CNUM,"(I5)") MXOUT
              MESSAGE = ':READ_SYSTEMF *5* Error: '//CNUM// &
     &                  ' Output lines expected -- '// &
     &                  'More than that produced in file '//RESULTFILE
              KERR = -5
              CLOSE(82)
              NOUT = NOUT - 1
              RETURN
         END IF
         IELEN = TRIMLEN(RESULTBUF)
         OUTPUT(NOUT) = RESULTBUF(1:IELEN)
      END DO
!
!     Success
!
 103  CLOSE (82,STATUS='DELETE')
      KERR = 0
      RETURN
!
!     Case where error reading result file
!
 102  WRITE(CNUM,"(I5)") IERR4
      MESSAGE = ':READ_SYSTEMF *5* Error: '//CNUM//' Reading result file '// &
     &          RESULTFILE
      KERR = -4
      CLOSE(82)
!
      RETURN
      END  !#!  READ_SYSTEMF  #!#
