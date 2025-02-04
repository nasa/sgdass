      SUBROUTINE SILENT_TRACE(PROGRAM_NAME,LNUM,INUM,DIRECTORY)
      IMPLICIT NONE
!
!     SILENT_TRACE
!
! 1.  SILENT_TRACE PROGRAM SPECIFICATION
!
! 1.1.   SILENT_TRACE TRACES HARD-TO-DEBUG PROGRAMS (E.G., ONES BEING RUN BY
!        CRON) BY CREATING AN EMPTY TRACER FILE ON THE REQUESTED DIRECTORY.
!
! 1.2.   RESTRICTIONS - RELIES HEAVILY ON UNIX COMMANDS EXECUTED THROUGH THE
!                       SYSTEM COMMAND.
!                       ALL CALLS TO SILENT_TRACE IN A PROGRAM SHARE THE
!                         INCREMENTAL NUMBERING.
!
! 1.3.   REFERENCES -
!
!
! 2.1.   CALLING SEQUENCE:
!
!     INPUT VARIABLES:
!
!       TRACER FILES WILL HAVE THE FORM
!         silent_trace_<prog_name>_<pid>_<location_number>_<instance_number>
!            where prog_name gives the program or subroutine name
!                  pid gives the process id
!                  location_number identifies a specific program/subroutine
!                     location at which the file is made.  This number must
!                     be input as an argument, and will be printed as 4 digits.
!                  instance_number identifies a specific instance of a tracer
!                      file made at a specific program location.
!                     This may have either 5 digits or 7 digits,
!                       to distinguish between 2 numbering systems:
!                          7 digits for user specified number input as an
!                              argument
!                          5 digits for an internally generated number
!
!
!      PROGRAM_NAME = gives the program name.  (A subroutine name may also be
!                     input.)
!      LNUM - number whose absolute value is less than or equal to 9999.
!            The absolute value identifies the code location
!              at which the file was made.  (The numbers are arbitrarily chosen
!              by the programmer.)
!            Also, the sign of the number determines how the instance number
!                is set:
!                LNUM positive or zero - the next argument, INUM, is used for
!                        the instance number
!                LNUM negative - silent_trace uses an internally incremented
!                        instance number
!
!      INUM - the instance number.  May be positive, negative or zero.
!             Only used if LNUM is positive or zero.
!      DIRECTORY - output directory for tracer files
!
!     examples:
!               call silent_trace("adjst",4J,-100J,"/tmp") to create
!                            /tmp/silent_trace_adjst_01453_0004_000-100
!                         for program adjst, (process id 01453),
!                             arbitrary location 4, input value -100 (perhaps
!                              the value of a suspect variable)
!         OR
!               call silent_trace("parm_add",-8J,0J,"/data3/trace") to make
!                        /data3/trace/silent_trace_parm_add_12531_0008_00050
!                        Indicates the 50th call to silent_trace in the
!                          execution of process 12531, which occurred at
!                          arbitrary location 8 in subroutine parm_add.
!                        (Note that if there are other calls to silent_trace,
!                            this will not be the 50th call to silent_trace
!                            at location 8.)
!
!      OUTPUT VARIABLES:
!          NONE
!
      CHARACTER*(*) PROGRAM_NAME
      INTEGER*4 INUM
      CHARACTER*(*) DIRECTORY
      INTEGER*4 LNUM
!
!
! 3.  LOCAL VARIABLES: NONE
!
      CHARACTER*255 COMMAND
      INTEGER*4 IPID,GETPID
      INTEGER*2 IERR2,SYSTEM,TRIMLEN
      INTEGER*2 PROG_LEN,TRACE_LEN,DIR_LEN,ICT
      CHARACTER*4 CLOCATION
      CHARACTER*5 CPID
      CHARACTER*7 CNUMBER
      INTEGER*4 INT_NUM
      CHARACTER*255 PROG_NAME,TRACER_NAME,DIR_NAME
      INTEGER*4 LOCNUM
      INTEGER*4 P40
!
      SAVE INT_NUM
      DATA INT_NUM /0/
!
      DATA P40 /0/
!
!     PROGRAMMER: K. BAVER 981125
!
!     PROGRAM STRUCTURE
!
!     Determine identifying instance number for file name.
!
      LOCNUM = LNUM
!
      IF (LOCNUM.GE.P40) THEN
!       use specific number input as argument
        WRITE(CNUMBER,"(I7)") INUM
        DO ICT = 1,7
          IF (CNUMBER(ICT:ICT).EQ.' ') CNUMBER(ICT:ICT) = '0'
        ENDDO
      ELSE
!       use internally generated number
        INT_NUM = INT_NUM + 1
        WRITE(CNUMBER,"(I5.5,2X)") INT_NUM
      ENDIF
!
      IF (LOCNUM.LT.P40) LOCNUM = -1 * LOCNUM
      WRITE(CLOCATION,"(I4.4)") LOCNUM
!
!     Get process id as character string
!
      IPID = GETPID()
      WRITE(CPID,"(I5.5)") IPID
!
!     Set up tracer file name
!
      PROG_NAME = PROGRAM_NAME
      PROG_LEN = TRIMLEN(PROG_NAME)
      TRACER_NAME = "silent_trace_"// &
     &    PROG_NAME(1:PROG_LEN)//"_"//CPID//"_"//CLOCATION//"_"//CNUMBER
      TRACE_LEN = TRIMLEN(TRACER_NAME)
!
!     Create the empty tracer file by touching it.
!
      DIR_NAME = DIRECTORY
      DIR_LEN = TRIMLEN(DIR_NAME)
      COMMAND = 'touch '//DIR_NAME(1:DIR_LEN)//'/' &
     &      //TRACER_NAME(1:TRACE_LEN)//CHAR(0)
!
      IERR2 = system(COMMAND)
!
      RETURN
      END
