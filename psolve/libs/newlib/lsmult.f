      SUBROUTINE LSMULT(NUM_PATHS,PATHS,PATHLENS,MASK,MASKLEN,SORT_KEY, &
     &                  IUNITR,OUTFILE,OUTLEN,NUM_ARRAY,LS_ARRAY, &
     &                  NUM_GOTTEN,KERR)
      IMPLICIT NONE
!
!     LSMULT
!
! 1.  LSMULT PROGRAM SPECIFICATION
!
! 1.1.   LSMULT LISTS THE DISK FILES FITTING A GIVEN FILE MASK ON A GIVEN
!        SET OF DIRECTORIES.
!
! 1.2.   RESTRICTIONS - RELIES HEAVILY ON UNIX COMMANDS EXECUTED THROUGH THE
!                       SYSTEM COMMAND.
!        ASSUMPTIONS -  ASSUMES THAT THE PATHS AND FILE NAMES WILL BE TOO
!                       SHORT TO OVERFLOW THE COMMAND VARIABLES
!
! 1.3.   REFERENCES -
!
! 2.  LS INTERFACE
!
! 2.1.   CALLING SEQUENCE: CALL LSMULT(NUM_PATHS,PATHS,PATHLENS,MASK,MASKLEN,
!                                      SORT_KEY,OUTFILE,OUTLEN,
!                                      NUM_ARRAY,LS_ARRAY,NUM_GOTTEN,KERR)
!
!     INPUT VARIABLES:
!
      CHARACTER*1 SORT_KEY
      INTEGER*2 MASKLEN,PATHLENS(*),OUTLEN,NUM_PATHS,NUM_ARRAY
      CHARACTER*(*) PATHS(*),MASK,OUTFILE
      INTEGER*2 IUNITR
!
!       NUM_PATHS - NUMBER OF DIRECTORIES TO BE SEARCHED
!       PATHS - LIST OF DIRECTORIES TO BE SEARCHED
!       PATHLENS - LENGTHS OF DIRECTORY NAMES
!       MASK - STRING WHICH MUST BE CONTAINED IN THE FILE NAMES
!       MASKLEN - LENGTH OF MASK STRING.  A LENGTH <= 0 TURNS OFF MASK USAGE.
!       SORT_KEY - F TO SORT BY FILE NAME
!                  D TO LEAVE THE FILE SORTED BY DIRECTORY (DEFAULT)
!       IUNITR - fortran file unit number to be used for internal operations.
!                (Should be passed in because this routine must be available
!                for a variety of systems, and some use getunit, and others
!                don't.)
!       OUTFILE - OUTPUT FILE FOR THE FINAL RESULTS
!       OUTLEN  - LENGTH OF NAME OF OUTPUT FILE
!                        (<= 0, DISABLES THE OUTPUT FILE)
!       NUM_ARRAY = IF >0, NUMBER OF ENTRIES TO PLACE IN OUTPUT ARRAY.
!                        (<= 0, DISABLES THE OUTPUT ARRAY)
!
!     OUTPUT VARIABLES:
!
      CHARACTER*(*) LS_ARRAY
      INTEGER*2 KERR
!
!     LS_ARRAY - OUTPUT ARRAY
!     NUM_GOTTEN - number gotten in output array (or 0 if array output wasn't
!             chosen
!     KERR - ERROR RETURN
!       0 - success
!       1 - more files than array could handle
!       2 - failed to clean up work file 1
!       3 - failed to clean up work file 2
!      -1 - failed to specify output option
!      -2 - directory name plus file mask is too long
!           for command
!      -3 - ls error
!      -4 - sort error
!      -5 error creating work file
!      -6 - error creating output array
!      -7 - error creating output file
!
!
! 3.  LOCAL VARIABLES: NONE
!
      CHARACTER*255 COMMAND
      CHARACTER*4 CCALL
      INTEGER*4 IPID,GETPID,JERR
      CHARACTER*24 OUTPATH1,OUTPATH2
      INTEGER*2 OUTLENGTH1,OUTLENGTH2
      CHARACTER*2 DIR_SYMB
      INTEGER*2 ICT,CUR_LEN,SYSTEM,NUM_GOTTEN,ICALL
      INTEGER*4  IERR
      CHARACTER*155 CUR_PATH
      CHARACTER*5 CPID
      INTEGER*2 ICOMLEN
      DATA ICALL /0/
      SAVE ICALL
!
!     PROGRAMMER: K. BAVER 980313, based on catlg4/ls.f
!
!     PROGRAM STRUCTURE
!
!     For each directory in the input list, list all files fitting the input
!       mask (that is, all files whose names contain that string)
!       (if one is given).
!     Then optionally sort on the file names.
!
!     Set up ls command. (This will take the form:
!       "ls <directory> | grep -i <file_mask> | xargs -n1 echo <dirctory>
!                                                    >> <output_file>
!     which will look for all files on <directory> whose names contain the
!     <file_mask>, prepend the directory name to each file name and append the
!     list of files to the output file.
!     (NOTE: it would be easier to simply run ls /directory/ *, which
!      automatically would prepend the directory name to each file.  However,
!      some directories may have too many file names to be handled that way.
!      ALSO, this subroutine's method inserts a space between the directory
!      and file names, creating two convenient fields for sorting purposes.)
!
!     initialize
!
      KERR = 0
      COMMAND = ' '
      NUM_GOTTEN = 0
!     Number of calls made to this program
      ICALL = ICALL + 1
      WRITE(CCALL,"(I4.4)") ICALL
!
      IPID = GETPID()
      WRITE(CPID,"(I5.5)") IPID
!
!     Make sure the caller has specified at least one type of output
!       (file or array)
!
      IF (OUTLEN.LE.0 .AND. NUM_ARRAY.LE.0) THEN
        KERR = -1
        RETURN
      ENDIF
!
!     temporary files.  Uniqueness is guaranteed by the process id and the
!     number of calls made to this subroutine so far in the program
!
      OUTPATH1 = '/tmp/lsmult_1_'//CPID//'_'//CCALL
      OUTLENGTH1 = 24
      OUTPATH2 = '/tmp/lsmult_2_'//CPID//'_'//CCALL
      OUTLENGTH2 = 24
!
!     Build up the raw list of files by doing an ls for each input directory
!
      DIR_SYMB = '> '
      DO ICT = 1,NUM_PATHS
        CUR_PATH = PATHS(ICT)
        CUR_LEN = PATHLENS(ICT)
        ICOMLEN = 2*CUR_LEN + OUTLENGTH1 + 36
        IF (MASKLEN.GT.0) ICOMLEN = ICOMLEN + MASKLEN
        IF (ICOMLEN .GT.255) THEN
!         directory name plus file mask is too long for command
          KERR = -2
          RETURN
        ENDIF
        if (masklen.gt.0) then
          COMMAND = 'ls '//CUR_PATH(1:CUR_LEN)// &
     &       ' | grep '//MASK(1:MASKLEN)// &
     &       ' | xargs -n1 echo '//CUR_PATH(1:CUR_LEN)// &
     &       ' '//DIR_SYMB//' '//OUTPATH1(1:OUTLENGTH1)
        else
          COMMAND = 'ls '//CUR_PATH(1:CUR_LEN)// &
     &       ' | xargs -n1 echo '//CUR_PATH(1:CUR_LEN)// &
     &       ' '//DIR_SYMB//' '//OUTPATH1(1:OUTLENGTH1)
        endif
        CALL ZTERM(COMMAND,JERR)
        IERR = system (COMMAND)
        IF (IERR.NE.0) THEN
          KERR = -3
          RETURN
        END IF
        DIR_SYMB = '>>'
      ENDDO
!
!     Sort the concatenated list by file name, if desired.
!
      IF (SORT_KEY.EQ.'F') THEN
!
!       Sort the unsorted file into a second file
!
        COMMAND = 'sort -k2 '//OUTPATH1(1:OUTLENGTH1)// &
     &    ' > '//OUTPATH2(1:OUTLENGTH2)
        CALL ZTERM(COMMAND,JERR)
        IERR = system (COMMAND)
        IF (IERR.NE.0) THEN
          KERR = -4
          RETURN
        END IF
!
!       Remove the unsorted file.  Do not do a system call to rm -- this is
!       not supported by all installations' versions of UNIX.
!
        OPEN (IUNITR,FILE=OUTPATH1(1:OUTLENGTH1), &
     &    ERR=210,IOSTAT=IERR,STATUS='OLD')
 210    IF (IERR.NE.0) THEN
!         Not a fatal error - just failed to clean up a file
          KERR = 2
        END IF
        CLOSE(IUNITR,STATUS='DELETE')
      ELSE
!
!       Move the unsorted file into the second file to achieve consistent
!       file names by the end of this step.
!
        COMMAND = 'mv '//OUTPATH1(1:OUTLENGTH1)// &
     &    ' '//OUTPATH2(1:OUTLENGTH2)
        CALL ZTERM(COMMAND,JERR)
        IERR = system (COMMAND)
        IF (IERR.NE.0) THEN
          KERR = -5
          RETURN
        END IF
      ENDIF
!
!     If array output is desired, read the file into the output array.
!
      IF (NUM_ARRAY.GT.0) THEN
        CALL READ_TO_LIMF(IUNITR,OUTPATH2(1:OUTLENGTH2),NUM_ARRAY, &
     &      LS_ARRAY,NUM_GOTTEN,IERR)
        IF (IERR.NE.0) THEN
          IF (IERR.EQ.1) THEN
!           Not a fatal error.  Just indicates more files
!           than the program is prepared to handle.
            KERR = 1
          ELSE IF (IERR.EQ.2) THEN
!           Error reading file at the end when seeing if there
!           is additional output
            KERR = 1
          ELSE IF (IERR.EQ.-1) THEN
!           Error opening file
            KERR = -6
            RETURN
          ELSE IF (IERR.EQ.-2) THEN
!           Error reading file
            KERR = -6
            RETURN
          ENDIF
        ENDIF
      ENDIF
!
!     If file output is desired, rename the sorted file to the
!     final output file.  Otherwise, delete the sorted file
!
      IF (OUTLEN.GT.0) THEN
        COMMAND = 'mv '//OUTPATH2(1:OUTLENGTH2)// &
     &    ' '//OUTFILE(1:OUTLEN)
        CALL ZTERM(COMMAND,JERR)
        IERR = system (COMMAND)
        IF (IERR.NE.0) THEN
          KERR = -7
          RETURN
        END IF
      ELSE
        OPEN (IUNITR,FILE=OUTPATH2(1:OUTLENGTH2), &
     &    ERR=420,IOSTAT=IERR,STATUS='OLD')
 420    IF (IERR.NE.0) THEN
!         Not fatal - just failed to clean up a file
          KERR = 3
        END IF
        CLOSE(IUNITR,STATUS='DELETE')
      ENDIF
!
      RETURN
      END
