      SUBROUTINE USE_SPLLK(FILDIR,FILBASE,IFILLU,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_SPLLK PROGRAM SPECIFICATION
!
! 1.1 ACCESS UTILITY FOR A FILE WHICH IS TO BE HANDLED IN THE
!     SAME WAY THAT THE SPOOL FILE IS HANDLED.
!     THIS VERSION REQUIRES THAT SOCOM BE LOADED BEFORE IT CALL FOR 'O'
!     AND THAT SOCOM BE WRITTEN OUT AFTER A CALL FOR 'C'
!     IT ALSO REQUIRES PRE_PROG TO HAVE BEEN USED
!
! 1.2 REFERENCES:
!
! 2.  USE_SPLLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING,FILDIR,FILBASE
      INTEGER*2 IFILLU
!
!     FILDIR + / + FILBASE +  user_initials = path to file to be accessed
!
!     IFILLU - lu to be used for accessing file
!
!     STRING - CHARACTER STRING WITH ONE CHARACTER PER OPERATION
!              O - OPEN
!              I - INITIALIZE
!              C - CLOSE
!              S - SEEK THE END OF FILE
!
! 2.3 OUTPUT Variables: none
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: glbset, eof_spllk, third, third_intermed, option
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 COUNT,MCOUNT,TRIMLEN,IL,IDUM
      INTEGER*4 IERR, EOFPOS,I4DUM,dum,fc_lseek,offset,whence,fd
      INTEGER*4 UNIT_TO_FILDESC
      CHARACTER*63 FNAME
      CHARACTER*1 TOKEN
      CHARACTER*150 errstr
      LOGICAL*2 eof
      INTEGER*2 INT2_ARG
      INTEGER*4 INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!  WHO  WHEN       WHAT
!  AEE 910515      Enhanced error messages written to the error file.
!  MWH 910912      Use lseek instead of freopen to position at end of file
!  pet 2003.09.24  Fixed  a bug related to type of the argument for &
!                  UNIT_TO_FILDESC
!
! 5.  USE_SPLLK PROGRAM STRUCTURE
!
      FNAME=FILDIR(:trimlen(fildir))// FILBASE//PRE_LETRS
      IL=TRIMLEN(FNAME)
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
        TOKEN=STRING(COUNT:COUNT)
1       CONTINUE
!
!     Open
!
      IF ( TOKEN .EQ. 'O' ) THEN
           OPEN(IFILLU,FILE=FNAME(1:IL),IOSTAT=IERR)
           IF ( IERR .NE. 0 ) THEN
                CALL FERR( INT2(224), ERRSTR, INT2(0), INT2(0) )
                GOTO 1
           ENDIF
           FD = UNIT_TO_FILDESC ( INT4(IFILLU) )
           OFFSET=0
           WHENCE=2
           DUM = FC_LSEEK ( FD, OFFSET, WHENCE )
#ifdef GNU
              CALL FSEEK ( INT4(IFILLU), 0, 2, DUM )
#else
              DUM = FC_LSEEK  ( FD, OFFSET, WHENCE )
#endif
           CALL FATAL_FILE ( DUM, 'seeking', FNAME, 'ftn_open' )
!
!     INITIALIZE
!
        ELSE IF(TOKEN.EQ.'I') THEN
          REWIND(IFILLU,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' rewinding 1, ',A)") IERR, &
     &                      FNAME(1:IL)
             call ferr( INT2(226), errstr, INT2(0), INT2(0) )
             GOTO 1
          END IF
          ENDFILE(IFILLU,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' endfile, ',A)") IERR, &
     &                      FNAME(1:IL)
             call ferr( INT2(227), errstr, INT2(0), INT2(0) )
             GOTO 1
          END IF
          REWIND(IFILLU,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' rewinding 2, ',A)") IERR, &
     &                      FNAME(1:IL)
             call ferr( INT2(228), errstr, INT2(0), INT2(0) )
             GOTO 1
          END IF
!
!  CLOSE
!
        ELSE IF(TOKEN.EQ.'C') THEN
          CLOSE(IFILLU,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' closing, ',A)") IERR, &
     &                      FNAME(1:IL)
             call ferr( INT2(229), errstr, INT2(0), INT2(0) )
             GOTO 1
          END IF
!
!  SEEK END OF FILE
!
        ELSE IF(TOKEN.EQ.'S') THEN
          REWIND(IFILLU,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' on rewind, ',A)") IERR, &
     &                      FNAME(1:IL)
            call ferr( INT2(230), errstr, INT2(0), INT2(0) )
            GO TO 1
          ENDIF
98        CONTINUE
          READ(IFILLU,'(A2)',END=99,IOSTAT=IERR) IDUM
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' reading, ',A)") IERR, &
     &                      FNAME(1:IL)
            call ferr( INT2(231), errstr, INT2(0), INT2(0) )
            GOTO 1
          ENDIF
          GOTO 98
99        CONTINUE
!
!  UNKOWN CONTROL
!
        ELSE
          WRITE(errstr,"('Unknown USE_SPLLK access control: ',A)") TOKEN
          call ferr( INT2(232), errstr, INT2(0), INT2(0) )
          GO TO 1
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END
