      SUBROUTINE USE_RESNDX(STRING,fname)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_RESNDX PROGRAM SPECIFICATION
!
! 1.1 Access utility for residual output index file.  This is an ASCII
!     file that contains index information needed to interpred the
!     binary file created to hold residuals for a batch run
!
! 1.2 REFERENCES:
!
! 2.  USE_RESNDX INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING,fname
!
! STRING - Requested access type: O - open
!                                 I - initialize
!                                 C - close
!                                 S - seek end of file
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fcfreopen
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 COUNT,MCOUNT,TRIMLEN,IL,IDUM
      INTEGER*4 IERR
      INTEGER*4 EOFPOS,I4DUM,dum,offset,whence,fd
      INTEGER*4 UNIT_TO_FILDESC
      CHARACTER*1 TOKEN
      LOGICAL*2 eof
      CHARACTER*130 errstr
      character*64 cbuf
      integer*2 ibuf(32)
      equivalence (ibuf,cbuf)
!
! COUNT - Number of STRING character currently being processed
! FNAME - Name of the spool file
! IDUM - Dummy to read into while seeking end of file
! IERR - IOSTAT return from OPEN, REWIND, etc.
! IL - Length of file name
! MCOUNT - Number of characters in STRING
! TOKEN - Single character from STRING being processed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  940209  Created, based on use_spool
!
! 5.  USE_RESNDX PROGRAM STRUCTURE
!
! Build name of spool file
!
!
      IL=TRIMLEN(FNAME)
!
! Loop through characters in input STRING
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
        TOKEN=STRING(COUNT:COUNT)
1       CONTINUE
!
!     Open
!
        IF(TOKEN.EQ.'O') THEN
          OPEN(37,FILE=FNAME(1:IL),IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' opening ',A)") IERR,FNAME(1:IL)
            call ferr( INT2(233), errstr, INT2(0), INT2(0) )
            GO TO 1
          ENDIF
          fd = UNIT_TO_FILDESC(37)
          offset=0
          whence=2
#ifdef GNU
          CALL FSEEK ( 37, 0, 2, DUM )
#else
          dum=fc_lseek(fd,offset,whence)
#endif
          call fatal_file(dum,'seeking',fname,'ftn_open' )
!
!     INITIALIZE
!
        ELSE IF(TOKEN.EQ.'I') THEN
          REWIND(37,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' rewinding 1, ',A)") IERR, FNAME(1:IL)
            call ferr( INT2(235), errstr, INT2(0), INT2(0) )
             GO TO 1
          END IF
          ENDFILE(37,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' endfile, ',A)") IERR, &
     &                      FNAME(1:IL)
            call ferr( INT2(236), errstr, INT2(0), INT2(0) )
             GO TO 1
          END IF
          REWIND(37,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' rewinding 2, ',A)") IERR, &
     &                     FNAME(1:IL)
            call ferr( INT2(237), errstr, INT2(0), INT2(0) )
             GO TO 1
          END IF
!
!  CLOSE
!
        ELSE IF(TOKEN.EQ.'C') THEN
          CLOSE(37,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' closing, ',A)") IERR, &
     &                     FNAME(1:IL)
            call ferr( INT2(238), errstr, INT2(0), INT2(0) )
             GO TO 1
          END IF
!
!  SEEK END OF FILE
!
        ELSE IF(TOKEN.EQ.'S') THEN
          REWIND(37,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' closing, ',A)") IERR, &
     &                      FNAME(1:IL)
            call ferr( INT2(239), errstr, INT2(0), INT2(0) )
            GO TO 1
          ENDIF
98        CONTINUE
          READ(37,'(A2)',END=99,IOSTAT=IERR) IDUM
          IF(IERR.NE.0) THEN
            WRITE(errstr,"('Error ',I7,' reading, ',A)") IERR, &
     &                      FNAME(1:IL)
            call ferr( INT2(240), errstr, INT2(0), INT2(0) )
            GO TO 1
          ENDIF
          GOTO 98
99        CONTINUE
!
!  UNKOWN CONTROL
!
        ELSE
         WRITE(errstr,"('Unknown USE_RESNDX access control: ',A)") TOKEN
            call ferr( INT2(241), errstr, INT2(0), INT2(0) )
          GO TO 1
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END
