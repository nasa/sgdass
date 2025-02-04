      PROGRAM DBDIAG
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.1.   DBDIAG - rough diagnostic tool being developed to give more info
!                 after CATLG encounters a real*6/8 to real*6/8 conversion
!                 error
!
! 1.2.   RESTRICTIONS
!
! 1.3.   REFERENCES -
!
! 2.
!
! 2.1.
!
      INTEGER*2 unitdb,unit_dump,ICONVERT,ICT,irtype
!     INTEGER*2 IBN(89)
!               - target file name and DCB array
!               - save name file record buffer
      INTEGER*2 IB(2224),IB_SAVE(2224)
      INTEGER*4 IRTYPE_CT(5),ISTARTER,ISTOPPER
      INTEGER*4  IERR4
      REAL*8 RB_SAVE(555)
      CHARACTER*2 CB_SAVE(2224)
      EQUIVALENCE (IB_SAVE(1),RB_SAVE(1))
      EQUIVALENCE (IB_SAVE(1),CB_SAVE(1))
!               - scratch arrays for disk and tape records
      INTEGER*2 IBX
!
! 2.2.   COMMON BLOCKS USED: none
!
! 2.3.   DATA BASE ACCESSES:
!
! 2.4.   EXTERNAL INPUT/OUTPUT
!
!     INPUT VARIABLES:
!
!
!     OUTPUT VARIABLES:
!
!
! 2.5.   SUBROUTINE INTERFACE:
!
!
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 DE_FOUND,DR_FOUND
      INTEGER*2 IAFTER_DE
      INTEGER*2 I,IBXP1,IERR,KERR,ilen
      INTEGER*2 ISWTCH
      INTEGER*4 NREC
      INTEGER*2 TRIMLEN,DB_LEN
      INTEGER*4 ierr4,setup_brk
      CHARACTER*63 DB_FILE
      CHARACTER*63 DUMP_FILE
      CHARACTER*1 HELP_CHECK
! 4.  CONSTANTS USED: none
!
! 5.  INITIALIZED VARIABLES
!
!
! 6.  PROGRAMMER: K. Baver 2/6/91
!
!     Modifications:
!      BA  95.09.18  Dropped unused variable, changed names to other
!                    than that of intrinsic functions.
!
!     PROGRAM STRUCTURE
!
!     set up for break
      ierr4 = setup_brk()
      if (ierr4.lt.0) then
        write(6,"(' Error from setup_brk = ',i5)") ierr4
        stop
      end if
!
      DO I = 1,5
        IRTYPE_CT(I) = 0
      END DO
      IBX = 2220
      IBXP1 = 2221
      NREC=0
      DR_FOUND = .FALSE.
      DE_FOUND = .FALSE.
      IAFTER_DE = 0
      UNITDB = 61
!
      WRITE(6,'("NEED ANY INSTRUCTIONS? ",$)')
      READ(5,'(A1)') HELP_CHECK
      IF (HELP_CHECK .EQ. 'Y' .OR. HELP_CHECK .EQ. 'y') THEN
        WRITE(6,'(///)')
        WRITE(6,'("FOR HELP, READ /mk3/src/dbase/dbdiag_help")')
        WRITE(6,'(///)')
        STOP
      END IF
!
      WRITE(6,'("DATA BASE FILE TO BE DUMPED ",$)')
      READ (5,'(A63)') DB_FILE
!
      DB_LEN=TRIMLEN(DB_FILE)
      IF (DB_FILE(1:2) .EQ. "::") STOP
!     If data base is on the juke, reserve it
      CALL RSV_SWITCH( 'L', INT2(-1), DB_FILE, DB_LEN, INT2(0), IERR )
      IF (IERR.EQ.-1) THEN
        WRITE(6,'("You broke and thus the data base was not opened", &
     &   " and",/," no dump occurred.")')
        STOP
      ELSEIF (IERR.EQ.-2) THEN
        WRITE(6,'("You requested a platter which does not exist.",/, &
     &   "Please check the path and try again.")')
        STOP
      ENDIF
      OPEN (UNITDB,FILE=DB_FILE,IOSTAT=IERR4,ERR=100,STATUS='OLD', &
     &  ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
 100  CONTINUE
      IERR = IERR4
      IF (IERR.NE.0) THEN
        WRITE(6, &
     &    '("ERROR ",I5," OPENING ",/,A)')IERR,DB_FILE
        CALL RSV_SWITCH( 'U', INT2(-1), DB_FILE, DB_LEN, INT2(0), IERR )
        STOP
      END IF
!
 99   WRITE(6,'("OUTPUT FILE TO RECEIVE DUMP "/, &
     &   "    (6) FOR TERMINAL ",$)')
      READ (5,'(A63)') DUMP_FILE
      CALL SWITCH_AREA( DUMP_FILE, INT2(63), ISWTCH )
      IF (ISWTCH.EQ.1) THEN
        WRITE(6,'("You are not allowed to dump to a file ", &
     &   "on the juke.",/,"Please select a different dump file.")')
       GO TO 99
      ENDIF
      IF (DUMP_FILE(1:1) .NE. '6') THEN
        UNIT_DUMP = 62
        OPEN (UNIT_DUMP,FILE=DUMP_FILE,IOSTAT=IERR4,ERR=101, &
     &    STATUS='UNKNOWN', &
     &    ACCESS='SEQUENTIAL',FORM='FORMATTED')
      ELSE
        UNIT_DUMP = 6
        IERR = 0
      END IF
 101  CONTINUE
      IERR = IERR4
      IF (IERR.NE.0) THEN
        WRITE(6, &
     &    '("ERROR ",I5," OPENING ",/,A)')IERR,DUMP_FILE
        CLOSE (UNITDB)
        CALL RSV_SWITCH( 'U', INT2(-1), DB_FILE, DB_LEN, INT2(0), IERR )
        STOP
      END IF
 50   WRITE(6,'("NORMAL DUMP (NO CONVERSION)  (0) ",/, &
     &          "  DUMP WITH 835 TO A900 CONVERSION = (-1)")')
      READ (5,'(I2)') ICONVERT
      IF (ICONVERT .NE. 0 .AND. ICONVERT .NE. -1) GO TO 50
      WRITE(6,'("START DUMP AFTER WHICH RECORD (0 to dump all) ? ",$)')
      READ (5,'(I10)') ISTARTER
      WRITE(6,'("FINAL RECORD TO BE PROCESSED (0 to do all) ? ",$)')
      READ (5,'(I10)') ISTOPPER
!     First record.
      CALL READF(unitdb,IERR,IB,IBXP1,ilen )
!     Loop
      DO WHILE (ilen.NE.-1)
!         If buffer overflow, write message, set flag, and exit.
          IF (ilen.GT.IBX) THEN
!           THEN BEGIN OVERFLOW
              WRITE(6,"('POSSIBLE BUFFER OVERFLOW ERROR ON REC  ',I11, &
     &          '. DBDIAG TERMINATED.')") NREC+1
              CLOSE (UNITDB)
              CALL RSV_SWITCH( 'U', INT2(-1), DB_FILE, DB_LEN, INT2(0), IERR )
              IF (UNIT_DUMP .NE. 6) CLOSE (UNIT_DUMP)
              STOP
!             ENDT OVERFLOW
          END IF
          NREC=NREC+1
!
!        ****DUMP***
!
          DO ICT = 1,ilen
            IB_SAVE(ICT) = IB(ICT)
          END DO
          CALL DIAGHELP(IB,ilen,ICONVERT,DE_FOUND,DR_FOUND, &
     &         IAFTER_DE,NREC,IBX,IRTYPE,KERR )
!
          IF (IRTYPE .GT. 0 .AND. IRTYPE .LT. &
     &      6)IRTYPE_CT(IRTYPE) = IRTYPE_CT(IRTYPE) + 1
!
!         currently only knows integer*2,4 real*8,"6" alpha records
!         as the appropriate number from 1-5: calls everything else type 0
!         at the moment
!
          IF (ISTARTER .GT. 0 .AND. NREC .LT. ISTARTER) THEN
            IF (NREC/500 * 500 .EQ. &
     &        NREC)WRITE(6,'("JUST DID RECORD ",I10)') NREC
          END IF
!
          IF (NREC .GT. ISTARTER) THEN
            WRITE(UNIT_DUMP,'("RECORD ",I11," :"15X, &
     &        "type = ",i2)') NREC,IRTYPE
            IF (IRTYPE .EQ. 1 .OR. IRTYPE .EQ. 4) THEN
              WRITE (UNIT_DUMP,*,IOSTAT=IERR4, &
     &          ERR=300)(RB_SAVE(ICT),ICT=1,ilen/4)
            ELSE IF (IRTYPE .EQ. 3 .OR. IRTYPE .EQ. 6) THEN
!             alpha data rec or history rec
              WRITE (UNIT_DUMP,*,IOSTAT=IERR4, &
     &          ERR=300)(CB_SAVE(ICT),ICT=1,ilen)
            ELSE IF (IRTYPE .EQ. 0) THEN
              WRITE (UNIT_DUMP,'(a2)',IOSTAT=IERR4, &
     &          ERR=300)IB_SAVE(1)
            ELSE
              WRITE (UNIT_DUMP,*,IOSTAT=IERR4, &
     &          ERR=300)(IB_SAVE(ICT),ICT=1,ilen)
            END IF
  300       CONTINUE
            IERR = IERR4
            IF (IERR.NE.0) THEN
              WRITE(6, &
     &          '("ERROR ",I5," WRITING DUMP FILE FOR REC ",I11)')IERR,NREC
              CLOSE(UNITDB)
              CALL RSV_SWITCH( 'U', INT2(-1), DB_FILE, DB_LEN, INT2(0), IERR )
              IF (UNIT_DUMP .NE. 6) CLOSE (UNIT_DUMP)
              STOP
            END IF
          END IF
!
!         Kill off when reach the error or user has had enough
!
          IF (KERR.NE.0 .OR. (ISTOPPER.GT.0 .AND. NREC.EQ. &
     &     ISTOPPER))THEN
            WRITE(6,'("CUTTING OFF AT RECORD ",I11)') NREC
            DO I = 1,5
              WRITE(UNIT_DUMP,'("COUNTED ",I10," RECORDS OF ", &
     &          "TYPE ",I2)') IRTYPE_CT(I),I
            END DO
            CLOSE (UNITDB)
            CALL RSV_SWITCH( 'U', INT2(-1), DB_FILE, DB_LEN, INT2(0), IERR )
            IF (UNIT_DUMP .NE. 6) CLOSE (UNIT_DUMP)
            STOP
          END IF
          CALL READF(unitdb,IERR,IB,IBXP1,ilen )
      END DO
      END
