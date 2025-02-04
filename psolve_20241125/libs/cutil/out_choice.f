      SUBROUTINE OUT_CHOICE(LUOD,KERR)
!
!     CHOOSE AN OUTPUT METHOD - TERMINAL OR FILE
!     KDB 4/2/91  - created rough copy
!     KDB 8/29/91 - overhauled to make more useful to multiple subs
!
!     input:
!
!     LUOD - on input, number of fortran unit to which file should be
!             opened, if the user chooses file output.  (Necessary because
!             subroutine should not choose an arbitrary number; some
!             programs may already be using that number).
!
!     output:
!
!     LUOD - unit number to which output will be dumped
!             (if terminal is used, LUOD will be 6)
!
!     KERR - 0 = successfully set up to dump
!           -1 = user chose to cancel the dump
!
      IMPLICIT NONE
!
      INTEGER*4  IERR
      INTEGER*2 LUOD,KERR,LUOUT,LUIN
      CHARACTER*1 TF
      CHARACTER*63 DUMP_FILE
!
      DATA LUOUT /6/, LUIN /5/
!
      TF = 'L'
      DO WHILE (TF .EQ. 'L')
        WRITE(LUOUT,'(/,"(T)ERMINAL OR (F)ILE OR (C)ANCEL DUMP ",$)')
        READ (LUIN,'(A1)') TF
        IF (TF .EQ. 'T' .OR. TF .EQ. 't') THEN
          LUOD = LUOUT
          KERR = 0
        ELSE IF (TF .EQ. 'F' .OR. TF .EQ. 'f') THEN
          WRITE(LUOUT,'("NAME OF DUMP FILE ? ",$)')
          READ(LUIN,'(A63)') DUMP_FILE
          IF (DUMP_FILE(1:2) .EQ. '::') THEN
            TF = 'L'
          ELSE
            OPEN(LUOD,FILE=DUMP_FILE,IOSTAT=IERR,ERR=110,STATUS='NEW', &
     &        ACCESS='SEQUENTIAL',FORM='FORMATTED')
  110       IF (IERR.NE.0) THEN
              WRITE(LUOUT,'("ERROR ",I5," OPENING ",/,A)') &
     &          IERR,DUMP_FILE
              TF = 'L'
            ELSE
              KERR = 0
            END IF
          END IF
        ELSE IF (TF .EQ. 'c' .OR. TF .EQ. 'C') THEN
          LUOD = 6
          KERR = -1
        ELSE
          TF = 'L'
        END IF
      END DO
!
      RETURN
      END
