      SUBROUTINE CHECK_SOLVE_LOCK ()
! ************************************************************************
! *                                                                      *
! *   Routine CHECK_SOLVE_LOCK
! *                                                                      *
! * ### 28-APR-2007 CHECK_SOLVE_LOCK v1.0 (c) L. Petrov 17-MAY-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      LOGICAL*4  FL_EXIST
      CHARACTER  LOCK_FILE*128, STR*256, STR_PID*128
      INTEGER*4  IOS, IB, IE, IK, LUN, PID
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, KILL
!
      LOCK_FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'LOCK'//PRE_LETRS
      INQUIRE ( FILE=LOCK_FILE, EXIST=FL_EXIST )
!
      IF ( FL_EXIST ) THEN
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=LOCK_FILE, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR ) 
                CALL ERR_LOG ( 1801, -2, 'CHECK_SOLVE_LOCK', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open lock '// &
     &              'file '//LOCK_FILE )
               CALL EXIT ( 1 )
           END IF
           READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR ) 
                CALL ERR_LOG ( 1802, -2, 'CHECK_SOLVE_LOCK', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to read lock file '// &
     &               LOCK_FILE(1:I_LEN(LOCK_FILE))//' -- the file perhaps '// &
     &               'is spoiled. Please remove it manually' )
               CALL EXIT ( 1 )
           END IF
           PID = 0
           IB = INDEX ( STR, 'proc ID' )
           IF ( IB > 0 ) THEN
                STR_PID = STR(IB+7:)
                CALL CHASHL ( STR_PID )
                IE = INDEX  ( STR_PID, ' ' )
                IF ( IE .GE. 2 ) THEN
                     CALL CHIN   ( STR_PID(1:IE), PID )
                END IF                
           END IF
           IF ( PID > 0 ) THEN
                IK = KILL ( %VAL(PID), %VAL(0) )
                IF ( IK == 0 ) THEN
                     WRITE ( 6, '(A)' ) '*********************************'// &
     &                                  '*********************************'// &
     &                                  '************'
                     WRITE ( 6, '(A)' ) 'Solve initials '// &
     &                                   PRE_LETRS(1:I_LEN(PRE_LETRS))// &
     &                                  ' are locked!'
                     WRITE ( 6, '(A)' ) STR(1:I_LEN(STR))
                     WRITE ( 6, '(A)' ) 'Lock file: '// &
     &                                   LOCK_FILE(1:I_LEN(LOCK_FILE))
                     WRITE ( 6, '(A)' ) 'Check, whether you have another '// &
     &                     'Solve process running under these initials.'
                     WRITE ( 6, '(A)' ) 'You should not have more than '// &
     &                                  'one Solve process under the same '// &
     &                                  'initals!'
                     WRITE ( 6, '(A)' ) 'If no Solve process runs under this '// &
     &                                  'initials, you can try to remove '// &
     &                                  'lock file'
                     WRITE ( 6, '(A)' ) '*********************************'// &
     &                                  '*********************************'// &
     &                                  '************'
                     WRITE ( 6, '(A)' ) 'Solve terminates now'
                     CALL EXIT ( 1 )
                  ELSE 
!
! ------------------ Remove stale file
!
                     CALL UNLINK ( LOCK_FILE(1:I_LEN(LOCK_FILE))//CHAR(0) )
                END IF
           END IF
           CLOSE ( LUN )
         ELSE 
           RETURN 
      END IF
      RETURN
      END  SUBROUTINE  CHECK_SOLVE_LOCK  !#!  
