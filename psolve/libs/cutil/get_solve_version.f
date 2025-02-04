      SUBROUTINE GET_SOLVE_VERSION ( RELEASE_DATE, REVISION_DATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_SOLVE_VERSION  returnds Solve release_date and Solve  *
! *   revision date. These dates are kept in the files                   *
! *   $PSOLVE_ROOT/bin/RELEASE_DATE and $MK4_ROOT/bin/REVISION_DATE      *
! *   respectively.                                                      *
! *                                                                      *
! * ### 17-NOV-1999 GET_SOLVE_VERSION v2.2 (c) L. Petrov 14-MAY-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      CHARACTER  RELEASE_DATE*(*), REVISION_DATE*(*)
      INTEGER*4  IUER
      CHARACTER  DIR_NAME*128, REVISION_NAME*128, RELEASE_NAME*128, STR*32
      INTEGER*4  I11, LUN
      LOGICAL*4  LEX
      INTEGER*4  ILEN, I_LEN, GET_UNIT
!
      CALL CLRCH ( RELEASE_DATE  )
      CALL CLRCH ( REVISION_DATE )
!
! --- Deterimine the name of the mk4 binary directory
!
      CALL CLRCH  ( DIR_NAME )
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', DIR_NAME )
      IF ( ILEN(DIR_NAME) .EQ. 0 ) THEN
           DIR_NAME = SOLVE_WORK_DIR
      END IF
      IF ( DIR_NAME(I_LEN(DIR_NAME):I_LEN(DIR_NAME)) .NE. '/' ) THEN
           DIR_NAME = DIR_NAME(1:I_LEN(DIR_NAME))//'/'
      END IF
      RELEASE_NAME  = DIR_NAME(1:I_LEN(DIR_NAME))//RELEASE_FILE
      REVISION_NAME = DIR_NAME(1:ILEN(DIR_NAME))//REVISION_FILE
!
      INQUIRE ( FILE=RELEASE_NAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6401, IUER, 'GET_SOLVE_VERSION', 'The file with '// &
     &                   'solve release date: '// &
     &                    RELEASE_NAME(1:I_LEN(RELEASE_NAME))//' has not '// &
     &                   'been found' )
           RETURN
      END IF
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=RELEASE_NAME, STATUS='OLD', IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I11, STR )
           CALL ERR_LOG ( 6402, IUER, 'GET_SOLVE_VERSION', 'Error opening '// &
     &                    'solve release date file: '// &
     &                    RELEASE_NAME(1:I_LEN(RELEASE_NAME))//' -- IOSTAT='// &
     &                    STR )
           RETURN
      END IF
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=I11 ) RELEASE_DATE
      IF ( I11 .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I11, STR )
           CALL ERR_LOG ( 6403, IUER, 'GET_SOLVE_VERSION', 'Error in '// &
     &                    'reading solve release date file: '// &
     &                    RELEASE_DATE(1:I_LEN(RELEASE_DATE))//' -- IOSTAT='// &
     &                    STR )
           RETURN
      END IF
      CLOSE ( UNIT=LUN )
!
      INQUIRE ( FILE=REVISION_NAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6404, IUER, 'GET_SOLVE_VERSION', 'The file with '// &
     &                   'solve revision date: '// &
     &                    REVISION_NAME(1:I_LEN(REVISION_NAME))//' has not '// &
     &                   'been found' )
           RETURN
      END IF
!
      OPEN ( UNIT=LUN, FILE=REVISION_NAME, STATUS='OLD', IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I11, STR )
           CALL ERR_LOG ( 6405, IUER, 'GET_SOLVE_VERSION', &
     &         'Error opening solve revision date file: '// &
     &          REVISION_DATE(1:I_LEN(REVISION_DATE))//' -- IOSTAT='//STR )
           RETURN
      END IF
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=I11 ) REVISION_DATE
      IF ( I11 .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I11, STR )
           CALL ERR_LOG ( 6406, IUER, 'GET_SOLVE_VERSION', 'Error in '// &
     &         'reading solve revision date file: '// &
     &          REVISION_DATE(1:I_LEN(REVISION_DATE))//' -- IOSTAT='//STR )
           RETURN
      END IF
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_SOLVE_VERSION  #!#
