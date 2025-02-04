      SUBROUTINE STATUS_SET ( NAME, ACTION )
! ************************************************************************
! *                                                                      *
! *   Routine  STATUS_SET  sets status of processing parameters          *
! *   estimation. It gets the name of the component NAME, its action:    *
! *   "started" or "done". It compute the database name, its version,    *
! *   learn the total number of the databases to be processed, the       *
! *   number of the current session an d generate a status line.         *
! *   This line is written in the file $WORK_DIR/STATxx, weere xx are    *
! *   SOLVE user initials. If FAST_DBG = F_MON then this line is written *
! *   in the screen too.                                                 *
! *                                                                      *
! *   Comment:                                                           *
! *      It is assumed that glbcm  and glbc4  were read already;         *
! *      It is assumed that the database was read already and its name   *
! *         is saved in namefil.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   NAME   ( CHARACTER ) -- Name of the component (program). Any       *
! *                           string up to 8 symbols long.               *
! *                           Comment: if the symbol of name is "-" then *
! *                           it is dropped and the name of the session  *
! *                           is not printed.                            *
! *   ACTION ( INTEGER*4 ) -- Action code (defined in solve.i). Only     *
! *                           codes  STA__BEG, STA__END, STA__INI and    *
! *                           STA__INT are allowed.                      *
! *                                                                      *
! *  ###  05-APR-99   STATUS_SET   v1.6  (c)  L. Petrov 05-JUL-2019 ###  * 
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'precm.i'
      INCLUDE   'socom.i' 
      CHARACTER  NAME*(*)
      INTEGER*4  ACTION
      INTEGER*4  I40
      CHARACTER  DBNAME_MES*32, STATUS_FILE*160, CUR_SESS*8, TOT_SESS*8, &
     &           ACTION_STR*7, OUT*80, WORK_DIR*160, NAME_COMP*80, STR_PID*8
      LOGICAL*4  F_HYPHEN
      INTEGER*4  PID
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*4, EXTERNAL :: FSTREAM, GETPID, ILEN, I_LEN
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Set string with action
!
      CALL CLRCH ( ACTION_STR )
      IF ( ACTION .EQ. STA__BEG ) THEN
           ACTION_STR = 'started'
         ELSE IF ( ACTION .EQ. STA__END ) THEN
           ACTION_STR = 'done'
         ELSE IF ( ACTION .EQ. STA__INI ) THEN
           ACTION_STR = 'initial'
         ELSE IF ( ACTION .EQ. STA__INT ) THEN
           ACTION_STR = 'interac'
         ELSE IF ( ACTION .EQ. STA__END ) THEN
           ACTION_STR = 'done'
         ELSE IF ( ACTION .EQ. STA__SUC ) THEN
           ACTION_STR = 'completed '
         ELSE
           WRITE ( 6, * ) ' Wrong value action: ', action
           action_str(5000+iabs(action):) = ' ' ! specially to cause abend
           STOP 'STATUS_SET'
      END IF
!
      CALL CLRCH ( NAME_COMP )
      NAME_COMP = NAME
!
      IF ( NAME_COMP(1:1) .EQ. '-' ) THEN
!
! -------- Spacial case: the first symbol of NAME is "-"
!
           F_HYPHEN = .TRUE. ! set flag
           CALL CLRCH ( NAME_COMP )
!
! -------- Stripe it out
!
           NAME_COMP = NAME(2:)
         ELSE
           F_HYPHEN = .FALSE.
      END IF
!
      CALL CLRCH ( DBNAME_MES )
      IF ( .NOT. F_HYPHEN ) THEN
!
! -------- Forming field DBNAME_MES
!
           CALL CLRCH ( DBNAME_MES )
           DBNAME_MES = DBNAME_CH
           DBNAME_MES(12:) = '<'
           IF ( DBNAME_VER .GT. 1  .AND.  DBNAME_VER .LT. 999 ) THEN
                CALL INCH ( DBNAME_VER, DBNAME_MES(13:) )
           END IF
           DBNAME_MES( I_LEN(DBNAME_MES)+1: ) = '>'
      END IF
!
! --- Set the file name where status will be written
!
      CALL CLRCH ( WORK_DIR )
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
      IF ( ILEN(WORK_DIR) .EQ. 0 ) THEN
           WORK_DIR = SOLVE_WORK_DIR
      END IF
!
      CALL CLRCH ( STATUS_FILE )
      STATUS_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'/'//'STAT'//PRE_LETRS
!
! --- Set the lines with current session number and total number of sessions
!
      CALL CLRCH ( CUR_SESS )
      CALL CLRCH ( TOT_SESS )
      IF ( NARCS .GT. INT2(1) ) THEN
           CALL INCH   ( IARCNM, CUR_SESS )
           CALL CHASHL ( CUR_SESS )
           CALL INCH   ( NARCS,  TOT_SESS )
           CALL CHASHL ( TOT_SESS )
           TOT_SESS = '('//TOT_SESS(1:I_LEN(TOT_SESS))//')'
      END IF
!
! --- Generate a status line
!
      CALL CLRCH ( OUT )
      IF ( ILEN(TOT_SESS) .EQ. 0 ) THEN
           OUT = 'SOLVE:  '//NAME_COMP(1:8)//'  '// &
     &                       DBNAME_MES(1:16)// &
     &                       ACTION_STR//'  ('//ISLTY2//') '
         ELSE
           OUT = 'SOLVE:  '//NAME_COMP(1:8)//'  '// &
     &           CUR_SESS(1:I_LEN(CUR_SESS))// &
     &           TOT_SESS(1:I_LEN(TOT_SESS))//'  '// &
     &           DBNAME_MES(1:16)//ACTION_STR//'  ('//ISLTY2//') '
      END IF
!
      IF ( ACTION .EQ. STA__SUC ) THEN
           OUT = 'SOLVE '//NAME_COMP(1:I_LEN(NAME_COMP))//' successful '// &
     &           'completion'
      END IF
!
      PID = GETPID()
      CALL CLRCH    (      STR_PID )
      CALL INCH     ( PID, STR_PID )
      CALL CHASHR   (      STR_PID )
      CALL BLANK_TO_ZERO ( STR_PID )
!
! --- Write status line in status file
!
      OPEN  ( UNIT=40, FILE=STATUS_FILE, STATUS='UNKNOWN', IOSTAT=I40 )
      WRITE ( UNIT=40, FMT='(A)', IOSTAT=I40 ) OUT(1:64)//'  '//STR_PID(1:8)
      CLOSE ( UNIT=40, IOSTAT=I40 )
!
      IF ( FAST_DBG .EQ. F__MON ) THEN
!
! -------- Write status line in the screen
!
           WRITE ( 6, FMT='(A,A,A$)' ) '  ', OUT(1:64), CHAR(13)
           CALL FLUSH ( 6 )
      END IF
!
      RETURN
      END  SUBROUTINE  STATUS_SET  !#!#
