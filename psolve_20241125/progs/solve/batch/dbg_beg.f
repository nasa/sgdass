      SUBROUTINE DBG_BEG ( RESTRT_L2, NARCS )
! ************************************************************************
! *                                                                      *
! *   Routine  DBG_END  makes some actions at the beginning of BATCH     *
! *   run:                                                               *
! *                                                                      *
! *   1) it creates a welcome line with message about starting SOLVE     *
! *      batch run;                                                      *
! *   2) it writes it in the screen and in the file TIMRxx where xx are  *
! *      SOLVE initials;                                                 *
! *   2) it writes a massage about SOLVE beginning in status file.       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   RESTRT_L2 ( LOGICAL*2 ) -- True if restarting is being done.       *
! *       NARCS ( INTEGER*2 ) -- Number of sessions to be processed.     *
! *                                                                      *
! *  ###  05-APR-99     DBG_BEG    v1.1  (c)  L. Petrov  06-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      LOGICAL*2  RESTRT_L2
      INTEGER*2  NARCS
      CHARACTER  TIMING_FILE*160, STATUS_FILE*160, WORK_DIR*160, OUT*80, &
     &           TOT_SESS*8, DATE_STR*19, ACTION*7
      CHARACTER  SYSNAME*16, NODENAME*16, HARDWARE*16
      CHARACTER  GET_CDATE*19
      INTEGER*4  I40
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Set the file name where timing information will be written
!
      CALL CLRCH ( WORK_DIR )
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
      IF ( ILEN(WORK_DIR) .EQ. 0 ) THEN
           WORK_DIR = SOLVE_WORK_DIR
      END IF
!
! --- Build a line
!
      CALL CLRCH (              TOT_SESS )
      CALL INCH  ( INT4(NARCS), TOT_SESS )
      DATE_STR = GET_CDATE ()
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
      CALL CLRCH ( OUT )
      IF ( RESTRT_L2 ) THEN
           ACTION = 'resumed'
         ELSE
           ACTION = 'started'
      END IF
!
      OUT = '# Solve '//PRE_LETRS//' for '//TOT_SESS(1:I_LEN(TOT_SESS))// &
     &           ' sessions '//ACTION//' on '//NODENAME(1:I_LEN(NODENAME))// &
     &       ' at '//DATE_STR(1:19)//' #'
      WRITE ( 6, FMT='(A)' ) OUT(1:I_LEN(OUT))
!
      IF ( .NOT. RESTRT_L2  .AND.  FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Form a timing file name
!
           CALL CLRCH ( TIMING_FILE )
           TIMING_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'/'//'TIMR'//PRE_LETRS
!
           OPEN  ( UNIT=40, FILE=TIMING_FILE, STATUS='UNKNOWN', IOSTAT=I40 )
           WRITE ( UNIT=40, FMT='(A)', IOSTAT=I40 ) OUT(1:I_LEN(OUT))
           CLOSE ( UNIT=40, IOSTAT=I40 )
      END IF
!
! --- Form a status file name
!
      CALL CLRCH ( STATUS_FILE )
      STATUS_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'/'//'STAT'//PRE_LETRS
!
      CALL CLRCH ( OUT )
      OUT = 'SOLVE  BATCH  started'
!
! --- Write a status line in STATUS file
!
      OPEN  ( UNIT=40, FILE=STATUS_FILE, STATUS='UNKNOWN', IOSTAT=I40 )
      WRITE ( UNIT=40, FMT='(A)', IOSTAT=I40 ) OUT(1:I_LEN(OUT))
      CLOSE ( UNIT=40, IOSTAT=I40 )
!
      RETURN
      END  !#!  DBG_BEG  #!#
