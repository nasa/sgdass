      SUBROUTINE DBG_END()
! ************************************************************************
! *                                                                      *
! *   Routine  DBG_END  makes some actions at the end of BATCH run:      *
! *                                                                      *
! *   1) it creates a farwell line with message about completion of      *
! *      batch run  of SOLVE;                                            *
! *   2) it writes it in the screen and in the file TIMRxx where xx are  *
! *      SOLVE initials;                                                 *
! *   2) it writes a massage about SOLVE completion in status file.      *
! *                                                                      *
! *  ###  05-APR-99     DBG_END   v1.2  (c)  L. Petrov  13-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'precm.i'
      CHARACTER  TIMING_FILE*160, WORK_DIR*160, OUT*80, DATE_STR*19
      CHARACTER  GET_CDATE*19
      INTEGER*4  I40
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
! --- Form a line
!
      DATE_STR = GET_CDATE ()
!
      CALL CLRCH ( OUT )
!
      OUT = '# Solve completed at '//DATE_STR(1:19)//' #'
      IF ( FAST_DBG .EQ. F__MON ) WRITE ( 6, FMT='(A)' ) ' '
      WRITE ( 6, FMT='(A)' ) OUT(1:I_LEN(OUT))
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Form a timing file name
!
           CALL CLRCH ( TIMING_FILE )
           TIMING_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'/'//'TIMR'//PRE_LETRS
!
           OPEN  ( UNIT=40, FILE=TIMING_FILE, STATUS='UNKNOWN', &
     &             ACCESS='APPEND',IOSTAT=I40 )
           WRITE ( UNIT=40, FMT='(A)', IOSTAT=I40 ) OUT(1:I_LEN(OUT))
           CLOSE ( UNIT=40, IOSTAT=I40 )
      END IF
!
! --- Form a status file name
!
!
      CALL STATUS_SET ( 'BATCH', STA__SUC )
!
      RETURN
      END  !#!  DBG_END  #!#
