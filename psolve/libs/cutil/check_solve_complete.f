      FUNCTION   CHECK_SOLVE_COMPLETE ( SOLVE_INIT )
! ************************************************************************
! *                                                                      *
! *   Function  CHECK_SOLVE_STATUS checks status of Solve or solve-like  *
! *   program. It is assumed that solve-like program writes status in    *
! *   file $WORK_DIR/STATxx . CHECK_SOLVE_COMPLETE returns .TRUE. if     *
! *   Solve or solve-like application terminated normally and .FALSE.    *
! *   in all other cases.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * SOLVE_INIT ( CHARACTER ) -- 2-letters Solve user initials.           *
! *                                                                      *
! * ### 19-SEP-00 CHECK_SOLVE_COMPLETE v1.0 (c) L. Petrov 19-SEP-00 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      LOGICAL*4  CHECK_SOLVE_COMPLETE
      CHARACTER  SOLVE_INIT*(*)
      LOGICAL*4  LEX
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 64 )
      CHARACTER  FINAM*128, BUF(MBUF)*128
      INTEGER*4  NBUF, IP, IER
!
      CHECK_SOLVE_COMPLETE = .FALSE.
!
! --- Bild the filename for status file
!
      CALL CLRCH ( FINAM )
      FINAM = PRE_SCR_DIR(1:PRE_SD_LEN)//'STAT'//SOLVE_INIT
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           RETURN
      END IF
!
! --- Read status file
!
      IER = 0
      CALL RD_TEXT ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           RETURN
      END IF
!
! --- Try to find word completed in the first line of this file
!
      IP = INDEX ( BUF(1), 'successful completion' )
      IF ( IP .GT. 0 ) THEN
           CHECK_SOLVE_COMPLETE = .TRUE.
           RETURN
      END IF
      END  !#!  CHECK_SOLVE_STATUS  #!#
