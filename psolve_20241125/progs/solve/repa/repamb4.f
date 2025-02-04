      SUBROUTINE REPAMB4 ( Y4, YC, AMB_4, IWAY )
! ************************************************************************
! *                                                                      *
! *  REPAMB4 checks the point with the value Y4 whether the value        *
! *  of ABS(Y4-YC) is greater then one ambiguity step.                   *
! *  In this case IWAY returns the number of ambiguity steps to shift    *
! *  the observation as close as possible to the curser value YC.        *
! *                                                                      *
! *  called subroutines: none                                            *
! *  calling routine: REPA                                               *
! *  REPAMB4 is a derivate of the subroutine REPAMBI                     *
! *                                                                      *
! *  ### 27-MAY-2003  REPAMB4              V. Thorandt  27-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      REAL*4     Y4                              ! observation value
      REAL*4     YC                              ! value of (pseudo) curser position
      REAL*4     AMB_4                           ! ambiguity step (real*4)
      INTEGER*4  IWAY                            ! # of ambiguity steps
!
      REAL*4     DIST                            ! positive distance | YC - Y4 |
      REAL*4     AMB_42                          ! half of AMB_4
!
      IWAY = 0                                   ! initialize IWAY
!
      DIST = ABS( Y4 - YC  )                     ! distance between curser and point
!                                                ! in terms of Y-axis values
!
! --- min. distance of point value to curser position
!
      AMB_42 = AMB_4 / 2.0
!
      IF ( DIST .GT. AMB_42 ) THEN
! ------ calculate number of steps
         DIST = DIST / AMB_4
         IWAY = ANINT( DIST )
         IWAY = SIGN( IWAY, YC - Y4 )            ! move signum (direction) to IWAY
      ENDIF
      RETURN
      END  !#!  REPAMB4  #!#
