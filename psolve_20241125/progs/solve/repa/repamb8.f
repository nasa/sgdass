      SUBROUTINE REPAMB8 ( Y8, YC, AMB_8, IWAY )
! ************************************************************************
! *                                                                      *
! *  REPAMB8 checks the point with the value Y8 whether the value        *
! *  of ABS(Y8-YC) is greater then one ambiguity step.                   *
! *  In this case IWAY returns the number of ambiguity steps to shift    *
! *  the observation as close as possible to the curser value YC.        *
! *                                                                      *
! *  called subroutines: none                                            *
! *  calling routine: REPA                                               *
! *  REPAMB8 is a derivate of the subroutine REPAMBI                     *
! *                                                                      *
! *  ### 28-MAY-2003  REPAMB8              V. Thorandt  28-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      REAL*8     Y8                              ! observation value
      REAL*8     YC                              ! value of (pseudo) curser position
      REAL*8     AMB_8                           ! ambiguity step
      INTEGER*4  IWAY                            ! # of ambiguity steps
!
      REAL*8     DIST                            ! positive distance | YC - Y4 |
      REAL*8     AMB_82                          ! half of AMB_8
!
      IWAY = 0                                   ! initialize IWAY
!
      DIST = ABS( Y8 - YC  )                     ! distance between curser and point
!                                                ! in terms of Y-axis values
!
! --- min. distance of point value to curser position
!
      AMB_82 = AMB_8 / 2.0
!
      IF ( DIST .GT. AMB_82 ) THEN
!
! ------ Calculate number of steps
!
         DIST = DIST / AMB_8
         IWAY = ANINT( DIST )
         IF ( (YC - Y8) .GT. 0 ) THEN
              IWAY = IABS(IWAY)
            ELSE
              IWAY = -IABS(IWAY)
         END IF
!!         IWAY = SIGN( IWAY, YC - Y8 )            ! move signum (direction) to IWAY
      ENDIF
      RETURN
      END  !#!  REPAMB8  #!#
