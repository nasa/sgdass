      SUBROUTINE REPAMBI ( M, IOBS, Y4_ARR, YC, AMB_4, IWAY )
! ************************************************************************
! *                                                                      *
! *  REPAMBI checks the point with index IOBS in Y4_ARR whether          *
! *  the value of ABS(Y4_ARR(IOBS)-YC) is greater then one ambiguity     *
! *  step. In this case IWAY returns the number of ambiguity steps to    *
! *  shift the observation as close as possible to the curser YC value.  *
! *                                                                      *
! *  called subroutines: none                                            *
! *  calling routine: REPGRSH                                            *
! *                                                                      *
! *  ### 21-OCT-2002  REPAMBI              V. Thorandt  21-OCT-2002 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      INTEGER*4  M                               ! # of elements in Y4_ARR
      INTEGER*4  IOBS                            ! index of current observation in Y4_ARR
      REAL*4     Y4_ARR(M)                       ! observation array of current colour
      REAL*4     YC                              ! value of curser position
      REAL*4     AMB_4                           ! ambiguity step (real*4)
      INTEGER*4  IWAY                            ! # of ambiguity steps (local variable!)
!
      REAL*4     DIST                            ! positive distance | YC - Y4_ARR(IOBS) |
      REAL*4     AMB_42                          ! half of AMB_4
!
      IWAY = 0                                   ! initialize IWAY
!
      DIST = ABS( Y4_ARR(IOBS) - YC  )           ! distance between curser and point
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
         IF ( ( YC - Y4_ARR(IOBS) ) .GT. 0 ) THEN
              IWAY = IABS(IWAY) 
            ELSE
              IWAY = -IABS(IWAY) 
         END IF
!!         IWAY = SIGN( IWAY, YC - Y4_ARR(IOBS) )  ! move signum (direction) to IWAY
      ENDIF
!
      RETURN
      END  !#!  REPAMBI  #!#
