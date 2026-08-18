      SUBROUTINE REPGRRD ( SUPKEY, NP1, NP2, YC, &
     &                     ARR1_X4, ARR1_Y4, ARR1_E4, ARR1_INFO, &
     &                     ARR2_X4, ARR2_Y4, ARR2_E4, ARR2_INFO, &
     &                     ARR1_X8, ARR1_Y8, ARR1_E8, &
     &                     ARR2_X8, ARR2_Y8, ARR2_E8, &
     &                     ARR1_REC, ARR2_REC )
!
! ************************************************************************
! *                                                                      *
! *   routine REPGRRD deletes the points which are "outside" the         *
! *   curser area abs(y) > abs(YC) from the arrays ARR1_X4, ARR1_Y4,     *
! *   ARR1_E4, ARR1_INFO, shrinks arrays and inserts this points into    *
! *   the arrays ARR2_X4, ARR2_Y4, ARR2_E4, ARR2_INFO                    *
! *   Analogously it recovers points                                     *
! *                                                                      *
! *   called subroutines:                                                *
! *   REPINSE                                                            *
! *   calling routines:                                                  *
! *   REPGRSU                                                            *
! *                                                                      *
! *  ### 09-SEP-2002    REPGRRD                       Volkmar Thorandt   *
! *                                                                      *
! ************************************************************************
!
      INTEGER*4     NP1             ! # of points in fields ARR1_  (1st arrays, good)
      INTEGER*4     NP2             ! # of points in fields ARR2_  (2nd arrays, man.down)
!
      CHARACTER     SUPKEY*1        ! suppression key
      REAL*4        ARR1_X4(*)      ! time field (good)
      REAL*4        ARR1_Y4(*)      ! value field (good)
      REAL*4        ARR1_E4(*)      ! error field (good)
      CHARACTER*87  ARR1_INFO(*)    ! array of info lines (good)
!
      REAL*4        ARR2_X4(*)      ! time field (recoverable)
      REAL*4        ARR2_Y4(*)      ! value field (recoverable)
      REAL*4        ARR2_E4(*)      ! error field (recoverable)
      CHARACTER*87  ARR2_INFO(*)    ! array of info lines (recoverable)
!
      REAL*8        ARR1_X8(*)      ! time field (good)
      REAL*8        ARR1_Y8(*)      ! value field (good)
      REAL*8        ARR1_E8(*)      ! error field (good)
      REAL*8        ARR2_X8(*)      ! time field (recoverable)
      REAL*8        ARR2_Y8(*)      ! value field (recoverable)
      REAL*8        ARR2_E8(*)      ! error field (recoverable)
!
      INTEGER*4     ARR1_REC(*)     ! record numbers (array 1)
      INTEGER*4     ARR2_REC(*)     ! record numbers (array 2)
!
      REAL*4        AUX_X(NP1+NP2)   ! buffer of remaining elements in ARR1_X4
      REAL*4        AUX_Y(NP1+NP2)   ! buffer of remaining elements in ARR1_Y4
      REAL*4        AUX_E(NP1+NP2)   ! buffer of remaining elements in ARR1_E4
      CHARACTER*87  AUX_I(NP1+NP2)   ! buffer of remaining elements in ARR1_INFO
      REAL*8        AUX_X8(NP1+NP2)  ! buffer
      REAL*8        AUX_Y8(NP1+NP2)  ! buffer
      REAL*8        AUX_E8(NP1+NP2)  ! buffer
      INTEGER*4     AUX_REC(NP1+NP2) ! buffer
      INTEGER*4  M1                  ! loop variable
      REAL*4     YC                  ! curser position coordinate (value)
      REAL*4     YC_A                ! abs. value of YC
      INTEGER*4  J1                  ! loop variable
!
! --- find points
!
      YC_A = ABS( YC )
      M1 = 0
      IF ( SUPKEY .EQ. '0' ) THEN   ! suppress points (G-->M)
!
         DO J1 = 1, NP1
            IF ( ABS( ARR1_Y4(J1) ) .LE. YC_A ) THEN
!
! ------------ remaining points in "good" arrays
!
               M1 = M1 + 1
               AUX_X(M1)   = ARR1_X4(J1)
               AUX_Y(M1)   = ARR1_Y4(J1)
               AUX_E(M1)   = ARR1_E4(J1)
               AUX_I(M1)   = ARR1_INFO(J1)
               AUX_X8(M1)  = ARR1_X8(J1)
               AUX_Y8(M1)  = ARR1_Y8(J1)
               AUX_E8(M1)  = ARR1_E8(J1)
               AUX_REC(M1) = ARR1_REC(J1)
            ELSE
!
! ------------ insert point into 2nd arrays (recoverable)
!
               CALL REPINSE ( NP2, ARR2_X4, ARR2_Y4, ARR2_E4, ARR2_INFO, &
     &                   ARR1_X4(J1), ARR1_Y4(J1), ARR1_E4(J1), &
     &                   ARR1_INFO(J1)(1:6)//'  1'//ARR1_INFO(J1)(10:85)//'MD', &
     &                   ARR2_X8, ARR2_Y8, ARR2_E8, &
     &                   ARR1_X8(J1), ARR1_Y8(J1), ARR1_E8(J1), ARR2_REC, ARR1_REC(J1) )
            END IF
         END DO
!
! ------ copy buffer arrays of remaining points into 1st arrays (good)
!
         DO J1 = 1, M1
            ARR1_X4(J1)   = AUX_X(J1)
            ARR1_Y4(J1)   = AUX_Y(J1)
            ARR1_E4(J1)   = AUX_E(J1)
            ARR1_INFO(J1) = AUX_I(J1)
            ARR1_X8(J1)   = AUX_X8(J1)
            ARR1_Y8(J1)   = AUX_Y8(J1)
            ARR1_E8(J1)   = AUX_E8(J1)
            ARR1_REC(J1)  = AUX_REC(J1)
         END DO
!
         NP1 = M1       ! # of elements in 1st arrays (good)
!
      ELSE IF ( SUPKEY .EQ. '1' ) THEN       ! recover points (M-->G)
!
         DO J1 = 1, NP2
            IF ( ABS( ARR2_Y4( J1 ) ) .GE. YC_A ) THEN
!
! ------------ remaining points in "man.down" arrays
!
               M1 = M1 + 1
               AUX_X(M1)   = ARR2_X4(J1)
               AUX_Y(M1)   = ARR2_Y4(J1)
               AUX_E(M1)   = ARR2_E4(J1)
               AUX_I(M1)   = ARR2_INFO(J1)
               AUX_X8(M1)  = ARR2_X8(J1)
               AUX_Y8(M1)  = ARR2_Y8(J1)
               AUX_E8(M1)  = ARR2_E8(J1)
               AUX_REC(M1) = ARR2_REC(J1)
            ELSE
!
! ------------ insert point into 1st arrays (good)
!
               CALL REPINSE ( NP1, ARR1_X4, ARR1_Y4, ARR1_E4, ARR1_INFO, &
     &                   ARR2_X4(J1), ARR2_Y4(J1), ARR2_E4(J1), &
     &                   ARR2_INFO(J1)(1:6)//'  0'//ARR2_INFO(J1)(10:85)//'GP', &
     &                   ARR1_X8, ARR1_Y8, ARR1_E8, &
     &                   ARR2_X8(J1), ARR2_Y8(J1), ARR2_E8(J1), ARR1_REC, ARR2_REC(J1) )
            END IF
         END DO
!
! ------ copy buffer arrays of remaining points into 2nd arrays (recoverable)
!
         DO J1 = 1, M1
            ARR2_X4(J1)   = AUX_X(J1)
            ARR2_Y4(J1)   = AUX_Y(J1)
            ARR2_E4(J1)   = AUX_E(J1)
            ARR2_INFO(J1) = AUX_I(J1)
            ARR2_X8(J1)   = AUX_X8(J1)
            ARR2_Y8(J1)   = AUX_Y8(J1)
            ARR2_E8(J1)   = AUX_E8(J1)
            ARR1_REC(J1)  = AUX_REC(J1)
         END DO
!
         NP2 = M1       ! # of elements in 1st arrays (man.down)
!
      END IF
!
      RETURN
      END  !#!  REPGRRD  #!#
