      SUBROUTINE REPINSE ( NP, ARR_X4, ARR_Y4, ARR_E4, ARR_INFO, &
     &                     ARG, VAL, ERR, INFO_LINE, &
     &                     ARR_X8, ARR_Y8, ARR_E8, &
     &                     ARG8, VAL8, ERR8, ARR_RECNUM, RECNUM )
!
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine REPINSE inserts the point ARG,VAL,ERR into       *
! *   the sorted arrays ARR_xx. The arrays ARR_X4,ARR_Y4,ARR_E4,ARR_INFO *
! *   are sorted by increasing ARR_X4. Ths point will be inserted in such*
! *   a place of the array that preserves the sorting order.             *
! *                                                                      *
! *   called subroutines: none                                           *
! *   calling routine: REPGRRD, REPPTSU                                  *
! *                                                                      *
! *  2002-09-09                  REPINSE               Volkmar Thorandt  *
! *  2002-10-28 VT - name changed: REPINSE                               *
! *  2002-12-16 VT - real*8 arrays added                                 *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4      NP
      REAL*4         ARR_X4(*), ARR_Y4(*), ARR_E4(*), ARG, VAL, ERR
      REAL*8         ARR_X8(*), ARR_Y8(*), ARR_E8(*), ARG8, VAL8, ERR8
      CHARACTER*87   ARR_INFO(*)
      INTEGER*4      ARR_RECNUM(*)
      INTEGER*4      RECNUM
      CHARACTER*87   INFO_LINE
      INTEGER*4      J1, J2
!
      IF ( NP .GT. 0 ) THEN
!
! -------- Search for the point which is just after the ARG
!
           DO J1 = 1, NP
              IF ( ARG .LT. ARR_X4(J1) ) THEN
! ---------------- point is found
                   DO J2 = NP, J1, -1
                      ARR_X4(J2+1)   = ARR_X4(J2)  ! shift all points up
                      ARR_Y4(J2+1)   = ARR_Y4(J2)
                      ARR_E4(J2+1)   = ARR_E4(J2)
                      ARR_INFO(J2+1) = ARR_INFO(J2)
                      ARR_X8(J2+1)   = ARR_X8(J2)
                      ARR_Y8(J2+1)   = ARR_Y8(J2)
                      ARR_E8(J2+1)   = ARR_E8(J2)
                      ARR_RECNUM(J2+1)   = ARR_RECNUM(J2)
                   END DO
! ---------------- insert the point
                   ARR_X4(J1)     = ARG
                   ARR_Y4(J1)     = VAL
                   ARR_E4(J1)     = ERR
                   ARR_INFO(J1)   = INFO_LINE
                   ARR_X8(J1)     = ARG8
                   ARR_Y8(J1)     = VAL8
                   ARR_E8(J1)     = ERR8
                   ARR_RECNUM(J1) = RECNUM
                   NP = NP + 1
                   RETURN
              END IF
           END DO
      END IF
!
! --- point not found --> insert the point to the end of the array
!
      ARR_X4(NP+1)     = ARG
      ARR_Y4(NP+1)     = VAL
      ARR_E4(NP+1)     = ERR
      ARR_INFO(NP+1)   = INFO_LINE
      ARR_X8(NP+1)     = ARG8
      ARR_Y8(NP+1)     = VAL8
      ARR_E8(NP+1)     = ERR8
      ARR_RECNUM(NP+1) = RECNUM
      NP = NP + 1
!
      RETURN
      END  !#!  REPINSE  #!#
