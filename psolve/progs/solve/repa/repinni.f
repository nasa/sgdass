      SUBROUTINE REPINNI ( NP, &
     &                     ARR_X4, ARR_Y4, ARR_E4, ARG, VAL, ERR, &
     &                     ARR_X8, ARR_Y8, ARR_E8, ARG8, VAL8, ERR8 )
!
! ************************************************************************
! *                                                                      *
! *   Routine REPINSE inserts the point ARGx,VALx,ERRx into              *
! *   the sorted arrays ARRx_xx. The arrays ARR_X4,ARR_Y4,ARR_E4         *
! *   are sorted by increasing ARR_X4. Ths point will be inserted in     *
! *   such a place of the array that preserves the sorting order.        *
! *                                                                      *
! *   called subroutines: none                                           *
! *   calling routine: REPCONN                                           *
! *                                                                      *
! *  2002-09-09                REPINNI                 Volkmar Thorandt  *
! *  2002-11-13 VT - new subroutine REPINNI without info field           *
! *  2002-12-18 VT - real*8 arrays added                                 *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4   NP                ! # of arguments
      REAL*4      ARR_X4(*)         ! arguments
      REAL*4      ARR_Y4(*)         ! values
      REAL*4      ARR_E4(*)         ! errors
      REAL*4      ARG, VAL, ERR     ! to be inserted into real*4 arrays
      REAL*8      ARR_X8(*)         ! arguments
      REAL*8      ARR_Y8(*)         ! values
      REAL*8      ARR_E8(*)         ! errors
      REAL*8      ARG8, VAL8, ERR8  ! to be inserted into real*8 arrays
      INTEGER*4   J1, J2
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
                      ARR_X8(J2+1)   = ARR_X8(J2)
                      ARR_Y8(J2+1)   = ARR_Y8(J2)
                      ARR_E8(J2+1)   = ARR_E8(J2)
                   END DO
! ---------------- insert the point
                   ARR_X4(J1)   = ARG
                   ARR_Y4(J1)   = VAL
                   ARR_E4(J1)   = ERR
                   ARR_X8(J1)   = ARG8
                   ARR_Y8(J1)   = VAL8
                   ARR_E8(J1)   = ERR8
                   NP = NP + 1
                   RETURN
              END IF
           END DO
      END IF
!
! --- point not found --> insert the point to the end of the arrays
!
      ARR_X4(NP+1)   = ARG
      ARR_Y4(NP+1)   = VAL
      ARR_E4(NP+1)   = ERR
      ARR_X8(NP+1)   = ARG8
      ARR_Y8(NP+1)   = VAL8
      ARR_E8(NP+1)   = ERR8
      NP = NP + 1
!
      RETURN
      END  !#!  REPINNI  #!#
