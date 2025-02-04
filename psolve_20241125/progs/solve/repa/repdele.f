      SUBROUTINE REPDELE ( IPQ, NP, ARR_X4, ARR_Y4, ARR_E4, ARR_INFO, &
     &                     ARR_X8, ARR_Y8, ARR_E8, ARR_RECNUM )
!
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine REPDELE deletes the point with index IPQ         *
! *   from the arrays ADD_X4,ARR_Y4,ARR_E4 and shrinks arrays.           *
! *                                                                      *
! *   called subroutines: none                                           *
! *   calling routine: REPPTSU                                           *
! *                                                                      *
! *  ### 08-AUG-2002    UD_DELETE  v1.0 (c)  L. Petrov  08-AUG-2002 ###  *
! *  28-OCT-2002 name changed: REPDELE       VT                          *
! *  2002-12-16 VT - real*8 arrays added                                 *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4     J1
      INTEGER*4     IPQ
      INTEGER*4     NP
      REAL*4        ARR_X4(*), ARR_Y4(*), ARR_E4(*)
      REAL*8        ARR_X8(*), ARR_Y8(*), ARR_E8(*)
      CHARACTER*87  ARR_INFO(*)        ! array of info lines
      INTEGER*4     ARR_RECNUM(*)      ! array of record #s
!
      IF ( IPQ .LE. 0  .OR.   IPQ .GT. NP ) RETURN
      IF ( IPQ .EQ. NP ) THEN
           NP = NP - 1
         ELSE
           DO J1=IPQ,NP-1
              ARR_X4(J1)     = ARR_X4(J1+1)
              ARR_Y4(J1)     = ARR_Y4(J1+1)
              ARR_E4(J1)     = ARR_E4(J1+1)
              ARR_INFO(J1)   = ARR_INFO(J1+1)
              ARR_X8(J1)     = ARR_X8(J1+1)
              ARR_Y8(J1)     = ARR_Y8(J1+1)
              ARR_E8(J1)     = ARR_E8(J1+1)
              ARR_RECNUM(J1) = ARR_RECNUM(J1+1)
           END DO
           NP = NP - 1
      END IF
!
      RETURN
      END  !#!  REPDELE  #!#
