      FUNCTION MULTI_INDEX ( NREP, STR, SUBSTR )
! ************************************************************************
! *                                                                      *
! *   Function  MULTI_INDEX returns an index of the NREP-th occurence    *
! *   of the substring STR in the string STR. It returns 0 if there      *
! *   were no occurence, -1 if NREP=0 and -2 if LEN(STR) = 0             *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  11-SEP-99   MULTI_INDEX  v1.0  (d)  L. Petrov  11-SEP-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MULTI_INDEX, NREP
      CHARACTER  STR*(*), SUBSTR*(*)
      INTEGER*4  J1, IB, IP, IL
!
      IF ( NREP .LE. 0 ) THEN
           MULTI_INDEX = -1
           RETURN
      END IF
!
      IL = LEN(STR)
      IF ( IL .LE. 0 ) THEN
           MULTI_INDEX = -2
           RETURN
      END IF
!
      MULTI_INDEX = 0
      IB = 1
      DO 410 J1=1,NREP
         IF ( IB .GT. IL ) RETURN
         IP = INDEX ( STR(IB:), SUBSTR ) + IB-1
         IF ( IP .LE. IB-1 ) RETURN
         IB=IP+1
 410  CONTINUE
      MULTI_INDEX = IP
!
      RETURN
      END  !#!  MULTI_INDEX  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MULTI_INDICES ( MREP, NREP, INDS, STR, SUBSTR )
! ************************************************************************
! *                                                                      *
! *   Function  MULTI_INDICES returns an array of indices of substring   *
! *   SUBSTR in string STR.                                              *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  15-NOV-2025  MULTI_INDICES v1.0  (d) L. Petrov 15-NOV-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MREP, NREP, INDS(MREP)
      CHARACTER  STR*(*), SUBSTR*(*)
      INTEGER*4  J1, IB, IP, IL
!
      IF ( MREP .LE. 0 ) THEN
           RETURN
      END IF
!
      NREP = 0
      INDS = 0
      IL = LEN(STR)
      IF ( IL .LE. 0 ) THEN
           RETURN
      END IF
!
      IB = 1
      DO 410 J1=1,MREP
         IF ( IB .GT. IL ) RETURN
         IP = INDEX ( STR(IB:), SUBSTR ) + IB-1
         IF ( IP .GT. IB-1 ) THEN
              NREP = NREP + 1
              INDS(NREP) = IP
              IB=IP+1
            ELSE
              RETURN
         END IF
 410  CONTINUE
!
      RETURN
      END  !#!  MULTI_INDEX  #!#
