      SUBROUTINE INT_TO_BASE26 ( I4, STR_26 )
! ************************************************************************
! *                                                                      *
! *   Routine INT_TO_BASE26 encodes an integer to base26. Characters     *
! *   are in a range from A (0) to Z(26).                                *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 21-MAR-2025  INT_TO_BASE26 v1.0 (d)  L. Petrov  21-MAR-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  I4
      CHARACTER  STR_26*(*)
      INTEGER*4  MLEN
      PARAMETER  ( MLEN = 7 ) 
      CHARACTER  TMP*(MLEN)
      INTEGER*4  TAB(MLEN)
      DATA        TAB /      &
     &                    1, & ! 1      &
     &                   26, & ! 26,    &
     &                  676, & ! 26**2, &
     &                17576, & ! 26**3, &
     &               456976, & ! 26**4, &
     &             11881376, & ! 26**5, &
     &            308915776  & ! 26**6, &
     &                /
       
      INTEGER*4  J1, J2, IP, IVAL, IDIG, RES, KLEN
!
      IVAL = I4
      KLEN = MIN(MLEN,LEN(STR_26))
      IP = 0
      DO 410 J1=KLEN,1,-1
         IDIG =  IVAL/TAB(J1)
         IP = IP + 1
         STR_26(IP:IP) = CHAR(65+IDIG)
         IVAL = IVAL - IDIG*TAB(J1)
 410  CONTINUE 
 810  CONTINUE 
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BASE26_TO_INT ( STR_26, I4 )
! ************************************************************************
! *                                                                      *
! *   Routine BASE26_TO_INT trainsforms a string in base26 to integer.   *
! *   The string in base26 may have up to 7 digits from A(0) to Z(26).   *
! *   Character '_' is treated as 'A'                                    *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 21-MAR-2025 BASE26_TO_INT v1.1 (d)  L. Petrov  22-MAR-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  STR_26*(*)
      INTEGER*4  I4
      INTEGER*4  MLEN
      PARAMETER  ( MLEN = 7 ) 
      INTEGER*4  J1, KLEN, IDIG, IBASE
!
      KLEN = MIN(MLEN,LEN(STR_26))
!
      I4 = 0
      IBASE = 1
      DO 410 J1=1,KLEN
         IF ( STR_26(LEN(STR_26)+1-J1:LEN(STR_26)+1-J1) == '_' ) THEN
              IDIG = 0
            ELSE
              IDIG = ICHAR ( STR_26(LEN(STR_26)+1-J1:LEN(STR_26)+1-J1) ) - 65
         END IF 
         I4 = IBASE*IDIG + I4
         IF ( J1 < MLEN ) IBASE =IBASE*26
 410  CONTINUE 
      RETURN 
      END SUBROUTINE  BASE26_TO_INT  !#!#
