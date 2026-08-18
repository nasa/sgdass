      FUNCTION   COMPAR_BDS_SUM ( ISTR1, ISTR2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program for sorting the BDS summary buffer.             *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 06-JAN-2026 COMPAR_BDS_SUM  v1.0 (d)  L. Petrov  06-JAN-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  COMPAR_BDS_SUM
      INTEGER*1  ISTR1(132) , ISTR2(132)
      CHARACTER  STR1*132, STR2*132
!
      CALL LIB$MOVC3 ( 128, ISTR1, %REF(STR1) )
      CALL LIB$MOVC3 ( 128, ISTR2, %REF(STR2) )
      IF ( STR1(11:18) > STR2(11:18) ) THEN
           COMPAR_BDS_SUM = 1
         ELSE IF ( STR1(11:18) < STR2(11:18) ) THEN
           COMPAR_BDS_SUM = -1
         ELSE 
           IF ( STR1(6:9) > STR2(6:9) ) THEN
                COMPAR_BDS_SUM = 1
              ELSE IF ( STR1(6:9) < STR2(6:9) ) THEN
                COMPAR_BDS_SUM = -1
              ELSE 
                COMPAR_BDS_SUM = 0
           END IF 
      END IF
      RETURN
      END  FUNCTION  COMPAR_BDS_SUM  !#!#
