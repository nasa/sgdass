      SUBROUTINE FREE_HEB ( HEB )
! ************************************************************************
! *                                                                      *
! *   Routine FREE_HEB 
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 21-NOV-2013    FREE_HEB   v1.0 (d)  L. Petrov  21-NOV-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE      ) :: HEB
!
      IF ( ASSOCIATED ( HEB%VAL ) ) THEN
           DEALLOCATE ( HEB%VAL )
      END IF
      IF ( ASSOCIATED ( HEB%ARR_LEV ) ) THEN
           DEALLOCATE ( HEB%ARR_LEV )
      END IF
!
      IF ( ASSOCIATED ( HEB%ARR_LON ) ) THEN
           DEALLOCATE ( HEB%ARR_LON )
      END IF
!      
      IF ( ASSOCIATED ( HEB%ARR_LAT ) ) THEN
           DEALLOCATE ( HEB%ARR_LAT )
      END IF
!      
      IF ( ASSOCIATED ( HEB%ARR_TIM ) ) THEN
           DEALLOCATE ( HEB%ARR_TIM )
      END IF
!      
      RETURN
      END  SUBROUTINE  FREE_HEB  !#!  
