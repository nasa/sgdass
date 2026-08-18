      FUNCTION   GVH_COMPAR_LCD ( REC1, REC2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine for sorting lcd records.                        *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 03-NOV-2005  GVH_COMPAR_LCD  v1.1 (d) L. Petrov  17-JAN-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
#ifdef GNU
      INTEGER*4  GVH_COMPAR_LCD 
#else
      INTEGER*2  GVH_COMPAR_LCD 
#endif
      TYPE     ( GVH_LCD__STRU ) :: REC1, REC2
      INTEGER*4  IM1, IM2
      INTEGER*4, EXTERNAL :: LTM_DIF
!
      IM1 = LTM_DIF ( 1, GVH__NMLCODE, GVH__MLCODE, REC1%LCODE )
      IM2 = LTM_DIF ( 1, GVH__NMLCODE, GVH__MLCODE, REC2%LCODE )
      IF ( IM1 > 0  .AND.  IM2 .LE. 0 ) THEN
           GVH_COMPAR_LCD = -1
           RETURN 
         ELSE IF ( IM1 .LE. 0  .AND.  IM2 > 0 ) THEN
           GVH_COMPAR_LCD =  1
           RETURN 
      END IF
!
      IF ( REC1%LCODE < REC2%LCODE ) THEN
           GVH_COMPAR_LCD = -1
         ELSE IF ( REC1%LCODE > REC2%LCODE ) THEN
           GVH_COMPAR_LCD =  1
         ELSE 
           GVH_COMPAR_LCD =  0
      END IF
      RETURN
      END  FUNCTION  GVH_COMPAR_LCD  !#!  
