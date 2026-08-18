      FUNCTION   VEX_STA_IDX ( VEX, STA_NAME )
! **********************************************************************
! *                                                                    *
! *   Auxiliary function VEX_STA_IDX finds the index of the station    *
! *   STA_NAME in internal data structure of VEX.                      * 
! *   If STA_NAME is not found, it returns 0.                          *
! *                                                                    *
! *   INPUT:                                                           *
! *          VEX       =  VLBI Experiment File Object   { VEX }        *
! *                                                                    *
! *          STA_NAME  =  Station name                  { CHAR }       *
! *                                                                    *
! *   OUTPUT:                                                          *
! *          <VEX_STA_IDX>  = Station index             { INT }        *
! *                           Index of station in the internal VEX     *
! *                           structure.                               *
! *                                                                    *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 06-NOV-2020   VEX_STA_IDX  v1.0 (d)  N. Habana 06-NOV-2020 ### *
! *                                                                    *
! **********************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      TYPE     ( VEX_TYPE ) :: VEX
      CHARACTER  STA_NAME*(*)
      INTEGER*4  VEX_STA_IDX
      INTEGER*4  J1
!
      DO 410 J1 = 1, VEX%N_STA
         IF ( VEX%STA(J1)%SITE_NAME .EQ. STA_NAME ) THEN
            VEX_STA_IDX  =  J1
            RETURN 
         END IF
 410  CONTINUE 
!
      VEX_STA_IDX  =  0
!
      RETURN
      END  FUNCTION   VEX_STA_IDX  
!
! ---------------------------------------------------------------------
!
      FUNCTION   VEX_STA_ID_IDX ( VEX, STA_ID )
! *************************************************************************
! *                                                                       *
! *   Auxiliary function VEX_STA_ID_IDX finds the index of the            *
! *   station STA_ID in internal data structure of VEX.                   * 
! *   If STA_ID is not found, it returns 0.                               *
! *                                                                       *
! *   INPUT:                                                              *
! *          VEX     =  VLBI Experiment File Object     { VEX }           *
! *                                                                       *
! *          STA_ID  =  Station ID                      { CHAR }          *
! *                                                                       *
! *   OUTPUT:                                                             *
! *          <VEX_STA_ID_IDX>  = Station ID index       { INT }           *
! *                              Index of station in the internal VEX     *
! *                              structure.                               *
! *                                                                       *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 06-NOV-2020   VEX_STA_ID_IDX  v1.0 (d)  N. Habana 06-NOV-2020 ### *
! *                                                                       *
! *************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      TYPE     ( VEX_TYPE ) :: VEX
      CHARACTER  STA_ID*(*)
      INTEGER*4  VEX_STA_ID_IDX
      INTEGER*4  J1
!
      DO 410 J1 = 1, VEX%N_STA
         IF ( VEX%STA(J1)%SITE_ID .EQ. STA_ID ) THEN
            VEX_STA_ID_IDX  =  J1
            RETURN 
         END IF
 410  CONTINUE 
!
      VEX_STA_ID_IDX = 0
!
      RETURN
      END  FUNCTION   VEX_STA_ID_IDX

