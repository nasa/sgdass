      SUBROUTINE SUR_SORT ( SUR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_SORT
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ###  04-OCT-2025    SUR_SORT  v1.0  (d) L. Petrov  04-OCT-2025  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( SUR__TYPE ), POINTER :: SUR_OLD
      INTEGER*4  IUER
      REAL*8     TIM_OBS(SUR__M_SCN), TIM_IND(SUR__M_SCN)
      INTEGER*4  IND_SCN, J1, J2, IER 
!
! --- Collect time of scan start
!
      ALLOCATE ( SUR_OLD )
      DO 410 J1=1,SUR%L_SCN
         TIM_OBS(J1) = ( SUR%MJD_OBS_BEG(J1) - J2000__MJD)*86400.0D0 + SUR%TAI_OBS_BEG(J1)
         TIM_IND(J1) = J1 + 0.01D0
!
! ------ Collect scan-related arrays to SUR_OLD
!
         SUR_OLD%MJD_OBS_BEG(J1) = SUR%MJD_OBS_BEG(J1)
         SUR_OLD%MJD_OBS_END(J1) = SUR%MJD_OBS_END(J1)
         SUR_OLD%TAI_OBS_BEG(J1) = SUR%TAI_OBS_BEG(J1)
         SUR_OLD%TAI_OBS_END(J1) = SUR%TAI_OBS_END(J1)
         SUR_OLD%SCA_PREV(1:SUR%L_STA,J1)   = SUR%SCA_PREV(1:SUR%L_STA,J1)
         SUR_OLD%OBS_STA(1:SUR%L_STA,J1)    = SUR%OBS_STA(1:SUR%L_STA,J1)
         SUR_OLD%EL_OBS(1:SUR%L_STA,J1)     = SUR%EL_OBS(1:SUR%L_STA,J1)
         SUR_OLD%AZ_OBS(1:SUR%L_STA,J1)     = SUR%AZ_OBS(1:SUR%L_STA,J1)
         SUR_OLD%HA_OBS(1:SUR%L_STA,J1)     = SUR%HA_OBS(1:SUR%L_STA,J1)
         SUR_OLD%AZ_ACC_OBS(1:SUR%L_STA,J1) = SUR%AZ_ACC_OBS(1:SUR%L_STA,J1)
         SUR_OLD%HA_ACC_OBS(1:SUR%L_STA,J1) = SUR%HA_ACC_OBS(1:SUR%L_STA,J1)
         SUR_OLD%SLEW_DUR(1:SUR%L_STA,J1)   = SUR%SLEW_DUR(1:SUR%L_STA,J1)
         SUR_OLD%SLEW_SCA(J1)  = SUR%SLEW_SCA(J1)
         SUR_OLD%SCAN_TYPE(J1) = SUR%SCAN_TYPE(J1)
         SUR_OLD%SOU_POCAL(J1) = SUR%SOU_POCAL(J1)
         SUR_OLD%MJD_POCAL(J1) = SUR%MJD_POCAL(J1)
         SUR_OLD%TAI_POCAL(J1) = SUR%TAI_POCAL(J1)
         SUR_OLD%IND_SRC(J1)   = SUR%IND_SRC(J1)
         SUR_OLD%SRC_TYP(J1)   = SUR%SRC_TYP(J1)
         SUR_OLD%IND_SCN_SRC(J1,1:SUR%L_SOU) = SUR%IND_SCN_SRC(J1,1:SUR%L_SOU) 
         IF ( SUR%L_SO2 > 0 ) THEN
              SUR_OLD%IND_SCN_SO2(J1,1:SUR%L_SO2) = SUR%IND_SCN_SO2(J1,1:SUR%L_SO2) 
         END IF
 410  CONTINUE 
!
! --- Sort time indices
!
      CALL SORT8 ( SUR%L_SCN, TIM_OBS, TIM_IND )
!
! --- Copy scan-related fields from SUR_OLD in the sorted order 
!
      DO 420 J2=1,SUR%L_SCN
         IND_SCN = IDINT ( TIM_IND(J2) )
         SUR%MJD_OBS_BEG(J2) = SUR_OLD%MJD_OBS_BEG(IND_SCN)
         SUR%MJD_OBS_END(J2) = SUR_OLD%MJD_OBS_END(IND_SCN)
         SUR%TAI_OBS_BEG(J2) = SUR_OLD%TAI_OBS_BEG(IND_SCN)
         SUR%TAI_OBS_END(J2) = SUR_OLD%TAI_OBS_END(IND_SCN)
         IF ( J2 > 1 ) THEN
              IF ( SUR%TAI_OBS_END(J2) == SUR%TAI_OBS_END(J2-1) ) THEN
!
! ---------------- To avoid two scans to start at the same time
!
                   SUR%TAI_OBS_END(J2) = SUR%TAI_OBS_END(J2) + 1.0D0
              END IF
         END IF
         SUR%SCA_PREV(1:SUR%L_STA,J2) = SUR_OLD%SCA_PREV(1:SUR%L_STA,IND_SCN)
         SUR%OBS_STA(1:SUR%L_STA,J2)  = SUR_OLD%OBS_STA(1:SUR%L_STA,IND_SCN)
         SUR%EL_OBS(1:SUR%L_STA,J2)   = SUR_OLD%EL_OBS(1:SUR%L_STA,IND_SCN)
         SUR%AZ_OBS(1:SUR%L_STA,J2)   = SUR_OLD%AZ_OBS(1:SUR%L_STA,IND_SCN)
         SUR%HA_OBS(1:SUR%L_STA,J2)   = SUR_OLD%HA_OBS(1:SUR%L_STA,IND_SCN)
         SUR%AZ_ACC_OBS(1:SUR%L_STA,J2) = SUR_OLD%AZ_ACC_OBS(1:SUR%L_STA,IND_SCN)
         SUR%HA_ACC_OBS(1:SUR%L_STA,J2) = SUR_OLD%HA_ACC_OBS(1:SUR%L_STA,IND_SCN)
         SUR%SLEW_DUR(1:SUR%L_STA,J2) = SUR_OLD%SLEW_DUR(1:SUR%L_STA,IND_SCN)
         SUR%SLEW_SCA(J2)  = SUR_OLD%SLEW_SCA(IND_SCN)
         SUR%SCAN_TYPE(J2) = SUR_OLD%SCAN_TYPE(IND_SCN)
         SUR%SOU_POCAL(J2) = SUR_OLD%SOU_POCAL(IND_SCN)
         SUR%MJD_POCAL(J2) = SUR_OLD%MJD_POCAL(IND_SCN)
         SUR%TAI_POCAL(J2) = SUR_OLD%TAI_POCAL(IND_SCN)
         SUR%IND_SRC(J2)   = SUR_OLD%IND_SRC(IND_SCN)
         SUR%SRC_TYP(J2)   = SUR_OLD%SRC_TYP(IND_SCN)
         SUR%IND_SCN_SRC(J2,1:SUR%L_SOU) = SUR%IND_SCN_SRC(IND_SCN,1:SUR%L_SOU) 
         IF ( SUR%L_SO2 > 0 ) THEN
              SUR%IND_SCN_SO2(J2,1:SUR%L_SO2) = SUR%IND_SCN_SO2(IND_SCN,1:SUR%L_SO2) 
         END IF
 420  CONTINUE 
!
      DEALLOCATE ( SUR_OLD )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_SORT  !#!  
