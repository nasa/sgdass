      FUNCTION SUR_ASTRO_SCORE ( SUR, SLEW_TIM, ELEV_REF, CUR_TYP, IND_SRC )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_ASTRO_SCORE
! *                                                                      *
! * ### 15-DEC-2011 SUR_ASTRO_SCORE v1.1 (c)  L. Petrov  03-FEB-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      REAL*8     SUR_ASTRO_SCORE 
      REAL*8     SLEW_TIM, ELEV_REF
      CHARACTER  STR*32
      REAL*8     DEL, EL_MAX, TIM_CUL, DEC_WGT, CUL_WGT, SLE_WGT, ELE_WGT, &
     &           TIM_SINCE_BEG, TIM_EXP_DUR, SUR_SKED_TIM_REM_RESCORE
      INTEGER*4  CUR_TYP, IND_SRC 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      TIM_SINCE_BEG = (SUR%MJD_CUR  - SUR%MJD_START)*86400.0D0 + (SUR%TAI_CUR  - SUR%TAI_START)
      TIM_EXP_DUR   = (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + (SUR%TAI_STOP - SUR%TAI_START)
      CALL GETENVAR ( 'SUR_SKED_TIM_REM_RESCORE', STR )
      IF ( ILEN(STR) == 0 ) THEN
           SUR_SKED_TIM_REM_RESCORE = 1.0D0
         ELSE
           READ ( UNIT=STR, FMT='(F12.5)' ) SUR_SKED_TIM_REM_RESCORE 
           IF ( TIM_SINCE_BEG/TIM_EXP_DUR .LT. 0.3 ) THEN
                SUR_SKED_TIM_REM_RESCORE = SUR_SKED_TIM_REM_RESCORE*3.0
             ELSE IF ( TIM_SINCE_BEG/TIM_EXP_DUR .LT. 0.3 ) THEN
                CONTINUE 
             ELSE IF ( TIM_SINCE_BEG/TIM_EXP_DUR .LT. 0.5 ) THEN
                SUR_SKED_TIM_REM_RESCORE = 1.0
             ELSE IF ( TIM_SINCE_BEG/TIM_EXP_DUR .LT. 0.7 ) THEN
                SUR_SKED_TIM_REM_RESCORE = 1.0/SUR_SKED_TIM_REM_RESCORE/3.0
             ELSE IF ( TIM_SINCE_BEG/TIM_EXP_DUR .LT. 0.8 ) THEN
                SUR_SKED_TIM_REM_RESCORE = 1.0/SUR_SKED_TIM_REM_RESCORE/5.0
             ELSE IF ( TIM_SINCE_BEG/TIM_EXP_DUR .LT. 0.9 ) THEN
                SUR_SKED_TIM_REM_RESCORE = 1.0/SUR_SKED_TIM_REM_RESCORE/10.0
             ELSE
                SUR_SKED_TIM_REM_RESCORE = 1.0/SUR_SKED_TIM_REM_RESCORE/30.0
           END IF
      END IF
!
      IF ( CUR_TYP == SUR__TYP_TAG ) THEN
           DEL = SUR%SOU(IND_SRC)%DELTA 
           EL_MAX = SUR%ELEV_MAX(IND_SRC,1)
           TIM_CUL = (SUR%MJD_CUR - SUR%SOU(IND_SRC)%MJD_UP_CULM)*86400.0D0 + &
     &               (SUR%TAI_CUR - SUR%SOU(IND_SRC)%TAI_UP_CULM)
         ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
           DEL = SUR%SO2(IND_SRC)%DELTA 
           EL_MAX = SUR%ELEV_MAX(IND_SRC,2)
           TIM_CUL = (SUR%MJD_CUR - SUR%SO2(IND_SRC)%MJD_UP_CULM)*86400.0D0 + &
     &               (SUR%TAI_CUR - SUR%SO2(IND_SRC)%TAI_UP_CULM)
         ELSE IF ( CUR_TYP == SUR__TYP_CAL) THEN
           DEL = SUR%CAL(IND_SRC)%DELTA 
           EL_MAX = SUR%ELEV_MAX(IND_SRC,3)
           TIM_CUL = (SUR%MJD_CUR - SUR%CAL(IND_SRC)%MJD_UP_CULM)*86400.0D0 + &
     &               (SUR%TAI_CUR - SUR%CAL(IND_SRC)%TAI_UP_CULM)
      END IF
!
      IF ( SUR%SOU(IND_SRC)%DELTA < 20.0D0*DEG__TO__RAD ) THEN
           DEC_WGT = 0.05*(30.0D0 - DEL/DEG__TO__RAD)**2
         ELSE IF ( SUR%SOU(IND_SRC)%DELTA > +20.0D0*DEG__TO__RAD ) THEN
           DEC_WGT = 1.0D0
         ELSE
           DEC_WGT = 1.0D0
      END IF
!
      CUL_WGT = 1.D6/(1.0D0 + TIM_CUL**4 )
      SLE_WGT = 1.D4/(1.0D0 + SLEW_TIM)
      ELE_WGT = 1.D4*(1.0D0 - DABS(EL_MAX - ELEV_REF))**8
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) ' ele_wgt= ', ele_wgt, ' el_dif= ', (elev_ref - el_max) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IF ( SUR%ALGORITHM == 'ASTROMET_12' ) THEN
           SUR_ASTRO_SCORE = 10.0D0*(DEC_WGT + SLE_WGT + CUL_WGT)
        ELSE IF ( SUR%ALGORITHM == 'ASTROMET_13' ) THEN
           IF ( ELEV_REF > 1.0D0 ) THEN
                SUR_ASTRO_SCORE = 1.0D4 + SLE_WGT 
              ELSE
                SUR_ASTRO_SCORE = ELE_WGT/DSIN( MAX(0.1D0, EL_MAX) )
           END IF 
        ELSE IF ( SUR%ALGORITHM == 'IMAGING_01' ) THEN
           SUR_ASTRO_SCORE = 1.0D0
      END IF
      SUR_ASTRO_SCORE = SUR_SKED_TIM_REM_RESCORE*SUR_ASTRO_SCORE 
!
      RETURN
      END  FUNCTION  SUR_ASTRO_SCORE  !#!#
