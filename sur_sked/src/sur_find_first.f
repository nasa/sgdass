      SUBROUTINE SUR_FIND_FIRST ( SUR, VTD, MJD_OBS, TAI_OBS, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_FIND_FIRST 
! *                                                                      *
! * ### 11-OCT-2005  SUR_FIND_FIRST  v1.0 (c)  L. Petrov 11-OCT-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD_OBS, IUER
      REAL*8     TAI_OBS
      REAL*8       SUR__EL_MIN_FIRST, SUR__EL_MAX_FIRST, &
     &             SUR__AZ_MIN_FIRST, SUR__AZ_MAX_FIRST, &
     &             SUR__EL_OPT_FIRST, SUR__AZ_OPT_FIRST
      PARAMETER  ( SUR__EL_OPT_FIRST =  70.0D0*DEG__TO__RAD )
      PARAMETER  ( SUR__AZ_OPT_FIRST =  00.0D0*DEG__TO__RAD )
      PARAMETER  ( SUR__EL_MIN_FIRST =  10.0D0*DEG__TO__RAD )
      PARAMETER  ( SUR__EL_MAX_FIRST =  84.0D0*DEG__TO__RAD )
      PARAMETER  ( SUR__AZ_MIN_FIRST =   1.0D0*DEG__TO__RAD )
      PARAMETER  ( SUR__AZ_MAX_FIRST = 359.0D0*DEG__TO__RAD )
!!!
!@      PARAMETER  ( SUR__EL_MIN_FIRST =  50.0D0*DEG__TO__RAD )
!@      PARAMETER  ( SUR__EL_MAX_FIRST =  84.0D0*DEG__TO__RAD )
!@      PARAMETER  ( SUR__AZ_MIN_FIRST =    0.0D0*DEG__TO__RAD )
!@      PARAMETER  ( SUR__AZ_MAX_FIRST =   60.0D0*DEG__TO__RAD )
      REAL*8     SUR__A_FIRST, SUR__B_FIRST
      PARAMETER  ( SUR__A_FIRST = 1.0D0 )
      PARAMETER  ( SUR__B_FIRST = 0.1D0 )
      REAL*8     SCORE(SUR__M_SOU), AZ, EL, HA
      LOGICAL*4  SUR_CHECK_VIS 
      INTEGER*4  J1, J2, J3, J4, J5, I_TYP, L_OBJ, IND_MAX, L_WIN, IER
      INTEGER*4, EXTERNAL :: MAX_LIST_R8 
!
      L_WIN = 0
      I_TYP = SUR__TYP_TAG 
!@      I_TYP = SUR__TYP_SEC
      IF ( I_TYP == SUR__TYP_TAG ) THEN
           L_OBJ = SUR%L_SOU
         ELSE
           L_OBJ = SUR%L_SO2
      END IF
!
      SUR%SLEW_DUR = 0.0D0
      DO 410 J1=1,L_OBJ
         SCORE(J1) = 0.0D0
         SUR%SOU(J1)%FL_USE = .FALSE.
         IF ( .NOT. SUR%SOU(J1)%FL_USE ) THEN
            DO 420 J2=1,SUR%L_STA
               CALL ERR_PASS ( IUER, IER )
               CALL SUR_AZEL ( SUR, VTD, I_TYP, MJD_OBS, TAI_OBS, J2, J1, &
     &                         AZ, EL, HA, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1671, IUER, 'SUR_FIND_FIRST', 'Error in '// &
     &                   'computing azimuth and elevation' ) 
                    RETURN 
               END IF
!
               IER = -1 
               IF ( .NOT. SUR_CHECK_VIS ( SUR, J2, I_TYP, J1, AZ, EL, HA, IER ) ) THEN
                    SCORE(J1) = 0.0D0
                    GOTO 410
               END IF
!
               IF ( AZ < 0.0D0 ) AZ = AZ + PI2
               IF ( EL < SUR__EL_MIN_FIRST .OR. &
     &              EL > SUR__EL_MAX_FIRST .OR. &
     &              AZ < SUR__AZ_MIN_FIRST .OR. &
     &              AZ > SUR__AZ_MAX_FIRST      ) THEN
                    SCORE(J1) = 0.0D0
                    GOTO 410
               END IF
               SCORE(J1) = SUR__A_FIRST*(AZ - SUR__AZ_OPT_FIRST)**2 + &
     &                     SUR__B_FIRST*(EL - SUR__EL_OPT_FIRST)**2
 420        CONTINUE 
            L_WIN = L_WIN + 1
         END IF
 410  CONTINUE 
      IF ( L_WIN == 0 ) THEN
           CALL ERR_LOG ( 1672, IUER, 'SUR_FIND_FIRST', 'No suitable '// &
     &         'source to start has been found' ) 
           RETURN 
      END IF
      IND_MAX = MAX_LIST_R8 ( SUR%L_SOU, SCORE )
!
      SUR%L_SCN = 1
      SUR%L_OBS_TAG = 1
      SUR%SOU(IND_MAX)%FL_USE = .TRUE.
      SUR%IND_SRC(SUR%L_SCN)  = IND_MAX
      SUR%L_TAP = 1
      SUR%IND_TAP(SUR%L_SCN)  = SUR%L_TAP 
      SUR%MJD_OBS_BEG(SUR%L_SCN) = MJD_OBS
      SUR%MJD_OBS_END(SUR%L_SCN) = MJD_OBS
      SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS
      SUR%TAI_OBS_END(SUR%L_SCN) = TAI_OBS + SUR%SOU(IND_MAX)%DUR
      IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_02' ) THEN
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) + &
     &                                  SUR__FIRST_EXTRA_TIM
      END IF
      IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
      END IF
      SUR%SRC_TYP(SUR%L_SCN) = I_TYP
      SUR%L_SCN_SO1 = 1
!    
      SUR%MJD_TAPE_START_CUR = SUR%MJD_OBS_BEG(SUR%L_SCN)
      SUR%TAI_TAPE_START_CUR = SUR%TAI_OBS_BEG(SUR%L_SCN) - SUR__TAPE_MOTION 
      IF ( SUR%TAI_TAPE_START_CUR < 0.0D0 ) THEN
           SUR%MJD_TAPE_START_CUR = SUR%MJD_TAPE_START_CUR - 1 
           SUR%TAI_TAPE_START_CUR = SUR%TAI_TAPE_START_CUR + 86400.0D0
      END IF
!
      SUR%MJD_CALIB_START_CUR = SUR%MJD_OBS_BEG(SUR%L_SCN) 
      SUR%TAI_CALIB_START_CUR = SUR%TAI_OBS_BEG(SUR%L_SCN) 
!
      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN) 
      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN) 
!
      DO 430 J3=1,SUR%L_STA
         CALL ERR_PASS ( IUER, IER )
         CALL SUR_AZEL ( SUR, VTD, I_TYP, MJD_OBS, TAI_OBS, J3, &
     &                   IND_MAX, AZ, EL, HA, IER )
         SUR%STA(J3)%EL_CUR = EL
         SUR%STA(J3)%AZ_CUR = AZ
         SUR%STA(J3)%AZ_ACC_CUR = AZ
         SUR%EL_OBS(J3,SUR%L_SCN) = EL
         SUR%AZ_OBS(J3,SUR%L_SCN) = AZ
         SUR%HA_OBS(J3,SUR%L_SCN) = HA
         SUR%AZ_ACC_OBS(J3,SUR%L_SCN) = AZ
         SUR%HA_ACC_OBS(J3,SUR%L_SCN) = HA
!
         SUR%STA(J3)%ALP_CUR    = SUR%SOU(IND_MAX)%ALPHA
         SUR%STA(J3)%DEL_CUR    = SUR%SOU(IND_MAX)%DELTA
         SUR%STA(J3)%HA_CUR     = HA
         SUR%STA(J3)%HA_ACC_CUR = HA
         SUR%OBS_STA(J3,SUR%L_SCN) = SUR__USED 
 430  CONTINUE 
      SUR%SCAN_TYPE(SUR%L_SCN) = SUR__FIRST
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      WRITE ( 6, * ) ' L_WIN =', L_WIN,' l_obj =', l_obj, ' IND_MAX = ', IND_MAX
!         write ( 6, * ) ' score= ', score(ind_max) ! %%%
!         write ( 6, * ) ' ind_max=',ind_max, ' name: ', sur%sou(ind_max)%j2000_name ! %%%
!         write ( 6, * ) ' sur%sou(ind_max)%alpha = ', sur%sou(ind_max)%alpha, &
!     &                  ' sur%sou(ind_max)%delta = ', sur%sou(ind_max)%delta  ! %%%
!         write ( 6, * ) ' al_1: mjd = ', mjd_obs, ' tai: ', tai_obs  ! %%%%%%
!         write ( 6, * ) ' el_1: ', SUR%EL_OBS(1,1)/DEG__TO__RAD
!         write ( 6, * ) ' el_2: ', SUR%EL_OBS(2,1)/DEG__TO__RAD
!         write ( 6, * ) ' el_3: ', SUR%EL_OBS(3,1)/DEG__TO__RAD
!         write ( 6, * ) ' el_4: ', SUR%EL_OBS(4,1)/DEG__TO__RAD
!         call pause ( 'sur_find_first' ) ! %%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_FIND_FIRST  !#!#
