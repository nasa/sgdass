      SUBROUTINE SIMOBS_COMP ( SIMUL_OBS, SIMUL_SIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMOBS_COMP
! *                                                                      *
! *  ### 14-DEC-2020   SIMOBS_COMP  v1.0 (c)  L. Petrov  14-DEC-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      INCLUDE   'simul.i'
      TYPE     ( SIMUL__TYPE ) :: SIMUL_OBS, SIMUL_SIM
      INTEGER*4  NUMOBS_SIM, NUMSCA_SIM, NUMSTA_SIM, NUMSOU_SIM
      INTEGER*4  NUMOBS_OBS, NUMSCA_OBS, NUMSTA_OBS, NUMSOU_OBS
      REAL*8     TIM_MARGIN
      PARAMETER  ( TIM_MARGIN = 180.0D0 )
      CHARACTER  STR_UTC_OBS*30
      REAL*8     TIM_UTC_OBS, TIM_UTC_SIM
      INTEGER*4  IND_OBS_SIM, N_MIS, J1, J2, J3, J4, IER
      INTEGER*4  IUER 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      WRITE ( 6, * ) 'NUMOBS_SIM= ', SIMUL_SIM%NOBS, ' NUMOBS_OBS= ', SIMUL_OBS%NOBS
      WRITE ( 6, * ) 'NUMSCA_SIM= ', SIMUL_SIM%NSCA, ' NUMSCA_OBS= ', SIMUL_OBS%NSCA
      WRITE ( 6, * ) 'NUMSTA_SIM= ', SIMUL_SIM%NSTA, ' NUMSTA_OBS= ', SIMUL_OBS%NSTA
      WRITE ( 6, * ) 'NUMSOU_SIM= ', SIMUL_SIM%NSOU, ' NUMSOU_OBS= ', SIMUL_OBS%NSOU
!
      N_MIS = 0
      DO 410 J1=1,SIMUL_OBS%NOBS
         TIM_UTC_OBS = (SIMUL_OBS%MJD_OBS(J1) - J2000__MJD)*86400.0D0 + SIMUL_OBS%UTC_OBS(J1)
         IND_OBS_SIM = 0
         DO 420 J2=1,SIMUL_SIM%NOBS
            TIM_UTC_SIM = (SIMUL_SIM%MJD_OBS(J2) - J2000__MJD)*86400.0D0 + SIMUL_SIM%UTC_OBS(J2)
            IF ( TIM_UTC_SIM < TIM_UTC_OBS - SIMUL_OBS%SCAN_DUR(J1) - TIM_MARGIN .OR. &
     &           TIM_UTC_SIM > TIM_UTC_OBS + SIMUL_OBS%SCAN_DUR(J1) + TIM_MARGIN      ) GOTO 420
!
            IF ( SIMUL_SIM%SOU_NAM(SIMUL_SIM%SOU_IND(J2)) == SIMUL_OBS%SOU_NAM(SIMUL_OBS%SOU_IND(J1)) .AND. &
     &           ( SIMUL_SIM%STA_NAM(SIMUL_SIM%STA_IND(1,J2)) == &
     &             SIMUL_OBS%STA_NAM(SIMUL_OBS%STA_IND(1,J1))    .AND. &
     &             SIMUL_SIM%STA_NAM(SIMUL_SIM%STA_IND(2,J2)) == &
     &             SIMUL_OBS%STA_NAM(SIMUL_OBS%STA_IND(2,J1))          ) .OR. &
     &           ( SIMUL_SIM%STA_NAM(SIMUL_SIM%STA_IND(1,J2)) == &
     &             SIMUL_OBS%STA_NAM(SIMUL_OBS%STA_IND(2,J1))    .AND. &
     &             SIMUL_SIM%STA_NAM(SIMUL_SIM%STA_IND(2,J2)) == &
     &             SIMUL_OBS%STA_NAM(SIMUL_OBS%STA_IND(1,J1))          )      ) THEN
!
                   IND_OBS_SIM = J2
            END IF
 420     CONTINUE 
         IF ( IND_OBS_SIM > 0 ) THEN
              STR_UTC_OBS = MJDSEC_TO_DATE ( SIMUL_OBS%MJD_OBS(J1), SIMUL_OBS%UTC_OBS(J1), IER )
              WRITE ( 6, 110 ) J1, STR_UTC_OBS(1:21), &
     &                             SIMUL_OBS%STA_NAM(SIMUL_OBS%STA_IND(1,J1)), &
     &                             SIMUL_OBS%STA_NAM(SIMUL_OBS%STA_IND(2,J1)), &
     &                             SIMUL_OBS%SOU_NAM(SIMUL_OBS%SOU_IND(J1)), &
     &                             SIMUL_OBS%SNR(1,J1), SIMUL_SIM%SNR(1,IND_OBS_SIM), &
     &                             1.D12*SIMUL_OBS%GR_DEL_ERR(1,J1), &
     &                             1.D12*SIMUL_SIM%GR_DEL_ERR(1,IND_OBS_SIM), &
     &                             SIMUL_OBS%SCAN_DUR(J1), SIMUL_SIM%SCAN_DUR(IND_OBS_SIM), &
     &                             SIMUL_OBS%AMP(1,J1),  SIMUL_SIM%AMP(1,IND_OBS_SIM), &
     &                             SIMUL_OBS%NOI(1,J1),  SIMUL_SIM%NOI(1,IND_OBS_SIM)
 110          FORMAT ( 'Ind_obs: ', I5, ' UTC: ',A, '  Sta: ', A, 1X, A, '  Sou: ', A, &
     &                 '  SNR: ', F8.2, 1X, F8.2, '  Gr_del_del: ', F8.2, 2X, F8.2, ' ps ', &
     &                 '  Dura: ', F6.1, 1X, F6.1, '  Amp: ', F9.7, 1X, F9.7, &
     &                 '  Noi: ', 1PD10.4, 1X, 1PD10.4 )
            ELSE
              WRITE ( 6, * ) 'Missed simulation for obs ', J1 
              N_MIS = N_MIS + 1
         END IF
 410  CONTINUE 
      WRITE ( 6, * ) 'N_MIS = ', N_MIS
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  SIMOBS_COMP  !#!#
