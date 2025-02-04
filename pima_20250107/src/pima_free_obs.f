      SUBROUTINE PIMA_FREE_OBS ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FREE_OBS  deallocates dynamic mmeory claimed when    *
! *   observations were loaded.                                          *
! *                                                                      *
! * ### 04-AUG-2006  PIMA_FREE_OBS  v1.7 (c)  L. Petrov  29-AUG-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4  IUER
      INTEGER*4  J2, J3, J4, J5, J6, J7, J8, J9
!
      DO 420 J2=1,PIM%NSTA
         DEALLOCATE ( PIM%STA(J2)%MOD )
         IF ( PIM%STA(J2)%L_MDC > 0 ) THEN
              DEALLOCATE ( PIM%STA(J2)%MDC%IND_SOU )
              DEALLOCATE ( PIM%STA(J2)%MDC%TIME_CEN )
              DEALLOCATE ( PIM%STA(J2)%MDC%CLOCK_OFFSET )
              DEALLOCATE ( PIM%STA(J2)%MDC%CLOCK_RATE )
              DEALLOCATE ( PIM%STA(J2)%MDC%ATMO_DELAY )
              DEALLOCATE ( PIM%STA(J2)%MDC%ATMO_RATE )
              DEALLOCATE ( PIM%STA(J2)%MDC%GDELAY )
              DEALLOCATE ( PIM%STA(J2)%MDC%GRATE )
         END IF
         IF ( PIM%STA(J2)%CABLE%CAB_AVAIL ) THEN
              DEALLOCATE ( PIM%STA(J2)%CABLE%CAB_DEL )
              DEALLOCATE ( PIM%STA(J2)%CABLE%TIM_CAB )
         END IF
         IF ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL ) THEN
!
! ----------- Deallocate dynamic memory for phase calibration data
!
              DEALLOCATE ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%FREQ  )
              DEALLOCATE ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%AMPL  )
              DEALLOCATE ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%PHAS  )
              DEALLOCATE ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%RATE  )
              DEALLOCATE ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%SOU_IND      )
              DEALLOCATE ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8  )
              DEALLOCATE ( PIM%STA(J2)%PCAL(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 )
         END IF
!
         IF ( PIM%STA(J2)%TSYS(PIM%CONF%FRQ_GRP)%AVAIL ) THEN
!
! ----------- Deallocate dynamic memory for system temperature data
!
              DEALLOCATE ( PIM%STA(J2)%TSYS(PIM%CONF%FRQ_GRP)%TSYS         )
              DEALLOCATE ( PIM%STA(J2)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8  )
              DEALLOCATE ( PIM%STA(J2)%TSYS(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 )
              DEALLOCATE ( PIM%STA(J2)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4   )
              DEALLOCATE ( PIM%STA(J2)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4 )
              DEALLOCATE ( PIM%STA(J2)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND )
         END IF
         IF ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%N_OPA > 0 ) THEN
              DEALLOCATE ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%OPA )
              PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%OPA_AVAIL = .FALSE.
         END IF
         IF ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%N_TAT > 0 ) THEN
              DEALLOCATE ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TAT )
         END IF
         IF ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%N_TREC > 0 ) THEN
              DEALLOCATE ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TREC )
              PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TREC_AVAIL = .FALSE.
         END IF
         IF ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%N_TSPI > 0 ) THEN
              DEALLOCATE ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TSPI )
              PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TSPI_AVAIL = .FALSE.
         END IF
         IF ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%N_TSYS > 0 ) THEN
              DEALLOCATE ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TSYS_CLN )
              DEALLOCATE ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD )
              PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TSYS_AVAIL = .FALSE.
         END IF
         IF ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%N_TTOA > 0 ) THEN
              DEALLOCATE ( PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TTOA )
              PIM%STA(J2)%STMO(PIM%CONF%FRQ_GRP)%TTOA_AVAIL = .FALSE.
         END IF
!
         IF ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL ) THEN
!
! ----------- Deallocate dynamic memory for gain information
!
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%TYP   )
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%NTERM )
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP )
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP )
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL )
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL )
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%GAIN  )
              DEALLOCATE ( PIM%STA(J2)%GAIN(PIM%CONF%FRQ_GRP)%SENS  )
         END IF
!
         IF ( PIM%STA(J2)%WEATHER%AVAIL ) THEN
!
! ----------- Deallocate dynamic memory for weather data
!
              DEALLOCATE ( PIM%STA(J2)%WEATHER%TIME_BEG )
              DEALLOCATE ( PIM%STA(J2)%WEATHER%TIME_END )
              DEALLOCATE ( PIM%STA(J2)%WEATHER%PRES  )
              DEALLOCATE ( PIM%STA(J2)%WEATHER%TEMP  )
              DEALLOCATE ( PIM%STA(J2)%WEATHER%HUMID )
         END IF
 420  CONTINUE 
!
      DO 440 J4=1,PIM%NSCA
         DEALLOCATE ( PIM%SCA(J4)%OBS_IND )
         DEALLOCATE ( PIM%SCA(J4)%AUT_IND )
 440  CONTINUE 
!
      DO 450 J5=1,PIM%L_FIL
         DEALLOCATE ( PIM%FILE(J5)%KEY )
 450  CONTINUE 
      DEALLOCATE ( PIM%FILE )
!
      DO 470 J7=1,PIM%NOBS
         DEALLOCATE ( PIM%OBS(J7)%UV_IND    )
         DEALLOCATE ( PIM%OBS(J7)%CORR_FLAG )
         DEALLOCATE ( PIM%OBS(J7)%RES_FRN   )
         IF ( ASSOCIATED ( PIM%OBS(J7)%USER_FLAG ) ) THEN
              DEALLOCATE ( PIM%OBS(J7)%USER_FLAG )
         END IF
         IF ( ASSOCIATED ( PIM%OBS(J7)%UV ) ) THEN
              DEALLOCATE ( PIM%OBS(J7)%UV )
         END IF
         IF ( ASSOCIATED ( PIM%OBS(J7)%UV_IF ) ) THEN
              DEALLOCATE ( PIM%OBS(J7)%UV_IF )
         END IF
         IF ( ASSOCIATED ( PIM%OBS(J7)%UV_BAND ) ) THEN
              DEALLOCATE ( PIM%OBS(J7)%UV_BAND )
         END IF
         IF ( ASSOCIATED ( PIM%OBS(J7)%WEI_1D ) ) THEN
              DEALLOCATE ( PIM%OBS(J7)%WEI_1D )
         END IF
!
         PIM%OBS(J7)%UV_IND    => NULL() ! UV indices
         PIM%OBS(J7)%CORR_FLAG => NULL() ! Flag set by the correlator
         PIM%OBS(J7)%RES_FRN   => NULL() ! Uncalibrated postfit residuals
         PIM%OBS(J7)%USER_FLAG => NULL() ! User-supplied time dependent weights
         PIM%OBS(J7)%UV        => NULL() ! UV data
         PIM%OBS(J7)%UV_IF     => NULL() ! UV data averaged over the IF
         PIM%OBS(J7)%UV_BAND   => NULL() ! UV data averaged over the band
         PIM%OBS(J7)%WEI_1D    => NULL() ! 1-dimension weights
 470  CONTINUE 
      DO 480 J8=1,PIM%NAUT
         DEALLOCATE ( PIM%AUT(J8)%UV_IND )
 480  CONTINUE 
      IF ( PIM%L_MKDB > 0 ) THEN
           DO 490 J9=1,PIM%L_MKDB
              IF ( ALLOCATED  ( PIM%SCADB(J9)%OBS_IND ) ) THEN
                   DEALLOCATE ( PIM%SCADB(J9)%OBS_IND )
              END IF
 490       CONTINUE 
      END IF
      DEALLOCATE ( PIM%UV_IND )
      DEALLOCATE ( PIM%OBS    )
      DEALLOCATE ( PIM%AUT    )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FREE_OBS  !#!#
