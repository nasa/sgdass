      SUBROUTINE PIMA_ONOF_SESS ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_ONOF_SESS 
! *                                                                      *
! * ### 17-NOV-2014  PIMA_ONOF_SESS  v1.1 (c)  L. Petrov 15-DEC-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      INTEGER*4  IUER
      CHARACTER  PIMA_FRI_USED_VERS*24, STR*128
      REAL*8     SNR, AP_LEN
      REAL*8     TIM_ARR(PIM__MUV), AMP_ARR(PIM__MUV)
      INTEGER*4  J1, J2, J3, J4, J5, J6, IND_OBS, FRG_IND, IND_BND, &
     &           UV_IND, LTIM, LFRQ, POL_MODE, FRI_STS, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IND_BND = 1 ! So far, only the first band
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
! --- Get fringe results and put them in appropriate slots of PIM object
!
      IF ( PIM%FRI_STATUS .NE. PIMA__LOADED ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_FRI_READ ( PIM, IND_BND, PIMA_FRI_USED_VERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8311, IUER, 'PIMA_ONOF_SESS', 'Error in an '// &
     &              'attempt to read results of fringing from the fringe file '// &
     &               PIM%CONF%FRINGE_FILE )
                RETURN
           END IF
      END IF
!
! --- Compute theoretical path delay for all observations (and UV coordinates as well)
!
      IF ( PIM%THE_STATUS .NE. PIMA__LOADED ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_THEO ( PIM, VTD, 'OBS_SRT', '1ST_STA', IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8312, IUER, 'PIMA_ONOF_SESS', 'Error in an attempt '// &
     &              'to compute theoretical path delays' )
                RETURN
           END IF
      END IF
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 8314, IUER, 'PIMA_ONOF_SESS', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
!
      DO 420 J2=1,PIM%CONF%FRIB_NOBS
         IND_OBS = PIM%CONF%FRIB_OBS(J2)
         IND_BND = 1
         FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
         SNR = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,IND_BND)/PIM%OBS(IND_OBS)%NOISE(IND_BND)
!
         IF ( PIM%CONF%ONOF_GEN_FLAGS_MODE == PIMA__ONOF_CREATE  .AND. &
     &        ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG  )               ) THEN
!
! ----------- If we are in create mode, then initialize all existing user
! ----------- flags
!
              PIM%OBS(IND_OBS)%USER_FLAG = 1.0
         END IF
!
! ------ Bypass observations that 
! ------ 1) have the SNR below the detection threshold
! ------ 2) are in the deselection list
!
         IF ( SNR < PIM%CONF%FRIB_SNR_DETECTION ) GOTO 420
         IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 420 ! Bypass deselected observation
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              WRITE ( 6, 110 ) PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE)), &
     &                        IND_OBS, PIM%CONF%FRIB_NOBS, CHAR(13)
 110          FORMAT ( 2X, 'PIMA onof ', A,' Processing observation: ', &
     &                 I6, ' ( ',I6, ' )', 2X,A$ )
              CALL FLUSH ( 6 )
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_POL_MODE ( PIM, PIM%CONF%POLAR, IND_OBS, POL_MODE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_LOG ( 8315, IUER, 'PIMA_ONOF_SESS', 'Cannot set polarization '// &
     &            'mode for observation '//STR )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_OBS ( PIM, VTD, IND_OBS, POL_MODE, 4, &
     &                       .FALSE., .TRUE., .TRUE., FRI_STS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_LOG ( 8316, IUER, 'PIMA_ONOF_SESS', 'Failure to load '// &
     &             'the '//STR(1:I_LEN(STR))//' th observation and apply '// &
     &             'all necessary calibrations'  )
              RETURN
         END IF
         IF ( FRI_STS .NE. 0 ) GOTO 420
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_ONOF_PROC ( PIM, IND_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_LOG ( 8317, IUER, 'PIMA_ONOF_SESS', 'Failure in processing '// &
     &            'on-off flagging request for the '//STR(1:I_LEN(STR))// &
     &            ' th observation' )
              RETURN
         END IF
!
         LTIM = PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)
         DO 430 J3=1,LTIM 
            UV_IND = PIM%OBS(IND_OBS)%UV_IND(J3,FRG_IND)
            TIM_ARR(J3) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                    PIM%OBS(IND_OBS)%TIM_BEG
            AMP_ARR(J3) = ABS(PIM%OBS(IND_OBS)%UV_BAND(J3,1))
 430     CONTINUE 
!
         IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV      ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV      )
         IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV_IF   ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV_IF   )
         IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV_BAND ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV_BAND )
         IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%WEI_1D  ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%WEI_1D  )
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_WRITE_TIME_FLAG ( PIM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8318, IUER, 'PIMA_ONOF_SESS', 'Failure in an '// &
     &         'attempt to write the user flag in the output file' )
           RETURN
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, 120 ) TRIM(PIM%CONF%TIME_FLAG_FILE)
 120       FORMAT ( 'PIMA_ONOF_SESS: Flags are written in ', A )
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_ONOF_SESS  !#!  
