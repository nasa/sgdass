      SUBROUTINE PIMA_TIM_MERGE ( PIM, IND_OBS, IPTM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_TIM_MERGE
! *                                                                      *
! * ### 02-JAN-2020  PIMA_TIM_MERGE  v1.0 (c) L. Petrov  02-JAN-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  IPTM, IND_OBS, IUER
      TYPE     ( PIMA__TYPE  ) :: PIM
!
!            INTEGER*4  TIM_IND
!	     INTEGER*4  NUM_EPC(PIM__MUVS)
!            INTEGER*4  TIM_END_IND
!	     INTEGER*4  NUM_AP_SPAN(PIM__MUVS)
!	     REAL*8     TIM_BEG
!	     REAL*8     TIM_END
!	     INTEGER*4, POINTER :: UV_IND(:,:)     => NULL() ! UV indices
!	     INTEGER*4, POINTER :: CORR_FLAG(:,:)  => NULL() ! Flag set by the correlator
!!
!	     COMPLEX*8, POINTER :: RES_FRN(:,:)        => NULL() ! Uncalibrated postfit residuals
!	     REAL*4,    POINTER :: USER_FLAG(:)        => NULL() ! User-supplied time dependent weights
!	     COMPLEX*8, POINTER :: UV(:,:,:,:)         => NULL() ! Visibility data
!	     COMPLEX*8, POINTER :: UV_IF(:,:,:)        => NULL() ! Visibility data averaged over the IF
!	     COMPLEX*8, POINTER :: UV_BAND(:,:)        => NULL() ! Visibility data averaged over the band
!!	     COMPLEX*8, POINTER :: AC(:,:,:,:,:)       => NULL() ! Autocorrelation data
!	     REAL*4,    POINTER :: AC_AVR_TIM(:,:,:,:) => NULL() ! Autocorrelation data averaged over time PIM%NCHN,LFRQ,2,PIM%NSTK
!	     REAL*4,    POINTER :: AC_MEAN(:,:,:)      => NULL() ! Autocorrelation data averaged over time after sampling correction LFRQ,2,PIM%NSTK
!	     REAL*4,    POINTER :: WEI_1D(:,:)         => NULL() ! 1-dimension weights
!	     REAL*4,    POINTER :: WVR_DELAY(:)        => NULL() ! WVR delay
! 	     REAL*4,    POINTER :: TSRF(:,:,:)         => NULL() ! Tsys Renormalization factor to account for discarding spactral channels LFRQ,2,PIM%NSTK
!
      WRITE ( 6, * ) 'TIM_IND     = ', PIM%OBS(IND_OBS)%TIM_BEG_IND
      WRITE ( 6, * ) 'NUM_EPC     = ', PIM%OBS(IND_OBS)%NUM_EPC
      WRITE ( 6, * ) 'TIM_END_IND = ', PIM%OBS(IND_OBS)%TIM_END_IND
      WRITE ( 6, * ) 'NUM_AP_SPAN = ', PIM%OBS(IND_OBS)%NUM_AP_SPAN
      WRITE ( 6, * ) 'TIM_BEG     = ', PIM%OBS(IND_OBS)%TIM_BEG
      WRITE ( 6, * ) 'TIM_END     = ', PIM%OBS(IND_OBS)%TIM_END
      WRITE ( 6, * ) 'Sha(UV_IND)     = ', SHAPE(PIM%OBS(IND_OBS)%UV_IND)
      WRITE ( 6, * ) 'Sha(CORR_FLAG)  = ', SHAPE(PIM%OBS(IND_OBS)%CORR_FLAG)
      WRITE ( 6, * ) 'Sha(RES_FRN)    = ', SHAPE(PIM%OBS(IND_OBS)%RES_FRN)
      WRITE ( 6, * ) 'Sha(USER_FLAG)  = ', SHAPE(PIM%OBS(IND_OBS)%USER_FLAG)
      WRITE ( 6, * ) 'Sha(UV)         = ', SHAPE(PIM%OBS(IND_OBS)%UV)
      WRITE ( 6, * ) 'Sha(UV_IF)      = ', SHAPE(PIM%OBS(IND_OBS)%UV_IF)
      WRITE ( 6, * ) 'Sha(UV_BAND)    = ', SHAPE(PIM%OBS(IND_OBS)%UV_BAND)
      WRITE ( 6, * ) 'Sha(AC)         = ', SHAPE(PIM%OBS(IND_OBS)%AC)
      WRITE ( 6, * ) 'Sha(AC_AVR_TIM) = ', SHAPE(PIM%OBS(IND_OBS)%AC_AVR_TIM)
      WRITE ( 6, * ) 'Sha(AC_MEAN)    = ', SHAPE(PIM%OBS(IND_OBS)%AC_MEAN)
      WRITE ( 6, * ) 'Sha(WEI_1D)     = ', SHAPE(PIM%OBS(IND_OBS)%WEI_1D)
      WRITE ( 6, * ) 'Sha(WVR_DELAY)  = ', SHAPE(PIM%OBS(IND_OBS)%WVR_DELAY)
      WRITE ( 6, * ) 'Sha(TSRF)       = ', SHAPE(PIM%OBS(IND_OBS)%TSRF)
      call exit ( 1 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_TIM_MERGE   !#!  
