      SUBROUTINE PIMA_SPLT_SORT ( PIM, LFRQ, LTIM, STA_IND_ARR, WEI_ARR, &
     &                            TIM_ARR, IND_OBS_ARR, UVO, SNR_ARR )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_SPLT_SORT 
! *                                                                      *
! * ### 19-MAY-2012  PIMA_SPLT_SORT  v1.0 (c) L. Petrov 19-MAY-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  IUER
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4  
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SPLT_SORT  !#!#
