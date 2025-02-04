      SUBROUTINE VTD_MOMENT_STRUC ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_MOMENT_STRUC  ??????????????????
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 01-MAR-2004  VTD_MOMENT_STRUC v1.0 (c) L. Petrov 09-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      INTEGER*4  IBAND, MDIM, NN_ARR(2), J1, J2, J3, IND1, IND2, IOS, IER
      REAL*8     TIM_SOU_BEG, TIM_SOU_END, TIM_MOM
      CHARACTER  STR*80
      INTEGER*4  I_LEN
!
      IF ( VTD%L_STR .GT. 0 ) THEN
         TIM_MOM = ( VTD%MOM%MJD - J2000__MJD - 0.5D0)*86400.0D0 + VTD%MOM%TAI
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  VTD_MOMENT_STRUC 
