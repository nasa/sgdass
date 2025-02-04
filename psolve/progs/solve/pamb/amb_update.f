      SUBROUTINE AMB_UPDATE ( BAND, AMB_ADD, OBSBAS, PAMBI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  AMB_UPDATE  updates fields of data strucutres  OBSBAS and *
! *   PAMBI for the changes of phase delay ambiguty at the band BAND.    *
! *   It update ambiguity counter, phase delay observable, PX_GXS and    *
! *   P_PXS residuals.                                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    BAND ( INTEGER*4 ) -- Code of the band in use: 1 -- X-band,       *
! *                          2 -- S-band.                                *
! * AMB_ADD ( INTEGER*4 ) -- Increment of integer ambiguity. Ambiguity   *
! *                          counter after work of of AMB_UPDATE will    *
! *                          be increased by AMB_ADD.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  OBSBAS ( RECORD    ) -- Array of data structures which keeps        *
! *                          baseline dependent information about the    *
! *                          session.                                    *
! *   PAMBI ( RECORD    ) -- Array of data structures keeping            *
! *                          information about phase delays, their       *
! *                          errors, ambiguities and etc.                *
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
! *  ###  01-OCT-98   AMB_UPDATE   v1.0  (c)  L. Petrov  01-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      INTEGER*4  BAND, AMB_ADD, IUER
      TYPE ( BAS_O__STRU ) ::  OBSBAS
      TYPE ( PAMBI__STRU ) ::  PAMBI
      CHARACTER  STR*20
!
      IF ( BAND .EQ. PAMB__XBAND ) THEN
!
! -------- Update of OBSBAS and PAMBI data structure for ambiguity at the X-band
!
           PAMBI%NPHAMB_X   = PAMBI%NPHAMB_X + AMB_ADD
           OBSBAS%TAUPH_OBS = OBSBAS%TAUPH_OBS + DFLOAT(AMB_ADD)/ &
     &                        OBSBAS%FREQ_OBSV_PH
           PAMBI%RES_PX_GXS = PAMBI%RES_PX_GXS + DFLOAT(AMB_ADD)/ &
     &                        OBSBAS%FREQ_OBSV_PH
           PAMBI%RES_P_PXS  = PAMBI%RES_P_PXS  + &
     &                        DFLOAT(AMB_ADD)/OBSBAS%FREQ_OBSV_PH * &
     &     OBSBAS%FREQ_IONO_PH/( OBSBAS%FREQ_IONO_PH - OBSBAS%FREQ_IONO_PH_OPP )
         ELSE IF ( BAND .EQ. PAMB__SBAND ) THEN
!
! -------- Update of OBSBAS and PAMBI data structure for ambiguity at the S-band
!
           PAMBI%NPHAMB_S       = PAMBI%NPHAMB_S + AMB_ADD
           OBSBAS%TAUPH_OBS_OPP = OBSBAS%TAUPH_OBS_OPP + DFLOAT(AMB_ADD)/ &
     &                            OBSBAS%FREQ_OBSV_PH_OPP
           PAMBI%RES_PS_GXS = PAMBI%RES_PS_GXS + DFLOAT(AMB_ADD)/ &
     &                        OBSBAS%FREQ_OBSV_PH_OPP
           PAMBI%RES_P_PXS  = PAMBI%RES_P_PXS - &
     &                        DFLOAT(AMB_ADD)/OBSBAS%FREQ_OBSV_PH * &
     &                        OBSBAS%FREQ_IONO_PH_OPP/ &
     &                        ( OBSBAS%FREQ_IONO_PH - OBSBAS%FREQ_IONO_PH_OPP )
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( BAND, STR )
           CALL ERR_LOG ( 5401, IUER, 'AMB_UPDATE', 'Wrong value of input '// &
     &         'parameter BAND: '//STR )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  AMB_UPDATE  #!#
