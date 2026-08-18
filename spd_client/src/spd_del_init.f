      SUBROUTINE SPD_DEL_INIT ( SPD_DEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_DEL_INIT  initializes internal data structure of      *
! *   object SPD_DEL.                                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   SPD_DEL ( SPD_DEL__TYPE ) -- Object with Slant Path Delays.        *
! *                                The internal fields of SPD_DEL        *
! *                                contain the coefficient of B-spline   *
! *                                expansion of path delay.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 24-AUG-2014   SPD_DEL_INIT  v1.3 (d) L. Petrov  08-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      SPD_DEL%LAB%OFF_DEL = 0
      SPD_DEL%LAB%LEN_DEL = 0
      SPD_DEL%NDEL        = 0
      SPD_DEL%ZEN_BIAS    = 0
      SPD_DEL%ZEN_SCALE   = 0
      SPD_DEL%N_FRQ       = 0
      SPD_DEL%N_TIM       = 0
      SPD_DEL%TIM_BEG     = 0
      SPD_DEL%STATUS      = 0
      SPD_DEL%ZEN_BIAS    = 0
      SPD_DEL%ZEN_SCALE   = 0
      SPD_DEL%N_FRQ       = 0
      SPD_DEL%N_TIM       = 0
      SPD_DEL%TIM_BEG     = 0
      SPD_DEL%STATUS      = 0
      SPD_DEL%LAB%TOT_NUM_DEL = 0
      SPD_DEL%IND_FIRST_EPO   = 0
!
      SPD_DEL%RES     => NULL()
!
      SPD_DEL%SUR_PRS => NULL()
      SPD_DEL%SUR_PWP => NULL()
      SPD_DEL%SUR_TEM => NULL()
      SPD_DEL%DELS    => NULL()
      SPD_DEL%OPA     => NULL()
      SPD_DEL%TAT     => NULL()
      SPD_DEL%MAP_ARR => NULL()
      SPD_DEL%TIM_ARR => NULL()
      SPD_DEL%FRQ_ARR => NULL()
      SPD_DEL%ZEN_DEL => NULL()
!
      SPD_DEL%MOD%TEXT  => NULL()
      SPD_DEL%MET%TEXT  => NULL()
      SPD_DEL%MF%EL_ARG => NULL()
      SPD_DEL%MF%MF_SPL => NULL()
      SPD_DEL%MF%MF_ARG => NULL()
      SPD_DEL%MF%EL_SPL => NULL()
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  SPD_DEL_INIT  !#!#
