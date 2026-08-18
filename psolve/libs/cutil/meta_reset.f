      SUBROUTINE META_RESET ()
! ************************************************************************
! *                                                                      *
! *   Routline META_RESET resets NOFS, NOFX, BQCS, BQCX, UNRC__SPS       *
! *   fields.                                                            *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 04-SEP-2019   META_RESET  v1.0 (d)  L. Petrov  04-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'oborg.i'
      INTEGER*2  IQC_X, IQC_S
      LOGICAL*4, EXTERNAL :: DATYP_INQ 
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      IQC_X = -1
      AUTO_SUP = 0  ! Added on 2025.04.05_22:53:40
      USER_SUP = 0  ! Added on 2025.04.05_22:53:42
!
!@      CALL SBIT ( AUTO_SUP, UNRC__SPS, INT2(0) )  ! equivalent to bit 31 for ibset
!@      CALL SBIT ( USER_SUP, UNRC__SPS, INT2(0) )  ! equivalent to bit 31 for ibset
!@!
!@      AUTO_SUP = IBCLR ( AUTO_SUP, INT4(CBAD__SPS) ) 
!@      USER_SUP = IBCLR ( USER_SUP, INT4(CBAD__SPS) ) 
!@!
!@      AUTO_SUP = IBCLR ( AUTO_SUP, INT4(NOFS__SPS) ) 
!@      AUTO_SUP = IBCLR ( AUTO_SUP, INT4(NOFX__SPS) ) 
!@      AUTO_SUP = IBCLR ( AUTO_SUP, INT4(BQCS__SPS) ) 
!@      AUTO_SUP = IBCLR ( AUTO_SUP, INT4(BQCX__SPS) ) 
!
      IF ( INDEX ( LQUAL_CHR, '0' ) .NE. 0 ) IQC_X =  0
      IF ( INDEX ( LQUAL_CHR, '1' ) .NE. 0 ) IQC_X =  1
      IF ( INDEX ( LQUAL_CHR, '2' ) .NE. 0 ) IQC_X =  2
      IF ( INDEX ( LQUAL_CHR, '3' ) .NE. 0 ) IQC_X =  3
      IF ( INDEX ( LQUAL_CHR, '4' ) .NE. 0 ) IQC_X =  4
      IF ( INDEX ( LQUAL_CHR, '5' ) .NE. 0 ) IQC_X =  5
      IF ( INDEX ( LQUAL_CHR, '6' ) .NE. 0 ) IQC_X =  6
      IF ( INDEX ( LQUAL_CHR, '7' ) .NE. 0 ) IQC_X =  7
      IF ( INDEX ( LQUAL_CHR, '8' ) .NE. 0 ) IQC_X =  8
      IF ( INDEX ( LQUAL_CHR, '9' ) .NE. 0 ) IQC_X =  9
      IF ( IQC_X .LT. 1  ) THEN
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(NOFX__SPS) ) 
         ELSE
           AUTO_SUP = IBCLR ( AUTO_SUP, INT4(NOFX__SPS) ) 
      END IF
      IF ( IQC_X .LT. QUALCODE_GOOD_LIM ) THEN
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(BQCX__SPS) ) 
         ELSE
           AUTO_SUP = IBCLR ( AUTO_SUP, INT4(BQCX__SPS) ) 
      END IF
!
! --- Set again fields NOFS, BQCS
!
      IQC_S = -1
      IF ( INDEX ( LQUALXS_CHR, '0' ) .NE. 0 ) IQC_S =  0
      IF ( INDEX ( LQUALXS_CHR, '1' ) .NE. 0 ) IQC_S =  1
      IF ( INDEX ( LQUALXS_CHR, '2' ) .NE. 0 ) IQC_S =  2
      IF ( INDEX ( LQUALXS_CHR, '3' ) .NE. 0 ) IQC_S =  3
      IF ( INDEX ( LQUALXS_CHR, '4' ) .NE. 0 ) IQC_S =  4
      IF ( INDEX ( LQUALXS_CHR, '5' ) .NE. 0 ) IQC_S =  5
      IF ( INDEX ( LQUALXS_CHR, '6' ) .NE. 0 ) IQC_S =  6
      IF ( INDEX ( LQUALXS_CHR, '7' ) .NE. 0 ) IQC_S =  7
      IF ( INDEX ( LQUALXS_CHR, '8' ) .NE. 0 ) IQC_S =  8
      IF ( INDEX ( LQUALXS_CHR, '9' ) .NE. 0 ) IQC_S =  9
      IF ( IQC_S .LT. 1  ) THEN
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(NOFS__SPS) ) 
         ELSE 
           AUTO_SUP = IBCLR ( AUTO_SUP, INT4(NOFS__SPS) ) 
      END IF
      IF ( IQC_S .LT. QUALCODE_GOOD_LIM ) THEN
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(BQCS__SPS) ) 
         ELSE 
           AUTO_SUP = IBCLR ( AUTO_SUP, INT4(BQCS__SPS) ) 
      END IF
!
      AUTO_SUP = IBSET ( AUTO_SUP, INT4(INIT__SPS) )
!
      RETURN
      END  SUBROUTINE  META_RESET  !#!  
