      FUNCTION BIT_CHECKSUM ( IVAL, BIT_BEG, BIT_END )
! ************************************************************************
! *                                                                      *
! *   Routine BIT_CHECKSUM computes a bit checksum of the specified      *
! *   bit-field summing bits from BIT_BET through BIT_END using bit-wise *
! *   XOR function. The result (0 or 1) is returned. The result can be   *
! *   that way: the number of "1" in bit-wise representation of the bit  *
! *   field (0 or 1) us summed. If the sum is even, BIT_CHECKSUM = 0.    *
! *   Otherwise, BIT_CHECKSUM = 1.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * IVAL    ( INTEGER*4 ) -- A value for which the checksum is computed. *
! * BIT_BEG ( INTEGER*4 ) -- The first bit of IVAL for the bit sum to be *
! *                          computed. Bits are numbered from 1.         *
! * BIT_END ( INTEGER*4 ) -- The last bit of IVAL for the bit sum to be  *
! *                          computed. Bits are numbered from 1.         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * BIT_CHECKSUM ( INTEGER*4 ) -- bit-wise checksum: 0 or 1.             *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 22-JUN-2025  BIT_CHECKSUM v1.0 (d)  L. Petrov  22-JUN-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  BIT_CHECKSUM, BIT_BEG, BIT_END
      INTEGER*4  IVAL(*)
      INTEGER*4  J1, IND, BIT, BIT_ACC
!
      BIT_CHECKSUM = 0
      BIT = 0
      DO 410 J1=BIT_BEG,BIT_END
         IND = (J1-1)/32 + 1
         CALL MVBITS ( IVAL(IND), J1-1 - 32*(IND-1), 1, BIT, 0 )
         BIT_CHECKSUM = XOR ( BIT_CHECKSUM, BIT )
 410  CONTINUE 
      RETURN
      END  FUNCTION BIT_CHECKSUM  !#!#
