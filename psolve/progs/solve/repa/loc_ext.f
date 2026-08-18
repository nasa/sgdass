      FUNCTION   LOC_EXT ( IARG )
! ************************************************************************
! *                                                                      *
! *   This auxilliary function LOG_EXT returns the address of its        *
! *   argument. The only reason of existance of this function is a bug   *
! *   in HP f90 compiler: compilers claims that getting the address of   *
! *   external function is an unsupported feature. !!!                   *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 21-SEP-2002     LOC_EXT    v1.0 (d)  L. Petrov  21-SEP-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      ADDRESS__TYPE :: LOC_EXT
      INTEGER*4     :: IARG
!
      LOC_EXT = LOC(IARG)
!
      RETURN
      END  !#!  LOC_EXT  #!#
