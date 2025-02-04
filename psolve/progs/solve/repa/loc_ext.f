      FUNCTION   LOC_EXT ( IARG )
! ************************************************************************
! *                                                                      *
! *   This auxilliary function LOG_EXT returns the address of its        *
! *   argument. The only reason of existance of this function is a bug   *
! *   in HP f90 compiler: compilers claims that getting the address of   *
! *   external function is an unsupported feature. !!!                   *
! *                                                                      *
! *  ### 21-SEP-2002     LOC_EXT    v1.0 (c)  L. Petrov  21-SEP-2002 ### *
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
