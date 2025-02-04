      PROGRAM    LIBVEC_CHECK
      IMPLICIT   NONE 
      INTEGER*4  N
      PARAMETER  ( N = 4 )
      INTEGER*4  VEC(N)
      CALL VEC_$IINIT ( VEC, N, 0 )
      END  !#!  LIBVEC_CHECK  #!#
