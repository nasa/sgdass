      PROGRAM    VCAT_INFO
! ************************************************************************
! *                                                                      *
! *   Program VCAT_INFO
! *                                                                      *
! *  ### 14-AUG-2003   VCAT_INFO   v1.0 (c)  L. Petrov  14-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      INCLUDE   'solve.i'
      INCLUDE   'vcat.i'
      CHARACTER  RELEASE_DATE*54
      CHARACTER  GET_VERSION*54
      INTEGER*4  I_LEN
!
      INCLUDE   'vcat_info_version.i'
!
      RELEASE_DATE = GET_VERSION ()
      WRITE ( 6, '(A)' ) RELEASE_DATE(1:I_LEN(RELEASE_DATE))
      WRITE ( 6, '(A)' ) ' '
      WRITE ( 6, '(A)' ) 'This progrma has not been yet developed!'
      CALL EXIT ( 1 )
!
      END  !#!  VCAT_INFO  #!#
