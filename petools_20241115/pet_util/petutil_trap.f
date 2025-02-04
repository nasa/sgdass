      SUBROUTINE PETUTIL_TRAP ()
! ************************************************************************
! *                                                                      *
! *   The purpose of this routine to cause a crash and unwind the        *
! *   stack of routine calls.                                            *
! *                                                                      *
! *  ### 11-JUL-2007  PETUTIL_TRAP  v1.0 (c)  L. Petrov  11-JUL-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IARR(2), IP
      WRITE ( 6, '(A)' ) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      WRITE ( 6, '(A)' ) 'Now we deliberatly crash the program in order to '// &
     &                   'unwind the stack.'
      WRITE ( 6, '(A)' ) 'Information below is imprtant for diagnostic '// &
                         'and debugging '
      WRITE ( 6, '(A)' ) 'Carefully paste and copy at least 40 lines before '// &
     &                   'this point and '
      WRITE ( 6, '(A)' ) 'to the bottom of error messages'
      WRITE ( 6, '(A)' ) 'Please, send this copy to the software maintainer'
      WRITE ( 6, '(A)' ) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      IP = 3
      IARR(IP) = IP
      CALL EXIT ( 1 )
      RETURN
      END  SUBROUTINE   PETUTIL_TRAP  !#!#
