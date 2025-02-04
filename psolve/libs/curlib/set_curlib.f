      SUBROUTINE SET_CURLIB ( VAL )
! ************************************************************************
! *                                                                      *
! *   Auxillary procedure SET_CURLIBB set status of ising curees library.*
! *                                                                      *
! *  ###  12-APR-99   SET_CURLIB   v1.0  (c)  L. Petrov  12-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'curlib.i'
      INTEGER*4  VAL
!
      CURLIB_FLAG = VAL
      RETURN
      END  !#!  SET_CURLIB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SET_CURLIB_ON()
! ************************************************************************
! *                                                                      *
! *   Auxillary procedure SET_CURLIB_ON sets flag of using curses        *
! *   library.                                                           *
! *                                                                      *
! *  ###  12-APR-99  SET_CURLIB_ON  v1.0 (c)  L. Petrov  12-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'curlib.i'
!
      CALL SET_CURLIB ( CRS__ON )
      RETURN
      END  !#!  SET_CURLIB_ON  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SET_CURLIB_OFF()
! ************************************************************************
! *                                                                      *
! *   Auxillary procedure SET_CURLIB_OFF clears flag of using curses     *
! *   library.                                                           *
! *                                                                      *
! *  ###  12-APR-99  SET_CURLIB_OFF  v1.0 (c) L. Petrov  12-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'curlib.i'
!
      CALL SET_CURLIB ( CRS__OFF )
      RETURN
      END  !#!  SET_CURLIB_OFF  #!#
