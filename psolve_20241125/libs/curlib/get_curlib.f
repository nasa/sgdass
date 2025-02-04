      FUNCTION   GET_CURLIB ( )
! ************************************************************************
! *                                                                      *
! *   Rotune  GET_CURLIB returns value of status of using curses library.*
! *   Values are defined in  ../include/curlib.i                         *
! *                                                                      *
! *  ###  12-APR-99   GET_CURLIB   v1.0  (c)  L. Petrov  12-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'curlib.i'
      INTEGER*4  GET_CURLIB
!
      GET_CURLIB = CURLIB_FLAG
!
      RETURN
      END  !#!  GET_CURLIB  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   IS_CURLIB_ON ( )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine IS_CURLIB_ON returns TRUE if curlib mode was set *
! *   up (and was not cleared off since that).                           *
! *                                                                      *
! *  ###  12-APR-99  IS_CURLIB_ON  v1.0  (c)  L. Petrov  12-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'curlib.i'
      LOGICAL*4  IS_CURLIB_ON
      LOGICAL*2 TRUE_L4, FALSE_L4
!
      TRUE_L4   = .TRUE.
      FALSE_L4  = .FALSE.
!
      IF ( CURLIB_FLAG .EQ. CRS__ON ) THEN
           IS_CURLIB_ON = TRUE_L4
         ELSE
           IS_CURLIB_ON = FALSE_L4
      END IF
!
      RETURN
      END  !#!  IS_CURLIB_ON  #!#
