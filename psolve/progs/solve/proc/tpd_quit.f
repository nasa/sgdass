      SUBROUTINE TPD_QUIT ( TPD, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine TPD_QUIT
! *                                                                      *
! *  ### 07-NOV-2007   TPD_QUIT   v1.0 (c)  L. Petrov   08-NOV-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      TYPE     ( TPD__TYPE     ) :: TPD
      INTEGER*4  IUER
      LOGICAL*4  LEX
      LOGICAL*4  LUN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      DEALLOCATE ( TPD%STA   )
      DEALLOCATE ( TPD%SOU   )
      DEALLOCATE ( TPD%PARAM )
      DEALLOCATE ( TPD%DELAY )
!
      IF ( TPD%HEADER%RATE_USE == SOLVE__YES ) THEN
           DEALLOCATE ( TPD%RATE   )
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPD_QUIT  !#!#
