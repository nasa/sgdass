      SUBROUTINE TPD_QUIT ( TPD, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine TPD_QUIT
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 07-NOV-2007   TPD_QUIT   v1.0 (d)  L. Petrov   08-NOV-2007 ###  *
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
