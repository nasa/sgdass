      FUNCTION   REPA_DF_CONT ()
! ************************************************************************
! *                                                                      *
! *   The auxiliary function REPA_DF_CONT does nothing.                  *
! *                                                                      *
! *  ### 13-DEC-2004  REPA_DF_CONT  v1.0 (c) L. Petrov  13-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      INTEGER*4  REPA_DF_CONT
      REPA_DF_CONT = DIAGI__CONT   
      RETURN
      END  FUNCTION  REPA_DF_CONT
