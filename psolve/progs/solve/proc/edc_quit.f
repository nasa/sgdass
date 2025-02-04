      SUBROUTINE EDC_QUIT ( EDC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EDC_QUIT 
! *                                                                      *
! *  ### 25-OCT-2007   EDC_QUIT    v1.0 (c)  L. Petrov  25-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      TYPE     ( EDC__TYPE     ) :: EDC
      INTEGER*4  IUER
!
      IF ( ASSOCIATED ( EDC%OBS   ) ) DEALLOCATE ( EDC%OBS   )
      IF ( ASSOCIATED ( EDC%C_STA ) ) DEALLOCATE ( EDC%C_STA )
      IF ( ASSOCIATED ( EDC%C_SOU ) ) DEALLOCATE ( EDC%C_SOU )
      CALL NOUT ( SIZEOF(EDC), EDC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EDC_QUIT  !#!#
