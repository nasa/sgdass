      SUBROUTINE REPEXPA ( M, N, ADR_X4, ADR_Y4, ADR_E4, IUER )
!
! *************************************************************************
! *                                                                       *
! *   Auxiliary routine REPEXPA expands dynamic memory requested for      *
! *   allocation of REAL*4 arrays ADR_X4, ADR_Y4, ADR_E4 sized to hold    *
! *   N points to the size sufficient to hold M points of these arrays.   *
! *   Current values of arrays X4/Y4/E4 are not changed.                  *
! *   called subroutines:                                                 *
! *   ERR_LOG, ERR_PASS, GRAB_MEM, CLRCH, IINCH, LIB$MOVC3, FREE          *
! *   calling routine:                                                    *
! *   REPGRSU, REPPTSU                                                    *
! *                                                                       *
! *   ### 08-AUG-2002   UD_EXPAND   v1.0 (c)  L. Petrov  08-AUG-2002 ###  *
! *   28-OCT-2002   changed name: REPEXPA     VT                          *
! *                                                                       *
! *************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M, N, IUER
      ADDRESS__TYPE :: ADR_X4, ADR_Y4, ADR_E4
      CHARACTER  STR*32
      ADDRESS__TYPE :: NEW_ADR_X4, NEW_ADR_Y4, NEW_ADR_E4, MEM_ADR
      INTEGER*8  MEM_LEN
      INTEGER*4  IER
      INTEGER*4  I_LEN
!
      IF ( M .LE. N ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Grab memory for new arrays
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 3, &
     &                INT8(4*M), NEW_ADR_X4, &
     &                INT8(4*M), NEW_ADR_Y4, &
     &                INT8(4*M), NEW_ADR_E4  )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH  ( MEM_LEN, STR )
           CALL ERR_LOG ( 7521, IUER, 'REPEXPA', 'Error '// &
     &          'during the next attempt of grabbing '// &
     &           STR(1:I_LEN(STR))//' bytes of memory' )
           RETURN
      END IF
!
! --- copy arrays to the new place
!
      CALL LIB$MOVC3 ( 4*N, %VAL(ADR_X4), %VAL(NEW_ADR_X4) )
      CALL LIB$MOVC3 ( 4*N, %VAL(ADR_Y4), %VAL(NEW_ADR_Y4) )
      CALL LIB$MOVC3 ( 4*N, %VAL(ADR_E4), %VAL(NEW_ADR_E4) )
!
! --- Free the memory which has been poreviously allocated for the arrays
!
      CALL FREE ( ADR_X4 )
!
! --- Copy the addresses
!
      ADR_X4 = NEW_ADR_X4
      ADR_Y4 = NEW_ADR_Y4
      ADR_E4 = NEW_ADR_E4
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!
