      SUBROUTINE ADD_SAVE ( M_SAV, L_SAV, ADR_SAV, VAL_SAV, SAVED_VALUE )
! ************************************************************************
! *                                                                      *
! *   Auxillary porocedure ADD_SAVE  increase counter of saved elements  *
! *   L_SAVE and puts value and address of the REAL*8 variable           *
! *   SAVED_VALUE to ADR_SAV(L_SAV), VAL_SAV(L_SAV).                     *
! *                                                                      *
! *  ###  12-MAR-99    ADD_SAVE    v1.0  (c)  L. Petrov  12-MAR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  M_SAV, L_SAV
      ADDRESS__TYPE ::  ADR_SAV(M_SAV)
      REAL*8     VAL_SAV(M_SAV), SAVED_VALUE
      CHARACTER  STR*32
      INTEGER*4  I_LEN
!
      IF ( L_SAV .GE. M_SAV ) THEN
           WRITE ( 6, * ) ' l_sav = ',l_sav
           CALL CLRCH   ( M_SAV, STR )
           CALL ERR_LOG ( 6000, -1, 'ADD_SAVE', 'Parameter M_SAV defined '// &
     &         'in ../adjst/adjst_do appeared too small: '// &
     &          STR(1:I_LEN(STR))//'. it is a fatal error. M_SAV value '// &
     &         'should be raised and SOLVE should be re-compiled' )
           STOP 'Fatal error'
      END IF
!
      L_SAV = L_SAV + 1
      ADR_SAV(L_SAV) = LOC(SAVED_VALUE)
      VAL_SAV(L_SAV) =     SAVED_VALUE
!
      RETURN
      END  !#!  ADD_SAVE  #!#
