      SUBROUTINE EQUMEM_INIT ( EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EQUMEM_INIT
! *                                                                      *
! *  ### 27-MAR-2000  EQUMEM_INIT  v1.0 (c)  L. Petrov  27-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'equmem.i'
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      INTEGER*4  IUER
      INTEGER*8  LEN_EQUMEM
!
! --- Clear memory
!
      LEN_EQUMEM = LOC(EQUMEM%LAST_FIELD) - LOC(EQUMEM%FIRST_FIELD) + 4
      CALL NOUT8 ( LEN_EQUMEM, EQUMEM )
!
      IF ( EQUMEM_FLAG ) THEN
           EQUMEM%USE_FLAG = .TRUE.
         ELSE
           EQUMEM%USE_FLAG = .FALSE.
      END IF
      EQUMEM%STATUS   = EQM__INI
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EQUMEM_INIT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EQUMEM_END ( EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EQUMEM_END
! *                                                                      *
! *  ### 27-MAR-2000  EQUMEM_END   v1.0 (c)  L. Petrov  28-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'equmem.i'
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      INTEGER*4  IUER
      INTEGER*4  J1
      INTEGER*8  LEN_EQUMEM
!
      IF ( EQUMEM%NOBS .GT. 0 ) THEN
!
! -------- Freeing dynamic memory allocated for keeping equations of condisitons
!
           DO 410 J1=1,EQUMEM%NOBS
              CALL FREE ( EQUMEM%ADR_MEMOBS(J1) )
 410       CONTINUE
      END IF
!
! --- Clearing all data structures
!
      LEN_EQUMEM = LOC(EQUMEM%LAST_FIELD) - LOC(EQUMEM%FIRST_FIELD) + 4
      CALL NOUT8 ( LEN_EQUMEM, EQUMEM )
!
! --- Setting flags
!
      EQUMEM%USE_FLAG = .FALSE.
      EQUMEM%STATUS   = EQM__UND
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EQUMEM_END  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EQUMEM_PUT ( IOBS, EQUMEM, PLACE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EQUMEM_PUT  puts the equation of conditions to the EQUMEM *
! *   data structure.                                                    *
! *                                                                      *
! *  ### 27-MAR-2000   EQUMEM_PUT   v1.0 (c) L. Petrov  28-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'equmem.i'
      INCLUDE 'fast.i'
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      TYPE ( PLACE__STRU  ) ::  PLACE
      INTEGER*4  IOBS, IUER
      CHARACTER  STR*32, STR1*32, STR2*32
      INTEGER*4  N_ELM
      ADDRESS__TYPE :: ADR_GLOEQU, ADR_LOCEQU, ADR_SG1EQU, ADR_SG2EQU
      ADDRESS__TYPE :: ADR_GLOIND, ADR_LOCIND, ADR_SG1IND, ADR_SG2IND
      INTEGER*4  I_LEN
!
      IF ( EQUMEM%STATUS  .NE.  EQM__INI  .AND. &
     &     EQUMEM%STATUS  .NE.  EQM__DON        ) THEN
           CALL ERR_LOG ( 6811, IUER, 'EQUMEM_PUT', 'EQUMEM data structure '// &
     &         'has not been initiliazed' )
           RETURN
      END IF
!
      IF ( .NOT. EQUMEM%USE_FLAG ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( IOBS .LE. 0  .OR.   IOBS .GT.  MAX_OBS ) THEN
           CALL CLRCH (          STR  )
           CALL INCH  ( IOBS,    STR  )
           CALL CLRCH (          STR2 )
           CALL INCH  ( MAX_OBS, STR2 )
           CALL ERR_LOG ( 6812, IUER, 'EQUMEM_PUT', 'Wrong index of the '// &
     &         'observation: '//STR(1:I_LEN(STR))//' -- expected value '// &
     &         'should been in the range [1,'//STR2(1:I_LEN(STR2))//']' )
           RETURN
      END IF
!
      N_ELM = PLACE%N_GLO + PLACE%N_LOC + PLACE%N_SG1 + PLACE%N_SG2
      IF ( EQUMEM%ADR_MEMOBS(IOBS) .NE. 0   .AND. &
     &     EQUMEM%LEN_MEMOBS(IOBS) .LT. N_ELM*12   ) THEN
!
! -------- Free dynamic memory if it was allocated but its size is not enough
!
           CALL FREE ( EQUMEM%ADR_MEMOBS(IOBS) )
           EQUMEM%LEN_TOTAL = EQUMEM%LEN_TOTAL - EQUMEM%LEN_MEMOBS(IOBS)
!
! -------- Set status: dynamic memory is freed. Set length of the required
! -------- pool of dynamic memory
!
           EQUMEM%ADR_MEMOBS(IOBS) = 0
           EQUMEM%LEN_MEMOBS(IOBS) = N_ELM*12
         ELSE IF ( EQUMEM%ADR_MEMOBS(IOBS) .EQ. 0 ) THEN
           EQUMEM%LEN_MEMOBS(IOBS) = N_ELM*12
      END IF
!
      IF ( EQUMEM%ADR_MEMOBS(IOBS) .EQ. 0 ) THEN
!
! -------- Allocation of dynamic memory for the equations of conditions of the
! -------- IOBS-th observation. We did it only if the memory has not been
! -------- allocated before
!
           CALL GET_MEM ( EQUMEM%LEN_MEMOBS(IOBS), EQUMEM%ADR_MEMOBS(IOBS) )
           IF ( EQUMEM%ADR_MEMOBS(IOBS) .EQ. 0 ) THEN
                CALL CLRCH ( STR           )
                CALL INCH  ( EQUMEM%LEN_MEMOBS(IOBS), STR  )
                CALL CLRCH ( STR1           )
                CALL IINCH ( EQUMEM%LEN_TOTAL, STR1  )
                CALL CLRCH (          STR2 )
                CALL INCH  ( IOBS,    STR2 )
                CALL ERR_LOG ( 6813, IUER, 'EQUMEM', 'Failure in grabbing '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &               'keeping equations of conditions of the '// &
     &               STR2(1:I_LEN(STR2))//' observation. '// &
     &               STR1(1:I_LEN(STR1))//' bytes of memory has been already '// &
     &              'allocated for keeping equations of conditions' )
                RETURN
           END IF
!
! -------- Update of counters
!
           EQUMEM%LEN_TOTAL = EQUMEM%LEN_TOTAL + EQUMEM%LEN_MEMOBS(IOBS)
           IF ( IOBS .GT. EQUMEM%NOBS ) THEN
                EQUMEM%NOBS = IOBS
           END IF
      END IF
!
      EQUMEM%N_GLO(IOBS) = PLACE%N_GLO
      EQUMEM%N_LOC(IOBS) = PLACE%N_LOC
      EQUMEM%N_SG1(IOBS) = PLACE%N_SG1
      EQUMEM%N_SG2(IOBS) = PLACE%N_SG2
!
      EQUMEM%CLO_SEG(IOBS) = PLACE%CLO_SEG
      EQUMEM%ATM_SEG(IOBS) = PLACE%ATM_SEG
      EQUMEM%EOP_SEG(IOBS) = PLACE%EOP_SEG
!
! --- Assign address
!
      ADR_GLOEQU = EQUMEM%ADR_MEMOBS(IOBS)
      ADR_LOCEQU = ADR_GLOEQU + 8*EQUMEM%N_GLO(IOBS)
      ADR_SG1EQU = ADR_LOCEQU + 8*EQUMEM%N_LOC(IOBS)
      ADR_SG2EQU = ADR_SG1EQU + 8*EQUMEM%N_SG1(IOBS)
!
      ADR_GLOIND = ADR_SG2EQU + 8*EQUMEM%N_SG2(IOBS)
      ADR_LOCIND = ADR_GLOIND + 4*EQUMEM%N_GLO(IOBS)
      ADR_SG1IND = ADR_LOCIND + 4*EQUMEM%N_LOC(IOBS)
      ADR_SG2IND = ADR_SG1IND + 4*EQUMEM%N_SG1(IOBS)
!
! --- Copy table of indices
!
      CALL COPY_I4 ( EQUMEM%N_GLO(IOBS), PLACE%IND_GLO, %VAL(ADR_GLOIND) )
      CALL COPY_I4 ( EQUMEM%N_LOC(IOBS), PLACE%IND_LOC, %VAL(ADR_LOCIND) )
      CALL COPY_I4 ( EQUMEM%N_SG1(IOBS), PLACE%IND_SG1, %VAL(ADR_SG1IND) )
      CALL COPY_I4 ( EQUMEM%N_SG2(IOBS), PLACE%IND_SG2, %VAL(ADR_SG2IND) )
!
! --- Copy equations
!
      CALL COPY_R8 ( PLACE%N_GLO, PLACE%EQU_GLO, %VAL(ADR_GLOEQU) )
      CALL COPY_R8 ( PLACE%N_LOC, PLACE%EQU_LOC, %VAL(ADR_LOCEQU) )
      CALL COPY_R8 ( PLACE%N_SG1, PLACE%EQU_SG1, %VAL(ADR_SG1EQU) )
      CALL COPY_R8 ( PLACE%N_SG2, PLACE%EQU_SG2, %VAL(ADR_SG2EQU) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EQUMEM_PUT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EQUMEM_GET ( IOBS, EQUMEM, PLACE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EQUMEM_GET  gets the equation of conditions to the EQUMEM *
! *   data structure and puts it in PLACE data strucure.                 *
! *                                                                      *
! *  ### 28-MAR-2000   EQUMEM_GET   v1.0 (c) L. Petrov  28-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'equmem.i'
      INCLUDE 'fast.i'
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      TYPE ( PLACE__STRU ) ::  PLACE
      INTEGER*4  IOBS, IUER
      CHARACTER  STR*32, STR2*32
      INTEGER*4  N_ELM
      ADDRESS__TYPE :: ADR_GLOEQU, ADR_LOCEQU, ADR_SG1EQU, ADR_SG2EQU
      ADDRESS__TYPE :: ADR_GLOIND, ADR_LOCIND, ADR_SG1IND, ADR_SG2IND
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( .NOT. EQUMEM%USE_FLAG ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( EQUMEM%STATUS  .NE.  EQM__DON        ) THEN
           CALL ERR_LOG ( 6821, IUER, 'EQUMEM_GET', 'EQUMEM data structure '// &
     &         'has not been filled' )
           RETURN
      END IF
!
      IF ( IOBS .LE. 0  .OR.   IOBS .GT.  MAX_OBS ) THEN
           CALL CLRCH (          STR  )
           CALL INCH  ( IOBS,    STR  )
           CALL CLRCH (          STR2 )
           CALL INCH  ( MAX_OBS, STR2 )
           CALL ERR_LOG ( 6822, IUER, 'EQUMEM_GET', 'Wrong index of the '// &
     &         'observation: '//STR(1:I_LEN(STR))//' -- expected value '// &
     &         'should been in the range [1,'//STR2(1:I_LEN(STR2))//']' )
           RETURN
      END IF
!
      N_ELM = EQUMEM%N_GLO(IOBS) + EQUMEM%N_LOC(IOBS) + &
     &        EQUMEM%N_SG1(IOBS) + EQUMEM%N_SG2(IOBS)
      IF ( EQUMEM%LEN_MEMOBS(IOBS) .LT. N_ELM*12   ) THEN
           CALL CLRCH (          STR  )
           CALL INCH  ( IOBS,    STR  )
           CALL ERR_LOG ( 6823, IUER, 'EQUMEM_GET', 'Trap of internal '// &
     &         'control: not enough memory was allocated for the '// &
     &         STR(1:I_LEN(STR))//'-th observation' )
           RETURN
      END IF
      IF ( EQUMEM%ADR_MEMOBS(IOBS) .EQ. 0 ) THEN
           CALL CLRCH (          STR  )
           CALL INCH  ( IOBS,    STR  )
           CALL ERR_LOG ( 6824, IUER, 'EQUMEM_GET', 'Trap of internal '// &
     &         'control: dynamic memory for the '//STR(1:I_LEN(STR))// &
     &         '-th observation has not been allocated' )
           RETURN
      END IF
!
! --- Assign address
!
      ADR_GLOEQU = EQUMEM%ADR_MEMOBS(IOBS)
      ADR_LOCEQU = ADR_GLOEQU + 8*EQUMEM%N_GLO(IOBS)
      ADR_SG1EQU = ADR_LOCEQU + 8*EQUMEM%N_LOC(IOBS)
      ADR_SG2EQU = ADR_SG1EQU + 8*EQUMEM%N_SG1(IOBS)
!
      ADR_GLOIND = ADR_SG2EQU + 8*EQUMEM%N_SG2(IOBS)
      ADR_LOCIND = ADR_GLOIND + 4*EQUMEM%N_GLO(IOBS)
      ADR_SG1IND = ADR_LOCIND + 4*EQUMEM%N_LOC(IOBS)
      ADR_SG2IND = ADR_SG1IND + 4*EQUMEM%N_SG1(IOBS)
!
      PLACE%N_GLO = EQUMEM%N_GLO(IOBS)
      PLACE%N_LOC = EQUMEM%N_LOC(IOBS)
      PLACE%N_SG1 = EQUMEM%N_SG1(IOBS)
      PLACE%N_SG2 = EQUMEM%N_SG2(IOBS)
!
      PLACE%CLO_SEG = EQUMEM%CLO_SEG(IOBS)
      PLACE%ATM_SEG = EQUMEM%ATM_SEG(IOBS)
      PLACE%EOP_SEG = EQUMEM%EOP_SEG(IOBS)
!
! --- Copy table of indices
!
      CALL COPY_I4 ( PLACE%N_GLO, %VAL(ADR_GLOIND), PLACE%IND_GLO )
      CALL COPY_I4 ( PLACE%N_LOC, %VAL(ADR_LOCIND), PLACE%IND_LOC )
      CALL COPY_I4 ( PLACE%N_SG1, %VAL(ADR_SG1IND), PLACE%IND_SG1 )
      CALL COPY_I4 ( PLACE%N_SG2, %VAL(ADR_SG2IND), PLACE%IND_SG2 )
!
! --- Copy equations
!
      CALL COPY_R8 ( PLACE%N_GLO, %VAL(ADR_GLOEQU), PLACE%EQU_GLO )
      CALL COPY_R8 ( PLACE%N_LOC, %VAL(ADR_LOCEQU), PLACE%EQU_LOC )
      CALL COPY_R8 ( PLACE%N_SG1, %VAL(ADR_SG1EQU), PLACE%EQU_SG1 )
      CALL COPY_R8 ( PLACE%N_SG2, %VAL(ADR_SG2EQU), PLACE%EQU_SG2 )
!
! --- Build an equation with all parameters of the equation of conditions
!
      PLACE%N_GEN = PLACE%N_GLO + PLACE%N_LOC + PLACE%N_SG1 + PLACE%N_SG2
      CALL COPY_R8 ( PLACE%N_GLO, PLACE%EQU_GLO, PLACE%EQU_GEN )
      CALL COPY_I4 ( PLACE%N_GLO, PLACE%IND_GLO, PLACE%IND_GEN )
!
      N_ELM = PLACE%N_GLO
      CALL COPY_R8 ( PLACE%N_LOC, PLACE%EQU_LOC, PLACE%EQU_GEN(N_ELM+1) )
      CALL COPY_I4 ( PLACE%N_LOC, PLACE%IND_LOC, PLACE%IND_GEN(N_ELM+1) )
!
      N_ELM = PLACE%N_GLO + PLACE%N_LOC
      CALL COPY_R8 ( PLACE%N_SG1, PLACE%EQU_SG1, PLACE%EQU_GEN(N_ELM+1) )
      CALL COPY_I4 ( PLACE%N_SG1, PLACE%IND_SG1, PLACE%IND_GEN(N_ELM+1) )
!
      N_ELM = PLACE%N_GLO + PLACE%N_LOC + PLACE%N_SG1
      CALL COPY_R8 ( PLACE%N_SG2, PLACE%EQU_SG2, PLACE%EQU_GEN(N_ELM+1) )
      CALL COPY_I4 ( PLACE%N_SG2, PLACE%IND_SG2, PLACE%IND_GEN(N_ELM+1) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EQUMEM_GET  #!#
