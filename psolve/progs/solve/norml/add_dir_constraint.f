      SUBROUTINE ADD_DIR_CONSTRAINT ( NPARM, NUM_CON, CNI_CODE, CARR, NOR_MAT, &
     &                                NOR_VEC, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_DIR_CONSTRAINT
! *                                                                      *
! * ### 09-MAR-2001 ADD_DIR_CONSTRAINT v1.0 (c) L. Petrov 09-MAR-2001 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'cnstr.i'
      INCLUDE    'fast.i'
      INTEGER*4  NPARM, NUM_CON, CNI_CODE, IUER
      REAL*8     CARR(NPARM+2,NUM_CON), NOR_MAT(*), NOR_VEC(*)
      TYPE ( CNSTR__STRU ) ::     CNSTROBJ
      REAL*8     VEC_CNS(M_GPA), SIG_CNS, VAL_CNS
      CHARACTER  STR*80
      INTEGER*4  IND_VEC(M_GPA), IEL, J1, J2
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( CNI_CODE .EQ. CNI__GLO ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NUM_CON, STR )
!
! -------- Direct applying global constraints
!
           DO 410 J1=1,NUM_CON
              SIG_CNS = CARR(NPARM+1,J1) ! sigma of constraint
              VAL_CNS = CARR(NPARM+2,J1) ! value of constraint
              IEL = 0
              DO 420 J2=1,NPARM
                 IF ( CARR(J2,J1) .NE. 0.D0 ) THEN
                      IEL = IEL + 1
                      VEC_CNS(IEL) = CARR(J2,J1)
                      IND_VEC(IEL) = J2
                 END IF
 420          CONTINUE
!
! ----------- Applying contraints to the normal matrix and normal vector
!
!
! ----------- Applying contraints to the normal matrix and normal vector
!
              CALL ADD_TRG ( VAL_CNS, SIG_CNS, IEL, IND_VEC, VEC_CNS, NPARM, &
     &                       NOR_VEC, NOR_MAT )
!
! ----------- Write down information about constraints
!
!
! ----------- Write down information about constraints
!
              CALL ADD_TYCNS ( 'USER', STR(1:I_LEN(STR))//' USER '// &
     &             'constraints', '???', CARR(NPARM+1,J1), .TRUE., &
     &             CNSTROBJ )
 410       CONTINUE
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( CNI_CODE, STR )
           CALL ERR_LOG ( 8671, IUER, 'ADD_DIR_CONSTRAINT', 'Constraint '// &
     &         'implementation code '//STR(1:I_LEN(STR))//' is not '// &
     &         'supported. Only global code is supported' )
           RETURN
      END IF
!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ADD_DIR_CONSTRAINT  #!#
