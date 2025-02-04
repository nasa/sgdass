      SUBROUTINE ADDCNS_NAM ( ABBR, SIND_EQU, DESCR, UNITS, RIGHT_PART, &
     &                        SIGMA, GLO, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ADDCNS_NAM  adds information to the data structure        *
! *   CNSTROBJ about the type of the constraint applied. It compares     *
! *   the abbreviation of the constraint with abbreaviations kept in     *
! *   the fields of the object CNSTROBJ. If the constrain with the same  *
! *   type have been applied already ADDCNS_NAM finishs its work. If     *
! *   not, it creates a new field in CNSTROBJ and adds there information *
! *   about the constraint.                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       ABBR ( CHARACTER ) -- Abbreviation of the constraint. It is    *
! *                             unique identifier of the constraint.     *
! *                             It has length 8 symbols.                 *
! *   SIND_EQU ( INTEGER*4 ) -- Constraint equation sub-index.           *
! *      DESCR ( CHARACTER ) -- Short (up to 32 symbols) descrition of   *
! *                             the constraint.                          *
! *      UNITS ( CHARACTER ) -- Line (up to 16 symbols) with units of    *
! *                             the constraint.                          *
! * RIGHT_PART ( REAL*8    ) -- Right hand part of the equation of       *
! *                             constraint.                              *
! *      SIGMA ( REAL*8    ) -- Sigma of the constraint. The value of    *
! *                             contribution to the normal matrix due    *
! *                             to constraint is reciprocal to the       *
! *                             the square of sigma.                     *
! *        GLO ( LOGICAL*4 ) -- .TRUE. means that constrain was applied  *
! *                             the global parameters in CGM.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   CNSTROBJ ( RECORD    ) -- The data structure with information      *
! *                             about constraints (position where the    *
! *                             matrix should be modified and the value  *
! *                             of constraints).                         *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-MAR-2002   ADDCNS_NAM  v1.1 (c)  L. Petrov 10-MAY-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'cnstr.i'
      CHARACTER  ABBR*(*), DESCR*(*), UNITS*(*)
      REAL*8     RIGHT_PART, SIGMA
      LOGICAL*4  GLO
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  SIND_EQU, J1, I_LEN
!
      IF ( CNSTROBJ%N_EQUAT .GT. 0 ) THEN
!
! -------- Firstly, scan the list of the constraints to determine whether the
! -------- constraint with abbreviation ABBR has been applied already
!
           DO 410 J1=1,CNSTROBJ%N_EQUAT
              IF ( CNSTROBJ%ABB_CNS(J1) .EQ. ABBR     .AND. &
     &             CNSTROBJ%SBI_CNS(J1) .EQ. SIND_EQU       ) THEN
!
! ---------------- Yes, we found such an constraint. Good bye!
!
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
              END IF
 410       CONTINUE
      END IF
!
! --- No we didn't find such an element. Create the new record of constraints
! --- data stucture
!
      CNSTROBJ%N_EQUAT  = CNSTROBJ%N_EQUAT  + 1
      IF ( CNSTROBJ%N_EQUAT .GT. MAX_EQUAT ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MAX_EQUAT, STR )
           CALL ERR_LOG ( 6667, IUER, 'ADDCNS_NAM', 'The number of '// &
     &         'constraints equations exceeded the limit '//STR(1:I_LEN(STR))// &
     &         ' constant MAX_EQUAT in $PSOLVE_ROOT/include/cnstr.i '// &
     &         'appeared too small' )
           RETURN
      END IF
!
! --- ... and add the new element
!
      CALL CLRCH ( CNSTROBJ%ABB_CNS(CNSTROBJ%N_EQUAT) )
      CNSTROBJ%ABB_CNS(CNSTROBJ%N_EQUAT) = ABBR
!
      CNSTROBJ%SBI_CNS(CNSTROBJ%N_EQUAT) = SIND_EQU
!
      CALL CLRCH ( CNSTROBJ%DSC_CNS(CNSTROBJ%N_EQUAT) )
      CNSTROBJ%DSC_CNS(CNSTROBJ%N_EQUAT) = DESCR
!
      CALL CLRCH ( CNSTROBJ%UNT_CNS(CNSTROBJ%N_EQUAT) )
      CNSTROBJ%UNT_CNS(CNSTROBJ%N_EQUAT) = UNITS
!
      CNSTROBJ%GLO_CNS(CNSTROBJ%N_EQUAT) = GLO
      CNSTROBJ%RTP_CNS(CNSTROBJ%N_EQUAT) = RIGHT_PART
      CNSTROBJ%SIG_CNS(CNSTROBJ%N_EQUAT) = SIGMA
      CNSTROBJ%DYN(CNSTROBJ%N_EQUAT)%STS = 0
!
      CALL GETENVAR ( 'DEBUG_CNS', STR ) 
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:3) == 'NAM' ) THEN
           WRITE ( 6, 110 ) CNSTROBJ%ABB_CNS(CNSTROBJ%N_EQUAT), &
     &                      SIND_EQU,                           &
     &                      CNSTROBJ%DSC_CNS(CNSTROBJ%N_EQUAT), &
     &                      CNSTROBJ%UNT_CNS(CNSTROBJ%N_EQUAT), &
     &                      RIGHT_PART, SIGMA, GLO
 110       FORMAT ( 'ADD_CNS: ', A8, ' Sind: ', I6, ' Descr= ', A32, ' Units: ', A8, &
     &              ' RH: ', 1PD14.6, ' Sig: ', 1PD14.6, ' Glo: ', L1 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   ADDCNS_NAM  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADDCNS_EQU ( ABBR, IND_EQU, IND_PAR, EQU_COEF, GLO, CNSTROBJ, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ADDCNS_EQU puts the coefficient of the equation of         *
! *   constraint in the record CONSTROBJ.                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ABBR ( CHARACTER ) -- Abbreviation of the constraint. It is     *
! *                            a unique constraint identifier.           *
! *                            It has length 8 symbols.                  *
! *   IND_EQU ( INTEGER*4 ) -- Index of equation of constraint of this   *
! *                            type (abbreviation).                      *
! *   IND_PAR ( INTEGER*4 ) -- Index of the parameter of this constraint.*
! *  EQU_COEF ( REAL*8    ) -- Coefficient of equation of constraints.   *
! *                            It is the value of (IND_EQU,IND_PAR) of   *
! *                            the matrix of constraint equations.       *
! *       GLO ( LOGICAL*4 ) -- Flag: global or local. .TRUE. means that  *
! *                            the constraint was imposed on global      *
! *                            parameters in the CGM.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  CNSTROBJ ( RECORD    ) -- The data structure with information about *
! *                            constraints (position where the matrix    *
! *                            should be modified and the value of       *
! *                            constraints).                             *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 18-MAR-2002    ADDCNS_EQU  v1.1 (c) L. Petrov  14-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'cnstr.i'
      CHARACTER  ABBR*(*)
      INTEGER*4  IND_EQU, IND_PAR, IUER
      REAL*8     EQU_COEF
      LOGICAL*4  GLO
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  STR*80
      INTEGER*4  IND_CNS, J1
!
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-31 ) ! Machine zero
      INTEGER*4,  EXTERNAL ::  I_LEN
!
      IF ( IND_EQU .LE. 0  ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_EQU, STR )
           CALL ERR_LOG ( 8781, IUER, 'ADDCNS_EQU', 'Wrong value of argument '// &
     &         'IND_EQU: '//STR )
           RETURN
      END IF
!
      IF ( IND_PAR .LE. 0  ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_PAR, STR )
           CALL ERR_LOG ( 8782, IUER, 'ADDCNS_EQU', 'Wrong value of argument '// &
     &         'IND_PAR: '//STR )
           RETURN
      END IF
!
      IF ( DABS(EQU_COEF) .LT. EPS ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Firstly check: does constraint have non-zero value. If yes, nothing to do
!
      IF ( DABS(EQU_COEF) .LT. EPS ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( CNSTROBJ%N_EQUAT .LE. 0 ) THEN
           CALL ERR_LOG ( 8783, IUER, 'ADDCNS_EQU', 'No constraint '// &
     &         'abbreviation is kept in CNSTROBJ at all. Please use '// &
     &         ' ADDCNS_NAM first' )
           RETURN
      END IF
!
      IND_CNS = 0
      DO 410 J1=CNSTROBJ%N_EQUAT,1,-1
         IF ( CNSTROBJ%ABB_CNS(J1) .EQ.  ABBR    .AND. &
     &        CNSTROBJ%SBI_CNS(J1) .EQ.  IND_EQU .AND. &
     &        CNSTROBJ%GLO_CNS(J1) .EQV. GLO           ) THEN
!
              IND_CNS = J1
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE
!
      IF ( IND_CNS .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_EQU, STR )
           CALL ERR_LOG ( 8784, IUER, 'ADDCNS_EQU', 'Constraint abbreviation '// &
     &          ABBR(1:I_LEN(ABBR))//' equation index '//STR(1:I_LEN(STR))// &
     &         ' was not found in CNSTROBJ% Please use ADDCNS_NAM first' )
           RETURN
      END IF
!
      IF ( CNSTROBJ%DYN(IND_CNS)%STS .EQ. CNS__ALC ) THEN
           CNSTROBJ%DYN(IND_CNS)%L_CNS = CNSTROBJ%DYN(IND_CNS)%L_CNS + 1
           IF ( CNSTROBJ%DYN(IND_CNS)%L_CNS .GT. &
     &          CNSTROBJ%DYN(IND_CNS)%M_CNS      ) THEN
                CNSTROBJ%DYN(IND_CNS)%L_CNS = CNSTROBJ%DYN(IND_CNS)%M_CNS      
                CALL CLRCH ( STR )
                CALL INCH  ( CNSTROBJ%DYN(IND_CNS)%M_CNS, STR )
                CALL ERR_LOG ( 8785, IUER, 'ADDCNS_EQU', 'The counter of '// &
     &              'constraint equations exceeded the limit '// &
     &               STR(1:I_LEN(STR))//' defined for dynamic constraint '// &
     &               ABBR(1:I_LEN(ABBR)) )
                RETURN
           END IF
!
           CNSTROBJ%DYN(IND_CNS)%EQU_INE(CNSTROBJ%DYN(IND_CNS)%L_CNS) = IND_CNS
           CNSTROBJ%DYN(IND_CNS)%EQU_INP(CNSTROBJ%DYN(IND_CNS)%L_CNS) = IND_PAR
           CNSTROBJ%DYN(IND_CNS)%EQU_CNS(CNSTROBJ%DYN(IND_CNS)%L_CNS) = EQU_COEF
         ELSE
           CNSTROBJ%N_ECNST = CNSTROBJ%N_ECNST + 1
           IF ( CNSTROBJ%N_ECNST .GT. MAX_ECNST ) THEN
                CNSTROBJ%N_ECNST = MAX_ECNST
                CALL CLRCH ( STR )
                CALL INCH  ( MAX_ECNST, STR )
                CALL ERR_LOG ( 8786, IUER, 'ADDCNS_EQU', 'The counter of '// &
     &              'constraint equations exceeded the limit '// &
     &               STR(1:I_LEN(STR))//' defined in '// &
     &              '$PSOLVE_ROOT/include/cnstr.i for general constraints '// &
     &              'in an attempt to put one more coefficient of '// &
     &              'constraint equations for the constraint '// &
     &               ABBR(1:I_LEN(ABBR)) )
                RETURN
           END IF
!
           CNSTROBJ%EQU_INE(CNSTROBJ%N_ECNST) = IND_CNS
           CNSTROBJ%EQU_INP(CNSTROBJ%N_ECNST) = IND_PAR
           CNSTROBJ%EQU_CNS(CNSTROBJ%N_ECNST) = EQU_COEF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   ADDCNS_EQU  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADD_OFD ( ABBR1, IND1_EQU, ABBR2, IND2_EQU, VAL_OFD, &
     &                     GLO, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ADD_OFS specifies the value of the off-diagonal element    *
! *   of the weights matrix of constraint.                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ABBR1 ( CHARACTER ) -- Abbreviation of the first equation of     *
! *                            constraint. It is a unique constraint     *
! *                            identifier. It has length 8 symbols.      *
! *  IND1_EQU ( INTEGER*4 ) -- Index of the first equation of constraint *
! *                            of this type (abbreviation).              *
! *     ABBR2 ( CHARACTER ) -- Abbreviation of the second equation of    *
! *                            constraint. It is a unique constraint     *
! *                            identifier. It has length 8 symbols.      *
! *  IND2_EQU ( INTEGER*4 ) -- Index of the second equation of           *
! *                            constraint of this type (abbreviation).   *
! *   VAL_OFD ( REAL*8    ) -- Value of the the off-diagonal element     *
! *                            of the weight matrix of constraints.      *
! *       GLO ( LOGICAL*4 ) -- Flag: global or local. .TRUE. means that  *
! *                            the constraint was imposed on global      *
! *                            parameters in the CGM.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  CNSTROBJ ( RECORD    ) -- The data structure with information about *
! *                            constraints (position where the matrix    *
! *                            should be modified and the value of       *
! *                            constraints).                             *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 08-MAY-2002     ADD_OFD   v2.0 (c)  L. Petrov  21-DEC-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'cnstr.i'
      CHARACTER  ABBR1*(*), ABBR2*(*)
      INTEGER*4  IND1_EQU, IND2_EQU, IUER
      REAL*8     VAL_OFD
      LOGICAL*4  GLO
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  STR*80
      INTEGER*4  IND1_CNS, IND2_CNS, J1
      INTEGER*4  I_LEN
!
      CALL ERR_LOG ( 8790, IUER, 'ADD_OFD', 'Setting off-diagonal terms '// &
     &    'in weight matrix is not currently supported. Current logic of '// &
     &    ' apply_cnstr assumes that the wight matrix is diagonal. Upgrade '// &
     &    'requires significant change of internal logic. Besides, '// &
     &    'write_sinex routine should be upgraded.' )
      if ( ind1_equ .ne. -1231231 ) RETURN
!
      IF ( IND1_EQU .LE. 0  ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND1_EQU, STR )
           CALL ERR_LOG ( 8791, IUER, 'ADD_OFD', 'Wrong value of argument '// &
     &         'IND1_EQU: '//STR )
           RETURN
      END IF
!
      IF ( IND2_EQU .LE. 0  ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND2_EQU, STR )
           CALL ERR_LOG ( 8792, IUER, 'ADD_OFD', 'Wrong value of argument '// &
     &         'IND2_EQU: '//STR )
           RETURN
      END IF
!
      IND1_CNS = 0
      IND2_CNS = 0
      DO 410 J1=1,CNSTROBJ%N_EQUAT
         IF ( CNSTROBJ%ABB_CNS(J1) .EQ.  ABBR1    .AND. &
     &        CNSTROBJ%SBI_CNS(J1) .EQ.  IND1_EQU .AND. &
     &        CNSTROBJ%GLO_CNS(J1) .EQV. GLO            ) THEN
!
              IND1_CNS = J1
         END IF
!
         IF ( CNSTROBJ%ABB_CNS(J1) .EQ.  ABBR2    .AND. &
     &        CNSTROBJ%SBI_CNS(J1) .EQ.  IND2_EQU .AND. &
     &        CNSTROBJ%GLO_CNS(J1) .EQV. GLO            ) THEN
!
              IND2_CNS = J1
         END IF
 410  CONTINUE
!
      IF ( IND1_CNS .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND1_EQU, STR )
           CALL ERR_LOG ( 8793, IUER, 'ADD_OFD', 'Constraint abbreviation '// &
     &          ABBR1(1:I_LEN(ABBR1))//' equation index '//STR(1:I_LEN(STR))// &
     &         ' was not found in CNSTROBJ. Please use ADDCNS_NAM first' )
           RETURN
      END IF
!
      IF ( IND2_CNS .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND2_EQU, STR )
           CALL ERR_LOG ( 8794, IUER, 'ADD_OFD', 'Constraint abbreviation '// &
     &          ABBR2(1:I_LEN(ABBR2))//' equation index '//STR(1:I_LEN(STR))// &
     &         'was not found in CNSTROBJ. Please use ADDCNS_NAM first' )
           RETURN
      END IF
!
      CNSTROBJ%N_OFD = CNSTROBJ%N_OFD + 1
      IF ( CNSTROBJ%N_OFD .GT. MAX_OFD ) THEN
           CNSTROBJ%N_OFD = MAX_OFD
           CALL CLRCH ( STR )
           CALL INCH  ( MAX_OFD, STR )
           CALL ERR_LOG ( 8795, IUER, 'ADD_OFD', 'Contrer of the '// &
     &         'off-diagonal elements of constraint equations exceeded '// &
     &         'the limit '//STR(1:I_LEN(STR))//' defined as MAX_OFD in '// &
     &         '$PSOLVE_ROOT/include/cnstr.i' )
           RETURN
      END IF
!
! --- Well, let's insert the value at last
!
      CNSTROBJ%INE1_OFD(CNSTROBJ%N_OFD) = IND1_CNS
      CNSTROBJ%INE2_OFD(CNSTROBJ%N_OFD) = IND2_CNS
      CNSTROBJ%WEI_OFD(CNSTROBJ%N_OFD)  = VAL_OFD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ADD_OFD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INIT_CNS ( CNS_TYP, OBJ_NAM, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  INIT_CNS  ionitializes CNSTROBJ object. It clears         *
! *   itsintenral fields and sets:                                       *
! *   1) type of the CNSTROBJ, one of the                                *
! *      CNI__LOC -- constraints on local parameters;                    *
! *      CNI__GLO -- constraints on global parameters;                   *
! *      CNI__ULC -- constraints on user local parameters;               *
! *      CNI__UGL -- constraints on user global parameters;              *
! *   2) object name, session name for constraints on local parameters   *
! *      and 'GLOBAL    ' in the case of constraints imposed on global   *
! *   parameters.                                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  CNI_TYP ( INTEGER*4 ) -- Constraints type.                          *
! *  OBJ_NAM ( CHARACTER ) -- Object name.                               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * CNSTROBJ ( RECORD    ) -- The data structure with information        *
! *                           about constraints (position where the      *
! *                           matrix should be modified and the value of *
! *                           constraints).                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 25-SEP-2002    INIT_CNS   v1.2 (c)  L. Petrov  14-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  CNS_TYP, IUER
      CHARACTER  OBJ_NAM*(*)
      TYPE     ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  STR*32
      INTEGER*4  J1
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Initialization
!
      CNSTROBJ%N_EQUAT = 0
      CNSTROBJ%N_ECNST = 0
      CNSTROBJ%N_OFD   = 0
      CNSTROBJ%N_MATEL = 0
      CNSTROBJ%N_TYCNS = 0
      CNSTROBJ%N_VECEL = 0
      CALL NOUT_I4 ( MAX_ECNST, CNSTROBJ%EQU_INE )
      CALL NOUT_I4 ( MAX_ECNST, CNSTROBJ%EQU_INP )
      CALL NOUT_R8 ( MAX_EQUAT, CNSTROBJ%RTP_CNS )
      CALL NOUT_R8 ( MAX_ECNST, CNSTROBJ%EQU_CNS )
      CALL NOUT_R8 ( MAX_OFD,   CNSTROBJ%WEI_OFD )
!
      IF ( CNS_TYP .EQ. CNI__LOC ) THEN
           CNSTROBJ%CNS_TYP = CNS_TYP
         ELSE IF ( CNS_TYP .EQ. CNI__GLO ) THEN
           CNSTROBJ%CNS_TYP = CNS_TYP
         ELSE IF ( CNS_TYP .EQ. CNI__ULC ) THEN
           CNSTROBJ%CNS_TYP = CNS_TYP
         ELSE IF ( CNS_TYP .EQ. CNI__UGL ) THEN
           CNSTROBJ%CNS_TYP = CNS_TYP
         ELSE
           CALL ERR_LOG ( 8771, IUER, 'INIT_CNS', 'Wrong argument CNS_TYP: '// &
     &          STR(1:I_LEN(STR))//' it is not supported' )
           RETURN
      END IF
!
      DO 410 J1=1,MAX_EQUAT
         CNSTROBJ%USR_CNS(J1) = .FALSE.
         CNSTROBJ%DYN(J1)%STS   = 0
         CNSTROBJ%DYN(J1)%M_CNS = 0
         CNSTROBJ%DYN(J1)%L_CNS = 0
 410  CONTINUE
!
      CNSTROBJ%OBJ_NAM = OBJ_NAM
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  INIT_CNS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SET_USER_CNS ( ABBR, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine SET_USER_CNS set flag "user constraint" on       *
! *   the constraints which has previosly been defined with ADCNS_NAM.   *
! *   This flag tells to further routines that this constraint is        *
! *   not a buiolt in constraint, but the constraint defined by a user.  *
! *                                                                      *
! *  ### 27-SEP-2002  SET_USER_CNS v1.0 (c)  L. Petrov  27-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'cnstr.i'
      CHARACTER  ABBR*(*)
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  IUER
      LOGICAL*4  FL_FOUND
      INTEGER*4  J1
      INTEGER*4  I_LEN
!
      FL_FOUND = .FALSE.
      IF ( CNSTROBJ%N_EQUAT .GT. 0 ) THEN
           DO 410 J1=1,CNSTROBJ%N_EQUAT
              IF ( CNSTROBJ%ABB_CNS(J1) .EQ. ABBR ) THEN
                   CNSTROBJ%USR_CNS(J1) = .TRUE.
                   FL_FOUND = .TRUE.
              END IF
 410       CONTINUE
      END IF
!
      IF ( .NOT. FL_FOUND ) THEN
           CALL ERR_LOG ( 8781, IUER, 'SET_USER_CNS', 'Hmm. Constraint '// &
     &         'abbreviation '//ABBR(1:I_LEN(ABBR))//' has not been found '// &
     &         'in the list of defined abbreviation. Can you swear that '// &
     &         'you have defined it??' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SET_USER_CNS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SET_DYN_CNS ( ABBR, IND_EQU, M_CNS, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine SET_DYN_CNS set flag "dynamic constraint         *
! *   equations" and allocates dynamic memory for coefficients of        *
! *   constraints equations. Flag "dynamic" assumes there is more than   *
! *   one equation with the same abbreviation and the same type.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       ABBR ( CHARACTER ) -- Abbreviation of the constraint. It is    *
! *                             unique identifier of the constraint.     *
! *                             It has length 8 symbols.                 *
! *    IND_EQU ( INTEGER*4 ) -- Index of equation of constraint of this  *
! *                             type (abbreviation).                     *
! *      M_CNS ( INTEGER*4 ) -- Maxumal number of non-zero coefficients  *
! *                             of constraint equations which is         *
! *                             expected to be defined to this           *
! *                             constraint.                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   CNSTROBJ ( RECORD    ) -- The data structure with information      *
! *                             about constraints (position where the    *
! *                             matrix should be modified and the value  *
! *                             of constraints).                         *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 14-JUN-2006   SET_DYN_CNS  v1.0 (c)  L. Petrov 14-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'cnstr.i'
      CHARACTER  ABBR*(*)
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  M_CNS, IUER
      CHARACTER  STR*128
      INTEGER*4  J1, IND_CNS, IND_EQU, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IND_CNS = 0
      IF ( CNSTROBJ%N_EQUAT .GT. 0 ) THEN
           DO 410 J1=1,CNSTROBJ%N_EQUAT
              IF ( CNSTROBJ%ABB_CNS(J1) .EQ. ABBR  .AND.  &
     &             CNSTROBJ%SBI_CNS(J1) .EQ. IND_EQU      ) THEN
                   IND_CNS = J1
              END IF
 410       CONTINUE
      END IF
!
      IF ( IND_CNS == 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_EQU, STR )
           CALL ERR_LOG ( 8581, IUER, 'SET_DYN_CNS', 'Hmm. Equation '// &
     &          STR(1:I_LEN(STR))//' and constraint abbreviation '// &
     &          ABBR(1:I_LEN(ABBR))//' has not been found in the list '// &
     &          'of defined abbreviation. Can you swear that you have '// &
     &          'defined it??' )
           RETURN
      END IF
!
      ALLOCATE ( CNSTROBJ%DYN(IND_CNS)%EQU_INE(M_CNS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_CNS, STR )
           CALL ERR_LOG ( 8582, IUER, 'SET_DYN_CNS', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
      ALLOCATE ( CNSTROBJ%DYN(IND_CNS)%EQU_INP(M_CNS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_CNS, STR )
           CALL ERR_LOG ( 8583, IUER, 'SET_DYN_CNS', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
      ALLOCATE ( CNSTROBJ%DYN(IND_CNS)%EQU_CNS(M_CNS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_CNS, STR )
           CALL ERR_LOG ( 8584, IUER, 'SET_DYN_CNS', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT_I4 ( M_CNS, CNSTROBJ%DYN(IND_CNS)%EQU_INE )
      CALL NOUT_I4 ( M_CNS, CNSTROBJ%DYN(IND_CNS)%EQU_INP )
      CALL NOUT_R8 ( M_CNS, CNSTROBJ%DYN(IND_CNS)%EQU_CNS )
!
      CNSTROBJ%DYN(IND_CNS)%STS   = CNS__ALC
      CNSTROBJ%DYN(IND_CNS)%M_CNS = M_CNS
      CNSTROBJ%DYN(IND_CNS)%L_CNS = 0
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SET_DYN_CNS  !#!#
