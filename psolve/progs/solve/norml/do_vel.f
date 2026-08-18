      SUBROUTINE DO_VEL ( ICNS_VEL_SUP, KVEL, IXATS, NPARMA, ISTA, ISUPVL, &
     &                    VEL_VER_SIGMA, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   FORCE DOT PRODUCT OF DIRECTION VECTOR TO ADJUSTMENT TO BE TO HIGH  *
! *   PRECISION, THUS THERE CAN ONLY BE ADJUSTMENT ORTHOGONAL            *
! *   TO DIRECTION VECTOR.                                               *
! *                                                                      *
! ************************************************************************
!
!CCCC
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980722 Made VEL_VER_SIGMA formal parameter instead of named
!             constatnt in solve.i block.
!  pet  2002.09.23   Completely re-wrote. Changed internal logic: the
!                    new version puts equations of constraitns in CNSTROBJ,
!                    while the old version modified normal matrix directly.
!CCCC
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*4  NPARMA, IXATS(NPARMA)
      INTEGER*2  ICNS_VEL_SUP, ISTA, ISUPVL
      LOGICAL*2  KVEL(3)
      INTEGER*4  IUER
!
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'cnstr.i'
!
      INTEGER*2  I, J, NPARM
      INTEGER*4  NP(3), IER
      LOGICAL*2  KBIT
      REAL*8     MAT(3,3), XYZ(3), D(3)
      COMMON    / LCLEMA / MAT
      REAL*8      VEL_VER_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      XYZ(1)=VSITEC(1,ISTA)
      XYZ(2)=VSITEC(2,ISTA)
      XYZ(3)=VSITEC(3,ISTA)
      CALL UEN_ROT(XYZ,MAT )
!
      DO J=1,NPARMA
         IF ( IXATS(J).EQ.0 ) THEN
              NPARM=J
              GOTO 810
         ENDIF
      ENDDO
      CALL FERR ( INT2(106), 'DO_VEL: Too Many Parameters.', INT2(0), INT2(0) )
 810  CONTINUE
!
      NP(1)=NPARM
      IF ( KVEL(1) ) NPARM=NPARM+1
      NP(2)=NPARM
      IF ( KVEL(2) ) NPARM=NPARM+1
      NP(3)=NPARM
!
      DO I=1,3
         IF ( KBIT(ISUPVL,I) ) THEN
              DO J=1,3
                 D(J)=MAT(I,J)
              ENDDO
!
! ----------- Add information about the type of the constraint applied
!
              ICNS_VEL_SUP = ICNS_VEL_SUP + 1
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'VEL_VER', INT4(ICNS_VEL_SUP), 'Velocity '// &
     &            'suppression component', 'meter/year', 0.0D0, VEL_VER_SIGMA, &
     &             TRUE_L4, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8661, IUER, 'DO_VEL', 'Error in an '// &
     &                 'attempt to put information about the velocity '// &
     &                 'component suppression' )
                   RETURN
              END IF
!
! ----------- Put coefficients of constraint equations
!
              DO J=1,3
                 IF ( KVEL(J) ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'VEL_VER', INT4(ICNS_VEL_SUP), NP(J), &
     &                                   D(J), TRUE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8662, IUER, 'DO_VEL', &
     &                         'Error in an attempt to put coefficients of '// &
     &                         'the velocity component suppression '// &
     &                         'constraint' )
                           RETURN
                      END IF
                 END IF
              END DO
         ENDIF
      ENDDO
!
! --- Fix up LSITEV
!
      DO I=1,3
         CALL KSBIT ( LSITEV(1,I), ISTA, KVEL(I) )
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_VEL  #!#
