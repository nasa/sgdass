      SUBROUTINE DO_RA ( KSRCC, IRA, IXATS, NPARM, RAS_ORG_SIGMA, CNSTROBJ, &
     &                   IUER )
!CCCCC
!
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980722 Made RAS_ORG_SIGMA formal parameter instead of named
!             constatnt in solve.i block.
!  pet  2002.09.23   Completely re-wrote. Changed internal logic: the
!                    new version puts equations of constraitns in CNSTROBJ,
!                    while the old version modified normal matrix directly.
!  pet 2017.11.07    Converted to INTEGER*4
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*2  KSRCC(SRC_BIT_WORDS)
      INTEGER*4  NPARM, IXATS(NPARM), IRA(MAX_SRC)
      INTEGER*4  IUER
!
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'cnstr.i'
!
      INTEGER*4   IER
      INTEGER*4   I, J, INOWI
      LOGICAL*2   KBIT4
      REAL*8      RAS_ORG_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
!
      INOWI=0
      DO I=1,NPARM
         IF ( IXATS(I) .EQ. 0 ) THEN
              INOWI = INOWI + 1
!
! ----------- Add information about the type of the constraint applied
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'RAS_ORG', 1, 'Right ascension '//'origin', &
     &            'rad', 0.0D0, RAS_ORG_SIGMA, TRUE_L4, CNSTROBJ,IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8681, IUER, 'DO_RA', 'Error '// &
     &                 'in an attempt to put information about the '// &
     &                 ' right ascension constraints' )
                   RETURN
              END IF
!
! ----------- Add coefficients of constraint equations
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'RAS_ORG', 1, I, DCOS(VSTARC(2,IRA(INOWI))), &
     &                           TRUE_L4, CNSTROBJ,IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8682, IUER, 'DO_RA', 'Error in an '// &
     &                 'attempt to put coefficients of the right ascension '// &
     &                 'suppression constraint' )
                   RETURN
              END IF
         ENDIF
      ENDDO
!
! --- Fix lstar
!
      DO J=1,NUMSTR
         IF ( KBIT4(KSRCC,J) ) CALL SBIT4 ( LSTAR(1,1), J, INT2(1) )
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_RA  #!#
