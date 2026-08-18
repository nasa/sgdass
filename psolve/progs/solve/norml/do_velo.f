      SUBROUTINE DO_VELO ( KVELO, IXATS, NPARM, ICMP, VEL_ORG_SIGMA, CNSTROBJ, &
     &                     IUER )
!CCCCC
!
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980722 Made VEL_ORG_SIGMA formal parameter instead of named
!             constatnt in solve.i block.
!  pet 2002.09.24   Completely re-wrote. Changed internal logic: the
!                   new version puts equations of constraitns in CNSTROBJ,
!                   while the old version modified normal matrix directly.
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INTEGER*4  NPARM, IUER
      INTEGER*4  IXATS(M_GPA)
      INTEGER*2  ICMP, KVELO(STA_BIT_WORDS)
!
      INCLUDE    'socom.i'
      INCLUDE    'cnstr.i'
!
      INTEGER*2   J
      INTEGER*4   I, IER
      LOGICAL*2   KBIT
      REAL*8      VEL_ORG_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      DO I=1,NPARM
         IF ( IXATS(I) .EQ. 0 ) THEN
   write ( 6, * ) 'DO_VELO-36  i= ', i, ' IXATS(I) = ', IXATS(I) ! %%%%%%%%
!
! ----------- Add information about the type of the constraint applied
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'VEL_ORG', INT4(ICMP), 'Station velocity '// &
     &            'origin', 'meter/year', 0.0D0, VEL_ORG_SIGMA, TRUE_L4, &
     &             CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8711, IUER, 'DO_VELO', 'Error '// &
     &                 'in an attempt to put information about the '// &
     &                 'velocity origin constraints' )
                   RETURN
              END IF
!
! ----------- Add coefficients of constraint equations
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'VEL_ORG', INT4(ICMP), I, 1.0D0, TRUE_L4, &
     &                          CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8712, IUER, 'DO_VELO', 'Error in an '// &
     &                 'attempt to put coefficients of the right ascension '// &
     &                 'suppression constraint' )
                   RETURN
              END IF
         ENDIF
      ENDDO
!
! --- Fix lsitev
!
      DO J=1,NUMSTA
         CALL KSBIT ( LSITEV(1,ICMP), J, KBIT(KVELO,J) )
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_VELO  #!#
