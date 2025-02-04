      SUBROUTINE DO_STAT ( ICNS_ORG, KSTAT, IXATS, NPARM, ICMP, &
     &                     STA_ORG_SIGMA, CNSTROBJ, IUER )
!CCCCC
!
!     This procedure generates constratin eqaution for constraining
!     adjustment of position of this station to zero.
!
!
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980722 Made STA_ORG_SIGMA formal parameter instead of named
!             constatnt in solve.i block.
!  pet  2002.09.20   Completely re-wrote. Changed internal logic: the
!                    new version puts equations of constraitns in CNSTROBJ,
!                    while the old version modified normal matrix directly.
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*4  NPARM, IXATS(M_GPA), IUER
      INTEGER*2  ICNS_ORG, ICMP, KSTAT(STA_BIT_WORDS)
!
      INCLUDE   'socom.i'
      INCLUDE   'cnstr.i'
!
      INTEGER*4   I, IER
      INTEGER*2   J
      LOGICAL*2   KBIT
      REAL*8      STA_ORG_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      ICNS_ORG = ICNS_ORG + 1
      DO I=1,NPARM
         IF ( IXATS(I) .EQ. 0 ) THEN
!
! ----------- Add information about the type of the constraint applied
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'STA_ORG', INT4(ICNS_ORG), 'Station position '// &
     &            'origin', 'meter', 0.0D0, STA_ORG_SIGMA, TRUE_L4, CNSTROBJ, &
     &             IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8651, IUER, 'DO_STAT', &
     &                 'Error in an attempt to put information '// &
     &                 'about the station position origin constraint' )
                   RETURN
              END IF
!
! ----------- Put coefficients of constraint equations
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'STA_ORG', INT4(ICNS_ORG), I, 1.0D0, &
     &                           TRUE_L4, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8652, IUER, 'DO_STAT', &
     &                 'Error in an attempt to put coefficients of '// &
     &                 'station origin constraint' )
                   RETURN
              END IF
         ENDIF
      ENDDO
!
! --- Fix lsitec  ??
!
      DO J=1,NUMSTA
         CALL KSBIT ( LSITEC(1,ICMP), INT2(J), KBIT(KSTAT,INT2(J)) )
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_STAT  #!#
