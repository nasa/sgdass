      SUBROUTINE DO_STATT ( ICNS_TIE, KST, IXATS, NPARM, ICMP, STA_TIE_SIGMA, &
     &                      CNSTROBJ, IUER )
!CCCCC
!
!   pet 980206 Declared the sigma of the constrain in solve.i instead of
!              hard-coded value. Write down information about the type
!              of applied constraint.
!   pet 980722 Made STA_TIE_SIGMA formal parameter instead of named
!              constant in solve.i block.
!   pet  2002.09.19  Again completely re-wrote. Changed internal logic: the
!                    new version puts equations of constraitns in CNSTROBJ,
!                    while the old version modified normal matrix directly.
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*2  ICNS_TIE, IXATS(M_GPA), NPARM, ICMP, KST(STA_BIT_WORDS)
      INTEGER*4  IUER, IER
!
      INCLUDE   'socom.i'
      INCLUDE   'cnstr.i'
!
      INTEGER*4   I, J
      LOGICAL*2   KBIT
      REAL*8      STA_TIE_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      DO I=1,NPARM
         IF ( IXATS(I).EQ.0) THEN
            DO J=1,I-1
               IF ( IXATS(J).EQ.0 ) THEN
                    ICNS_TIE = ICNS_TIE + 1
!
! ----------------- Add information about the type of the constraint applied
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL ADDCNS_NAM ( 'STA_TIE', INT4(ICNS_TIE), &
     &                   'Station position ties', 'meter', 0.0D0, &
     &                   STA_TIE_SIGMA, TRUE_L4, CNSTROBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8641, IUER, 'DO_STATT', &
     &                       'Error in an attempt to put information '// &
     &                       'about velocity ties constraint' )
                         RETURN
                    END IF
!
! ----------------- Put coefficients of constraint equations
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL ADDCNS_EQU ( 'STA_TIE', INT4(ICNS_TIE), J, &
     &                                 1.0D0, TRUE_L4, CNSTROBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8632, IUER, 'DO_VELT', &
     &                       'Error in an attempt to put coefficients of '// &
     &                       'station position ties constraint' )
                         RETURN
                    END IF
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL ADDCNS_EQU ( 'STA_TIE', INT4(ICNS_TIE), I, &
     &                                -1.0D0, TRUE_L4, CNSTROBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8633, IUER, 'DO_VELT', &
     &                       'Error in an attempt to put coefficients of '// &
     &                       'station ties constraint' )
                         RETURN
                    END IF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
!
! --- Fix lsitec
!
      DO J=1,NUMSTA
         CALL KSBIT ( LSITEC(1,ICMP), INT2(J), KBIT(KST,INT2(J)) )
      ENDDO
!
      CALL ERR_LOG( 0, IUER )
      RETURN
      END  !#!  DO_STATT  #!#
