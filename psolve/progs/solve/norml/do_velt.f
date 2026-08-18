      SUBROUTINE DO_VELT ( ICNS_TIE, KVELO, IXATS, NPARM, ICMP, &
     &                     VEL_TIE_SIGMA, CNSTROBJ, IUER )
!CCCCC
!
!  pet  980206 Declared the sigma of the constrain in solve.i instead of
!              hard-coded value. Write down information about the type
!              of applied constraint.
!  pet  980722 Made VEL_TIE_SIGMA formal parameter instead of named
!              constant in solve.i block.
!  pet  2002.09.19  Again completely re-wrote. Changed internal logic: the
!                   new version puts equations of constraitns in CNSTROBJ,
!                   while the old version modified normal matrix directly.
!  pet  2004.03.15  Imporoved comments
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'cnstr.i'
      INTEGER*2  ICNS_TIE, ICMP, KVELO(STA_BIT_WORDS)
      INTEGER*4  IXATS(M_GPA), NPARM, J1, IP, IUER
!
      INTEGER*4   I, J, IER
      LOGICAL*2   KBIT
      REAL*8      VEL_TIE_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      DO I=1,NPARM
         IF ( IXATS(I).EQ.0 ) THEN
              DO J=1,I-1
                 IF ( IXATS(J) .EQ. 0 ) THEN
                      ICNS_TIE = ICNS_TIE + 1
!
! ------------------- Add information about the type of the constraint applied
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_NAM ( 'VEL_TIE', INT4(ICNS_TIE), &
     &                     'Station velocity ties', 'meter/year', 0.0D0, &
     &                     VEL_TIE_SIGMA, TRUE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8631, IUER, 'DO_VELT', &
     &                         'Error in an attempt to put information '// &
     &                         'about velocity ties constraint' )
                           RETURN
                      END IF
!
! ------------------- Put coefficients of constraint equations
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'VEL_TIE', INT4(ICNS_TIE), J, &
     &                                  1.0D0, TRUE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8632, IUER, 'DO_VELT', &
     &                         'Error in an attempt to put coefficients of '// &
     &                         'velocity ties constraint' )
                           RETURN
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'VEL_TIE', INT4(ICNS_TIE), I, &
     &                                  -1.0D0, TRUE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8633, IUER, 'DO_VELT', &
     &                         'Error in an attempt to put coefficients of '// &
     &                         'velocity ties constraint' )
                           RETURN
                      END IF
                 ENDIF
              ENDDO
         ENDIF
      ENDDO
!
! --- Fix LSITEV. The true is that, in order to apply velocity ties constraints,
! --- the program FIND_VELT modified LSITEV. Now we have to play back and undo
! --- these modifications.
!
      DO J=1,NUMSTA
         CALL KSBIT ( LSITEV(1,ICMP), INT2(J), KBIT(KVELO,INT2(J)) )
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_VELT  #!#
