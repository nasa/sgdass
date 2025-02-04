      SUBROUTINE DO_DIR ( KVEL, IXATS, NPARMA, ISTAO, ISUPVL, VEL_DIR_SIGMA, &
     &                    CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INTEGER*2  ISTAO, ISUPVL, NPARMA, IXATS(NPARMA)
      LOGICAL*2  KVEL(3)
!
      INCLUDE    'socom.i'
      INCLUDE    'prfil.i'
      INCLUDE    'glbc3.i'
      INCLUDE    'cnstr.i'
!
      INTEGER*4  IUER, IER
      INTEGER*2  I, J, ISTAD, NPARM, NP(3), K
      LOGICAL*2  KBIT, KUSE(3), KUEN(3)
      LOGICAL*4  TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      REAL*8     MAT(3,3), XYZ(3), D(3), TEMP(3), PART(3), VEL_DIR_SIGMA
      COMMON   / LCLEMA / MAT
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!CCCCC
!
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980722 Made VEL_DIR_SIGMA formal parameter instead of named
!             constatnt in solve.i block.
!  pet  2002.09.23   Completely re-wrote. Changed internal logic: the
!                    new version puts equations of constraitns in CNSTROBJ,
!                    while the old version modified normal matrix directly.
!
!CCCCC
!
      DO I=1,NUMSTA
         IF ( ISITN_CHR(I) .EQ. DTOSTA_CHR ) THEN
              ISTAD=I
              GOTO 20
         ENDIF
      ENDDO
      CALL FERR ( INT2(102), 'DO_DIR Direction Station Not Found.', INT2(0), &
     &     INT2(0) )
!
20    CONTINUE
      XYZ(1) = VSITEC(1,ISTAO)
      XYZ(2) = VSITEC(2,ISTAO)
      XYZ(3) = VSITEC(3,ISTAO)
      CALL UEN_ROT ( XYZ, MAT )
!
      D(1) = VSITEC(1,ISTAD) - VSITEC(1,ISTAO)
      D(2) = VSITEC(2,ISTAD) - VSITEC(2,ISTAO)
      D(3) = VSITEC(3,ISTAD) - VSITEC(3,ISTAO)
!
      CALL VUNIT ( D, TEMP )
!
! --- Rotate axes so that direction vector is in uen frame
!
      DO I=1,3
         D(I)=0.0D0
         DO J=1,3
            D(I) = D(I) + MAT(I,J)*TEMP(J)
         ENDDO
      ENDDO
!
      DO I=1,NPARMA
         IF ( IXATS(I) .EQ. 0 ) THEN
              NPARM=I
              GOTO 101
         ENDIF
      ENDDO
      CALL FERR ( INT2(103), 'DO_DIR: Too Many Parameters.', INT2(0), INT2(0) )
101   CONTINUE
!
      NP(1)=NPARM
      IF(KVEL(1)) NPARM=NPARM+1
      NP(2)=NPARM
      IF(KVEL(2)) NPARM=NPARM+1
      NP(3)=NPARM
!
! --- Force cross product of direction vector to adjustment to be
! --- zero to high precision, thus there can only be adjustment along
! --- direction vector
!
      DO I=1,3
         KUEN(I)=(  .NOT. KBIT(ISUPVL,I)).AND.(( .NOT. KBIT( IUEN, INT2(1))) &
     &    .OR. (KVEL(I) .AND. KBIT( IUEN, INT2(1) )) )
         KUSE(I)=( KVEL(I) .AND. .NOT.KBIT( IUEN, INT2(1) )) .OR.( KBIT( IUEN, &
     &    INT2(1) ) .AND. KVEL(I) .AND. .NOT. KBIT(ISUPVL,I) )
      ENDDO
!
! --- Select partials for each component of cross product result
!
      DO I=1,3
         IF ( I .EQ. 1 .AND. KUEN(2) .AND. KUEN(3) ) THEN
              PART(1)= 0.0D0
              PART(2)=-D(3)
              PART(3)= D(2)
            ELSE IF ( I .EQ. 2  .AND.  KUEN(3)  .AND.  KUEN(1) ) THEN
              PART(1) =  D(3)
              PART(2) =  0.0D0
              PART(3) = -D(1)
           ELSE IF(I.EQ.3.AND.KUEN(1).AND.KUEN(2)) THEN
              PART(1)=-D(2)
              PART(2)= D(1)
              PART(3)= 0.0D0
           ELSE
              PART(1)= 0.0D0
              PART(2)= 0.0D0
              PART(3)= 0.0D0
         ENDIF
!
! ------ If we are actually estimating XYZ, rotate axis to get partials in XYZ
!
         IF ( .NOT. KBIT( IUEN, INT2(1) ) ) THEN
              DO J=1,3
                 TEMP(J)=PART(J)
              ENDDO
              DO J=1,3
                 PART(J)=0.0D0
                 DO K=1,3
                    PART(J) = PART(J) + MAT(K,J)*TEMP(K)
                 ENDDO
              ENDDO
        ENDIF
!
! ----- Add the constraint matrix only if parameters are being adjusted
!
        DO J=1,3
           IF ( KUSE(J) ) THEN
!
! ------------- Add information about the type of the constraint applied
!
                CALL ADDCNS_NAM ( 'VEL_DIR', INT4(I), 'Velocity direction'// &
     &              'meter/year', 0.0D0, VEL_DIR_SIGMA, TRUE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8671, IUER, 'DO_DIR', 'Error in an '// &
     &                   'attempt to put information about the velocity '// &
     &                   'direction suppression constraint' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_EQU ( 'VEL_DIR', INT4(I), NP(J), PART(J), TRUE_L4, &
     &                             CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8672, IUER, 'DO_DIR', 'Error in '// &
     &                   'an attempt to put coefficients of the velocity '// &
     &                   'component suppression constraint' )
                     RETURN
                END IF
           END IF
        ENDDO
      ENDDO
!
! --- Fix up LSITEV
!
      DO I=1,3
         CALL KSBIT(LSITEV(1,I),ISTAO,KVEL(I) )
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_DIR  #!#
