      SUBROUTINE DO_VELOH ( NPARM, IPARM, VEL_SET_SIGMA, CNSTROBJ, IUER )
!
!  This subroutine applies uen velocity_origin constraints for a
!    specified set of stations.
!  The en (horizontal) constraint is a rotation, and the
!  the up (vertical) constraint is a translation.  Either the horizontal
!  or the vertical or both constraints may be applied.
!
!  modifications
!   kdb 950720   Add the ability to have horizontal-only or vertical-only
!                velocity_origin constraints.
!   jmg 9611??   Parameterize arrays to max_sta,max_sta_par.
!   pet 980206   Declared the sigma of the constrain in solve.i instead of
!                hard-coded value. Write down information about the type
!                of applied constraint.
!   pet 980722   Made VEL_SET_SIGMA formal parameter instead of named
!                constatnt in solve.i block.
!   pet 2002.09.23   Completely re-wrote. Changed internal logic: the
!                    new version puts equations of constraitns in CNSTROBJ,
!                    while the old version modified normal matrix directly.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*4  NPARM, IUER
      INTEGER*2  IPARM(10,M_GPA)
!
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'cnstr.i'
!
      INTEGER*4  IEQU, IER
      INTEGER*2    MAX_STA_PAR
      PARAMETER  ( MAX_STA_PAR=3*MAX_STA)
      INTEGER*2  I, J, N, K,NORIG, ORIGLST(MAX_STA), L, N1
      INTEGER*4  M(MAX_STA_PAR)
      CHARACTER  CPARM*20
      LOGICAL*2  KBIT, EQUAL, KINCL
      REAL*8     XYZ(3,MAX_STA), V(MAX_STA_PAR,6)
      REAL*8     TRAN_CON(3,3,MAX_STA), IDMAT(3,3), ROT_CON(3,3,MAX_STA)
      REAL*8     DENOM
      INTEGER*2  IVSTART,IVSTOP
      REAL*8      VEL_SET_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      DATA IDMAT / 1.D0, 3*0.D0, 1.D0, 3*0.D0, 1.D0 /
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      NORIG = 0
      DO I=1,MAX_STA
         ORIGLST(I) = 0
      ENDDO
!
! --- Build a list of the stations whose velocities are to be
! --- constrained
!
      DO I=1,NUMSTA
         KINCL = KBIT( DEFVEL, INT2(4) )
         DO J=1,ISTASP
            IF ( STASUP(J) == ISITN_CHR(I) ) THEN
                 KINCL = KINCL .NEQV. KBIT ( VELSUP(1,4), J )
                 GOTO 10
            ENDIF
         ENDDO
10       CONTINUE
         IF ( KINCL ) THEN
              NORIG = NORIG+1
             ORIGLST(NORIG) = I
         ENDIF
      ENDDO
!
! --- Determine xyz for unit up vector at each station
!
      DO I=1,NORIG
         N = ORIGLST(I)
         DENOM = DSQRT ( VSITEC(1,N)**2 + VSITEC(2,N)**2 + VSITEC(3,N)**2 )
         DO J=1,3
            XYZ(J,I) = VSITEC(J,N)/DENOM
         ENDDO
      ENDDO
!
! --- Build matrix of projection operators. First set up translation.
!
      IF ( KBIT ( VELOHORIZ, INT2(2) ) ) THEN
           DO I=1,NORIG
              N = ORIGLST(I)
              DO J=1,3
                 DO K=1,3
                    TRAN_CON(J,K,I) = IDMAT(J,K) - XYZ(J,I)*XYZ(K,I)
                 ENDDO
              ENDDO
           ENDDO
      END IF
!
! --- Now set up rotation part. Factor of 1.d11 is to make rotational
! --- constraints have magnitude about equal to 1
!
      IF ( KBIT( VELOHORIZ, INT2(1) ) ) THEN
           DO I=1,NORIG
              N=ORIGLST(I)
              DO J=1,3
                 ROT_CON(J,J,I)=0.0D0
              END DO
              ROT_CON(1,2,I)=XYZ(3,I)
              ROT_CON(2,3,I)=XYZ(1,I)
              ROT_CON(3,1,I)=XYZ(2,I)
              ROT_CON(2,1,I)=-ROT_CON(1,2,I)
              ROT_CON(3,2,I)=-ROT_CON(2,3,I)
              ROT_CON(1,3,I)=-ROT_CON(3,1,I)
           END DO
      END IF
!
! --- Set up index to relevant parameter numbers in nrmfil.
!
      N = 0
      DO I=1,NORIG
         DO J=1,NPARM
            CALL HOL2CHAR ( IPARM(1,J), INT2(1), INT2(20), CPARM )
            IF ( ( EQUAL ( IPARM(1,J), INT2(1), ISITN(1,ORIGLST(I)), INT2(1), &
     &           INT2(8)) ) .AND.( INDEX ( CPARM, 'VELOCITY' ) .NE. 0 ) ) THEN
                 N = N+1
                 M(N) = J
            ENDIF
         ENDDO
      ENDDO
!
! --- Convert projection operator matrix to six vectors Each parameter
! --- (e.g, Haystack X velocity) will have 6 applicable values.
!
      DO I=1,NORIG  ! Run over sites to be constrained
         DO J=1,3
            N1 = (I-1)*3+J
            DO K=1,3
               V(N1,K)   = TRAN_CON ( J, K, I )
               V(N1,K+3) = ROT_CON  ( J, K, I )
            ENDDO
         ENDDO
      ENDDO
!
! --- Add appropriate values to the normal equations matrix
!
      IF ( VELOHORIZ .EQ. 3 ) THEN
!
! -------- doing both translation and rotation - use whole range of v
!
           IVSTART = 1
           IVSTOP  = 6
        ELSE IF ( VELOHORIZ .EQ. 1 ) THEN
!
! -------- just rotation (horizontal)
!
           IVSTART = 4
           IVSTOP  = 6
        ELSE IF ( VELOHORIZ .EQ. 2 ) THEN
!
! -------- just translation (vertical)
!
           IVSTART = 1
           IVSTOP  = 3
      ENDIF
!
      DO I=1,NORIG*3
         IEQU = 0
         DO L=IVSTART,IVSTOP
            IEQU = IEQU + 1
!
! --------- Add information about the type of the constraint applied
!
            CALL ERR_PASS ( IUER, IER )
            CALL ADDCNS_NAM ( 'VEL_SET', IEQU, 'Velocity origin for set '// &
     &          'of stat.', 'meter/year', 0.0D0, VEL_SET_SIGMA, TRUE_L4, &
     &           CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8701, IUER, 'DO_VELOH', 'Error '// &
     &               'in an attempt to put information about the '// &
     &               ' right ascension constraints' )
                 RETURN
            END IF
!
! --------- Add coefficients of constraint equations
!
            CALL ERR_PASS ( IUER, IER )
            CALL ADDCNS_EQU ( 'VEL_SET', IEQU, M(I), V(I,L), &
     &                         TRUE_L4, CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8702, IUER, 'DO_VELOH', 'Error in an '// &
     &               'attempt to put coefficients of the velocity origin '// &
     &               'suppression constraint' )
                 RETURN
            END IF
          ENDDO
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_VELOH  #!#
