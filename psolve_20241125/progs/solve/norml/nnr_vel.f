      SUBROUTINE NNR_VEL ( FAST_DBG, SPOOL_L1, NOR_MAT, LPARM, NPARM, &
     &                     NNR_VEL_RTP, NNR_VEL_SIGMA, CNSTROBJ, IUER )
!CCCCC
!
!  This subroutine constrains a set of velocities
!  to minimize the distance to the a priori model.
!
!  This is done by applying a rotation.
!  NNR stands for no net rotation.
!
!  modifications
!
!  jmg 9611?? created in test mode (by overwriting paths used for other
!             batch control features)
!  kdb 961125 officially install into solve, connecting to new batch control
!             keywords
!  kdb,970312 Implement the ability to use weighted or uniform constraints via
!   jmg         the batopt file.
!             (Algorithm from jmg, batopt connection from kdb).
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980401 Corrected a bug: array of indecis m was not initialized;
!             procedure was trying to impose constraints even if no velocities
!             were estimated
!  pet 980721 Added printing the list of stations participated in constrints
!             to the spool-file when SPOOL_L1 is .TRUE.
!  pet 980721 Added a trap of internal control. For a case.
!  pet 980722 Made NNR_VEL_SIGMA formal parameter instead of named
!             constant in solve.i block.
!  pet 980727 Added sorting in alphabetic order the stations list to be
!             printed
!  pet 2002.05.10  Add support of argument IUER. Changed internal logic: the
!                  previous version modified normal equations directly.
!                  The new version saves constraitn equations in CNSTROBJ data
!                  structure. Added argument right part of constraint
!                  equations
!  pet 2002.05.17  Improved comments
!  pet 2007.05.11  Corrected logic in accordance to changfes in GCONST
!  pet 2021.10.22  Fixed logic for dealing with station exception list.
!                  No the excpetion station list is exclusive for this constraint.
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'fast.i'
!
      INTEGER*4    FAST_DBG, IUER
      LOGICAL*1    SPOOL_L1
      INTEGER*4    NPARM
      CHARACTER*20 LPARM(*)
      REAL*8       NNR_VEL_RTP(3), NNR_VEL_SIGMA, NOR_MAT(*)
      TYPE       ( CNSTR__STRU ) ::    CNSTROBJ
!
      INTEGER*4 MAX_STA_PAR
      PARAMETER ( MAX_STA_PAR=3*MAX_STA )
      INTEGER*2   I, J, N, K, N_VEL, VEL_STA(MAX_STA), N1
      INTEGER*2   NELEM, ICON, ISTA
      INTEGER*4   IXREF(MAX_STA_PAR), J1
      INTEGER*8   IND
      LOGICAL*2   KBIT, EQUAL, KINCL
      REAL*8      XYZ(3,MAX_STA), CNS_EQU(MAX_STA_PAR,3), &
     &            ROT_CON(3,3,MAX_STA), DENOM
      LOGICAL*4    TRUE_L4
      PARAMETER  ( TRUE_L4 = .TRUE. )
      REAL*8       RLEN_SQ, REA, EPS
      PARAMETER  ( REA = 6378136.D0 ) ! Earth radius (in m)
      PARAMETER  ( EPS = 1.D-32 ) ! A very small number
      INTEGER*4  IP, IPARM, IER
      CHARACTER  LIS_STA(MAX_STA)*8, OUT*4096, STR*20, STR1*20
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*8, EXTERNAL :: INDX8
!
! --- Initialization of the array of indices
!
      CALL NOUT_I4 (   MAX_STA_PAR, IXREF )
      CALL NOUT_R8 ( 3*MAX_STA_PAR, CNS_EQU )
!
      N_VEL = 0
      DO I=1,NUMSTA
         VEL_STA(I) = 0
      ENDDO
!
! --- Build a list of the stations whose velocities are to be constrained
!
      DO I=1,NUMSTA
         KINCL = KBIT ( DEFVEL, INT2(6) )
         IF ( IND_NNR_VEL(1) > 0  .AND. IND_NNR_VEL(2) .GE. IND_NNR_VEL(1) ) THEN
              DO J=IND_NNR_VEL(1),IND_NNR_VEL(2)
                 IF ( STASUP(J) .EQ. ISITN_CHR(I) ) THEN
                      IF ( KBIT ( VELSUP(1,6), J ) ) THEN
                           KINCL = .NOT. KINCL
                      END IF 
                      GOTO 810
                 ENDIF
              ENDDO
  810         CONTINUE
         END IF
!
! ------ Test: are velocities of the I-th station estimated?
!
         IF ( .NOT. KBIT(LSITEV(1,1),I)  .AND. &
     &        .NOT. KBIT(LSITEV(1,2),I)  .AND. &
     &        .NOT. KBIT(LSITEV(1,3),I)         ) KINCL = .FALSE.
         IF ( KINCL ) THEN
              N_VEL = N_VEL + 1
              VEL_STA(N_VEL) = I
         ENDIF
      ENDDO
!
      NELEM=3*N_VEL
      IF ( NELEM .EQ. 0 ) THEN
!
! -------- No station velocities are to be constrainted: good bye!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Determine xyz for unit up vector at each station
!
      DO I=1,N_VEL
         N = VEL_STA(I)
         DENOM = DSQRT ( VSITEC(1,N)**2 + VSITEC(2,N)**2 + VSITEC(3,N)**2 )
         DO J=1,3
            XYZ(J,I) = VSITEC(J,N)/DENOM
         ENDDO
      ENDDO
!
! --- Now set up rotation part.
!
      DO I=1,N_VEL
         DO J=1,3
            ROT_CON(J,J,I) = 0.0D0
         END DO
!
         ROT_CON(1,2,I) =  XYZ(3,I)
         ROT_CON(2,3,I) =  XYZ(1,I)
         ROT_CON(3,1,I) =  XYZ(2,I)
         ROT_CON(2,1,I) = -ROT_CON(1,2,I)
         ROT_CON(3,2,I) = -ROT_CON(2,3,I)
         ROT_CON(1,3,I) = -ROT_CON(3,1,I)
      END DO
!
! --- Set up index to relevant parameter numbers in nrmfil.
!
      N = 0
      DO I=1,N_VEL
         DO J1=1,NPARM
            IF ( (LPARM(J1)(1:8)  .EQ. ISITN_CHR(VEL_STA(I)) ) .AND. &
     &           ( LPARM(J1)(12:20) .EQ. "VELOCITY ") ) THEN
                 N = N+1
                 IXREF(N) = J1
            ENDIF
         ENDDO
      ENDDO
!
! --- It wouldn't be amiss to check consistency of the array ixref. They were
! --- cases in the past when it contained zero elements...
!
      DO J=1,NELEM
         IF ( IXREF(J) .LE. 0  .OR.  IXREF(J) .GT. NPARM ) THEN
              WRITE ( 6, * ) ' nelem = ',nelem
              WRITE ( 6, * ) ' ixref = ',(ixref(i),i=1,nelem)
!
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(J), STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IXREF(J), STR1 )
!
              CALL ERR_LOG ( 8531, IUER, 'NNR_VEL', 'Trap of '// &
     &            'internal control: array "IXREF" of indices of the '// &
     &            'parameters to be constraints contains a bad '// &
     &             STR(1:I_LEN(STR))//'-th element: '//STR1(1:I_LEN(STR1)) )
              RETURN
         END IF
      END DO
!
! --- Convert projection operator matrix to six vectors
! --- Each parameter (e.g, Haystack X velocity) will have 3 applicable values.
!
      DO I=1,N_VEL  ! Run over sites to be constrained
         DO J=1,3
            N1 = (I-1)*3+J
            DO K=1,3
               CNS_EQU(N1,K)=ROT_CON(J,K,I)
            ENDDO
         ENDDO
         IF ( SPOOL_L1 ) THEN
!
! ----------- Add station to the list of stations for further printing
!
              LIS_STA(I) = ISITN_CHR(VEL_STA(I))
         END IF
      ENDDO
!
! --- If we want to weight by aproximate sigmas, do so.
!
      IF ( KSIG_SCALE_NNRV .EQ. 'WG' ) THEN
           DO ICON=1,3
              DO I=1,NELEM
                 IND=INDX8 ( IXREF(I), IXREF(I) )
                 CNS_EQU(I,ICON) = CNS_EQU(I,ICON)*NOR_MAT(IND)
              END DO
              CALL NORMALIZE_VECTOR ( CNS_EQU(1,ICON), NELEM )
           END DO
         ELSE IF ( KSIG_SCALE_NNRV .EQ. 'UN' ) THEN
!
! -------- Uniform constraints
!
           CALL NOUT_R8 ( 3*MAX_STA_PAR, CNS_EQU )
           DO I=1,INT2(N_VEL)  ! Run over sites to be constrained
!
              ISTA = VEL_STA(I)
              RLEN_SQ = VSITEC(1,ISTA)**2 +VSITEC(2,ISTA)**2 +VSITEC(3,ISTA)**2
!
!@              CNS_EQU((I-1)*3+2,1) = -VSITEC(3,ISTA)/RLEN_SQ*REA ! Rotation around
!@              CNS_EQU((I-1)*3+3,1) =  VSITEC(2,ISTA)/RLEN_SQ*REA ! X-axis
!@!
!@              CNS_EQU((I-1)*3+1,2) =  VSITEC(3,ISTA)/RLEN_SQ*REA ! Rotation around
!@              CNS_EQU((I-1)*3+3,2) = -VSITEC(1,ISTA)/RLEN_SQ*REA ! Y-axis
!@!
!@              CNS_EQU((I-1)*3+1,3) = -VSITEC(2,ISTA)/RLEN_SQ*REA ! Rotation around
!@              CNS_EQU((I-1)*3+2,3) =  VSITEC(1,ISTA)/RLEN_SQ*REA ! Z-axis
!
              CNS_EQU((I-1)*3+2,1) =  VSITEC(3,ISTA)/REA ! Rotation around
              CNS_EQU((I-1)*3+3,1) = -VSITEC(2,ISTA)/REA ! X-axis
!
              CNS_EQU((I-1)*3+1,2) = -VSITEC(3,ISTA)/REA ! Rotation around
              CNS_EQU((I-1)*3+3,2) =  VSITEC(1,ISTA)/REA ! Y-axis
!
              CNS_EQU((I-1)*3+1,3) =  VSITEC(2,ISTA)/REA ! Rotation around
              CNS_EQU((I-1)*3+2,3) = -VSITEC(1,ISTA)/REA ! Z-axis
           ENDDO
         ELSE
           CALL ERR_LOG ( 8532, IUER, 'NNR_VEL', 'Trap of internal '// &
     &         'control: unknown value of KSIG_SCALE_NNRV: '//KSIG_SCALE_NNRV )
           RETURN
      ENDIF
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) 'nnr_vel: rtp(1:3):  ', nnr_vel_rtp(1:3)      ! %%%%
!   write ( 6, * ) 'nnr_vel:    sigma:  ', nnr_vel_sigma         ! %%%%
!   write ( 6, * ) 'nnr_vel: equ_cns(1) ', cns_equ(1:3*n_vel,1)  ! %%%%
!   write ( 6, * ) 'nnr_vel: equ_cns(2) ', cns_equ(1:3*n_vel,2)  ! %%%%
!   write ( 6, * ) 'nnr_vel: equ_cns(3) ', cns_equ(1:3*n_vel,3)  ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      DO ICON=1,3
!
! ------ Add information about the type of the constraint applied
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'NNR_VEL', INT4(ICON), 'No-net rotation for '// &
     &       'velocities', 'meter/year', NNR_VEL_RTP(ICON), NNR_VEL_SIGMA, &
     &        TRUE_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8533, IUER, 'NNR_VEL', 'Failure '// &
     &            'to add description of constraint to an internal data '// &
     &            'structure' )
              RETURN
         END IF
!
! ------ Put the non-zero elements of equations of constraints into CNSTROBJ
!
         DO IPARM=1,NELEM
            IF ( IXREF(IPARM) .GT. 0 ) THEN
                 IF ( DABS(CNS_EQU(IPARM,ICON)) .GT. EPS ) THEN
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'NNR_VEL', INT4(ICON), &
     &                     IXREF(IPARM), CNS_EQU(IPARM,ICON), TRUE_L4, &
     &                     CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8534, IUER, 'NNR_VEL', &
     &                         'Failure to write a NNR_VEL equation of '// &
     &                         'constraints' )
                           RETURN
                      END IF
                 END IF
            END IF
         END DO
      END DO
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' nnr_vel       n_cnstr = ',cnstrobj%n_ecnst
      END IF
!
      IF ( SPOOL_L1 ) THEN
!
! -------- Writing the list of stations in spool-file
!
           WRITE ( 23, '(A,I4,A)' ) 'NNR_VEL: ', N_VEL, ' stations '// &
     &                              'participated in NNR_VEL constraints: '
           IF ( N_VEL .GT. 0 ) THEN
!
! -------------- Sorting the list of stations
!
                 CALL SORT_CH ( INT4(N_VEL), LIS_STA )
!
! -------------- Cleaning the output line with the list stations
!
                 CALL CLRCH ( OUT )
                 DO I=1,N_VEL
!
! ----------------- Add the I-th station to the list of stations for further
! ----------------- printing
!
                    IP  = (I-1)*9 + 1
                    OUT = OUT(1:IP)//LIS_STA(I)
                 END DO
!
! -------------- Writing long lines in spool-file by splitting it onto
! -------------- sublines no nore than 72 characters long
!
                 CALL WRITE_LONG ( 23, 72, OUT(2:) )
           END IF
           WRITE ( 23, 220 ) NNR_VEL_SIGMA, NNR_VEL_RTP
 220       FORMAT ( 'NNR_VEL sigma: ', 1PD13.6, ' right hand sides: ', 3(1PD13.6,1X), ' m/yr' ) 
           WRITE ( 23, '(A)' ) ' '
      END IF
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  NNR_VEL  !#!#
