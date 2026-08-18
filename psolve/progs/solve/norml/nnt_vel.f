      SUBROUTINE NNT_VEL ( FAST_DBG, SPOOL_L1, NOR_MAT, LPARM, NPARM, &
     &                     NNT_VEL_RTP, NNT_VEL_SIGMA, CNSTROBJ, IUER )
!CCCCC
!
!  This subroutine constrains a set of velocities
!  to minimize the distance to the a priori model.
!
!  This is done by applying a translation.
!  NN stands for no net
!  modifications
!
!  jmg 9611?? created in test mode (by overwriting paths used for other
!             batch control features)
!  kdb 961125 officially install into solve, connecting to new batch control
!             keywords
!  kdb 970312 Implement the ability to use weighted or uniform constraints via
!      jmg    the batopt file.
!             Also implement in batopt the ability to use the horizontal
!               projection matrix vs. just the identity matrix.
!             (Algorithm from jmg, batopt connection from kdb).
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980401 Corrected a bug: array of indices ixref was not initialized;
!             procedure was trying to impose constraints even if no velocities
!             were estimated
!  pet 980721 Added printing the list of stations participated in constraints
!             to the spool-file when SPOOL_L1 is .TRUE.
!  pet 980721 Added a trap of internal control. For a case.
!  pet 980722 Made NNT_VEL_SIGMA formal parameter instead of named
!             constant in solve.i block.
!  pet 980727 Added sorting in alphabetic order the stations list to be
!             printed
!  pet 2002.05.10  Add support of argument IUER. Changed internal logic: the
!                  previous version modified normal equations directly.
!                  The new version saves constraitn equations in CNSTROBJ data
!                  structure. Added argument right part of constraint
!                  equations
!  pet 2002.05.17  Fixed the bug: the previous version always normalized
!                  constraint equations
!  pet 2002.07.26  Fixed the bug: changes made on 2002.05.17 effectively
!                  disabled horizontal only no-net-translation constraints
!  pet 2007.05.11  Corrected logic in accordance to changfes in GCONST
!  pet 2021.10.22  Fixed logic for dealing with station exception list. &
!                  No the excpetion station list is exclusive for this constraint.
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
!     Updated to specificaly type integers which
!-------------------------------------------------
!
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'fast.i'
!
      LOGICAL*1    SPOOL_L1
      INTEGER*4    FAST_DBG, NPARM, IUER
      CHARACTER*20 LPARM(*)
      REAL*8       NNT_VEL_RTP(3), NNT_VEL_SIGMA, NOR_MAT(*)
      TYPE       ( CNSTR__STRU ) :: CNSTROBJ
!
      INTEGER*2 MAX_STA_PAR
      PARAMETER  ( MAX_STA_PAR=3*MAX_STA)
      INTEGER*2    I, J, N, K, N_VEL, VEL_STA(MAX_STA), &
     &             N1, IND_STA, NELEM
      INTEGER*4    IXREF(MAX_STA_PAR), J1
      INTEGER*8    IND 
      LOGICAL*2    KBIT, EQUAL, KINCL, FL_OBSOLETE
      REAL*8       CNS_EQU(MAX_STA_PAR,3), IDMAT(3,3)
      REAL*8       DENOM
      DATA         IDMAT / &
     &                     1.0D0, 0.0D0, 0.0D0, &   ! identitiy
     &                     0.0D0, 1.0D0, 0.0D0, &   ! matrix
     &                     0.0D0, 0.0D0, &
     &                   1.0D0/
      LOGICAL*4    TRUE_L4
      PARAMETER  ( TRUE_L4 = .TRUE. )
!
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-32 ) ! A very small number
      INTEGER*2  ICON
      INTEGER*4  IP, IPARM, IER
      CHARACTER  ARR_NAM(MAX_STA)*8, OUT*4096, STR*20, STR1*20
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4,  EXTERNAL :: I_LEN
      INTEGER*8,  EXTERNAL :: INDX8
!
!
! --- Initialization of the array of indecis IXREF, constraint equations CNS_EQU
! --- and array VEL_STA
!
      CALL NOUT    ( 2*INT4(MAX_STA_PAR), IXREF   )
      CALL NOUT_R8 ( 3*INT4(MAX_STA_PAR), CNS_EQU )
!
      N_VEL = 0
      DO I=1,NUMSTA
         VEL_STA(I) = 0
      ENDDO
!
! --- Allow to use obosolete logic
!
      FL_OBSOLETE = .FALSE.
      CALL GETENVAR ( 'NNT_VEL_PREMAY200', STR )
      IF ( STR .EQ. 'YES'  .OR.  STR .EQ. 'yes' ) FL_OBSOLETE = .TRUE.
!
! --- Build a list of the stations whose velocities are to be constrained
!
      DO I=1,NUMSTA
         KINCL = KBIT( DEFVEL, INT2(5))
         IF ( IND_NNT_VEL(1) > 0  .AND. IND_NNT_VEL(2) .GE. IND_NNT_VEL(1) ) THEN
              DO J=IND_NNT_VEL(1),IND_NNT_VEL(2)
                 IF ( STASUP(J) .EQ. ISITN_CHR(I) ) THEN
                      IF ( KBIT ( VELSUP(1,5), J ) ) THEN
                           KINCL = .NOT. KINCL
                      END IF 
                      GOTO 810
                 ENDIF
              ENDDO
 810          CONTINUE
         END IF
!
! ------ Test: are velocities of the I-th station estimated?
!
         IF ( .NOT. KBIT(LSITEV(1,1),I)  .AND. &
     &        .NOT. KBIT(LSITEV(1,2),I)  .AND. &
     &        .NOT. KBIT(LSITEV(1,3),I)         ) KINCL = .FALSE.
         IF ( KINCL ) THEN
              N_VEL = N_VEL+1
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
! --- Set up index to relevant parameter numbers in nrmfil.
!
      N = 0
      DO I=1,N_VEL
         DO J1=1,NPARM
            IF ( ( LPARM(J1)(1:8)   .EQ. ISITN_CHR(VEL_STA(I))) .AND. &
     &           ( LPARM(J1)(12:20) .EQ. "VELOCITY ") ) THEN
                 N = N + 1
                 IXREF(N) = J1
            ENDIF
         ENDDO
      ENDDO
!
! --- It wouldn't be amiss to check consistency of the array m. They were
! --- cases in the past when it contained zero elements...
!
      DO J=1,NELEM
         IF ( IXREF(J) .LE. 0  .OR.  IXREF(J) .GT. NPARM ) THEN
              WRITE ( 6, * ) ' ISTASP = ', ISTASP
              IF ( ISTASP > 0 ) THEN
                   WRITE ( 6, * ) 'STASUP= ', STASUP(1:ISTASP)//' '
              END IF
              WRITE ( 6, * ) ' N_VEL = ',N_VEL
              WRITE ( 6, * ) ' NELEM = ',NELEM
              WRITE ( 6, * ) ' IXREF = ',(IXREF(I),I=1,NELEM)
!
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(J), STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IXREF(J), STR1 )
!
              CALL ERR_LOG ( 8542, IUER, 'NNT_VEL', 'Trap of '// &
     &            'internal control: array "IXREF" of indices of the '// &
     &            'parameters to be constraints contains a bad '// &
     &             STR(1:I_LEN(STR))//'-th element: '//STR1(1:I_LEN(STR1)) )
              RETURN
         END IF
      END DO
!
! --- Convert projection operator matrix to three vectors
! --- Each parameter (e.g, Haystack X velocity) will have 3 applicable values.
!
      DO I=1,N_VEL  ! Run over sites to be constrained
         IF ( KMATRIX_NNTV .EQ. 'HR' ) THEN
!
! ----------- Horizontal only components
!
              IND_STA = VEL_STA(I)
              DENOM = ( VSITEC(1,IND_STA)**2 + VSITEC(2,IND_STA)**2 + &
     &                  VSITEC(3,IND_STA )**2 )
              DO J=1,3
                 DO K=1,3
                    N1 = (I-1)*3+K
                    CNS_EQU(N1,J) = IDMAT(J,K) - &
     &                              VSITEC(J,IND_STA)*VSITEC(K,IND_STA)/DENOM
                 ENDDO
              ENDDO
            ELSE
!
! ----------- All components
!
              DO J=1,3
                 N1 = (I-1)*3+J
                 CNS_EQU(N1,J) = 1.0D0
              ENDDO
         END IF
!
         IF ( SPOOL_L1 ) THEN
!
! ----------- Add the I-th station to the list of stations for further printing
!
              ARR_NAM(I) = ISITN_CHR(VEL_STA(I))
         END IF
      ENDDO
!
      DO ICON=1,3
         IF ( KSIG_SCALE_NNTV .EQ. 'WG' ) THEN
!
! ----------- If we want to weight by aproximate sigmas, do so.
!
              DO IPARM=1,NELEM
                 IF ( IXREF(IPARM) .GT. 0 ) THEN
                      IND=INDX8 ( IXREF(I), IXREF(I) )
                      CNS_EQU(I,ICON) = CNS_EQU(I,ICON)*NOR_MAT(IND)
                 END IF
              END DO
         ENDIF
!
         IF ( KMATRIX_NNTV .EQ. 'AL'  .AND.  KSIG_SCALE_NNTV .NE. 'WG' ) THEN
!
! ----------- Simple case: "ALL components, not weighted"
!
              IF ( FL_OBSOLETE ) THEN
                   CALL NORMALIZE_VECTOR ( CNS_EQU(1,ICON), NELEM )
              END IF
            ELSE
!
! ----------- Normalize constraint vectors for all stations to 1.
!
              CALL NORMALIZE_VECTOR ( CNS_EQU(1,ICON), NELEM )
          END IF
      END DO
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) 'nnt_vel: rtp(1:3):  ', nnt_vel_rtp(1:3)      ! %%%%
!   write ( 6, * ) 'nnt_vel:    sigma:  ', nnt_vel_sigma         ! %%%%
!   write ( 6, * ) 'nnt_vel: equ_cns(1) ', cns_equ(1:3*n_vel,1)  ! %%%%
!   write ( 6, * ) 'nnt_vel: equ_cns(2) ', cns_equ(1:3*n_vel,2)  ! %%%%
!   write ( 6, * ) 'nnt_vel: equ_cns(3) ', cns_equ(1:3*n_vel,3)  ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IF ( N .GT. 0 ) THEN
           DO ICON=1,3
!
! ----------- Add information about the type of the constraint applied
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'NNT_VEL', INT4(ICON), 'No-net translation '// &
     &            'for velocity', 'meter/year', NNT_VEL_RTP(ICON), &
     &             NNT_VEL_SIGMA, TRUE_L4, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8543, IUER, 'NNT_VEL', 'Failure '// &
     &                 'to add description of constraint to an internal '// &
     &                 'data structure' )
                   RETURN
              END IF
!
! ----------- Put the non-zero elements of equations of constraints in CNSTROBJ
!
              DO IPARM=1,NELEM
                 IF ( IXREF(IPARM) .GT. 0 ) THEN
                      IF ( DABS(CNS_EQU(IPARM,ICON)) .GT. EPS ) THEN
!
                           CALL ERR_PASS ( IUER, IER )
                           CALL ADDCNS_EQU ( 'NNT_VEL', INT4(ICON), &
     &                          IXREF(IPARM), CNS_EQU(IPARM,ICON), &
     &                          TRUE_L4, CNSTROBJ, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8544, IUER, 'NNT_VEL', &
     &                              'Failure to write a NNT_VEL equation of '// &
     &                              'constraints' )
                                RETURN
                           END IF
                      END IF
                 END IF
              END DO
           END DO
      END IF
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' nnt_vel       n_cnstr = ',cnstrobj%n_ecnst
      END IF
!
      IF ( SPOOL_L1 ) THEN
!
! -------- Writing the list of stations in spool-file
!
           WRITE ( 23, '(A,I4,A)' ) 'NNT_VEL: ', N_VEL, ' stations '// &
     &                              'participated in NNT_VEL constraints: '
           IF ( N_VEL .GT. 0 ) THEN
!
! -------------- Sorting the list of stations
!
                 CALL SORT_CH ( INT4(N_VEL), ARR_NAM )
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
                    OUT = OUT(1:IP)//ARR_NAM(I)
                 END DO
!
! -------------- Writing long lines in spool-file by splitting it onto
! -------------- sublines no nore than 72 characters long
!
                 CALL WRITE_LONG ( 23, 72, OUT(2:) )
           END IF
           WRITE ( 23, 220 ) NNT_VEL_SIGMA, NNT_VEL_RTP
 220       FORMAT ( 'NNT_VEL sigma: ', 1PD13.6, ' right hand sides: ', 3(1PD13.6,1X), ' m' ) 
           WRITE ( 23, '(A)' ) ' '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NNT_VEL  !#!#
