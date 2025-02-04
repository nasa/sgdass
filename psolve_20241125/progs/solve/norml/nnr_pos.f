      SUBROUTINE NNR_POS ( FAST_MODE, FAST_DBG, SPOOL_L1, FGLOBAL_L4, LPARM, &
     &                     NPARM, NOR_MAT, NNR_POS_RTP, NNR_POS_SIGMA, B3DOBJ, &
     &                     CNSTROBJ, IUER )
!CCCCC
!
!  This subroutine constrains a set of positions
!  to minimize the distance to the a priori model.
!
!  This is done by applying a rotation.
!  NNR stands for no net rotation.
!
!  Modifications
!
!  jmg 9611??   created in test mode (by overwriting paths used for other
!               batch control features)
!  kdb 961125   officially install into solve, connecting to new batch control
!               keywords
!  kdb,970312   Implement the ability to use weighted or uniform constraints via
!  jmg          the batopt file.
!               (Algorithm from jmg, batopt connection from kdb).
!  pet  971202  Added logic for bypassing deselected station
!  pet  980206  Declared the sigma of the constrain in solve.i instead of
!               hard-coded value. Write down information about the type
!               of applied constraint.
!  pet  980401  Corrected a bug: array of indices IXREF was not initialized;
!               procedure was trying to impose constraints even if no station
!               positions were estimated
!  pet 980721   Added printing the list of stations participated in constraints
!               to the spool-file when SPOOL_L1 is .TRUE.
!  pet 980721   Fixed NET_ESM-JUL98 bug: Previous version actually didn't
!               impose constraints on stations with episodic motions
!  pet 980722   Made NNR_POS_SIGMA formal parameter instead of named
!               constant in solve.i block.
!  pet 980727   Added sorting in alphabetic order the stations list to be
!               printed
!  pet 980730   Rewrote once more to support the case when at least one station
!               had episodic motion and therefore should be constrained more
!               than once
!  pet 981227   Added constraints to CNSTROBJ, but not to the normal matrix
!               directly. Changed equations of constraints to take into account
!               terms of order Earth flattening.
!  pet 2002.03.19  Changes in logic which allows to use nnr_pos for
!                  setting constraints on local parameters
!  pet 2002.05.17  Improved comments
!  pet 2002.05.29  Fixed a bug: the previous version incorrectly recognized
!                  paramater name of the station position with episodic motion.
!                  The previous logic worked in global mode, but failed in
!                  local mode.
!  pet 2007.05.11  Corrected logic in accordance to changfes in GCONST
!  pet 2021.10.22  Fixed logic for dealing with station exception list. &
!                  No the excpetion station list is exclusive for this constraint.
!
!CCCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE     'solve.i'
      INTEGER*4    NPARM
!     Updated to specificaly type integers which
!-------------------------------------------------
      CHARACTER*20 LPARM(*)
      REAL*8       NNR_POS_RTP(3), NNR_POS_SIGMA, NOR_MAT(*)
      INTEGER*4    IUER
!
      INCLUDE      'socom.i'
      INCLUDE      'prfil.i'
      INCLUDE      'glbcm.i'
      INCLUDE      'glbc3.i'
      INCLUDE      'fast.i'
      INCLUDE      'cnstr.i'
!
      LOGICAL*1    SPOOL_L1
      INTEGER*4    MAX_STA_PAR
      PARAMETER  ( MAX_STA_PAR=3*MAX_STA )
      INTEGER*4  IXREF(MAX_STA_PAR)
      INTEGER*2  I, J, N, K, N1, N_ELM, ICON, ISTA
      INTEGER*4  IP, IPARM, L_STA, LIS_STA(MAX_STA_PAR), ITST, IER
      LOGICAL*2  KBIT, KINCL
      CHARACTER  C_STA(MAX_STA)*8, OUT*4096, STR*20, STR1*20
      REAL*8     EPS, XYZ(3,MAX_STA), CNS_EQU(MAX_STA_PAR,3), VAL
      PARAMETER  ( EPS = 1.D-32 ) ! A very small number
!
!---- rot_con constrains rotations
!
      REAL*8     ROT_CON(3,3,MAX_STA)
      REAL*8     DENOM(MAX_STA), RLEN_SQ, REA
!
      INTEGER*4  J1, IYEAR_I4, IMONTH_I4, IDAY_I4
      INTEGER*4  FAST_MODE, FAST_DBG
      LOGICAL*4  FGLOBAL_L4, SUB_TYPE_OK
      LOGICAL*4  CHECK_STABIT
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      ADDRESS__TYPE :: IAD
      PARAMETER  ( REA = 6378136.D0 ) ! Earth radius (in m)
      INTEGER*8   IND
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
      INTEGER*8,     EXTERNAL :: INDX8
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D
      ITST = 0
!
! --- Check whether we should try to apply constraints at all
!
      IF ( FGLOBAL_L4 .AND. NNR_POS_GLO ) THEN
           CONTINUE
         ELSE IF ( .NOT. FGLOBAL_L4 .AND. NNR_POS_LOC ) THEN
           CONTINUE
         ELSE IF ( .NOT. FGLOBAL_L4 .AND. &
     &             ( NNR_POS_GLO .AND. ISLTY2 .EQ. 'I' ) ) THEN
!
! -------- Comment: in indepdent mode global = local
!
           CONTINUE
         ELSE
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Initialization of the array of indices IXREF and equation matrix CNS_EQU
!
      CALL NOUT_I4 (    MAX_STA_PAR, IXREF   )
      CALL NOUT_R8 (  3*MAX_STA_PAR, CNS_EQU )
      L_STA = 0
      N = 0
!
! --- Set up index to relevant parameter numbers in nrmfil.
!
      DO I=1,NUMSTA ! Scan all stations. NB: stations with episodic motions
!                   ! enters the array ISITN_CHR more than once
!
! ------ Firstly set flag of eligibility for participation in constraint
! ------ in according with global flag.
!
         KINCL = KBIT ( DEFCMP, INT2(6) )
!
! ------ Then look the list of the stations to be included or barred from
! ------ participation in constraint
!
         IF ( IND_NNR_POS(1) > 0  .AND. IND_NNR_POS(2) .GE. IND_NNR_POS(1) ) THEN
              DO J=IND_NNR_POS(1),IND_NNR_POS(2)
                  IF ( STASUP(J) .EQ. ISITN_CHR(I) ) THEN
!
! -------------------- Yes, the I-th station is in the list. 
!
                       IF ( KBIT ( CMPSUP(1,6), J ) ) THEN
                            KINCL = .NOT. KINCL
                       END IF 
                       GOTO 810
                  ENDIF
              ENDDO
  810         CONTINUE
         END IF
!
! ------ Test: are all 3 components of the station positions estimated?
!
         IF ( .NOT. KBIT(LSITEC(1,1),I)  .AND. &
     &        .NOT. KBIT(LSITEC(1,2),I)  .AND. &
     &        .NOT. KBIT(LSITEC(1,3),I)         ) KINCL = .FALSE.
!
! ------ If station participated and eligibile for constraint go further
!
         IF ( CHECK_STABIT(I) .AND. KINCL ) THEN
            DO J1=1,NPARM ! Scan all parameters
               IF ( LPARM(J1)(1:8) .EQ. ISITN_CHR(I) ) THEN
!
! ----------------- This parameter is related to the station
! ----------------- Oh! It is a tricky place! If the station had episodic
! ----------------- motion it is in the list of station more than once.
! ----------------- And parameter name before and after episodic motion
! ----------------- is different
!
                    SUB_TYPE_OK = .FALSE.
                    IF ( LPARM(J1)(12:20) .EQ. 'COMPONENT'  .AND. &
     &                   DABS ( VSITED(I) ) .LT. EPS              ) THEN
!
! ---------------------- It is a component without episodic motion
!
                         SUB_TYPE_OK = .TRUE.
                       ELSE IF ( ( LPARM(J1)(10:10) .EQ. 'X' .OR. &
     &                             LPARM(J1)(10:10) .EQ. 'Y' .OR. &
     &                             LPARM(J1)(10:10) .EQ. 'Z'      ) .AND. &
     &                             DABS ( VSITED(I) ) .GT. EPS           ) THEN
!
! ---------------------- We check byts 11-16. If they contain a date then
! ---------------------- it means that this parameter is a component of the
! ---------------------- station position when a station had episodic motion.
!
                         CALL CHIN ( LPARM(J1)(11:12), IYEAR_I4  )
                         CALL CHIN ( LPARM(J1)(13:14), IMONTH_I4 )
                         CALL CHIN ( LPARM(J1)(15:16), IDAY_I4   )
!
                         IF ( IYEAR_I4  .GE. 0  .AND. &
     &                        IMONTH_I4 .GE. 1  .AND.  IMONTH_I4 .LE. 12 .AND. &
     &                        IDAY_I4   .GE. 1  .AND.  IDAY_I4   .LE. 31  ) THEN
!
                              SUB_TYPE_OK = .TRUE.
                         END IF
                    END IF ! lparm
!
                    IF ( SUB_TYPE_OK ) THEN
!
! ---------------------- OK. It was component of station position
!
                         N = N+1
                         IXREF(N) = J1
!
                         IF ( LPARM(J1)(10:10) .EQ. 'X' ) THEN
!
! --------------------------- New station. It is assumed that the order of
! --------------------------- components of station position is strict: X, Y, Z
! --------------------------- NB: it may be "old" station but after episodic
! --------------------------- motion, nevertheless we treat as if it is
! --------------------------- the new station
!
                              L_STA = L_STA + 1
                              LIS_STA(L_STA) = INT4(I)
                              C_STA(L_STA)   = ISITN_CHR(I)
                         END IF ! 'X'
                    ENDIF ! subtype
               ENDIF ! lparm
            ENDDO ! nparam
         END IF ! kincl
      ENDDO ! numsta
!
      N_ELM = 3*INT2(L_STA)
!
      IF ( N_ELM .EQ. 0 ) THEN
!
! -------- Nothing to do. Go home.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- It wouldn't be amiss to check consistency of the array m. They were
! --- cases in the past when it contained zero elements...
!
      DO J=1,N_ELM
         IF ( IXREF(J) .LE. 0  .OR.  IXREF(J) .GT. NPARM ) THEN
              WRITE ( 6, * ) ' n_elm= ',n_elm
              WRITE ( 6, * ) ' ixref = ',(ixref(i),i=1,n_elm)
              WRITE ( 6, * ) ' lis_sta = ',( lis_sta(i), i=1,l_sta )
!
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(J), STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IXREF(J), STR1 )
!
              CALL ERR_LOG ( 8491, IUER, 'NNR_POS', 'Trap of '// &
     &            'internal control: array "ixref" of indices of the '// &
     &            'parameters to be constrainted contains a bad '// &
     &             STR(1:I_LEN(STR))//'-th element: '//STR1(1:I_LEN(STR1)) )
              RETURN
         END IF
      END DO
!
! --- Determine XYZ is a unit vector of station coordinates
!
      DO I=1,INT2(L_STA)
         N = INT2(LIS_STA(I))
         DENOM(I) = DSQRT ( VSITEC(1,N)**2 + VSITEC(2,N)**2 + VSITEC(3,N)**2)
         DO J=1,3
            XYZ(J,I) = VSITEC(J,N)/DENOM(I)
         ENDDO
      ENDDO
!
! --- Now set up rotation part.
!
      DO I=1,INT2(L_STA)
         DO J=1,3
            ROT_CON(J,J,I)=0.0D0
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
! --- Convert projection operator matrix to six vectors
! --- Each parameter (e.g, Haystack X position) will have 3 applicable values.
!
      DO I=1,INT2(L_STA)  ! Run over sites to be constrained
         DO J=1,3
            N1 = (I-1)*3+J
            DO K=1,3
               CNS_EQU(N1,K) = ROT_CON(J,K,I)
            ENDDO
         ENDDO
      ENDDO
!
! --- If we want to scale by sigmas, do so.
!
      IF ( KSIG_SCALE_NNRP .EQ. 'WG' ) THEN
!
! -------- Not uniform constraints
!
           DO ICON=1,3
              DO I=1,N_ELM
                 IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------------- A bit akward way to get the value (IXREF(I), IXREF(I))
! ------------------- in global-global block of normal matrix
!
                      IAD = FULL_B3D ( B3DOBJ, IXREF(I), IXREF(I), &
     &                                 %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
                      CALL COPY_V ( 1, %VAL(IAD), VAL )
                      CNS_EQU(I,ICON) = CNS_EQU(I,ICON)*NOR_MAT(IAD)
                    ELSE
                      IND = INDX8(IXREF(I),IXREF(I))
                      CNS_EQU(I,ICON)=CNS_EQU(I,ICON)*NOR_MAT(IND)
                 END IF
              END DO
!
! ----------- Normalize constraint vectors to 1.
!
              CALL NORMALIZE_VECTOR ( CNS_EQU(1,ICON), N_ELM )
           END DO
         ELSE IF ( KSIG_SCALE_NNRP .EQ. 'UN' ) THEN
!
! -------- Uniform constraints
!
           CALL NOUT_R8 (  3*MAX_STA_PAR, CNS_EQU )
           DO I=1,INT2(L_STA)  ! Run over sites to be constrained
              ISTA = INT2(LIS_STA(I))
              RLEN_SQ = VSITEC(1,ISTA)**2 +VSITEC(2,ISTA)**2 +VSITEC(3,ISTA)**2
!@!
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
           CALL ERR_LOG ( 8492, IUER, 'NNR_POS', 'Trap of '// &
     &         'internal control: variable KSIG_SCALE_NNRP = '//KSIG_SCALE_NNRP )
           RETURN
      ENDIF
! 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) 'nnr_pos: rtp(1:3):  ', nnr_pos_rtp(1:3)      ! %%%%
!   write ( 6, * ) 'nnr_pos:    sigma:  ', nnr_pos_sigma         ! %%%%
!   write ( 6, * ) 'nnr_pos: equ_cns(1) ', cns_equ(1:3*l_sta,1)  ! %%%%
!   write ( 6, * ) 'nnr_pos: equ_cns(2) ', cns_equ(1:3*l_sta,2)  ! %%%%
!   write ( 6, * ) 'nnr_pos: equ_cns(3) ', cns_equ(1:3*l_sta,3)  ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      DO ICON=1,3
         IF ( ITST > 0 ) THEN
              WRITE ( 6, 210 ) ICON, NNR_POS_RTP(ICON)
 210          FORMAT ( 'NNR_POS: Icon: ', I1, ' NNR_POS_RTP CNS: ', 1PD12.5 )
         END IF
!
! ------ Add constraints
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'NNR_POS', INT4(ICON), 'No-net rotation for '// &
     &       'positions', 'meter', NNR_POS_RTP(ICON), NNR_POS_SIGMA, &
     &        FGLOBAL_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8493, IUER, 'NNT_POS', 'Failure '// &
     &            'to add description of constraint to an internal data '// &
     &            'structure' )
              RETURN
         END IF
!
! ------ Put the non-zero elements of equations of constraints into CNSTROBJ
!
         DO IPARM=1,N_ELM
            IF ( IXREF(IPARM) .GT. 0 ) THEN
                 IF ( DABS(CNS_EQU(IPARM,ICON)) .GT. EPS ) THEN
                      IF ( ITST > 0 ) THEN
                           WRITE ( 6, 220 ) ICON, IPARM, IXREF(IPARM), CNS_EQU(IPARM,ICON)
 220                       FORMAT ( 'NNR_POS: Icon: ', I1, ' Elm: ', I4, ' IND_PAR: ', I6, ' CNS: ', 1PD12.5 )
                      END IF
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'NNR_POS', INT4(ICON), &
     &                     IXREF(IPARM), CNS_EQU(IPARM,ICON), FGLOBAL_L4, &
     &                     CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8494, IUER, 'NNR_POS', &
     &                         'Failure to write a NNR_POS equation of '// &
     &                         'constraints' )
                           RETURN
                      END IF
                 END IF
            END IF
         END DO
      ENDDO
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' nnr_pos       fast_mode = ',fast_mode,' n_cnstr = ', &
     &                            cnstrobj%n_ecnst
      END IF
!
      IF ( SPOOL_L1 ) THEN
!
! -------- Writing the list of stations in spool-file
!
           WRITE ( 23, '(A,I4,A)' ) 'NNR_POS: ', L_STA, ' stations '// &
     &                              'participated in NNR_POS constraints: '
           IF ( L_STA .GT. 0 ) THEN
!
! -------------- Sorting the list of stations
!
                 CALL SORT_CH ( L_STA, C_STA )
!
! -------------- Cleaning the output line with the list of stations
!
                 CALL CLRCH ( OUT )
                 DO I=1,L_STA
!
! ----------------- Add the I-th station to the list of stations for further
! ----------------- printing
!
                    IP  = (I-1)*9 + 1
                    OUT = OUT(1:IP)//C_STA(I)
                 END DO
!
! -------------- Writing long lines in spool-file by splitting it onto
! -------------- sub-lines no nore than 72 characters long
!
                 CALL WRITE_LONG ( 23, 72, OUT(2:) )
           END IF
           WRITE ( 23, 230 ) NNR_POS_SIGMA, NNR_POS_RTP
 230       FORMAT ( 'NNR_POS sigma: ', 1PD13.6, ' right hand sides: ', 3(1PD13.6,1X), ' m' ) 
           WRITE ( 23, '(A)' ) ' '
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NNR_POS  !#!#
