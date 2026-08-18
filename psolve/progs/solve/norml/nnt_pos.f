      SUBROUTINE NNT_POS ( FAST_MODE, FAST_DBG, SPOOL_L1, FGLOBAL_L4, LPARM, &
     &                     NPARM, NOR_MAT, NNT_POS_RTP, NNT_POS_SIGMA, B3DOBJ, &
     &                     CNSTROBJ, IUER )
!CCCCC
!
!  This subroutine imposes contraints on the set of stations which makes the
!  net translation of this set ofstations with respect to apriori postions
!  zero.
!
!  This is done by applying a translation.
!  NNT stands for no net translation.
!
!  jmg 9611?? created in test mode (by overwriting paths used for other
!             batch control features)
!  kdb 961125 officially install into solve, connecting to new batch control
!             keywords
!  kdb,970312 Implement the ability to use weighted or uniform constraints via
!   jmg         the batopt file.
!             Also implement in batopt the ability to use the horizontal
!               projection matrix vs. just the identity matrix.
!             (Algorithm from jmg, batopt connection from kdb).
!  pet 970927 Support of B3D-mode added
!  pet 971202 Added logic for bypassing deselected station
!  pet 980119 Rewrote for support of CNSTROBJ data structure
!  pet 980205 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980206 Corrected a bug: previous version didn't check whether the
!             stations were in solution
!  pet 980401 Corrected a bug: array of indices IXREF was not initialized;
!             procedure was trying to impose constraints even if no station
!             positions were estimated
!  pet 980721 Added printing the list of stations participated in constraints
!             to the spool-file when SPOOL_L1 is .TRUE.
!  pet 980722 Made NNT_POS_SIGMA formal parameter instead of named
!             constant in solve.i block.
!  pet 980727 Added sorting in alphabetic order the stations list to be
!             printed
!  pet 980730 Rewrote once more to support the case when at least one station
!             had eposidic motion and therefore should be constrainted more
!             than once
!  pet 981227 Prohibited normalizing vecotr of constraints in the case when
!             all components are constrainted without weights.
!  pet 2002.03.19  Changes in logic which allows to use nnt_pos for
!                  setting constraints on local parameters
!  pet 2002.05.17  Improved comments. Fixed the bug: the old version normalized
!                  only constraints only for the first station
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
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'fast.i'
      INCLUDE 'cnstr.i'
!
      LOGICAL*1    SPOOL_L1
      INTEGER*4    NPARM
      CHARACTER    LPARM(*)*20
      REAL*8       NNT_POS_RTP(3), NNT_POS_SIGMA, NOR_MAT(*)
      LOGICAL*4    FGLOBAL_L4, SUB_TYPE_OK
      INTEGER*4    IUER
!
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
!
      INTEGER*2 MAX_STA_PAR
      PARAMETER (MAX_STA_PAR=3*MAX_STA)
!
      INTEGER*4  J1, IYEAR_I4, IMONTH_I4, IDAY_I4
      INTEGER*4  FAST_MODE, FAST_DBG
      INTEGER*2  I, J, N, K, N1, N_ELM, ICON
      INTEGER*4  IP, IPARM, L_STA, LIS_STA(MAX_STA_PAR), IXREF(MAX_STA_PAR), &
     &           ITST, IER
      REAL*8     EPS, VAL, CNS_EQU(MAX_STA_PAR,3)
      PARAMETER  ( EPS = 1.D-32 ) ! A very small number
      CHARACTER  C_STA(MAX_STA)*8, OUT*4096, STR*20, STR1*20
      LOGICAL*2  KBIT, KINCL
      LOGICAL*2 CHECK_STABIT
      ADDRESS__TYPE :: IAD
      INTEGER*8   IND
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  ILEN, I_LEN
      INTEGER*8,     EXTERNAL :: INDX8
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D
!
! --- TRAN_CON constrains translations
! --- ROT_CON constrains rotations
!
      REAL*8     TRAN_CON(3,3,MAX_STA), IDMAT(3,3), DENOM
      DATA   IDMAT / 1.D0, 3*0.D0, 1.D0, 3*0.D0, 1.D0 /
      ITST = 0
!
! --- Check whether we should try to apply constraints at all
!
      IF ( FGLOBAL_L4 .AND. NNT_POS_GLO ) THEN
           CONTINUE
         ELSE IF ( .NOT. FGLOBAL_L4 .AND. NNT_POS_LOC ) THEN
           CONTINUE
         ELSE IF ( .NOT. FGLOBAL_L4 .AND. &
     &             ( NNT_POS_GLO .AND. ISLTY2 .EQ. 'I' ) ) THEN
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
! ------ Firstly set flag of eligibility for participation in cosntraint
! ------ in according with global flag.
!
         KINCL = KBIT( DEFCMP, INT2(5))
!
! ------ Then look the list of the stations to be included or barred from
! ------ participation in constraint
!
         IF ( IND_NNT_POS(1) > 0  .AND. IND_NNT_POS(2) .GE. IND_NNT_POS(1) ) THEN
              DO J=IND_NNT_POS(1),IND_NNT_POS(2)
                 IF ( STASUP(J) .EQ. ISITN_CHR(I) ) THEN
!
! ------------------- Yes, the I-th station is in the list. 
!
                      IF ( KBIT ( CMPSUP(1,5), J ) ) THEN
                           KINCL = .NOT. KINCL
                      END IF 
                      GOTO 810
                 ENDIF
              ENDDO
 810          CONTINUE
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
!
            DO J1=1,NPARM ! Scan all parameters
               IF ( LPARM(J1)(1:8) .EQ. ISITN_CHR(I) ) THEN
!
! --------------- This parameter is related to the station.
! --------------- Oh! It is a tricky place! If the station had episodic motion
! --------------- it enters the list of stations more than once. And parameter
! --------------- name before and after episodic motion is different
!
                  SUB_TYPE_OK = .FALSE.
                  IF ( LPARM(J1)(12:20) .EQ. 'COMPONENT'  .AND. &
     &                 DABS ( VSITED(I) ) .LT. EPS             ) THEN
!
!
! -------------------- It is a component without episodic motion or BEFORE
! -------------------- episodic motion
!
                       SUB_TYPE_OK = .TRUE.
                     ELSE IF ( ( LPARM(J1)(10:10) .EQ. 'X' .OR. &
     &                           LPARM(J1)(10:10) .EQ. 'Y' .OR. &
     &                           LPARM(J1)(10:10) .EQ. 'Z'      ) .AND. &
     &                           DABS ( VSITED(I) ) .GT. EPS           ) THEN
!
! -------------------- We check bytes 11-16. If they contain a date then
! -------------------- it means that this parameter is a component of the
! -------------------- station position when a station had episodic motion and
! -------------------- it is a parameter: station position AFTER episodic
! -------------------- motion
!
                       CALL CHIN ( LPARM(J1)(11:12), IYEAR_I4  )
                       CALL CHIN ( LPARM(J1)(13:14), IMONTH_I4 )
                       CALL CHIN ( LPARM(J1)(15:16), IDAY_I4   )
!
                       IF ( IYEAR_I4  .GE. 0  .AND. &
     &                      IMONTH_I4 .GE. 1  .AND.  IMONTH_I4 .LE. 12 .AND. &
     &                      IDAY_I4   .GE. 1  .AND.  IDAY_I4   .LE. 31  ) THEN
!
                            SUB_TYPE_OK = .TRUE.
                       END IF
                  END IF
!
                  IF ( SUB_TYPE_OK ) THEN
!
! -------------------- OK. It was component of station position
!
                       N = N+1
                       IXREF(N) = J1
!
                       IF ( LPARM(J1)(10:10) .EQ. 'X' ) THEN
!
! ------------------------- New station. It is assumed that the order of
! ------------------------- components of station position is strict: X, Y, Z
! ------------------------- NB: it may be "old" station but after episodic
! ------------------------- motion, nevertheless we treat it as if it is
! ------------------------- the new station
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
              CALL ERR_LOG ( 8481, IUER, 'NNT_POS', 'Trap of '// &
     &            'internal control: array "ixref" of indices of the '// &
     &            'parameters to be constraints contains a bad '// &
     &             STR(1:I_LEN(STR))//'-th element: '//STR1(1:I_LEN(STR1)) )
              RETURN
         END IF
      END DO
!
! --- Make a 3x3 matrix for each site.
! --- 1.) If kmatrix_nntp is HR, then this is the horizontal projection matrix:
!         PH=I-|up><up|
! ---     We try to minimize the horizontal projections.
! --- 2.) if kmatrix_nntp is AL, then just the identity matrix.
!
      IF ( KMATRIX_NNTP .EQ. 'HR' ) THEN
!
! -------- Only horizontal projection will be constrainted
!
           DO I=1,INT2(L_STA)
              N = INT2(LIS_STA(I))
              DENOM = (VSITEC(1,N)**2+VSITEC(2,N)**2+VSITEC(3,N)**2)
!
              DO J=1,3
                 DO K=1,3
                    TRAN_CON(J,K,I) = IDMAT(J,K) - VSITEC(J,N)*VSITEC(K,N)/DENOM
                 ENDDO
              ENDDO
           ENDDO
         ELSE IF ( KMATRIX_NNTP .EQ. 'AL' ) THEN
!
! -------- All components will be constrainted
!
           DO I=1,INT2(L_STA)
              DO J=1,3
                 DO K=1,3
                    TRAN_CON(J,K,I)=IDMAT(J,K)
                 ENDDO
              ENDDO
           ENDDO
         ELSE
           CALL ERR_LOG ( 8482, IUER, 'NNT_POS', 'Trap of '// &
     &         'internal control: variable KMATRIX_NNTP = '//KMATRIX_NNTP )
           RETURN
      ENDIF
!
! --- Convert projection operator matrix to three vectors
! --- Each parameter (e.g, Haystack X position) will have 6 applicable values.
!
      DO I=1,INT2(L_STA)  ! Run over sites to be constrained
         DO J=1,3
            N1 = (I-1)*3+J
            DO K=1,3
               CNS_EQU(N1,K) = TRAN_CON(J,K,I)
            ENDDO
         ENDDO
      ENDDO
!
! --- If we want to weight by aproximate sigmas, do so.
!
      IF ( KSIG_SCALE_NNTP .EQ. 'WG' ) THEN
           DO ICON=1,3
              DO I=1,INT2(L_STA)*3
                 IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------------- A bit akward way to get the value (IXREF(I), IXREF(I))
! ------------------- in global-global block of normal matrix
!
                      IAD = FULL_B3D ( B3DOBJ, IXREF(I), IXREF(I), &
     &                                 %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
                      CALL COPY_V ( 1, %VAL(IAD), VAL )
                      CNS_EQU(I,ICON) = CNS_EQU(I,ICON)*VAL
                    ELSE
                      IND=INDX8(IXREF(I),IXREF(I))
                      CNS_EQU(I,ICON) = CNS_EQU(I,ICON)*NOR_MAT(IND)
                 END IF
              END DO
           END DO
      ENDIF
!
      DO ICON=1,3
         IF ( KMATRIX_NNTP .EQ. 'AL' .AND. KSIG_SCALE_NNTP .NE. 'WG' ) THEN
!
! ----------- Simple case: "ALL components, not weighted"
!
              CONTINUE
            ELSE
!
! ----------- Normalize constraint vectors for all stations
!
              CALL NORMALIZE_VECTOR ( CNS_EQU(1,ICON), N_ELM ) ! correct
         END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) 'nnt_pos: rtp(1:3):  ', nnt_pos_rtp(1:3)                 ! %%%%
!   write ( 6, * ) 'nnt_pos:    sigma:  ', nnt_pos_sigma                    ! %%%%
!   write ( 6, * ) 'nnt_pos: equ_cns(',icon,' ) ', cns_equ(1:3*l_sta,icon)  ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Apply constraint
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'NNT_POS', INT4(ICON), 'No-net translation for '// &
     &       'positions', 'meter', NNT_POS_RTP(ICON), NNT_POS_SIGMA, &
     &        FGLOBAL_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8483, IUER, 'NNT_POS', 'Failure '// &
     &            'to add description of constraint to an internal data '// &
     &            'structure' )
              RETURN
         END IF
!
! ------ Put the non-zero elements of constraint equations into CNSTROBJ
!
         DO IPARM=1,N_ELM
            IF ( IXREF(IPARM) .GT. 0 ) THEN
                 IF ( DABS(CNS_EQU(IPARM,ICON)) .GT. EPS ) THEN
                      IF ( ITST > 0 ) THEN
                           WRITE ( 6, 210 ) ICON, IPARM, IXREF(IPARM), CNS_EQU(IPARM,ICON)
 210                       FORMAT ( 'NNT_POS: Icon: ', I1, ' Elm: ', I4, ' IND_PAR: ', I6, ' CNS: ', 1PD12.5 )
                      END IF
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'NNT_POS', INT4(ICON), &
     &                     IXREF(IPARM), CNS_EQU(IPARM,ICON), FGLOBAL_L4, &
     &                     CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8484, IUER, 'NNT_POS', &
     &                         'Failure to write a NNT_POS equation of '// &
     &                         'constraints' )
                           RETURN
                      END IF
                  END IF
            END IF
         END DO
      END DO
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' nnt_pos       fast_mode = ',fast_mode,' n_cnstr = ', &
     &                            cnstrobj%n_ecnst
      END IF
      IF ( SPOOL_L1 ) THEN
!
! -------- Writing the list of stations in spool-file
!
           WRITE ( 23, '(A,I4,A)' ) 'NNT_POS: ', L_STA, ' stations '// &
     &                              'participated in NNT_POS constraints: '
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
! -------------- sublines no nore than 72 characters long
!
                 CALL WRITE_LONG ( 23, 72, OUT(2:) )
           END IF
           WRITE ( 23, 220 ) NNT_POS_SIGMA, NNT_POS_RTP
 220       FORMAT ( 'NNT_POS sigma: ', 1PD13.6, ' right hand sides: ', 3(1PD13.6,1X), ' m' ) 
           WRITE ( 23, '(A)' ) ' '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NNT_POS  !#!#
