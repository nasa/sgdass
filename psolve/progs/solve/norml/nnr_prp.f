      SUBROUTINE NNR_PRP ( FAST_MODE, FAST_DBG, SPOOL_L1, LPARM, NPARM, &
     &                     NOR_MAT, NNR_PRP_SIGMA, NNR_PRP_RTP, B3DOBJ, &
     &                     CNSTROBJ, IUER )
!
!  This subroutine constrains a net rotation of the source proper motion
!
!  Who When       What
!
!  pet 2003.09.02 Wrote the first version in a similar way as NNR_SRC
!  pet 2007.05.11  Corrected logic in accordance to changfes in GCONST
!
!CC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
!
      LOGICAL*1    SPOOL_L1
      INTEGER*4    FAST_MODE, FAST_DBG
      INTEGER*2    NPARM
      CHARACTER*20 LPARM(*)
      REAL*8       NNR_PRP_SIGMA, NNR_PRP_RTP(3), NOR_MAT(*)
      LOGICAL*4    TRUE_L4
      PARAMETER  ( TRUE_L4 = .TRUE. )
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4   IUER
!
      INTEGER*4   I, J, NC(3), IXREF_PAR(M_GPA,3), IXREF_SRC(MAX_SRC)
!
      REAL*8      CVEC(M_GPA,3), WT
      INTEGER*4   NUM_CON_STAR, IPARM, ISTAR, ICON, NS
      CHARACTER*8 LSTAR_NAME(MAX_SRC)
      REAL*8      RA, DEC, VAL, EPS
      PARAMETER  ( EPS = 1.D-32 )
      INTEGER*4   IP, NSRC, IER
      INTEGER*8   IND
!
! note: numstr=number of stars in source list.
!       num_con_star = number of stars to constrain
!
      LOGICAL*2   KINCL, KBIT, KBIT4
      INTEGER*4   ML_OUT
      PARAMETER ( ML_OUT = (MAX_SRC+1)*9 )
!
      ADDRESS__TYPE :: IAD
      CHARACTER   ARR_NAM(MAX_SRC)*8, OUT*(ML_OUT)
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*8,     EXTERNAL :: INDX8
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D
!
      IF ( IND_NNR_PRP(2) .LT. IND_NNR_PRP(1) .OR. &
     &     IND_NNR_PRP(2) .EQ. 0                   ) THEN
!
! -------- It means that no-net-rotation constraints on proper motion
! -------- have not been imposed.  Nothing to do!
!
           CALL ERR_LOG ( 0, IUER ) 
           RETURN 
      END IF 
!
      IF ( ISLTY2 .EQ. 'I' ) THEN
           CALL ERR_LOG ( 8681, IUER, 'NNR_PRP', 'No-net-rotation '// &
     &         'constraint on proper motion cannot be imposed in '// &
     &         'INDEPENDENT type of solutions' )
           RETURN 
      END IF
!
      NUM_CON_STAR = 0
      NSRC = 0
      DO I=1,MAX_SRC
         IXREF_SRC(I) = 0
         LSTAR_NAME(I) = '        '
      ENDDO
!
! --- Build a list of the sources to be constrained
!
      DO ISTAR=1,NUMSTR
         KINCL = KBIT( DEFSRC, INT2(8) )
         IF ( IND_NNR_PRP(1) > 0 .AND. IND_NNR_PRP(2) .GE. IND_NNR_PRP(1) ) THEN
              DO J=IND_NNR_PRP(1), IND_NNR_PRP(2) 
!
! -------------- Scan suppression list
!
                 IF ( SRCSUP(J) .EQ. ISTRN_CHR(ISTAR) ) THEN
!
! ------------------- The source fell in suppression list for sources
!
                      IF ( KBIT4 ( SOUSUP(1,8), J ) ) THEN
                           KINCL = .NOT. KINCL
                      END IF 
                 ENDIF
              END DO
         END IF
!
! ------ Test: is proper motion for right ascension or declination of 
! ------ the ISTAR-th source estimated?
!
         IF ( .NOT. KBIT4(LPROP(1,1),ISTAR) .AND. &
     &        .NOT. KBIT4(LPROP(1,2),ISTAR)       ) KINCL = .FALSE.
         IF ( KINCL ) THEN
              NUM_CON_STAR = NUM_CON_STAR + 1
              LSTAR_NAME(NUM_CON_STAR) = ISTRN_CHR(ISTAR)
              IXREF_SRC(NUM_CON_STAR)  = ISTAR
         ENDIF
      ENDDO
!
      IF ( NUM_CON_STAR .EQ. 0 ) THEN
!
! -------- No source proper motions were estimated: good bye!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Initialize constraint vectors to 0.
!
      DO ICON=1,3
         DO IPARM=1,NPARM
            CVEC(IPARM,ICON)      = 0.0D0
            IXREF_PAR(IPARM,ICON) = 0
         END DO
      END DO
!
! --- Now find three constraint vectors.
!
      NC(1) = 0
      NC(2) = 0
      NC(3) = 0
      DO IPARM=1,NPARM
         DO ISTAR=1,NUM_CON_STAR
            IF ( LSTAR_NAME(ISTAR) .EQ. LPARM(IPARM)(1:8) ) THEN
                 RA =VSTARC ( 1, IXREF_SRC(ISTAR) )
                 DEC=VSTARC ( 2, IXREF_SRC(ISTAR) )
!
                 IF ( LPARM(IPARM)(10:20) .EQ. 'RIGHT ASC V' ) THEN
                      NC(1) = NC(1) + 1
                      NC(2) = NC(2) + 1
                      NC(3) = NC(3) + 1
                      IXREF_PAR(NC(1),1) = IPARM
                      IXREF_PAR(NC(2),2) = IPARM
                      IXREF_PAR(NC(3),3) = IPARM
                      CVEC(NC(1),1) = -COS(DEC)*SIN(DEC)*COS(RA)
                      CVEC(NC(2),2) = -COS(DEC)*SIN(DEC)*SIN(RA)
                      CVEC(NC(3),3) =  COS(DEC)**2
!
                      IF ( SPOOL_L1 ) THEN
!
! ------------------------ Add source to the list of sources for further
! ------------------------ printing
!
                           NSRC = NSRC+1
                           ARR_NAM(NSRC) = LPARM(IPARM)(1:8)
                      END IF
                   ELSE IF ( LPARM(IPARM)(10:20) .EQ. 'DEC VELO   ' ) THEN
                      NC(1)=NC(1)+1
                      NC(2)=NC(2)+1
                      IXREF_PAR(NC(1),1) = IPARM
                      IXREF_PAR(NC(2),2) = IPARM
                      CVEC(NC(1),1) =  SIN(RA)
                      CVEC(NC(2),2) = -COS(RA)
                   ELSE IF ( LPARM(IPARM)(10:20) .EQ. 'RIGHT ASCEN' ) THEN
                      CONTINUE
                   ELSE IF ( LPARM(IPARM)(10:20) .EQ. 'DECLINATION' ) THEN
                      CONTINUE
                   ELSE
                      WRITE ( 7, 110 ) IPARM, LPARM(IPARM)
 110                  FORMAT ( 1X,'Trap of internal control: ',I5,'-th '// &
     &                            'parameter "',A,'"  is not recognized' )
                      CALL ERR_LOG ( 8682, IUER, 'NNR_PRP', 'Wrong '// &
     &                    'parameter name: '//LPARM(IPARM) )
                      RETURN
               ENDIF
            ENDIF
         END DO
      END DO
!
!%        write ( 6, * ) 'NNR_PRP  C nc=',nc ! %%
      IF ( NC(1) .EQ. 0  .AND.  NC(2) .EQ. 0  .AND.  NC(3) .EQ. 0 ) THEN
!
! -------- Nothing to do. Go home.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- If we want to weight by aproximate sigmas, do so.
!
      IF ( KSIG_SCALE_NNRS .EQ. 'WG' ) THEN
           DO ICON=1,3
              DO I=1,NC(ICON)
                 IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------------- A bit akward way to get the value (IXREF(I), IXREF(I))
! ------------------- in global-global block of normal matrix
!
                      IAD = FULL_B3D ( B3DOBJ, IXREF_PAR(I,ICON), &
     &                                         IXREF_PAR(I,ICON), &
     &                                 %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
                      CALL COPY_V ( 1, %VAL(IAD), VAL )
                      CVEC(I,ICON) = CVEC(I,ICON)*VAL
                    ELSE
                      IND = INDX8(IXREF_PAR(I,ICON),IXREF_PAR(I,ICON))
                      CVEC(I,ICON) = CVEC(I,ICON)*NOR_MAT(IND)
                 END IF
              END DO
!
! ----------- Noramlize vector of contraints to unity
!
              CALL NORMALIZE_VECTOR4 ( CVEC(1,ICON), NC(ICON) )
           END DO
      ENDIF
!
      WT=1.D0/NNR_PRP_SIGMA**2
      DO ICON=1,3
!
! ------ Add information about the type of the constraint applied
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'NNR_PRP', ICON, 'No-net rotation for prop.motion', &
     &        'r/yr', 0.0D0, NNR_PRP_SIGMA, TRUE_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8683, IUER, 'NNR_PRP', 'Failure '// &
     &            'to add description of constraint to an internal data '// &
     &            'structure' )
              RETURN
         END IF
!
! ------ Set status "dynamic" for NNR_SRC constraint
!
         CALL ERR_PASS ( IUER, IER )
         NS = 3*INT4(NUMSTR)
         CALL SET_DYN_CNS ( 'NNR_PRP', ICON, NS, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8684, IUER, 'NNR_SRC', 'Failure to set status '// &
     &            '"dynamic" to the NNR_PRP constraint' ) 
              RETURN
         END IF
!
! ------ Put the non-zero elements of equations of constraints into CNSTROBJ
!
         DO IPARM=1,NC(ICON)
            IF ( IXREF_PAR(IPARM,ICON) .GT. 0 ) THEN
                 IF ( DABS(CVEC(IPARM,ICON)) .GT. EPS ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'NNR_PRP', ICON, &
     &                     IXREF_PAR(IPARM,ICON), CVEC(IPARM,ICON), &
     &                     TRUE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8685, IUER, 'NNR_PRP', &
     &                         'Failure to write a NNR_PRP equation of '// &
     &                         'constraints' )
                           RETURN
                      END IF
                 END IF
            END IF
         END DO
      END DO
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' NNR_PRP       fast_mode = ',fast_mode, &
     &                    ' n_cnstr= ', cnstrobj%n_ecnst, &
     &                    ' nc(1)= ' ,nc(1), &
     &                    ' n_prp_src= ', num_con_star 
      END IF 
!
      IF ( SPOOL_L1 ) THEN
!
! -------- Writing the list of sources in spool-file
!
           WRITE ( 23, '(A,I4,A)' ) 'NNR_PRP: ', NSRC, ' sources '// &
     &                              'participated in NNR_PRP constraints: '
           IF ( NSRC .GT. 0 ) THEN
!
! -------------- Sorting the list of stations
!
                 CALL SORT_CH ( NSRC, ARR_NAM )
!
! -------------- Cleaning the output line with the sources list
!
                 CALL CLRCH ( OUT )
                 DO I=1,NSRC
!
! ----------------- Add the I-th station to the list of sources for further
! ----------------- printing
!
                    IP  = (I-1)*9 + 1
                    OUT = OUT(1:IP)//ARR_NAM(I)
                 END DO
!
! -------------- Writing long lines in spool-file by splitting it onto
! -------------- sublines no more than 78 characters long
!
                 CALL WRITE_LONG ( 23, 72, OUT(2:) )
                 WRITE ( 23, '(A)' ) ' '
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  NNR_PRP  #!#
