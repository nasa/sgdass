      SUBROUTINE NNR_SRC ( FAST_MODE, FAST_DBG, SPOOL_L1, FGLOBAL_L4, LPARM, &
     &                     NPARM, NOR_MAT, NNR_SRC_SIGMA, NNR_SRC_RTP, B3DOBJ, &
     &                     CNSTROBJ, IUER )
!
!  This subroutine constrains a set of source positions
!  to minimize the distance to the a priori model.
!
!  This is done by applying a rotation.
!  NNR stands for no net rotation.
!
!  modifications
!
!  jmg 9611?? Created as test module.
!  kdb 961125 Installed officially; now get source names from the batch control
!             file, not from an external file.  Also change parameterization
!             of array of "included stars" to max_src from local parameter.
!  jmg 970115 Fix error: was multiplying by invalid, uninitialized variable
!             (scale).  Add subroutine name to error message.
!  kdb,970312 Implement the ability to use weighted or uniform constraints via
!   jmg         the batopt file.
!             (Algorithm from jmg, batopt connection from kdb).
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980401 Corrected a bug: procedure was trying to impose constraints even
!             if no source positions were estimated
!  pet 980721 Added printing the list of sources participated in constrints
!             to the spool-file when SPOOL_L1 is .TRUE.
!  pet 980722 Made NNR_SRC_SIGMA formal parameter instead of named
!             constatnt in solve.i block.
!  pet 980727 Added sorting in alphabetic order the sources list to be
!             printed
!  pet 990103 Corrected a bug: a variable NSRC was not initialized
!  pet 1999.05.15 Corrected a bug: length of variable OUT was hardcoded and
!                 not enough for processing more than 408 sources. Made its
!                 length dependent on parameter MAX_SRC definbde in solve.i
!  pet 1999.05.21 Corrected a bug: it was an error in the test: whether position
!                 of the specific source are estimated: I instead of ISTAR
!  pet 2002.03.20 Changed the logic in order to allow setting nnr_src
!                 constaints on source positions estimated as local parameters.
!                 Made more code more efficient: the previous version computed
!                 all elemnts of normal matrix of constaints, including zeros,
!                 and added them to the normal matrix. The new version computes
!                 only non-zero elements of the normal matrix of constaints
!  pet 2007.05.11  Corrected logic in accordance to changfes in GCONST
!  pet 2017.11.25  Transformed to 64-bit
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
      LOGICAL*4    FGLOBAL_L4
      INTEGER*4    FAST_MODE, FAST_DBG
      INTEGER*4    NPARM
      CHARACTER*20 LPARM(*)
      REAL*8       NNR_SRC_SIGMA, NNR_SRC_RTP(3), NOR_MAT(*)
      LOGICAL*4    TRUE_L4
      PARAMETER  ( TRUE_L4 = .TRUE. )
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4   IUER
!
      INTEGER*4   I, J, NC(3), IXREF_PAR(M_GPA,3), IXREF_SRC(MAX_SRC), &
     &            ITST, NS
!
      REAL*8      CVEC(M_GPA,3), WT
      INTEGER*4   NUM_CON_STAR, ISTAR, IPARM, ICON
      CHARACTER   LSTAR_NAME(MAX_SRC)*8, CC_SOU(MAX_SRC,3)*8
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
      ITST = 0
!
! --- Check whether we should try to apply constraints at all
!
      IF ( FGLOBAL_L4 .AND. NNR_SRC_GLO ) THEN
           CONTINUE
         ELSE IF ( .NOT. FGLOBAL_L4 .AND. NNR_SRC_LOC ) THEN
           CONTINUE
         ELSE IF ( .NOT. FGLOBAL_L4 .AND. &
     &             ( NNR_SRC_GLO .AND. ISLTY2 .EQ. 'I' ) ) THEN
!
! -------- Comment: in indepdent mode global = local
!
           CONTINUE
         ELSE
           CALL ERR_LOG ( 0, IUER )
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
         KINCL = KBIT( DEFSRC, INT2(7))
         IF ( IND_NNR_SRC(1) > 0 .AND. IND_NNR_SRC(2) .GE. IND_NNR_SRC(1) ) THEN
              DO J=IND_NNR_SRC(1),IND_NNR_SRC(2) 
!
! -------------- Scan suppression list
!
                 IF ( SRCSUP(J) .EQ. ISTRN_CHR(ISTAR) ) THEN
!
! ------------------- The source fell in suppression list -- do not use
! ------------------- for constraint equations
!
                      IF ( KBIT4 ( SOUSUP(1,7), J ) ) THEN
                           KINCL = .NOT. KINCL
                      END IF 
                 ENDIF
              ENDDO
         END IF
!
! ------ Test: is right ascension or declination of the ISTAR-th source
! ------ estimated?
!
         IF ( .NOT. KBIT4(LSTAR(1,1),ISTAR)  .AND. &
     &        .NOT. KBIT4(LSTAR(1,2),ISTAR)         ) KINCL = .FALSE.
         IF ( KINCL ) THEN
              NUM_CON_STAR = NUM_CON_STAR + 1
              LSTAR_NAME(NUM_CON_STAR) = ISTRN_CHR(ISTAR)
              IXREF_SRC(NUM_CON_STAR)  = ISTAR
         ENDIF
      ENDDO
!
      IF ( NUM_CON_STAR .EQ. 0 ) THEN
!
! -------- No source positions were estimated: good bye!
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
                 RA =VSTARC(1,IXREF_SRC(ISTAR))
                 DEC=VSTARC(2,IXREF_SRC(ISTAR))
!
                 IF ( LPARM(IPARM)(10:20) .EQ. 'RIGHT ASCEN' ) THEN
                      NC(1) = NC(1)+1
                      NC(2) = NC(2)+1
                      NC(3) = NC(3)+1
                      CC_SOU(NC(1),1) = LSTAR_NAME(ISTAR)
                      CC_SOU(NC(2),2) = LSTAR_NAME(ISTAR)
                      CC_SOU(NC(3),3) = LSTAR_NAME(ISTAR)
                      IXREF_PAR(NC(1),1) = IPARM
                      IXREF_PAR(NC(2),2) = IPARM
                      IXREF_PAR(NC(3),3) = IPARM
!
!                      cvec(nc(1),1) = -cos(dec)*sin(dec)*cos(ra)
!                      cvec(nc(2),2) = -cos(dec)*sin(dec)*sin(ra)
!                      cvec(nc(3),3) =  cos(dec)**2
!
                      CVEC(NC(1),1) = -COS(RA)*TAN(DEC)
                      CVEC(NC(2),2) = -SIN(RA)*TAN(DEC)
                      CVEC(NC(3),3) =  1.0D0
!
                      IF ( SPOOL_L1 ) THEN
!
! ------------------------ Add source to the list of sources for further
! ------------------------ printing
!
                           NSRC = NSRC+1
                           ARR_NAM(NSRC) = LPARM(IPARM)(1:8)
                      END IF
                   ELSE IF ( LPARM(IPARM)(10:20) .EQ. 'DECLINATION' ) THEN
                      NC(1) = NC(1)+1
                      NC(2) = NC(2)+1
                      IXREF_PAR(NC(1),1) = IPARM
                      IXREF_PAR(NC(2),2) = IPARM
                      CC_SOU(NC(1),1) = LSTAR_NAME(ISTAR)
                      CC_SOU(NC(2),2) = LSTAR_NAME(ISTAR)
!
                      CVEC(NC(1),1) =  SIN(RA)
                      CVEC(NC(2),2) = -COS(RA)
                      CVEC(NC(2),3) =  0.0D0
                   ELSE IF ( LPARM(IPARM)(10:20) .EQ. 'RIGHT ASC V' ) THEN
                      CONTINUE
                   ELSE IF ( LPARM(IPARM)(10:20) .EQ. 'DEC VELO   ' ) THEN
                      CONTINUE
                   ELSE
                      WRITE ( 7, 110 ) IPARM, LPARM(IPARM)
 110                  FORMAT ( 1X,'Trap of internal control: ',I5,'-th '// &
     &                            'parameter "',A,'"  is not recognized' )
                      CALL ERR_LOG ( 8671, IUER, 'NNR_SRC', 'Wrong '// &
     &                    'parameter name: '//LPARM(IPARM) )
                      RETURN
               ENDIF
            ENDIF
         END DO
      END DO
!
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
      IF ( ITST > 1 ) THEN
           WRITE ( 6, * ) 'NNR_SRC: KSIG_SCALE_NNRS= ', KSIG_SCALE_NNRS, ' NC= ', NC
           DO ICON=1,3
              DO IPARM=1,NC(ICON)
                 WRITE ( 6, 210 ) ICON, CC_SOU(IPARM,ICON), IXREF_PAR(IPARM,ICON), CVEC(IPARM,ICON)
 210             FORMAT ( 'NNR_SRC: ICON= ', I1, ' Sou= ', A, ' IXREF= ', I6, ' CVEC= ', 1PD13.6  )
              END DO
           END DO
      END IF
!
      WT=1.D0/NNR_SRC_SIGMA**2
      DO ICON=1,3
         IF ( ITST > 0 ) THEN
              WRITE ( 6, 220 ) ICON, NNR_SRC_RTP(ICON), NNR_SRC_SIGMA
 220          FORMAT ( 'NNR_SRC: Icon: ', I1, ' NNR_SRC_RTP CNS: ', 1PD12.5, ' SIGMA= ', 1PD12.5 )
         END IF
!
! ------ Add information about the type of the constraint applied
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'NNR_SRC', ICON, 'No-net rotation for sources', &
     &                     'rad', NNR_SRC_RTP(ICON), NNR_SRC_SIGMA, &
     &                     FGLOBAL_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8672, IUER, 'NNR_SRC', 'Failure '// &
     &            'to add description of constraint to an internal data '// &
     &            'structure' )
              RETURN
         END IF
!
! ------ Set status "dynamic" for NNR_SRC constraint
!
         CALL ERR_PASS ( IUER, IER )
         NS = 3*INT4(NUMSTR)
         CALL SET_DYN_CNS ( 'NNR_SRC', ICON, NS, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8673, IUER, 'NNR_SRC', 'Failure to set status '// &
     &            '"dynamic" to the NNR_SRC constraint' ) 
              RETURN
         END IF
!
! ------ Put the non-zero elements of equations of constraints into CNSTROBJ
!
         DO IPARM=1,NC(ICON)
            IF ( IXREF_PAR(IPARM,ICON) .GT. 0 ) THEN
                 IF ( DABS(CVEC(IPARM,ICON)) .GT. EPS ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'NNR_SRC', ICON, &
     &                     IXREF_PAR(IPARM,ICON), CVEC(IPARM,ICON), &
     &                     FGLOBAL_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8674, IUER, 'NNR_SRC', &
     &                         'Failure to write a NNR_SRC equation of '// &
     &                         'constraints' )
                           RETURN
                      END IF
                 END IF
            END IF
         END DO
      END DO
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' nnr_src       fast_mode = ',fast_mode, &
     &                    ' n_cnstr= ', cnstrobj%n_ecnst, &
     &                    ' nc(1)= ',nc(1), &
     &                    ' n_nnr_src= ', num_con_star 
      END IF
!
      IF ( SPOOL_L1 ) THEN
!
! -------- Writing the list of sources in spool-file
!
           WRITE ( 23, '(A,I4,A)' ) 'NNR_SRC: ', NSRC, ' sources '// &
     &                              'participated in NNR_SRC constraints: '
           IF ( NSRC .GT. 0 ) THEN
!
! -------------- Sorting the list of sources
!
                 CALL SORT_CH ( NSRC, ARR_NAM )
!
! -------------- Cleaning the output line with the sources list
!
                 CALL CLRCH ( OUT )
                 DO I=1,NSRC
!
! ----------------- Add the I-th source to the list of sources for further
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
           END IF
           WRITE ( 23, 230 ) NNR_SRC_SIGMA, NNR_SRC_RTP
 230       FORMAT ( 'NNR_SRC sigma: ', 1PD13.6, ' right hand sides: ', 3(1PD13.6,1X), ' rad' ) 
           WRITE ( 23, '(A)' ) ' '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  NNR_SRC  #!#
