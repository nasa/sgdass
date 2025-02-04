      SUBROUTINE TRI_GRP ( L_STA, LIS_STA, L_BAS, LIS_BAS, &
     &                     M_TRI, L_TRI, LIS_TRI, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  TRI_GRP  generates the list of linearly-independent     *
! *   closed triangles using sorted lists of stations and baselines.     *
! *                                                                      *
! *     It uses lists of stations and baselines. It returns the list     *
! *   of baselines which produces closed triangle and this list will     *
! *   contain all possible linear independent combinations of baselines  *
! *   which produce closed triangle.                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     L_STA ( INTEGER*4 ) -- The number of stations.                   *
! *   LIS_STA ( INTEGER*4 ) -- List of the stations containing codes     *
! *                            of the stations ( sorted in increasing    *
! *                            of the codes ).                           *
! *     L_BAS ( INTEGER*4 ) -- The number of baselines.                  *
! *   LIS_BAS ( INTEGER*4 ) -- List of the baselines containing codes    *
! *                            of the baselines ( sorted in increasing   *
! *                            of the codes ).                           *
! *     M_TRI ( INTEGER*4 ) -- Maximum number of linearly-independent    *
! *                            triangles.                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     L_TRI ( INTEGER*4 ) -- Number of found closed triangles: actual  *
! *                            length of LIS_TRI list.                   *
! *   LIS_TRI ( INTEGER*4 ) -- List of closed triangles dimension of     *
! *                            L_TRI*3. Each row of the list contains    *
! *                            the index in the list of baselines for    *
! *                            those three baselines, which close        *
! *                            triangle. Indices of the baselines are in *
! *                            increasing of baselines codes.            *
! *                                                                      *
! *     Comments:                                                        *
! *                                                                      *
! *      1) In the case when observations took place at all possible     *
! *   baselines then the number of baselines is n*(n-1)/2 and number     *
! *   of linearly independent closed triangles is (n-1)*(n-2)/2 where    *
! *   n is the number of stations. If there were no observations at one  *
! *   or more baselines then the number of linearly independent closed   *
! *   triangles is less or even zero.                                    *
! *                                                                      *
! *      2)  It is possible that one or more found triangles have no     *
! *   observation to be common for all three baseline since observations *
! *   at different baselines took place at different moments of time.    *
! *                                                                      *
! *      3) It is critical that lists LIS_STA and LIS_BAS  be strictly   *
! *   sorted in order of modules of codes of their objects. Otherwise    *
! *   an error will be detected.                                         *
! *                                                                      *
! *      4) If there is at least one pair of baselines in the list of    *
! *   the baselines which is produced from the same stations but in      *
! *   different order, then error will be detected. If a database        *
! *   actually have baselines from stations in different order one       *
! *   baseline should be deselected from solution and the list of        *
! *   baselines should be recalculated before call of TRI_GRP.           *
! *                                                                      *
! * ###  06-NOV-1991    TRI_GRP    v3.1  (c)  L. Petrov 09-AUG-2001 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  L_STA, L_BAS, M_TRI, IUER
        INTEGER*4  LIS_STA(L_STA), LIS_BAS(L_BAS), LIS_TRI(3,M_TRI), IBTR(3)
        INTEGER*4  K1, K2, K3, J1, J2, J3, J4, L_TRI, ISTA_1, ISTA_2, ISTA_3, &
     &             IBMN, IBMX, IBMD, ISG(3), LAST_STA, LAST_BAS
        LOGICAL*4  LMN, LMD, LMX
        INTEGER*4  NSTBA, IFIND_PL, I_LEN
        CHARACTER  STR*20
!
        IF ( L_STA .LT. 1 ) THEN
             CALL CLRCH (        STR  )
             CALL INCH  ( L_STA,   STR  )
             CALL ERR_LOG ( 7291, IUER, 'TRI_GRP', 'Parameter L_STA '// &
     &           'has the wrong value '//STR(1:I_LEN(STR)) )
             RETURN
        END IF
!
        IF ( L_BAS .LT. 1 ) THEN
             CALL CLRCH (        STR  )
             CALL INCH  ( L_BAS,   STR  )
             CALL ERR_LOG ( 7292, IUER, 'TRI_GRP', 'Parameter L_BAS '// &
     &           'has the wrong value '//STR(1:I_LEN(STR)) )
             RETURN
        END IF
!
        IF ( M_TRI .LT. 1 ) THEN
             CALL CLRCH (        STR  )
             CALL INCH  ( M_TRI,   STR  )
             CALL ERR_LOG ( 7293, IUER, 'TRI_GRP', 'Parameter M_TRI '// &
     &           'has the wrong value '//STR(1:I_LEN(STR)) )
             RETURN
        END IF
!
! ----- Check of the station list
!
        DO 310 K1=1,L_STA
           IF ( K1 .GT. 1 ) THEN
                IF ( LIS_STA(K1) .EQ. LAST_STA ) THEN
                     CALL ERR_LOG ( 7294, IUER, 'TRI_GRP', 'List of '// &
     &                   'stations contains duplicate elements' )
                     RETURN
                  ELSE IF ( LIS_STA(K1) .LT. LAST_STA ) THEN
                     CALL ERR_LOG ( 7295, IUER, 'TRI_GRP', 'List of '// &
     &                   'stations is not in increasing order' )
                     RETURN
                END IF
           END IF
           LAST_STA = LIS_STA(K1)
 310    CONTINUE
!
! ----- Check of the baseline list
!
        DO 320 K2=1,L_BAS
           IF ( K2 .GT. 1 ) THEN
                IF ( LIS_BAS(K2) .EQ. LAST_BAS ) THEN
                     CALL ERR_LOG ( 7296, IUER, 'TRI_GRP', 'List of '// &
     &                   'baselines contains duplicate elements' )
                     RETURN
                  ELSE IF ( ABS(LIS_BAS(K2)) .LT. ABS(LAST_BAS) ) THEN
                     CALL ERR_LOG ( 7297, IUER, 'TRI_GRP', 'List of '// &
     &                   'baselines is not in increasing order' )
                     RETURN
                END IF
!
                DO 330 K3=1,K2-1
                   IF ( LIS_BAS(K3) .EQ. -LIS_BAS(K2) ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( ABS(LIS_BAS(K2)), STR )
                        CALL ERR_LOG ( 7298, IUER, 'TRI_GRP', 'List of '// &
     &                       'baselines contains elements which are equal '// &
     &                       'by modulo but have different sign: '// &
     &                        STR(1:I_LEN(STR))//' and -'//STR(1:I_LEN(STR))// &
     &                       '. It means that there are baselines produced '// &
     &                       ' by the same stations but in different order. '// &
     &                       'TRI_GRP doesn''t support such a case. Hint: '// &
     &                       'deselect one of these two baselines from '// &
     &                       'the list ' )
                        RETURN
                   END IF
 330            CONTINUE
           END IF
           LAST_BAS = LIS_BAS(K2)
 320    CONTINUE
!
        L_TRI=0
!
! ----- Form all possible triangles and test them
!
        DO 410 J1=1,L_STA-2
           DO 420 J2=J1+1,L_STA-1
!!              DO 430 J3=J1+2,L_STA ! old
              DO 430 J3=J2+1,L_STA
                  L_TRI=L_TRI+1
!
! --------------- Form the list of stations making closured triangle
!
                  ISTA_1=LIS_STA(J1)
                  ISTA_2=LIS_STA(J2)
                  ISTA_3=LIS_STA(J3)
!
! --------------- Form a short 3-element list of baseline codes making closured
! --------------- triangle.
!
                  IBTR(1)=IABS ( NSTBA ( ISTA_1, ISTA_2 ) )
                  IBTR(2)=IABS ( NSTBA ( ISTA_2, ISTA_3 ) )
                  IBTR(3)=IABS ( NSTBA ( ISTA_3, ISTA_1 ) )
!
! --------------- Search there baselines with max in module code (IBMX), min
! --------------- in module code (IBMN) and a middle code (IBMD).
!
                  IBMX=IBTR(1)
                  IF ( IBTR(2) .GT. IBMX ) IBMX=IBTR(2)
                  IF ( IBTR(3) .GT. IBMX ) IBMX=IBTR(3)
!
                  IBMN=IBTR(1)
                  IF ( IBTR(2) .LT. IBMN ) IBMN=IBTR(2)
                  IF ( IBTR(3) .LT. IBMN ) IBMN=IBTR(3)
!
                  IF (IBTR(1).NE.IBMN .AND. IBTR(1).NE.IBMX) IBMD=IBTR(1)
                  IF (IBTR(2).NE.IBMN .AND. IBTR(2).NE.IBMX) IBMD=IBTR(2)
                  IF (IBTR(3).NE.IBMN .AND. IBTR(3).NE.IBMX) IBMD=IBTR(3)
!
! --------------- Putting baselines codes in the list of closured triangles
! --------------- in order of increasing baselines codes.
!
                  LIS_TRI(1,L_TRI) = IFIND_PL ( L_BAS, LIS_BAS, IBMN )
                  IF ( LIS_TRI(1,L_TRI) .LE. 0) &
     &                 LIS_TRI(1,L_TRI) = IFIND_PL ( L_BAS, LIS_BAS, -IBMN )
                  IF ( LIS_TRI(1,L_TRI) .LE. 0 ) THEN
!
! -------------------- Removing this triangle from the list of triangles since
! -------------------- the baseline with minimal code is not in the baseline
! -------------------- list
!
                       L_TRI=L_TRI-1
                       GOTO 430
                 END IF
!
                 LIS_TRI(2,L_TRI)=IFIND_PL ( L_BAS, LIS_BAS, IBMD )
                 IF ( LIS_TRI(2,L_TRI) .LE. 0) &
     &                LIS_TRI(2,L_TRI) = IFIND_PL ( L_BAS, LIS_BAS, -IBMD )
                 IF ( LIS_TRI(2,L_TRI).LE.0) THEN
!
! ------------------ Removing this triangle from the list of triangles since
! ------------------ the baseline with middle code is not in the baseline list
!
                     L_TRI=L_TRI-1
                     GOTO 430
                 END IF
!
                 LIS_TRI(3,L_TRI)=IFIND_PL ( L_BAS, LIS_BAS, IBMX )
                 IF ( LIS_TRI(3,L_TRI) .LE. 0) &
     &                LIS_TRI(3,L_TRI) = IFIND_PL ( L_BAS, LIS_BAS, -IBMX )
                 IF ( LIS_TRI(3,L_TRI).LE.0 ) THEN
!
! ------------------- Removing this triangle from the list of triangles since
! ------------------- the baseline with maximal code is not in the baseline list
!
                      L_TRI=L_TRI-1
                      GOTO 430
                 END IF
!
! -------------- Learn signs for baselines in closured triangles and check the
! -------------- triangle as well.
!
                 CALL SIGN_TRI ( LIS_TRI(1,L_TRI), L_BAS, LIS_BAS, ISG, -2 )
!
                 IF ( L_TRI .GE. 2 ) THEN
!
! ------------------- Test: isn't a newborn triangle linear-dependent on any
! ------------------- triangles formed before it.
!
                      LMN=.FALSE.
                      LMD=.FALSE.
                      LMX=.FALSE.
!
! ------------------- To do it let's check: whether the baselines of newborn
! ------------------- triangle participated in previously formed triangles?
!
                      DO 440 J4=1,L_TRI-1
                         IF ( (LIS_TRI(1,J4) .EQ.  LIS_TRI(1,L_TRI)) .OR. &
     &                        (LIS_TRI(1,J4) .EQ. -LIS_TRI(1,L_TRI)) .OR. &
     &                        (LIS_TRI(2,J4) .EQ.  LIS_TRI(1,L_TRI)) .OR. &
     &                        (LIS_TRI(2,J4) .EQ. -LIS_TRI(1,L_TRI)) .OR. &
     &                        (LIS_TRI(3,J4) .EQ.  LIS_TRI(1,L_TRI)) .OR. &
     &                        (LIS_TRI(3,J4) .EQ. -LIS_TRI(1,L_TRI))) LMN=.TRUE.
!
                         IF ( (LIS_TRI(1,J4) .EQ.  LIS_TRI(2,L_TRI)) .OR. &
     &                        (LIS_TRI(1,J4) .EQ. -LIS_TRI(2,L_TRI)) .OR. &
     &                        (LIS_TRI(2,J4) .EQ.  LIS_TRI(2,L_TRI)) .OR. &
     &                        (LIS_TRI(2,J4) .EQ. -LIS_TRI(2,L_TRI)) .OR. &
     &                        (LIS_TRI(3,J4) .EQ.  LIS_TRI(2,L_TRI)) .OR. &
     &                        (LIS_TRI(3,J4) .EQ. -LIS_TRI(2,L_TRI))) LMD=.TRUE.
!
                         IF ( (LIS_TRI(1,J4) .EQ.  LIS_TRI(3,L_TRI)) .OR. &
     &                        (LIS_TRI(1,J4) .EQ. -LIS_TRI(3,L_TRI)) .OR. &
     &                        (LIS_TRI(2,J4) .EQ.  LIS_TRI(3,L_TRI)) .OR. &
     &                        (LIS_TRI(2,J4) .EQ. -LIS_TRI(3,L_TRI)) .OR. &
     &                        (LIS_TRI(3,J4) .EQ.  LIS_TRI(3,L_TRI)) .OR. &
     &                        (LIS_TRI(3,J4) .EQ. -LIS_TRI(3,L_TRI))) LMX=.TRUE.
  440                 CONTINUE
!
! ------------------- If all three baselines of the triangle has been used
! ------------------- already in making the previous trinagles then any new
! ------------------- triangle would be a linealry-dependent from the old one
! ------------------- and therefore should be repelled.
!
                      IF ( LMN  .AND.  LMD  .AND.  LMX ) THEN
                           L_TRI=L_TRI-1
                           GOTO 430
                      END IF
                 END IF
!
! -------------- Test: maybe this triangle is the last?
!
                 IF ( L_TRI .EQ. M_TRI ) THEN
                      CALL ERR_LOG ( 0, IUER )
                      RETURN
                 END IF
  430         CONTINUE
  420      CONTINUE
  410   CONTINUE
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  TRI_GRP  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SIGN_TRI ( ITR, L_BAS, LIS_BAS, ISG, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  SIGN_TRI  calculates signature of closed triangle.      *
! *   The following expression used for evaluation of the phase closure  *
! *   (or baseline closure):                                             *
! *   S(1)*T(1) + S(2)*T(2) +  S(3)*T(3) = 0 ,  where T(i) -- delay for  *
! *   the i-th baseline, S(i)  --  sign ( +1 or -1 ). Routine  SIGN_TRG  *
! *   calculates such an array S in order to close triangle.             *
! *   It is assumed that observations were made at the same moment of    *
! *   geocentric time (what is not generally a case).                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     ITR ( INTEGER*4 ) -- Array dimension of 3 which contains indices *
! *                          of the baselines for triangle closure.      *
! *   L_BAS ( INTEGER*4 ) -- The number of baselines.                    *
! * LIS_BAS ( INTEGER*4 ) -- The list of baselines coddes sorted in      *
! *                          according with baselines codes increasing.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     ISG ( INTEGER*4 ) -- Array dimension of 3, which contains        *
! *                          1 or -1 and which determines asignature of  *
! *                          the closed triangle.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  29-DEC-94    SIGN_TRI   V2.1  (c)   L. Petrov  05-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  ITR(3), L_BAS, LIS_BAS(L_BAS), IUER, ISG(3), NS(6), &
     &             L_STA, LIS_STA(6), K_STA(6), J1, J2
        INTEGER*4  I_LEN
        CHARACTER  STR*4, STR1*20, STR2*20, STR3*20
!
        DO 410 J1=1,3
           IF ( ITR(J1) .LT. 0   .OR.  ITR(J1) .GT. L_BAS ) THEN
                STR(1:4)=' -th'
                CALL INCH ( J1, STR(1:1) )
!
! ------------- Test: whether this list is a list of baseline indices?
!
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL CLRCH ( STR3 )
                CALL INCH  ( ITR(1), STR1 )
                CALL INCH  ( ITR(2), STR2 )
                CALL INCH  ( ITR(3), STR3 )
!
                CALL ERR_LOG( 4181, IUER, 'SIGN_TRI', 'Element '// &
     &               STR(1:I_LEN(STR))//' has a wrong values '// &
     &              'in the baseline list [ '// &
     &               STR1(1:I_LEN(STR1))//', '// &
     &               STR2(1:I_LEN(STR2))//', '// &
     &               STR3(1:I_LEN(STR3))//' ]' )
           END IF
           CALL NBAST ( LIS_BAS( ITR(J1) ), NS(J1*2-1), NS(J1*2) )
  410   CONTINUE
!
! ----- Forming list of the stations for the test: whether ITR is an actual
! ----- list of closed triangles or not?
!
        L_STA=0
        DO 420 J2=1,6
           CALL ADC_LIS ( 6, L_STA, LIS_STA, K_STA, NS(J2), -3 )
  420   CONTINUE
!
! ----- Test: whether the list ITR is a list of closed triangles?
!
        IF ( L_STA .NE. 3      .OR. &
     &       K_STA(1) .NE. 2   .OR. &
     &       K_STA(2) .NE. 2   .OR. &
     &       K_STA(3) .NE. 2        ) THEN
!
             WRITE ( 6, * ) ' k_sta(1)=',k_sta(1)
             WRITE ( 6, * ) ' k_sta(2)=',k_sta(2)
             WRITE ( 6, * ) ' k_sta(3)=',k_sta(3)
             WRITE ( 6, * ) ' l_sta=',l_sta,' ns=',ns
             WRITE ( 6, * ) ' itr  =',itr
!
             CALL ERR_LOG ( 4182, IUER, 'SIGN_TRI', 'The baseline list '// &
     &           'under investigation is not a list of closed triangles' )
             RETURN
        END IF
!
        ISG(1)=1
!
        IF ( NS(1) .EQ. NS(3) ) ISG(2)=-1
        IF ( NS(1) .EQ. NS(4) ) ISG(2)=1
        IF ( NS(1) .EQ. NS(5) ) ISG(3)=-1
        IF ( NS(1) .EQ. NS(6) ) ISG(3)=1
!
        IF ( NS(2) .EQ. NS(3) ) ISG(2)=1
        IF ( NS(2) .EQ. NS(4) ) ISG(2)=-1
        IF ( NS(2) .EQ. NS(5) ) ISG(3)=1
        IF ( NS(2) .EQ. NS(6) ) ISG(3)=-1
!
        CALL ERR_LOG ( 0, IUER )
!
        RETURN
        END  !#!  SIGN_TRI  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SIGN_TRICLS ( ITR, L_BAS, LIS_BAS, ISG, ISG_COR, &
     &                         IND_FRE, IND_TAU, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  SIGN_TRICLS  calculates signature of triangle phase     *
! *   closure. The following expression used for evaluation of the       *
! *   expression for closure phase or delays:                            *
! *                                                                      *
! *   S(1)*T_obs(1) + S(2)*T_obs(2) + S(3)*T_obs(3) +                    *
! *   SC(1)*F_obs(IND_FRE(1))*T_theor(IND_TAU(1)) +                      *
! *   SC(2)*F_obs(IND_FRE(2))*T_theor(IND_TAU(2)) = 0                    *
! *                                                                      *
! *   where T(i) -- delay for the i-th baseline, F(j) -- delay rate for  *
! *   the j-th baseline, S(i) and Sc(i) -- sign ( -1, 0, +1 ).           *
! *   Routine SIGN_TRICLS calculates such an array S in order to close   *
! *   triangle to the epoch of the observation for the baseline 1.       *
! *                                                                      *
! *   This routine takes into account the fact that observations may     *
! *   occur not at the same moment of geocentic time and it calculates   *
! *   an appropriate correction. It finds baselines and signs for        *
! *   calculation of correction to the closure which gives rise for the  *
! *   fact that dalays may be measured to the different epochs.          *
! *                                                                      *
! *   Comments:                                                          *
! *   1) Observed delays should be taken for the first, second and third *
! *      terms of the expression above;                                  *
! *   2) theoretical delays should be taken for the 4-th and 5-th term   *
! *      (since these term take into account theoretical correction to   *
! *       the closures, and observed values can be contaminated by clock *
! *       errors. We must get rod from instrumental delays here);        *
! *   3) observed delay rates should be taken for the 4-th and 5-th term *
! *      (we must take into account instrumental delay rate );           *
! *   4) 4-th and 5-th term may appear to be zero. ISG_COR(i) will be    *
! *      zero for such terms but, IND_TAU, IND_FRE will be 1 in order    *
! *      to prevent a possible "index out of range" error.               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     ITR ( INTEGER*4 ) -- Array dimension of 3 which contains indices *
! *                          of the baselines for triangle closure.      *
! *   L_BAS ( INTEGER*4 ) -- The number of baselines.                    *
! * LIS_BAS ( INTEGER*4 ) -- The list of baselines codes sorted in       *
! *                          according with baselines codes increasing.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     ISG ( INTEGER*4 ) -- Array dimension of 3, which contains        *
! *                          1 or -1 and which determines asignature of  *
! *                          the closed triangle.                        *
! * ISG_COR ( INTEGER*4 ) -- Array of signs (-1 or -1) for the term with *
! *                          correction for difference in epochs.        *
! * IND_FRE ( INTEGER*4 ) -- Array of indices of the baselines for the   *
! *                          term "delay rate " of calculation of the    *
! *                          correction for difference in epochs.        *
! * IND_TAU ( INTEGER*4 ) -- Array of indices of the baselines for the   *
! *                          term "delay" of calculation of the          *
! *                          correction for difference in epochs.        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ###  22-MAY-1998  SIGN_TRICLS   v1.1 (c) L. Petrov  03-AUG-1998  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  ITR(3), L_BAS, LIS_BAS(L_BAS), ISG(3), ISG_COR(2), &
     &           IND_FRE(2), IND_TAU(2), IUER
      INTEGER*4  NS(6), L_STA, LIS_STA(6), K_STA(6), ITAU_BAS, IFRE_BAS, &
     &           N_COR, J1, J2
      INTEGER*4  I_LEN, IFIND_PL, NSTBA
      CHARACTER  STR*4, STR1*20, STR2*20, STR3*20
      INTEGER*4  TAKE_SGN, ARG_SGN, ISGN_T
      TAKE_SGN(ARG_SGN) = ISIGN ( 1, ARG_SGN )
!
      DO 410 J1=1,3
         IF ( ITR(J1) .LT. 0   .OR.  ITR(J1) .GT. L_BAS ) THEN
              STR(1:4)=' -th'
              CALL INCH ( J1, STR(1:1) )
!
! ----------- Test: whether this list is a list of baseline indices?
!
              CALL CLRCH ( STR1 )
              CALL CLRCH ( STR2 )
              CALL CLRCH ( STR3 )
              CALL INCH  ( ITR(1), STR1 )
              CALL INCH  ( ITR(2), STR2 )
              CALL INCH  ( ITR(3), STR3 )
!
              CALL ERR_LOG( 4181, IUER, 'SIGN_TRICLS', 'Element '// &
     &               STR(1:I_LEN(STR))//' has a wrong values '// &
     &              'in the baseline list [ '// &
     &               STR1(1:I_LEN(STR1))//', '// &
     &               STR2(1:I_LEN(STR2))//', '// &
     &               STR3(1:I_LEN(STR3))//' ]' )
           END IF
           CALL NBAST ( LIS_BAS( ITR(J1) ), NS(J1*2-1), NS(J1*2) )
  410   CONTINUE
!
! ----- Forming list of the stations for the test: whether ITR is an actual
! ----- list of closed triangles or not?
!
        L_STA=0
        DO 420 J2=1,6
           CALL ADC_LIS ( 6, L_STA, LIS_STA, K_STA, NS(J2), -3 )
  420   CONTINUE
!
! ----- Test: whether the list ITR is a list of closed triangles?
!
        IF ( L_STA .NE. 3      .OR. &
     &       K_STA(1) .NE. 2   .OR. &
     &       K_STA(2) .NE. 2   .OR. &
     &       K_STA(3) .NE. 2        ) THEN
!
             WRITE ( 6, * ) ' k_sta(1)=',k_sta(1)
             WRITE ( 6, * ) ' k_sta(2)=',k_sta(2)
             WRITE ( 6, * ) ' k_sta(3)=',k_sta(3)
             WRITE ( 6, * ) ' l_sta=',l_sta,' ns=',ns
             WRITE ( 6, * ) ' itr  =',itr
!
             CALL ERR_LOG ( 4182, IUER, 'SIGN_TRICLS', 'The baseline list '// &
     &           'under investigation is not a list of closed triangles' )
             RETURN
        END IF
!
        ISG(1)=1
!
        IF ( NS(1) .EQ. NS(3) ) ISG(2)=-1
        IF ( NS(1) .EQ. NS(4) ) ISG(2)=1
        IF ( NS(1) .EQ. NS(5) ) ISG(3)=-1
        IF ( NS(1) .EQ. NS(6) ) ISG(3)=1
!
        IF ( NS(2) .EQ. NS(3) ) ISG(2)=1
        IF ( NS(2) .EQ. NS(4) ) ISG(2)=-1
        IF ( NS(2) .EQ. NS(5) ) ISG(3)=1
        IF ( NS(2) .EQ. NS(6) ) ISG(3)=-1
!
! ----- Initialzation
!
        N_COR = 0
        ISG_COR(1) = 0
        ISG_COR(2) = 0
        IND_TAU(1) = 1
        IND_TAU(2) = 1
        IND_FRE(1) = 1
        IND_FRE(2) = 1
!
        IF ( NS(3) .NE. NS(1)  ) THEN
!
! ---------- First station of the second baseline is not the same as
! ---------- for the first baseline
!
             N_COR = N_COR + 1
             IND_FRE(N_COR) = 2
             ITAU_BAS = IFIND_PL ( L_BAS, LIS_BAS, NSTBA ( NS(3), NS(1) ) )
             ISGN_T = 1
             IF ( ITAU_BAS .LE. 0 ) THEN
                  ITAU_BAS = IFIND_PL ( L_BAS, LIS_BAS, NSTBA ( NS(1), NS(3) ) )
                  ISGN_T = -1
             END IF
!
             IND_TAU(N_COR) =  IFIND_PL ( 3, ITR, ITAU_BAS )
             ISG_COR(N_COR) = -ISGN_T*ISG(IND_FRE(N_COR))
        END IF
!
        IF ( NS(5) .NE. NS(1) ) THEN
!
! ---------- First station of the third baseline is not the same as
! ---------- for the first baseline
!
             N_COR = N_COR + 1
             IND_FRE(N_COR) = 3
             ITAU_BAS = IFIND_PL ( L_BAS, LIS_BAS, NSTBA ( NS(5), NS(1) ) )
             ISGN_T = 1
             IF ( ITAU_BAS .LE. 0 ) THEN
                  ITAU_BAS = IFIND_PL ( L_BAS, LIS_BAS, NSTBA ( NS(1), NS(5) ) )
                  ISGN_T = -1
             END IF
!
             IND_TAU(N_COR) =  IFIND_PL ( 3, ITR, ITAU_BAS )
             ISG_COR(N_COR) = -ISGN_T*ISG(IND_FRE(N_COR))
        END IF
!
        CALL ERR_LOG ( 0, IUER )
!
        RETURN
        END  !#!  SIGN_TRICLS  #!#
