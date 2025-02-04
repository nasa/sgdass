      SUBROUTINE CHI_UPDATE ( ISGN, IMTP, IOBS, IDB2, IDBF, N_OBS, &
     &           L_SCA, L_STA, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CHI_UPDATE  calculates IOBS-th equation of conditions and *
! *   update accumulators for the mathematical expactations of the       *
! *   partial weighted sum of squares of residuals for the addition into *
! *   consideration of the IOBS-th observation.                          *
! *                                                                      *
! *   Contribution to mathematical expectation of the cho-square for     *
! *   the IOBS-th observation is calculated as                           *
! *                                                                      *
! *   CHI_MAT := CHI_MAT + ISGN*wei**2 *[                                *
! *              (a_g, V_gg * a_g + V_1g(T) * a_1 + V_2g(T) * a_2 ) +    *
! *              (a_1, V_1g * a_g + V_11 * a_1    + V_21(T) * a_2 ) +    *
! *              (a_2, V_2g * a_g + V_21 * a_1    + V_22 * a_2    ) ]    *
! *                                                                      *
! *       where wei -- weight of the observation, a_g, a_1, a_2 --       *
! *   glolbal, segmented-current and segmented-next part of the equation *
! *   of conditions; V_gg, V_1g, V_2g, V_11, V_21, V_22  -- bloks of the *
! *   covariance matrix: global-global, segment_current-global,          *
! *   segment_next-globa, segment_current-current, segment_next-current  *
! *   segment_next-next.                                                 *
! *                                                                      *
! *   Chi-square accumaltors is also updated.                            *
! *   Chi-mat with squares of weights is also updated.                   *
! *   Accumulator for the sum of squares of weights is updated.          *
! *   Counter of observations is updated.                                *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      ISGN ( INTEGER*4 ) -- Switch of mode:                           *
! *                            ISGN= 1 -- Accumultor will be updated     *
! *                                       for adding new observation.    *
! *                                       Counter of observations will   *
! *                                       be incremented.                *
! *                            ISGN=-1 -- Accumultor will be updated     *
! *                                       for substracting new           *
! *                                       observation. Counter of        *
! *                                       observations will be           *
! *                                       decremented.                   *
! *      IMTP ( INTEGER*4 ) -- Matrix type                               *
! *                            IMTP= 1 -- Covariance matrix has been     *
! *                                       unscaled already.              *
! *                            IMTP= 2 -- Covariance matrix has not been *
! *                                       unscaled yet and its values    *
! *                                       are scaled.                    *
! *      IOBS ( INTEGER*4 ) -- Index of the observations concerned.      *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_STA ( INTEGER*4 ) -- Number of participated stations.          *
! *     L_SCA ( INTEGER*4 ) -- Number of common scans.                   *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *     PLACE ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *                            It keeps O-C and coefficients of the      *
! *                            IOBS-th equation of conditions.           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will pe put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successful       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  22-JAN-1998 CHI_UPDATE   v1.1  (c)  L. Petrov 20-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INTEGER*2  IDB2
      INTEGER*4  ISGN, IMTP, IOBS, IDBF, N_OBS, L_SCA, L_STA, IUER
!
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( NCREC__STRU   ) ::  NCREC
      TYPE ( SCA_O__STRU   ) ::  OBSSCA(L_SCA)
      TYPE ( STA_O__STRU   ) ::  OBSSTA(L_STA)
      TYPE ( BAS_O__STRU   ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU   ) ::  RES(N_OBS)
      TYPE ( PLACE__STRU   ) ::  PLACE
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( CHIACC__STRU  ) ::  CHIOBJ
      REAL*8,    ALLOCATABLE, TARGET :: VEC_ALLOCATED(:)
      REAL*8,    POINTER     :: CONTR_GG(:), CONTR_G1(:), CONTR_G2(:), &
     &           CONTR_1G(:), CONTR_11(:), CONTR_12(:), &
     &           CONTR_2G(:), CONTR_21(:), CONTR_22(:), &
     &           EQU_GLO(:),  EQU_SG1(:),  EQU_SG2(:), &
     &           UNS_GLO(:),  UNS_SG1(:),  UNS_SG2(:)
!@      REAL*8, SAVE :: CONTR_GG(MAX_PAR), CONTR_G1(MAX_PAR), CONTR_G2(MAX_PAR), &
!@     &           CONTR_1G(MAX_PSG), CONTR_11(MAX_PSG), CONTR_12(MAX_PSG), &
!@     &           CONTR_2G(MAX_PSG), CONTR_21(MAX_PSG), CONTR_22(MAX_PSG), &
!@     &           EQU_GLO(MAX_PAR),  EQU_SG1(MAX_PSG),  EQU_SG2(MAX_PSG), &
!@     &           UNS_GLO(MAX_PAR),  UNS_SG1(MAX_PSG),  UNS_SG2(MAX_PSG)
      REAL*8     CONTR_GLO, CONTR_CUR, CONTR_NXT, ATVA
      ADDRESS__TYPE :: AD_GG, AD_1G, AD_2G, AD_11, AD_21, AD_22
      INTEGER*4  GLO, SG1, SG2, NEL
      INTEGER*4  IND, IP_SOU, IP_BAS, IER
      CHARACTER  STR*80
!
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, NSTBA, I_LEN
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IF ( ISGN .NE. 1  .AND.  ISGN .NE. -1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( ISGN, STR )
           CALL ERR_LOG ( 6481, IUER, 'CHI_UPDATE', 'Wrong value of the '// &
     &         'parameter  ISGN :'//STR(1:I_LEN(STR))//'. Valid vales are '// &
     &         '-1, 1 only' )
           RETURN
      END IF
!
      IF ( IMTP .NE. 1  .AND.  IMTP .NE. 2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IMTP, STR )
           CALL ERR_LOG ( 6482, IUER, 'CHI_UPDATE', 'Wrong value of the '// &
     &         'parameter  IMTP :'//STR(1:I_LEN(STR))//'. Valid vales are '// &
     &         '1, 2 only' )
           RETURN
      END IF
!
! --- Extraction form B3D-object dimensions of the submatrices and their
! --- addreses for facilitation further coding
!
      SG1   = B3DOBJ%SB
      GLO   = B3DOBJ%N_GLO
      AD_GG = B3DOBJ%AD_B0
      AD_1G = B3DOBJ%AD_B(PLACE%CURR_CSG)
      AD_11 = B3DOBJ%AD_C(PLACE%CURR_CSG)
!
      IF ( PLACE%CURR_CSG .EQ. B3DOBJ%NBS-1 ) THEN
           AD_2G = B3DOBJ%AD_BX
           AD_21 = B3DOBJ%AD_DX
           AD_22 = B3DOBJ%AD_CX
           SG2   = B3DOBJ%SX
        ELSE
           AD_2G = B3DOBJ%AD_B(PLACE%CURR_CSG+1)
           AD_21 = B3DOBJ%AD_D(PLACE%CURR_CSG+1)
           AD_22 = B3DOBJ%AD_C(PLACE%CURR_CSG+1)
           SG2   = B3DOBJ%SB
      END IF
!
! --- Allocate dynamic memore. To reduce overheads, one block is allocated
! --- and then the memory is disctrivuted over 15 arrays
!
      NEL = 5*GLO + 5*SG1 + 5*SG2
      ALLOCATE ( VEC_ALLOCATED(NEL) )
      CONTR_GG => VEC_ALLOCATED(1:GLO)                                   ! glo
      CONTR_G1 => VEC_ALLOCATED(GLO+1               : 2*GLO)             ! glo
      CONTR_G2 => VEC_ALLOCATED(2*GLO+1             : 3*GLO)             ! glo
      CONTR_1G => VEC_ALLOCATED(3*GLO+1             : 3*GLO+  SG1)       ! sg1 
      CONTR_11 => VEC_ALLOCATED(3*GLO+  SG1      +1 : 3*GLO+2*SG1      ) ! sg1
      CONTR_12 => VEC_ALLOCATED(3*GLO+2*SG1      +1 : 3*GLO+3*SG1      ) ! sg1
      CONTR_2G => VEC_ALLOCATED(3*GLO+3*SG1      +1 : 3*GLO+3*SG1+  SG2) ! sg2
      CONTR_21 => VEC_ALLOCATED(3*GLO+3*SG1+  SG2+1 : 3*GLO+3*SG1+2*SG2) ! sg2
      CONTR_22 => VEC_ALLOCATED(3*GLO+3*SG1+2*SG2+1 : 3*GLO+3*SG1+3*SG2) ! sg2
      EQU_GLO  => VEC_ALLOCATED(3*GLO+3*SG1+3*SG2+1 : 4*GLO+3*SG1+3*SG2) ! glo
      EQU_SG1  => VEC_ALLOCATED(4*GLO+3*SG1+3*SG2+1 : 4*GLO+4*SG1+3*SG2) ! sg1
      EQU_SG2  => VEC_ALLOCATED(4*GLO+4*SG1+3*SG2+1 : 4*GLO+4*SG1+4*SG2) ! sg2
      UNS_GLO  => VEC_ALLOCATED(4*GLO+4*SG1+4*SG2+1 : 5*GLO+4*SG1+4*SG2) ! glo
      UNS_SG1  => VEC_ALLOCATED(5*GLO+4*SG1+4*SG2+1 : 5*GLO+5*SG1+4*SG2) ! sg1
      UNS_SG2  => VEC_ALLOCATED(5*GLO+5*SG1+4*SG2+1 : 5*GLO+5*SG1+5*SG2) ! sg2
!
      IF ( IMTP .EQ. 1 ) THEN
!
! --------- Unscaling has been done already. We merely copy arrays of
! --------- global part, segmented-current and segmented-next parts of
! --------- equatiuon of conditions
!
            CALL COPY_V ( PLACE%N_GLO, PLACE%EQU_GLO, EQU_GLO )
            CALL COPY_V ( PLACE%N_SG1, PLACE%EQU_SG1, EQU_SG1 )
            CALL COPY_V ( PLACE%N_SG2, PLACE%EQU_SG2, EQU_SG2 )
        ELSE IF ( IMTP .EQ. 2 ) THEN
!
! --------- No unscaling has been done. We need scale global, segmented-current
! --------- and segmented-next parts equation of conditions.
!
! --------- Firstly squeeze vectors of scales in according with indeces of parts
! --------- of equations of conditions. Squeesed vectors will be kept in arrays
! --------- UNS_GLO, UNS_SG1, UNS_SG2
!
            CALL DGATHER ( PLACE%N_GLO, PLACE%IND_GLO, %VAL(B3DOBJ%AD_U0), &
     &                     UNS_GLO  )
            CALL DGATHER ( PLACE%N_SG1, PLACE%IND_SG1, &
     &                     %VAL(B3DOBJ%AD_US(PLACE%CURR_CSG)), UNS_SG1 )
            IF ( PLACE%CURR_CSG .EQ. B3DOBJ%NBS-1 ) THEN
                 CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, &
     &                          %VAL(B3DOBJ%AD_USX), UNS_SG2 )
               ELSE
                 CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, &
     &                          %VAL(B3DOBJ%AD_US(PLACE%CURR_CSG+1)), UNS_SG2 )
            END IF
!
! --------- Then element-by-element multiplying vectors of equations of
! --------- conditions by unscale vectors.
!
            CALL VEC_MULT_VECTOR ( UNS_GLO, PLACE%EQU_GLO, PLACE%N_GLO, &
     &                             EQU_GLO )
            CALL VEC_MULT_VECTOR ( UNS_SG1, PLACE%EQU_SG1, PLACE%N_SG1, &
     &                             EQU_SG1 )
            CALL VEC_MULT_VECTOR ( UNS_SG2, PLACE%EQU_SG2, PLACE%N_SG2, &
     &                             EQU_SG2 )
      END IF
!
! --- Contribution from the global part
!
! --- Calculation of vector CONTR_GG = V_gg * a_g
!
      CALL MUL_MV_SR_R ( GLO, %VAL(AD_GG), &
     &                   GLO, PLACE%N_GLO, EQU_GLO,  PLACE%IND_GLO, &
     &                   GLO, PLACE%N_GLO, CONTR_GG, PLACE%IND_GLO, -3 )
!
! --- Calculation of vector CONTR_G1 = V_1g(T) * a_1
!
      CALL MUL_MV_TR_R ( SG1, GLO, %VAL(AD_1G), &
     &                   SG1, PLACE%N_SG1, EQU_SG1,  PLACE%IND_SG1, &
     &                   GLO, PLACE%N_GLO, CONTR_G1, PLACE%IND_GLO, -3 )
!
! --- Calcultion of vector CONTR_G2 = V_2g(T) * a_2
!
      CALL MUL_MV_TR_R ( SG2, GLO, %VAL(AD_2G), &
     &                   SG2, PLACE%N_SG2, EQU_SG2,  PLACE%IND_SG2, &
     &                   GLO, PLACE%N_GLO, CONTR_G2, PLACE%IND_GLO, -3 )
!
! --- Then CONTR_GG : = CONTR_GG + CONTR_G1
!
      CALL ADD_VV ( PLACE%N_GLO, CONTR_GG, CONTR_G1 )
!
! --- Then CONTR_GG : = CONTR_GG + CONTR_G1
!
      CALL ADD_VV ( PLACE%N_GLO, CONTR_GG, CONTR_G2 )
!
! --- Calcultion CONTR_GLO = CONTR_GG * ( CONTR_GG + CONTR_G1 + CONTR_G2 )
!
      CONTR_GLO = DP_VV_V ( PLACE%N_GLO, EQU_GLO, CONTR_GG )
!
! --- Contribution from the current segment part
!
! --- Calculation of vector CONTR_1G = V_1g * a_g
!
      CALL MUL_MV_IR_R ( SG1, GLO, %VAL(AD_1G), &
     &                   GLO, PLACE%N_GLO, EQU_GLO,  PLACE%IND_GLO, &
     &                   SG1, PLACE%N_SG1, CONTR_1G, PLACE%IND_SG1, -3 )
!
! --- Calculation of vector CONTR_11 = V_11 * a_1
!
      CALL MUL_MV_SR_R ( SG1, %VAL(AD_11), &
     &                   SG1, PLACE%N_SG1, EQU_SG1,  PLACE%IND_SG1, &
     &                   SG1, PLACE%N_SG1, CONTR_11, PLACE%IND_SG1, -3 )
!
! --- Calculation of vector CONTR_12 = V_21(T) * a_2
!
      CALL MUL_MV_TR_R ( SG2, SG1, %VAL(AD_21), &
     &                   SG2, PLACE%N_SG2, EQU_SG2,  PLACE%IND_SG2, &
     &                   SG1, PLACE%N_SG1, CONTR_12, PLACE%IND_SG1, -3 )
!
      CALL ADD_VV ( PLACE%N_SG1, CONTR_11, CONTR_1G )
      CALL ADD_VV ( PLACE%N_SG1, CONTR_11, CONTR_12 )
!
! --- Calcultion CONTR_CUR = CONTR_11 * ( CONTR_1G + CONTR_11 + CONTR_12 )
!
      CONTR_CUR = DP_VV_V ( PLACE%N_SG1, EQU_SG1, CONTR_11 )
!
! --- Contribution from the next segment part
!
! --- Calculation of vector CONTR_2G = V_2g * a_g
!
      CALL MUL_MV_IR_R ( SG2, GLO, %VAL(AD_2G), &
     &                   GLO, PLACE%N_GLO, EQU_GLO,  PLACE%IND_GLO, &
     &                   SG2, PLACE%N_SG2, CONTR_2G, PLACE%IND_SG2, -3 )
!
! --- Calculation of vector CONTR_21 = V_21 * a_1
!
      CALL MUL_MV_IR_R ( SG2, SG1, %VAL(AD_21), &
     &                   SG1, PLACE%N_SG1, EQU_SG1,  PLACE%IND_SG1, &
     &                   SG2, PLACE%N_SG2, CONTR_21, PLACE%IND_SG2, -3 )
!
! --- Calculation of vector CONTR_22 = V_22 * a_2
!
      CALL MUL_MV_SR_R ( SG2, %VAL(AD_22), &
     &                   SG2, PLACE%N_SG2, EQU_SG2,  PLACE%IND_SG2, &
     &                   SG2, PLACE%N_SG2, CONTR_22, PLACE%IND_SG2, -3 )
!
      CALL ADD_VV ( PLACE%N_SG2, CONTR_22, CONTR_2G )
      CALL ADD_VV ( PLACE%N_SG2, CONTR_22, CONTR_21 )
!
! --- Calcultion CONTR_NXT = CONTR_22 * ( CONTR_2G + CONTR_21 + CONTR_22 )
!
      CONTR_NXT = DP_VV_V ( PLACE%N_SG2, EQU_SG2, CONTR_22 )
!
! --- A(T) * V * A
!
      ATVA = CONTR_GLO + CONTR_CUR + CONTR_NXT
!
! --- Obtain indeces of the baseline and source
!
      IP_BAS = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                    NSTBA( INT4(OBSBAS(IOBS)%ISITE(1)), &
     &                           INT4(OBSBAS(IOBS)%ISITE(2)) ) )
      IND = OBSBAS(IOBS)%IND_SCA
      IP_SOU = OBSSCA(IND)%ISTAR
!
! --- Update of accumulators. NB: if ISGN=1, then the amount in accumulators
! --- will be increased, if ISGN=-1, then the amount will be decreased
!
! --- Firstly update of global accumulators
!
      CHIOBJ%CHIMAT_GLO = CHIOBJ%CHIMAT_GLO + ISGN*ATVA*RES(IOBS)%WEI_DEL**2
      CHIOBJ%CHIMA4_GLO = CHIOBJ%CHIMA4_GLO + ISGN*ATVA*RES(IOBS)%WEI_DEL**4
      CHIOBJ%CHI_GLO    = CHIOBJ%CHI_GLO    + ISGN* &
     &                      (RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL)**2
      CHIOBJ%WEI2_GLO   = CHIOBJ%WEI2_GLO + ISGN*RES(IOBS)%WEI_DEL**2
      CHIOBJ%NEQU_GLO   = CHIOBJ%NEQU_GLO + ISGN*1
!
! --- Then the same work for update of baseline-dependent accumulators
!
      CHIOBJ%CHIMAT_BAS(IP_BAS) = CHIOBJ%CHIMAT_BAS(IP_BAS) + ISGN* &
     &                              ATVA*RES(IOBS)%WEI_DEL**2
      CHIOBJ%CHIMA4_BAS(IP_BAS) = CHIOBJ%CHIMA4_BAS(IP_BAS) + ISGN* &
     &                              ATVA*RES(IOBS)%WEI_DEL**4
      CHIOBJ%CHI_BAS(IP_BAS)    = CHIOBJ%CHI_BAS(IP_BAS) + ISGN* &
     &                              (RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL)**2
      CHIOBJ%WEI2_BAS(IP_BAS)   = CHIOBJ%WEI2_BAS(IP_BAS) + ISGN* &
     &                                                      RES(IOBS)%WEI_DEL**2
      CHIOBJ%NEQU_BAS(IP_BAS)   = CHIOBJ%NEQU_BAS(IP_BAS) + ISGN*1
!
! --- Then the same work for update of source-dependent accumulators
!
      CHIOBJ%CHIMAT_SOU(IP_SOU) = CHIOBJ%CHIMAT_SOU(IP_SOU) + ISGN* &
     &                              ATVA*RES(IOBS)%WEI_DEL**2
      CHIOBJ%CHIMA4_SOU(IP_SOU) = CHIOBJ%CHIMA4_SOU(IP_SOU) + ISGN* &
     &                              ATVA*RES(IOBS)%WEI_DEL**4
      CHIOBJ%CHI_SOU(IP_SOU)    = CHIOBJ%CHI_SOU(IP_SOU)    + ISGN* &
     &                              (RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL)**2
      CHIOBJ%WEI2_SOU(IP_SOU)   = CHIOBJ%WEI2_SOU(IP_SOU) + ISGN* &
     &                                                      RES(IOBS)%WEI_DEL**2
      CHIOBJ%NEQU_SOU(IP_SOU)   = CHIOBJ%NEQU_SOU(IP_SOU) + ISGN*1
!
      DEALLOCATE ( VEC_ALLOCATED )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CHI_UPDATE  #!#
