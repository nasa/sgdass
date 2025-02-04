      SUBROUTINE RESPLINE_BSP ( FJDCT_BEG_BSP, FJDCT_END_BSP, L_BSP, I_BSP, &
     &                          BSP, L_STA, C_STA, TIM_BSP, VALSPL_BSP, &
     &                          COESPL_BSP, STAUSE_BSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RESPLINE_BSP  computes the coefficients of the            *
! *   interpolational spline over the time span of the observing session *
! *   for incremental including the model of site position displacements *
! *   presented as an expansion with the B-spline basis.                 *
! *                                                                      *
! *   NB: routine RESPLINE_BSP  **updates** array of site displacements  *
! *   at the nodes and **updates** array of spline interpolation         *
! *   coeffients. This means that the resulting arrays wilkl correspond  *
! *   to the **sum** of the previous model and the current one. If only  *
! *   one model is to be applied, the input values of VALSPL_BSP must    *
! *   be initialized. When RESPLINE_BPS is called for the first model    *
! *   the output arrays correspond to displacments of this model.        *
! *   When RESPLINE_BPS is called to the second model the output array   *
! *   correspnd to the sum of the first and the second model, and so on. *
! *                                                                      *
! *   Each BSP model defines expansion for only one station. It is       *
! *   possible that displacements for some stations will remain          *
! *   undefined. RESPLINE_BSP computes interpolation polynomial for      *
! *   the station defined in this model, keaping section of the array    *
! *   which correspond to other stations unmodified.                      *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FJDCT_BEG_BSP ( REAL*8    ) -- Julian data of the beginning of       *
! *                                interpolation interval. NB: it is     *
! *                                selected to be a little bit before    *
! *                                the nominal session start.            *
! * FJDCT_END_BSP ( REAL*8    ) -- Julian data of the end of             *
! *                                interpolation interval. NB: it is     *
! *                                selected to be a little bit after     *
! *                                the nominal session end.              *
! *         L_BSP ( INTEGER*4 ) -- The total number of models of site    *
! *                                displacements presetned in the form   *
! *                                of expansion with the B-spline basis. *
! *         I_BSP ( INTEGER*4 ) -- The index of the model of site        *
! *                                displacements presetned in the form   *
! *                                of expansion with the B-spline basis  *
! *                                under consideration.                  *
! *           BSP ( RECORD    ) -- Array of the objects which describe   *
! *                                site  position evolution modeled      *
! *                                with a spline. Dimension: L_BSP.      *
! *         L_STA ( INTEGER*4 ) -- The total number of stations          *
! *                                participated in the experiment.       *
! *         C_STA ( CHARACTER ) -- List of stations particiapted in the  *
! *                                experiment. Dimension: L_BSP.         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       TIM_BSP ( REAL*8    ) -- Array of time epoch for which         *
! *                                interpolation spline will be computed.*
! *                                Dimension: M__BSP_INT. Units: days.   *
! *    COESPL_BSP ( REAL*8    ) -- Array of spline interpolation         *
! *                                cofficients which correspond to the   *
! *                                model under consideration and all     *
! *                                previouly applied models.             *
! *                                Dimension:  M__BSP_INT,3,MAX_ARC_STA. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    VALSPL_BSP ( REAL*8    ) -- Array of site displacements at time   *
! *                                epochs of nodes. Routine RESPLINE_BSP *
! *                                adds the values of displacements      *
! *                                defined in the I_BSP model to         *
! *                                existing values.                      *
! *                                Dimension:  M__BSP_INT,3,MAX_ARC_STA. *
! *    STAUSE_BSP ( LOGICAL*1 ) -- Array of station usage. RESPLINE_BSP  *
! *                                sets .TRUE. for the station for       *
! *                                which to I_BSP model defines          *
! *                                displacements, keeping other elements *
! *                                unmodified. Dimension: L_STA.         *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 15-MAR-2005  RESPLINE_BSP  v1.0 (c)  L. Petrov 01-APR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'bsp.i'
      REAL*8     FJDCT_BEG_BSP, FJDCT_END_BSP, TIM_BSP(M__BSP_INT), &
     &           VALSPL_BSP(M__BSP_INT,3,MAX_ARC_STA), &
     &           COESPL_BSP(M__BSP_INT,3,MAX_ARC_STA)
      INTEGER*4  L_BSP, I_BSP, L_STA, IUER
      TYPE       ( BSPSTA__TYPE ) :: BSP(L_BSP)
      CHARACTER  C_STA(L_STA)*(*)
      LOGICAL*1  STAUSE_BSP(L_STA)
      REAL*8     SPL_ORIG(1-M__SPD:M__SPN,3), WORK_ARR(M__BSP_INT), &
     &           TIM_ARG, TIM_STEP
      CHARACTER  STA_NAM*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, ISTA, IER
      REAL*8,    EXTERNAL :: BSPL_VAL 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! --- GEt I_STA -- station index in he L_STA/C_STA array
!
      STA_NAM = BSP(I_BSP)%STATION
      CALL UNDSCR ( STA_NAM )
      ISTA = LTM_DIF ( 1, L_STA, C_STA, STA_NAM )
      IF ( ISTA < 1 ) THEN
!
! -------- The station not found? This miean it did not observe in this session.
! -------- Nothing to do.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      DO 410 J1=1-BSP(I_BSP)%DEGREE,BSP(I_BSP)%L_NOD-1
         DO 420 J2=1,3
            SPL_ORIG(J1,J2) =BSP(I_BSP)%POS(J2,J1)
 420     CONTINUE 
 410  CONTINUE 
!
! --- Compute time step
!
      TIM_STEP = (FJDCT_END_BSP - FJDCT_BEG_BSP)/(M__BSP_INT-1)
      DO 430 J3=1,M__BSP_INT
         TIM_BSP(J3) = TIM_STEP*(J3-1) 
!
! ------ Compute time argment at the J3-th node: time elapsed from J2000.0
! ------ epoch in seconds
!
         TIM_ARG = ( FJDCT_BEG_BSP - J2000__JD )*86400.0D0 + &
     &               TIM_BSP(J3)*86400.0D0
         DO 440 J4=1,3
            DO 450 J5=1-BSP(I_BSP)%DEGREE,BSP(I_BSP)%L_NOD-1
!
! ------------ Update displacements
!
               VALSPL_BSP(J3,J4,ISTA) = VALSPL_BSP(J3,J4,ISTA) + &
     &                BSP(I_BSP)%POS(J4,J5)* &
     &                BSPL_VAL ( BSP(I_BSP)%L_NOD, BSP(I_BSP)%TIM(1), &
     &                           BSP(I_BSP)%DEGREE, J5, TIM_ARG )
 450        CONTINUE 
 440     CONTINUE 
 430  CONTINUE 
!
! --- For each component compute spline coefficients which interpolate 
! --- displacements defined in VALSPL_BSP at time epochs TIM_BSP
!
      DO 460 J6=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL MAKE_SPLINE ( 3, M__BSP_INT, TIM_BSP, VALSPL_BSP(1,J6,ISTA), &
     &                      0.0D0, 0.0D0, COESPL_BSP(1,J6,ISTA), WORK_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3751, IUER, 'RESPLNE_BSP', 'Error in an '// &
     &            'to compute spline coefficients for in-session '// &
     &            'interpolation' )
              RETURN 
         END IF
 460  CONTINUE 
!
      STAUSE_BSP(ISTA) = .TRUE.
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  RESPLINE_BSP 
