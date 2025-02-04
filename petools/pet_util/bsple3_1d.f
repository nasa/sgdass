      SUBROUTINE BSPLE3_1D_CMP ( M, ARG_ARR, VAL_ARR, BCF_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  BSPLE3_1D_CMP  performs a fast computation of            *
! *   of the 1-dimensional B-spline transform of the 3rd degree of       *
! *   a one-dimensional function. The interpolated function is given     *
! *   at a increasing sequence of knots. Initial conditions: for the     *
! *   left side (ARG(1)): the first derivative is equal to the first     *
! *   difference; for the right side (ARG(M)): the first derivative      *
! *   is equal to the first difference.                                  *
! *                                                                      *
! *   Important comments:                                                *
! *   1) This routine does not support a sequence with multiple knots!   *
! *   2) Array of arguments should be sized as [-2:M+3]. Arguments       *
! *      with indices -2, 1, 0, M+1, M+2, M+3 are undefined on entry.    *
! *      They are filled at exit.                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       M ( INTEGER*4 ) -- Dimension of the function.                  *
! * VAL_ARR ( REAL*8    ) -- Array of values of function. Dimension:     *
! *                          [1:M].                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * BCF_ARR ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          [-2:M-1].                                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * ARG_ARR ( REAL*8    ) -- Array of arguments over the first           *
! *                          dimension. Dimension: [-2:M+3].             *
! *                          Elements with indices -2, 1, 0, M+1, M+2,   *
! *                          M+3 are modified.                           *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 02-MAR-2009   BSPLE3_1D_CMP   v3.0 (c) L. Petrov 31-MAR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M, IUER
      INTEGER*4  DEG
      PARAMETER  ( DEG = 3 )
      REAL*8     ARG_ARR(1-DEG:M+DEG-1), VAL_ARR(M)
      REAL*8     BCF_ARR(1-DEG:M-1)
      REAL*8     EPS
      PARAMETER  ( EPS    = 1.D-12 )
      REAL*4     EQU(2), DER, MIN_STEP
      INTEGER*4  J1, J2, J3, J4, IER
      CHARACTER  STR*32, STR1*32, STR2*32
      REAL*8,    ALLOCATABLE :: MAT_B8(:,:)
      REAL*8,    EXTERNAL  :: BSPLE3_VAL, BSPLE3_DER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( M < 5 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M, STR)
           CALL ERR_LOG ( 1781, IUER, 'BSPLE3_1D_CMP', 'Too small '// &
     &         'argument M: '//STR(1:I_LEN(STR))//' -- it should be '// &
     &         'at least 5' )
           RETURN 
      END IF
!
! --- Allocate dynamic memory for the band matrix of interpolation 
! --- equations
!
      ALLOCATE ( MAT_B8(DEG,2:M-1), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*DEG*(M-1), STR)
           CALL ERR_LOG ( 1782, IUER, 'BSPLE3_1D_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
! --- Set the step fo exteneded knots. Strictly speaking, the step should be zero.
! --- In order to avoid 0/0, we set it to a very small number
!
      MIN_STEP = EPS*(ARG_ARR(M) - ARG_ARR(1))
      DO 410 J1=2,M
         IF ( (ARG_ARR(J1) - ARG_ARR(J1-1)) < MIN_STEP ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL CLRCH ( STR2 )
              CALL INCH  ( J1-1, STR  )
              CALL INCH  ( J1,   STR1 )
              WRITE ( UNIT=STR(1:15), FMT='(F15.8)' ) ARG_ARR(J1) - ARG_ARR(J1-1)
              CALL ERR_LOG ( 1783, IUER, 'BSPLE3_1D_CMP_R4', 'The difference '// &
     &           'in arguments '//STR(1:I_LEN(STR))//' and '//STR1(1:I_LEN(STR1))// &
     &           ' is too small: '//STR2(1:I_LEN(STR2))//' -- the knot sequence '// &
     &           ' should be increasing' )
              RETURN 
         END IF
 410  CONTINUE 
!
! --- Extend the knot sequence
!
      ARG_ARR(0)   = ARG_ARR(1)   - MIN_STEP
      ARG_ARR(-1)  = ARG_ARR(0)   - MIN_STEP
      ARG_ARR(-2)  = ARG_ARR(-1)  - MIN_STEP
      ARG_ARR(M+1) = ARG_ARR(M)   + MIN_STEP
      ARG_ARR(M+2) = ARG_ARR(M+1) + MIN_STEP
      ARG_ARR(M+3) = ARG_ARR(M+2) + MIN_STEP
!
! --- Compute two values of the B-spline transform at the left edge of the sequence
!
      BCF_ARR(-2) = VAL_ARR(1)/BSPLE3_VAL ( M, ARG_ARR, -2, ARG_ARR(1) )
!
      EQU(1) = BSPLE3_DER ( M, ARG_ARR, -2, ARG_ARR(1) )
      EQU(2) = BSPLE3_DER ( M, ARG_ARR, -1, ARG_ARR(1) )
!
! --- Set boundary condition at the left side
!
      DER = (VAL_ARR(2) - VAL_ARR(1))/(ARG_ARR(2) - ARG_ARR(1))
      BCF_ARR(-1) = (DER - EQU(1)*BCF_ARR(-2))/EQU(2)
!
! --- Compute two values of the B-spline transform at the right edge
!
      BCF_ARR(M-1) = VAL_ARR(M)/BSPLE3_VAL ( M, ARG_ARR, M-1, ARG_ARR(M) )
!
      EQU(1) = BSPLE3_DER ( M, ARG_ARR, M-1, ARG_ARR(M) )
      EQU(2) = BSPLE3_DER ( M, ARG_ARR, M-2, ARG_ARR(M) )
!
! --- Set boundary condition at the right side
!
      DER = (VAL_ARR(M) - VAL_ARR(M-1))/(ARG_ARR(M) - ARG_ARR(M-1))
      BCF_ARR(M-2) = (DER - EQU(1)*BCF_ARR(M-1))/EQU(2)
!
! --- Fill the tri-diagonal matrix
!
      MAT_B8(2,2) = BSPLE3_VAL ( M, ARG_ARR, 0, ARG_ARR(2) )
      MAT_B8(3,2) = BSPLE3_VAL ( M, ARG_ARR, 1, ARG_ARR(2) )
      BCF_ARR(0) = VAL_ARR(2) - BCF_ARR(-1)*BSPLE3_VAL ( M, ARG_ARR, -1, ARG_ARR(2) )
      DO 420 J2=3,M-2
         MAT_B8(1,J2) = BSPLE3_VAL ( M, ARG_ARR, J2-3, ARG_ARR(J2) )
         MAT_B8(2,J2) = BSPLE3_VAL ( M, ARG_ARR, J2-2, ARG_ARR(J2) ) 
         MAT_B8(3,J2) = BSPLE3_VAL ( M, ARG_ARR, J2-1, ARG_ARR(J2) )
         BCF_ARR(J2-2) = VAL_ARR(J2)
 420  CONTINUE 
      MAT_B8(1,M-1) = BSPLE3_VAL ( M, ARG_ARR, M-4, ARG_ARR(M-1) )
      MAT_B8(2,M-1) = BSPLE3_VAL ( M, ARG_ARR, M-3, ARG_ARR(M-1) )
      BCF_ARR(M-3) = VAL_ARR(M-1) - BCF_ARR(M-2)*BSPLE3_VAL ( M, ARG_ARR, M-2, ARG_ARR(M-1) )
!
! --- Decomposition run
!
      DO 430 J3=3,M-1
         MAT_B8(2,J3) = MAT_B8(2,J3) - MAT_B8(1,J3)/MAT_B8(2,J3-1)* MAT_B8(3,J3-1)
         BCF_ARR(J3-2)    = BCF_ARR(J3-2)    - MAT_B8(1,J3)/MAT_B8(2,J3-1)* BCF_ARR(J3-3)
 430  CONTINUE 
!
! --- Backward substituion run
!
      BCF_ARR(M-3) = BCF_ARR(M-3)/MAT_B8(2,M-1)
      DO 440 J4=M-2,2,-1
         BCF_ARR(J4-2) = (BCF_ARR(J4-2) - MAT_B8(3,J4)*BCF_ARR(J4-1))/MAT_B8(2,J4)
 440  CONTINUE 
!
! --- Deallocate the matrices
!
      DEALLOCATE ( MAT_B8  )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPLE3_1D_CMP  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION VAL_1D_BSPLE3 ( ARG, M, IND, ARG_ARR, BCF_ARR )
! ************************************************************************
! *                                                                      *
! *     Routine VAL_1D_BSPLE3  computes the value of the expansion of    *
! *   a function into the basis B-splines of the 3rd degree defined at   *
! *   the extended sequence of knots at the point with argument ARG.     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- Argument of the point.                      *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*8    ) -- Array of arguments. Dimension: [-2:M+3].    *
! *                          NB: when the pivotal index is computed,     *
! *                          the fact that array ARG_ARR has index       *
! *                          starting from -2, must be taken into        *
! *                          account.                                    *
! * BCF_ARR ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          [1-DEG:M-1].                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * VAL_1D_BSPLE3 ( REAL*8 ) -- Value of the expansion at argment ARG.   *
! *                                                                      *
! *  ### 02-MAR-2009 VAL_1D_BSPLE3  v2.0 (c) L. Petrov  31-MAR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M, DEG
      PARAMETER  ( DEG = 3 )
      REAL*8     ARG, ARG_ARR(1-DEG:M+DEG), BCF_ARR(1-DEG:M-1)
      REAL*8     VAL_1D_BSPLE3
      REAL*8     BSPL(-DEG:0)
      INTEGER*4  J1, J2 
      REAL*8,    EXTERNAL :: BSPLE3_VAL 
!
      IF ( IND < 1 .OR. IND > M ) THEN
           VAL_1D_BSPLE3 = -1.0D30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         BSPL(J1) = BSPLE3_VAL ( M, ARG_ARR, IND+J1, ARG )
 410  CONTINUE 
!
      VAL_1D_BSPLE3 = 0.0
      DO 420 J2=-DEG,0
         VAL_1D_BSPLE3 = VAL_1D_BSPLE3 + BSPL(J2)*BCF_ARR(IND+J2)
 420  CONTINUE 
!
      RETURN
      END  FUNCTION  VAL_1D_BSPLE3  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_DER_1D_BSPLE3 ( ARG, M, IND, ARG_ARR, BCF_ARR, &
     &                               VAL, DER )
! ************************************************************************
! *                                                                      *
! *      Routine VAL_1D_BSPLE3  computes the value and the first         *
! *   derivative of the expansion of a function into the basis B-splines *
! *   of the 3rd degree defined at the extended sequence of knots        *
! *   at the point with argument ARG.                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*8    ) -- Array of arguments. Dimension: [-2:M+3].    *
! *                          NB: when the pivotal index is computed,     *
! *                          the fact that array ARG_ARR has index       *
! *                          starting from -2, must be taken into        *
! *                          account.                                    *
! * BCF_ARR ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          [1-DEG:M-1].                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     VAL ( REAL*8    ) -- Value of the expansion at argment ARG.      *
! *     DER ( REAL*8    ) -- The first derivative of the expansion at    *
! *                          argument ARG.                               *
! *                                                                      *
! * ### 02-MAR-2009  VAL_DER_1D_BSPLE3 v2.0 (c) L. Petrov 31-MAR-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M
      INTEGER*4    DEG
      PARAMETER  ( DEG = 3)
      REAL*8     ARG, ARG_ARR(1-DEG:M+DEG), BCF_ARR(1-DEG:M-1)
      REAL*8     VAL, DER
      REAL*8     BSPL(-DEG:0), DSPL(-DEG:0)
      INTEGER*4  J1, J2 
      REAL*8,    EXTERNAL :: BSPLE3_VAL, BSPLE3_DER
!
      IF ( IND < 1 .OR. IND > M ) THEN
           VAL = -1.0D30
           DER = -1.0D30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         BSPL(J1) = BSPLE3_VAL ( M, ARG_ARR, IND+J1, ARG )
         DSPL(J1) = BSPLE3_DER ( M, ARG_ARR, IND+J1, ARG )
 410  CONTINUE 
!
      VAL = 0.0
      DER = 0.0
      DO 420 J2=-DEG,0
         VAL = VAL + BSPL(J2)*BCF_ARR(IND+J2)
         DER = DER + DSPL(J2)*BCF_ARR(IND+J2) 
 420  CONTINUE 
!
      RETURN
      END  SUBROUTINE  VAL_DER_1D_BSPLE3  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_DE2_1D_BSPLE3 ( ARG, M, IND, ARG_ARR, BCF_ARR, &
     &                               VAL, DER, DR2 )
! ************************************************************************
! *                                                                      *
! *     Routine  VAL_1D_BSPLE3  computes the value, the first, and the   *
! *   second derivatives of the expansion of a function into the basis   *
! *   B-splines of the 3rd degree defined at the extended sequence of    *
! *   knots at the point with argument ARG.                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*8    ) -- Array of arguments. Dimension: [-2:M+3].    *
! *                          NB: when the pivotal index is computed,     *
! *                          the fact that array ARG_ARR has index       *
! *                          starting from -2, must be taken into        *
! *                          account.                                    *
! * BCF_ARR ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          [1-DEG:M-1].                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     VAL ( REAL*8    ) -- Value of the expansion at argment ARG.      *
! *     DER ( REAL*8    ) -- The first derivative of the expansion at    *
! *                          argument ARG.                               *
! *     DR2 ( REAL*8    ) -- The second derivative of the expansion at   *
! *                          argument ARG.                               *
! *                                                                      *
! * ### 02-MAR-2009  VAL_DER_1D_BSPL v2.0 (c) L. Petrov 31-MAR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M
      INTEGER*4    DEG
      PARAMETER  ( DEG = 3 )
      REAL*8     ARG, ARG_ARR(1-DEG:M+DEG), BCF_ARR(1-DEG:M-1)
      REAL*8     VAL, DER, DR2
      REAL*8     BSPL(-DEG:0), DSPL(-DEG:0), DSP2(-DEG:0)
      INTEGER*4  J1, J2 
      REAL*8,    EXTERNAL :: BSPLE3_VAL, BSPLE3_DER, BSPLE3_DR2
!
      IF ( IND < 1 .OR. IND > M ) THEN
           VAL = -1.0D30
           DER = -1.0D30
           DR2 = -1.0D30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         BSPL(J1) = BSPLE3_VAL ( M, ARG_ARR, IND+J1, ARG )
         DSPL(J1) = BSPLE3_DER ( M, ARG_ARR, IND+J1, ARG )
         DSP2(J1) = BSPLE3_DR2 ( M, ARG_ARR, IND+J1, ARG )
 410  CONTINUE 
!
      VAL = 0.0
      DER = 0.0
      DR2 = 0.0
      DO 420 J2=-DEG,0
         VAL = VAL + BSPL(J2)*BCF_ARR(IND+J2)
         DER = DER + DSPL(J2)*BCF_ARR(IND+J2) 
         DR2 = DR2 + DSP2(J2)*BCF_ARR(IND+J2) 
 420  CONTINUE 
!
      RETURN
      END  SUBROUTINE  VAL_DE2_1D_BSPLE3  !#!#
