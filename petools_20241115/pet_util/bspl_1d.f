#include <mk5_preprocessor_directives.inc>
      SUBROUTINE BSPL_1D_CMP ( DEG, BC_CODE, M, ARG_ARR, BCF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BSPL_1D_CMP  computes coefficients of the 1-dimensional   *
! *   interpolation B-spline spline for a one-dimensional function. The  *
! *   interpolated fiunction is given at nodes of a regular mesh that    *
! *   forms the rectangular. Boundary conditions on the first derivative *
! *   of the spline at the border of the iterval are applied in          *
! *   accordance with parameter BC_CODE.                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *   BC_CODE ( INTEGER*4 ) -- Code of boundary conditions.              *
! *                       Meaning:                                       *
! *                       0 -- for the left side: the first derivative   *
! *                            is equal to the first difference;         *
! *                            for the right side: the first derivative  *
! *                            is equal to the first difference,         *
! *                       1 -- for both sides the first derivative is    *
! *                            equal to the mean value of first          *
! *                            difference computed for the left and      *
! *                            right sides.                              *
! *                       2 -- for the left side the first derivative    *
! *                            is zero; for the right side: the first    *
! *                            derivative  is equal to the first         *
! *                            difference.                               *
! *                       3 -- for the left side: the first derivative   *
! *                            is equal to the first difference;         *
! *                            for the right side: zero.                 *
! *                       For the most cases code of boundary conditions *
! *                       0 is adequate.                                 *
! *         M ( INTEGER*4 ) -- Dimension of the function.                *
! *   ARG_ARR ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: M.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       BCF ( REAL*8    ) -- On input: array of values of function.    *
! *                            On output: array of B-spline coefficients.*
! *                            Dimension: (1-DEG:M).                     *
! *                            NB: 1) elements with dimensions < 1 are   *
! *                                   not defined on input.              *
! *                                2) Elements with indexes              *
! *                                   at 1st dimension equal to M        *
! *                                   are not defined at ouptut.         *
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
! *  ### 02-MAR-2009   BSPL_1D_CMP  v2.1 (c) L. Petrov  25-OCT-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, BC_CODE, M, IUER
      REAL*8     ARG_ARR(M)
      REAL*8     BCF(1-DEG:M)
      REAL*8     EQU
      REAL*8     EPS
      PARAMETER  ( EPS    = 1.D-15 )
      INTEGER*4  I, J, LOC_TO_BAND
      INTEGER*4  J1, J2, J3, J4, IER
      CHARACTER  STR*32
      REAL*8,    EXTERNAL  :: BSPL_VAL, BSPL_DER
      INTEGER*4, ALLOCATABLE :: IPIV(:)
      REAL*8,    ALLOCATABLE :: MAT_B4(:), RH(:), BC(:)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      LOC_TO_BAND(I,J,DEG) = I-J+DEG + (DEG+1)*(DEG+J-1)
!
! --- Allocate dynamic memory for the band matrix of interpolation 
! --- equations
!
      ALLOCATE ( MAT_B4((DEG+1)*(M+DEG-1)), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(DEG+1)*(M+DEG-1), STR)
           CALL ERR_LOG ( 1781, IUER, 'BSPL_1D_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
! --- Allocate dynamic memory for the temporary array 
!
      ALLOCATE ( BC(0:M+DEG-1), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(M+DEG-1)*(M+DEG-1), STR)
           CALL ERR_LOG ( 1712, IUER, 'BSPL_1D_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
! --- Zeroing the temprorary array
!
      BC = 0.0
!
! --- Allocate memory for indexes of pivotal elements
!
      ALLOCATE ( IPIV(M+DEG-1), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(M+DEG-1), STR)
           CALL ERR_LOG ( 1713, IUER, 'BSPL_1D_CMP', 'Failure to '// &
     &         'allocate bytes of dynamic memory' )
           RETURN 
      END IF
      IPIV = 0
!
! --- Build the matrix of interpolation equations, including two equations for 
! --- boundary conditions
!
      DO 410 J1=1-DEG,M-1
         DO 420 J2=MAX(J1+1,0),MIN(J1+DEG,M+1)
            IF ( J2 == 0 ) THEN
                 EQU = BSPL_DER ( M, ARG_ARR, DEG, J1, ARG_ARR(1) )
               ELSE IF ( J2 == M+1 ) THEN
                 EQU = BSPL_DER ( M, ARG_ARR, DEG, J1, ARG_ARR(M)*(1-EPS) )
               ELSE 
                 IF ( J2 == M ) THEN
                      EQU = BSPL_VAL ( M, ARG_ARR, DEG, J1, ARG_ARR(M)*(1-EPS) )
                    ELSE
                      EQU = BSPL_VAL ( M, ARG_ARR, DEG, J1, ARG_ARR(J2) )
                 END IF
            END IF
!
! --------- Put the element of the equation into the band matrix (J1-1 + DEG*(J2+2))
!
            MAT_B4(LOC_TO_BAND(J1,J2-DEG+1,DEG)) = EQU
 420     CONTINUE 
 410  CONTINUE 
!
! --- Build the right-band sides of interpolating equations. 
!
      DO 430 J3=1,M
         BC(J3) = BCF(J3)
 430  CONTINUE 
!
! --- Two boundary condition imposed to first derivatives at the 
! --- the first point (left side) and the last point (right side)
!
      IF ( BC_CODE == 0 ) THEN
!
! -------- At the left side: the first derivative is equal to 
! -------- the first difference at the left side
!
! -------- At the right side: the first derivative is equal to 
! -------- the first difference at the right side
!
           BC(0)   = (BCF(2) - BCF(1)  )/(ARG_ARR(2) - ARG_ARR(1))
           BC(M+1) = (BCF(M) - BCF(M-1))/(ARG_ARR(M) - ARG_ARR(M-1))
         ELSE IF ( BC_CODE == 1 ) THEN
           BC(0)   = ( (BCF(2) - BCF(1)  )/(ARG_ARR(2) - ARG_ARR(1)) + &
    &                  (BCF(M) - BCF(M-1))/(ARG_ARR(M) - ARG_ARR(M-1))  )/2.0
           BC(M+1) = BC(0)   
         ELSE IF ( BC_CODE == 2 ) THEN
!
! -------- At the left side: zero
!
! -------- At the right side: the first derivative is equal to 
! -------- the first difference at the rightside
!
           BC(0)   = 0.0
           BC(M+1) = (BCF(M) - BCF(M-1))/(ARG_ARR(M) - ARG_ARR(M-1))
         ELSE IF ( BC_CODE == 3 ) THEN
!
! -------- At the left side: the first derivative is equal to 
! -------- the first difference at the left side
!
! -------- At the right side: zero
!
           BC(0)   = (BCF(2) - BCF(1)  )/(ARG_ARR(2) - ARG_ARR(1))
           BC(M+1) = (BCF(M) - BCF(M-1))/(ARG_ARR(M) - ARG_ARR(M-1))
      END IF
!
! --- Decompose the band matrix
!
      CALL DGBTRF ( M+DEG-1, M+DEG-1, 1, 1, MAT_B4, 4, IPIV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1222, IUER, 'BSPL_1D_CMP', 'Error in DGBTRF '// &
     &          'INFO= '//STR )
           RETURN 
      END IF
!
! --- Solve interploation equations with am array of right hand sides and
! --- then put the updated coefficients into array BCF. 
!
      CALL DGBTRS ( 'T', M+DEG-1, 1, 1, 1, MAT_B4, 4, IPIV, BC, M+DEG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1223, IUER, 'BSPL_1D_CMP', 'Error in '// &
     &          'DGBTRS INFO= '//STR )
           RETURN 
      END IF
      DO 440 J4=1-DEG,M-1
         BCF(J4) = BC(J4+DEG-1)
 440  CONTINUE 
!
! --- Deallocate the matrices
!
      DEALLOCATE ( MAT_B4 )
      DEALLOCATE ( IPIV  )
      DEALLOCATE ( BC    )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPL_1D_CMP  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION VAL_1D_BSPL ( ARG, M, DEG, IND, ARG_ARR, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_1D_BSPL  computes the value of the spline of the       *
! *   DEG th degree at the point with coordinate ARG_1.                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*8    ) -- Array of arguments. Dimension: M.           *
! *     BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * VAL_1D_BSPL ( REAL*8 ) -- Value of the B-spline function.            *
! *                                                                      *
! *  ### 02-MAR-2009  VAL_1D_BSPL  v1.0  (c) L. Petrov  02-MAR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M, DEG
      REAL*8     ARG, ARG_ARR(M), BCF(1-DEG:M)
      REAL*8     VAL_1D_BSPL 
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*8     BSPL(-MDEG:0)
      INTEGER*4  J1, J2 
      REAL*8,    EXTERNAL :: BSPL_VAL 
!
      IF ( IND < 1 .OR. IND > M ) THEN
           VAL_1D_BSPL = -1.0E30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         BSPL(J1) = BSPL_VAL ( M, ARG_ARR, DEG, IND+J1, ARG )
 410  CONTINUE 
!
      VAL_1D_BSPL = 0.0
      DO 420 J2=-DEG,0
         VAL_1D_BSPL = VAL_1D_BSPL + BCF(IND+J2)*BSPL(J2)
 420  CONTINUE 
!
      RETURN
      END  FUNCTION VAL_1D_BSPL  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION DER_1D_BSPL ( ARG, M, DEG, IND, ARG_ARR, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine DER_1D_BSPL  computes the derivative of the spline of the  *
! *   DEG th degree at the point with coordinate ARG_1.                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*8    ) -- Array of arguments. Dimension: M.           *
! *     BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * DER_1D_BSPL ( REAL*8 ) -- Derivative of the B-spline function.       *
! *                                                                      *
! *  ### 02-MAR-2009  DER_1D_BSPL  v1.0  (c) L. Petrov  08-JUL-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M, DEG
      REAL*8     ARG, ARG_ARR(M), BCF(1-DEG:M)
      REAL*8     DER_1D_BSPL 
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*8     DER_BSPL(-MDEG:0)
      INTEGER*4  J1, J2 
      REAL*8,    EXTERNAL :: BSPL_DER
!
      IF ( IND < 1 .OR. IND > M ) THEN
           DER_1D_BSPL = -1.0E30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         DER_BSPL(J1) = BSPL_DER ( M, ARG_ARR, DEG, IND+J1, ARG )
 410  CONTINUE 
!
      DER_1D_BSPL = 0.0
      DO 420 J2=-DEG,0
         DER_1D_BSPL = DER_1D_BSPL + BCF(IND+J2)*DER_BSPL(J2)
 420  CONTINUE 
!
      RETURN
      END  FUNCTION DER_1D_BSPL  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION DR2_1D_BSPL ( ARG, M, DEG, IND, ARG_ARR, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine DR2_1D_BSPL  computes the second derivative of the spline  *
! *   of the DEG th degree at the point with coordinate ARG_1.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*8    ) -- Array of arguments. Dimension: M.           *
! *     BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * DR2_1D_BSPL ( REAL*8 ) -- 2nd derivative of the B-spline function.   *
! *                                                                      *
! *  ### 02-MAR-2009  DR2_1D_BSPL  v1.0  (c) L. Petrov  08-JUL-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M, DEG
      REAL*8     ARG, ARG_ARR(M), BCF(1-DEG:M)
      REAL*8     DR2_1D_BSPL 
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*8     DR2_BSPL(-MDEG:0)
      INTEGER*4  J1, J2 
      REAL*8,    EXTERNAL :: BSPL_DR2
!
      IF ( IND < 1 .OR. IND > M ) THEN
           DR2_1D_BSPL = -1.0E30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         DR2_BSPL(J1) = BSPL_DR2 ( M, ARG_ARR, DEG, IND+J1, ARG )
 410  CONTINUE 
!
      DR2_1D_BSPL = 0.0
      DO 420 J2=-DEG,0
         DR2_1D_BSPL = DR2_1D_BSPL + BCF(IND+J2)*DR2_BSPL(J2)
 420  CONTINUE 
!
      RETURN
      END  FUNCTION DR2_1D_BSPL  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_DER_1D_BSPL ( ARG, M, DEG, IND, ARG_ARR, BCF, VAL, DER )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_DER_1D_BSPL  computes the value of the spline of the   *
! *   DEG th degree at the point with coordinate ARG_ARR and the first   *
! *   derivative of the spline.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*8    ) -- Array of arguments. Dimension: M.           *
! *     BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * ### 02-MAR-2009 VAL_DER_1D_BSPL v1.0 (c) L. Petrov 02-MAR-2009 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M, DEG
      REAL*8     ARG, ARG_ARR(M), BCF(1-DEG:M)
      REAL*8     VAL, DER
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*8     BSPL(-MDEG:0), DSPL(-MDEG:0)
      INTEGER*4  J1, J2 
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER 
!
      IF ( IND < 1 .OR. IND > M ) THEN
           VAL = -1.0E30
           DER = -1.0E30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         BSPL(J1) = BSPL_VAL ( M, ARG_ARR, DEG, IND+J1, ARG )
         DSPL(J1) = BSPL_DER ( M, ARG_ARR, DEG, IND+J1, ARG )
 410  CONTINUE 
!
      VAL = 0.0
      DER = 0.0
      DO 420 J2=-DEG,0
         VAL = VAL + BSPL(J2)*BCF(IND+J2)
         DER = DER + DSPL(J2)*BCF(IND+J2) 
 420  CONTINUE 
!
      RETURN
      END  SUBROUTINE VAL_DER_1D_BSPL !#!#
