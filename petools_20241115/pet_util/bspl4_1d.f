#include <mk5_preprocessor_directives.inc>
      SUBROUTINE BSPL4_1D_CMP ( DEG, BC_CODE, M, ARG_ARR, BCF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BSPL4_1D_CMP  computes coefficients of the 1-dimensional  *
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
! *   ARG_ARR ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: M.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       BCF ( REAL*4    ) -- On input: array of values of function.    *
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
! *  ### 02-MAR-2009  BSPL4_1D_CMP  v2.0 (c) L. Petrov  19-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, BC_CODE, M, IUER
      REAL*4     ARG_ARR(M)
      REAL*4     BCF(1-DEG:M)
      REAL*4     EQU
      REAL*4     EPS
      PARAMETER  ( EPS    = 1.E-6 )
      INTEGER*4  I, J, LOC_TO_BAND
      INTEGER*4  J1, J2, J3, J4, IER
      CHARACTER  STR*32
      REAL*4,    EXTERNAL  :: BSPL4_VAL, BSPL4_DER
      INTEGER*4, ALLOCATABLE :: IPIV(:)
      REAL*4,    ALLOCATABLE :: MAT_B4(:), RH(:), BC(:)
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
           CALL ERR_LOG ( 1781, IUER, 'BSPL4_1D_CMP', 'Failure to '// &
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
           CALL ERR_LOG ( 1712, IUER, 'BSPL4_1D_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
! --- Zeroing the temporary array
!
      BC = 0.0
!
! --- Allocate memory for indexes of pivotal elements
!
      ALLOCATE ( IPIV(M+DEG-1), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(M+DEG-1), STR)
           CALL ERR_LOG ( 1713, IUER, 'BSPL4_1D_CMP', 'Failure to '// &
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
                 EQU = BSPL4_DER ( M, ARG_ARR, DEG, J1, ARG_ARR(1) )
               ELSE IF ( J2 == M+1 ) THEN
                 EQU = BSPL4_DER ( M, ARG_ARR, DEG, J1, ARG_ARR(M)*(1.0E0-EPS) )
               ELSE 
                 EQU = BSPL4_VAL ( M, ARG_ARR, DEG, J1, ARG_ARR(J2) )
                 IF ( J2 == M ) THEN
                      EQU = BSPL4_VAL ( M, ARG_ARR, DEG, J1, ARG_ARR(J2)*(1.0E0-EPS) )
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
! --- They are different for different dimensions
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
      CALL SGBTRF ( M+DEG-1, M+DEG-1, 1, 1, MAT_B4, 4, IPIV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1222, IUER, 'BSPL4_1D_CMP', 'Error in SGBTRF '// &
     &          'INFO= '//STR )
           RETURN 
      END IF
!
! --- Solve interploation equations with am array of right hand sides and
! --- then put the updated coefficients into array BCF. 
!
      CALL SGBTRS ( 'T', M+DEG-1, 1, 1, 1, MAT_B4, 4, IPIV, BC, M+DEG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1223, IUER, 'BSPL4_1D_CMP', 'Error in '// &
     &          'SGBTRS INFO= '//STR )
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
      END  SUBROUTINE  BSPL4_1D_CMP  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION VAL_1D_BSPL4 ( ARG, M, DEG, IND, ARG_ARR, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_1D_BSPL4  computes the value of the spline of the      *
! *   DEG th degree at the point with coordinate ARG_1.                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*4    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*4    ) -- Array of arguments. Dimension: M.           *
! *     BCF ( REAL*4    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * VAL_1D_BSPL4 ( REAL*4 ) -- Value of the B-spline function.           *
! *                                                                      *
! *  ### 02-MAR-2009  VAL_1D_BSPL4  v1.0 (c) L. Petrov  02-MAR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M, DEG
      REAL*4     ARG, ARG_ARR(M), BCF(1-DEG:M)
      REAL*4     VAL_1D_BSPL4 
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*4     BSPL(-MDEG:0)
      INTEGER*4  J1, J2 
      REAL*4,    EXTERNAL :: BSPL4_VAL 
!
      IF ( IND < 1 .OR. IND > M ) THEN
           VAL_1D_BSPL4 = -1.0E30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         BSPL(J1) = BSPL4_VAL ( M, ARG_ARR, DEG, IND+J1, ARG )
 410  CONTINUE 
!
      VAL_1D_BSPL4 = 0.0
      DO 420 J2=-DEG,0
         VAL_1D_BSPL4 = VAL_1D_BSPL4 + BCF(IND+J2)*BSPL(J2)
 420  CONTINUE 
!
      RETURN
      END  FUNCTION VAL_1D_BSPL4  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_DER_1D_BSPL4 ( ARG, M, DEG, IND, ARG_ARR, BCF, VAL, DER )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_DER_1D_BSPL4  computes the value of the spline of the  *
! *   DEG th degree at the point with coordinate ARG_ARR and the first   *
! *   derivative of the spline.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ARG ( REAL*4    ) -- coordinate of the point.                    *
! *       M ( INTEGER*4 ) -- First dimension of the spline.              *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! *     IND ( INTEGER*4 ) -- Index of a pivotal element. The pivotal     *
! *                          element is the index of the maximal         *
! *                          argument on the mesh that does not exceed   *
! *                          the coordinate along the same dimension.    *
! * ARG_ARR ( REAL*4    ) -- Array of arguments. Dimension: M.           *
! *     BCF ( REAL*4    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     BCF ( REAL*4    ) -- Array of B-spline coefficients. Dimension:  *
! *                          (1-DEG:M).                                  *
! *                                                                      *
! * ### 02-MAR-2009 VAL_DER_1D_BSPL4 v1.0 (c) L. Petrov 02-MAR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND, M, DEG
      REAL*4     ARG, ARG_ARR(M), BCF(1-DEG:M)
      REAL*4     VAL, DER
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*4     BSPL(-MDEG:0), DSPL(-MDEG:0)
      INTEGER*4  J1, J2 
      REAL*4,    EXTERNAL :: BSPL4_VAL, BSPL4_DER 
!
      IF ( IND < 1 .OR. IND > M ) THEN
           VAL = -1.0E30
           DER = -1.0E30
           RETURN 
      END IF
!
      DO 410 J1=-DEG,0
         BSPL(J1) = BSPL4_VAL ( M, ARG_ARR, DEG, IND+J1, ARG )
         DSPL(J1) = BSPL4_DER ( M, ARG_ARR, DEG, IND+J1, ARG )
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
      END  SUBROUTINE VAL_DER_1D_BSPL4 !#!#
