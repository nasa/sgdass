      SUBROUTINE BSPL_3D_CMP ( DEG, BC_CODE, DIMS, ARG_ARR_1, ARG_ARR_2, &
     &                          ARG_ARR_3, BCF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BSPL_3D_CMP  computes coefficients of the 3-dimensional   *
! *   interpolation B-spline spline for a 3 dimensional function. The    *
! *   interpolated function is given at nodes of a regular mesh that     *
! *   forms the parallelepiped. Boundary conditions on the first         *
! *   derivatives of the 2D spline at the border of the area are applied *
! *   in accordance with parameter BC_CODE.                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *   BC_CODE ( INTEGER*4 ) -- Code of boundary conditions.              *
! *                         The code of boundary conditions consists of  *
! *                         three decimal digits: the first digit counted*
! *                         from right to left, defines the boundary     *
! *                         condition for the first dimension, the       *
! *                         second digits defines the condition for the  *
! *                         second dimension, and the third digits       *
! *                         defines the condition for the third          *
! *                         dimension.                                   *
! *                       Meaning of digits:                             *
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
! *      DIMS ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! * ARG_ARR_1 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(1).            *
! * ARG_ARR_2 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(2).            *
! * ARG_ARR_3 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(3).            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       BCF ( REAL*8    ) -- On input: array of values of function.    *
! *                            On output: array of B-spline coefficients.*
! *                            Dimension:                                *
! *                         (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)). *
! *                            NB: 1) elements with dimensions < 1 are   *
! *                                   not defined on input.              *
! *                                2) Elements with indexes              *
! *                                   at 1st dimension equal to DIMS(1)  *
! *                                      and/or                          *
! *                                   at 2nd dimension equal to DIMS(2)  *
! *                                      and/or                          *
! *                                   at 3rd dimension equal to DIMS(3)  *
! *                                      and/or                          *
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
! *  ### 28-FEB-2008   BSPL_3D_CMP  v2.0 (c) L. Petrov  19-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, BC_CODE, DIMS(3), IUER
      REAL*8     ARG_ARR_1(DIMS(1)), ARG_ARR_2(DIMS(2)), ARG_ARR_3(DIMS(3))
      REAL*8     BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3))
      INTEGER*4  IER
!
! --- Process dimensions one by one, starting from 1.
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL_3D_DIM_CMP ( 1, DEG, BC_CODE, DIMS(1), ARG_ARR_1, &
     &                        DIMS(1), DIMS(2), DIMS(3), BCF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1861, IUER, 'BSPL_3D_CMP', 'Failure in computing '// &
     &         'coefficients of B-spline expansion over the 1st dimension' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL_3D_DIM_CMP ( 2, DEG, BC_CODE, DIMS(2), ARG_ARR_2, &
     &                        DIMS(1), DIMS(2), DIMS(3), BCF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1862, IUER, 'BSPL_3D_CMP', 'Failure in computing '// &
     &         'coefficients of B-spline expansion over the 2nd dimension' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL_3D_DIM_CMP ( 3, DEG, BC_CODE, DIMS(3), ARG_ARR_3, &
     &                        DIMS(1), DIMS(2), DIMS(3), BCF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1863, IUER, 'BSPL_3D_CMP', 'Failure in computing '// &
     &         'coefficients of B-spline expansion over the 3rd dimension' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE BSPL_3D_CMP  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BSPL_3D_DIM_CMP ( DIM, DEG, BC_CODE, M, ARG, M1, M2, M3, &
     &                              BCF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BSPL_3D_DIM_CMP  updates computation of coefficients of   *
! *   the 3-dimensional interpolation B-spline spline for                *
! *   a 3 dimensional function. This funcion perfreoms update for the    *
! *   dimension DIM. The interpolated fiunction is given at nodes of     *
! *   a regular mesh that forms the parallelepiped. The following        *
! *   boundary conditions are used for computing B-spline: first         *
! *   partical derivatives on the boundary of the 3D figure are equal    *
! *   to their first differences.                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DIM ( INTEGER*4 ) -- The dimension being updated, in range       *
! *                          [1, 4].                                     *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! * BC_CODE ( INTEGER*4 ) -- Code of boundary conditions.                *
! *                          The code of boundary conditions consists of *
! *                          three decimal digits: the first digit       *
! *                          counted from right to left, defines the     *
! *                          boundary condition for the first dimension, *
! *                          the second digits defines the condition for *
! *                          the second dimension, and the third digits  *
! *                          defines the condition for the third         *
! *                          dimension.                                  *
! *                       Meaning of digits:                             *
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
! *       M ( INTEGER*4 ) -- The number of elements over this dimension. *
! *     ARG ( REAL*8    ) -- Array of arguments over the dimension being *
! *                          updated. Dimension: DIM.                    *
! *      M1 ( INTEGER*4 ) -- First dimension of the function being       *
! *                          interpolated.                               *
! *      M2 ( INTEGER*4 ) -- Second dimension of the function being      *
! *                          interpolated.                               *
! *      M3 ( INTEGER*4 ) -- Third dimension of the function being       *
! *                          interpolated.                               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  BCF ( REAL*8    ) -- Array of B-spline coefficients.                *
! *                       If DIM == 1, then on input: array of           *
! *                       values of the function.                        *
! *                       Dimensions: (1-DEG:M1,1-DEG:M2,1-DEG:M3).      *
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
! * ### 28-FEB-2008  BSPL_3D_DIM_CMP  v2.0 (c) L. Petrov 19-APR-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DIM, DEG, BC_CODE, M, M1, M2, M3, IUER
      REAL*8     ARG(M), BCF(1-DEG:M1,1-DEG:M2,1-DEG:M3)
      REAL*8     EQU
      REAL*8     EPS
      PARAMETER  ( EPS    = 1.E-6 )
      INTEGER*4  I, J, LOC_TO_BAND
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           J11, J12, J13, J14, J15, J16, J17, J18, J19, J20, &
     &           BC_CODE_1, BC_CODE_2, BC_CODE_3, IER
#ifdef GNU
      INTEGER*4  NTHR,    NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
      CHARACTER  STR*32
      REAL*8,    EXTERNAL  :: BSPL_VAL, BSPL_DER
      INTEGER*4, ALLOCATABLE :: IPIV(:)
      REAL*8,    ALLOCATABLE :: MAT_B4(:), RH(:), BC(:,:,:)
      LOGICAL*4, EXTERNAL    :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL    :: I_LEN, ILEN
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#endif
      LOC_TO_BAND(I,J,DEG) = I-J+DEG + (DEG+1)*(DEG+J-1)
!
#ifndef SERIAL
      NTHR_SAVED = OMP_GET_NUM_THREADS()
      IF ( OMP_IN_PARALLEL() ) THEN
!
! -------- Do serial if we are already in the parallel region
!
           NTHR = 1
         ELSE
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) == 0 ) THEN
                NTHR = 1
              ELSE 
                CALL CHIN ( STR, NTHR )
           END IF
           CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
           NTHR = OMP_GET_MAX_THREADS()
      END IF
#endif
      IF ( DEG .NE. 3 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG )
           CALL ERR_LOG ( 1840, IUER, 'BSPL_3D_DIM_CMP', 'DEG='// &
     &          STR(1:I_LEN(STR))//' is currently not supported' )
           RETURN 
      END IF
!
      BC_CODE_1 = MOD(BC_CODE,10)
      BC_CODE_2 = MOD(BC_CODE/10,10)
      BC_CODE_3 = MOD(BC_CODE/100,10)
!
! --- Allocate dynamic memory for the band matrix of interpolation 
! --- equations
!
      ALLOCATE ( MAT_B4((DEG+1)*(M+DEG-1)), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(DEG+1)*(M+DEG-1), STR)
           CALL ERR_LOG ( 1841, IUER, 'BSPL_3D_DIM_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
      MAT_B4 = 0.0
!
! --- Allocate dynamic memory for the temporary array 
!
      IF ( DIM == 1 ) THEN
           ALLOCATE ( BC(0:M1+DEG-1,1:M2,1:M3), STAT=IER ) 
         ELSE IF ( DIM == 2 ) THEN
           ALLOCATE ( BC(0:M2+DEG-1,1-DEG:M1,1:M3), STAT=IER ) 
         ELSE IF ( DIM == 3 ) THEN
           ALLOCATE ( BC(0:M3+DEG-1,1-DEG:M1,1-DEG:M2), STAT=IER ) 
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(M1+DEG)*(M2+DEG)*(M3+DEG), STR)
           CALL ERR_LOG ( 1842, IUER, 'BSPL_3D_DIM_CMP', 'Failure to '// &
     &         'allocate '//STR(1:ILEN(STR))//' bytes of dynamic memory' )
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
           CALL ERR_LOG ( 1843, IUER, 'BSPL_3D_DIM_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
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
                 EQU = BSPL_DER ( M, ARG, DEG, J1, ARG(1) )
               ELSE IF ( J2 == M+1 ) THEN
                 EQU = BSPL_DER ( M, ARG, DEG, J1, ARG(M)*(1-EPS) )
               ELSE 
                 EQU = BSPL_VAL ( M, ARG, DEG, J1, ARG(J2) )
                 IF ( J2 == M ) THEN
                      EQU = BSPL_VAL ( M, ARG, DEG, J1, ARG(J2)*(1-EPS) )
                 END IF
            END IF
!
! --------- Put the element of the equation into the band matrix
! --------- MAT_B4(LOC_TO_BAND(J1,J2-DEG+1,DEG)) = EQU
!
            
            MAT_B4(J1-1 + DEG*(J2+2)) = EQU
 420     CONTINUE 
 410  CONTINUE 
!
! --- Build the right-band sides of interpolating equations. 
! --- They are different for different dimensions
!
      IF ( DIM == 1 ) THEN
!
! -------- The order of dimensions: 1,2,3
!
           DO 430 J3=1,M3
              DO 440 J4=1,M2
                 DO 450 J5=1,M1
                    BC(J5,J4,J3) = BCF(J5,J4,J3)
 450             CONTINUE 
!
! -------------- Two boundary condition are imposed to first derivatives at the 
! -------------- the first point (left side) and the last point (right side)
!
                 IF ( BC_CODE_1 == 0 ) THEN
!
! ------------------- At the left side: the first derivative is equal to 
! ------------------- the first difference at the left side
!
! ------------------- At the right side: the first derivative is equal to 
! ------------------- the first difference at the right side
!
                      BC(0,J4,J3)   = (BCF(2,J4,J3) - BCF(1,J4,J3)  )/(ARG(2) - ARG(1))
                      BC(M+1,J4,J3) = (BCF(M,J4,J3) - BCF(M-1,J4,J3))/(ARG(M) - ARG(M-1))
                    ELSE IF ( BC_CODE == 1 ) THEN
!
! ------------------- At both sides: the first derivative is an average of the
! ------------------- first differences at the left side and the right side
!
                      BC(0,J4,J3)   = ( (BCF(2,J4,J3) - BCF(1,J4,J3)  )/(ARG(2) - ARG(1)) + &
    &                                   (BCF(M,J4,J3) - BCF(M-1,J4,J3))/(ARG(M) - ARG(M-1))  )/2.0
                      BC(M+1,J4,J3) = BC(0,J4,J3)
                    ELSE IF ( BC_CODE == 2 ) THEN
!
! ------------------- At the left side: zero
!
! ------------------- At the right side: the first derivative is equal to 
! ------------------- the first difference at the rightside
!
                      BC(0,J4,J3)   = 0.0
                      BC(M+1,J4,J3) = (BCF(M,J4,J3) - BCF(M-1,J4,J3))/(ARG(M) - ARG(M-1))
                    ELSE IF ( BC_CODE == 3 ) THEN
!
! ------------------- At the left side: the first derivative is equal to 
! ------------------- the first difference at the left side
!
! ------------------- At the right side: zero
!
                      BC(0,J4,J3)   = (BCF(2,J4,J3) - BCF(1,J4,J3)  )/(ARG(2) - ARG(1))
                      BC(M+1,J4,J3) = 0.0
                 END IF
 440          CONTINUE 
 430       CONTINUE 
        ELSE IF ( DIM == 2 ) THEN
!
! -------- The order of dimensions: 2,1,3
!
           DO 460 J6=1,M3
              DO 470 J7=1-DEG,M1
                 DO 480 J8=1,M2
                    BC(J8,J7,J6) = BCF(J7,J8,J6)
 480             CONTINUE 
                 IF ( BC_CODE_2 == 0 ) THEN
!
! ------------------- At the left side: the first derivative is equal to 
! ------------------- the first difference at the left side
!
! ------------------- At the right side: the first derivative is equal to 
! ------------------- the first difference at the right side
!
                      BC(0,J7,J6)   = (BCF(J7,2,J6) - BCF(J7,1,J6)  )/(ARG(2) - ARG(1))
                      BC(M+1,J7,J6) = (BCF(J7,M,J6) - BCF(J7,M-1,J6))/(ARG(M) - ARG(M-1))
                    ELSE IF ( BC_CODE_2 == 1 ) THEN
!
! ------------------- At both sides: the first derivative is an average of the
! ------------------- first differences at the left side and the right side
!
                      BC(0,J7,J6)   = ( (BCF(J7,2,J6) - BCF(J7,1,J6)  )/(ARG(2) - ARG(1)) + &
    &                                   (BCF(J7,M,J6) - BCF(J7,M-1,J6))/(ARG(M) - ARG(M-1))  )/2.0
                      BC(M+1,J7,J6) = BC(0,J7,J6)
                    ELSE IF ( BC_CODE_2 == 2 ) THEN
!
! ------------------- At the left side: zero
!
! ------------------- At the right side: the first derivative is equal to 
! ------------------- the first difference at the rightside
!
                      BC(0,J7,J6)   = 0.0
                      BC(M+1,J7,J6) = (BCF(J7,M,J6) - BCF(J7,M-1,J6))/(ARG(M) - ARG(M-1))
                    ELSE IF ( BC_CODE_2 == 3 ) THEN
!
! ------------------- At the left side: the first derivative is equal to 
! ------------------- the first difference at the left side
!
! -------------------- At the right side: zero
!
                      BC(0,J7,J6)   = (BCF(J7,2,J6) - BCF(J7,1,J6)  )/(ARG(2) - ARG(1))
                      BC(M+1,J7,J6) = 0.0
                 END IF
 470          CONTINUE 
 460       CONTINUE 
        ELSE IF ( DIM == 3 ) THEN
!
! -------- The order of dimensions: 3,1,2
!
           DO 490 J9=1-DEG,M2
              DO 4100 J10=1-DEG,M1
                 DO 4110 J11=1,M3
                    BC(J11,J10,J9) = BCF(J10,J9,J11)
 4110            CONTINUE 
                 IF ( BC_CODE_3 == 0 ) THEN
!
! ------------------- At the left side: the first derivative is equal to 
! ------------------- the first difference at the left side
!
! ------------------- At the right side: the first derivative is equal to 
! ------------------- the first difference at the right side
!
                      BC(0,J10,J9)   = (BCF(J10,J9,2) - BCF(J10,J9,1)  )/(ARG(2) - ARG(1))
                      BC(M+1,J10,J9) = (BCF(J10,J9,M) - BCF(J10,J9,M-1))/(ARG(M) - ARG(M-1))
                   ELSE IF ( BC_CODE_3 == 1 ) THEN
!
! ------------------- At both sides: the first derivative is an average of the
! ------------------- first differences at the left side and the right side
!
                      BC(0,J10,J9)   = ( (BCF(J10,J9,2) - BCF(J10,J9,1)  )/(ARG(2) - ARG(1)) + &
    &                                    (BCF(J10,J9,M) - BCF(J10,J9,M-1))/(ARG(M) - ARG(M-1))  )/2.0
                      BC(M+1,J10,J9) = BC(0,J10,J9)   
                   ELSE IF ( BC_CODE_3 == 2 ) THEN
!
! ------------------- At the left side: zero
!
! ------------------- At the right side: the first derivative is equal to 
! ------------------- the first difference at the rightside
!
                      BC(0,J10,J9)   = 0.0
                      BC(M+1,J10,J9) = (BCF(J10,J9,M) - BCF(J10,J9,M-1))/(ARG(M) - ARG(M-1))
                   ELSE IF ( BC_CODE_3 == 3 ) THEN
!
! ------------------- At the left side: the first derivative is equal to 
! ------------------- the first difference at the left side
!
! -------------------- At the right side: zero
!
                      BC(0,J10,J9)   = (BCF(J10,J9,2) - BCF(J10,J9,1)  )/(ARG(2) - ARG(1))
                      BC(M+1,J10,J9) = 0.0
                 END IF
 4100         CONTINUE 
 490       CONTINUE 
      END IF
!
! --- Decompose the band matrix
!
      CALL DGBTRF ( M+DEG-1, M+DEG-1, 1, 1, MAT_B4, DEG+1, IPIV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1814, IUER, 'BSPL_3D_DIM_CMP', 'Error in SGBTRF '// &
     &          'INFO= '//STR )
           RETURN 
      END IF
!
! --- Solve the interpolation eqautions with a 3D array of right hand sides and
! --- then put the updated spline coefficnets into array BCF. The rules for 
! --- manipulating the elements differ from dimension to dimension
!
      IF ( DIM == 1 ) THEN
#ifdef SERIAL
           CALL DGBTRS ( 'T', M+DEG-1, 1, 1, M2*M3, MAT_B4, DEG+1, IPIV, BC, M1+DEG, IER )
#else
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J12, J3, J14 ), SCHEDULE ( STATIC )
#endif
!
! -------- The order of dimensions: 1,2,3
!
           DO 4120 J12=1,M3
#ifndef SERIAL
              CALL DGBTRS ( 'T', M+DEG-1, 1, 1, M2, MAT_B4, DEG+1, IPIV, BC(0,1,J12), M1+DEG, IER )
#endif
              DO 4130 J13=1,M2
                 DO 4140 J14=1-DEG,M1-1
                     BCF(J14,J13,J12) = BC(J14+DEG-1,J13,J12)
 4140            CONTINUE 
 4130         CONTINUE 
 4120      CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
         ELSE IF ( DIM == 2 ) THEN
#ifdef SERIAL
           CALL DGBTRS ( 'T', M+DEG-1, 1, 1, (M1+DEG)*M3, MAT_B4, DEG+1, IPIV, BC, M2+DEG, IER )
#else
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J15, J6, J17 ), SCHEDULE ( STATIC )
#endif
!
! -------- The order of dimensions: 2,1,3
!
           DO 4150 J15=1,M3
#ifndef SERIAL
           CALL DGBTRS ( 'T', M+DEG-1, 1, 1, (M1+DEG), MAT_B4, DEG+1, IPIV, BC(0,1-DEG,J15), M2+DEG, IER )
#endif
              DO 4160 J16=1-DEG,M1
                 DO 4170 J17=1-DEG,M2-1
                    BCF(J16,J17,J15) = BC(J17+DEG-1,J16,J15)
 4170            CONTINUE 
 4160         CONTINUE 
 4150      CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
         ELSE IF ( DIM == 3 ) THEN
#ifdef SERIAL
           CALL DGBTRS ( 'T', M+DEG-1, 1, 1, (M1+DEG)*(M2+DEG), MAT_B4, DEG+1, IPIV, BC, M3+DEG, IER )
#else
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J18, J19, J20 ), SCHEDULE ( STATIC )
#endif
!
! -------- The order of dimensions: 3,1,2
!
           DO 4180 J18=1-DEG,M2
#ifndef SERIAL
           CALL DGBTRS ( 'T', M+DEG-1, 1, 1, (M1+DEG), MAT_B4, DEG+1, IPIV, BC(0,1-DEG,J18), M3+DEG, IER )
#endif
              DO 4190 J19=1-DEG,M1
                 DO 4200 J20=1-DEG,M3-1
                    BCF(J19,J18,J20) = BC(J20+DEG-1,J19,J18)
 4200            CONTINUE 
 4190         CONTINUE 
 4180      CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1844, IUER, 'BSPL_3D_DIM_CMP', 'Error in SGBTRS '// &
     &          'INFO= '//STR )
           RETURN 
      END IF
!
! --- Deallocat the matrix
!
      DEALLOCATE ( MAT_B4 )
      DEALLOCATE ( IPIV   )
      DEALLOCATE ( BC     )
#ifndef SERIAL
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPL_3D_DIM_CMP  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION VAL_3D_BSPL ( ARGS, DEG, DIMS, INDS, ARG_ARR_1, ARG_ARR_2, &
     &                        ARG_ARR_3, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_3D_BSPL  computes the value of the 3D spline of the    *
! *   DEG th degree at the point with coordinates                        *
! *   (ARG_1, ARG_2, ARG_3).                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ARGS ( REAL*8    ) -- Vector of coordinate of the point along   *
! *                            dimensions. Dimension: 3.                 *
! *     ARG_2 ( REAL*8    ) -- coordinate of the point along dimension 2 *
! *     ARG_3 ( REAL*8    ) -- coordinate of the point along dimension 3 *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *      DIMS ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! *      INDS ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! * ARG_ARR_1 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(1).            *
! * ARG_ARR_2 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(2).            *
! * ARG_ARR_3 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(3).            *
! *       BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:*
! *                       (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)).   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <VAL_3D_BSPL> ( REAL*8 ) -- Value of the 3D spline at point with     *
! *                              coordinates (ARG_1, ARG_2, ARG_3)       *
! *                                                                      *
! * ###  28-FEB-2008   VAL_3D_BSPL   v1.0 (c)  L. Petrov 28-FEB-2008 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, INDS(3), DIMS(3)
      REAL*8     ARGS(3), &
     &           ARG_ARR_1(DIMS(1)), ARG_ARR_2(DIMS(2)), &
     &           ARG_ARR_3(DIMS(3)), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3))
      REAL*8     VAL_3D_BSPL 
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*8     BSPL(-MDEG:0,3)
      INTEGER*4  J1, J2, J3, J4, J5, J6
      REAL*8,    EXTERNAL :: BSPL_VAL 
!
! --- Check dimensions
!
      IF ( INDS(1) < 1 .OR. &
     &     INDS(2) < 1 .OR. &
     &     INDS(3) < 1      ) THEN
!
           VAL_3D_BSPL = -1.E30
           RETURN 
      END IF
!
! --- Compute values of 1D B-splines
!
      DO 410 J1=-DEG,0
         BSPL(J1,1) = BSPL_VAL ( DIMS(1), ARG_ARR_1, DEG, INDS(1)+J1, ARGS(1) )
 410  CONTINUE 
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPL_VAL ( DIMS(2), ARG_ARR_2, DEG, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPL_VAL ( DIMS(3), ARG_ARR_3, DEG, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
!
! --- Compute the tensor products of B-splines
!
      VAL_3D_BSPL = 0.0
      DO 440 J4=-DEG,0
         DO 450 J5=-DEG,0
            DO 460 J6=-DEG,0
               VAL_3D_BSPL = VAL_3D_BSPL + &
     &                BCF(INDS(1)+J6,INDS(2)+J5,INDS(3)+J4)* &
     &                BSPL(J6,1)*BSPL(J5,2)*BSPL(J4,3)
 460        CONTINUE 
 450     CONTINUE 
 440  CONTINUE 
!
      RETURN
      END  FUNCTION VAL_3D_BSPL  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_DER_3D_BSPL ( ARGS, DEG, DIMS, INDS, ARG_ARR_1, &
     &                              ARG_ARR_2, ARG_ARR_3, BCF, &
     &                              VAL, DER )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_3D_BSPL  computes the value of the 3D spline of the    *
! *   DEG th degree at the point with coordinates                        *
! *   (ARG_1, ARG_2, ARG_3) and a vector or partial derivatives over all *
! *   arguments.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ARGS ( REAL*8    ) -- Vector of coordinate of the point along   *
! *                            dimensions. Dimension: 3.                 *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *      DIMS ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! *      INDS ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! * ARG_ARR_1 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(1).            *
! * ARG_ARR_2 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(2).            *
! * ARG_ARR_3 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(3).            *
! *       BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:*
! *           (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)).               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       VAL ( REAL*8    ) -- Value of the 3D spline at the point with  *
! *                            coordinates (ARG_1, ARG_2, ARG_3 )        *
! *       DER ( REAL*8    ) -- Vector of partial derivatives at the      *
! *                            point with coordinates                    *
! *                            (ARG_1, ARG_2, ARG_3).                    *
! *                                                                      *
! * ### 01-FEB-2008  VAL_DER_3D_BSPL  v1.0 (c) L. Petrov 04-FEB-2008 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, INDS(3), DIMS(3)
      REAL*8     ARGS(3), &
     &           ARG_ARR_1(DIMS(1)), ARG_ARR_2(DIMS(2)), &
     &           ARG_ARR_3(DIMS(3)), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3))
      REAL*8     VAL, DER(3)
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*8     BSPL(-MDEG:0,3), DSPL(-MDEG:0,3)
      INTEGER*4  J1, J2, J3, J4, J5, J6
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER
!
! --- Check dimensions
!
      IF ( INDS(1) < 1 .OR. &
     &     INDS(2) < 1 .OR. &
     &     INDS(3) < 1      ) THEN
!
           VAL = -1.E30
           DER = -1.E30
           RETURN 
      END IF
!
! --- Compute values of 1D B-splines
!
      DO 410 J1=-DEG,0
         BSPL(J1,1) = BSPL_VAL ( DIMS(1), ARG_ARR_1, DEG, INDS(1)+J1, ARGS(1) )
         DSPL(J1,1) = BSPL_DER ( DIMS(1), ARG_ARR_1, DEG, INDS(1)+J1, ARGS(1) )
 410  CONTINUE 
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPL_VAL ( DIMS(2), ARG_ARR_2, DEG, INDS(2)+J2, ARGS(2) )
         DSPL(J2,2) = BSPL_DER ( DIMS(2), ARG_ARR_2, DEG, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPL_VAL ( DIMS(3), ARG_ARR_3, DEG, INDS(3)+J3, ARGS(3) )
         DSPL(J3,3) = BSPL_DER ( DIMS(3), ARG_ARR_3, DEG, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
!
! --- Compute the tensor product of B-splines
!
      VAL = 0.0D0
      DER = 0.0D0
      DO 440 J4=-DEG,0
         DO 450 J5=-DEG,0
            DO 460 J6=-DEG,0
               VAL = VAL + &
     &               BCF(INDS(1)+J6,INDS(2)+J5,INDS(3)+J4)* &
     &               BSPL(J6,1)*BSPL(J5,2)*BSPL(J4,3)
               DER(1) = DER(1) + &
     &                  BCF(INDS(1)+J6,INDS(2)+J5,INDS(3)+J4)* &
     &                  DSPL(J6,1)*BSPL(J5,2)*BSPL(J4,3)
               DER(2) = DER(2) + &
     &                  BCF(INDS(1)+J6,INDS(2)+J5,INDS(3)+J4)* &
     &                  BSPL(J6,1)*DSPL(J5,2)*BSPL(J4,3)
               DER(3) = DER(3) + &
     &                  BCF(INDS(1)+J6,INDS(2)+J5,INDS(3)+J4)* &
     &                  BSPL(J6,1)*BSPL(J5,2)*DSPL(J4,3)
 460        CONTINUE 
 450     CONTINUE 
 440  CONTINUE 
!
      RETURN
      END  SUBROUTINE  VAL_DER_3D_BSPL  !#!#
