      FUNCTION VAL_BSPLJ4 ( ARG, DEG, DIM, IND, ARG_ARR, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_BSPLJ4   computes the integral over the ARG_ARG        *
! *   from ARG to ARG_ARR(DIM) at the 3D field of function               *
! *   expanded over B-spline basis of the DEG th degree.                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       ARG ( REAL*8    ) -- Low limit of the integral.                *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *       DIM ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! *       IND ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! *   ARG_ARR ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(3).            *
! *       BCF ( REAL*8    ) -- Array of B-spline coefficients.           *
! *                            Dimension: (1-DEG:DIM).                   *
! *                                                                      *
! * ________________________ Output parameters: _______________________  *
! *                                                                      *
! * <VAL_BSPLJ44> ( REAL*8 ) -- Value of the integral over interval      *
! *                             [ARG, ARG_ARR(DIMS)].                    *
! *                                                                      *
! * ### 11-SEP-2014    VAL_BSPLJ4    v1.0 (c) L. Petrov 11-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, IND, DIM
      REAL*4     ARG, ARG_ARR(DIM), BCF(1-DEG:DIM)
      REAL*4     VAL_BSPLJ4
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*4     BSPLJ4(1-MDEG:DIM)
      INTEGER*4  J1, J2
      REAL*4,    EXTERNAL :: BSPL4_VAL, BSPL4_JNT 
!
! --- Check dimensions
!
      IF ( IND < 1 ) THEN
           VAL_BSPLJ4 = -1.E30
           RETURN 
      END IF
!
! --- Compute intgeral over dimension 1 from ARGS(1) to ARGS(DIMS(1))
!
      DO 410 J1=IND-DEG,DIM-1
         BSPLJ4(J1) = BSPL4_JNT ( DIM, ARG_ARR, DEG, J1, ARG )
 410  CONTINUE 
!
! --- Compute the tensor products of B-splines
!
      VAL_BSPLJ4 = 0.0
      DO 420 J2=IND-DEG,DIM-1
         VAL_BSPLJ4 = VAL_BSPLJ4 + BCF(J2)*BSPLJ4(J2)
 420  CONTINUE 
!
      RETURN
      END  FUNCTION  VAL_BSPLJ4  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION VAL_3D_BSPLJ4 ( ARGS, DEG, DIMS, INDS, ARG_ARR_1, ARG_ARR_2, &
     &                         ARG_ARR_3, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_3D_BSPLJ4  computes the integral over the dimension    *
! *   1 from ARGS(1) to ARGS(DIMS(1)) at the 3D field of function        *
! *   expanded over B-spline basis of the DEG th degree.                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ARGS ( REAL*4    ) -- Vector of coordinate of the point along   *
! *                            dimensions. Dimension: 3.                 *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *      DIMS ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! *      INDS ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! * ARG_ARR_1 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(1).            *
! * ARG_ARR_2 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(2).            *
! * ARG_ARR_3 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(3).            *
! *       BCF ( REAL*4    ) -- Array of B-spline coefficients. Dimension:*
! *            (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)).              *
! *                                                                      *
! * ________________________ Output parameters: _______________________  *
! *                                                                      *
! * <VAL_3D_BSPLJ4> ( REAL*4 ) -- Value of the 3D integral over interval *
! *                               with coordinages                       *
! *                               (ARG(1), ARG(2), ARG(3)) to            *
! *                               (ARG(DIMS(1)), ARG(2), ARG(3) )        *
! *                                                                      *
! * ### 15-FEB-2014  VAL_3D_BSPLJ4   v1.0 (c) L. Petrov 15-FEB-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, INDS(3), DIMS(3)
      REAL*4     ARGS(3), &
     &           ARG_ARR_1(DIMS(1)), ARG_ARR_2(DIMS(2)), &
     &           ARG_ARR_3(DIMS(3)), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3))
      REAL*4     VAL_3D_BSPLJ4 
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*4     BSPL(-MDEG:0,3), BSPLJ(1-MDEG:DIMS(1))
      INTEGER*4  J1, J2, J3, J4, J5, J6
      REAL*4,    EXTERNAL :: BSPL4_VAL, BSPL4_JNT 
!
! --- Check dimensions
!
      IF ( INDS(1) < 1 .OR. &
     &     INDS(2) < 1 .OR. &
     &     INDS(3) < 1      ) THEN
!
           VAL_3D_BSPLJ4 = -1.E30
           RETURN 
      END IF
!
! --- Compute intgeral over dimension 1 from ARGS(1) to ARGS(DIMS(1))
!
      DO 410 J1=INDS(1)-DEG,DIMS(1)-1
         BSPLJ(J1) = BSPL4_JNT  ( DIMS(1), ARG_ARR_1, DEG, J1, ARGS(1) )
 410  CONTINUE 
!
! --- Compute values of 1D B-splines
!
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPL4_VAL ( DIMS(2), ARG_ARR_2, DEG, INDS(2)+J2, ARGS(2) )
 420  CONTINUE
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPL4_VAL ( DIMS(3), ARG_ARR_3, DEG, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
!
! --- Compute the tensor products of B-splines
!
      VAL_3D_BSPLJ4 = 0.0
      DO 440 J4=-DEG,0
         DO 450 J5=-DEG,0
            DO 460 J6=INDS(1)-DEG,DIMS(1)-1
               VAL_3D_BSPLJ4 = VAL_3D_BSPLJ4 + &
     &             BCF(J6,INDS(2)+J5,INDS(3)+J4)*BSPLJ(J6)*BSPL(J5,2)*BSPL(J4,3)
 460        CONTINUE 
 450     CONTINUE 
 440  CONTINUE 
!
      RETURN
      END  FUNCTION VAL_3D_BSPLJ4  !#!  
