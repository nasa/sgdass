      FUNCTION   VAL_3D_BSPLE3_R4 ( ARGS, DIMS, INDS, ARG_ARR_1, &
     &                              ARG_ARR_2, ARG_ARR_3, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_3D_BSPLE3_R4 computes the value of the 3D spline       *
! *   of the 3rd degree at the point with coordinates                    *
! *   (ARG_1, ARG_2, ARG_3).                                             *
! *                                                                      *
! *   NB: Vectors ARGS_ARR_1, ARGS_ARR_2, and ARGS_ARR_3 are so-called   *
! *   expanded vectors of knots. They have have three extra knots before *
! *   the first knot and three extra knots after the last knots:         *
! *                                                                      *
! *   -2, -1, 0,    1, 2, 3, ... n-1, n,   n+1, n+2, n+3                 *
! *                                                                      *
! *   Arguments between expanded knots should have a small positive      *
! *   step. Recommended step at the beginning of the sequence of knots:  *
! *   eps*(arg[2] - arg[1]), where eps=1.0e-5. Analogously, the step     *
! *   at the end is recommended to be eps*(arg[n] - arg[n-1]).           *
! *                                                                      *
! *   NB,NB: VAL_3D_BSPLE3_R4 does not make a check of validity of       *
! *   extended knots. If the step is too big, the result will be wrong.  *
! *   If the step is too short, VAL_3D_BSPLE3_R4 will crash.             *
! *                                                                      *
! *   It is recommended to use routine BSPLE3_EXTEND_R4 for computing    *
! *   extended knots.                                                    *
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
! *                            dimension. Dimension: [-2:DIMS(1)+3].     *
! * ARG_ARR_2 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(2)+3].     *
! * ARG_ARR_3 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(3)+3].     *
! *       BCF ( REAL*4    ) -- Array of B-spline coefficients. Dimension:*
! *           (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)).               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <VAL_3D_BSPLE3> ( REAL*4    ) -- Value of the 3D spline at the point *
! *                            with coordinates (ARG_1, ARG_2, ARG_3 ).  *
! *                                                                      *
! * ### 01-FEB-2008  VAL_3D_BSPLE3_R4 v1.1 (c) L. Petrov 20-SEP-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  INDS(3), DIMS(3)
      REAL*4     VAL_3D_BSPLE3_R4
      INTEGER*4    DEG
      PARAMETER  ( DEG = 3 )
      REAL*4     ARGS(3), &
     &           ARG_ARR_1(1-DEG:DIMS(1)+DEG), &
     &           ARG_ARR_2(1-DEG:DIMS(2)+DEG), &
     &           ARG_ARR_3(1-DEG:DIMS(3)+DEG), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3))
      REAL*4     VAL
      REAL*4     BSPL(-DEG:0,3), DSPL(-DEG:0,3)
      INTEGER*4  J1, J2, J3, J4, J5, J6
      REAL*4,    EXTERNAL :: BSPLE3_VAL_R4
!
! --- Check dimensions
!
      IF ( INDS(1) < 1 .OR. &
     &     INDS(2) < 1 .OR. &
     &     INDS(3) < 1      ) THEN
!
           VAL = -1.E30
           RETURN 
      END IF
!
! --- Compute values of 1D B-splines
!
      DO 410 J1=-DEG,0
         BSPL(J1,1) = BSPLE3_VAL_R4 ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
 410  CONTINUE 
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPLE3_VAL_R4 ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPLE3_VAL_R4 ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
!
! --- Compute the tensor product of B-splines
!
      VAL = 0.0D0
      DO 440 J4=-DEG,0
         DO 450 J5=-DEG,0
            DO 460 J6=-DEG,0
               VAL = VAL + &
     &               BCF(INDS(1)+J6,INDS(2)+J5,INDS(3)+J4)* &
     &               BSPL(J6,1)*BSPL(J5,2)*BSPL(J4,3)
 460        CONTINUE 
 450     CONTINUE 
 440  CONTINUE 
      VAL_3D_BSPLE3_R4 = VAL
!
      RETURN
      END  FUNCTION  VAL_3D_BSPLE3_R4  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_DER_3D_BSPLE3_R4 ( ARGS, DIMS, INDS, ARG_ARR_1, &
     &                                  ARG_ARR_2, ARG_ARR_3, BCF, &
     &                                  VAL, DER )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_DER_3D_BSPLE3_R4 computes the value of the 3D spline   *
! *   of the DEG th degree at the point with coordinates                 *
! *   (ARG_1, ARG_2, ARG_3) and a vector or partial derivatives over all *
! *   arguments.                                                         *
! *                                                                      *
! *   NB: Vectors ARGS_ARR_1, ARGS_ARR_2, and ARGS_ARR_3 are so-called   *
! *   expanded vectors of knots. They have have three extra knots before *
! *   the first knot and three extra knots after the last knots:         *
! *                                                                      *
! *   -2, -1, 0,    1, 2, 3, ... n-1, n,   n+1, n+2, n+3                 *
! *                                                                      *
! *   Arguments between expanded knots should have a small positive      *
! *   step. Recommended step at the beginning of the sequence of knots:  *
! *   eps*(arg[2] - arg[1]), where eps=1.0e-5. Analogously, the step     *
! *   at the end is recommended to be eps*(arg[n] - arg[n-1]).           *
! *                                                                      *
! *   NB,NB: VAL_DER_3D_BSPLE3_R4 does not make a check of validity of   *
! *   extended knots. If the step is too big, the result will be wrong.  *
! *   If the step is too short, VAL_DER_3D_BSPLE3_R4 will crash.         *
! *                                                                      *
! *   It is recommended to use routine BSPLE3_EXTEND_R4 for computing    *
! *   extended knots.                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ARGS ( REAL*4    ) -- Vector of coordinate of the point along   *
! *                            dimensions. Dimension: 3.                 *
! *      DIMS ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! *      INDS ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! * ARG_ARR_1 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(1)+3].     *
! * ARG_ARR_2 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(2)+3].     *
! * ARG_ARR_3 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(3)+3].     *
! *       BCF ( REAL*4    ) -- Array of B-spline coefficients. Dimension:*
! *           (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)).               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       VAL ( REAL*4    ) -- Value of the 3D spline at the point with  *
! *                            coordinates (ARG_1, ARG_2, ARG_3 )        *
! *       DER ( REAL*4    ) -- Vector of partial derivatives at the      *
! *                            point with coordinates                    *
! *                            (ARG_1, ARG_2, ARG_3).                    *
! *                                                                      *
! * ## 01-FEB-2008 VAL_DER_3D_BSPLE3_R4 v1.1 (c) L. Petrov 20-SEP-2014 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  INDS(3), DIMS(3)
      INTEGER*4  DEG
      PARAMETER  ( DEG = 3 )
      REAL*4     ARGS(3), &
     &           ARG_ARR_1(1-DEG:DIMS(1)+DEG), &
     &           ARG_ARR_2(1-DEG:DIMS(2)+DEG), &
     &           ARG_ARR_3(1-DEG:DIMS(3)+DEG), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3))
      REAL*4     VAL, DER(3)
      REAL*4     BSPL(-DEG:0,3), DSPL(-DEG:0,3)
      INTEGER*4  J1, J2, J3, J4, J5, J6
      REAL*8,    EXTERNAL :: BSPLE3_VAL_R4, BSPLE3_DER_R4
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
         BSPL(J1,1) = BSPLE3_VAL_R4 ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
         DSPL(J1,1) = BSPLE3_DER_R4 ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
 410  CONTINUE 
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPLE3_VAL_R4 ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
         DSPL(J2,2) = BSPLE3_DER_R4 ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPLE3_VAL_R4 ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
         DSPL(J3,3) = BSPLE3_DER_R4 ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
!
! --- Compute the tensor product of B-splines
!
      VAL = 0.0
      DER = 0.0
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
      END  SUBROUTINE  VAL_DER_3D_BSPLE3_R4  !#!#
