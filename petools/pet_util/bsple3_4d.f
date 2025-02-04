      FUNCTION   VAL_4D_BSPLE3 ( ARGS, DIMS, INDS, ARG_ARR_1, &
     &                           ARG_ARR_2, ARG_ARR_3, ARG_ARR_4, BCF )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_4D_BSPLE3  computes the value of the 4D spline         *
! *   of the 3rd degree at the point with coordinates                    *
! *   (ARG_1, ARG_2, ARG_3, ARG_4).                                      *
! *                                                                      *
! *   NB: Vectors ARGS_ARR_1, ARGS_ARR_2, ARGS_ARR_3, and ARGS_ARR_4     *
! *   are so-called expanded vectors of knots. They have have three      *
! *   extra knots before the first knot and three extra knots after      *
! *   the last knot:                                                     *
! *                                                                      *
! *   -2, -1, 0,    1, 2, 3, ... n-1, n,   n+1, n+2, n+3                 *
! *                                                                      *
! *   Arguments between expanded knots should have a small positive      *
! *   step. Recommended step at the beginning of the sequence of knots:  *
! *   eps*(arg[2] - arg[1]), where eps=1.0e-5. Analogously, the step     *
! *   at the end is recommended to be eps*(arg[n] - arg[n-1]).           *
! *                                                                      *
! *   NB: VAL_4D_BSPLE3 does not make a check of validity of             *
! *   extended knots. If the step is too big, the result will be wrong.  *
! *   If the step is too short, VAL_4D_BSPLE3 will crash.                *
! *                                                                      *
! *   It is recommended to use routine BSPLE3_EXTEND for computing       *
! *   extended knots.                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ARGS ( REAL*8    ) -- Vector of coordinate of the point along   *
! *                            dimensions. Dimension: 4.                 *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *      DIMS ( INTEGER*4 ) -- Array of 4 dimensions.                    *
! *      INDS ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! *                            Dimension: 4.                             *
! * ARG_ARR_1 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(1)+3].     *
! * ARG_ARR_2 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(2)+3].     *
! * ARG_ARR_3 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(3)+3].     *
! * ARG_ARR_4 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(4)+3].     *
! *       BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:*
! *           (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3),1-DEG:DIMS(4)). *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <VAL_4D_BSPLE3> ( REAL*8    ) -- Value of the 3D spline at the point *
! *                                  with coordinates                    *
! *                                  (ARG_1, ARG_2, ARG_3, ARG_4 ).      *
! *                                                                      *
! * ### 10-NOV-2014  VAL_4D_BSPLE3  v1.1 (c)  L. Petrov 10-NOV-2014  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  INDS(3), DIMS(3)
      REAL*8     VAL_4D_BSPLE3
      INTEGER*4    DEG
      PARAMETER  ( DEG = 3 )
      REAL*8     ARGS(4), &
     &           ARG_ARR_1(1-DEG:DIMS(1)+DEG), &
     &           ARG_ARR_2(1-DEG:DIMS(2)+DEG), &
     &           ARG_ARR_3(1-DEG:DIMS(3)+DEG), &
     &           ARG_ARR_4(1-DEG:DIMS(4)+DEG), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3),1-DEG:DIMS(4))
      REAL*8     VAL
      REAL*8     BSPL(-DEG:0,4), DSPL(-DEG:0,4)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8
      REAL*8,    EXTERNAL :: BSPLE3_VAL
!
! --- Check dimensions
!
      IF ( INDS(1) < 1 .OR. &
     &     INDS(2) < 1 .OR. &
     &     INDS(3) < 1 .OR. &
     &     INDS(4) < 1      ) THEN
!
           VAL = -1.E30
           RETURN 
      END IF
!
! --- Compute values of 1D B-splines
!
      DO 410 J1=-DEG,0
         BSPL(J1,1) = BSPLE3_VAL ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
 410  CONTINUE 
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPLE3_VAL ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPLE3_VAL ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
      DO 440 J4=-DEG,0
         BSPL(J4,4) = BSPLE3_VAL ( DIMS(4), ARG_ARR_4, INDS(4)+J4, ARGS(4) )
 440  CONTINUE 
!
! --- Compute the tensor product of B-splines
!
      VAL = 0.0D0
      DO 450 J5=-DEG,0
         DO 460 J6=-DEG,0
            DO 470 J7=-DEG,0
               DO 480 J8=-DEG,0
                  VAL = VAL + &
     &                  BCF(INDS(1)+J8,INDS(2)+J7,INDS(3)+J6,INDS(4)+J5)* &
     &                  BSPL(J8,1)*BSPL(J7,2)*BSPL(J6,3)*BSPL(J5,4)
 480           CONTINUE 
 470        CONTINUE 
 460     CONTINUE 
 450  CONTINUE 
      VAL_4D_BSPLE3 = VAL
!
      RETURN
      END  FUNCTION  VAL_4D_BSPLE3  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_DER_4D_BSPLE3 ( ARGS, DIMS, INDS, ARG_ARR_1, &
     &                               ARG_ARR_2, ARG_ARR_3, BCF, &
     &                               VAL, DER )
! ************************************************************************
! *                                                                      *
! *   Routine  VAL_DER_4D_BSPLE3  computes the value of the 4D spline    *
! *   of the DEG th degree at the point with coordinates                 *
! *   (ARG_1, ARG_2, ARG_3, ARG_4) and a vector or partial derivatives   *
! *   over all arguments.                                                *
! *                                                                      *
! *   NB: Vectors ARG_ARR_1, ARG_ARR_2, ARG_ARR_3, and ARG_ARR_4  are    *
! *   the so-called expanded vectors of knots. They have have three      *
! *   extra knots before the first knot and three extra knots after      *
! *   the last knot:                                                     *     
! *                                                                      *
! *   -2, -1, 0,    1, 2, 3, ... n-1, n,   n+1, n+2, n+3                 *
! *                                                                      *
! *   Arguments between expanded knots should have a small positive      *
! *   step. Recommended step at the beginning of the sequence of knots:  *
! *   eps*(arg[2] - arg[1]), where eps=1.0e-5. Analogously, the step     *
! *   at the end is recommended to be eps*(arg[n] - arg[n-1]).           *
! *                                                                      *
! *   NB: VAL_DER_4D_BSPLE3 does not make a check of validity of         *
! *   extended knots. If the step is too big, the result will be wrong.  *
! *   If the step is too short, VAL_DER_4D_BSPLE3 will crash.            *
! *                                                                      *
! *   It is recommended to use routine BSPLE3_EXTEND for computing       *
! *   extended knots.                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ARGS ( REAL*8    ) -- Vector of coordinate of the point along   *
! *                            dimensions. Dimension: 3.                 *
! *      DIMS ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! *      INDS ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! * ARG_ARR_1 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(1)+3].     *
! * ARG_ARR_2 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(2)+3].     *
! * ARG_ARR_3 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(3)+3].     *
! * ARG_ARR_4 ( REAL*8    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(4)+3].     *
! *       BCF ( REAL*8    ) -- Array of B-spline coefficients. Dimension:*
! *           (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3),1-DEG:DIMS(4).  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       VAL ( REAL*8    ) -- Value of the 3D spline at the point with  *
! *                            coordinates (ARG_1, ARG_2, ARG_3, ARG_4 ) *
! *       DER ( REAL*8    ) -- Vector of partial derivatives at the      *
! *                            point with coordinates                    *
! *                            (ARG_1, ARG_2, ARG_3, ARG_4).             *
! *                                                                      *
! * ### 01-FEB-2008 VAL_DER_4D_BSPLE3 v1.1 (c) L. Petrov 10-NOV-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  INDS(4), DIMS(4)
      INTEGER*4  DEG
      PARAMETER  ( DEG = 3 )
      REAL*8     ARGS(4), &
     &           ARG_ARR_1(1-DEG:DIMS(1)+DEG), &
     &           ARG_ARR_2(1-DEG:DIMS(2)+DEG), &
     &           ARG_ARR_3(1-DEG:DIMS(3)+DEG), &
     &           ARG_ARR_4(1-DEG:DIMS(4)+DEG), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3),1-DEG:DIMS(4))
      REAL*8     VAL, DER(4)
      REAL*8     BSPL(-DEG:0,4), DSPL(-DEG:0,4)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8
      REAL*8,    EXTERNAL :: BSPLE3_VAL, BSPLE3_DER
!
! --- Check dimensions
!
      IF ( INDS(1) < 1 .OR. &
     &     INDS(2) < 1 .OR. &
     &     INDS(3) < 1 .OR. &
     &     INDS(4) < 1      ) THEN
!
           VAL = -1.E30
           DER = -1.E30
           RETURN 
      END IF
!
! --- Compute values of 1D B-splines
!
      DO 410 J1=-DEG,0
         BSPL(J1,1) = BSPLE3_VAL ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
         DSPL(J1,1) = BSPLE3_DER ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
 410  CONTINUE 
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPLE3_VAL ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
         DSPL(J2,2) = BSPLE3_DER ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPLE3_VAL ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
         DSPL(J3,3) = BSPLE3_DER ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
      DO 440 J4=-DEG,0
         BSPL(J4,4) = BSPLE3_VAL ( DIMS(4), ARG_ARR_4, INDS(4)+J4, ARGS(4) )
         DSPL(J4,4) = BSPLE3_DER ( DIMS(4), ARG_ARR_4, INDS(4)+J4, ARGS(4) )
 440  CONTINUE 
!
! --- Compute the tensor product of B-splines
!
      VAL = 0.0D0
      DER = 0.0D0
      DO 450 J5=-DEG,0
         DO 460 J6=-DEG,0
            DO 470 J7=-DEG,0
               DO 480 J8=-DEG,0
                  VAL = VAL + &
     &                  BCF(INDS(1)+J8,INDS(2)+J7,INDS(3)+J6,INDS(4)+J5)* &
     &                  BSPL(J8,1)*BSPL(J7,2)*BSPL(J6,3)*BSPL(J5,4)
                  DER(1) = DER(1) + &
     &                     BCF(INDS(1)+J8,INDS(2)+J7,INDS(3)+J6,INDS(4)+J5)* &
     &                     DSPL(J8,1)*BSPL(J7,2)*BSPL(J6,3)*BSPL(J5,4)
                  DER(2) = DER(2) + &
     &                     BCF(INDS(1)+J8,INDS(2)+J7,INDS(3)+J6,INDS(4)+J5)* &
     &                     BSPL(J8,1)*DSPL(J7,2)*BSPL(J6,3)*BSPL(J5,4)
                  DER(3) = DER(3) + &
     &                     BCF(INDS(1)+J8,INDS(2)+J7,INDS(3)+J6,INDS(4)+J5)* &
     &                     BSPL(J8,1)*BSPL(J7,2)*DSPL(J6,3)*BSPL(J5,4)
                  DER(4) = DER(4) + &
     &                     BCF(INDS(1)+J8,INDS(2)+J7,INDS(3)+J6,INDS(4)+J5)* &
     &                     BSPL(J8,1)*BSPL(J7,2)*BSPL(J6,3)*DSPL(J5,4)
 480            CONTINUE 
 470        CONTINUE 
 460     CONTINUE 
 450  CONTINUE 
!
      RETURN
      END  SUBROUTINE  VAL_DER_4D_BSPLE3  !#!#
