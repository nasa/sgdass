      SUBROUTINE VAL_DER_3D_BSPLE4 ( ARGS, DEG, DIMS, INDS, ARG_ARR_1, &
     &                               ARG_ARR_2, ARG_ARR_3, BCF, &
     &                               VAL, DER )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_3D_BSPL4  computes the value of the 3D spline of the   *
! *   DEG th degree at the point with coordinates                        *
! *   (ARG_1, ARG_2, ARG_3) and a vector or partial derivatives over all *
! *   arguments.                                                         *
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
! * ### 01-FEB-2008  VAL_DER_3D_BSPL4 v1.0 (c) L. Petrov 04-FEB-2008 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, INDS(3), DIMS(3)
      REAL*4     ARGS(3), &
     &           ARG_ARR_1(1-DEG:DIMS(1)), &
     &           ARG_ARR_2(1-DEG:DIMS(2)), &
     &           ARG_ARR_3(1-DEG:DIMS(3)), &
     &           BCF(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3))
      REAL*4     VAL, DER(3)
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 16 )
      REAL*8     BSPL(-MDEG:0,3), DSPL(-MDEG:0,3) !!
      INTEGER*4  J1, J2, J3, J4, J5, J6
      REAL*8,    EXTERNAL :: BSPLE3_VAL_R3, BSPLE3_DER_R4  !!
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
!! write ( 6, * ) 'get_refr_der_hlp 116 inds= ', inds, ' args= ', args ; call flush(6) ;  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO 410 J1=-DEG,0
         BSPL(J1,1) = BSPLE3_VAL_R3 ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
         DSPL(J1,1) = BSPLE3_DER_R4 ( DIMS(1), ARG_ARR_1, INDS(1)+J1, ARGS(1) )
 410  CONTINUE 
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPLE3_VAL_R3 ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
         DSPL(J2,2) = BSPLE3_DER_R4 ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPLE3_VAL_R3 ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
         DSPL(J3,3) = BSPLE3_DER_R4 ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
!! write ( 6, * ) 'get_refr_der_hlp 129 inds= ', inds, ' args= ', args ; call flush(6) ;  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
!! write ( 6, * ) 'get_refr_der_hlp 153' ; call flush(6) ; ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      RETURN
      END  SUBROUTINE  VAL_DER_3D_BSPLE4  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPLE3_VAL_R3 ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_VAL_R3 computes the value of the normalized       *
! *   B-spline  of the 3rd degree defined at the knots KNOT, KNOT+1, ... *
! *   KNOT+4 of the array ARR at the point with argument ARR[INOT].      *
! *   Recurrent formula of de Bor [1972] and Cox [1972] is used.         *
! *   Normalization is chosen in such a way that for any given sequence  *
! *   of N knots ( N > IDEG ) and any given X                            *
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   It is assimed that ARR is an extended sequence of nodes            *
! *   [-2,-1,0, 1, 2, 3 ... N, N+1, N+2, N+3] annd                       *
! *   ARR(K) - ARR(K-1) > EPS, where EPS is small number, say 1.D-12.    *
! *                                                                      *
! *   The function uses recurrent expression for the B-spline, but it    *
! *   does not uses explicit recursion in source code.                   *
! *                                                                      *
! *   The code is produced by loop unravlling and manual tuning.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*4    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  KNOT ( REAL*4    ) -- The leading knot of the spline. The spline is *
! *                        zero at  [-inf, ARR(KNOT)) and                *
! *                                 [ARR(KNOT+IDEG+1), +inf].            *
! *                        Since the first (and the last) knot of the    *
! *                        sequence are multiple, splines of             *
! *                        1-IDEG, ..., -2, -1, 0  are defined.          *
! *  ARG  ( REAL*4    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPL_VAL> ( REAL*4    ) -- The value of the B-spline.               *
! *                                                                      *
! * ### 28-MAR-2014  BSPLE3_VAL_R3  v1.0 (c)  L. Petrov  28-MAR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT
      INTEGER*4    IDEG
      PARAMETER  ( IDEG=3 ) ! maximal order of B-spline
      REAL*4     BSPLE3_VAL_R3, ARR(1-IDEG:MAR+IDEG), ARG
      REAL*4     A(IDEG), AL, AR
      INTEGER*4, external ::   omp_get_thread_num
!
! --- Check whether the argument is in reasonable range
!
      IF ( ARG .LT. ARR(1)  .OR.  ARG .GT. ARR(MAR) ) THEN
           BSPLE3_VAL_R3 = 0.0
           RETURN
      END IF
!
      IF ( ARG .GE. ARR(KNOT) .AND. ARG .LT. ARR(KNOT+1) ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+1) - ARR(KNOT))
        ELSE
           AL = 0.0
      END IF
      IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
           AR = (ARG - ARR(KNOT+2))/(ARR(KNOT+1) - ARR(KNOT+2))
           A(1) = AL + AR
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
        ELSE
           A(1) = AL
           AL = 0.0
      END IF
!
      IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
           AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
           A(2) = AL + AR
           AL = (ARG - ARR(KNOT+2))/(ARR(KNOT+3) - ARR(KNOT+2))
        ELSE
           A(2) = AL
           AL = 0.0
      END IF
      IF ( ARG .GE. ARR(KNOT+3) .AND. ARG .LT. ARR(KNOT+4) ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+3) - ARR(KNOT+4))
        ELSE
           AR = 0.0
      END IF
      A(3) = AL + AR
!
      AL = (ARG - ARR(KNOT))/(ARR(KNOT+2)   - ARR(KNOT))*A(1)
      AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+1) - ARR(KNOT+3))*A(2)
      A(1) = AL + AR
!
   write ( 6, * ) 'MMM-1 ', omp_get_thread_num(), ' arg= ', arg, ' arr= ', arr(knot:knot+4), ' a2/3= ', a(2), a(3); call flush(6) ! %%%%%
      AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+3) - ARR(KNOT+1))*A(2)
   write ( 6, * ) 'MMM-2 ', omp_get_thread_num(), ' arg= ', arg, ' arr= ', arr(knot:knot+4), ' al= ', al; call flush(6) ! %%%%%
      AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+2) - ARR(KNOT+4))*A(3)
   write ( 6, * ) 'MMM-3 ', omp_get_thread_num(), ' arg= ', arg, ' arr= ', arr(knot:knot+4), ' al= ', al, ' ar= ', ar; call flush(6)  ! %%
      A(2) = AL + AR
   write ( 6, * ) 'MMM-4 ', omp_get_thread_num(), ' arg= ', arg, ' arr= ', arr(knot:knot+4); call flush(6) ! %%%%%
!
      AL = (ARG - ARR(KNOT))/(ARR(KNOT+3)   - ARR(KNOT))*A(1)
      AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+1) - ARR(KNOT+4))*A(2)
      BSPLE3_VAL_R3 = AL + AR
!
      RETURN
      END  !#!  BSPLE3_VAL_R3  #!#
