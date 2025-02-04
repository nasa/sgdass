      FUNCTION   BSPLE4_VAL ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE4_VAL computes the value of the normalized B-spline *
! *   of the 4th degree defined at the knots KNOT, KNOT+1, ...           *
! *   KNOT+4 the array ARR at the point with argument ARR[INOT].         *
! *   Recurrent formula of de Bor [1972] and Cox [1972] is used.         *
! *   Normalization is chosen in such a way that for any given sequence  *
! *   of N knots ( N > IDEG ) and any given X                            *
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   It is assumed that ARR is an extended sequence of nodes            *
! *   [-2,-1,0, 1, 2, 3 ... N, N+1, N+2, N+3] annd                       *
! *   ARR(K) - ARR(K-1) > EPS, where EPS is small number, say 1.D-12.    *
! *                                                                      *
! *   The function uses recurrent expression for the B-spline, but that  *
! *   relantionship is completely unraveled for code optimization.       *
! *   The code is produced by loop unravlling and manual tuning.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*8    ) -- Array of knots used for computation of the    *
! *                        B-spline. Dimension: [-2,MARR+3].             *
! *  KNOT ( REAL*8    ) -- The leading knot of the spline. The spline is *
! *                        zero at                                       *
! *                        [-inf, ARR(KNOT)) and [ARR(KNOT+4), +inf].    *
! *                        Since the first (and the last) knot of the    *
! *                        sequence are quasu-multiple, splines of       *
! *                        1-IDEG, ..., -2, -1, 0  are defined.          *
! *  ARG  ( REAL*8    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPL_VAL> ( REAL*8    ) -- The value of the B-spline.               *
! *                                                                      *
! * ###  07-AUG-2017  BSPLE4_VAL  v1.0 (c)  L. Petrov  07-AUG-2017  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT
      INTEGER*4    IDEG
      PARAMETER  ( IDEG=4 ) ! maximal order of B-spline
      REAL*8     ARR(1-IDEG:MAR), ARG
      REAL*8     BSPLE4_VAL
      REAL*8,    EXTERNAL :: BSPLE3_VAL
!
      BSPLE4_VAL = (ARG - ARR(KNOT))/(ARR(KNOT+IDEG) - ARR(KNOT))* &
     &                               BSPLE3_VAL ( MAR, ARR(1-IDEG+1), KNOT, ARG ) + &
     &             (ARG - ARR(KNOT+IDEG+1))/(ARR(KNOT+1) - ARR(KNOT+IDEG+1))* &
     &                                      BSPLE3_VAL ( MAR, ARR(1-IDEG+1), KNOT+1, ARG ) 
      RETURN
      END  FUNCTION   BSPLE4_VAL  !#!  
