      FUNCTION   BSPLE3_VAL ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_VAL computes the value of the normalized B-spline *
! *   of the 3rd degree defined at the knots KNOT, KNOT+1, ...           *
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
! * ###  28-MAR-2014  BSPLE3_VAL  v2.0 (c)  L. Petrov  15-MAY-2017  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT
      INTEGER*4    IDEG
      PARAMETER  ( IDEG=3 ) ! maximal order of B-spline
      REAL*8     BSPLE3_VAL, ARR(1-IDEG:MAR+IDEG), ARG
      REAL*8     A(IDEG+1), AL, AR
!
! --- Check whether the argument is in reasonable range
!
      IF ( ARG .LT. ARR(1)  .OR.  ARG .GT. ARR(MAR) ) THEN
           BSPLE3_VAL = 0.0D0
           RETURN
      END IF
!
      IF ( ARG .GE. ARR(KNOT) .AND. ARG .LT. ARR(KNOT+1) ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+1) - ARR(KNOT))
        ELSE
           AL = 0.0D0
      END IF
      IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
           AR = (ARG - ARR(KNOT+2))/(ARR(KNOT+1) - ARR(KNOT+2))
           A(1) = AL + AR
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
        ELSE
           A(1) = AL
           AL = 0.0D0
      END IF
      IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
           AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
           A(2) = AL + AR
           AL = (ARG - ARR(KNOT+2))/(ARR(KNOT+3) - ARR(KNOT+2))
        ELSE
           A(2) = AL
           AL = 0.0D0
      END IF
      IF ( ARG .GE. ARR(KNOT+3) .AND. ARG .LT. ARR(KNOT+4) ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+3) - ARR(KNOT+4))
        ELSE
           AR = 0.0D0
      END IF
      A(3) = AL + AR
!
      IF ( A(1) .NE. 0.0D0 ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+2)   - ARR(KNOT))*A(1)
         ELSE
           AL = 0.0D0
      END IF
      IF ( A(2) .NE. 0.0D0 ) THEN
           AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+1) - ARR(KNOT+3))*A(2)
         ELSE
           AR = 0.0D0
      END IF
      A(1) = AL + AR
!
      IF ( A(2) .NE. 0.0D0 ) THEN
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+3) - ARR(KNOT+1))*A(2)
         ELSE
           AL = 0.0D0
      END IF
      IF ( A(3) .NE. 0.0D0 ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+2) - ARR(KNOT+4))*A(3)
         ELSE 
           AR = 0.0D0
      END IF
      A(2) = AL + AR
!
      IF ( A(1) .NE. 0.0D0 ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+3)   - ARR(KNOT))*A(1)
         ELSE
           AL = 0.0D0
      END IF
      IF ( A(2) .NE. 0.0D0 ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+1) - ARR(KNOT+4))*A(2)
         ELSE
           AR = 0.0D0
      END IF
      BSPLE3_VAL = AL + AR
!
      RETURN
      END  !#!  BSPLE3_VAL  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPLE3_DER ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_DER computes the first derivative of the          *
! *   normalized B-spline of the 3rd degree defined at the sequence      *
! *   of knots KNOT, KNOT+1, ... KNOT+4 of the array ARR at the point    *
! *   with argument ARG. Normalization is chosen in such a way that for  *
! *   any given sequence of N knots ( N > IDEG ) and any given X         *
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
! *                        B-spline.                                     *
! *  KNOT ( REAL*8    ) -- The leading knot of the spline. Spline is     *
! *                        zero at  [-inf, ARR(KNOT)) and                *
! *                                 [ARR(KNOT+IDEG+1), +inf].            *
! *                        Since the first (and the last) knot are       *
! *                        multiple, splines of 1-IDEG, ..., -2, -1, 0   *
! *                        are defined.                                  *
! *  ARG  ( REAL*8    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPLE3_DER> ( REAL*8    ) -- First derivative of the B-spline.      *
! *                                                                      *
! *  ### 01-MAY-2003   BSPLE3_DER   v2.0 (c)  L. Petrov 15-MAY-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT
      INTEGER*4  IDEG
      PARAMETER  ( IDEG = 3 )
      REAL*8     BSPLE3_DER, ARR(1-IDEG:MAR+IDEG), ARG
      REAL*8     A(IDEG), AL, AR, AL3, AR3
      REAL*8     BSPLE2_VAL, BSPL2_VAL, BSPL_VAL
!
! --- First, process the lower part
!
      IF ( ARG .GE. ARR(KNOT) .AND. ARG .LT. ARR(KNOT+1) ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+1) - ARR(KNOT))
           IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
                AR = (ARG - ARR(KNOT+2))/(ARR(KNOT+1) - ARR(KNOT+2))
                A(1) = AL + AR
                AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
             ELSE
                A(1) = AL
                AL   = 0.0D0
           END IF
        ELSE
           IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
                AR = (ARG - ARR(KNOT+2))/(ARR(KNOT+1) - ARR(KNOT+2))
                A(1) = AR
                AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
             ELSE
                A(1) = 0.0D0
                AL   = 0.0D0
           END IF
      END IF
!
      IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
           AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
           A(2) = AL + AR
           AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+1) - ARR(KNOT+3))*A(2)
           AL = (ARG - ARR(KNOT)  )/(ARR(KNOT+2) - ARR(KNOT)  )*A(1)
        ELSE
           AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+1) - ARR(KNOT+3))*AL
           AL = (ARG - ARR(KNOT)  )/(ARR(KNOT+2) - ARR(KNOT)  )*A(1)
      END IF
!
      AL3 = 3.0D0/(ARR(KNOT+3) - ARR(KNOT) )*(AL+AR)
!
! --- Now process the upper part
!
      IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
           IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
                AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
                A(1) = AL + AR
                AL = (ARG - ARR(KNOT+2))/(ARR(KNOT+3) - ARR(KNOT+2))
             ELSE
                A(1) = AL
                AL   = 0.0D0
           END IF
        ELSE
           IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
                AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
                A(1) = AR
                AL = (ARG - ARR(KNOT+2))/(ARR(KNOT+3) - ARR(KNOT+2))
             ELSE
                A(1) = 0.0D0
                AL   = 0.0D0
           END IF
      END IF
!
      IF ( ARG .GE. ARR(KNOT+3) .AND. ARG .LT. ARR(KNOT+4) ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+3) - ARR(KNOT+4))
           A(2) = AL + AR
        ELSE
           A(2) = AL
      END IF
!
      IF ( A(1) .NE. 0.0D0 ) THEN
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+3) - ARR(KNOT+1))*A(1)
         ELSE
           AL = 0.0
      END IF
      IF ( A(2) .NE. 0.0D0 ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+2) - ARR(KNOT+4))*A(2)
         ELSE
           AR = 0.0
      END IF
      AR3 = 3.0D0/(ARR(KNOT+1) - ARR(KNOT+4))*(AL+AR)
!
      BSPLE3_DER = AL3 + AR3
!
      RETURN
      END  FUNCTION  BSPLE3_DER  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPLE3_DR2 ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_DR2 computes the second derivative of the         *
! *   normalized B-spline of the 3rd degree defined at the sequence      *
! *   of knots KNOT, KNOT+1, ... KNOT+4 of the array ARR at the point    *
! *   with argument ARG. Normalization is chosen in such a way that for  *
! *   any given sequence of N knots ( N > IDEG ) and any given X         *
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
! *                        B-spline.                                     *
! *  KNOT ( REAL*8    ) -- The leading knot of the spline. Spline is     *
! *                        zero at                                       *
! *                        [-inf, ARR(KNOT)) and [ARR(KNOT+4), +inf].    *
! *                        Since the first (and the last) knot are       *
! *                        multiple, splines of 1-IDEG, ..., -2, -1, 0   *
! *                        are defined.                                  *
! *  ARG  ( REAL*8    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPLE3_DR2> ( REAL*8    ) -- First derivative of the B-spline.      *
! *                                                                      *
! *  ### 01-MAY-2003    BSPLE3_DR2    v2.0 (c) L. Petrov 15-MAY-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      PARAMETER  ( IDEG = 3 )
      REAL*8     BSPLE3_DR2, ARR(-2:MAR+3), ARG
      REAL*8     AL, AR, DR1L, DR1R, B1S0, B1S1, B1S2, B2S0, B2S1
!
      B1S0 = 0.0D0
      B1S1 = 0.0D0
      B1S2 = 0.0D0
!
      IF ( ARG .GE. ARR(KNOT) .AND. ARG .LT. ARR(KNOT+1) ) THEN
           B1S0 = (ARG - ARR(KNOT))/(ARR(KNOT+1) - ARR(KNOT))
         ELSE IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
           B1S0 = (ARG - ARR(KNOT+2))/(ARR(KNOT+1) - ARR(KNOT+2))
           B1S1 = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
         ELSE IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
           B1S1 = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
           B1S2 = (ARG - ARR(KNOT+2))/(ARR(KNOT+3) - ARR(KNOT+2))
         ELSE IF ( ARG .GE. ARR(KNOT+3) .AND. ARG .LT. ARR(KNOT+4) ) THEN
           B1S2 = (ARG - ARR(KNOT+4))/(ARR(KNOT+3) - ARR(KNOT+4))
      END IF
!
      IF ( B1S0 .NE. 0.0D0 ) THEN
           AL = 2.0D0/(ARR(KNOT+2) - ARR(KNOT)  )*B1S0
         ELSE
           AL = 0.0D0
      END IF
      IF ( B1S1 .NE. 0.0D0 ) THEN
           AR = 2.0D0/(ARR(KNOT+1) - ARR(KNOT+3))*B1S1
         ELSE
           AR = 0.0D0
      END IF
      B2S0 = AL + AR
!
      IF ( B1S1 .NE. 0.0D0 ) THEN
           AL = 2.0D0/(ARR(KNOT+3) - ARR(KNOT+1) )*B1S1
         ELSE
           AL = 0.0D0
      END IF
      IF ( B1S2 .NE. 0.0D0 ) THEN
           AR = 2.0D0/(ARR(KNOT+2) - ARR(KNOT+4) )*B1S2
         ELSE
           AR = 0.0D0
      END IF
      B2S1 = AL + AR
!
      IF ( B2S0 .NE. 0.0D0 ) THEN
           AL = 3.0D0/(ARR(KNOT+3) - ARR(KNOT)  )*B2S0
         ELSE
           AL = 0.0D0
      END IF
      IF ( B2S1 .NE. 0.0D0 ) THEN
           AR = 3.0D0/(ARR(KNOT+1) - ARR(KNOT+4))*B2S1
         ELSE
           AR = 0.0D0
      END IF
!
      BSPLE3_DR2 = AL + AR
!
      RETURN
      END  FUNCTION  BSPLE3_DR2  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPLE3_INT ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_INT  computes the integral of the normalized      *
! *   B-spline of the IDEG degree defined at the sequence of knots       *
! *   KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR in the limits       *
! *   from minus infinity to ARG. Normalization is chosen in such        *
! *   a way that for any given sequence of N knots ( N > IDEG ) and any  *
! *   given X                                                            *
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   The first and the last knots have multiplicity IDEG.               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*8    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
! *  KNOT ( REAL*8    ) -- The leading knot of the spline. Spline is     *
! *                        zero at  [-inf, ARR(KNOT)) and                *
! *                                 [ARR(KNOT+IDEG+1), +inf].            *
! *                        Since the first (and the last) knot are       *
! *                        multiple, splines of 1-IDEG, ..., -2, -1, 0   *
! *                        are defined.                                  *
! *  ARG  ( REAL*8    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPL_INT> ( REAL*8    ) -- Integral of the B-spline in the limits   *
! *                             from minus infinity to ARG.              *
! *                                                                      *
! *  ### 07-AUG-2017 BSPLE3_INT    v1.9 (c)  L. Petrov  07-AUG-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT
      INTEGER*4  IDEG
      PARAMETER  ( IDEG = 3 )
      REAL*8     BSPLE3_INT, ARR(1-IDEG:MAR), ARG
      REAL*8     BSPLE4_VAL
      INTEGER*4  IND_L, IND_R, J1
!
      BSPLE3_INT = 0.0
      IF ( IDEG .LT. 0 ) RETURN
!
      IND_L = KNOT
      IF ( IND_L .GT. MAR ) IND_L = MAR
      IF ( IND_L .LT.   1 ) IND_L = 1
!
      IND_R = KNOT + IDEG + 1
      IF ( IND_R .GT. MAR ) IND_R = MAR
      IF ( IND_R .LT.   1 ) IND_R = 1
!
      IF ( ARG .LE. ARR(IND_L) ) THEN
           BSPLE3_INT = 0.0D0
         ELSE IF ( ARG .GE. ARR(IND_R) ) THEN
!
! -------- Use the a unity partitioning property
!
           BSPLE3_INT = 1.0D0
         ELSE
           DO 410 J1=KNOT,KNOT+IDEG+1
              IF ( J1 .GE. MAR ) GOTO 410
              BSPLE3_INT = BSPLE3_INT + BSPLE4_VAL ( MAR, %VAL(LOC(ARR(-2))-8), J1, ARG )
 410       CONTINUE
      END IF
!
      BSPLE3_INT = BSPLE3_INT * ( ARR(IND_R) - ARR(IND_L) )/( IDEG + 1 )
!
      RETURN
      END  !#!  BSPLE3_INT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPLE3_KINT ( MAR, ARR, BSPL, X_DOWN, X_UP )
! ************************************************************************
! *                                                                      *
! *   Function SPLE3_KINT computes an integral within limits from X_DOWN *
! *   till X_UP of the function expanded over basis of B-splines of      *
! *   the 3rd degree.                                                    *
! *                                                                      *
! *   At a given sequence of knots ARR(1), ARR(2), ... ARR(MAR), the     *
! *   first and the last knots are multiple with the miltiplicity        *
! *   IDEG+1, other knots being simple.                                  *
! *                                                                      *
! *   The spline is defined as                                           *
! *                                                                      *
! *     S(x) = sum_{k=1-ideg}^(k=mar-1) coef(k+ideg) bspl^ideg_k(x)      *
! *                                                                      *
! *   where bspl^m_k(x) is a B-spline of degree IDEG, at knots           *
! *   K, K+1, ... K+IDEG+1.                                              *
! *                                                                      *
! *   Array of coefficients has dimension MARR+IDEG-1                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   MARR ( INTEGER*4 ) -- The total number of points in the array ARR. *
! *    ARR ( REAL*8    ) -- Array of knots at which the spline is        *
! *                         defined. Dimension: [-3:MARR].               *
! *   BSPL ( REAL*8    ) -- Array of coefficents of expamsion of the     *
! *                         spline on the basis of B-splines.            *
! *                         Dimension: [-3:MARR].                        *
! * X_DOWN ( REAL*8    ) -- The lower limit of integration.              *
! *                         If X_DOWN > ARR(MARR), then the integral is  *
! *                         set to zero.                                 *
! *   X_UP ( REAL*8    ) -- The Upper limit of integration.              *
! *                         If X_UP < ARR(1), then the integral is set   *
! *                         to zero.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <SPLE3_KINT> ( REAL*8 ) -- Integral of the spline of the 3rd degree  *
! *                            in the limits [X_DOWN, X_UP].             *
! *                                                                      *
! *  ### 07-AUG-2017  SPLE3_KINT   v1.0 (c)  L. Petrov  07-AUG-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     SPLE3_KINT
      INTEGER*4  MAR
      INTEGER*4  IDEG
      PARAMETER  ( IDEG = 3 )
      REAL*8     ARR(1-IDEG:MAR), BSPL(1-IDEG:MAR), X_DOWN, X_UP
      INTEGER*4  IND_L, IND_R, J1, J2, J3, J4, J5, J6, J7, IB
      REAL*8     BSPL_INT, BSPLE3_INT
!
      SPLE3_KINT = 0.0D0
      IF ( IDEG .LT. 0 ) THEN
           RETURN
      END IF
!
      IF ( X_UP .LE. ARR(1) ) THEN
           CONTINUE
         ELSE IF ( X_DOWN .LE. ARR(1) .AND. X_UP .GE. ARR(MAR) ) THEN
!
! -------- The case when the upper limit is equal or greater than the last knot
! -------- of the spline. These is some simplification for this case.
!
           DO 410 J1=1-IDEG,MAR-1
              IND_L = J1+IDEG+1
              IND_R = J1
              IF ( IND_L .LT.   1 ) IND_L = 1
              IF ( IND_L .GT. MAR ) IND_L = MAR
              IF ( IND_R .LT.   1 ) IND_R = 1
              IF ( IND_R .GT. MAR ) IND_R = MAR
              SPLE3_KINT = SPLE3_KINT + BSPL(J1)* ( ARR(IND_L) - ARR(IND_R) )
 410       CONTINUE
           SPLE3_KINT = SPLE3_KINT/(IDEG+1)
         ELSE
!
! -------- The case when the upper limit of integration is within the
! -------- range of the spline
!
           DO 420 J2=1,MAR-1
              IF ( ARR(J2+1) .LE. X_DOWN ) THEN
                   GOTO 420
                ELSE IF ( ARR(J2) .GE. X_UP   ) THEN
                   GOTO 420
                ELSE IF ( X_DOWN > ARR(J2) .AND. X_UP .GE. ARR(J2+1) ) THEN
!
! -----------------   |____#####|
!
                   DO 440 J4=J2-IDEG,J2
                      SPLE3_KINT = SPLE3_KINT + BSPL(J4)* ( BSPLE3_INT ( MAR, ARR, J4, ARR(J2+1) ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J4, X_DOWN    )   )
 440               CONTINUE 
                ELSE IF ( X_DOWN .LE. ARR(J2) .AND. X_UP < ARR(J2+1) ) THEN
!
! -----------------   |#####____|
!
                   DO 450 J5=J2-IDEG,J2
                      SPLE3_KINT = SPLE3_KINT + BSPL(J5)* ( BSPLE3_INT ( MAR, ARR, J5, X_UP    ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J5, ARR(J2) )   )
 450               CONTINUE 
                ELSE IF ( X_DOWN .GE. ARR(J2) .AND. X_UP .LE. ARR(J2+1) ) THEN
!
! -----------------   |__#####__|
!
                   DO 460 J6=J2-IDEG,J2
                      SPLE3_KINT = SPLE3_KINT + BSPL(J6)* ( BSPLE3_INT ( MAR, ARR, J6, X_UP    ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J6, X_DOWN  )   )
 460               CONTINUE 
                ELSE
!
! -----------------   |#########|
!
                   DO 470 J7=J2-IDEG,J2
                      SPLE3_KINT = SPLE3_KINT + BSPL(J7)* ( BSPLE3_INT ( MAR, ARR, J7, ARR(J2+1) ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J7, ARR(J2)   )   )
 470               CONTINUE 
              END IF
 420       CONTINUE
      END IF
!
      RETURN
      END  !#!  SPLE3_KINT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPLE3_KINTI ( MAR, ARR, BSPL, X_DOWN, X_UP, IND_DOWN, IND_UP )
! ************************************************************************
! *                                                                      *
! *   Function SPLE3_KINTI computes an integral within limits from X_DOWN*
! *   till X_UP of the function expanded over basis of B-splines of      *
! *   the 3rd degree.                                                    *
! *                                                                      *
! *   At a given sequence of knots ARR(1), ARR(2), ... ARR(MAR), the     *
! *   first and the last knots are multiple with the miltiplicity        *
! *   IDEG+1, other knots being simple.                                  *
! *                                                                      *
! *   The spline is defined as                                           *
! *                                                                      *
! *     S(x) = sum_{k=1-ideg}^(k=mar-1) coef(k+ideg) bspl^ideg_k(x)      *
! *                                                                      *
! *   where bspl^m_k(x) is a B-spline of degree IDEG, at knots           *
! *   K, K+1, ... K+IDEG+1.                                              *
! *                                                                      *
! *   Array of coefficients has dimension MARR+IDEG-1                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   MARR ( INTEGER*4 ) -- The total number of points in the array ARR. *
! *    ARR ( REAL*8    ) -- Array of knots at which the spline is        *
! *                         defined. Dimension: [-3:MARR].               *
! *   BSPL ( REAL*8    ) -- Array of coefficents of expamsion of the     *
! *                         spline on the basis of B-splines.            *
! *                         Dimension: [-3:MARR].                        *
! * X_DOWN ( REAL*8    ) -- The lower limit of integration.              *
! *                         If X_DOWN > ARR(MARR), then the integral is  *
! *                         set to zero.                                 *
! *   X_UP ( REAL*8    ) -- The Upper limit of integration.              *
! *                         If X_UP < ARR(1), then the integral is set   *
! *                         to zero.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <SPLE3_KINT> ( REAL*8 ) -- Integral of the spline of the 3rd degree  *
! *                            in the limits [X_DOWN, X_UP].             *
! *                                                                      *
! *  ### 07-AUG-2017  SPLE3_KINTI  v1.0 (c)  L. Petrov  07-AUG-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     SPLE3_KINTI
      INTEGER*4  MAR, IND_UP, IND_DOWN
      INTEGER*4  IDEG
      PARAMETER  ( IDEG = 3 )
      REAL*8     ARR(1-IDEG:MAR), BSPL(1-IDEG:MAR), X_DOWN, X_UP
      INTEGER*4  IND_L, IND_R, J1, J2, J3, J4, J5, J6, J7, IB
      REAL*8     BSPL_INT, BSPLE3_INT
!
      SPLE3_KINTI = 0.0D0
      IF ( IDEG .LT. 0 ) THEN
           RETURN
      END IF
!
      IF ( X_UP .LE. ARR(1) ) THEN
           CONTINUE
         ELSE IF ( X_DOWN .LE. ARR(1) .AND. X_UP .GE. ARR(MAR) ) THEN
!
! -------- The case when the upper limit is equal or greater than the last knot
! -------- of the spline. These is some simplification for this case.
!
           DO 410 J1=1-IDEG,MAR-1
              IND_L = J1+IDEG+1
              IND_R = J1
              IF ( IND_L .LT.   1 ) IND_L = 1
              IF ( IND_L .GT. MAR ) IND_L = MAR
              IF ( IND_R .LT.   1 ) IND_R = 1
              IF ( IND_R .GT. MAR ) IND_R = MAR
              SPLE3_KINTI = SPLE3_KINTI + BSPL(J1)* ( ARR(IND_L) - ARR(IND_R) )
 410       CONTINUE
           SPLE3_KINTI = SPLE3_KINTI/(IDEG+1)
         ELSE
!
! -------- The case when the upper limit of integration is within the
! -------- range of the spline
!
           DO 420 J2=IND_DOWN,IND_UP
              IF ( ARR(J2+1) .LE. X_DOWN ) THEN
                   GOTO 420
                ELSE IF ( ARR(J2) .GE. X_UP   ) THEN
                   GOTO 420
                ELSE IF ( X_DOWN > ARR(J2) .AND. X_UP .GE. ARR(J2+1) ) THEN
!
! -----------------   |____#####|
!
                   DO 440 J4=J2-IDEG,J2
                      SPLE3_KINTI = SPLE3_KINTI + BSPL(J4)* ( BSPLE3_INT ( MAR, ARR, J4, ARR(J2+1) ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J4, X_DOWN    )   )
 440               CONTINUE 
                ELSE IF ( X_DOWN .LE. ARR(J2) .AND. X_UP < ARR(J2+1) ) THEN
!
! -----------------   |#####____|
!
                   DO 450 J5=J2-IDEG,J2
                      SPLE3_KINTI = SPLE3_KINTI + BSPL(J5)* ( BSPLE3_INT ( MAR, ARR, J5, X_UP    ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J5, ARR(J2) )   )
 450               CONTINUE 
                ELSE IF ( X_DOWN .GE. ARR(J2) .AND. X_UP .LE. ARR(J2+1) ) THEN
!
! -----------------   |__#####__|
!
                   DO 460 J6=J2-IDEG,J2
                      SPLE3_KINTI = SPLE3_KINTI + BSPL(J6)* ( BSPLE3_INT ( MAR, ARR, J6, X_UP    ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J6, X_DOWN  )   )
 460               CONTINUE 
                ELSE
!
! -----------------   |#########|
!
                   DO 470 J7=J2-IDEG,J2
                      SPLE3_KINTI = SPLE3_KINTI + BSPL(J7)* ( BSPLE3_INT ( MAR, ARR, J7, ARR(J2+1) ) - &
     &                                                      BSPLE3_INT ( MAR, ARR, J7, ARR(J2)   )   )
 470               CONTINUE 
              END IF
 420       CONTINUE
      END IF
!
      RETURN
      END  FUNCTION  SPLE3_KINTI  !#!#
