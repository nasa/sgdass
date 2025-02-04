      FUNCTION   BSPLE3_VAL_R4 ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_VAL_R4 computes the value of the normalized       *
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
! * ### 28-MAR-2014  BSPLE3_VAL_R4  v2.0 (c)  L. Petrov  15-MAY-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT
      INTEGER*4    IDEG
      PARAMETER  ( IDEG=3 ) ! maximal order of B-spline
      REAL*4     BSPLE3_VAL_R4, ARR(1-IDEG:MAR+IDEG), ARG
      REAL*4     A(IDEG+1), AL, AR
!
! --- Check whether the argument is in reasonable range
!
      IF ( ARG .LT. ARR(1)  .OR.  ARG .GT. ARR(MAR) ) THEN
           BSPLE3_VAL_R4 = 0.0
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
      IF ( A(1) .NE. 0.0 ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+2)   - ARR(KNOT))*A(1)
         ELSE
           AL = 0.0
      END IF
      IF ( A(2) .NE. 0.0 ) THEN
           AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+1) - ARR(KNOT+3))*A(2)
         ELSE
           AR = 0.0
      END IF
      A(1) = AL + AR
!   
      IF ( A(2) .NE. 0.0 ) THEN
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+3) - ARR(KNOT+1))*A(2)
         ELSE
           AL = 0.0
      END IF           
      IF ( A(3) .NE. 0.0 ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+2) - ARR(KNOT+4))*A(3)
         ELSE
           AR = 0.0
      ENDIF
      A(2) = AL + AR
!
      IF ( A(1) .NE. 0.0 ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+3)   - ARR(KNOT))*A(1)
         ELSE 
           AL = 0.0
      END IF
      IF ( A(2) .NE. 0.0 ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+1) - ARR(KNOT+4))*A(2)
         ELSE 
           AR = 0.0
      END IF
!
      BSPLE3_VAL_R4 = AL + AR
!
      RETURN
      END  !#!  BSPLE3_VAL_R4  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPLE3_DER_R4 ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_DER_R4 computes first derivative of the           *
! *   normalized B-spline of the IDEG degree defined at the sequence     *
! *   of knots KNOT, KNOT+1, ... KNOT+4 of the array ARR at the point    *
! *   with argument ARG. Normalization is chosen in such a way that for  *
! *   any given sequence of N knots ( N > IDEG ) and any given X         *                                          
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   The first and the last knots have multiplicity IDEG.               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*4    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  KNOT ( REAL*4    ) -- The leading knot of the spline. Spline is     *
! *                        zero at  [-inf, ARR(KNOT)) and                *
! *                                 [ARR(KNOT+IDEG+1), +inf].            *
! *                        Since the first (and the last) knot are       *
! *                        multiple, splines of 1-IDEG, ..., -2, -1, 0   *
! *                        are defined.                                  *
! *  ARG  ( REAL*4    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPLE3_DER_R4> ( REAL*4    ) -- First derivative of the B-spline.   *
! *                                                                      *
! *  ### 01-MAY-2003   BSPLE3_DER_R4  v2.0 (c) L. Petrov 15-MAY-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT
      INTEGER*4  IDEG
      PARAMETER  ( IDEG = 3 )
      REAL*4     BSPLE3_DER_R4, ARR(1-IDEG:MAR+IDEG), ARG
      REAL*4     A(IDEG), AL, AR, AL3, AR3
      REAL*4     BSPLE2_VAL_R4, BSPL2_VAL, BSPL_VAL
!
      IF ( ARG .GE. ARR(KNOT) .AND. ARG .LT. ARR(KNOT+1) ) THEN
           AL = (ARG - ARR(KNOT))/(ARR(KNOT+1) - ARR(KNOT))
           IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
                AR = (ARG - ARR(KNOT+2))/(ARR(KNOT+1) - ARR(KNOT+2))
                A(1) = AL + AR
                AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
             ELSE
                A(1) = AL
                AL   = 0.0
           END IF
        ELSE
           IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
                AR = (ARG - ARR(KNOT+2))/(ARR(KNOT+1) - ARR(KNOT+2))
                A(1) = AR
                AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
             ELSE
                A(1) = 0.0
                AL   = 0.0
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
      AL3 = 3.0/(ARR(KNOT+3) - ARR(KNOT) )*(AL+AR)
!!
      IF ( ARG .GE. ARR(KNOT+1) .AND. ARG .LT. ARR(KNOT+2) ) THEN
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+2) - ARR(KNOT+1))
           IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
                AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
                A(1) = AL + AR
                AL = (ARG - ARR(KNOT+2))/(ARR(KNOT+3) - ARR(KNOT+2))
             ELSE
                A(1) = AL
                AL   = 0.0
           END IF
        ELSE
           IF ( ARG .GE. ARR(KNOT+2) .AND. ARG .LT. ARR(KNOT+3) ) THEN
                AR = (ARG - ARR(KNOT+3))/(ARR(KNOT+2) - ARR(KNOT+3))
                A(1) = AR
                AL = (ARG - ARR(KNOT+2))/(ARR(KNOT+3) - ARR(KNOT+2))
             ELSE
                A(1) = 0.0
                AL   = 0.0
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
      IF ( A(1) .NE. 0.0 ) THEN
           AL = (ARG - ARR(KNOT+1))/(ARR(KNOT+3) - ARR(KNOT+1))*A(1)
         ELSE
           AL = 0.0
      END IF
      IF ( A(2) .NE. 0.0 ) THEN
           AR = (ARG - ARR(KNOT+4))/(ARR(KNOT+2) - ARR(KNOT+4))*A(2)
         ELSE
           AR = 0.00
      END IF
      AR3 = 3.0/(ARR(KNOT+1) - ARR(KNOT+4))*(AL+AR)
!
      BSPLE3_DER_R4 = AL3 + AR3
!
      RETURN
      END  FUNCTION  BSPLE3_DER_R4  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPLE3_DR2_R4 ( MAR, ARR, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPLE3_DR2_R4 computes first derivative of the           *
! *   normalized B-spline of the IDEG degree defined at the sequence     *
! *   of knots KNOT, KNOT+1, ... KNOT+4 of the array ARR at the point    *
! *   with argument ARG. Normalization is chosen in such a way that for  *
! *   any given sequence of N knots ( N > IDEG ) and any given X         *
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   The first and the last knots have multiplicity IDEG.               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*4    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  KNOT ( REAL*4    ) -- The leading knot of the spline. Spline is     *
! *                        zero at  [-inf, ARR(KNOT)) and                *
! *                                 [ARR(KNOT+IDEG+1), +inf].            *
! *                        Since the first (and the last) knot are       *
! *                        multiple, splines of 1-IDEG, ..., -2, -1, 0   *
! *                        are defined.                                  *
! *  ARG  ( REAL*4    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPLE3_DR2_R4> ( REAL*4    ) -- First derivative of the B-spline.   *
! *                                                                      *
! *  ### 01-MAY-2003   BSPLE3_DR2_R4  v2.0 (c) L. Petrov 15-MAY-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      PARAMETER  ( IDEG = 3 )
      REAL*4     BSPLE3_DR2_R4, ARR(-2:MAR+3), ARG
      REAL*4     AL, AR, DR1L, DR1R, B1S0, B1S1, B1S2, B2S0, B2S1
!
      B1S0 = 0.0
      B1S1 = 0.0
      B1S2 = 0.0
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
      IF ( B1S0 .NE. 0.0 ) THEN
           AL = 2.0/(ARR(KNOT+2) - ARR(KNOT)  )*B1S0
         ELSE
           AL = 0.0
      END IF
      IF ( B1S1 .NE. 0.0 ) THEN
           AR = 2.0/(ARR(KNOT+1) - ARR(KNOT+3))*B1S1
         ELSE
           AR = 0.0
      END IF
      B2S0 = AL + AR
!
      IF ( B1S1 .NE. 0.0 ) THEN
           AL = 2.0/(ARR(KNOT+3) - ARR(KNOT+1) )*B1S1
         ELSE
           AL = 0.0
      END IF
      IF ( B1S2 .NE. 0.0 ) THEN
           AR = 2.0/(ARR(KNOT+2) - ARR(KNOT+4) )*B1S2
         ELSE
           AR = 0.0
      END IF
      B2S1 = AL + AR
!
      IF ( B2S0 .NE. 0.0 ) THEN
           AL = 3.0/(ARR(KNOT+3) - ARR(KNOT)  )*B2S0
         ELSE
           AL = 0.0
      END IF
      IF ( B2S1 .NE. 0.0 ) THEN
           AR = 3.0/(ARR(KNOT+1) - ARR(KNOT+4))*B2S1
         ELSE
           AR = 0.0
      END IF
!
      BSPLE3_DR2_R4 = AL + AR
!
      RETURN
      END  FUNCTION  BSPLE3_DR2_R4  !#!#
