      FUNCTION   BSPL4_VAL ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL4_VAL computes the value of the normalized B-spline   *
! *   of the IDEG degree defined at the knots KNOT, KNOT+1, ...          *
! *   KNOT+IDEG+1 of the array ARR at the point with argument ARG.       *
! *   Recurrent formula of de Bor [1972] and Cox [1972] is used.         *
! *   Normalization is chosen in such a way that for any given sequence  *
! *   of N knots ( N > IDEG ) and any given X                            *
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   The first and the last knots of the sequence of knots,             *
! *   1, 2, 3, ... MAR have multiplicity IDEG+1, other knots being       *
! *   simple.                                                            *
! *                                                                      *
! *   The function uses recurrent expression for the B-spline, but it    *
! *   does not uses explicit recursion in source code.                   *
! *                                                                      *
! *   The B-spline is conedred to be non zero at the semi-open interval  *
! *   9KNOT, ... KNOT+1+M]. If ARG=ARG(KNOT+1+M) within 1.e-6, then      *
! *   the B-sppline is in genreal non-zero.                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*4    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
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
! * <BSPL4_VAL> ( REAL*4    ) -- The value of the B-spline.              *
! *                                                                      *
! *  ### 01-MAY-2003   BSPL4_VAL   v1.1 (c)  L. Petrov  31-MAY-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      INTEGER*4  MORD
      PARAMETER  ( MORD=32 ) ! maximal order of B-spline
      REAL*4     BSPL4_VAL, ARR(MAR), ARG
      REAL*4     A(MORD), AL, AR
      REAL*4     EPS
      PARAMETER  ( EPS = 1.0E-6 )
      INTEGER*4  IND, IND_L1, IND_L2, IND_R1, IND_R2, IND_L, IND_R, J1, J2, J3
!
! --- Check whether the argument is in reasonable range
!
      IF ( ARG .LT. ARR(1)             .OR.  &
     &     ARG .GT. ARR(MAR)*(1.0+EPS) .OR.  &
     &     IDEG .LT. 0                       ) THEN
!
           BSPL4_VAL = 0.0E0
           RETURN
      END IF
!
! --- Initialization
!
      CALL NOUT_R4 ( IDEG+1, A )
!
! --- Set the values of spline of the zero-th degree
!
      DO 410 J1=1,IDEG+1
         IND_L = KNOT + J1-1
         IND_R = KNOT + J1
         IF ( IND_L .LT.   1 ) IND_L = 1
         IF ( IND_L .GT. MAR ) IND_L = MAR
         IF ( IND_R .LT.   1 ) IND_R = 1
         IF ( IND_R .GT. MAR ) IND_R = MAR
         IF ( ARG .GE. ARR(IND_L)  .AND.  ARG .LT. ARR(IND_R) ) THEN
              A(J1) = 1.0E0
         END IF
!
! ------ A special care is taken for the case when the argument is near the
! ------ end of the interval in order to avoid the situation when due to
! ------ rounding the B-spline at the point _AT_ the right end of the interval 
! ------ the value of B-spline is computed for the point _PAST_ the right end of 
! ------ the interval
!
         IF ( ARG .GE. ARR(IND_R)*(1.0-EPS)  .AND. &
     &        ARG .LE. ARR(IND_R)*(1.0+EPS)  .AND. &
     &        IND_R .EQ. MAR ) THEN
              A(J1) = 1.0E0
         END IF
 410  CONTINUE
      IF ( IDEG .EQ. 0 ) THEN
           BSPL4_VAL = A(1)
           RETURN
      END IF
!
! --- Cycle over degree. We compute consecutively B-spline of order 1, 2 ...
! --- till m
!
      DO 420 J2=1,IDEG
         DO 430 J3=1,IDEG+1-J2
            IND = KNOT+(J3-1)
!
            IND_L1 = IND
            IND_L2 = IND+J2
!
            IF ( IND_L1 .LT.   1 ) IND_L1 = 1
            IF ( IND_L2 .LT.   1 ) IND_L2 = 1
            IF ( IND_L1 .GT. MAR ) IND_L1 = MAR
            IF ( IND_L2 .GT. MAR ) IND_L2 = MAR
!
            IF ( IND_L2 .GT. IND_L1 ) THEN
                 AL = ( ARG -ARR(IND_L1) )/( ARR(IND_L2) - ARR(IND_L1) )*A(J3)
               ELSE
                 AL = 0.0D0
            END IF
!
            IND_R1 = IND+J2+1
            IND_R2 = IND+1
            IF ( IND_R1 .LT.   1 ) IND_R1 = 1
            IF ( IND_R2 .LT.   1 ) IND_R2 = 1
            IF ( IND_R1 .GT. MAR ) IND_R1 = MAR
            IF ( IND_R2 .GT. MAR ) IND_R2 = MAR
!
            IF ( IND_R1 .GT. IND_R2 ) THEN
                 AR =( ARG - ARR(IND_R1) )/( ARR(IND_R2) - ARR(IND_R1) )*A(J3+1)
               ELSE
                 AR = 0.0D0
            END IF
!
            A(J3) = AL + AR
 430     CONTINUE
 420  CONTINUE
      BSPL4_VAL = A(1)
!
      RETURN
      END  !#!  BSPL4_VAL  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL4_DER ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL4_DER computes first derivative of the normalized    *
! *   B-spline of the IDEG degree defined at the sequence of knots       *
! *   KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR at the point with   *
! *   argument ARG. Normalization is chosen in such a way that for any   *
! *   given sequence of N knots ( N > IDEG ) and any given X             *
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
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
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
! * <BSPL4_DER> ( REAL*4    ) -- First derivative of the B-spline.        *
! *                                                                      *
! *  ### 01-MAY-2003   BSPL4_DER    v1.0 (c)  L. Petrov  01-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*4     BSPL4_DER, ARR(MAR), ARG
      INTEGER*4  IND_L1, IND_L2, IND_R1, IND_R2
      REAL*4     AL, AR
      REAL*4     BSPL4_VAL
!
      IND_L1 = KNOT+IDEG
      IND_L2 = KNOT
!
      IF ( IND_L1 .LT.   1 ) IND_L1 = 1
      IF ( IND_L2 .LT.   1 ) IND_L2 = 1
      IF ( IND_L1 .GT. MAR ) IND_L1 = MAR
      IF ( IND_L2 .GT. MAR ) IND_L2 = MAR
!
      IND_R1 = KNOT+IDEG+1
      IND_R2 = KNOT+1
!
      IF ( IND_R1 .LT.   1 ) IND_R1 = 1
      IF ( IND_R2 .LT.   1 ) IND_R2 = 1
      IF ( IND_R1 .GT. MAR ) IND_R1 = MAR
      IF ( IND_R2 .GT. MAR ) IND_R2 = MAR
!
      IF ( IND_L1 .GT. IND_L2 ) THEN
           AL = IDEG/(ARR(IND_L1) - ARR(IND_L2))* &
     &          BSPL4_VAL ( MAR, ARR, IDEG-1, KNOT, ARG )
         ELSE
           AL = 0.D0
      END IF
!
      IF ( IND_R1 .GT. IND_R2 ) THEN
           AR = IDEG/(ARR(IND_R2) - ARR(IND_R1))* &
     &          BSPL4_VAL ( MAR, ARR, IDEG-1, KNOT+1, ARG )
         ELSE
           AR = 0.D0
      END IF
!
      BSPL4_DER = AL + AR
!
      RETURN
      END  !#!  BSPL4_DER  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL4_DR2 ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL4_DR2 computes second derivative of the normalized   *
! *   B-spline of the IDEG degree defined at the sequence of knots       *
! *   KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR at the point with   *
! *   argument ARG. Normalization is chosen in such a way that for any   *
! *   given sequence of N knots ( N > IDEG ) and any given X             *
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
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
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
! * <BSPL4_DR2> ( REAL*4    ) -- Second derivative of the B-spline.      *
! *                                                                      *
! *  ### 18-JAN-2004   BSPL4_DR2   v1.0 (c)  L. Petrov  18-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*4     BSPL4_DR2, ARR(MAR), ARG
      INTEGER*4  IND_L1, IND_L2, IND_R1, IND_R2
      REAL*4     AL, AR
      REAL*4     BSPL4_DER
!
      IND_L1 = KNOT+IDEG
      IND_L2 = KNOT
!
      IF ( IND_L1 .LT.   1 ) IND_L1 = 1
      IF ( IND_L2 .LT.   1 ) IND_L2 = 1
      IF ( IND_L1 .GT. MAR ) IND_L1 = MAR
      IF ( IND_L2 .GT. MAR ) IND_L2 = MAR
!
      IND_R1 = KNOT+IDEG+1
      IND_R2 = KNOT+1
!
      IF ( IND_R1 .LT.   1 ) IND_R1 = 1
      IF ( IND_R2 .LT.   1 ) IND_R2 = 1
      IF ( IND_R1 .GT. MAR ) IND_R1 = MAR
      IF ( IND_R2 .GT. MAR ) IND_R2 = MAR
!
      IF ( IND_L1 .GT. IND_L2 ) THEN
           AL = IDEG/(ARR(IND_L1) - ARR(IND_L2))* &
     &          BSPL4_DER ( MAR, ARR, IDEG-1, KNOT, ARG )
         ELSE
           AL = 0.0
      END IF
!
      IF ( IND_R1 .GT. IND_R2 ) THEN
           AR = IDEG/(ARR(IND_R2) - ARR(IND_R1))* &
     &          BSPL4_DER ( MAR, ARR, IDEG-1, KNOT+1, ARG )
         ELSE
           AR = 0.0
      END IF
!
      BSPL4_DR2 = AL + AR
!
      RETURN
      END  !#!  BSPL4_DR2  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL4_INT ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL4_INT  computes the integral of the normalized       *
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
! *   ARR ( REAL*4    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
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
! * <BSPL4_INT> ( REAL*4    ) -- Integral of the B-spline in the limits  *
! *                             from minus infinity to ARG.              *
! *                                                                      *
! *  ### 01-MAY-2003   BSPL4_INT    v1.1 (c)  L. Petrov  05-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*4     BSPL4_INT, ARR(MAR), ARG
      REAL*4     BSPL_VAL
      INTEGER*4  IND_L, IND_R, J1
!
      BSPL4_INT = 0.0
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
           BSPL4_INT = 0.0D0
         ELSE IF ( ARG .GE. ARR(IND_R) ) THEN
!
! -------- Use the a unity partitioning property
!
           BSPL4_INT = 1.0D0
         ELSE
           DO 410 J1=KNOT,KNOT+IDEG+1
              BSPL4_INT = BSPL4_INT + BSPL_VAL ( MAR, ARR, IDEG+1, J1, ARG )
 410       CONTINUE
      END IF
!
      BSPL4_INT = BSPL4_INT * ( ARR(IND_R) - ARR(IND_L) )/( IDEG + 1 )
!
      RETURN
      END  !#!  BSPL4_INT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL4_JNT ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL4_JNT  computes the integral of the normalized        *
! *   B-spline of the IDEG degree defined at the sequence of knots       *
! *   KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR in the limits       *
! *   from ARG + to +ininity. Normalization is chosen in such            *
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
! *   ARR ( REAL*4    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
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
! * <BSPL4_JNT> ( REAL*4    ) -- Integral of the B-spline in the limits   *
! *                             from minus infinity to ARG.              *
! *                                                                      *
! *  ### 18-OCT-2013   BSPL4_JNT    v1.1 (c)  L. Petrov  18-OCT-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*4     BSPL4_JNT, ARR(MAR), ARG, r1, r2
      REAL*4     BSPL4_VAL
      INTEGER*4  IND_L, IND_R, J1
!
      BSPL4_JNT = 0.0
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
           BSPL4_JNT = 1.0
         ELSE IF ( ARG .GE. ARR(IND_R) ) THEN
           BSPL4_JNT = 0.0
         ELSE
           BSPL4_JNT = 1.0
           DO 410 J1=KNOT,KNOT+IDEG+1
              BSPL4_JNT = BSPL4_JNT - BSPL4_VAL ( MAR, ARR, IDEG+1, J1, ARG )
 410       CONTINUE
      END IF
!
      BSPL4_JNT = BSPL4_JNT * ( ARR(IND_R) - ARR(IND_L) )/( IDEG + 1 )
!
      RETURN
      END  !#!  BSPL4_JNT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL4_KNT ( MAR, ARR, IDEG, KNOT, INOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL4_KNT  computes the integral of the normalized       *
! *   B-spline of the IDEG degree defined at the sequence of knots       *
! *   KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR in the limits       *
! *   [INOT-1, INOT]. Normalization of B-splines is chosen in such       *
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
! * <BSPL4_KNT> ( REAL*8    ) -- Integral of the B-spline in the limits  *
! *                             from minus infinity to ARG.              *
! *                                                                      *
! *  ### 16-JAN-2014   BSPL4_KNT   v1.2 (c)  L. Petrov  23-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT, INOT
      REAL*4     BSPL4_KNT, ARR(MAR), ARG
      REAL*4     EPS
      PARAMETER  ( EPS = 1.D-6 )
      INTEGER*4  IND_L, IND_R, J1, J2
      REAL*4,    EXTERNAL :: BSPL4_VAL
!
      BSPL4_KNT = 0.0
      IF ( IDEG < 0 ) RETURN
      IF ( INOT < 2 ) RETURN
!
      IND_L = KNOT
      IF ( IND_L > MAR ) IND_L = MAR
      IF ( IND_L <   1 ) IND_L = 1
!
      IND_R = KNOT + IDEG + 1
      IF ( IND_R > MAR ) IND_R = MAR
      IF ( IND_R <   1 ) IND_R = 1
      IF ( INOT < MAR ) THEN
           DO 410 J1=KNOT,KNOT+IDEG
              BSPL4_KNT = BSPL4_KNT + BSPL4_VAL ( MAR, ARR, IDEG+1, J1, ARR(INOT)   ) - &
     &                                BSPL4_VAL ( MAR, ARR, IDEG+1, J1, ARR(INOT-1) ) 
 410       CONTINUE
       ELSE 
           DO 420 J2=KNOT,KNOT+IDEG
              BSPL4_KNT = BSPL4_KNT + BSPL4_VAL ( MAR+1, ARR, IDEG+1, J2, ARR(INOT)   ) - &
     &                                BSPL4_VAL ( MAR+1, ARR, IDEG+1, J2, ARR(INOT-1) ) 
 420       CONTINUE
      END IF
!
      IND_R = KNOT + IDEG + 1
      IF ( IND_R .GT. MAR ) IND_R = MAR
!
      BSPL4_KNT = BSPL4_KNT * ( ARR(IND_R) - ARR(IND_L) )/( IDEG + 1 )
!
      RETURN
      END  FUNCTION  BSPL4_KNT  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL4_INT1_FULL ( MAR, ARG, IDEG, KNOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL4_INT1_FILL  computes the integral of the normalized  *
! *   B-spline of the IDEG degree defined at the sequence of knots       *
! *   KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR in the limits       *
! *   from minus infinity to plus unfinity. Normalization is chosen in   *
! *   such a way that for any given sequence of N knots ( N > IDEG )     *
! *   and any given X                                                    *
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
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
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
! * <BSPL4_INT1_FULL> ( REAL*4   ) -- Integral of the B-spline in the    *
! *                                   limits from minus infinity to plus *
! *                                   infinity.                          *
! *                                                                      *
! * ### 07-MAR-2004  BSPL4_INT1_FULL v2.0 (c) L. Petrov  18-OCT-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, IDEG, KNOT
      REAL*4     ARG(MAR)
      REAL*4     BSPL4_INT1_FULL
      INTEGER*4  J1, IBEG, IEND
!
      IF ( KNOT .GE. MAR  .OR. KNOT .LE. -IDEG ) THEN
           BSPL4_INT1_FULL = 0.0
           RETURN
      END IF
!
      IBEG = KNOT
      IEND = KNOT+IDEG
      IF ( IBEG .LT.   1 ) IBEG = 1
      IF ( IEND .LT.   1 ) IEND = 1
      IF ( IBEG .GT. MAR-1 ) IBEG = MAR-1
      IF ( IEND .GT. MAR-1 ) IEND = MAR-1
      BSPL4_INT1_FULL = (ARG(IEND+1) - ARG(IBEG))/(IDEG+1)
!
      RETURN
      END  FUNCTION   BSPL4_INT1_FULL  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPL4_INT ( MAR, ARR, IDEG, COEF, X_UP )
! ************************************************************************
! *                                                                      *
! *   Function SPL4_INT computes an integral of the spline defined as an *
! *   expansion over B-splines in limits from ARR(1) till X_UP.          *
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
! *  MARR ( INTEGER*4 ) -- The total number of points in the array ARR.  *
! *   ARR ( REAL*4    ) -- Array of knots at whcih the spline is defined.*
! *                        Dimension: MAR.                               *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
! *  COEF ( REAL*4    ) -- Array of coefficents of expamsion of the      *
! *                        spline on the basis of B-splines.             *
! *                        Dimension: MARR+IDEG-1.                       *
! *  X_UP ( REAL*4    ) -- Upper limit of integration. If X_UP < ARR(1)  *
! *                        then the integral is set to zero.             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <SPL4_INT> ( REAL*4  ) -- Integral of the spline in the limits       *
! *                           [ARR(1), X_UP].                            *
! *                                                                      *
! *  ### 13-MAY-2003   SPL4_INT    v1.1 (c)  L. Petrov  18-FEB-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*4     SPL4_INT
      INTEGER*4  MAR, IDEG
      REAL*4     ARR(MAR), COEF(1-IDEG:MAR), X_UP
      REAL*4     R
      INTEGER*4  IND_L, IND_R, J1, J2, J3
      REAL*4     BSPL4_VAL
!
      SPL4_INT = 0.0E0
      IF ( IDEG .LT. 0 ) THEN
           RETURN
      END IF
!
      IF ( X_UP .LE. ARR(1) ) THEN
           CONTINUE
         ELSE IF ( X_UP .GE. ARR(MAR) ) THEN
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
              SPL4_INT = SPL4_INT + COEF(J1)* ( ARR(IND_L) - ARR(IND_R) )
 410      CONTINUE
         ELSE
!
! -------- The case when the upper limit of integration is within the
! -------- range of the spline
!
           DO 420 J2=1-IDEG,MAR-1
              R = 0.0D0
              DO 430 J3=1-IDEG,J2
                 IND_L = J3+IDEG+1
                 IND_R = J3
                 IF ( IND_L .LT.   1 ) IND_L = 1
                 IF ( IND_L .GT. MAR ) IND_L = MAR
                 IF ( IND_R .LT.   1 ) IND_R = 1
                 IF ( IND_R .GT. MAR ) IND_R = MAR
                 R = R + COEF(J3)* ( ARR(IND_L) - ARR(IND_R) )
 430          CONTINUE
              SPL4_INT = SPL4_INT + R*BSPL4_VAL( MAR, ARR, IDEG+1, J2, X_UP )
 420       CONTINUE
      END IF
!
      SPL4_INT = SPL4_INT/(IDEG+1)
      RETURN
      END  !#!  SPL4_INT  #!#
