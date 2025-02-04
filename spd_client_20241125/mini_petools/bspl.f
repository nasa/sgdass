      FUNCTION   BSPL_VAL ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_VAL computes the value of the normalized B-spline   *
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
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*8    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
! *  KNOT ( REAL*8    ) -- The leading knot of the spline. The spline is *
! *                        zero at  [-inf, ARR(KNOT)) and                *
! *                                 [ARR(KNOT+IDEG+1), +inf].            *
! *                        Since the first (and the last) knot of the    *
! *                        sequence are multiple, splines of             *
! *                        1-IDEG, ..., -2, -1, 0  are defined.          *
! *  ARG  ( REAL*8    ) -- The argument for which the B-spline is to be  *
! *                        computed.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPL_VAL> ( REAL*8    ) -- The value of the B-spline.               *
! *                                                                      *
! *  ### 01-MAY-2003   BSPL_VAL    v1.0 (c)  L. Petrov  01-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      INTEGER*4  MORD
      PARAMETER  ( MORD=32 ) ! maximal order of B-spline
      REAL*8     BSPL_VAL, ARR(MAR), ARG
      REAL*8     A(MORD), AL, AR
      INTEGER*4  IND, IND_L1, IND_L2, IND_R1, IND_R2, IND_L, IND_R, J1, J2, J3
!
! --- Check whether the argument is in reasonable range
!
      IF ( ARG .LT. ARR(1)  .OR.  ARG .GT. ARR(MAR)  .OR.  IDEG .LT. 0 ) THEN
           BSPL_VAL = 0.0D0
           RETURN
      END IF
!
! --- Initliatization
!
      CALL NOUT_R8 ( IDEG+1, A )
!
! --- Set the values of spine of the zero-th degree
!
      DO 410 J1=1,IDEG+1
         IND_L = KNOT + J1-1
         IND_R = KNOT + J1
         IF ( IND_L .LT.   1 ) IND_L = 1
         IF ( IND_L .GT. MAR ) IND_L = MAR
         IF ( IND_R .LT.   1 ) IND_R = 1
         IF ( IND_R .GT. MAR ) IND_R = MAR
         IF ( ARG .GE. ARR(IND_L)  .AND.  ARG .LT. ARR(IND_R) ) THEN
              A(J1) = 1.0D0
         END IF
 410  CONTINUE
      IF ( IDEG .EQ. 0 ) THEN
           BSPL_VAL = A(1)
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
      BSPL_VAL = A(1)
!
      RETURN
      END  !#!  BSPL_VAL  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_DER ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_DER computes first derivative of the normalized     *
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
! * <BSPL_DER> ( REAL*8    ) -- First derivative of the B-spline.        *
! *                                                                      *
! *  ### 01-MAY-2003   BSPL_DER    v1.0 (c)  L. Petrov  01-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*8     BSPL_DER, ARR(MAR), ARG
      INTEGER*4  IND_L1, IND_L2, IND_R1, IND_R2
      REAL*8     AL, AR
      REAL*8     BSPL_VAL
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
     &          BSPL_VAL ( MAR, ARR, IDEG-1, KNOT, ARG )
         ELSE
           AL = 0.D0
      END IF
!
      IF ( IND_R1 .GT. IND_R2 ) THEN
           AR = IDEG/(ARR(IND_R2) - ARR(IND_R1))* &
     &          BSPL_VAL ( MAR, ARR, IDEG-1, KNOT+1, ARG )
         ELSE
           AR = 0.D0
      END IF
!
      BSPL_DER = AL + AR
!
      RETURN
      END  !#!  BSPL_DER  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_DR2 ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_DR2 computes second derivative of the normalized    *
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
! * <BSPL_DR2> ( REAL*8    ) -- Second derivative of the B-spline.       *
! *                                                                      *
! *  ### 18-JAN-2004   BSPL_DR2    v1.0 (c)  L. Petrov  18-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*8     BSPL_DR2, ARR(MAR), ARG
      INTEGER*4  IND_L1, IND_L2, IND_R1, IND_R2
      REAL*8     AL, AR
      REAL*8     BSPL_DER
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
     &          BSPL_DER ( MAR, ARR, IDEG-1, KNOT, ARG )
         ELSE
           AL = 0.D0
      END IF
!
      IF ( IND_R1 .GT. IND_R2 ) THEN
           AR = IDEG/(ARR(IND_R2) - ARR(IND_R1))* &
     &          BSPL_DER ( MAR, ARR, IDEG-1, KNOT+1, ARG )
         ELSE
           AR = 0.D0
      END IF
!
      BSPL_DR2 = AL + AR
!
      RETURN
      END  !#!  BSPL_DR2  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_DR3 ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_DR3 computes third derivative of the normalized    *
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
! * <BSPL_DR3> ( REAL*8    ) -- Second derivative of the B-spline.       *
! *                                                                      *
! *  ### 01-MAY-2003   BSPL_DR3    v1.0 (c)  L. Petrov  18-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*8     BSPL_DR3, ARR(MAR), ARG
      INTEGER*4  IND_L1, IND_L2, IND_R1, IND_R2
      REAL*8     AL, AR
      REAL*8     BSPL_DR2
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
     &          BSPL_DR2 ( MAR, ARR, IDEG-1, KNOT, ARG )
         ELSE
           AL = 0.D0
      END IF
!
      IF ( IND_R1 .GT. IND_R2 ) THEN
           AR = IDEG/(ARR(IND_R2) - ARR(IND_R1))* &
     &          BSPL_DR2 ( MAR, ARR, IDEG-1, KNOT+1, ARG )
         ELSE
           AR = 0.D0
      END IF
!
      BSPL_DR3 = AL + AR
!
      RETURN
      END  !#!  BSPL_DR3  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_INT ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_INT  computes the integral of the normalized        *
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
! *  ### 01-MAY-2003   BSPL_INT    v1.1 (c)  L. Petrov  05-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*8     BSPL_INT, ARR(MAR), ARG
      REAL*8     BSPL_VAL
      INTEGER*4  IND_L, IND_R, J1
!
      BSPL_INT = 0.0
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
           BSPL_INT = 0.0D0
         ELSE IF ( ARG .GE. ARR(IND_R) ) THEN
!
! -------- Use the a unity partitioning property
!
           BSPL_INT = 1.0D0
         ELSE
           DO 410 J1=KNOT,KNOT+IDEG+1
              BSPL_INT = BSPL_INT + BSPL_VAL ( MAR, ARR, IDEG+1, J1, ARG )
 410       CONTINUE
      END IF
!
      BSPL_INT = BSPL_INT * ( ARR(IND_R) - ARR(IND_L) )/( IDEG + 1 )
!
      RETURN
      END  !#!  BSPL_INT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_JNT ( MAR, ARR, IDEG, KNOT, ARG )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_JNT  computes the integral of the normalized        *
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
! * <BSPL_JNT> ( REAL*8    ) -- Integral of the B-spline in the limits   *
! *                             from minus infinity to ARG.              *
! *                                                                      *
! *  ### 18-OCT-2013   BSPL_JNT    v1.1 (c)  L. Petrov  18-OCT-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT
      REAL*8     BSPL_JNT, ARR(MAR), ARG, r1, r2
      REAL*8     BSPL_VAL
      INTEGER*4  IND_L, IND_R, J1
!
      BSPL_JNT = 0.0D0
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
           BSPL_JNT = 1.0D0
         ELSE IF ( ARG .GE. ARR(IND_R) ) THEN
           BSPL_JNT = 0.0D0
         ELSE
           BSPL_JNT = 1.0D0
           DO 410 J1=KNOT,KNOT+IDEG+1
              BSPL_JNT = BSPL_JNT - BSPL_VAL ( MAR, ARR, IDEG+1, J1, ARG )
 410       CONTINUE
      END IF
!
      BSPL_JNT = BSPL_JNT * ( ARR(IND_R) - ARR(IND_L) )/( IDEG + 1 )
!
      RETURN
      END  !#!  BSPL_JNT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_KNT ( MAR, ARR, IDEG, KNOT, INOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_KNT  computes the integral of the normalized        *
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
! * <BSPL_KNT> ( REAL*8    ) -- Integral of the B-spline in the limits   *
! *                             from minus infinity to ARG.              *
! *                                                                      *
! *  ### 16-JAN-2014   BSPL_KNT    v1.2 (c)  L. Petrov  23-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDEG, MAR, KNOT, INOT
      REAL*8     BSPL_KNT, ARR(MAR), ARG
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-13 )
      INTEGER*4  IND_L, IND_R, J1, J2
      REAL*8,    EXTERNAL :: BSPL_VAL
!
      BSPL_KNT = 0.0D0
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
              BSPL_KNT = BSPL_KNT + BSPL_VAL ( MAR, ARR, IDEG+1, J1, ARR(INOT)   ) - &
     &                              BSPL_VAL ( MAR, ARR, IDEG+1, J1, ARR(INOT-1) ) 
 410       CONTINUE
         ELSE IF ( INOT == MAR ) THEN
           DO 420 J2=KNOT,KNOT+IDEG
              BSPL_KNT = BSPL_KNT + BSPL_VAL ( MAR+1, ARR, IDEG+1, J2, ARR(INOT)*(1.0D0 - EPS)   ) - &
     &                              BSPL_VAL ( MAR+1, ARR, IDEG+1, J2, ARR(INOT-1) ) 
 420       CONTINUE
      END IF
!
      IND_R = KNOT + IDEG + 1
      IF ( IND_R .GT. MAR ) IND_R = MAR
!
      BSPL_KNT = BSPL_KNT * ( ARR(IND_R) - ARR(IND_L) )/( IDEG + 1 )
!
      RETURN
      END  FUNCTION  BSPL_KNT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BSPL_FOUR ( MAR, ARR, IDEG, KNOT, OMEG, FC, FS )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_FOUR  computes sine and cosine constituent with     *
! *   frequency OMEGA of the Fourier transform of a normalized B-spline  *
! *   of the IDEG degree defined at the sequence of knots                *
! *   KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR.                    *
! *   Normalization is chosen in such a way that for any given sequence  *
! *   of N knots ( N > IDEG ) and any given X                            *
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
! *                        Since the first (and the last) knot of the    *
! *                        sequence are multiple, splines of             *
! *                        1-IDEG, ..., -2, -1, 0  are defined.          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    FC ( REAL*8    ) -- cosine component of the constuent with        *
! *                        frequency OMEGA of the Fourier transform      *
! *                        of the B-spline.                              *
! *    FS ( REAL*8    ) -- sine component of the constuent with          *
! *                        frequency OMEGA of the Fourier transform      *
! *                        of the B-spline.                              *
! *                                                                      *
! *  ### 13-MAY-2003    BSPL_FOUR   v1.2 (c) L. Petrov  08-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, KNOT, IDEG
      REAL*8     OMEG, ARR(MAR), FC, FS
      INTEGER*4  MORD
      PARAMETER  ( MORD = 32 )
      REAL*8     FM_C(MORD), FM_S(MORD)
      REAL*8     AL_C, AL_S, AR_C, AR_S
      INTEGER*4  IND_L1, IND_L2, IND_R1, IND_R2, J1, J2, IND_FM
!
      FC = 0.0D0
      FS = 0.0D0
!
! --- We compute first Fourie integral of the B-spline of 0-th degree and then
! --- iterativelely compute Fourie interal of the B-spline pf higher degrees.
!
      DO 410 J1=0,IDEG
         DO 420 J2=KNOT,KNOT+IDEG-J1
            IND_FM = J2-KNOT+1
            IND_L1 = J2+J1
            IND_L2 = J2
            IND_R1 = J2+1
            IND_R2 = J2+J1+1
!
! --------- Taking into account the fact that the edge nodes have a multiplicity
! --------- IDEG+1. Therefore, the recurrent formule is updated. IDEG nodes
! --------- are added before the first node with indeces 1-IDEG, ..., -2, -1, 0
! --------- with the same argument as the first node and IDEG nodes are added
! --------- after the last node with indeces MAR+1, MAR+2,... MAR+IDEG with
! --------- the same argument as the last node.
!
            IF ( IND_L1 .LT. 1 ) IND_L1 = 1
            IF ( IND_L2 .LT. 1 ) IND_L2 = 1
            IF ( IND_R1 .LT. 1 ) IND_R1 = 1
            IF ( IND_R2 .LT. 1 ) IND_R2 = 1
            IF ( IND_L1 .GT. MAR ) IND_L1 = MAR
            IF ( IND_L2 .GT. MAR ) IND_L2 = MAR
            IF ( IND_R1 .GT. MAR ) IND_R1 = MAR
            IF ( IND_R2 .GT. MAR ) IND_R2 = MAR
!
            IF ( J1 .EQ. 0 ) THEN
!
! -------------- Special case of the spline of the 0-th order
!
                 AL_C = -DSIN(OMEG*ARR(IND_L1))
                 AR_C =  DSIN(OMEG*ARR(IND_R1))
                 AL_S =  DCOS(OMEG*ARR(IND_L1))
                 AR_S = -DCOS(OMEG*ARR(IND_R1))
               ELSE
                 IF ( IND_L1 .GT. IND_L2 ) THEN
!
! ------------------- Recurrent formule
!
                      AL_C = -J1*FM_S(IND_FM)/( ARR(IND_L1) - ARR(IND_L2) )
                      AL_S =  J1*FM_C(IND_FM)/( ARR(IND_L1) - ARR(IND_L2) )
                    ELSE
                      AL_C = -DSIN(OMEG*ARR(IND_L1))
                      AL_S =  DCOS(OMEG*ARR(IND_L1))
                 END IF
                 IF ( IND_R2 .GT. IND_R1 ) THEN
!
! ------------------- Recurrent formule
!
                      AR_C = -J1*FM_S(IND_FM+1)/( ARR(IND_R1) - ARR(IND_R2) )
                      AR_S =  J1*FM_C(IND_FM+1)/( ARR(IND_R1) - ARR(IND_R2) )
                    ELSE
                      AR_C =  DSIN(OMEG*ARR(IND_R1))
                      AR_S = -DCOS(OMEG*ARR(IND_R1))
                 END IF
            END IF
!
! --------- Combine left and right side of the expresion
!
            FM_C(IND_FM) = (AL_C + AR_C)/OMEG
            FM_S(IND_FM) = (AL_S + AR_S)/OMEG
 420     CONTINUE
 410  CONTINUE
      FC = FM_C(1)
      FS = FM_S(1)
!
      RETURN
      END  !#!  BSPL_FOUR  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPL_INT ( MAR, ARR, IDEG, COEF, X_UP )
! ************************************************************************
! *                                                                      *
! *   Function SPL_INT computes an integral of the spline defined as an  *
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
! *   ARR ( REAL*8    ) -- Array of knots at which the spline is defined.*
! *                        Dimension: MAR.                               *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
! *  COEF ( REAL*8    ) -- Array of coefficents of expamsion of the      *
! *                        spline on the basis of B-splines.             *
! *                        Dimension: MARR+IDEG-1.                       *
! *  X_UP ( REAL*8    ) -- Upper limit of integration. If X_UP < ARR(1)  *
! *                        then the integral is set to zero.             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <SPL_INT> ( REAL*8   ) -- Integral of the spline in the limits       *
! *                           [ARR(1), X_UP].                            *
! *                                                                      *
! *  ### 13-MAY-2003    SPL_INT    v1.1 (c)  L. Petrov  08-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     SPL_INT
      INTEGER*4  MAR, IDEG
      REAL*8     ARR(MAR), COEF(MAR+IDEG-1), X_UP
      REAL*8     R
      INTEGER*4  IND_L, IND_R, J1, J2, J3
      REAL*8     BSPL_VAL
!
      SPL_INT = 0.0D0
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
              SPL_INT = SPL_INT + COEF(J1+IDEG)* ( ARR(IND_L) - ARR(IND_R) )
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
                 R = R + COEF(J3+IDEG)* ( ARR(IND_L) - ARR(IND_R) )
 430          CONTINUE
              SPL_INT = SPL_INT + R*BSPL_VAL( MAR, ARR, IDEG+1, J2, X_UP )
 420       CONTINUE
      END IF
!
      SPL_INT = SPL_INT/(IDEG+1)
      RETURN
      END  !#!  SPL_INT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPL_FOUR ( MAR, ARR, IDEG, COEF, OMEG, VC, VS )
! ************************************************************************
! *                                                                      *
! *   Routine  SPL_FOUR computes a harmnonic with frequency OMEG of the *
! *   Fourier transform of a spline using recurrent formula.             *
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
! *  MARR ( INTEGER*4 ) -- Total number of points in the array ARR.      *
! *   ARR ( REAL*8    ) -- Array of knots used for computation of the    *
! *                        B-spline.                                     *
! *  IDEG ( INTEGER*4 ) -- The degree of the spline. The degree should   *
! *                        be no less than 0.                            *
! *  COEF ( REAL*8    ) -- Array of coeficients of the expansion of the  *
! *                        spline into the  B-spline basis.              *
! *                        Dimension: MAR+IDEG-1                         *
! *  OMEG ( REAL*8    ) -- Cyclic frequency of the harminic.             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    VC ( REAL*8    ) -- cosine component of the Fourier transform.    *
! *    VS ( REAL*8    ) -- sine   component of the Fourier transform.    *
! *                                                                      *
! *  ### 09-MAY-2003    SPL_FOUR   v1.0 (c)  L. Petrov  09-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, IDEG
      REAL*8     OMEG, ARR(MAR), COEF(MAR+IDEG-1), VC, VS
      REAL*8     FM_C(-32:1024), FM_S(-32:1024)
      REAL*8     AL_C, AL_S, AR_C, AR_S
      INTEGER*4  IND_L1, IND_L2, IND_R1, IND_R2, J1, J2
!
      VC = 0.0D0
      VS = 0.0D0
!
! --- We compute first Fourie integral of the B-spline of 0-th degree and then
! --- iterativelely compute Fourie interal of the B-spline pf higher degrees.
!
      DO 410 J1=0,IDEG
         DO 420 J2=1-J1,MAR-1
            IND_L1 = J2+J1
            IND_L2 = J2
            IND_R1 = J2+1
            IND_R2 = J2+J1+1
!
! --------- Taking into account the fact that the edge knots have a multiplicity
! --------- IDEG+1. Therefore, the recurrent formule is updated. IDEG knots
! --------- are added before the first knot with indeces 1-IDEG0, ..., -2, -1, 0
! --------- with the same argument as the first knot and IDEG knots are added
! --------- after the last knot with indeces MAR+1, MAR+2,... MAR+IDEG with
! --------- the same argument as the last knot.
!
            IF ( IND_L1 .LT. 1 ) IND_L1 = 1
            IF ( IND_L2 .LT. 1 ) IND_L2 = 1
            IF ( IND_R1 .LT. 1 ) IND_R1 = 1
            IF ( IND_R2 .LT. 1 ) IND_R2 = 1
            IF ( IND_L1 .GT. MAR ) IND_L1 = MAR
            IF ( IND_L2 .GT. MAR ) IND_L2 = MAR
            IF ( IND_R1 .GT. MAR ) IND_R1 = MAR
            IF ( IND_R2 .GT. MAR ) IND_R2 = MAR
!
            IF ( J1 .EQ. 0 ) THEN
!
! -------------- Special case of the spline of the 0-th degree
!
                 AL_C = -DSIN(OMEG*ARR(IND_L1))
                 AR_C =  DSIN(OMEG*ARR(IND_R1))
                 AL_S =  DCOS(OMEG*ARR(IND_L1))
                 AR_S = -DCOS(OMEG*ARR(IND_R1))
               ELSE
                 IF ( IND_L1 .GT. IND_L2 ) THEN
!
! ------------------- Recurrent formule
!
                      AL_C = -J1*FM_S(J2)/( ARR(IND_L1) - ARR(IND_L2) )
                      AL_S =  J1*FM_C(J2)/( ARR(IND_L1) - ARR(IND_L2) )
                    ELSE
                      AL_C = -DSIN(OMEG*ARR(IND_L1))
                      AL_S =  DCOS(OMEG*ARR(IND_L1))
                 END IF
                 IF ( IND_R2 .GT. IND_R1 ) THEN
!
! ------------------- Recurrent formule
!
                      AR_C = -J1*FM_S(J2+1)/( ARR(IND_R1) - ARR(IND_R2) )
                      AR_S =  J1*FM_C(J2+1)/( ARR(IND_R1) - ARR(IND_R2) )
                    ELSE
                      AR_C =  DSIN(OMEG*ARR(IND_R1))
                      AR_S = -DCOS(OMEG*ARR(IND_R1))
                 END IF
            END IF
!
! --------- Combine left and right side of the expresion
!
            FM_C(J2) = (AL_C + AR_C)/OMEG
            FM_S(J2) = (AL_S + AR_S)/OMEG
!
            IF ( J1 .EQ. IDEG ) THEN
                 VC = VC + COEF(J2+IDEG)*FM_C(J2)
                 VS = VS + COEF(J2+IDEG)*FM_S(J2)
            END IF
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  SPL_FOUR  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_MOM1_FULL ( MAR, ARG, IDEG, KNOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_MOM1_FULL  computes the first moment of a B-spline  *
! *   of degree IDEG defined at the knots KNOT, KNOT+1, ... KNOT+IDEG+1. *
! *                                                                      *
! *   BSPL_MOM1_FULL^{ideg}_{knot} =                                     *
! *                  \int_{-infty}^{+\infty} x B^{ideg}_{knot} dx        *
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
! *                        Since the first (and the last) knot of the    *
! *                        sequence are multiple, splines of             *
! *                        1-IDEG, ..., -2, -1, 0  are defined.          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPL_MOM1_FULL> ( REAL*8    ) -- first moment of the B-spline.      *
! *                                                                      *
! * ### 07-MAR-2004  BSPL_MOM1_FULL v1.0 (c)  L. Petrov 08-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, IDEG, KNOT
      REAL*8     ARG(MAR)
      REAL*8     BSPL_MOM1_FULL
      REAL*8     SUM
      INTEGER*4  J1, IBEG, IEND
!
      IF ( KNOT .GE. MAR  .OR. KNOT .LE. -IDEG ) THEN
           BSPL_MOM1_FULL = 0.0D0
           RETURN
      END IF
!
      SUM = 0.0D0
      DO 410 J1=KNOT,MAR-1
         IBEG = J1+IDEG+2
         IEND = J1
         IF ( IBEG .LT.   1 ) IBEG = 1
         IF ( IEND .LT.   1 ) IEND = 1
         IF ( IBEG .GT. MAR ) IBEG = MAR
         IF ( IEND .GT. MAR ) IEND = MAR
         SUM = SUM + (ARG(IBEG)-ARG(IEND))
 410  CONTINUE
!
      IBEG = KNOT+IDEG+1
      IEND = KNOT
      IF ( IBEG .LT.  1 ) IBEG = 1
      IF ( IEND .LT.  1 ) IEND = 1
      IF ( IBEG .GT. MAR ) IBEG = MAR
      IF ( IEND .GT. MAR ) IEND = MAR
      BSPL_MOM1_FULL = (ARG(IBEG)-ARG(IEND))*(ARG(MAR) - SUM/(IDEG+2))/(IDEG+1)
!
      RETURN
      END  FUNCTION  BSPL_MOM1_FULL
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_MOM2_FULL ( MAR, ARG, IDEG, KNOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_MOM2_FULL  computes the second moment of a B-spline *
! *   of degree IDEG defined at the knots KNOT, KNOT+1, ... KNOT+IDEG+1. *
! *                                                                      *
! *   BSPL_MOM2_FULL^{ideg}_{knot} =                                     *
! *                  \int_{-infty}^{+\infty} x^2 B^{ideg}_{knot} dx      *
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
! *                        Since the first (and the last) knot of the    *
! *                        sequence are multiple, splines of             *
! *                        1-IDEG, ..., -2, -1, 0  are defined.          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <BSPL_MOM2_FULL> ( REAL*8    ) -- first moment of the B-spline.      *
! *                                                                      *
! * ### 07-MAR-2004  BSPL_MOM2_FULL  v1.0 (c) L. Petrov 08-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, IDEG, KNOT
      REAL*8     ARG(MAR)
      REAL*8     BSPL_MOM2_FULL
      REAL*8     SUM1, SUM2
      INTEGER*4  J1, J2, IBEG, IEND
!
      IF ( KNOT .GE. MAR  .OR. KNOT .LE. -IDEG ) THEN
           BSPL_MOM2_FULL = 0.0D0
           RETURN
      END IF
!
      SUM1 = 0.0D0
      DO 410 J1=KNOT,MAR-1
         SUM2 = 0.0D0
         DO 420 J2=J1,MAR-1
            IBEG = J2+IDEG+3
            IEND = J2
            IF ( IBEG .LT.   1 ) IBEG = 1
            IF ( IEND .LT.   1 ) IEND = 1
            IF ( IBEG .GT. MAR ) IBEG = MAR
            IF ( IEND .GT. MAR ) IEND = MAR
            SUM2 = SUM2 + (ARG(IBEG)-ARG(IEND))
 420     CONTINUE
         IBEG = J1+IDEG+2
         IEND = J1
         IF ( IBEG .LT.   1 ) IBEG = 1
         IF ( IEND .LT.   1 ) IEND = 1
         IF ( IBEG .GT. MAR ) IBEG = MAR
         IF ( IEND .GT. MAR ) IEND = MAR
         SUM1 = SUM1 + (ARG(IBEG)-ARG(IEND))*(ARG(MAR) - SUM2/(IDEG+3))
 410  CONTINUE
!
      IBEG = KNOT+IDEG+1
      IEND = KNOT
      IF ( IBEG .LT.  1 ) IBEG = 1
      IF ( IEND .LT.  1 ) IEND = 1
      IF ( IBEG .GT. MAR ) IBEG = MAR
      IF ( IEND .GT. MAR ) IEND = MAR
      BSPL_MOM2_FULL = (ARG(IBEG)-ARG(IEND))* &
     &                 (ARG(MAR)**2 - 2.0D0*SUM1/(IDEG+2) )/(IDEG+1)
!
      RETURN
      END  FUNCTION   BSPL_MOM2_FULL
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_INT1_FULL ( MAR, ARG, IDEG, KNOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_INT1_FILL  computes the integral of the normalized  *
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
! * <BSPL_INT1_FULL> ( REAL*8    ) -- Integral of the B-spline in the    *
! *                                   limits from minus infinity to plus *
! *                                   infinity.                          *
! *                                                                      *
! * ### 07-MAR-2004  BSPL_INT1_FULL  v2.0 (c) L. Petrov  18-OCT-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, IDEG, KNOT
      REAL*8     ARG(MAR)
      REAL*8     BSPL_INT1_FULL
      INTEGER*4  J1, IBEG, IEND
!
      IF ( KNOT .GE. MAR  .OR. KNOT .LE. -IDEG ) THEN
           BSPL_INT1_FULL = 0.0D0
           RETURN
      END IF
!
      IBEG = KNOT
      IEND = KNOT+IDEG
      IF ( IBEG .LT.   1 ) IBEG = 1
      IF ( IEND .LT.   1 ) IEND = 1
      IF ( IBEG .GT. MAR-1 ) IBEG = MAR-1
      IF ( IEND .GT. MAR-1 ) IEND = MAR-1
      BSPL_INT1_FULL = (ARG(IEND+1) - ARG(IBEG))/(IDEG+1)
!
      RETURN
      END  FUNCTION   BSPL_INT1_FULL
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_INT2_FULL ( MAR, ARG, IDEG, KNOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_INT2_FILL  computes the double integral of the      *
! *   normalized  B-spline of the IDEG degree defined at the sequence    *
! *   of knots KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR in the     *
! *   limits from minus infinity to plus unfinity. Normalization is      *
! *   chosen in such a way that for any given sequence of N knots        *
! *   ( N > IDEG ) and any given X                                       *
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   The first and the last knots have multiplicity IDEG.               *
! *                                                                      *
! *  BSPL_INT2_FULL^{ideg}_{knot} =                                      *
! *             \int \int_{-infty}^{+\infty} B^{ideg}_{knot} dx dx       *
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
! * <BSPL_INT2_SPL> ( REAL*8    ) -- Double integral of the B-spline     *
! *                                  in the limits from minus infinity   *
! *                                  to plus infinity.                   *
! *                                                                      *
! * ### 07-MAR-2004  BSPL_INT2_FULL v1.0 (c) L. Petrov  08-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, IDEG, KNOT
      REAL*8     ARG(MAR)
      REAL*8     BSPL_INT2_FULL
      INTEGER*4  J1, IBEG, IEND
      REAL*8     SUM
!
      IF ( KNOT .GE. MAR  .OR.  KNOT .LE. -IDEG ) THEN
           BSPL_INT2_FULL = 0.0D0
           RETURN
      END IF
!
      IBEG = KNOT+IDEG+1
      IEND = KNOT
      IF ( IBEG .LT.   1 ) IBEG = 1
      IF ( IEND .LT.   1 ) IEND = 1
      IF ( IBEG .GT. MAR ) IBEG = MAR
      IF ( IEND .GT. MAR ) IEND = MAR
      BSPL_INT2_FULL = (ARG(IBEG)-ARG(IEND))/((IDEG+1)*(IDEG+2))
      SUM = 0.0D0
      DO 410 J1=KNOT,MAR-1
         IBEG = J1+IDEG+2
         IEND = J1
         IF ( IBEG .LT.   1 ) IBEG = 1
         IF ( IEND .LT.   1 ) IEND = 1
         IF ( IBEG .GT. MAR ) IBEG = MAR
         IF ( IEND .GT. MAR ) IEND = MAR
         SUM = SUM + (ARG(IBEG)-ARG(IEND))
 410  CONTINUE
      BSPL_INT2_FULL = BSPL_INT2_FULL*SUM
!
      RETURN
      END  FUNCTION   BSPL_INT2_FULL
!
! ------------------------------------------------------------------------
!
      FUNCTION   BSPL_INT3_FULL ( MAR, ARG, IDEG, KNOT )
! ************************************************************************
! *                                                                      *
! *   Function  BSPL_INT3_FILL  computes the triple integral of the      *
! *   normalized  B-spline of the IDEG degree defined at the sequence    *
! *   of knots KNOT, KNOT+1, ... KNOT+IDEG+1 of the array ARR in the     *
! *   limits from minus infinity to plus unfinity. Normalization is      *
! *   chosen in such a way that for any given sequence of N knots        *
! *   ( N > IDEG ) and any given X                                       *
! *                                                                      *
! *     Sum (over i from 1 till N) { Bspl_i(X) } = 1                     *
! *                                                                      *
! *   The first and the last knots have multiplicity IDEG.               *
! *                                                                      *
! *  BSPL_INT3_FULL^{ideg}_{knot} =                                      *
! *      \int \int \int_{-infty}^{+\infty} B^{ideg}_{knot} dx dx dx      *
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
! * <BSPL_INT3_SPL> ( REAL*8    ) -- Triple integral of the B-spline     *
! *                                  in the limits from minus infinity   *
! *                                  to plus infinity.                   *
! *                                                                      *
! * ### 07-MAR-2004  BSPL_INT3_FULL  v1.0 (c) L. Petrov  08-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAR, IDEG, KNOT
      REAL*8     ARG(MAR)
      REAL*8     BSPL_INT3_FULL
      INTEGER*4  J1, J2, IBEG, IEND
      REAL*8     SUM1, SUM2
!
      IF ( KNOT .GE. MAR  .OR.  KNOT .LE. -IDEG ) THEN
           BSPL_INT3_FULL = 0.0D0
           RETURN
      END IF
!
      IBEG = KNOT+IDEG+1
      IEND = KNOT
      IF ( IBEG .LT.   1 ) IBEG = 1
      IF ( IEND .LT.   1 ) IEND = 1
      IF ( IBEG .GT. MAR ) IBEG = MAR
      IF ( IEND .GT. MAR ) IEND = MAR
      BSPL_INT3_FULL = (ARG(IBEG)-ARG(IEND))/((IDEG+1)*(IDEG+2)*(IDEG+3))
      SUM1 = 0.0D0
      DO 410 J1=KNOT,MAR-1
         SUM2 = 0.0D0
         DO 420 J2=J1,MAR-1
            IBEG = J2+IDEG+3
            IEND = J2
            IF ( IBEG .LT.   1 ) IBEG = 1
            IF ( IEND .LT.   1 ) IEND = 1
            IF ( IBEG .GT. MAR ) IBEG = MAR
            IF ( IEND .GT. MAR ) IEND = MAR
            SUM2 = SUM2 + (ARG(IBEG)-ARG(IEND))
 420     CONTINUE
         IBEG = J1+IDEG+2
         IEND = J1
         IF ( IBEG .LT.   1 ) IBEG = 1
         IF ( IEND .LT.   1 ) IEND = 1
         IF ( IBEG .GT. MAR ) IBEG = MAR
         IF ( IEND .GT. MAR ) IEND = MAR
         SUM1 = SUM1 + (ARG(IBEG)-ARG(IEND))*SUM2
 410  CONTINUE
      BSPL_INT3_FULL = BSPL_INT3_FULL*SUM1
!
      RETURN
      END  FUNCTION   BSPL_INT3_FULL
