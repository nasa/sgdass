        SUBROUTINE MAKE_SPLINE ( IPAR, N, X, Y, D1, DN, COEF, WORK, &
     &             IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  MAKE_SPLINE  computes an array of the coefficients      *
! *   of the interpolating cubic spline. Depending on the value of IPAR  *
! *   argument the following boundary conditions are imposed:            *
! *     IPAR=1 -- condition of equality to zero the second derivatives   *
! *               at the ends of the interpolating range (so-called      *
! *               natural spline or the spline with free ends).          *
! *     IPAR=2 -- condition that the first derivatives at the ends of    *
! *               the range are equal to the specified values D1 and DN. *
! *     IPAR=3 -- condition that the first derivatives at the ends of    *
! *               the range are equal to the first differences:          *
! *               (Y(2) - Y(1))/(X(2) - X(1)) and                        *
! *               (Y(N) - Y(N-1))/(X(N) - X(N-1))                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     IPAR ( INTEGER*4 ) -- Parameter of the choice of the boundary    *
! *                           conditions:                                *
! *            IPAR=1 -- natural spline.                                 *
! *            IPAR=2 -- spline with the specified first derivatives     *
! *                      at the ends.                                    *
! *            IPAR=3 -- spline with the first derifvaties at the end    *
! *                      equal to first differences.                     *
! *        N ( INTEGER*4 ) -- Number of points used for computation of   *
! *                           spline coefficients.                       *
! *        X ( REAL*8    ) -- Array of arguments of the function under   *
! *                           consideration.                             *
! *        Y ( REAL*8    ) -- Array of values of the function under      *
! *                           consideration.                             *
! *       D1 ( REAL*8    ) -- First derivative of the function in point  *
! *                           X(1) ( Ignored if IPAR=2 or IPAR=3 ).      *
! *       DN ( REAL*8    ) -- First derivative of the function in point  *
! *                           X(N) ( Ignored if IPAR=1 or IPAR=3 ).      *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *     COEF ( REAL*8    ) -- Array of spline coefficients.              *
! *                           ( The coefficient at the quadratic term).  *
! *                           Dimension: N.                              *
! *     WORK ( REAL*8    ) -- Work array. Dimension: N.                  *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
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
! *  ### 05-JUL-1995   MAKE_SPLINE  v2.0 (c) L. Petrov 24-DEC-1998  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  IPAR, N, IUER
        REAL*8     X(N), Y(N), COEF(N), WORK(N), D1, DN
        INTEGER*4  J1, J2, IO, I_LEN
        REAL*8     H1, H2, B, Z, EPS, DER_BEG, DER_END
        CHARACTER  STR*10, STR1*10, STR2*20
        LOGICAL*4, EXTERNAL :: IS_R8_NAN
        PARAMETER ( EPS=1.D-31 )
!!
!! .....................\\\
!!                       \\\
!        LOGICAL PRESENT
!        INTEGER*4 NUM$ARG, NA, N_ARG
!!
!        PARAMETER ( N_ARG=9 )  !  The number of formal parameters
!!
!! ----- Check whehter the number of formal arguments is equal to the number
!! ----- of actual arguments
!!
!        NA=NUM$ARG()  !  Number of actual arguiments
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL  VER$ARG ( N_ARG )
!                        ///
! ......................///  ...   end of check ...
!
        IF ( N.LT.4 ) THEN
             CALL CLRCH (    STR )
             CALL INCH  ( N, STR )
             CALL ERR_LOG ( 1, IUER, 'MAKE_SPLINE', 'Parameter '// &
     &           'N is too few ( N<4 )  N='//STR(1:I_LEN(STR)) )
             RETURN
        END IF
!
        IF ( IPAR .NE. 1  .AND.  IPAR .NE. 2  .AND. IPAR .NE. 3 ) THEN
             CALL CLRCH ( STR )
             CALL INCH  ( IPAR, STR )
             CALL ERR_LOG ( 2, IUER, 'MAKE_SPLINE', 'Wrong value of '// &
     &           'parameter IPAR:  IPAR='//STR(1:I_LEN(STR)) )
             RETURN
        END IF
!
! ----- Building and decomposition of the tri-diagonal system
!
        DO 410 J1=2,N-1
!
! -------- Some checks
!
           IF ( IS_R8_NAN ( X(J1-1) ) ) THEN
                CALL CLRCH ( STR        )
                CALL INCH  ( J1-1, STR )
                CALL ERR_LOG ( 61, IUER, 'MAKE_SPLINE', STR(1:I_LEN(STR))// &
     &              '-th argument has a wrong float format: "not-a-number"' )
                RETURN
           END IF
!
           IF ( IS_R8_NAN ( X(J1) ) ) THEN
                CALL CLRCH ( STR        )
                CALL INCH  ( J1, STR )
                CALL ERR_LOG ( 62, IUER, 'MAKE_SPLINE', STR(1:I_LEN(STR))// &
     &              '-th value has a wrong float format: "not-a-number"' )
                RETURN
           END IF
!
           IF ( IS_R8_NAN ( Y(J1-1) ) ) THEN
                CALL CLRCH ( STR        )
                CALL INCH  ( J1-1, STR )
                CALL ERR_LOG ( 63, IUER, 'MAKE_SPLINE', STR(1:I_LEN(STR))// &
     &              '-th value has a wrong float format: "not-a-number"' )
                RETURN
           END IF
!
           IF ( IS_R8_NAN ( Y(J1) ) ) THEN
                CALL CLRCH   ( STR     )
                CALL INCH    ( J1, STR )
                CALL ERR_LOG ( 64, IUER, 'MAKE_SPLINE', STR(1:I_LEN(STR))// &
     &              '-th value has a wrong float format: "not-a-number"' )
                RETURN
           END IF
!
! -------- H1 --  beloe-diagonal term term of the J1-th equation ( equal
! --------        to the above-diagonal term of the J1-1 -th equation )
! -------- H2 --  above-diagonal term of the J1-th euqatuion ( equal
! --------        to the below-diagonal term of the J1-1 -th equation )
!
           H1=X(J1)-X(J1-1)
           H2=X(J1+1)-X(J1)
!
           IF ( H1.LT.EPS ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( J1-1, STR )
                CALL CLRCH ( STR1      )
                CALL INCH  ( J1,  STR1 )
!!                CALL WRDA8 ( H1, STR2,,3 )
                WRITE ( UNIT = STR2, FMT ='(1PE11.4)', IOSTAT = IO ) H1
                CALL ERR_LOG ( 3, IUER, 'MAKE_SPLINE', 'Difference '// &
     &              'between ARGUMENTS X('// &
     &               STR1(1:I_LEN(STR1))//') and X('// &
     &               STR(1:I_LEN(STR))//') is equal '// &
     &               STR2(1:I_LEN(STR2))//', -- it is less than 1.D-31' )
                RETURN
           END IF
!
           IF ( H2.LT.EPS ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( J1,   STR  )
                CALL CLRCH ( STR1       )
                CALL INCH  ( J1+1, STR1 )
!!                CALL WRDA8 ( H2, STR2,,3 )
                WRITE ( UNIT = STR2, FMT ='(1PE11.4)', IOSTAT = IO ) H2
                CALL ERR_LOG ( 4, IUER, 'MAKE_SPLINE', 'Difference '// &
     &              'between ARGUMENTS X('// &
     &               STR1(1:I_LEN(STR1))//') and X('// &
     &               STR(1:I_LEN(STR))//') is equal '// &
     &               STR2(1:I_LEN(STR2))//', -- it is less than 1.D-31' )
                RETURN
           END IF
!
! -------- Computation of B  --  diagonal  term of the J1-th equation,
! --------                Z  --  right term of the J1-th equation
!
           IF ( J1.EQ.2 ) THEN
!
! ------------- Boundary condition at the left end of the interpolation range
!
                IF ( IPAR .EQ. 1 ) THEN
                     B = 2.0D0*(H1+H2)
                     Z = 3.0D0 * ( ( Y(J1+1) - Y(J1)  )/H2 - &
     &                             ( Y(J1)   - Y(J1-1))/H1 )
                  ELSE IF ( IPAR .EQ. 2 ) THEN
                     B = 1.5D0*H1 + 2.D0*H2
                     Z = 3.0D0 * ( Y(J1+1) - Y(J1)  )/H2 - &
     &                   4.5D0 * ( Y(J1)   - Y(J1-1))/H1 + 1.5D0*D1
                  ELSE IF ( IPAR .EQ. 3 ) THEN
                     DER_BEG = (Y(2)-Y(1))/(X(2)-X(1))
                     B = 1.5D0*H1 + 2.D0*H2
                     Z = 3.0D0 * ( Y(J1+1) - Y(J1)  )/H2 - &
     &                   4.5D0 * ( Y(J1)   - Y(J1-1))/H1 + 1.5D0*DER_BEG
                END IF
             ELSE IF ( J1.EQ.N-1 ) THEN
!
! ------------- Boundary condition at the right end of the interploation range
!
                IF ( IPAR .EQ. 1 ) THEN
                     B = 2.D0*(H1+H2)
                     Z = 3.D0 * ( ( Y(J1+1) - Y(J1)  )/H2 - &
     &                            ( Y(J1)   - Y(J1-1))/H1 )
                  ELSE IF ( IPAR .EQ. 2 ) THEN
                     B = 2.0D0*H1 + 1.5D0*H2
                     Z =  4.5D0 * ( Y(J1+1) - Y(J1)   )/H2 - &
     &                    3.0D0 * ( Y(J1)   - Y(J1-1) )/H1 - 1.5D0*DN
                  ELSE IF ( IPAR .EQ. 3 ) THEN
                     DER_END = (Y(N)-Y(N-1))/(X(N)-X(N-1))
                     B = 2.0D0*H1 + 1.5D0*H2
                     Z =  4.5D0 * ( Y(J1+1) - Y(J1)   )/H2 - &
     &                    3.0D0 * ( Y(J1)   - Y(J1-1) )/H1 - 1.5D0*DER_END
                END IF
             ELSE
!
! ------------- This point is inside of the interpolation range
!
                B = 2.D0*(H1+H2)
                Z = 3.D0 * ( ( Y(J1+1) - Y(J1)  )/H2 - &
     &                       ( Y(J1)   - Y(J1-1))/H1 )
           END IF
!
! -------- Decomposition of the tri-diagonal system.
! -------- COEF(J1) -- diagonal term of the matrix after decomposition.
! -------- WORK(J1) -- updated right part of the decomposed system.
!
           IF ( J1 .EQ. 2 ) THEN
                COEF(J1) = B
                WORK(J1) = Z
             ELSE
                COEF(J1) = B - H1**2/COEF(J1-1)
                WORK(J1) = Z - H1*WORK(J1-1)/COEF(J1-1)
           END IF
  410   CONTINUE
!
! ----- Backward run (progonka)
!
        COEF(N-1)=WORK(N-1)/COEF(N-1)
        DO 420 J2=N-2,2,-1
           H2=X(J2+1)-X(J2) !  Restoration of above-diagonal term
           COEF(J2)=( WORK(J2) - H2*COEF(J2+1) ) / COEF(J2)
  420   CONTINUE
!
! ----- Set value COEF for the 1-st and the N-th point
!
        IF ( IPAR.EQ.1 ) THEN
             COEF(1)=0.0D0
             COEF(N)=0.0D0
          ELSE IF ( IPAR.EQ.2 ) THEN
             H1=X(2)-X(1)
             COEF(1)= -0.5D0*COEF(2) + 1.5D0*( Y(2) - Y(1) )/H1**2 - &
     &                 1.5D0*D1/H1
             H1=X(N)-X(N-1)
             COEF(N)= -0.5D0*COEF(N-1) - 1.5D0*( Y(N) - Y(N-1) )/H1**2 + &
     &                 1.5D0*DN/H1
          ELSE IF ( IPAR.EQ.3 ) THEN
             H1=X(2)-X(1)
             COEF(1)= -0.5D0*COEF(2) + 1.5D0*( Y(2) - Y(1) )/H1**2 - &
     &                 1.5D0*DER_BEG/H1
             H1=X(N)-X(N-1)
             COEF(N)= -0.5D0*COEF(N-1) - 1.5D0*( Y(N) - Y(N-1) )/H1**2 + &
     &                 1.5D0*DER_END/H1
        END IF
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  MAKE_SPLINE  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  FSPL8 ( XC, N, X, Y, IXC, COEF )
! ************************************************************************
! *                                                                      *
! *     Function  FSPL8  computes the interploated value of the function *
! *   at the point XC using the coefficients of the cubic spline COEF    *
! *   computed by the program MAKE_SPLINE before the call of FSPL8.      *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of FSPL8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the fucntino Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -1 means that XC < X(1).                *
! *                        IXC = -2 means that XC > X(N).                *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <FSPL8> ( REAL*8  ) -- Value of the cubic spline interpolating the   *
! *                        the function Y(X) in the point XC.            *
! *                                                                      *
! *  Comment: IF XC is out of range [ X(1), X(N) ], then extrapolation   *
! *  is applied and the precision of the result is not guaranteed.       *
! *                                                                      *
! *  ###  05-JUL-1995     FSPL8    v1.1  (c) L. Petrov  10-APR-2006 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IXC
        REAL*8    FSPL8, XC, X(N), Y(N), COEF(N)
        REAL*8    H, DH, B, C, D
!
! ----- Check of the number of actual arguments
!
!!        CALL  VER$ARG ( 6 )
!
        IF ( IXC .EQ. -1 ) THEN
!
! ---------- Extrapolation beyond the left end of the interval
!
             DH = X(2)-X(1)
             D  = ( COEF(2) - COEF(1) ) / 3.D0
             C  = COEF(1)
             B  = ( Y(2) - Y(1) ) / DH  - D*DH**2 -C*DH
             H  = XC-X(1)
             FSPL8 = Y(1) + B*H
          ELSE IF ( IXC.EQ.N .OR. IXC.EQ. -2 ) THEN
!
! ---------- Extrapolation beyond the right end edge of the interval
!
             DH = X(N)-X(N-1)
             D  = ( COEF(N) - COEF(N-1) ) / 3.D0
             C  = COEF(N-1)
             B  = ( Y(N) - Y(N-1) ) / DH  - D*DH**2 -C*DH
             H  = XC - X(N)
             FSPL8 = Y(N) + ( B + 2.D0*C*DH + 3.D0*D*DH**2 ) *H
          ELSE
!
! ---------- Point XC turned out inside the interpolation range.
!
             DH = X(IXC+1)-X(IXC)
             D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
             C  = COEF(IXC)
             B  = ( Y(IXC+1) - Y(IXC) ) / DH  - D*DH**2 - C*DH
             H  = XC-X(IXC)
             FSPL8 = Y(IXC) + B*H + C*H**2 + D*H**3
        END IF
!
        RETURN
        END  !#!  FSPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  DSPL8 ( XC, N, X, Y, IXC, COEF )
! ************************************************************************
! *                                                                      *
! *     Function  DSPL8  computes the interploated value of the first    *
! *   derivative of the function Y(X) at the point XC using the          *
! *   coefficients of the cubic spline COEF computed by the program      *
! *   MAKE_SPLINE before the call of DSPL8.                              *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of DSPL8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the fucntino Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -1 means that XC < X(1).                *
! *                        IXC = -2 means that XC > X(N).                *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <DSPL8> ( REAL*8  ) -- Value of the first derivative of the cubic    *
! *                        spline interpolating the the function Y(X)    *
! *                        in the point XC.                              *
! *                                                                      *
! *  Comment: IF XC is out of range [ X(1), X(N) ], then extrapolation   *
! *  is applied and the precision of the result is not guaranteed.       *
! *                                                                      *
! *  ###  06-JUL-1995    DSPL8    v1.1  (c) L. Petrov  10-APR-2006  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IXC
        REAL*8    DSPL8, XC, X(N), Y(N), COEF(N)
        REAL*8    H, DH, B, C, D
!
! ----- Check of the number of actual arguments
!
!!        CALL  VER$ARG ( 6 )
!
        IF ( IXC .EQ. -1 ) THEN
!
! ---------- Extrapolation beyond the left end of the interval
!
             DH = X(2)-X(1)
             D  = ( COEF(2) - COEF(1) ) / 3.D0
             C  = COEF(1)
             B  = ( Y(2) - Y(1) ) / DH  - D*DH**2 -C*DH
             DSPL8 = B
          ELSE IF ( IXC .EQ. N  .OR.  IXC .EQ. -2 ) THEN
!
! ---------- Extrapolation beyond the right end edge of the interval
!
!
             DH = X(N)-X(N-1)
             D  = ( COEF(N) - COEF(N-1) ) / 3.D0
             C  = COEF(N-1)
             B  = ( Y(N) - Y(N-1) ) / DH  - D*DH**2 -C*DH
             DSPL8 = B + 2.D0*C*DH + 3.D0*D*DH**2
          ELSE
!
! ---------- Point XC turned out inside the interploation range.
!
             DH = X(IXC+1)-X(IXC)
             D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
             C  = COEF(IXC)
             B  = ( Y(IXC+1) - Y(IXC) ) / DH  - D*DH**2 - C*DH
             H  = XC-X(IXC)
             DSPL8 = B + 2.0D0*C*H + 3.D0*D*H**2
        END IF
!
        RETURN
        END  !#!  DSPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  D2SPL8 ( XC, N, X, Y, IXC, COEF )
! ************************************************************************
! *                                                                      *
! *     Function  D2SPL8  computes the interploated value of the second  *
! *   derivative of the function Y(X) at the point XC using the          *
! *   coefficients of the cubic spline COEF computed by the program      *
! *   MAKE_SPLINE before the call of D2SPL8.                              *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of DSPL8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the fucntino Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -1 means that XC < X(1).                *
! *                        IXC = -2 means that XC > X(N).                *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <D2SPL8> ( REAL*8 ) -- Value of the second derivative of the cubic   *
! *                        spline interpolating the the function Y(X)    *
! *                        in the point XC.                              *
! *                                                                      *
! *  Comment: IF XC is out of range [ X(1), X(N) ], then extrapolation   *
! *  is applied and the precision of the result is not guaranteed.       *
! *                                                                      *
! *  ###  06-JUL-1995    D2SPL8   v1.2  (c) L. Petrov  10-APR-2006  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IXC
        REAL*8    D2SPL8, XC, X(N), Y(N), COEF(N)
        REAL*8    H, DH, C, D
!
! ----- Check of the number of actual arguments
!
!!        CALL  VER$ARG ( 6 )
!
        IF ( IXC .EQ. -1 ) THEN
             D2SPL8 = 0.0D0
          ELSE IF ( IXC .EQ. N  .OR.  IXC .EQ. -2 ) THEN
             D2SPL8 = 0.0D0
          ELSE
!
! ---------- Point XC turned out inside the interploation range.
!
             DH = X(IXC+1)-X(IXC)
             D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
             C  = COEF(IXC)
             H  = XC-X(IXC)
             D2SPL8 = 2.0D0*C + 6.D0*D*H
        END IF
!
        RETURN
        END  !#!  D2SPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  ISPL8 ( XC, N, X, Y, IBG, IXC, COEF, IUER )
! ************************************************************************
! *                                                                      *
! *     Function  ISPL8  computes the value of the integral of the       *
! *   function Y(X) in limits [ X(IBG), XC ] using the coefficients of   *
! *   the cubic spline COEF computed by routine MAKE_SPLINE before the   *
! *   call of ISPL8. Argument IXC -- the index of pivot element should   *
! *   be computed before call of ISPL8 using function IXMN8 or IXMN8_S.  *
! *   The pivot element is the minimal element of the fucntion Y(X)      *
! *   which is equal or greater than XC.                                 *
! *                                                                      *
! *   X(IBG) ... -----X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)- *
! *    ^                             ^                                   *
! *    |                             |                                   *
! *    ------------------------------XC                                  *
! *                                                                      *
! *     The follosing expression should be satisifed:                    *
! *        X(1) =< XC =< X(N)                                            *
! *        IBG < IXC                                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IBG ( INTEGER*4 ) -- Index of the lower element that is the lower  *
! *                        limit. (usually IBG = 1 ).                    *
! *   IXC ( INTEGER*4 ) -- Index of the pivot element.                   *
! *                        IXC = -1 means that XC < X(1).                *
! *                        IXC = -2 means that XC > X(N).                *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). Dimension: N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <ISPL8> ( REAL*8  ) -- Value of the integral in limits [X(IBG), XC]  *
! *                        of the cubic spline interpolating the         *
! *                        function Y(X) in the point XC.                *
! *                                                                      *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
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
! *  ###  10-JUL-1995    ISPL8   v 1.1  (c)  L. Petrov  10-MAR-2008 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IBG, IXC, IUER
        REAL*8    ISPL8, XC, X(N), Y(N), COEF(N)
        REAL*8    H, DH, B, C, D
        INTEGER*4 J1
!C
!C .....................\\\
!C                       \\\
!        LOGICAL PRESENT
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=8 )  !  The number of formal parameters
!C
!C ----- Check whehter the number of formal arguments is equal to the number
!C ----- of actual arguments
!C
!        NA=NUM$ARG()  !  Number of actual arguiments
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL  VER$ARG ( N_ARG )
!                        ///
! ......................///  ...   end of check ...
!
!
! ----- Additional tests
!
        ISPL8=-1.11111111111111111D11
        IF ( IXC.LT.1 ) THEN
             CALL ERR_LOG ( 1, IUER, 'ISPL8', 'IXC < 1' )
             RETURN
        END IF
        IF ( IXC.GT.N ) THEN
             CALL ERR_LOG ( 2, IUER, 'ISPL8', 'IXC > N' )
             RETURN
        END IF
!
        IF ( IBG.LT.1 ) THEN
             CALL ERR_LOG ( 3, IUER, 'ISPL8', 'IBG < 1' )
             RETURN
        END IF
        IF ( IBG.GT.IXC ) THEN
             CALL ERR_LOG ( 4, IUER, 'ISPL8', 'IBG > IXC' )
             RETURN
        END IF
!
        ISPL8 = 0.D0
        IF ( IXC .GT. IBG ) THEN
!
! ---------- find the integral for complete nodes
!
             DO 410 J1=IBG,IXC-1
                DH = X(J1+1)-X(J1)
                D  = ( COEF(J1+1) - COEF(J1) ) / (3.D0*DH)
                C  = COEF(J1)
                B  = ( Y(J1+1) - Y(J1) ) / DH  - D*DH**2 - C*DH
                ISPL8 = ISPL8 + Y(J1)*DH + B*DH**2/2.D0 + &
     &                  C*DH**3/3.D0 + D*DH**4/4.D0
  410        CONTINUE
        END IF
!
! ----- Find the integral in the incomplete node
!
        IF ( IXC < N ) THEN
             DH = X(IXC+1)-X(IXC)
             D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
             C  = COEF(IXC)
             B  = ( Y(IXC+1) - Y(IXC) ) / DH  - D*DH**2 - C*DH
             H  = XC-X(IXC)
             ISPL8 = ISPL8 + Y(IXC)*H + B*H**2/2.D0 + C*H**3/3.D0 + &
     &                       D*H**4/4.D0
        END IF
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  ISPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  ISPL8_UP ( XC, N, X, Y, IXC, IEN, COEF, IUER )
! ************************************************************************
! *                                                                      *
! *     Function  ISPL8_UP computes the value of the integral of the     *
! *   function Y(X) in limits [ X(IC), XC(IEN) ] using the coefficients  *
! *   of the cubic spline COEF computed by the routine MAKE_SPLINE       *
! *   called before the call of ISPL8.                                   *
! *                                                                      *
! *   Argument IXC -- the index of a pivot element should be computed    *
! *   before call of ISPL8_UP using function IXMN8 or IXMN8_S. The pivot *
! *   element is the minimal element of the fucntion Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *   ----X(IXC-1)----X(IXC)-----X(IXC+1)------X(IXC+2)-----X(IEN)       *
! *                            ^                            ^            *
! *                            |                            |            *
! *                           XC----------------------------|            *
! *                                                                      *
! *     The following expression should be satisifed:                    *
! *                                                                      *
! *        X(1) =< XC =< X(N)                                            *
! *        IXC < IEN, IEN = or < N                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index of the pivot element.                   *
! *                        IXC = -1 means that XC < X(1).                *
! *                        IXC = -2 means that XC > X(N).                *
! *   IEN ( INTEGER*4 ) -- Index of the element that is the upper limit  *
! *                        of integration (usually IEN=N)                *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <ISPL8> ( REAL*8  ) -- Value of the integral in limits [XC, X(IEN)]  *
! *                        of the cubic spline interpolating the         *
! *                        function Y(X).                                *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
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
! *  ###  25-JUN-2013  ISPL8_UP  v 1.0  (c)  L. Petrov  25-JUN-2013 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IXC, IEN, IUER
        REAL*8    ISPL8_UP, XC, X(N), Y(N), COEF(N)
        REAL*8    H, HC, DH, B, C, D
        INTEGER*4 J1
!
! ----- Additional tests
!
        IF ( IXC .LT. 1 ) THEN
             ISPL8_UP = -1.11111111111111111D11
             CALL ERR_LOG ( 1, IUER, 'ISPL8_UP', 'IXC < 1' )
             RETURN
        END IF
        IF ( IXC .GE. N ) THEN
             ISPL8_UP = -1.11111111111111111D11
             CALL ERR_LOG ( 2, IUER, 'ISPL8_UP', 'IXC > N' )
             RETURN
        END IF
!
        IF ( IEN .LT. 1 ) THEN
             ISPL8_UP = -1.11111111111111111D11
             CALL ERR_LOG ( 3, IUER, 'ISPL8_UP', 'IEN < 1' )
             RETURN
        END IF
        IF ( IEN < IXC ) THEN
             ISPL8_UP = -1.11111111111111111D11
             CALL ERR_LOG ( 4, IUER, 'ISPL8_UP', 'IEN < IXC' )
             RETURN
        END IF
!
! ----- Find the integral in the incomplete node
!
        DH = X(IXC+1) - X(IXC)
        D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
        C  = COEF(IXC)
        B  = ( Y(IXC+1) - Y(IXC) )/ DH  - D*DH**2 - C*DH
        H   = XC - X(IXC)
        ISPL8_UP = -Y(IXC)*H - B*H**2/2.D0 - C*H**3/3.D0 - D*H**4/4.D0
!
        IF ( IXC+1 < IEN ) THEN
!
! ---------- find the integral for complete nodes
!
             DO 410 J1=IXC,IEN-1
                DH = X(J1+1) - X(J1)
                D  = ( COEF(J1+1) - COEF(J1) )/ (3.D0*DH)
                C  = COEF(J1)
                B  = ( Y(J1+1) - Y(J1) )/ DH  - D*DH**2 - C*DH
                ISPL8_UP = ISPL8_UP + Y(J1)*DH + B*DH**2/2.D0 + &
     &                     C*DH**3/3.D0 + D*DH**4/4.D0
  410        CONTINUE
        END IF
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  FUNCTION  ISPL8_UP  !#!#
!
! ------------------------------------------------------------------------
!
        FUNCTION FLIN8 ( XC, N, X, Y, IXC )
! ************************************************************************
! *                                                                      *
! *     Function  FLIN8  computes at the point XC the value of           *
! *   function Y(X), defined as a table of N paris argument/value, using *
! *   linear interpolation method.                                       *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of FLIN8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the argument X which is equal    *
! *   or greater than XC.                                                *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points used for computation of      *
! *                        spline coefficients.                          *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -1 means that XC < X(1).                *
! *                        IXC = -2 means that XC > X(N).                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *  <FLIN8> ( REAL*8    ) -- Interpolated value in the point XC.        *
! *                                                                      *
! *  ###  20-OCT-1989    FLIN8    v 1.1 (c) L. Petrov  10-APR-2006  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IXC, JXC, JXL
        REAL*8    FLIN8, XC, X(N), Y(N), A, B
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 5 )
!
        JXC=IXC+1
        JXL=IXC
!
        IF ( JXL.LT.1 ) THEN
             JXC=2
             JXL=1
        END IF
!
        IF ( JXC.GT.N ) THEN
             JXL=N-1
             JXC=N
        END IF
!
! ----- Y(X)=A*X+B
!
        A=( Y(JXC)-Y(JXL) )/( X(JXC)-X(JXL) )
        B=Y(JXL)-A*X(JXL)
        FLIN8=A*XC+B
!
        RETURN
        END  !#!  FLIN8  #!#
