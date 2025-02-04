      SUBROUTINE POLY_SOLVE_SIMPLE ( DEG, TIM, T0, VAL, POLY_COEF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine POLY_SOLVE computes the coeffients of the interpolating    *
! *   polynomial. Expansion is unique and is                             *
! *   computed exactly.                                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * TIM       ( REAL*8     ) -- The argument of the function.            *
! * T0        ( REAL*8     ) -- Reference argument. The polynomial       *
! *                             is a funtion of TIM(i) - T0.             *
! * VAL       ( REAL*8     ) -- Array of the values. Dimension: NP.      *
! * DEG       ( INTEGER*4  ) -- Degree of the polynomial in a range      *
! *                             of [0, 63]. Should be less than NP.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * POLY_COEF ( REAL*8    ) -- Arracy of the polynomial expansion.       *
! *                            Dimension: (0:DEG).                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
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
! *  ### 12-APR-2024               v1.0 (c)  J. Skeens  12-APR-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, IUER
      REAL*8     TIM(DEG+1), T0, VAL(DEG+1), POLY_COEF(0:DEG)
      CHARACTER  STR*32
      INTEGER*4  MP
      PARAMETER  ( MP = 64 )
      REAL*8     NOR_MAT(DEG+1,DEG+1), NOR_VEC(DEG+1), EQU_OBS(DEG+1), RC
      INTEGER*4  J1, J2, IER
!
      RC = 1.0D-15
      IF ( DEG <  0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5925, IUER, 'POLY_SOLVE_SIMPLE', 'Wrong degree of '// &
     &         'the polynomial. A non-negative integer was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
      IF ( DEG >  MP-1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5926, IUER, 'POLY_SOLVE_SIMPLE', 'Wrong degree of '// &
     &         'the polyonal. A non-negative integer < 64 was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
!
      NOR_MAT = 0.0D0
      DO 410 J1=1,DEG+1
         EQU_OBS(1) = 1.0D0
         IF ( DEG > 0) THEN
              DO 420 J2=2,DEG+1
                 EQU_OBS(J2) = EQU_OBS(J2-1)*(TIM(J1) - T0)
 420          CONTINUE 
         END IF
         NOR_MAT(J1,:) = EQU_OBS
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVA ( DEG+1, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5927, IUER, 'POLY_SOLVE', 'Error in an attempt '// &
     &         'to invert normal matrix' )
           RETURN 
      END IF

      CALL MUL_MV_IV_V ( DEG+1, DEG+1, NOR_MAT, DEG+1, VAL, DEG+1, POLY_COEF(0:DEG), IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE POLY_SOLVE_SIMPLE   !#!

      SUBROUTINE POLY_SOLVE_VAND ( NP, TIM, T0, VAL, DEG, POLY_COEF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine POLY_SOLVE_VAND computes the coeffients of the 
! *   interpolating polynomial in the Vandermonde system with LAPACK.    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * NP        ( INTEGER*4  ) -- The number of points. Dimension: NP.     *
! * TIM       ( REAL*8     ) -- The argument of the function.            *
! * T0        ( REAL*8     ) -- Reference argument. The polynomial       *
! *                             is a funtion of TIM(i) - T0.             *
! * VAL       ( REAL*8     ) -- Array of the vallyes. Dimension: NP.     *
! * DEG       ( INTEGER*4  ) -- Degree of the polynomial in a range      *
! *                             of [0, 63]. Should be less than NP.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * POLY_COEF ( REAL*8    ) -- Arracy of the polynomial expansion.       *
! *                            Dimension: (0:DEG).                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
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
! *  ### 12-APR-2024               v1.0 (c)  J. Skeens  12-APR-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NP, DEG, IUER
      REAL*8     TIM(NP), T0, VAL(NP), POLY_COEF(0:DEG)
      CHARACTER  STR*32
      INTEGER*4  MP
      PARAMETER  ( MP = 64 )
      REAL*8     NOR_MAT(NP,DEG+1), NOR_VEC(NP), EQU_OBS(NP), RC
      INTEGER*4  J1, J2, IER, IPIV(NP)
!
      IF ( DEG <  0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5928, IUER, 'POLY_SOLVE_VAND', 'Wrong degree of '// &
     &         'the polyonal. A non-negative integer was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
      IF ( DEG >  MP-1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5929, IUER, 'POLY_SOLVE_VAND', 'Wrong degree of '// &
     &         'the polyonal. A non-negative integer < 64 was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
      IF ( DEG .GE. NP ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5930, IUER, 'POLY_SOLVE_VAND', 'Wrong degree of '// &
     &         'the polyonal. It should be less than the number of point' )
           RETURN 
      END IF
!
      NOR_MAT = 0.0D0
      DO 410 J1=1,NP
         EQU_OBS(1) = 1.0D0
         IF ( DEG > 0) THEN
              DO 420 J2=2,DEG+1
                 EQU_OBS(J2) = EQU_OBS(J2-1)*(TIM(J1) - T0)
 420          CONTINUE 
         END IF
         NOR_MAT(J1,:) = EQU_OBS
 410  CONTINUE 
      NOR_VEC = VAL
!
      CALL ERR_PASS ( IUER, IER )
      CALL DGESV ( NP, 1, NOR_MAT, NP, IPIV, NOR_VEC, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5931, IUER, 'POLY_SOLVE_VAND', 'Error in an attempt '// &
     &         'to solve linear system' )
           RETURN 
      END IF
      POLY_COEF = NOR_VEC
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE POLY_SOLVE_VAND   !#! 

      SUBROUTINE POLY_SOLVE_NDD ( DEG, TIM, T0, VAL, POLY_COEF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine POLY_SOLVE_NDD computes the coeffients of the              *
! *   interpolating polynomial using Newton's divided difference method. *
! *   This is a numerically stable way of producing a polynomial.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * TIM       ( REAL*8     ) -- The argument of the function.            *
! * T0        ( REAL*8     ) -- Reference argument. The polynomial       *
! *                             is a funtion of TIM(i) - T0.             *
! * VAL       ( REAL*8     ) -- Array of the values. Dimension: NP.      *
! * DEG       ( INTEGER*4  ) -- Degree of the polynomial in a range      *
! *                             of [0, 63]. Should be less than NP.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * POLY_COEF ( REAL*8    ) -- Arracy of the polynomial expansion.       *
! *                            Dimension: (0:DEG).                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
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
! *  ### 12-APR-2024               v1.0 (c)  J. Skeens  12-APR-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, IUER
      REAL*8     TIM(DEG+1), TIM_ADJ(DEG+1), T0, VAL(DEG+1), POLY_COEF(0:DEG), &
     &           TEMP_COEF(DEG+1), NDD_COEF(DEG+1)
      CHARACTER  STR*32
      INTEGER*4  MP
      PARAMETER  ( MP = 64 )
      REAL*8     DIVIDED_DIFF(DEG+1,DEG+1)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IER
!
      IF ( DEG <  0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5932, IUER, 'POLY_SOLVE_NDD', 'Wrong degree of '// &
     &         'the polyonal. A non-negative integer was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
      IF ( DEG >  MP-1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5933, IUER, 'POLY_SOLVE_NDD', 'Wrong degree of '// &
     &         'the polyonal. A non-negative integer < 64 was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
      TIM_ADJ = TIM - T0
!
!---- Initialize the first column of the divided difference table with VAL
!
      DIVIDED_DIFF = 0.0D0
      DO 410 J1=1,DEG+1
         DIVIDED_DIFF(J1, 1) = VAL(J1)
 410  CONTINUE
!
!---- Compute the divided differences
!
      DO 420 J2=2,DEG+1
         DO 430 J3=1,DEG+2-J2
            DIVIDED_DIFF(J3, J2) = (DIVIDED_DIFF(J3+1, J2-1) - DIVIDED_DIFF(J3, J2-1)) &
     &                           / (TIM_ADJ(J3+J2-1) - TIM_ADJ(J3))
 430     CONTINUE
 420  CONTINUE
!
!---- Extract the polynomial coefficients
!
      DO 440 J4=1,DEG+1
         NDD_COEF(J4) = DIVIDED_DIFF(1, J4)
 440  CONTINUE
      POLY_COEF = 0.0D0
      POLY_COEF(0) = NDD_COEF(1)
!
!---- Convert Newton coefficients to standard polynomial coefficients
!
      DO 450 J5=2,DEG+1
         TEMP_COEF = 0.0D0 
         TEMP_COEF(J5) = NDD_COEF(J5) 
         DO 460 J6=J5-1,1,-1
            DO 470 J7=J6,J5-1
                TEMP_COEF(J7) = TEMP_COEF(J7) - TIM_ADJ(J6) * TEMP_COEF(J7+1)
 470        CONTINUE
 460     CONTINUE
         DO 480 J8=1,J5
             POLY_COEF(J8-1) = POLY_COEF(J8-1) + TEMP_COEF(J8)
 480     CONTINUE
 450  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE POLY_SOLVE_NDD   !#!  
