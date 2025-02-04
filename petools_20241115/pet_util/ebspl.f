      SUBROUTINE EBSPL_LSQ ( MS, MF, ARG_VEC, FUN_VEC, DEG, ARG_NOD, &
     &                       SPL_VEC, POSTFIT_RMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_LSQ  computes coefficients of expansion of function *
! *   of FUN_VEC of argument ARG_VEC which is defined as a table of      *
! *   MF arguments and values over the B-spline of degree DEG with       *
! *   MS knots using the LSQ method. The number of knots MS should be    *
! *   no more than MF-2. Using another language, smoothing B-spline      *
! *   defined at MF knots is computed in such a way to minimize the      *
! *   differences with the function FUN_VEC(ARG_VEC) in a mean quadratic *
! *   sense. The coefficients are stored in array SPL_VEC of dimension   *
! *   [1-DEG:MS-1]. The root mean square of postfit residuals is         *
! *   computed as well. All values of function are considered to have    *
! *   equal weights.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *           MS ( INTEGER*4 ) -- The number of knots of the B-spline.   *
! *           MF ( INTEGER*4 ) -- The number values of function that is  *
! *                               being expanded into B-spline basis.    *
! *      ARG_VEC ( REAL*8    ) -- Array arguments of the function to be  *
! *                               expanded. Dimension: MF.               *
! *      FUN_VEC ( REAL*8    ) -- Array values of the function to be     *
! *                               expanded. Dimension: MF.               *
! *          DEG ( INTEGER*4 ) -- Degree of the B-spline.                *
! *      ARG_NOD ( REAL*8    ) -- Array arguments of the B-spline. The   *
! *                               array defines the knots of the spline. *
! *                               Dimension: MS.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      SPL_VEC ( REAL*8    ) -- Array of B-spline coefficients that    *
! *                               approximates the function.             *
! *                               Dimension: [1-DEG:MS-1].               *
! *  POSTFIT_RMS ( REAL*8    ) -- Root mean square of postfit residuals. *
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
! *  ### 24-MAR-2010   EBSPL_LSQ    v1.0 (c)  L. Petrov  24-MAR-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MS, MF, DEG, IUER
      REAL*8     ARG_NOD(MS), ARG_VEC(MF), FUN_VEC(MF), &
     &           SPL_VEC(1-DEG:MS-1), POSTFIT_RMS
!
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), NOR_MAT(:), NOR_VEC(:)
      REAL*8     RC, RES
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, LPAR, LPA2, IER
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, DP_VV_V, EBSPL_VAL_R8 
      INTEGER*4, EXTERNAL :: I_LEN
!
      LPAR = MS + DEG - 1
      LPA2 = (LPAR*(LPAR+1))/2 
!
! --- Allocate dynamic memory for temporary arrays
!
      ALLOCATE ( EQU_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1411, IUER, 'EBSPL_LSQ', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array EQU' )
           RETURN 
      END IF
      ALLOCATE ( NOR_MAT(LPA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPA2, STR )
           CALL ERR_LOG ( 1412, IUER, 'EBSPL_LSQ', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array NOR_MAT' )
           DEALLOCATE ( EQU_VEC )
           RETURN 
      END IF
      ALLOCATE ( NOR_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1413, IUER, 'EBSPL_LSQ', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array NOR_VEC' )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT_R8 ( LPAR, NOR_VEC )
      CALL NOUT_R8 ( LPAR, EQU_VEC )
      CALL NOUT_R8 ( LPA2, NOR_MAT )
!
! --- Cycle over array of arguments/function value and build the normal equation
!
      DO 410 J1=1,MF
         CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ------ Compute observation equation
!
         DO 420 J2=1-DEG,MS-1
            EQU_VEC(J2+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J2, ARG_VEC(J1) )
 420     CONTINUE 
!
! ------ Compute its contribution to NOR_MAT ( normal matrix ) 
!
         CALL DIAD_CVT_S ( 1.0D0, LPAR, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ ... and NOR_VEC ( normal vector )
!
         DO 430 J3=1,LPAR
            NOR_VEC(J3) = NOR_VEC(J3) + EQU_VEC(J3)*FUN_VEC(J1)
 430     CONTINUE 
 410  CONTINUE 
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( LPAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1414, IUER, 'EBSPL_LSQ', 'Failure to invert '// &
     &         'normal matrix' )
           DEALLOCATE ( NOR_VEC )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
           CALL NOUT_R8 ( 1-DEG+MS, SPL_VEC )
           RETURN 
      END IF
!
! --- Compute array of spline coefficients using normal vector and the 
! --- inverse to normnal matrix
!
      CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, SPL_VEC, -2 )
!
! --- Compute statstics of postfit residuals
!
      POSTFIT_RMS = 0.0D0
      DO 440 J4=1,MF
!!
!         CALL NOUT_R8 ( LPAR, EQU_VEC )
!         DO 450 J5=1-DEG,MS-1
!            EQU_VEC(J5+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J5, ARG_VEC(J4) )
! 450     CONTINUE 
!         RES = FUN_VEC(J4) - DP_VV_V ( LPAR, EQU_VEC, SPL_VEC )
!!
         RES = FUN_VEC(J4) - EBSPL_VAL_R8 ( MS, DEG, ARG_VEC(J4), ARG_NOD, SPL_VEC )
         POSTFIT_RMS = POSTFIT_RMS + RES**2
 440  CONTINUE 
      POSTFIT_RMS = DSQRT ( POSTFIT_RMS/MF )
!
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( EQU_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EBSPL_LSQ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EBSPL_WLSQ ( MS, MF, ARG_VEC, FUN_VEC, WEI_VEC, &
     &                        DEG, ARG_NOD, SPL_VEC, POSTFIT_WRMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_WLSQ computes coefficients of expansion of function *
! *   FUN_VEC of argument ARG_VEC which is defined as a table of         *
! *   MF arguments and values over the B-spline of degree DEG with       *
! *   MS knots using the weghted LSQ method. The number of knots MS      *
! *   should be no more than MF-2. Using another language, smoothing     *
! *   B-spline defined at MF knots is computed in such a way to minimize *
! *   the differences with the function FUN_VEC(ARG_VEC) in a mean       *
! *   quadratic sense with applying weights. The coefficients are stored *
! *   in array SPL_VEC of dimension [1-DEG:MS-1]. The weighted root mean *
! *   square of postfit residuals is computed as well. 
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *           MS ( INTEGER*4 ) -- The number of knots of the B-spline.   *
! *           MF ( INTEGER*4 ) -- The number values of function that is  *
! *                               being expanded into B-spline basis.    *
! *      ARG_VEC ( REAL*8    ) -- Array arguments of the function to be  *
! *                               expanded. Dimension: MF.               *
! *      FUN_VEC ( REAL*8    ) -- Array values of the function to be     *
! *                               expanded. Dimension: MF.               *
! *      WEI_VEC ( REAL*8    ) -- Array values of the function to be     *
! *                               expanded. Dimension: MF.               *
! *          DEG ( INTEGER*4 ) -- Degree of the B-spline.                *
! *      ARG_NOD ( REAL*8    ) -- Array arguments of the B-spline. The   *
! *                               define knots of the spline.            *
! *                               Dimension: MS.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      SPL_VEC ( REAL*8    ) -- Array of B-spline coefficients that    *
! *                               approximate the function.              *
! *                               Dimension: [1-DEG:MS-1].               *
! *  POSTFIT_RMS ( REAL*8    ) -- Root mean square of postfit residuals. *
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
! *  ### 25-MAR-2010   EBSPL_WLSQ   v1.0 (c)  L. Petrov  25-MAR-2010 ### *
! *                                                                      *
! ************************************************************************
       IMPLICIT   NONE 
      INTEGER*4  MS, MF, DEG, IUER
      REAL*8     ARG_NOD(MS), ARG_VEC(MF), FUN_VEC(MF), WEI_VEC(MF), &
     &           SPL_VEC(1-DEG:MS-1), POSTFIT_WRMS
!
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), NOR_MAT(:), NOR_VEC(:)
      REAL*8     RC, RES, WW
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, LPAR, LPA2, IER
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, DP_VV_V, EBSPL_VAL_R8 
      INTEGER*4, EXTERNAL :: I_LEN
!
      LPAR = MS + DEG - 1
      LPA2 = (LPAR*(LPAR+1))/2 
!
! --- Allocate dynamic memory for temporary arrays
!
      ALLOCATE ( EQU_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1421, IUER, 'EBSPL_WLSQ', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array EQU' )
           RETURN 
      END IF
      ALLOCATE ( NOR_MAT(LPA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPA2, STR )
           CALL ERR_LOG ( 1412, IUER, 'EBSPL_WLSQ', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array NOR_MAT' )
           DEALLOCATE ( EQU_VEC )
           RETURN 
      END IF
      ALLOCATE ( NOR_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1423, IUER, 'EBSPL_WLSQ', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array NOR_VEC' )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT_R8 ( LPAR, NOR_VEC )
      CALL NOUT_R8 ( LPAR, EQU_VEC )
      CALL NOUT_R8 ( LPA2, NOR_MAT )
!
! --- Cycle over array of arguments/function value and build the normal equation
!
      DO 410 J1=1,MF
         CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ------ Compute observation equation
!
         DO 420 J2=1-DEG,MS-1
            EQU_VEC(J2+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J2, ARG_VEC(J1) )
 420     CONTINUE 
!
! ------ Compute its contribution to NOR_MAT ( normal matrix ) 
!
         CALL DIAD_CVT_S ( WEI_VEC(J1)**2, LPAR, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ ... and NOR_VEC ( normal vector )
!
         DO 430 J3=1,LPAR
            NOR_VEC(J3) = NOR_VEC(J3) + EQU_VEC(J3)*FUN_VEC(J1)*WEI_VEC(J1)**2
 430     CONTINUE 
 410  CONTINUE 
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( LPAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1424, IUER, 'EBSPL_WLSQ', 'Failure to invert '// &
     &         'normal matrix' )
           DEALLOCATE ( NOR_VEC )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
           CALL NOUT_R8 ( 1-DEG+MS, SPL_VEC )
           RETURN 
      END IF
!
! --- Compute array of spline coefficients using normal vector and the 
! --- inverse to normnal matrix
!
      CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, SPL_VEC, -2 )
!
! --- Compute statstics of postfit residuals
!
      POSTFIT_WRMS = 0.0D0
      WW = 0.0D0
      DO 440 J4=1,MF
         RES = FUN_VEC(J4) - EBSPL_VAL_R8 ( MS, DEG, ARG_VEC(J4), ARG_NOD, SPL_VEC )
         POSTFIT_WRMS = POSTFIT_WRMS + RES**2*WEI_VEC(J4)**2
         WW = WW + WEI_VEC(J4)**2
 440  CONTINUE 
      POSTFIT_WRMS = DSQRT ( POSTFIT_WRMS/WW )
!
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( EQU_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EBSPL_WLSQ  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION EBSPL_VAL_R8 ( MN, DEG, ARG, ARG_VEC, BCOE_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_VAL_R8  computes value of function at point with    *
! *   argument ARG using coefficients of its expansion over the B-spline *
! *   basis.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MN ( INTEGER*4 ) -- The number of knots of the B-spline.       *
! *      DEG ( INTEGER*4 ) -- Degree of B-spline.                        *
! *      ARG ( REAL*8    ) -- Argument for which the function is         *
! *                           computed.
! *  ARG_VEC ( REAL*8    ) -- Array of arguments for B-spline at knots.  *
! *                           Dimension: [1:MN].                         *
! * BCOE_VEC ( REAL*8    ) -- Array of B-spline coefficients.            *
! *                           Dimension: [1-DEG:MN-1].                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <EBSPL_VAL_R8> ( REAL*8    ) -- value of the function computed using *
! *                                 its expansion over B-spline basis.   *
! *                                                                      *
! *  ### 25-MAR-2010  EBSPL_VAL_R8  v1.0 (c)  L. Petrov 25-MAR-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EBSPL_VAL_R8 
      INTEGER*4  MN, DEG
      REAL*8     ARG, ARG_VEC(MN), BCOE_VEC(1-DEG:MN-1)
      INTEGER*4  J1, IP
      REAL*8,    EXTERNAL :: BSPL_VAL 
      INTEGER*4, EXTERNAL :: IXMN8 
!
      EBSPL_VAL_R8 = 0.0D0
      IF ( ARG < ARG_VEC(1)  .OR.  ARG > ARG_VEC(MN) ) THEN
!
! -------- If the argument is away from the the interpolation range,
! -------- nothing to do: its value is zero.
!
           RETURN 
      END IF
      IP = IXMN8 ( MN, ARG_VEC, ARG )
      IF ( IP < 1    ) IP = 1 
      IF ( IP > MN-1 ) IP = MN-1
      DO 410 J1=0,DEG
         EBSPL_VAL_R8 = EBSPL_VAL_R8 + BCOE_VEC(IP-J1)* &
     &                                 BSPL_VAL ( MN, ARG_VEC, DEG, IP-J1, ARG )
 410  CONTINUE 
      RETURN
      END  FUNCTION EBSPL_VAL_R8  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION EBSPL_DER_R8 ( MN, DEG, ARG, ARG_VEC, BCOE_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_DER_R8  computes the first derivative of a function *
! *   at the point with argument ARG using coefficients of its expansion *
! *   over the B-spline basis.                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MN ( INTEGER*4 ) -- The number of knots of the B-spline.       *
! *      DEG ( INTEGER*4 ) -- Degree of B-spline.                        *
! *      ARG ( REAL*8    ) -- Argument for which the first derivative    *
! *                           of the function is computed.               *
! *  ARG_VEC ( REAL*8    ) -- Array of arguments for B-spline at knots.  *
! *                           Dimension: [1:MN].                         *
! * BCOE_VEC ( REAL*8    ) -- Array of B-spline coefficients.            *
! *                           Dimension: [1-DEG:MN-1].                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <EBSPL_DER_R8> ( REAL*8    ) -- Value of the first derivative of the *
! *                                 function computed using its          *
! *                                 expansion over the B-spline basis.   *
! *                                                                      *
! *  ### 25-MAR-2010  EBSPL_DER_R8  v1.0 (c)  L. Petrov 25-MAR-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EBSPL_DER_R8 
      INTEGER*4  MN, DEG
      REAL*8     ARG, ARG_VEC(MN), BCOE_VEC(1-DEG:MN-1)
      INTEGER*4  J1, IP
      REAL*8,    EXTERNAL :: BSPL_DER
      INTEGER*4, EXTERNAL :: IXMN8 
!
      EBSPL_DER_R8 = 0.0D0
      IF ( ARG < ARG_VEC(1)  .OR.  ARG > ARG_VEC(MN) ) THEN
!
! -------- If the argument is away from the the interpolation range,
! -------- nothing to do: its value is zero.
!
           RETURN 
      END IF
      IP = IXMN8 ( MN, ARG_VEC, ARG )
      IF ( IP < 1    ) IP = 1 
      IF ( IP > MN-1 ) IP = MN-1
      DO 410 J1=0,DEG
         EBSPL_DER_R8 = EBSPL_DER_R8 + BCOE_VEC(IP-J1)* &
     &                                 BSPL_DER ( MN, ARG_VEC, DEG, IP-J1, ARG )
 410  CONTINUE 
      RETURN
      END  FUNCTION EBSPL_DER_R8  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION EBSPL_DR2_R8 ( MN, DEG, ARG, ARG_VEC, BCOE_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_DR2_R8  computes the second derivative of the       *
! *   function at point with argument ARG using coefficients of its      *
! *   expansion over the B-spline basis.                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MN ( INTEGER*4 ) -- The number of knots of the B-spline.       *
! *      DEG ( INTEGER*4 ) -- Degree of B-spline.                        *
! *      ARG ( REAL*8    ) -- Argument for which the second derivative   *
! *                           of the function is computed.               *
! *  ARG_VEC ( REAL*8    ) -- Array of arguments for B-spline at knots.  *
! *                           Dimension: [1:MN].                         *
! * BCOE_VEC ( REAL*8    ) -- Array of B-spline coefficients.            *
! *                           Dimension: [1-DEG:MN-1].                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <EBSPL_DE2_R8> ( REAL*8    ) -- Value of the second derivative of    *
! *                                 the function computed using its      *
! *                                 expansion over the B-spline basis.   *
! *                                                                      *
! *  ### 25-MAR-2010  EBSPL_DR2_R8  v1.0 (c)  L. Petrov 25-MAR-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EBSPL_DR2_R8 
      INTEGER*4  MN, DEG
      REAL*8     ARG, ARG_VEC(MN), BCOE_VEC(1-DEG:MN-1)
      INTEGER*4  J1, IP
      REAL*8,    EXTERNAL :: BSPL_DR2
      INTEGER*4, EXTERNAL :: IXMN8 
!
      EBSPL_DR2_R8 = 0.0D0
      IF ( ARG < ARG_VEC(1)  .OR.  ARG > ARG_VEC(MN) ) THEN
!
! -------- If the argument is away from the the interpolation range,
! -------- nothing to do: its value is zero.
!
           RETURN 
      END IF
      IP = IXMN8 ( MN, ARG_VEC, ARG )
      IF ( IP < 1    ) IP = 1 
      IF ( IP > MN-1 ) IP = MN-1
      DO 410 J1=0,DEG
         EBSPL_DR2_R8 = EBSPL_DR2_R8 + BCOE_VEC(IP-J1)* &
     &                                 BSPL_DR2 ( MN, ARG_VEC, DEG, IP-J1, ARG )
 410  CONTINUE 
      RETURN
      END  FUNCTION EBSPL_DR2_R8  !#!#
