      SUBROUTINE EBSPL_LSQ_CNS ( MF, ARG_VEC, FUN_VEC, MS, DEG, ARG_NOD, &
     &                           SPL_VEC, CNS_VAL_SIG, CNS_DER_SIG, &
     &                           CNS_DR2_SIG, POSTFIT_RMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_LSQ_CNS  computes coefficients of expansion of      *
! *   function of FUN_VEC of argument ARG_VEC which is defined as a      *
! *   table of MF arguments and values over the B-spline of degree DEG   *
! *   with MS knots using the LSQ method. The number of knots MS should  *
! *   be no more than MF-2. Using another language, smoothing B-spline   *
! *   defined at MF knots is computed in such a way to minimize the      *
! *   differences with the function FUN_VEC(ARG_VEC) in a mean quadratic *
! *   sense. The coefficients are stored in array SPL_VEC of dimension   *
! *   [1-DEG:MS-1]. The root mean square of postfit residuals is         *
! *   computed as well. All values of function are considered to have    *
! *   equal weights.                                                     *
! *                                                                      *
! *   If CNS_VAL_SIG > 0.0D0, then a constraint on the value of the      *
! *      B-spline at each node is set to zero with recoprocal weight     *
! *      CNS_VAL_SIG.                                                    *
! *                                                                      *
! *   If CNS_DER_SIG > 0.0D0, then a constraint on the first derivatice  *
! *      of the B-spline at each node is set to zero with recoprocal     *
! *      weight CNS_DER_SIG.                                             *
! *                                                                      *
! *   If CNS_DR2_SIG > 0.0D0, then a constraint on the second derivatice *
! *      of the B-spline at each node is set to zero with recoprocal     *
! *      weight CNS_DR2_SIG.                                             *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *           MF ( INTEGER*4 ) -- The number values of function that is  *
! *                               being expanded into B-spline basis.    *
! *      ARG_VEC ( REAL*8    ) -- Array arguments of the function to be  *
! *                               expanded. Dimension: MF.               *
! *      FUN_VEC ( REAL*8    ) -- Array values of the function to be     *
! *                               expanded. Dimension: MF.               *
! *           MS ( INTEGER*4 ) -- The number of knots of the B-spline.   *
! *          DEG ( INTEGER*4 ) -- Degree of the B-spline.                *
! *      ARG_NOD ( REAL*8    ) -- Array arguments of the B-spline. The   *
! *                               define knots of the spline.            *
! *                               Dimension: MS.                         *
! *  CNS_VAL_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
! *                               imposed on value of B-spline           *
! *                               at every knot. If CNS_VAL_SIG <= 0.0D0,*
! *                               the constraint is not imposed.         *
! *  CNS_DER_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
! *                               imposed on the first derivative        *
! *                               of B-spline at every knot.             *
! *                               If CNS_DER_SIG <= 0.0D0,               *
! *                               the constraint is not imposed.         *
! *  CNS_DR2_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
! *                               imposed on the second derivative       *
! *                               of B-spline at every knot.             *
! *                               If CNS_DR2_SIG <= 0.0D0,               *
! *                               the constraint is not imposed.         *
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
! * ### 24-MAR-2010  EBSPL_LSQ_CNS  v1.1 (c)  L. Petrov  12-AUG-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MS, MF, DEG, IUER
      REAL*8     ARG_NOD(MS), ARG_VEC(MF), FUN_VEC(MF), &
     &           SPL_VEC(1-DEG:MS-1), POSTFIT_RMS, CNS_VAL_SIG, &
     &           CNS_DER_SIG, CNS_DR2_SIG
!
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), NOR_MAT(:), NOR_VEC(:)
      REAL*8     RC, RES
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, LPAR, LPA2, &
     &           PIV_IND, IER
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2, DP_VV_V, EBSPL_VAL_R8 
      INTEGER*4, EXTERNAL :: I_LEN, IXMN8
!
      LPAR = MS + DEG - 1
      LPA2 = (LPAR*(LPAR+1))/2 
!
! --- Allocate dunamic memory for temporary arrays
!
      ALLOCATE ( EQU_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1411, IUER, 'EBSPL_LSQ_CNS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array EQU' )
           RETURN 
      END IF
      ALLOCATE ( NOR_MAT(LPA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPA2, STR )
           CALL ERR_LOG ( 1412, IUER, 'EBSPL_LSQ_CNS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array NOR_MAT' )
           DEALLOCATE ( EQU_VEC )
           RETURN 
      END IF
      ALLOCATE ( NOR_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1413, IUER, 'EBSPL_LSQ_CNS', 'Failure to allocate '// &
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
      CALL NOUT_R8 ( 1-DEG+MS, SPL_VEC(1-DEG) )
!
! --- Cycle over array of arguments/function value and build the normal equation
!
      DO 410 J1=1,MF
         IF ( J1 == 1 ) THEN
              PIV_IND = 1
           ELSE IF ( J1 == MF ) THEN
              PIV_IND = MS - 1
           ELSE 
              PIV_IND = IXMN8 ( MS, ARG_NOD, ARG_VEC(J1) )
         END IF
!
         CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ------ Compute observation equation
!
         DO 420 J2=PIV_IND-DEG,PIV_IND
            EQU_VEC(J2+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J2, ARG_VEC(J1) )
 420     CONTINUE 
!
! ------ Compute its contribution to NOR_MAT ( normal matrix ) 
!
         CALL DIAD_CVT_S_PIV ( 1.0D0, LPAR, PIV_IND, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ ... and NOR_VEC ( normal vector )
!
         DO 430 J3=PIV_IND,PIV_IND+DEG
            NOR_VEC(J3) = NOR_VEC(J3) + EQU_VEC(J3)*FUN_VEC(J1)
 430     CONTINUE 
!
! ------ Constraints
!
         IF ( CNS_VAL_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 450 J5=PIV_IND-DEG,PIV_IND
                 EQU_VEC(J5+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J5, ARG_VEC(J1) )
 450          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_VAL_SIG**2, LPAR, PIV_IND, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
         END IF
!
         IF ( CNS_DER_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 460 J6=PIV_IND-DEG,PIV_IND
                 EQU_VEC(J6+DEG) = BSPL_DER ( MS, ARG_NOD, DEG, J6, ARG_VEC(J1) )
 460          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_DER_SIG**2, LPAR, PIV_IND, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
         END IF
         IF ( CNS_DR2_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 470 J7=PIV_IND-DEG,PIV_IND
                 EQU_VEC(J7+DEG) = BSPL_DR2 ( MS, ARG_NOD, DEG, J7, ARG_VEC(J1) )
 470          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_DR2_SIG**2, LPAR, PIV_IND, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
         END IF
 410  CONTINUE 
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( LPAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1414, IUER, 'EBSPL_LSQ_CNS', 'Failure to invert '// &
     &         'normal matrix' )
           DEALLOCATE ( NOR_VEC )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
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
      DO 4110 J11=1,MF
         RES = FUN_VEC(J11) - EBSPL_VAL_R8 ( MS, DEG, ARG_VEC(J11), ARG_NOD, SPL_VEC )
         POSTFIT_RMS = POSTFIT_RMS + RES**2
 4110 CONTINUE 
      POSTFIT_RMS = DSQRT ( POSTFIT_RMS/MF )
!
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( EQU_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EBSPL_LSQ_CNS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EBSPL_WLSQ_CNS ( MS, MF, ARG_VEC, FUN_VEC, WEI_VEC, &
     &                            DEG, ARG_NOD, SPL_VEC, &
     &                            CNS_VAL_SIG, CNS_DER_SIG, &
     &                            CNS_DR2_SIG, POSTFIT_WRMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_WLSQ computes coefficients of expansion of function *
! *   FUN_VEC of argument ARG_VEC which is defined as a table of         *
! *   MF arguments and values over the B-spline of degree DEG with       *
! *   MS knots using the weighted LSQ method. The number of knots MS     *
! *   should be no more than MF-2. Using another language, smoothing     *
! *   B-spline defined at MF knots is computed in such a way to minimize *
! *   the differences with the function FUN_VEC(ARG_VEC) in a mean       *
! *   quadratic sense with applying weights. The coefficients are stored *
! *   in array SPL_VEC of dimension [1-DEG:MS-1]. The weighted root mean *
! *   square of postfit residuals is computed as well. Weights of        *
! *   function values are supplied in array WEI_VEC. 
! *                                                                      *
! *   If CNS_VAL_SIG > 0.0D0, then a constraint on the value of the      *
! *      B-spline at each node is set to zero with recoprocal weight     *
! *      CNS_VAL_SIG.                                                    *
! *                                                                      *
! *   If CNS_DER_SIG > 0.0D0, then a constraint on the first derivatice  *
! *      of the B-spline at each node is set to zero with recoprocal     *
! *      weight CNS_DER_SIG.                                             *
! *                                                                      *
! *   If CNS_DR2_SIG > 0.0D0, then a constraint on the second derivatice *
! *      of the B-spline at each node is set to zero with recoprocal     *
! *      weight CNS_DR2_SIG.                                             *
! *                                                                      *
! *   Imposing constraints is necessary when there are some data losses. *
! *   NB: strong constraints ( CNS_VAL_SIG is small ) distort results.   *
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
! *      WEI_VEC ( REAL*8    ) -- Weight vector associated with values   *
! *                               of the function to be expanded.        *
! *                               Dimension: MF.                         *
! *          DEG ( INTEGER*4 ) -- Degree of the B-spline.                *
! *      ARG_NOD ( REAL*8    ) -- Array arguments of the B-spline. The   *
! *                               define knots of the spline.            *
! *                               Dimension: MS.                         *
! *  CNS_VAL_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
! *                               imposed on value of B-spline           *
! *                               at every knot. If CNS_VAL_SIG <= 0.0D0,*
! *                               the constraint is not imposed.         *
! *  CNS_DER_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
! *                               imposed on the first derivative        *
! *                               of B-spline at every knot.             *
! *                               If CNS_DER_SIG <= 0.0D0,               *
! *                               the constraint is not imposed.         *
! *  CNS_DR2_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
! *                               imposed on the second derivative       *
! *                               of B-spline at every knot.             *
! *                               If CNS_DR2_SIG <= 0.0D0,               *
! *                               the constraint is not imposed.         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      SPL_VEC ( REAL*8    ) -- Array of B-spline coefficients that    *
! *                               approximate the function.              *
! *                               Dimension: [1-DEG:MS-1].               *
! *  POSTFIT_WRMS ( REAL*8   ) -- Weighted Root mean square of postfit   *
!*                                residuals.                             *
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
! * ### 25-MAR-2010  EBSPL_WLSQ_CNS v2.1 (c)  L. Petrov  15-NOV-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MS, MF, DEG, IUER
      REAL*8     ARG_NOD(MS), ARG_VEC(MF), FUN_VEC(MF), WEI_VEC(MF), &
     &           SPL_VEC(1-DEG:MS-1), POSTFIT_WRMS, CNS_VAL_SIG, &
     &           CNS_DER_SIG, CNS_DR2_SIG
!
      REAL*8     ARG_VAL
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-6 )
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), NOR_MAT(:), NOR_VEC(:)
      REAL*8     RC, RES, WW
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, LPAR, LPA2, PIV_IND, IER
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2, DP_VV_V, EBSPL_VAL_R8 
      INTEGER*4, EXTERNAL :: I_LEN, IXMN8
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
           CALL ERR_LOG ( 1911, IUER, 'EBSPL_WLSQ_CNS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array EQU' )
           RETURN 
      END IF
      ALLOCATE ( NOR_MAT(LPA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPA2, STR )
           CALL ERR_LOG ( 1912, IUER, 'EBSPL_WLSQ_CNS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary array NOR_MAT' )
           DEALLOCATE ( EQU_VEC )
           RETURN 
      END IF
      ALLOCATE ( NOR_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1913, IUER, 'EBSPL_WLSQ_CNS', 'Failure to allocate '// &
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
         IF ( J1 == 1 ) THEN
              PIV_IND = 1
           ELSE IF ( J1 == MF ) THEN
              PIV_IND = MS - 1
           ELSE 
              PIV_IND = IXMN8 ( MS, ARG_NOD, ARG_VEC(J1) )
         END IF
!
         CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ------ Compute observation equation
!
         DO 420 J2=PIV_IND-DEG,PIV_IND
            EQU_VEC(J2+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J2, ARG_VEC(J1) )
 420     CONTINUE 
!
! ------ Compute its contribution to NOR_MAT ( normal matrix ) 
!
         CALL DIAD_CVT_S_PIV ( WEI_VEC(J1)**2, LPAR, PIV_IND, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ ... and NOR_VEC ( normal vector )
!
         DO 430 J3=PIV_IND,PIV_IND+DEG
            NOR_VEC(J3) = NOR_VEC(J3) + EQU_VEC(J3)*FUN_VEC(J1)*WEI_VEC(J1)**2
 430     CONTINUE 
!
! ------ Constraints
!
 410  CONTINUE 
      DO 440 J4=1,MS-1
!
! ------ Compute observation equation
!
         IF ( J4 == 1 ) THEN
              ARG_VAL = ARG_NOD(J4) + EPS*(ARG_NOD(2)  - ARG_NOD(1))
            ELSE IF ( J4 == MS ) THEN
              ARG_VAL = ARG_NOD(J4) - EPS*(ARG_NOD(MS) - ARG_NOD(MS-1))
            ELSE
              ARG_VAL = ARG_NOD(J4)
         END IF
         IF ( CNS_VAL_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
              DO 450 J5=J4-DEG,J4
                 EQU_VEC(J5+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J5, ARG_VAL )
 450          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_VAL_SIG**2, LPAR, J4, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
         END IF
!
         IF ( CNS_DER_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 460 J6=J4-DEG,J4
                 EQU_VEC(J6+DEG) = BSPL_DER ( MS, ARG_NOD, DEG, J6, ARG_VAL )
 460          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_DER_SIG**2, LPAR, J4, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
         END IF
         IF ( CNS_DR2_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 470 J7=J4-DEG,J4
                 EQU_VEC(J7+DEG) = BSPL_DR2 ( MS, ARG_NOD, DEG, J7, ARG_VAL )
 470          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_DR2_SIG**2, LPAR, J4, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
         END IF
 440  CONTINUE 
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( LPAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1914, IUER, 'EBSPL_WLSQ_CNS', 'Failure to invert '// &
     &         'normal matrix' )
           DEALLOCATE ( NOR_VEC )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
           CALL NOUT_R8 ( 1-DEG+MS, SPL_VEC )
           RETURN 
      END IF
!
! --- Compute array of spline coefficients using normal vector and the 
! --- inverse to normal matrix
!
      CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, SPL_VEC, -2 )
!
! --- Compute statstics of postfit residuals
!
      POSTFIT_WRMS = 0.0D0
      WW           = 0.0D0
      DO 480 J8=1,MF
         RES = FUN_VEC(J8) - EBSPL_VAL_R8 ( MS, DEG, ARG_VEC(J8), ARG_NOD, SPL_VEC )
         POSTFIT_WRMS = POSTFIT_WRMS + RES**2*WEI_VEC(J8)**2
         WW           = WW           +        WEI_VEC(J8)**2
 480  CONTINUE 
      IF ( WW > 0.0D0 ) THEN
           POSTFIT_WRMS = DSQRT ( POSTFIT_WRMS/WW )
      END IF
!
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( EQU_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EBSPL_WLSQ_CNS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAD_CVT_S_PIV ( CONST, N, PIV_IND, NEL, VEC_A, VEC_T, MAT )
! ************************************************************************
! *                                                                      *
! *   Routine DIAD_CVT_S_PIV 
! *                                                                      *
! * ### 12-AUG-2016 DIAD_CVT_S_PIV  v1.0 (c) L. Petrov  12-AUG-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, PIV_IND, NEL
      REAL*8     CONST, VEC_A(N), VEC_T(N), MAT(*)
      REAL*8     RT
      INTEGER*4  J1, J2, IP0, IP
!
      IP0 = (PIV_IND*(PIV_IND+1))/2
      DO 410 J1=PIV_IND,PIV_IND+NEL
         IP  = IP0
         DO 420 J2=PIV_IND,J1
            MAT(IP) = MAT(IP) + CONST*VEC_A(J2)*VEC_T(J1)
            IP = IP + 1
 420     CONTINUE 
         IP0 = IP0 + J1
 410  CONTINUE 
!
      RETURN
      END SUBROUTINE  DIAD_CVT_S_PIV  !#!#
