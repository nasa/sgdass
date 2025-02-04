      SUBROUTINE EBSPL_LSQ_CNS3_VEC ( MF, MD, ARG_VEC, FUN, &
     &                                MS, DEG, ARG_NOD, SPL_VEC, &
     &                 CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_LSQ_CNS3_VEC computes coefficients of expansion of  *
! *   a 2D function of FUN of argument ARG_VEC which is defined as a     *
! *   table of MF arguments and values over the B-spline of degree DEG   *
! *   with MS knots using the LSQ method. Degree DEG must be 3.          *
! *   This routine is hand-tuned to degree 3. Please use EBSPL_LSQ_CNS   *
! *   if you need to expand into B-spline bassis of degree other than 3. *
! *   The number of knots MS should be no more than MF-2. Using another  *
! *   language, smoothing B-spline defined at MF knots is computed in    *
! *   such a way to minimize the differences with the function           *
! *   FUN(ARG_VEC,I) in a mean quadratic sense. The coefficients are     *
! *   stored in array SPL_VEC of dimension [1-DEG:MS-1]. The root mean   *
! *   square of postfit residuals is computed as well. All values        *
! *   of function are considered to have equal weights.                  *
! *                                                                      *
! *   MD is the second dimension of function FUN.                        *
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
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *           MS ( INTEGER*4 ) -- The number of knots of the B-spline.   *
! *           MD ( INTEGER*4 ) -- The second dimension of function FUN.  *
! *           MF ( INTEGER*4 ) -- The number values of function that is  *
! *                               being expanded into B-spline basis.    *
! *      ARG_VEC ( REAL*8    ) -- Array arguments of the function to be  *
! *                               expanded. Dimension: MF.               *
! *          FUN ( REAL*8    ) -- Array values of the function to be     *
! *                               expanded. Dimension: MF,MD.            *
! *          DEG ( INTEGER*4 ) -- Degree of the B-spline. Must be 3.     *
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
! *                               Dimension: [1-DEG:MS-1,MS].            *
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
! * ## 24-MAR-2010 EBSPL_LSQ_CNS3_VEC v2.1 (c) L. Petrov 01-AUG-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MF, MD, MS, DEG, IUER
      REAL*8     ARG_VEC(MF), ARG_NOD(MS), FUN(MF,MD), &
     &           SPL_VEC(1-DEG:MS-1,MD), &
     &           CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG
!
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), NOR_MAT(:), NOR_VEC(:,:), BMAT(:), FUND(:)
      INTEGER*4, ALLOCATABLE :: IPIV(:)
      REAL*8     RC, RES
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, LPAR, LPA2, PIV_IND, IER
      INTEGER*4  I, J, LOC_TO_BAND, LOCS
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2, DP_VV_V, EBSPL_VAL_R8 
      INTEGER*4, EXTERNAL :: I_LEN, IXMN8
      LOC_TO_BAND(I,J,DEG) = I-J+DEG + 1 + DEG + (3*DEG+1)*(J-1)
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      IF ( DEG .NE. 3 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( DEG, STR )
           CALL ERR_LOG ( 1511, IUER, 'EBSPL_LSQ_CNS3', 'The routine was '//   &
     &         'called for computation of expansion ionto B-spline basis '//   &
     &         'of degree '//TRIM(STR)//' while obnly DEG=3 is supported. '// &
     &         'Please use EBSPL_LSQ_CNS' )
           RETURN 
      END IF
!
      LPAR = MS + DEG - 1
      LPA2 = (LPAR*(LPAR+1))/2 
!
! --- Allocate dynamic memory for temporary arrays
!
      ALLOCATE ( EQU_VEC(LPAR),        &
     &           NOR_MAT(LPA2),        &
     &           NOR_VEC(LPAR,MD),     &
     &           BMAT((3*DEG+1)*LPAR), &
     &           IPIV(LPAR),           &
     &           FUND(MD),             &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LPAR, STR )
           CALL ERR_LOG ( 1512, IUER, 'EBSPL_LSQ_CNS3', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'temporary arrays' )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT_R8 ( MD*LPAR, NOR_VEC     )
      CALL NOUT_R8 ( LPAR, EQU_VEC        )
      CALL NOUT_R8 ( LPA2, NOR_MAT        )
      CALL NOUT_R8 ( (3*DEG+1)*LPAR, BMAT )
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
         IF ( PIV_IND < 1 ) GOTO 410
         DO 420 J2=PIV_IND-DEG,PIV_IND
            EQU_VEC(J2+DEG-PIV_IND+1) = BSPL_VAL ( MS, ARG_NOD, DEG, J2, ARG_VEC(J1) )
 420     CONTINUE 
!
! ------ Compute its contribution to NOR_MAT ( normal matrix ) 
!
         DO 430 J3=1,MD
            FUND(J3) = FUN(J1,J3)
 430     CONTINUE 
         CALL DIAD_BAND_NEQ_3VEC ( 1.0D0, LPAR, MD, PIV_IND, EQU_VEC, FUND, &
     &                             BMAT, NOR_VEC )
!
! ------ Constraints
!
         IF ( CNS_VAL_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 440 J4=PIV_IND-DEG,PIV_IND
                 EQU_VEC(J4+DEG-PIV_IND+1) = BSPL_VAL ( MS, ARG_NOD, DEG, J4, ARG_VEC(J1) )
 440          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_BAND_NEQ_3ZERO ( 1.0D0/CNS_VAL_SIG**2, LPAR, PIV_IND, EQU_VEC, BMAT )
         END IF
!
         IF ( CNS_DER_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 450 J5=PIV_IND-DEG,PIV_IND
                 EQU_VEC(J5+DEG-PIV_IND+1) = BSPL_DER ( MS, ARG_NOD, DEG, J5, ARG_VEC(J1) )
 450          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_BAND_NEQ_3ZERO ( 1.0D0/CNS_DER_SIG**2, LPAR, PIV_IND, EQU_VEC, BMAT )
         END IF
         IF ( CNS_DR2_SIG > 0.0D0 ) THEN
              CALL NOUT_R8 ( LPAR, EQU_VEC )
!
! ----------- Compute observation equation
!
              DO 460 J6=PIV_IND-DEG,PIV_IND
                 EQU_VEC(J6+DEG-PIV_IND+1) = BSPL_DR2 ( MS, ARG_NOD, DEG, J6, ARG_VEC(J1)  )
 460          CONTINUE 
!
! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!
              CALL DIAD_BAND_NEQ_3ZERO ( 1.0D0/CNS_DR2_SIG**2, LPAR, PIV_IND, EQU_VEC, BMAT )
         END IF
 410  CONTINUE 
!!   call matview_1 ( 10, lpar, bmat ) ! %%%%%%%%%%
!
! --- Decompose the band matrix
!
      CALL DGBTRF ( LPAR, LPAR, DEG, DEG, BMAT, 3*DEG+1, IPIV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1513, IUER, 'EBSPL_LSQ_CNS3', 'Error in DGBTRF '// &
     &          'INFO= '//STR )
           RETURN 
      END IF
!
! --- Solve the decomposed linear equations 
!
      CALL DGBTRS ( 'T', LPAR, DEG, DEG, MD, BMAT, 3*DEG+1, IPIV, NOR_VEC, LPAR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1514, IUER, 'EBSPL_LSQ_CNS3', 'Error in '// &
     &          'DGBTRS INFO= '//STR )
           RETURN 
      END IF
      SPL_VEC = NOR_VEC
!
      DEALLOCATE ( FUND, IPIV, BMAT, NOR_VEC, NOR_MAT, EQU_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EBSPL_LSQ_CNS3_VEC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAD_BAND_NEQ_3VEC ( CONST, N, M, ICOL, EQU, FUN, &
     &                                NOR_MAT, NOR_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine DIAD_BAND_NEQ_3                                            *
! *                                                                      *
! *   o    o    o    o    o    o                                         *
! *   o    o    o    o    o    o                                         *
! *   o    o    o    o    o    o                                         *
! *   *    *    *    a14  a25  a36                                       *
! *   *    *    a13  a24  a35  a46                                       *
! *   *    a12  a23  a34  a45  a56                                       *
! *   A11  A22  A33  A44  A55  A66                                       *
! *   a21  a32  a43  a54  a65  a76                                       *
! *   a31  a42  a53  a64  a75  a86                                       *
! *   a41  a52  a63  a74  a85  a96                                       *
! *                                                                      *
! * ## 19-AUG-2016 DIAD_BAND_NEQ_3VEC v1.0 (c) L. Petrov 19-AUG-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, M, ICOL
      REAL*8     CONST, EQU(N), FUN(M), NOR_MAT(10,N), NOR_VEC(N,M)
      INTEGER*4  J1
!
      NOR_MAT(7,ICOL)   = NOR_MAT(7,ICOL  ) + CONST*EQU(1)*EQU(1)
!
      NOR_MAT(8,ICOL)   = NOR_MAT(8,ICOL  ) + CONST*EQU(1)*EQU(2)
      NOR_MAT(6,ICOL+1) = NOR_MAT(6,ICOL+1) + CONST*EQU(1)*EQU(2)
      NOR_MAT(7,ICOL+1) = NOR_MAT(7,ICOL+1) + CONST*EQU(2)*EQU(2)
!
      NOR_MAT(9,ICOL)   = NOR_MAT(9,ICOL  ) + CONST*EQU(1)*EQU(3)
      NOR_MAT(5,ICOL+2) = NOR_MAT(5,ICOL+2) + CONST*EQU(1)*EQU(3)
      NOR_MAT(8,ICOL+1) = NOR_MAT(8,ICOL+1) + CONST*EQU(2)*EQU(3)
      NOR_MAT(6,ICOL+2) = NOR_MAT(6,ICOL+2) + CONST*EQU(2)*EQU(3)
      NOR_MAT(7,ICOL+2) = NOR_MAT(7,ICOL+2) + CONST*EQU(3)*EQU(3)
!
      NOR_MAT(10,ICOL)  = NOR_MAT(10,ICOL)  + CONST*EQU(1)*EQU(4)
      NOR_MAT(4,ICOL+3) = NOR_MAT(4,ICOL+3) + CONST*EQU(1)*EQU(4)
      NOR_MAT(9,ICOL+1) = NOR_MAT(9,ICOL+1) + CONST*EQU(2)*EQU(4)
      NOR_MAT(5,ICOL+3) = NOR_MAT(5,ICOL+3) + CONST*EQU(2)*EQU(4)
      NOR_MAT(8,ICOL+2) = NOR_MAT(8,ICOL+2) + CONST*EQU(3)*EQU(4)
      NOR_MAT(6,ICOL+3) = NOR_MAT(6,ICOL+3) + CONST*EQU(3)*EQU(4)
      NOR_MAT(7,ICOL+3) = NOR_MAT(7,ICOL+3) + CONST*EQU(4)*EQU(4)
!
      NOR_VEC(ICOL,1:M)   = NOR_VEC(ICOL,1:M)   + EQU(1)*FUN(1:M)
      NOR_VEC(ICOL+1,1:M) = NOR_VEC(ICOL+1,1:M) + EQU(2)*FUN(1:M)
      NOR_VEC(ICOL+2,1:M) = NOR_VEC(ICOL+2,1:M) + EQU(3)*FUN(1:M)
      NOR_VEC(ICOL+3,1:M) = NOR_VEC(ICOL+3,1:M) + EQU(4)*FUN(1:M)
!
      RETURN
      END SUBROUTINE  DIAD_BAND_NEQ_3VEC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAD_BAND_NEQ_3ZERO ( CONST, N, ICOL, EQU, NOR_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine DIAD_BAND_NEQ_3                                            *
! *                                                                      *
! *   o    o    o    o    o    o                                         *
! *   o    o    o    o    o    o                                         *
! *   o    o    o    o    o    o                                         *
! *   *    *    *    a14  a25  a36                                       *
! *   *    *    a13  a24  a35  a46                                       *
! *   *    a12  a23  a34  a45  a56                                       *
! *   A11  A22  A33  A44  A55  A66                                       *
! *   a21  a32  a43  a54  a65  a76                                       *
! *   a31  a42  a53  a64  a75  a86                                       *
! *   a41  a52  a63  a74  a85  a96                                       *
! *                                                                      *
! * ## 19-AUG-2016 DIAD_BAND_NEQ_3ZERO v1.0 (c) L. Petrov 19-AUG-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, ICOL
      REAL*8     CONST, EQU(N), NOR_MAT(10,N)
      INTEGER*4  J1
!
      NOR_MAT(7,ICOL)   = NOR_MAT(7,ICOL  ) + CONST*EQU(1)*EQU(1)
!
      NOR_MAT(8,ICOL)   = NOR_MAT(8,ICOL  ) + CONST*EQU(1)*EQU(2)
      NOR_MAT(6,ICOL+1) = NOR_MAT(6,ICOL+1) + CONST*EQU(1)*EQU(2)
      NOR_MAT(7,ICOL+1) = NOR_MAT(7,ICOL+1) + CONST*EQU(2)*EQU(2)
!
      NOR_MAT(9,ICOL)   = NOR_MAT(9,ICOL  ) + CONST*EQU(1)*EQU(3)
      NOR_MAT(5,ICOL+2) = NOR_MAT(5,ICOL+2) + CONST*EQU(1)*EQU(3)
      NOR_MAT(8,ICOL+1) = NOR_MAT(8,ICOL+1) + CONST*EQU(2)*EQU(3)
      NOR_MAT(6,ICOL+2) = NOR_MAT(6,ICOL+2) + CONST*EQU(2)*EQU(3)
      NOR_MAT(7,ICOL+2) = NOR_MAT(7,ICOL+2) + CONST*EQU(3)*EQU(3)
!
      NOR_MAT(10,ICOL)  = NOR_MAT(10,ICOL)  + CONST*EQU(1)*EQU(4)
      NOR_MAT(4,ICOL+3) = NOR_MAT(4,ICOL+3) + CONST*EQU(1)*EQU(4)
      NOR_MAT(9,ICOL+1) = NOR_MAT(9,ICOL+1) + CONST*EQU(2)*EQU(4)
      NOR_MAT(5,ICOL+3) = NOR_MAT(5,ICOL+3) + CONST*EQU(2)*EQU(4)
      NOR_MAT(8,ICOL+2) = NOR_MAT(8,ICOL+2) + CONST*EQU(3)*EQU(4)
      NOR_MAT(6,ICOL+3) = NOR_MAT(6,ICOL+3) + CONST*EQU(3)*EQU(4)
      NOR_MAT(7,ICOL+3) = NOR_MAT(7,ICOL+3) + CONST*EQU(4)*EQU(4)
!
      RETURN
      END SUBROUTINE  DIAD_BAND_NEQ_3ZERO  !#!#
