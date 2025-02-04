      PROGRAM    HEB_2D_STA
! ************************************************************************
! *                                                                      *
! *   Program HEB_2D_STA
! *                                                                      *
! *  ### 11-MAR-2014   HEB_2D_STA  v1.2 (c)  L. Petrov  01-MAY-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB
      CHARACTER  FILHEB*128, STR*128, LAT_STR*12, LON_STR*12, DATE_STR*32
      REAL*8     COO_HLP(3), COO(3), PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC
      REAL*4     LON_ARR_R4(MALO__MDIM), LAT_ARR_R4(MALO__MDIM), &
     &           VAL, ARGS(2)
      REAL*4,    ALLOCATABLE :: VAL_ARR_R4(:,:)  
      INTEGER*4  MODE, L_LON, L_LAT, J1, J2, J3, INDS(2), IUER
      INTEGER*4, EXTERNAL :: IXMN4, ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*4,    EXTERNAL :: VAL_2D_BSPL4 
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: heb_2d_sta mode coo1 coo2 coo3 heb_file'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, STR      )
           CALL CHIN   ( STR, MODE   )
           CALL GETARG ( 2, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) COO(1)
           CALL GETARG ( 3, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           IF ( INDEX ( STR, '_' ) < 1 ) THEN
                READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) COO(2)
              ELSE
                IUER = -1
                CALL GR_TAT ( STR, COO(2), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6621, IUER, 'HEB_2D_STA', 'Error in parsing  '// &
     &                   ' the third agrument' )
                     CALL EXIT ( 1 )
                END IF
                COO(2) = COO(2)/DEG__TO__RAD
           END IF
           CALL GETARG ( 4, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           IF ( INDEX ( STR, '_' ) < 1 ) THEN
                READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) COO(3)
              ELSE 
                CALL GR_TAT ( STR, COO(3), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6622, IUER, 'HEB_2D_STA', 'Error in parsing  '// &
     &                   ' the third agrument' )
                     CALL EXIT ( 1 )
                END IF
                COO(3) = COO(3)/DEG__TO__RAD
           END IF 
           CALL GETARG ( 5, FILHEB )
      END IF
!
      IUER = 0
      IF ( MODE == 1 ) THEN             ! XYZ
           CALL REF_ELL ( 1, COO, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, &
     &                    RD, G_ACC )
           IUER = -1
           COO_HLP(1) = H_ELL
           COO_HLP(2) = LAMBDA
           COO_HLP(3) = PHI_GDT
         ELSE IF ( MODE == 2 ) THEN
           COO_HLP(1) = COO(1)      ! H_geoid, L,P
           COO_HLP(2) = COO(2)*DEG__TO__RAD
           COO_HLP(3) = COO(3)*DEG__TO__RAD
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6623, IUER, 'HEB_2D_STA', 'Unsupported argument '// &
     &         'mode: '//STR(1:I_LEN(STR))//' -- only 1 or 2 '// &
     &         'are supported' )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL READ_HEB ( FILHEB, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN 
           CALL ERR_LOG ( 6624, IUER, 'HEB_2D_STA', 'Failure in '// &
     &         'an attempt to read input HEB file '//FILHEB )
           CALL EXIT ( 1 )
      END IF
!
      L_LON = HEB%DIMS(1) + 1
      L_LAT = HEB%DIMS(2)
      ALLOCATE ( VAL_ARR_R4(1-MALO__MDEG:L_LON,1-MALO__MDEG:L_LAT), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN 
           CALL CLRCH ( STR )
           CALL IINCH ( (L_LON+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 6625, IUER, 'HEB_2D_STA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'for temporary array ARR_R4' )
           CALL EXIT ( 1 )
      END IF
      VAL_ARR_R4 = 0.0
!
      DO 410 J1=1,L_LON
         LON_ARR_R4(J1) = (J1-1)*PI2/HEB%DIMS(1)
 410  CONTINUE 
!
      DO 420 J2=1,L_LAT
         LAT_ARR_R4(J2) = -P2I + (J2-1)*PI__NUM/(HEB%DIMS(2)-1)
         DO 430 J3=1,HEB%DIMS(1)
            VAL_ARR_R4(J3,J2) = HEB%VAL(J3,J2,1,1)
 430     CONTINUE 
         VAL_ARR_R4(L_LON,J2) = HEB%VAL(1,J2,1,1)
 420  CONTINUE 
!
!! call wall_timer ( %val(0) ) ! %%%
      IUER = -1
      CALL BSPL4_2D_CMPA ( MALO__MDEG, 0, L_LON, L_LAT, LON_ARR_R4, LAT_ARR_R4, &
     &                    VAL_ARR_R4, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6626, IUER, 'HEB_2D_STA', 'Failure in an attempt '// &
     &         'to compute coefficients of the 2D interpolating spline' )
           CALL EXIT ( 1 )
      END IF
!! call wall_timer ( %val(1) ) ! %%%
!
      ARGS(1) = COO_HLP(2)
      ARGS(2) = COO_HLP(3)
      INDS(1) = IXMN4 ( L_LON, LON_ARR_R4, ARGS(1) )
      IF ( ARGS(1) .LE. LON_ARR_R4(1)       ) INDS(1) = 1
      IF ( ARGS(1) .GE. LON_ARR_R4(L_LON) ) INDS(1) = L_LON
!
      INDS(2) = IXMN4 ( L_LAT, LAT_ARR_R4, ARGS(2) )
      IF ( ARGS(2) .LE. LAT_ARR_R4(1)     ) INDS(2) = 1
      IF ( ARGS(2) .GE. LAT_ARR_R4(L_LAT) ) INDS(2) = L_LAT-1
!
      VAL = VAL_2D_BSPL4 ( ARGS(1), ARGS(2), L_LON, L_LAT, MALO__MDEG,  &
     &                     INDS(1), INDS(2), LON_ARR_R4, LAT_ARR_R4, &
     &                     VAL_ARR_R4 )
      IF ( COO_HLP(2) > 0.0D0 ) THEN
           CALL RG_TAT ( COO_HLP(2), 1, LON_STR, IUER )
         ELSE 
           CALL RG_TAT ( COO_HLP(2)-PI2, 1, LON_STR, IUER )
      END IF
      CALL RG_TAT ( COO_HLP(3), 1, LAT_STR, IUER )
      IUER = -1
      DATE_STR = MJDSEC_TO_DATE ( HEB%MJD, HEB%UTC, IUER )
      IF ( INDEX ( HEB%SDS_NAME, 'temperature' ) > 0 ) THEN
           WRITE ( 6, 110 ) DATE_STR(1:19), LON_STR(1:9), LAT_STR(1:9), VAL, &
     &                 HEB%SDS_NAME(1:I_LEN(HEB%SDS_NAME))
 110       FORMAT ( A, 2X, 'Long: ', A, ' Lat_geod: ', A, ' Value: ', F5.1, 2X, A )
         ELSE
           WRITE ( 6, 120 ) DATE_STR(1:19), LON_STR(1:9), LAT_STR(1:9), VAL, &
     &                 HEB%SDS_NAME(1:I_LEN(HEB%SDS_NAME))
 120       FORMAT ( A, 2X, 'Long: ', A, ' Lat_geod: ', A, ' Value: ', F6.4, 2X, A )
      END IF
!
      END  PROGRAM  HEB_2D_STA  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BSPL4_2D_CMPA ( DEG, BC_CODE, M1, M2, &
     &                          ARG_ARR_1, ARG_ARR_2, BCF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BSPL4_2D_CMP  computes coefficients of the 2-dimensional  *
! *   interpolation B-spline spline for a 2 dimensional function. The    *
! *   interpolated function is given at nodes of a regular mesh that     *
! *   forms the rectangular area. Boundary conditions on the first       *
! *   derivatives of the 2D spline at the border of the area are applied *
! *   in accordance with parameter BC_CODE.                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! * BC_CODE ( INTEGER*4 ) -- Code of boundary conditions.                *
! *                          The code of boundary conditions consists of *
! *                          two decimal digits: the first digit counted *
! *                          from right to left, defines the boundary    *
! *                          condition for the first dimension, the      *
! *                          second digits defines the condition for the *
! *                          second dimension.                           *
! *                       Meaning of digits:                             *
! *                       0 -- for the left side: the first derivative   *
! *                            is equal to the first difference;         *
! *                            for the right side: the first derivative  *
! *                            is equal to the first difference,         *
! *                       1 -- for both sides the first derivative is    *
! *                            equal to the mean value of first          *
! *                            difference computed for the left and      *
! *                            right sides.                              *
! *                       2 -- for the left side the first derivative    *
! *                            is zero; for the right side: the first    *
! *                            derivative  is equal to the first         *
! *                            difference.                               *
! *                       3 -- for the left side: the first derivative   *
! *                            is equal to the first difference;         *
! *                            for the right side: zero.                 *
! *                       For the most cases code of boundary conditions *
! *                       0 is adequate.                                 *
! *        M1 ( INTEGER*4 ) -- First dimension.                          *
! *        M2 ( INTEGER*4 ) -- First dimension.                          *
! * ARG_ARR_1 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(1).            *
! * ARG_ARR_2 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: DIMS(2).            *
! *                            dimension. Dimension: DIMS(4).            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       BCF ( REAL*4    ) -- On input: array of values of function.    *
! *                            On output: array of B-spline coefficients.*
! *                            Dimension: (1-DEG:M1,1-DEG:M2).           *
! *                            NB: 1) elements with dimensions < 1 are   *
! *                                   not defined on input.              *
! *                                2) Elements with indexes              *
! *                                   at 1st dimension equal to M1 and/or*
! *                                   at 2nd dimension equal to M2       *
! *                                   are not defined at output.         *
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
! *  ### 04-FEB-2008  BSPL4_2D_CMP  v2.0 (c) L. Petrov  19-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, BC_CODE, M1, M2, IUER
      REAL*4     ARG_ARR_1(M1), ARG_ARR_2(M2)
      REAL*4     BCF(1-DEG:M1,1-DEG:M2)
      INTEGER*4  IER
!
! --- Process dimensions one by one, starting from 1.
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_2D_DIM_CMPA ( 1, DEG, BC_CODE, M1, ARG_ARR_1, &
     &                        M1, M2, BCF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6631, IUER, 'BSPL4_2D_CMP', 'Failure in computing '// &
     &         'coefficients of B-spline expansion over the 1st dimension' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_2D_DIM_CMPA ( 2, DEG, BC_CODE, M2, ARG_ARR_2, &
     &                        M1, M2, BCF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6632, IUER, 'BSPL4_2D_CMP', 'Failure in computing '// &
     &         'coefficients of B-spline expansion over the 2nd dimension' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE BSPL4_2D_CMPA  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BSPL4_2D_DIM_CMPA ( DIM, DEG, BC_CODE, M, ARG, M1, M2, &
     &                              BCF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BSPL4_2D_DIM_CMP updates computation of coefficients of   *
! *   the 2-dimensional interpolation B-spline spline for a              *
! *   2 dimensional function. This funcion performs update for the       *
! *   dimension DIM. The interpolated fiunction is given at nodes of     *
! *   a regular rectangular area. Boundary conditions on the first       *
! *   derivatives of the 2D spline at the border of the area are applied *
! *   in accordance with parameter BC_CODE.                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DIM ( INTEGER*4 ) -- The dimension being updated, in range       *
! *                          [1, 2].                                     *
! *     DEG ( INTEGER*4 ) -- Degree of the spline.                       *
! * BC_CODE ( INTEGER*4 ) -- Code of boundary conditions.                *
! *                          The code of boundary conditions consists of *
! *                          two decimal digits: the first digit counted *
! *                          from right to left, defines the boundary    *
! *                          condition for the first dimension, the      *
! *                          second digits defines the condition for the *
! *                          second dimension.                           *
! *                       Meaning of digits:                             *
! *                       0 -- for the left side: the first derivative   *
! *                            is equal to the first difference;         *
! *                            for the right side: the first derivative  *
! *                            is equal to the first difference,         *
! *                       1 -- for both sides the first derivative is    *
! *                            equal to the mean value of first          *
! *                            difference computed for the left and      *
! *                            right sides.                              *
! *                       2 -- for the left side the first derivative    *
! *                            is zero; for the right side: the first    *
! *                            derivative  is equal to the first         *
! *                            difference.                               *
! *                       3 -- for the left side: the first derivative   *
! *                            is equal to the first difference;         *
! *                            for the right side: zero.                 *
! *                       For the most cases code of boundary conditions *
! *                       0 is adequate.                                 *
! *       M ( INTEGER*4 ) -- The number of elements over this dimension. *
! *     ARG ( REAL*4    ) -- Array of arguments over the dimension being *
! *                          updated. Dimension: DIM.                    *
! *      M1 ( INTEGER*4 ) -- First dimension of the function being       *
! *                       interpolated.                                  *
! *      M2 ( INTEGER*4 ) -- Second dimension of the function being      *
! *                       interpolated.                                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       BCF ( REAL*4    ) -- Array of B-spline coefficients.           *
! *                            If DIM == 1, then on input: array of      *
! *                            values of the function.                   *
! *                            Dimensions:                               *
! *                            (1-DEG:M1,1-DEG:M2,1-DEG:M3,1-DEG:M4).    *
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
! * ## 04-FEB-2008  BSPL4_2D_DIM_CMP  v2.1 (c) L. Petrov  14-MAR-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DIM, DEG, BC_CODE, M, M1, M2, IUER
      REAL*4     ARG(M), BCF(1-DEG:M1,1-DEG:M2)
      REAL*4     EQU
      REAL*4     EPS
      PARAMETER  ( EPS    = 1.E-6 )
      INTEGER*4  I, J, LOC_TO_BAND
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           BC_CODE_1, BC_CODE_2, NTHR, NTHR_SAVED, IER
      CHARACTER  STR*32
      REAL*4,    EXTERNAL  :: BSPL4_VAL, BSPL4_DER
      INTEGER*4, ALLOCATABLE :: IPIV(:)
      REAL*4,    ALLOCATABLE :: MAT_B4(:), RH(:), BC(:,:)
      LOGICAL*4, EXTERNAL    :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL    :: I_LEN, ILEN, OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
      LOC_TO_BAND(I,J,DEG) = I-J+DEG + (DEG+1)*(DEG+J-1)
!
#ifndef SERIAL
      NTHR_SAVED = OMP_GET_NUM_THREADS()
      IF ( OMP_IN_PARALLEL() ) THEN
!
! -------- Do serial if we are already in the parallel region
!
           NTHR = 1
         ELSE
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) == 0 ) THEN
                NTHR = 1
              ELSE 
                CALL CHIN ( STR, NTHR )
           END IF
           CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
           NTHR = OMP_GET_MAX_THREADS()
      END IF
#endif
      IF ( DEG .NE. 3 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG )
           CALL ERR_LOG ( 1710, IUER, 'BSPL4_2D_DIM_CMP', 'DEG='// &
     &          STR(1:I_LEN(STR))//' is currently not supported' )
           RETURN 
      END IF
!
      BC_CODE_1 = MOD(BC_CODE,10)
      BC_CODE_2 = MOD(BC_CODE/10,10)
!
! --- Allocate dynamic memory for the band matrix of interpolation 
! --- equations
!
      ALLOCATE ( MAT_B4((DEG+1)*(M+DEG-1)), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(DEG+1)*(M+DEG-1), STR)
           CALL ERR_LOG ( 1711, IUER, 'BSPL4_2D_DIM_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
      MAT_B4 = 0.0
!
! --- Allocate dynamic memory for the temporary array 
!
      IF ( DIM == 1 ) THEN
           ALLOCATE ( BC(0:M1+DEG-1,1:M2), STAT=IER ) 
         ELSE IF ( DIM == 2 ) THEN
           ALLOCATE ( BC(0:M2+DEG-1,1-DEG:M1), STAT=IER ) 
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(M+DEG-1)*(M+DEG-1), STR)
           CALL ERR_LOG ( 1712, IUER, 'BSPL4_2D_DIM_CMP', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
! --- Zeroing the temporary array
!
      BC = 0.0
!
! --- Allocate memory for indexes of pivotal elements
!
      ALLOCATE ( IPIV(M+DEG-1), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(M+DEG-1), STR)
           CALL ERR_LOG ( 1713, IUER, 'BSPL4_2D_DIM_CMP', 'Failure to '// &
     &         'allocate bytes of dynamic memory' )
           RETURN 
      END IF
      IPIV = 0
!
! --- Build the matrix of interpolation equations, including two equation for 
! --- boundary conditions
!
      DO 410 J1=1-DEG,M-1
         DO 420 J2=MAX(J1+1,0),MIN(J1+DEG,M+1)
            IF ( J2 == 0 ) THEN
                 EQU = BSPL4_DER ( M, ARG, DEG, J1, ARG(1) ) 
               ELSE IF ( J2 == M+1 ) THEN
                 EQU = BSPL4_DER ( M, ARG, DEG, J1, ARG(M)*(1-EPS) )
               ELSE 
                 IF ( J2 == M ) THEN
                      EQU = BSPL4_VAL ( M, ARG, DEG, J1, ARG(J2)*(1-EPS) )
                   ELSE
                      EQU = BSPL4_VAL ( M, ARG, DEG, J1, ARG(J2) )
                 END IF
            END IF
!
! --------- Put the element of the equation into the band matrix
! --------- MAT_B4(LOC_TO_BAND(J1,J2-DEG+1,DEG)) = EQU
!
            MAT_B4(J1-1 + DEG*(J2+2)) = EQU
 420     CONTINUE 
 410  CONTINUE 
!
! --- Build the right-band sides of interpolating equations. 
! --- They are different for different dimensions
!
      IF ( DIM == 1 ) THEN
           DO 430 J3=1,M2
              DO 440 J4=1,M1
                 BC(J4,J3) = BCF(J4,J3)
 440          CONTINUE 
!
! ----------- Two boundary condition imposed to first derivatives at the 
! ----------- the first point (left side) and the last point (right side)
!
              IF ( BC_CODE_1 == 0 ) THEN
!
! ---------------- At the left side: the first derivative is equal to 
! ---------------- the first difference at the left side
!
! ---------------- At the right side: the first derivative is equal to 
! ---------------- the first difference at the right side
!
                   BC(0,J3)   = (BCF(2,J3) - BCF(1,J3)  )/(ARG(2) - ARG(1))
                   BC(M+1,J3) = (BCF(M,J3) - BCF(M-1,J3))/(ARG(M) - ARG(M-1))
                 ELSE IF ( BC_CODE == 1 ) THEN
!
! ---------------- At both sides: the first derivative is an average of the
! ---------------- first differences at the left side and the right side
!
                   BC(0,J3)   = ( (BCF(2,J3) - BCF(1,J3)  )/(ARG(2) - ARG(1)) + &
    &                             (BCF(M,J3) - BCF(M-1,J3))/(ARG(M) - ARG(M-1))  )/2.0
                   BC(M+1,J3) = BC(0,J3)   
                 ELSE IF ( BC_CODE == 2 ) THEN
!
! ---------------- At the left side: zero
!
! ---------------- At the right side: the first derivative is equal to 
! ---------------- the first difference at the rightside
!
                   BC(0,J3)   = 0.0
                   BC(M+1,J3) = (BCF(M,J3) - BCF(M-1,J3))/(ARG(M) - ARG(M-1))
                 ELSE IF ( BC_CODE == 3 ) THEN
!
! ---------------- At the left side: the first derivative is equal to 
! ---------------- the first difference at the left side
!
! ---------------- At the right side: zero
!
                   BC(0,J3)   = (BCF(2,J3) - BCF(1,J3)  )/(ARG(2) - ARG(1))
                   BC(M+1,J3) = 0.0
              END IF
 430       CONTINUE 
        ELSE IF ( DIM == 2 ) THEN
           DO 450 J5=1-DEG,M1
              DO 460 J6=1,M2
                 BC(J6,J5) = BCF(J5,J6)
 460          CONTINUE 
!
! ----------- Boundary condition imposed to first derivatives at the 
! ----------- the first point (left side) and the last point (right side)
!
              IF ( BC_CODE_2 == 0 ) THEN
!
! ---------------- At the left side: the first derivative is equal to 
! ---------------- the first difference at the left side
!
! ---------------- At the right side: the first derivative is equal to 
! ---------------- the first difference at the right side
!
                   BC(0,J5)   = (BCF(J5,2) - BCF(J5,1)  )/(ARG(2) - ARG(1))
                   BC(M+1,J5) = (BCF(J5,M) - BCF(J5,M-1))/(ARG(M) - ARG(M-1))
                 ELSE IF ( BC_CODE_2 == 1 ) THEN
!
! ---------------- At both sides: the first derivative is an average of the
! ---------------- first differences at the left side and the right side
!
                   BC(0,J5)   = ( (BCF(J5,2) - BCF(J5,1)  )/(ARG(2) - ARG(1)) + &
    &                             (BCF(J5,M) - BCF(J5,M-1))/(ARG(M) - ARG(M-1))  )/2.0
                   BC(M+1,J5) = BC(0,J5)   
                 ELSE IF ( BC_CODE_2 == 2 ) THEN
!
! ---------------- At the left side: zero
!
! ---------------- At the right side: the first derivative is equal to 
! ---------------- the first difference at the rightside
!
                   BC(0,J5)   = 0.0
                   BC(M+1,J5) = (BCF(J5,M) - BCF(J5,M-1))/(ARG(M) - ARG(M-1))
                 ELSE IF ( BC_CODE_2 == 3 ) THEN
!
! ---------------- At the left side: the first derivative is equal to 
! ---------------- the first difference at the left side
!
! ---------------- At the right side: zero
!
                   BC(0,J5)   = (BCF(J5,2) - BCF(J5,1)  )/(ARG(2) - ARG(1))
                   BC(M+1,J5) = 0.0
              END IF
 450       CONTINUE 
      END IF
!
! --- Decompose the band matrix
!
      CALL SGBTRF ( M+DEG-1, M+DEG-1, 1, 1, MAT_B4, DEG+1, IPIV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1714, IUER, 'BSPL4_2D_DIM_CMP', 'Error in SGBTRF '// &
     &          'INFO= '//STR )
           RETURN 
      END IF
!
! --- Solve interploation equations with a 2D array of right hand sides and
! --- then put the updated coefficnets into array BCF. The rules for 
! --- manipulating the elements differs from dimension to dimension
!
      IF ( DIM == 1 ) THEN
#ifdef SERIAL
           CALL SGBTRS ( 'T', M+DEG-1, 1, 1, M2, MAT_B4, DEG+1, IPIV, BC, M1+DEG, IER )
#else
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J7, J8 ), SCHEDULE ( STATIC )
#endif
           DO 470 J7=1,M2
#ifndef SERIAL
              CALL SGBTRS ( 'T', 1, 1, 1, M2, MAT_B4, DEG+1, IPIV, BC(0,J7), M1+DEG, IER )
#endif
              DO 480 J8=1-DEG,M1-1
                 BCF(J8,J7) = BC(J8+DEG-1,J7)
 480          CONTINUE 
 470       CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
         ELSE IF ( DIM == 2 ) THEN
#ifdef SERIAL
           CALL SGBTRS ( 'T', M+DEG-1, 1, 1, M1+DEG, MAT_B4, DEG+1, IPIV, BC, M2+DEG, IER )
#else
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J9, J10 ), SCHEDULE ( STATIC )
#endif
           DO 490 J9=1-DEG,M2
#ifndef SERIAL
              CALL SGBTRS ( 'T', 1, 1, 1, M1+DEG, MAT_B4, DEG+1, IPIV, BC(0,J9), M2+DEG, IER )
#endif
              DO 4100 J10=1-DEG,M1-1
                 BCF(J10,J9) = BC(J9+DEG-1,J10)
 4100          CONTINUE 
 490       CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 1715, IUER, 'BSPL4_2D_DIM_CMP', 'Error in '// &
     &          'SGBTRS INFO= '//STR )
           RETURN 
      END IF
!
! --- Deallocat the matrices
!
      DEALLOCATE ( MAT_B4 )
      DEALLOCATE ( IPIV  )
      DEALLOCATE ( BC    )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPL4_2D_DIM_CMPA  !#!#
