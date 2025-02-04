      SUBROUTINE DIAGI_MUL ( M1, M2, N1_TAB, N2, MES, T, X, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_MUL  makes a plot of a set of function using DiaGI  *
! *   plotting procedure. Temporary copies of the functions are sorted   *
! *   in increasing of the argument.                                     *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_ILST, DIAGI_IOST, DIAGI_IPST,       *
! *   DIAGI_IWST, DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default     *
! *   values if these environment variables have not been set up.        *
! *   Bounding box is chosen as the minimal box to include all points.   *
! *                                                                      *
! * ________________________ Input parameters:  ________________________ *
! *                                                                      *
! *     M1 ( INTEGER*4 ) -- maximal number of values of a function to be *
! *                         plotted;                                     *
! *     M2 ( INTEGER*4 ) -- maximal number of finctions to be plotted    *
! *                         (should be no gtreater than 32).             *
! * N1_TAB ( INTEGER*4 ) -- Arrays dimension M1. N1_TAB(k) is the number *
! *                         of points of the k-th function.              *
! *     N2 ( INTEGER*4 ) -- actuall number of functions to be plotted.   *
! *    MES ( CHARACTER ) -- title message to be put at the header. If    *
! *                         MES consist of only blanks than DiaGI        *
! *                         default title will be used.                  *
! *      T ( REAL*8    ) -- two-dimensional array of argumants.          *
! *                         Dimension of the array is (M1,M2). K-th      *
! *                         column of the matrix T contains M1 values of *
! *                         the argument, N1_TAN(k) being actually used. *
! *      X ( REAL*8    ) -- two-dimensional array of values.             *
! *                         Dimension of the array is (M1,M2). K-th      *
! *                         column of the matrix X contains M1 values of *
! *                         the functions, N1_TAN(k) being actually used.*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  16-SEP-98    DIAGI_MUL   v1.4  (c) L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M1, M2, N1_TAB(M2), N2, IUER
      CHARACTER  MES*(*), ZAG*128, UNIT*128, STR*32, STR1*32
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER, J1
      INTEGER*4  DIAGI_LEN
      REAL*8     T(M1,M2), X(M1,M2)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Clear DIAGI_S object
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4161, IUER, 'DIAGI_MUL', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
      IF ( N2 .GT. NPTS_LARGE ) THEN
           CALL CLRCH  ( STR )
           CALL INCH   ( N2, STR )
           CALL CLRCH  ( STR1 )
           CALL INCH   ( NPTS_LARGE, STR1 )
           CALL ERR_LOG ( 4162, IUER, 'DIAGI_MUL', 'Parameter N2 is too '// &
     &         'large: N2='//STR(1:I_LEN(STR))//' max nyumber of functions '// &
     &         'for DiaGI is '//STR1 )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV = IDEV
      DIAGI_S%NCLR = N2
      DO 410 J1=1,N2
         DIAGI_S%NPOI(J1)   = N1_TAB(J1)
         DIAGI_S%ADR_X8(J1) = LOC(T(1,J1))
         DIAGI_S%ADR_Y8(J1) = LOC(X(1,J1))
         DIAGI_S%ADR_E8(J1) = 0
         DIAGI_S%LER(J1)    = .FALSE.
         DIAGI_S%ICOL(J1)   = J1
         DIAGI_S%IBST(J1)   = 0
         DIAGI_S%ILST(J1)   = ILST
         DIAGI_S%IOST(J1)   = IOST
         DIAGI_S%IPST(J1)   = IPST
         DIAGI_S%IWST(J1)   = IWST
!
         CALL SORT8 ( N1_TAB(J1), %VAL(DIAGI_S%ADR_X8(J1)), &
     &                            %VAL(DIAGI_S%ADR_Y8(J1))  )
 410  CONTINUE
!
      DIAGI_S%ICLR          = 1
      DIAGI_S%XMIN          = 1.0
      DIAGI_S%XMAX          = 0.0
      DIAGI_S%YMIN          = 1.0
      DIAGI_S%YMAX          = 0.0
      DIAGI_S%IBATCH        = 0
      DIAGI_S%ITRM          = 0
!
      IF ( ILEN(MES) .GT. 0 ) THEN
           DIAGI_S%ZAG      = MES
         ELSE
           DIAGI_S%ZAG      = ZAG
      END IF
      DIAGI_S%NAME          = NAME__DEF
      DIAGI_S%ARG_UNITS     = UNIT
!
      DIAGI_S%STATUS        = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_MUL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_EMUL ( M1, M2, N1_TAB, N2, MES, T, X, E, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_EMUL  makes a plot of a set of function using DiaGI *
! *   plotting procedure. Temporary copies of the functions are sorted   *
! *   in increasing of the argument. Error bar for all functions is      *
! *   displayed.                                                         *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_ILST, DIAGI_IOST, DIAGI_IPST,       *
! *   DIAGI_IWST, DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default     *
! *   values if these environment variables have not been set up.        *
! *   Bounding box is chosen as the minimal box to include all points.   *
! *                                                                      *
! * ________________________ Input parameters:  ________________________ *
! *                                                                      *
! *     M1 ( INTEGER*4 ) -- maximal number of values of a function to be *
! *                         plotted;                                     *
! *     M2 ( INTEGER*4 ) -- maximal number of finctions to be plotted    *
! *                         (should be no gtreater than 32).             *
! * N1_TAB ( INTEGER*4 ) -- Arrays dimension M1. N1_TAB(k) is the number *
! *                         of points of the k-th function.              *
! *     N2 ( INTEGER*4 ) -- actuall number of functions to be plotted.   *
! *    MES ( CHARACTER ) -- title message to be put at the header. If    *
! *                         MES consist of only blanks than DiaGI        *
! *                         default title will be used.                  *
! *      T ( REAL*8    ) -- two-dimensional array of argumants.          *
! *                         Dimension of the array is (M1,M2). K-th      *
! *                         column of the matrix T contains M1 values of *
! *                         the argument, N1_TAN(k) being actually used. *
! *      X ( REAL*8    ) -- two-dimensional array of values.             *
! *                         Dimension of the array is (M1,M2). K-th      *
! *                         column of the matrix X contains M1 values of *
! *                         the functions, N1_TAN(k) being actually used.*
! *      E ( REAL*8    ) -- two-dimensional array of formal uncertainties*
! *                         Dimension of the array is (M1,M2). K-th      *
! *                         column of the matrix X contains M1 values of *
! *                         the functions, N1_TAN(k) being actually used.*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  16-SEP-98   DIAGI_EMUL   v1.2  (c)  L. Petrov 08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M1, M2, N1_TAB(M2), N2, IUER
      CHARACTER  MES*(*), ZAG*128, STR*32, STR1*32
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER, J1
      REAL*8     T(M1,M2), X(M1,M2), E(M1,M2)
      INTEGER*4  DIAGI_LEN
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Clear DIAGI_S object
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4171, IUER, 'DIAGI_EMUL', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
      IF ( N2 .GT. NPTS_LARGE ) THEN
           CALL CLRCH  ( STR )
           CALL INCH   ( N2, STR )
           CALL CLRCH  ( STR1 )
           CALL INCH   ( NPTS_LARGE, STR1 )
           CALL ERR_LOG ( 4172, IUER, 'DIAGI_EMUL', 'Parameter N2 is too '// &
     &         'large: N2='//STR(1:I_LEN(STR))//' max nyumber of functions '// &
     &         'for DiaGI is '//STR1 )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV = IDEV
      DIAGI_S%NCLR = N2
      DO 410 J1=1,N2
         DIAGI_S%NPOI(J1)   = N1_TAB(J1)
         DIAGI_S%ADR_X8(J1) = LOC(T(1,J1))
         DIAGI_S%ADR_Y8(J1) = LOC(X(1,J1))
         DIAGI_S%ADR_E8(J1) = LOC(E(1,J1))
         DIAGI_S%LER(J1)    = .TRUE.
         DIAGI_S%ICOL(J1)   = J1
         DIAGI_S%IBST(J1)   = IBST
         DIAGI_S%ILST(J1)   = ILST
         DIAGI_S%IOST(J1)   = IOST
         DIAGI_S%IPST(J1)   = IPST
         DIAGI_S%IWST(J1)   = IWST
!
         CALL SORT83 ( N1_TAB(J1), %VAL(DIAGI_S%ADR_X8(J1)), &
     &                             %VAL(DIAGI_S%ADR_Y8(J1)), &
     &                             %VAL(DIAGI_S%ADR_E8(J1))  )
 410  CONTINUE
!
      DIAGI_S%ICLR          = 1
      DIAGI_S%XMIN          = 1.0
      DIAGI_S%XMAX          = 0.0
      DIAGI_S%YMIN          = 1.0
      DIAGI_S%YMAX          = 0.0
      DIAGI_S%IBATCH        = 0
      DIAGI_S%ITRM          = 0
!
      IF ( ILEN(MES) .GT. 0 ) THEN
           DIAGI_S%ZAG      = MES
         ELSE
           DIAGI_S%ZAG      = ZAG
      END IF
      DIAGI_S%NAME          = NAME__DEF
      DIAGI_S%ARG_UNITS     = ARG_UNITS__DEF
!
      DIAGI_S%STATUS        = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_EMUL  #!#
