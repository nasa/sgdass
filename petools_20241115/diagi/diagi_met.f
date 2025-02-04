      SUBROUTINE DIAGI_1 ( N, T1, X1, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_1  provides simplified interface to the package   *
! *   of interactive graphic DiaGI (Dialogue Graphic Interface). DiaGI   *
! *   is the routine for interactive one-dimension graphic for           *
! *   X-environment. It allows to to change interactively boundary of    *
! *   the plotting area, point style, line style, line width, allows to  *
! *   make hardcopy in PostScript or GIF format and to send it at the    *
! *   printing device.                                                   *
! *                                                                      *
! *     DIAGI_1 draws at the graphic window the plot of one-dimension    *
! *   function specified by the arrays of the arguments and its values.  *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_IPST, DIAGI_ILST, DIAGI_IWST,       *
! *   DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default values if these *
! *   environment variables have not been set up. Bounding box is chosen *
! *   as the minimal box to include all points.                          *
! *                                                                      *
! *     Initial geometry of the graphic window is specified by           *
! *   X-resource pgxwin.Win.geometry.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N ( INTEGER*4 ) -- The number of points to be plotted.            *
! *   T1 ( REAL*8    ) -- Array of arguments of the function to be       *
! *                       plotted. Dimension: N.                         *
! *   X1 ( REAL*8    ) -- Array of values of the function to be plotted. *
! *                       Dimension: N.                                  *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-OCT-97     DIAGI_1   v1.4  (c)  L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  N, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      INTEGER*4  DIAGI_LEN
      REAL*8     T1(N), X1(N)
      CHARACTER  ZAG*128, UNIT*128
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
           CALL ERR_LOG ( 4151, IUER, 'DIAGI_1', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 1
      DIAGI_S%NPOI(1)   = N
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = IPST
      DIAGI_S%IWST(1)   = IWST
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = UNIT
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_1  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_1E ( N, T1, X1, E1, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_1E  provides simplified interface to the package  *
! *   of interactive graphic DiaGI (Dialogue Graphic Interface). DiaGI   *
! *   is the routine for interactive one-dimension graphic for           *
! *   X-environment. It allows to change interactively boundary of the   *
! *   plotting area, point style, line style, line width, error bar      *
! *   style; allows to make hardcopy in PostScript or GIF format and to  *
! *   send it at the printing device.                                    *
! *                                                                      *
! *     DIAGI_1E  draws at the graphic window the plot of one-dimension  *
! *   function with error bars for each point specified by the arrays of *
! *   the arguments, array of its values and array of the errors.        *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_IPST, DIAGI_ILST, DIAGI_IWST,       *
! *   DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default values if these *
! *   environment variables have not been set up. Bounding box is chosen *
! *   as the minimal box to include all points.                          *
! *                                                                      *
! *     Initial geometry of the graphic window is specified by           *
! *   X-resource pgxwin.Win.geometry.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N ( INTEGER*4 ) -- The number of points to be plotted.            *
! *   T1 ( REAL*8    ) -- Array of arguments of the function to be       *
! *                       plotted. Dimension: N.                         *
! *   X1 ( REAL*8    ) -- Array of values of the function to be plotted. *
! *                       Dimension: N.                                  *
! *   E1 ( REAL*8    ) -- Array of errors of the function to be plotted. *
! *                       Dimension: N.                                  *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-OCT-97    DIAGI_1E   v1.3  (c)  L. Petrov  06-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  N, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      REAL*8     T1(N), X1(N), E1(N)
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128
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
           CALL ERR_LOG ( 4152, IUER, 'DIAGI_1E', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 1
      DIAGI_S%NPOI(1)   = N
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%ADR_E8(1) = LOC(E1)
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = IBST
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = IPST
      DIAGI_S%IWST(1)   = IWST
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = UNIT
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_1E  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_2 ( M1, T1, X1, M2, T2, X2, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_2  provides simplified interface to the package   *
! *   of interactive graphic DiaGI (Dialogue Graphic Interface). DiaGI   *
! *   is the routine for interactive one-dimension graphic for           *
! *   X-environment. It allows to to change interactively boundary of    *
! *   the plotting area, point style, line style, line width, allows to  *
! *   make hardcopy in PostScript or GIF format and to send it at the    *
! *   printing device.                                                   *
! *                                                                      *
! *     DIAGI_2 draws at the graphic window the plot of two              *
! *   one-dimension functions specified by the arrays of their arguments *
! *   and their values. Plots are displayed at the same plotting area by *
! *   different colours. The last function overlaps the previous one.    *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_IPST, DIAGI_ILST, DIAGI_IWST,       *
! *   DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default values if these *
! *   environment variables have not been set up. Bounding box is chosen *
! *   as the minimal box to include all points.                          *
! *                                                                      *
! *     Initial geometry of the graphic window is specified by           *
! *   X-resource pgxwin.Win.geometry.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- The number of points of the first function.    *
! *   T1 ( REAL*8    ) -- Array of arguments of the first function to be *
! *                       plotted. Dimension: M1.                        *
! *   X1 ( REAL*8    ) -- Array of values of the first function to be    *
! *                       plotted. Dimension: M1.                        *
! *   M2 ( INTEGER*4 ) -- The number of points of the second function.   *
! *   T2 ( REAL*8    ) -- Array of arguments of the second function to   *
! *                       be plotted. Dimension: M2.                     *
! *   X2 ( REAL*8    ) -- Array of values of the second function to be   *
! *                       plotted. Dimension: M2.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-OCT-97     DIAGI_2   v1.4  (c)  L. Petrov  06-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M1, M2, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      REAL*8     T1(M1), X1(M1)
      REAL*8     T2(M2), X2(M2)
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128
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
           CALL ERR_LOG ( 4153, IUER, 'DIAGI_2', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 2
      DIAGI_S%NPOI(1)   = M1
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%ADR_E8(1) = 0
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = IPST
      DIAGI_S%IWST(1)   = IWST
!
      DIAGI_S%NPOI(2)   = M2
      DIAGI_S%ADR_X8(2) = LOC(T2)
      DIAGI_S%ADR_Y8(2) = LOC(X2)
      DIAGI_S%ADR_E8(2) = 0
      DIAGI_S%LER(2)    = .FALSE.
      DIAGI_S%ICOL(2)   = ICL2
      DIAGI_S%IBST(2)   = 0
      DIAGI_S%ILST(2)   = ILST
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = IPST
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%ICLR      = 2
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = UNIT
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_2  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_2E ( M1, T1, X1, E1, M2, T2, X2, E2, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_2E  provides simplified interface to the package  *
! *   of interactive graphic DiaGI (Dialogue Graphic Interface). DiaGI   *
! *   is the routine for interactive one-dimension graphic for           *
! *   X-environment. It allows to change interactively boundary of the   *
! *   plotting area, point style, line style, line width, error bar      *
! *   style; allows to make hardcopy in PostScript or GIF format and to  *
! *   send it at the printing device.                                    *
! *                                                                      *
! *     DIAGI_2E draws at the graphic window the plot of two             *
! *   one-dimension functions with error bars for each point specified   *
! *   by the arrays of their arguments their values and their errors.    *
! *   Plots are displayed at the same plotting area by different         *
! *   colours. The last function overlaps the previous one.              *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_IPST, DIAGI_ILST, DIAGI_IWST,       *
! *   DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default values if these *
! *   environment variables have not been set up. Bounding box is chosen *
! *   as the minimal box to include all points.                          *
! *                                                                      *
! *     Initial geometry of the graphic window is specified by           *
! *   X-resource pgxwin.Win.geometry.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- The number of points of the first function.    *
! *   T1 ( REAL*8    ) -- Array of arguments of the first function to be *
! *                       plotted. Dimension: M1.                        *
! *   X1 ( REAL*8    ) -- Array of values of the first function to be    *
! *                       plotted. Dimension: M1.                        *
! *   E1 ( REAL*8    ) -- Array of errors of the first function to be    *
! *                       plotted. Dimension: M1.                        *
! *   M2 ( INTEGER*4 ) -- The number of points of the second function.   *
! *   T2 ( REAL*8    ) -- Array of arguments of the second function to   *
! *                       be plotted. Dimension: M2.                     *
! *   X2 ( REAL*8    ) -- Array of values of the second function to be   *
! *                       plotted. Dimension: M2.                        *
! *   E2 ( REAL*8    ) -- Array of errors of the second function to be   *
! *                       plotted. Dimension: M2.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-OCT-97    DIAGI_2E   v1.4  (c)  L. Petrov  06-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M1, M2, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      REAL*8     T1(M1), X1(M1), E1(M1)
      REAL*8     T2(M2), X2(M2), E2(M2)
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128
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
           CALL ERR_LOG ( 4154, IUER, 'DIAGI_2E', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 2
      DIAGI_S%NPOI(1)   = M1
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%ADR_E8(1) = LOC(E1)
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = IBST
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = IPST
      DIAGI_S%IWST(1)   = IWST
!
      DIAGI_S%NPOI(2)   = M2
      DIAGI_S%ADR_X8(2) = LOC(T2)
      DIAGI_S%ADR_Y8(2) = LOC(X2)
      DIAGI_S%ADR_E8(2) = LOC(E2)
      DIAGI_S%LER(2)    = .TRUE.
      DIAGI_S%ICOL(2)   = ICL2
      DIAGI_S%IBST(2)   = IBST
      DIAGI_S%ILST(2)   = ILST
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = IPST
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%ICLR      = 2
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = UNIT
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_2E  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_3 ( M1, T1, X1, M2, T2, X2, M3, T3, X3, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_3  provides simplified interface to the package   *
! *   of interactive graphic DiaGI (Dialogue Graphic Interface). DiaGI   *
! *   is the routine for interactive one-dimension graphic for           *
! *   X-environment. It allows to to change interactively boundary of    *
! *   the plotting area, point style, line style, line width, allows to  *
! *   make hardcopy in PostScript or GIF format and to send it at the    *
! *   printing device.                                                   *
! *                                                                      *
! *     DIAGI_3 draws at the graphic window the plot of three            *
! *   one-dimension functions specified by the arrays of their arguments *
! *   and their values. Plots are displayed at the same plotting area by *
! *   different colours. The last function overlaps the previous one.    *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_IPST, DIAGI_ILST, DIAGI_IWST,       *
! *   DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default values if these *
! *   environment variables have not been set up. Bounding box is chosen *
! *   as the minimal box to include all points.                          *
! *                                                                      *
! *     Initial geometry of the graphic window is specified by           *
! *   X-resource pgxwin.Win.geometry.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- The number of points of the first function.    *
! *   T1 ( REAL*8    ) -- Array of arguments of the first function to be *
! *                       plotted. Dimension: M1.                        *
! *   X1 ( REAL*8    ) -- Array of values of the first function to be    *
! *                       plotted. Dimension: M1.                        *
! *   M2 ( INTEGER*4 ) -- The number of points of the second function.   *
! *   T2 ( REAL*8    ) -- Array of arguments of the second function to   *
! *                       be plotted. Dimension: M2.                     *
! *   X2 ( REAL*8    ) -- Array of values of the second function to be   *
! *                       plotted. Dimension: M2.                        *
! *   M3 ( INTEGER*4 ) -- The number of points of the third  function.   *
! *   T3 ( REAL*8    ) -- Array of arguments of the third function to    *
! *                       be plotted. Dimension: M3.                     *
! *   X3 ( REAL*8    ) -- Array of values of the third function to be    *
! *                       plotted. Dimension: M3.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-OCT-97     DIAGI_3   v1.3  (c)  L. Petrov  06-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M1, M2, M3, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      REAL*8     T1(M1), X1(M1), T2(M2), X2(M2), T3(M3), X3(M3)
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128
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
           CALL ERR_LOG ( 4155, IUER, 'DIAGI_3', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 3
      DIAGI_S%NPOI(1)   = M1
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%ADR_E8(1) = 0
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = IPST
      DIAGI_S%IWST(1)   = IWST
!
      DIAGI_S%NPOI(2)   = M2
      DIAGI_S%ADR_X8(2) = LOC(T2)
      DIAGI_S%ADR_Y8(2) = LOC(X2)
      DIAGI_S%ADR_E8(2) = 0
      DIAGI_S%LER(2)    = .FALSE.
      DIAGI_S%ICOL(2)   = ICL2
      DIAGI_S%IBST(2)   = 0
      DIAGI_S%ILST(2)   = ILST
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = IPST
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%NPOI(3)   = M3
      DIAGI_S%ADR_X8(3) = LOC(T3)
      DIAGI_S%ADR_Y8(3) = LOC(X3)
      DIAGI_S%ADR_E8(3) = 0
      DIAGI_S%LER(3)    = .FALSE.
      DIAGI_S%ICOL(3)   = ICL3
      DIAGI_S%IBST(3)   = 0
      DIAGI_S%ILST(3)   = ILST
      DIAGI_S%IOST(3)   = IOST
      DIAGI_S%IPST(3)   = IPST
      DIAGI_S%IWST(3)   = IWST
!
      DIAGI_S%ICLR      = 3
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = UNIT
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_3  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_4 ( M1, T1, X1, M2, T2, X2, M3, T3, X3, M4, T4, X4, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_3  provides simplified interface to the package   *
! *   of interactive graphic DiaGI (Dialogue Graphic Interface). DiaGI   *
! *   is the routine for interactive one-dimension graphic for           *
! *   X-environment. It allows to to change interactively boundary of    *
! *   the plotting area, point style, line style, line width, allows to  *
! *   make hardcopy in PostScript or GIF format and to send it at the    *
! *   printing device.                                                   *
! *                                                                      *
! *     DIAGI_3 draws at the graphic window the plot of three            *
! *   one-dimension functions specified by the arrays of their arguments *
! *   and their values. Plots are displayed at the same plotting area by *
! *   different colours. The last function overlaps the previous one.    *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_IPST, DIAGI_ILST, DIAGI_IWST,       *
! *   DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default values if these *
! *   environment variables have not been set up. Bounding box is chosen *
! *   as the minimal box to include all points.                          *
! *                                                                      *
! *     Initial geometry of the graphic window is specified by           *
! *   X-resource pgxwin.Win.geometry.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- The number of points of the first function.    *
! *   T1 ( REAL*8    ) -- Array of arguments of the first function to be *
! *                       plotted. Dimension: M1.                        *
! *   X1 ( REAL*8    ) -- Array of values of the first function to be    *
! *                       plotted. Dimension: M1.                        *
! *   M2 ( INTEGER*4 ) -- The number of points of the second function.   *
! *   T2 ( REAL*8    ) -- Array of arguments of the second function to   *
! *                       be plotted. Dimension: M2.                     *
! *   X2 ( REAL*8    ) -- Array of values of the second function to be   *
! *                       plotted. Dimension: M2.                        *
! *   M3 ( INTEGER*4 ) -- The number of points of the third  function.   *
! *   T3 ( REAL*8    ) -- Array of arguments of the third function to    *
! *                       be plotted. Dimension: M3.                     *
! *   X3 ( REAL*8    ) -- Array of values of the third function to be    *
! *                       plotted. Dimension: M3.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  09-FEB-2021   DIAGI_4   v1.3  (c)  L. Petrov  09-FEB-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M1, M2, M3, M4, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, ICL4, IER
      REAL*8     T1(M1), X1(M1), T2(M2), X2(M2), T3(M3), X3(M3), T4(M4), X4(M4)
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128
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
           CALL ERR_LOG ( 4155, IUER, 'DIAGI_3', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
      ICL4 = 5
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 4
      DIAGI_S%NPOI(1)   = M1
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%ADR_E8(1) = 0
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = IPST
      DIAGI_S%IWST(1)   = IWST
!
      DIAGI_S%NPOI(2)   = M2
      DIAGI_S%ADR_X8(2) = LOC(T2)
      DIAGI_S%ADR_Y8(2) = LOC(X2)
      DIAGI_S%ADR_E8(2) = 0
      DIAGI_S%LER(2)    = .FALSE.
      DIAGI_S%ICOL(2)   = ICL2
      DIAGI_S%IBST(2)   = 0
      DIAGI_S%ILST(2)   = ILST
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = IPST
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%NPOI(3)   = M3
      DIAGI_S%ADR_X8(3) = LOC(T3)
      DIAGI_S%ADR_Y8(3) = LOC(X3)
      DIAGI_S%ADR_E8(3) = 0
      DIAGI_S%LER(3)    = .FALSE.
      DIAGI_S%ICOL(3)   = ICL3
      DIAGI_S%IBST(3)   = 0
      DIAGI_S%ILST(3)   = ILST
      DIAGI_S%IOST(3)   = IOST
      DIAGI_S%IPST(3)   = IPST
      DIAGI_S%IWST(3)   = IWST
!
      DIAGI_S%NPOI(4)   = M4
      DIAGI_S%ADR_X8(4) = LOC(T4)
      DIAGI_S%ADR_Y8(4) = LOC(X4)
      DIAGI_S%ADR_E8(4) = 0
      DIAGI_S%LER(4)    = .FALSE.
      DIAGI_S%ICOL(4)   = ICL4
      DIAGI_S%IBST(4)   = 0
      DIAGI_S%ILST(4)   = ILST
      DIAGI_S%IOST(4)   = IOST
      DIAGI_S%IPST(4)   = IPST
      DIAGI_S%IWST(4)   = IWST
!
      DIAGI_S%ICLR      = 4
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = UNIT
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_3E ( M1, T1, X1, E1, M2, T2, X2, E2, M3, T3, X3, E3, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_3E  provides simplified interface to the package  *
! *   of interactive graphic DiaGI (Dialogue Graphic Interface). DiaGI   *
! *   is the routine for interactive one-dimension graphic for           *
! *   X-environment. It allows to change interactively boundary of the   *
! *   plotting area, point style, line style, line width, error bar      *
! *   style; allows to make hardcopy in PostScript or GIF format and to  *
! *   send it at the printing device.                                    *
! *                                                                      *
! *     DIAGI_3E draws at the graphic window the plot of two             *
! *   one-dimension functions with error bars for each point specified   *
! *   by the arrays of their arguments their values and their errors.    *
! *   Plots are displayed at the same plotting area by different         *
! *   colours. The last function overlaps the previous one.              *
! *                                                                      *
! *     Initial point style, line style, line width, title, colour,      *
! *   boundary box, screen size are specified in accordance with values  *
! *   of environment variables DIAGI_IPST, DIAGI_ILST, DIAGI_IWST,       *
! *   DIAGI_SCREEN, DIAGI_CTIT, DIAGI_ICL1 or by default values if these *
! *   environment variables have not been set up. Bounding box is chosen *
! *   as the minimal box to include all points.                          *
! *                                                                      *
! *     Initial geometry of the graphic window is specified by           *
! *   X-resource pgxwin.Win.geometry.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- The number of points of the first function.    *
! *   T1 ( REAL*8    ) -- Array of arguments of the first function to be *
! *                       plotted. Dimension: M1.                        *
! *   X1 ( REAL*8    ) -- Array of values of the first function to be    *
! *                       plotted. Dimension: M1.                        *
! *   E1 ( REAL*8    ) -- Array of errors of the first function to be    *
! *                       plotted. Dimension: M1.                        *
! *   M2 ( INTEGER*4 ) -- The number of points of the second function.   *
! *   T2 ( REAL*8    ) -- Array of arguments of the second function to   *
! *                       be plotted. Dimension: M2.                     *
! *   X2 ( REAL*8    ) -- Array of values of the second function to be   *
! *                       plotted. Dimension: M2.                        *
! *   E2 ( REAL*8    ) -- Array of errors of the second function to be   *
! *                       plotted. Dimension: M2.                        *
! *   M3 ( INTEGER*4 ) -- The number of points of the third function.    *
! *   T3 ( REAL*8    ) -- Array of arguments of the third function to    *
! *                       be plotted. Dimension: M3.                     *
! *   X3 ( REAL*8    ) -- Array of values of the third function to be    *
! *                       plotted. Dimension: M3.                        *
! *   E3 ( REAL*8    ) -- Array of errors of the third function to be    *
! *                       plotted. Dimension: M3.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-OCT-97    DIAGI_3E   v1.4  (c)  L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M1, M2, M3, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      INTEGER*4  DIAGI_LEN
      REAL*8     T1(M1), X1(M1), E1(M1)
      REAL*8     T2(M2), X2(M2), E2(M2)
      REAL*8     T3(M3), X3(M3), E3(M3)
      CHARACTER  ZAG*128, UNIT*128
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
           CALL ERR_LOG ( 4156, IUER, 'DIAGI_3E', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 3
      DIAGI_S%NPOI(1)   = M1
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%ADR_E8(1) = LOC(E1)
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = IBST
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = IPST
      DIAGI_S%IWST(1)   = IWST
!
      DIAGI_S%NPOI(2)   = M2
      DIAGI_S%ADR_X8(2) = LOC(T2)
      DIAGI_S%ADR_Y8(2) = LOC(X2)
      DIAGI_S%ADR_E8(2) = LOC(E2)
      DIAGI_S%LER(2)    = .TRUE.
      DIAGI_S%ICOL(2)   = ICL2
      DIAGI_S%IBST(2)   = IBST
      DIAGI_S%ILST(2)   = ILST
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = IPST
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%NPOI(3)   = M3
      DIAGI_S%ADR_X8(3) = LOC(T3)
      DIAGI_S%ADR_Y8(3) = LOC(X3)
      DIAGI_S%ADR_E8(3) = LOC(E3)
      DIAGI_S%LER(3)    = .TRUE.
      DIAGI_S%ICOL(3)   = ICL3
      DIAGI_S%IBST(3)   = IBST
      DIAGI_S%ILST(3)   = ILST
      DIAGI_S%IOST(3)   = IOST
      DIAGI_S%IPST(3)   = IPST
      DIAGI_S%IWST(3)   = IWST
!
      DIAGI_S%ICLR      = 3
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = UNIT
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IUER )
!
      RETURN
      END  !#!  DIAGI_3E  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, &
     &                       UNIT, ICL1, ICL2, ICL3, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_DEF  sets default values for the following plotting *
! *   parameters of DiaGI: IBST -- error bar style, ILST -- line style,  *
! *   IPST -- point style, IWST -- line width style, ZAG -- title of the *
! *   plot, ICL1, index of the first colour, ICL2, index of the second   *
! *   colour, ICL3 -- index of the third colour. It firstly look at      *
! *   the corresponding environment variables, in the case of failure    *
! *   uses built-in defaults defined in diagi.i                          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   IBST ( INTEGER*4 ) -- Error bar style. Corresponding environment   *
! *                         variable is DIAGI_IBST. Acceptabel values    *
! *                         are in the range [ 0, MBST-1 ].              *
! *   ILST ( INTEGER*4 ) -- Line style. Corresponding environment        *
! *                         variable is DIAGI_ILST. Acceptabel values    *
! *                         are in the range [ 1, MLST ].                *
! *   IOST ( INTEGER*4 ) -- Overplot style. Corresponding environment    *
! *                         variable is DIAGI_IOST. Acceptabel values    *
! *                         are in the range [ 1, MOST ].                *
! *   IPST ( INTEGER*4 ) -- Point style. Corresponding environment       *
! *                         variable is DIAGI_ILST. Acceptabel values    *
! *                         are in the range [ 1, MPST ].                *
! *   IWST ( INTEGER*4 ) -- Line width code. Corresponding environment   *
! *                         variable is DIAGI_IWST. Acceptabel values    *
! *                         are in the range [ 1, MWST ].                *
! * IDEV ( INTEGER*4 ) - Device type for XS-screen. Two values are    *
! *                         supported:                                   *
! *                         1 -- for a  big screen 1280x1024, 340x270mm; *
! *                         2 -- for a small screen 1024x768, 340x270mm. *
! *    ZAG ( CHARACTER ) -- Title of the plot. Corresponding environment *
! *                         variable is DIAGI_CTIT.                      *
! *   UNIT ( CHARACTER ) -- Name of units to be printed in the right     *
! *                         bottom corner of the plot.                   *
! *                         Correpsonding environment variable is        *
! *                         DIAGI_UNIT.                                  *
! *   ICL1 ( INTEGER*4 ) -- Index of the colour for the first displayed  *
! *                         function in internal colour table            *
! *                         of predefined colours of DiaGI. Corresponding*
! *                         environment variable is DIAGI_ICL1.          *
! *                         Accepatble values are in the range [0, MCLR] *
! *   ICL2 ( INTEGER*4 ) -- Index of the colour for the second displayed *
! *                         function in internal colour table            *
! *                         of predefined colours of DiaGI. Corresponding*
! *                         environment variable is DIAGI_ICL2.          *
! *                         Accepatble values are in the range [0, MCLR] *
! *   ICL3 ( INTEGER*4 ) -- Index of the colour for the third displayed  *
! *                         function in internal colour table            *
! *                         of predefined colours of DiaGI. Corresponding*
! *                         environment variable is DIAGI_ICL3.          *
! *                         Accepatble values are in the range [0, MCLR] *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  21-OCT-97    DIAGI_DEF   v1.9  (c)  L. Petrov 11-JAN-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INCLUDE   'diagi_local.i'
      CHARACTER  DIAGI_IBST_ENV*128, DIAGI_ILST_ENV*128, &
     &           DIAGI_IOST_ENV*128, DIAGI_IPST_ENV*128, &
     &           DIAGI_IWST_ENV*128, DIAGI_CTIT_ENV*128, &
     &           DIAGI_UNIT_ENV*128, DIAGI_ICL1_ENV*128, &
     &           DIAGI_ICL2_ENV*128, DIAGI_ICL3_ENV*128
      COMMON  / DIAGI_ENV / DIAGI_IBST_ENV, DIAGI_ILST_ENV, &
     &                      DIAGI_IOST_ENV, DIAGI_IPST_ENV, &
     &                      DIAGI_IWST_ENV, DIAGI_CTIT_ENV, &
     &                      DIAGI_UNIT_ENV, DIAGI_ICL1_ENV, &
     &                      DIAGI_ICL2_ENV, DIAGI_ICL3_ENV
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IUER
      CHARACTER  ZAG*(*), UNIT*(*)
      CHARACTER  STR*128
      CHARACTER  GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Setting error bar representation style
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_IBST, STR )
      IF ( ILEN(STR) .LE. 0 ) THEN
           IF ( DIAGI_IBST_ENV(1:10) .EQ. DIAGI_IBST ) THEN
                STR = DIAGI_IBST_ENV(12:)
           END IF
      END IF
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, IBST )
           IF ( IBST .LT. 0  .OR.  IBST .GE. MBST ) THEN
                CALL ERR_LOG  ( 4141, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_IBST: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           IBST = IBST__DEF
      END IF
!
! --- Setting line style
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_ILST, STR )
      IF ( ILEN(STR) .LE. 0 ) THEN
           IF ( DIAGI_ILST_ENV(1:10) .EQ. DIAGI_ILST ) THEN
                STR = DIAGI_ILST_ENV(12:)
           END IF
      END IF
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, ILST )
           IF ( ILST .LE. 0  .OR.  ILST .GT. MLST ) THEN
                CALL ERR_LOG  ( 4142, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_ILST: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           ILST = ILST__DEF
      END IF
!
! --- Setting overplot style
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_IOST, STR )
      IF ( ILEN(STR) .LE. 0 ) THEN
           IF ( DIAGI_IOST_ENV(1:10) .EQ. DIAGI_IOST ) THEN
                STR = DIAGI_IOST_ENV(12:)
           END IF
      END IF
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, IOST )
           IF ( IOST .LE. 0  .OR.  IOST .GT. MOST ) THEN
                CALL ERR_LOG  ( 4143, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_IOST: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           IOST = IOST__DEF
      END IF
!
! --- Setting point style
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_IPST, STR )
      IF ( ILEN(STR) .LE. 0 ) THEN
           IF ( DIAGI_IPST_ENV(1:10) .EQ. DIAGI_IPST ) THEN
                STR = DIAGI_IPST_ENV(12:)
           END IF
      END IF
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, IPST )
           IF ( IPST .LE. 0  .OR.  IPST .GT. MPST ) THEN
                CALL ERR_LOG  ( 4143, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_IPST: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           IPST = IPST__DEF
      END IF
!
! --- Setting line width
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_IWST, STR )
      IF ( ILEN(STR) .LE. 0 ) THEN
           IF ( DIAGI_IWST_ENV(1:10) .EQ. DIAGI_IWST ) THEN
                STR = DIAGI_IWST_ENV(12:)
           END IF
      END IF
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, IWST )
           IF ( IWST .LE. 0  .OR.  IWST .GT. MWST ) THEN
                CALL ERR_LOG  ( 4144, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_IWST: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           IWST = IWST__DEF
      END IF
!
! --- Setting screen size
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_SCREEN, STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:4) .EQ. 'TINY' ) THEN
                IDEV = 1
              ELSE IF ( STR(1:5) .EQ. 'SMALL' ) THEN
                IDEV = 2
              ELSE IF ( STR(1:3) .EQ. 'BIG' ) THEN
                IDEV = 3
              ELSE IF ( STR(1:4) .EQ. 'HUGE' ) THEN
                IDEV = 4
              ELSE IF ( STR(1:4) .EQ. 'VAST' ) THEN
                IDEV = 5
              ELSE
                CALL ERR_LOG  ( 4145, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_SCREEN: '// &
     &               STR(1:I_LEN(STR))//'  -- one of TINY, SMALL, BIG, '// &
     &               'HUGE, VAST, was expected' )
                RETURN
           END IF
         ELSE
!
! -------- Default
!
           IDEV = IXS__DEF
      END IF
!
! --- Setting default title
!
      CALL CLRCH  ( ZAG )
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_CTIT, STR )
      IF ( ILEN(STR) .LE. 0 ) THEN
           IF ( DIAGI_CTIT_ENV(1:10) .EQ. DIAGI_CTIT ) THEN
                STR = DIAGI_CTIT_ENV(12:)
           END IF
      END IF
      IF ( ILEN(STR) .GT. 0 ) THEN
           ZAG = STR
         ELSE
!
! -------- Adding date and time to the end of the title
!
           ZAG = ZAG__DEF//'    '//GET_CDATE()
      END IF
!
! --- Setting default units name
!
      CALL CLRCH  ( UNIT )
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_UNIT, STR )
      IF ( ILEN(STR) .LE. 0 ) THEN
           IF ( DIAGI_UNIT_ENV(1:10) .EQ. DIAGI_UNIT ) THEN
                STR = DIAGI_UNIT_ENV(12:)
           END IF
      END IF
      IF ( ILEN(STR) .GT. 0 ) THEN
           UNIT = STR
         ELSE
           CALL CLRCH ( UNIT )
      END IF
!
! --- Setting the index for first colour
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_ICL1, STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, ICL1 )
           IF ( ICL1 .LE. 0  .OR.  ICL1 .GT. MCLR ) THEN
                CALL ERR_LOG  ( 4146, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_ICL1: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           ICL1 = ICL1__DEF
      END IF
!
! --- Setting the index for second colour
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_ICL2, STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, ICL2 )
           IF ( ICL2 .LE. 0  .OR.  ICL2 .GT. MCLR ) THEN
                CALL ERR_LOG  ( 4147, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_ICL2: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           ICL2 = ICL2__DEF
      END IF
!
! --- Setting the index for third colour
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( DIAGI_ICL3, STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, ICL3 )
           IF ( ICL3 .LE. 0  .OR.  ICL3 .GT. MCLR ) THEN
                CALL ERR_LOG  ( 4148, IUER, 'DIAGI_DEF', 'Erroneous '// &
     &              'value of the environment variable DIAGI_ICL3: '// &
     &               STR )
                RETURN
           END IF
         ELSE
           ICL3 = ICL3__DEF
      END IF
!
      CALL ERR_PASS ( 0, IUER )
      RETURN
      END  !#!  DIAGI_DEF  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_SETDEF ( IUER, ARG_NAME, ENV_VALUE, &
     &                          ARG1_HIDDEN, ARG2_HIDDEN )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_SETDEF sets an environment variable for DiaGI.      *
! *   This environment variable will be in effect during execution the   *
! *   program. DIAGI_SETDEF should be called before the first call of    *
! *   DIAGI. DIAGI_SETDEF is useful for changing default plotting        *
! *   parameters wiht using short interface of DIAGI, namely             *
! *   DIAGI_SETDEF can be called before DIAGI_1, DIAGI_1E, DIAGI_2,      *
! *   DIAGI_2E, DIAGI_3, DIAGI_3E, DIAGI_MUL, DIAGI_EMUL. If more than   *
! *   one environment variable is needed to be changed then DIAGI_SETDEF *
! *   can be called several times with different parameters.             *
! *   DIAGI_SETDEF does not affect plotting when long interface to       *
! *   DIAGI is used.                                                     *
! *                                                                      *
! *   Comments:                                                          *
! *     1) ARG1_HIDDEN and ARG2_HIDDEN should *not* be specified         *
! *        implicitly in arguments list.                                 *
! *     2) Values of environment variables changed by DIAGI_SETDEF are   *
! *        not inhereted to the calling shell. DIAGI_SETDEF does not     *
! *        change shell eviroment variables.                             *
! *                                                                      *
! *   Examples:                                                          *
! *                                                                      *
! *           CALL DIAGI_SETDEF ( IUER, 'DIAGI_ILST', 1 )                *
! *           CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'UT1 rate' )       *
! *           CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Megaparsec' )     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  ENV_NAME ( CHARACTER ) -- Name of the environment variable.         *
! *                            One of:                                   *
! *                                                                      *
! *                                     DIAGI_IBST                       *
! *                                     DIAGI_ILST                       *
! *                                     DIAGI_IOST                       *
! *                                     DIAGI_IPST                       *
! *                                     DIAGI_IWST                       *
! *                                     DIAGI_CTIT                       *
! *                                     DIAGI_UNIT                       *
! *                                     DIAGI_ICL1                       *
! *                                     DIAGI_ICL2                       *
! *                                     DIAGI_ICL3                       *
! *                                                                      *
! *  ENV_VALUE ( INTEGER*4/CHARACTER ) -- Value of environement variable.*
! *                                       Can be of integer or character *
! *                                       type depending on ENV_NAME:    *
! *                                       character for DIAGI_CTIT and   *
! *                                       DIAGI_UNIT, integer for all    *
! *                                       others.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 30-MAY-2001  DIAGI_SETDEF v1.3 (c) L. Petrov  19-NOV-2014  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INCLUDE   'diagi_local.i'
      INTEGER*4  IUER, ENV_VALUE
#ifdef INTEL
      INTEGER*2  ARG1_HIDDEN, ARG2_HIDDEN
#else
      ADDRESS__TYPE  ARG1_HIDDEN, ARG2_HIDDEN
#endif
      INTEGER*1  ARG_NAME(*)
      CHARACTER  ZAG*128, UNIT*128, ENV_NAME*10, STR*128
      INTEGER*4  IL, LEN_ARG, LEN_ENV, IBST, ILST, IOST, IPST, IWST, IDEV, &
     &           ICL1, ICL2, ICL3, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LOC__SUN$$_STR
!
      LEN_ENV = LOC(ARG1_HIDDEN)
!
! --- A trick: the second argument was not defined as Character in order
! --- to be able to use the 3rd argument as an argument of any type.
! --- We need carefully copy the contents of the address of the 2nd argument
!
      CALL CLRCH ( ENV_NAME )
#ifdef SUN
      CALL LIB$MOVC3 ( MIN(LEN_ENV,LEN(ENV_NAME)), ARG_NAME, &
     &                 %VAL(LOC__SUN$$_STR(ENV_NAME)) )
#else
      CALL LIB$MOVC3 ( MIN(LEN_ENV,LEN(ENV_NAME)), ARG_NAME, %REF(ENV_NAME) )
#endif
      LEN_ENV = I_LEN( ENV_NAME(1:LEN_ENV) )
!
      IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_IBST ) THEN
           CALL INCH ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_IBST//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_ILST ) THEN
           CALL INCH   ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_ILST//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_IOST ) THEN
           CALL INCH   ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_IOST//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_IPST ) THEN
           CALL INCH   ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_IPST//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_IWST ) THEN
           CALL INCH   ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_IWST//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_CTIT ) THEN
           LEN_ARG = LOC(ARG2_HIDDEN)
           IF ( LEN_ARG .GT. 110 ) LEN_ARG=110
           IF ( LEN_ARG .LT. 1   ) LEN_ARG=1
           CALL CLRCH ( STR )
!
! -------- This strange construction is for circumventing a bug in Intel
! -------- Linux compiler 8.0 discovered on 2003.12.25
!
#ifdef SUN
           CALL LIB$MOVC3 ( LEN_ARG, ENV_VALUE, &
     &                      %VAL(LOC__SUN$$_STR(STR))) )
#else
           CALL LIB$MOVC3 ( LEN_ARG, ENV_VALUE, %REF(STR) )
#endif
           CALL SETENV ( DIAGI_CTIT//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_UNIT ) THEN
           LEN_ARG = LOC(ARG2_HIDDEN)
           IF ( LEN_ARG .GT. 110 ) LEN_ARG=110
           IF ( LEN_ARG .LT. 1   ) LEN_ARG=1
           CALL CLRCH ( STR )
!
! -------- This strange construction is for circumventing a bug in Intel
! -------- Linux compiler 8.0 discovered on 2003.12.25
!
#ifdef SUN
           CALL LIB$MOVC3 ( LEN_ARG, %VAL(LOC__SUN$$_STR(ENV_VALUE)), &
     &                               %VAL(LOC__SUN$$_STR(DIAGI_UNIT_ENV(IL:))) )
#else
           CALL LIB$MOVC3 ( LEN_ARG, %REF(ENV_VALUE), %REF(STR) )
#endif
           CALL SETENV ( DIAGI_UNIT//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_ICL1 ) THEN
           CALL INCH   ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_ICL1//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_ICL2 ) THEN
           CALL INCH   ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_ICL2//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE IF ( ENV_NAME(1:LEN_ENV) .EQ. DIAGI_ICL3 ) THEN
           CALL INCH   ( ENV_VALUE, STR )
           CALL CHASHL ( STR )
           CALL SETENV ( DIAGI_ICL3//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
         ELSE
           CALL ERR_LOG ( 4671, IUER, 'DIAGI_SETDEF', 'Wrong environment '// &
     &         'variable '//ENV_NAME(1:LEN_ENV)//' -- check documentation' )
           RETURN
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                 ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4672, IUER, 'DIAGI_SETDEF', 'Error in attempt to '// &
     &         'set environment variables for DiaGI' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIAGI_SETDEF  #!#
