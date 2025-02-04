      SUBROUTINE DIAGI_DRAW ( DIAGI_S, ICLR, KCLR, N, XARR4, YARR4, &
     &                        EARR4, X1, Y1 )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_DRAW  actually draws the plot of the specified      *
! *   colour at previously openned PGPLOT graphic device. It doesn't     *
! *   draw boinding box, labels and title -- it assumed that they are    *
! *   drawn already by DIAGI_SET_FRAME.                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI internal  *
! *                           parameters.                                *
! *     ICLR ( INTEGER*4 ) -- Index of the current DiaGI plotting        *
! *                           function.                                  *
! *     KCLR ( INTEGER*4 ) -- Index of the color from DiaGI palette      *
! *                           which is to be used for plotting the       *
! *                           points and error barrs of the function.    *
! *                           IF KLCR =< 0, then KCLR is set to ICLR.    *
! *        N ( INTEGER*4 ) -- Number of elements in the arrays of the    *
! *                           function.                                  *
! *    XARR4 ( REAL*4    ) -- Array of arguments of the function.        *
! *    YARR4 ( REAL*4    ) -- Array of values of the function.           *
! *    EARR4 ( REAL*4    ) -- Array of errors of the function.           *
! *       X1 ( REAL*8    ) -- Array of arguments of the function.        *
! *       Y1 ( REAL*8    ) -- Array of values of the function.           *
! *                                                                      *
! *    Comment: XARR4 and X1, YARR4 and Y1 holds the same values but     *
! *    different type: REAL*4 anad REAL*8 respectively.                  *
! *                                                                      *
! *  ###  13-OCT-97   DIAGI_DRAW   v2.1  (c)  L. Petrov 05-NOV-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  N, ICLR, KCLR
      REAL*4     XARR4(N), YARR4(N), EARR4(N)
      REAL*8     X1(N), Y1(N)
      INTEGER*4  NPTS_C, IBST, ILST, IPST, IWST, CLR_IND, J1, J2, J3, J4, J5, &
     &           J6, IER, IUER
      CHARACTER  STR*32
      REAL*4     RAD_MM, XRAD_WC, YRAD_WC, XSPL(MSPL), YSPL(MSPL), XP, YP, &
     &           X2(2), Y2(2), X4(4), Y4(4), XLEFT_MM, XRIGHT_MM, &
     &           YBOT_MM, YTOP_MM, XY_RATIO
      LOGICAL*4  FL_OVE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IBST = DIAGI_S%IBST(ICLR)
      ILST = DIAGI_S%ILST(ICLR)
      IPST = DIAGI_S%IPST(ICLR)
      IWST = DIAGI_S%IWST(ICLR)
!
      CLR_IND  = KCLR
      IF ( CLR_IND .LE. 0 .OR. CLR_IND .GT. MCLR ) THEN
           CLR_IND  = ICLR
      END IF
      CALL PGSAVE  ! 1
      CALL PGBBUF
!
! --- Determine the radii of the circle
!
      RAD_MM  = 0.0
      XRAD_WC = 0.0
      YRAD_WC = 0.0
      IF ( IPST .EQ. 2  .OR. IPST .EQ. 4 ) THEN
           RAD_MM  = DIAGI_S%RAD_SMALL
           NPTS_C  = NPTS_SMALL
        ELSE IF ( IPST .EQ. 3  .OR.  IPST .EQ. 5 ) THEN
           RAD_MM  = DIAGI_S%RAD_LARGE
           NPTS_C  = NPTS_LARGE
           RAD_MM  = DIAGI_S%RAD_LARGE
      END IF
      IF ( DIAGI_S%XRIGHT > 600.0 ) RAD_MM = 1.6*RAD_MM
      IF ( IPST .EQ. 2 .OR. IPST .EQ. 3 .OR. IPST .EQ. 4 .OR. IPST .EQ. 5 ) THEN
           XRAD_WC = RAD_MM*(DIAGI_S%XMAX   - DIAGI_S%XMIN)/ &
     &                      (DIAGI_S%XRIGHT - DIAGI_S%XLEFT + DIAGI_S%XL_ADJ )
           YRAD_WC = RAD_MM*(DIAGI_S%YMAX   - DIAGI_S%YMIN)/ &
     &                      (DIAGI_S%YTOP   - DIAGI_S%YBOT )
      END IF
!
! --- A kludge environment variable for adjusting the aspect ratio
!
      CALL GETENVAR ( 'DIAGI_RATIO', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT=*, IOSTAT=IER ) XY_RATIO
           IF ( IER == 0 .AND. XY_RATIO > 0.1 .AND. XY_RATIO < 10.0 ) THEN
                YRAD_WC = XY_RATIO*RAD_MM*(DIAGI_S%YMAX - DIAGI_S%YMIN)/ &
     &                                    (DIAGI_S%XRIGHT - DIAGI_S%XLEFT )
           END IF
      END IF
!
! --- Setting the colour and temporary line width ( for drawing error bar )
!
      CALL PGSLW ( 1  )
      CALL PGSCI ( ITAB_CLR(CLR_IND,1) )
!
! --- Firstly draw vertical error bar (in order not to overlap the main plot)
!
      IF ( IBST .EQ. 1 ) THEN
!
! -------- "Stiky" style of the error bars with terminals
!
           CALL PGERRB ( 6, N, XARR4, YARR4, EARR4, 1.0 )
        ELSE IF ( IBST .EQ. 2 ) THEN
!
! -------- "Stiky" style of the error bars without terminals
!
           CALL PGERRB ( 6, N, XARR4, YARR4, EARR4, 0.0 )
        ELSE IF ( IBST .EQ. 3 ) THEN
!
! -------- Continuous line + sticks
!
           CALL PGERRB ( 6, N, XARR4, YARR4, EARR4, 0.0 )
           DO 410 J1=1,N-1
              X2(1)=XARR4(J1)
              X2(2)=XARR4(J1+1)
              Y2(1)=YARR4(J1) + EARR4(J1)
              Y2(2)=YARR4(J1+1) + EARR4(J1+1)
              CALL PGLINE ( 2, X2, Y2 )
              Y2(1)=YARR4(J1) - EARR4(J1)
              Y2(2)=YARR4(J1+1) - EARR4(J1+1)
              CALL PGLINE ( 2, X2, Y2 )
 410       CONTINUE
        ELSE IF ( IBST .EQ. 4 ) THEN
!
! -------- Fill confidence interval area
!
           CALL PGSCI  ( ITAB_CLR(CLR_IND,2) )
           CALL PGSFS ( 1   )
!
! -------- To do it we split filling area at N-1 adjacent parallelograms and
! -------- use function "fill polygons"
!
           DO 420 J2=1,N-1
              X4(1)=XARR4(J2)
              X4(2)=XARR4(J2)
              X4(3)=XARR4(J2+1)
              X4(4)=XARR4(J2+1)
              Y4(1)=YARR4(J2) - EARR4(J2)
              Y4(2)=YARR4(J2) + EARR4(J2)
              Y4(3)=YARR4(J2+1) + EARR4(J2+1)
              Y4(4)=YARR4(J2+1) - EARR4(J2+1)
              CALL PGPOLY ( 4, X4, Y4 )
 420       CONTINUE
      END IF
!
! --- Drawing the main plot
!
      CALL PGSCI ( ITAB_CLR(CLR_IND,1) )
      IF ( ILST .EQ. 1 ) THEN
!
! -------- Line_style: point by point
!
           CALL PGSAVE ! 2
           CALL PGSLW  ( 1   )
           CALL PGSCH  ( 0.1 )
           DO 430 J3=1,N
              CALL PGPNTS ( 1, XARR4(J3), YARR4(J3), 1, 1 )
 430       CONTINUE
           CALL PGUNSA ! 2
        ELSE IF ( ILST .EQ. 2 ) THEN
!
! -------- Line_style: continuous piecewise line
!
           CALL PGSLW  ( DIAGI_S%IWD_LINS(IWST) )
           CALL PGLINE ( N, XARR4, YARR4 )
        ELSE IF ( ILST .EQ. 3 ) THEN
!
! -------- Line_style: cubic spline
!
! -------- Firstly calculate spline curve
!
           IUER = -1
           IF ( N .GE. 3 ) THEN
                CALL DIAGI_SPL ( N, X1, Y1, DIAGI_S%XMIN, DIAGI_S%XMAX, &
     &                           MSPL, XSPL, YSPL, IUER )
           END IF
           CALL PGSLW     ( DIAGI_S%IWD_LINS(IWST) )
           IF ( IUER .NE. 0 ) THEN
                IF ( IUER .NE. -1 ) THEN
!
! ------------------ Sending error message ...
!
                     CALL ERR_LOG ( 4111, -1, 'DIAGI_DRAW', 'Error in making '// &
     &                   'spline interpolation' )
                END IF
!
! ------------- ... cnange the line style by the fly ...
!
                ILST = 2
                DIAGI_S%ILST(ICLR) = ILST
!
! ------------- ... and plot line in the case of failure of spline interpolation
!
                CALL PGLINE ( N, XARR4, YARR4 )
             ELSE
!
! ------------- ... then plot spline curve
!
                CALL PGLINE ( MSPL, XSPL, YSPL )
           END IF
      END IF
!
! --- Now additionally plotting the points
!
      IF ( IPST .EQ. 2 .OR. IPST .EQ. 3 ) THEN
!
! -------- Point_style: Outlined circles
!
           CALL PGSLW ( 1 )
           DO 440 J4=1,N
              CALL PGSFS ( 1 )
              CALL PGSCI ( 0 )
              CALL PGCIRC_PET ( NPTS_C, XARR4(J4), YARR4(J4), XRAD_WC, YRAD_WC )
              CALL PGSFS ( 2  )
              CALL PGSCI ( ITAB_CLR(CLR_IND,1) )
              CALL PGCIRC_PET ( NPTS_C, XARR4(J4), YARR4(J4), XRAD_WC, YRAD_WC )
 440       CONTINUE
      END IF
      IF ( IPST .EQ. 4 .OR. IPST .EQ. 5 ) THEN
!
! -------- Point_style: Solid circles
!
           CALL PGSLW ( 1 )
           CALL PGSFS ( 1 )
           DO 450 J5=1,N
              CALL PGCIRC_PET ( NPTS_C, XARR4(J5), YARR4(J5), XRAD_WC, YRAD_WC )
 450       CONTINUE
      END IF
!
! --- Flushing the output
!
      CALL PGEBUF
      CALL PGUPDT
      CALL PGUNSA ! 1
!
      IF ( DIAGI_S%IOST(ICLR) .EQ. 1 ) THEN
!
! -------- Overplot mode. Now we plot the points which are beyond the plotting
! -------- area. We put them near the boundaries
!
! -------- Increase a little bit plotting window
!
           CALL PGQVP  ( 2, XLEFT_MM, XRIGHT_MM, YBOT_MM, YTOP_MM )
           CALL PGVSIZ ( (XLEFT_MM  - 2.0*DIAGI_S%RAD_LARGE)/25.4, &
     &                   (XRIGHT_MM + 2.0*DIAGI_S%RAD_LARGE)/25.4, &
     &                   (YBOT_MM   - 2.0*DIAGI_S%RAD_LARGE)/25.4, &
     &                   (YTOP_MM   + 2.0*DIAGI_S%RAD_LARGE)/25.4  )
           CALL PGSWIN ( DIAGI_S%XMIN, DIAGI_S%XMAX, &
     &                   DIAGI_S%YMIN, DIAGI_S%YMAX )
!
           CALL PGSAVE  ! 1
           CALL PGBBUF
!
           CALL PGSCI ( ITAB_CLR(CLR_IND,1) )
           DO 460 J6=1,N
              XP = XARR4(J6)
              YP = YARR4(J6)
              FL_OVE = .FALSE.
!
! ----------- Check whether the point is beyond of one of the four boundaries
!
              IF ( XARR4(J6) .LT. DIAGI_S%XMIN ) THEN
                   XP = DIAGI_S%XMIN + DIAGI_S%RAD_LARGE/ &
     &                (DIAGI_S%XRIGHT - DIAGI_S%XLEFT + 4.0*DIAGI_S%RAD_LARGE)* &
     &                (DIAGI_S%XMAX - DIAGI_S%XMIN)
                   FL_OVE = .TRUE.
              END IF
              IF ( XARR4(J6) .GT. DIAGI_S%XMAX ) THEN
                   XP = DIAGI_S%XMAX - DIAGI_S%RAD_LARGE/ &
     &                (DIAGI_S%XRIGHT - DIAGI_S%XLEFT + 4.0*DIAGI_S%RAD_LARGE)* &
     &                (DIAGI_S%XMAX - DIAGI_S%XMIN)
                   FL_OVE = .TRUE.
              END IF
!
              IF ( YARR4(J6) .LT. DIAGI_S%YMIN ) THEN
                   YP = DIAGI_S%YMIN + DIAGI_S%RAD_LARGE/ &
     &                  (DIAGI_S%YTOP - DIAGI_S%YBOT + 4.0*DIAGI_S%RAD_LARGE)* &
     &                  (DIAGI_S%YMAX - DIAGI_S%YMIN)
                   FL_OVE = .TRUE.
              END IF
              IF ( YARR4(J6) .GT. DIAGI_S%YMAX ) THEN
                   YP = DIAGI_S%YMAX - DIAGI_S%RAD_LARGE/ &
     &                  (DIAGI_S%YTOP - DIAGI_S%YBOT + 4.0*DIAGI_S%RAD_LARGE)* &
     &                  (DIAGI_S%YMAX - DIAGI_S%YMIN)
                   FL_OVE = .TRUE.
              END IF
!
              IF ( .NOT. FL_OVE ) GOTO 460 ! not beyond? Nothing to do
              IF ( IPST .EQ. 1 ) THEN
!
! ---------------- Drawing a little point
!
                   CALL PGPNTS ( 1, XP, YP, 1, 1 )
                 ELSE IF ( IPST .EQ. 2 .OR. IPST .EQ. 3 ) THEN
!
! ---------------- Point_style: Outlined circles
!
                   CALL PGSFS ( 1 )
                   CALL PGSCI ( 0 )
                   CALL PGCIRC_PET ( NPTS_C, XP, YP, XRAD_WC, YRAD_WC )
                   CALL PGSFS ( 2  )
                   CALL PGSCI ( ITAB_CLR(CLR_IND,1) )
                   CALL PGCIRC_PET ( NPTS_C, XP, YP, XRAD_WC, YRAD_WC )
                 ELSE IF ( IPST .EQ. 4 .OR. IPST .EQ. 5 ) THEN
!
! ---------------- Point_style: Solid circles
!
                   CALL PGSLW ( 1 )
                   CALL PGSFS ( 1 )
                   CALL PGCIRC_PET ( NPTS_C, XP, YP, XRAD_WC, YRAD_WC )
              END IF
 460       CONTINUE
!
! -------- Flushing the output
!
           CALL PGEBUF
           CALL PGUPDT
           CALL PGUNSA ! 1
!
! -------- Return back to normal style
!
           CALL PGVSIZ  ( XLEFT_MM/25.4, XRIGHT_MM/25.4, &
     &                    YBOT_MM/25.4, YTOP_MM/25.4  )
           CALL PGSWIN  ( DIAGI_S%XMIN, DIAGI_S%XMAX, &
     &                    DIAGI_S%YMIN, DIAGI_S%YMAX )
      END IF
!
      RETURN
      END  !#!  DIAG_DRAW  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_SET_FRAME ( DIAGI_S, DIAGI_LABEL  )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_SET_FRAME  draws the frame-box around the plotting  *
! *   area at the graphic window, prints plot title and DiaGI label.     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! * DIAGI_LABEL ( CHARACTER ) -- Line with DiaGI label whcih will be     *
! *                              be printed at the bottom of the window. *
! *                                                                      *
! * ### 13-OCT-97  DIAGI_SET_FRAME  v1.3 (c) L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'diagi.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  DIAGI_LABEL*(*)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( DIAGI_S%SET_VP ) THEN
!
! -------- Setting window size in physical coordinates
!
           CALL PGPAP ( (DIAGI_S%XLEFT+DIAGI_S%XRIGHT)/25.4, &
     &         (DIAGI_S%YBOT+DIAGI_S%YTOP)/(DIAGI_S%XLEFT+DIAGI_S%XRIGHT) )
!
! -------- Setting physical size of plotting area
!
           CALL PGVSIZ ( DIAGI_S%XLEFT/25.4, &
     &                   DIAGI_S%XRIGHT/25.4 + (1-DIAGI_XRM)*DIAGI_S%XLEFT/25.4, &
     &                   DIAGI_S%YBOT/25.4,  DIAGI_S%YTOP/25.4   )
      END IF
!
! --- Setting limits of world coordinates and mapping world space to the device
! --- area
!
      CALL PGSWIN ( DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX )
      IF ( DIAGI_S%ERASE_SCREEN ) THEN
!
! -------- Erasing plotting area
!
           CALL PGERAS
      END IF
!
! --- Drawing coordinate box
!
      CALL PGSCH   ( DIAGI_S%SCH_FRM  )
      CALL PGSLW   ( DIAGI_S%ISLW_LAB )
      CALL PGSCI   ( 1   )
      CALL PGBOX   ( 'BCNTS', 0.0, 10, 'BCNTSV', 0.0, 10 )
!
      IF ( ILEN(DIAGI_S%ARG_UNITS) .GT. 0 ) THEN
           IF ( DIAGI_S%XRIGHT > 600.0 ) CALL PGSCH  ( DIAGI_S%SCH_TIT    )
           CALL PGPTXT ( DIAGI_S%XMAX, DIAGI_S%YMIN + &
     &         (DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%SCH_FRM*DIAGI_S%YSH_ARU, &
     &          0.0, 1.0, DIAGI_S%ARG_UNITS(1:I_LEN(DIAGI_S%ARG_UNITS)) )
      END IF
!
! --- Drawing the title of the plot
!
      CALL PGSCI   ( 1 )
      CALL PGSCH   ( DIAGI_S%SCH_TIT    )
      CALL PGSLW   ( DIAGI_S%ISLW_TIT   )
      CALL PGPTXT  ( (DIAGI_S%XMAX+DIAGI_S%XMIN)/2, &
     &             DIAGI_S%YMIN + (DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%YSH_TIT, &
     &             0.0, 0.5, DIAGI_S%ZAG(1:I_LEN(DIAGI_S%ZAG)) )
      IF ( DIAGI_S%IBATCH .EQ. 0 ) THEN
!
! -------- Drawing DiaGI label at the bottom of the plot if DiaGI is called
! -------- in interactive mode
!
           CALL PGSCI   ( 1 )
           CALL PGSCH   ( DIAGI_S%SCH_LAB  )
           CALL PGSLW   ( DIAGI_S%ISLW_LAB )
           CALL PGPTXT  ( DIAGI_S%XMIN, &
     &             DIAGI_S%YMIN + (DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%YSH_LAB, &
     &             0.0, 0.0, DIAGI_LABEL(1:I_LEN(DIAGI_LABEL)) )
      END IF
!
      RETURN
      END  !#!  DIAGI_SET_FRAME  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_SPL ( N1, X1, Y1, XMIN, XMAX, N2, X2, Y2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_SPL  makes spline interpolation of the function     *
! *   specified by the input arrays N1, X1, Y1. Output array Y2 of the   *
! *   values of N2 points for arguments X2 is generated. NB: Input       *
! *   arrays have REAL*8 type, but output arrays have REAL*4 type.       *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *     N1 ( INTEGER*4 ) -- The number of elements in the input array.   *
! *     X1 ( REAL*8    ) -- Array of arguments of input function.        *
! *                         Dimension: N1.                               *
! *     Y1 ( REAL*8    ) -- Array of values of input function.           *
! *                         Dimension: N1.                               *
! *   XMIN ( REAL*4    ) -- Recommended left boundary of the the range   *
! *                         for which spline is to be computed. However, *
! *                         the actual boundary will be less than X1(1)  *
! *                         in order to prevent extrapolation.           *
! *   XMAX ( REAL*4    ) -- Recommended right boundary of the the range  *
! *                         for which spline is to be computed. However, *
! *                         the actual boundary will be less than X1(N1) *
! *                         in order to prevent extrapolation.           *
! *     N2 ( INTEGER*4 ) -- The number of elements in the output array.  *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *     X2 ( REAL*4    ) -- Array of arguments of output function.       *
! *                         Dimension: N2.                               *
! *     Y2 ( REAL*4    ) -- Array of values of output function, produced *
! *                         by spline interploation of the input array.  *
! *                         Dimension: N2.                               *
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
! *   Restriction: Arrays X1 and X2 should be ordered in increasing of   *
! *                the elements.                                         *
! *                                                                      *
! *  ###  13-OCT-97    DIAGI_SPL   v2.0  (c) L. Petrov  25-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N1, N2, IUER, IXC, IER, J1, IXMN8_S
      REAL*8     X1(N1), Y1(N1)
      REAL*4     XMIN, XMAX, X2(N2), Y2(N2)
      REAL*8     COEF(:),  WORK(:)
      REAL*8     D1, DN, XC8, XFIRST, XLAST, YC8, STEP, FSPL8
      ALLOCATABLE  COEF, WORK
!
      ALLOCATE   ( COEF(N1), WORK(N1) )
!
      IF ( N1 .LE. 2 ) THEN
           RETURN
      END IF
!
      IF ( X1(2)-X1(1)  .GT.  1.D-30 ) THEN
           D1 = ( Y1(2)-Y1(1) )/( X1(2)-X1(1) )
         ELSE
           D1 = 1.D30
      END IF
      IF ( X1(N1) - X1(N1-1)  .GT.  1.D-30 ) THEN
           DN = ( Y1(N1)-Y1(N1-1) )/( X1(N1)-X1(N1-1) )
         ELSE
           DN = 1.D30
      END IF
!
! --- Calculation spline coeffitients
!
      CALL ERR_PASS ( IUER, IER )
      CALL MAKE_SPLINE ( 2, N1, X1, Y1, D1, DN, COEF, WORK, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4112, IUER, 'DIAGI_SPL', 'Error in calculating '// &
     &         'spline coefficients' )
           RETURN
      END IF
      XFIRST = MAX ( X1(1),  DBLE(XMIN) )
      XLAST  = MIN ( X1(N1), DBLE(XMAX) )
      IF ( ( XLAST - XFIRST ) .LT. 1.D-30*(2*N2) ) THEN
           XLAST = XFIRST + 1.D-30*(2*N2)
      END IF
!
! --- Generating output array of the points
!
      STEP = (XLAST - XFIRST )/(N2-1)
      IXC = 1
      DO 410 J1=1,N2
!
! ------ Calculation of XC8 -- argument of the J1-th output point
!
         XC8 = XFIRST + STEP*(J1-1)
         IXC = IXMN8_S ( IXC, N1, X1, XC8 )
!
! ------ Calculation of YC8 -- values of the J1-th output point
!
         YC8 = FSPL8   ( XC8, N1, X1, Y1, IXC, COEF )
         X2(J1) = XC8
         Y2(J1) = YC8
 410  CONTINUE
      DEALLOCATE   ( COEF, WORK )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  DIAGI_SPL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_R8_R4 ( N, ARR8, ARR4 )
! ************************************************************************
! *                                                                      *
! *   Copying array N elemets of the array ARR8 of REAL*8 typoe to the   *
! *   array ARR4 REAL*4 and making type conversion.                      *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      N ( INTEGER*4 ) -- The number of elements in the array ARR4.    *
! *   ARR8 ( REAL*8    ) -- Input array.                                 *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *   ARR4 ( REAL*4    ) -- Output array.                                *
! *                                                                      *
! *  ###  10-OCT-1997  COPY_R8_R4  v1.1  (c)  L. Petrov  13-MAY-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1
      REAL*8     ARR8(N)
      REAL*4     ARR4(N)
      REAL*8       MIN__VAL, MAX__VAL
      PARAMETER  ( MIN__VAL = -1.0D33 )
      PARAMETER  ( MAX__VAL =  1.0D33 )
!
      DO 410 J1=1,N
         IF ( ARR8(J1) > MAX__VAL ) THEN
              ARR4(J1) = MAX__VAL 
            ELSE IF ( ARR8(J1) < MIN__VAL ) THEN
              ARR4(J1) = MIN__VAL
            ELSE 
              ARR4(J1) = ARR8(J1)
         END IF
 410  CONTINUE
!
      RETURN
      END  !#!  COPY_R8_R4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_R4_R8 ( N, ARR4, ARR8 )
! ************************************************************************
! *                                                                      *
! *   Copying array N elemets of the array ARR4 of REAL*4 typoe to the   *
! *   array ARR8 REAL*8 and making type conversion.                      *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      N ( INTEGER*4 ) -- The number of elements in the array ARR4.    *
! *   ARR4 ( REAL*4    ) -- Input array.                                 *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *   ARR8 ( REAL*8    ) -- Output array.                                *
! *                                                                      *
! *  ###  13-OCT-97   COPY_R4_R8   v1.0  (c)  L. Petrov  13-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1
      REAL*4     ARR4(N)
      REAL*8     ARR8(N)
!
      DO 410 J1=1,N
         ARR8(J1) = ARR4(J1)
 410  CONTINUE
!
      RETURN
      END  !#!  COPY_R4_R8  #!#
