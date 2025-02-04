      PROGRAM    DIAGI_DEC__MAIN
! ************************************************************************
! *                                                                      *
! *   Program  DEIAGI_DEC__MAIN  shows the table of DiaGI predefined     *
! *   colours.
! *                                                                      *
! *  ###  22-OCT-97  DIAGI_DEC_MAIN  v1.0 (c) L. Petrov  22-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      IUER = -1
      CALL DIAGI_DEC ( IUER )
      END  !#!  DIAGI_DEC__MAIN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_DEC ( IUER )
! ************************************************************************
! *                                                                      *
! *   Program  DEIAGI_DEC shows the table of DiaGI predefined colours.   *
! *                                                                      *
! *  ###  22-OCT-97     DIAGI_DEC  v1.2 (c) L. Petrov  05-JUN-2001  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INCLUDE   'diagi_local.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  IUER
      CHARACTER  DEVICE*20, DIAGI_LABEL*80
      INTEGER*4  IER, ID_XW, J1, ICLR, IER
      CHARACTER  STR*20
      REAL*8     X8, Y8, E8
      INTEGER*4  I_LEN, PGOPEN
!
      X8 = 1.0D0
      Y8 = 1.0D0
      E8 = 1.0D0
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%NCLR      = MCLR
      DIAGI_S%IDEV      = IXS__DEF
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = 0
      DIAGI_S%IOST(1)   = 0
      DIAGI_S%IPST(1)   = 0
      DIAGI_S%IWST(1)   = 0
      DIAGI_S%ICLR      = 0
      DIAGI_S%XMIN      = 0.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 0.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = ' '
      DIAGI_S%NAME      = ' '
      DIAGI_S%ARG_UNITS = ' '
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
      DO 410 J1=1,DIAGI_S%NCLR
         DIAGI_S%ICOL(J1)   = J1
         DIAGI_S%NPOI(J1)   = 1
         DIAGI_S%ADR_X8(J1) = LOC(X8)
         DIAGI_S%ADR_Y8(J1) = LOC(Y8)
         DIAGI_S%ADR_E8(J1) = LOC(E8)
 410  CONTINUE
!
      CALL CLRCH ( DIAGI_LABEL )
      DIAGI_LABEL = DIAGI_LABEL__DEF
!
! --- Setting plotting parameters
!
      CALL DIAGI_SET ( DIAGI_S%IDEV, DIAGI_S )
!
! --- Oprenning X-window plotting device
!
      ID_XW = PGOPEN ( DIAGI_S%DEVICE )
      IF ( ID_XW .LE. 0 ) THEN
           IER = 0
           CALL CLRCH ( STR )
           CALL INCH  ( ID_XW, STR )
           CALL CLRCH ( DEVICE )
           DEVICE = DIAGI_S%DEVICE
           CALL ERR_LOG ( 4101, IUER, 'DIAGI', 'Error in openning '// &
     &         'the graphic device '//DEVICE(1:I_LEN(DEVICE))// &
     &         ' IER='//STR )
           RETURN
      END IF
!
! --- Allocation memory for internal DIAGI_S data structure, setting
! --- current values of plotting parameters
!
      CALL ERR_PASS  ( IUER, IER )
      ICLR = 1
      CALL DIAGI_INT ( 1, DIAGI_S, ICLR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4102, IUER, 'DIAGI', 'Error in initialization '// &
     &         'DiaGI internal data structures' )
           RETURN
      END IF
!
! --- Setting colours
!
      CALL DIAGI_CLS ( DIAGI_S, IER )
!
! --- Setting default font type
!
      CALL PGSCF     ( 2 )
!CC
      CALL DIAGI_COLTAB ( DIAGI_S )
!
! --- Closing plotting device
!
      CALL PGENDQ
!
! --- Antiinitialization of internal data structure (freeing dynamic memory etc)
!
      CALL ERR_PASS  ( IUER, IER )
      CALL DIAGI_INT ( 2, DIAGI_S, ICLR, IER )
      DIAGI_S%STATUS = DIA__UND
!
! --- Printing farwell message at the user terminal and waiting for
! --- user reaction
!
      CALL ERR_PASS ( 0, IUER )
      RETURN
      END  !#!  DIAGI_DEC
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_COLTAB ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_COLTAB  show DiaGI predefined colours.              *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *                                                                      *
! *  ###  22-OCT-97  DIAGI_COLTAB  v1.0  (c)  L. Petrov  22-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      TYPE ( DIAGI_BOXES ) ::  BOX(MCLR)
      INTEGER*4  ICLR, J1, J2
      CHARACTER  MES1*128, CH*1, STR*20
      REAL*4     XCT, YCT
      REAL*4     XC, YC, SIZ, Y0, YS, RAD_MM, XRAD_WC, YRAD_WC, &
     &           X4(4), Y4(4), X8(8), Y8(8)
      INTEGER*4  MROW, MCOL, IR, IC, ICLR_N, IDI, NPTS
      INTEGER*4  I_LEN, IDIV8, IM, DIAGI_INBOX
      IDIV8(IM) = IM/8 + MIN ( 1, MOD( IM, 8 ) )
!
      RAD_MM = DIAGI_S%RAD_LARGE
      NPTS   = NPTS_LARGE
!
! --- Setting coordinates of template curve
!
      DATA ( X4(IM), IM=1,4 ), ( Y4(IM), IM=1,4 ) &
     &     / &
     &       0.20, 0.50, 0.75, 0.90, &
     &       0.30, 0.60, 0.40, 0.50 &
     &     /
!
      X8(1)  = X4(1) - 0.025
      Y8(1)  = Y4(1) - 0.2
      X8(2)  = X4(1) - 0.025
      Y8(2)  = Y4(1) + 0.2
      X8(3)  = X4(2)
      Y8(3)  = Y4(2) + 0.2
      X8(4)  = X4(3)
      Y8(4)  = Y4(3) + 0.2
      X8(5)  = X4(4) + 0.025
      Y8(5)  = Y4(4) + 0.2
      X8(6)  = X4(4) + 0.025
      Y8(6)  = Y4(4) - 0.2
      X8(7)  = X4(3)
      Y8(7)  = Y4(3) - 0.2
      X8(8)  = X4(2)
      Y8(8)  = Y4(2) - 0.2
!
      CALL CLRCH ( MES1 )
      MES1 = 'Default DiaGI colours'
!
! --- Setting coordiunates of
!
      YCT  = 0.95
      XCT  = 0.5
      ICLR = 1
!
! --- Deleting previous window
!
      CALL PGERAS
!
      CALL PGSAVE ! 1
!
! --- Setting new world coodrinates
!
      CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
      CALL PGSCI   ( 1   )
!
! --- Printing the prompt
!
      CALL PGSCH   ( 2.4 )
      CALL PGSLW   ( 8   )
      CALL PGPTXT  ( XCT, YCT, 0.0, 0.5, MES1(1:I_LEN(MES1)) )
!
! --- Setting maximum device independent coordiantes
!
      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
!
! --- Calculation the number of rows and the number of columns
!
      MROW = (8+DIAGI_S%NCLR)/8
      IF ( MOD(DIAGI_S%NCLR,8) .EQ. 0 ) MROW = MROW -1
      IF ( DIAGI_S%NCLR .GE. 8 ) MCOL = 8
      IF ( DIAGI_S%NCLR .LT. 8 ) MCOL = DIAGI_S%NCLR
!
! --- Setting the sizes of the colour rectangular
!
      SIZ = 1.0/(2*MCOL+1)
      YC  = 0.50
      Y0  = 0.75
      YS  = 0.20
!
! --- Setting the size of circles for the points
!
      XRAD_WC = RAD_MM/((DIAGI_S%XRIGHT - DIAGI_S%XLEFT)*SIZ)
      YRAD_WC = RAD_MM/((DIAGI_S%YTOP - DIAGI_S%YBOT)*2.*SIZ)
!
      DO 410 J1=1,DIAGI_S%NCLR
         IR = IDIV8(J1)
         IC = MOD(J1,8)
         IF ( IC .EQ. 0 ) IC = 8
!
! ------ Specifing the corner of the J1-th box
!
         IF ( MROW .EQ. 1 ) THEN
!
! ----------- One-row mode
!
              BOX(J1)%XLB = SIZ*((J1-1)*2+1)
              BOX(J1)%YLB = YC-SIZ
              BOX(J1)%XTU = SIZ*((J1-1)*2+1)+SIZ
              BOX(J1)%YTU = YC+SIZ
            ELSE
!
! ----------- Multi-rows mode
!
              BOX(J1)%XLB = SIZ*((IC-1)*2+1)
              BOX(J1)%YLB = (Y0 - (IR-1)*YS) - SIZ
              BOX(J1)%XTU = SIZ*((IC-1)*2+1) + SIZ
              BOX(J1)%YTU = (Y0 - (IR-1)*YS) + SIZ
         END IF
!
         CALL PGSAVE ! 3A
!
! ------ Printing the box as an outlined rectangular
!
         CALL PGSFS  ( 2 )
         CALL PGSLW  ( 1 )
         CALL PGSCI  ( 1 )
         CALL PGRECT ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
!
! ------ Temporary setting world coordinate space for the current colour
! ------ box
!
         CALL PGSVP  ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
!
! ------ Printing the filled area of error bars
!
         CALL PGSCI  ( ITAB_CLR(J1,2) )
         CALL PGSFS  ( 1   )
         CALL PGPOLY ( 8, X8, Y8 )
!
! ------ Ptinting filled circles
!
         CALL PGSCI  ( ITAB_CLR(J1,1) )
         DO 420 J2=1,4
            CALL PGCIRC_PET ( NPTS, X4(J2), Y4(J2), XRAD_WC, YRAD_WC )
 420     CONTINUE
!
! ------ Printing the line (with thick nwidth)
!
         CALL PGSLW   ( DIAGI_S%IWD_LINS(3) )
         CALL PGLINE  ( 4, X4, Y4 )
!
         CALL PGUNSA ! 3A
!
! ------ Restoring world coordinated space
!
         CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
         CALL PGSAVE ! 3B
!
! ------ Printing explaining text
!
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
         CALL PGSCF  ( 3 )
         CALL PGSLW  ( 4 )
         CALL PGSCH  ( 1.2 )
         CALL PGPTXT ( BOX(J1)%XLB+SIZ/2., BOX(J1)%YLB-0.04, 0.0, 0.5, &
     &                 'CLR='//STR(1:2) )
         CALL PGUNSA ! 3B
         CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
 410  CONTINUE
!
! --- Position cursor in the box with the current line-style
!
      XC = BOX(ICLR)%XLB + ( BOX(ICLR)%XTU - BOX(ICLR)%XLB )/3.
      YC = BOX(ICLR)%YLB + ( BOX(ICLR)%YTU - BOX(ICLR)%YLB )/3.
!
! --- Waiting for user reaction
!
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
! --- Determine: which box has been specified
!
      IF ( CH .EQ. '1'  .OR.  CH .EQ. '2'  .OR.  CH .EQ. '3'  .OR. &
     &     CH .EQ. '4'  .OR.  CH .EQ. '5'  .OR.  CH .EQ. '6'  .OR. &
     &     CH .EQ. '7'  .OR.  CH .EQ. '8'  .OR.  CH .EQ. '9'       ) THEN
           CALL CHIN ( CH, ICLR_N )
           IF ( ICLR_N .LE. DIAGI_S%NCLR ) ICLR = ICLR_N
         ELSE IF ( CH .NE. 'X' ) THEN
           IDI = DIAGI_INBOX ( DIAGI_S%NCLR, BOX, XC, YC )
           IF ( IDI .GT. 0 ) ICLR = IDI
      END IF
!
      CALL PGUNSA ! 1
      DIAGI_S%ICLR = ICLR
!
      RETURN
      END  !#!  DIAGI_COLTAB  #!#
