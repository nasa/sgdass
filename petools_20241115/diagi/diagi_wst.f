      SUBROUTINE DIAGI_WST ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_WST  bids user to change the line width for         *
! *   representation the plot of the current colour. Field               *
! *   DIAGI_S.IWST(ICLR) is updated.                                     *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *                                                                      *
! *  ###  15-OCT-97    DIAGI_WST   v1.2  (c)  L. Petrov 08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  ICLR, ILST, IPST, IWST, MP, NSP, NPTS
      PARAMETER  ( MP=7,  NSP=64 )
      CHARACTER  MES*128, CH*1, STR*20
      REAL*4     XC, YC, SIZ, X4(MP), Y4(MP), XSPL(NSP), YSPL(NSP)
      REAL*4     RAD_MM, XRAD_WC, YRAD_WC, X4_MIN, X4_MAX
      REAL*8     X8(MP), Y8(MP)
      INTEGER*4  J1, J2, J3, NN, IUER, IDI
      INTEGER*4  I_LEN, DIAGI_INBOX
      TYPE ( DIAGI_BOXES ) ::  BOX(MWST)
!
! --- Template curve
!
      DATA ( X4(NN), Y4(NN), NN=1,MP ) &
     &     / -0.0,  1.2, &
     &        0.1,  0.7, &
     &        0.3,  0.15, &
     &        0.5,  0.5, &
     &        0.7,  0.1, &
     &        0.9,  0.2, &
     &        1.4,  0.3 &
     &     /
!
      ICLR = DIAGI_S%ICLR
      ILST = DIAGI_S%ILST(ICLR)
      IPST = DIAGI_S%IPST(ICLR)
      IWST = DIAGI_S%IWST(ICLR)
      IF ( IWST .GT. MWST ) IWST = MWST
      IF ( IWST .LT. MWST ) IWST = 1
!
      CALL CLRCH ( MES )
      MES = 'Select line width'
      CALL PGSAVE ! 1
!
! --- Deleting previous window
!
      CALL PGERAS
!
! --- Setting new world coodrinates
!
      CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
      CALL PGSCI   ( 1   )
      CALL PGSCH   ( 2.5 )
      CALL PGSLW   ( 8   )
!
! --- Printing the prompt
!
      XC = 0.50
      YC = 0.66
      CALL PGPTXT  ( XC, YC, 0.5, 0.5, MES(1:I_LEN(MES)) )
!
      CALL PGSAVE ! 2L
      SIZ = 1.0/(2*MWST+1)
      YC  = 0.33
!
! --- Determine the radii of the circle
!
      IF ( IPST .EQ. 2  .OR. IPST .EQ. 4 ) THEN
           RAD_MM = 0.7
           NPTS   = 16
         ELSE IF ( IPST .EQ. 3 .OR. IPST .EQ. 5 ) THEN
           RAD_MM = 1.2
           NPTS   = 32
      END IF
      IF ( DIAGI_S%XRIGHT > 600.0 ) RAD_MM = 1.6*RAD_MM
      XRAD_WC = RAD_MM/((DIAGI_S%XRIGHT - DIAGI_S%XLEFT)*SIZ)
      YRAD_WC = RAD_MM/((DIAGI_S%YTOP - DIAGI_S%YBOT)*SIZ)
!
! --- Setting the size of the plotting area in device coordinate
!
      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
!
      DO 410 J1=1,MWST
!
! ------ Specifing the corner of the boxes to be displayed
!
         BOX(J1)%XLB = SIZ*((J1-1)*2+1)
         BOX(J1)%YLB = YC-SIZ/2.0
         BOX(J1)%XTU = SIZ*((J1-1)*2+1)+SIZ
         BOX(J1)%YTU = YC+SIZ/2.0
         CALL PGSAVE ! 3A
!
! ------ Settiing filling colour
!
         IF ( J1 .EQ. IWST ) THEN
              CALL PGSCI  ( 4 )  ! current line width
            ELSE
              CALL PGSCI  ( 3 )  ! other line width
         END IF
!
! ------ Printing the box with point style. Firstly as filled rectangular
!
         CALL PGSFS  ( 1 )
         CALL PGRECT ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
!
! ------ ... then as outlined rectangular
!
         CALL PGSFS  ( 2 )
         CALL PGSLW  ( 1 )
         CALL PGSCI  ( 1 )
         CALL PGRECT ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
!
         CALL PGUNSA ! 3A
         CALL PGSAVE ! 3B
!
! ------ Printing explaining text
!
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
         CALL PGSCF  ( 3 )
         CALL PGSLW  ( 4 )
         CALL PGSCH  ( 1.5 )
         CALL PGPTXT ( BOX(J1)%XLB+SIZ/2., BOX(J1)%YLB-SIZ/2., 0.5, 0.5, &
     &                 'WST='//STR(1:1) )
!
         CALL PGSVP  ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
         CALL PGSCI  ( ITAB_CLR(ICLR,1) )
!
! ------ Drawing the line
!
         IF ( ILST .EQ. 1 ) THEN
!
! ----------- Point by point style
!
              CALL PGSLW  ( 1 )
              CALL PGSCH  ( 0.1 )
              DO 420 J2=1,MP
                 IF ( IPST .EQ. 1 ) THEN
                      CALL PGPNTS ( 1, X4(J2), Y4(J2), 1, 1 )
                 END IF
 420          CONTINUE
            ELSE IF ( ILST .EQ. 2 ) THEN
!
! ----------- Piece-wise linear line
!
              CALL PGSLW  ( DIAGI_S%IWD_LINS(J1) )
              CALL PGLINE ( MP, X4, Y4    )
            ELSE IF ( ILST .EQ. 3 ) THEN
!
! ----------- Cubic spline
!
              CALL COPY_R4_R8 ( MP, X4, X8 )
              CALL COPY_R4_R8 ( MP, Y4, Y8 )
              X4_MIN = X8(1)
              X4_MAX = X8(MP)
              IUER = -1
              CALL DIAGI_SPL ( MP, X8, Y8, X4_MIN, X4_MAX, NSP, XSPL, YSPL, &
     &                         IUER )
              CALL PGSLW     ( DIAGI_S%IWD_LINS(J1)   )
              CALL PGLINE    ( NSP, XSPL, YSPL )
         END IF
!
! ------ Now drawing points
!
         CALL PGSLW ( 1 )
         DO 430 J3=1,MP
            IF ( IPST .EQ. 2  .OR.  IPST .EQ. 3  ) THEN
!
! -------------- Outlined circles
!
                 CALL PGSFS ( 1 )
                 CALL PGSCI ( 0 )
                 CALL PGCIRC_PET ( NPTS, X4(J3), Y4(J3), XRAD_WC, YRAD_WC )
                 CALL PGSFS ( 2  )
                 CALL PGSCI ( ITAB_CLR(ICLR,1) )
                 CALL PGCIRC_PET ( NPTS, X4(J3), Y4(J3), XRAD_WC, YRAD_WC )
               ELSE IF ( IPST .EQ. 4  .OR.  IPST .EQ. 5 ) THEN
!
! -------------- Filled circles
!
                 CALL PGSFS ( 1  )
                 CALL PGSCI ( ITAB_CLR(ICLR,1) )
                 CALL PGCIRC_PET ( NPTS, X4(J3), Y4(J3), XRAD_WC, YRAD_WC )
            END IF
 430     CONTINUE
         CALL PGUNSA ! 3B
         CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0  )
         CALL PGSCI   ( 1 )
 410  CONTINUE
      CALL PGUNSA ! 2
!
! --- Position cursor in the box with the current line width
!
      XC =  BOX(IWST)%XLB + ( BOX(IWST)%XTU - BOX(IWST)%XLB )/3.
      YC =  BOX(IWST)%YLB + ( BOX(IWST)%YTU - BOX(IWST)%YLB )/3.
!
! --- Waiting for user reaction
!
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
      CALL PGUNSA ! 1
!
! --- Determine: which box has been specified
!
      IF ( CH .EQ. '1'  .OR.  CH .EQ. '2'  .OR.  CH .EQ. '3' ) THEN
           CALL CHIN ( CH, IWST )
         ELSE IF ( CH .NE. 'X' ) THEN
           IDI = DIAGI_INBOX ( MWST, BOX, XC, YC )
           IF ( IDI .GT. 0 ) IWST = IDI
      END IF
!
! --- At last: updating line width field
!
      DIAGI_S%IWST(ICLR) = IWST
!
      RETURN
      END  !#!  DIAGI_WST  #!#
