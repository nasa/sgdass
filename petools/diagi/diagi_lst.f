      SUBROUTINE DIAGI_LST ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_LST  bids user to change the line style             *
! *   representation of the plot of the current colour. Field            *
! *   DIAGI_S.ILST(ICLR) is updated.                                     *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *                                                                      *
! *  ###  13-OCT-97    DIAGI_LST   v1.2  (c)  L. Petrov 08-AUG-2002 ###  *
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
      TYPE ( DIAGI_BOXES ) ::  BOX(MLST)
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
      IF ( ILST .GT. MLST ) ILST = MLST
      IF ( ILST .LT. 1    ) ILST = 1
!
      CALL CLRCH ( MES )
      MES = 'Select line style'
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
      CALL PGPTXT  ( XC, YC, 0.0, 0.5, MES(1:I_LEN(MES)) )
!
      CALL PGSAVE ! 2
      SIZ = 1.0/(2*MLST+1)
      YC  = 0.33
!
! --- Determine the radii of the circle
!
      IF ( IPST .EQ. 2  .OR. IPST .EQ. 4 ) THEN
           RAD_MM = DIAGI_S%RAD_SMALL
           NPTS   = NPTS_SMALL
         ELSE IF ( IPST .EQ. 3 .OR. IPST .EQ. 5 ) THEN
           RAD_MM = DIAGI_S%RAD_LARGE
           NPTS   = NPTS_LARGE
      END IF
      IF ( DIAGI_S%XRIGHT > 600.0 ) RAD_MM = 1.6*RAD_MM
      XRAD_WC = RAD_MM/((DIAGI_S%XRIGHT - DIAGI_S%XLEFT)*SIZ)
      YRAD_WC = RAD_MM/((DIAGI_S%YTOP - DIAGI_S%YBOT)*SIZ)
!
! --- Some sorcery
!
      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
!
      DO 410 J1=1,MLST
!
! ------ Specifing the corner of the boxes
!
         BOX(J1)%XLB = SIZ*((J1-1)*2+1)
         BOX(J1)%YLB = YC-SIZ/2.0
         BOX(J1)%XTU = SIZ*((J1-1)*2+1)+SIZ
         BOX(J1)%YTU = YC+SIZ/2.0
         CALL PGSAVE ! 3A
!
! ------ Setting filling colour
!
         IF ( J1 .EQ. ILST ) THEN
              CALL PGSCI  ( 4 )  ! current point style
            ELSE
              CALL PGSCI  ( 3 )  ! other point-styles
         END IF
!
! ------ Printing the box with line style. Firstly as filled rectangular
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
         CALL PGPTXT ( BOX(J1)%XLB+SIZ/2., BOX(J1)%YLB-SIZ/2., 0.0, 0.5, &
     &                 'LST='//STR(1:1) )
!
         CALL PGSVP ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
!
! ------ Drawing template line
!
         CALL PGSCI ( ITAB_CLR(ICLR,1) )
         IF ( J1 .EQ. 1 ) THEN
              CALL PGSLW (   1 )
              CALL PGSCH ( 0.1 )
              DO 420 J2=1,MP
                 IF ( IPST .EQ. 1 ) THEN
                      CALL PGPNTS ( 1, X4(J2), Y4(J2), 1, 1 )
                 END IF
 420          CONTINUE
            ELSE IF ( J1 .EQ. 2 ) THEN
              CALL PGSLW  ( DIAGI_S%IWD_LINS(IWST) )
              CALL PGLINE ( MP, X4, Y4      )
            ELSE IF ( J1 .EQ. 3 ) THEN
              CALL COPY_R4_R8 ( MP, X4, X8 )
              CALL COPY_R4_R8 ( MP, Y4, Y8 )
              X4_MIN = X8(1)
              X4_MAX = X8(MP)
              IUER = -1
              CALL DIAGI_SPL ( MP, X8, Y8, X4_MIN, X4_MAX, NSP, XSPL, YSPL, &
     &                         IUER )
              CALL PGSLW     ( DIAGI_S%IWD_LINS(IWST) )
              CALL PGLINE    ( NSP, XSPL, YSPL )
         END IF
!
! ------ Drawing points in the box
!
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
! --- Position cursor in the box with the current line-style
!
      XC =  BOX(ILST)%XLB + ( BOX(ILST)%XTU - BOX(ILST)%XLB )/3.
      YC =  BOX(ILST)%YLB + ( BOX(ILST)%YTU - BOX(ILST)%YLB )/3.
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
           CALL CHIN ( CH, ILST )
         ELSE IF ( CH .NE. 'X' ) THEN
           IDI = DIAGI_INBOX ( MLST, BOX, XC, YC )
           IF ( IDI .GT. 0 ) ILST = IDI
      END IF
!
      DIAGI_S%ILST(ICLR)= ILST
!
      RETURN
      END  !#!  DIAGI_LST  #!#
