      SUBROUTINE DIAGI_PST ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_PST  bids user to change the point style for        *
! *   representation of the plot of the current colour. Field            *
! *   DIAGI_S.IPST(ICLR) is updated.                                     *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *                                                                      *
! *  ###  10-OCT-97    DIAGI_PST   v1.1  (c)  L. Petrov 08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  ICLR, IPST
      CHARACTER  MES*128, CH*1, STR*20
      INTEGER*4  J1, NPTS_C, IDI
      REAL*4     XC, YC, SIZ
      REAL*4     RAD_MM, XRAD_WC, YRAD_WC
      INTEGER*4  I_LEN, DIAGI_INBOX
      TYPE ( DIAGI_BOXES ) ::  BOX(MPST)
!
      ICLR = DIAGI_S%ICLR
      IPST = DIAGI_S%IPST(ICLR)
      IF ( IPST .GT. MPST ) IPST = MPST
      IF ( IPST .LT. 1    ) IPST = 1
!
      CALL CLRCH ( MES )
      MES = 'Select point style'
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
      SIZ = 1.0/(2*MPST+1)
      YC  = 0.33
      DO 410 J1=1,MPST
!
! ------ Specifing the corner of the boxes
!
         BOX(J1)%XLB = SIZ*((J1-1)*2+1)
         BOX(J1)%YLB = YC-SIZ/2.0
         BOX(J1)%XTU = SIZ*((J1-1)*2+1)+SIZ
         BOX(J1)%YTU = YC+SIZ/2.0
         CALL PGSAVE ! 3A
!
! ------ Settiing filling colour
!
         IF ( J1 .EQ. IPST ) THEN
              CALL PGSCI  ( 4 )  ! current point style
            ELSE
              CALL PGSCI  ( 3 )  ! other point-styles
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
!
! ------ Printing explaining text
!
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
         CALL PGSCF  ( 3 )
         CALL PGSLW  ( 4 )
         CALL PGSCH  ( 1.5 )
         CALL PGPTXT ( BOX(J1)%XLB+SIZ/2., BOX(J1)%YLB-SIZ/2., 0.0, 0.5, &
     &                 'PST='//STR(1:1) )
!
! ------ Determine the radii of the circle
!
         IF ( J1 .EQ. 2  .OR. J1 .EQ. 4 ) THEN
              RAD_MM = DIAGI_S%RAD_SMALL
              NPTS_C = NPTS_SMALL
           ELSE IF ( J1 .EQ. 3 .OR. J1 .EQ. 5 ) THEN
              RAD_MM = DIAGI_S%RAD_LARGE
              NPTS_C = NPTS_LARGE
         END IF
         IF ( DIAGI_S%XRIGHT > 600.0 ) RAD_MM = 1.6*RAD_MM
!
! ------ Determine the center point of the box
!
         XC = ( BOX(J1)%XLB + BOX(J1)%XTU )/2.
         YC = ( BOX(J1)%YLB + BOX(J1)%YTU )/2.
         CALL PGSAVE ! 3B
!
! ------ Printing the example of the point-style
!
         IF ( J1 .EQ. 1 ) THEN
              CALL PGSLW  ( 1 )
              CALL PGSCH  ( 0.1 )
              CALL PGPNTS ( 1, XC, YC, 1, 1 )
            ELSE IF ( J1 .EQ. 2  .OR.  J1 .EQ. 3 ) THEN
!
! ----------- Unfilled circles
!
              XRAD_WC = RAD_MM/(DIAGI_S%XRIGHT - DIAGI_S%XLEFT)
              YRAD_WC = RAD_MM/(DIAGI_S%YTOP   - DIAGI_S%YBOT)
!
! ----------- Filled white circle
!
              CALL PGSFS  ( 1  )
              CALL PGSCI  ( 0  )
              CALL PGCIRC_PET ( NPTS_C, XC, YC, XRAD_WC, YRAD_WC )
!
! ----------- Outlined circle
!
              CALL PGSFS  ( 2  )
              CALL PGSCI  ( ITAB_CLR(ICLR,1) )
              CALL PGCIRC_PET ( NPTS_C, XC, YC, XRAD_WC, YRAD_WC )
            ELSE IF ( J1 .EQ. 4  .OR.  J1 .EQ. 5 ) THEN
!
! ----------- filled circles
!
              XRAD_WC = RAD_MM/(DIAGI_S%XRIGHT - DIAGI_S%XLEFT)
              YRAD_WC = RAD_MM/(DIAGI_S%YTOP   - DIAGI_S%YBOT)
              CALL PGSFS  ( 1  )
              CALL PGSCI  ( ITAB_CLR(ICLR,1) )
              CALL PGCIRC_PET ( NPTS_C, XC, YC, XRAD_WC, YRAD_WC )
         END IF
         CALL PGUNSA ! 3B
 410  CONTINUE
      CALL PGUNSA ! 2
!
! --- Position cursor in the box with the current point-style
!
      XC =  BOX(IPST)%XLB + ( BOX(IPST)%XTU - BOX(IPST)%XLB )/3.
      YC =  BOX(IPST)%YLB + ( BOX(IPST)%YTU - BOX(IPST)%YLB )/3.
!
! --- Waiting for user reaction
!
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
      CALL PGUNSA ! 1
!
! --- Determine: which box has been specified
!
      IF ( CH .EQ. '1'  .OR.  CH .EQ. '2'  .OR.  CH .EQ. '3' .OR. &
     &     CH .EQ. '4'  .OR.  CH .EQ. '5'                         ) THEN
           CALL CHIN ( CH, IPST )
         ELSE IF ( CH .NE. 'X' ) THEN
           IDI = DIAGI_INBOX ( MPST, BOX, XC, YC )
           IF ( IDI .GT. 0 ) IPST = IDI
      END IF
      DIAGI_S%IPST(ICLR) = IPST
!
      RETURN
      END  !#!  DIAGI_PST  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   DIAGI_INBOX ( NBOX, BOX, XC, YC )
! ************************************************************************
! *                                                                      *
! *   Function  DIAGI_INBOX  analyses the set of NBOX boxes and          *
! *   determine in which box the point with coordinates XC, YC is.       *
! *   DIAGI_INBOX returns 0 in the case when the point with world        *
! *   coordinates is out of aby box, or the index of the box if the      *
! *   point is within one of them.                                       *
! *   World coorinates of the left bottom and right top corner of the    *
! *   boxes are specified in the asrray of data structures BOX. Boxes    *
! *   can overlap each other. In that case the index of the first fitted *
! *   box will be returned.                                              *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   NBOX ( INTEGER*4 ) -- Number of boxes.                             *
! *    BOX ( RECORD    ) -- Array of the data structures of boxes.       *
! *                         Dimension of NBUF. World coordinates of the  *
! *                         corners are specifeid in the data structure. *
! *     XC ( REAL*4    ) -- Current X-world coordinate of the cursor.    *
! *     YC ( REAL*4    ) -- Current Y-world coordinate of the cursor.    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <DIAGI_INBOX> ( INTEGER*4 ) -- Index of the first box where cursor   *
! *                                is pointing out or 0 if cursor is     *
! *                                pointing at the point aout of any box.*
! *                                                                      *
! *  ###  13-OCT-97   DIAGI_INBOX   v1.0 (c)  L. Petrov  13-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INTEGER*4  DIAGI_INBOX, NBOX, J1
      REAL*4     XC, YC
      TYPE ( DIAGI_BOXES ) ::  BOX(NBOX)
!
      DIAGI_INBOX = 0
      IF ( NBOX .GT. 0 ) THEN
           DO 410 J1=1,NBOX
              IF ( XC .GE. BOX(J1)%XLB  .AND.  XC .LE. BOX(J1)%XTU  .AND. &
     &             YC .GE. BOX(J1)%YLB  .AND.  YC .LE. BOX(J1)%YTU        ) THEN
                   DIAGI_INBOX = J1
                   RETURN
              END IF
 410       CONTINUE
      END IF
!
      RETURN
      END  !#!  DIAGI_INBOX  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PGCIRC_PET ( NPTS_I, XCENT, YCENT, XRADIUS, YRADIUS )
! ************************************************************************
! *                                                                      *
! *   Drawing the circle using PGPLOT. PGPLOT routine PGCIRC draws       *
! *   rather ellipses than circles.                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    NPTS_I ( INTEGER*4 ) -- Number of points for drawing.             *
! *     XCENT ( REAL*4    ) -- X-World cordinates of the center.         *
! *     YCENT ( REAL*4    ) -- Y-World cordinates of the center.         *
! *   XRADIUS ( REAL*4    ) -- X-radius (world coordinates).             *
! *   YRADIUS ( REAL*4    ) -- Y-radius (world coordinates).             *
! *                                                                      *
! *  ###  09-OCT-97   PGCIRC_PET   v1.1  (c)  L. Petrov 08-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      REAL*4      XCENT, YCENT, XRADIUS, YRADIUS
      PARAMETER ( MAXPTS=72 )
!
      INTEGER    NPTS_I, NPTS, I
      REAL*4    ANGLE
      REAL*4    X(MAXPTS),Y(MAXPTS)
!
      NPTS = MAX ( 8,      NPTS_I )
      NPTS = MIN ( MAXPTS, NPTS   )
      IF ( XRADIUS .LT. 1.D-30 ) XRADIUS = 1.D-30 ! In order to prevent
      IF ( YRADIUS .LT. 1.D-30 ) YRADIUS = 1.D-30 ! underrflow
      DO 10 I=1,NPTS
         ANGLE = I*360.0/REAL(NPTS)/57.3
         X(I) = XCENT + XRADIUS*COS(ANGLE)
         Y(I) = YCENT + YRADIUS*SIN(ANGLE)
   10 CONTINUE
      CALL PGPOLY ( NPTS, X, Y )
!
      END  !#!  PGCIRC_PET  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PGENTER ( PROMPT, XIN, YIN, TERMN, OUT, IL )
! ************************************************************************
! *                                                                      *
! *   Routine  PGENTER  accepts the line from PGPLOT interactive device. *
! *   It first draws at the screen the prompt line PROMPT starting from  *
! *   point with world coordinates XIN, YIN. (If the length of the       *
! *   PROMPT is zero , f.e. actual parameter was '' it will not do it.   *
! *   Then PGENTER accpets symbols from the PGPLOT iuntective device.    *
! *   It terminate the work if one of the events occur: 1) Enter symbol  *
! *   <Enter> will be hit; 2) the number of entered symbols reachs the   *
! *   length of OUT. Input will be aborted and emty line OUT will        *
! *   returned if user hit one symbol from the line of terminators.      *
! *   Entered line will be deleted from the screen after entering.       *
! *                                                                      *
! * _______________________ INPUT PARAMETERS: __________________________ *
! *                                                                      *
! * PROMPT ( CHARACTER ) -- The prompt which will be drawn before.       *
! *    XIN ( REAL*4    ) -- X world coordinate of the prompt. It will be *
! *                         X world coorinate of the first symbol of the *
! *                         entering line if PROMPT = ''                 *
! *    YIN ( REAL*4    ) -- Y world coordinate of the prompt. It will be *
! *                         Y alse world coorinate of the first symbol   *
! *                         of the entering line.                        *
! *  TERMN ( CHARACTER ) -- Line with possible terminators.              *
! *                                                                      *
! * _______________________ OUTPUT PARAMETERS: _________________________ *
! *                                                                      *
! *    OUT ( CHARACTER ) -- Output line.                                 *
! *    IL  ( CHARACTER ) -- Number of symbols in the the output line.    *
! *                                                                      *
! *  ###  10-OCT-97     PGENTER    v1.0  (c)  L. Petrov  10-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  PROMPT*(*), OUT*(*), TERMN, CH
      INTEGER*4  IL, J1, IABORT, ITL
      REAL*4     XIN, YIN, XL, YL, XC, YC, XB, X_DUMMY, Y_DUMMY
!
! --- Drawing the prompt line
!
      IF ( LEN(PROMPT) .GT. 0 ) THEN
           CALL PGPTXT  ( XIN, YIN, 0.0, 0.0, PROMPT )
           CALL PGLEN   ( 4, PROMPT, XL, Y_DUMMY )
           XC = XIN + XL
        ELSE
           XC = XIN
      END IF
!
! --- Initialization
!
      IL=0
      YC = YIN
      XL=0.0
      YL=0.0
      XB=XC
      IABORT=0
      ITL = LEN(TERMN)
!
! --- Infinite loop for entering symbols
!
      DO 410 J1=1,1048576
!
! ------ Calculate the position of the cursor
!
         X_DUMMY = XC  + XL
         Y_DUMMY = YIN + YL/2
!
! ------ Awaiting the new symbol
!
         CALL PGBAND ( 0, 1, XC, YC, X_DUMMY, Y_DUMMY, CH )
         IF ( ITL .GT. 0 ) THEN
              IF ( INDEX ( TERMN, CH ) .NE. 0 ) THEN
!
! ---------------- One of the terminators has been hit. Abort the routine
!
                   IABORT=1
                   GOTO 810
              END IF
         END IF
         IF ( ICHAR(CH) .EQ. 13 ) THEN
!
! ----------- <Enter> has been hit
!
              GOTO 810
           ELSE IF ( ( ICHAR(CH) .EQ. 8  .OR.  ICHAR(CH) .EQ. 127 ) .AND. &
     &               IL .GE. 1 ) THEN
!
! ----------- Backspace hes been hit. Calculate the length of the previous
! ----------- character
!
              CALL PGLEN   ( 4, OUT(IL:IL), XL, Y_DUMMY )
              XC = XC - XL
!
! ----------- Deleting the previous symbol
!
              CALL PGSAVE
              CALL PGSCI   ( 0    )
              CALL PGPTXT  ( XC, YC, 0.0, 0.0, OUT(IL:IL) )
              CALL PGUNSA
              OUT(IL:IL) = ' '
              IL = IL - 1
           ELSE IF ( ICHAR(CH) .GE. 32 .AND. ICHAR(CH) .LE. 126 ) THEN
!
! ----------- Printable symbol has bin entered. Draw it at the screen
!
              CALL PGPTXT  ( XC, YC, 0.0, 0.0, CH )
!
! ----------- Calculate its size and update cursor position
!
              CALL PGLEN   ( 4, CH, XL, YL )
              XC = XC + XL
!
! ----------- Increment symol counter for the output string
!
              IL = IL+1
              OUT(IL:IL)=CH
              IF ( IL .EQ. LEN(OUT) ) GOTO 810
         END IF
 410  CONTINUE
!
! --- End of work
!
 810  CONTINUE
!
! --- Delete the line from the screen
!
      CALL PGSAVE
      CALL PGSCI   ( 0    )
!
! --- Firstly delete the prompt
!
      IF ( LEN(PROMPT) .GT. 0 ) THEN
           CALL PGPTXT  ( XIN, YIN, 0.0, 0.0, PROMPT )
      END IF
!
! --- Then delete output line
!
      IF ( IL .GE. 1 ) THEN
           CALL PGPTXT  ( XB, YIN, 0.0, 0.0, OUT(1:IL) )
      END IF
      CALL PGUNSA
      IF ( IABORT .EQ. 1 ) THEN
!
! -------- If abort took place then clean output line
!
           CALL CLRCH ( OUT )
           IL = 0
      END IF
!
      RETURN
      END  !#!  PGENTER  #!#
