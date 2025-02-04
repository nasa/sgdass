      SUBROUTINE REPA_CHANGE_PAR ( ID_XS, MES, L_PAR, C_PAR, IND_CUR )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_CHANGE_PAR provides a user interface for change of    *
! *   the argument or the value for plotting the function in the context *
! *   of REPA. A menu is displayed with use of PGPLOT and a user is      *
! *   encouraged to select a box with the new argument or value.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    ID_XS ( INTEGER*4 ) -- Index of the plotting device according to  *
! *                           diagi.i                                    *
! *      MES ( CHARACTER ) -- The string with the message displayed at   *
! *                           the top of the page.                       *
! *    L_PAR ( INTEGER*4 ) -- The number of boxes with parameter names.  *
! *    C_PAR ( CHARACTER ) -- Array of parameter names. Dimension: L_PAR.*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IND_CUR ( INTEGER*4 ) -- Index of the current parameter.            *
! *                                                                      *
! * ### 13-DEC-2004  REPA_CHANGE_PAR  v1.1 (c) L. Petrov 20-MAR-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      INTEGER*4  ID_XS, L_PAR, IND_CUR
      CHARACTER  MES*(*), C_PAR(L_PAR)*(*)
!
      TYPE     ( DIAGI_STRU  ) :: DIAGI_S
      TYPE     ( DIAGI_BOXES ) :: BOX_TRI(L_PAR)
      INTEGER*4  ITRI(3), ITRI_LAST(3)
      INTEGER*4  COL_TRI(3,2), ICOL_TRI(2), N1$ARG, N2$ARG
!
! --- Define colors
!
      DATA       ( (  COL_TRI(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_TRI(N2$ARG), N2$ARG=1,2           ) &
     &           / &
     &               220, 209, 185,  11,    220, 194, 133,  12 &   ! ( 41;40-100)
     &           /
      INTEGER*4  MBX
      PARAMETER  ( MBX = 12 )
      REAL*4     XCT, YCT, XCB, YCB, SIZV_BOX, SIZH_BOX, DLMH_BOX, XC, YC, &
     &           XLF, XRF, YBF, YTF, XHL, YHL, SIFR_H, SIFR_V
      PARAMETER  ( SIFR_H = 0.003 )
      PARAMETER  ( SIFR_V = 0.005 )
      PARAMETER  ( XCT = 0.50, YCT = 0.95 ) ! Title coordinates
      PARAMETER  ( XCB = 0.02, YCB = 0.90 ) ! Bottom line coorinates
      PARAMETER  ( XLF = 0.02, XRF = 0.98 ) ! Left angle of the frame coord.
      PARAMETER  ( YBF = 0.02, YTF = 0.91 ) ! Rightt angle of the frame coord.
      PARAMETER  ( XHL = 0.10, YHL = 0.02 ) ! Bottom line of help
      PARAMETER  ( SIZH_BOX = 0.28 ) ! horizontal box size
      PARAMETER  ( DLMH_BOX = 0.06 ) ! horizontal space between boxes
      LOGICAL*4  FL_SAVE_1
      CHARACTER  STR*80, CH*1
      INTEGER*4  ID_XW, IC, IR, IER
      INTEGER*4  J1, J2, ID
      INTEGER*4, EXTERNAL :: DIAGI_INBOX, PGOPEN, I_LEN, IFIND_PL
!
!      MES = "Change plot's argument"
!
! --- Setting plotting parameters
!
      CALL DIAGI_SET ( ID_XS, DIAGI_S )
!
! --- Openning X-window plotting device
!
      ID_XW = PGOPEN ( DIAGI_S%DEVICE )
      IF ( ID_XW .LE. 0 ) THEN
           CALL CLRCH   (        STR )
           CALL INCH    ( ID_XW, STR )
           CALL ERR_LOG ( 6221, -1, 'REPA_CHANGE_PAR', 'Error in openning '// &
     &         'the graphic device '//DIAGI_S%DEVICE//' IER='//STR )
           RETURN
      END IF
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 0
      CALL DIAGI_CLS ( DIAGI_S, IER )
!
      CALL PGCOL_RGB ( ICOL_TRI(1), COL_TRI(1,1), COL_TRI(2,1), COL_TRI(3,1) )
      CALL PGCOL_RGB ( ICOL_TRI(2), COL_TRI(1,2), COL_TRI(2,2), COL_TRI(3,2) )
!
! --- Setting default font type
!
      CALL PGSCF  ( 2 )
      CALL PGERAS()       ! Erase the screen
!
      XC =  0.5
      YC = (YCT + YCB)/2.0
!
! --- Initialization
!
 910  CONTINUE
         CALL PGBBUF()
         CALL PGSAVE() ! 1
         FL_SAVE_1 = .TRUE.
!
! ------ Setting new world coodrinates
!
         CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0 )
         CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
         CALL PGSCI   ( 1   )
!
! ------ Printing the banner
!
         CALL PGSCH   ( 2.0 )
         CALL PGSLW   ( 8   )
         CALL PGPTXT  ( XCT, YCT, 0.0, 0.5, MES(1:I_LEN(MES)) )
!
         CALL PGSCH   ( 1.2 )
         CALL PGSLW   ( 1   )
         CALL PGPTXT  ( XHL, YHL, 0.0, 0.0, 'Left mouse button to change '// &
     &                  'parameter, Right mouse button to exit'  )
!
         SIZV_BOX = (YCB-YBF)/MBX
!
         CALL PGSVP ( 0.0, 1.0, 0.0, 1.0  )
 920     CONTINUE
!
! ------ Setting maximum device independent coordiantes
!
         DO 410 J1=1,L_PAR
            IR = J1/3 + MIN ( 1, MOD(J1,3) )
            IC = J1 - (IR-1)*3
!
! --------- Specifing the corner of the J1-th box
!
            BOX_TRI(J1)%XLB = XCB + (IC-1)*(SIZH_BOX+DLMH_BOX)
            BOX_TRI(J1)%YLB = YCB - SIZV_BOX*IR
            BOX_TRI(J1)%XTU = BOX_TRI(J1)%XLB + SIZH_BOX
            BOX_TRI(J1)%YTU = YCB - SIZV_BOX*(IR-0.667)
!
            CALL PGSAVE() ! 2A
            IF ( IND_CUR .EQ. J1 ) THEN
                 CALL PGSCI  ( ICOL_TRI(2) )
                ELSE
                 CALL PGSCI  ( ICOL_TRI(1) )
            END IF
!
! --------- Drawing the box
!
            CALL PGSLW  ( 1 )
            CALL PGSFS  ( 1 )
            CALL PGRECT ( BOX_TRI(J1)%XLB, BOX_TRI(J1)%XTU, &
     &                    BOX_TRI(J1)%YLB, BOX_TRI(J1)%YTU )
!
! --------- Filling the box by appropriate colour
!
            CALL PGSCI  ( 1 )
            CALL PGSFS  ( 2 )
            CALL PGRECT ( BOX_TRI(J1)%XLB, BOX_TRI(J1)%XTU, &
     &                    BOX_TRI(J1)%YLB, BOX_TRI(J1)%YTU )
!
! --------- Temporary setting world coordinate space for the parameter box
!
            CALL PGSVP  ( BOX_TRI(J1)%XLB, BOX_TRI(J1)%XTU, &
     &                    BOX_TRI(J1)%YLB, BOX_TRI(J1)%YTU )
!
            IF ( ID_XS .EQ. 1 ) THEN
                 CALL PGSCH ( 0.8 )
               ELSE 
                 CALL PGSCH ( 0.9 )
            END IF
            CALL PGSLW ( 2   )
!
! --------- Printing name of the parameter
!
            CALL PGPTXT  ( 0.02, 0.33, 0.0, 0.0, C_PAR(J1) )
!
! --------- Restoring world coordinated space
!
            CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
            CALL PGUNSA() ! 2A
 410     CONTINUE
         CALL PGSFS  ( 1 )
!
         IF ( FL_SAVE_1 ) THEN
              CALL PGUNSA() ! 1
              CALL PGEBUF()
              FL_SAVE_1 = .FALSE.
         END IF
         CALL PGUPDT()
 930     CONTINUE
!
! ------ Asking user the input
!
         CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
! ------ Then analysis: in which box the cursor is pointing
!
         IF ( CH .EQ. 'x' ) CH = 'X'
         IF ( CH .EQ. 'q' ) CH = 'Q'
         IF ( CH .EQ. 'X'  .OR.  CH .EQ. 'Q' ) THEN
              GOTO 810
         END IF
         ID = DIAGI_INBOX ( L_PAR, BOX_TRI, XC, YC )
         IF ( ID .GT. 0 ) THEN
              IR = ID/3 + MIN ( 1, MOD(ID,3) )
              IC = ID - (IR-1)*3
              DO 420 J2=1,3
                 IF ( J2 .NE. IC  .AND. ITRI(J2) .EQ. IR ) GOTO 930
 420          CONTINUE
              XC = (BOX_TRI(ID)%XTU + BOX_TRI(ID)%XLB)/2.0
              YC = (BOX_TRI(ID)%YTU + BOX_TRI(ID)%YLB)/2.0
              IND_CUR = ID
!
! ----------- Special trick: we will not repaint all boxes but we repaint only
! ----------- modified ones. It accelerate the work of the prlogram
! ----------- substantially
!
              GOTO 920
            ELSE
              GOTO 810
         END IF
      GOTO 910  ! Infinte loop
 810  CONTINUE
      CALL PGERAS()
      CALL PGENDQ()
!
      RETURN
      END  SUBROUTINE REPA_CHANGE_PAR
