      FUNCTION PIMA_SELSTA ( MES, PIM, ISTA_LAST, ISTA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_SELSTA  allows user to chose the station uging DiaGI *
! *   interface. New PGPLOT X-window will be created. The list of        *
! *   used in solution stations will be displayed as colored boxes.      *
! *   The boxes with previously selected (visited) stations will be      *
! *   dipslayed by a bit different color. User is able to select the     *
! *   station using mouse or keyboard. Index of the selected station     *
! *   will be returned.                                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MES ( CHARACTER ) -- Lines with message to printed at the top  *
! *                            of window.                                *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! * ISTA_LAST ( INTEGER*4 ) -- Index of the station which has been       *
! *                            selected at the previous call of          *
! *                            PIMA_SELSTA or 0 if this call of          *
! *                            PIMA_SELSTA is the first call.            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <PIMA_SELSTA> ( LOGICAL*4 ) -- If .TRUE. -- user has chosen the      *
! *                            station. If .FALSE. User terminated       *
! *                            execution by the command 'X' without      *
! *                            selection new station.                    *
! *     ISTA ( INTEGER*4 ) -- Index of the selected station in the       *
! *                           genreal list of stations.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ###  02-JUN-1998  PIMA_SELSTA   v1.0 (c) L. Petrov  08-AUG-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'pima.i'
      INCLUDE    'diagi.i'
      TYPE     ( PIMA__TYPE  ) ::  PIM
      TYPE     ( DIAGI_BOXES ) ::  BOX_STA(PIM__MSTA)
      TYPE     ( DIAGI_STRU  ) ::  DIAGI_S
      INTEGER*4  ISTA, ISTA_LAST, IUER
      LOGICAL*4  PIMA_SELSTA
      INTEGER*4  COL_STA(3,2), ICOL_STA(2), N1$ARG, N2$ARG, IER
!
! --- Define colors
!
      DATA       ( (  COL_STA(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_STA(N2$ARG), N2$ARG=1,2           ) &
     &           / &
     &               220, 209, 185,  11,     220, 194, 133,  12 &   ! ( 41;40-100)
     &           /
      INTEGER*4  MBX
      PARAMETER  ( MBX = 20 )
      REAL*4     XCT, YCT, XCB, YCB, SIZV_STA, SIZH_STA, XC, YC, &
     &           XLF, XRF, YBF, YTF, SIFR_H, SIFR_V
      PARAMETER  ( SIFR_H = 0.003 )
      PARAMETER  ( SIFR_V = 0.005 )
      PARAMETER  ( XCT = 0.50, YCT = 0.95 ) ! Title coordinates
      PARAMETER  ( XCB = 0.02, YCB = 0.90 ) ! Bottom line coorinates
      PARAMETER  ( XLF = 0.02, XRF = 0.98 ) ! Left angle of the frame coord.
      PARAMETER  ( YBF = 0.02, YTF = 0.91 ) ! Rightt angle of the frame coord.
      LOGICAL*4  FL_SAVE_1
      CHARACTER  MES*(*), STR*80, CH*1
      INTEGER*4  ID_XW, IR, J1, ID, NB, INEW_ROW, IOLD_ROW
      INTEGER*4  DIAGI_INBOX, PGOPEN, I_LEN
!
! --- Setting plotting parameters
!
      CALL DIAGI_SET ( 1, DIAGI_S )
!
! --- Openning X-window plotting device
!
      ID_XW = PGOPEN ( DIAGI_S%DEVICE )
      IF ( ID_XW .LE. 0 ) THEN
           CALL CLRCH   (        STR )
           CALL INCH    ( ID_XW, STR )
           CALL ERR_LOG ( 6261, IUER, 'PIMA_SELSTA', 'Error in openning '// &
     &         'the graphic device '//DIAGI_S%DEVICE//' IER='//STR )
           RETURN
      END IF
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 0
      CALL DIAGI_CLS ( DIAGI_S, IER )
!
      CALL PGCOL_RGB ( ICOL_STA(1), COL_STA(1,1), COL_STA(2,1), COL_STA(3,1) )
      CALL PGCOL_RGB ( ICOL_STA(2), COL_STA(1,2), COL_STA(2,2), COL_STA(3,2) )
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
      ISTA = ISTA_LAST
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
! ------ Setting the sizes of the station rectangulars
!
         SIZV_STA = (YCB-YBF)/MBX
         SIZH_STA = (XRF-XCB)/6
!
         CALL PGSVP ( 0.0, 1.0, 0.0, 1.0  )
         INEW_ROW = 0
         IOLD_ROW = 0
 920     CONTINUE
!
! ------ Setting maximum device independent coordiantes
!
         NB  = 0
         DO 410 J1=1,PIM%NSTA
            IR = J1
            NB = NB + 1
            IF ( INEW_ROW .GT. 0  .AND.  J1 .NE. INEW_ROW  .AND. &
     &                                   J1 .NE. IOLD_ROW         ) GOTO 410
!
! --------- Specifing the corner of the NB-th box
!
            BOX_STA(NB)%XLB = XCB + SIZH_STA*2.5
            BOX_STA(NB)%YLB = YCB - SIZV_STA*IR
            BOX_STA(NB)%XTU = XCB + SIZH_STA*3.5
            BOX_STA(NB)%YTU = YCB - SIZV_STA*(IR-0.9)
!
            CALL PGSAVE() ! 2A
            IF ( ISTA .EQ. J1 ) THEN
                 CALL PGSCI  ( ICOL_STA(2) )
               ELSE
                 CALL PGSCI  ( ICOL_STA(1) )
            END IF
!
! --------- Printing the box
!
            CALL PGSLW  ( 1 )
            CALL PGSFS  ( 1 )
            CALL PGRECT ( BOX_STA(NB)%XLB, BOX_STA(NB)%XTU, &
     &                    BOX_STA(NB)%YLB, BOX_STA(NB)%YTU )
!
! --------- Filling the box by appropriate colour
!
            CALL PGSCI  ( 1 )
            CALL PGSFS  ( 2 )
            CALL PGRECT ( BOX_STA(NB)%XLB, BOX_STA(NB)%XTU, &
     &                    BOX_STA(NB)%YLB, BOX_STA(NB)%YTU )
!
! --------- Temporary setting world coordinate space for the station box
!
            CALL PGSVP  ( BOX_STA(NB)%XLB, BOX_STA(NB)%XTU, &
     &                    BOX_STA(NB)%YLB, BOX_STA(NB)%YTU )
!
            CALL PGSCH   ( 1.4 )
            CALL PGSLW   ( 5   )
!
! --------- Printing name of the satation
!
            CALL PGPTXT  ( 0.1, 0.2, 0.0, 0.0, PIM%STA(J1)%IVS_NAME(1:8) )
!
! --------- Restoring world coordinated space
!
            CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
            CALL PGUNSA() ! 2A
 410     CONTINUE
         INEW_ROW = 0  ! print all
         IOLD_ROW = 0  ! boxes
!
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
              PIMA_SELSTA = .FALSE.
              GOTO 810
         END IF
         ID = DIAGI_INBOX ( NB, BOX_STA, XC, YC )
         IF ( ID .GT. 0 ) THEN
              IOLD_ROW = ISTA
              ISTA = ID
              XC = (BOX_STA(ID)%XTU + BOX_STA(ID)%XLB)/2.0
              YC = (BOX_STA(ID)%YTU + BOX_STA(ID)%YLB)/2.0
              INEW_ROW = ID
              PIMA_SELSTA = .TRUE.
              IF ( CH .EQ. 'D' ) GOTO 810
!
! ----------- Special trick: we will not repaint all boxes but we repaint only
! ----------- modified ones. It accelerate the work of the prlogram
! ----------- substantially
!
              GOTO 920
            ELSE
              PIMA_SELSTA = .TRUE.
              IF ( CH .EQ. 'D' ) GOTO 810
              GOTO 810
         END IF
      GOTO 910  ! Infinte loop
 810  CONTINUE
      CALL PGERAS()
      CALL PGENDQ()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  PIMA_SELSTA  !#!#
