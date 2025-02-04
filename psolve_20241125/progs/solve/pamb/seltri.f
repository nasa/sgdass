      FUNCTION SELTRI ( MES, DBOBJ, ITRI_LAST, ITRI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SELTRI  allows user to chose the triangle uging DiaGI     *
! *   interface. New PGPLOT X-window will be created. The list of        *
! *   the linear independent triangles used in solution will be          *
! *   displayed as colored boxes. The boxes with triangles previously    *
! *   selected (visited) will be dipslayed by a bit different color.     *
! *   User is able to select the triangle using mouse or keyboard.       *
! *   Index of the selected triangle will be returned.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MES ( CHARACTER ) -- Lines with message to printed at the top  *
! *                            of window.                                *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! * ITRI_LAST ( INTEGER*4 ) -- Index of the triangle which has been      *
! *                            selected at the previous call of          *
! *                            SELTRI or 0 if this call of               *
! *                            SELTRI is the first call.                 *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <SELTRI> ( LOGICAL*4 ) -- If .TRUE. -- user has chosen the triangle. *
! *                           If .FALSE. User terminated execution by    *
! *                           the command 'X' without selection new      *
! *                           triangle.                                  *
! *     ITRI ( INTEGER*4 ) -- Index of the selected triangle in the      *
! *                           genreal list of linear independent         *
! *                           triangles.                                 *
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
! *  ###  22-MAY-98     SELTRI     v1.2  (c)  L. Petrov  03-AUG-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      TYPE ( DIAGI_BOXES ) ::  BOX_TRI(3*MO_STA)
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*4  ITRI(3), ITRI_LAST(3), IUER
      LOGICAL*4  SELTRI
      INTEGER*4  COL_TRI(3,2), ICOL_TRI(2), N1$ARG, N2$ARG
!
! --- Define colors
!
      DATA       ( (  COL_TRI(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_TRI(N2$ARG), N2$ARG=1,2           ) &
     &           / &
     &               220, 209, 185,  11,      220, 194, 133,  12 &   ! ( 41;40-100)
     &           /
      INTEGER*4  MBX
      PARAMETER  ( MBX = 20 )
      REAL*4     XCT, YCT, XCB, YCB, SIZV_TRI, SIZH_TRI, XC, YC, &
     &           XLF, XRF, YBF, YTF, SIFR_H, SIFR_V
      PARAMETER  ( SIFR_H = 0.003 )
      PARAMETER  ( SIFR_V = 0.005 )
      PARAMETER  ( XCT = 0.50, YCT = 0.95 ) ! Title coordinates
      PARAMETER  ( XCB = 0.02, YCB = 0.90 ) ! Bottom line coorinates
      PARAMETER  ( XLF = 0.02, XRF = 0.98 ) ! Left angle of the frame coord.
      PARAMETER  ( YBF = 0.02, YTF = 0.91 ) ! Rightt angle of the frame coord.
      LOGICAL*4  FL_SAVE_1
      CHARACTER  MES*(*), STR*80, CH*1
      INTEGER*4  ID_XW, IC, IR
      INTEGER*4  J0, J1, J2, J3, ID, NB, INEW_COL, INEW_ROW, IOLD_ROW, &
     &           ISTA_REF(MO_STA), IER
      INTEGER*4  DIAGI_INBOX, PGOPEN, I_LEN, IFIND_PL
!
      DO 400 J0=1,DBOBJ%U_STA
         ISTA_REF(J0)=IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%UIS_STA(J0) )
 400  CONTINUE
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
           CALL ERR_LOG ( 6221, IUER, 'SELTRI', 'Error in openning '// &
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
      ITRI(1) = ITRI_LAST(1)
      ITRI(2) = ITRI_LAST(2)
      ITRI(3) = ITRI_LAST(3)
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
         SIZV_TRI = (YCB-YBF)/MBX
         SIZH_TRI = (XRF-XCB)/6
!
         CALL PGSVP ( 0.0, 1.0, 0.0, 1.0  )
         INEW_COL = 0
         INEW_ROW = 0
         IOLD_ROW = 0
 920     CONTINUE
!
! ------ Setting maximum device independent coordiantes
!
         NB  = 0
         DO 410 J1=1,DBOBJ%U_STA
            DO 420 J2=1,3
               IR = J1
               IC = J2
               NB = NB + 1
               IF ( INEW_COL .GT. 0  .AND.  J2 .NE. INEW_COL         ) GOTO 420
               IF ( INEW_ROW .GT. 0  .AND.  J1 .NE. INEW_ROW  .AND. &
     &                                      J1 .NE. IOLD_ROW         ) GOTO 420
!
! ------------ Specifing the corner of the NB-th box
!
               BOX_TRI(NB)%XLB = XCB + SIZH_TRI*(2*IC-1.5)
               BOX_TRI(NB)%YLB = YCB - SIZV_TRI*IR
               BOX_TRI(NB)%XTU = XCB + SIZH_TRI*(2*IC-0.5)
               BOX_TRI(NB)%YTU = YCB - SIZV_TRI*(IR-0.9)
!
               CALL PGSAVE() ! 2A
               IF ( ITRI(J2) .EQ. J1 ) THEN
                    CALL PGSCI  ( ICOL_TRI(2) )
                  ELSE
                    CALL PGSCI  ( ICOL_TRI(1) )
               END IF
!
! ------------ Printing the box
!
               CALL PGSLW  ( 1 )
               CALL PGSFS  ( 1 )
               CALL PGRECT ( BOX_TRI(NB)%XLB, BOX_TRI(NB)%XTU, &
     &                       BOX_TRI(NB)%YLB, BOX_TRI(NB)%YTU )
!
! ------------ Filling the box by appropriate colour
!
               CALL PGSCI  ( 1 )
               CALL PGSFS  ( 2 )
               CALL PGRECT ( BOX_TRI(NB)%XLB, BOX_TRI(NB)%XTU, &
     &                       BOX_TRI(NB)%YLB, BOX_TRI(NB)%YTU )
!
! ------------ Temporary setting world coordinate space for the station box
!
               CALL PGSVP  ( BOX_TRI(NB)%XLB, BOX_TRI(NB)%XTU, &
     &                       BOX_TRI(NB)%YLB, BOX_TRI(NB)%YTU )
!
               CALL PGSCH   ( 1.4 )
               CALL PGSLW   ( 5   )
!
! ------------ Printing name of the satation
!
               CALL PGPTXT  ( 0.1, 0.2, 0.0, 0.0, &
     &                        DBOBJ%C_STA( ISTA_REF(J1) )(1:8) )
!
! ------------ Restoring world coordinated space
!
               CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
               CALL PGUNSA() ! 2A
 420        CONTINUE
 410     CONTINUE
         INEW_COL = 0  ! Setting attrribute:
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
              SELTRI = .FALSE.
              GOTO 810
         END IF
         ID = DIAGI_INBOX ( NB, BOX_TRI, XC, YC )
         IF ( ID .GT. 0 ) THEN
              IR = ID/3 + MIN ( 1, MOD(ID,3) )
              IC = ID - (IR-1)*3
              DO 430 J3=1,3
                 IF ( J3 .NE. IC  .AND. ITRI(J3) .EQ. IR ) GOTO 930
 430          CONTINUE
              IOLD_ROW = ITRI(IC)
              ITRI(IC) = IR
              XC = (BOX_TRI(ID)%XTU + BOX_TRI(ID)%XLB)/2.0
              YC = (BOX_TRI(ID)%YTU + BOX_TRI(ID)%YLB)/2.0
              INEW_COL = IC
              INEW_ROW = IR
!
! ----------- Special trick: we will not repaint all boxes but we repaint only
! ----------- modified ones. It accelerate the work of the prlogram
! ----------- substantially
!
              GOTO 920
            ELSE
              SELTRI = .TRUE.
              GOTO 810
         END IF
      GOTO 910  ! Infinte loop
 810  CONTINUE
      CALL PGERAS()
      CALL PGENDQ()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SELTRI  #!#
