      SUBROUTINE SELSOU ( MES, DBOBJ, IMODE, LSEL_SOU, ISR_LAST, ISR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SELSOU  allows user to chose the source(s) using          *
! *   DiaGI interface. New PGPLOT X-window will be created. The list of  *
! *   the sources used in solution will be displayed as colored boxes.   *
! *   The boxes with sources previously selected (visited) will be       *
! *   displayed by a bit different color. User is able to select the     *
! *   sources using mouse or keyboard. Index of the last selected source *
! *   as well as arrays with selection/deselection status of all sources *
! *   will be returned.                                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      MES ( CHARACTER ) -- Lines with message to printed at the top   *
! *                           of window.                                 *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! *    IMODE ( INTEGER*4 ) -- Mode switcher. If IMODE=1 then SELSOU      *
! *                           work in one-source mode. When user         *
! *                           position cursor on the source and select   *
! *                           it, SELSOU terminate work and return index *
! *                           of the selected sources. When IMODE>1      *
! *                           SELSOU works in multi-source mode. Then it *
! *                           allows to select several sources:          *
! *                           min (IMODE, MBR ), where MBR is constant - *
! *                           number of rows. Hitting <Left_mouse> in    *
! *                           multi-source mode toggles                  *
! *                           selection/deselection status of the source *
! *                           to be pointed. Hitting <Middle_mouse>      *
! *                           causes printing the table of selected      *
! *                           sources. If then user click <Left_mouse>   *
! *                           or <Middle_mouse> SELSOU terminates its    *
! *                           work and return array LSEL_SOU which holds *
! *                           selection status of the sources. If user   *
! *                           clicks <Right_mouse> then SELSOU allows    *
! *                           to change selection/deselection status of  *
! *                           the sources once more.                     *
! * LSEL_SOU ( LOGICAL*4 ) -- Array with length of number of sources.    *
! *                           LSEL_SOU(k) .TRUE. that means that the     *
! *                           k-th source from the general list          *
! *                           DBOBJ.LIS_SOU (which includes all sources  *
! *                           where there were at least one observation  *
! *                           regardless of its suppression status) of   *
! *                           sources has been selected previously.      *
! * ISR_LAST ( INTEGER*4 ) -- Index of the source which has been         *
! *                           selected at the previous call of SELSOU or *
! *                           0 if this call of SELSOU is the first      *
! *                           call.                                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      ISR ( INTEGER*4 ) -- Index of the selected sources in the       *
! *                           general list of sources.                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  26-MAY-98     SELSOU     v1.1  (c)  L. Petrov  09-JUN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      TYPE ( DIAGI_BOXES ) ::  BOX_SOU(MO_SOU)
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*4  IMODE, ISR, ISR_LAST, IUER
      LOGICAL*4  LSEL_SOU(MO_SOU)
      INTEGER*4  COL_SOU(3,2), ICOL_SOU(2), ICOL_SNM, ICOL_NMR, &
     &           N1$ARG, N2$ARG, IER
!
! --- Define colors
!
      DATA       ( (  COL_SOU(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_SOU(N2$ARG), N2$ARG=1,2           ) &
     &           / &
     &               220, 209, 185,  29,      220, 194, 133,  30 &   ! ( 41;40-100)
     &           /
      INTEGER*4  ISOU(MO_SOU), MBR, MBC
      PARAMETER  ( MBR = 12 )
      PARAMETER  ( MBC =  8 )
      PARAMETER  ( ICOL_SNM = 31 )
      PARAMETER  ( ICOL_NMR = 32 )
      REAL*4     XCT, YCT, XCB, YCB, SIZV_SOU, SIZH_SOU, XC, YC, XCP, YCP, &
     &           XLF, XRF, YBF, YTF, SIFR_H, SIFR_V
      PARAMETER  ( SIFR_H = 0.003 )
      PARAMETER  ( SIFR_V = 0.005 )
      CHARACTER  MES*(*), STR*80, CH*1
      LOGICAL*4  FL_SAVE_1
      INTEGER*4  ID_XW, NPAGE, IPAGE, ISB, IRB, ICB, IRE, ICE, IFB
      INTEGER*4  J0, J1, J2, J3, I1, I2, ID, IS, ISS, NBX, IUSE_SOU
!
! --- Plotting constants
!
      PARAMETER  ( XCT = 0.50, YCT = 0.95 ) ! Title coordinates
      PARAMETER  ( XCB = 0.02, YCB = 0.90 ) ! Bottom line coorinates
      PARAMETER  ( XLF = 0.02, XRF = 0.98 ) ! Left angle of the frame coord.
      PARAMETER  ( YBF = 0.02, YTF = 0.91 ) ! Rightt angle of the frame coord.
      PARAMETER  ( XCP = 0.90, YCP = 0.95 ) ! "Page" texst coordinates
!
! --- Size of the source rectangular
!
      PARAMETER  ( SIZV_SOU = (YCB-YBF)/MBR ) ! Vertical size
      PARAMETER  ( SIZH_SOU = (XRF-XCB)/MBC ) ! horizontal size
!
      INTEGER*4  DIAGI_INBOX, IFIND_PL, PGOPEN, I_LEN, IDIV
      IDIV ( I1, I2 ) = I1/I2 + MIN ( 1, MOD ( I1, I2 ) )
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
           CALL ERR_LOG ( 6221, IUER, 'SELSOU', 'Error in openning '// &
     &         'the graphic device '//DIAGI_S%DEVICE//' IER='//STR )
           RETURN
      END IF
!
! --- Setting colours
!
      DIAGI_S%NCLR  = MBR
      DO 400 J0 =1,DIAGI_S%NCLR
         DIAGI_S%ICOL(J0) = J0 ! use colors from DIAGI internal tables
 400  CONTINUE
!
      CALL DIAGI_CLS ( DIAGI_S, IER )
!
! --- Setting additional colours
!
      CALL PGCOL_RGB ( ICOL_SOU(1), COL_SOU(1,1), COL_SOU(2,1), COL_SOU(3,1) )
      CALL PGCOL_RGB ( ICOL_SOU(2), COL_SOU(1,2), COL_SOU(2,2), COL_SOU(3,2) )
      CALL PGCOL_RGB ( ICOL_SNM,    IRGB_DEF(1,1,1), IRGB_DEF(1,1,2), &
     &                              IRGB_DEF(1,1,3)                  )
      CALL PGCOL_RGB ( ICOL_NMR,    IRGB_DEF(3,1,1), IRGB_DEF(3,1,2), &
     &                              IRGB_DEF(3,1,3)                  )
!
! --- Setting default font type
!
      CALL PGSCF     ( 2 )
!
! --- Determine how many pages will we have (NPAGE) and what will be the
! --- current page (IPAGE)
!
      NPAGE = IDIV ( DBOBJ%U_SOU, MBR*MBC )
      IPAGE = 1
      IF ( ISR_LAST .GT. 0  .AND.  ISR_LAST .LE. DBOBJ%L_SOU ) THEN
!
! -------- If previous source was selected then try to find appropriate
! -------- page whete it was located
!
           IPAGE = IDIV ( ISR_LAST, MBR*MBC )
      END IF
!
! --- Initialization
!
      IUSE_SOU = 0
      ISR = 0
!
 910  CONTINUE
         CALL PGERAS()  ! erase the screen
         CALL PGBBUF()  ! starting buffering
         CALL PGSAVE()  ! 1
         FL_SAVE_1 = .TRUE.
!
! ------ Setting new world coodrinates
!
         CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0 )
         CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
         CALL PGSCI   ( 1   )
!
! ------ Printing the upper banner
!
         CALL PGSCH   ( 2.0 )
         CALL PGSLW   ( 8   )
         CALL PGPTXT  ( XCT, YCT, 0.0, 0.5, MES(1:I_LEN(MES)) )
!
         CALL CLRCH ( STR )
         CALL INCH  ( IPAGE, STR )
         STR(I_LEN(STR)+1:)='('
         CALL INCH  ( NPAGE, STR(I_LEN(STR)+1:) )
         STR(I_LEN(STR)+1:)=')'
!
! ------ Printing the number of displayed page
!
         STR = 'page '//STR
         CALL PGSCH   ( 1.0 )
         CALL PGSLW   ( 4    )
         CALL PGPTXT  ( XCP, YCP, 0.0, 0.0, STR(1:I_LEN(STR)) )
!
         XC = 0.5
         YC = 0.5
!
! ------ Setting coordinate space once more
!
         CALL PGSVP ( 0.0, 1.0, 0.0, 1.0  )
!
! ------ Setting first (ICB) and the last (ICE) column number
!
         ICB = 1
         ICE = IDIV ( DBOBJ%U_SOU - (IPAGE-1)*MBR*MBC, MBR )
         IF ( ICE .GT. MBC ) ICE = MBC
!
! ------ Setting first (IRB) and the last (IRE) row number
!
         IRB  = 1
         IRE  = DBOBJ%U_SOU - (IPAGE-1)*MBR*MBC
         IF ( IRE .GT. MBR ) IRE = MBR
         ISS = 0
 920     CONTINUE
!
         NBX  = 0
         DO 410 J1=ICB,ICE
            DO 420 J2=IRB,IRE
!
! ------------ Find indeces of the next source among all sourcees (ISB) and
! ------------ among selected in solution sources (IFB)
!
               IS  = (IPAGE-1)*MBR*MBC + (J1-1)*MBR + J2
               ISB = IFIND_PL ( DBOBJ%L_SOU, DBOBJ%LIS_SOU, IS )
               IFB = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, IS )
!
               IF ( IFB .GT. 0 ) THEN
!
! --------------- Source was observed? Create a new box for it!
!
                  NBX = NBX + 1
                  ISOU(NBX) = ISB
                  IF ( IUSE_SOU .GT. 0  .AND. IUSE_SOU .NE. ISB ) GOTO 420
!
! --------------- Specifing the corner of the NBX-th box
!
                  BOX_SOU(NBX)%XLB = XCB + SIZH_SOU*(J1-1.0)
                  BOX_SOU(NBX)%YLB = YCB - SIZV_SOU*J2
                  BOX_SOU(NBX)%XTU = XCB + SIZH_SOU*(J1-0.1)
                  BOX_SOU(NBX)%YTU = YCB - SIZV_SOU*(J2-0.9)
                  IF ( ISB .EQ. ISR_LAST ) THEN
                       XC = ( BOX_SOU(NBX)%XLB + BOX_SOU(NBX)%XTU )/2.
                       YC = ( BOX_SOU(NBX)%YLB + BOX_SOU(NBX)%YTU )/2.
                  END IF
!
                  CALL PGSAVE() ! 2A
!
                  IF ( LSEL_SOU(ISB) ) THEN
!
! -------------------- Source has been already selected
!
                       CALL PGSCI  ( ICOL_SOU(2) )
                       IF ( IUSE_SOU .EQ. 0 ) ISS = ISS + 1
                    ELSE
                       CALL PGSCI  ( ICOL_SOU(1) )
                  END IF
!
! --------------- Printing the box and filling it by appropriate colour
!
                  CALL PGSLW  ( 1 )
                  CALL PGSFS  ( 1 )
                  CALL PGRECT ( BOX_SOU(NBX)%XLB, BOX_SOU(NBX)%XTU, &
     &                          BOX_SOU(NBX)%YLB, BOX_SOU(NBX)%YTU )
!
! --------------- Printing the frame of the box
!
                  CALL PGSCI  ( 1 )
                  CALL PGSFS  ( 2 )
                  CALL PGRECT ( BOX_SOU(NBX)%XLB, BOX_SOU(NBX)%XTU, &
     &                          BOX_SOU(NBX)%YLB, BOX_SOU(NBX)%YTU )
!
! --------------- Temporary setting world coordinate space for the source box
!
                  CALL PGSVP  ( BOX_SOU(NBX)%XLB, BOX_SOU(NBX)%XTU, &
     &                          BOX_SOU(NBX)%YLB, BOX_SOU(NBX)%YTU )
!
                  CALL PGSCH   ( 1.25 )
                  CALL PGSLW   ( 5    )
!
! --------------- Printing names of the sources
!
                  CALL PGPTXT  ( 0.03, 0.33, 0.0, 0.0, DBOBJ%C_SOU(ISB)(1:8) )
                  CALL CLRCH   ( STR )
                  CALL INCH    ( DBOBJ%KU_SOU(IFB), STR )
                  STR='('//STR(1:I_LEN(STR))//')'
!
                  CALL PGSCH   ( 0.7 )
                  CALL PGSLW   ( 4   )
                  CALL PGSCI   ( ICOL_SNM )
                  CALL PGPTXT  ( 0.96, 0.08, 0.0, 1.0, STR(1:I_LEN(STR)) )
!
                  CALL CLRCH   ( STR )
                  CALL INCH    ( ISB, STR )
                  CALL PGSCI   ( ICOL_NMR )
                  CALL PGPTXT  ( 0.03, 0.78, 0.0, 0.0, STR(1:I_LEN(STR)) )
!
! --------------- Restoring world coordinated space
!
                  CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
                  CALL PGUNSA() ! 2A
               END IF
 420        CONTINUE
 410     CONTINUE
         IUSE_SOU = 0 ! Set attribute: repaint entire page
!
         CALL PGSFS  ( 1 )
         IF ( IPAGE .GT. 1 ) THEN
!
! ----------- Printing upper boundary
!
              CALL PGRECT ( XLF, XRF, YTF+SIFR_V*2, YTF+SIFR_V*3 )
         END IF
!
         IF ( IPAGE .LT. NPAGE ) THEN
!
! ----------- Printing bottom boundary
!
              CALL PGRECT ( XLF, XRF, YBF-SIFR_V*2, YBF-SIFR_V*3 )
         END IF
!
         IF ( FL_SAVE_1 ) THEN
              FL_SAVE_1 = .FALSE.
              CALL PGUNSA() ! 1
         END IF
!
         CALL PGEBUF()
         CALL PGUPDT()
!
! ------ Asking user the input
!
         CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
         IF ( IMODE .LE. 1  .AND.  CH .EQ. 'A'  ) CH = 'D'
!
! ------ Then analysis: in which box the cursor is pointing
!
         IF ( CH .EQ. 'x' ) CH = 'X'
         IF ( CH .EQ. 'q' ) CH = 'Q'
         IF ( CH .EQ. 'X'  .OR.  CH .EQ. 'Q' ) THEN
              GOTO 810
         END IF
!
         IF ( YC .GT. YTF  .AND. IPAGE .GT. 1 ) THEN
              IPAGE = IPAGE - 1
              GOTO 910
         END IF
!
         IF ( YC .LT. YBF  .AND. IPAGE .LT. NPAGE ) THEN
              IPAGE = IPAGE + 1
              GOTO 910
         END IF
!
         ID = DIAGI_INBOX ( NBX, BOX_SOU, XC, YC )
         IF (  ID .GT. 0 ) ISR = ISOU(ID)
!
         IF ( CH .EQ. 'A' ) THEN
              IF ( LSEL_SOU(ISR) ) THEN
!
! ---------------- Sourse has already been selected. Deselect it
!
                   LSEL_SOU(ISR) = .FALSE.
                   ISS = ISS - 1
                 ELSE
!
! ---------------- Source has not been selected. Try to select it if we can
!
                   IF ( ISS .LT. IMODE  .AND.  ISS .LT. MBR ) THEN
                        LSEL_SOU(ISR) = .TRUE.
                        ISS = ISS + 1
                   END IF
              END IF
!
! ----------- Special trick: to avoid repainting entire page, we hint that
! ----------- the box with ISR-th source shoul be only repainted.
!
              IUSE_SOU = ISR
!
              IF ( ISS .LT. IMODE  .AND.  ISS .LT. MBR ) THEN
                   GOTO 920
                ELSE
                   IUSE_SOU = 0 ! Set attribute: repaint entire page
                   CH = 'D'
              END IF
         END IF
!
         IF ( CH    .EQ. 'D'  .AND. &
     &        IMODE .GT. 1    .AND. &
     &        ISS   .GT. 0          ) THEN
!
              CALL PGSAVE() ! 3
              CALL PGBBUF() ! 3
              CALL PGERAS()
!
! ----------- Printing a banner
!
              CALL PGSCH   ( 2.0 )
              CALL PGSLW   ( 8   )
              CALL PGPTXT  ( XCT, YCT, 0.0, 0.5, 'List of the sources '// &
     &                      'you have selected' )
!
              ISS = 0
              DO 430 J3=1,DBOBJ%L_SOU
                 IF ( LSEL_SOU(J3) ) THEN
                      ISS = ISS + 1
                      IFB = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, &
     &                                 DBOBJ%LIS_SOU(J3)  )
!
! ------------------- Creation the box woth ISS-th selected source
!
                      BOX_SOU(ISS)%XLB = XCB + SIZH_SOU*(MBC/2.0 - 1.0)
                      BOX_SOU(ISS)%YLB = YCB - SIZV_SOU*ISS
                      BOX_SOU(ISS)%XTU = XCB + SIZH_SOU*(MBC/2.0 + 1.0)
                      BOX_SOU(ISS)%YTU = YCB - SIZV_SOU*(ISS-0.85)
!
! ------------------- Filling the box by appropriate colour
!
                      CALL PGSLW  ( 1 )
                      CALL PGSCI  ( ITAB_CLR(ISS,2) )
                      CALL PGSFS  ( 1 )
                      CALL PGRECT ( BOX_SOU(ISS)%XLB, BOX_SOU(ISS)%XTU, &
     &                              BOX_SOU(ISS)%YLB, BOX_SOU(ISS)%YTU )
!
! ------------------- Printing the frame of the box
!
                      CALL PGSCI  ( ITAB_CLR(ISS,1) )
                      CALL PGSLW  ( 10 )
                      CALL PGSFS  ( 2  )
                      CALL PGRECT ( BOX_SOU(ISS)%XLB, BOX_SOU(ISS)%XTU, &
     &                              BOX_SOU(ISS)%YLB, BOX_SOU(ISS)%YTU )
!
! ------------------- Temporary setting world coordinate space for the
! ------------------- source box
!
                      CALL PGSVP  ( BOX_SOU(ISS)%XLB, BOX_SOU(ISS)%XTU, &
     &                              BOX_SOU(ISS)%YLB, BOX_SOU(ISS)%YTU )
!
                      CALL CLRCH ( STR )
                      CALL INCH  ( ISS, STR )
                      STR(I_LEN(STR)+1:) = ')'
                      STR(I_LEN(STR)+2:) = DBOBJ%C_SOU(J3)
!
                      CALL PGSCH   ( 1.25 )
                      CALL PGSLW   ( 5    )
!
! ------------------- Printing names of the stations of the sources
!
                      CALL PGPTXT  ( 0.1, 0.33, 0.0, 0.0, STR(1:I_LEN(STR)) )
!
                      CALL CLRCH   ( STR )
                      IF ( IFB .GT. 0 ) THEN
                           CALL INCH    ( DBOBJ%KU_SOU(IFB), STR )
                         ELSE
                           STR='0'
                      END IF
                      STR='('//STR(1:I_LEN(STR))//')'
                      CALL PGSCI   ( 1    )
                      CALL PGSCH   ( 0.7 )
                      CALL PGSLW   ( 4   )
!
! ------------------- Printing the number of observations of the source
!
                      CALL PGPTXT  ( 0.95, 0.15, 0.0, 1.0, STR(1:I_LEN(STR)) )
!
! ------------------- Restoring world coordinated space
!
                      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
                  END IF
 430          CONTINUE
!
              CALL PGUNSA() ! 3
              CALL PGEBUF() ! 3
              CALL PGUPDT()
!
! ----------- Awaiting user reaction
!
              CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
              IF ( CH .EQ. 'A'  .OR.  CH .EQ. 'D' ) THEN
!
! ---------------- End of work
!
                   GOTO 810
              END IF
         END IF  ! ch .eq. 'D' .and.  ...
!
         IF ( CH .EQ. 'D'   .AND. &
     &        IMODE .LE. 1  .AND. &
     &        ISR   .GT. 0         )  THEN
!
              GOTO 810
         END IF
      GOTO 910  ! Infinte loop
 810  CONTINUE
!
! --- Closing plotting device
!
      CALL PGENDQ()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SELSOU  #!#
