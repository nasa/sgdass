      SUBROUTINE REPA_SELSOU ( MES, M_SOU, L_SOU, C_SOU, KG_SOU, &
     &                         KB_SOU, EF_SOU, LSEL_SOU, ISR_LAST, ISR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REPA_SELSOU  allows user to chose the source(s) using     *
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
! *    L_SOU ( INTEGER*4 ) -- 
! *    C_SOU ( CHARACTER ) -- 
! *   KG_SOU ( INTEGER*4 ) -- 
! *   KB_SOU ( INTEGER*4 ) -- 
! *   EF_SOU ( LOGIGAL*4 ) -- Source coordinate estimation flag.         *
! * LSEL_SOU ( LOGICAL*4 ) -- Array with length of number of sources.    *
! *                           LSEL_SOU(k) .TRUE. that means that the     *
! *                           k-th source from the general list          *
! *                           L_SOU/C_SOU (which includes all sources    *
! *                           where there were at least one observation  *
! *                           regardless of its suppression status) of   *
! *                           sources has been selected previously.      *
! * ISR_LAST ( INTEGER*4 ) -- Index of the source which has been         *
! *                           selected at the previous call of           *
! *                           REPA_SELSOU or if this call of REPA_SELSOU *
! *                           is the first call.                         *
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
! * ### 26-MAY-1998   REPA_SELSOU   v3.0  (c) L. Petrov  06-JAN-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INTEGER*4  M_SOU, ISR, ISR_LAST, L_SOU, IUER
      TYPE ( DIAGI_STRU  ) ::  DIAGI_S
      TYPE ( DIAGI_BOXES ) ::  BOX_SOU(M_SOU)
      CHARACTER  MES*(*), C_SOU(L_SOU)*(*)
      INTEGER*4  KG_SOU(L_SOU), KB_SOU(L_SOU)
      LOGICAL*4  EF_SOU(M_SOU), LSEL_SOU(M_SOU)
      INTEGER*4  COL_SOU(3,2), ICOL_SOU(2), ICOL_SNM, ICOL_NMR, ICOL_EST, &
     &           N1$ARG, N2$ARG, IER
!
! --- Define colors
!
      DATA       ( (  COL_SOU(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_SOU(N2$ARG), N2$ARG=1,2           ) &
     &           / &
     &               220, 209, 185,  29,   220, 194, 133,  30 &   ! ( 41;40-100)
     &           /
      INTEGER*4  ISOU(M_SOU), MBR, MBC
      PARAMETER  ( MBR = 12 )
      PARAMETER  ( MBC =  8 )
      PARAMETER  ( ICOL_EST = 24 )
      PARAMETER  ( ICOL_SNM = 31 )
      PARAMETER  ( ICOL_NMR = 32 )
      REAL*4     XCT, YCT, XCB, YCB, SIZV_SOU, SIZH_SOU, XC, YC, XCP, YCP, &
     &           XLF, XRF, YBF, YTF, SIFR_H, SIFR_V
      PARAMETER  ( SIFR_H = 0.003 )
      PARAMETER  ( SIFR_V = 0.005 )
      CHARACTER  STR*80, CH*1
      LOGICAL*4  FL_SAVE_1, FL_CHANGE 
      INTEGER*4  ID_XW, NPAGE, IPAGE, IRB, ICB, IRE, ICE, IFB
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3
      CHARACTER  ZAG*128, UNIT*128
      INTEGER*4  J0, J1, J2, J3, I1, I2, ID, IS, NBX, ISR_NOW
!
! --- Plotting constants
!
      PARAMETER  ( XCT = 0.50, YCT = 0.95 ) ! Title coordinates
      PARAMETER  ( XCB = 0.02, YCB = 0.90 ) ! Bottom line coorinates
      PARAMETER  ( XLF = 0.02, XRF = 0.98 ) ! Left  corner of the frame coord.
      PARAMETER  ( YBF = 0.04, YTF = 0.91 ) ! Right corner of the frame coord.
      PARAMETER  ( XCP = 0.90, YCP = 0.95 ) ! "Page" text coordinates
!
      PARAMETER  ( DIAGI__PGDN = CHAR(221) ) ! PageDown key code
      PARAMETER  ( DIAGI__PGUP = CHAR(220) ) ! PageUp   key code
!
! --- Size of the source rectangular
!
      PARAMETER  ( SIZV_SOU = (YCB-YBF)/MBR ) ! Vertical size
      PARAMETER  ( SIZH_SOU = (XRF-XCB)/MBC ) ! horizontal size
!
      INTEGER*4  DIAGI_INBOX, IFIND_PL, PGOPEN, I_LEN, IDIV
      IDIV ( I1, I2 ) = I1/I2 + MIN ( 1, MOD ( I1, I2 ) )
!
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, &
     &                 UNIT, ICL1, ICL2, ICL3, IUER )
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
           CALL ERR_LOG ( 6221, IUER, 'REPA_SELSOU', 'Error in openning '// &
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
     &                              IRGB_DEF(1,1,3)                   )
      CALL PGCOL_RGB ( ICOL_EST,    IRGB_DEF(2,1,1), IRGB_DEF(2,1,2), &
     &                              IRGB_DEF(2,1,3)                   )
      CALL PGCOL_RGB ( ICOL_NMR,    IRGB_DEF(3,1,1), IRGB_DEF(3,1,2), &
     &                              IRGB_DEF(3,1,3)                   )
!
! --- Setting default font type
!
      CALL PGSCF     ( 2 )
!
! --- Determine how many pages will we have (NPAGE) and what will be the
! --- current page (IPAGE)
!
      NPAGE = IDIV ( L_SOU+1, MBR*MBC )
      IPAGE = 1
      IF ( ISR_LAST .GT. 0  .AND.  ISR_LAST .LE. L_SOU ) THEN
!
! -------- If previous source was selected then try to find appropriate
! -------- page whete it was located
!
           IPAGE = IDIV ( ISR_LAST, MBR*MBC )
      END IF
!
! --- Initialization
!
      ISR = 0
      ISR_NOW = 0
      FL_CHANGE = .FALSE.
      CH= '?'
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
         IF ( CH .NE. 'A' ) THEN
              XC = 0.5
              YC = 0.5
         END IF
!
! ------ Setting coordinate space once more
!
         CALL PGSVP ( 0.0, 1.0, 0.0, 1.0  )
!
! ------ Setting first (ICB) and the last (ICE) column number
!
         IRB = 1
         IRE = IDIV ( (L_SOU+1) - (IPAGE-1)*MBR*MBC, MBC )
         IF ( IRE .GT. MBC ) IRE = MBR
!
! ------ Setting first (IRB) and the last (IRE) row number
!
         ICB  = 1
         ICE  = (L_SOU+1) - (IPAGE-1)*MBR*MBC
         IF ( ICE .GT. MBC ) ICE = MBC
 920     CONTINUE
!
         NBX  = 0
         DO 410 J1=IRB,IRE
            DO 420 J2=ICB,ICE
!
! ------------ Find indeces of the next source among the sources
!
               IS = (IPAGE-1)*MBR*MBC + (J1-1)*MBC + J2
               IF ( IS > L_SOU+1 ) GOTO 420
!
! ------------ Specifing the corner of the NBX-th box
!
               NBX = NBX + 1
               ISOU(NBX) = IS
!
               BOX_SOU(NBX)%XLB = XCB + SIZH_SOU*(J2-1.0)
               BOX_SOU(NBX)%YLB = YCB - SIZV_SOU*J1
               BOX_SOU(NBX)%XTU = XCB + SIZH_SOU*(J2-0.1)
               BOX_SOU(NBX)%YTU = YCB - SIZV_SOU*(J1-0.9)
!
               CALL PGSAVE() ! 2A
!
               IF ( IS > L_SOU  .AND.  ISR_NOW == 0 ) THEN
                    CALL PGSCI  ( ICOL_SOU(2) )
                  ELSE 
                    IF ( LSEL_SOU(IS) ) THEN
!
! ---------------------- This source is selected
!
                         CALL PGSCI  ( ICOL_SOU(2) )
                         ISR_NOW = IS
                       ELSE
                         CALL PGSCI  ( ICOL_SOU(1) )
                    END IF
               END IF
!
! ------------ Printing the box and filling it by appropriate colour
!
               CALL PGSLW  ( 1 )
               CALL PGSFS  ( 1 )
               CALL PGRECT ( BOX_SOU(NBX)%XLB, BOX_SOU(NBX)%XTU, &
     &                       BOX_SOU(NBX)%YLB, BOX_SOU(NBX)%YTU )
!
! ------------ Printing the frame of the box
!
               IF ( EF_SOU(IS) ) THEN
                    CALL PGSCI  ( ICOL_EST )
                    CALL PGSFS  ( 2 )
                    CALL PGSLW  ( 6 )
                    CALL PGRECT ( BOX_SOU(NBX)%XLB, BOX_SOU(NBX)%XTU, &
     &                            BOX_SOU(NBX)%YLB, BOX_SOU(NBX)%YTU )
                    CALL PGSLW  ( 1 )
                    CALL PGSCI  ( 1 )
                  ELSE
                    CALL PGSCI  ( 1 )
                    CALL PGSFS  ( 2 )
                    CALL PGRECT ( BOX_SOU(NBX)%XLB, BOX_SOU(NBX)%XTU, &
     &                            BOX_SOU(NBX)%YLB, BOX_SOU(NBX)%YTU )
               END IF
!
! ------------ Temporary setting world coordinate space for the source box
!
               CALL PGSVP  ( BOX_SOU(NBX)%XLB, BOX_SOU(NBX)%XTU, &
     &                       BOX_SOU(NBX)%YLB, BOX_SOU(NBX)%YTU )
!
               CALL PGSCH   ( 1.1 )
               IF ( IDEV == 1 ) THEN
                    CALL PGSLW ( 6 )
                  ELSE 
                    CALL PGSLW ( 4 )
               END IF
               IF ( IS .LE. L_SOU ) THEN
!
! ----------------- Printing names of the sources
!
                    CALL PGPTXT  ( 0.05, 0.50, 0.00, 0.00, C_SOU(IS) )
!
                    CALL CLRCH   ( STR )
                    CALL INCH    ( KB_SOU(IS), STR )
                    STR='('//STR(1:I_LEN(STR))//')'
!
                    CALL PGSCH   ( 0.7 )
                    CALL PGSLW   ( 4   )
                    CALL PGSCI   ( ICOL_NMR )
                    CALL PGPTXT  ( 0.96, 0.08, 0.00, 1.00, STR(1:I_LEN(STR)) )
!
                    CALL CLRCH   ( STR )
                    CALL INCH    ( KG_SOU(IS), STR )
                    CALL PGSCI   ( ICOL_SNM )
                    CALL PGPTXT  ( 0.03, 0.08, 0.00, 0.00, STR(1:I_LEN(STR)) )
                  ELSE
                    CALL PGPTXT  ( 0.05, 0.50, 0.00, 0.00, 'Unmark' )
               END IF
!
! ----------------- Restoring world coordinated space
!
               CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
               CALL PGUNSA() ! 2A
 420        CONTINUE
 410     CONTINUE
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
!
! ------ Then analysis: in which box the cursor is pointing
!
         IF ( CH .EQ. 'x' ) CH = 'X'
         IF ( CH .EQ. 'q' ) CH = 'Q'
         IF ( CH .EQ. 'Q' ) THEN
              IF ( .NOT. FL_CHANGE ) THEN
                   ISR = ISR_LAST
              END IF
              GOTO 810
         END IF
!
         IF ( CH == 'U'  .OR.  CH == 'u' ) THEN
              FL_CHANGE = .TRUE.
              IF ( ISR_NOW > 0 ) THEN
                   LSEL_SOU(ISR_NOW) = .FALSE.
              END IF
              ISR = 0
              ISR_NOW = 0
              IPAGE = NPAGE
              GOTO 910
         END IF
!
         IF ( CH == DIAGI__PGUP  .OR.  &
     &        CH == 'P'          .OR.  &
     &        CH == 'p'          .OR.  &
     &        YC .GT. YTF              ) THEN
!
              IPAGE = IPAGE - 1
              IF (  IPAGE < 1 ) IPAGE = NPAGE
              GOTO 910
         END IF
!
         IF ( CH == DIAGI__PGDN  .OR.  &
     &        CH == 'N'          .OR.  &
     &        CH == 'n'          .OR.  &
     &        YC .LT. YBF              ) THEN
!
              IPAGE = IPAGE + 1
              IF ( IPAGE > NPAGE ) IPAGE = 1
              GOTO 910
         END IF
!
         ID = DIAGI_INBOX ( NBX, BOX_SOU, XC, YC )
!
         IF ( CH .EQ. 'D' .OR. CH .EQ. 'X' ) THEN
              IF ( ID .GT. 0  .AND. ISR_NOW > 0 ) THEN
                   LSEL_SOU(ISR_NOW) = .FALSE.
                   FL_CHANGE = .TRUE.
              END IF
!
              IF ( ID > 0 ) ISR = ISOU(ID)
!
              IF ( ISR > L_SOU ) THEN
                   ISR = 0
                   ISR_NOW = 0
                   FL_CHANGE = .TRUE.
                 ELSE IF ( ISR > 0 ) THEN
                   IF ( LSEL_SOU(ISR) ) THEN
!
! --------------------- Source has already been selected. Deselect it
!
                        LSEL_SOU(ISR) = .FALSE.
                     ELSE
!
! --------------------- Source has not been selected. Try to select it if we can
!
                       LSEL_SOU(ISR) = .TRUE.
                   END IF
                   FL_CHANGE = .TRUE.
              END IF
           ELSE IF ( CH .EQ. 'A' ) THEN
              IF ( ID > 0 ) ISR = ISOU(ID)
              IF ( ISR .GE. 1 .AND. ISR .LE. L_SOU ) THEN
                   EF_SOU(ISR) = .NOT. EF_SOU(ISR) 
                   FL_CHANGE = .TRUE.
              END IF
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
      END  SUBROUTINE  REPA_SELSOU  !#!#
