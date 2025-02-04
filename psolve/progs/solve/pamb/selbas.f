      SUBROUTINE SELBAS ( MES, DBOBJ, LSEL_BAS, IBL_LAST, IBL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SELBAS  allows user to chose the baseline using           *
! *   DiaGI interface. New PGPLOT X-window will be created. The list of  *
! *   the baselines used in solution will be displayed as colored boxes. *
! *   The boxes with baselines previously selected (visited) will be     *
! *   displayed by a bit different color. User is able to select the     *
! *   baseline using mouse or keyboard. Index of the selected baseline   *
! *   will be returned.                                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      MES ( CHARACTER ) -- Lines with message to printed at the top   *
! *                           of window.                                 *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! * LSEL_BAS ( LOGICAL*4 ) -- Array with length of number of baselines.  *
! *                           LSEL_BAS(k) .TRUE. that means that the     *
! *                           k-th baseline from the general list of     *
! *                           baselines has been selected previously.    *
! * IBL_LAST ( INTEGER*4 ) -- Index of the baseline which has been       *
! *                           selected at the previous call of           *
! *                           SELBAS or 0 if this call of                *
! *                           SELBAS is the first call.                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      IBL ( INTEGER*4 ) -- Index of the selected baseline in the      *
! *                           general list of baselines.                 *
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
! *  ###  11-NOV-97     SELBAS     v1.4  (c)  L. Petrov  09-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      TYPE ( DIAGI_BOXES ) ::  BOX_BAS(MO_BAS)
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*4  IBL, IBL_LAST, IUER
      LOGICAL*4  LSEL_BAS(MO_BAS)
      INTEGER*4  COL_BAS(3,2), ICOL_BAS(2), N1$ARG, N2$ARG
!
! --- Define colors
!
      DATA       ( (  COL_BAS(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_BAS(N2$ARG), N2$ARG=1,2           ) &
     &           / &
     &               220, 209, 185,  11,      220, 194, 133,  12 &   ! ( 41;40-100)
     &           /
      INTEGER*4  IBAS(MO_BAS), MBX
      PARAMETER  ( MBX = 12 )
      REAL*4     XCT, YCT, XCB, YCB, SIZV_BAS, SIZH_BAS, XC, YC, &
     &           XLF, XRF, YBF, YTF, SIFR_H, SIFR_V
      PARAMETER  ( SIFR_H = 0.003 )
      PARAMETER  ( SIFR_V = 0.005 )
      CHARACTER  MES*(*), STR*80, CH*1
      INTEGER*4  ID_XW, IC, IR, IRB, ICB, ISB, IRE, ICE, IFB, IER
      INTEGER*4  J1, J2, ID, NB, NST1, NST2, ISWAP
      INTEGER*4  DIAGI_INBOX, IFIND_PL, PGOPEN, NSTBA, I_LEN
!
      IBL = 0
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
           CALL ERR_LOG ( 6221, IUER, 'SELBAS', 'Error in openning '// &
     &         'the graphic device '//DIAGI_S%DEVICE//' IER='//STR )
           RETURN
      END IF
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 0
      CALL DIAGI_CLS ( DIAGI_S, IER )
!
      CALL PGCOL_RGB ( ICOL_BAS(1), COL_BAS(1,1), COL_BAS(2,1), COL_BAS(3,1) )
      CALL PGCOL_RGB ( ICOL_BAS(2), COL_BAS(1,2), COL_BAS(2,2), COL_BAS(3,2) )
!
! --- Setting default font type
!
      CALL PGSCF     ( 2 )
!
      IRB = 1
      ICB = 1
      IF ( IBL_LAST .GT. 0  .AND.  IBL_LAST .LE. DBOBJ%L_BAS) THEN
!
! -------- If previous baseline was selected then try to find appropriate
! -------- page whete it was located
!
           CALL NBAST ( DBOBJ%LIS_BAS(IBL_LAST), NST1, NST2 )
           IF ( NST1 .GT. 0  .AND.  NST1 .LE. DBOBJ%U_STA  .AND. &
     &          NST2 .GT. 0  .AND.  NST2 .LE. DBOBJ%U_STA        ) THEN
!
                IRB = 1 + (NST1/MBX + MIN ( 1, MOD(NST1,MBX) ) - 1)*MBX
                ICB = 1 + (NST2/MBX + MIN ( 1, MOD(NST2,MBX) ) - 1)*MBX
           END IF
      END IF
!
 910  CONTINUE
         CALL PGERAS()
         CALL PGBBUF()
         CALL PGSAVE() ! 1
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
         XCT  = 0.5
         YCT  = 0.95
         XCB  = 0.02
         YCB  = 0.90
!
         XLF = 0.02
         XRF = 0.98
         YBF = 0.02
         YTF = 0.91
!
         XC = 0.5
         YC = 0.5
!
         CALL PGSCH   ( 2.0 )
         CALL PGSLW   ( 8   )
         CALL PGPTXT  ( XCT, YCT, 0.0, 0.5, MES(1:I_LEN(MES)) )
!
! ------ Setting the DIAGI_S of the baseline  rectangulars
!
         SIZV_BAS = (YCB-YBF)/MBX
         SIZH_BAS = (XRF-XCB)/MBX
!
         CALL PGSVP ( 0.0, 1.0, 0.0, 1.0  )
!
! ------ Setting maximum device independent coordiantes
!
         NB  = 0
         IRE = IRB + MBX-1
         IF ( IRE .GT. DBOBJ%U_STA ) IRE = DBOBJ%U_STA
         ICE = ICB + MBX-1
         IF ( ICE .GT. DBOBJ%U_STA ) ICE = DBOBJ%U_STA
         DO 410 J1=IRB,IRE
            DO 420 J2=ICB,ICE
!
! ------------ Find indices of the baseline among all baselines (ISB) and
! ------------ among selected in solution baselines (IFB)
!
               ISB = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                          NSTBA ( DBOBJ%UIS_STA(J1), DBOBJ%UIS_STA(J2) ) )
               IFB = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, &
     &                          NSTBA ( DBOBJ%UIS_STA(J1), DBOBJ%UIS_STA(J2) ) )
!
               IF ( IFB .GT. 0 ) THEN
                  NB = NB + 1
                  IBAS(NB) = ISB
                  IR = J1 - (IRB-1)
                  IC = J2 - (ICB-1)
                  IF ( IC .LT. IR ) THEN
                       ISWAP = IR
                       IR = IC
                       IC = ISWAP
                  END IF
!
! --------------- Specifing the corner of the NB-th box
!
                  BOX_BAS(NB)%XLB = XCB + SIZH_BAS*(IC-1)
                  BOX_BAS(NB)%YLB = YCB - SIZV_BAS*IR
                  BOX_BAS(NB)%XTU = XCB + SIZH_BAS*IC
                  BOX_BAS(NB)%YTU = YCB - SIZV_BAS*(IR-1)
                  IF ( ISB .EQ. IBL_LAST ) THEN
                       XC = ( BOX_BAS(NB)%XLB + BOX_BAS(NB)%XTU )/2.
                       YC = ( BOX_BAS(NB)%YLB + BOX_BAS(NB)%YTU )/2.
                  END IF
!
                  CALL PGSAVE() ! 2A
!
                  IF ( LSEL_BAS(ISB) ) THEN
                       CALL PGSCI  ( ICOL_BAS(2) )
                    ELSE
                       CALL PGSCI  ( ICOL_BAS(1) )
                  END IF
!
! --------------- Printing the box
!
                  CALL PGSLW  ( 1 )
                  CALL PGSFS  ( 1 )
                  CALL PGRECT ( BOX_BAS(NB)%XLB, BOX_BAS(NB)%XTU, &
     &                          BOX_BAS(NB)%YLB, BOX_BAS(NB)%YTU )
!
! --------------- Filling the box by appropriate colour
!
                  CALL PGSCI  ( 1 )
                  CALL PGSFS  ( 2 )
                  CALL PGRECT ( BOX_BAS(NB)%XLB, BOX_BAS(NB)%XTU, &
     &                          BOX_BAS(NB)%YLB, BOX_BAS(NB)%YTU )
!
! --------------- Temporary setting world coordinate space for the station box
!
                  CALL PGSVP  ( BOX_BAS(NB)%XLB, BOX_BAS(NB)%XTU, &
     &                          BOX_BAS(NB)%YLB, BOX_BAS(NB)%YTU )
!
                  CALL PGSCH   ( 0.7 )
                  CALL PGSLW   ( 4    )
!
! --------------- Printing names of the stations of the baseline
!
                  CALL PGPTXT  ( 0.05, 0.6, 0.0, 0.0, DBOBJ%C_BAS(ISB)(1:9)   )
                  CALL PGPTXT  ( 0.95, 0.3, 0.0, 1.0, DBOBJ%C_BAS(ISB)(10:17) )
!
! --------------- Restoring world coordinated space
!
                  CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
                  CALL PGUNSA() ! 2A
               END IF
 420        CONTINUE
 410     CONTINUE
!
         CALL PGSFS  ( 1 )
         IF ( IRB .GT. 1 ) THEN
!
! ----------- Printing upper boundary
!
              CALL PGRECT ( XLF, XRF, YTF+SIFR_V*2, YTF+SIFR_V*3 )
         END IF
!
         IF ( (IRB-1+MBX) .LT. DBOBJ%U_STA ) THEN
!
! ----------- Printing bottom boundary
!
              CALL PGRECT ( XLF, XRF, YBF-SIFR_V*2, YBF-SIFR_V*3 )
         END IF
         IF ( ICB .GT. 1 ) THEN
!
! ----------- Printing left boundary
!
              CALL PGRECT ( XLF-SIFR_H*3, XLF-SIFR_H*2, YBF, YTF )
         END IF
!
         IF ( (ICB-1+MBX) .LT. DBOBJ%U_STA ) THEN
!
! ----------- Printing right boundary
!
              CALL PGRECT ( XRF+SIFR_H*2, XRF+SIFR_H*3, YBF, YTF )
         END IF
!
         CALL PGUNSA() ! 1
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
         IF ( CH .EQ. 'X'  .OR.  CH .EQ. 'Q' ) THEN
              GOTO 810
         END IF
!
         IF ( IRB .GT. 1 .AND.  YC .GT. YTF ) THEN
              IRB = IRB - MBX
         END IF
!
         IF ( (IRB-1+MBX) .LT. DBOBJ%U_STA  .AND.  YC .LT. YBF ) THEN
              IRB = IRB + MBX
         END IF
         IF ( ICB .GT. 1  .AND.  XC .LT. XLF ) THEN
              ICB = ICB - MBX
         END IF
!
         IF ( (ICB-1+MBX) .LT. DBOBJ%U_STA  .AND.  XC .GT. XRF ) THEN
              ICB = ICB + MBX
         END IF
!
         ID = DIAGI_INBOX ( NB, BOX_BAS, XC, YC )
         IF (  ID .GT. 0 ) IBL = IBAS(ID)
         IF ( IBL .GT. 0 ) GOTO 810
      GOTO 910  ! Infinte loop
 810  CONTINUE
!      IF ( CH .EQ. 'X'   .OR.   CH .EQ. 'Q' ) THEN
!           CALL PGERAS
!C
!C -------- Printing farwell message
!C
!           CALL PGSCH  ( 1.666 )
!           CALL PGSLW  ( 5     )
!           CALL PGPTXT ( 0.5, 0.5, 0.0, 0.5,
!     #                  'Please, iconify (by <Alt/blank> <n>) '//
!     #                  'graphic window manually' )
!      END IF
!
! --- Closing plotting device
!
      CALL PGCLOQ()
      CALL PGENDQ()
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  SELBAS  #!#
