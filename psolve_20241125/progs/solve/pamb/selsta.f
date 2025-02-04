      FUNCTION SELSTA ( IPAR, MES, DBOBJ, LSUP_STA, LSEL_STA, ISTA, ISTA_LAST, &
     &                  IFD_STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SELSTA  allows user to chose the station using DiaGI      *
! *   interface. New PGPLOT X-window will be created. The list of        *
! *   used in solution stations will be displayed as colored boxes.      *
! *   The boxes with previously selected (visited) stations will be      *
! *   displayed by a bit different color. User is able to select the     *
! *   station using mouse or keyboard. Index of the selected station     *
! *   will be returned. Fiducial stations is outlined by red color.      *
! *   If IPAR = 2  then used is able to change fiducial station.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        IPAR ( INTEGER*4 ) -- Mode switcher.                          *
! *               IPAR = 1 -- boxes for selection of fiducial station    *
! *                           are not displayed and user is not able     *
! *                           to select fiducial station.                *
! *               IPAR = 2 -- boxes for selection of fiducial station    *
! *                           are displayed and user is able to select   *
! *                           fiducial station.                          *
! *         MES ( CHARACTER ) -- Lines with message to printed at the    *
! *                              top of window.                          *
! *       DBOBJ ( RECORD    ) -- Data structure which keeps general      *
! *                              information about the database such as  *
! *                              lists of the objects.                   *
! *    LSUP_STA ( LOGICAL*4 ) -- Array flags of suppressed stations.     *
! *                              Box with suppressed station will not be *
! *                              displayed (but will hold the place) and *
! *                              user is not able to select it.          *
! *                              If LSUP_STA(k) .TRUE. it means that the *
! *                              k-th station from the general list of   *
! *                              stations (DBOBJ.LIS) is suppressed from *
! *                              selection.                              *
! *   ISTA_LAST ( INTEGER*4 ) -- Index of the station which has been     *
! *                              selected at the previous call of        *
! *                              SELSTA or 0 if this call of             *
! *                              SELSTA is the first call.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    <SELSTA> ( LOGICAL*4 ) -- If .TRUE. -- user has chosen the        *
! *                              station. If .FALSE. User terminated     *
! *                              execution by the command 'X' without    *
! *                              selection of the new station.           *
! *        ISTA ( INTEGER*4 ) -- Index of the selected station in the    *
! *                              list of stations DBOBJ.LIS_STA.         *
! *     IFD_STA ( INTEGER*4 ) -- Index of the fiducial station in the    *
! *                              list of stations DBOBJ.LIS_STA.         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    LSEL_STA ( LOGICAL*4 ) -- Array with length of number of stations.*
! *                              If LSEL_STA(k) .TRUE. it means that the *
! *                              k-th station from the general list of   *
! *                              stations has been selected previously.  *
! *        IUER ( INTEGER*4, OPT ) -- Universal error handler.           *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  02-JUN-98     SELSTA     v3.0  (c)  L. Petrov  26-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      TYPE ( DIAGI_BOXES ) ::  BOX_STA(MO_STA)
      TYPE ( DIAGI_BOXES ) ::  BOX_TYP(2)
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*4  IPAR, ISTA, ISTA_LAST, IFD_STA, IUER
      LOGICAL*4  SELSTA, LSEL_STA(MO_STA), LSUP_STA(MO_STA)
      INTEGER*4  COL_STA(3,2), ICOL_STA(2), &
     &           COL_OPR(3,2), ICOL_OPR(2), N1$ARG, N2$ARG
!
! --- Define colors
!
      DATA       ( (  COL_STA(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_STA(N2$ARG), N2$ARG=1,2           ), &
     &           ( (  COL_OPR(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_OPR(N2$ARG), N2$ARG=1,2           ) &
     &           / &
     &               220, 209, 185,  11,     220, 194, 133,  12, &   ! ( 41;40-100)
     &               196, 220, 185,  13,     142, 210, 111,  14 &    !
!!     #               196, 220, 185,  13,      67, 180,  38,  14   !
     &           /
      INTEGER*4  MBX
      PARAMETER  ( MBX = 20 )
      REAL*4     XCT, YCT, XCB, YCB, SIZV_STA, SIZH_STA, XC, YC, &
     &           XLF, XRF, YBF, YTF, SIFR_H, SIFR_V
      REAL*4     XC_O, YC_O, SIZV_OPR, SIZH_OPR
      PARAMETER  ( SIFR_H = 0.003 )
      PARAMETER  ( SIFR_V = 0.005 )
      PARAMETER  ( XCT = 0.50, YCT = 0.95 ) ! Title coordinates
      PARAMETER  ( XCB = 0.02, YCB = 0.90 ) ! Bottom line coorinates
      PARAMETER  ( XLF = 0.02, XRF = 0.98 ) ! Left angle of the frame coord.
      PARAMETER  ( YBF = 0.02, YTF = 0.91 ) ! Rightt angle of the frame coord.
      LOGICAL*4  FL_SAVE_1, F_PRO
      INTEGER*4  ID_XW, IR, J1, J2, ID, NB, INEW_ROW, IOLD_ROW, IPL_STA, IER
      CHARACTER  MES*(*), STR*80, CH*1, OPR_MES(2)*22, OPR_ABR(2)*1
      DATA       ( OPR_ABR(J1), OPR_MES(J1), J1=1,2 ) &
     &           / &
     &             'F',  'Fiducial station      ', &
     &             'P',  'Station for processing' &
     &           /
      INTEGER*4  DIAGI_INBOX, PGOPEN, I_LEN, IFIND_PL
!C
      IF ( IPAR .NE. 1  .AND.  IPAR .NE. 2 ) THEN
           CALL CLRCH   (       STR )
           CALL INCH    ( IPAR, STR )
           CALL ERR_LOG ( 6261, IUER, 'SELSTA', 'Wrong value of IPAR: '// &
     &          STR(1:I_LEN(STR))//' 1 or 2 was expected' )
           RETURN
      END IF
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
           CALL ERR_LOG ( 6262, IUER, 'SELSTA', 'Error in openning '// &
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
      CALL PGCOL_RGB ( ICOL_OPR(1), COL_OPR(1,1), COL_OPR(2,1), COL_OPR(3,1) )
      CALL PGCOL_RGB ( ICOL_OPR(2), COL_OPR(1,2), COL_OPR(2,2), COL_OPR(3,2) )
!
! --- Setting default font type
!
      CALL PGSCF  ( 2 )
      CALL PGERAS()       ! Erase the screen
!
      XC   =  0.5
      YC   = (YCT + YCB)/2.0
      XC_O = 0.05
      YC_O = 0.07
      SIZV_OPR = 0.07
      SIZH_OPR = 0.27
!
! --- Initialization
!
      IF ( ISTA_LAST .LE. 0  .OR.  ISTA_LAST .GT. DBOBJ%L_STA ) THEN
           ISTA_LAST = 1
      END IF
      IF ( IFD_STA .LE. 0  .OR.  IFD_STA .GT. DBOBJ%L_STA ) THEN
           IFD_STA = 1
      END IF
      ISTA = ISTA_LAST
      F_PRO = .TRUE.
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
! ------ Setting the DIAGI_S of the station rectangulars
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
         DO 410 J1=1,DBOBJ%L_STA
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
            IF ( LSUP_STA(J1) ) GOTO 410 ! Bypassed suppressed station
            CALL PGSAVE() ! 2A
            IF ( LSEL_STA(J1) ) THEN
                 CALL PGSCI  ( ICOL_STA(2) )
               ELSE
                 CALL PGSCI  ( ICOL_STA(1) )
            END IF
!
! --------- Printing the box
!
            CALL PGSLW  ( 6 )
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
            IF ( DBOBJ%LIS_STA(J1) .EQ. DBOBJ%LIS_STA(IFD_STA) ) THEN
!
! -------------- Outline by red colour the rectangular with fiducial station
!
                 CALL PGSCI  ( ERR_CLRI )
                 CALL PGSFS  ( 2 )
                 CALL PGRECT ( BOX_STA(NB)%XLB, BOX_STA(NB)%XTU, &
     &                         BOX_STA(NB)%YLB, BOX_STA(NB)%YTU )
                 CALL PGSCI  ( 1 )
            END IF
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
            CALL PGPTXT  ( 0.1, 0.2, 0.0, 0.0, DBOBJ%C_STA(J1)(1:8) )
!
! --------- Restoring world coordinated space
!
            CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
            CALL PGUNSA() ! 2A
 410     CONTINUE
!
 930     CONTINUE
         IF ( IPAR .EQ. 2 ) THEN
              BOX_TYP(1)%XLB = XC_O
              BOX_TYP(1)%XTU = BOX_TYP(1)%XLB + SIZH_OPR
              BOX_TYP(1)%YLB = YC_O
              BOX_TYP(1)%YTU = BOX_TYP(1)%YLB + SIZV_OPR
!
              BOX_TYP(2)%XLB = XC_O
              BOX_TYP(2)%XTU = BOX_TYP(2)%XLB + SIZH_OPR
              BOX_TYP(2)%YLB = YC_O + SIZV_OPR*2.0
              BOX_TYP(2)%YTU = BOX_TYP(2)%YLB + SIZV_OPR
!
              DO 420 J2=1,2
!
! --------------- Filling the box by appropriate colour
!
                  IF ( ( J2 .EQ. 1   .AND.         F_PRO  ) .OR. &
     &                 ( J2 .EQ. 2   .AND.   .NOT. F_PRO  )     ) THEN
                       CALL PGSCI ( ICOL_OPR(1) )
                     ELSE
                       CALL PGSCI ( ICOL_OPR(2) )
                  END IF
!
                  CALL PGSLW  ( 6 )
                  CALL PGSFS  ( 1 )
                  CALL PGRECT ( BOX_TYP(J2)%XLB, BOX_TYP(J2)%XTU, &
     &                          BOX_TYP(J2)%YLB, BOX_TYP(J2)%YTU )
!
! --------------- Outline it
!
                  CALL PGSCI  ( 1 )
                  CALL PGSFS  ( 2 )
                  CALL PGRECT ( BOX_TYP(J2)%XLB, BOX_TYP(J2)%XTU, &
     &                          BOX_TYP(J2)%YLB, BOX_TYP(J2)%YTU )
                  CALL PGSVP   ( BOX_TYP(J2)%XLB, BOX_TYP(J2)%XTU, &
     &                           BOX_TYP(J2)%YLB, BOX_TYP(J2)%YTU )
!
                  CALL PGSCH   ( 0.6 )
                  CALL PGSLW   ( 1   )
                  CALL PGPTXT  ( 0.02, 0.80, 0.0, 0.0, OPR_ABR(J2) )
                  CALL PGSCH   ( 1.2 )
                  CALL PGSLW   ( 1   )
                  CALL PGPTXT  ( 0.1, 0.4, 0.0, 0.0, OPR_MES(J2) )
!
! --------------- Restoring world coordinates
!
                  CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0  )
 420          CONTINUE
         END IF
!
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
!
! ------ Asking user the input
!
         CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
! ------ Then analysis: in which box the cursor is pointing
!
         IF ( CH .EQ. 'x' ) CH = 'X'
         IF ( CH .EQ. 'q' ) CH = 'Q'
         IF ( CH .EQ. 'f' ) CH = 'F'
         IF ( CH .EQ. 'p' ) CH = 'P'
         IF ( CH .EQ. 'X'  .OR.  CH .EQ. 'Q' ) THEN
              ISTA   = 0
              SELSTA = .FALSE.
              GOTO 810
         END IF
         IF ( IPAR .EQ. 2  .AND.  CH .EQ. 'F' ) THEN
              F_PRO = .FALSE.
              GOTO 930
         END IF
         IF ( IPAR .EQ. 2  .AND.  CH .EQ. 'P' ) THEN
              F_PRO = .TRUE.
              GOTO 930
         END IF
!
! ------ Decide: which rectangular was selected
!
         ID = DIAGI_INBOX ( NB, BOX_STA, XC, YC )
         IF ( ID .GT. 0 ) THEN
              IF ( LSUP_STA(ID) ) GOTO 910 ! It was box with suppressed station
              IF ( F_PRO ) THEN
                   IF ( ISTA .GE. 1  .AND.  ISTA .LE. DBOBJ%L_STA ) THEN
                        IOLD_ROW = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                                     DBOBJ%LIS_STA(ISTA) )
                   END IF
                   ISTA = ID
                   XC = (BOX_STA(ID)%XTU + BOX_STA(ID)%XLB)/2.0
                   YC = (BOX_STA(ID)%YTU + BOX_STA(ID)%YLB)/2.0
                   INEW_ROW = ID
                   LSEL_STA(ISTA) = .TRUE.
                   SELSTA = .TRUE.
                   GOTO 810
                 ELSE
                   IF ( IFD_STA .GE. 1  .AND.  IFD_STA .LE. DBOBJ%L_STA ) THEN
                        IOLD_ROW = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                        DBOBJ%LIS_STA(IFD_STA) )
                   END IF
                   IFD_STA  = ID
                   XC = (BOX_STA(ID)%XTU + BOX_STA(ID)%XLB)/2.0
                   YC = (BOX_STA(ID)%YTU + BOX_STA(ID)%YLB)/2.0
                   INEW_ROW = ID
                   IF ( CH .EQ. 'D' ) THEN
                        SELSTA = .TRUE.
                        GOTO 810
                   END IF
                   GOTO 920
              END IF
         END IF
         IF ( IPAR .EQ. 2  .AND. ID .EQ. 0 ) THEN
              ID = DIAGI_INBOX ( 2, BOX_TYP, XC, YC )
              IF ( ID .EQ. 1 ) THEN
                   F_PRO = .FALSE.
                   GOTO 930
              END IF
              IF ( ID .EQ. 2 ) THEN
                   F_PRO = .TRUE.
                   GOTO 930
              END IF
         END IF
!!      GOTO 910  ! Infinte loop
 810  CONTINUE
      CALL PGERAS()
      CALL PGENDQ()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SELSTA  #!#
