      FUNCTION   DIAGI_CHF ( DIAGI_S, XC, YC )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_CHF  changes the boundaries of the plotting area.   *
! *   IT returns 0 if no actul changes of the boundaries took place and  *
! *   1 if boundaries changed.                                           *
! *                                                                      *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *  <DIAGI_CHF> ( INTEGER*4 ) -- Return value.                          *
! *                            0  -- no real changes of the plotting     *
! *                                  area occurred.                      *
! *                            1  -- plotting area changed.              *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *      DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI       *
! *                               internal parameters.                   *
! *                                                                      *
! *  ###  10-OCT-97    DIAGI_CHF   v1.2  (c)  L. Petrov 08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  DIAGI_CHF
      REAL*4     XMIN4, XMAX4, YMIN4, YMAX4
      CHARACTER  MES1*128, MES2*128, CH*1, STR*80, OUT*80, TERMN*3
      REAL*4     LV_DIS, RV_DIS, BH_DIS, TH_DIS, XA, YA, B_MIN, &
     &           XC, YC, XNEW, YNEW, VAL
      INTEGER*4  MODE, BOU, UN__BOU, LV__BOU, RV__BOU, BH__BOU, TH__BOU, IL, &
     &           I5
      PARAMETER  ( UN__BOU = -1     )
      PARAMETER  ( LV__BOU =  1 )
      PARAMETER  ( RV__BOU =  2 )
      PARAMETER  ( BH__BOU =  3 )
      PARAMETER  ( TH__BOU =  4 )
      INTEGER*4, EXTERNAL :: I_LEN
!
      TERMN='Xx'//CHAR(3)  ! symbols terminators
      XMIN4 = DIAGI_S%XMIN
      XMAX4 = DIAGI_S%XMAX
      YMIN4 = DIAGI_S%YMIN
      YMAX4 = DIAGI_S%YMAX
!
! --- Deleting the previous prompt
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --- Update the screen
!
      CALL PGUPDT
!
      MES1 = 'Change plotting area. Move cursor to change the boundary '// &
     &       'and then click middle button'
!
! --- Drawing the genral prompt
!
      CALL PGSCI   ( 1        )
      CALL PGSCH   ( DIAGI_S%SCH_LAB  )
      CALL PGSLW   ( DIAGI_S%ISLW_LAB )
      CALL PGPTXT  ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, 0.0, 0.0, &
     &               MES1(1:I_LEN(MES1)) )
!
! --- Calculation the distance from the specified point to the 4 boundaries
!
      LV_DIS = ABS(XMIN4-XC)/(XMAX4-XMIN4)
      RV_DIS = ABS(XMAX4-XC)/(XMAX4-XMIN4)
      BH_DIS = ABS(YMIN4-YC)/(YMAX4-YMIN4)
      TH_DIS = ABS(YMAX4-YC)/(YMAX4-YMIN4)
!
! --- Find the mimnal distance to the boundaries
!
      B_MIN = MIN ( LV_DIS, RV_DIS, BH_DIS, TH_DIS )
!
! --- Determine
! --- BOU  -- the nearest boundary,
! --- MODE -- anchoring mode,
! --- XA   -- X-coordinate of the anchoring point
! --- YA   -- Y-coordinate of the anchoring point
!
      BOU = UN__BOU
      IF ( ( LV_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
           BOU  = LV__BOU
           MODE = 4
           XA   = XMIN4
           YA   = YMIN4
      END IF
      IF ( ( RV_DIS - B_MIN ) .LT. DIAGI_EPS ) THEN
           BOU = RV__BOU
           MODE = 4
           XA  = XMIN4
           YA  = YMAX4
      END IF
      IF ( ( BH_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
           BOU  = BH__BOU
           MODE = 3
           XA   = XMIN4
           YA   = YMIN4
      END IF
      IF ( ( TH_DIS - B_MIN ) .LT. DIAGI_EPS ) THEN
           BOU  = TH__BOU
           MODE = 3
           XA   = XMAX4
           YA   = YMIN4
      END IF
!
! --- Awaiting for user specify the new boundary by cursor. If cursor will
! --- be in plotting area then its coordinates will specify the position of the
! --- new boundary. If cursor will be out of the plotting area then new value
! --- of boundary will be inquired additionally
!
      XNEW = XC
      YNEW = YC
      CALL PGBAND ( MODE, 1, XA, YA, XNEW, YNEW, CH )
!
! --- Removing prompt from the screen
!
      CALL PGSAVE
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
      CALL PGUNSA
!
      IF ( CH .EQ. 'X' ) THEN
!
! -------- Right button of the mouse has been hit. THat means abort of the
! -------- operation.
!
           DIAGI_CHF = 0
           GOTO 810
      END IF
!
 910  CONTINUE
      CALL CLRCH ( STR )
!
! --- Analysis the new trying boundary
!
      IF ( ( LV_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Left vertical boundary of the frame was chosen
!
           IF ( XNEW .GT. XMIN4                            .AND. &
     &          XNEW .LT. XMAX4 - DIAGI_EPS*(XMAX4-XMIN4)  .AND. &
     &          ABS(XMAX4-XNEW) .GT. DIAGI_EPS*MAX(ABS(XMAX4),ABS(XMIN4)) ) THEN
!
! ------------- New boundary appeared within the plotting area
! ------------- Remove the prompt and update the value of the boundary
!
                CALL PGSAVE
                CALL DIAGI_PURGE_BOT ( DIAGI_S )
                CALL PGUNSA
!
                XMIN4 = XNEW
                DIAGI_CHF = 1
                GOTO 810
           END IF
!
! -------- New boundary appeared out of the plotting area. Generate the prompt
! -------- for further inquiry of the new boundary of the plotting area
!
           WRITE ( UNIT=STR, FMT='(1PE12.4)' ) XMIN4
           CALL CHASHL ( STR )
           MES2 = 'XMIN='//STR(1:I_LEN(STR))//' Enter new values >>'
      END IF
!
      IF ( ( RV_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Right vertical boundary of the frame was chosen
!
           IF ( XNEW .GT. XMIN4                            .AND. &
     &          XNEW .LT. XMAX4 - DIAGI_EPS*(XMAX4-XMIN4)  .AND. &
     &          ABS(XNEW-XMIN4) .GT. DIAGI_EPS*MAX(ABS(XMAX4),ABS(XMIN4)) ) THEN
!
! ------------- New boundary appeared within the plotting area
! ------------- Remove the prompt and update the value of the boundary
!
                CALL PGSAVE
                CALL DIAGI_PURGE_BOT ( DIAGI_S )
                CALL PGUNSA
!
                XMAX4 = XNEW
                DIAGI_CHF = 1
                GOTO 810
           END IF
!
! -------- New boundary appeared out of the plotting area. Generate the prompt
! -------- for further inquiry of the new boundary of the plotting area
!
           WRITE ( UNIT=STR, FMT='(1PE12.4)' ) XMAX4
           CALL CHASHL ( STR )
           MES2 = 'XMAX4='//STR(1:I_LEN(STR))//' Enter new values >>'
      END IF
!
      IF ( ( BH_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Bottom horizontal boundary of the frame was chosen
!
           IF ( YNEW .GT. YMIN4                            .AND. &
     &          YNEW .LT. YMAX4 - DIAGI_EPS*(YMAX4-YMIN4)  .AND. &
     &          ABS(YMAX4-YNEW) .GT. DIAGI_EPS*MAX(ABS(YMAX4),ABS(YMIN4)) ) THEN
!
! ------------- New boundary appeared within the plotting area
! ------------- Remove the prompt and update the value of the boundary
!
                CALL PGSAVE
                CALL DIAGI_PURGE_BOT ( DIAGI_S )
                CALL PGUNSA
!
                YMIN4 = YNEW
                DIAGI_CHF = 1
                GOTO 810
           END IF
!
! -------- New boundary appeared out of the plotting area. Generate the prompt
! -------- for further inquiry of the new boundary of the plotting area
!
           WRITE ( UNIT=STR, FMT='(1PE12.4)' ) YMIN4
           CALL CHASHL ( STR )
           MES2 = 'YMIN4='//STR(1:I_LEN(STR))//' Enter new values >>'
      END IF
!
      IF ( ( TH_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Upper horizontal boundary of the frame was chosen
!
           IF ( YNEW .GT. YMIN4                            .AND. &
     &          YNEW .LT. YMAX4 - DIAGI_EPS*(YMAX4-YMIN4)  .AND. &
     &          ABS(YNEW-YMIN4) .GT. DIAGI_EPS*MAX(ABS(YMAX4),ABS(YMIN4)) ) THEN
!
! ------------- New boundary appeared within the plotting area
! ------------- Remove the prompt and update the value of the boundary
!
                CALL PGSAVE
                CALL DIAGI_PURGE_BOT ( DIAGI_S )
                CALL PGUNSA
!
                YMAX4 = YNEW
                DIAGI_CHF = 1
                GOTO 810
           END IF
!
! -------- New boundary appeared out of the plotting area. Generate the prompt
! -------- for further inquiry of the new boundary of the plotting area
!
           WRITE ( UNIT=STR, FMT='(1PE12.4)' ) YMAX4
           CALL CHASHL ( STR )
           MES2 = 'YMAX4='//STR(1:I_LEN(STR))//' Enter new values >>'
      END IF
!
! --- Boundary has been specifed incorrectly or user intentionally put
! --- cursor out of the plotting area. Inquiry the new boundary. New boundary
! --- will be specified explicitly from keybord.
!
      CALL CLRCH   ( OUT )
      CALL PGENTER ( MES2(1:I_LEN(MES2)), XMIN4, &
     &               YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, TERMN, &
     &               OUT(1:12), IL )
!
! --- Deleting prompt for entering new boundary
!
      CALL PGSAVE
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
      CALL PGUNSA
      IF ( IL .EQ. 0 ) THEN
!
! -------- Empty line has been entered. That means abort of the operation
!
           DIAGI_CHF = 0
           GOTO 810
         ELSE IF ( IL .GE. 1 ) THEN
!
! -------- Adding decimal point to prevent error of decoding
!
           IF ( INDEX ( OUT, '.' ) .EQ. 0 ) OUT(I_LEN(OUT)+1:)='.'
!
! -------- Decoding the entered line
!
           READ ( UNIT=OUT(1:I_LEN(OUT)), FMT='(F32.32)', IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Entered line has not format of the numer of REAL*4 type.
! ------------- Print error message
!
                CALL PGSAVE
                CALL PGSCI   ( 2 )
                CALL PGPTXT  ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, &
     &               0.0, 0.0, 'Wrong format of the line '//OUT(1:I_LEN(OUT)) )
                CALL PGUNSA
!
! ------------- Waiting for user to hit something
!
                XC = XMIN4
                YC = YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB
                CALL PGBAND  ( 0, 1, XC, YC, XC, YC, CH )
!
! ------------- Deleting error message
!
                CALL PGSAVE
                CALL DIAGI_PURGE_BOT ( DIAGI_S )
                CALL PGUNSA
!
! ------------- And go to try again
!
                GOTO 910
           END IF
      END IF
!
      CALL CLRCH ( MES2 )
!
! --- Now check valdity of the new attmpted boundary
!
      IF ( ( LV_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Left vertical boundary
!
           XNEW = VAL
           IF ( XNEW .LT. XMAX4 - DIAGI_EPS*(XMAX4-XMIN4)  .AND. &
     &          ABS(XMAX4-XNEW) .GT. DIAGI_EPS*MAX(ABS(XMAX4),ABS(XMIN4)) ) THEN
!
! ------------- Boundary OK
!
                XMIN4 = VAL
                DIAGI_CHF = 1
                GOTO 810
              ELSE
!
! ------------- Not OK
!
                MES2 = 'Error: attempt to move the left boundary through '// &
     &                 'the right boundary'
           END IF
      END IF
!
      IF ( ( RV_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Right vertical boundary
!
           XNEW = VAL
           IF ( XNEW .GT. XMIN4 + DIAGI_EPS*(XMAX4-XMIN4)  .AND. &
     &          ABS(XNEW-XMIN4) .GT. DIAGI_EPS*MAX(ABS(XMAX4),ABS(XMIN4)) ) THEN
!
! ------------- Boundary OK
!
                XMAX4 = VAL
                DIAGI_CHF = 1
                GOTO 810
              ELSE
!
! ------------- Not OK
!
                MES2 = 'Error: attempt to move the right boundary through '// &
     &                 'the left boundary'
           END IF
      END IF
!
      IF ( ( BH_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Bottom horizontal boundary
!
           YNEW = VAL
           IF ( YNEW .LT. YMAX4 - DIAGI_EPS*(YMAX4-YMIN4)  .AND. &
     &          ABS(YMAX4-YNEW) .GT. DIAGI_EPS*MAX(ABS(YMAX4),ABS(YMIN4)) ) THEN
!
! ------------- Boundary OK
!
                YMIN4 = VAL
                DIAGI_CHF = 1
                GOTO 810
              ELSE
!
! ------------- Not OK
!
                MES2 = 'Error: attempt to move the bottom boundary through '// &
     &                 'the upper boundary'
           END IF
      END IF
!
      IF ( ( TH_DIS - B_MIN ) .LE. DIAGI_EPS ) THEN
!
! -------- Upper horizontal boundary
!
           YNEW = VAL
           IF ( YNEW .GT. YMIN4 + DIAGI_EPS*(YMAX4-YMIN4)  .AND. &
     &          ABS(YNEW-YMIN4) .GT. DIAGI_EPS*MAX(ABS(YMAX4),ABS(YMIN4)) ) THEN
!
! ------------- Boundary OK
!
                YMAX4 = VAL
                DIAGI_CHF = 1
                GOTO 810
              ELSE
!
! ------------- Not OK
!
                MES2 = 'Error: attempt to move the upper boundary through '// &
     &                 'the bottom boundary'
           END IF
      END IF
!
! --- Printing error message about wrong new position of the boundary
!
      CALL PGSAVE
      CALL PGSCI  ( 2 )
      CALL PGPTXT ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, 0.0, 0.0, &
     &              MES2(1:I_LEN(MES2)) )
      CALL PGUNSA
!
! --- Awaiting for user reaction
!
      XC = XMIN4
      YC = YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
! --- Removing error message from the screen
!
      CALL PGSAVE
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
      CALL PGUNSA
!
! --- Try once more
!
      GOTO 910
!
! --- Successful termination
!
 810  CONTINUE
!
! --- Putting DiaGI_label at the screen again
!
      CALL PGSCH   ( DIAGI_S%SCH_LAB  )
      CALL PGSLW   ( DIAGI_S%ISLW_LAB )
      DIAGI_S%MESS_BOT = DIAGI_LABEL__DEF
      CALL PGPTXT  ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, 0.0, 0.0, &
     &               DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT)) )
!
      DIAGI_S%XMIN = XMIN4
      DIAGI_S%XMAX = XMAX4
      DIAGI_S%YMIN = YMIN4
      DIAGI_S%YMAX = YMAX4
!
      RETURN
      END  !#!  DIAGI_CHF  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   DIAGI_TIT ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_TIT  from DiaGI package allows user to change the   *
! *   the title of the plot in interactive mode.                         *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *  <DIAGI_TIT> ( INTEGER*4 ) -- Return value.                          *
! *                            0 -- no real changes of the title of the  *
! *                                 plot occurred.                       *
! *                            1 -- Title of the plot have been changed. *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *      DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI       *
! *                               internal parameters.                   *
! *                                                                      *
! *  ###  13-OCT-97    DIAGI_TIT   v1.2  (c)  L. Petrov 08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  DIAGI_TIT, IL, I_LEN
      REAL*4     XMIN4, XMAX4, YMIN4, YMAX4
      CHARACTER  NEW_TITLE*128, MESS*128, TERMN*1
!
      TERMN=CHAR(3)
      XMIN4 = DIAGI_S%XMIN
      XMAX4 = DIAGI_S%XMAX
      YMIN4 = DIAGI_S%YMIN
      YMAX4 = DIAGI_S%YMAX
      CALL CLRCH ( MESS )
      MESS = 'Enter new title >>'
!
! --- Deleting the previous prompt
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --- Asking user to enter the new title
!
      CALL CLRCH   ( NEW_TITLE )
      CALL PGENTER ( MESS(1:I_LEN(MESS)), XMIN4, &
     &               YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, TERMN, NEW_TITLE, IL )
      DIAGI_TIT = 0
      IF ( IL .GT. 0 ) THEN
!
! -------- Somethinng has been typed
!
           IF ( DIAGI_S%ZAG(1:I_LEN(DIAGI_S%ZAG)) .NE. NEW_TITLE ) THEN
!
! ------------- Yes, new title differs from the previous one
!
                CALL CLRCH ( DIAGI_S%ZAG )
                DIAGI_S%ZAG = NEW_TITLE
                DIAGI_TIT = 1
           END IF
      END IF
!
! --- Putting the prompt back to the screen
!
      CALL PGSCH   ( DIAGI_S%SCH_LAB  )
      CALL PGSLW   ( DIAGI_S%ISLW_LAB )
      CALL PGPTXT  ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, 0.0, 0.0, &
     &               DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT)) )
!
      RETURN
      END  !#!  DIAGI_TIT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   DIAGI_UNITS ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_UNITS  from DiaGI package allows user to change the *
! *   the units of the x-axis.                                           *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI       *
! *                               internal parameters.                   *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *  <DIAGI_UNITS> ( INTEGER*4 ) -- Return value.                        *
! *                            0 -- no real changes of the units of the  *
! *                                 plot occurred.                       *
! *                            1 -- Units of the plot have been changed. *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *      DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI       *
! *                               internal parameters.                   *
! *                                                                      *
! *  ###  28-SEP-99   DIAGI_UNITS   v1.1 (c)  L. Petrov 07-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  DIAGI_UNITS, IL, I_LEN
      REAL*4     XMIN4, XMAX4, YMIN4, YMAX4
      CHARACTER  NEW_UNITS*128, MESS*128, TERMN*1
!
      TERMN=CHAR(3)
      XMIN4 = DIAGI_S%XMIN
      XMAX4 = DIAGI_S%XMAX
      YMIN4 = DIAGI_S%YMIN
      YMAX4 = DIAGI_S%YMAX
      CALL CLRCH ( MESS )
!
      MESS = 'Enter new units of x-axis >>'
!
! --- Deleting the previous prompt
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --- Asking user to enter the new units
!
      CALL CLRCH   ( NEW_UNITS )
      CALL PGENTER ( MESS(1:I_LEN(MESS)), XMIN4, &
     &               YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, TERMN, &
     &               NEW_UNITS, IL )
      DIAGI_UNITS = 0
      IF ( IL .GT. 0 ) THEN
!
! -------- Somethinng has been typed
!
           IF ( DIAGI_S%ARG_UNITS(1:I_LEN(DIAGI_S%ARG_UNITS)) .NE. &
     &          NEW_UNITS ) THEN
!
! ------------- Yes, new UNITS differs from the previous one
!
                CALL CLRCH ( DIAGI_S%ARG_UNITS )
                DIAGI_S%ARG_UNITS = NEW_UNITS
                DIAGI_UNITS = 1
           END IF
      END IF
!
! --- Putting the prompt back to the screen
!
      CALL PGSCH   ( DIAGI_S%SCH_LAB  )
      CALL PGSLW   ( DIAGI_S%ISLW_LAB )
      CALL PGPTXT  ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, 0.0, 0.0, &
     &               DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT)) )
!
      RETURN
      END  !#!  DIAGI_UNITS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_PURGE_BOT ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine DIAGI_PURGE_BOT purges the bottom message.       *
! *   It affects only interactive DaiGI device (IDEV=1 or IDEV=2) only   *
! *   and does nbot affect Muti_DiaGI.                                   *
! *                                                                      *
! * ### 08-AUG-2002  DIAGI_PURGE_BOT v1.0 (c) L. Petrov 08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      REAL*4     XLEFT_NC, XRIGHT_NC, YBOT_NC, YTOP_NC
      REAL*4     XLEFT_BV, XRIGHT_BV, YBOT_BV, YTOP_BV
      INTEGER*4  IFST, ICLI
!
      IF ( DIAGI_S%IDEV .GE. IXS__MIN  .AND.  &
     &     DIAGI_S%IDEV .LT. IBT__MIN         ) THEN
!
! -------- Inquire the current state
!
           CALL PGQVP  ( 0, XLEFT_NC, XRIGHT_NC, YBOT_NC, YTOP_NC )
           CALL PGQWIN (    XLEFT_BV, XRIGHT_BV, YBOT_BV, YTOP_BV )
           CALL PGQFS  ( IFST )
           CALL PGQCI  ( ICLI )
!
! -------- Re-map view port
!
           CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0 )
           CALL PGSWIN ( 0.0, 1.0, 0.0, 1.0 )
           CALL PGSFS  ( 1 )
           CALL PGSCI  ( 0 )
!
! -------- Put the box with background color
!
           CALL PGRECT ( 0.0, BOX_BOTMES(1,DIAGI_S%IDEV), &
     &                   0.0, BOX_BOTMES(2,DIAGI_S%IDEV)  )
!
! -------- Restore previous state
!
           CALL PGSFS  ( IFST )
           CALL PGSCI  ( ICLI )
           CALL PGSVP  ( XLEFT_NC, XRIGHT_NC, YBOT_NC, YTOP_NC )
           CALL PGSWIN ( XLEFT_BV, XRIGHT_BV, YBOT_BV, YTOP_BV )
      END IF
      RETURN
      END  !#!  DIAGI_PURGE_BOT  #!#
