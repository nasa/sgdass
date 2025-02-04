      SUBROUTINE DIAGI ( DIAGI_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DiaGI  is the main entry point to the DiaGI (Dialogue     *
! *   Graphic Interface) utility. DiaGI is the routine for interactive   *
! *   one-dimension graphic for X-envronment. It allows to show the      *
! *   plot up to 32 functions at the same plotting surface, to change    *
! *   interactively boundary of the plotting area, point style,          *
! *   line style, line width, error bar style; allows to make hardcopy   *
! *   in PostScript or GIF format and to send it at the printing device. *
! *                                                                      *
! *      Fields of the data structure DIAGI_S should be filled before    *
! *   usage of DIAGI.                                                    *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  10-OCT-1997    DIAGI    v2.55 (c)  L. Petrov 02-FEB-2024  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INCLUDE   'diagi_local.i'
      INCLUDE   'solve_paths.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  IUER
      CHARACTER  DEVICE*20, CH*1, LAST_CH*1, PGPLOT_DEFSTR*32
      INTEGER*4  IER, ID, ID_XW, ID_PRN, IC, IL, IP, IPRN, J1, J2, J3
      INTEGER*4  ICLR
      LOGICAL*4  EXIT_FLAG, PLOT_UPDATE
      CHARACTER  STR*128, STR1*80, DEVICE_PRN*128
#ifdef HPUX  ! Actually, it is a bug in HP-UX Fortran compiler
      PARAMETER  ( DIAGI__PGDN = 221 ) ! PageDown key code
      PARAMETER  ( DIAGI__PGUP = 220 ) ! PageUp   key code
#else
      PARAMETER  ( DIAGI__PGDN = CHAR(221) ) ! PageDown key code
      PARAMETER  ( DIAGI__PGUP = CHAR(220) ) ! PageUp   key code
#endif
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, LIB$CALLG, PGOPEN, &
     &           DIAGI_CHF, DIAGI_TIT, DIAGI_PRN, DIAGI_INQ, DIAGI_UNITS
!
      IF ( ILEN(DIAGI_S%MESS_BOT) .EQ. 0 ) THEN
           DIAGI_S%MESS_BOT = DIAGI_LABEL__DEF
      END IF
      DIAGI_S%MESS_BOT_SAVED = DIAGI_S%MESS_BOT
!
! --- Setting plotting parameters
!
      IF ( DIAGI_S%IBATCH .EQ. 0  .AND. &
     &     ( DIAGI_S%IDEV .LT. IXS__MIN  .OR. &
     &       DIAGI_S%IDEV .GT. IXS__MAX       )  ) THEN
!
! ---------- Correct default device type if it is necessary
!
             DIAGI_S%IDEV = IXS__DEF
        ELSE IF ( DIAGI_S%IBATCH == 1  .OR.  DIAGI_S%IBATCH == 2 ) THEN
!
! -------- Check validity of the device index
!
           IF ( DIAGI_S%IDEV .LT. IBT__MIN  .OR.  DIAGI_S%IDEV .GT. MDEV ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( DIAGI_S%IDEV, STR )
                CALL ERR_LOG ( 4101, IUER, 'DIAGI', 'Wrong device index '// &
     &               STR(1:I_LEN(STR))//' This device is not supported in '// &
     &              'batch mode' )
                RETURN
           END IF
      END IF
!
! --- Set sizes
!
      CALL DIAGI_SET ( DIAGI_S%IDEV, DIAGI_S )
!
! --- Learn which pgplot device is currently opened (if any)
!
      CALL PGQINF ( 'DEV/TYPE', STR, IL )
      IF ( INDEX ( STR, '/XS' ) .GT. 0 ) THEN
!
! -------- Aga, xwindow is already opened. Then merely erase it
!
           CALL PGERAS
           CALL PGQID ( ID_XW )
         ELSE IF ( DIAGI_S%IBATCH .EQ. 0 ) THEN
!
! -------- Setting X-resource for starting pgxwin server
!
           CALL CLRCH ( STR )
           STR = 'echo "pgxwin.Win.iconize: True" | xrdb -merge'
           CALL SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
!
! -------- Openning X-window plotting device
!
           ID_XW = PGOPEN ( DIAGI_S%DEVICE )
           IF ( ID_XW .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( ID_XW, STR )
                CALL CLRCH ( DEVICE )
                DEVICE = DIAGI_S%DEVICE
                CALL ERR_LOG ( 4102, IUER, 'DIAGI', 'Error in openning '// &
     &              'the graphic device '//DEVICE(1:I_LEN(DEVICE))// &
     &              ' IER='//STR )
                RETURN
           END IF
         ELSE IF ( DIAGI_S%IBATCH == 1  .OR.  DIAGI_S%IBATCH == 2 ) THEN
           IF ( ILEN(DIAGI_S%NAME) .EQ. 0 ) DIAGI_S%NAME = NAME__DEF
           DEVICE_PRN = DIAGI_S%NAME(1:I_LEN(DIAGI_S%NAME))// &
     &                  DEVS(DIAGI_S%IDEV)
           ID_PRN = PGOPEN ( DEVICE_PRN )
           IF ( ID_PRN .LE. 0 ) THEN
                CALL INCH  ( ID_PRN, STR )
                CALL CLRCH ( STR )
                CALL ERR_LOG ( 4103, IUER, 'DIAGI', 'Error in openning '// &
     &              'the graphic device '//DEVICE_PRN(1:I_LEN(DEVICE_PRN))// &
     &              ' IER='//STR )
                RETURN
           END IF
           ID_XW = 0
      END IF
!
! --- Allocation memory for internal DIAGI_S data structure, setting
! --- current values of plotting parameters
!
      CALL ERR_PASS  ( IUER, IER )
      CALL DIAGI_INT ( 1, DIAGI_S, ICLR, IER )
      IF ( IER > 0 ) THEN
           CALL PGEND()
!
! -------- Antiinitialization of internal data structure
! -------- (freeing dynamic memory etc)
!
           CALL DIAGI_INT ( 2, DIAGI_S, ICLR, 0 )
           DIAGI_S%STATUS = DIA__UND
!
           CALL ERR_LOG ( 4104, IUER, 'DIAGI', 'Error in initialization '// &
     &         'of DiaGI internal data structures' )
           RETURN
      END IF
!
! --- Setting colours
!
      CALL ERR_PASS ( IUER, IER )
      CALL DIAGI_CLS ( DIAGI_S, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 4105, IUER, 'DIAGI', 'Error in setting '// &
     &         'DiaGI colors' )
           RETURN
      END IF
      IF ( DIAGI_S%IBATCH .EQ. 1 ) THEN
           CALL PGSCR ( 0, 1.0, 1.0, 1.0 ) ! pure white background
      END IF
!
! --- Setting initial cursor position
!
      DIAGI_S%XC = ( DIAGI_S%XMIN + DIAGI_S%XMAX )/2.0
      DIAGI_S%YC = ( DIAGI_S%YMIN + DIAGI_S%YMAX )/2.0
!
! --- Setting default font type
!
      CALL PGSCF ( 2 )
      PLOT_UPDATE = .TRUE.
!
! --- Execute user init-function if necessary
!
      IF ( DIAGI_S%INIT_USER_FUNC .GT. 0                   .AND. &
     &     DIAGI_S%INIT_USER_FUNC .LE. DIAGI_S%NUSER_FUNC        ) THEN
!
! -------- Execute User init function
!
           IL = LIB$CALLG ( DIAGI_S%USER_ARG(0,DIAGI_S%INIT_USER_FUNC), &
     &                      %VAL(DIAGI_S%USER_FUNC(DIAGI_S%INIT_USER_FUNC)) )
           IF ( IL .NE. DIAGI__CONT  .AND.  IL .NE. DIAGI__QUIT ) THEN
!
! ------------- Oh! An error in user function. It is very bad...
!
                CALL CLRCH ( STR )
                CALL INCH  ( IL, STR )
                CALL CLRCH ( STR1 )
                IC = ICHAR ( CH )
                IF ( IC .LT. 0 ) IC=256+IC
                CALL INCH ( IC, STR1 )
                CALL ERR_LOG ( 4106, IUER, 'DIAGI', 'Error '// &
     &               STR(1:I_LEN(STR))//' during execution init user '// &
     &              'function: '//STR1 )
                RETURN
             ELSE IF ( IL .EQ. DIAGI__QUIT ) THEN
                GOTO 810
           END IF
           PLOT_UPDATE = .FALSE.
      END IF
      DIAGI_S%IPQ = 0
      DIAGI_S%ICQ = 0
!
! --- Initiliaze user repsonse
!
      CH = CHAR(0)
      EXIT_FLAG   = .TRUE.
!
! === Main loop. Main loop will be executed until user enter E or Q key
!     ~~~~~~~~~
!
      DO WHILE ( EXIT_FLAG )
         IF ( PLOT_UPDATE ) THEN
!
! ----------- Update the plot:
! ----------- Firstly erasing graphic window and printing axis box and the title
!
              DIAGI_S%MESS_BOT = DIAGI_S%MESS_BOT_SAVED
              CALL DIAGI_SET_FRAME ( DIAGI_S, DIAGI_S%MESS_BOT )
!
! ----------- ... then drawing plots of the functions, colour by colour.
! ----------- The next colour overlap the previous one.
!
              DO 410 J1=1,DIAGI_S%NCLR
                 CALL DIAGI_DRAW ( DIAGI_S, J1, 0, &
     &                DIAGI_S%NPOI(J1), %VAL(DIAGI_S%ADR_X4(J1)), &
     &                %VAL(DIAGI_S%ADR_Y4(J1)), %VAL(DIAGI_S%ADR_E4(J1)), &
     &                %VAL(DIAGI_S%ADR_X8(J1)), %VAL(DIAGI_S%ADR_Y8(J1)) )
 410          CONTINUE
!
! ----------- Setting status "No inquire has been made"
!
              DIAGI_S%IPQ = 0
              DIAGI_S%ICQ = 0
         END IF
         PLOT_UPDATE = .FALSE.
         IF ( DIAGI_S%UPDATE_USER_FUNC .GT. 0                   .AND. &
     &        DIAGI_S%UPDATE_USER_FUNC .LE. DIAGI_S%NUSER_FUNC        ) THEN
!
! ----------- Execute User update function
!
              IL = LIB$CALLG ( DIAGI_S%USER_ARG(0,DIAGI_S%UPDATE_USER_FUNC), &
     &                    %VAL(DIAGI_S%USER_FUNC(DIAGI_S%UPDATE_USER_FUNC)) )
              IF ( IL .NE. DIAGI__CONT  .AND.  IL .NE. DIAGI__QUIT ) THEN
!
! ---------------- Oh! An error in user update function. It is very bad...
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( IL, STR )
                   CALL CLRCH ( STR1 )
                   IC = ICHAR ( CH )
                   IF ( IC .LT. 0 ) IC=256+IC
                   CALL INCH ( IC, STR1 )
                   CALL ERR_LOG ( 4107, IUER, 'DIAGI', 'Error '// &
     &                  STR(1:I_LEN(STR))//' during execution update user '// &
     &                 'function: '//STR1 )
                   RETURN
                ELSE IF ( IL .EQ. DIAGI__QUIT ) THEN
                   GOTO 810
              END IF
         END IF
!
         IF ( DIAGI_S%IBATCH .EQ. 0 ) THEN
!
! ----------- Waiting for reading the key or mouse button.
!
              CALL PGCURS ( DIAGI_S%XC, DIAGI_S%YC, CH )
            ELSE IF ( DIAGI_S%IBATCH == 1  .OR.  DIAGI_S%IBATCH == 2 ) THEN
!
! ----------- It is batch mode. There is no sence to wait a user who might
! ----------- be sleeping now or entertaining while DiaGI is working hard.
! ----------- If a user does not want to do anything, we also have to finish
!
              CH = 'E'
              IF ( DIAGI_S%IBATCH == 2 ) THEN
                   CALL ERR_PASS  ( IUER, IER )
                   CALL DIAGI_SAV ( DIAGI_S, IER )
!
! ---------------- Antiinitialization of internal data structure 
! ---------------- (freeing dynamic memory etc)
!
                   CALL ERR_PASS  ( IUER, IER )
                   CALL DIAGI_INT ( 2, DIAGI_S, ICLR, IER )
                   DIAGI_S%STATUS = DIA__UND
                   CALL ERR_LOG ( 0, IUER )
                   RETURN 
              END IF
              DIAGI_S%ITRM = DIAGI__CLOSE
         END IF
!
         IF ( DIAGI_S%NUSER_FUNC  .GT.  0 ) THEN
!
! ----------- Aga! The were user defined function. check binding code.
! ----------- It has the precedence over thge built-in functions
!
              DO 420 J2=1,DIAGI_S%NUSER_FUNC
                 IF ( DIAGI_S%USER_CHR(J2) .EQ. CHAR(0) ) GOTO 420
                 IF ( CH .EQ. DIAGI_S%USER_CHR(J2) ) THEN
                      IF ( DIAGI_S%USER_FUNC(J2) .EQ. 0 ) THEN
                           CALL CLRCH ( STR1 )
                           IC = ICHAR ( CH )
                           IF ( IC .LT. 0 ) IC=256+IC
                           CALL INCH ( IC, STR1 )
                           CALL ERR_LOG ( 4109, IUER, 'DIAGI', 'Error '// &
     &                         'in an attempt to invoke the user function '// &
     &                         'bound the the key code '//STR1(1:I_LEN(STR1))// &
     &                         ' -- the function address is zero' )
                           RETURN
                      END IF
!
                      IL = LIB$CALLG ( DIAGI_S%USER_ARG(0,J2), &
     &                                 %VAL(DIAGI_S%USER_FUNC(J2)) )
                      IF ( IL .NE. DIAGI__CONT  .AND. &
     &                     IL .NE. DIAGI__QUIT            ) THEN
!
! ------------------------ Ohhh! An error in user function. It is very bad...
!
                           CALL CLRCH ( STR )
                           CALL INCH ( IL, STR )
                           CALL CLRCH ( STR1 )
                           IC = ICHAR ( CH )
                           IF ( IC .LT. 0 ) IC=256+IC
                           CALL INCH ( IC, STR1 )
                           CALL ERR_LOG ( 4110, IUER, 'DIAGI', 'Error '// &
     &                          STR(1:I_LEN(STR))//' during execution user '// &
     &                         'function invoked by the key with code '// &
     &                          STR1 )
                           RETURN
                      END IF
                      LAST_CH = CH
!
! ------------------- Special treatment of -1 code.
!
                      IF ( IL .EQ. DIAGI__QUIT ) THEN
                           DIAGI_S%LAST_KEY = LAST_CH ! Store the key
                           GOTO 810
                      END IF
!
! ------------------- Disable the code
!
                      CH = CHAR(0)
                 END IF
 420         CONTINUE
         END IF
!
         DIAGI_S%LAST_KEY = CH ! Store the key
         IF ( CH .EQ. CHAR(16) ) THEN
!
! =========== CNTRL/P -- printing the plot.
!
! ----------- Firstly, select the plotting device and the plotting type
!
              ID = DIAGI_PRN ( DIAGI_S%NAME, DEVICE_PRN, ID_PRN, IPRN )
!
              IF ( IPRN .EQ. 2 ) THEN
                   CALL PGCLOQ
                   CALL ERR_LOG ( 4111, IUER, 'DIAGI', 'Error in '// &
     &                 'attempt to open the output file for hardcopy: '// &
     &                  DEVICE_PRN )
                   RETURN
              END IF
!
              IF ( ID_PRN .GT. 0 ) THEN
!
! ---------------- Set parameters for the new plotting device
!
                   CALL DIAGI_SET ( ID_PRN, DIAGI_S )
!
! ---------------- Open plotting device
!
                   IP = PGOPEN ( DEVICE_PRN )
                   IF ( IP .GE. 0 ) THEN
!
! --------------------- Successfull openning. Setting new plotting window
!
                        CALL PGSVP ( 0.0, 1.0, 0.0, 1.0 )
!
! --------------------- Setting anew colour table for the new colour device
!
                        CALL ERR_PASS  ( IUER, IER )
                        CALL DIAGI_CLS ( DIAGI_S, IER )
                        IF ( IER > 0 ) THEN
                             CALL ERR_LOG ( 4112, IUER, 'DIAGI', 'Error in '// &
     &                           'setting DiaGI colors for device '//DEVICE_PRN )
                        END IF
                        CALL PGSCR ( 0, 1.0, 1.0, 1.0 ) ! pure white background
!
! --------------------- Drawing the plot: axis box and functions
!
                        CALL DIAGI_SET_FRAME ( DIAGI_S, ' ' )
                        DO 430 J3=1,DIAGI_S%NCLR
                           CALL DIAGI_DRAW ( DIAGI_S, J3, 0, &
     &                          DIAGI_S%NPOI(J3), %VAL(DIAGI_S%ADR_X4(J3)), &
     &                          %VAL(DIAGI_S%ADR_Y4(J3)), &
     &                          %VAL(DIAGI_S%ADR_E4(J3)), &
     &                          %VAL(DIAGI_S%ADR_X8(J3)), &
     &                          %VAL(DIAGI_S%ADR_Y8(J3)) )
 430                    CONTINUE
!
! --------------------- Inquiring environment variable for the consequent
! --------------------- printing
!
                        CALL GETENVAR ( DIAGI_PRICOM, STR )
                        IF ( IPRN .EQ. 1  .AND. ILEN(STR) .EQ. 0 ) THEN
!
! -------------------------- Printing command has not been selected
!
                             IPRN = 0
                          ELSE IF ( IPRN .EQ. 1 .AND. ILEN(STR) .GT. 0 ) THEN
!
! -------------------------- Physical printing mode was selected and environment
! -------------------------- variable has been set up
!
                             IL = LINDEX ( DEVICE_PRN, '/' ) - 1
!
! -------------------------- Issuing the UNIX command for printing
!
                             CALL SYSTEM ( STR(1:I_LEN(STR))//' '// &
     &                                     DEVICE_PRN(1:IL)//' &'//CHAR(0) )
!                             write ( 6, * ) ' >>',STR(1:I_LEN(STR))//' '// ! %%
!     #                                      DEVICE_PRN(1:IL)//' &<<'       ! %%
                        END IF
                      ELSE
!
! --------------------- Error in opening printing device
!
                        CALL PGSLCT    ( ID_XW )
                        CALL PGCLOQ
!
                        CALL DIAGI_INT ( 2, DIAGI_S, ICLR, 0 )
                        DIAGI_S%STATUS = DIA__UND
!
                        CALL CLRCH ( STR )
                        CALL INCH  ( IP, STR )
                        CALL ERR_LOG ( 4113, IUER, 'DIAGI', 'Error in '// &
     &                      'openning the graphic device '// &
     &                       DEVICE_PRN(1:I_LEN(DEVICE_PRN))//' IER='//STR )
                        RETURN
                   END IF
!                           type *,' >>',DEVICE_PRN(1:IL),'<<',         ! %%%
!     #                            ' id_prn = ',id_prn,' iprn = ',iprn  ! %%%
!
! ---------------- Closing printing devise
!
                   CALL PGCLOS
                   CALL PGSLCT    ( ID_XW )
!
! ---------------- Resetting current plottiing parameters
!
                   CALL DIAGI_SET ( DIAGI_S%IDEV, DIAGI_S )
                   CALL PGSVP     ( 0.0, 1.0, 0.0, 1.0  )
!
! ---------------- Issuing the final printing message
!
                   CALL DIAGI_PPR ( DEVICE_PRN, IP, IPRN )
              END IF
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'A'  .OR.  CH .EQ. 'a' ) THEN
!
! =========== Inquire the point coordinate. Status "Inquire has been made" is
!             set up.
!
              ID = DIAGI_INQ ( DIAGI_S, DIAGI_S%NPOI(ICLR), &
     &             %VAL(DIAGI_S%ADR_X4(ICLR)), %VAL(DIAGI_S%ADR_Y4(ICLR)), &
     &             %VAL(DIAGI_S%ADR_E4(ICLR)), DIAGI_S%XC, DIAGI_S%YC, &
     &             DIAGI_S%IPQ )
              DIAGI_S%ICQ = ICLR
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'B'  .OR.  CH .EQ. 'b' ) THEN
!
! =========== Setting new error bar representation mode
!
              IF ( DIAGI_S%LER(ICLR) ) THEN
                   CALL DIAGI_BST ( DIAGI_S )
                   PLOT_UPDATE = .TRUE.
              END IF
              DIAGI_S%IPQ = 0
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'C'  .OR.  CH .EQ. 'c' ) THEN
!
! =========== Setting new main colour
!
              IF ( DIAGI_S%NCLR .GT. 1 ) THEN
                   CALL DIAGI_CCL ( DIAGI_S, ICLR )
                   PLOT_UPDATE = .TRUE.
              END IF
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'D'  .OR.  CH .EQ. 'd' ) THEN
!
! =========== Setting new boundary of the plotting area
!
              ID = DIAGI_CHF ( DIAGI_S, DIAGI_S%XC, DIAGI_S%YC )
              IF ( ID .EQ. 1  .OR.  DIAGI_S%IPQ .GT. 0 ) THEN
                   PLOT_UPDATE = .TRUE.
              END IF
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'E'  .OR.  &
     &               CH .EQ. 'e'  .OR.  &
     &               CH .EQ. 'Q'  .OR.  &
     &               CH .EQ. 'q'        ) THEN
!
! =========== Termination of the routune
!
              GOTO 810
           ELSE IF ( CH .EQ. 'H'  .OR.  &
     &               CH .EQ. 'h'  .OR.  &
     &               CH .EQ. '?'        ) THEN
!
! =========== Printing he content of Help file at the graphic device
!
              CALL ERR_PASS  ( IUER, IER )
              CALL DIAGI_HLP ( ' ', IER )
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'I'  .OR.  CH .EQ. 'i' ) THEN
!
! =========== Resetting initial parameters
!
              IER = 0
              CALL DIAGI_INT ( -1, DIAGI_S, ICLR, IER )
!
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'L'  .OR.  CH .EQ. 'l' ) THEN
!
! =========== Setting new line style
!
              CALL DIAGI_LST ( DIAGI_S )
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'M'  .OR.  CH .EQ. 'm' ) THEN
!
! =========== Setting boundaries to the minimal box
!
              CALL ERR_PASS  ( IUER,    IER )
              CALL DIAGI_MNB ( DIAGI_S, IER )
              IF ( IER > 0 ) THEN
                   CALL ERR_LOG ( 4114, IUER, 'DIAGI', 'Trap of internal '// &
     &                 'control: DIAGI_S data structure is corrupted' )
                   RETURN
              END IF
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'O'  .OR.  CH .EQ. 'o' ) THEN
!
! =========== Setting new Overplot style
!
              CALL DIAGI_OST ( DIAGI_S )
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'P'  .OR.  CH .EQ. 'p' ) THEN
!
! =========== Setting new point style
!
              CALL DIAGI_PST ( DIAGI_S )
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'R'  .OR.  CH .EQ. 'r' ) THEN
              PLOT_UPDATE = .TRUE.
           ELSE IF ( CH .EQ. 'S'  .OR.  CH .EQ. 's' ) THEN
!
! =========== Save plot
!
              CALL ERR_PASS  ( IUER, IER )
              CALL DIAGI_SAV ( DIAGI_S, IER )
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'T'  .OR.  CH .EQ. 't' ) THEN
!
! =========== Setting new plot title
!
              ID = DIAGI_TIT ( DIAGI_S )
              IF ( ID .EQ. 1 ) THEN
                   PLOT_UPDATE = .TRUE.
              END IF
!
! ----------- Setting status "No inquire has been made"
!
              DIAGI_S%IPQ = 0
              DIAGI_S%ICQ = 0
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'U'  .OR.  CH .EQ. 'u' ) THEN
!
! =========== Setting new plot units
!
              ID = DIAGI_UNITS ( DIAGI_S )
              IF ( ID .EQ. 1 ) THEN
                   PLOT_UPDATE = .TRUE.
              END IF
!
! ----------- Setting status "No inquire has been made"
!
              DIAGI_S%IPQ = 0
              DIAGI_S%ICQ = 0
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'V'  .OR.  CH .EQ. 'v' ) THEN
!
! =========== Printing at the test window the matrix of the plotting values
! =========== for the current color
!
              CALL DIAGI_VIE ( DIAGI_S )
              WRITE ( 6, * ) ' '
              WRITE ( 6, * ) ' '
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'W'  .OR.  CH .EQ. 'w' ) THEN
!
! =========== Change the width of the line
!
              CALL DIAGI_WST ( DIAGI_S )
              PLOT_UPDATE = .TRUE.
              LAST_CH = CH
           ELSE IF ( CH .EQ. 'X'  .OR.  CH .EQ. 'x' ) THEN
              IF ( LAST_CH .EQ. 'X'  .OR.  LAST_CH .EQ. 'x' ) THEN
!
! ================ Terminate plotting
!
                   GOTO 810
              END IF
              LAST_CH = CH
           ELSE IF ( CH .EQ. DIAGI__PGUP  .OR.  CH .EQ. DIAGI__PGDN ) THEN
              GOTO 810
         END IF
      END DO
!
! --- End of work
!
 810  CONTINUE
!
! --- Check, whether we need to execute user quit function
!
      IF ( DIAGI_S%QUIT_USER_FUNC .GT. 0                   .AND. &
     &     DIAGI_S%QUIT_USER_FUNC .LE. DIAGI_S%NUSER_FUNC        ) THEN
!
! -------- Execute User quit function
!
           IL = LIB$CALLG ( DIAGI_S%USER_ARG(0,DIAGI_S%QUIT_USER_FUNC), &
     &                      %VAL(DIAGI_S%USER_FUNC(DIAGI_S%QUIT_USER_FUNC)) )
           IF ( IL .NE. DIAGI__CONT  .AND.  IL .NE. DIAGI__QUIT ) THEN
!
! ------------- Oh! An error in user function. It is very bad...
!
                CALL CLRCH ( STR )
                CALL INCH  ( IL, STR )
                CALL CLRCH ( STR1 )
                IC = ICHAR ( CH )
                IF ( IC .LT. 0 ) IC=256+IC
                CALL INCH ( IC, STR1 )
                CALL ERR_LOG ( 4115, IUER, 'DIAGI', 'Error '// &
     &               STR(1:I_LEN(STR))//' during execution user quit '// &
     &              'function: '//STR1 )
                RETURN
           END IF
      END IF
!
! --- Erasing the screen
!
      CALL GRQCAP ( PGPLOT_DEFSTR )
      IF ( DIAGI_S%ITRM .EQ. DIAGI__CLOSE_VERBOSE  .AND. &
     &     PGPLOT_DEFSTR(8:8) .EQ. 'N' ) THEN
!
           CALL PGERAS
!
! -------- Printing farwell message
!
           CALL PGSCH    ( 1.666 )
           CALL PGSLW    ( 5     )
           CALL PGPTXT   ( (DIAGI_S%XMAX+DIAGI_S%XMIN)/2, &
     &                     (DIAGI_S%YMAX+DIAGI_S%YMIN)/2, 0.0, 0.5, &
     &                     'Please, iconify (by <Alt/blank> <n>) '// &
     &                     'graphic window manually' )
      END IF
!
      IF ( DIAGI_S%ITRM .EQ. DIAGI__ERASE  ) THEN
           CALL PGERAS
         ELSE IF ( DIAGI_S%ITRM .EQ. DIAGI__KEEP ) THEN
           CONTINUE
         ELSE
!
! -------- Closing plotting device
!
           CALL PGENDQ
      END IF
!
! --- Antiinitialization of internal data structure (freeing dynamic memory etc)
!
      CALL ERR_PASS  ( IUER, IER )
      CALL DIAGI_INT ( 2, DIAGI_S, ICLR, IER )
      DIAGI_S%STATUS = DIA__UND
!
! --- Printing farwell message at the user terminal and waiting for
! --- user reaction
!
      IF ( DIAGI_S%ITRM .EQ. 1 ) THEN
           CALL HIT_CONT ( 'DiaGI completed its work. Please hit any key to '// &
     &                     'proceed  '//CHAR(1), %VAL(0) )
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIAGI  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_CLS ( DIAGI_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_CLS sets up colour table. It binds colour indeces   *
! *   with RGB colour representation. Couur indeces then used by PGPLOT. *
! *   5 colours are defined to use for internall needs of DiaGI:         *
! *   Background (white, but not snowy white); Foreground (black),       *
! *   Error colour (brightly red); Slightly grey (for representation     *
! *   option boxes); Deepy grey (fro representation chosen boxes).       *
! *   Then colours for representation plot of functions are set up:      *
! *   twocolours per function: main colur and "light main" colour (for   *
! *   representation uncertainties of the function).                     *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *                                                                      *
! *  ###  16-OCT-1997  DIAGI_CLS    v2.0 (c)  L. Petrov 27-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INCLUDE   'diagi_local.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4    IUER
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = 128 ) 
      PARAMETER  ( MIND =  32 ) 
      CHARACTER  DIAGI_COLOR_FILE*128, BUF(MBUF)*128, STR*128, STR1*128
      INTEGER*4  IP, IRGB_USE(MCLR,2,3), NBUF, CLR_IND, CLR_DEF(2,3), &
     &           LIND, IND(2,MIND), INDW, J1, J2, J3, J4, IER 
      INTEGER*4, EXTERNAL ::   ILEN
!
      IRGB_USE = IRGB_DEF
      CALL GETENVAR ( 'DIAGI_COLOR', DIAGI_COLOR_FILE )
      IF ( ILEN(DIAGI_COLOR_FILE) == 0 ) THEN
           DIAGI_COLOR_FILE = PETOOLS_PREFIX//'/share/'//DIAGI_COLOR_FILE_DEFAULT
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( DIAGI_COLOR_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 4141, IUER, 'DIAGI_CLS', 'Error in reading '// &
     &         'DiaGI color definition file '//DIAGI_COLOR_FILE )
           RETURN 
      END IF
      IF ( BUF(1)(1:LEN(DIAGI_COLOR__LABEL)) .NE. DIAGI_COLOR__LABEL ) THEN 
           CALL CLRCH ( STR )
           STR = BUF(1)
           CALL TRAN ( 13, STR,  STR )
           CALL ERR_LOG ( 4142, IUER, 'DIAGI_CLS', 'Wrong format of the color '// &
     &         'definition file '//TRIM(DIAGI_COLOR_FILE)//' -- the first line '// &
     &         'is '//TRIM(STR)//' while '//DIAGI_COLOR__LABEL//' was expected' )
           RETURN 
      END IF
!
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND .LT. 8 ) THEN
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4143, IUER, 'DIAGI_CLS', 'Error in parsing line '// &
     &             TRIM(STR)//' of the color definition file '// &
     &             TRIM(DIAGI_COLOR__LABEL)//' -- it has less than 8 words' )
              RETURN 
         END IF
         CALL CHIN ( BUF(J1)(IND(1,1):IND(2,1)), CLR_IND )
         IF ( CLR_IND < 1 .OR. CLR_IND > MCLR ) THEN
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4144, IUER, 'DIAGI_CLS', 'Error in parsing line '// &
     &             TRIM(STR)//' of the color definition file '// &
     &             TRIM(DIAGI_COLOR__LABEL)//' -- wrong color index' )
              RETURN 
         END IF
         INDW = 2
         DO 420 J2=1,2
            DO 430 J3=1,3
               INDW = INDW + 1
               CALL CHIN ( BUF(J1)(IND(1,INDW):IND(2,INDW)), CLR_DEF(J2,J3) )
               IF ( CLR_DEF(J2,J3) < 0 .OR. CLR_DEF(J2,J3) > 255 ) THEN
                    CALL CLRCH (       STR )
                    CALL INCH  ( J1,   STR )
                    CALL CLRCH (       STR1 )
                    CALL INCH  ( INDW, STR1 )
                    CALL ERR_LOG ( 4145, IUER, 'DIAGI_CLS', 'Error in parsing line '// &
     &                   TRIM(STR)//' of the color definition file '// &
     &                   TRIM(DIAGI_COLOR__LABEL)//' -- wrong color index in '// &
     &                  'word '//STR1 )
                    RETURN 
               END IF
 430        CONTINUE 
 420     CONTINUE 
         IRGB_USE(CLR_IND,1:2,1:3) = CLR_DEF
 410  CONTINUE 
!
! --- Hard coded colours
!
      CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
      CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
      CALL PGCOL_RGB ( ERR_CLRI, ERR_CLR(1), ERR_CLR(2), ERR_CLR(3) )
      CALL PGCOL_RGB ( SLG_CLRI, SLG_CLR(1), SLG_CLR(2), SLG_CLR(3) )
      CALL PGCOL_RGB ( DEG_CLRI, DEG_CLR(1), DEG_CLR(2), DEG_CLR(3) )
!
! --- User defined colours
!
      DO 440 J4=1,DIAGI_S%NCLR
         IF ( DIAGI_S%ICOL(J4) .EQ. -1 ) THEN
!
! ----------- Colour was specified directly
!
              CALL PGCOL_RGB ( ITAB_CLR(J4,1),     DIAGI_S%MCOL_R(J4), &
     &                         DIAGI_S%MCOL_G(J4), DIAGI_S%MCOL_B(J4)  )
              CALL PGCOL_RGB ( ITAB_CLR(J4,2),     DIAGI_S%SCOL_R(J4), &
     &                         DIAGI_S%SCOL_G(J4), DIAGI_S%SCOL_B(J4)  )
            ELSE IF (DIAGI_S%ICOL(J4) .GE. 1  .AND.  DIAGI_S%ICOL(J4) .LE. MCLR) &
     &      THEN
!
! ----------- Colour was specified as index in the table of default colours
!
              IP = DIAGI_S%ICOL(J4)
              CALL PGCOL_RGB ( ITAB_CLR(J4,1), &
     &             IRGB_USE(IP,1,1), IRGB_USE(IP,1,2), IRGB_USE(IP,1,3)  )
              CALL PGCOL_RGB ( ITAB_CLR(J4,2), &
     &             IRGB_USE(IP,2,1), IRGB_USE(IP,2,2), IRGB_USE(IP,2,3)  )
            ELSE
!
! ----------- Colour was specified as default colour (index of the colour
! ----------- coincides with index of built-in colour table).
!
              IP = J4
              CALL PGCOL_RGB ( ITAB_CLR(J4,1), &
     &             IRGB_USE(IP,1,1), IRGB_USE(IP,1,2), IRGB_USE(IP,1,3)  )
              CALL PGCOL_RGB ( ITAB_CLR(J4,2), &
     &             IRGB_USE(IP,2,1), IRGB_USE(IP,2,2), IRGB_USE(IP,2,3)  )
         END IF
 440  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIAGI_CLS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PGCOL_RGB ( ICL, IR, IG, IB )
! ************************************************************************
! *                                                                      *
! *   Auxullary routine  PGCOL_RGB  sets RGB colour for the internal     *
! *   PGPLOT colour index ICL. Colour is coded as INTEGER*4 values for   *
! *   Read, Gren, Blue. Values are aloowed to be in the range [0, 256].  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   ICL ( INTEGER*4 ) -- colour index. Should be in the range          *
! *                        [0, max_col] max_col < 256.                   *
! *    IR ( INTEGER*4 ) -- Intensivity of red   in the range [0, 256].   *
! *    IG ( INTEGER*4 ) -- Intensivity of green in the range [0, 256].   *
! *    IB ( INTEGER*4 ) -- Intensivity of blue  in the range [0, 256].   *
! *                                                                      *
! *  ###  16-OCT-97    PGCOL_RGB   v1.0  (c)  L. Petrov  16-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  ICL, IR, IG, IB
      REAL*4     R, G, B
      R = FLOAT(IR)/256.
      G = FLOAT(IG)/256.
      B = FLOAT(IB)/256.
      CALL PGSCR ( ICL, R, G, B )
      RETURN
      END  !#!  PGCOL_RGB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_SET ( ID, DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine DIAGI_SET  sets in internal data structure DIAGI_S         *
! *   device-dependent PGPLOT parameters for the supported device with   *
! *   index ID.                                                          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *        ID ( INTEGER*4 ) -- Index of the plotting device in the array *
! *                            DEVS defined in diagi.i                   *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI internal *
! *                            parameters.                               *
! *                                                                      *
! *  ###  13-OCT-1997  DIAGI_SET   v1.3  (c)  L. Petrov  10-DEC-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INTEGER*4  ID
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  WID_STR*80, HEI_STR*80, STR*20
      INTEGER*4  IWID, IHEI, IOS
      REAL*4     XRES, PGPLOT_GIF_RES, PGPLOT_PNG_RES
      PARAMETER  ( PGPLOT_GIF_RES = 85.0 )
      PARAMETER  ( PGPLOT_PNG_RES = 98.7 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      SAVE       WID_STR, HEI_STR  ! Should SAVE for putenv !!!
!
      IF ( ID .GE. 1  .AND.  ID .LE. MDEV ) THEN
           DIAGI_S%DEVICE    = DEVS(ID)
           DIAGI_S%XLEFT     = XLEFTS(ID)
           DIAGI_S%XRIGHT    = XRIGHTS(ID)
           DIAGI_S%YBOT      = YBOTS(ID)
           DIAGI_S%YTOP      = YTOPS(ID)
           DIAGI_S%SCH_LAB   = SCH_LABS(ID)
           DIAGI_S%YSH_LAB   = YSH_LABS(ID)
           DIAGI_S%ISLW_LAB  = ISLW_LABS(ID)
           DIAGI_S%SCH_TIT   = SCH_TITS(ID)
           DIAGI_S%YSH_TIT   = YSH_TITS(ID)
           DIAGI_S%ISLW_TIT  = ISLW_TITS(ID)
           DIAGI_S%SCH_FRM   = SCH_FRMS(ID)
           DIAGI_S%IWD_LINS(1) = IWID_THN
           DIAGI_S%IWD_LINS(2) = IWID_MED(ID)
           DIAGI_S%IWD_LINS(3) = IWID_THK(ID)
           DIAGI_S%YSH_ARU     = YSH_ARU(ID)
           DIAGI_S%RAD_SMALL   = RAD_SMALL_MM
           DIAGI_S%RAD_LARGE   = RAD_LARGE_MM
           IF ( (DIAGI_S%XRIGHT - DIAGI_S%XLEFT) .LE. 60.0 ) THEN
                DIAGI_S%RAD_SMALL = RAD_TINY_MM
                DIAGI_S%RAD_LARGE = RAD_SMALL_MM
           END IF
           DIAGI_S%IFIRST_FIELD = ID
         ELSE
           DIAGI_S%IFIRST_FIELD = 0
      END IF
!
      IF ( INDEX ( DEVS(ID), 'VCPS' ) > 0  ) THEN
!
! -------- Ad hoc adjusment of the left border. For some reason Postscript
! -------- PGPLOT driver adds extra space and distorts computation of the aspect
! -------- ratio. Here we adjust it.
!
           DIAGI_S%XL_ADJ = 27.0 ! mm
         ELSE 
           DIAGI_S%XL_ADJ = 0.0
      END IF
      IF ( INDEX ( DEVS(ID), 'GIF' ) .NE. 0 ) THEN
!
! -------- In the case of GIF "device" we should define environment variables
! -------- to specify physical size of the plotting area (in pixels)
!
           IWID = (XLEFTS(ID)+XRIGHTS(ID))*PGPLOT_GIF_RES/25.4
           CALL CLRCH ( STR )
           CALL CLRCH ( WID_STR )
           CALL INCH  ( IWID, STR )
           WID_STR = 'PGPLOT_GIF_WIDTH='//STR(1:I_LEN(STR))//CHAR(0)
           CALL PUTENV ( WID_STR(1:I_LEN(WID_STR)+1) )
!
           IHEI = (YBOTS(ID)+YTOPS(ID))*PGPLOT_GIF_RES/25.4
           CALL CLRCH ( STR )
           CALL CLRCH ( HEI_STR )
           CALL INCH  ( IHEI, STR )
           HEI_STR = 'PGPLOT_GIF_HEIGHT='//STR(1:I_LEN(STR))//CHAR(0)
           CALL PUTENV ( HEI_STR(1:I_LEN(HEI_STR)+1) )
!
! -------- Now check environment variable DIAGI_X_RESOLUTION
! -------- This variable keeps actual resolution of the screen
!
           CALL GETENVAR ( 'DIAGI_X_RESOLUTION', STR )
           IF ( ILEN(STR) .GT. 0 ) THEN
                IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR = STR(1:I_LEN(STR))//'.0'
                READ ( UNIT=STR, FMT='(F16.10)', IOSTAT=IOS ) XRES
                IF ( IOS .EQ. 0  .AND. XRES .GE. 10 .AND. XRES .LE. 200 ) THEN
!
! ------------------ Well, we know actual resolution of the screen and we
! ------------------ know resolution of the GIF driver. Now we have to
! ------------------ re-scale sizes of the plot.
!
                     DIAGI_S%XLEFT  = XRES/PGPLOT_GIF_RES * DIAGI_S%XLEFT
                     DIAGI_S%XRIGHT = XRES/PGPLOT_GIF_RES * DIAGI_S%XRIGHT
                     DIAGI_S%YBOT   = XRES/PGPLOT_GIF_RES * DIAGI_S%YBOT
                     DIAGI_S%YTOP   = XRES/PGPLOT_GIF_RES * DIAGI_S%YTOP
                END IF
          END IF
      END IF
      IF ( INDEX ( DEVS(ID), 'PNG' ) .NE. 0 ) THEN
!
! -------- In the case of GIF "device" we should define environment variables
! -------- to specify physical size of the plotting area (in pixels)
!
           IWID = (XLEFTS(ID)+XRIGHTS(ID))*PGPLOT_PNG_RES/25.4
           CALL CLRCH ( STR )
           CALL CLRCH ( WID_STR )
           CALL INCH  ( IWID, STR )
           WID_STR = 'PGPLOT_PNG_WIDTH='//STR(1:I_LEN(STR))//CHAR(0)
           CALL PUTENV ( WID_STR(1:I_LEN(WID_STR)+1) )
!
           IHEI = (YBOTS(ID)+YTOPS(ID))*PGPLOT_PNG_RES/25.4
           CALL CLRCH ( STR )
           CALL CLRCH ( HEI_STR )
           CALL INCH  ( IHEI, STR )
           HEI_STR = 'PGPLOT_PNG_HEIGHT='//STR(1:I_LEN(STR))//CHAR(0)
           CALL PUTENV ( HEI_STR(1:I_LEN(HEI_STR)+1) )
!
! -------- Now check environment variable DIAGI_X_RESOLUTION
! -------- This variable keeps actual resolution of the screen
!
           CALL GETENVAR ( 'DIAGI_X_RESOLUTION', STR )
           IF ( ILEN(STR) .GT. 0 ) THEN
                IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR = STR(1:I_LEN(STR))//'.0'
                READ ( UNIT=STR, FMT='(F16.10)', IOSTAT=IOS ) XRES
                IF ( IOS .EQ. 0  .AND. XRES .GE. 10 .AND. XRES .LE. 200 ) THEN
!
! ------------------ Well, we know actual resolution of the screen and we
! ------------------ know resolution of the PNG driver. Now we have to
! ------------------ re-scale sizes of the plot.
!
                     DIAGI_S%XLEFT  = XRES/PGPLOT_PNG_RES * DIAGI_S%XLEFT
                     DIAGI_S%XRIGHT = XRES/PGPLOT_PNG_RES * DIAGI_S%XRIGHT
                     DIAGI_S%YBOT   = XRES/PGPLOT_PNG_RES * DIAGI_S%YBOT
                     DIAGI_S%YTOP   = XRES/PGPLOT_PNG_RES * DIAGI_S%YTOP
                END IF
          END IF
      END IF
!
      RETURN
      END  !#!  DIAGI_SET  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_INT ( IP, DIAGI_S, ICLR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_INT  in dependence of the values of parameter  IP   *
! *   makes one of the action:  IP=1 -- "Allocation" -- initialization   *
! *   internal data structure DIAGI_S for further drawing plot. It also  *
! *   saves current plotting parameters which can be restored lately by  *
! *   calling DIAGI_INT woth code operation -1. IP=-1 -- "Coming back to *
! *   initial values of plotting parameters", IP=2 -- "Dealocation" --   *
! *   freeing grabbed dynamic memory.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *        IP ( INTEGER*4 ) -- Code of operation. One of -1, 1, 2.       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      ICLR ( INTEGER*4 ) -- Index of the main colour.                 *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI internal *
! *                            parameters.                               *
! * IUER ( INTEGER*4, OPT ) -- Universal error habdler.                  *
! *                            Input: swicth IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will pe put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successfull      *
! *                                    completion and non-zero in the    *
! *                                    case of error.                    *
! *                                                                      *
! *  ###  13-OCT-97    DIAGI_INT   v2.4  (c)  L. Petrov 31-DEC-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  IP, ICLR, IUER
      CHARACTER  STR*20
      INTEGER*4  J1, J2, J3, J4, J5, IER, NPT
      ADDRESS__TYPE  :: MEM_LEN, MEM_ADR
      REAL*4     EPS_MIN4, EPS_MAX4
      PARAMETER  ( EPS_MIN4 = -1.E30 )
      PARAMETER  ( EPS_MAX4 =  1.E30 )
      INTEGER*4, EXTERNAL :: I_LEN
!
      ICLR=-1
      IF ( IP .EQ. 1 ) THEN
!
! -------- Operation "Allocation"
!
           IF ( DIAGI_S%STATUS .EQ. DIA__DEF ) THEN
!
! ------------- Compute NPT -- to total number of points among all colors.
! ------------- We will later grab actually more memory than needed, since
! ------------- some user DiaGi functions may distribute points among colors.
! ------------- In order to prevent inadvertent memory corruption, we merely
! ------------- allocate more memory
!
                NPT = 0
                DO 410 J1=1,DIAGI_S%NCLR
                   NPT = NPT + DIAGI_S%NPOI(J1)
 410            CONTINUE
!
! ------------- Cycle on colors and grabbing dynamic memory for REAL*4 arrays
! ------------- of arguments, values (and errors) of the plottinng function
!
                DO 420 J2=1,DIAGI_S%NCLR
                   IER = 0
                   IF ( DIAGI_S%LER(J2) ) THEN
!
! --------------------- Grabbing memory for array of arguments, values, errors
!
#ifdef ADR_32BIT                     
                        CALL GRAB_MEM ( IER,   DIAGI_S%MEM_LEN(J2),    &
     &                                         DIAGI_S%MEM_ADR(J2), 3, &
     &                                  4*NPT, DIAGI_S%ADR_X4(J2),     &
     &                                  4*NPT, DIAGI_S%ADR_Y4(J2),     &
     &                                  4*NPT, DIAGI_S%ADR_E4(J2)      )
#else
                        CALL GRAB_MEM ( IER,   DIAGI_S%MEM_LEN(J2),      &
     &                                         DIAGI_S%MEM_ADR(J2), 3,   &
     &                                  INT8(4*NPT), DIAGI_S%ADR_X4(J2), &
     &                                  INT8(4*NPT), DIAGI_S%ADR_Y4(J2), &
     &                                  INT8(4*NPT), DIAGI_S%ADR_E4(J2)  )
#endif
                      ELSE
!
! --------------------- Grabbing memory for array of argument and values
!
#ifdef ADR_32BIT                     
                        CALL GRAB_MEM ( IER,   DIAGI_S%MEM_LEN(J2),    &
     &                                         DIAGI_S%MEM_ADR(J2), 2, &
     &                                  4*NPT, DIAGI_S%ADR_X4(J2),     &
     &                                  4*NPT, DIAGI_S%ADR_Y4(J2)      )
#else
                        CALL GRAB_MEM ( IER,   DIAGI_S%MEM_LEN(J2),      &
     &                                         DIAGI_S%MEM_ADR(J2), 2,   &
     &                                  INT8(4*NPT), DIAGI_S%ADR_X4(J2), &
     &                                  INT8(4*NPT), DIAGI_S%ADR_Y4(J2)  )
#endif
                        DIAGI_S%IBST = 0
                   END IF
                   IF ( IER > 0 ) THEN
                        WRITE ( 6, * ) ' Color number: ',J2,' Number of points: ', &
     &                           DIAGI_S%NPOI(J2)
                        CALL CLRCH  ( STR )
                        CALL IINCH  ( MEM_LEN, STR )
                        CALL ERR_LOG ( 4131, IUER, 'DIAGI_INT', 'Error '// &
     &                      'during the next attempt of grabbing '// &
     &                       STR(1:I_LEN(STR))//' bytes of memory' )
                        RETURN
                   END IF
!
! ---------------- Copying with transformation RELA*8 --> REAL*4
!
                   CALL COPY_R8_R4 ( DIAGI_S%NPOI(J2), %VAL(DIAGI_S%ADR_X8(J2)), &
     &                                                 %VAL(DIAGI_S%ADR_X4(J2)))
                   CALL COPY_R8_R4 ( DIAGI_S%NPOI(J2), %VAL(DIAGI_S%ADR_Y8(J2)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(J2)))
!
! ---------------- Check of -INF or +INF values in arrays of formal errors
!
                   CALL DIAGI_RANGE4 ( DIAGI_S%NPOI(J2), &
     &                  %VAL(DIAGI_S%ADR_X4(J2)), EPS_MIN4, EPS_MAX4 )
                   CALL DIAGI_RANGE4 ( DIAGI_S%NPOI(J2), &
     &                  %VAL(DIAGI_S%ADR_Y4(J2)), EPS_MIN4, EPS_MAX4 )
!
                   IF ( DIAGI_S%LER(J2) ) THEN
                        CALL COPY_R8_R4 ( DIAGI_S%NPOI(J2), &
     &                       %VAL(DIAGI_S%ADR_E8(J2)), %VAL(DIAGI_S%ADR_E4(J2)))
                        CALL DIAGI_RANGE4 ( DIAGI_S%NPOI(J2), &
     &                       %VAL(DIAGI_S%ADR_E4(J2)), EPS_MIN4, EPS_MAX4 )
                   END IF
!
 420            CONTINUE
                DIAGI_S%STATUS = DIA__ALL
                IF ( ( (DIAGI_S%XMAX - DIAGI_S%XMIN) .LT. 1.E-30 ) .OR. &
     &               ( (DIAGI_S%YMAX - DIAGI_S%YMIN) .LT. 1.E-30 ) .OR. &
     &               ( (DIAGI_S%XMAX - DIAGI_S%XMIN) .LT. DIAGI_EPS*    &
     &                 MAX(ABS(DIAGI_S%XMIN),ABS(DIAGI_S%XMAX))  ) .OR. &
     &               ( (DIAGI_S%YMAX - DIAGI_S%YMIN) .LT. DIAGI_EPS*    &
     &                 MAX(ABS(DIAGI_S%YMIN),ABS(DIAGI_S%YMAX))  )      ) THEN
!
! ------------------ If the current values of the boundaries are set incorrectly
! ------------------ calculate the minimal box
!
                     CALL ERR_PASS  ( IUER, IER )
                     CALL DIAGI_MNB ( DIAGI_S, IER )
                     IF ( IER > 0 ) THEN
                          CALL ERR_LOG ( 4132, IUER, 'DIAGI', 'Trap of '// &
     &                        'internal control: DIAGI_S data structure '// &
     &                        'is corrupted' )
                          RETURN
                     END IF
                END IF
             ELSE IF ( DIAGI_S%STATUS .EQ. DIA__ALL ) THEN
                CONTINUE
             ELSE
                WRITE ( 6, * ) ' DIAGI_S%STATUS = ',DIAGI_S%STATUS
                CALL ERR_LOG ( 4133, IUER, 'DIAGI_INT', 'Status of DIAGI_S '// &
     &                        'data structure was not defined and operation '// &
     &                        '"allocation" cannot be executed' )
                RETURN
           END IF
!
! -------- Saving current values of plotting parameters
!
           IF ( DIAGI_S%STATUS .EQ. DIA__ALL ) THEN
                 IF ( DIAGI_S%ICLR .LT. 1  .OR.      &
     &               DIAGI_S%ICLR .GT. DIAGI_S%NCLR ) THEN
                     DIAGI_S%ICLR = 1
                END IF
!
                DIAGI_S%XMIN_SAV = DIAGI_S%XMIN
                DIAGI_S%XMAX_SAV = DIAGI_S%XMAX
                DIAGI_S%YMIN_SAV = DIAGI_S%YMIN
                DIAGI_S%YMAX_SAV = DIAGI_S%YMAX
                DIAGI_S%ZAG_SAV  = DIAGI_S%ZAG
                DIAGI_S%ICLR_SAV = DIAGI_S%ICLR
!
                DO 430 J3=1,DIAGI_S%NCLR
                   DIAGI_S%IBST_SAV(J3) = DIAGI_S%IBST(J3)
                   DIAGI_S%ILST_SAV(J3) = DIAGI_S%ILST(J3)
                   DIAGI_S%IPST_SAV(J3) = DIAGI_S%IPST(J3)
                   DIAGI_S%IWST_SAV(J3) = DIAGI_S%IWST(J3)
 430            CONTINUE
             ELSE
                CALL ERR_LOG ( 4134, IUER, 'DIAGI_INT', 'Status of DIAGI_S '// &
     &                        'data structure was not "Allocated" and '// &
     &                        'operation "return to initial parameters" '// &
     &                        'cannot be executed' )
                RETURN
           END IF
        ELSE IF ( IP .EQ. -1 ) THEN
!
! -------- Set current plotting parameters to the initial values (which were
! -------- set up jast after the call of DIAGI
!
           IF ( DIAGI_S%STATUS .EQ. DIA__ALL ) THEN
!
                DIAGI_S%XMIN = DIAGI_S%XMIN_SAV
                DIAGI_S%XMAX = DIAGI_S%XMAX_SAV
                DIAGI_S%YMIN = DIAGI_S%YMIN_SAV
                DIAGI_S%YMAX = DIAGI_S%YMAX_SAV
                DIAGI_S%ZAG  = DIAGI_S%ZAG_SAV
                DIAGI_S%ICLR = DIAGI_S%ICLR_SAV
                DO 440 J4=1,DIAGI_S%NCLR
                   DIAGI_S%IBST(J4) = DIAGI_S%IBST_SAV(J4)
                   DIAGI_S%ILST(J4) = DIAGI_S%ILST_SAV(J4)
                   DIAGI_S%IPST(J4) = DIAGI_S%IPST_SAV(J4)
                   DIAGI_S%IWST(J4) = DIAGI_S%IWST_SAV(J4)
 440            CONTINUE
             ELSE
                CALL ERR_LOG ( 4135, IUER, 'DIAGI_INT', 'Status of DIAGI_S '// &
     &                        'data structure was not "Allocated" and '//      &
     &                        'operation "Coming back to inital values of '//  &
     &                        'polottig parameters" cannot be executed' )
                RETURN
           END IF
        ELSE IF ( IP .EQ. 2 ) THEN
!
! -------- Operation "Deallocation"
!
           IF ( DIAGI_S%STATUS .EQ. DIA__ALL ) THEN
!
! ------------- Freeing dynamic memory
!
                DO 450 J5=1,DIAGI_S%NCLR
                   CALL FREE_MEM ( DIAGI_S%MEM_ADR(J5) )
 450            CONTINUE
                DIAGI_S%STATUS = DIA__DEF
             ELSE
                CALL ERR_LOG ( 4136, IUER, 'DIAGI_INT', 'Status of DIAGI_S '// &
     &                        'data structure was not "Allocated" and '//      &
     &                        'operation "Deallocation" cannot be executed' )
                RETURN
           END IF
        ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( IP, STR )
           CALL ERR_LOG ( 4137, IUER, 'DIAGI_INT', 'Wrong code of '//     &
     &                   'operation: IP='//STR(1:I_LEN(STR))//'. Only '// &
     &                   'codes (-1,1,2) are supported' )
           RETURN
      END IF
!
      DIAGI_S%SET_VP       = .TRUE.
      DIAGI_S%ERASE_SCREEN = .TRUE.
      ICLR = DIAGI_S%ICLR
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIAGI_INT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_MNB ( DIAGI_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routune  DIAGI_MNB  calculates new boundaries of the plotting area *
! *   in automatic mode. It finds miminal box which would contain all    *
! *   points of all functions. Precisely spaking DIAGI_MNB sets boundary *
! *   a bit larger to leave small fields ( 2% ) in order to improve      *
! *   readability of the plot.                                           *
! *                                                                      *
! *   NB: This is internal routine. It should not be called from user    *
! *   program directly.                                                  *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *                                                                      *
! *  ###  21-OCT-97    DIAGI_MNB   v1.4  (c)  L. Petrov 04-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      REAL*4     XMIN_V, XMAX_V, YMIN_V, YMAX_V, XSPR, YSPR
      INTEGER*4  IUER, J1
!
! --- Scan all functions...
!
      IF ( DIAGI_S%STATUS .NE. DIA__ALL ) THEN
           CALL ERR_LOG ( 4211, IUER, 'DIAGI_MNB', 'Trap of internal '// &
     &         'control: memory for internal data structures in DIAGI_S '// &
     &         'has not been yet allocated' )
           RETURN
      END IF
!
      DO 410 J1=1,DIAGI_S%NCLR
         IF ( DIAGI_S%NPOI(J1) .LE. 0 ) GOTO 410  ! Bypass empty array
!
! ------ ... and find minimal and maximal values of the argument and the value
! ------ of the function
!
         CALL DIAGI_MINMAX_VAL ( DIAGI_S%NPOI(J1), %VAL(DIAGI_S%ADR_X4(J1)), &
     &                           XMIN_V, XMAX_V )
         CALL DIAGI_MINMAX ( DIAGI_S%NPOI(J1), %VAL(DIAGI_S%ADR_Y4(J1)), &
     &                       DIAGI_S%LER(J1),  %VAL(DIAGI_S%ADR_E4(J1)), &
     &                       YMIN_V, YMAX_V )
!
         IF ( J1 .EQ. 1 ) THEN
              DIAGI_S%XMIN = XMIN_V
              DIAGI_S%XMAX = XMAX_V
              DIAGI_S%YMIN = YMIN_V
              DIAGI_S%YMAX = YMAX_V
           ELSE
              IF ( XMIN_V .LT. DIAGI_S%XMIN ) DIAGI_S%XMIN = XMIN_V
              IF ( XMAX_V .GT. DIAGI_S%XMAX ) DIAGI_S%XMAX = XMAX_V
              IF ( YMIN_V .LT. DIAGI_S%YMIN ) DIAGI_S%YMIN = YMIN_V
              IF ( YMAX_V .GT. DIAGI_S%YMAX ) DIAGI_S%YMAX = YMAX_V
         END IF
 410  CONTINUE
!
! --- Making small fields
!
      XSPR = DIAGI_S%XMAX - DIAGI_S%XMIN
!
! --- Making important trick: we trace if the relative difference between
! --- the boudaries exceeds the limit (else PGPLOT may fail due to rounding
! --- errors)
!
      IF ( XSPR .LT. DIAGI_EPS*ABS(DIAGI_S%XMIN)  .OR. XSPR .LT. 1.E-30 ) THEN
           XSPR = DIAGI_EPS*(MAX(ABS(DIAGI_S%XMIN),(ABS(DIAGI_S%XMAX))))/ &
     &           (DIAGI_FIE/2.01)
           IF ( XSPR .LT. 1.E-30 ) XSPR = 1.E-30
      END IF
!
      YSPR = DIAGI_S%YMAX - DIAGI_S%YMIN
      IF ( YSPR .LT. DIAGI_EPS*ABS(DIAGI_S%YMIN)  .OR. YSPR .LT. 1.E-30 ) THEN
           YSPR = DIAGI_EPS*(MAX(ABS(DIAGI_S%YMIN),(ABS(DIAGI_S%YMAX))))/ &
     &           (DIAGI_FIE/2.01)
           IF ( YSPR .LT. 1.E-30 ) YSPR = 1.E-30
      END IF
!
! --- Setting small fields
!
      DIAGI_S%XMIN = DIAGI_S%XMIN - XSPR*DIAGI_FIE
      DIAGI_S%XMAX = DIAGI_S%XMAX + XSPR*DIAGI_FIE
      DIAGI_S%YMIN = DIAGI_S%YMIN - YSPR*DIAGI_FIE
      DIAGI_S%YMAX = DIAGI_S%YMAX + YSPR*DIAGI_FIE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIAGI_MNB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_MINMAX ( N, ARR_R4, LER, ERR_R4, ARR_MIN, ARR_MAX )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  DIAGI_MINMAX  calculates minimal and maximal    *
! *   values of the function. It takes into account errors. If errors    *
! *   are specified it fill calculate minimal and maximal values with    *
! *   taking into account error bar. Thus, minimal value will be         *
! *   "min(y) - sigma(y_min)" and maximal value will be                  *
! *   "max(y) + sigma(y_max)".                                           *
! *                                                                      *
! *  ###  28-NOV-97  DIAGI_MINMAX  v2.0  (c)  L. Petrov  24-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1
      LOGICAL*4  LER
      REAL*4     ARR_R4(N), ERR_R4(N), ARR_MIN, ARR_MAX
!
      IF ( LER ) THEN
           ARR_MIN = ARR_R4(1) - ABS(ERR_R4(1))
           ARR_MAX = ARR_R4(1) + ABS(ERR_R4(1))
           DO 410 J1=2,N
              IF ( ARR_R4(J1) - ABS(ERR_R4(J1)) .LT. ARR_MIN ) THEN
                   ARR_MIN = ARR_R4(J1) - ABS(ERR_R4(J1))
              END IF
              IF ( ARR_R4(J1) + ABS(ERR_R4(J1)) .GT. ARR_MAX ) THEN
                   ARR_MAX = ARR_R4(J1) + ABS(ERR_R4(J1))
              END IF
 410       CONTINUE
        ELSE
           CALL DIAGI_MINMAX_VAL ( N, ARR_R4, ARR_MIN, ARR_MAX )
      END IF
!
      RETURN
      END  !#!  DIAGI_MINMAX  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_RANGE4 ( N, ARR, EPS_MIN4, EPS_MAX4 )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  DIAGI_RANGE4  examine values of REAL*4 array    *
! *   ARR. If a value is outside the range [EPS_MIN4, EPS_MAX4] it is    *
! *   assigned to the boundary of the range. If it is "not-a-number" it  *
! *   is assigned to 0.                                                  *
! *                                                                      *
! *  ###  24-DEC-98   DIAGI_RANGE4 v1.0  (c)  L. Petrov  24-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1
      REAL*4     ARR(N), EPS_MIN4, EPS_MAX4
      LOGICAL*4  IS_R4_NAN
!
      DO 410 J1=1,N
         IF ( ARR(J1) .LT. EPS_MIN4 ) ARR(J1) = EPS_MIN4
         IF ( ARR(J1) .GT. EPS_MAX4 ) ARR(J1) = EPS_MAX4
         IF ( IS_R4_NAN ( ARR(J1) ) ) ARR(J1) = 0.0
 410  CONTINUE
      RETURN
      END  !#!  DIAGI_RANGE4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_MINMAX_VAL ( N, ARR, ARR_MIN, ARR_MAX )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine for computin minimal and maximal value of the    *
! *   vector.                                                            *
! *                                                                      *
! * ### 01-AUG-2002 DIAGI_MINMAX_VAL v1.0 (c) L. Petrov 01-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N
      REAL*4     ARR(N), ARR_MIN, ARR_MAX
      INTEGER*4  J1
!
      DO 410 J1=1,N
         IF ( J1 .EQ. 1 ) THEN
              ARR_MIN = ARR(1)
              ARR_MAX = ARR(1)
            ELSE IF ( ARR(J1) .GT. ARR_MAX ) THEN
              ARR_MAX = ARR(J1)
            ELSE IF ( ARR(J1) .LT. ARR_MIN ) THEN
              ARR_MIN = ARR(J1)
         END IF
 410  CONTINUE
      RETURN
      END  !#!  DIAGI_MINMAX_VAL  #!#
