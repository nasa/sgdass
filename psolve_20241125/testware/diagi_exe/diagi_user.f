      PROGRAM    DIAGI_USER
! ************************************************************************
! *                                                                      *
! *   Demonstration program for using DiaGI user functions.              *
! *                                                                      *
! *   DIAGI_USER binds 3 user-defined functions with four keys:          *
! *   1) A ( Left Mouse Button ) -- SHIFT_POINT -- shifts the point down *
! *      to a specified amount ("ambiguity spacing"). The place the      *
! *      point was is shown by light grey color.                         *
! *   2) X ( Right Mouse Button ) -- SHIFT_POINT -- shifts the point up  *
! *      to a specified amount ("ambiguity spacing"). The place the      *
! *      point was is shown by light grey color. This function emulates  *
! *      manual ambiguity shift.                                         *
! *   3) D ( Middle Mouse Button ) -- SUPP_TOGGLE -- toggles the color   *
! *      of the point. If the color of the point was 1 (green)           *
! *      it becomes 2 (red) and vice versus. This function emulates      *
! *      manual ambiguity suppression/restoration.                       *
! *   4) Z -- TOGGLE_SUP_MODE -- toggles normal DiaGI binding of A, D, X *
! *      (mouse buttons) with user defined. Initial state is: user       *
! *      defined mode.                                                   *
! *   5) USER_INIT function.                                             *
! *   6) USER_QUIT function.                                             *
! *                                                                      *
! *  ###  06-AUG-2002  DIAGI_USER  v1.1  (c)  L. Petrov 23-MAR-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'diagi.i'
      INCLUDE    'diagi_local.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M, L1, L2
      PARAMETER  ( M=80 )
      REAL*8     T(M), T1(M), X1(M), E1(M), XMIN, XMAX, YMIN, YMAX, AMB_SP
      REAL*8     X(M), T2(M), X2(M), E2(M), E(M)
      LOGICAL*4  FL_USE(M)
      LOGICAL*4  FL_FIRST
!
      EXTERNAL   SHIFT_POINT, SUPP_TOGGLE, TOGGLE_SUP_MODE, USER_INIT, &
     &           USER_QUIT  ! Important!
      INTEGER*4  DIAGI_LOC_EXT
!
      INTEGER*4  NAMB(M), J1, MP, IPQ, IBST, ILST, IPST, IWST, ICLR, IUER
      INTEGER*4  DIAGI_LEN
      INTEGER*4  IWAY_UP, IWAY_DOWN
      CHARACTER  ZAG*80, STR*64
!
! --- Define a function
!
      CALL CLRCH ( ZAG )
      L1 = 48
      L2 = 32
      IPQ = 0
!
      L1 = 0
      L2 = 0
      DO 410 J1=1,M
         T(J1) = 0.D0 + 0.07*(J1-1)
         X(J1) = COS(T(J1)) + 0.2*SIN(30.*T(J1)) - 1.0
         E(J1) = 0.05
         IF ( MOD(J1,2) .EQ. 0 ) THEN
              L1 = L1 + 1
              T1(L1) = T(J1)
              X1(L1) = X(J1)
              E1(L1) = E(J1)
              FL_USE(J1) = .TRUE.
              NAMB(J1) = 0
            ELSE
              L2 = L2 + 1
              T2(L2) = T(J1)
              X2(L2) = X(J1) + 3.00
              E2(L2) = E(J1)
              FL_USE(J1) = .FALSE.
              NAMB(J1) = 1
         END IF
 410  CONTINUE
!
! --- Set the title
!
      ZAG = 'Ambiguity shift and suppression'
!
! --- Copy some variables. We cannot use pass a constants as an argument to
! --- a DiaGI user programs
!
      MP = M
      AMB_SP = 1.0D0
      IWAY_DOWN = -1
      IWAY_UP   =  1
!
! --- We need this argument. A special memory re-arrangments should be done in
! --- firstr call of this specific DiaGI user programms. We have to
! --- distinguish whether DiaFI user progrma was called the first times or not
!
      FL_FIRST = .TRUE.
!
      XMIN = -0.5
      XMAX =  4.5
      YMIN = -3.0
      YMAX =  2.0
      IBST = 0
      ILST = 3
      IPST = 2
      IWST = 2
      ICLR = 1
      IUER = -1
!
! --- Clear DIAGI_S object. We always must do it!
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4 ! Learn length
      CALL NOUT ( DIAGI_LEN, DIAGI_S ) ! Put zeroes
!
! --- New define fields of the DIAGI_S object
!
      DIAGI_S%IDEV      = IXS__DEF
      CALL GETENV ( 'DIAGI_SCREEN', STR )
      CALL TRAN   ( 11, STR, STR ) 
      IF ( STR .EQ. 'SMALL' ) THEN
           DIAGI_S%IDEV = 2
         ELSE IF ( STR .EQ. 'BIG' ) THEN
           DIAGI_S%IDEV = 1
      END IF
!
      DIAGI_S%NPOI(1)   = L1 ! number of points oif the first function (green)
      DIAGI_S%ADR_X8(1) = LOC(T1)  ! put address of the dirst element
      DIAGI_S%ADR_Y8(1) = LOC(X1)  ! put address of the dirst element
      DIAGI_S%ADR_E8(1) = LOC(E1)  ! put address of the dirst element
      DIAGI_S%LER(1)    = .TRUE.
!
      DIAGI_S%ICOL(1)   = 1
      DIAGI_S%IBST(1)   = 2
      DIAGI_S%ILST(1)   = 1
      DIAGI_S%IOST(1)   = 1
      DIAGI_S%IPST(1)   = 5
      DIAGI_S%IWST(1)   = 1
!
      DIAGI_S%NPOI(2)   = L2 ! number of points oif the first function (red)
      DIAGI_S%ADR_X8(2) = LOC(T2)
      DIAGI_S%ADR_Y8(2) = LOC(X2)
      DIAGI_S%ADR_E8(2) = LOC(E2)
      DIAGI_S%LER(2)    = .TRUE.
!
      DIAGI_S%ICOL(2)   = 3
      DIAGI_S%IBST(2)   = 2
      DIAGI_S%ILST(2)   = 1
      DIAGI_S%IOST(2)   = 1
      DIAGI_S%IPST(2)   = 5
      DIAGI_S%IWST(2)   = 1
!
      DIAGI_S%NCLR      = 2
      DIAGI_S%ICLR      = 1
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%ARG_UNITS = 'Arg'
      DIAGI_S%NAME      = '/tmp/test'
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%XMIN      = -0.1
      DIAGI_S%XMAX      =  6.3
      DIAGI_S%YMIN      = -3.0
      DIAGI_S%YMAX      =  3.0
!
! --- This part for DiaGI user functions
!
      DIAGI_S%NUSER_FUNC = 6 ! total number of user functions
      DIAGI_S%USER_FUNC(1) = DIAGI_LOC_EXT(SHIFT_POINT)     ! Address of the entry point
      DIAGI_S%USER_FUNC(2) = DIAGI_LOC_EXT(SUPP_TOGGLE)     ! of each user fuinction
      DIAGI_S%USER_FUNC(3) = DIAGI_LOC_EXT(SHIFT_POINT)     ! User functions must be
      DIAGI_S%USER_FUNC(4) = DIAGI_LOC_EXT(TOGGLE_SUP_MODE) ! declared as EXTERNAL
      DIAGI_S%USER_FUNC(5) = DIAGI_LOC_EXT(USER_INIT)       ! declared as EXTERNAL
      DIAGI_S%USER_FUNC(6) = DIAGI_LOC_EXT(USER_QUIT)       ! declared as EXTERNAL
!
      DIAGI_S%INIT_USER_FUNC = 5
      DIAGI_S%QUIT_USER_FUNC = 6
!
      DIAGI_S%USER_ARG(0,1)  = 9 ! number of arguments for SHIFT_POINT
      DIAGI_S%USER_ARG(1,1)  = LOC(DIAGI_S)   !  Argument list for
      DIAGI_S%USER_ARG(2,1)  = LOC(MP)        !  SHIFT_POINT
      DIAGI_S%USER_ARG(3,1)  = LOC(T)         !
      DIAGI_S%USER_ARG(4,1)  = LOC(X)         !  NB: all arguments are passed
      DIAGI_S%USER_ARG(5,1)  = LOC(E)         !  by reference.
      DIAGI_S%USER_ARG(6,1)  = LOC(NAMB)      !
      DIAGI_S%USER_ARG(7,1)  = LOC(AMB_SP)    !  Array USER_ARG keeps addresses
      DIAGI_S%USER_ARG(8,1)  = LOC(IWAY_DOWN) !  of the arguments
      DIAGI_S%USER_ARG(9,1)  = LOC(IUER)      !
!
      DIAGI_S%USER_ARG(0,2)  = 8 ! number of arguments for SUPP_TOGGLE
      DIAGI_S%USER_ARG(1,2)  = LOC(DIAGI_S)
      DIAGI_S%USER_ARG(2,2)  = LOC(MP)
      DIAGI_S%USER_ARG(3,2)  = LOC(T)
      DIAGI_S%USER_ARG(4,2)  = LOC(X)
      DIAGI_S%USER_ARG(5,2)  = LOC(E)
      DIAGI_S%USER_ARG(6,2)  = LOC(FL_USE)
      DIAGI_S%USER_ARG(7,2)  = LOC(FL_FIRST)
      DIAGI_S%USER_ARG(8,2)  = LOC(IUER)
!
      DIAGI_S%USER_ARG(0,3)  = 9 ! numer of arguments for SHIFT_POINT
      DIAGI_S%USER_ARG(1,3)  = LOC(DIAGI_S)  ! This time this function is
      DIAGI_S%USER_ARG(2,3)  = LOC(MP)       ! called with another
      DIAGI_S%USER_ARG(3,3)  = LOC(T)        ! value of the 8-th argument
      DIAGI_S%USER_ARG(4,3)  = LOC(X)
      DIAGI_S%USER_ARG(5,3)  = LOC(E)
      DIAGI_S%USER_ARG(6,3)  = LOC(NAMB)
      DIAGI_S%USER_ARG(7,3)  = LOC(AMB_SP)
      DIAGI_S%USER_ARG(8,3)  = LOC(IWAY_UP)
      DIAGI_S%USER_ARG(9,3)  = LOC(IUER)
!
      DIAGI_S%USER_ARG(0,4)  = 1 ! number of arguments for TOGGLE_SUP_MODE
      DIAGI_S%USER_ARG(1,4)  = LOC(DIAGI_S)
!
! --- Bind user functions with keys
!
      DIAGI_S%USER_CHR(1) = 'A'  ! The same as Mouse Left   Button
      DIAGI_S%USER_CHR(2) = 'D'  ! The same as Mouse Middle Button
      DIAGI_S%USER_CHR(3) = 'X'  ! The same as Mouse Right  Button
      DIAGI_S%USER_CHR(4) = 'Z'
      DIAGI_S%USER_CHR(5) = CHAR(5)
      DIAGI_S%USER_CHR(6) = CHAR(6)
!
      DIAGI_S%USER_ARG(0,5)  = 5 ! number of arguments for DIAGI_INIT
      DIAGI_S%USER_ARG(1,5)  = LOC(DIAGI_S)
!
      DIAGI_S%USER_ARG(0,6)  = 6 ! number of arguments for DIAGI_QUIT
      DIAGI_S%USER_ARG(1,6)  = LOC(DIAGI_S)
!
! --- Re-defined bottoom message (if the bottom message were undefined
! --- DIAGI_LBALE__DEF would have been used as a default )
!
      DIAGI_S%MESS_BOT = 'Left/Right Mouse for shift, Middle Mouse for '// &
     &                   'toggle/supp mode, Z for normal mode'
      DIAGI_S%STATUS   = DIA__DEF ! status: DiaGI fields are defined
!
! --- Call DiaGI
!
      IUER  = -1
      CALL DIAGI ( DIAGI_S, IUER )
!
      WRITE ( 6, * ) ' IUER = ' ,IUER
      END  !#!  DIAGI_USER  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SHIFT_POINT ( DIAGI_S, M, T, X, E, NAMB, AMB_SP, IWAY, IUER )
! ************************************************************************
! *                                                                      *
! *   DiaGI user finction function SHIFT_POINT causes the point which    *
! *   the cursor points at to be shifted at AMP_SP up (if IWAY=1) or     *
! *   AMB_SP down (if IWAY=-1). Array NAMB keeps tracks of shifting.     *
! *   If the K-th point is hsfted UP then NAMB(K) := NAMB(K) + 1. If     *
! *   it is shifted down then NAMB(K) := NAMB(K) - 1.                    *
! *                                                                      *
! *  ### 06-AUG-2002  SHIFT_POINT   v1.0 (c) L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  SHIFT_POINT
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M, IWAY, IUER
      REAL*8     T(M), X(M), E(M), AMB_SP
      INTEGER*4  NAMB(M)
      REAL*4     XC_ARG, YC_VAL, EC_SIG, DIST_X, DIST_Y, ARG_DIST
      REAL*4     DIST_MM
      INTEGER*4  J1, IPM, ICLR, IPQ, ILST_SAVED, ICLR_GRY
      PARAMETER  ( ICLR_GRY = 16 )
      PARAMETER  ( DIST_MM  = 10.0 ) ! If there is no point closer to the
!                                    ! the current cursor position than
!                                    ! DIST_MM mm, then SHIFT_POINT will
!                                    ! do nothing.
!
! --- Define light grey color. In fact DiaGI does not devine all possible
! --- colors, but only colors for the functions which are used.
!
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_GRY,1),   IRGB_DEF(ICLR_GRY,1,1), &
     &                 IRGB_DEF(ICLR_GRY,1,2), IRGB_DEF(ICLR_GRY,1,3)  )
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_GRY,2),   IRGB_DEF(ICLR_GRY,1,1), &
     &                 IRGB_DEF(ICLR_GRY,1,2), IRGB_DEF(ICLR_GRY,1,3)  )
!
      DIST_X = 10.0/(XRIGHTS(1)-XLEFTS(1))*(DIAGI_S%XMAX - DIAGI_S%XMIN)
      DIST_Y = 10.0/(YTOPS(1)-YBOTS(1))*(DIAGI_S%YMAX - DIAGI_S%YMIN)
!
! --- Search for the point the closest to the current cursor position
! --- within the sqrt(dist_2** + dist_y**2) area
! --- Only the points within 10.0 mm at each coordintes will be taken into
! --- consideration
!
      CALL UD_SEARCH ( &
     &     DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
     &     DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX, &
     &     DIAGI_S%XC, DIAGI_S%YC, DIST_X, DIST_Y, ICLR, IPQ )
      IF ( IPQ .GT. 0 ) THEN
!
! -------- Hoplya! The point was found. Let's copy its coordinates
!
           CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_X4(ICLR)+(IPQ-1)*4), XC_ARG )
           CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_Y4(ICLR)+(IPQ-1)*4), YC_VAL )
           CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_E4(ICLR)+(IPQ-1)*4), EC_SIG )
!
! -------- Redraw the point
!
           ILST_SAVED = DIAGI_S%ILST(ICLR)
           DIAGI_S%ILST(1) = 1 ! enter point-by-point mode. It is done as
!                              ! a precaution. If the current line mode is
!                              ! "spline" than DIAGI may abnormally terminated
!
! -------- First draw it by background color (extinguish the current point)
!
           CALL DIAGI_DRAW ( DIAGI_S, ICLR, 0, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                       0.0D0, 0.0D0 )
!
! -------- Then draw the current point by light grey color
!
           CALL DIAGI_DRAW ( DIAGI_S, ICLR, ICLR_GRY, 1, XC_ARG, YC_VAL, &
     &                       EC_SIG, 0.0D0, 0.0D0 )
!
! -------- Chane the value ofthe point
!
           YC_VAL = YC_VAL + IWAY*AMB_SP
!
! -------- ... cange this value in the internal plotting array
!
           CALL LIB$MOVC3( 4, YC_VAL, %VAL(DIAGI_S%ADR_Y4(ICLR)+(IPQ-1)*4) )
!
! -------- Redraw the point with the current color
!
           CALL DIAGI_DRAW ( DIAGI_S, ICLR, ICLR, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                       0.0D0, 0.0D0 )
           DIAGI_S%ILST(1) = ILST_SAVED
!
! -------- Now we should find the point in the extended list. Internal plotting
! -------- array will be destroyed after leaving DiaGI, but a user wants
! -------- to save infromation about point shifts
!
           ARG_DIST = (DIAGI_S%XMAX - DIAGI_S%XMIN)
           DO 410 J1=1,M
              IF ( ABS ( T(J1) - XC_ARG ) .LT. ARG_DIST ) THEN
                   ARG_DIST = ABS ( T(J1) - XC_ARG )
                   IPM = J1
              END IF
 410       CONTINUE
!
! -------- ... in order to save the changes in the array NAMB which a user
! -------- really wants
!
           NAMB(IPM) = NAMB(IPM) + IWAY
      END IF
!
      SHIFT_POINT = 1 ! Important: function should always return 1.
!                     ! if the function returns not 1, then DiaGI will print
!                     !  the error mesage and return to the main program
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SHIFT_POINT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION TOGGLE_SUP_MODE ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  TOGGLE_SUP_MODE  toggles suppression+shift mode with      *
! *   normal mode. Mouse buttons are bound with suppression and shift    *
! *   operations in the suppression+shift mode. These button have        *
! *   different function in normal DiaGI mode.                           *
! *                                                                      *
! * ### 08-AUG-2002 TOGGLE_SUP_MODE  v1.0 (c)  L. Petrov 08-AUG-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  TOGGLE_SUP_MODE
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  I_LEN
!
! --- Purge the current bottom message
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
      IF ( DIAGI_S%USER_CHR(1) .EQ. 'A' ) THEN
!
! -------- Unbind user functions. CHAR(0) code reserved as an "unbound" value
!
           DIAGI_S%USER_CHR(1) = CHAR(0)
           DIAGI_S%USER_CHR(2) = CHAR(0)
           DIAGI_S%USER_CHR(3) = CHAR(0)
!
! -------- Restore default label value
!
           DIAGI_S%MESS_BOT = DIAGI_LABEL__DEF
        ELSE
!
! -------- Bind three user functions.
!
           DIAGI_S%USER_CHR(1) = 'A'
           DIAGI_S%USER_CHR(2) = 'D'
           DIAGI_S%USER_CHR(3) = 'X'
           DIAGI_S%MESS_BOT = 'Left/Right Mouse for shift, Middle Mouse for '// &
     &                        'toggle/supp mode, Z for normal mode'
      END IF
!
! --- We need an additional copoy of MESS_BOT
!
      DIAGI_S%MESS_BOT_SAVED = DIAGI_S%MESS_BOT
!
! --- Print new bottom message.
!
      CALL PGSCI  ( 1 )
      CALL PGPTXT ( DIAGI_S%XMIN, &
     &              DIAGI_S%YMIN + (DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%YSH_LAB, &
     &              0.0, 0.0, DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT)) )
!
      TOGGLE_SUP_MODE = 1 ! Important: DuaGI user function should always
!                         ! return 1. If the function returns not 1, then DiaGI
!                         ! will print the error mesage and return to the main
!                         ! program
      RETURN
      END  !#!  TOGGLE_SUP_MODE  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SUPP_TOGGLE ( DIAGI_S, M, T, X, E, FL_USE, FL_FIRST, IUER )
! ************************************************************************
! *                                                                      *
! *   Example function SUPP_TOGGLE
! *                                                                      *
! *  ### 06-AUG-2002  SUPP_TOGGLE  v1.0 (c)  L. Petrov  06-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  SUPP_TOGGLE
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M
      REAL*8     T(M), X(M), E(M)
      LOGICAL*4  FL_USE(M)
      INTEGER*4  IPQ, IUER
      REAL*4     XC_ARG, YC_VAL, EC_SIG, ARG_DIST,  DIST_X, DIST_Y
      REAL*4     DIST_MM
      INTEGER*4  ICLR, ILST_SAVED_1, ILST_SAVED_2, IPM, J1, IER
      LOGICAL*4  FL_FIRST
      PARAMETER  ( DIST_MM  = 10.0 ) ! If there is no point closer to the
!                                    ! the current cursor position than
!                                    ! DIST_MM mm, then SHIFT_POINT will
!                                    ! do nothing.
      ICLR = 1
      IPQ = 0
      IF ( FL_FIRST ) THEN
!
! -------- If it was the first call of SUPP_TOGGLE then we have to
! -------- expand temporary array X4/Y4/E4, since DIAGI sized them for
! -------- only NPOI points. Since arrays may grow up to M elements,
! -------- we re-size them to M points.
!
! -------- Expand the array for the first (green) color ...
!
           CALL ERR_PASS ( IUER, IER )
           CALL UD_EXPAND ( M, DIAGI_S%NPOI(1), DIAGI_S%ADR_X4(1), &
     &                      DIAGI_S%ADR_Y4(1), DIAGI_S%ADR_E4(1), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7511, IUER, 'SUPP_TOGGLE', 'Error in an '// &
     &              'attempt to grab more memory for internal arrays' )
                RETURN
           END IF
!
! -------- ... and for the second (red) color
!
           CALL ERR_PASS ( IUER, IER )
           CALL UD_EXPAND ( M, DIAGI_S%NPOI(2), DIAGI_S%ADR_X4(2), &
     &                      DIAGI_S%ADR_Y4(2), DIAGI_S%ADR_E4(2), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7512, IUER, 'SUPP_TOGGLE', 'Error in an '// &
     &              'attempt to grab more memory for internal arrays' )
                RETURN
           END IF
!
           FL_FIRST = .FALSE.  ! Lift the flag in order to prevent this
!                              ! operation the second time
      END IF
!
! --- Set maximal distance: DIST_MM mm for each coorinates. If the point is
! --- located at more than sqrt(2)*10.0 mm, then it will be ignored.
!
      DIST_X = 10.0/(XRIGHTS(1)-XLEFTS(1))*(DIAGI_S%XMAX - DIAGI_S%XMIN)
      DIST_Y = 10.0/(YTOPS(1)-YBOTS(1))*(DIAGI_S%YMAX - DIAGI_S%YMIN)
!
! --- Search for the nearest point within sqrt(dist_2** + dist_y**2) area.
! --- Points beyond the plotting area are ignored
!
      CALL UD_SEARCH ( &
     &     DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
     &     DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX, &
     &     DIAGI_S%XC, DIAGI_S%YC, DIST_X, DIST_Y, ICLR, IPQ )
      IF ( IPQ .GT. 0 ) THEN
!
! -------- Hoplya! The point was found. Let's copy its coordinates
!
           CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_X4(ICLR)+(IPQ-1)*4), XC_ARG )
           CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_Y4(ICLR)+(IPQ-1)*4), YC_VAL )
           CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_E4(ICLR)+(IPQ-1)*4), EC_SIG)
!
           ILST_SAVED_1 = DIAGI_S%ILST(1)  ! Save the line style and
           ILST_SAVED_2 = DIAGI_S%ILST(1)  ! Save the line style and
           DIAGI_S%ILST(1) = 1           ! set temporarily point-by-point style
           IF ( ICLR .EQ. 1 ) THEN
!
! ------------- Redraw the point with opposite color
!
                CALL DIAGI_DRAW ( DIAGI_S, 2, 0, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                            0.0D0, 0.0D0 )
!
! ------------- Insert the point to the array of bad points (array 2)  :-(
!
                CALL UD_INSERT ( DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                         %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)), &
     &                         XC_ARG, YC_VAL, EC_SIG )
!
! ------------- Remove the point from the array of good points (array 1) :-(
!
                CALL UD_DELETE ( IPQ, DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                        %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)) )
           END IF
!
           IF ( ICLR .EQ. 2 ) THEN
!
! ------------- Re-draw the point with opposite color
!
                CALL DIAGI_DRAW ( DIAGI_S, 1, 0, &
     &                            1, XC_ARG, YC_VAL, EC_SIG, 0.0D0, 0.0D0 )
!
! ------------- Insert the point to the array of good points  :-)
!
                CALL UD_INSERT ( DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                         %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), &
     &                         XC_ARG, YC_VAL, EC_SIG )
!
! ------------- Remove the point from the array of good points  :-)
!
                CALL UD_DELETE ( IPQ, DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                        %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)) )
           END IF
           DIAGI_S%ILST(1) = ILST_SAVED_1  ! Restore original
           DIAGI_S%ILST(2) = ILST_SAVED_2  ! line style
!
! -------- Now we should find  it  in the extended list in order to report
! -------- a user about the work to have been done
!
           ARG_DIST = (DIAGI_S%XMAX - DIAGI_S%XMIN)
           DO 410 J1=1,M
              IF ( ABS ( T(J1) - XC_ARG ) .LT. ARG_DIST ) THEN
                   ARG_DIST = ABS ( T(J1) - XC_ARG )
                   IPM = J1
              END IF
 410       CONTINUE
!
! -------- ... in order to save the changes ibn the array FL_USE which a user
! -------- really wants
!
           FL_USE(IPM) = .NOT. FL_USE(IPM)
      END IF
      SUPP_TOGGLE = 1 ! Important: DiaGI user function should always return 1.
!                     ! If the function returns not 1, then DiaGI will print
!                     ! the error mesage and return to the main program!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SUPP_TOGGLE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UD_EXPAND ( M, N, ADR_X4, ADR_Y4, ADR_E4, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine UD_EXPAND expands dynamic memory requested for   *
! *   allocation of REAL*4 arrays ADR_X4, ADR_Y4, ADR_E4 sized to hold   *
! *   N points to the size sufficient to hold M points of these arrays.  *
! *   Current values of arrays X4/Y4/E4 are not changed.                 *
! *                                                                      *
! *  ### 08-AUG-2002   UD_EXPAND   v1.0 (c)  L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  M, N, ADR_X4, ADR_Y4, ADR_E4, IUER
      CHARACTER  STR*32
      INTEGER*4  NEW_ADR_X4, NEW_ADR_Y4, NEW_ADR_E4, IER
      INTEGER*4  I_LEN
!
      IF ( M .LE. N ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Grab memory for new arrays
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 3, &
     &                4*M, NEW_ADR_X4, &
     &                4*M, NEW_ADR_Y4, &
     &                4*M, NEW_ADR_E4  )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH  ( MEM_LEN, STR )
           CALL ERR_LOG ( 7521, IUER, 'UD_EXPAND', 'Error '// &
     &          'during the next attempt of grabbing '// &
     &           STR(1:I_LEN(STR))//' bytes of memory' )
           RETURN
      END IF
!
! --- copy arrays to the new place
!
      CALL LIB$MOVC3 ( 4*N, %VAL(ADR_X4), %VAL(NEW_ADR_X4) )
      CALL LIB$MOVC3 ( 4*N, %VAL(ADR_Y4), %VAL(NEW_ADR_Y4) )
      CALL LIB$MOVC3 ( 4*N, %VAL(ADR_E4), %VAL(NEW_ADR_E4) )
!
! --- Free the memory whcih has been poreviously allocated for the arrays
!
      CALL FREE ( ADR_X4 )
!
! --- Copy the addresses
!
      ADR_X4 = NEW_ADR_X4
      ADR_Y4 = NEW_ADR_Y4
      ADR_E4 = NEW_ADR_E4
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UD_SEARCH ( NP_ARR1, X4_ARR1, Y4_ARR1, &
     &                       NP_ARR2, X4_ARR2, Y4_ARR2, &
     &                       XMIN, XMAX, YMIN, YMAX, &
     &                       XC, YC, DIST_X, DIST_Y, ICLR, IPQ )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine UD_SEARCH searches for the point closest to      *
! *   XC,YC which is                                                     *
! *   1) within [XMIN, XMAX], [YMIN, YMAX] range                         *
! *   2) within [XC-DIST_X, XC+DIST_X], [YC-DIST_Y, YC+DIST_Y] range.    *
! *                                                                      *
! *   The search is donw over two set (color) of points. If such a point *
! *   is found, then IPQ keeps the return the index of the point in the  *
! *   array, ICLR -- the set index (color index). Otherwize IPQ=0,       *
! *   ICLR=0 .                                                           *
! *                                                                      *
! *  ### 08-AUG-2002   UD_SEARCH   v1.0 (c)  L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  NP_ARR1, NP_ARR2, ICLR, IPQ
      REAL*4     X4_ARR1(NP_ARR1), Y4_ARR1(NP_ARR1)
      REAL*4     X4_ARR2(NP_ARR2), Y4_ARR2(NP_ARR2)
      REAL*4     XC, YC, DIST_X, DIST_Y
      REAL*4     DX, DY
!
      ICLR = 0
      IPQ = 0
!
      DIST_MIN = 2.0
      DO 410 J1=1,NP_ARR1
!
! ------ Ignore the points which are beyond plotting area
!
         IF ( X4_ARR1(J1) .LT. XMIN ) GOTO 410
         IF ( X4_ARR1(J1) .GT. XMAX ) GOTO 410
         IF ( Y4_ARR1(J1) .LT. YMIN ) GOTO 410
         IF ( Y4_ARR1(J1) .GT. YMAX ) GOTO 410
!
! ------ Compute the distance on X and Y acix
!
         DX = (XC - X4_ARR1(J1))/DIST_X
         DY = (YC - Y4_ARR1(J1))/DIST_Y
!
! ------ Reject points whcih are too far
!
         IF ( ABS(DX) .GT. 1.0 ) GOTO 410
         IF ( ABS(DY) .GT. 1.0 ) GOTO 410
!
! ------ Check whether the J1-th point is the cosest to the cursor
!
         IF ( SQRT(DX**2 + DY**2) .LT. DIST_MIN ) THEN
              DIST_MIN = SQRT(DX**2 + DY**2)
              ICLR = 1
              IPQ = J1
         END IF
 410  CONTINUE
!
      DO 420 J2=1,NP_ARR2
!
! ------ Ignore the points which are beyond plotting area
!
         IF ( X4_ARR2(J2) .LT. XMIN ) GOTO 420
         IF ( X4_ARR2(J2) .GT. XMAX ) GOTO 420
         IF ( Y4_ARR2(J2) .LT. YMIN ) GOTO 420
         IF ( Y4_ARR2(J2) .GT. YMAX ) GOTO 420
!
         DX = (XC - X4_ARR2(J2))/DIST_X
         DY = (YC - Y4_ARR2(J2))/DIST_Y
         IF ( ABS(DX) .GT. 1.0 ) GOTO 420
         IF ( ABS(DY) .GT. 1.0 ) GOTO 420
         IF ( SQRT(DX**2 + DY**2) .LT. DIST_MIN ) THEN
              DIST_MIN = SQRT(DX**2 + DY**2)
              ICLR = 2
              IPQ = J2
         END IF
 420  CONTINUE
!
      RETURN
      END  !#!  UD_SEARCH  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UD_INSERT ( NP, ARR_X4, ARR_Y4, ARR_E4, ARG, VAL, ERR )
! ************************************************************************
! *                                                                      *
! *   Auxiliary function UD_UNSERT inserts the point ARG,VAL,ERR into    *
! *   the sorted array ADR_X4. The arrays ARR_X4,ARR_Y4,ARR_E4 are       *
! *   sorted by increasing ARR_X4. Ths point will be inserted in such    *
! *   a place of the array that preserves the sorting order.             *
! *                                                                      *
! *  ### 08-AUG-2002               v1.0 (c)  L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  NP
      REAL*4     ARR_X4(*), ARR_Y4(*), ARR_E4(*), ARG, VAL, ERR
!
      IF ( NP .GT. 0 ) THEN
!
! -------- Search for the point which is just after the ARG
!
           DO 410 J1=1,NP
              IF ( ARG .GT. ARR_X4(J1) ) THEN
!
! ---------------- Yeah, we found sush a point
!
                   DO 420 J2=NP,J1,-1
                      ARR_X4(J2+1) = ARR_X4(J2)  ! shift all points up
                      ARR_Y4(J2+1) = ARR_Y4(J2)
                      ARR_E4(J2+1) = ARR_E4(J2)
 420               CONTINUE
!
! ---------------- Insert the point
!
                   ARR_X4(J1) = ARG
                   ARR_Y4(J1) = VAL
                   ARR_E4(J1) = ERR
                   NP = NP + 1
                   RETURN
              END IF
 410       CONTINUE
      END IF
!
! --- We did not find. Well, insert the point to the end of the array
!
      NP = NP + 1
      ARR_X4(NP) = ARG
      ARR_Y4(NP) = VAL
      ARR_E4(NP) = ERR
      RETURN
      END  !#!  UD_INSERT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UD_DELETE ( IPQ, NP, ARR_X4, ARR_Y4, ARR_E4 )
! ************************************************************************
! *                                                                      *
! *   Auxiliary function UD_DELETE deletes the point with index IPQ from *
! *   the arrays ADD_X4,ARR_Y4,ARR_E4 and shrinks arrays.                *
! *                                                                      *
! *  ### 08-AUG-2002    UD_DELETE  v1.0 (c)  L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  NP
      REAL*4    ARR_X4(*), ARR_Y4(*), ARR_E4(*)
      IF ( IPQ .LE. 0  .OR.   IPQ .GT. NP ) RETURN
      IF ( IPQ .EQ. NP ) THEN
           NP = NP - 1
         ELSE
           DO 410 J1=IPQ,NP-1
              ARR_X4(J1) = ARR_X4(J1+1)
              ARR_Y4(J1) = ARR_Y4(J1+1)
              ARR_E4(J1) = ARR_E4(J1+1)
 410       CONTINUE
      END IF
      RETURN
      END  !#!  UD_DELETE  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   DIAGI_LOC_EXT ( IARG )
! ************************************************************************
! *                                                                      *
! *   This auxilliary function DIAGI_LOG_EXT retrins the address of its  *
! *   arguiment. The only reason of existance of this function is a bug  *
! *   in HP f90 compiler: compilers claims that getting the address of   *
! *   external function is an unsupported feature. !!!                   *
! *                                                                      *
! *  ### 21-SEP-2002  DIAGI_LOC_EXT v1.0 (c)  L. Petrov  21-SEP-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DIAGI_LOC_EXT
      INTEGER*4  IARG
!
      DIAGI_LOC_EXT = LOC(IARG)
!
      RETURN
      END  !#!  DIAGI_LOC_EXT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   USER_INIT ( DIAGI_S ) 
! ************************************************************************
! *                                                                      *
! *   Example of the user init-fincution. Does not do anything useful.   *
! *   Prints the message in the graphic window and proceeds after a user *
! *   hit any key.                                                       *
! *                                                                      *
! *  ### 23-MAR-2003   USER_INIT   v1.0 (c)  L. Petrov  23-MAR-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  USER_INIT
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) :: DIAGI_S
      CHARACTER  CH*1
!
      CALL PGERAS ()
      CALL PGSCH  ( 2.0 )
      CALL PGSLW  ( 8     )
!!      CALL PGSWIN ( 0.0, 1.0, 0.0, 1.0 ) 
      CALL PGPTXT ( 0.5, 0.5, 0.0, 0.5, &
     &             'This is user_init finction. Hit any key to continue' ) 
!
! --- Wait for hitting any key in graphic window
!
      CALL PGCURS ( DIAGI_S%XC, DIAGI_S%YC, CH )
      CALL PGERAS ()
!
      USER_INIT = 1 ! Important: DiaGI user function should always return 1.
!                   ! If the function returns not 1, then DiaGI will print
!                   ! the error mesage and return to the main program!
      RETURN
      END  !#!  USER_INIT   #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   USER_QUIT ( DIAGI_S ) 
! ************************************************************************
! *                                                                      *
! *   Example of the user quit-fincution. Does not do anything useful.   *
! *   Prints the message in the graphic window and proceeds after a user *
! *   hit any key.                                                       *
! *                                                                      *
! *  ### 23-MAR-2003   USER_QUIT   v1.0 (c)  L. Petrov  23-MAR-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  USER_QUIT
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  CH*1
!
      CALL PGERAS ()  
      CALL PGSCH  ( 2.0 )
      CALL PGSLW  ( 8     )
      CALL PGPTXT ( (DIAGI_S%XMAX+DIAGI_S%XMIN)/2, &
     &              (DIAGI_S%YMAX+DIAGI_S%YMIN)/2, 0.0, 0.5, &
     &              'This is user_quit finction. Hit any key to continue' ) 
      CALL PGCURS ( DIAGI_S%XC, DIAGI_S%YC, CH )
      CALL PGERAS ()
!
      USER_QUIT = 1 ! Important: DiaGI user function should always return 1.
!                   ! If the function returns not 1, then DiaGI will print
!                   ! the error mesage and return to the main program!
      RETURN
      END  !#!  USER_QUIT   #!#
