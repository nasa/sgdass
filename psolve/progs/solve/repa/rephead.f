      SUBROUTINE REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_K, NEW_BUTT )
!
! ************************************************************************
! *                                                                      *
! *  REPHEAD displays the button headline and changes the connects the   *
! *  user functions to the mouse buttons.                                *
! *                                                                      *
! *  called subroutines: PGSCH, PGSCI, PGPTXT, DIAGI_HLP, REPBOTT,       *
! *                      DIAGI_PURGE_BOT, CLRCH                          *
! *  calling functions: REPCONN, REPPTSU, REPGRSU, REPPTSH, REPGRSH,     *
! *                     REPINFO, REPMODE                                 *
! *                                                                      *
! *  ### 20-DEC-2002  REPHEAD              V. Thorandt  20-DEC-2002 ###  *
! *                                                                      *
! *  2003-01-02 VT  - connected single supp./rec. to middle mouse button *
! *  2003-01-02 VT  - help button added                                  *
! *  2003.05.14 pet - changed SCA array for big screen. Prevnts the list *
! *                   of source appreance on the screen in               *
! *                   non-connection mode.                               *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'repab.i'
      INCLUDE    'diagi.i'                         ! DiaGi include
      TYPE ( DIAGI_STRU ) ::  DIAGI_S         ! record of DiaGi structure
      INTEGER*4  FUNC_N                            ! # of current user function set
      CHARACTER  FUNC_K(FUNC_N)*1                  ! keybord keys of current user function set
      CHARACTER  FUNC_B(FUNC_N)*8                  ! button names of current user function set
      CHARACTER  NEW_BUTT*8                        ! chosen button name
      REAL*4     DIST_MIN                          ! minimum distance curser-source name
      REAL*4     DIST_X                            ! distance cursor - button
      INTEGER*4  IPS                               ! index of user function
      INTEGER*4  J1                                ! loop variable
      CHARACTER  STRING*128                        ! misc string
      INTEGER*4  IUER                              ! error handler
      REAL*4     SCA(6)                            ! scaling factors for big (BS) and small screen (SM)
! SCA(1) - X shift for func. buttons (SM)
! SCA(2) - Y shift for func. buttons (SM)
! SCA(3) - character hight for func. buttons (SM)
! SCA(1) - X shift for LMR buttons (SM)
! SCA(2) - Y shift for LMR buttons (SM)
! SCA(3) - character hight for LMR buttons (SM)
      DATA SCA / 1.0, -1.1, 0.8, -10.5, 4.0, 0.8 / ! default values (SM)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      CHARACTER  ZAG*128, UNIT*128
      INTEGER*4  CH_SIZE                           ! save character size
! *****************************************************************************************************
!
! --- set scaling factors depending on DIAGI_SCREEN environment
!
      CALL PGQCH ( CH_SIZE )       ! inquire old PGPLOT font size
      DIAGI_S%SCH_TIT = 1.4        ! character hight of DiaGi titel (SM)
!
! --- Learh the screen size
!
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                 ICL1, ICL2, ICL3, IER )
      IF ( IDEV .EQ. 1 ) THEN      ! big screen
           SCA(1) =  1.0           ! X shift for func. buttons (BS)
           SCA(2) = -0.95          ! Y shift for func. buttons (BS)
           SCA(3) =  0.75          ! character hight for func. buttons (BS)
           SCA(4) = -9.0           ! X shift for LMR buttons (BS)
           SCA(5) =  3.3           ! Y shift for LMR buttons (BS)
           SCA(6) =  0.7           ! character hight for LMR buttons (BS)
           DIAGI_S%SCH_TIT = 1.3   ! character hight of DiaGi titel (BS)
      END IF
!
! --- inizialize values
!
      DIST_MIN = DIAGI_S%XMAX - DIAGI_S%XMIN
      CALL PGSCH ( SCA(3) )                        ! font size
      CALL PGSCI ( 7 )                             ! colour
!
! --- display HELP button
!
      CALL PGSCI ( 2 )
      CALL PGPTXT ( DIAGI_S%XMIN + (FUNC_N)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/FUNC_N, &
     &                 DIAGI_S%YMAX + SCA(2)*((DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%YSH_LAB), &
     &                 0.0, 0.0, '??HELP??' )
!
! --- curser position obove the upper edge of plotting area
!
      IPS = 0
      IF ( DIAGI_S%YC .GT. DIAGI_S%YMAX .AND. DIAGI_S%XC .GT. DIAGI_S%XMIN ) THEN
!
! ------- find the closest headline button
!
          DO J1=1,FUNC_N+1                 ! +1 for help text display
!
! ----------  look for middle of button -->
! ---------- (max-min)/26 is about one half of the length of a button (8 capitals)
!
             DIST_X = ABS ( DIAGI_S%XC - (DIAGI_S%XMIN  + (J1- &
     &                                                  1)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/FUNC_N+ (DIAGI_S%XMAX-DIAGI_S%XMIN)/26 ) )
             IF ( DIST_X .LT. DIST_MIN ) THEN
                DIST_MIN = DIST_X
                IPS = J1                   ! index
             END IF
          END DO
!
! ------- display helptext file
!
          IF ( IPS .EQ. FUNC_N+1 ) THEN
             CALL PGSCI ( 1 )
             CALL DIAGI_HLP ( HELPFILE, IUER )
             IPS = FUNC_N                  ! set init function
          END IF
!
! ------- reset previous mouse functions
!
          DO J1=1,FUNC_N
             IF ( DIAGI_S%USER_CHR(J1) .EQ. 'A' .OR. DIAGI_S%USER_CHR(J1) &
     &                                          .EQ. 'X'.OR. DIAGI_S%USER_CHR(J1) .EQ. 'D' ) THEN
                DIAGI_S%USER_CHR(J1) = FUNC_K(J1)
             END IF
          END DO
!
! ------- set new left mouse function
!
          DIAGI_S%USER_CHR(IPS) = 'A'
          NEW_BUTT = FUNC_B(IPS)        ! new functionality (new button)
!
! ------- special cases
!
! ------- connect three buttons
!
          IF ( NEW_BUTT .EQ. FUNC_BUTT(1) ) THEN                                ! chosen (left) function was "single shift down"
!
! ---------- set right mouse function
!
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (single shift up)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(3) ) THEN
                   IPS = J1
                END IF
             END DO
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'X'
             END IF
!
! ---------- set middle mouse function
!
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (single suppress/recover)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(2) ) THEN
                   IPS = J1
                END IF
             END DO
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'D'
             END IF
          END IF
          IF ( NEW_BUTT .EQ. FUNC_BUTT(3) ) THEN                                ! chosen (left) function was "single shift up"
             DIAGI_S%USER_CHR(IPS) = 'X'                                        ! move to right mouse button
! ---------- set left mouse function
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (single shift down)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(1) ) THEN
                   IPS = J1
                END IF
             END DO
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'A'
             END IF
!
! ---------- set middle mouse function
!
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (single suppress/recover)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(2) ) THEN
                   IPS = J1
                END IF
             END DO
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'D'
              END IF
          END IF
          IF ( NEW_BUTT .EQ. FUNC_BUTT(2) ) THEN                                ! chosen (left) function was "single shift up"
             DIAGI_S%USER_CHR(IPS) = 'D'                                        ! move to middle mouse button
!
! ---------- set left mouse function
!
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (single shift down)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(1) ) THEN
                   IPS = J1
                END IF
             END DO
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'A'
             END IF
!
! ---------- set right mouse function
!
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (single shift up)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(3) ) THEN
                   IPS = J1
                END IF
             END DO
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'X'
               END IF
          END IF
!
! ------- connect two buttons
!
          IF ( NEW_BUTT .EQ. FUNC_BUTT(6) ) THEN                               ! chosen function was "group suppress"
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (group recover)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(7) ) THEN
                   IPS = J1
                END IF
             END DO
! ---------- set right mouse function
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'X'
             END IF
          END IF
          IF ( NEW_BUTT .EQ. FUNC_BUTT(7) ) THEN                               ! chosen function was "group recover"
             DIAGI_S%USER_CHR(IPS) = 'X'
             IPS = 0
             DO J1=1,FUNC_N           !  search for belonging functionality (group suppress)
                IF ( FUNC_B(J1) .EQ. FUNC_BUTT(6) ) THEN
                   IPS = J1
                END IF
             END DO
! ---------- set left mouse function
             IF ( IPS .GT. 0 ) THEN
                DIAGI_S%USER_CHR(IPS) = 'A'
             END IF
          END IF
!
! ------- bottom line messages
!
! ------- message for information line
          IF ( NEW_BUTT .EQ. FUNC_BUTT(5) ) THEN
              CALL DIAGI_PURGE_BOT ( DIAGI_S )
              STRING = 'left mouse click on observation point to get information in bottom line'
              CALL REPBOTT ( DIAGI_S, STRING, 7 )
              DIAGI_S%MESS_BOT = 'ACTIVE USER FUNCTION: INFORMATION ON OBSERVATION'
          END IF
! ------- message for single recover/suppress and shift
          IF ( NEW_BUTT .EQ. FUNC_BUTT(1) .OR. NEW_BUTT .EQ. FUNC_BUTT(2) .OR. NEW_BUTT .EQ. FUNC_BUTT(3) ) THEN
              CALL DIAGI_PURGE_BOT ( DIAGI_S )
              STRING = 'left --> single shift down, middle --> recover/suppress, right --> single shift up'
              CALL REPBOTT ( DIAGI_S, STRING, 7 )
              DIAGI_S%MESS_BOT = 'ACTIVE USER FUNCTIONS: SINGLE AMBIGUITY SHIFTING AND SUPPRESS/RECOVER'
          END IF
! ------- message for group suppress/recover
          IF ( NEW_BUTT .EQ. FUNC_BUTT(6) .OR. NEW_BUTT .EQ. FUNC_BUTT(7) ) THEN
              CALL DIAGI_PURGE_BOT ( DIAGI_S )
              STRING = 'left --> group suppress (outside curser position), right --> group recover (inside curser position)'
              CALL REPBOTT ( DIAGI_S, STRING, 7 )
              DIAGI_S%MESS_BOT = 'ACTIVE USER FUNCTIONS: GROUP SUPPRESS/RECOVER'
          END IF
! ------- message for group ambiguity shifting
          IF ( NEW_BUTT .EQ. FUNC_BUTT(8) ) THEN
              CALL DIAGI_PURGE_BOT ( DIAGI_S )
              STRING = 'left mouse click to shift ambiguities for groups of observations'
              CALL REPBOTT ( DIAGI_S, STRING, 7 )
              DIAGI_S%MESS_BOT = 'ACTIVE USER FUNCTION: GROUP AMBIGUITY SHIFTING'
          END IF
! ------- message point connecting
          IF ( NEW_BUTT .EQ. FUNC_BUTT(9) ) THEN
              CALL DIAGI_PURGE_BOT ( DIAGI_S )
              STRING = 'left mouse click on point or source name to connect observation points with the same source'
              CALL REPBOTT ( DIAGI_S, STRING, 7 )
              DIAGI_S%MESS_BOT = 'ACTIVE USER FUNCTION: CONNECT POINTS WITH THE SAME SOURCE'
          END IF
! ------- message for user input of source name
          IF ( NEW_BUTT .EQ. FUNC_BUTT(11) ) THEN
              CALL DIAGI_PURGE_BOT ( DIAGI_S )
              STRING = 'left mouse click on button '//FUNC_BUTT(11)
              CALL REPBOTT ( DIAGI_S, STRING, 7 )
          END IF
! ------- message for ambiguity reset
          IF ( NEW_BUTT .EQ. FUNC_BUTT(12) ) THEN
              CALL DIAGI_PURGE_BOT ( DIAGI_S )
              STRING = 'left mouse click on button '//FUNC_BUTT(12)//' to reset ambiguities'
              CALL REPBOTT ( DIAGI_S, STRING, 7 )
              DIAGI_S%MESS_BOT = 'AMBIGUITY RESET --> CLICK LEFT TO GET USER FUNCTION BUTTONS'
          END IF
!
! ------- if there are more cases --> add here!!
!
      END IF
!
! --- display mouse status to the upper left corner
!
      CALL PGSCH ( SCA(6) )
      CALL PGSCI ( 11 )         ! gray
      CALL PGPTXT ( DIAGI_S%XMIN + SCA(4)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                  DIAGI_S%YMAX + SCA(5)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/50, &
     &                  0.0, 0.0, 'LEFT' )
      CALL PGPTXT ( DIAGI_S%XMIN + SCA(4)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                  DIAGI_S%YMAX + (SCA(5)-1)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/50, &
     &                  0.0, 0.0, 'MIDDLE' )
      CALL PGPTXT ( DIAGI_S%XMIN + SCA(4)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                  DIAGI_S%YMAX + (SCA(5)-2)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/50, &
     &                  0.0, 0.0, 'RIGHT' )
      CALL PGSCI ( 2 )          ! red for active buttons,
!                               ! the remaining got the DiaGi functionalities
      DO J1=1,FUNC_N
          IF ( DIAGI_S%USER_CHR(J1) .EQ. 'A' ) THEN
             CALL PGPTXT ( DIAGI_S%XMIN + SCA(4)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                     DIAGI_S%YMAX + SCA(5)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/50, &
     &                     0.0, 0.0, 'LEFT' )
          ELSE IF ( DIAGI_S%USER_CHR(J1) .EQ. 'D' ) THEN
             CALL PGPTXT ( DIAGI_S%XMIN + SCA(4)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                     DIAGI_S%YMAX + (SCA(5)-1)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/50, &
     &                     0.0, 0.0, 'MIDDLE' )
          ELSE IF ( DIAGI_S%USER_CHR(J1) .EQ. 'X' ) THEN
             CALL PGPTXT ( DIAGI_S%XMIN + SCA(4)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                     DIAGI_S%YMAX + (SCA(5)-2)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/50, &
     &                     0.0, 0.0, 'RIGHT' )
          END IF
      END DO
      CALL PGSCH ( SCA(3) )
!
! --- display headline buttons (activ in different colour)
!
      DO J1=1,FUNC_N
          CALL PGSCI ( 7 )
          IF ( DIAGI_S%USER_CHR(J1) .EQ. 'A' .OR.  DIAGI_S%USER_CHR(J1) .EQ. 'X' &
     &         .OR. DIAGI_S%USER_CHR(J1) .EQ. 'D') THEN
               CALL PGSCI ( 17 )
          END IF
!
! ------- display all buttons ( mouse functions in different colour!)
!
          CALL PGPTXT ( &
     &          DIAGI_S%XMIN + (J1-1)*(DIAGI_S%XMAX-DIAGI_S%XMIN)/FUNC_N, &
     &          DIAGI_S%YMAX + SCA(2)*((DIAGI_S%YMAX-DIAGI_S%YMIN)* &
     &                         DIAGI_S%YSH_LAB), &
     &          0.0, 0.0, FUNC_B(J1) )
      END DO
!
      CALL PGSCH ( CH_SIZE )                     ! reset old PGPLOT font size
!
      RETURN
      END
