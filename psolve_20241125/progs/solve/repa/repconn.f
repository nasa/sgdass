      FUNCTION REPCONN ( CONFLAG, M, DIAGI_S, C4, &
     &                   INFO_CHR_G, INFO_CHR_M, INFO_CHR_B, &
     &                   TG, XG, EG, TM, XM, EM, TB, XB, EB, TS, XS, ES, &
     &                   FUNC_N, FUNC_B, FUNC_K, FUNC_NUM, FUNC_BUTT, IUER )
!
! ************************************************************************
! *                                                                      *
! *  Function REPCONN connects points with equal source without respect  *
! *  of membership to one of the point arrays (good, recoverable, bad)   *
! *  (G-good points, M-manually downweighted points, B-bad points)       *
! *  Four modes:  (1) connect lines by curser close to a point           *
! *               (2) connect lines after user input of source name      *
! *               (3) connect lines after chosing name from              *
! *                   source name columns left and right(if #>50)        *
! *               (4) delete connecting lines                            *
! *                                                                      *
! *   called subroutines:                                                *
! *   DIAGI_SET_FRAME, DIAGI_DRAW, ERR_PASS, REPEXPA, ERR_LOG, REPCCLO,  *
! *   LIB$MOVC3, REPINNI, PGSCI, DIAGI_PURGE_BOT, PGPTXT, CLRCH, PGENTER,*
! *   REPBOTT, REPHEAD                                                   *
! *   ( PGENTER is a subroutine of DiaGi package )                       *
! *                                                                      *
! *   calling routines:                                                  *
! *   DIAGI (via REPA and MULTI_DIAGI)                                   *
! *                                                                      *
! *  ### 11-NOV-2002     REPCONN      Volkmar Thorandt  11-NOV-2002 ###  *
! *  02-12-09 VT INFO arrays enlarged to 87                              *
! *  02-12-11 VT user input of source name                               *
! *  02-12-13 VT display of source names and interaction with the names  *
! *  02-12-19 VT REPHEAD call                                            *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4  REPCONN
      INCLUDE    'diagi.i'                   ! DiaGi include
!
      TYPE ( DIAGI_STRU ) ::  DIAGI_S   ! record of DiaGi structure
      INTEGER*4  FUNC_N                      ! current # of function keys
      CHARACTER  FUNC_K(FUNC_N)*1            ! current keybord keys of user functions (s. repa.i)
      CHARACTER  FUNC_B(FUNC_N)*8            ! current button names of user functions (s. repa.i)
      INTEGER*4  FUNC_NUM                    ! max. # of user function buttons (s. repa.i)
      CHARACTER  FUNC_BUTT(FUNC_NUM)*8       ! button names of user functions (s. repa.i)
      CHARACTER  NEW_BUTT*8                  ! chosen button
      INTEGER*4  C4                          ! index of special case arrays in DIAGI_S (4th colour)
      INTEGER*4  M                           ! total number of all observations
      CHARACTER  INFO_CHR_G(M)*87            ! information for bottom line (good)
      CHARACTER  INFO_CHR_M(M)*87            ! information for bottom line (man.suppr.)
      CHARACTER  INFO_CHR_B(M)*87            ! information for bottom line (bad)
      REAL*8     TG(M), XG(M), EG(M)         ! real*8 arrays for display in MultiDiagi (good)
      REAL*8     TM(M), XM(M), EM(M)         ! real*8 arrays for display in MultiDiagi (man.suppr.))
      REAL*8     TB(M), XB(M), EB(M)         ! real*8 arrays for display in MultiDiagi (bad)
      REAL*8     TS(M), XS(M), ES(M)         ! real*8 arrays for display in MultiDiagi (conncting lines))
      CHARACTER  MESS_BOT_SAV*128            ! copy of DIAGI_S.MESS_BOT
      INTEGER*4  IPQ                         ! REPCCLO return index of found point in field ICLR (color)
      INTEGER*4  ICLR                        ! REPCCLO return colour #
      REAL*4     DIST_X                      ! X value of curser to point distance
      REAL*4     DIST_Y                      ! Y value of curser to point distance
      REAL*4     DIST_MM                     ! max. distance point-curser (mm)
      REAL*4     DIST_MIN                    ! minimum distance curser-source name
      INTEGER*4  IPS                         ! index of found star in array stars
      CHARACTER  IPS_CHR*3                   ! index of found star in array stars (character)
      PARAMETER  ( DIST_MM  = 10.0 )         ! if there is no point closer to the the cursor than do nothing.
      REAL*4     XC_ARG_G(M)                 ! copy of arguments (good)
      REAL*4     YC_VAL_G(M)                 ! copy of values (good)
      REAL*4     EC_SIG_G(M)                 ! copy of errors (good)
      REAL*4     XC_ARG_M(M)                 ! copy of arguments (recoverable)
      REAL*4     YC_VAL_M(M)                 ! copy of values (recoverable)
      REAL*4     EC_SIG_M(M)                 ! copy of errors (recoverable)
      REAL*4     XC_ARG_B(M)                 ! copy of arguments (bad)
      REAL*4     YC_VAL_B(M)                 ! copy of values (bad)
      REAL*4     EC_SIG_B(M)                 ! copy of errors (bad)
      REAL*4     XC_ARG_S(M)                 ! copy of arguments (special case)
      REAL*4     YC_VAL_S(M)                 ! copy of values (special case)
      REAL*4     EC_SIG_S(M)                 ! copy of errors (special case)
      REAL*8     XC_ARG_S8(M)                ! = XC_ARG_S but real*8
      REAL*8     YC_VAL_S8(M)                ! = YC_VAL_S but real*8
      REAL*8     EC_SIG_S8(M)                ! = EC_SIG_S but real*8
      INTEGER*4  STAR_NUM                    ! # of points in special case arrays
      CHARACTER  STAR_NUM_CH*3               ! # of points in special case arrays (character)
      CHARACTER  STAR*8                      ! name of current source
      CHARACTER  STARS(M)*8                  ! names of available sources
      REAL*4     STARS_YC(M)                 ! Y coordinates of source names displayed at the edges
      INTEGER*4  NSTARS                      ! # of available sources
      CHARACTER  NSTARS_CHR*3                ! # of available sources (char)
      INTEGER*4  IUER                        ! universal error handler
      INTEGER*4  IER                         ! error handler
      INTEGER*4  J1, J2, J3                  ! loop variables
      INTEGER*4  ILEN                        ! function: length of character string
      CHARACTER  DIAGI_LABEL*100             ! copy of DIAGI_LABEL__DEF (see diagi.i)
      CHARACTER  CONFLAG*1                   ! flag for deleting of connecting lines (if true)
      CHARACTER  FLAG*1                      ! copy of CONFLAG
      INTEGER*4  IL                          ! # of symbols in the output line of PGENTER
      REAL*4     XMIN4, XMAX4, YMIN4, YMAX4  ! copies of DiaGi plot sizes
      CHARACTER  MESS*128                    ! bottom line prompt for user input
      CHARACTER  TERMN*1                     ! terminator for PGENTER
      LOGICAL*4  RED                         ! red colour flag for error message
      INTEGER*4  SINGLE(2)                   ! colour index and observation index in INFO_CHR_x arrays
      CHARACTER  STRING*128                  ! misc string
      REAL*4     XSL                         ! X-shift of source names in left column
      DATA XSL / -0.105 /                    ! default for small screen
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3
      CHARACTER  ZAG*128, UNIT*128
!C    INTEGER*4  I_LEN
!C    INTEGER*4  IDX                         ! index varaiable
!
! ------------------------------------------------------------------------------------------------------
!C    write(6,*) 'REPCONN: DIAGI_S.MESS_BOT=',DIAGI_S.MESS_BOT
      FLAG = CONFLAG
!C    write(6,*) 'REPCONN: FLAG=',FLAG
!C    write(6,*) 'REPCONN: DIAGI_S.INIT_USER_FUNC = ',DIAGI_S.INIT_USER_FUNC
      IF ( FLAG .EQ. 'M' ) THEN              ! minimum action of REPCONN for met. data
         CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_K, NEW_BUTT )
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C   test for initialization (only meteorological plots!!)
!C       write(6,*) 'REPCONN(3): FLAG=',FLAG
!C       CALL PGPTXT ( DIAGI_S.XMIN - 10.5*(DIAGI_S.XMAX-DIAGI_S.XMIN)/100,
!C   #                 DIAGI_S.YMAX + (DIAGI_S.YMAX-DIAGI_S.YMIN)/150,
!C   #                 0.0, 0.0, 'xx sources' )
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         GOTO 810
      END IF
      CALL DIAGI_PURGE_BOT ( DIAGI_S )       ! purge bottom line display
!
! --- reset number of elements in special case arrays
!
      DIAGI_S%NPOI(C4) = 0
!
! --- redraw the plot with initial arrays
!
      CALL CLRCH ( DIAGI_LABEL )                    ! clear bottom line message
!
      CALL DIAGI_SET_FRAME ( DIAGI_S, DIAGI_LABEL ) ! set frame
!
! --- draw all colours (function)
!
      DO J1=1,DIAGI_S%NCLR
         CALL DIAGI_DRAW ( DIAGI_S, J1, 0, &
     &                DIAGI_S%NPOI(J1), %VAL(DIAGI_S%ADR_X4(J1)), &
     &                %VAL(DIAGI_S%ADR_Y4(J1)), %VAL(DIAGI_S%ADR_E4(J1)), &
     &                0.D0, 0.D0 )
      END DO
!
! --- write button headline
!
      CALL CLRCH ( NEW_BUTT )
      CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_K, NEW_BUTT )  ! button headline
!C    write (6,*) 'REPCONN: FLAG=',FLAG
!
! --- set flags for further run of this function
!
!C      write(6,*) 'REPCONN(4): FLAG=',FLAG
      IF ( NEW_BUTT .EQ. FUNC_BUTT(11) ) THEN
             FLAG = 'I'
!C             write(6,*) 'REPCONN(5): FLAG=',FLAG
      ELSE IF ( NEW_BUTT .EQ. FUNC_BUTT(10) ) THEN
             FLAG = 'D'
      ELSE IF ( NEW_BUTT .EQ. FUNC_BUTT(9) ) THEN
             FLAG = 'C'
!C    ELSE IF ( FLAG .NE. 'C' .AND. FLAG .NE. 'D' ) THEN
!C           GOTO 810
      END IF
!
! --- display available sources
!
      NSTARS = 0
      DO J1=1,M
         CALL CLRCH ( STARS(J1) )
      END DO
      DO 310 J1=1,DIAGI_S%NPOI(1)
         DO J2=1,M
            IF ( STARS(J2) .EQ. INFO_CHR_G(J1)(11:18) ) GOTO 310
         END DO
         NSTARS = NSTARS + 1
         STARS(NSTARS) = INFO_CHR_G(J1)(11:18)
  310 CONTINUE
      DO 320 J1=1,DIAGI_S%NPOI(2)
         DO J2=1,M
            IF ( STARS(J2) .EQ. INFO_CHR_M(J1)(11:18) ) GOTO 320
         END DO
         NSTARS = NSTARS + 1
         STARS(NSTARS) = INFO_CHR_M(J1)(11:18)
  320 CONTINUE
      DO 330 J1=1,DIAGI_S%NPOI(3)
         DO J2=1,M
            IF ( STARS(J2) .EQ. INFO_CHR_B(J1)(11:18) ) GOTO 330
         END DO
         NSTARS = NSTARS + 1
         STARS(NSTARS) = INFO_CHR_B(J1)(11:18)
  330 CONTINUE
!
      WRITE ( NSTARS_CHR, '(I3)' ) NSTARS
      CALL CHASHL ( NSTARS_CHR )
!
! --- display headline message for source columns
!
! --- learn the screen size
!
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, &
     &                       UNIT, ICL1, ICL2, ICL3, IER )
!
! --- set scaling factor depending on screen size
!
      IF ( IDEV .EQ. 1 ) THEN   ! big screen
           XSL = -0.09          ! X-shift of source names in left column
      END IF
      CALL PGSCH ( 0.55 )             ! font size
      CALL PGSCI ( 5 )                ! colour
!C    CALL PGPTXT ( DIAGI_S.XMIN - 10.5*(DIAGI_S.XMAX-DIAGI_S.XMIN)/100,
      CALL PGPTXT ( DIAGI_S%XMIN + XSL*(DIAGI_S%XMAX-DIAGI_S%XMIN), &
     &                 DIAGI_S%YMAX + (DIAGI_S%YMAX-DIAGI_S%YMIN)/150, &
     &                 0.0, 0.0, NSTARS_CHR(1:ILEN(NSTARS_CHR))//' sources' )
!
! --- display source name columns
!
      IF ( FLAG .EQ. 'C' ) THEN
           CALL PGSCI ( 17 )
           J3 = NSTARS
           IF ( NSTARS .GT. 50 ) J3=50
           DO J1=1,J3
              CALL PGPTXT ( DIAGI_S%XMIN + XSL*(DIAGI_S%XMAX-DIAGI_S%XMIN), &
     &                      DIAGI_S%YMAX - 2*J1*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100, &
     &                      0.0, 0.0, STARS(J1) )
              STARS_YC(J1) = DIAGI_S%YMAX - 2*J1*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100
           END DO
           IF ( NSTARS .GT. 50 ) THEN
              CALL PGSCI ( 5 )
              CALL PGPTXT ( DIAGI_S%XMAX + (DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                      DIAGI_S%YMAX + (DIAGI_S%YMAX-DIAGI_S%YMIN)/150, &
     &                      0.0, 0.0, &
     &                      NSTARS_CHR(1:ILEN(NSTARS_CHR))//' sources' )
              CALL PGSCI ( 17 )
              DO J1=51,NSTARS
              CALL PGPTXT ( DIAGI_S%XMAX + (DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                 DIAGI_S%YMAX - 2*(J1-50)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100, &
     &                 0.0, 0.0, STARS(J1) )
              STARS_YC(J1) = DIAGI_S%YMAX - &
     &                       2*(J1-50)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100
              END DO
           END IF
      END IF
      CALL PGSCI ( 1 )
      CALL PGSCH ( DIAGI_S%SCH_LAB )
!
!
! --- delete connecting lines and return
!
      IF ( FLAG .EQ. 'D' ) GOTO 810
!
! --- set initial values
!
      ICLR = 1
      IPQ = 0
      IPS = 0
      REPCONN = 0
      STAR(1:1) = '%'
      STAR_NUM = 0
      MESS_BOT_SAV = DIAGI_S%MESS_BOT
      RED = .FALSE.
!
! --- expand the array for the colour C4 (special case: connect) to M points
!
      CALL ERR_PASS ( IUER, IER )
      CALL REPEXPA ( M, DIAGI_S%NPOI(C4), DIAGI_S%ADR_X4(C4), &
     &               DIAGI_S%ADR_Y4(C4), DIAGI_S%ADR_E4(C4), IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7511, IUER, 'REPCONN', 'Error in an '// &
     &         'attempt to grab more memory for internal arrays' )
           RETURN
      END IF
!
! --- get source name
!
      IF ( FLAG .EQ. 'C' .AND. &                                 ! connect lines by mouse click
     &     DIAGI_S%YC .LT. DIAGI_S%YMAX .AND. &
     &     DIAGI_S%YC .GT. DIAGI_S%YMIN) THEN
!
! ------ Set maximum distance: DIST_MM mm for each coordinates. If the point is
! ------ located at more than sqrt(2)*10.0 mm, then it will be ignored.
!
         DIST_X = 10.0/(XRIGHTS(1)-XLEFTS(1))*(DIAGI_S%XMAX - DIAGI_S%XMIN)
         DIST_Y = 10.0/(YTOPS(1)-YBOTS(1))*(DIAGI_S%YMAX - DIAGI_S%YMIN)
!
! ------ Search for the nearest point within sqrt(dist_x**2 + dist_y**2) area.
! ------ Points beyond the plotting area are ignored
!
         CALL REPCCLO ( &
     &        DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
     &        DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &        DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), %VAL(DIAGI_S%ADR_Y4(3)), &
     &        DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX, &
     &        DIAGI_S%XC, DIAGI_S%YC, DIST_X, DIST_Y, ICLR, IPQ )
!
! ------ get source name of current point
!
         IF ( IPQ .GT. 0 ) THEN                                ! curser in plotting area and point found
            IF ( ICLR .EQ. 1 ) STAR = INFO_CHR_G(IPQ)(11:18)
            IF ( ICLR .EQ. 2 ) STAR = INFO_CHR_M(IPQ)(11:18)
            IF ( ICLR .EQ. 3 ) STAR = INFO_CHR_B(IPQ)(11:18)
            DO J1=1,NSTARS
               IF ( STAR .EQ. STARS(J1) ) IPS = J1
            END DO
         ELSE IF ( DIAGI_S%XC .LT. DIAGI_S%XMIN .AND.           & ! curser outside the left edge of plotting area
     &             DIAGI_S%YC .LT. DIAGI_S%YMAX .AND. DIAGI_S%YC .GT. DIAGI_S%YMIN ) THEN
                   DIST_MIN = DIAGI_S%YMAX - DIAGI_S%YMIN
                   IF ( NSTARS .LE. 50 ) THEN
                      J3 = NSTARS
                   ELSE
                      J3 = 50
                   ENDIF
                   DO J1=1,J3
                      DIST_Y = ABS ( DIAGI_S%YC - STARS_YC(J1))
                      IF ( DIST_Y .LT. DIST_MIN ) THEN
                         DIST_MIN = DIST_Y
                         IPS = J1                      ! index in STARS(.)
                      END IF
                   END DO
                   IF ( DIST_MIN .LT. 2*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100 ) STAR = STARS(IPS)    ! found name
!C                 write (6,*) 'REPCONN: STARS(',IPS,')=',IPS
         ELSE IF ( DIAGI_S%XC .GT. DIAGI_S%XMAX .AND.            & ! curser outside the right edge of plotting area
     &             DIAGI_S%YC .LT. DIAGI_S%YMAX .AND. DIAGI_S%YC .GT. DIAGI_S%YMIN ) THEN
                   IF ( NSTARS .GT. 50 ) THEN
                      DIST_MIN = DIAGI_S%YMAX - DIAGI_S%YMIN
                      J2 = 51
                      J3 = NSTARS
                      DO J1=J2,J3
                         DIST_Y = ABS ( DIAGI_S%YC - STARS_YC(J1))
                         IF ( DIST_Y .LT. DIST_MIN ) THEN
                            DIST_MIN = DIST_Y
                            IPS = J1                   ! index in STARS(.)
                         END IF
                      END DO
                      IF ( DIST_MIN .LT. 2*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100 ) STAR = STARS(IPS) ! found name
!C                 write (6,*) 'REPCONN: STARS(',IPS,')=',IPS
                   END IF
         END IF
         IF ( STAR(1:1) .EQ. '%' ) THEN
            DIAGI_S%MESS_BOT = 'Move the cursor closer to '// &
     &                         'observation or source name to connect observations!'
            GOTO 400
         END IF
!
      ELSE IF ( FLAG .EQ. 'I' ) THEN         ! user input of source name
!
         TERMN = CHAR(3)                     ! define terminator for PGENTER
         XMIN4 = DIAGI_S%XMIN                ! copy DiaGi size values
         XMAX4 = DIAGI_S%XMAX
         YMIN4 = DIAGI_S%YMIN
         YMAX4 = DIAGI_S%YMAX
         CALL CLRCH ( MESS )
         MESS = 'Enter Source Name >>'       ! new prompt for bottom line
         CALL DIAGI_PURGE_BOT ( DIAGI_S )    ! purge current bottom line
         CALL CLRCH ( STAR )
!
! ------ ask user for source name
!
         CALL PGENTER ( MESS(1:ILEN(MESS)), XMIN4, &
     &                  YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, &
     &                  TERMN, STAR, IL )
         DO J1=1,NSTARS
            IF ( STAR .EQ. STARS(J1) ) IPS = J1
         END DO
! ------ reset function keys
         DO J1=1,FUNC_N
            IF ( FUNC_B(J1) .EQ. FUNC_BUTT(5) ) THEN
               DIAGI_S%USER_CHR(J1) = 'A'
            END IF
         END DO
         DO J1=1,FUNC_N
            IF ( FUNC_B(J1) .EQ. FUNC_BUTT(11) ) THEN
               DIAGI_S%USER_CHR(J1) = '~'
            END IF
         END DO
      END IF
!
!     change colour of source name
!
!C    CALL PGSCH ( 0.6 )
      CALL PGSCH ( 0.55 )       ! font size
      CALL PGSCI ( 7 )          ! colour
      IF ( IPS .GT. 50 ) THEN   ! more than 50 sources --> column
         CALL PGPTXT ( DIAGI_S%XMAX + (DIAGI_S%XMAX-DIAGI_S%XMIN)/100, &
     &                 DIAGI_S%YMAX - 2*(IPS-50)*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100, &
     &                 0.0, 0.0, STARS(IPS) )
      ELSE IF ( IPS .GT. 0 .AND. IPS .LE. 50 ) THEN
!C       CALL PGPTXT ( DIAGI_S.XMIN - 10.5*(DIAGI_S.XMAX-DIAGI_S.XMIN)/100,
         CALL PGPTXT ( DIAGI_S%XMIN + XSL*(DIAGI_S%XMAX-DIAGI_S%XMIN), &
     &                 DIAGI_S%YMAX - 2*IPS*(DIAGI_S%YMAX-DIAGI_S%YMIN)/100, &
     &                 0.0, 0.0, STARS(IPS) )
      END IF
      CALL PGSCH ( DIAGI_S%SCH_LAB )
      CALL PGSCI ( 1 )
!
      WRITE ( IPS_CHR, '(I3)' ) IPS
      CALL CHASHL ( IPS_CHR )
!
! --- copy observation arrays into temporary arrays (real*4 and real*8)
! --- good points
!
      DO J1=1,DIAGI_S%NPOI(1)
         IF ( INFO_CHR_G(J1)(11:18) .EQ. STAR ) THEN
            IF ( STAR_NUM .EQ. 0 ) THEN
               SINGLE(1) = 1
               SINGLE(2) = J1
            END IF
            STAR_NUM = STAR_NUM + 1
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_X4(1)+(J1-1)*4), XC_ARG_S(STAR_NUM) )
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_Y4(1)+(J1-1)*4), YC_VAL_S(STAR_NUM) )
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_E4(1)+(J1-1)*4), EC_SIG_S(STAR_NUM) )
            XC_ARG_S8(STAR_NUM) = TG(J1)  ! arguments (real*8)
            YC_VAL_S8(STAR_NUM) = XG(J1)  ! values (real*8)
            EC_SIG_S8(STAR_NUM) = EG(J1)  ! error (real*8)
         END IF
      END DO
!
! --- recoverable points
!
      DO J1=1,DIAGI_S%NPOI(2)
         IF ( INFO_CHR_M(J1)(11:18) .EQ. STAR ) THEN
            IF ( STAR_NUM .EQ. 0 ) THEN
               SINGLE(1) = 2
               SINGLE(2) = J1
            END IF
            STAR_NUM = STAR_NUM + 1
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_X4(2)+(J1-1)*4), XC_ARG_S(STAR_NUM) )
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_Y4(2)+(J1-1)*4), YC_VAL_S(STAR_NUM) )
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_E4(2)+(J1-1)*4), EC_SIG_S(STAR_NUM) )
            XC_ARG_S8(STAR_NUM) = TM(J1)  ! arguments (real*8)
            YC_VAL_S8(STAR_NUM) = XM(J1)  ! values (real*8)
            EC_SIG_S8(STAR_NUM) = EM(J1)  ! error (real*8)
         END IF
      END DO
!
! --- bad points
!
      DO J1=1,DIAGI_S%NPOI(3)
         IF ( INFO_CHR_B(J1)(11:18) .EQ. STAR ) THEN
            IF ( STAR_NUM .EQ. 0 ) THEN
               SINGLE(1) = 3
               SINGLE(2) = J1
            END IF
            STAR_NUM = STAR_NUM + 1
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_X4(3)+(J1-1)*4), XC_ARG_S(STAR_NUM) )
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_Y4(3)+(J1-1)*4), YC_VAL_S(STAR_NUM) )
            CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_E4(3)+(J1-1)*4), EC_SIG_S(STAR_NUM) )
            XC_ARG_S8(STAR_NUM) = TB(J1)  ! arguments (real*8)
            YC_VAL_S8(STAR_NUM) = XB(J1)  ! values (real*8)
            EC_SIG_S8(STAR_NUM) = EB(J1)  ! error (real*8)
         END IF
      END DO
!
! --- bottom message
!
      CALL CLRCH ( STAR_NUM_CH )
      WRITE ( STAR_NUM_CH, '(I3)' ) STAR_NUM
      CALL CHASHL ( STAR_NUM_CH )
!
      IF ( STAR_NUM .GT. 1 ) THEN
         DIAGI_S%MESS_BOT = 'SOURCE #'//IPS_CHR(1:ILEN(IPS_CHR))//': '//STAR//' --> connect '// &
     &                       STAR_NUM_CH(1:ILEN(STAR_NUM_CH))//' points'
      ELSE IF (STAR_NUM .EQ. 1 ) THEN
         IF ( SINGLE(1) .EQ. 1 ) THEN
             DIAGI_S%MESS_BOT = INFO_CHR_G(SINGLE(2))
         ELSE IF ( SINGLE(1) .EQ. 2 ) THEN
             DIAGI_S%MESS_BOT = INFO_CHR_M(SINGLE(2))
         ELSE IF ( SINGLE(1) .EQ. 3 ) THEN
             DIAGI_S%MESS_BOT = INFO_CHR_B(SINGLE(2))
         END IF
         CALL CLRCH ( STRING )
         CALL REPBOTT ( DIAGI_S, STRING, 7 )   ! display observation info line
!
!C       DIAGI_S.MESS_BOT = 'SOURCE #'//IPS_CHR(1:ILEN(IPS_CHR))//': '//STAR//' --> only one observation existing!'
!
      ELSE IF (STAR_NUM .EQ. 0 ) THEN
         RED = .TRUE.
         DIAGI_S%MESS_BOT = 'Source '//STAR//' not found, please try again.'
      END IF
!
! --- copy temporary arrays into the Diagi arrays for special case (index C4)
!
      DO J1=1,STAR_NUM
         CALL REPINNI ( DIAGI_S%NPOI(C4), %VAL(DIAGI_S%ADR_X4(C4)), &
     &                  %VAL(DIAGI_S%ADR_Y4(C4)), %VAL(DIAGI_S%ADR_E4(C4)), &
     &                  XC_ARG_S(J1), YC_VAL_S(J1), EC_SIG_S(J1), &
     &                  TS, XS, ES, XC_ARG_S8(J1),YC_VAL_S8(J1), EC_SIG_S8(J1) )
      END DO
!
! --- draw connected points
!
      CALL DIAGI_DRAW ( DIAGI_S, C4, -1, STAR_NUM, &
     &                  %VAL(DIAGI_S%ADR_X4(C4)), %VAL(DIAGI_S%ADR_Y4(C4)), &
     &                  %VAL(DIAGI_S%ADR_E4(C4)), 0.D0, 0.D0 )
!
! --- set number of elements in special case array back to zero
!C    DIAGI_S.NPOI(C4) = 0
! --- print new bottom message
  400 CONTINUE
      CALL PGSCI  ( 1 )
!
! --- write bottom line
!
      IF ( RED ) THEN
           CALL PGSCI ( 7 )
         ELSE
           CALL PGSCI ( 1 )
      END IF
!
      IF (STAR_NUM .NE. 1 .AND. STAR(1:1) .NE. '%') THEN
!C       CALL DIAGI_PURGE_BOT ( DIAGI_S )
         STRING = DIAGI_S%MESS_BOT
         CALL REPBOTT ( DIAGI_S, STRING, 7 )
      END IF
!
      CALL PGSCI  ( 1 )
      DIAGI_S%MESS_BOT = MESS_BOT_SAV
!
! --- set DiaGi return code (if not =1 --> DiaGI error message & return to calling routine)
!
810   CONTINUE
      CALL PGSCI  ( 1 )
      REPCONN = 1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REPCONN  #!#
