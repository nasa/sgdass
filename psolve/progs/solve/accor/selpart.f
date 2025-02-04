      SUBROUTINE SELPART ( JPART, PARTIAL, QSITN, JNSTA, &
     &                   NPART,  PROGCOM, LDBNAM, NNVER)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
!
! 1.  SELPART PROGRAM SPECIFICATION
!
! 1.1
!     THIS SUBROUTINE ENABLES THE USER TO SELECT WHICH PARTIALS
!     WILL BE USED FOR GIVEN STATIONS.
!
!
! 1.2 REFERENCES:
!
! 2.  SELPART INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2  JPART(7,*)
      INTEGER*2  LDBNAM(*)
      CHARACTER  PROGCOM*1, PARTIAL(*)*8, QSITN(*)*8, STR*79, GET_VERSION*54
      INTEGER*2  JNSTA, NPART,  NNVER
!
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: accor
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, ICALIBHDY, IERRLIN, IERRX, IXTITLE, IYTITLE, &
     &          J, ISW, IAS2B, TRIMLEN
      INTEGER*4 IX, IY, IOS, ICH, ISCRCHNGLIN
      CHARACTER BUFSTR*79, CCH*4
      EQUIVALENCE (ICH,CCH)
!
      DATA ICALIBHDY/2/
      DATA IERRX/0/, IERRLIN/22/
      DATA ISCRCHNGLIN/21/
      DATA IXTITLE/0/
      DATA IYTITLE/0/
!
      INTEGER*4 I_LEN
      INTEGER*2 INT2_ARG
!
! 4.  HISTORY
!   WHO   WHEN    WHAT
!   AEE   920522  Created, based on selcorf, also added code to switch between
!                 flyby and non-flyby menus.
!   MWH  920728   Modified menu format, made much simpler
!   JLR  921215   replaced 0J with I4P0
!   KDB  990210   Fix error: reference to illegal value (part_applied = 0)
!   pet  2000.07.04   Added printing a version label at the first line and
!                     the session name at the second line
!   jwr  2002.12.17   Call in 'ias2b', which had a sleeping bug, replaced
!                     with in core read.
!   pet  2004.05.16   Fix an error in the previous fix
!
! 5.  SELPART PROGRAM STRUCTURE
!
!     subroutine initialization
!
!     BUILD/REBUILD SCREEN.
!
!     CLEAR SCREEN AND WRITE TITLE
!
 75   CONTINUE
      call start_mn()
      CALL CLEAR_MN()
      IX = IXTITLE
      IY = IYTITLE
      CALL SETCR_MN ( IX, IY )
      BUFSTR = 'Atmosphere Partials: '
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( BUFSTR(1:10) )
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( BUFSTR(11:)  )
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      CALL SETCR_MN ( 58, 1 )
      WRITE ( BUFSTR, 1010 ) (LDBNAM(J), J = 1, 5), NNVER
 1010 FORMAT ( 5A2, '  Vers.', I4 )
      CALL ADDSTR_F ( BUFSTR(1:21) )
!
! --- Then list partial options
!
      IX = 0
      iy = icalibhdy
      call setcr_mn(ix,iy )
      do i=1,npart
        WRITE(bufstr,"(I3,2X,A8)") i,partial(i)
        if (part_applied.eq.i) then
          call reverse_on_mn()
          call addstr_f(bufstr(:trimlen(bufstr)) )
          call reverse_off_mn()
        else
          call addstr_f(bufstr )
        endif
        call nl_mn()
        call refresh_mn()
      enddo
!
!     THEN BUILD LINE OF SCREEN CHANGING INSTRUCTIONS
!
      CALL SETCR_MN ( 0, ISCRCHNGLIN )
      call addstr_f("return (O)ptions  (F)lyby  (D)atabase" )
      call nl_mn()
!
!     ACCEPT, INTERPRET USER'S COMMAND.  ERASE ANY MESSAGES FROM THE
!     USER'S PREVIOUS COMMAND.
!
 90   CONTINUE
      call use_glbfil_4('ORC' )
      ix = 0
      iy = icalibhdy+part_applied-1
 910  CONTINUE 
      CALL SETCR_MN ( IX, IY )
      CALL SENKR_MN ( IX, IY, ICH )
!
!     NORMAL COMMAND PROCESSING:  SELPART WILL RECOGNIZE EACH COMMAND
!     AS A PAIR OF COORDINATES, SO CONVERT LETTER AND NUMBER COMMANDS
!     TO THE PROPER COORDINATES.  (0, 0) WILL BE INVALID COORDINATES,
!     GENERATING AN INVALID MESSAGE FOR AN INVALID KEY COMMAND.
!
      IF (CCH(4:4) .EQ. ' ') GO TO 100
!
!
 101  CONTINUE
      IF (CCH(4:4) .EQ. 'F') THEN  !  (F)lyby
        progcom = 'F'
        goto 150
      ELSE IF (CCH(4:4) .EQ. 'D') THEN  !  (D)atabase
        progcom = 'D'
        goto 150
      ELSE IF (CCH(4:4) .EQ. 'O') THEN
        progcom = 'R'
        goto 150
      ELSE IF (CCH(4:4) .EQ. 'R') THEN !Hidden option (refresh screen)
        GO TO 75
      ELSE IF (CCH(4:4) .EQ. '1' .AND. num_part .GE. 1) THEN
        part_applied= 1
      ELSE IF (CCH(4:4) .EQ. '2' .AND. num_part .GE. 2) THEN
        part_applied= 2
      ELSE IF (CCH(4:4) .EQ. '3' .AND. num_part .GE. 3) THEN
        part_applied= 3
      ELSE IF (CCH(4:4) .EQ. '4' .AND. num_part .GE. 4) THEN
        part_applied= 4
      ELSE IF (CCH(4:4) .EQ. '5' .AND. num_part .GE. 5) THEN
        part_applied= 5
      ELSE IF (CCH(4:4) .EQ. '6' .AND. num_part .GE. 6) THEN
        part_applied= 6
      ELSE IF (CCH(4:4) .EQ. '7' .AND. num_part .GE. 7) THEN
        part_applied= 7
      ELSE IF (CCH(4:4) .EQ. '8' .AND. num_part .GE. 8) THEN
        part_applied= 8
      ELSE IF (CCH(4:4) .EQ. '9' .AND. num_part .GE. 9) THEN
        part_applied= 9
      ELSE
      goto 75
      END IF
!
! --- Call in lnfch routine to convert ascii to binary replaced with
! --- in fortran read.  Old statement had a sleeping bug.
!
      READ ( CCH(4:4), '(I4)', IOSTAT=IOS ) ISW
      IF ( IOS .NE. 0 ) THEN
           IX = 1 
           IY = 20
           CALL SETCR_MN ( IX, IY )
           CALL ADDSTR_F ( 'Hit any key to proceed and then enter '// &
     &                     'an integer number' )
           CALL SETCR_MN (IX, IY )
           IX = 1 
           IY = 20
           CALL ADDSTR_F ( '                                      '// &
     &                     '                 ' )
           GOTO 910
      END IF
!
      IF ( ISW .GE. 0  .AND.  ISW .LE. NUM_PART ) THEN
           CALL USE_GLBFIL_4 ( 'OWC' )
           GOTO 75
      ENDIF
!
!     THE USER'S INPUT IS NOW EXPRESSED AS COORDINATES.  PERFORM THE
!     THE USER'S COMMAND.
!
 100  CONTINUE
!
      IF ( IY .GE. ICALIBHDY           .AND. &
     &     IY .LT. (ICALIBHDY + NPART)       ) THEN
!
           PART_APPLIED = IY - ICALIBHDY + 1
           CALL USE_GLBFIL_4 ('OWC' )
           GOTO 75
      END IF
!
      goto 90
 150  CONTINUE
      RETURN
      END
