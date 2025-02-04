      SUBROUTINE PAMB_FREEZE_MENU ( IACT_FREEZE )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_FREEZE_MENU  gets parameter of freezeing/unfreezing  *
! *   suppression status for phase delay observables in the mode of      *
! *   screen menu.                                                       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * IACT_FREEZE ( INTEGER*4 ) -- Code of operation. Supported codes:     *
! *                         -1 -- no operation (nothing to do);          *
! *                         21 -- freeze X-band delays;                  *
! *                         22 -- freeze S-band delays;                  *
! *                         23 -- freeze both X-band and S-band delays;  *
! *                         31 -- unfreeze X-band delays;                *
! *                         32 -- unfreeze S-band delays;                *
! *                         33 -- unfreeze both X-band and S-band delays;*
! *                                                                      *
! * ### 19-DEC-98  PAMB_FREEZE_MENU  v1.0 (c)  L. Petrov  19-DEC-98  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IACT_FREEZE
      INTEGER*4  IFRZ, IUFR, FREEZE_MIN, FREEZE_MAX, NN, IX, IY
      PARAMETER  ( FREEZE_MIN = 0 )
      PARAMETER  ( FREEZE_MAX = 3 )
      CHARACTER  FREEZE_STR(FREEZE_MIN:FREEZE_MAX)*13, CC4*4, SIM*1
      DATA       ( FREEZE_STR(NN), NN=FREEZE_MIN,FREEZE_MAX) &
     &           / &
     &            'no           ', &
     &            'at X-band    ', &
     &            'at S-band    ', &
     &            'at both bands' &
     &           /
      INTEGER*4  I_LEN
!
      IFRZ = 0
      IUFR = 0
!
! --- Start curser
!
      CALL START_MN()
 910  CONTINUE
!
! ----- Printing the first line: title of the program
!
        CALL CLEAR_MN()
        CALL SETCR_MN (  0, 0 )
        CALL ADDSTR_F ( 'Freeze/unfreeze suppression status for phase '// &
     &                  'delay observables' )
        CALL NL_MN()
!
        CALL ADDSTR_F ( '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'// &
     &                  '°°°°°°°°°°°°°°°°°' )
        CALL NL_MN()
        CALL NL_MN()
!
! ----- Printing status of freezing operation
!
        CALL ADDSTR_F ( '(F) Freeze suppression status   ' )
        CALL REVERSE_ON_MN()
        CALL ADDSTR_F ( FREEZE_STR(IFRZ)(1:I_LEN(FREEZE_STR(IFRZ))) )
        CALL REVERSE_OFF_MN()
        CALL NL_MN()
        CALL NL_MN()
!
! ----- Printing status of unfreezing operation
!
        CALL ADDSTR_F ( '(U) Unfreeze suppression status ' )
        CALL REVERSE_ON_MN()
        CALL ADDSTR_F ( FREEZE_STR(IUFR)(1:I_LEN(FREEZE_STR(IUFR))) )
        CALL REVERSE_OFF_MN()
        CALL NL_MN()
        CALL NL_MN()
!
! ----- Printing the bottom lines
!
        CALL SETCR_MN (  0, 22 )
        CALL ADDSTR_F ( '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'// &
     &                  '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'   )
        CALL NL_MN()
        CALL ADDSTR_F ( '(*) Go ahead    (-) Go back' )
        CALL SETCR_MN ( 1, 23 )
!
! ----- Awaiting for user response
!
        CALL SENKR_MN ( IX, IY, CC4 )
        SIM = CC4(4:4)
!
        CALL TRAN ( 11, SIM, SIM )
!
! ----- Transforming cursor coordinamtes in letters
!
        IF ( SIM .EQ. ' '  .OR.  SIM .EQ. CHAR(13) ) THEN
             IF ( IY .EQ. 4                     ) SIM = 'F'
             IF ( IY .EQ. 6                     ) SIM = 'U'
             IF ( IY .EQ. 23  .AND.  IX .LE. 15 ) SIM = '*'
             IF ( IY .EQ. 23  .AND.  IX .GT. 15 ) SIM = '-'
        END  IF
!
! ----- Parsing user response
!
        IF ( SIM .EQ. 'F' ) THEN
!
! ---------- Change "freeze" action
!
             IFRZ = IFRZ + 1
             IF ( IFRZ .GT. FREEZE_MAX ) IFRZ = FREEZE_MIN
             IUFR = FREEZE_MIN
          ELSE IF ( SIM .EQ. 'U' ) THEN
!
! ---------- Change "unfreeze" action
!
             IUFR = IUFR + 1
             IF ( IUFR .GT. FREEZE_MAX ) IUFR = FREEZE_MIN
             IFRZ = FREEZE_MIN
          ELSE IF ( SIM .EQ. '*' ) THEN
!
! ---------- Go ahead command was selected. Set IACT_FREEZE code action
!
             IF ( IUFR .EQ. 0 ) THEN
                  IF ( IFRZ .EQ. 0 ) THEN
                       IACT_FREEZE = -1
                     ELSE IF ( IFRZ .EQ. 1 ) THEN
                       IACT_FREEZE = 21
                     ELSE IF ( IFRZ .EQ. 2 ) THEN
                       IACT_FREEZE = 22
                     ELSE IF ( IFRZ .EQ. 3 ) THEN
                       IACT_FREEZE = 23
                  END IF
             END IF
!
             IF ( IFRZ .EQ. 0 ) THEN
                  IF ( IUFR .EQ. 0 ) THEN
                       IACT_FREEZE = -1
                     ELSE IF ( IUFR .EQ. 1 ) THEN
                       IACT_FREEZE = 31
                     ELSE IF ( IUFR .EQ. 2 ) THEN
                       IACT_FREEZE = 32
                     ELSE IF ( IUFR .EQ. 3 ) THEN
                       IACT_FREEZE = 33
                  END IF
             END IF
!
             GOTO 810
          ELSE IF ( SIM .EQ. '-' ) THEN
!
! ---------- Go back
!
             IACT_FREEZE = -1
             GOTO 810
        END IF
      GOTO 910
!
 810  CONTINUE
!
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
      RETURN
      END  !#!  PAMB_FREEZE_MENU  #!#
