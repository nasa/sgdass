        SUBROUTINE BLINK ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  BLINK  “‘’€€‚‹ˆ‚€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’        *
! *                          Œ…–€ˆ…                                    *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*4
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            STR=CHAR(27)//'[5m'
            CALL PRCH( STR )
            RETURN
          ELSE IF ( IT.EQ.6 ) THEN
!
! ------- not implemented
!
        END IF
        RETURN
        END  !#!  BLINK  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE UNDER ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  UNDER  “‘’€€‚‹ˆ‚€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’        *
! *                          Ž„—…Šˆ‚€ˆ…                               *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*4
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            STR=CHAR(27)//'[4m'
          ELSE IF ( IT.EQ.6 ) THEN
            STR=CHAR(27)//'&dD'
        END IF
        CALL PRCH( STR )
        RETURN
        END  !#!  UNDER  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE BRIEF ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  BRIEF  “‘’€€‚‹ˆ‚€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’        *
! *                          Ž‚›˜…€Ÿ ŸŠŽ‘’œ                          *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*4
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            STR=CHAR(27)//'[1m'
          ELSE IF ( IT.EQ.6 ) THEN
              STR=CHAR(27)//'(3B'
        END IF
        CALL PRCH( STR )
        RETURN
        END  !#!  BRIEF  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE NEG ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  NEG  “‘’€€‚‹ˆ‚€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’          *
! *                        ˆ‚…‘Ž… Ž’Ž€†…ˆ… ‘ˆŒ‚Ž‹Ž‚                *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*4
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            STR=CHAR(27)//'[7m'
          ELSE IF ( IT .EQ. 6 ) THEN
            STR=CHAR(27)//'&dB'
        END IF
        CALL PRCH( STR )
        RETURN
        END  !#!  NEG  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE UN_BLINK ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  UN_BLINK  ‘ˆŒ€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’           *
! *                             Œ…–€ˆ…                                 *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*5
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( IT.GE.1 .AND. IT.LE.5 ) THEN
            STR=CHAR(27)//'[25m'
            CALL PRCH( STR )
          ELSE IF ( IT.EQ.6 ) THEN
            STR=CHAR(27)//'&d@'
            CALL PRCH( STR(1:4) )
          ELSE IF ( IT.EQ.7 ) THEN
            CALL ATTR_NORMAL
        END IF
        RETURN
        END  !#!  UN_BLINK  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE UN_UNDER ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  UN_UNDER  ‘ˆŒ€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’           *
! *                             Ž„—…Šˆ‚€ˆ…                            *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*5
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( IT.GE.1 .AND. IT.LE.5 ) THEN
            STR=CHAR(27)//'[24m'
            CALL PRCH( STR )
          ELSE IF ( IT.EQ.6 ) THEN
            STR=CHAR(27)//'&d@'
            CALL PRCH( STR )
          ELSE IF ( IT.EQ.7 ) THEN
            CALL ATTR_NORMAL
        END IF
        RETURN
        END  !#!  UN_UNDER  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE UN_BRIEF ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  UN_BRIEF  ‘ˆŒ€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’           *
! *                          Ž‚›˜…€Ÿ ŸŠŽ‘’œ                          *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*5
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( IT.GE.1 .AND. IT.LE.5 ) THEN
            STR=CHAR(27)//'[22m'
            CALL PRCH( STR )
          ELSE IF ( IT.EQ.6 ) THEN
            STR=CHAR(27)//'&d@'
            CALL PRCH( STR )
          ELSE IF ( IT.EQ.7 ) THEN
            CALL ATTR_NORMAL
        END IF
        RETURN
        END  !#!  UN_BRIEF  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE UN_NEG ()
! ************************************************************************
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  UN_NEG  ‘ˆŒ€…’ „‹Ÿ ’…Œˆ€‹€ €’’ˆ“’             *
! *                           ˆ‚…‘Ž… Ž’Ž€†…ˆ… ‘ˆŒ‚Ž‹Ž‚             *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*5
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( IT.GE.1 .AND. IT.LE.5 ) THEN
            STR=CHAR(27)//'[27m'
            CALL PRCH( STR )
          ELSE IF ( IT.EQ.6 ) THEN
            STR=CHAR(27)//'&d@'
            CALL PRCH( STR )
          ELSE IF ( IT.EQ.7 ) THEN
            CALL ATTR_NORMAL
        END IF
        RETURN
        END  !#!  UN_NEG  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE ATTR_NORMAL ()
! ************************************************************************ 
! *                                                                      *
! *     Ž„Žƒ€ŒŒ€  ATTR_NORMAL  “‘’€€‚‹ˆ‚€…’ „‹Ÿ ’…Œˆ€‹€ …†ˆŒ:    *
! *                   ‚‘… €’’ˆ“’› ‚Š‹ž—…›.                            *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*4
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( IT.GE.1 .AND. IT.LE.5 ) THEN
            STR=CHAR(27)//'[0m'
          ELSE IF ( IT.EQ.6 ) THEN
            STR=CHAR(27)//'&d@'
          ELSE IF ( IT .LE. 7 ) THEN
            STR=CHAR(27)//'[0m'
        END IF
        CALL PRCH ( STR )
        RETURN
        END  !#!  ATTR_NORMAL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SET_COLOR ( NCOL )
! ************************************************************************
! *                                                                      *
! *     Subroutine  SET_COLOR  sets up the color which should be used    *
! *     from the current cusor position and eighter up to the end of     *
! *     or up to the new color change.                                   *
! *                                                                      *
! *  ###   17-Dec-96   SET_COLOR  v1.2  (c)   L. Petrov 20-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      CHARACTER STR*5
      LOGICAL*4  USE_TERM_COLOR
!
      CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
!
      IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR () ) THEN
           IF ( NCOL.LT.0    .OR.   NCOL.GT.7 ) NCOL=0
           STR=CHAR(27)//'&v0S'
           CALL INCH ( NCOL, STR(4:4) )
           CALL PRCH ( STR )
        ELSE IF ( IT .EQ. 7  .AND.  USE_TERM_COLOR () ) THEN
           IF ( NCOL.LT.0    .OR.   NCOL.GT.7 ) NCOL=0
           STR=CHAR(27)//'[30m'
           CALL INCH ( NCOL, STR(4:4) )
           CALL PRCH ( STR )
      END IF
      RETURN
      END  !#!  SET_COLOR  #!#
