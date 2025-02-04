        SUBROUTINE BLINK ()
! ************************************************************************
! *                                                                      *
! *     ������������  BLINK  ������������� ��� ��������� ��������        *
! *                          ��������                                    *
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
! *     ������������  UNDER  ������������� ��� ��������� ��������        *
! *                          �������������                               *
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
! *     ������������  BRIEF  ������������� ��� ��������� ��������        *
! *                          ���������� �������                          *
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
! *     ������������  NEG  ������������� ��� ��������� ��������          *
! *                        ��������� ����������� ��������                *
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
! *     ������������  UN_BLINK  ������� ��� ��������� ��������           *
! *                             ��������                                 *
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
! *     ������������  UN_UNDER  ������� ��� ��������� ��������           *
! *                             �������������                            *
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
! *     ������������  UN_BRIEF  ������� ��� ��������� ��������           *
! *                          ���������� �������                          *
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
! *     ������������  UN_NEG  ������� ��� ��������� ��������             *
! *                           ��������� ����������� ��������             *
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
! *     ������������  ATTR_NORMAL  ������������� ��� ��������� �����:    *
! *                   ��� ��������� ��������.                            *
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
