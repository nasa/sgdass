      SUBROUTINE SPLITSTRING ( STRING, PART1, PART2 )
! ************************************************************************
! *                                                                      *
! *   Routine SPLITSTRING
! *                                                                      *
! *  ### 15-AUG-2003   SPLITSTRING  v1.0 (c)  L. Petrov 15-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  STRING*(*), PART1*(*), PART2*(*)
      INTEGER*4  FIRST_IND, LAST_IND, J1
      LOGICAL*4  FL_BODY
!
      FL_BODY = .FALSE.
      FIRST_IND = 0
      DO 410 J1=1,LEN(STRING)
         IF ( ICHAR(STRING(J1:J1)) .GT. 32  .AND. STRING(J1:J1) .NE. ',' ) THEN
              IF ( FIRST_IND .EQ. 0 ) FIRST_IND = J1
              FL_BODY = .TRUE.
            ELSE 
              IF ( FL_BODY ) THEN
                   LAST_IND = J1-1
                   GOTO 810
              END IF
         END IF
 410  CONTINUE 
      LAST_IND = LEN(STRING)
 810  CONTINUE 
      CALL CLRCH ( PART1 )
      IF ( FIRST_IND .GT. 0  .AND.  LAST_IND .GT. 0 ) THEN
           PART1 = STRING(FIRST_IND:LAST_IND)
           IF ( LAST_IND .LT. LEN(STRING)-1 ) THEN
                PART2 = STRING(LAST_IND+2:)
           END IF
         ELSE 
           CALL CLRCH ( PART1 ) 
           PART2 = STRING
      END IF
!
      RETURN
      END  !#!  SPLITSTRING  #!#
!@      Subroutine SplitString(string,part1,part2)
!@      Implicit none
!@      Character*(*) string, part1, part2
!@!
!@!  acceptable delimiters are blank and ',' only
!@!***** MWH  940218  added TAB to list of acceptable delimiters*****
!@!
!@      INTEGER*2 LENGSTR,I,ISTART,ISTOP,INEXT
!@!
!@      lengstr = len(string)
!@!
!@!   find first nonblank
!@!
!@      I=1
!@      DO WHILE(I.LE.LENGSTR)
!@        IF(STRING(I:I).NE.' '.and.string(i:i).ne.char(9)) THEN
!@          ISTART=I
!@          GO TO 100
!@        ENDIF
!@        I=I+1
!@      ENDDO
!@      ISTART=LENGSTR+1
!@!
!@!  find first delimiter
!@!
!@100   CONTINUE
!@      DO WHILE(I.LE.LENGSTR)
!@         IF(INDEX(' ,'//char(9),STRING(I:I)).NE.0) THEN
!@!         IF(INDEX(' ,',STRING(I:I)).NE.0) THEN
!@           ISTOP=I-1
!@           GO TO 200
!@         ENDIF
!@         I=I+1
!@      ENDDO
!@      ISTOP=LENGSTR
!@!
!@!  save everything after the delimiter
!@!
!@200   CONTINUE
!@      INEXT=ISTOP+2
!@!
!@! if there is something to copy, do so, otherwise blank
!@!
!@      IF(ISTART.LE.ISTOP) THEN
!@        PART1=STRING(ISTART:ISTOP)
!@      ELSE
!@        PART1=' '
!@      ENDIF
!@!
!@! save the rest of the string
!@!
!@      IF(INEXT.LE.LENGSTR) THEN
!@        PART2=STRING(INEXT:)
!@      ELSE
!@        PART2=' '
!@      ENDIF
!@!
!@      RETURN
!@      END
