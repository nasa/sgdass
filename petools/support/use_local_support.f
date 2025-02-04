        SUBROUTINE CHASHL ( STR )
! ************************************************************************
! *                                                                      *
! *   Routine   CHASHL  adjusts string STR at the left edge. Thus,       *
! *   the first symbol of the string STR becomes the symbol which is not *
! *   a blank or a binary zero. Freed symbols at the right edge are      *
! *   replaced by blanks.                                                *
! *                                                                      *
! *  ###  21-DEC-90      CHASHL    v1.0  (c)  L. Petrov  21-DEC-90  ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*(*)
!
        LN=LEN(STR)
!
! ----- Search for the fist non-blankl symbol
!
        DO 410 J1=1,LN
           IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) THEN
                IB=J1
                GOTO 810
           END IF
 410    CONTINUE
        RETURN
!
 810    CONTINUE
        IF ( IB .EQ. 1 ) RETURN
!
! ----- Shift IB-1 sybols to the left
!
        STR(1:LN-(IB-1))=STR(IB:LN)
!
! ----- Replacing symbols at the right edge by blanks
!
        CALL CLRCH ( STR(LN-(IB-1)+1:) )
!
        RETURN
        END  !#!  CHASHL  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION ILEN(STR)
      CHARACTER STR*(*)
      ILEN=0
      DO 410 J1=LEN(STR),1,-1
         ILEN=J1
         IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) RETURN
 410  CONTINUE
      ILEN=0
      RETURN
      END  !#!  ILEN  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION I_LEN(STR)
      CHARACTER STR*(*)
      I_LEN=1
      DO 410 J1=LEN(STR),1,-1
         I_LEN=J1
         IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) RETURN
 410  CONTINUE
      I_LEN=1
      RETURN
      END  !#!  ILEN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CLRCH ( STR )
      CHARACTER  STR*(*)
      DO 410 J1=1,LEN(STR)
         STR(J1:J1)=' '
 410  CONTINUE
      RETURN
      END  !#!  CLRCH  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_FILE ( FINAM, MBUF, BUF, NBUF, IERR )
! ************************************************************************
! *                                                                      *
! *  Subprogram  READ_FILE  reads text file  FINAM and puts its content  *
! *  in the character array BUF.                                         *
! *                                                                      *
! *  ### 22-MAY-2000   READ_FILE   v1.0 (c)  L. Petrov  22-MAY-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MBUF, NBUF, IERR
      CHARACTER  FINAM*(*), BUF(MBUF)*(*)
!
      LOGICAL*4  LEX
      INTEGER*4  I11, J1
!
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
            IERR = 1
            RETURN
      END IF
!
      OPEN ( UNIT=11, FILE=FINAM, STATUS='OLD', IOSTAT=IERR )
      IF ( IERR .NE. 0 ) THEN
           RETURN
      END IF
!
      NBUF = 0
      DO 410 J1=1,MBUF
         READ ( UNIT=11, FMT='(A)', IOSTAT=I11 ) BUF(J1)
         IF ( I11 .EQ. -1 ) THEN
              IERR = 0
              RETURN
           ELSE IF ( IERR .NE. 0 ) THEN
              IERR = I11
              RETURN
         END IF
         NBUF = NBUF + 1
 410  CONTINUE
      IERR = 2
      RETURN
      END  !#!  READ_FILE  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION FIND_ELEM ( NPAR, CHAR_ARRAY, CHAR_ELEM )
! ************************************************************************
! *                                                                      *
! *   Finction FIND_ELEM scans character array CHAR_ARRAY of NPAR        *
! *   character strings and if it finds there a string CHAR_ELEM it      *
! *   returns the index of the element of the array CHAR_ARRAY which     *
! *   is equal CHAR_ELEM.                                                *
! *                                                                      *
! *  ### 24-MAY-2000   FIND_ELEM   v1.0 (c)  L. Petrov  24-MAY-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NPAR, FIND_ELEM
      CHARACTER  CHAR_ARRAY(NPAR)*(*), CHAR_ELEM*(*)
      INTEGER*4  IP, J1, I_LEN
!
      FIND_ELEM = 0
      IF ( NPAR .LE. 0 ) THEN
           FIND_ELEM = -1
           RETURN
      END IF
!
      IP = I_LEN(CHAR_ELEM)
      DO 410 J1=1,NPAR
         IF ( CHAR_ARRAY(J1)(1:IP) .EQ. CHAR_ELEM(1:IP) ) THEN
              FIND_ELEM = J1
              RETURN
        END IF
 410  CONTINUE
      RETURN
      END  !#!  FIND_ELEM  #!#
