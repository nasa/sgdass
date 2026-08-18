        SUBROUTINE RMPAR ( IPAR )
        IMPLICIT NONE                         !Added by IMP/jwr
!
!  provides compatibility for HP RTE-A command line argument get calls.
!  This routine was written to speed up the RTE-A to HP-UX conversion
!  process.  The new HP-UX calls IGETARG() and ARGC() are more general
!  since they return character strings rather than just integers and
!  thus should be the basis of subsequent developments.
!  Note: This version does not support twin characters (e.g. ::) on input.
!
!
        INTEGER*4 I, IO
        INTEGER*2 IPAR(5), LEN_ARGCH, INUM, TRIMLEN, IARGCH
        CHARACTER ARGCH*80      ! length of string should = BUFFSIZE
        EQUIVALENCE (IARGCH,ARGCH)
        INTEGER*4  IARGC
!
        DO I= 1,5
           IPAR(I) = 0
        ENDDO
!
        INUM = IARGC()
        IF ( INUM .EQ. 0 ) THEN
!
! ---------- leave with no parameters
!
             IPAR(1) = 1
             RETURN
        ENDIF
!
        IF ( INUM > 5 ) INUM = 5
        DO I = 1,INUM
           CALL GETARG ( I, ARGCH )
           LEN_ARGCH = TRIMLEN(ARGCH)
           IF ( LEN_ARGCH .GT. 0 ) THEN
                IF ( INDEX ( '+-0123456789', ARGCH(1:1)) .EQ. 0 ) THEN
                     CALL CASEFOLD ( ARGCH(1:2) )
                     IPAR(I)=IARGCH
                   ELSE
                     READ ( ARGCH(1:LEN_ARGCH), *, IOSTAT=IO ) IPAR(I)
                END IF
           END IF
        ENDDO
!
        RETURN
        END  !#!  RMPAR  #!#
