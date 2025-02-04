      SUBROUTINE WRITE_LONG ( LUN, LPRN, OUT )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine WRITE_LONG writes string OUT in the file openned *
! *   at the logical device LUN being splited the line so that the       *
! *   largest portion will not exceed LPRN bytes.                        *
! *                                                                      *
! *  ###  07-JUL-98    WRITE_LONG  v1.0  (c)  L. Petrov  07-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  OUT*(*)
      INTEGER*4  LUN, LPRN, LOUT, IB, LR, J1, IR, IBG, IE, IE_NEW
      INTEGER*4, EXTERNAL :: ILEN, LINDEX
!
      IBG=1
!
        LOUT = ILEN(OUT)  ! their length without trailing blanks
        IF ( LOUT .LE. 0 ) RETURN
!
        IB=1
        LR=LOUT
!
! ----- Printing it on the screen
!
        DO 410 J1=1,999
           IR = LPRN - IBG  ! acceptable lenght of one line on the screen
           IE = IB+IR  ! the last position of the symbol to be print
!
! -------- Adjusting the last position (moving back if needed)
!
           IF ( IE .GE. LOUT ) THEN
                IE=LOUT
             ELSE
!
! ------------  Seek the last blank symbol on the line.
!
                IE_NEW = LINDEX ( OUT(IB:IE), ' ' ) + IB-1
                IF ( IE_NEW .GT. IB ) IE=IE_NEW  !  moving the right boundary
           END IF
           WRITE ( LUN, FMT='(A)' ) OUT(IB:IE) ! writing
           IB = IE + 1
           IF ( IB .GT. LOUT ) GOTO 810
           IBG = 1
 410    CONTINUE
 810    CONTINUE
!
      RETURN
      END  !#!  WRITE_LONG  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_LONGB ( LUN, LBEG, LPRN, OUT )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine WRITE_LONGB splits the string OUT onto           *
! *   sub-strings. It preserves words. splitting occurs only on          *
! *   delimiters. All sub-lines starts from LBEG-th position and have    *
! *   length not exceedings LPRN symbols. Sublines are written in the    *
! *   file openned at the logical device LUN.                            *
! *                                                                      *
! *  ###  20-APR-99   WRITE_LONGB  v1.0  (c)  L. Petrov  20-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  OUT*(*), BLANK*80
      INTEGER*4  LUN, LBEG, LPRN, LOUT, IB, LR, J1, IR, IBG, IE, IE_NEW
      INTEGER*4, EXTERNAL :: ILEN, LINDEX
      CALL CLRCH ( BLANK )
!
      IBG=1
!
        LOUT = ILEN(OUT)  ! their length without trailing blanks
!
        IB=1
        LR=LOUT
!
! ----- Printing it on the screen
!
        DO 410 J1=1,999
           IR = LPRN - IBG  ! acceptable lenght of one line on the screen
           IE = IB+IR  ! the last position of the symbol to be print
!
! -------- Adjusting the last position (moving back if needed)
!
           IF ( IE .GE. LOUT ) THEN
                IE=LOUT
             ELSE
!
! ------------  Seek the last blank symbol on the line.
!
                IE_NEW = LINDEX ( OUT(IB:IE), ' ' ) + IB-1
                IF ( IE_NEW .GT. IB ) IE=IE_NEW  !  moving the right boundary
           END IF
           IF ( LBEG .GT. 1 ) THEN
                WRITE ( LUN, FMT='(A)' ) BLANK(1:LBEG-1)//OUT(IB:IE) ! writing
              ELSE
                WRITE ( LUN, FMT='(A)' ) OUT(IB:IE) ! writing
           END IF
           IB = IE + 1
           IF ( IB .GT. LOUT ) GOTO 810
           IBG = 1
 410    CONTINUE
 810    CONTINUE
!
      RETURN
      END  !#!  WRITE_LONG  #!#
