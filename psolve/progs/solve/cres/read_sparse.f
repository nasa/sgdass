      SUBROUTINE READ_SPARSE ( CVAL, IX, IY, MAXC, NUMC )
      IMPLICIT NONE                         !Added by IMP/jwr
!
! read in a sparse matrix and store it as a vector.
!
      INCLUDE 'precm.i'
      REAL*8    CVAL(*)
      INTEGER*4 NUMC,MAXC
      INTEGER*4 IX(*), IY(*)
      LOGICAL*4 LEX
      CHARACTER FNAME*128
!
      FNAME   = PRE_SCR_DIR(:PRE_SD_LEN)//'CSPR'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( LEX ) THEN
           OPEN ( UNIT=66, FILE=FNAME, STATUS='OLD' )
!
           NUMC=1
           DO NUMC=1,MAXC
              READ ( 66, * ) IX(NUMC), IY(NUMC), CVAL(NUMC)
!
! ----------- this is a flag indicating we are done.
!
              IF ( IX(NUMC) .EQ. 0 ) GOTO 100
           END DO
!
! -------- should get kicked out before here
!
100        CONTINUE
           NUMC=NUMC-1
           CLOSE ( UNIT=66 )
         ELSE
           NUMC = 0
      END IF
      RETURN
      END  !#!  READ_SPARSE  #!#
