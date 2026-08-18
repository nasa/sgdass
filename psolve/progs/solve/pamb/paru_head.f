      PROGRAM   PARU_HEAD
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'pamb.i'
      CHARACTER  PARU_FIL*180
      TYPE ( PAR__STRU ) ::  PAR
      INTEGER*4  IUER
!
      CALL CLRCH ( PARU_FIL )
      PARU_FIL = '/disk4/vlbi/petrov/paru/plain.par'
      IUER = -1
      CALL PARU_COMPILE ( PARU_FIL, PAR, IUER )
      WRITE ( 6, * ) 'PARU_HEAD:  IUER =',IUER
      WRITE ( 6, * ) '------------------------------------------------'
      CALL PARU_DUMP ( PAR )
!
      END  !#!  PARU_HEAD
