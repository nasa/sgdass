      SUBROUTINE PRINT_NORMAL ( LUN, ARR, JA, NPARIN )
! ************************************************************************
! *                                                                      *
! *   Rotine  PRINT_NORMAL  prints normal matrix to the file opened at   *
! *   the logical divice  LUN.                                           *
! *                                                                      *
! *  ###  09-MAY-98  PRINT_NORMAL  v1.0  (c)  L. Petrov  09-MAY-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*2  LUN, NPARIN
      INTEGER*4  JA
      REAL*8     ARR(*)
      INTEGER*4  FIRST, LAST, I, J, K, NLINES
!
! --- To print normal matrix in spool file
!
      WRITE ( LUN, '("NORMAL MATRIX:")' )
      FIRST = 0
      LAST = 0
      DO I=1,NPARIN
         NLINES = (I+6)/7
         DO J=1,NLINES
            FIRST = 1+LAST
            LAST = MIN(7,I-7*(J-1))+LAST
            IF ( J.EQ.1 ) THEN
                 WRITE ( LUN, '(I6, 7(1X,E15.9) )' ) I, &
     &                   ( ARR(JA-1+K), K=FIRST,LAST )
               ELSE
                 WRITE ( LUN, '(6X, 7(1X, E15.9) )' ) &
     &                   ( ARR(JA-1+K), K=FIRST,LAST )
            ENDIF
         ENDDO
      ENDDO
!
      RETURN
      END  !#!  PRINT_NORMAL  #!#
