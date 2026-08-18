      SUBROUTINE DO_SUPRS ( FAST_DBG, A, B, IXATS, NPARAM, LPARMA )
!CCCC
!
!  pet 980206 Added debugging capacity
!  pet 980206 Fixed old-old bug in logic: previous version zeroed only the
!             upper part of the matrix column/row (above the main diagonal)
!
!CCCC
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INTEGER*4  NPARAM, IXATS(M_GPA)
      REAL*8     A(* ),B(*)
      CHARACTER  LPARMA(M_GPA)*(*)
!
      INTEGER*4  I, J, ICOU, FAST_DBG
      INTEGER*8, EXTERNAL :: INDX8
!
      ICOU = 0
      DO I=1,NPARAM
         IF ( IXATS(I).EQ.0 ) THEN
              ICOU = ICOU + 1
              B(I)=0.0D0
              DO J=1,NPARAM
                 A(INDX8(J,I))=0.0D0
              ENDDO
              A(INDX8(I,I))=1.0D0
!
              IF ( FAST_DBG .EQ. F__PRI ) THEN
                   WRITE ( 6, 110 )  ICOU, I, LPARMA(I)
 110               FORMAT ( 1X,'NORML: ',I4,') suppressed parameter: ',i4, &
     &                         '  --  "',A,'"' )
              END IF
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  DO_SUPRS  #!#
