      SUBROUTINE ADD_CONSTRAINT ( A, V, NP, WT )
      IMPLICIT   NONE
      REAL*8     A(*), V(*), WT
      INTEGER*4 NP
      INTEGER*4 I, J, K
      INTEGER*8 INDX8, IND
!
      DO I=1,NP
         IF ( V(I) .NE. 0 ) THEN
              DO J=1,I
                 IND=INDX8(I,J)
                 A(IND) = A(IND) + V(I)*V(J)*WT
              ENDDO
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  ADD_CONSTRAINT  #!#
!
