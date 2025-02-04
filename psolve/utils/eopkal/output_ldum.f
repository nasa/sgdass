      SUBROUTINE OUTPUT_LDUM ( LDUM )
      IMPLICIT   NONE
      CHARACTER  LDUM*(*)
!
      WRITE ( 6, '(A)' ) LDUM(1:75)
      WRITE ( 2, '(A)' ) LDUM(1:75)
!
      RETURN
      END  !#!  OUTPUT_LDUM  #!#
