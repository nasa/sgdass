      SUBROUTINE sint2decr(INT,ostring)
      IMPLICIT NONE
      INTEGER*2 INT
      character*(*) ostring
!
! SINT2DECR : convert I*2 variable to ascii string right justified
!               (subroutine version of INTTODECIMALR)
!
      CHARACTER*6 STRING
      INTEGER*2 ILR
!
      WRITE(STRING,'(I6)') INT
      ILR=LEN(ostring)
      ostring(MAX(1,ILR-5):ILR)=STRING(MAX(1,7-ILR):6)
      IF(ILR.GT.6) ostring(1:ILR-6)=' '
!
      RETURN
      END
