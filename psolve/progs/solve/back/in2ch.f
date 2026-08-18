      SUBROUTINE IN2CH(INT_VAR,CHR_VAR)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  IN2CH PROGRAM SPECIFICATION
!
! 1.1 Convert integer into character string (right justified).
!
! 1.2 REFERENCES:
!
! 2.  IN2CH INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 INT_VAR
!
! INT_VAR - Integer to be converted
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) CHR_VAR
!
! CHR_VAR - Character string to contain specified integer
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arc_i
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*6 CHR_OF_INT,INTTODECIMAL
      INTEGER*2 IL,LE,I,TRIMLEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  IN2CH PROGRAM STRUCTURE
!
      CHR_OF_INT=INTTODECIMAL(INT_VAR)
      IL=TRIMLEN(CHR_OF_INT)
      LE=LEN(CHR_VAR)
!     IF(IL.GT.LE) PAUSE 'IN2CH argument too short'
      IF(IL.GT.LE) call ferr( INT2(133), 'IN2CH argument too short', INT2(0), &
     &   INT2(0) )
      DO I=1,LE-IL
        CHR_VAR(I:I)='0'
      ENDDO
      CHR_VAR(LE-IL+1:)=CHR_OF_INT(1:IL)
!
      RETURN
      END
