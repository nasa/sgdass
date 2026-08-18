      SUBROUTINE HOL2CHAR(IARR,IFC,ILC,CH)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER*(*) CH
      INTEGER*2 IARR(*),IFC,ILC
!
! HOL2CHAR: move a Hollerith string into a character variable
!           blank filled to the right
!
! Input:
!       IARR: Hollerith string
!       IFC:  first character in IARR to move
!       ILC:  last character in IARR to move
!       CH:   destination character string
!
! Output:
!       CH: contains characters IFC...ILC from IARR
!           blank filled to the right if necessery
!
! 2004.06.30  pet  Updated logic for support both BIG_ENDIAN and LITTLE_ENDIAN
!                  architecture
!
      INTEGER*2 IWORD,LN,IEND,I,JCHAR
      CHARACTER*1 CWORD(2)
      EQUIVALENCE (CWORD(1),IWORD)
!
      LN=LEN(CH)
      IEND=MIN(LN,INT2(ILC-IFC+1))
!
      DO I=1,IEND
        IWORD=JCHAR( IARR, INT2(IFC+I-1) )
#ifdef BIG_ENDIAN        
        CH(I:I)=CWORD(2)
#else
        CH(I:I)=CWORD(1)
#endif
      ENDDO
!
      IF ( IEND .LT. LN ) CH(IEND+1:)=' '
!
      RETURN
      END
