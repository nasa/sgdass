      SUBROUTINE CHAR2HOL(CH,IARR,IFC,ILC)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER*(*) CH
      INTEGER*2 IARR(*),IFC,ILC
!
! CHAR2HOL: move a character variable into a hollerith string
!           blank fill to the right
!
! Input:
!       CH:   character string
!       IFC:  first character in IARR to move
!       ILC:  last character in IARR to move
!       IARR: destination Hollerith string
!
! Output:
!        IARR: characters IFC...ILC contain CH
!              blank filled to the right if necessery
!
      INTEGER*2 IWORD,LN,IEND,I
      CHARACTER*1 CWORD(2)
      EQUIVALENCE (CWORD(1),IWORD)
!
      LN=ILC-IFC+1
      IEND=MIN(LN,INT2(LEN(CH)))
!
      DO I=1,IEND
        CWORD(2)=CH(I:I)
        CALL PCHAR( IARR, INT2(IFC+I-1), IWORD )
      ENDDO
!
      IF(IEND.LT.LN) THEN
        DO I=IEND+1,LN
          CALL PCHAR( IARR, INT2(IFC+I-1), 2H  )
        ENDDO
      ENDIF
!
      RETURN
      END
