!This is the start of file &BUFF1
!
      INTEGER*2 BUFF1_WORDS
      PARAMETER (BUFF1_WORDS=(3*MAX_STA_CMP+1)*INTS_WORDS &
     &                       +(3*MAX_STA+1)*REALL_WORDS)
      INTEGER*2 ISCNT(3,MAX_STA_CMP),NSCNT1
      REAL*8 VSITE1(3,MAX_STA),prepoch
!
      INTEGER*2 BUFF3_WORDS
      PARAMETER (BUFF3_WORDS=(3*MAX_STA_CMP+1)*INTS_WORDS &
     &                       +(3*MAX_STA+1)*REALL_WORDS)
      integer*2 BUFF4_WORDS,ifill(3)
      PARAMETER (BUFF4_WORDS=BUFF1_WORDS+BUFF3_WORDS+3)
      INTEGER*2 ISCNTV(3,MAX_STA_CMP),NSCNT1V
      REAL*8 VSITE1V(3,MAX_STA),prepochv
      integer*2 bufbuf4(BUFF4_WORDS)
      equivalence (bufbuf4,vsite1)
      COMMON/BUFF4/VSITE1,prepoch, ISCNT, NSCNT1, &
     &    VSITE1V,prepochv, ISCNTV, NSCNT1V,ifill
