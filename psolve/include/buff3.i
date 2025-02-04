!This is the start of file &BUFF3
!
      INTEGER*2 BUFF3_WORDS
      PARAMETER (BUFF3_WORDS=(3*MAX_STA_CMP+1)*INTS_WORDS &
     &                       +(3*MAX_STA+1)*REALL_WORDS)
      INTEGER*2 ISCNTV(3,MAX_STA_CMP),NSCNT1V
      REAL*8 VSITE1V(3,MAX_STA),prepochv
      integer*2 bufbuf3(BUFF3_WORDS)
      equivalence (bufbuf3,vsite1v)
      COMMON/BUFF3/VSITE1V,prepochv, ISCNTV, NSCNT1V
