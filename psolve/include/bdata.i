!@This is the start of file &BDATA
!
!   Flags and their meanings:
!
      LOGICAL*2 KDSRC,KDSTA
      INTEGER*4 IDSRC, IDSTA, NDSTA, NDSRC
      INTEGER*2 IDELAR(4*(MAX4_SIT+MAX4_SRC))
      CHARACTER DTYPE*6
!
      COMMON /BDATA/ &
     &              KDSRC,KDSTA,IDSRC,IDSTA,NDSRC,NDSTA,IDELAR,DTYPE
