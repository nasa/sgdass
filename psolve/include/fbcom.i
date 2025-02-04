!This is the start of file &FBCOM
!
! Last update: 12-OCT-2004 15:04:11
!
      COMMON /FBCOM/ &
     &               SUBXYZ(3,MAX_STA),  LSINAM(4,MAX_STA),           &
     &               SUBRD(2,MAX_SRC),   LSONAM(4,MAX_SRC),           &
     &               SUBXYZ2(3,MAX_STA), LSINAM2(4,MAX_STA),          &
     &               SUBVEL(3,MAX_STA),  LVELNAM(4,MAX_STA),          &
     &               SUBVEL2(3,MAX_STA), LVELNAM2(4,MAX_STA),         &
     &               SUBRD2(2,MAX_SRC),  LSONAM2(4,MAX_SRC),          &
     &               SUBAX(MAX_STA),     LAXNAM_CHR(MAX_STA), NUMAXOF
!
      INTEGER*4      NUMAXOF
      INTEGER*2      LSINAM, LSONAM, LSINAM2, LSONAM2, LVELNAM, LVELNAM2
      REAL*8         SUBXYZ, SUBRD, SUBXYZ2, SUBRD2, SUBVEL, SUBVEL2
      REAL*8         SUBAX
      CHARACTER      LAXNAM_CHR*8
!
