!@This is the start of file @EXCEPTIONS
!
!  pet  2001.08.10  Created instead of the old velcm.i and propnm.i
!
      INTEGER*2   NUMEXC_STA
      CHARACTER   STA_CMP_ALL*3, STA_CMP(MAX_STA)*3, STA_NAM(MAX_STA)*8
!
      INTEGER*2   NUMEXC_VEL
      CHARACTER   VEL_CMP_ALL*3, VEL_CMP(MAX_STA)*3, VEL_NAM(MAX_STA)*8
!
      INTEGER*2   NUMEXC_SOU
      CHARACTER   SOU_CMP_ALL*2, SOU_CMP(MAX_SRC)*2, SOU_NAM(MAX_SRC)*8
!
      INTEGER*2   NUMEXC_PRO
      CHARACTER   PRO_CMP_ALL*2, PRO_CMP(MAX_SRC)*2, PRO_NAM(MAX_SRC)*8
!
      COMMON / EXCEPTIONS / NUMEXC_STA, STA_CMP_ALL, STA_CMP, STA_NAM, &
     &                      NUMEXC_VEL, VEL_CMP_ALL, VEL_CMP, VEL_NAM, &
     &                      NUMEXC_SOU, SOU_CMP_ALL, SOU_CMP, SOU_NAM, &
     &                      NUMEXC_PRO, PRO_CMP_ALL, PRO_CMP, PRO_NAM
