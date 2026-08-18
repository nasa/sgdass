! Note: If any changes are made here, changes may be necessary in
!       ../sdbh/namblk.f.
!
! Modifications:
! BA  98.08.26    Added above note.
! pet 2021.07.12  Made IREC nad ILAST INTEGER*4, added ILAST_I2
!
      CHARACTER*70 KBUF
      CHARACTER*63 NNAME
      CHARACTER*4  LHOLD
      INTEGER*2    ISTATUS, IACTSEC, KSTATUS
      INTEGER*2    IFIRST(14), IQQ(16), LBUF(35), IDBS, ILAST_I2
      INTEGER*4    IREC, ILAST
!
      COMMON  / NAMHOL / ISTATUS, IREC, IQQ, IACTSEC, KSTATUS, LBUF, ILAST
      SAVE    / NAMHOL /
      COMMON  / NAMCHR / NNAME, LHOLD
      SAVE    / NAMCHR /
!
      EQUIVALENCE (IQQ,IDBS),(IQQ(2),ILAST_I2),(IQQ(3),IFIRST)
      EQUIVALENCE (LBUF,KBUF)
!
