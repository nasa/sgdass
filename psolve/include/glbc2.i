!@This is the start of file &GLBC2
!
!   Flags and their meanings:
!
!   NB: when change this block one should trace its actual size. It
!       should correspond to JGLBC2_BLOCKS from solve.i
!
!     modifications
!
!     jmg 960610 Parameterize iselar size and equivalence to lselar.
!                lselar contains a list of stations and sources which
!                are carried.
!     pet 980720      Increased MAX_SELAR from 125.5 to 1000
!     pet 2004.06.29  Increased MAX_SELAR from 1000  to 8000
!     pet 2016.08.10  Increased MAX_SELAR from 8000  to 8191
!     pet 2017.07.15  Introduced variable MAX4_SLAR
!
      INTEGER*2   LMAX_SELAR, MAX_SELAR, GLBC2_BT
      INTEGER*4   LMAX4_SELAR, MAX4_SELAR
      PARAMETER ( LMAX4_SELAR = 32*1024, MAX4_SELAR = 4*LMAX4_SELAR )
      PARAMETER ( LMAX_SELAR=8191, MAX_SELAR=4*LMAX_SELAR  )
      PARAMETER ( GLBC2_BT = 222 )
      CHARACTER   FILL*1, CARCMP*3, FILLER_GLBC2_BT*(GLBC2_BT)
!
! --- Common area GLBC2 keeps information which objects should be
! --- carried (not carried) to the list of global parameters list
! --- KCSTA, IACSTA, NACSTA
! --- IF  KCSTA is .TRUE.  -- stations form the list should NOT be treated
! ---                         as global parameters (but all others should)
! --- IF  KCSTA is .FALSE. -- stations form the list SHOULD be treated
! ---                         as global parameters (but all others should not)
! ---    NACSTA -- number of stations in the list
! ---    IACSTA -- shift of the first station in the INTEGER*2 array ISELAR
! ---              where the list of stations starts. NB: the shift is in
! ---              INTEGER*2 words!!
! ---  Variables KCSRS, IACSRC, NACSRC analogously define the list of sources.
! ---  Variables KCAXIS, IACXIS do something related with axis offset
!
      LOGICAL*2   KCSRC,  KCSTA,  KCAXIS, KCTIDE
      INTEGER*4   IACSRC, IACSTA, NACSTA, NACSRC
      INTEGER*2   ISELAR(MAX4_SELAR)
      CHARACTER*8 LSELAR(LMAX4_SELAR)
      EQUIVALENCE (LSELAR,ISELAR)
      INTEGER*2   FIRST_GLBC2_I2, LAST_GLBC2_I2
!
      COMMON / GLBC2 / &
     &              FIRST_GLBC2_I2, &
     &              KCSRC, KCSTA, IACSRC, IACSTA, NACSRC, NACSTA, &
     &              FILL, CARCMP, KCAXIS, KCTIDE, ISELAR, &
     &              FILLER_GLBC2_BT, &
     &              LAST_GLBC2_I2
