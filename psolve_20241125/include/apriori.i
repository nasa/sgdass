!
! >>>>> INCLUDE-BLOCK with descriptions of data structures used by apriori
!                     and dbedit/
!
!    dbedit.i
!
!    Who   When        What
!    pet   2000.06.23  Created using source code of apriori
!    pet   2000.09.07  Added table of EOP_LCODE
!
!
!
! --- Table of Lcodes of mathematical and astronomical constants
!     ==========================================================
!
      INTEGER*2    NLC_MATH
      PARAMETER  ( NLC_MATH = 18 )
      CHARACTER    LC_NAME_MATH(NLC_MATH)*8, &
     &             LC_DESC_MATH(NLC_MATH)*32
      INTEGER*2    TOC_MATH(NLC_MATH), DIM_MATH(3,NLC_MATH), TYP_MATH(NLC_MATH)
      INTEGER*2    AP_DEF_1$, AP_DEF_2$, EOP_DEF1$
      DATA        (   LC_NAME_MATH(AP_DEF_1$), &
     &                LC_DESC_MATH(AP_DEF_1$), &
     &                TOC_MATH(AP_DEF_1$), &
     &              ( DIM_MATH(AP_DEF_2$,AP_DEF_1$), AP_DEF_2$=1,3 ), &
     &                TYP_MATH(AP_DEF_1$), &
     &                AP_DEF_1$=1,NLC_MATH ) &
     &            / &
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
!    |  Lcode     |  Lcode description                |toc|dim|dim|dim|typ ! N
!    |  name      |                                   |   | 1 | 2 | 3 |    !
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
     &  'ETD DATA', 'Earth tide module data (la.,h,l)',  1,  3,  1,  1, 1, &  !  1
     &  'PRE DATA', 'Precession constant (asec/cent).',  1,  1,  1,  1, 1, &  !  2
     &  'REL DATA', 'Relativity mod data (gamma).    ',  1,  1,  1,  1, 1, &  !  3
     &  'PI      ', 'Pi - to 17 digits accuracy.     ',  1,  1,  1,  1, 1, &  !  4
     &  'HALF PI ', 'Half pi - to 17 digits accuracy.',  1,  1,  1,  1, 1, &  !  5
     &  'RAD/DEG ', 'Number of radians per degree.   ',  1,  1,  1,  1, 1, &  !  6
     &  'VLIGHT  ', 'The speed of light (m/sec).     ',  1,  1,  1,  1, 1, &  !  7
     &  'GAUSS   ', "Gauss's constant (rad).         ",  1,  1,  1,  1, 1, &  !  8
     &  'ACCELGRV', 'Accel grav at erth surface m/s^2',  1,  1,  1,  1, 1, &  !  9
     &  'GMSUN   ', 'GM of the Sun (m^3/sec^2).      ',  1,  1,  1,  1, 1, &  ! 10
     &  'GMEARTH ', 'GM of the Earth (m^3/sec^2).    ',  1,  1,  1,  1, 1, &  ! 11
     &  'GMMOON  ', 'GM of the Moon (m^3/sec^2).     ',  1,  1,  1,  1, 1, &  ! 12
     &  'TSEC/AU ', 'Number of seconds per A.U.      ',  1,  1,  1,  1, 1, &  ! 13
     &  'EARTHRAD', "Earth's equatorial radius (m).  ",  1,  1,  1,  1, 1, &  ! 14
     &  'EMS/MMS ', 'The Earth-Moon mass ratio.      ',  1,  1,  1,  1, 1, &  ! 15
     &  'U-GRV-CN', 'Universal grav con m^3/kg*s^3.  ',  1,  1,  1,  1, 1, &  ! 16
     &  'E-FLAT  ', 'Earth flattening con (unitless).',  1,  1,  1,  1, 1, &  ! 17
     &  'TECTMODL', 'Default tectonic plate modelname',  1, 40,  1,  1, 3 &   ! 18
     &            /
!
!
! --- Table of Lcodes with apriori EOP used by Calc/Solve
!
      INTEGER*2    NEOP_LCODE
      PARAMETER  ( NEOP_LCODE = 18 )
      CHARACTER  EOP_LCODE(NEOP_LCODE)*8
      DATA ( EOP_LCODE(EOP_DEF1$), EOP_DEF1$=1,NEOP_LCODE ) &
     &     / &
     &       'FUT1 INF', &    !   1
     &       'FUT1 PTS', &    !   2
     &       'FUT1TEXT', &    !   3
     &       'FWOB INF', &    !   4
     &       'FWOBX&YT', &    !   5
     &       'FWOBTEXT', &    !   6
     &       'PUT1 INF', &    !   7
     &       'PUT1 PTS', &    !   8
     &       'PUT1TEXT', &    !   9
     &       'PWOB INF', &    !  10
     &       'PWOBX&YT', &    !  11
     &       'PWOBTEXT', &    !  12
     &       'EUT1 INF', &    !  13
     &       'EUT1 PTS', &    !  14
     &       'EUT1TEXT', &    !  15
     &       'EWOB INF', &    !  16
     &       'EWOBX&YT', &    !  17
     &       'EWOBTEXT' &     !  18
     &     /
!
!
! <<<<< end of  INCLUDE-BLOCK  apriori.i
