!       CONST_INC   03-AUG-1994  --  01-AUG-97 13:55:11
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!       #######################
! |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
! |                        Mathematical constants                              |
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
        REAL*8      PI, PI2, P2I, SECRAD$VRN, ARSRAD$VRN
        PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi
        PARAMETER ( SECRAD$VRN = 86400.D0/PI2 ) ! Number of sec(time) in radian
        PARAMETER ( ARSRAD$VRN = 180.D0*3600.D0/PI  ) ! Number of arcsec in
!                                                     ! radian
! |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
! |                          Pfysical constants                                |
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
        REAL*8      C$VRN, GRAV$VRN, R_GAS$VRN, M_DRY$VRN, AIR_REF$VRN
        PARAMETER (    C$VRN=299792458.D0   ) !  Light velocity ( m/sec )
        PARAMETER ( GRAV$VRN = 6.67259D-11 )  !  Universal gravity constant
!                                             !  ( IERS-92 )
        PARAMETER ( R_GAS$VRN=8.31434D3 )     !  Universall gas constant
        PARAMETER ( M_DRY$VRN=28.9644D0 )     !  Molar massa of the dry air
!
        PARAMETER ( AIR_REF$VRN=7.7604D-5 )   !  Refractivity index for the
!                                             !  dry air
! |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
! |                     Earth's constants                                      |
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!
! ===================== €€…’› ‚€™… ‡…‹ ===============================
!
        REAL*8      DZETA1$VRN, DZETA2$VRN, DZETA3$VRN
!
! ..... ””–…’› €‡‹†… “ƒ‹€ …–…‘‘ DZETA ‚ ‘’‚…‘’‘’‚ ‘
! ..... ‘’€„€’€ IERS 1992 ‘’. 31 ( ‚ €„€€• )
!
        PARAMETER ( DZETA1$VRN= 2306.2181D0   / ARSRAD$VRN )
        PARAMETER ( DZETA2$VRN=    0.30188D0  / ARSRAD$VRN )
        PARAMETER ( DZETA3$VRN=    0.017998D0 / ARSRAD$VRN )
!
        REAL*8      TETA1$VRN, TETA2$VRN, TETA3$VRN
!
! ..... ””–…’› €‡‹†… “ƒ‹€ …–…‘‘ TETA ‚ ‘’‚…‘’‘’‚ ‘
! ..... ‘’€„€’€ IERS 1992 ‘’. 31 ( ‚ €„€€• )
!
        PARAMETER ( TETA1$VRN= 2004.3109D0   / ARSRAD$VRN )
        PARAMETER ( TETA2$VRN=   -0.42665D0  / ARSRAD$VRN )
        PARAMETER ( TETA3$VRN=   -0.041833D0 / ARSRAD$VRN )
!
        REAL*8      ZET1$VRN, ZET2$VRN, ZET3$VRN
!
! ..... ””–…’› €‡‹†… “ƒ‹€ …–…‘‘ ZET ‚ ‘’‚…‘’‘’‚ ‘
! ..... ‘’€„€’€ IERS 1992 ‘’. 31 ( ‚ €„€€• )
!
        PARAMETER ( ZET1$VRN= 2306.2181D0   / ARSRAD$VRN )
        PARAMETER ( ZET2$VRN=    1.09468D0  / ARSRAD$VRN )
        PARAMETER ( ZET3$VRN=    0.018203D0 / ARSRAD$VRN )
!
        REAL*8      EPS0$VRN, EPS1$VRN, EPS2$VRN, EPS3$VRN
!
! ..... ””–…’› €‡‹†… ‘…„…ƒ “ƒ‹€ €‹€ ‹’  ‚€’“
! ..... ‚ ‘’‚…‘’‘’‚ ‘ ‘’€„€’€ IERS 1992 ‘’. 31 ( ‚ €„€€• )
!
        PARAMETER ( EPS0$VRN= 84381.448D0    / ARSRAD$VRN )
        PARAMETER ( EPS1$VRN=   -46.8150D0   / ARSRAD$VRN )
        PARAMETER ( EPS2$VRN=    -0.00059D0  / ARSRAD$VRN )
        PARAMETER ( EPS3$VRN=     0.001813D0 / ARSRAD$VRN )
!
        REAL*8      AOKI0$VRN, AOKI1$VRN, AOKI2$VRN, AOKI3$VRN
!
! ..... ””–…’› €‡‹†… ƒ‚—…‘ƒ ‘…„…ƒ ‚……  ”“‹…
! ..... € ‚ ‘’‚…‘’‘’‚ ‘ ‘’€„€’€ IERS 1992 ‘’. 30 ( ‚ ‘…“„€• )
!
        PARAMETER ( AOKI0$VRN=   67310.54841D0   )
        PARAMETER ( AOKI1$VRN= 8640184.812866D0  )
        PARAMETER ( AOKI2$VRN=       0.093104D0  )
        PARAMETER ( AOKI3$VRN=      -0.0000062D0 )
!
        REAL*8      ACNUT1$VRN, ACNUT2$VRN
        INTEGER*4   IDT_ACNUT$VRN
!
! ..... €‹…€ “’€– ‚  ‚‘•†„… ‚ ‘’‚…‘’‘’‚ ‘
! ..... ‘’€„€’€ IERS 1992 ‘’. 30 ( ‚ €„€€• )
! .....     ACNUT1$VRN  -- —‹…  ‘“‘… ‘…„…‰ „‹ƒ’› ‚‘•„™…ƒ “‡‹€
!                          ‹“‰ ’›
! .....     ACNUT2$VRN  -- —‹…  ‘“‘… “„‚…‰ ‘…„…‰ „‹ƒ’›
!                          ‚‘•„™…ƒ “‡‹€ ‹“‰ ’›
! .....   IDT_ACNUT$VRN -- „€’€, ‘ ’‰ €—€…’‘ “—π’ €‹…‰
!                          “’€–  ……„€– IERS-92 STANDARDS, ‘’. 30
!
        PARAMETER ( ACNUT1$VRN= 0.00264D0  / ARSRAD$VRN ) ! SIN(OMEGA)
        PARAMETER ( ACNUT2$VRN= 0.000063D0 / ARSRAD$VRN ) ! SIN(2*OMEGA)
        PARAMETER ( IDT_ACNUT$VRN= -1039 ) ! 26-FEB-97
!
        REAL*8      GCNUT1$VRN, GCNUT2$VRN
!
! ..... ‡€—… ””–…’‚ „‹ “—π’€ ƒ…„…‡—…‘‰ “’€–
! ..... ‚ ‘’‚…‘’‘’‚ ‘ ‘’€„€’€ IERS 1992 ‘’. 39 ( ‚ €„€€• )
! .....     GCNUT1$VRN  -- —‹…  ‘“‘… ‘…„…‰ €€‹ ‘‹–€
! .....     GCNUT2$VRN  -- —‹…  ‘“‘… “„‚…‰ ‘…„…‰ €€‹ ‘‹–€
!
        PARAMETER ( GCNUT1$VRN= -0.000153D0 / ARSRAD$VRN )
        PARAMETER ( GCNUT2$VRN= -0.000002D0 / ARSRAD$VRN )
!
        REAL*8      YS$VRN, UT1_S$VRN
        PARAMETER ( YS$VRN = 31556925.9747D0 )  !  —‘‹ ‘…“„ ‚ ƒ„“
        PARAMETER ( UT1_S$VRN = 1.D0 + 86400.D0/YS$VRN ) ! —‘‹ ‡‚. ‘…. ‚ ‘…„.
!
        REAL*8  HANG0$VRN, OMPR$VRN, OMPRT$VRN, OMPRTT$VRN, OMNM$VRN, &
     &          OM$VRN
!
! ..... ‚›‚„›… ’‘€’› ‘…‚ƒ ‚€™… ‡…‹ ‚ ‘’‚…‘’‘’‚ ‘
! ..... ‘’€„€’€ IERS
! .....    HANG0$VRN -- ‡‚π‡„›‰ €ƒ“…’€ € •“ J2000.0
! .....     OMNM$VRN -- €‹€ “ƒ‹‚€ ‘‘’ ‚€™… ‡…‹
! .....     OMPR$VRN -- “ƒ‹‚€ ‘‘’ …–…‘‘ ‚  ‚‘•†„… €
!                       •“ J2000.0
! .....       OM$VRN -- ‘…„ “ƒ‹‚€ ‘‘’ ‚€™… ‡…‹ € •“ J2000.0
! .....    OMPRT$VRN -- “ƒ‹‚… “‘…… …–…‘‘ ‚  ‚‘•†„… €
!                       •“ J2000.0
! .....   OMPRTT$VRN -- ’…’ ‡‚„€ ’ …–…‘‘ ‚  ‚‘•†„… €
!                       •“ J2000.0
!
        PARAMETER (  HANG0$VRN= AOKI0$VRN / SECRAD$VRN )
        PARAMETER (   OMPR$VRN= (DZETA1$VRN + ZET1$VRN) / &
     &                          (36525.D0*86400.D0) )
        PARAMETER (  OMPRT$VRN= AOKI2$VRN / SECRAD$VRN / &
     &                          (36525.D0*86400.D0)**2 )
        PARAMETER ( OMPRTT$VRN= AOKI3$VRN / SECRAD$VRN / &
     &                          (36525.D0*86400.D0)**3 )
        PARAMETER (   OMNM$VRN= ( 1.D0 + AOKI1$VRN/ &
     &                          (36525.D0*86400.D0) ) / SECRAD$VRN - &
     &                          OMPR$VRN  )
        PARAMETER (     OM$VRN= OMNM$VRN + OMPR$VRN )
!
! ===================== €€…’› ”ƒ“› ‡…‹ ===============================
!
        REAL*8      REA$VRN, FE$VRN
!
! ..... ‚€’€‹›‰ €„“‘ ( ‚ …’€• )  ‘†€’… ‡…‹ ‚ ‘’‚…‘’‘’‚ ‘
! ..... ‘’€„€’€ IERS
! .....    REA$VRN -- ‚€’€‹›‰ €„“‘ ‡…‹
! .....     FE$VRN -- ‘†€’… ”“ƒ› ‡…‹
!
        PARAMETER ( REA$VRN= 6378136.3D0  )      ! €„“‘ ‡…‹ (IERS-92)
        PARAMETER (  FE$VRN= 1.D0/298.257D0 )  ! ‘†€’… ‡…‹ (IERS-92)
!
! =================== €€…’› ƒ€‚’€–ƒ ‹ ‡…‹ =====================
!
        REAL*8      J2EA$VRN, GE$VRN, ACEQEA$VRN, GRVLAT$VRN, GRVHEI$VRN
!
! .....      GE$VRN -- ƒ…–…’—…‘€ ƒ€‚’€–€ ‘’€’€ ‚ ‘’‚…’‘’‚…
!                      ‘ ‘’€„€’€ IERS 92 ( ‚ ‘‘’…… ‘ )
! .....    J2EA$VRN -- J2-””–…’ €‡‹†… ƒ…O’…–€‹€  ‘”…—…‘
!                      ”“– ‚ ‘’‚…’‘’‚… ‘ ‘’€„€’€ IERS 92
! .....  ACEQEA$VRN -- “‘…… ‘‹› ’†…‘’ € ‚€’… -- Geodetic Refernce
!                      System, 1967 -- B.E.Emerson, G.A.Wilkinson, Celestial
!                      Mechanics, Vol.4(2), 1971, p.128 -149.   αβΰ ­¨ζ ε
!                      142-143  ( ‚ ‘‘’…… ‘ ).
! .....  GRVLAT$VRN -- ‡……… “‘… ‘‹› ’†…‘’ ‘ ’‰ ( ’ ¬ ¦¥ ).
! .....  GRVHEI$VRN -- ‡……… “‘… ‘‹› ’†…‘’ ‘ ‚›‘’‰ €„ ƒ…„
!                      ( Cƒ‹€‘‚€ ‘ ’‘’€’‰  ACEQEA$VRN    REA$VRN )
        PARAMETER (   J2EA$VRN=  0.0010826362D0 )
        PARAMETER (     GE$VRN=  3.986004418D14 )
        PARAMETER ( ACEQEA$VRN=  9.7803184558D0 )
        PARAMETER ( GRVLAT$VRN=  0.001931663D0  )
        PARAMETER ( GRVHEI$VRN= -2.D0*ACEQEA$VRN/REA$VRN )
!
! ============================== —‘‹€ ‹‚€ ===================================
!
        REAL*8      LOVE_H2$VRN, LOVE_L2$VRN, LOVE_H2K$VRN, AMP_K1$VRN, &
     &              LOVE_H_CALC$VRN
!
! ..... —‘‹€ ‹‚€ ‘ƒ‹€‘ ‘’€„€’€ IERS 1992
! .....         LOVE_H2$VRN -- —‘‹ ‹‚€  H2
! .....        LOVE_H2K$VRN -- …–  —‘‹“ ‹‚€  H2  „‹ ‹‚‰ ‚‹›
!                              K2  „…‹… Whar'a
! .....          AMP_K1$VRN   -- €‹’“„€ ‚‹› K1 (Cartwright)
! .....         LOVE_L2$VRN -- —‘‹ ‹‚€  H2
! ..... —‘‹ ‹‚€ H2, ’… ‚ €…’…  CALC
! .....    LOOVE_H_CALC$VRN -- —‘‹ ‹‚€ H €…’€ CALC
!
        PARAMETER (     LOVE_H2$VRN=  0.6090D0  )
        PARAMETER (    LOVE_H2K$VRN= -0.0887D0  )
        PARAMETER (      AMP_K1$VRN=  0.36878D0 )
        PARAMETER (     LOVE_L2$VRN=  0.0852D0  )
        PARAMETER ( LOVE_H_CALC$VRN=  0.60967D0 )
!
! |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
! |              ‘’€’›, ‘‚‡€›… ‘ ‹€…’€ ‘‹…—‰ ‘‘’…›            |
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!
        REAL*8      GS$VRN, AUN$VRN, AUN_JPL$VRN, SE_RAT$VRN, ME_RAT$VRN
!
! .....      GS$VRN -- ƒ…‹–…’—…‘€ ƒ€‚’€–€  ‘’€’€ ‚
!                      ‘’‚…‘’‘’‚ ‘ ‘’€„€’€ IERS ( ‚ ‘‘’…… ‘ )
! .....     AUN$VRN -- €‘’—…‘€ …„–€ ‚ ‘’‚…‘’‘’‚ ‘ ‘’€„€’€
!                      IERS ( ‚ …’€• )
! ..... AUN_JPL$VRN -- €‘’—…‘€ …„–€, ’€ ‚ ”……„€•
!                      DE200/LE200 ( ‚ …’€• )
! .....  SE_RAT$VRN -- €‘‘€ ‘‹–€/€‘‘€ ‡…‹ ‚ ‘’‚…‘’‘’‚ ‘ ‘’€„€’€
!                      IERS
! .....  ME_RAT$VRN -- €‘‘€ ‹“›/€‘‘€ ‡…‹ ‚ ‘’‚…‘’‘’‚ ‘ ‘’€„€’€
!                      IERS
!
        PARAMETER (      GS$VRN = 1.32712440D20   )
        PARAMETER (     AUN$VRN = 149597870610.D0 )
        PARAMETER ( AUN_JPL$VRN = 149597870660.D0 )
        PARAMETER ( SE_RAT$VRN = 332946.045D0     )
        PARAMETER ( ME_RAT$VRN = 0.012300034D0    )
!
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
