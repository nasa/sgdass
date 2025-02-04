      FUNCTION   SUR_CHECK_VIS ( SUR, IND_STA, CUR_TYP, IND_SRC, AZ, EL, HA, &
     &                           IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_CHECK_VIS 
! *                                                                      *
! *  ### 19-NOV-2005  SUR_CHECK_VIS  v1.7 (c) L. Petrov 26-JUN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      INTEGER*4  IND_STA
      LOGICAL*4  SUR_CHECK_VIS 
      REAL*8     AZ, EL, HA, DEL
      INTEGER*4  CUR_TYP, IND_SRC, IUER
!      
      REAL*8     A, B, AZ_USED
      REAL*8       EL__MARGIN
      PARAMETER  ( EL__MARGIN = 0.25D0*DEG__TO__RAD  )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18
      INTEGER*4    NUMHOR_VERAMZSW, NUMHOR_VERAIRKI, &
     &             NUMHOR_VERAOGSW, NUMHOR_VERAISGK, &
     &             NUMHOR_KASHIMA,  NUMHOR_WARK12M, &
     &             NUMHOR_HARTRAO

      INTEGER*4  NUMHOR_BR
      INTEGER*4  NUMHOR_FD
      INTEGER*4  NUMHOR_HN
      INTEGER*4  NUMHOR_KP
      INTEGER*4  NUMHOR_LA
      INTEGER*4  NUMHOR_MK
      INTEGER*4  NUMHOR_NL
      INTEGER*4  NUMHOR_OV
      INTEGER*4  NUMHOR_PT
      INTEGER*4  NUMHOR_SC
!
      PARAMETER  ( NUMHOR_VERAMZSW =  2 )
      PARAMETER  ( NUMHOR_VERAIRKI =  9 )
      PARAMETER  ( NUMHOR_VERAOGSW =  9 )
      PARAMETER  ( NUMHOR_VERAISGK =  8 )
      PARAMETER  ( NUMHOR_KASHIMA  =  2 )
      PARAMETER  ( NUMHOR_WARK12M  = 72 )
      PARAMETER  ( NUMHOR_HARTRAO  = 50 )
      PARAMETER  ( NUMHOR_BR =  38 )
      PARAMETER  ( NUMHOR_FD =  50 )
      PARAMETER  ( NUMHOR_HN =  52 )
      PARAMETER  ( NUMHOR_KP =  33 )
      PARAMETER  ( NUMHOR_LA =  17 )
      PARAMETER  ( NUMHOR_MK =  45 )
      PARAMETER  ( NUMHOR_NL =  25 )
      PARAMETER  ( NUMHOR_OV =  54 )
      PARAMETER  ( NUMHOR_PT =  24 )
      PARAMETER  ( NUMHOR_SC =  49 )
!
      REAL*8     AZELHOR_VERAMZSW(3,NUMHOR_VERAMZSW)
      REAL*8     AZELHOR_VERAIRKI(3,NUMHOR_VERAIRKI)
      REAL*8     AZELHOR_VERAOGSW(3,NUMHOR_VERAOGSW)
      REAL*8     AZELHOR_VERAISGK(3,NUMHOR_VERAISGK)
      REAL*8     AZELHOR_KASHIMA(3,NUMHOR_KASHIMA)
      REAL*8     AZELHOR_WARK12M(3,NUMHOR_WARK12M)
      REAL*8     AZELHOR_HARTRAO(3,NUMHOR_HARTRAO)
      REAL*8     AZELHOR_BR(3,NUMHOR_BR)
      REAL*8     AZELHOR_FD(3,NUMHOR_FD)
      REAL*8     AZELHOR_HN(3,NUMHOR_HN)
      REAL*8     AZELHOR_KP(3,NUMHOR_KP)
      REAL*8     AZELHOR_LA(3,NUMHOR_LA)
      REAL*8     AZELHOR_MK(3,NUMHOR_MK)
      REAL*8     AZELHOR_NL(3,NUMHOR_NL)
      REAL*8     AZELHOR_OV(3,NUMHOR_OV)
      REAL*8     AZELHOR_PT(3,NUMHOR_PT)
      REAL*8     AZELHOR_SC(3,NUMHOR_SC)

      CHARACTER  STA_NAM*8
!
      DATA       AZELHOR_VERAMZSW &
     &         /   0.D0, 180.D0, 2.5D0, &
     &           180.D0, 360.D0, 2.5D0  &
     &         /
!
      DATA       AZELHOR_VERAIRKI &
     &         /   0.D0,  10.D0,  5.0D0, &
     &            10.D0,  30.D0,  8.0D0, &
     &            30.D0,  60.D0, 12.0D0, &
     &            60.D0,  70.D0, 10.0D0, &
     &            70.D0, 130.D0,  2.0D0, &
     &           130.D0, 150.D0,  5.0D0, &
     &           150.D0, 200.D0,  7.0D0, &
     &           200.D0, 340.D0,  2.0D0, &
     &           340.D0, 360.D0,  5.0D0  &
     &         /
!
      DATA       AZELHOR_VERAOGSW &
     &         /   0.D0,  10.D0,  10.0D0, &
     &            10.D0,  30.D0,  15.0D0, &
     &            30.D0,  70.D0,  18.0D0, &
     &            70.D0, 100.D0,  15.0D0, &
     &           100.D0, 120.D0,  18.0D0, &
     &           120.D0, 160.D0,  15.0D0, &
     &           160.D0, 210.D0,  18.0D0, &
     &           210.D0, 300.D0,  12.0D0, &
     &           300.D0, 360.D0,  10.0D0  &
     &         /
!
      DATA       AZELHOR_VERAISGK &
     &         /   0.D0,  10.D0,  15.0D0, &
     &            10.D0,  70.D0,  20.0D0, &
     &            70.D0,  90.D0,  15.0D0, &
     &            90.D0,  95.D0,  10.0D0, &
     &            95.D0, 170.D0,   8.0D0, &
     &           170.D0, 270.D0,   5.0D0, &
     &           270.D0, 330.D0,   8.0D0, &
     &           330.D0, 360.D0,  10.0D0  &
     &         /
      DATA       AZELHOR_KASHIMA &
     &         /   0.D0, 180.D0, 7.5D0, &
     &           180.D0, 360.D0, 7.5D0  &
     &         /
!
      DATA       AZELHOR_BR &
     &         / &
     &            0.D0,   5.D0,  2.0D0, & !   1
     &            5.D0,  10.D0,  2.0D0, & !   2
     &           10.D0,  15.D0,  3.0D0, & !   3
     &           15.D0,  25.D0,  2.0D0, & !   4
     &           25.D0,  30.D0,  2.0D0, & !   5
     &           30.D0,  40.D0,  3.0D0, & !   6
     &           40.D0,  45.D0,  3.0D0, & !   7
     &           45.D0,  70.D0,  4.0D0, & !   8
     &           70.D0,  75.D0,  4.0D0, & !   9
     &           75.D0, 120.D0,  5.0D0, & !  10
     &          120.D0, 125.D0,  5.0D0, & !  11
     &          125.D0, 130.D0,  4.0D0, & !  12
     &          130.D0, 135.D0,  4.0D0, & !  13
     &          135.D0, 155.D0,  3.0D0, & !  14
     &          155.D0, 160.D0,  3.0D0, & !  15
     &          160.D0, 185.D0,  2.0D0, & !  16
     &          185.D0, 190.D0,  2.0D0, & !  17
     &          190.D0, 195.D0,  3.0D0, & !  18
     &          195.D0, 220.D0,  4.0D0, & !  19
     &          220.D0, 225.D0,  4.0D0, & !  20
     &          225.D0, 235.D0,  3.0D0, & !  21
     &          235.D0, 240.D0,  3.0D0, & !  22
     &          240.D0, 245.D0,  4.0D0, & !  23
     &          245.D0, 250.D0,  4.0D0, & !  24
     &          250.D0, 255.D0,  5.0D0, & !  25
     &          255.D0, 265.D0,  6.0D0, & !  26
     &          265.D0, 270.D0,  6.0D0, & !  27
     &          270.D0, 275.D0,  5.0D0, & !  28
     &          275.D0, 300.D0,  6.0D0, & !  29
     &          300.D0, 305.D0,  6.0D0, & !  30
     &          305.D0, 310.D0,  5.0D0, & !  31
     &          310.D0, 315.D0,  6.0D0, & !  32
     &          315.D0, 330.D0,  5.0D0, & !  33
     &          330.D0, 335.D0,  5.0D0, & !  34
     &          335.D0, 340.D0,  4.0D0, & !  35
     &          340.D0, 345.D0,  4.0D0, & !  36
     &          345.D0, 350.D0,  3.0D0, & !  37
     &          350.D0, 360.D0,  2.0D0  & !  38
     &         /
      DATA       AZELHOR_FD &
     &         / &
     &            0.D0,   5.D0,  5.0D0, & !   1
     &            5.D0,  10.D0,  4.0D0, & !   2
     &           10.D0,  15.D0,  5.0D0, & !   3
     &           15.D0,  20.D0,  5.0D0, & !   4
     &           20.D0,  30.D0,  3.0D0, & !   5
     &           30.D0,  35.D0,  3.0D0, & !   6
     &           35.D0,  40.D0,  2.0D0, & !   7
     &           40.D0,  45.D0,  3.0D0, & !   8
     &           45.D0,  50.D0,  2.0D0, & !   9
     &           50.D0,  55.D0,  2.0D0, & !  10
     &           55.D0,  60.D0,  3.0D0, & !  11
     &           60.D0,  65.D0,  4.0D0, & !  12
     &           65.D0,  70.D0,  7.0D0, & !  13
     &           70.D0,  75.D0,  5.0D0, & !  14
     &           75.D0,  80.D0,  4.0D0, & !  15
     &           80.D0,  85.D0,  4.0D0, & !  16
     &           85.D0,  90.D0,  5.0D0, & !  17
     &           90.D0,  95.D0,  6.0D0, & !  18
     &           95.D0, 100.D0,  6.0D0, & !  19
     &          100.D0, 105.D0,  5.0D0, & !  20
     &          105.D0, 110.D0,  4.0D0, & !  21
     &          110.D0, 115.D0,  3.0D0, & !  22
     &          115.D0, 150.D0,  2.0D0, & !  23
     &          150.D0, 155.D0,  2.0D0, & !  24
     &          155.D0, 160.D0,  3.0D0, & !  25
     &          160.D0, 220.D0,  2.0D0, & !  26
     &          220.D0, 225.D0,  2.0D0, & !  27
     &          225.D0, 230.D0,  4.0D0, & !  28
     &          230.D0, 240.D0,  2.0D0, & !  29
     &          240.D0, 245.D0,  2.0D0, & !  30
     &          245.D0, 250.D0,  3.0D0, & !  31
     &          250.D0, 255.D0,  3.0D0, & !  32
     &          255.D0, 260.D0,  4.0D0, & !  33
     &          260.D0, 265.D0,  5.0D0, & !  34
     &          265.D0, 270.D0,  5.0D0, & !  35
     &          270.D0, 275.D0,  4.0D0, & !  36
     &          275.D0, 280.D0,  4.0D0, & !  37
     &          280.D0, 285.D0,  3.0D0, & !  38
     &          285.D0, 290.D0,  3.0D0, & !  39
     &          290.D0, 295.D0,  2.0D0, & !  40
     &          295.D0, 300.D0,  2.0D0, & !  41
     &          300.D0, 305.D0,  3.0D0, & !  42
     &          305.D0, 310.D0,  4.0D0, & !  43
     &          310.D0, 315.D0,  5.0D0, & !  44
     &          315.D0, 325.D0,  4.0D0, & !  45
     &          325.D0, 330.D0,  4.0D0, & !  46
     &          330.D0, 335.D0,  5.0D0, & !  47
     &          335.D0, 340.D0,  6.0D0, & !  48
     &          340.D0, 345.D0,  6.0D0, & !  49
     &          345.D0, 360.D0,  5.0D0  & !  50
     &         /
      DATA       AZELHOR_HN &
     &         / &
     &            0.D0,   5.D0,  6.0D0, & !   1
     &            5.D0,  30.D0,  6.0D0, & !   2
     &           30.D0,  35.D0,  6.0D0, & !   3
     &           35.D0,  40.D0,  4.0D0, & !   4
     &           40.D0,  45.D0,  5.0D0, & !   5
     &           45.D0,  65.D0,  4.0D0, & !   6
     &           65.D0,  70.D0,  4.0D0, & !   7
     &           70.D0,  80.D0,  5.0D0, & !   8
     &           80.D0,  85.D0,  5.0D0, & !   9
     &           85.D0,  90.D0,  4.0D0, & !  10
     &           90.D0,  95.D0,  5.0D0, & !  11
     &           95.D0, 100.D0,  4.0D0, & !  12
     &          100.D0, 105.D0,  4.0D0, & !  13
     &          105.D0, 110.D0,  5.0D0, & !  14
     &          110.D0, 115.D0,  3.0D0, & !  15
     &          115.D0, 120.D0,  4.0D0, & !  16
     &          120.D0, 125.D0,  4.0D0, & !  17
     &          125.D0, 130.D0,  5.0D0, & !  18
     &          130.D0, 135.D0,  4.0D0, & !  19
     &          135.D0, 140.D0,  6.0D0, & !  20
     &          140.D0, 145.D0,  5.0D0, & !  21
     &          145.D0, 150.D0,  7.0D0, & !  22
     &          150.D0, 155.D0,  7.0D0, & !  23
     &          155.D0, 160.D0,  5.0D0, & !  24
     &          160.D0, 165.D0,  3.0D0, & !  25
     &          165.D0, 170.D0,  5.0D0, & !  26
     &          170.D0, 190.D0,  4.0D0, & !  27
     &          190.D0, 195.D0,  4.0D0, & !  28
     &          195.D0, 200.D0,  2.0D0, & !  29
     &          200.D0, 205.D0,  5.0D0, & !  30
     &          205.D0, 210.D0,  5.0D0, & !  31
     &          210.D0, 220.D0,  6.0D0, & !  32
     &          220.D0, 225.D0,  6.0D0, & !  33
     &          225.D0, 230.D0,  5.0D0, & !  34
     &          230.D0, 235.D0,  6.0D0, & !  35
     &          235.D0, 240.D0,  4.0D0, & !  36
     &          240.D0, 245.D0,  5.0D0, & !  37
     &          245.D0, 250.D0,  5.0D0, & !  38
     &          250.D0, 255.D0,  4.0D0, & !  39
     &          255.D0, 270.D0,  5.0D0, & !  40
     &          270.D0, 275.D0,  5.0D0, & !  41
     &          275.D0, 290.D0,  4.0D0, & !  42
     &          290.D0, 295.D0,  4.0D0, & !  43
     &          295.D0, 315.D0,  5.0D0, & !  44
     &          315.D0, 320.D0,  5.0D0, & !  45
     &          320.D0, 325.D0,  6.0D0, & !  46
     &          325.D0, 330.D0,  5.0D0, & !  47
     &          330.D0, 335.D0,  5.0D0, & !  48
     &          335.D0, 345.D0,  6.0D0, & !  49
     &          345.D0, 350.D0,  6.0D0, & !  50
     &          350.D0, 355.D0,  5.0D0, & !  51
     &          355.D0, 360.D0,  5.0D0  & !  52
     &         /
      DATA       AZELHOR_KP &
     &         / &
     &            0.D0,   5.D0,  2.0D0, & !   1
     &            5.D0,  45.D0,  2.0D0, & !   2
     &           45.D0,  50.D0,  2.0D0, & !   3
     &           50.D0,  55.D0,  5.0D0, & !   4
     &           55.D0,  60.D0,  6.0D0, & !   5
     &           60.D0,  65.D0,  7.0D0, & !   6
     &           65.D0,  70.D0,  7.0D0, & !   7
     &           70.D0,  75.D0,  8.0D0, & !   8
     &           75.D0,  80.D0,  8.0D0, & !   9
     &           80.D0,  85.D0,  9.0D0, & !  10
     &           85.D0,  90.D0,  6.0D0, & !  11
     &           90.D0,  95.D0,  3.0D0, & !  12
     &           95.D0, 105.D0,  2.0D0, & !  13
     &          105.D0, 110.D0,  2.0D0, & !  14
     &          110.D0, 115.D0,  3.0D0, & !  15
     &          115.D0, 120.D0,  3.0D0, & !  16
     &          120.D0, 150.D0,  2.0D0, & !  17
     &          150.D0, 155.D0,  2.0D0, & !  18
     &          155.D0, 165.D0,  3.0D0, & !  19
     &          165.D0, 170.D0,  3.0D0, & !  20
     &          170.D0, 180.D0,  2.0D0, & !  21
     &          180.D0, 185.D0,  2.0D0, & !  22
     &          185.D0, 215.D0,  3.0D0, & !  23
     &          215.D0, 220.D0,  3.0D0, & !  24
     &          220.D0, 225.D0,  4.0D0, & !  25
     &          225.D0, 230.D0,  4.0D0, & !  26
     &          230.D0, 235.D0,  5.0D0, & !  27
     &          235.D0, 240.D0,  5.0D0, & !  28
     &          240.D0, 255.D0,  4.0D0, & !  29
     &          255.D0, 260.D0,  4.0D0, & !  30
     &          260.D0, 265.D0,  3.0D0, & !  31
     &          265.D0, 270.D0,  3.0D0, & !  32
     &          270.D0, 360.D0,  2.0D0  & !  33
     &         /
      DATA       AZELHOR_LA &
     &         / &
     &            0.D0,   5.D0,  2.0D0, & !   1
     &            5.D0,  75.D0,  2.0D0, & !   2
     &           75.D0,  80.D0,  2.0D0, & !   3
     &           80.D0,  85.D0,  3.0D0, & !   4
     &           85.D0, 130.D0,  2.0D0, & !   5
     &          130.D0, 135.D0,  2.0D0, & !   6
     &          135.D0, 145.D0,  3.0D0, & !   7
     &          145.D0, 150.D0,  3.0D0, & !   8
     &          150.D0, 250.D0,  2.0D0, & !   9
     &          250.D0, 255.D0,  2.0D0, & !  10
     &          255.D0, 300.D0,  3.0D0, & !  11
     &          300.D0, 305.D0,  3.0D0, & !  12
     &          305.D0, 315.D0,  4.0D0, & !  13
     &          315.D0, 320.D0,  4.0D0, & !  14
     &          320.D0, 340.D0,  3.0D0, & !  15
     &          340.D0, 345.D0,  3.0D0, & !  16
     &          345.D0, 360.D0,  2.0D0  & !  17
     &         /
      DATA       AZELHOR_MK &
     &         / &
     &            0.D0,   5.D0,  5.0D0, & !   1
     &            5.D0,  10.D0,  4.0D0, & !   2
     &           10.D0,  15.D0,  3.0D0, & !   3
     &           15.D0,  20.D0,  3.0D0, & !   4
     &           20.D0, 120.D0,  2.0D0, & !   5
     &          120.D0, 125.D0,  2.0D0, & !   6
     &          125.D0, 130.D0,  4.0D0, & !   7
     &          130.D0, 135.D0,  5.0D0, & !   8
     &          135.D0, 140.D0,  5.0D0, & !   9
     &          140.D0, 145.D0,  4.0D0, & !  10
     &          145.D0, 150.D0,  4.0D0, & !  11
     &          150.D0, 155.D0,  6.0D0, & !  12
     &          155.D0, 160.D0,  8.0D0, & !  13
     &          160.D0, 165.D0,  8.0D0, & !  14
     &          165.D0, 170.D0, 11.0D0, & !  15
     &          170.D0, 175.D0, 12.0D0, & !  16
     &          175.D0, 185.D0, 13.0D0, & !  17
     &          185.D0, 190.D0, 13.0D0, & !  18
     &          190.D0, 195.D0, 11.0D0, & !  19
     &          195.D0, 200.D0, 11.0D0, & !  20
     &          200.D0, 205.D0,  9.0D0, & !  21
     &          205.D0, 210.D0,  7.0D0, & !  22
     &          210.D0, 215.D0,  5.0D0, & !  23
     &          215.D0, 220.D0,  3.0D0, & !  24
     &          220.D0, 255.D0,  2.0D0, & !  25
     &          255.D0, 260.D0,  2.0D0, & !  26
     &          260.D0, 270.D0,  3.0D0, & !  27
     &          270.D0, 275.D0,  3.0D0, & !  28
     &          275.D0, 280.D0,  5.0D0, & !  29
     &          280.D0, 285.D0,  6.0D0, & !  30
     &          285.D0, 290.D0,  8.0D0, & !  31
     &          290.D0, 295.D0, 10.0D0, & !  32
     &          295.D0, 300.D0, 12.0D0, & !  33
     &          300.D0, 305.D0, 14.0D0, & !  34
     &          305.D0, 310.D0, 12.0D0, & !  35
     &          310.D0, 315.D0, 11.0D0, & !  36
     &          315.D0, 320.D0,  9.0D0, & !  37
     &          320.D0, 325.D0, 10.0D0, & !  38
     &          325.D0, 330.D0, 11.0D0, & !  39
     &          330.D0, 335.D0, 10.0D0, & !  40
     &          335.D0, 340.D0, 12.0D0, & !  41
     &          340.D0, 345.D0, 14.0D0, & !  42
     &          345.D0, 350.D0, 12.0D0, & !  43
     &          350.D0, 355.D0,  9.0D0, & !  44
     &          355.D0, 360.D0,  7.0D0  & !  45
     &         /
      DATA       AZELHOR_NL &
     &         / &
     &            0.D0,   5.D0,  2.0D0, & !   1
     &            5.D0,  75.D0,  2.0D0, & !   2
     &           75.D0,  80.D0,  2.0D0, & !   3
     &           80.D0,  85.D0,  3.0D0, & !   4
     &           85.D0, 100.D0,  6.0D0, & !   5
     &          100.D0, 105.D0,  6.0D0, & !   6
     &          105.D0, 110.D0,  8.0D0, & !   7
     &          110.D0, 115.D0,  7.0D0, & !   8
     &          115.D0, 120.D0,  7.0D0, & !   9
     &          120.D0, 125.D0,  6.0D0, & !  10
     &          125.D0, 130.D0,  7.0D0, & !  11
     &          130.D0, 135.D0,  7.0D0, & !  12
     &          135.D0, 140.D0,  6.0D0, & !  13
     &          140.D0, 145.D0,  6.0D0, & !  14
     &          145.D0, 150.D0,  7.0D0, & !  15
     &          150.D0, 155.D0,  7.0D0, & !  16
     &          155.D0, 160.D0,  6.0D0, & !  17
     &          160.D0, 165.D0,  5.0D0, & !  18
     &          165.D0, 170.D0,  4.0D0, & !  19
     &          170.D0, 190.D0,  3.0D0, & !  20
     &          190.D0, 195.D0,  3.0D0, & !  21
     &          195.D0, 200.D0,  2.0D0, & !  22
     &          200.D0, 220.D0,  3.0D0, & !  23
     &          220.D0, 225.D0,  3.0D0, & !  24
     &          225.D0, 360.D0,  2.0D0  & !  25
     &         /
      DATA       AZELHOR_OV &
     &         / &
     &            0.D0,   5.D0,  3.0D0, & !   1
     &            5.D0,  10.D0,  5.0D0, & !   2
     &           10.D0,  15.D0,  5.0D0, & !   3
     &           15.D0,  20.D0,  7.0D0, & !   4
     &           20.D0,  25.D0,  9.0D0, & !   5
     &           25.D0,  30.D0, 10.0D0, & !   6
     &           30.D0,  35.D0, 12.0D0, & !   7
     &           35.D0,  40.D0, 13.0D0, & !   8
     &           40.D0,  45.D0, 13.0D0, & !   9
     &           45.D0,  50.D0, 14.0D0, & !  10
     &           50.D0,  60.D0, 15.0D0, & !  11
     &           60.D0,  65.D0, 15.0D0, & !  12
     &           65.D0,  70.D0, 13.0D0, & !  13
     &           70.D0,  75.D0, 12.0D0, & !  14
     &           75.D0,  80.D0, 11.0D0, & !  15
     &           80.D0,  85.D0, 10.0D0, & !  16
     &           85.D0,  90.D0,  9.0D0, & !  17
     &           90.D0,  95.D0,  8.0D0, & !  18
     &           95.D0, 100.D0,  7.0D0, & !  19
     &          100.D0, 105.D0,  6.0D0, & !  20
     &          105.D0, 110.D0,  6.0D0, & !  21
     &          110.D0, 115.D0,  5.0D0, & !  22
     &          115.D0, 120.D0,  4.0D0, & !  23
     &          120.D0, 125.D0,  3.0D0, & !  24
     &          125.D0, 130.D0,  3.0D0, & !  25
     &          130.D0, 145.D0,  4.0D0, & !  26
     &          145.D0, 150.D0,  4.0D0, & !  27
     &          150.D0, 155.D0,  3.0D0, & !  28
     &          155.D0, 175.D0,  2.0D0, & !  29
     &          175.D0, 180.D0,  2.0D0, & !  30
     &          180.D0, 185.D0,  3.0D0, & !  31
     &          185.D0, 190.D0,  3.0D0, & !  32
     &          190.D0, 195.D0,  4.0D0, & !  33
     &          195.D0, 200.D0,  5.0D0, & !  34
     &          200.D0, 205.D0,  5.0D0, & !  35
     &          205.D0, 210.D0,  6.0D0, & !  36
     &          210.D0, 230.D0,  7.0D0, & !  37
     &          230.D0, 235.D0,  7.0D0, & !  38
     &          235.D0, 240.D0,  6.0D0, & !  39
     &          240.D0, 245.D0,  7.0D0, & !  40
     &          245.D0, 250.D0,  8.0D0, & !  41
     &          250.D0, 260.D0,  9.0D0, & !  42
     &          260.D0, 265.D0,  9.0D0, & !  43
     &          265.D0, 270.D0,  8.0D0, & !  44
     &          270.D0, 280.D0,  7.0D0, & !  45
     &          280.D0, 285.D0,  7.0D0, & !  46
     &          285.D0, 290.D0,  5.0D0, & !  47
     &          290.D0, 295.D0,  4.0D0, & !  48
     &          295.D0, 300.D0,  4.0D0, & !  49
     &          300.D0, 305.D0,  3.0D0, & !  50
     &          305.D0, 310.D0,  3.0D0, & !  51
     &          310.D0, 350.D0,  2.0D0, & !  52
     &          350.D0, 355.D0,  2.0D0, & !  53
     &          355.D0, 360.D0,  3.0D0  & !  54
     &         /
      DATA       AZELHOR_PT &
     &         / &
     &            0.D0,   5.D0,  2.0D0, & !   1
     &            5.D0,  60.D0,  2.0D0, & !   2
     &           60.D0,  65.D0,  2.0D0, & !   3
     &           65.D0,  70.D0,  3.0D0, & !   4
     &           70.D0,  75.D0,  3.0D0, & !   5
     &           75.D0,  80.D0,  2.0D0, & !   6
     &           80.D0,  85.D0,  3.0D0, & !   7
     &           85.D0, 165.D0,  2.0D0, & !   8
     &          165.D0, 170.D0,  2.0D0, & !   9
     &          170.D0, 180.D0,  3.0D0, & !  10
     &          180.D0, 185.D0,  3.0D0, & !  11
     &          185.D0, 190.D0,  4.0D0, & !  12
     &          190.D0, 195.D0,  4.0D0, & !  13
     &          195.D0, 200.D0,  3.0D0, & !  14
     &          200.D0, 240.D0,  4.0D0, & !  15
     &          240.D0, 245.D0,  4.0D0, & !  16
     &          245.D0, 250.D0,  3.0D0, & !  17
     &          250.D0, 255.D0,  4.0D0, & !  18
     &          255.D0, 265.D0,  3.0D0, & !  19
     &          265.D0, 270.D0,  3.0D0, & !  20
     &          270.D0, 275.D0,  4.0D0, & !  21
     &          275.D0, 280.D0,  3.0D0, & !  22
     &          280.D0, 285.D0,  3.0D0, & !  23
     &          285.D0, 360.D0,  2.0D0  & !  24
     &         /
      DATA       AZELHOR_SC &
     &         / &
     &            0.D0,   5.D0,  2.0D0, & !   1
     &            5.D0,  10.D0,  2.0D0, & !   2
     &           10.D0,  20.D0,  3.0D0, & !   3
     &           20.D0,  25.D0,  3.0D0, & !   4
     &           25.D0,  40.D0,  2.0D0, & !   5
     &           40.D0,  45.D0,  2.0D0, & !   6
     &           45.D0,  50.D0,  3.0D0, & !   7
     &           50.D0,  55.D0,  3.0D0, & !   8
     &           55.D0,  60.D0,  4.0D0, & !   9
     &           60.D0,  65.D0,  6.0D0, & !  10
     &           65.D0,  70.D0,  6.0D0, & !  11
     &           70.D0,  75.D0,  8.0D0, & !  12
     &           75.D0,  80.D0,  9.0D0, & !  13
     &           80.D0,  85.D0,  9.0D0, & !  14
     &           85.D0,  95.D0,  8.0D0, & !  15
     &           95.D0, 100.D0,  8.0D0, & !  16
     &          100.D0, 105.D0,  9.0D0, & !  17
     &          105.D0, 110.D0, 10.0D0, & !  18
     &          110.D0, 115.D0, 12.0D0, & !  19
     &          115.D0, 120.D0, 14.0D0, & !  20
     &          120.D0, 125.D0, 16.0D0, & !  21
     &          125.D0, 130.D0, 16.0D0, & !  22
     &          130.D0, 135.D0, 15.0D0, & !  23
     &          135.D0, 140.D0, 13.0D0, & !  24
     &          140.D0, 145.D0, 13.0D0, & !  25
     &          145.D0, 150.D0, 12.0D0, & !  26
     &          150.D0, 155.D0, 11.0D0, & !  27
     &          155.D0, 160.D0, 11.0D0, & !  28
     &          160.D0, 165.D0, 10.0D0, & !  29
     &          165.D0, 175.D0,  9.0D0, & !  30
     &          175.D0, 180.D0,  9.0D0, & !  31
     &          180.D0, 185.D0, 11.0D0, & !  32
     &          185.D0, 190.D0, 13.0D0, & !  33
     &          190.D0, 200.D0, 14.0D0, & !  34
     &          200.D0, 205.D0, 14.0D0, & !  35
     &          205.D0, 210.D0, 15.0D0, & !  36
     &          210.D0, 215.D0, 13.0D0, & !  37
     &          215.D0, 220.D0, 12.0D0, & !  38
     &          220.D0, 230.D0, 10.0D0, & !  39
     &          230.D0, 235.D0, 10.0D0, & !  40
     &          235.D0, 240.D0,  9.0D0, & !  41
     &          240.D0, 245.D0,  8.0D0, & !  42
     &          245.D0, 250.D0,  8.0D0, & !  43
     &          250.D0, 260.D0,  7.0D0, & !  44
     &          260.D0, 265.D0,  7.0D0, & !  45
     &          265.D0, 270.D0,  6.0D0, & !  46
     &          270.D0, 275.D0,  4.0D0, & !  47
     &          275.D0, 280.D0,  3.0D0, & !  48
     &          280.D0, 360.D0,  2.0D0  & !  49
     &         /
      DATA       AZELHOR_WARK12M &
     &         / &
     &              0.D0,   5.D0, 5.0D0, & !   1
     &              5.D0,  10.D0, 5.D0,  & !   2
     &             10.D0,  15.D0, 5.D0,  & !   3
     &             15.D0,  20.D0, 5.D0,  & !   4
     &             20.D0,  25.D0, 5.D0,  & !   5
     &             25.D0,  30.D0, 5.D0,  & !   6
     &             30.D0,  35.D0, 5.D0,  & !   7
     &             35.D0,  40.D0, 5.D0,  & !   8
     &             40.D0,  45.D0, 5.D0,  & !   9
     &             45.D0,  50.D0, 5.D0,  & !  10
     &             50.D0,  55.D0, 5.D0,  & !  11
     &             55.D0,  60.D0, 5.D0,  & !  12
     &             60.D0,  65.D0, 5.D0,  & !  13
     &             65.D0,  70.D0, 5.D0,  & !  14
     &             70.D0,  75.D0, 5.D0,  & !  15
     &             75.D0,  80.D0, 5.D0,  & !  16
     &             80.D0,  85.D0, 5.D0,  & !  17
     &             85.D0,  90.D0, 5.D0,  & !  18
     &             90.D0,  95.D0, 5.D0,  & !  19
     &             95.D0, 100.D0, 5.D0,  & !  20
     &            100.D0, 105.D0, 5.D0,  & !  21
     &            105.D0, 110.D0, 5.D0,  & !  22
     &            110.D0, 115.D0, 5.D0,  & !  23
     &            115.D0, 120.D0, 5.D0,  & !  24
     &            120.D0, 125.D0, 5.D0,  & !  25
     &            125.D0, 130.D0, 5.D0,  & !  26
     &            130.D0, 135.D0, 6.D0,  & !  27
     &            135.D0, 140.D0, 7.D0,  & !  28
     &            140.D0, 145.D0, 7.D0,  & !  29
     &            145.D0, 150.D0, 8.D0,  & !  30
     &            150.D0, 155.D0, 8.D0,  & !  31
     &            155.D0, 160.D0, 8.D0,  & !  32
     &            160.D0, 165.D0, 8.D0,  & !  33
     &            165.D0, 170.D0, 8.D0,  & !  34
     &            170.D0, 175.D0, 9.D0,  & !  35
     &            175.D0, 180.D0, 9.D0,  & !  36
     &            180.D0, 185.D0, 8.D0,  & !  37
     &            185.D0, 190.D0, 8.D0,  & !  38
     &            190.D0, 195.D0, 7.D0,  & !  39
     &            195.D0, 200.D0, 7.D0,  & !  40
     &            200.D0, 205.D0, 7.D0,  & !  41
     &            205.D0, 210.D0, 7.D0,  & !  42
     &            210.D0, 215.D0, 7.D0,  & !  43
     &            215.D0, 220.D0, 6.D0,  & !  44
     &            220.D0, 225.D0, 5.D0,  & !  45
     &            225.D0, 230.D0, 5.D0,  & !  46
     &            230.D0, 235.D0, 5.D0,  & !  47
     &            235.D0, 240.D0, 5.D0,  & !  48
     &            240.D0, 245.D0, 5.D0,  & !  49
     &            245.D0, 250.D0, 5.D0,  & !  50
     &            250.D0, 255.D0, 5.D0,  & !  51
     &            255.D0, 260.D0, 5.D0,  & !  52
     &            260.D0, 265.D0, 5.D0,  & !  53
     &            265.D0, 270.D0, 5.D0,  & !  54
     &            270.D0, 275.D0, 5.D0,  & !  55
     &            275.D0, 280.D0, 5.D0,  & !  56
     &            280.D0, 285.D0, 5.D0,  & !  57
     &            285.D0, 290.D0, 5.D0,  & !  58
     &            290.D0, 295.D0, 5.D0,  & !  59
     &            295.D0, 300.D0, 5.D0,  & !  60
     &            300.D0, 305.D0, 5.D0,  & !  61
     &            305.D0, 310.D0, 5.D0,  & !  62
     &            310.D0, 315.D0, 5.D0,  & !  63
     &            315.D0, 320.D0, 5.D0,  & !  64
     &            320.D0, 325.D0, 5.D0,  & !  65
     &            325.D0, 330.D0, 5.D0,  & !  66
     &            330.D0, 335.D0, 5.D0,  & !  67
     &            335.D0, 340.D0, 5.D0,  & !  68
     &            340.D0, 345.D0, 5.D0,  & !  69
     &            345.D0, 350.D0, 5.D0,  & !  70
     &            350.D0, 355.D0, 5.D0,  & !  71
     &            355.D0, 360.D0, 5.D0   & !  72
     &         /
!
      DATA       AZELHOR_HARTRAO &
     &         / &
     &            0.0D0,  10.0D0, 19.0D0, & !   1
     &           10.0D0,  14.0D0, 18.0D0, & !   2
     &           14.0D0,  19.0D0, 17.0D0, & !   3
     &           19.0D0,  23.0D0, 15.0D0, & !   4
     &           23.0D0,  26.0D0, 13.0D0, & !   5
     &           26.0D0,  29.0D0, 11.0D0, & !   6
     &           29.0D0,  31.0D0,  9.0D0, & !   7
     &           31.0D0,  55.0D0,  7.0D0, & !   8
     &           55.0D0,  63.0D0,  6.0D0, & !   9
     &           63.0D0,  72.0D0,  6.0D0, & !  10
     &           72.0D0,  95.0D0,  7.0D0, & !  11
     &           95.0D0, 104.0D0,  8.0D0, & !  12
     &          104.0D0, 110.0D0,  9.0D0, & ! 13
     &          110.0D0, 114.0D0, 11.0D0, & ! 14
     &          114.0D0, 119.0D0, 13.0D0, & ! 15
     &          119.0D0, 125.0D0, 15.0D0, & ! 16
     &          125.0D0, 131.0D0, 17.0D0, & ! 17
     &          131.0D0, 138.0D0, 19.0D0, & ! 18
     &          138.0D0, 147.0D0, 21.0D0, & ! 19
     &          147.0D0, 159.0D0, 23.0D0, & ! 20
     &          159.0D0, 172.0D0, 25.0D0, & ! 21
     &          172.0D0, 178.0D0, 26.0D0, & ! 22
     &          178.0D0, 182.0D0, 33.0D0, & ! 23
     &          182.0D0, 188.0D0, 33.0D0, & ! 24
     &          188.0D0, 201.0D0, 26.0D0, & ! 25
     &          201.0D0, 213.0D0, 25.0D0, & ! 26
     &          213.0D0, 222.0D0, 23.0D0, & ! 27
     &          222.0D0, 229.0D0, 21.0D0, & ! 28
     &          229.0D0, 235.0D0, 19.0D0, & ! 29
     &          235.0D0, 241.0D0, 17.0D0, & ! 30
     &          241.0D0, 246.0D0, 15.0D0, & ! 31
     &          246.0D0, 250.0D0, 13.0D0, & ! 32
     &          250.0D0, 256.0D0, 11.0D0, & ! 33
     &          256.0D0, 260.0D0,  9.0D0, & ! 34
     &          260.0D0, 264.0D0,  7.0D0, & ! 35
     &          264.0D0, 270.0D0,  5.0D0, & ! 36
     &          270.0D0, 279.0D0,  3.0D0, & ! 37
     &          279.0D0, 285.0D0,  3.0D0, & ! 38
     &          285.0D0, 293.0D0,  5.0D0, & ! 39
     &          293.0D0, 302.0D0,  7.0D0, & ! 40
     &          302.0D0, 311.0D0,  9.0D0, & ! 41
     &          311.0D0, 318.0D0, 10.0D0, & ! 42
     &          318.0D0, 325.0D0, 10.0D0, & ! 43
     &          325.0D0, 331.0D0,  9.0D0, & ! 44
     &          331.0D0, 334.0D0,  9.0D0, & ! 45
     &          334.0D0, 337.0D0, 11.0D0, & ! 46
     &          337.0D0, 341.0D0, 13.0D0, & ! 47
     &          341.0D0, 346.0D0, 15.0D0, & ! 48
     &          346.0D0, 350.0D0, 17.0D0, & ! 49
     &          350.0D0, 360.0D0, 18.0D0  & ! 50
     &         /
!
      SUR_CHECK_VIS = .TRUE.
      STA_NAM = SUR%STA(IND_STA)%NAME
      IF ( SUR%STA(IND_STA)%MOUNT_TYPE == MT__ALTAZ ) THEN
           IF ( EL > SUR%STA(IND_STA)%EL_MAX ) SUR_CHECK_VIS = .FALSE.
           IF ( EL < SUR%STA(IND_STA)%EL_MIN ) SUR_CHECK_VIS = .FALSE.
           IF ( .NOT. SUR_CHECK_VIS ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
         ELSE IF ( SUR%STA(IND_STA)%MOUNT_TYPE == MT__EQUAT ) THEN
           IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                DEL = SUR%SOU(IND_SRC)%DELTA
              ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                DEL = SUR%SO2(IND_SRC)%DELTA
              ELSE IF ( CUR_TYP == SUR__TYP_CAL ) THEN
                DEL = SUR%CAL(IND_SRC)%DELTA
           END IF
           IF ( DEL > SUR%STA(IND_STA)%EL_MAX ) SUR_CHECK_VIS = .FALSE.
           IF ( DEL < SUR%STA(IND_STA)%EL_MIN ) SUR_CHECK_VIS = .FALSE.
           IF ( HA  > SUR%STA(IND_STA)%AZ_ACC_MAX ) SUR_CHECK_VIS = .FALSE.
           IF ( HA  < SUR%STA(IND_STA)%AZ_ACC_MIN ) SUR_CHECK_VIS = .FALSE.
           IF ( .NOT. SUR_CHECK_VIS ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
         ELSE IF ( SUR%STA(IND_STA)%MOUNT_TYPE == MT__XY_E ) THEN
           IF ( DABS(DTAN(EL)) < 1.D-6 ) THEN
                A = P2I
              ELSE 
                A = DATAN ( DCOS(AZ)/DTAN(EL) )
                B = DASIN ( DSIN(AZ)*DCOS(EL) )
           END IF
           IF ( A  > SUR%STA(IND_STA)%AZ_ACC_MAX ) SUR_CHECK_VIS = .FALSE.
           IF ( A  < SUR%STA(IND_STA)%AZ_ACC_MIN ) SUR_CHECK_VIS = .FALSE.
           IF ( B > SUR%STA(IND_STA)%EL_MAX ) SUR_CHECK_VIS = .FALSE.
           IF ( B < SUR%STA(IND_STA)%EL_MIN ) SUR_CHECK_VIS = .FALSE.
           IF ( .NOT. SUR_CHECK_VIS ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
      END IF
      AZ_USED = AZ - PI2*(IDINT(AZ/PI2))
      IF ( AZ_USED < 0.0D0 ) AZ_USED = AZ_USED + PI2
      IF ( SUR%STA(IND_STA)%N_HM > 1 ) THEN
           DO 410 J1=1,SUR%STA(IND_STA)%N_HM-1
              IF ( AZ_USED .GE. SUR%STA(IND_STA)%AZ_HM(J1)   .AND. &
     &             AZ_USED .LT. SUR%STA(IND_STA)%AZ_HM(J1+1)       ) THEN
                   IF ( EL < SUR%STA(IND_STA)%EL_HM(J1) + EL__MARGIN  ) THEN
                        SUR_CHECK_VIS = .FALSE.
                   END IF
              END IF
 410       CONTINUE 
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      IF ( STA_NAM == 'VERAMZSW' ) THEN
           DO 510 J1=1,NUMHOR_VERAMZSW
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_VERAMZSW(1,J1) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_VERAMZSW(2,J1) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_VERAMZSW(3,J1)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 510       CONTINUE 
         ELSE IF ( STA_NAM == 'VERAIRKI' ) THEN
           DO 420 J2=1,NUMHOR_VERAIRKI
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_VERAIRKI(1,J2) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_VERAIRKI(2,J2) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_VERAIRKI(3,J2)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 420       CONTINUE 
         ELSE IF ( STA_NAM == 'VERAOGSW' ) THEN
           DO 430 J3=1,NUMHOR_VERAOGSW
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_VERAOGSW(1,J3) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_VERAOGSW(2,J3) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_VERAOGSW(3,J3)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 430       CONTINUE 
         ELSE IF ( STA_NAM == 'VERAISGK' ) THEN
           DO 440 J4=1,NUMHOR_VERAISGK
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_VERAISGK(1,J4) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_VERAISGK(2,J4) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_VERAISGK(3,J4)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 440       CONTINUE 
         ELSE IF ( STA_NAM == 'KASHIMA ' ) THEN
           DO 450 J5=1,NUMHOR_KASHIMA
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_KASHIMA(1,J5) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_KASHIMA(2,J5) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_KASHIMA(3,J5)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 450       CONTINUE 
         ELSE IF ( STA_NAM == 'BR-VLBA ' ) THEN
           DO 460 J6=1,NUMHOR_BR
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_BR(1,J6) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_BR(2,J6) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_BR(3,J6)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 460       CONTINUE 
         ELSE IF ( STA_NAM == 'FD-VLBA ' ) THEN
           DO 470 J7=1,NUMHOR_FD
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_FD(1,J7) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_FD(2,J7) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_FD(3,J7)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 470       CONTINUE 
         ELSE IF ( STA_NAM == 'HN-VLBA ' ) THEN
           DO 480 J8=1,NUMHOR_HN
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_HN(1,J8) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_HN(2,J8) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_HN(3,J8)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 480       CONTINUE 
         ELSE IF ( STA_NAM == 'KP-VLBA ' ) THEN
           DO 490 J9=1,NUMHOR_KP
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_KP(1,J9) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_KP(2,J9) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_KP(3,J9)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 490       CONTINUE 
         ELSE IF ( STA_NAM == 'LA-VLBA ' ) THEN
           DO 4100 J10=1,NUMHOR_LA
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_LA(1,J10) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_LA(2,J10) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_LA(3,J10)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4100       CONTINUE 
         ELSE IF ( STA_NAM == 'MK-VLBA ' ) THEN
           DO 4110 J11=1,NUMHOR_MK
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_MK(1,J11) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_MK(2,J11) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_MK(3,J11)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4110       CONTINUE 
         ELSE IF ( STA_NAM == 'NL-VLBA ' ) THEN
           DO 4120 J12=1,NUMHOR_NL
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_NL(1,J12) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_NL(2,J12) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_NL(3,J12)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4120       CONTINUE 
         ELSE IF ( STA_NAM == 'OV-VLBA ' ) THEN
           DO 4130 J13=1,NUMHOR_OV
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_OV(1,J13) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_OV(2,J13) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_OV(3,J13)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4130       CONTINUE 
         ELSE IF ( STA_NAM == 'PIETOWN ' ) THEN
           DO 4140 J14=1,NUMHOR_PT
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_PT(1,J14) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_PT(2,J14) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_PT(3,J14)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4140       CONTINUE 
         ELSE IF ( STA_NAM == 'SC-VLBA ' ) THEN
           DO 4150 J15=1,NUMHOR_SC
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_SC(1,J15) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_SC(2,J15) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_SC(3,J15)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4150       CONTINUE 
         ELSE IF ( STA_NAM == 'WARK12M ' ) THEN
           DO 4160 J16=1,NUMHOR_WARK12M
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_WARK12M(1,J16) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_WARK12M(2,J16) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_WARK12M(3,J16)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4160       CONTINUE 
         ELSE IF ( STA_NAM == 'HARTRAO ' ) THEN
           DO 4170 J17=1,NUMHOR_HARTRAO
              IF ( AZ_USED/DEG__TO__RAD .GE. AZELHOR_HARTRAO(1,J17) .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. AZELHOR_HARTRAO(2,J17) .AND. &
     &             EL/DEG__TO__RAD .LT. AZELHOR_HARTRAO(3,J17)       ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4170      CONTINUE 
        ELSE IF ( SUR%STA(IND_STA)%N_HM > 1 ) THEN
           DO 4180 J18=1,SUR%STA(IND_STA)%N_HM-1
              IF ( AZ_USED/DEG__TO__RAD .GE. SUR%STA(IND_STA)%AZ_HM(J18)   .AND. &
     &             AZ_USED/DEG__TO__RAD .LE. SUR%STA(IND_STA)%AZ_HM(J18+1) .AND. &
     &             EL/DEG__TO__RAD .LT. SUR%STA(IND_STA)%EL_HM(J18) + EL__MARGIN ) THEN
                   SUR_CHECK_VIS = .FALSE.
              END IF
 4180      CONTINUE 
      END IF
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  FUNCTION  SUR_CHECK_VIS  !#!#

