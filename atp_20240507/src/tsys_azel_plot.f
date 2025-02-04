      SUBROUTINE TSYS_AZEL_PLOT ( STA_NAM, FRQ, POL, IDEV, NP, EL_SCA,  &
     &                            AZ_SCA, TSYS_SCA )
! ************************************************************************
! *                                                                      *
! *   Routine TSYS_AZEL_PLOT
! *                                                                      *
! *  ### 13-APR-2022 TSYS_AZEL_PLOT v1.0 (c)  L. Petrov  13-APR-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      INTEGER*4  IDEV, NP
      CHARACTER  STA_NAM*(*), POL*(*)
      REAL*8     FRQ, EL_SCA(NP), AZ_SCA(NP), TSYS_SCA(NP)
      TYPE       ( DIAGI_STRU ) ::  DIAGI_S
      REAL*4     PAP_SIZE
      CHARACTER  FILOUT*128, CH*1, STR*128, TIT
      INTEGER*4  MH, IUER 
      PARAMETER  ( MH = 15 )
      REAL*4     HOR_AZ(MH), HOR_EL(MH)
      REAL*4     AZ_MIN, AZ_MAX,  EL_MIN, EL_MAX, XC, YC, XP, YP,       &
     &           X2(2), Y2(2), RADP, XCOEF, YP_TIT(3), TSYS_RANGE(4)
      INTEGER*4  J1, J2, J3, J4, LH, WHITE_CLR, IVRB
      INTEGER*4, EXTERNAL :: PGBEG, ILEN, I_LEN
!
      IF ( STA_NAM == 'KOKEE12M' ) THEN
           HOR_AZ(1)  =   0.0 ; HOR_EL(1)  = 10.0
           HOR_AZ(2)  = 260.0 ; HOR_EL(2)  = 10.0
           HOR_AZ(3)  = 280.0 ; HOR_EL(3)  = 10.0
           HOR_AZ(4)  = 295.0 ; HOR_EL(4)  =  5.0
           HOR_AZ(5)  = 300.0 ; HOR_EL(5)  = 10.0
           HOR_AZ(6)  = 305.0 ; HOR_EL(6)  = 25.0
           HOR_AZ(7)  = 310.0 ; HOR_EL(7)  = 35.0
           HOR_AZ(8)  = 325.0 ; HOR_EL(8)  = 40.0
           HOR_AZ(9)  = 330.0 ; HOR_EL(9)  = 45.0
           HOR_AZ(10) = 335.0 ; HOR_EL(10) = 40.0
           HOR_AZ(11) = 340.0 ; HOR_EL(11) = 35.0
           HOR_AZ(12) = 350.0 ; HOR_EL(12) = 30.0
           HOR_AZ(13) = 360.0 ; HOR_EL(13) = 10.0
           LH = 13
      END IF
!
      IF ( STA_NAM == 'MGO12M  ' ) THEN
           HOR_AZ(1)  =   0.0 ; HOR_EL(1)  =  5.0
           HOR_AZ(2)  =  70.0 ; HOR_EL(2)  =  6.0
           HOR_AZ(3)  =  75.0 ; HOR_EL(3)  =  7.0
           HOR_AZ(4)  =  80.0 ; HOR_EL(4)  =  8.0
           HOR_AZ(5)  =  85.0 ; HOR_EL(5)  =  9.0
           HOR_AZ(6)  =  95.0 ; HOR_EL(6)  =  8.0
           HOR_AZ(7)  = 105.0 ; HOR_EL(7)  =  6.0
           HOR_AZ(8)  = 145.0 ; HOR_EL(8)  =  7.0
           HOR_AZ(9)  = 150.0 ; HOR_EL(9)  =  8.0
           HOR_AZ(10) = 155.0 ; HOR_EL(10) =  9.0
           HOR_AZ(11) = 165.0 ; HOR_EL(11) = 10.0
           HOR_AZ(12) = 185.0 ; HOR_EL(12) =  8.0
           HOR_AZ(13) = 190.0 ; HOR_EL(13) =  7.0
           HOR_AZ(14) = 195.0 ; HOR_EL(14) =  5.0
           HOR_AZ(15) = 360.0 ; HOR_EL(15) =  5.0
           LH = 15
      END IF
!
      IF ( STA_NAM == 'GGAO12M ' ) THEN
           HOR_AZ(1)  =   0.0  ; HOR_EL(1)  =  6.0
           HOR_AZ(2)  =  15.0  ; HOR_EL(2)  =  7.0
           HOR_AZ(3)  =  58.0  ; HOR_EL(3)  =  7.0
           HOR_AZ(4)  =  90.0  ; HOR_EL(4)  =  7.5
           HOR_AZ(5)  = 130.0  ; HOR_EL(5)  =  7.0
           HOR_AZ(6)  = 149.99 ; HOR_EL(6)  =  7.0
           HOR_AZ(7)  = 150.0  ; HOR_EL(7)  = 42.0
           HOR_AZ(8)  = 234.0  ; HOR_EL(8)  = 42.0
           HOR_AZ(9)  = 234.01 ; HOR_EL(9)  =  7.0
           HOR_AZ(10) = 360.0  ; HOR_EL(10) =  7.0
           LH = 10
      END IF
!
      IVRB   = 3
      FILOUT = '/tmp/foo'
!
      IF ( IVRB .GE. 2 ) THEN
         DO 410 J1=1,NP
            WRITE ( 6, 120 ) EL_SCA(J1), AZ_SCA(J1), TSYS_SCA(J1)
  120       FORMAT ( 'El: ', F6.2, ' Az: ', F6.2, ' Tsys: ', F7.2 )
  410    CONTINUE 
      END IF
!
      IF ( IDEV .EQ. 1 ) THEN
         IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
      ELSE IF ( IDEV .EQ. 2 ) THEN
         FILOUT = FILOUT(1:I_LEN(FILOUT))//PS_DIAGI//'/CPS'
         IF ( PGBEG ( 0, FILOUT, 1, 1 ) .NE. 1 ) STOP
      ELSE IF ( IDEV .EQ. 3 ) THEN
         FILOUT = FILOUT(1:I_LEN(FILOUT))//GIF_DIAGI//'/GIF'
         IF ( PGBEG ( 0, FILOUT, 1, 1 ) .NE. 1 ) STOP
      ELSE IF ( IDEV .EQ. 4 ) THEN
         FILOUT = FILOUT(1:I_LEN(FILOUT))//PS_DIAGI//'/VCPS'
         IF ( PGBEG ( 0, FILOUT, 1, 1 ) .NE. 1 ) STOP
      END IF
      IF ( IDEV .EQ. 1 ) THEN
         PAP_SIZE = 300.0 
      ELSE IF ( IDEV .EQ. 2 ) THEN
         PAP_SIZE = 270.0
      ELSE IF ( IDEV .EQ. 3 ) THEN
         PAP_SIZE = 400.0
      ELSE IF ( IDEV .EQ. 4 ) THEN
         PAP_SIZE = 600.0
      ELSE
         PAP_SIZE = 240.0
      END IF
      CALL PGPAP  ( PAP_SIZE/25.4, 1.0 )
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 3
      IUER = -1
      CALL DIAGI_CLS ( DIAGI_S, IUER )
!
! --- Set white color
!
      CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
      CALL PGCOL_RGB ( 11,  67, 180,  38 )
      CALL PGCOL_RGB ( 12, 180, 151,  38 )
      CALL PGCOL_RGB ( 13, 180,  38,  67 )
      WHITE_CLR = BCG_CLRI
!
      AZ_MIN = -2.0
      AZ_MAX = 362.0
      EL_MIN =  0.0
      EL_MAX = 90.0
!
      CALL PGSWIN  ( AZ_MIN, AZ_MAX, EL_MIN, EL_MAX ) 
      CALL PGSVP   ( 0.10, 0.90, 0.10, 0.50  )          ! makes fields for labels
      CALL PGSCR   ( 0, 1.0, 1.0, 1.0 )                 ! pure white background
      CALL PGSCH  ( 0.8 )
      CALL PGSLW  ( 3 )
      CALL PGBOX  ( 'bicnts', 30.0, 3, 'bicnts', 10.0, 5 )
      CALL PGSCH  ( 1.4 )
      WRITE ( UNIT=STR, FMT=110 ) STA_NAM, FRQ, POL
 110  FORMAT ( 'Tsys at ', A, ' Frq: ', F8.1, ' MHz Pol: ', A1 )
      CALL PGPTEXT ( (AZ_MIN+AZ_MAX)/2.0,                               &
     &               EL_MAX + 0.05*(EL_MAX - EL_MIN), .0, .5, TRIM(STR))
      CALL PGSCH   ( 0.8 )
!
      DO 420 J2=1,LH-1
         X2(1) = HOR_AZ(J2)
         X2(2) = HOR_AZ(J2+1)
         Y2(1) = HOR_EL(J2)
         Y2(2) = HOR_EL(J2)
         CALL PGLINE ( 2, X2, Y2 )
!
         X2(1) = HOR_AZ(J2+1)
         X2(2) = HOR_AZ(J2+1)
         Y2(1) = HOR_EL(J2)
         Y2(2) = HOR_EL(J2+1)
         CALL PGLINE ( 2, X2, Y2 )
 420  CONTINUE 
!
      RADP = 0.5
      XCOEF = (AZ_MAX - AZ_MIN)/(EL_MAX - EL_MIN)/2.0
!
!      TSYS_RANGE(1) = 50.0
!      TSYS_RANGE(2) = 90.0
!      TSYS_RANGE(3) = 130.0
!      TSYS_RANGE(4) = 300.0
!
      TSYS_RANGE(1) = 40.0
      TSYS_RANGE(2) = 60.0
      TSYS_RANGE(3) = 100.0
      TSYS_RANGE(4) = 300.0
!
!      TSYS_RANGE(1) = 40.0
!      TSYS_RANGE(2) = 90.0
!      TSYS_RANGE(3) = 130.0
!      TSYS_RANGE(4) = 300.0
!
      DO 430 J3=1,NP
         XP = AZ_SCA(J3)
         YP = EL_SCA(J3)
         CALL PGSFS ( 1 )
         IF ( TSYS_SCA(J3) < TSYS_RANGE(2) ) THEN
            CALL PGSCI ( 11 )
         ELSE IF ( TSYS_SCA(J3) < TSYS_RANGE(3) ) THEN
            CALL PGSCI ( 12 )
         ELSE
            CALL PGSCI ( 13 )
         END IF
         CALL PGCIRC_PET ( 72, XP, YP, XCOEF*RADP, RADP )
         CALL PGSFS ( 2 )
         CALL PGSCI ( 1 )
         CALL PGCIRC_PET ( 72, XP, YP, XCOEF*RADP, RADP )
 430  CONTINUE 
!
      XP = 10.0
      YP_TIT(1) = 41.0
      YP_TIT(2) = 44.0
      YP_TIT(3) = 47.0
      DO 440 J4=1,3
         CALL PGSFS ( 1     )
         CALL PGSCI ( 10+J4 )
         CALL PGCIRC_PET ( 72, XP, YP_TIT(J4), XCOEF*RADP, RADP )
         CALL PGSCI ( 1 )
         CALL CLRCH ( STR )
         WRITE ( UNIT=STR, FMT='(I3, " - ", I3," K")' )                 &
     &                     INT(TSYS_RANGE(J4)), INT(TSYS_RANGE(J4+1))
         CALL PGTEXT ( XP + 10, YP_TIT(J4)-1.0, TRIM(STR) )
 440  CONTINUE 
!
      IF ( IDEV .EQ. 1 ) THEN
         XC = 0.95
         YC = 0.0
         CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
      END IF
      CALL PGCLOQ()
!
      RETURN
      END  SUBROUTINE TSYS_AZEL_PLOT !#!

