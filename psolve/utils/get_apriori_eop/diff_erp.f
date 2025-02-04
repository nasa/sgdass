      SUBROUTINE DIFF_ERP ( NR, JDR, XR_VAL, YR_VAL, UR_VAL, &
     &                               XR_ERR, YR_ERR, UR_ERR, &
     &                NF, JDF, XF_VAL, XF_ERR, YF_VAL, YF_ERR, UF_VAL, UF_ERR, &
     &                XP_SH_VAL, XP_DR_VAL, XP_SH_ERR, XP_DR_ERR, &
     &                YP_SH_VAL, YP_DR_VAL, YP_SH_ERR, YP_DR_ERR, &
     &                U1_SH_VAL, U1_DR_VAL, U1_SH_ERR, U1_DR_ERR, &
     &                CH_FLAG, JD_BEGROT, JD_ENDROT, FL_ROT, FL_EQUWEI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DIFF_ERP  computes parameters of the linear trend in the   *
! *   differences between two series of pole coordinates and UT1.        *
! *   R - F = A + B*(T-T0) where T0 is J2000 epoch and T is time in      *
! *           Julian years.                                              *
! *                                                                      *
! *   After that, if flag FL_ROT is .TRUE., F-series are modified:       *
! *   F_new = F_old + A + V*(T-T0).                                      *
! *                                                                      *
! *  ### 02-NOPV-2000   DIFF_ERP  v2.0 (c)  L. Petrov  15-MAY-2001 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INTEGER*4  NF, NR
      REAL*8     JDR(NR), XR_VAL(NR), XR_ERR(NR), YR_VAL(NR), YR_ERR(NR), &
     &           UR_VAL(NR), UR_ERR(NR), JDF(NF), JD_BEGROT, JD_ENDROT, &
     &           XF_VAL(NF), XF_ERR(NF), YF_VAL(NF), YF_ERR(NF), &
     &           UF_VAL(NF), UF_ERR(NF)
      REAL*8     XP_SH_VAL, XP_DR_VAL, XP_SH_ERR, XP_DR_ERR, &
     &           YP_SH_VAL, YP_DR_VAL, YP_SH_ERR, YP_DR_ERR, &
     &           U1_SH_VAL, U1_DR_VAL, U1_SH_ERR, U1_DR_ERR
      CHARACTER  CH_FLAG(NF)*3
      REAL*8     MEAN_T, SH_VAL
      INTEGER*4  MP
      PARAMETER  ( MP = 16384 )
      REAL*8     JD_BEG, JD_INT, DP, TIM(MP), WEI(MP), VAL(MP)
      LOGICAL*4  FL_ROT, FL_EQUWEI
      INTEGER*4  IUER
      INTEGER*4  JDR_IND, JDF_IND, INDR, INDF, J1, J2, J3, J4, J5, J6, &
     &           IP, NP, IER
!
! --- Both series may start from the different dates and may end at different
! --- dates. We will be using for computing the differences only the points
! --- at both series which correspond to the sames dates
!
      IF ( JDR(1) .LE. JDF(1) ) THEN
           JDR_IND = NINT ( JDF(1) - JDR(1) + 1 + 0.001 )
           JDF_IND = 1
         ELSE
           JDR_IND = 1
           JDF_IND = NINT ( JDR(1) - JDF(1) + 1 + 0.001 )
      END IF
!
      DP = JD_BEGROT - JDR(JDR_IND)
      IF ( DP .GT. 0.0 ) THEN
           JDR_IND = JDR_IND + IDNINT ( DP + 0.1D0 )
           JDF_IND = JDF_IND + IDNINT ( DP + 0.1D0 )
      END IF
      IP = MIN ( NR-JDR_IND+1, NF-JDF_IND+1 )
      IF ( IP .LT. 3 ) THEN
           CALL ERR_LOG ( 8231, IUER, 'DIFF_ERP', 'Insufficient number of '// &
     &         'points for computation linear drift and shift' )
           RETURN
      END IF
!
! --- Scan the "final" series and look for the last point marked as
! --- "derived from observations"
!
      NP = 0
      DO 410 J1=1,IP
         IF ( CH_FLAG(J1)(1:1) .EQ. 'I' ) THEN
              NP = NP + 1
              JD_ENDROT  = JDR( JDR_IND + J1-1 )
         END IF
 410  CONTINUE
      IF ( NP .LT. 3 ) THEN
           CALL ERR_LOG ( 8232, IUER, 'DIFF_ERP', 'Insufficient number of '// &
     &         'points for computation linear drift and shift' )
           RETURN
      END IF
!
! --- Scan the reference series form back to beginning and look for the first
! --- point with positive sigma. This point mark the last eppoch with
! --- observations.
!
      DO 420 J2=NP,1,-1
         IF ( UR_ERR(J2) .GT. 0.0D0 ) THEN
              JD_ENDROT  = JDR( JDR_IND + J2-1 )
              NP = J2
              GOTO 820
         END IF
 420  CONTINUE
 820  CONTINUE
!
      IF ( NP .LT. 3 ) THEN
           CALL ERR_LOG ( 8233, IUER, 'DIFF_ERP', 'Insufficient number of '// &
     &         'points for computation linear drift and shift' )
           RETURN
      END IF
!
      JD_INT = (JDR(JDR_IND+IP-1) + JDR(JDR_IND))/2.0D0
      JD_BEG = (JDR(JDR_IND+IP-1) - JDR(JDR_IND))/2.0D0
!
! --- Make the differences:
! --- TIM -- array of time arguments in years
! --- VAL -- difference in X pole coordinates
! --- WEI -- wieght of the differences. We assume that R and F series are
! ---        not correlated (although, in practice, it is not true)
!
      DO 430 J3=1,NP
         INDR = JDR_IND+J3-1
         INDF = JDF_IND+J3-1
!
         TIM(J3) = ( JDR(INDR) -2451544.5D0 )/365.25D0
         VAL(J3) = XR_VAL(INDR) - XF_VAL(INDF)
         IF ( FL_EQUWEI ) THEN
              WEI(J3) = 1.D0
            ELSE
              WEI(J3) = 1.D0/DSQRT ( XR_ERR(INDR)**2 + XF_ERR(INDF)**2 )
         END IF
 430  CONTINUE
!
! --- Compute parameters of linear regeression for X pole coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL RGRW8 ( NP, TIM, VAL, WEI, %VAL(0), MEAN_T, XP_DR_VAL, SH_VAL, &
     &             XP_DR_ERR, XP_SH_ERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8234, IUER, 'DIFF_ERP', 'Error in computation '// &
     &         'linear regression for X pole' )
           RETURN
      END IF
!
! --- Refer shift to the J2000 epoch
!
      XP_SH_VAL = SH_VAL - MEAN_T*XP_DR_VAL
!
      DO 440 J4=1,NP
         INDR = JDR_IND+J4-1
         INDF = JDF_IND+J4-1
!
         TIM(J4) = ( JDR(INDR) -2451544.5D0 )/365.25D0
         VAL(J4) = YR_VAL(INDR) - YF_VAL(INDF)
         IF ( FL_EQUWEI ) THEN
              WEI(J4) = 1.D0
            ELSE
              WEI(J4) = 1.D0/DSQRT ( YR_ERR(INDR)**2 + YF_ERR(INDF)**2 )
         END IF
 440  CONTINUE
!
! --- Compute parameters of linear regeression for Y pole coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL RGRW8 ( NP, TIM, VAL, WEI, %VAL(0), MEAN_T, YP_DR_VAL, SH_VAL, &
     &             YP_DR_ERR, YP_SH_ERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8235, IUER, 'DIFF_ERP', 'Error in computation '// &
     &         'linear regression for X pole' )
           RETURN
      END IF
!
      YP_SH_VAL = SH_VAL - MEAN_T*YP_DR_VAL
!
      DO 450 J5=1,NP
         INDR = JDR_IND+J5-1
         INDF = JDF_IND+J5-1
!
         TIM(J5) = ( JDR(INDR) -2451544.5D0 )/365.25D0
         VAL(J5) = UR_VAL(INDR) - UF_VAL(INDF)
         IF ( FL_EQUWEI ) THEN
              WEI(J5) = 1.D0
            ELSE
              WEI(J5) = 1.D0/DSQRT ( UR_ERR(INDR)**2 + UF_ERR(INDF)**2 )
         END IF
 450  CONTINUE
!
! --- Compute parameters of linear regeression for UT1
!
      CALL ERR_PASS ( IUER, IER )
      CALL RGRW8 ( NP, TIM, VAL, WEI, %VAL(0), MEAN_T, U1_DR_VAL, SH_VAL, &
     &             U1_DR_ERR, U1_SH_ERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8236, IUER, 'DIFF_ERP', 'Error in computation '// &
     &         'linear regression for X pole' )
           RETURN
      END IF
!
      U1_SH_VAL = SH_VAL - MEAN_T*U1_DR_VAL
      IF ( FL_ROT ) THEN
!
! -------- Add linear regression from F-series
!
           DO 460 J6=1,NF
              TIM(J6) = ( JDF(J6) -2451544.5D0 )/365.25D0
              XF_VAL(J6) = XF_VAL(J6) + ( XP_SH_VAL + XP_DR_VAL*TIM(J6) )
              YF_VAL(J6) = YF_VAL(J6) + ( YP_SH_VAL + YP_DR_VAL*TIM(J6) )
              UF_VAL(J6) = UF_VAL(J6) + ( U1_SH_VAL + U1_DR_VAL*TIM(J6) )
 460       CONTINUE
       END IF
!
!      TYPE *,' Xp_sh = ',XP_SH_VAL,' -+ ',XP_SH_ERR
!      TYPE *,' Xp_dr = ',XP_DR_VAL,' -+ ',XP_DR_ERR
!      TYPE *,' Yp_sh = ',YP_SH_VAL,' -+ ',YP_SH_ERR
!      TYPE *,' Yp_dr = ',YP_DR_VAL,' -+ ',YP_DR_ERR
!      TYPE *,' U1_sh = ',U1_SH_VAL,' -+ ',U1_SH_ERR
!      TYPE *,' U1_dr = ',U1_DR_VAL,' -+ ',U1_DR_ERR
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIFF_ERP  #!#
