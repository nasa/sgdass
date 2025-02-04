      SUBROUTINE IONO_REFINE ( OBS, GAMBX, GAMBS, KION )
! ************************************************************************
! *                                                                      *
! *
! *                                                                      *
! *  ###  12-AUG-97   IONO_REFINE  v1.0  (c)  L. Petrov  12-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'gamb.i'
      TYPE ( OBS__STRU ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMBX, GAMBS
      INTEGER*4  KION, J1, J2, IAM, IPS, IPX
      REAL*8     SPAC, EPS, GIONX_TAU_OLD, GIONS_TAU_OLD, DOBS_TAU_XS, &
     &                      GIONX_TAU_NEW, GIONS_TAU_NEW, DELTA_AV
      REAL*8     TEFF, DEV_X, DEV_S
      INTEGER*4  IFIND_PL
      PARAMETER  ( EPS = 1.D-12 )
      INTEGER*4  K_IOX(MG_BAS), K_IOS(MG_BAS)
      REAL*8     A_IOX(MG_BAS), A_IOS(MG_BAS)
!
      WRITE ( 6, * ) ' OBS%CUTOFF = ',OBS%CUTOFF
!
      KION = 0
!
      CALL NOUT ( 4*MG_BAS, K_IOX )
      CALL NOUT ( 4*MG_BAS, K_IOS )
!
      DO 410 J1=1,OBS%NOBS
         IPX = IFIND_PL ( GAMBX%L_BAS, GAMBX%LIS_BAS, OBS%IBA(J1) )
         IPS = IFIND_PL ( GAMBS%L_BAS, GAMBS%LIS_BAS, OBS%IBA(J1) )
         IF ( IPX .LE. 0  .OR.  IPS .LE. 0 ) THEN
!
! ----------- Baseline with this observation is removed. We exclude this
! ----------- observation from statistic.
!
              GOTO 410
         END IF
         TEFF = ( OBS%TT(J1) - OBS%TAVALL ) / OBS%TLNALL
!C
         DEV_X = GAMBX%OCT(J1) - ( GAMBX%SH_BAS(IPX) + &
     &                             GAMBX%DR_BAS(IPX)*TEFF + &
     &                             GAMBX%SQ_BAS(IPX)*(1.5D0*TEFF**2 - 0.5D0) )
         DEV_S = GAMBS%OCT(J1) - ( GAMBS%SH_BAS(IPS) + &
     &                             GAMBS%DR_BAS(IPS)*TEFF + &
     &                             GAMBS%SQ_BAS(IPS)*(1.5D0*TEFF**2 - 0.5D0) )
!C
         IF ( DABS(DEV_X) .GT. OBS%CUTOFF  .OR. &
     &        DABS(DEV_S) .GT. OBS%CUTOFF        ) THEN
!
              K_IOX(IPX) =   K_IOX(IPX) + 1
              A_IOX(IPX) = ( A_IOX(IPX)*(K_IOX(IPX)-1) + GAMBX%GION_TAU(J1) )/ &
     &                       K_IOX(IPX)
!
              K_IOS(IPS) =   K_IOS(IPS) + 1
              A_IOS(IPS) = ( A_IOS(IPS)*(K_IOS(IPS)-1) + GAMBS%GION_TAU(J1) )/ &
     &                       K_IOS(IPS)
         END IF
 410  CONTINUE
!
      DO 420 J2=1,OBS%NOBS
         DOBS_TAU_XS = ( GAMBX%OCT(J2) + GAMBX%GION_TAU(J2) ) - &
     &                 ( GAMBS%OCT(J2) + GAMBS%GION_TAU(J2) )
!
         GIONX_TAU_OLD = GAMBX%GION_TAU(J2)
         GIONS_TAU_OLD = GAMBS%GION_TAU(J2)
!
         GIONX_TAU_NEW = -DOBS_TAU_XS * GAMBS%FREQ_GR**2/ &
     &                              ( GAMBX%FREQ_GR**2 - GAMBS%FREQ_GR**2 )
         GIONS_TAU_NEW = -DOBS_TAU_XS * GAMBX%FREQ_GR**2/ &
     &                              ( GAMBX%FREQ_GR**2 - GAMBS%FREQ_GR**2 )
!
         IPX = IFIND_PL ( GAMBX%L_BAS, GAMBX%LIS_BAS, OBS%IBA(J2) )
         IPS = IFIND_PL ( GAMBS%L_BAS, GAMBS%LIS_BAS, OBS%IBA(J2) )
         IF ( IPX .LE. 0  .OR.  IPS .LE. 0 ) THEN
!
! ----------- Baseline with this observation is removed. There is no
! ----------- chances to resurrect this point.
!
              GOTO 420
         END IF
         TEFF = ( OBS%TT(J2) - OBS%TAVALL ) / OBS%TLNALL
!
         DEV_X = GAMBX%OCT(J2) - ( GAMBX%SH_BAS(IPX) + &
     &                             GAMBX%DR_BAS(IPX)*TEFF + &
     &                             GAMBX%SQ_BAS(IPX)*(1.5D0*TEFF**2 - 0.5D0) )
         DEV_S = GAMBS%OCT(J2) - ( GAMBS%SH_BAS(IPS) + &
     &                              GAMBS%DR_BAS(IPS)*TEFF + &
     &                              GAMBS%SQ_BAS(IPS)*(1.5D0*TEFF**2 - 0.5D0) )
!
!         IF ( ( DABS(DEV_X) .LT. OBS.CUTOFF  .OR.
!     #          DABS(DEV_S) .LT. OBS.CUTOFF       )  .AND.
!     #          ( .NOT. GAMBX.USE(J2)  .OR. .NOT. GAMBS.USE(J2) ) ) THEN
         IF ( .NOT. GAMBX%USE(J2)  .OR. .NOT. GAMBS%USE(J2) ) THEN
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              WRITE ( 6, 210 )  j2, GAMBX%USE(J2), GAMBS%USE(J2),dev_x*1.d9, dev_s*1.d9, &
     &                  dobs_tau_xs*1.d9, a_iox(ipx)*1.d9, a_ios(ips)*1.d9
 210          format ( 1x,'i=',i4,' x_s=',2l2,' d_x=',f9.3, &
     &                                        ' d_s=',f9.3, &
     &                                        ' |X-S=',f11.3, &
     &                                        ' ax=',f5.1, &
     &                                        ' as=',f5.1 )
!              type 220, j2, GAMBX.GION_TAU(J2)*1.D9, GAMBS.GION_TAU(J2)*1.D9,
!     #                      GAMBX.JMP(J2)*1.D9,      GAMBS.JMP(J2)*1.D9
! 220          format ( 1x,'i=',i4,' gionx = ',f9.3,' gions = ',f9.3,
!     #                            ' jmpx = ',f9.3,' jmps = ',f9.3 )
              WRITE ( 6, 230 )  J2, GAMBX%GION_TAU(J2)*1.D9, GAMBS%GION_TAU(J2)*1.D9
 230          format ( 1x,'i=',i4,' gionX =',f11.3,' gionS = ',f11.3 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         END IF
         GAMBX%STATUS_ION = GAMB__IONO_2
         GAMBS%STATUS_ION = GAMB__IONO_2
 420  CONTINUE
                    call pause ( 'iono_refine' ) ! %%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      RETURN
      END   !#!  IONO_REFINE  #!#
