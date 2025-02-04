      SUBROUTINE VTD_SETRANGE ( VTD, MJD_BEG, TAI_BEG, MJD_END, TAI_END, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_SETRANGE 
! *                                                                      *
! * ### 26-JAN-2004   VTD_SETRANGE   v1.0 (c) L. Petrov  26-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD_BEG, MJD_END, IUER
      REAL*8     TAI_BEG, TAI_END
      CHARACTER  C_STA(VTD__M_STA)*(SOTID__NAM_LEN)
      REAL*8     R_TRS(3,VTD__M_STA)
      CHARACTER  STR*80, STR2*80
      REAL*8     CF, SF, CL, SL, PP, MU
      INTEGER*4  J1, J2, J3, J4, J5, NN, IER
      INTEGER*4  ILEN, I_LEN
!
      VTD%MJD_BEG = MJD_BEG
      VTD%MJD_END = MJD_END
      VTD%TAI_BEG = TAI_BEG
      VTD%TAI_END = TAI_END
!
      DO 410 J1=1,VTD%L_STA
         CALL COPY_R8 ( 3, VTD%STA(J1)%COO_TRS(1,1), VTD%STA(J1)%BEG_TRS )
         IF ( VTD%STA(J1)%N_EPC .GT. 1 ) THEN
              DO 420 J2=2,VTD%STA(J1)%N_EPC 
                 IF ( VTD%STA(J1)%MJD_EPC(J2) .GT. MJD_BEG .AND. &
     &                VTD%STA(J1)%TAI_EPC(J2) .GT. TAI_BEG       ) THEN
                      CALL COPY_R8 ( 3, VTD%STA(J1)%COO_TRS(1,J2), &
     &                                  VTD%STA(J1)%BEG_TRS )
                 END IF
 420          CONTINUE 
         END IF
!
         DO 430 J3=1,VTD%STA(J1)%N_ECC
            IF ( ( VTD%STA(J1)%ECC_EPOCH_MJD(J3)*86400.0D0 +       &
     &             VTD%STA(J1)%ECC_EPOCH_TAI(J3) ) >               &
     &             ( MJD_BEG*86400.0D0 + TAI_BEG )           .AND. &
     &           ( VTD%STA(J1)%ECC_EPOCH_MJD(J3)*86400.0D0 +       &
     &             VTD%STA(J1)%ECC_EPOCH_TAI(J3) ) <               &
     &             ( MJD_END*86400.0D0 + TAI_END )                 ) THEN
!
                 CALL ADD_VV ( 3, VTD%STA(J1)%BEG_TRS, &
     &                            VTD%STA(J1)%ECC_TRS(1,J3) )
            END IF         
 430     CONTINUE 
!
! ------ Calculation of longitude LONGITUDE and geocentric lattitude PHI_GCN of
! ------ the station
!
         IF ( ( VTD%STA(J2)%BEG_TRS(1)**2 + &
     &          VTD%STA(J2)%BEG_TRS(2)**2 + &
     &          VTD%STA(J2)%BEG_TRS(3)**2   ) .GT. VTD__HEIGHT_MIN**2 .AND. &
     &        ( VTD%STA(J2)%BEG_TRS(1)**2 + &
     &          VTD%STA(J2)%BEG_TRS(2)**2 + &
     &          VTD%STA(J2)%BEG_TRS(3)**2   ) .LT. VTD__HEIGHT_MAX**2       ) THEN
!
              IF ( DABS ( VTD%STA(J1)%BEG_TRS(1) ) .GT. 1.D-8 ) THEN
                   VTD%STA(J1)%LONG = DATAN ( VTD%STA(J1)%BEG_TRS(2)/ &
     &                                        VTD%STA(J1)%BEG_TRS(1)  )
                 ELSE
                   VTD%STA(J1)%LONG = P2I
              END IF
!
              IF ( VTD%STA(J1)%BEG_TRS(1) .LT. 0.0D0 ) VTD%STA(J1)%LONG = PI  + VTD%STA(J1)%LONG 
              IF ( VTD%STA(J1)%LONG       .LT. 0.0D0 ) VTD%STA(J1)%LONG = PI2 + VTD%STA(J1)%LONG 
!
              PP  = DSQRT ( VTD%STA(J1)%BEG_TRS(1)**2 + &
     &                      VTD%STA(J1)%BEG_TRS(2)**2 )
              IF ( DABS(PP) .LT. 1.D-8 ) PP=1.D-8
              VTD%STA(J1)%RAD = DSQRT ( VTD%STA(J1)%BEG_TRS(3)**2 + PP**2    )
              IF ( DABS ( VTD%STA(J2)%RAD - VTD__REA ) .GT. VTD__HEIGHT_MIN .AND. &
                   DABS ( VTD%STA(J2)%RAD - VTD__REA ) .LT. VTD__HEIGHT_MAX       ) THEN
!
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR, FMT='(I12)' ) J1
                   CALL CHASHL ( STR  )
                   CALL CLRCH  ( STR2 )
                   WRITE ( UNIT=STR2, FMT='(3(F15.3,2X))' ) &
     &                    ( VTD%STA(J1)%BEG_TRS(NN), NN=1,3 )
                   CALL ERR_LOG ( 2181, IUER, 'VTD_SETRANGE', 'Wrong '// &
     &                 'positions of station '//VTD%STA(J1)%IVS_NAME// &
     &                 ' -- '//STR2(1:ILEN(STR2))// &
     &                 ' -- they are not on the surface of our planet' )
                   RETURN
              END IF
!
              VTD%STA(J1)%LAT_GCN = DATAN( VTD%STA(J1)%BEG_TRS(3)/PP )
!
! ----------- Calculation matrix of transformation from REN (local topocentric,
! ----------- (Up,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
              CF = DCOS(VTD%STA(J1)%LAT_GCN)
              SF = DSIN(VTD%STA(J1)%LAT_GCN)
              CL = DCOS(VTD%STA(J1)%LONG)
              SL = DSIN(VTD%STA(J1)%LONG)
!
              VTD%STA(J1)%REN_TO_TRS(1,1) = CF*CL
              VTD%STA(J1)%REN_TO_TRS(2,1) = CF*SL
              VTD%STA(J1)%REN_TO_TRS(3,1) = SF
!
              VTD%STA(J1)%REN_TO_TRS(1,2) = -SL
              VTD%STA(J1)%REN_TO_TRS(2,2) =  CL
              VTD%STA(J1)%REN_TO_TRS(3,2) =  0.D0
!
              VTD%STA(J1)%REN_TO_TRS(1,3) = -SF*CL
              VTD%STA(J1)%REN_TO_TRS(2,3) = -SF*SL
              VTD%STA(J1)%REN_TO_TRS(3,3) =  CF
!
              C_STA(J1)   = VTD%STA(J1)%IVS_NAME
              CALL COPY_R8 ( 3, VTD%STA(J1)%BEG_TRS, R_TRS(1,J1) )
!
! ----------- Comutation of geodetic latitude
!
              MU = DATAN ( VTD%STA(J1)%BEG_TRS(3)/PP * &
     &                     ( (1.D0 - VTD__FE) + VTD__EXC_SQ*VTD__REA/ &
     &                     VTD%STA(J1)%RAD  ) )
!
              VTD%STA(J1)%LAT_GDT = &
     &           DATAN( ( (1.D0 - VTD__FE)*VTD%STA(J1)%BEG_TRS(3) + &
     &                     VTD__EXC_SQ*VTD__REA*DSIN(MU)**3 ) / &
     &                    ( (1.D0 - VTD__FE)* &
     &                    ( PP  - VTD__EXC_SQ*VTD__REA*DCOS(MU)**3 )) )
!
! ----------- Computation of height above ellipsoid
!
              VTD%STA(J1)%HEI_ELL = PP*DCOS(VTD%STA(J1)%LAT_GDT) + &
     &              VTD%STA(J1)%BEG_TRS(3)*DSIN(VTD%STA(J1)%LAT_GDT) - &
     &              VTD__REA* &
     &              DSQRT( 1.D0 - VTD__EXC_SQ*DSIN(VTD%STA(J1)%LAT_GDT)**2 )
!
! ----------- Computation of the local gravity accelratino on the ellipsoid
!
              VTD%STA(J1)%GAC_ELL = VTD__ACC_EQU* &
     &             (1.D0 + VTD__GRV_LAT* DSIN(VTD%STA(J1)%LAT_GCN)**2 + &
     &              VTD__GRV_H*VTD__GRV_H )/ &
     &             DSQRT (1.D0 - VTD__EXC_SQ*DSIN(VTD%STA(J1)%LAT_GCN)**2 )
            ELSE 
!
! ----------- Special case of the station in the hell (i.e, geocenter)
!
              VTD%STA(J1)%LONG    = 0.0D0
              VTD%STA(J1)%LAT_GCN = 0.0D0
              VTD%STA(J1)%LAT_GDT = 0.0D0
              VTD%STA(J1)%HEI_ELL = 0.0D0
              VTD%STA(J1)%GAC_ELL = 0.0D0
              CALL NOUT_R8 ( 9, VTD%STA(J1)%REN_TO_TRS )
              VTD%STA(J1)%REN_TO_TRS(1,1) = 1.0D0
              VTD%STA(J1)%REN_TO_TRS(2,2) = 1.0D0
              VTD%STA(J1)%REN_TO_TRS(3,3) = 1.0D0
              VTD%STA(J1)%RAD = DSQRT ( VTD%STA(J1)%BEG_TRS(1)**2 + &
     &                                  VTD%STA(J1)%BEG_TRS(2)**2 + &
     &                                  VTD%STA(J1)%BEG_TRS(3)**2   ) 
!
! ----------- We do it in order to cheat SOTID_PRE
!
              C_STA(J1)   = VTD%STA(J1)%IVS_NAME
              R_TRS(1,J1) = VTD__REA 
              R_TRS(2,J1) = 0.0D0
              R_TRS(3,J1) = 0.0D0
         END IF
!
         VTD%STA(J1)%STATUS = VTD__STRT 
 410  CONTINUE 
!
! --- Compute time independent argumetns for solid Earth tides computation
!
      CALL ERR_PASS  ( IUER, IER )
      CALL SOTID_PRE ( VTD%L_STA, C_STA, R_TRS, VTD%TIDCNF_STD, VTD%STATID, &
     &                 IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2182, IUER, 'VTD_SETRANGE', 'Error in '// &
     &         'an attempt to compute time independent arguments for '// &
     &         'further computation of displacements caused by solid '// &
     &         'Earth tides' )
           RETURN
      END IF
!
      DO 440 J4=1,VTD%L_STA
         DO 450 J5=1,VTD__M_PSF
            IF ( ILEN(VTD%CONF%POSVAR_FIL(J5)) .EQ. 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL VTD_POSVAR_INIT ( VTD, J4, J5, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 2183, IUER, 'VTD_SETRANGE', 'Error in '// &
     &                    'an attempt to initialize position variations '// &
     &                    'determined in the file '// &
     &                     VTD%CONF%POSVAR_FIL(J5)(1:I_LEN(VTD%CONF%POSVAR_FIL(J5)))// &
     &                    ' for station '//VTD%STA(J4)%IVS_NAME )
                      RETURN
                 END IF
            END IF
 450     CONTINUE 
 440  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_SETRANGE
