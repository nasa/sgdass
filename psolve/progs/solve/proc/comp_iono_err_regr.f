      SUBROUTINE COMP_IONO_ERR_REGR ( MJD_CEN, TAI_CEN, L_STA, L_BAS, &
     &                               C_STA, C_BAS, AVR_BAS, RMS_BAS, &
     &                               ME_BAS, VTD, ISEED, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_IONO_ERR_REGR
! *                                                                      *
! *  ### 10-APR-2022 COMP_IONO_ERR_REGR v1.2 (c) L. Petrov 11-JAN-2023 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  MJD_CEN, L_STA, L_BAS, ISEED, IUER 
      CHARACTER  C_STA(L_STA)*(*), C_BAS(L_BAS)*(*)
      REAL*8     TAI_CEN, AVR_BAS(L_BAS), RMS_BAS(L_BAS), ME_BAS(L_BAS)
      REAL*8     EPS_TIM, DUR, EL_MIN, DST_SHR
      PARAMETER  ( DUR     = 86400.0D0 )
      PARAMETER  ( EPS_TIM = 1.0D0 )
      PARAMETER  ( EL_MIN  = 5.0D0*DEG__TO__RAD )
      PARAMETER  ( DST_SHR = 0.96D0 ) 
      INTEGER*4  M_SOU, M_BUF, M_SCA, MIND
      PARAMETER  ( M_SOU = 16384 )
      PARAMETER  ( M_BUF =   256 )
      PARAMETER  ( M_SCA =   481 )
      PARAMETER  ( MIND  =   32  )
!!      PARAMETER  ( M_SCA =   8641 )
      REAL*8     RA(M_SOU), DEC(M_SOU), TIM_STEP, TAI_OBS, TAU_GR, RATE_PH, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), TIM_TAI
      REAL*8     AZ(2), EL(2), HA(2), AZ_RATE(2), EL_RATE(2), HA_RATE(2), &
     &           TEC(2), IONO_MAP(2), IONO_DEL(2), IONO_RATE(2), &
     &           LAT_GDT_PP(2), LONG_PP(2), DST_SQ, WT_DUMMY, WT(2), ME
      PARAMETER  ( TIM_STEP = DUR/(M_SCA-1) )
      CHARACTER  C_SOU(M_SOU)*8, SOU_CAT(M_SOU+64)*128, BUF(M_BUF)*256, &
     &           FILSOU*128, FILVTD*128, STR*32, VTD_CONF_FILE*128
      INTEGER*4  L_SOU, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           IB_SOU, I_SOU, I_STA(2), IP, NS, NB, MJD_OBS, IONO_STATUS(2), &
     &           I_BAS, L_PAR, LIND, IND(2,MIND), DEBUG_LEVEL, IER 
      REAL*8,    EXTERNAL :: RANDOM_NUMB, DP_VV_V, WALL_TIMER
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = 2.2D9
      OBS_TYP%FRQ_REF(2) = 8.0D9
      OBS_TYP%N_BND      = 2
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = 8.0D9
      OBS_TYP%FRQ_ION_EFF(2) = 2.3D9 
      OBS_TYP%STATUS     = VTD__BND
!
      IF ( VTD%STATUS .NE. VTD__LOAD ) THEN
           CALL ERR_LOG ( 7311, IUER, 'COMP_IONO_ERR_REGR', 'Trap of internal '// &
     &         'control: VTD object is not initialized and loaded with apriori '// &
     &         'data' )
           RETURN 
      END IF
!
      DEBUG_LEVEL = 0
      VTD_CONF_FILE = VTD%CONF%CONFIG_FINAM
!
      NS = 0
      NS = NS + 1 ; SOU_CAT(NS) = '#  Source position file format  1.0  of 2019.08.08 '
      NS = NS + 1 ;  SOU_CAT(NS)= '#  FAST'
      NS = NS + 1 ;  SOU_CAT(NS)= '#  Random source positions'
      NS = NS + 1 ;  SOU_CAT(NS)= '#'
      NS = NS + 1 ;  SOU_CAT(NS)= '#  Last update: '//GET_CDATE()
      NS = NS + 1 ;  SOU_CAT(NS)= '#'
      DO 410 J1=1,M_SOU
         RA(J1)  = RANDOM_NUMB ( ISEED, 0.0D0, PI2 )
         DEC(J1) = DASIN ( RANDOM_NUMB ( ISEED, -0.999999D0, 0.999999D0 ) )
         NS = NS + 1
         CALL CLRCH ( SOU_CAT(NS) )
         C_SOU(J1) = 'SO_'
         CALL INCH   ( J1,    C_SOU(J1)(4:8) )
         CALL CHASHR (        C_SOU(J1)(4:8) )
         CALL BLANK_TO_ZERO ( C_SOU(J1)(4:8) )
         SOU_CAT(NS)(4:11) = C_SOU(J1)
         SOU_CAT(NS)(14:23) = 'JSOU_'//C_SOU(J1)(4:8)
         CALL RH_TAT ( RA(J1),  6, SOU_CAT(NS)(25:40), IER )
         CALL RG_TAT ( DEC(J1), 4, SOU_CAT(NS)(43:57), IER )
         IF ( DEC(J1) > 0.0D0 ) SOU_CAT(NS)(43:43) = '+'
         SOU_CAT(NS)(61:) = '999.999  0  ! random source position'
 410  CONTINUE 
!
      CALL INCH ( GETPID(), FILSOU(1:8) )
      CALL CHASHR (         FILSOU(1:8) )
      CALL BLANK_TO_ZERO (  FILSOU(1:8) )
      FILSOU = '/tmp/random_sou_'//FILSOU(1:8)//'.src'
!
      CALL ERR_PASS   ( IUER, IER )
      CALL WR_TEXT ( NS, SOU_CAT, FILSOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7312, IUER, 'COMP_IONO_ERR_REGR', 'Error in an '// &
     &         'attempt to write a temporary source file '//FILSOU )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD_CONF_FILE, M_BUF, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7313, IUER, 'COMP_IONO_ERR_REGR', 'Error in an '// &
     &         'attempt to read VTD confirtation file '//VTD_CONF_FILE )
           RETURN 
      END IF
!
      DO 420 J2=1,NB
         IF ( BUF(J2)(1:19) == 'SOURCE_COORDINATES:' ) THEN
              BUF(J2)(26:) = FILSOU
         END IF
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, CHAR(32)//CHAR(9), IER )
         IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'LEAP_SECOND:' ) THEN
              BUF(J2) = 'LEAP_SECOND:             NERS'
         END IF
         IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'IONOSPHERE_SCALE:' ) THEN
              BUF(J2) = 'IONOSPHERE_SCALE:               1.000'
         END IF
 420  CONTINUE 
!
      CALL INCH   ( GETPID(), FILVTD(1:8) )
      CALL CHASHR (           FILVTD(1:8) )
      CALL BLANK_TO_ZERO (    FILVTD(1:8) )
      FILVTD = '/tmp/gen_iono_err_regr_'//FILVTD(1:8)//'.vtd'
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NB, BUF, FILVTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7314, IUER, 'COMP_IONO_ERR_REGR', 'Error in an '// &
     &         'attempt to write a temporary vtd control file '//FILVTD )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_QUIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7315, IUER, 'COMP_IONO_ERR_REGR', 'Error in an attempt to '// &
     &        'quit VTD oibject' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7316, IUER, 'COMP_IONO_ERR_REGR', 'Error in an attempt to '// &
     &        'initialize VTD oibject' )
           RETURN 
      END IF
!
! --- Read and parse configuration file
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_CONF ( FILVTD, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7317, IUER, 'COMP_IONO_ERR_REGR', 'Error in an attempt '// &
     &         'to read configuration file '//FILVTD )
           RETURN 
      END IF
      CALL UNLINK ( TRIM(FILVTD)//CHAR(0) )
!
      VTD%CONF%IVRB = 0
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_LOAD  ( VTD, L_STA, C_STA, M_SOU, C_SOU, &
     &                 MJD_CEN, TAI_CEN - DUR/2.0D0 - EPS_TIM, &
     &                 MJD_CEN, TAI_CEN + DUR/2.0D0 + EPS_TIM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7318, IUER, 'COMP_IONO_ERR_REGR', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN 
      END IF
      CALL UNLINK ( TRIM(FILSOU)//CHAR(0) )
!
      AVR_BAS = 0.0D0
      RMS_BAS = 0.0D0
      ME_BAS  = 0.0D0
!
      IB_SOU = 1
      DO 430 J3=1,M_SCA
         MJD_OBS = MJD_CEN 
         TAI_OBS = TAI_CEN - DUR/2.0D0 + (J3-1)*TIM_STEP
         IF ( TAI_OBS > 86400.0D0 ) THEN
              MJD_OBS = MJD_OBS + 1
              TAI_OBS = TAI_OBS - 86400.0D0
         END IF
         IF ( TAI_OBS > 86400.0D0 ) THEN
              MJD_OBS = MJD_OBS + 1
              TAI_OBS = TAI_OBS - 86400.0D0
         END IF
         IF ( TAI_OBS < 0.0D0 ) THEN
              MJD_OBS = MJD_OBS - 1
              TAI_OBS = TAI_OBS + 86400.0D0
         END IF
         IF ( TAI_OBS < 0.0D0 ) THEN
              MJD_OBS = MJD_OBS - 1
              TAI_OBS = TAI_OBS + 86400.0D0
         END IF
         TIM_TAI = (MJD_OBS - J2000__MJD)*86400.0D0 + TAI_OBS
!
         VTD%MOM%MJD = MJD_OBS
         VTD%MOM%TAI = TAI_OBS
         CALL ERR_PASS ( IUER, IER )
         CALL NERS_GET_EOP ( VTD%NERS, TIM_TAI, 'mat', 9, L_PAR, VTD%MOM%TRS_TO_CRS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7319, IUER, 'COMP_IONO_ERR_REGR', 'Error in an attempt '// &
     &            'to compute the Earth rotation matrix using NERS' )
              RETURN 
         END IF
!
         I_BAS  = 0
         IF ( IB_SOU > 0.75*M_SOU ) THEN
              IB_SOU = 1
         END IF
         DO 440 J4=1,L_STA-1
            DO 450 J5=J4+1,L_STA
               I_BAS = I_BAS + 1
               I_STA(1) = J4
               I_STA(2) = J5
               C_BAS(I_BAS) = C_STA(J4)//'/'//C_STA(J5)
               IP = 0
               DST_SQ = (VTD%STA(J4)%COO_TRS(1,1) - VTD%STA(J5)%COO_TRS(1,1))**2 + &
     &                  (VTD%STA(J4)%COO_TRS(2,1) - VTD%STA(J5)%COO_TRS(2,1))**2 + &
     &                  (VTD%STA(J4)%COO_TRS(3,1) - VTD%STA(J5)%COO_TRS(3,1))**2 
               IF ( DST_SQ < DST_SHR*(2.0D0*VTD__REA)**2 ) THEN
                    DO 460 J6=IB_SOU,M_SOU
                       DO 470 J7=1,2
                          CALL ERR_PASS ( IUER, IER )
                          CALL NERS_AZELHA_COMP ( VTD%NERS, TIM_TAI, VTD%STA(I_STA(J7))%COO_TRS(1,1), &
     &                                            RA(J6), DEC(J6), 'radio', AZ(J7), EL(J7), HA(J7), &
     &                                            AZ_RATE(J7), EL_RATE(J7), HA_RATE(J7), IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL ERR_LOG ( 7320, IUER, 'COMP_IONO_ERR_REGR', 'Error in an attempt '// &
     &                             'to compute azimuth and elevation' )
                               RETURN 
                          END IF
                          VTD%STA(I_STA(J7))%ELEV = EL(J7)
                          VTD%STA(I_STA(J7))%AZ   = AZ(J7)
                          IP = IP + 1
 470                   CONTINUE 
                       IF ( EL(1) > EL_MIN .AND. EL(2) > EL_MIN       ) THEN
                            I_SOU = J6
                            GOTO 860
                       END IF
 460                CONTINUE 
 860                CONTINUE 
                  ELSE
!
! ----------------- The baseline is too long, f.e. KOKEE/HARTRAO. We put something,
! ----------------- since in any case, no usable observatinos will be available
!
                    I_SOU = 1
                    AZ(1) = RANDOM_NUMB ( ISEED, 0.0D0, PI2 )
                    AZ(2) = AZ(1)
                    EL = EL_MIN
                    VTD%STA(I_STA(1))%ELEV = EL(1)
                    VTD%STA(I_STA(2))%ELEV = EL(2)
                    VTD%STA(I_STA(1))%AZ   = AZ(1)
                    VTD%STA(I_STA(2))%AZ   = AZ(2)
               END IF
!
               DO 480 J8=1,2
!
! --------------- Compute station position vector in CRS. 
! --------------- NB: we neglect aberration here
!
                  CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, VTD%STA(I_STA(J8))%COO_TRS, &
     &                                                         3, VTD%STA(I_STA(J8))%COO_CRS, IER )
                  CALL ERR_PASS ( IUER, IER )
                  CALL VTD_IONO_DELAY ( VTD, OBS_TYP, I_STA(J8), I_SOU, TEC(J8), IONO_MAP(J8), &
     &                                  IONO_DEL(J8), IONO_RATE(J8), LAT_GDT_PP(J8), LONG_PP(J8), &
     &                                  IONO_STATUS(J8), IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 7321, IUER, 'COMP_IONO_ERR_REGR', 'Error in an attempt '// &
     &                     'to compute the ionosphere path delay' )
                       RETURN 
                  END IF
 480           CONTINUE 
!
               IF ( DEBUG_LEVEL .GE. 2 ) THEN
                    CALL ERR_PASS ( IUER, IER )
                    CALL VTD_DELAY ( C_SOU(I_SOU), C_STA(J4), C_STA(J5), MJD_OBS, TAI_OBS, &
     &                              OBS_TYP, VTD, TAU_GR, RATE_PH, &
     &                              DER_DEL, DER_RAT, IER )
                    write ( 6, * ) 'j3= ', int2(j3), ' j4= ', int2(j4), ' j5= ', int2(j5), ' i_sou = ', int2(i_sou)
                    write ( 6, * ) 'NERS: iono= ', sngl(iono_del)
                    write ( 6, * ) 'VTD:  iono= ', sngl(der_del(vtd__iono1)), sngl(der_del(vtd__iono2))
                    write ( 6, * ) ' ' ! %%
                    write ( 6, * ) 'NERS el= ', el
                    write ( 6, * ) 'VTD  el= ', der_del(VTD__ELEV1), der_del(VTD__ELEV2)
                    write ( 6, * ) 'NERS az= ', az
                    write ( 6, * ) 'VTD  az= ', der_del(VTD__AZIM1), der_del(VTD__AZIM2)
                 ELSE IF ( DEBUG_LEVEL .GE. 1 ) THEN
                    write ( 6, * ) 'j3= ', int2(j3), ' c_bas= ', c_bas(i_bas), &
     &                             ' iono: ', sngl(iono_del(1)/iono_map(1)), sngl(iono_del(2)/iono_map(2)), &
     &                             ' tec: ', sngl(tec(1)),   sngl(tec(2))  ! %%%
                 ELSE IF ( DEBUG_LEVEL .GE. 3 ) THEN
                    IF ( C_STA(J4) == 'HARTRAO ' .AND. C_STA(J5) == 'NYALES20' ) THEN
                         STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER )
                         WRITE ( 6, 220 ) MJD_OBS, TAI_OBS, &
     &                                    IONO_DEL(1)/IONO_MAP(1)/(8.64/8.00)**2, &
     &                                    IONO_DEL(2)/IONO_MAP(2)/(8.64/8.00)**2
 220                     FORMAT ( 'MJD= ', I5, ' TAI= ', F8.1, ' Iono_zen: ', 1PD13.6, 1X, 1PD13.6 )
                    END IF
               END IF
               IB_SOU = I_SOU + 1
!
               ME = (IONO_MAP(1) + IONO_MAP(2))/2.0D0
               ME_BAS(I_BAS)  = ME_BAS(I_BAS)  + ME
               AVR_BAS(I_BAS) = AVR_BAS(I_BAS) +  (IONO_DEL(2) - IONO_DEL(1))/ME
               RMS_BAS(I_BAS) = RMS_BAS(I_BAS) + ((IONO_DEL(2) - IONO_DEL(1))/ME)**2
 450        CONTINUE 
 440     CONTINUE 
         IF ( DEBUG_LEVEL .GE. 1 ) THEN
              WRITE ( 6, * ) 'Scan: ', J3 ; CALL FLUSH ( 6 )
         END IF
 430  CONTINUE 
!
      DO 490 J9=1,L_BAS
         ME_BAS(J9)  = ME_BAS(J9)/M_SCA
         AVR_BAS(J9) = AVR_BAS(J9)/M_SCA
         RMS_BAS(J9) = DSQRT ( RMS_BAS(J9)/M_SCA - AVR_BAS(J9)**2 )
 490  CONTINUE 
      VTD%CONF%CONFIG_FINAM = VTD_CONF_FILE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_IONO_ERR_REGR  !#!#
