      SUBROUTINE COMP_THEO ( GVH, VTD, L_STA, C_STA, L_SOU, C_SOU, &
     &                       MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                       L_ACM, STAT_ACM, CLOOF_ACM, CLODR_ACM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_THEO 
! *                                                                      *
! *  ### 26-NOV-2005   COMP_THEO   v1.2 (c)  L. Petrov  13-SEP-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vtd.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      TYPE    ( VTD__TYPE ) :: GVH
      TYPE    ( VTD__TYPE ) :: VTD
      TYPE    ( VTD__OBS_TYPE ) :: OBS_TYP
      INTEGER*4  L_STA, L_SOU, MJD_BEG, MJD_END, L_ACM, IUER
      REAL*8     TAI_BEG, TAI_END, CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM)
      CHARACTER  C_STA(L_STA)*8, C_SOU(L_SOU)*8, STAT_ACM(M_ACM)*(*)
      REAL*8       EPSILON_0 
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
      REAL*8       JUL_YEAR__TO__SEC
      PARAMETER  ( JUL_YEAR__TO__SEC = 365.25D0*86400.0D0 ) ! Julian year
      CHARACTER  STR*80
      REAL*8     UTC_OBS, TAI_OBS, DELAY_THR, RATE_THR, PRES(2), TEMP(2), &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER)
      REAL*8     ECC_TRS(3,MAX_ARC_STA), TT, TT2, TMIN, FREQ, &
     &           TIME_BEG, TIME_END, TIME_ECC_BEG, TIME_ECC_END 
      INTEGER*4  CDP_NUMBER(MAX_ARC_STA)
      INTEGER*4, ALLOCATABLE :: OBS_TAB(:,:)
      LOGICAL*4  FL_FOUND, FL_ECC_FOUND 
      INTEGER*2  ICONT_I2, IERR_I2
      LOGICAL*2  FL_11 
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IP1, IP2, MJD_OBS, &
     &           IND_SOU, DIMS(2), IER
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LTM_DIF
!
! --- Load arrays
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_LOAD ( VTD, L_STA, C_STA, L_SOU, C_SOU, MJD_BEG, TAI_BEG, &
     &                MJD_END, TAI_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9041, IUER, 'COMP_THEO', 'Error in an attempt '// &
     &         'to load files with auxiliary information need for '// &
     &         'computation of theoretical path delay' )
           RETURN 
      END IF
!
! --- Check for a special kludge variable
!
      CALL GETENVAR ( 'SOLVE_STRUC_GAMMA', STR )
      IF ( STR == 'YES'  .OR.  STR == 'yes' ) THEN
           VTD%CONF%TEST(1) = 1
      END IF
!
      TIME_BEG = (MJD_BEG - J2000__MJD)*86400.0D0 + (TAI_BEG - 43200.0D0)
      TIME_END = (MJD_END - J2000__MJD)*86400.0D0 + (TAI_END - 43200.0D0)
!
      ALLOCATE ( OBS_TAB(3,NUMOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*NUMOBS, STR )      
           CALL ERR_LOG ( 9042, IUER, 'COMP_THEO', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for observation table' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 1, 0, 4*3*NUMOBS, DIMS(1), DIMS(2), &
     &                  OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9043, IUER, 'COMP_THEO', 'Error in '// &
     &         'getting lcode OBS_TAB' )
           DEALLOCATE ( OBS_TAB  )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_MTAI', 1, 0, 8, DIMS(1), DIMS(2), &
     &                  UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9044, IUER, 'COMP_THEO', 'Error in '// &
     &         'geting "UTC_M_TAI" lcode' )
           DEALLOCATE ( OBS_TAB  )
           RETURN 
      END IF
!
! --- Open OBSFIL file
!
      FL_11 = KBIT( PRE_IBATCH, INT2(11) ) 
      CALL SBIT ( PRE_IBATCH, INT2(11), 0 )
      CALL ACS_OBSFIL ( 'O' )
      IF ( FL_11 ) CALL SBIT ( PRE_IBATCH, INT2(11), 1 )
!
      DO 410 J1=1,NUMOBS
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
         IF ( J1 == 1 ) THEN
              TMIN = FJD + FRACTC
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'MJD_OBS ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     MJD_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9045, IUER, 'COMP_THEO', 'Error in getting '// &
     &            '"MJD_OBS" lcode' )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'UTC_OBS ', J1, 0, 8, DIMS(1), DIMS(2), &
     &                     UTC_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9046, IUER, 'COMP_THEO', 'Error in getting '// &
     &            '"UTC_OBS" lcode' )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
!
!#         CALL ERR_PASS   ( IUER, IER )
!#         CALL GVH_GLCODE ( GVH, 'ATM_PRES', J1, 1, 8, DIMS(1), DIMS(2), &
!#     &                     PRES(1), IER )
!#         IF ( IER .NE. 0 ) THEN
!#              CALL ERR_LOG ( 9047, IUER, 'COMP_THEO', 'Error in '// &
!#     &            'getting lcode AIR_TEMP for the first station' )
!#              DEALLOCATE ( OBS_TAB  )
!#              RETURN 
!#         END IF
!#!
!#! ------ Get air pressure for the first station
!#!
!#         CALL ERR_PASS   ( IUER, IER )
!#         CALL GVH_GLCODE ( GVH, 'ATM_PRES', J1, 1, 8, DIMS(1), DIMS(2), &
!#     &                     PRES(1), IER )
!#         IF ( IER .NE. 0 ) THEN
!#              CALL ERR_LOG ( 9048, IUER, 'COMP_THEO', 'Error in '// &
!#     &            'getting lcode AIR_TEMP for the first station' )
!#              DEALLOCATE ( OBS_TAB  )
!#              RETURN 
!#         END IF
!#!
!#! ------ Get air temperature for the first station
!#!
!#         CALL ERR_PASS   ( IUER, IER )
!#         CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J1, 1, 8, DIMS(1), DIMS(2), &
!#     &                     TEMP(1), IER )
!#         IF ( IER .NE. 0 ) THEN
!#              CALL ERR_LOG ( 9049, IUER, 'COMP_THEO', 'Error in '// &
!#     &            'getting lcode AIR_TEMP for the first station' )
!#              DEALLOCATE ( OBS_TAB  )
!#              RETURN 
!#         END IF
!#!
!#         CALL ERR_PASS   ( IUER, IER )
!#         CALL GVH_GLCODE ( GVH, 'ATM_PRES', J1, 2, 8, DIMS(1), DIMS(2), &
!#     &                     PRES(2), IER )
!#         IF ( IER .NE. 0 ) THEN
!#              CALL ERR_LOG ( 9051, IUER, 'COMP_THEO', 'Error in '// &
!#     &            'getting lcode ATM_PRES for the second station' )
!#              DEALLOCATE ( OBS_TAB  )
!#              RETURN 
!#         END IF
!#!
!#         CALL ERR_PASS   ( IUER, IER )
!#         CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J1, 2, 8, DIMS(1), DIMS(2), &
!#     &                     TEMP(2), IER )
!#         IF ( IER .NE. 0 ) THEN
!#              CALL ERR_LOG ( 9052, IUER, 'COMP_THEO', 'Error in '// &
!#     &            'getting lcode AIR_TEMP for the second station' )
!#              DEALLOCATE ( OBS_TAB  )
!#              RETURN 
!#         END IF
!
         PRES(1) = ATMPR(1)*100.0D0
         PRES(2) = ATMPR(2)*100.0D0
         TEMP(1) = TEMPC(1) + 273.16D0
         TEMP(2) = TEMPC(2) + 273.16D0
!
! ------ Load meteorological parameters of the first station into the VTD record
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_METEO_IN ( VTD, C_STA(OBS_TAB(2,J1)), PRES(1), TEMP(1), &
     &                       TEMP(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9050, IUER, 'COMP_THEO', 'Error in an '// &
     &            'attempt to load meteorological parameters for station '// &
     &             C_STA(OBS_TAB(2,J1)) )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
!
! ------ Load meteorological parameters of the first station into the VTD record
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_METEO_IN ( VTD, C_STA(OBS_TAB(3,J1)), PRES(2), TEMP(2), &
     &                       TEMP(2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9053, IUER, 'COMP_THEO', 'Error in an '// &
     &            'attempt to load meteorological parameters for station '// &
     &             C_STA(OBS_TAB(3,J1)) )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SOU_IND ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     IND_SOU, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9054, IUER, 'COMP_THEO', 'Error in '// &
     &            'getting lcode SOU_IND for the second station' )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
!       
         TAI_OBS = UTC_OBS - UTC_M_TAI
!
! ------ Set fields of OBS_TYP
!
         CALL SET_OBSTYP ( OBS_TYP )
!
! ------ Compute path delay
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( C_SOU(IND_SOU), C_STA(OBS_TAB(2,J1)), &
     &                    C_STA(OBS_TAB(3,J1)), MJD_OBS, TAI_OBS, OBS_TYP, &
     &                    VTD, DELAY_THR, RATE_THR, DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9055, IUER, 'COMP_THEO', 'Error in an '// &
     &            'attempt to compute VLBI time delay' )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
!
         DT = DELAY_THR*1.D6
         RT = RATE_THR
!
! ------ Delay derivative
!
         BP(1,1,1) = DER_DEL(VTD__DER_ST1X)
         BP(2,1,1) = DER_DEL(VTD__DER_ST1Y)
         BP(3,1,1) = DER_DEL(VTD__DER_ST1Z)
         BP(1,2,1) = DER_DEL(VTD__DER_ST2X)
         BP(2,2,1) = DER_DEL(VTD__DER_ST2Y)
         BP(3,2,1) = DER_DEL(VTD__DER_ST2Z)
         SP(1,1)   = DER_DEL(VTD__DER_RA)
         SP(2,1)   = DER_DEL(VTD__DER_DL)
         ROTP(1,1) = DER_DEL(VTD__DER_E2)
         ROTP(2,1) = DER_DEL(VTD__DER_E1)
         ROTP(3,1) = -DER_DEL(VTD__DER_E3)*UT1__TO__E3
         NUTP(1,1) = -( DER_DEL(VTD__DER_E1)*DSIN(VTD%MOM%S_ANG) + &
     &                  DER_DEL(VTD__DER_E2)*DCOS(VTD%MOM%S_ANG)   )* &
     &                  DSIN(EPSILON_0)
         NUTP(2,1) =  DER_DEL(VTD__DER_E1)*DCOS(VTD%MOM%S_ANG) - &
     &                DER_DEL(VTD__DER_E2)*DSIN(VTD%MOM%S_ANG)
!
         AP(1,1)   = DER_DEL(VTD__DER_AT1)
         AP(2,1)   = DER_DEL(VTD__DER_AT2)
         AGRAD_PART(1,1,1) = -DER_DEL(VTD__DER_ATN1)
         AGRAD_PART(2,1,1) = DER_DEL(VTD__DER_ATN2)
         AGRAD_PART(1,2,1) = DER_DEL(VTD__DER_ATE1)
         AGRAD_PART(2,2,1) = DER_DEL(VTD__DER_ATE2)
         AXOFP(1,1) = DER_DEL(VTD__DER_AXF1)
         AXOFP(2,1) = DER_DEL(VTD__DER_AXF2)
         FEED_ANG(1) = DER_DEL(VTD__FEED1)
         FEED_ANG(2) = DER_DEL(VTD__FEED2)
         RELP(1) = DER_DEL(VTD__DER_GAMMA)
!
! ------ Delay rate derivative
!
         BP(1,1,2) =  DER_RAT(VTD__DER_ST1X)
         BP(2,1,2) =  DER_RAT(VTD__DER_ST1Y)
         BP(3,1,2) =  DER_RAT(VTD__DER_ST1Z)
         BP(1,2,2) =  DER_RAT(VTD__DER_ST2X)
         BP(2,2,2) =  DER_RAT(VTD__DER_ST2Y)
         BP(3,2,2) =  DER_RAT(VTD__DER_ST2Z)
!
         SP(1,2)   =  DER_RAT(VTD__DER_RA)
         SP(2,2)   =  DER_RAT(VTD__DER_DL)
!
         ROTP(1,2) =  DER_RAT(VTD__DER_E2)
         ROTP(2,2) =  DER_RAT(VTD__DER_E1)
         ROTP(3,2) = -DER_RAT(VTD__DER_E3)*UT1__TO__E3
!
         NUTP(1,2) = -( DER_RAT(VTD__DER_E1)*DSIN(VTD%MOM%S_ANG) + &
     &                  DER_RAT(VTD__DER_E2)*DCOS(VTD%MOM%S_ANG)   )* &
     &                  DSIN(EPSILON_0)
         NUTP(2,2) =    DER_RAT(VTD__DER_E1)*DCOS(VTD%MOM%S_ANG) - &
     &                  DER_RAT(VTD__DER_E2)*DSIN(VTD%MOM%S_ANG)
!
         AZ(1)   = VTD%STA(OBS_TAB(2,J1))%AZ
         AZ(2)   = VTD%STA(OBS_TAB(3,J1))%AZ
         ELEV(1) = VTD%STA(OBS_TAB(2,J1))%ELEV
         ELEV(2) = VTD%STA(OBS_TAB(3,J1))%ELEV
!
         UV_COOR(1) = VTD%UV_COOR(1)
         UV_COOR(2) = VTD%UV_COOR(2)
!
         UT1_M_TAI = VTD%MOM%UT1_M_TAI
         X_POLE    = VTD%MOM%XPL/MAS__TO__RAD
         Y_POLE    = VTD%MOM%YPL/MAS__TO__RAD
         UT1_RATE  = VTD%MOM%UT1_RATE
         XP_RATE   = VTD%MOM%XPL_RATE
         YP_RATE   = VTD%MOM%YPL_RATE
!
! ------ Initializing contribution of a priori clock model
!
         TAU_ACM  = 0.0D0
         RATE_ACM = 0.0D0
         IF ( L_ACM > 0 ) THEN
!
! ----------- Adding contributions from a priori clok model to theoretical
! ----------- time delay and delay rate
!
! ----------- TT  -- proper time of the first station from TMIN epoch to the moment
! -----------        of observation (in sec)
! ----------- TT2 -- proper time of the second station from TMIN epoch to the
! -----------        moment of observation (in sec)
!
              TT  = ((FJD - TMIN) + FRACTC)*86400.0D0
              TT2 = TT + DT*1.D-6
!
! ----------- IP1 -- index of the first station of the baseine in
! -----------        the list of stations for which a priori clock model
! -----------        has been applied
!
              IP1 = LTM_DIF ( 0, L_ACM, STAT_ACM, ISITN_CHR(ISITE(1)) )
              IF ( IP1 .GT. 0 ) THEN
!
! ---------------- New correction of theoreticals due to up to date ACM
!
                   TAU_ACM  = TAU_ACM  - CLOOF_ACM(IP1) - CLODR_ACM(IP1)*TT
                   RATE_ACM = RATE_ACM - CLODR_ACM(IP1)
              END IF
!
! ----------- The same for the second station. But NB sign!
!
              IP2 = LTM_DIF ( 0, L_ACM, STAT_ACM, ISITN_CHR(ISITE(2)) )
              IF ( IP2 .GT. 0 ) THEN
                   TAU_ACM  = TAU_ACM  + CLOOF_ACM(IP2) + CLODR_ACM(IP2)*TT2
                   RATE_ACM = RATE_ACM + CLODR_ACM(IP2)
              END IF
!
! ----------- Correction for theoreticals. NB units for DT!
!
              DT = ( DT*1.D-6 + TAU_ACM  ) *1.D6
              RT = ( RT       + RATE_ACM )
         END IF
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE 
      CALL ACS_OBSFIL ( 'C' )
!
      DO 420 J2=1,L_STA
         FL_FOUND = .FALSE.
         DO 430 J3=1,VTD%L_STA
            IF ( VTD%STA(J3)%IVS_NAME == C_STA(J2) ) THEN
                 FL_FOUND = .TRUE.
                 VSITEC(1,J2) = VTD%STA(J3)%COO_TRS(1,1) 
                 VSITEC(2,J2) = VTD%STA(J3)%COO_TRS(2,1)
                 VSITEC(3,J2) = VTD%STA(J3)%COO_TRS(3,1)
!
                 VSITEV(1,J2) = VTD%STA(J3)%VEL_TRS(1)*JUL_YEAR__TO__SEC
                 VSITEV(2,J2) = VTD%STA(J3)%VEL_TRS(2)*JUL_YEAR__TO__SEC
                 VSITEV(3,J2) = VTD%STA(J3)%VEL_TRS(3)*JUL_YEAR__TO__SEC
                 CALL CLRCH (                         MONUMENTS_CHR(J2)      )
                 CALL INCH  ( VTD%STA(J3)%CDP_NUMBER, MONUMENTS_CHR(J2)(1:4) )
!
! -------------- Search for eccentricity
!
                 FL_ECC_FOUND = .FALSE.
                 DO 440 J4=1,VTD%STA(J3)%N_ECC 
                    TIME_ECC_BEG = &
     &                  ( VTD%STA(J3)%ECC_MJD_BEG(J4) - J2000__MJD)*86400.0D0 + &
     &                  ( VTD%STA(J3)%ECC_TAI_BEG(J4) - 43200.0D0)
                    TIME_ECC_END = &
     &                  ( VTD%STA(J3)%ECC_MJD_END(J4) - J2000__MJD)*86400.0D0 + &
     &                  ( VTD%STA(J3)%ECC_TAI_END(J4) - 43200.0D0)
!
                    IF ( TIME_BEG .GE. TIME_ECC_BEG .AND. &
     &                   TIME_END .LE. TIME_ECC_END       ) THEN
!
                         ECC_TRS(1,J2) = VTD%STA(J3)%ECC_TRS(1,J4)
                         ECC_TRS(2,J2) = VTD%STA(J3)%ECC_TRS(2,J4)
                         ECC_TRS(3,J2) = VTD%STA(J3)%ECC_TRS(3,J4)
!
                         FL_ECC_FOUND = .TRUE.
                    END IF
 440             CONTINUE 
                 IF ( .NOT. FL_ECC_FOUND ) THEN
                       write ( 6, * ) ' VTD%STA(J3)%N_ECC = ', VTD%STA(J3)%N_ECC 
                       write ( 6, * ) ' time_ecc_beg = ',time_ecc_beg
                       write ( 6, * ) ' time_ecc_end = ',time_ecc_end
                       write ( 6, * ) ' time_beg = ', time_beg, ' time_end = ', time_end 
                       CALL ERR_LOG ( 9056, IUER, 'COMP_THEO', &
     &                     'No eccentricity was found for station '// &
     &                      VTD%STA(J3)%IVS_NAME//' for the range [ '// &
     &                      MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -3 )//', '// &
     &                      MJDSEC_TO_DATE ( MJD_END, TAI_END, -3 )//'] ' )
                       DEALLOCATE ( OBS_TAB  )
                       RETURN 
                 END IF
!
                 CDP_NUMBER(J2) = VTD%STA(J3)%CDP_NUMBER
                 GOTO 830
            END IF
 430     CONTINUE 
 830     CONTINUE 
         IF ( .NOT. FL_FOUND ) THEN
              CALL ERR_LOG ( 9056, IUER, 'COMP_THEO', 'Coordinates '// &
     &            'of the participated station '//C_STA(J2)//' were '// &
     &            'not found in the catalogue '//VTD%CONF%FINAM_STACOO )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
!
         DO 450 J5=1,L_STA
            IF ( J5 == 1 ) THEN
                 ICONT_I2 = 1 
               ELSE 
                 ICONT_I2 = 0
            END IF
            CALL GETCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
            IF ( IERR_I2 .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( INT4(IERR_I2), STR )
                 CALL ERR_LOG ( 9057, IUER, 'COMP_THEO', 'Error in reading '// &
     &               'SITE namefile card: '//STR )
                 DEALLOCATE ( OBS_TAB  )
                 RETURN 
            END IF
            IF ( STR(6:13) == VTD%STA(J2)%IVS_NAME ) THEN
                 WRITE ( UNIT=STR(15:18), FMT='(I4)'    ) CDP_NUMBER(J2) 
                 WRITE ( UNIT=STR(29:38), FMT='(F10.4)' ) ECC_TRS(1,J2)  
                 WRITE ( UNIT=STR(40:49), FMT='(F10.4)' ) ECC_TRS(2,J2)  
                 WRITE ( UNIT=STR(51:60), FMT='(F10.4)' ) ECC_TRS(3,J2)  
                 STR(62:63) = 'XY'
            END IF
            ICONT_I2 = 4
            CALL PUTCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
            IF ( IERR_I2 .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( INT4(IERR_I2), STR )
                 CALL ERR_LOG ( 9058, IUER, 'COMP_THEO', 'Error in writing '// &
     &               'SITE namefile card: '//STR )
                 DEALLOCATE ( OBS_TAB  )
                 RETURN 
            END IF
 450     CONTINUE 
 420  CONTINUE 
!
      DO 460 J6=1,L_SOU
         FL_FOUND = .FALSE.
         DO 470 J7=1,VTD%L_SOU
            IF ( VTD%SOU(J7)%IVS_NAME == C_SOU(J6) ) THEN
                 FL_FOUND = .TRUE.
                 VSTARC(1,J6) = VTD%SOU(J7)%ALPHA
                 VSTARC(2,J6) = VTD%SOU(J7)%DELTA
                 GOTO 870
            END IF
 470     CONTINUE 
 870     CONTINUE 
         IF ( .NOT. FL_FOUND ) THEN
              CALL ERR_LOG ( 9059, IUER, 'COMP_THEO', 'Coordinates '// &
     &            'of the observed source '//C_SOU(J6)//' were '// &
     &            'not found in the catalogue '//VTD%CONF%FINAM_SOUCOO )
              DEALLOCATE ( OBS_TAB  )
              RETURN 
         END IF
 460  CONTINUE 
!
      DEALLOCATE ( OBS_TAB  )
!
! --- Write VSITEC_TIM_REF_MJD into parfil
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_THEO  !#!#
