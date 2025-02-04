      FUNCTION   GET_LST ( MJD, TAI, STA_NAM, SUR, VTD, S_ANG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_LST  computes local siderial for a given station.
! *                                                                      *
! *  ### 06-JUN-2010     GET_LST   v1.0 (c)  L. Petrov  2018.06.01  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'ners_local.i'
      REAL*8     GET_LST
      REAL*8     S_ANG
      INTEGER*4  MJD, IUER
      REAL*8     TAI
      CHARACTER  STA_NAM*(*)
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  PREC_CODE, NUT_CODE, NUT_GDS, EROT_COMPAT
      PARAMETER  ( PREC_CODE   = PREC__CAPITAINE2005 )
      PARAMETER  ( NUT_CODE    = NUT__MHB2000 )
      PARAMETER  ( NUT_GDS     = NUT__GDS_YES  )


      PARAMETER  ( EROT_COMPAT = VTD__NONE    )
!
      REAL*8     TIM_MOM, S_ANG_RATE
      REAL*8     TRS_TO_CRS(3,3), TRS_TO_CRS_DER1(3,3), &
     &           TRS_TO_CRS_DER2(3,3), PTRS_TO_CRS_DEOP(3,3,3), &
     &           DPTRS_TO_CRS_DEOP(3,3,3)
      INTEGER*4  J1, J2, J3, J4, IND_STA, IPAR, L_PAR, KNOT, ITURN, IER
      CHARACTER  NERS_CONFIG_FILE*128, STR*128
      REAL*8     TW, WALL_TIMER 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: FSPL8 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
!
      GET_LST = -1.D30
      IND_STA = 0
      DO 410 J1=1,SUR%L_STA
         IF ( SUR%STA(J1)%NAME == STA_NAM ) IND_STA = J1
 410  CONTINUE 
      IF ( IND_STA == 0 ) THEN
           CALL ERR_LOG ( 1741, IUER, 'GET_LST', 'Station '// &
     &          STA_NAM(1:I_LEN(STA_NAM))//' was not scheduled' )
           RETURN 
      END IF
      CALL GETENVAR ( 'NERS_CONFIG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           NERS_CONFIG_FILE = 'NERS_CONFIG'
         ELSE
           NERS_CONFIG_FILE = NERS__CONFIG
      END IF
!
! --- Let us initialize NERS
!
      CALL ERR_PASS  ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG_FILE, VTD%NERS, &
     &             (VTD%MJD_BEG - J2000__MJD)*86400.0D0 + VTD%TAI_BEG - VTD__EOP_TIM_MAR, &
     &             (VTD%MJD_END - J2000__MJD)*86400.0D0 + VTD%TAI_END + VTD__EOP_TIM_MAR, &
     &              IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1654, IUER, 'SUR_SKED_CONF', 'Error in an attempt '// &
     &         'to initialize NERS data structure' )
           RETURN 
      END IF
!
      VTD%MOM%MJD = MJD
      VTD%MOM%TAI = TAI
!
      TIM_MOM = ( VTD%MOM%MJD - J2000__MJD)*86400.0D0 + VTD%MOM%TAI
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( VTD%NERS, TIM_MOM, 'xpol',  1, L_PAR, VTD%MOM%XPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1744, IUER, 'GET_LST', 'Error in computing '// &
     &          'xpol using NERS on epoch '//MJDSEC_TO_DATE( MJD, TAI, -2 ) )
           RETURN 
      ENDIF
      VTD%MOM%XPL = ARCSEC__TO__RAD*VTD%MOM%XPL
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( VTD%NERS, TIM_MOM, 'ypol',  1, L_PAR, VTD%MOM%YPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1745, IUER, 'GET_LST', 'Error in computing '// &
     &          'ypol using NERS on epoch '//MJDSEC_TO_DATE( MJD, TAI, -2 ) )
           RETURN 
      ENDIF
      VTD%MOM%YPL = ARCSEC__TO__RAD*VTD%MOM%YPL
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( VTD%NERS, TIM_MOM, 'ut1mtai', 1, L_PAR, VTD%MOM%UT1_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1746, IUER, 'GET_LST', 'Error in computing '// &
     &          'ut1mtai using NERS on epoch '//MJDSEC_TO_DATE( MJD, TAI, -2 ) )
           RETURN 
      ENDIF
!    
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_ERM_NA ( 1, 0, PREC_CODE, NUT_CODE, NUT_GDS, EROT_COMPAT, &
     &                  VTD%NERS, MJD, TAI, &
     &                  VTD%MOM%XPL, VTD%MOM%YPL, VTD%MOM%UT1_M_TAI, &
     &                  0.0D0, 0.0D0, 0.0D0, &
     &                  S_ANG, S_ANG_RATE, &
     &                  0, 0.0D0, 0.0D0, &
     &                  TRS_TO_CRS, TRS_TO_CRS_DER1, &
     &                  TRS_TO_CRS_DER2, PTRS_TO_CRS_DEOP, DPTRS_TO_CRS_DEOP, IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1747, IUER, 'GET_LST', 'Error in computing '// &
     &          'matrix of the Earth rotation on epochs '// &
     &          MJDSEC_TO_DATE( MJD, TAI, -2 ) )
           RETURN 
      ENDIF
!
      GET_LST = S_ANG + SUR%STA(IND_STA)%LONG ! + 56.0D0/86400.0D0 ! plus empirical correction
      ITURN = IDINT ( GET_LST/PI2 )
      GET_LST = GET_LST - PI2*ITURN
      S_ANG = S_ANG - PI2*ITURN
      IF ( S_ANG < 0.0 ) S_ANG = S_ANG + PI2
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  GET_LST  !#!  
