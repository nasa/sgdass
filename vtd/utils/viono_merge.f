#define SERIAL
      SUBROUTINE VIONO_MERGE ( FILG, FILR, FILO, VIOG, VIOR, VIOO, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  VIONO_MERGE merges two ionospheric models global          *
! *   (1st) and the regional (2nd).                                      *
! *                                                                      *
! *  ### 09-FEB-2018  VIONO_MERGE  v1.0 (c)  L. Petrov  26-FEB-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      CHARACTER  FILG*(*), FILR*(*), FILO*(*)
      TYPE     ( IONO__TYPE ) :: VIOG, VIOR, VIOO
      INTEGER*4  IVRB, IUER
      CHARACTER  STR*128
      REAL*8     EPS, WEIR, WEIG, CNS_DR2_SIG, CNS_DR2
      PARAMETER  ( EPS = 0.1D0 )
      PARAMETER  ( WEIR = 1.0D0 )
      PARAMETER  ( WEIG = 1.0D-3 )
      PARAMETER  ( CNS_DR2_SIG = 1.0D3 )
      LOGICAL*1  FL_ERROR
      REAL*8     LAT_STEP, LON_STEP, UTC_R_OBS, UTC_G_OBS, LATO, LONO
      REAL*8     X1
      REAL*4       FILL_VAL
      PARAMETER  ( FILL_VAL = -1.0 )
      REAL*8     VIOR_LON_MAX, VIOR_LAT_MAX
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           MJD_R_OBS, MJD_G_OBS, NR_LON, NR_LAT, NR_TIM, IR_LAT, IR_LON, NP, LO, &
     &           IRG_LON_MIN, IRG_LON_MAX, IRG_LAT_MIN, IRG_LAT_MAX, IND_TIM, &
     &           IND_LON_G, IND_LAT_G, IND_LON_R, IND_LAT_R, NEPC_OUT, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      VIOO%HEADER = VIOR%HEADER
      VIOO%HEADER%NLON   = IDNINT(PI2/VIOO%HEADER%LON_STEP) + 1
      VIOO%HEADER%NLAT   = IDNINT(PI__NUM/VIOO%HEADER%LAT_STEP) + 1
      VIOR_LON_MAX = (VIOR%HEADER%LON_MIN + (VIOR%HEADER%NLON-1)*VIOR%HEADER%LON_STEP)
      VIOR_LAT_MAX = (VIOR%HEADER%LAT_MIN + (VIOR%HEADER%NLAT-1)*VIOR%HEADER%LAT_STEP)
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'Global   model: NEPC=     ', VIOG%HEADER%NEPC, ' Npoi= ', VIOG%HEADER%NEPC*VIOG%HEADER%NLON*VIOG%HEADER%NLAT
           WRITE ( 6, * ) 'Global   model: NLON=     ', VIOG%HEADER%NLON, ' Lon_range: ', &
     &                                                  SNGL(VIOG%HEADER%LON_MIN/DEG__TO__RAD), &
     &                                                  SNGL((VIOG%HEADER%LON_MIN + (VIOG%HEADER%NLON-1)*VIOG%HEADER%LON_STEP)/DEG__TO__RAD) 
           WRITE ( 6, * ) 'Global   model: NLAT=     ', VIOG%HEADER%NLAT, ' Lat_range: ', &
     &                                                  SNGL(VIOG%HEADER%LAT_MIN/DEG__TO__RAD), &
     &                                                  SNGL((VIOG%HEADER%LAT_MIN + (VIOG%HEADER%NLAT-1)*VIOG%HEADER%LAT_STEP)/DEG__TO__RAD) 
           WRITE ( 6, * ) 'Global   model: Lon/Lat/Tim_step= ', SNGL(VIOG%HEADER%LON_STEP/DEG__TO__RAD), &
     &                                                          SNGL(VIOG%HEADER%LAT_STEP/DEG__TO__RAD), &
     &                                                          SNGL(VIOG%HEADER%TIM_STEP)
!
           WRITE ( 6, * ) 'Regional model: NEPC=     ', VIOR%HEADER%NEPC, ' Npoi= ', VIOR%HEADER%NEPC*VIOR%HEADER%NLON*VIOR%HEADER%NLAT
           WRITE ( 6, * ) 'Regional model: NLON=     ', VIOR%HEADER%NLON, ' Lon_range: ', &
     &                                                  SNGL(VIOR%HEADER%LON_MIN/DEG__TO__RAD), &
     &                                                  SNGL(VIOR_LON_MAX/DEG__TO__RAD)
!!     &                                                  SNGL((VIOR%HEADER%LON_MIN + (VIOR%HEADER%NLON-1)*VIOR%HEADER%LON_STEP)/DEG__TO__RAD) 
           WRITE ( 6, * ) 'Regional model: NLAT=     ', VIOR%HEADER%NLAT, ' Lat_range: ', &
     &                                                  SNGL(VIOR%HEADER%LAT_MIN/DEG__TO__RAD), &
     &                                                  SNGL(VIOR_LAT_MAX/DEG__TO__RAD)
!!     &                                                  SNGL((VIOR%HEADER%LAT_MIN + (VIOR%HEADER%NLAT-1)*VIOR%HEADER%LAT_STEP)/DEG__TO__RAD) 
           WRITE ( 6, * ) 'Regional model: Lon/Lat/Tim_step= ', SNGL(VIOR%HEADER%LON_STEP/DEG__TO__RAD), &
     &                                                          SNGL(VIOR%HEADER%LAT_STEP/DEG__TO__RAD), &
     &                                                          SNGL(VIOR%HEADER%TIM_STEP)
           WRITE ( 6, * ) 'Output   model: NLON=     ', VIOO%HEADER%NLON, ' NLAT= ', VIOO%HEADER%NLAT
      END IF
!
      VIOO%HEADER%LON_MIN  = VIOG%HEADER%LON_MIN 
      VIOO%HEADER%LAT_MIN  = VIOG%HEADER%LAT_MIN 
      VIOO%HEADER%LON_STEP = VIOR%HEADER%LON_STEP
      VIOO%HEADER%LAT_STEP = VIOR%HEADER%LAT_STEP
      VIOO%HEADER%TIM_STEP = VIOR%HEADER%TIM_STEP
      VIOO%HEADER%NEPC     = 1
      VIOO%HEADER%NLON     = IDNINT(PI2/VIOR%HEADER%LON_STEP) + 1
      VIOO%HEADER%NLAT     = IDNINT(PI__NUM/VIOR%HEADER%LAT_STEP) + 1
!
      ALLOCATE ( VIOO%TEC_VAL(VIOO%HEADER%NLON,VIOO%HEADER%NLAT,1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*VIOO%HEADER%NLON*VIOO%HEADER%NLAT*VIOO%HEADER%NEPC, STR )
           CALL ERR_LOG ( 4511, IUER, 'VIONO_MERGE', 'Failure to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                   'dynamic memory for TEC spline' )
           RETURN 
      END IF
      NEPC_OUT = VIOR%HEADER%NEPC
      NR_TIM = IDNINT ( VIOG%HEADER%TIM_STEP/VIOR%HEADER%TIM_STEP )
      NP = VIOR%HEADER%NEPC/NR_TIM - 1
      MJD_G_OBS = VIOR%HEADER%MJD_BEG
      UTC_G_OBS = VIOR%HEADER%UTC_BEG
      MJD_R_OBS = VIOR%HEADER%MJD_BEG
      UTC_R_OBS = VIOR%HEADER%UTC_BEG
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   write ( 6, * ) 'nr_tim = ', nr_tim, ' np= ', np, ' npr= ', vior%header%nepc ! %%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%w
      LO = 0
      IND_TIM = 0 
      DO 410 J1=1,NP
         FL_ERROR = .FALSE.
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_GET_IONO ( FILG, MJD_G_OBS, UTC_G_OBS, 1, VIOG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              STR = MJDSEC_TO_DATE ( MJD_G_OBS, UTC_G_OBS, IER )
              CALL ERR_LOG ( 4513, IUER, 'VIONO_MERGE', 'Error in reading '// &
     &            'global TEC from file '//TRIM(FILG)//' on epoch '//STR(1:19) )
              RETURN 
         END IF
         DO 420 J2=1,NR_TIM
            IF ( IND_TIM > NEPC_OUT ) THEN
                 IND_TIM = IND_TIM + 1
                 GOTO 420
            END IF
            CALL ERR_PASS ( IUER, IER )
            CALL VTD_GET_IONO ( FILR, MJD_R_OBS, UTC_R_OBS, NR_TIM, VIOR, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR ) 
                 STR = MJDSEC_TO_DATE ( MJD_R_OBS, UTC_R_OBS, IER )
                 CALL ERR_LOG ( 4513, IUER, 'VIONO_MERGE', 'Error in reading '// &
     &               'regional TEC from file '//TRIM(FILR)//' on epoch '//STR(1:19) )
                 RETURN 
            END IF
            VIOO%TEC_VAL = FILL_VAL
!            VIOO%TEC_VAL = 20.0
!  write ( 6, * ) 'vior_top:     ',  VIOR%TEC_VAL(1:VIOR%HEADER%NLON,VIOR%HEADER%NLAT,1) ! %%%%
!  write ( 6, * ) 'vior_top1:    ',  VIOR%TEC_VAL(1:VIOR%HEADER%NLON,VIOR%HEADER%NLAT-1,1) ! %%%%
!  write ( 6, * ) 'vior_bottom1: ',  VIOR%TEC_VAL(1:VIOR%HEADER%NLON,2,1) ! %%%%
!  write ( 6, * ) 'vior_bottom:  ',  VIOR%TEC_VAL(1:VIOR%HEADER%NLON,1,1) ! %%%%
            DO 430 J3=1,VIOO%HEADER%NLAT
               LATO = VIOO%HEADER%LAT_MIN + (J3-1)*VIOO%HEADER%LAT_STEP
               IND_LAT_G = IDNINT ( (LATO - VIOG%HEADER%LAT_MIN)/VIOG%HEADER%LAT_STEP ) + 1
               IND_LAT_R = IDNINT ( (LATO - VIOR%HEADER%LAT_MIN)/VIOR%HEADER%LAT_STEP ) + 1
               DO 440 J4=1,VIOO%HEADER%NLON
                  LONO = VIOO%HEADER%LON_MIN + (J4-1)*VIOO%HEADER%LON_STEP
                  IND_LON_G = IDNINT ( (LONO - VIOG%HEADER%LON_MIN)/VIOG%HEADER%LON_STEP ) + 1
                  IND_LON_R = IDNINT ( (LONO - VIOR%HEADER%LON_MIN)/VIOR%HEADER%LON_STEP ) + 1
!!  if ( j3 == 8 ) write ( 6, * ) ' lono = ', sngl(lono/deg__to__rad), ' j4= ', j4, ' ind_lon_r= ', ind_lon_r ! %%%%
                  VIOO%TEC_VAL(J4,J3,1) = VIOG%TEC_VAL(IND_LON_G,IND_LAT_G,1)
                  IF ( IND_LON_R .GE. 1 .AND. IND_LON_R .LE. VIOR%HEADER%NLON .AND. &
     &                 IND_LAT_R .GE. 1 .AND. IND_LAT_R .LE. VIOR%HEADER%NLAT       ) THEN
                       VIOO%TEC_VAL(J4,J3,1) = VIOR%TEC_VAL(IND_LON_R,IND_LAT_R,1)
                  END IF
 440           CONTINUE 
!!               VIOO%TEC_VAL(VIOO%HEADER%NLON,J3,1) = VIOO%TEC_VAL(1,J3,1) 
 430        CONTINUE 
            VIOO%HEADER%MJD_BEG = MJD_R_OBS
            VIOO%HEADER%UTC_BEG = UTC_R_OBS
            IND_TIM = IND_TIM + 1
            IF ( IND_TIM == 1 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL GTI_CREATE ( VIOO, FILO, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 4514, IUER, 'VIONO_MERGE', 'Error in an '// &
     &                     'attempt to created the output file with merged '// &
     &                     'TEC model' )
                       RETURN 
                  END IF
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL GTI_APPEND ( VIOO, FILO, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4515, IUER, 'VIONO_MERGE', 'Error in an '// &
     &               'attempt to update the output file with merged '// &
     &               'TEC model' )
                 RETURN 
            END IF
!
            UTC_R_OBS = UTC_R_OBS + VIOR%HEADER%TIM_STEP
            IF ( UTC_R_OBS + EPS > 86400.0D0 ) THEN
                 MJD_R_OBS = MJD_R_OBS + 1
                 UTC_R_OBS = UTC_R_OBS - 86400.0D0
            END IF
 420     CONTINUE 
         UTC_G_OBS = UTC_G_OBS + VIOG%HEADER%TIM_STEP
         IF ( UTC_G_OBS + EPS > 86400.0D0 ) THEN
              MJD_G_OBS = MJD_R_OBS + 1
              UTC_G_OBS = UTC_R_OBS - 86400.0D0
         END IF
 410  CONTINUE 
    write ( 6, * ) 'viono_merege: the end' ; call flush ( 6 ) ! %%%%%%%%%%%%%%%%%%%%%
!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  VIONO_MERGE !#!  
