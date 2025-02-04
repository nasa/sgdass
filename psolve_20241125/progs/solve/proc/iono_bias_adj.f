      SUBROUTINE IONO_BIAS_ADJ ( DBNAME_CH, VTD, &
     &                           GIM_INFO_DIR, GIM_DTEC_DIR, BCL_FIL, BRK_FIL, &
     &                           GIM_MODE, GIM_DEG, GIM_TIM_STEP, GIM_SCALE,   &
     &                           GIM_COLLECT_INFO, GIM_EST, GIM_WRI, GIM_VERB, &
     &                           IUER )
! ************************************************************************
! *                                                                      *
! *   Routine IONO_BIAS_ADJ 
! *                                                                      *
! *  ### 06-DEC-2022  IONO_BIAS_ADJ  v1.0 (c) L. Petrov  06-DEC-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'iono_solve.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( IONO_DEL__TYPE ) :: IONO(M_OBS)
      TYPE     ( IONO__EST_TYPE ) :: IONO_EST
      CHARACTER  DBNAME_CH*(*), GIM_INFO_DIR*(*), GIM_DTEC_DIR*(*), &
     &           BCL_FIL*(*), BRK_FIL*(*)
      INTEGER*4  GIM_MODE, GIM_DEG, GIM_VERB, IUER
      REAL*8     GIM_TIM_STEP, GIM_SCALE
      LOGICAL*1  GIM_COLLECT_INFO, GIM_EST, GIM_WRI
      INTEGER*4  MP
      PARAMETER  ( MP = 128*1024 )
      CHARACTER   FIL_IONO*128, FIL_DTEC*128, EXP_NAME*16, &
     &            BUF_BRK(MP)*128, BUF_BCL(MP)*128
      INTEGER*4   N_BCL, N_BRK, NOBS, EXP_VERS, IER
!
      IF ( LOC(VTD) == 0 ) THEN
           CALL ERR_LOG ( 7211, IUER, 'IONO_BIAS_ADJ', 'Trap of internal '// &
     &         'control: VTD object is not allocated' )
           RETURN
      END IF
      IF ( VTD%IONO%STATUS_SPL .NE. VIO__COMP ) THEN
           CALL ERR_LOG ( 7212, IUER, 'IONO_BIAS_ADJ', 'Trap of '// &
     &         'internal control: coefficients of TEC maps interpolating '// &
     &         'spline have not been computed. VTD control file used: '// &
     &          VTD%CONF%CONFIG_FINAM )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( BRK_FIL, MP, BUF_BRK, N_BRK, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7212, IUER, 'IONO_BIAS_ADJ', 'Error in reading '// &
     &         'clock break file '//BRK_FIL )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( BCL_FIL, MP, BUF_BCL, N_BCL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7213, IUER, 'IONO_BIAS_ADJ', 'Error in reading '// &
     &         'baseline clock file '//BCL_FIL )
           RETURN
      END IF
!
      FIL_IONO = TRIM(GIM_INFO_DIR)//'/'//TRIM(DBNAME_CH)//'_iono.txt'
      FIL_DTEC = TRIM(GIM_DTEC_DIR)//'/'//TRIM(DBNAME_CH)//'.dtec'
!
      CALL ERR_PASS ( IUER, IER )
      CALL IONO_READ_INFO ( FIL_IONO, IONO__3BND, M_OBS, NOBS, IONO, EXP_NAME, &
     &                      EXP_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7214, IUER, 'IONO_BIAS_ADJ', 'Error in parsing '// &
     &         'pSolve iono information file '//FIL_IONO )
           CALL EXIT ( 1 )
      END IF
!
      IONO_EST%MODE      = GIM_MODE 
      IONO_EST%MDEG      = GIM_DEG
      IONO_EST%TIM_STEP  = GIM_TIM_STEP
      IONO_EST%APR_SCALE = GIM_SCALE    
!
      CALL ERR_PASS ( IUER, IER )
      CALL COMP_IONO_MOD ( FIL_IONO, FIL_DTEC, IONO, VTD, &
     &                     N_BRK, BUF_BRK, &
     &                     N_BCL, BUF_BCL, &
     &                     IONO_EST, EXP_NAME, &
     &                     GIM_VERB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7215, IUER, 'IONO_BIAS_ADJ', 'Error in computing '// &
     &         'the ionosphere bias model from VLBI data and GNSS maps for '// &
     &         'database '//EXP_NAME )
           RETURN
      END IF
!
      IF ( GIM_WRI ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL IONO_DTEC_DB_UPDATE ( EXP_NAME, EXP_VERS, FIL_DTEC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7216, IUER, 'IONO_BIAS_ADJ', 'Error in an '// &
     &              'attempt to update a database '//TRIM(EXP_NAME)// &
     &              ' for including there dTEC' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  IONO_BIAS_ADJ  !#!#
