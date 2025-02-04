      SUBROUTINE GET_PRES_MOD ( NEPC, MJD, TAI, POS_HLP, &
     &                          MJD_BEG, TAI_BEG, MJD_END, TAI_END, TIM_INT, &
     &                          DIR_HEB, HEB_G, MAL, PRES_MOD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_PRES_MOD 
! *                                                                      *
! *  ### 14-FEB-2018  GET_PRES_MOD  v1.1 (c)  L. Petrov  19-FEB-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4  NEPC, MJD(NEPC), MJD_BEG, MJD_END, IVRB, IUER 
      REAL*8     TAI_BEG, TAI_END
      TYPE       ( MALO__TYPE ) :: MAL
      TYPE       ( HEB__TYPE  ) :: HEB_G
      REAL*8     SHR
      PARAMETER  ( SHR = 0.10D0 )
      REAL*8     POS_HLP(3,NEPC), TAI(NEPC), PRES_MOD(NEPC), TIM_INT
      CHARACTER  DIR_HEB*(*)
!
      TYPE     ( HEB__TYPE  ) :: HEB_D, HEB_Q, HEB_T
      INTEGER*4  J1, J2, INDS(3), DIMS(3), IER
      REAL*8     TIM_DIF, TIM_STEP, TIM_SHR
      REAL*4     ARGS(3), PRS(2)
      CHARACTER  STR1*32, STR2*32
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4
      INTEGER*4, EXTERNAL :: IXMN4
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'NEPC= ', NEPC, ' MJD= ', MJD(1), ' TAI= ', SNGL(TAI(1)/3600.0)
      END IF
      MAL%IVRB = IVRB
!
      CALL ERR_PASS ( IUER, IER )
      STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
      STR1 = STR1(1:4)//STR1(6:7)//STR1(9:10)//'_'//STR1(12:13)//STR1(15:16)
!
      CALL ERR_PASS ( IUER, IER )
      STR2 = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
      STR2 = STR2(1:4)//STR2(6:7)//STR2(9:10)//'_'//STR2(12:13)//STR2(15:16)
      TIM_DIF = (MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG)
!
      IF ( .NOT. ASSOCIATED ( MAL%MJD_ARR ) ) THEN
           ALLOCATE ( MAL%MJD_ARR(2) )
           ALLOCATE ( MAL%TAI_ARR(2) )
      END IF
      IF ( .NOT. ASSOCIATED ( MAL%PRES_3D ) .OR. (TIM_DIF - TIM_INT) > SHR*TIM_INT ) THEN
            CALL ERR_PASS ( IUER, IER )
            CALL READ_DQT_HEB ( DIR_HEB, STR1(1:13), HEB_D, HEB_Q, HEB_T, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3821, IUER, 'GET_PRES_MOD', &
     &               'Error in reading data with meteorological parameters '// &
     &               'from directory '//TRIM(DIR_HEB)//' for date '// &
     &                STR1(1:13) )
                 CALL EXIT ( 1 )
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL MALO_INTRP_PRES ( HEB_D, HEB_T, HEB_Q, HEB_G, &
     &                             2, 1, MAL, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3822, IUER, 'GET_PRES_MOD', 'Error in expanding the '// &
     &               'pressure field into the B-spline basis for date '// &
     &                STR1(1:13) )
                 CALL EXIT ( 1 )
            END IF
            MAL%MJD_ARR(1) = HEB_D%MJD
            MAL%TAI_ARR(1) = HEB_D%UTC
            DEALLOCATE ( HEB_T%VAL )
            DEALLOCATE ( HEB_D%VAL )
            DEALLOCATE ( HEB_Q%VAL )
         ELSE
            MAL%PRES_3D(1-MALO__MDEG:MAL%NLEV,1-MALO__MDEG:MAL%NLON,1-MALO__MDEG:MAL%NLAT,1) = &
     &          MAL%PRES_3D(1-MALO__MDEG:MAL%NLEV,1-MALO__MDEG:MAL%NLON,1-MALO__MDEG:MAL%NLAT,2) 
            MAL%MJD_ARR(1) = MAL%MJD_ARR(2) 
            MAL%TAI_ARR(1) = MAL%TAI_ARR(2) 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_DQT_HEB ( DIR_HEB, STR2(1:13), HEB_D, HEB_Q, HEB_T, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3823, IUER, 'GET_PRES_MOD', &
     &         'Error in reading data with meteorological parameters '// &
     &         'from directory '//TRIM(DIR_HEB)//' for date '// &
     &          STR2(1:13) )
           CALL EXIT ( 1 )
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_INTRP_PRES ( HEB_D, HEB_T, HEB_Q, HEB_G, &
     &                       2, 2, MAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3824, IUER, 'GET_PRES_MOD', 'Error in expanding the '// &
     &         'pressure field into the B-spline basis for date '// &
     &          STR1(1:13) )
           CALL EXIT ( 1 )
      END IF
      MAL%MJD_ARR(2) = HEB_D%MJD
      MAL%TAI_ARR(2) = HEB_D%UTC
      TIM_STEP = (MAL%MJD_ARR(2) - MAL%MJD_ARR(1))*86400.0D0 + (MAL%TAI_ARR(2) - MAL%TAI_ARR(1))
      DEALLOCATE ( HEB_T%VAL )
      DEALLOCATE ( HEB_D%VAL )
      DEALLOCATE ( HEB_Q%VAL )
!
      DIMS(1) = MAL%NLEV
      DIMS(2) = MAL%NLON
      DIMS(3) = MAL%NLAT
      DO 410 J1=1,NEPC
         ARGS(1) = POS_HLP(1,J1) 
         ARGS(2) = POS_HLP(2,J1)
         ARGS(3) = POS_HLP(3,J1) 
         INDS(1) = IXMN4 ( MAL%NLEV, MAL%LEV, ARGS(1) )
         INDS(2) = IXMN4 ( MAL%NLON, MAL%LON, ARGS(2) )
         INDS(3) = IXMN4 ( MAL%NLAT, MAL%LAT, ARGS(3) )
         PRS(1) = EXP ( VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                           MAL%LEV, MAL%LON, MAL%LAT, &
     &                           MAL%PRES_3D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1) ) )
         PRS(2) = EXP ( VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                           MAL%LEV, MAL%LON, MAL%LAT, &
     &                           MAL%PRES_3D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,2) ) )
         TIM_SHR = (MJD(J1) - MAL%MJD_ARR(1))*86400.0D0 + (TAI(J1) - MAL%TAI_ARR(1))
         PRES_MOD(J1) = PRS(1) + (PRS(2) - PRS(1))*TIM_SHR/TIM_STEP
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_PRES_MOD  !#!  
