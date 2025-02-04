      PROGRAM    EOP_FCS_TEST
! ************************************************************************
! *                                                                      *
! *   Program  EOP_FCS_TEST
! *                                                                      *
! * ### 15-APR-2016    EOP_FCS_TEST   v1.1 (c) L. Petrov 31-AUG-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
!
      TYPE     ( MALO__EOP_TYPE ) :: EOP, EOP_SAVE
      TYPE     ( MALO__TYPE     ), POINTER :: MAL(:)
      TYPE     ( NERS__TYPE     ) :: NERS, NERS_SAVE
      CHARACTER  FILHEO*128, FILOUT*128, DATE_BEG*24, DATE_END*24, STR*128
      REAL*8     TAI_BEG, TAI_END, TIM_MOD_STEP, TIM_RES_STEP
      REAL*8     LAT_EOP_MIN(M__EOPS), LAT_EOP_MAX(M__EOPS), UPD_EOP(M__EOPS), &
     &           LAT_AAM_MIN, LAT_AAM_MAX, UPD_AAM
      REAL*8     TIM_FCS, TIM_SER(M__EOPS), TIM_AAM, TIM_LAST_SER(M__EOPS), TIM_LAST_AAM
      REAL*8     EVEC(3,0:2), EVEC_REF(3,0:2), RMS_FCS(3)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, SHA_E3(1), SHA_E12(2), &
     &           NS, NN, KP, MJD_BEG, MJD_END, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      TIM_MOD_STEP =    6.0*3600.0D0
      TIM_RES_STEP = 3600.0D0
!
      LAT_EOP_MIN(EOPS__J) =  4.0*3600.0D0  ! - 120.0*3600.D0
      LAT_EOP_MAX(EOPS__J) = 28.0*3600.0D0  ! - 120.0*3600.D0
      UPD_EOP(EOPS__J)     =      86400.0D0
!
      LAT_EOP_MIN(EOPS__I) = 30.0*3600.0D0  ! - 120.0*3600.D0 ! - 40*86400.0D0
      LAT_EOP_MAX(EOPS__I) = 54.0*3600.0D0  ! - 120.0*3600.D0 ! - 40*86400.0D0
      UPD_EOP(EOPS__I)     =      86400.0D0
!
      LAT_EOP_MIN(EOPS__U) = 16.0*3600.0D0  ! - 120.0*3600.D0
      LAT_EOP_MAX(EOPS__U) = 24.0*3600.0D0  ! - 120.0*3600.D0
      UPD_EOP(EOPS__U)     =    6*3600.0D0
!
      LAT_EOP_MIN(EOPS__R) = 32.0*3600.0D0  !   - 120.0*3600.D0 
      LAT_EOP_MAX(EOPS__R) = 60.0*3600.0D0  !   - 120.0*3600.D0 
      UPD_EOP(EOPS__R)     =      86400.0D0
!
      LAT_EOP_MIN(EOPS__F) =  7.0*86400.0D0   
      LAT_EOP_MAX(EOPS__F) = 14.0*86400.0D0
      UPD_EOP(EOPS__F)     =  7.0*86400.0D0
!
      LAT_EOP_MIN(EOPS__S) = 10.0*86400.0D0
      LAT_EOP_MAX(EOPS__S) = 20.0*86400.0D0
      UPD_EOP(EOPS__S)     = 10.0*86400.0D0
!
      LAT_EOP_MIN(EOPS__C) = 30.0*86400.0D0 ! - 40.0*86400.0D0 
      LAT_EOP_MAX(EOPS__C) = 25.0*86400.0D0 ! - 40.0*86400.0D0 
      UPD_EOP(EOPS__C)     =  1.0*86400.0D0 ! - 40.0*86400.0D0 
!
      LAT_EOP_MIN(EOPS__L) = 30.0*86400.0D0 ! - 40.0*86400.0D0 
      LAT_EOP_MAX(EOPS__L) = 25.0*86400.0D0 ! - 40.0*86400.0D0 
      UPD_EOP(EOPS__L)     =  1.0*86400.0D0 ! - 40.0*86400.0D0 
!
      LAT_AAM_MIN          = -60.0*3600.0D0
      LAT_AAM_MAX          = -52.0*3600.0D0
      UPD_AAM              =   9.0*3600.0D0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   lat_eop_min(eops__j) = lat_eop_min(eops__j) + 40*24.0*3600.0d0
   lat_eop_max(eops__j) = lat_eop_max(eops__j) + 40*24.0*3600.0d0
   lat_eop_min(eops__i) = 18.0*3600.0d0
   lat_eop_max(eops__i) = 30.0*3600.0d0
!
      CALL EOP_INIT ( EOP )
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: eop_fcs_test config_file date_beg date_end'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, EOP%CNF_FILE )
           CALL GETARG ( 2, DATE_BEG     )
           CALL GETARG ( 3, DATE_END     )
      END IF 
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TAI_BEG, IUER  )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 3601, -2, 'EOP_FCS_TEST', 'Error in parsing '// &
     &         'date_beg argument' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TAI_END, IUER  )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 3602, -2, 'EOP_FCS_TEST', 'Error in parsing '// &
     &         'date_ebd1 argument' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3604, -2, 'EOP_FCS_TEST', 'Error in an attempt '// &
     &         'to allocate memory for object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3605, -2, 'EOP_FCS_TEST', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL PARSE_EOP_CONF ( EOP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3606, -2, 'EOP_FCS_TEST', 'Error in parsing '// &
     &         'configuration file '//EOP%CNF_FILE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_LOAD_EOP ( MAL(1), EOP, NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3607, -2, 'EOP_FCS_TEST', 'Error in loading '// &
     &         'the EOP object' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      EOP%IVRB = 0
      CALL EOP_FCS ( MAL(1), EOP, NERS, IUER )
      CALL NERS_FCS_RESHAPE ( NERS )
!
      EOP%IVRB = 0
!! EOP%IVRB = 52
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3608, -2, 'EOP_FCS_TEST', 'Error in an attempt '// &
     &         'to generate EOP forecaset' )
           CALL EXIT ( 1 )
      END IF
      EOP_SAVE = EOP
      NERS_SAVE = NERS
!
      NS = (MJD_END*86400.0D0 + TAI_END - MJD_BEG*86400.0D0 - TAI_BEG)/TIM_MOD_STEP
      write ( 6, * ) ' ns = ', ns ! %%%
      write ( 6, * ) 'EOPS__J: ', TIM_TO_DATE(EOP%EOPS(EOPS__J)%SER(EOP%EOPS(EOPS__J)%NP)%TIM, IUER )
      write ( 6, * ) 'EOPS__I: ', TIM_TO_DATE(EOP%EOPS(EOPS__I)%SER(EOP%EOPS(EOPS__I)%NP)%TIM, IUER) 
      write ( 6, * ) 'EOPS__C: ', TIM_TO_DATE(EOP%EOPS(EOPS__C)%SER(EOP%EOPS(EOPS__C)%NP)%TIM, IUER) 
      write ( 6, * ) ' '
!
      TIM_FCS = (MJD_BEG - J2000__MJD)*86400.0D0 + TAI_BEG
      CALL NERS_GET_EVEC ( NERS_SAVE, TIM_FCS, EVEC_REF, IUER )
!
      DO 410 J1=1,M__EOPS
         TIM_SER(J1) = TIM_FCS - ( LAT_EOP_MIN(J1) + LAT_EOP_MAX(J1) )/2.0D0
 410  CONTINUE 
      TIM_AAM = TIM_FCS + LAT_AAM_MIN
!         
      WRITE ( 6, 110 ) DATE_BEG, DATE_END
 110  FORMAT ( 'EOP forecast for epochs ', A, 2X, A )
!
      KP = 0
      RMS_FCS = 0.0D0
      DO 420 J2=1,NS
         EOP = EOP_SAVE
         NERS = NERS_SAVE
         TIM_FCS = (MJD_BEG - J2000__MJD)*86400.0D0 + TAI_BEG + (J2-1)*TIM_MOD_STEP
         DO 430 J3=1,M__EOPS
            DO 440 J4=1,EOP%EOPS(J3)%NP
               IF ( EOP%EOPS(J3)%SER(J4)%TIM > TIM_FCS - LAT_EOP_MIN(J3) ) THEN
                    EOP%EOPS(J3)%NP = J4 - 1
                    GOTO 840
               END IF
 440        CONTINUE 
 840        CONTINUE 
            TIM_LAST_SER(J3) = EOP%EOPS(J3)%SER(EOP%EOPS(J3)%NP)%TIM
 430     CONTINUE 
!
         IF ( TIM_FCS - TIM_AAM > LAT_AAM_MAX) THEN
              TIM_AAM = TIM_FCS - LAT_AAM_MIN
         END IF
         DO 450 J5=1,EOP%AAM%NP
            IF ( EOP%AAM%TIM(J5) > TIM_AAM ) THEN
                 EOP%AAM%NP = J5 - 1
                 GOTO 850
            END IF
 450     CONTINUE 
 850     CONTINUE 
         TIM_LAST_AAM = EOP%AAM%TIM(EOP%AAM%NP)
!!
!         WRITE ( 6, 210 ) J2, ( (TIM_FCS - TIM_LAST_SER(NN))/3600.0D0, NN=1,M__EOPS), &
!     &                          (TIM_FCS - TIM_LAST_AAM)/3600.0D0
! 210     FORMAT ( 'Ind: ', I4, ' Tim_eop: ',7(F6.1,1X), ' Tim_aam: ', F6.1 )
!         CALL FLUSH ( 6 )
!
!
!         WRITE ( 6, * ) 'TIM_FCS: ', TIM_TO_DATE(TIM_FCS, IUER), ' ' ,               &
!     &                               TIM_TO_DATE(TIM_LAST_SER(EOPS__J), IUER), ' ',  &
!     &                               TIM_TO_DATE(TIM_LAST_SER(EOPS__I), IUER), ' ',  &
!     &                               TIM_TO_DATE(TIM_LAST_SER(EOPS__U), IUER), ' ',  &
!     &                               TIM_TO_DATE(TIM_LAST_SER(EOPS__C), IUER), ' ',  &
!     &                               TIM_TO_DATE(TIM_LAST_AAM, IUER)  ! %%%%%%%
         IUER = -1
         CALL EOP_FCS ( MAL(1), EOP, NERS, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 3609, -2, 'EOP_FCS_TEST', 'Error in an attempt '// &
     &            'to generate EOP forecaset' )
              CALL EXIT ( 1 )
         END IF
!
         CALL NERS_FCS_RESHAPE ( NERS )
         IUER = -1
         CALL NERS_GET_EVEC ( NERS,      TIM_FCS, EVEC,     IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 3609, IUER, 'EOP_FCS_TEST', 'Error in an attempt '// &
     &            'to compute the EOP using the forecast' )
              CALL EXIT ( 1 )
         END IF
!
         CALL NERS_GET_EVEC ( NERS_SAVE, TIM_FCS, EVEC_REF, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 3610, IUER, 'EOP_FCS_TEST', 'Error in an attempt '// &
     &            'to compute the EOP using the saved forecast' )
              CALL EXIT ( 1 )
         END IF
         WRITE ( 6, 120 ) J2, NS, (J2-1)*TIM_MOD_STEP/86400.D0, &
     &                            1.0D9*(EVEC(1,0) - EVEC_REF(1,0)), &
     &                            1.0D9*(EVEC(2,0) - EVEC_REF(2,0)), &
     &                            1.0D9*(EVEC(3,0) - EVEC_REF(3,0))
 120     FORMAT ( 'Epoch: ', I4, ' ( ', I4, ' )   Tim: ', F8.3, 2X, 3(F8.3,1X) )
         DO 460 J6=1,3
            RMS_FCS(J6) = RMS_FCS(J6) + (EVEC(J6,0) - EVEC_REF(J6,0))**2
 460     CONTINUE 
         KP = KP + 1
 420  CONTINUE 
      DO 470 J7=1,3
         RMS_FCS(J7) = DSQRT ( RMS_FCS(J7)/KP )
 470  CONTINUE 
      WRITE ( 6, 130 ) 1.0D9*RMS_FCS, 1.0D9*DSQRT ( (RMS_FCS(1)**2 + RMS_FCS(2)**2)/2.0D0 )
 130  FORMAT ( 'rms: ', 33X, 3(F8.3,1X), 5X, F8.3, ' nrad' )
      CALL FCS_QUIT ( NERS )
!
      END  PROGRAM   EOP_FCS_TEST  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_FCS_RESHAPE ( NERS )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_FCS_RESHAPE
! *                                                                      *
! * ### 02-JUN-2020 NERS_FCS_RESHAPE  v1.0 (c) L. Petrov 02-JUN-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE     ) :: NERS
      REAL*8,    ALLOCATABLE :: TMP_ARR(:,:)
      INTEGER*4  SHA_E3(1), SHA_E12(2)
!
      SHA_E3 = SHAPE ( NERS%FCS%BSPL_E3 )
      ALLOCATE ( TMP_ARR(1-NERS__MDEG:NERS%FCS%NK_3,1) )
      TMP_ARR = 0.D0
      TMP_ARR(1-NERS__MDEG:SHA_E3(1)-3,1) = NERS%FCS%BSPL_E3(1:SHA_E3(1))
      DEALLOCATE ( NERS%FCS%BSPL_E3 )
      ALLOCATE   ( NERS%FCS%BSPL_E3(1-NERS__MDEG:NERS%FCS%NK_3) )
      NERS%FCS%BSPL_E3(1-NERS__MDEG:NERS%FCS%NK_3) = TMP_ARR(1-NERS__MDEG:NERS%FCS%NK_3,1)
      DEALLOCATE ( TMP_ARR )
!
      SHA_E12 = SHAPE ( NERS%FCS%BSPL_E12 )
      ALLOCATE ( TMP_ARR(1-NERS__MDEG:NERS%FCS%NK_12,2) )
      TMP_ARR = 0.D0
      TMP_ARR(1-NERS__MDEG:SHA_E12(1)-3,1) = NERS%FCS%BSPL_E12(1:SHA_E12(1),1)
      TMP_ARR(1-NERS__MDEG:SHA_E12(1)-3,2) = NERS%FCS%BSPL_E12(1:SHA_E12(1),2)
      DEALLOCATE ( NERS%FCS%BSPL_E12 )
      ALLOCATE   ( NERS%FCS%BSPL_E12(1-NERS__MDEG:NERS%FCS%NK_12,2) )
      NERS%FCS%BSPL_E12(1-NERS__MDEG:NERS%FCS%NK_12,1) = TMP_ARR(1-NERS__MDEG:NERS%FCS%NK_12,1)
      NERS%FCS%BSPL_E12(1-NERS__MDEG:NERS%FCS%NK_12,2) = TMP_ARR(1-NERS__MDEG:NERS%FCS%NK_12,2)
      DEALLOCATE ( TMP_ARR )
      RETURN
      END  SUBROUTINE  NERS_FCS_RESHAPE  !#!  
