      SUBROUTINE PIMA_BPASS ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_BPASS 
! *                                                                      *
! *  ### 23-OCT-2009   PIMA_BPASS   v1.4 (c)  L. Petrov  04-JUL-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      INTEGER*4  IUER
      CHARACTER  POL_ARR*6, STR*128
      INTEGER*4  L_NOD, L_DEG, NZO_REF
      INTEGER*4  J1, J2, J3, J4, J5, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( PIM%BPS, STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(PIM%BPS), STR )
           CALL ERR_LOG ( 6311, IUER, 'PIMA_BPASS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for PIMA '// &
     &         'internal data structures. What is going on? Do you really '// &
     &         'have so few memory?' )
           DEALLOCATE ( PIM%BPS )
           RETURN 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
           WRITE ( 6, 210 ) PIM%CONF%STA_REF
 210       FORMAT ( 'INFO PIMA_BPASS: Get observations list', &
     &              ' for the reference station ', A )
      END IF
!
! --- Initlialize VTD object
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6312, IUER, 'PIMA_BPASS', 'Error in an attempt '// &
     &         'to initialize VTD oibject' )
           RETURN 
      END IF
!
! --- Read and parse configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6313, IUER, 'PIMA_BPASS', 'Error in an attempt '// &
     &         'to read configuration file '//PIM%CONF%VTD_CONFIG_FILE )
           RETURN 
      END IF
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      CALL ERR_PASS ( IUER, IER )
      PIM%C_STA(PIM%NSTA+1) = 'GEOCENTR' 
      CALL VTD_LOAD ( VTD, PIM%NSTA+1, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                PIM%MJD_0, PIM%TAI_0 - PIM__MSCL, PIM%MJD_0, &
     &                PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) + PIM__MSCL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6314, IUER, 'PIMA_BPASS', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN 
      END IF
!
! --- Disable automatic NERS update during run
!
      VTD%NERS%CNF%AGE_FCS = 1.0D15
      VTD%NERS%CNF%AGE_SPL = 1.0D15
!
      IF ( ILEN(PIM%CONF%EPHEMERIDES_FILE) > 0 ) THEN
!
! -------- Read the ephemeride of the orbiting station
!
           PIM%NZO%FILNZO = PIM%CONF%EPHEMERIDES_FILE           
           CALL VTD_READ_NZO ( PIM%NZO%FILNZO, PIM__MNZO, PIM%NZO%L_NZO, &
     &                         PIM%NZO%MJD_ARR, PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                         PIM%NZO%VEL_ARR, PIM%NZO%NZO_NAME, PIM%NZO%OBJ_TYPE, &
     &                         PIM%NZO%CENTER_NAME, PIM%NZO%REF_NAME, &
     &                         PIM%NZO%TIM_CODE, PIM%NZO%COO_CODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6315, IUER, 'PIMA_BPASS', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
           IF ( PIM%NZO%CENTER_NAME == 'EARTH BARYCENTER' ) THEN
                NZO_REF = VTD__EME
              ELSE 
                CALL ERR_LOG ( 6316, IUER, 'PIMA_BPASS', 'Unsupported '// &
     &              'coordinate center name: '//PIM%NZO%CENTER_NAME )
                RETURN 
           END IF
!
! -------- Expand the orbiting station ephemeride into the B-spliine basis
!
           L_NOD = MIN ( PIM%NZO%L_NZO/PIMA__NZO_NOT_SCAL, VTD__M_NOD )
           L_DEG = 3
           CALL VTD_LOAD_OBJ_NZO ( PIM%NZO%NZO_NAME, VTD__ES, VTD__OR, &
     &                             NZO_REF, PIM%NZO%TIM_CODE, &
     &                             VTD, PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                             PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                             L_NOD, L_DEG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6317, IUER, 'PIMA_BPASS', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
      END IF
!
! --- Read ringe results and creates lists of observations that will be
! --- used for bandpass compuation
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_BPASS_STA_LIST ( PIM, POL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( J2, STR )
           CALL ERR_LOG ( 6318, IUER, 'PIMA_BPASS', 'Error in '// &
     &         'generating scan list for the refernce station '// &
     &          PIM%CONF%STA_REF )
           DEALLOCATE ( PIM%BPS )
           RETURN
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
           WRITE ( 6, 220 ) PIM%CONF%STA_REF
 220       FORMAT ( 'INFO PIMA_BPASS: Compute initial bandpass for the ', &
     &              'reference station ', A )
      END IF
!
! --- Perform initial bandpass computation
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_BPASS_INIT ( PIM, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( J3, STR )
           CALL ERR_LOG ( 6319, IUER, 'PIMA_BPASS', 'Error in '// &
     &         'computing initial bandpasses for the reference '// &
     &         'station '//PIM%CONF%STA_REF )
           DEALLOCATE ( PIM%BPS )
           RETURN 
      END IF
      CALL CHIN ( PIMA__LABEL(6:13), PIM%BPS%PIMA_VERS )
      IF ( PIM%BPS%PIMA_VERS < 1 .OR. PIM%BPS%PIMA_VERS > 20651012 ) THEN
           CALL ERR_LOG ( 6320, IUER, 'PIMA_BPASS', 'Trap of internal control: '// &
     &         'PIMA label '//PIMA__LABEL(15:18)//PIMA__LABEL(20:21)//PIMA__LABEL(23:24)// &
     &         ' is not an integer number. Please contact to PIMA developer' )
           RETURN 
      END IF
      PIM%BPS%STATUS = PIMA__BPASS_INIT
!
      IF ( PIM%CONF%POLAR == 'I' .AND. PIM%CONF%POLARCAL_FILE .NE. 'NO' ) THEN
           DO 410 J1=1,PIM%NSTA
              IF ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_GRDEL_STATUS == PIMA__INIT ) THEN
                   PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_GRDEL_STATUS = PIMA__LOADED
              END IF
 410       CONTINUE 
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_PBP_INIT ( PIM, VTD, PIM%BPS, POL_ARR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( J3, STR )
                CALL ERR_LOG ( 6321, IUER, 'PIMA_BPASS', 'Error in '// &
     &              'computing polarization bandpasses for '// &
     &              'the reference station '//PIM%CONF%STA_REF )
                DEALLOCATE ( PIM%BPS )
                RETURN 
           END IF
      END IF
!
      IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_ACCUM .OR. &
     &     PIM%CONF%BPS_MODE == PIMA__BPASS_FINE       ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_BPASS_ACCUM ( PIM, VTD, PIM%BPS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( J3, STR )
                CALL ERR_LOG ( 6322, IUER, 'PIMA_BPASS', 'Error in '// &
     &              'computing accumulated bandpasses for the reference '// &
     &              'station '//PIM%CONF%STA_REF )
                DEALLOCATE ( PIM%BPS )
                RETURN 
           END IF
!
           PIM%BPS%STATUS = PIMA__BPASS_ACCUM
           IF ( PIM%CONF%POLARCAL_FILE .NE. 'NO' ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL PIMA_PBP_ACCUM ( PIM, VTD, PIM%BPS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( J3, STR )
                     CALL ERR_LOG ( 6323, IUER, 'PIMA_BPASS', 'Error in '// &
     &                   'computing polarization bandpasses with '// &
     &                   'the reference station '//PIM%CONF%STA_REF )
                     DEALLOCATE ( PIM%BPS )
                     RETURN 
                END IF
           END IF
      END IF
!
      IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_FINE       ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_BPASS_FINE ( PIM, VTD, PIM%BPS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( J3, STR )
                CALL ERR_LOG ( 6324, IUER, 'PIMA_BPASS', 'Error in '// &
     &              'computing fine bandpasses for the reference '// &
     &              'station '//PIM%CONF%STA_REF )
                DEALLOCATE ( PIM%BPS )
                RETURN 
           END IF
           PIM%BPS%STATUS = PIMA__BPASS_FINE
!
           IF ( PIM%CONF%POLARCAL_FILE .NE. 'NO' ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL PIMA_PBP_FINE ( PIM, VTD, PIM%BPS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( J3, STR )
                     CALL ERR_LOG ( 6325, IUER, 'PIMA_BPASS', 'Error in '// &
     &                   'computing polarization bandpasses for '// &
     &                   'the reference station '//PIM%CONF%STA_REF )
                     DEALLOCATE ( PIM%BPS )
                     RETURN 
                END IF
           END IF
      END IF
!
      IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_ACCUM .OR. &
     &     PIM%CONF%BPS_MODE == PIMA__BPASS_FINE       ) THEN
           DO 420 J2=1,PIM%NSTA
              IF ( PIM%C_STA(J2) .NE. PIM%CONF%STA_REF ) THEN
                   CALL BPASS_STA_STAT ( J2, PIM, PIM%BPS )
              END IF
 420       CONTINUE 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_WRITE_BPASS ( PIM, PIM%BPS, PIM%CONF%BPS_SNR_MIN_ACCUM, &
     &                        PIM%CONF%BPS_SEFD_USE, PIM%CONF%BPS_DEG_AMP, &
     &                        PIM%CONF%BPS_DEG_PHS, &
     &                        PIM%CONF%BANDPASS_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6326, IUER, 'PIMA_BPASS', 'Error in '// &
     &         'an attempt to write the bandpass in the output file '// &
     &          PIM%CONF%BANDPASS_FILE ) 
           DEALLOCATE ( PIM%BPS )
           RETURN 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Bandpass output file: '// &
     &                PIM%CONF%BANDPASS_FILE(1:I_LEN(PIM%CONF%BANDPASS_FILE))
      END IF
!
      IF ( PIM%CONF%POLAR == 'I' .AND. PIM%CONF%POLARCAL_FILE .NE. 'NO' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PBP_WRI ( PIM, PIM%BPS, PIM%CONF%BPS_SNR_MIN_ACCUM, &
     &                    PIM%CONF%BPS_SEFD_USE, PIM%CONF%BPS_DEG_AMP, &
     &                    PIM%CONF%BPS_DEG_PHS, &
     &                    PIM%CONF%POLARCAL_FILE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6327, IUER, 'PIMA_BPASS', 'Error in '// &
     &              'an attempt to write the polarizaation bandpass in '// &
     &              'the output file '//PIM%CONF%POLARCAL_FILE ) 
                DEALLOCATE ( PIM%BPS )
                RETURN 
           END IF
!
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                WRITE ( 6, '(A)' ) 'Polarization bandpass output file: '// &
     &                PIM%CONF%POLARCAL_FILE(1:I_LEN(PIM%CONF%POLARCAL_FILE))
           END IF
      END IF
!
      DEALLOCATE ( PIM%BPS )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS  !#!#  
