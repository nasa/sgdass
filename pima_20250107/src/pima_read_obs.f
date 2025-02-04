      SUBROUTINE PIMA_READ_OBS ( FINAM, NEW_CONF, PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_READ_OBS
! *                                                                      *
! * ### 11-JAN-2006  PIMA_READ_OBS  v1.25  (c) L. Petrov 12-MAY-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      TYPE     ( PIM_CONF__TYPE ) :: NEW_CONF
      INTEGER*4  IUER
      CHARACTER  FINAM*(*)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, IND_REF_STA, &
     &           LUN, LN, SIZE_BYTES, NBND_SAVE, MAX_NUM_OBS, LEN_MDC, &
     &           NFRG, KPOL, IER
      CHARACTER  STR*128, FILNAM_SAVE(PIM__MFIL)*128, STAGING_DIR_SAVE*128
      LOGICAL*4  LEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT, LTM_DIF, MAX_I4
      LOGICAL*4, EXTERNAL :: SANITIZE_LOGVAR_L4
      LOGICAL*1, EXTERNAL :: SANITIZE_LOGVAR_L1
!
      NBND_SAVE = PIM%NBND ! Save the number of bands
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7401, IUER, 'PIMA_READ_OBS', 'Input file '// &
     &          FINAM(1:I_LEN(FINAM))//' was not found' )
           RETURN
      END IF
!
! --- Open input file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7402, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to open input file '//FINAM(1:I_LEN(FINAM))// &
     &         ' '//STR )
           RETURN
      END IF
!
! --- Read the format label
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7403, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to read the first record, format lable from '// &
     &         'the input file '//FINAM(1:I_LEN(FINAM))//' . The most '// &
     &         'probable reasone is that this file is not in pima binary '// &
     &         'format' )
           RETURN
      END IF
!
      IF ( STR(1:MAX(1,LN)) .NE. PIMA__FORMAT_LABEL ) THEN
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 7404, IUER, 'PIMA_READ_OBS', 'The first line '// &
     &         'of input file '//FINAM(1:I_LEN(FINAM))//' does not contain '// &
     &         'a valid label format. It contains '//STR(1:I_LEN(STR))// &
     &         ' but '//PIMA__FORMAT_LABEL//' was expected. Please re-run '// &
     &         'task load to clear this error condition' )
           RETURN
      END IF
      IF ( ILEN(PIM%CONF%STAGING_DIR) > 0 ) THEN
           DO 510 J1=1,PIM%L_FIL
              FILNAM_SAVE(J1) = PIM%FILE(J1)%NAME  
 510       CONTINUE 
      END IF
      STAGING_DIR_SAVE = PIM%CONF%STAGING_DIR
!
      SIZE_BYTES = LOC(PIM%STATUS) - LOC(PIM) + SIZEOF(PIM%STATUS)
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7405, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to read the first part of body of PIMA data '// &
     &         'structure from the input file '//FINAM )
           RETURN
      END IF
!
      IF ( PIM%NPOL == 1 ) THEN
           KPOL = 1
         ELSE
           KPOL = 2
      END IF
!
      PIM%FRI_STATUS = PIMA__UNDEF
      PIM%THE_STATUS = PIMA__UNDEF
      DO 520 J2=1,PIM%NOBS
         PIM%USE_OBS(J2) = SANITIZE_LOGVAR_L1 ( PIM%USE_OBS(J2) )
 520  CONTINUE 
!
      SIZE_BYTES = LOC(PIM%CONF%LAST_FIELD) - LOC(PIM%CONF) + &
     &             SIZEOF(PIM%CONF%LAST_FIELD) 
      CALL GETENVAR ( 'PIMAVAR_CONF_LEN', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           WRITE ( 6, * ) 'PIMA_READ_OBS CONF SIZE_BYTES= ', SIZE_BYTES
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%CONF, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7406, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to read a part CONF of PIMA data '// &
     &         'structure from the input file '//FINAM )
           RETURN
      END IF
      PIM%CONF%WARNING = SANITIZE_LOGVAR_L4 ( PIM%CONF%WARNING )
      PIM%CONF%FRIB_OBS => NULL()
      PIM%CONF%STAGING_DIR = STAGING_DIR_SAVE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_CHECK_RESUME ( NEW_CONF, PIM%CONF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7407, IUER, 'PIMA_READ_OBS', 'Checks showed '// &
     &         'that the new configuration file has changes which make '// &
     &         'impossible continuation of processing. Please run pima '// &
     &         'with the second argument begin' )
           RETURN
      END IF
      PIM%NBND = NBND_SAVE 
!
      DO 410 J1=1,PIM%NSOU
         SIZE_BYTES = SIZEOF(PIM%SOU(J1))
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%SOU(J1), LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7408, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to read a part CONF of PIMA data '// &
     &         'structure from the input file '//FINAM )
              RETURN
         END IF
 410  CONTINUE
!
      DO 420 J2=1,PIM%NSTA
         SIZE_BYTES = LOC(PIM%STA(J2)%STATUS) - LOC(PIM%STA(J2)) + &
     &                SIZEOF(PIM%STA(J2)%STATUS)
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J2), LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7409, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a STA part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
         IF ( PIM%FRG_USE == PIMA__SINGLE .OR. PIM%FRG_USE == PIMA__COMBINED ) THEN
              NFRG = PIM%NFRG
           ELSE 
              NFRG = PIM%VIRT_NFRG 
         END IF
         DO 430 J3=1,NFRG
!
! --------- Read beginning of the pcal record
!
            SIZE_BYTES = LOC(PIM%STA(J2)%PCAL(J3)%PCAL_MASK_STATUS) - LOC(PIM%STA(J2)%PCAL(J3)) + &
     &                   SIZEOF(PIM%STA(J2)%PCAL(J3)%NPOL)
            CALL ERR_PASS ( IUER, IER )
            CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J2)%PCAL(J3), LN, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7410, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &               'attempt to read a PCAL part of PIMA data '// &
     &               'structure from the input file '//FINAM )
                 RETURN
            END IF
            PIM%STA(J2)%PCAL(J3)%PCAL_MB_GRDEL = 0.0
            PIM%STA(J2)%PCAL(J3)%PCAL_SB_GRDEL = 0.0
            PIM%STA(J2)%PCAL(J3)%PCAL_GRDEL_STATUS = PIMA__UNDF
            PIM%STA(J2)%PCAL(J3)%PCAL_MASK_STATUS  = PIMA__UNDF
            PIM%STA(J2)%PCAL(J3)%PCAL_AVAIL = SANITIZE_LOGVAR_L4 ( PIM%STA(J2)%PCAL(J3)%PCAL_AVAIL )
!
            IF ( PIM%STA(J2)%PCAL(J3)%PCAL_AVAIL ) THEN
!
! -------------- Allocate dynamic memory for phase calibration data
!
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%PHAS ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%AMPL ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%PHAS_RGR ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%AMPL_RGR ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%PHAS_SCA ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%AMPL_SCA ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%PRAT_SCA ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%FREQ ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%RATE ( PIM%STA(J2)%PCAL(J3)%NO_TONES, &
     &                                       PIM%NFRQ,  PIM%STA(J2)%PCAL(J3)%NPOI, PIM%STA(J2)%PCAL(J3)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%TIME_MID_R8 (PIM%STA(J2)%PCAL(J3)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%TIME_SCA_R8 (PIM%STA(J2)%PCAL(J3)%NSCA) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%TIME_SPAN_R4(PIM%STA(J2)%PCAL(J3)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%SOU_IND(PIM%STA(J2)%PCAL(J3)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%IPOI_SCA(PIM%STA(J2)%PCAL(J3)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%PCAL(J3)%ISCA_POI(PIM%STA(J2)%PCAL(J3)%NPOI) )
!
! -------------- Read information related to phase calibration
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%PHAS, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7411, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                   'an attempt to read a PCAL(J3)%PHAS part of PIMA data '// &
     &                   'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%AMPL, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7412, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%AMPL part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%PHAS_RGR, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7413, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                   'an attempt to read a PCAL(J3)%PHAS_RGR part of PIMA data '// &
     &                   'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%AMPL_RGR, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7414, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                   'an attempt to read a PCAL(J3)%AMPL_RGR part of PIMA data '// &
     &                   'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%PHAS_SCA, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7415, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                   'an attempt to read a PCAL(J3)%PHAS_SCA part of PIMA data '// &
     &                   'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%AMPL_SCA, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7416, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                   'an attempt to read a PCAL(J3)%AMPL_SCA part of PIMA data '// &
     &                   'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%PRAT_SCA, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7417, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                   'an attempt to read a PCAL(J3)%PRAT_SCA part of PIMA data '// &
     &                   'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI, &
     &                   PIM%STA(J2)%PCAL(J3)%FREQ, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7418, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%FREQ part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', &
     &                   PIM%STA(J2)%PCAL(J3)%NO_TONES*PIM%NFRQ*PIM%STA(J2)%PCAL(J3)%NPOI*PIM%STA(J2)%PCAL(J3)%NPOL, &
     &                   PIM%STA(J2)%PCAL(J3)%RATE, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7419, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%RATE part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%PCAL(J3)%NPOI, &
     &                              PIM%STA(J2)%PCAL(J3)%IPOI_SCA, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7420, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%IPOI_SCA part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%PCAL(J3)%NPOI, &
     &                              PIM%STA(J2)%PCAL(J3)%ISCA_POI, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7421, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%IPOI_SCA part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%PCAL(J3)%NPOI, &
     &                              PIM%STA(J2)%PCAL(J3)%SOU_IND, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7422, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%SOU_IND part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%PCAL(J3)%NPOI, &
     &                              PIM%STA(J2)%PCAL(J3)%TIME_MID_R8, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7423, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%TIME_MID_R8 part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%PCAL(J3)%NSCA, &
     &                              PIM%STA(J2)%PCAL(J3)%TIME_SCA_R8, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7424, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%TIME_SCA_R8 part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', PIM%STA(J2)%PCAL(J3)%NPOI, &
     &                              PIM%STA(J2)%PCAL(J3)%TIME_SPAN_R4, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7425, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a PCAL(J3)%TIME_SPAN_R4 part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE
                 NULLIFY ( PIM%STA(J2)%PCAL(J3)%FREQ )
                 NULLIFY ( PIM%STA(J2)%PCAL(J3)%PHAS )
                 NULLIFY ( PIM%STA(J2)%PCAL(J3)%AMPL )
                 NULLIFY ( PIM%STA(J2)%PCAL(J3)%RATE )
                 NULLIFY ( PIM%STA(J2)%PCAL(J3)%TIME_MID_R8 )
                 NULLIFY ( PIM%STA(J2)%PCAL(J3)%TIME_SPAN_R4 )
                 NULLIFY ( PIM%STA(J2)%PCAL(J3)%SOU_IND )
            END IF
  430    CONTINUE 
!
         DO 440 J4=1,PIM%NFRG
            SIZE_BYTES = LOC(PIM%STA(J2)%TSYS(J4)%NPOL) - LOC(PIM%STA(J2)%TSYS(J4)) + &
     &                SIZEOF(PIM%STA(J2)%TSYS(J4)%NPOL)
            CALL ERR_PASS ( IUER, IER )
            CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J2)%TSYS(J4), LN, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7426, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &               'attempt to read a TSYS part of PIMA data '// &
     &               'structure from the input file '//FINAM )
                 RETURN
            END IF
            PIM%STA(J2)%TSYS(J4)%AVAIL = SANITIZE_LOGVAR_L4 ( PIM%STA(J2)%TSYS(J4)%AVAIL )
!
            IF ( PIM%STA(J2)%TSYS(J4)%AVAIL ) THEN
!
! -------------- Allocate dynamic memory for system temperature data
!
                 ALLOCATE ( PIM%STA(J2)%TSYS(J4)%TSYS(PIM%NFRQ,PIM%STA(J2)%TSYS(J4)%NPOI,PIM%STA(J2)%TSYS(J4)%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%TSYS(J4)%TIME_MID_R8 (PIM%STA(J2)%TSYS(J4)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%TSYS(J4)%TIME_SPAN_R4(PIM%STA(J2)%TSYS(J4)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%TSYS(J4)%AZ_R4(PIM%STA(J2)%TSYS(J4)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%TSYS(J4)%ELEV_R4(PIM%STA(J2)%TSYS(J4)%NPOI) )
                 ALLOCATE ( PIM%STA(J2)%TSYS(J4)%SOU_IND(PIM%STA(J2)%TSYS(J4)%NPOI) )
!
! -------------- Read information related to system temperature data
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%TSYS(J4)%NPOI*PIM%STA(J2)%TSYS(J4)%NPOL, &
     &                              PIM%STA(J2)%TSYS(J4)%TSYS, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7427, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a TSYS(J4)%TSYS part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%TSYS(J4)%NPOI, &
     &                              PIM%STA(J2)%TSYS(J4)%TIME_MID_R8, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7428, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a TSYS(J4)%TIME_MID_R8 part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', PIM%STA(J2)%TSYS(J4)%NPOI, &
     &                              PIM%STA(J2)%TSYS(J4)%TIME_SPAN_R4, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7429, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a TSYS(J4)%TIME_SPAN_R8 part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', PIM%STA(J2)%TSYS(J4)%NPOI, &
     &                              PIM%STA(J2)%TSYS(J4)%AZ_R4, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7430, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a TSYS(J4)%AZ_R4 part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', PIM%STA(J2)%TSYS(J4)%NPOI, &
     &                              PIM%STA(J2)%TSYS(J4)%ELEV_R4, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7431, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a TSYS(J4)%ELEV_R4 part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%TSYS(J4)%NPOI, &
     &                              PIM%STA(J2)%TSYS(J4)%SOU_IND, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7432, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a TSYS(J4)%SOU_IND part of '// &
     &                    'PIMA data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE
                 NULLIFY ( PIM%STA(J2)%TSYS(J4)%TSYS )
                 NULLIFY ( PIM%STA(J2)%TSYS(J4)%TIME_MID_R8  )
                 NULLIFY ( PIM%STA(J2)%TSYS(J4)%TIME_SPAN_R4 )
                 NULLIFY ( PIM%STA(J2)%TSYS(J4)%AZ_R4 )
                 NULLIFY ( PIM%STA(J2)%TSYS(J4)%ELEV_R4 )
                 NULLIFY ( PIM%STA(J2)%TSYS(J4)%SOU_IND )
            END IF
!
            SIZE_BYTES = LOC(PIM%STA(J2)%STMO(J4)%STATUS) - LOC(PIM%STA(J2)%STMO(J4)) + &
     &                   SIZEOF(PIM%STA(J2)%STMO(J4)%STATUS)
            CALL ERR_PASS ( IUER, IER )
            CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J2)%STMO(J4), LN, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7433, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &               'attempt to read a STMO part of PIMA data '// &
     &               'structure from the input file '//FINAM )
                 RETURN
            END IF
            IF ( PIM%STA(J2)%STMO(J4)%N_OPA > 0 ) THEN
!
! -------------- Allocate dynamic memory for system temperature data
!
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%IND_SCA(PIM%STA(J2)%STMO(J4)%N_OPA) )
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%TIM(PIM%STA(J2)%STMO(J4)%N_OPA) )
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%EL(PIM%STA(J2)%STMO(J4)%N_OPA) )
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%AZ(PIM%STA(J2)%STMO(J4)%N_OPA) )
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%OPA(PIM%STA(J2)%STMO(J4)%N_OPA,PIM%NFRQ) )
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%STMO(J4)%N_OPA, &
     &                              PIM%STA(J2)%STMO(J4)%IND_SCA, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7434, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%OPA part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%STMO(J4)%N_OPA, &
     &                              PIM%STA(J2)%STMO(J4)%TIM, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7435, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TIM part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%STMO(J4)%N_OPA, &
     &                              PIM%STA(J2)%STMO(J4)%EL, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7436, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%EL part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%STMO(J4)%N_OPA, &
     &                              PIM%STA(J2)%STMO(J4)%AZ, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7437, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%AZ part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%STMO(J4)%N_OPA, &
     &                              PIM%STA(J2)%STMO(J4)%OPA, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7438, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%OPA part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE 
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%IND_SCA )
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%TIM     )
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%EL      )
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%AZ      )
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%OPA     )
            END IF
!
            IF ( PIM%STA(J2)%STMO(J4)%N_TAT > 0 ) THEN
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%TAT(PIM%STA(J2)%STMO(J4)%N_OPA,PIM%NFRQ) )
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%STMO(J4)%N_TAT, &
     &                              PIM%STA(J2)%STMO(J4)%TAT, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7439, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TAT part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE 
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%TAT )
            END IF
!
            IF ( PIM%STA(J2)%STMO(J4)%N_TREC > 0 ) THEN
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%TREC(PIM%STA(J2)%STMO(J4)%N_TREC,PIM%NFRQ) )
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%STMO(J4)%N_TREC, &
     &                              PIM%STA(J2)%STMO(J4)%TREC, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7440, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TREC part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE 
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%TREC )
            END IF
!
            IF ( PIM%STA(J2)%STMO(J4)%N_TSPI > 0 ) THEN
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%TSPI(PIM%STA(J2)%STMO(J4)%N_TSPI,PIM%NFRQ) )
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%STMO(J4)%N_TSPI, &
     &                              PIM%STA(J2)%STMO(J4)%TSPI, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7441, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TSPI part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE 
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%TSPI )
            END IF
!
            IF ( PIM%STA(J2)%STMO(J4)%N_TSYS > 0 ) THEN
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%TSYS_CLN(PIM%STA(J2)%STMO(J4)%N_TSYS,PIM%NFRQ,KPOL) )
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%STMO(J4)%N_TSYS*KPOL, &
     &                              PIM%STA(J2)%STMO(J4)%TSYS_CLN, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7442, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TSYS_CLN part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%TSYS_MOD(PIM%STA(J2)%STMO(J4)%N_TSYS,PIM%NFRQ,KPOL) )
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%STMO(J4)%N_TSYS*KPOL, &
     &                              PIM%STA(J2)%STMO(J4)%TSYS_MOD, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7443, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TSYS_MOD part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE 
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%TSYS_CLN )
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%TSYS_MOD )
            END IF
!
            IF ( PIM%STA(J2)%STMO(J4)%TSRAT_AVAIL ) THEN
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*2, PIM%STA(J2)%STMO(J4)%TSRAT, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7444, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TSRAT part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
            END IF
!
            IF ( PIM%STA(J2)%STMO(J4)%N_TTOA > 0 ) THEN
                 ALLOCATE ( PIM%STA(J2)%STMO(J4)%TTOA(PIM%STA(J2)%STMO(J4)%N_TTOA,PIM%NFRQ) )
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J2)%STMO(J4)%N_TTOA, &
     &                              PIM%STA(J2)%STMO(J4)%TTOA, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7445, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a STA(J2)%STMO(J4)%TTOA part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE 
                 NULLIFY ( PIM%STA(J2)%STMO(J4)%TTOA )
            END IF
!
            SIZE_BYTES = LOC(PIM%STA(J2)%GAIN(J4)%NTAB) - LOC(PIM%STA(J2)%GAIN(J4)) + &
     &                   SIZEOF(PIM%STA(J2)%GAIN(J4)%NTAB)
            CALL ERR_PASS ( IUER, IER )
            CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J2)%GAIN(J4), LN, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7446, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &               'attempt to read a GAIN part of PIMA data '// &
     &               'structure from the input file '//FINAM )
                 RETURN
            END IF
            PIM%STA(J2)%GAIN(J4)%AVAIL = SANITIZE_LOGVAR_L4 ( PIM%STA(J2)%GAIN(J4)%AVAIL )
!
            IF ( PIM%STA(J2)%GAIN(J4)%AVAIL ) THEN
!
! -------------- Allocate dynamic memory for gain information
!
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%TYP  (PIM%NFRQ,PIM%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%NTERM(PIM%NFRQ,PIM%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%X_TYP(PIM%NFRQ,PIM%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%Y_TYP(PIM%NFRQ,PIM%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%X_VAL(PIM%NFRQ,PIM%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%Y_VAL(0:PIM%STA(J2)%GAIN(J4)%NTAB,PIM%NFRQ,PIM%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%GAIN (0:PIM%STA(J2)%GAIN(J4)%NTAB,PIM%NFRQ,PIM%NPOL) )
                 ALLOCATE ( PIM%STA(J2)%GAIN(J4)%SENS (PIM%NFRQ,PIM%NPOL) )
!
! -------------- Read information related to gain
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                                         PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                              PIM%STA(J2)%GAIN(J4)%TYP, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7447, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%TYP part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                                         PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                              PIM%STA(J2)%GAIN(J4)%NTERM, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7448, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%NTERM part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                                         PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                              PIM%STA(J2)%GAIN(J4)%X_TYP, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7449, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%X_TYP part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                                         PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                              PIM%STA(J2)%GAIN(J4)%Y_TYP, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7450, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%Y_TYP part of PIMA '// &
     &                    'data structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                                         PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                              PIM%STA(J2)%GAIN(J4)%X_VAL, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7451, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%X_VAL part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                              (PIM%STA(J2)%GAIN(J4)%NTAB+1)* &
     &                              PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                              PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                              PIM%STA(J2)%GAIN(J4)%Y_VAL, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7452, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%Y_VAL part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', &
     &                              (PIM%STA(J2)%GAIN(J4)%NTAB+1)* &
     &                               PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                               PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                               PIM%STA(J2)%GAIN(J4)%GAIN, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7453, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%GAIN part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL RDBIN_ARRAY ( LUN, 'R4', PIM%STA(J2)%GAIN(J4)%NFRQ* &
     &                                         PIM%STA(J2)%GAIN(J4)%NPOL, &
     &                              PIM%STA(J2)%GAIN(J4)%SENS, LN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7454, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                    'an attempt to read a GAIN(J4)%SENS part of PIMA data '// &
     &                    'structure from the input file '//FINAM )
                      RETURN
                 END IF
               ELSE 
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%TYP   )
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%NTERM )
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%X_TYP )
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%Y_TYP )
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%X_VAL )
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%Y_VAL )
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%GAIN  )
                 NULLIFY ( PIM%STA(J2)%GAIN(J4)%SENS  )
            END IF
 440     CONTINUE 
!
         SIZE_BYTES = LOC(PIM%STA(J2)%CABLE%CABLE_SIGN) - LOC(PIM%STA(J2)%CABLE) + &
     &                    SIZEOF(PIM%STA(J2)%CABLE%CABLE_SIGN)
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J2)%CABLE, LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7455, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a CABLE part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
         PIM%STA(J2)%CABLE%CAB_AVAIL  = SANITIZE_LOGVAR_L4 ( PIM%STA(J2)%CABLE%CAB_AVAIL )
         IF ( PIM%STA(J2)%CABLE%CAB_AVAIL ) THEN
!
! ----------- Read information related to cable calibration
!
              ALLOCATE ( PIM%STA(J2)%CABLE%TIM_CAB(PIM%STA(J2)%CABLE%NPOI) )
              ALLOCATE ( PIM%STA(J2)%CABLE%CAB_DEL(PIM%STA(J2)%CABLE%NPOI) )
!
              CALL ERR_PASS    ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%CABLE%NPOI, &
     &                              PIM%STA(J2)%CABLE%TIM_CAB, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7456, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a PIM%STA(J2)%CABLE%TIM_CAB '// &
     &                 'part of PIMA data structure from the input '// &
     &                 'file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS    ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%CABLE%NPOI, &
     &                              PIM%STA(J2)%CABLE%CAB_DEL, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7457, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a PIM%STA(J2)%CABLE%CAB_DEL '// &
     &                 'part of PIMA data structure from the input '// &
     &                 'file '//FINAM )
                   RETURN
              END IF
            ELSE
              NULLIFY ( PIM%STA(J2)%CABLE%TIM_CAB )
              NULLIFY ( PIM%STA(J2)%CABLE%CAB_DEL )
         END IF
!
         SIZE_BYTES = LOC(PIM%STA(J2)%WEATHER%NPOI) - LOC(PIM%STA(J2)%WEATHER) + &
     &                SIZEOF(PIM%STA(J2)%WEATHER%NPOI)
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J2)%WEATHER, LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7458, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a WEATHER part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN 
         END IF
         PIM%STA(J2)%WEATHER%AVAIL = SANITIZE_LOGVAR_L4 ( PIM%STA(J2)%WEATHER%AVAIL )
!
         IF ( PIM%STA(J2)%WEATHER%AVAIL ) THEN
              ALLOCATE ( PIM%STA(J2)%WEATHER%TIME_BEG(PIM%STA(J2)%WEATHER%NPOI), &
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%WEATHER%NPOI, STR )
                   CALL ERR_LOG ( 7459, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WEATHER%PRES '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WEATHER%TIME_END(PIM%STA(J2)%WEATHER%NPOI), &
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%WEATHER%NPOI, STR )
                   CALL ERR_LOG ( 7460, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WEATHER%PRES '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WEATHER%PRES(PIM%STA(J2)%WEATHER%NPOI), &
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%WEATHER%NPOI, STR )
                   CALL ERR_LOG ( 7461, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WEATHER%PRES '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WEATHER%TEMP(PIM%STA(J2)%WEATHER%NPOI), &
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%WEATHER%NPOI, STR )
                   CALL ERR_LOG ( 7462, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WEATHER%TEMP '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WEATHER%HUMID(PIM%STA(J2)%WEATHER%NPOI), &
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%WEATHER%NPOI, STR )
                   CALL ERR_LOG ( 7463, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WEATHER%HUMID '// &
     &                  'object' )
                   RETURN
              END IF
!             
! ----------- Read information related to weather
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%WEATHER%NPOI, &
     &                           PIM%STA(J2)%WEATHER%TIME_BEG, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7464, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WEATHER%TIME_BEG part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%WEATHER%NPOI, &
     &                           PIM%STA(J2)%WEATHER%TIME_END, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7465, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WEATHER%TIME_END part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%WEATHER%NPOI, &
     &                           PIM%STA(J2)%WEATHER%PRES, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7466, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WEATHER%PRES part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%WEATHER%NPOI, &
     &                           PIM%STA(J2)%WEATHER%TEMP, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7467, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WEATHER%TEMP part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%WEATHER%NPOI, &
     &                           PIM%STA(J2)%WEATHER%HUMID, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7468, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to red a WEATHER%HUMID part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
            ELSE 
              NULLIFY ( PIM%STA(J2)%WEATHER%TIME_END )
              NULLIFY ( PIM%STA(J2)%WEATHER%TEMP  )
              NULLIFY ( PIM%STA(J2)%WEATHER%HUMID )
         END IF
!
         IF ( PIM%STA(J2)%L_MOD > 0 ) THEN
!
! ----------- Allocate memory for a priori model for the J2-th station
!
              ALLOCATE ( PIM%STA(J2)%MOD(PIM%STA(J2)%L_MOD), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( PIM%STA(J2)%L_MOD*SIZEOF(PIM%STA(J2)%MOD(1)), STR )
                   CALL ERR_LOG ( 7469, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MOD '// &
     &                  'object' )
                   RETURN
              END IF
!
! ----------- Read a priori model for the J2-th station
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'B1', &
     &                           PIM%STA(J2)%L_MOD*SIZEOF(PIM%STA(J2)%MOD(1)), &
     &                           PIM%STA(J2)%MOD, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7470, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to READ a MOD part of PIMA data '// &
     &                 'structure into the output file '//FINAM )
                   RETURN 
              END IF
         END IF
!
! ------ Read arrays related to the MDC data structure
!
         CALL ERR_PASS ( IUER, IER )
         LEN_MDC = LOC(PIM%STA(J2)%MDC%CLO_MODEL_STATUS) - &
     &             LOC(PIM%STA(J2)%MDC%CLO_OFFS) + &
     &             SIZEOF(PIM%STA(J2)%MDC%CLO_MODEL_STATUS)
         CALL RDBIN_ARRAY ( LUN, 'B1', LEN_MDC, PIM%STA(J2)%MDC%CLO_OFFS, &
     &                      LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7471, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &            'an attempt to write a MDC%CLO_OFFS part of PIMA '// &
     &            'data structure into the output file '//FINAM )
              RETURN
         END IF
!
!
! -------Fix incorrect CLO_MODEL_STATUS that may happend if processing old
! -------pim files
!
         IF ( PIM%STA(J2)%MDC%CLO_MODEL_STATUS == PIMA__MDC_GLO_INCLUDED .OR. &
     &        PIM%STA(J2)%MDC%CLO_MODEL_STATUS == PIMA__MDC_GLO_EXCLUDED .OR. &
     &        PIM%STA(J2)%MDC%CLO_MODEL_STATUS == PIMA__MDC_SCA_INCLUDED .OR. &
     &        PIM%STA(J2)%MDC%CLO_MODEL_STATUS == PIMA__MDC_SCA_EXCLUDED .OR. &
     &        PIM%STA(J2)%MDC%CLO_MODEL_STATUS == PIMA__UNDF                  ) THEN
              CONTINUE 
            ELSE
              PIM%STA(J2)%MDC%CLO_MODEL_STATUS = PIMA__MDC_SCA_INCLUDED 
         END IF
!
         IF ( PIM%STA(J2)%L_MDC > 0 ) THEN
!
! ----------- Allocate memory for elements of the structure MDC
!
              ALLOCATE ( PIM%STA(J2)%MDC%IND_SOU(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7472, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%IND_SOU '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%MDC%TIME_CEN(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7473, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%TIME_CEN '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%MDC%CLOCK_OFFSET(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7474, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%CLOCK_OFFSET '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%MDC%CLOCK_RATE(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7475, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%CLOCK_RATE '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%MDC%ATMO_DELAY(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7476, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%ATMO_DELAY '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%MDC%ATMO_RATE(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7477, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%ATMO_RATE '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%MDC%GDELAY(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7478, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%GDELAY '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%MDC%GRATE(PIM%STA(J2)%L_MDC), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_MDC, STR )
                   CALL ERR_LOG ( 7479, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%MDC%GRATE '// &
     &                  'object' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'I4', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%IND_SOU, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7480, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%IND_SOU part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%TIME_CEN, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7481, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%TIME_CEN part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%CLOCK_OFFSET, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7482, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%CLOCK_OFFSET part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%CLOCK_RATE, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7483, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%CLOCK_RATE part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%ATMO_DELAY, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7484, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%ATMO_DELAY part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%ATMO_RATE, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7485, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%ATMO_RATE part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%GDELAY, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7486, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%GDELAY part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_MDC, &
     &                           PIM%STA(J2)%MDC%GRATE, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7487, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a MDC%GRATE part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN 
              END IF
         END IF
!
         IF ( PIM%STA(J2)%L_WVR > 0 ) THEN
              ALLOCATE ( PIM%STA(J2)%WVR%TIM_ARR(PIM%STA(J2)%L_WVR), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_WVR, STR )
                   CALL ERR_LOG ( 7488, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WVR%TIM_ARR '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WVR%DEL_ARR(PIM%STA(J2)%L_WVR), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_WVR, STR )
                   CALL ERR_LOG ( 7489, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WVR%DEL_ARR '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WVR%DEL_ERR(PIM%STA(J2)%L_WVR), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_WVR, STR )
                   CALL ERR_LOG ( 7490, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WVR%ERR_ARR '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WVR%EL_ARR(PIM%STA(J2)%L_WVR), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_WVR, STR )
                   CALL ERR_LOG ( 7491, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WVR%EL_ARR '// &
     &                  'object' )
                   RETURN
              END IF
!
              ALLOCATE ( PIM%STA(J2)%WVR%AZ_ARR(PIM%STA(J2)%L_WVR), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%STA(J2)%L_WVR, STR )
                   CALL ERR_LOG ( 7492, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                  'dynamic memory for PIM%STA(J2)%WVR%AZ_ARR '// &
     &                  'object' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_WVR, &
     &                           PIM%STA(J2)%WVR%TIM_ARR, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7493, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WVR%TIM_ARR part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_WVR, &
     &                           PIM%STA(J2)%WVR%DEL_ARR, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7494, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WVR%DEL_ARR part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_WVR, &
     &                           PIM%STA(J2)%WVR%DEL_ERR, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7495, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WVR%DEL_ARR part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_WVR, &
     &                           PIM%STA(J2)%WVR%EL_ARR, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7496, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WVR%EL_ARR part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', PIM%STA(J2)%L_WVR, &
     &                           PIM%STA(J2)%WVR%AZ_ARR, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7497, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WVR%AZ_ARR part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', 1, PIM%STA(J2)%WVR%HEI_WVR, LN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7498, IUER, 'PIMA_READ_OBS', 'Error in '// &
     &                 'an attempt to read a WVR%HEI_WVR part of PIMA '// &
     &                 'data structure from the input file '//FINAM )
                   RETURN
              END IF
         END IF
 420  CONTINUE
!
      SIZE_BYTES = SIZEOF(PIM%FRQ(1,1))
      DO 450 J5=1,PIM%NFRG
         DO 460 J6=1,PIM%NFRQ
            CALL ERR_PASS    ( IUER, IER )
            CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%FRQ(J6,J5), LN, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7499, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &               'attempt to read a PIM%FRQ part of PIMA data '// &
     &               'structure from the input file '//FINAM )
                RETURN
            END IF
 460     CONTINUE
 450  CONTINUE
!
      DO 470 J7=1,PIM%NSCA
         SIZE_BYTES = LOC(PIM%SCA(J7)%SCAN_NAME) - LOC(PIM%SCA(J7)) + &
     &                SIZEOF(PIM%SCA(J7)%SCAN_NAME)
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%SCA(J7), LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7500, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a SCA part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
!
         IF ( ALLOCATED ( PIM%SCA(J7)%AUT_IND ) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J7, STR )
              CALL ERR_LOG ( 7501, IUER, 'PIMA_READ_OBS', 'Attempt to '// &
     &            'allocate dynamic memory for AUT_IND in the '// &
     &             STR(1:I_LEN(STR))//' th scan, but memory has '// &
     &            'already been allocated ' )
              RETURN
         END IF
         IF ( ALLOCATED ( PIM%SCA(J7)%OBS_IND ) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J7, STR )
              CALL ERR_LOG ( 7502, IUER, 'PIMA_READ_OBS', 'Attempt to '// &
     &            'allocate dynamic memory for OBS_IND in the '// &
     &             STR(1:I_LEN(STR))//' th scan, but memory has '// &
     &            'already been allocated ' )
              RETURN
         END IF
!
         ALLOCATE ( PIM%SCA(J7)%AUT_IND(PIM%SCA(J7)%NSTA), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7503, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &            'allocate dynamic memory for AUT_IND at a scan' )
              RETURN
         END IF
!
         ALLOCATE ( PIM%SCA(J7)%OBS_IND(PIM%SCA(J7)%NBAS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( PIM%SCA(J7)%NBAS*SIZEOF(PIM%SCA(J7)%OBS_IND), STR )
              CALL ERR_LOG ( 7504, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for OBS_IND at a scan' )
              RETURN
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'I4', PIM%SCA(J7)%NSTA, PIM%SCA(J7)%AUT_IND, &
     &                      LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7505, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a SCA%AUT_IND part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'I4', PIM%SCA(J7)%NBAS, PIM%SCA(J7)%OBS_IND, &
     &                      LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7506, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a SCA%OBS_IND part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
 470  CONTINUE
!
      ALLOCATE ( PIM%FILE(PIM%L_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%L_FIL*SIZEOF(PIM%FILE(1)), STR )
           CALL ERR_LOG ( 7507, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for PIMA internal data structures for keeping '// &
     &         'inforamtion about UV-files and their keywords. '// &
     &         'What is going on? Do you really have so few memory?' )
           RETURN
      END IF
!
      DO 480 J8=1,PIM%L_FIL
         SIZE_BYTES = LOC(PIM%FILE(J8)%STATUS) - LOC(PIM%FILE(J8)) + &
     &                SIZEOF(PIM%FILE(J8)%STATUS)
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%FILE(J8), LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7508, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a FILE part of PIMA data '// &
     &            'structure from the output file '//FINAM )
              RETURN
         END IF
!
         ALLOCATE ( PIM%FILE(J8)%KEY(PIM%FILE(J8)%M_KWD,PIM%FILE(J8)%L_HDR), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( LEN(PIM%FILE(J8)%KEY(1,1))*PIM%FILE(J8)%M_KWD* &
     &                     PIM%FILE(J8)%L_HDR, STR )
              CALL ERR_LOG ( 7509, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for keys' )
              RETURN
         END IF
!
         SIZE_BYTES = PIM%FILE(J8)%L_HDR*PIM%FILE(J8)%M_KWD* &
     &                LEN(PIM%FILE(J8)%KEY(1,1))
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%FILE(J8)%KEY, LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7510, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a PIM%FILE%KEY part of PIMA data '// &
     &            'structure from the output file '//FINAM )
              RETURN
         END IF
 480  CONTINUE
!
      ALLOCATE ( PIM%UV_IND(PIM%NUV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NUV*SIZEOF(PIM%UV_IND(1)), STR )
           CALL ERR_LOG ( 7511, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for UV indexes' )
           RETURN
      END IF
!
      SIZE_BYTES = PIM%NUV*SIZEOF(PIM%UV_IND(1))
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%UV_IND, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7512, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to read a UV_IND part of PIMA data '// &
     &         'structure from the input file '//FINAM )
           RETURN
      END IF
!
      ALLOCATE ( PIM%OBS(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NOBS*SIZEOF(PIM%OBS(1)), STR )
           CALL ERR_LOG ( 7513, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      ALLOCATE ( PIM%AUT(PIM%NAUT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NAUT*SIZEOF(PIM%AUT(1)), STR )
           CALL ERR_LOG ( 7514, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      DO 490 J9=1,PIM%NOBS
         SIZE_BYTES = LOC(PIM%OBS(J9)%REF_FRG_INDS(PIM__MUVS)) - LOC(PIM%OBS(J9)) + &
     &                SIZEOF(PIM%OBS(J9)%REF_FRG_INDS(PIM__MUVS))
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%OBS(J9), LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7515, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a OBS part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
!
         MAX_NUM_OBS = MAX_I4 ( PIM__MUVS, PIM%OBS(J9)%NUM_EPC )
         ALLOCATE ( PIM%OBS(J9)%UV_IND(MAX_NUM_OBS,PIM%OBS(J9)%NUVS), &
     &             STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*MAX_NUM_OBS*PIM%OBS(J9)%NUVS, STR )
              CALL ERR_LOG ( 7516, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &            'allocate dynamic memory for uv data' )
              RETURN
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'I4', MAX_NUM_OBS*PIM%OBS(J9)%NUVS, &
     &                      PIM%OBS(J9)%UV_IND, LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7517, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a OBS%UV_IND part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
!
         ALLOCATE ( PIM%OBS(J9)%CORR_FLAG(MAX_NUM_OBS,PIM%NFRG), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( PIM%NOBS*SIZEOF(PIM%OBS(1)), STR )
              CALL ERR_LOG ( 7518, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &            'allocate dynamic memory for correlator flags' )
              RETURN
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'I4', MAX_NUM_OBS*PIM%NFRG, &
     &                      PIM%OBS(J9)%CORR_FLAG, LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7519, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a OBS%CORR_FLAG part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
!
         ALLOCATE ( PIM%OBS(J9)%RES_FRN(PIM%NFRQ,PIM%NBND), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%OBS(J9)%RES_FRN(PIM%NFRQ,1)*PIM%NBND, STR )
              CALL ERR_LOG ( 7520, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &            'allocate dynamic memory for residual cross correlation '// &
     &            'function' )
              RETURN
         END IF
!
! ------ Initialize fringe residuals
!
         CALL NOUT_R4 ( 2*PIM%NFRQ*PIM%NBND, PIM%OBS(J9)%RES_FRN )
         PIM%OBS(J9)%UV      => NULL()
         PIM%OBS(J9)%UV_IF   => NULL()
         PIM%OBS(J9)%UV_BAND => NULL()
         PIM%OBS(J9)%WEI_1D  => NULL()
 490  CONTINUE
!
      DO 4100 J10=1,PIM%NAUT
         SIZE_BYTES = LOC(PIM%AUT(J10)%REF_FRG_INDS(PIM__MUVS)) - LOC(PIM%AUT(J10)) + &
     &                SIZEOF(PIM%AUT(J10)%REF_FRG_INDS(PIM__MUVS))
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%AUT(J10), LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7521, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a AUT part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
!
         MAX_NUM_OBS = MAX_I4 ( PIM__MUVS, PIM%AUT(J10)%NUM_EPC )
         ALLOCATE ( PIM%AUT(J10)%UV_IND(MAX_NUM_OBS,PIM%AUT(J10)%NUVS), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( PIM%NOBS*SIZEOF(PIM%OBS(1)), STR )
              CALL ERR_LOG ( 7522, IUER, 'PIMA_READ_OBS', 'Failure to '// &
     &            'allocate dynamic memory for autocorrelation' )
              RETURN
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'I4', MAX_NUM_OBS*PIM%AUT(J10)%NUVS, &
     &                      PIM%AUT(J10)%UV_IND, LN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7523, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to read a AUT%UV_IND part of PIMA data '// &
     &            'structure from the input file '//FINAM )
              RETURN
         END IF
 4100 CONTINUE
!
      ALLOCATE ( PIM%FREQ_ARR(PIM%NCHN,PIM%NFRQ,PIM%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NCHN*PIM%NFRQ*PIM%NFRG, STR )
           CALL ERR_LOG ( 7524, IUER, 'PIMA_READ_OBS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'FREQ_ARR' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'R8', PIM%NCHN*PIM%NFRQ*PIM%NFRG, &
     &                   PIM%FREQ_ARR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7525, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to read the frequency array from the output '// & 
     &         'file '//FINAM )
           RETURN 
      END IF
!
      IND_REF_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, NEW_CONF%STA_REF )
      IF ( IND_REF_STA .LE. 0 ) THEN
           CALL ERR_LOG ( 7526, IUER, 'PIMA_READ_OBS', 'Cannot find '// &
     &         'reference station '//NEW_CONF%STA_REF//' in the '// &
     &         'station list. Please check configuration parameter '//&
     &         'STA_REF' )
           RETURN
      END IF
!
! --- Open FITS-IDI files
!
      DO 4110 J11=1,PIM%L_FIL
         CALL ERR_PASS   ( IUER, IER )
         PIM%FILE(J11)%NAME = NEW_CONF%UVFILE_NAME(J11) ! NB: the path may change
         IF ( ILEN(PIM%CONF%STAGING_DIR) > 0 ) THEN
!
! ----------- Restore the file name since in may be changed to the staging direcotry
!
              PIM%FILE(J11)%NAME = FILNAM_SAVE(J11)
         END IF
         CALL FFITS_OPEN ( PIM%FILE(J11)%NAME, PIM%FILE(J11)%FITS_DESC, 'OLD', &
     &                     IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7527, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &            'attempt to open FITS UV-file '//PIM%FILE(J11)%NAME )
              RETURN
         END IF
 4110 CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7528, IUER, 'PIMA_READ_OBS', 'Error in an '// &
     &         'attempt to close input file '//FINAM )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_READ_OBS  !#!#
