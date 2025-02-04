      SUBROUTINE PIMA_LOAD_KJCC_MOD ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LOAD_KJCC_MOD  reads the directory of files in KJCC   *
! *   internal ascii format with VLBI interferomeric model, parses      *
! *   them, and creates MOD and MDC sub-objects for the station objects  *
! *   in the main PIMA data structure.                                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *       VTD ( PIMA__TYP ) -- Object with information related to        *
! *                            the package VTD for computation of the    *
! *                            VLBI Time Delay.                          *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ## 12-DEC-2015 PIMA_LOAD_KJCC_MOD v1.0 (c) L. Petrov 12-DEC-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE      ) :: PIM
      TYPE     ( VTD__TYPE       ) :: VTD
      TYPE     ( VTD__OBS_TYPE   ) :: OBS_TYP
      TYPE     ( PIMA__KJCC_TYPE ), POINTER :: MOD(:,:) 
      INTEGER*4  IUER
      CHARACTER  FINAM*128, MDU_FILE*128, STR*128
      INTEGER*8  DIR_DESC(16)
      REAL*8     TIM_SCAN_EPS, TIM_MOD_MIN_STEP 
      PARAMETER  ( TIM_SCAN_EPS     = 10.0D0 )
      PARAMETER  ( TIM_MOD_MIN_STEP = 0.01D0 )
      REAL*8     DEL_FIRST, DEL(0:2), MOD_STEP, TIM_POI, &
     &           TIM_OBS_BEG, TIM_OBS_END, TIM_MOD_BEG, TIM_MOD_END
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, LEV, IL, IP, IS, L_FIL, M_MOD, L_MOD(PIM__MSTA), L_ARR, &
     &           IND_STA, IND_SOU, IND_SCA, IND_MOD_BEG, IND_MOD_END, &
     &           LAST_IND_SCA, LAST_IND_STA, LAST_IND_MOD, FIRST_IND_MOD, &
     &           IND_POI, K_MOD(PIM__MSTA), LUN_MDU, IND_MOD(PIM__MSTA), IER
      LOGICAL*1  FL_FOU(2)
      REAL*8,    EXTERNAL :: FSPL8, DSPL8, D2SPL8
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, GET_UNIT, ILEN, I_LEN, &
     &                       IXMN8, LTM_DIF
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_KJCC, PIMA_COMPAR_MOD
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_KJCC, PIMA_COMPAR_MOD
#endif
!
      M_MOD = 16 * PIM%NEPC
      ALLOCATE ( MOD(M_MOD,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 16 * PIM%NEPC * PIM%NSTA * SIZEOF(MOD(1,1)), STR )
           CALL ERR_LOG ( 6411, IUER, 'PIMA_LOAD_KJCC_MOD', 'Trap of '// &
     &         'internal control: failure in an attempt to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes od dynamic memory for '// &
     &         'array MOD' )
           RETURN 
      END IF
      L_FIL = 0
      L_MOD = 0
      LEV   = 0
      DO 410 J1=1,PIM%CONF%L_INM
         DO 420 J2=1,PIM__MEPC
            IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, PIM%CONF%INTMOD_FILE(J1), FINAM )
            IF ( IS .NE. 0 ) THEN
                 CALL ERR_LOG ( 6412, IUER, 'PIMA_LOAD_KJCC_MOD', 'Error '// &
     &               'in reading input directory '// &
     &                PIM%CONF%INTMOD_FILE(J1)(1:I_LEN(PIM%CONF%INTMOD_FILE(J1)))// &
     &               ' with KJCC Inteferometric Model: '//FINAM )
                 RETURN 
            END IF
            IF ( LEV == 0 ) GOTO 820 ! End of work
!
            IL = ILEN(FINAM)
            IF ( IL < 10 ) GOTO 420
            IF ( INDEX ( FINAM, '#' ) > 0 ) GOTO 420
            IF ( FINAM(IL-5:IL)   == '.delay' .OR. &
     &           FINAM(IL-5:IL-1) == '/ANT.'  .OR. &
     &           FINAM(IL-6:IL-2) == '/ANT.'       ) THEN
!
                 L_FIL = L_FIL + 1
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_PARSE_KJCC_MOD ( PIM, FINAM, L_FIL, M_MOD, L_MOD(L_FIL), &
     &                                      MOD(1,L_FIL), PIM%UTC_MTAI, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6413, IUER, 'PIMA_LOAD_KJCC_MOD', 'Error '// &
     &                    'in parsing KJCC delay file '//FINAM )
                      RETURN 
                 END IF
            END IF
 420     CONTINUE 
 820     CONTINUE 
 410  CONTINUE 
!
      IF ( L_FIL < 1 ) THEN
           CALL ERR_LOG ( 6414, IUER, 'PIMA_LOAD_KJCC_MOD', 'Did not '// &
     &         'found delay files' )
           DEALLOCATE ( MOD )
           RETURN 
      END IF
!
!
      DO 430 J3=1,PIM%NSTA
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              WRITE ( 6, 210 ) PIM%C_STA(J3)
 210          FORMAT ( 'PIMA_LOAD_KJCC_MOD:  analyzing delays for station ', A )
              CALL FLUSH ( 6 )
         END IF
         IND_MOD(J3) = 0
         DO 440 J4=1,L_FIL
            IF ( MOD(1,J4)%STA_NAM == PIM%STA(J3)%ORIG_NAME ) THEN
                 IND_MOD(J3) = J4
              ELSE IF ( MOD(1,J4)%STA_NAM == PIM%STA(J3)%IVS_NAME ) THEN
                 IND_MOD(J3) = J4
              ELSE IF ( MOD(1,J4)%STA_NAM == PIM%STA(J3)%NAME ) THEN
                 IND_MOD(J3) = J4
            END IF
 440     CONTINUE 
         IF ( IND_MOD(J3) == 0 ) THEN
              CALL ERR_LOG ( 6415, IUER, 'PIMA_LOAD_KJCC_MOD', 'Did not find '// & 
     &             'interferometric model for station '//PIM%C_STA(J3)// &
     &             'in the delay directory '//PIM%CONF%INTMOD_FILE(1) )
              RETURN
         END IF
         CALL FOR_QSORT ( MOD(1,IND_MOD(J3)), L_MOD(IND_MOD(J3)), &
     &                    SIZEOF(MOD(1,1)), PIMA_COMPAR_KJCC )
!
         IF ( ASSOCIATED ( PIM%STA(J3)%MOD ) ) THEN
              DEALLOCATE ( PIM%STA(J3)%MOD )
         END IF
         PIM%STA(J3)%L_MOD = 0
!
! ------ Check each model epoch, whether it is associated with any scan
!
         DO 450 J5=1,L_MOD(IND_MOD(J3))
            IND_SCA = 0
            TIM_POI = (MOD(J5,IND_MOD(J3))%MJD*86400.0D0 + MOD(J5,IND_MOD(J3))%TAI) - &
     &                (PIM%MJD_0*86400.0D0 + PIM%TAI_0)
            DO 460 J6=1,PIM%NSCA
               IF ( TIM_POI > PIM%TIM_R8(PIM%SCA(J6)%TIM_IND) - TIM_SCAN_EPS .AND. &
     &              TIM_POI < PIM%TIM_R8(PIM%SCA(J6)%TIM_IND+PIM%SCA(J6)%NUM_EPC) + TIM_SCAN_EPS ) THEN
                    IND_SCA = J6
               END IF
  460       CONTINUE 
            IF ( IND_SCA > 0 ) PIM%STA(J3)%L_MOD = PIM%STA(J3)%L_MOD + 1
  450    CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
              WRITE ( 6, 220 ) PIM%C_STA(J3), L_MOD(IND_MOD(J3)), PIM%STA(J3)%L_MOD 
 220          FORMAT ( 'PIMA_LOAD_KJCC_MOD:  station ', A, ' Number of model delays: ', I6, &
     &                 ' Number of associated delays: ', I6 )
              CALL FLUSH ( 6 )
         END IF
         ALLOCATE ( PIM%STA(J3)%MOD(PIM%STA(J3)%L_MOD), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( PIM%STA(J3)%L_MOD*SIZEOF(PIM%STA(J3)%MOD), STR )
              CALL ERR_LOG ( 6416, IUER, 'PIMA_LOAD_KJCC_MOD', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for PIM%STA(J3)%MOD object' )
              RETURN
         END IF
!
         LAST_IND_SCA = 0
         LAST_IND_STA = 0
         K_MOD = 0
         MOD_STEP = (MOD(2,IND_MOD(J3))%MJD - MOD(1,IND_MOD(J3))%MJD)*86400.0D0 + &
     &              (MOD(2,IND_MOD(J3))%TAI - MOD(1,IND_MOD(J3))%TAI)
         DO 470 J7=1,L_MOD(IND_MOD(J3))
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                 WRITE  ( 6, 230 ) J7, MOD(J7,IND_MOD(J3))%STA_NAM, MOD(J7,IND_MOD(J3))%MJD, &
     &                          MOD(J7,IND_MOD(J3))%TAI
 230             FORMAT ( 'KJCC_MOD: ', I6, 2X, A, 2X, I5, 2X, F8.2 )
            END IF
!
            IND_SCA = 0
            TIM_POI = (MOD(J7,IND_MOD(J3))%MJD*86400.0D0 + MOD(J7,IND_MOD(J3))%TAI) - &
     &                (PIM%MJD_0*86400.0D0 + PIM%TAI_0)
            DO 480 J8=1,PIM%NSCA
               IF (  TIM_POI > PIM%TIM_R8(PIM%SCA(J8)%TIM_IND) - TIM_SCAN_EPS .AND. &
     &               TIM_POI < PIM%TIM_R8(PIM%SCA(J8)%TIM_IND+PIM%SCA(J8)%NUM_EPC) + TIM_SCAN_EPS ) THEN
                    IND_SCA = J8
               END IF
  480       CONTINUE 
            IF ( IND_SCA == 0 ) GOTO 470
!
            K_MOD(J3) = K_MOD(J3) + 1
            IF ( K_MOD(J3) > PIM%STA(J3)%L_MOD ) THEN
                 CALL ERR_LOG ( 6417, IUER, 'PIMA_LOAD_KJCC_MOD', 'Trap of '// &
     &            'internal control: K_MOD is in overflow for station '// &
     &             PIM%C_STA(J3) )
                 RETURN 
            END IF
            PIM%STA(J3)%MOD(K_MOD(J3))%TIM_BEG = TIM_POI
            PIM%STA(J3)%MOD(K_MOD(J3))%TIM_END = TIM_POI + MOD_STEP
!
            PIM%STA(J3)%MOD(K_MOD(J3))%SOU_IND = PIM%SCA(IND_SCA)%SOU_IND
            IF ( IND_SCA > 0 ) THEN
                 PIM%STA(J3)%MOD(K_MOD(J3))%SCANNAME = PIM%SCA(IND_SCA)%SCAN_NAME 
               ELSE 
                 PIM%STA(J3)%MOD(K_MOD(J3))%SCANNAME = 'unknown'
            END IF
    	    PIM%STA(J3)%MOD(K_MOD(J3))%SOU_IND = PIM%SCA(IND_SCA)%SOU_IND
            PIM%STA(J3)%MOD(K_MOD(J3))%GDEL_POL(0:3,1) = MOD(J7,IND_MOD(J3))%DEL(0:3)
            PIM%STA(J3)%MOD(K_MOD(J3))%PDEL_POL(0:3,1) = MOD(J7,IND_MOD(J3))%DEL(0:3)
            PIM%STA(J3)%MOD(K_MOD(J3))%PRAT_POL(0,1)   = MOD(J7,IND_MOD(J3))%DEL(1)
            PIM%STA(J3)%MOD(K_MOD(J3))%PRAT_POL(1,1)   = 2.0D0*MOD(J7,IND_MOD(J3))%DEL(2)
            PIM%STA(J3)%MOD(K_MOD(J3))%PRAT_POL(2,1)   = 3.0D0*MOD(J7,IND_MOD(J3))%DEL(3)
 470     CONTINUE 
!
         PIM%NMOD = 0
         PIM%NMOD = PIM%NMOD + PIM%STA(J3)%L_MOD
         CALL FOR_QSORT ( PIM%STA(J3)%MOD, PIM%STA(J3)%L_MOD, &
     &                    SIZEOF(PIM%STA(J3)%MOD(1)), PIMA_COMPAR_MOD )
 430  CONTINUE 
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           MDU_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdu'
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//MDU_FILE
              ELSE
                MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MDU_FILE
           END IF
           LUN_MDU = GET_UNIT()
           OPEN ( UNIT=LUN_MDU, FILE=MDU_FILE, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 6420, IUER, 'PIMA_LOAD_KJCC_MOD', &
     &              'Failure to open information file '// &
     &               MDU_FILE(1:I_LEN(MDU_FILE))//' -- error '//STR )
                RETURN
           END IF
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_LOAD_KJCC_MOD:  association delays with observations'
           CALL FLUSH ( 6 )
      END IF
      DO 4120 J12=1,PIM%NOBS
         FL_FOU(1) = .FALSE.
         FL_FOU(2) = .FALSE.
         TIM_OBS_BEG =  1.D30
         TIM_OBS_END = -1.D30
         DO 4130 J13=1,PIM%OBS(J12)%NUVS
            IF ( PIM%OBS(J12)%UV_IND(1,J13) > 0 ) THEN
                 IF ( FL_FOU(1) ) THEN
                      TIM_OBS_BEG = MIN ( TIM_OBS_BEG, &
     &                                    PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J12)%UV_IND(1,J13))%TIM_IND) )
                   ELSE
                      TIM_OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J12)%UV_IND(1,J13))%TIM_IND)
                 END IF
                 FL_FOU(1) = .TRUE.
            END IF
!
            IF ( PIM%OBS(J12)%UV_IND(PIM%OBS(J12)%NUM_EPC(J13),J13) > 0 ) THEN
                 IF ( FL_FOU(2) ) THEN
                      TIM_OBS_END = MAX ( TIM_OBS_END, &
     &                                    PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J12)%UV_IND(PIM%OBS(J12)%NUM_EPC(J13),J13))%TIM_IND) )
                   ELSE
                      TIM_OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J12)%UV_IND(PIM%OBS(J12)%NUM_EPC(J13),J13))%TIM_IND)
                 END IF
                 FL_FOU(2) = .TRUE.
            END IF
 4130    CONTINUE
!
         IF ( .NOT. FL_FOU(1) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J12, STR )
              CALL ERR_LOG ( 6420, IUER, 'PIMA_LOAD_KJCC_MOD', 'Trap of internal '// &
     &            'control: did not found a valid UV index for the 1st '// &
     &            'AP of observation #'//STR )
              RETURN
         END IF
!
         IF ( .NOT. FL_FOU(2) ) THEN
               CALL CLRCH ( STR )
               CALL INCH  ( J12, STR )
               CALL ERR_LOG ( 6420, IUER, 'PIMA_LOAD_KJCC_MOD', 'Trap of internal '// &
     &             'control: did not found a valid UV index for the last '// &
     &             'AP of observation #'//STR )
               RETURN
         END IF
!
         PIM%OBS(J12)%MOD_IND_BEG = 0
         PIM%OBS(J12)%MOD_IND_END = 0
         DO 4140 J14=1,2
            DO 4150 J15=1,PIM%STA(PIM%OBS(J12)%STA_IND(J14))%L_MOD
!
! ------------ Do not consider sources others than the source observed
! ------------ in the J12-th observation
!
               IF ( PIM%STA(PIM%OBS(J12)%STA_IND(J14))%MOD(J15)%SOU_IND .NE. &
     &              PIM%OBS(J12)%ROOT_SOU_IND ) GOTO 4150
!
               TIM_MOD_BEG = PIM%STA(PIM%OBS(J12)%STA_IND(J14))%MOD(J15)%TIM_BEG
               TIM_MOD_END = PIM%STA(PIM%OBS(J12)%STA_IND(J14))%MOD(J15)%TIM_END
!
! ------------ Check whether the model interval fits into the observation
! ------------ range.
! ------------ We consider four cases:
! ------------ a) Observation start is within the model range
! ------------ b) Observation end   is within the model range
! ------------ c) the model range is within the observation range
! ------------ d) the observation range is within the model range
!
               IF ( ( TIM_MOD_BEG < TIM_OBS_BEG + PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_MOD_END > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE       &
     &               ) .OR. &
     &             ( TIM_MOD_BEG < TIM_OBS_END + PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_MOD_END > TIM_OBS_END - PIM%CONF%AP_TOLERANCE       &
     &               ) .OR. &
     &             ( TIM_MOD_BEG > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_MOD_END < TIM_OBS_END + PIM%CONF%AP_TOLERANCE       &
     &               ) .OR. &
     &             ( TIM_OBS_BEG > TIM_MOD_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_OBS_END < TIM_MOD_END + PIM%CONF%AP_TOLERANCE       &
     &               ) ) THEN
!
                     IF ( PIM%OBS(J12)%MOD_IND_BEG(J14) == 0 ) THEN
                          PIM%OBS(J12)%MOD_IND_BEG(J14) = J15
                     END IF
!
                     PIM%OBS(J12)%MOD_IND_END(J14) = J15
                     PIM%STA(PIM%OBS(J12)%STA_IND(J14))%MOD(J15)%SCANNAME = &
     &                       PIM%SCA(PIM%OBS(J12)%SCA_IND)%SCAN_NAME
               END IF
 4150       CONTINUE
 4140    CONTINUE
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              IF ( PIM%OBS(J12)%MOD_IND_BEG(1) > 0 .AND. &
     &             PIM%OBS(J12)%MOD_IND_END(1) > 0 .AND. &
     &             PIM%OBS(J12)%MOD_IND_BEG(2) > 0 .AND. &
     &             PIM%OBS(J12)%MOD_IND_END(2) > 0       ) THEN
!
                   WRITE ( UNIT=LUN_MDU, FMT=130 ) J12, &
     &                     PIM%OBS(J12)%MOD_IND_BEG, &
     &                     PIM%OBS(J12)%MOD_IND_END, &
     &                     PIM%C_SOU(PIM%OBS(J12)%SOU_IND), &
     &                     PIM%C_STA(PIM%OBS(J12)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(J12)%STA_IND(2)), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J12)%STA_IND(1))%MOD(PIM%OBS(J12)%MOD_IND_BEG(1))%TIM_BEG, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J12)%STA_IND(1))%MOD(PIM%OBS(J12)%MOD_IND_END(1))%TIM_END, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J12)%STA_IND(2))%MOD(PIM%OBS(J12)%MOD_IND_BEG(2))%TIM_BEG, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J12)%STA_IND(2))%MOD(PIM%OBS(J12)%MOD_IND_END(2))%TIM_END, -2 )
 130               FORMAT ( 'Obs: ',I6,' MOD_BEG: ', I8, 1X, I8, &
     &                      ' MOD_END: ', I8, 1X, I8, &
     &                      ' C_SOU: ', A, &
     &                      ' C_STA: ', A8, 1X, A8, &
     &                      ' TIM_1: ', A30, 1X, A30, &
     &                      ' TIM_2: ', A21, 1X, A21 )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( IER, STR )
                        CALL ERR_LOG ( 6420, IUER, 'PIMA_LOAD_KJCC_MOD', 'Failure '// &
     &                      'in writing into file '// &
     &                       MDU_FILE(1:I_LEN(MDU_FILE))// &
     &                      ' -- error '//STR )
                        RETURN
                   END IF
                 ELSE
                   STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%OBS(J12)%TIM_BEG_IND), &
     &                                    -2 )
                   WRITE ( UNIT=LUN_MDU, FMT=135 ) J12, &
     &                     PIM%OBS(J12)%MOD_IND_BEG, &
     &                     PIM%OBS(J12)%MOD_IND_END, &
     &                     PIM%C_SOU(PIM%OBS(J12)%SOU_IND), &
     &                     PIM%C_STA(PIM%OBS(J12)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(J12)%STA_IND(2)), &
     &                     STR(1:22)
 135               FORMAT ( 'NO MODEL for obs ', I6, &
     &                      ' MOD_BEG: ', I4, 1X, I4, &
     &                      ' MOD_END: ', I4, 1X, I4, &
     &                      ' C_SOU: ', A, &
     &                      ' C_STA: ', A8, 1X, A8, &
     &                      ' TIM: ', A22 )
              END IF
         END IF
 4120 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           CLOSE ( UNIT=LUN_MDU )
           WRITE ( 6, '(A)' ) 'PIMA_LOAD_KJCC_MOD:  Finished delay processing'
           CALL FLUSH ( 6 )
      END IF
!
      DEALLOCATE ( MOD )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_LOAD_KJCC_MOD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PARSE_KJCC_MOD ( PIM, FINAM, FIL_IND, M_MOD, &
     &                                 L_MOD, MOD, UTC_MTAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PARSE_KJCC_MOD parses input file with interferometric *
! *   delay in KJCC format and puts results of parsing into the array    *
! *   of objects MOD.                                                    *
! *                                                                      *
! * ## 13-DEC-2015 PIMA_PARSE_KJCC_MOD v1.0 (c) L. Petrov 13-DEC-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE      ) :: PIM
      CHARACTER  FINAM*(*)
      INTEGER*4  FIL_IND, M_MOD, L_FIL, L_MOD, IUER
      REAL*8     UTC_MTAI
      TYPE     ( PIMA__KJCC_TYPE ) :: MOD(M_MOD)
      CHARACTER  EXPER_WORD*32, STA_WORD*32, CLO_OFF_WORD*64, &
     &           CLO_RAT_WORD*64, CLO_EPO_WORD*64, DATE_STR*32
      CHARACTER  STR*128, STA_NAM*8, SOU_NAM*8, REG*7
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'#,;=' )
      INTEGER*1, ALLOCATABLE :: ARR(:)
      INTEGER*8  SIZE_I8
      INTEGER*4  LEN_STR, EXTRA_LINES, MIND
      PARAMETER  ( LEN_STR = 100 ) ! Expected lenght size
      PARAMETER  ( MIND = 32 ) ! Expected lenght size
      PARAMETER  ( EXTRA_LINES = 128 )
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      INTEGER*4  IS, LUN, J1, J2, J3, J4, J5, UNIX_DATE, I4_VAR, OFFSET, &
     &           MJD, MJD_CLO_EPOCH, MP, NP, IND(2,MIND), DOY, ISTA, LIND, &
     &           IER
      REAL*8     TAI_CLO_EPOCH, CLO_OFF, CLO_RAT, UTC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ, FILE_INFO, LTM_DIF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_PARSE_KJCC_MOD: processing delay file '// &
     &                         FINAM(1:I_LEN(FINAM))
           CALL FLUSH ( 6 )
      END IF
      IS = FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6741, IUER, 'PIMA_PARSE_KJCC_MOD', 'Error '// &
     &         'in collecting information about input file '//FINAM )
           RETURN 
      END IF
!
      MP = SIZE_I8/LEN_STR + EXTRA_LINES
      ALLOCATE ( BUF(MP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6742, IUER, 'PIMA_PARSE_KJCC_MOD', 'Error '// &
     &         'in an attempt to allocate memory for buffer BUF' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6743, IUER, 'PIMA_PARSE_KJCC_MOD', 'Error '// &
     &         'in reading input file with interferometric model '// &
     &          FINAM )
           RETURN 
      END IF
      CALL CLRCH ( STR ) 
      CALL TRAN ( 13, BUF(1), STR )
      IF ( STR(1:LEN(PIMA__KJCC_V1_LABEL)) == PIMA__KJCC_V1_LABEL ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 6744, IUER, 'PIMA_PARSE_KJCC_MOD', 'Error '// &
     &         'in parsing input file with interferometric model '// &
     &          FINAM(1:I_LEN(STR))//' the first line is '// &
     &          STR(1:I_LEN(STR))//' while the format label '// &
     &          PIMA__KJCC_V1_LABEL//' is expected' )
           RETURN 
      END IF
      DO 420 J2=2,NP
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, IER )
         IF ( LIND < 2 ) GOTO 420
         IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'Exper' ) THEN
              EXPER_WORD = BUF(J2)(IND(1,2):IND(2,2)) 
              IF ( EXPER_WORD .NE. PIM%CONF%SESS_CODE ) THEN
                   CALL ERR_LOG ( 6745, IUER, 'PIMA_PARSE_KJCC_MOD', 'Error '// &
     &                 'in parsing input file with interferometric model '// &
     &                  FINAM(1:I_LEN(FINAM))//' -- the model file is for '// &
     &                 'experiment '//EXPER_WORD(1:I_LEN(EXPER_WORD))// &
     &                 ' while the control file is for experiment '// &
     &                 PIM%CONF%SESS_CODE )
                   RETURN 
              END IF
              STA_WORD   = BUF(J2)(IND(1,4):IND(2,4))
              ISTA = 0
              DO 430 J3=1,PIM%NSTA
                 IF ( STA_WORD  == PIM%STA(J3)%ORIG_NAME ) THEN
                      ISTA = J3
                      GOTO 830
                   ELSE IF ( STA_WORD  == PIM%STA(J3)%IVS_NAME ) THEN
                      ISTA = J3
                      GOTO 830
                   ELSE IF ( STA_WORD  == PIM%STA(J3)%NAME ) THEN
                      ISTA = J3
                      GOTO 830
                 END IF
 430          CONTINUE 
 830          CONTINUE 
              IF ( ISTA < 1 ) THEN
                   CALL ERR_LOG ( 6746, IUER, 'PIMA_PARSE_KJCC_MOD', 'Error '// &
     &                 'in parsing input file with interferometric model '// &
     &                  FINAM(1:I_LEN(FINAM))//' station '//STA_WORD// &
     &                 'did not observe experiment '//PIM%CONF%SESS_CODE )
                   RETURN 
              END IF
           ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'Clock' ) THEN
              CLO_OFF_WORD = BUF(J2)(IND(1,3):IND(2,3))
              IF ( LIND .GE. 7 ) THEN
                   CLO_RAT_WORD = BUF(J2)(IND(1,5):IND(2,5))
                   CLO_EPO_WORD = BUF(J2)(IND(1,7):IND(2,7))
                 ELSE 
                   CLO_RAT_WORD = '0.0'
                   CLO_EPO_WORD = '2015001010100.0000'
              END IF
              DATE_STR = CLO_EPO_WORD(1:4)//'.01.01_'//CLO_EPO_WORD(8:9)//':'// &
     &                   CLO_EPO_WORD(10:11)//':'//CLO_EPO_WORD(12:18)
              CALL CHIN  ( CLO_EPO_WORD(5:7), DOY )
!
! ----------- TAI or UTC?
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( DATE_STR, MJD_CLO_EPOCH, TAI_CLO_EPOCH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 6747, IUER, 'PIMA_PARSE_KJCC_MOD', 'Trap of '// &
     &                 'internal control in parsing input file with '// &
     &                 'interferometric model '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- Failure in parsing date '// &
     &                 BUF(J2)(IND(1,1):IND(2,1)) )
                   RETURN 
              END IF
              READ ( UNIT=CLO_OFF_WORD, FMT='(D22.15)' ) CLO_OFF
              READ ( UNIT=CLO_RAT_WORD, FMT='(D22.15)' ) CLO_RAT
           ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'yyyyDDDhhmmss.ssss' ) THEN
              CONTINUE 
           ELSE                                                     
              L_MOD = L_MOD + 1
              IF ( L_MOD > M_MOD ) THEN
                   CALL ERR_LOG ( 6748, IUER, 'PIMA_PARSE_KJCC_MOD', 'Trap '// &
     &                 'of internal control in parsing input file with '// &
     &                 'interferometric model '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- parameter M_MOD  = '//STR(1:I_LEN(STR))// &
     &                 ' is too small' )
                   RETURN 
              END IF
              DATE_STR = BUF(J2)(1:4)//'.01.01_'//BUF(J2)(8:9)//':'// &
     &                   BUF(J2)(10:11)//':'//BUF(J2)(12:18)
              CALL CHIN  ( BUF(J2)(5:7), DOY )
!
! ----------- TAI or UTC?
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( DATE_STR, MOD(L_MOD)%MJD, UTC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 6749, IUER, 'PIMA_PARSE_KJCC_MOD', 'Trap of '// &
     &                 'internal control in parsing input file with '// &
     &                 'interferometric model '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- Failure in parsing date '// &
     &                 BUF(J2)(IND(1,1):IND(2,1))//' while processing '// &
     &                'line '//STR )
                   RETURN 
              END IF
              MOD(L_MOD)%TAI = UTC - PIM%UTC_MTAI
              MOD(L_MOD)%MJD = MOD(L_MOD)%MJD + DOY - 1
              DO 440 J4=0,3
                 READ ( UNIT=BUF(J2)(IND(1,2+J4):IND(2,2+J4)), &
     &                  FMT='(D22.15)' ) MOD(L_MOD)%DEL(J4)
 440          CONTINUE 
         END IF 
 420  CONTINUE 
!
      DO 450 J5=1,L_MOD
         MOD(J5)%STA_NAM = PIM%C_STA(ISTA)
         MOD(J5)%SOU_NAM = '????????'
         MOD(J5)%CLO(0) = CLO_OFF + CLO_RAT* &
     &                              ( (MOD(J5)%MJD - MJD_CLO_EPOCH)*86400.0D0 - &
     &                                (MOD(J5)%TAI - TAI_CLO_EPOCH) ) - PIM%UTC_MTAI
         MOD(J5)%CLO(1) = CLO_RAT
         MOD(J5)%ATM = 0.0D0
 450  CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, 110 ) MOD(J5)%STA_NAM, CLO_OFF, CLO_RAT
 110       FORMAT ( 'PIMA_PARSE_KJCC_MOD: station ', A, &
     &              ' Clock offset: ', 1PD15.7, &
     &              ' Clock rate: ', 1PD15.7 )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_PARSE_KJCC_MOD: finshed processing delay file '// &
     &                         FINAM(1:I_LEN(FINAM))
           CALL FLUSH ( 6 )
      END IF
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PARSE_KJCC_MOD   !#!#
