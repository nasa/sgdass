      SUBROUTINE PIMA_LOAD_VERA_MOD ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LOAD_VERA_MOD  reads the directory of files in CODA   *
! *   format with VLBI interferomeric model, parses then and creates     *
! *   MOD and MDC sub-objects for the station objects in the main        *
! *   PIMA data structure.                                               *
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
! * ## 26-FEB-2012 PIMA_LOAD_VERA_MOD v2.1 (c) L. Petrov 17-DEC-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      TYPE ( CODA_IFMOD__HEADER ) :: CIH
      TYPE ( CODA_IFMOD__DATA   ) :: CID
      INTEGER*4  IUER
      CHARACTER  FINAM*128, STR*128
      ADDRESS__TYPE :: DIR_DESC(32)
      REAL*8     TIM_VER__MAR
      INTEGER*4  IND_SOU__OUT
      PARAMETER  ( TIM_VER__MAR = 0.1D0 )
      PARAMETER  ( IND_SOU__OUT = 30003 )
      CHARACTER  MOD_FILE, MDU_FILE*128, MDC_FILE*128
      INTEGER*4  LEV, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           J11, J12, J13, J14, J15, J16, J17, J18, J19, &
     &           L_NOD, L_DEG, NZO_REF, &
     &           IND_SOU_VTD, IL, IS, L_FIL, K_MOD(PIM__MSTA), &
     &           IND_FRA, LUN_MOD, LUN_MDU, LUN_MDC, IP, ID, IER
      REAL*8     FREQ_REF, GR_DEL, PH_RAT, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), &
     &           MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG

      REAL*8     TIM_OBS_BEG, TIM_OBS_END, TIM_MOD_BEG, TIM_MOD_END
      LOGICAL*1  FL_FOU(2)
      REAL*8,    ALLOCATABLE :: TIM_ARR(:), DED_ARR(:)
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN, GET_UNIT, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_MOD
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_MOD
#endif
!
! --- Set up for VTD
!
      IF ( VTD%STATUS .EQ. VTD__INIT .OR. &
     &     VTD%STATUS .EQ. VTD__ALLC .OR. &
     &     VTD%STATUS .EQ. VTD__LOAD      ) THEN
!
! -------- Deallocate VTD object it it was already allocated in order
! -------- to clean its state
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_QUIT ( VTD,  IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7441, IUER, 'PIMA_LOAD_VERA_MOD', &
     &              'Error in an attempt to initialize VTD oibject' )
                RETURN
           END IF
      END IF
!
! --- Initlialize VTD object
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7442, IUER, 'PIMA_LOAD_VERA_MOD', 'Error in '// &
     &         'an attempt to initialize VTD oibject' )
           RETURN
      END IF
!
! --- Read and parse VTD configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7443, IUER, 'PIMA_LOAD_VERA_MOD', 'Error in '// &
     &         'an attempt to read configuration file '// &
     &          PIM%CONF%VTD_CONFIG_FILE )
           RETURN
      END IF
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      CALL ERR_PASS ( IUER, IER )
      PIM%C_STA(PIM%NSTA+1) = 'GEOCENTR'
      CALL VTD_LOAD ( VTD, PIM%NSTA+1, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                     PIM%MJD_0, PIM%TAI_0 - PIM__MSCL, PIM%MJD_0, &
     &                     PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) + PIM__MSCL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7444, IUER, 'PIMA_LOAD_VERA_MOD', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN
      END IF
!
      IF ( ILEN(PIM%CONF%EPHEMERIDES_FILE) > 0 ) THEN
!
! -------- Read the ephemeride of the orbiting station
!
           PIM%NZO%FILNZO = PIM%CONF%EPHEMERIDES_FILE           
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_READ_NZO ( PIM%NZO%FILNZO, PIM%NZO%NZO_NAME, PIM__MNZO, &
     &                         PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                         PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, PIM%NZO%VEL_ARR, &
     &                         PIM%NZO%CENTER_NAME, PIM%NZO%REF_NAME, &
     &                         PIM%NZO%TIM_CODE, PIM%NZO%COO_CODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7445, IUER, 'PIMA_LOAD_VERA_MOD', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
           IF ( PIM%NZO%CENTER_NAME == 'EARTH BARYCENTER' ) THEN
                NZO_REF = VTD__EME
              ELSE 
                CALL ERR_LOG ( 7446, IUER, 'PIMA_LOAD_VERA_MOD', 'Unsupported '// &
     &              'coordinate center name: '//PIM%NZO%CENTER_NAME )
                RETURN 
           END IF
!
! -------- Expand the orbiting station ephemeride into the B-spline basis
!
           L_NOD = MIN ( PIM%NZO%L_NZO/PIMA__NZO_NOT_SCAL, VTD__M_NOD )
           L_DEG = 3
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_OBJ_NZO ( PIM%NZO%NZO_NAME, VTD__ES, VTD__OR, &
     &                             NZO_REF, PIM%NZO%TIM_CODE, &
     &                             VTD, PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                             PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                             L_NOD, L_DEG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7447, IUER, 'PIMA_LOAD_VERA_MOD', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
      END IF
!
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           IND_FRA = PIMA__DRF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           IND_FRA = PIMA__MUL
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           IND_FRA = PIMA__ADD
      END IF
!
! --- Set up the reference frequency
!
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
!
! --- Default data type for delay computation: group delay
!
      OBS_TYP%PLRZ       = 'RR'
      OBS_TYP%FRQ_REF(1) = FREQ_REF
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = FREQ_REF
      OBS_TYP%STATUS     = VTD__BND
!
! --- Initialization
!
      DO 410 J1=1,PIM%NSTA
!
! ------ Deallocate interferometric model if it was allocated
!
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%IND_SOU ) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%IND_SOU )
         END IF
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%TIME_CEN ) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%TIME_CEN )
         END IF
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%CLOCK_OFFSET) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%CLOCK_OFFSET)
         END IF
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%CLOCK_RATE) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%CLOCK_RATE)
         END IF
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%ATMO_DELAY) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%ATMO_DELAY)
         END IF
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%ATMO_RATE) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%ATMO_RATE)
         END IF
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%GDELAY) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%GDELAY)
         END IF
         IF ( ASSOCIATED ( PIM%STA(J1)%MDC%GRATE) ) THEN
              DEALLOCATE ( PIM%STA(J1)%MDC%GRATE)
         END IF
         PIM%STA(J1)%L_MOD = 0
         PIM%STA(J1)%L_MDC = 0
         K_MOD(J1) = 0
 410  CONTINUE
      PIM%NMOD = 3
!
      DO 420 J2=1,2
         L_FIL = 0
         DO 430 J3=1,PIM%CONF%L_INM
            LEV = 0
            DO 440 J4=1,PIM__MEPC
               IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, PIM%CONF%INTMOD_FILE(J3), FINAM )
               IF ( IS .NE. 0 ) THEN
                    CALL ERR_LOG ( 7448, IUER, 'PIMA_LOAD_VERA_MOD', 'Error '// &
     &                  'in reading input directory '// &
     &                   PIM%CONF%INTMOD_FILE(J3)(1:I_LEN(PIM%CONF%INTMOD_FILE(J3)))// &
     &                  ' with VERA Inteferometric Model: '//FINAM )
                    RETURN 
               END IF
               IF ( LEV == 0 ) GOTO 840 ! End of work
!
               IL = ILEN(FINAM)
               IF ( IL < 10 ) GOTO 440
!
!!  if ( finam(1:i_len(finam)) .ne. '/d3/vera_ant/r10273b/r10273b5_ANT.3'  ) goto 440 ! %%%%
!
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                    WRITE ( 6, '(A)' ) 'PIMA_LOAD_VERA_MOD: Processing file '// &
     &                                  FINAM(1:I_LEN(FINAM))
               END IF
               IF ( INDEX ( FINAM, 'ANT.' ) > 0 ) THEN
!
! ----------------- Read the header of the interferometric model file
!
                    L_FIL = L_FIL + 1
                    CALL ERR_PASS ( IUER, IER )
                    CALL GET_CODA_IFMOD_HEADER ( FINAM, PIM%CONF%INTMOD_TYPE, &
     &                                           CIH, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 7449, IUER, 'PIMA_LOAD_VERA_MOD', &
     &                       'Error in parsing the header of the input '// &
     &                       'CODA data file '//FINAM )
                         RETURN
                    END IF
                    IF ( PIM%CONF%INTMOD_TYPE == PIMA__MOD_VERA1000 ) THEN
                         ID = LINDEX ( FINAM, '.' ) 
                         IF ( ID < 1 ) THEN
                              CALL ERR_LOG ( 7449, IUER, 'PIMA_LOAD_VERA_MOD', &
     &                            'Hm! the file name does not contain a dot. '// &
     &                            'do not know what to do with such a file.' )
                              RETURN
                         END IF
                         CALL CHIN ( FINAM(ID+1:), CIH%IND_ANT )
                    END IF
!
                    CIH%STA_IND = 0
                    DO 450 J5=1,PIM%NSTA
                       IF ( PIM%STA(J5)%IND_ORIG == CIH%IND_ANT ) THEN
                            CIH%STA_IND = J5
                       END IF
 450                CONTINUE
                    IF ( CIH%STA_IND == 0 ) THEN
                         CALL ERR_LOG ( 7450, IUER, 'PIMA_LOAD_VERA_MOD', &
     &                       'Trap of internal control: antenna index '// &
     &                        STR(1:I_LEN(STR))//' specified in CODA file '// &
     &                        FINAM(1:I_LEN(FINAM))//' was not found' )
                         RETURN
                    END IF
                    IF ( J2 == 1 ) THEN
                         PIM%STA(CIH%STA_IND)%L_MOD = PIM%STA(CIH%STA_IND)%L_MOD + CIH%NDAT
                    END IF
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                         WRITE ( 6, 210 ) FINAM(1:I_LEN(FINAM)), &
     &                                    CIH%NDAT, CIH%IND_ANT, &
     &                                    CIH%STA_IND, PIM%STA(CIH%STA_IND)%L_MOD 
 210                     FORMAT ( 'PIMA_LOAD_VERA_MOD File: ',A, &
     &                           ' Ndat: ', I6, ' Ind_ant: ', I2, &
     &                           ' Sta_ind: ', I2, ' L_mod: ', I6 )
                    END IF
               END IF
               IF ( J2 == 2 ) THEN
!
! ----------------- Second run: read the body of the interferometric model file
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL GET_CODA_IFMOD_DATA ( PIM, PIM%CONF%INTMOD_TYPE, &
     &                                         K_MOD, FINAM, CIH, CID, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 7451, IUER, 'PIMA_LOAD_VERA_MOD', &
     &                       'Failure in an attempt to read and parse the '// &
     &                       'interferometric model from the input file '//FINAM )
                         RETURN
                    END IF
               END IF
 440        CONTINUE
 840        CONTINUE
 430     CONTINUE
!
         IF ( J2 == 1 ) THEN
              DO 460 J6=1,PIM%NSTA
                 IF ( PIM%STA(J6)%L_MOD == 0 ) THEN
                      CALL ERR_LOG ( 7452, IUER, 'PIMA_LOAD_VERA_MOD', 'Trap '// &
     &                    'of internal control: no interferometric model was '// &
     &                    'found for station '//PIM%C_STA(J6) )
                      RETURN
                 END IF
!
                 ALLOCATE ( PIM%STA(J6)%MOD(PIM%STA(J6)%L_MOD), STAT=IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL IINCH ( PIM%STA(J6)%L_MOD*SIZEOF(PIM%STA(J6)%MOD(1)), STR )
                      CALL ERR_LOG ( 7453, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &                    'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                    'memory for interferometic model for station '// &
     &                     PIM%C_STA(J6) )
                      RETURN
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                      WRITE ( 6, 220 ) 1, J6, PIM%NSTA, PIM%C_STA(J6), &
     &                                 PIM%STA(J6)%L_MOD
 220                  FORMAT ( 'PIMA_LOAD_VERA_MOD/',I1,'/  I_Sta: ', I2, &
     &                         '(',I2,')  Sta: ', A, ' L_mod: ', I8 )
                 END IF
 460          CONTINUE
            ELSE IF ( J2 == 2 ) THEN
              DO 470 J7=1,PIM%NSTA
                 PIM%STA(J7)%L_MOD = K_MOD(J7)
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                      WRITE ( 6, 220 ) 2, J7, PIM%NSTA, PIM%C_STA(J7), &
     &                                 PIM%STA(J7)%L_MOD
                 END IF
                 CALL FOR_QSORT ( PIM%STA(J7)%MOD, PIM%STA(J7)%L_MOD, &
     &                            SIZEOF(PIM%STA(J7)%MOD(1)), PIMA_COMPAR_MOD )
!
                 DO 480 J8=1,PIM%STA(J7)%L_MOD
                    DO 490 J9=1,PIM%NOBS
                       PIM%OBS(J9)%MOD_IND_BEG = 0
                       PIM%OBS(J9)%MOD_IND_END = 0
                       IF ( PIM%OBS(J9)%SOU_IND == PIM%STA(J7)%MOD(J8)%SOU_IND ) THEN
                            IF ( PIM%OBS(J9)%STA_IND(1) == J7 .OR. &
     &                           PIM%OBS(J9)%STA_IND(2) == J7      ) THEN
!
                                 IF ( PIM%STA(J7)%MOD(J8)%TIM_BEG > PIM%OBS(J9)%TIM_BEG - TIM_VER__MAR .AND. &
     &                                PIM%STA(J7)%MOD(J8)%TIM_END < PIM%OBS(J9)%TIM_END + TIM_VER__MAR       ) THEN
                                      PIM%STA(J7)%MOD(J8)%SCANNAME = PIM%SCA(PIM%OBS(J9)%SCA_IND)%SCAN_NAME
                                 END IF
                            END IF
                       END IF
 490                CONTINUE
 480             CONTINUE
 470          CONTINUE
         END IF
 420  CONTINUE
!
      DO 4100 J10=1,PIM%NSOU
         IND_SOU_VTD = 0
         DO 4110 J11=1,VTD%L_SOU
            IF ( PIM%SOU(J10)%IVS_NAME == VTD%SOU(J11)%IVS_NAME ) THEN
                 IND_SOU_VTD = J11
            END IF
            IF ( PIM%SOU(J10)%J2000_NAME == VTD%SOU(J11)%J2000_NAME ) THEN
                 IND_SOU_VTD = J11
            END IF
            IF ( PIM%SOU(J10)%DB_NAME    == VTD%SOU(J11)%J2000_NAME ) THEN
                 IND_SOU_VTD = J11
            END IF
            IF ( PIM%SOU(J10)%DB_NAME    == VTD%SOU(J11)%IVS_NAME ) THEN
                 IND_SOU_VTD = J11
            END IF
 4110    CONTINUE 
         IF ( IND_SOU_VTD == 0 ) THEN
              WRITE ( 6, * ) 'VTD%L_SOU = ', VTD%L_SOU
              CALL ERR_LOG ( 7454, IUER, 'PIMA_LOAD_VERA_MOD', 'Cannot '// &
     &            'find source name '//PIM%SOU(J10)%IVS_NAME// &
     &            ' in the VTD source  catalogue' )
              RETURN
         END IF
!
         VTD%SOU(IND_SOU_VTD)%ALPHA = PIM%SOU(J10)%ALPHA_INP
         VTD%SOU(IND_SOU_VTD)%DELTA = PIM%SOU(J10)%DELTA_INP
         VTD%SOU(IND_SOU_VTD)%SOU_CRS(1) = DCOS(VTD%SOU(IND_SOU_VTD)%DELTA)*DCOS(VTD%SOU(IND_SOU_VTD)%ALPHA)
         VTD%SOU(IND_SOU_VTD)%SOU_CRS(2) = DCOS(VTD%SOU(IND_SOU_VTD)%DELTA)*DSIN(VTD%SOU(IND_SOU_VTD)%ALPHA)
         VTD%SOU(IND_SOU_VTD)%SOU_CRS(3) = DSIN(VTD%SOU(IND_SOU_VTD)%DELTA)
         VTD%SOU(IND_SOU_VTD)%S_CRS = VTD%SOU(IND_SOU_VTD)%SOU_CRS
 4100 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           MOD_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mod'
           MDU_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdu'
           MDC_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdc'
!             
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                MOD_FILE = PIM%CONF%EXPER_DIR(1:IP)//MOD_FILE
              ELSE
                MOD_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MOD_FILE
           END IF
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//MDU_FILE
              ELSE
                MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MDU_FILE
           END IF
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                MDC_FILE = PIM%CONF%EXPER_DIR(1:IP)//MDC_FILE
              ELSE
                MDC_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MDC_FILE
           END IF
!
           LUN_MDU = GET_UNIT()
           OPEN ( UNIT=LUN_MDU, FILE=MDU_FILE, STATUS='UNKNOWN', &
     &            IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7455, IUER, 'PIMA_INDX', 'Failure to '// &
     &              'open information file '// &
     &               MDU_FILE(1:I_LEN(MDU_FILE))//' -- error '//STR )
                RETURN
           END IF
      END IF
!
      DO 4120 J12=1,PIM%NSTA
         DO 4130 J13=1,PIM%STA(J12)%L_MOD
            IF ( ILEN(PIM%STA(J12)%MOD(J13)%SCANNAME) == 0 ) THEN
                 PIM%STA(J12)%MOD(J13)%SOU_IND = IND_SOU__OUT
            END IF
 4130    CONTINUE
         CALL FOR_QSORT ( PIM%STA(J12)%MOD, PIM%STA(J12)%L_MOD, &
     &                    SIZEOF(PIM%STA(J12)%MOD(1)), PIMA_COMPAR_MOD )
!
         ALLOCATE ( TIM_ARR(PIM%STA(J12)%L_MOD), STAT=IER ) 
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MOD, STR )
              CALL ERR_LOG ( 7456, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for a temporary array TIM_ARR' )
              RETURN
         END IF
!
         ALLOCATE ( DED_ARR(PIM%STA(J12)%L_MOD), STAT=IER ) 
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MOD, STR )
              CALL ERR_LOG ( 7457, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for temporary array DED_ARR' )
              RETURN
         END IF
         DO 4140 J14=1,PIM%STA(J12)%L_MOD
            IF ( PIM%STA(J12)%MOD(J14)%SOU_IND == IND_SOU__OUT ) THEN
                 PIM%STA(J12)%L_MOD = J14 - 1
                 GOTO 8140
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL VTD_DELAY ( PIM%C_SOU(PIM%STA(J12)%MOD(J14)%SOU_IND), 'GEOCENTR', &
     &                       PIM%C_STA(J12), PIM%MJD_0, &
     &                       PIM%TAI_0 + PIM%STA(J12)%MOD(J14)%TIM_BEG, &
     &                       OBS_TYP, VTD, GR_DEL, PH_RAT, DER_DEL, DER_RAT, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7458, IUER, 'PIMA_LOAD_VERA_MOD', 'Error in an '// &
     &               'attempt to compute theoretical path delay' )
                 RETURN
            END IF
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%STA(J12)%MOD(J14)%TIM_BEG, &
     &                                  -2 )
                 WRITE ( 6, 230 ) PIM%C_STA(J12), PIM%C_SOU(PIM%STA(J12)%MOD(J14)%SOU_IND), &
     &                            STR(1:30), GR_DEL, PH_RAT, &
     &                            PIM%STA(J12)%MOD(J14)%GDEL_POL(0,1), PIM%STA(J12)%MOD(J14)%GDEL_POL(1,1), &
     &                            (PIM%STA(J12)%MOD(J14)%GDEL_POL(0,1) - GR_DEL)*1.D9, &
     &                            DER_DEL(VTD__ELEV2)/DEG__TO__RAD
 230             FORMAT ( 'PLVM Sta: ', A, ' Sou: ', A, ' Date: ', A, ' Gr_del: ', 1PD19.12, &
     &                    ' Ph_rat: ', 1PD19.12, ' V_del: ', 1pd19.12, &
     &                    ' V_rat: ', 1pd19.12, ' D_del: ', 0pf14.5, ' ns ', &
     &                    ' Elev: ', 0PF6.2, ' deg' )
            END IF
            TIM_ARR(J14) = PIM%STA(J12)%MOD(J14)%TIM_BEG
            DED_ARR(J14) = PIM%STA(J12)%MOD(J14)%GDEL_POL(0,1) - GR_DEL 
 4140    CONTINUE
 8140    CONTINUE
         CALL SORT8 ( PIM%STA(J12)%L_MOD, TIM_ARR, DED_ARR )
         CALL RGR8_NOWEI ( PIM%STA(J12)%L_MOD, TIM_ARR, DED_ARR, &
     &                     MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG, IER )
         SH_VAL = SH_VAL - DR_VAL*MEAN_T
         DEALLOCATE ( TIM_ARR )
         DEALLOCATE ( DED_ARR )
!
         PIM%STA(J12)%L_MDC = PIM%STA(J12)%L_MOD
         PIM%STA(J12)%MDC%CLO_OFFS = SH_VAL
         PIM%STA(J12)%MDC%CLO_RATE = DR_VAL
         PIM%STA(J12)%MDC%CLO_OFFS_ERR = SH_SIG
         PIM%STA(J12)%MDC%CLO_RATE_ERR = DR_VAL
!
         ALLOCATE ( PIM%STA(J12)%MDC%IND_SOU(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7459, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%IND_SOU' )
              RETURN
         END IF 
!
         ALLOCATE ( PIM%STA(J12)%MDC%TIME_CEN(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7460, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%TIME_CEN' )
              RETURN
         END IF 
!
         ALLOCATE ( PIM%STA(J12)%MDC%CLOCK_OFFSET(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7461, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%CLOCK_OFFSET' )
              RETURN
         END IF 
!
         ALLOCATE ( PIM%STA(J12)%MDC%CLOCK_RATE(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7462, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%CLOCK_RATE' )
              RETURN
         END IF 
!
         ALLOCATE ( PIM%STA(J12)%MDC%ATMO_DELAY(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7463, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%ATMO_DELAY' )
              RETURN
         END IF 
!
         ALLOCATE ( PIM%STA(J12)%MDC%ATMO_RATE(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7464, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%ATMO_RATE' )
              RETURN
         END IF 
!
         ALLOCATE ( PIM%STA(J12)%MDC%GDELAY(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7465, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%GDELAY' )
              RETURN
         END IF 
!
         ALLOCATE ( PIM%STA(J12)%MDC%GRATE(PIM%STA(J12)%L_MDC), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%STA(J12)%L_MDC, STR )
              CALL ERR_LOG ( 7466, IUER, 'PIMA_LOAD_VERA_MOD', 'Failure '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array %MDC%GRATE' )
              RETURN
         END IF 
!
         DO 4150 J15=1,PIM%STA(J12)%L_MDC
            PIM%STA(J12)%MDC%IND_SOU(J15)      = PIM%STA(J12)%MOD(J15)%SOU_IND
            PIM%STA(J12)%MDC%TIME_CEN(J15)     = PIM%STA(J12)%MOD(J15)%TIM_BEG
            PIM%STA(J12)%MDC%CLOCK_OFFSET(J15) = PIM%STA(J12)%MDC%CLO_OFFS + &
     &                   PIM%STA(J12)%MOD(J15)%TIM_BEG*PIM%STA(J12)%MDC%CLO_RATE
            PIM%STA(J12)%MDC%CLOCK_RATE(J15)   = PIM%STA(J12)%MDC%CLO_RATE
            PIM%STA(J12)%MDC%ATMO_DELAY(J15)   = 0.0D0
            PIM%STA(J12)%MDC%ATMO_RATE(J15)    = 0.0D0
            PIM%STA(J12)%MDC%GDELAY(J15)       = PIM%STA(J12)%MOD(J15)%GDEL_POL(0,1)
            PIM%STA(J12)%MDC%GRATE(J15)        = PIM%STA(J12)%MOD(J15)%GDEL_POL(1,1)
 4150    CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              WRITE ( 6, 240 ) J12, PIM%NSTA, PIM%C_STA(J12), &
     &                         PIM%STA(J12)%L_MDC, PIM%STA(J12)%L_MOD
 240          FORMAT ( ' Sta_ind: ', I2, ' ( ', I2, &
     &                 ' ) PIMA_LOAD_VERA_MOD.  Finished station ', A, &
     &                 ' L_MDC: ', I6, ' L_MOD: ', I6 )
         END IF
 4120 CONTINUE
!
      DO 4160 J16=1,PIM%NOBS
         TIM_OBS_BEG =  1.D30
         TIM_OBS_END = -1.D30
         DO 4170 J17=1,PIM%OBS(J16)%NUVS
            IF ( PIM%OBS(J16)%UV_IND(1,J17) > 0 ) THEN
                 IF ( FL_FOU(1) ) THEN
                      TIM_OBS_BEG = MIN ( TIM_OBS_BEG, &
     &                                    PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J16)%UV_IND(1,J17))%TIM_IND) )
                    ELSE
                      TIM_OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J16)%UV_IND(1,J17))%TIM_IND)
                 END IF
                 FL_FOU(1) = .TRUE.
            END IF
            IF ( PIM%OBS(J16)%UV_IND(PIM%OBS(J16)%NUM_EPC(J17),J17) > 0 ) THEN
                 IF ( FL_FOU(2) ) THEN
                      TIM_OBS_END = MAX ( TIM_OBS_END, &
     &                                    PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J16)%UV_IND(PIM%OBS(J16)%NUM_EPC(J17),J17))%TIM_IND) )
                    ELSE
                      TIM_OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J16)%UV_IND(PIM%OBS(J16)%NUM_EPC(J17),J17))%TIM_IND)
                 END IF
                 FL_FOU(2) = .TRUE.
            END IF
            DO 4180 J18=1,2
               DO 4190 J19=1,PIM%STA(PIM%OBS(J16)%STA_IND(J18))%L_MOD
!
! --------------- Do not consider sources others than the source observed
! --------------- in the J16-th observation
!
                  IF ( PIM%STA(PIM%OBS(J16)%STA_IND(J18))%MOD(J19)%SOU_IND .NE. &
     &                 PIM%OBS(J16)%ROOT_SOU_IND ) GOTO 4190
!
                  TIM_MOD_BEG = PIM%STA(PIM%OBS(J16)%STA_IND(J18))%MOD(J19)%TIM_BEG
                  TIM_MOD_END = PIM%STA(PIM%OBS(J16)%STA_IND(J18))%MOD(J19)%TIM_END
!
! --------------- Check whether the model interval fits into the observation
! --------------- range.
! --------------- We consider four cases:
! --------------- a) Observation start is within the model range
! --------------- b) Observation end   is within the model range
! --------------- c) the model range is within the observation range
! --------------- d) the observation range is within the model range
!
                  IF ( ( TIM_MOD_BEG < TIM_OBS_BEG + PIM%CONF%AP_TOLERANCE .AND. &
     &                   TIM_MOD_END > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE       &
     &                 ) .OR. &
     &                 ( TIM_MOD_BEG < TIM_OBS_END + PIM%CONF%AP_TOLERANCE .AND. &
     &                   TIM_MOD_END > TIM_OBS_END - PIM%CONF%AP_TOLERANCE       &
     &                 ) .OR. &
     &                 ( TIM_MOD_BEG > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                   TIM_MOD_END < TIM_OBS_END + PIM%CONF%AP_TOLERANCE       &
     &                 ) .OR. &
     &                 ( TIM_OBS_BEG > TIM_MOD_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                   TIM_OBS_END < TIM_MOD_END + PIM%CONF%AP_TOLERANCE       &
     &                 ) ) THEN
!
                       IF ( PIM%OBS(J16)%MOD_IND_BEG(J18) == 0 ) THEN
                            PIM%OBS(J16)%MOD_IND_BEG(J18) = J19
                       END IF
!
                       PIM%OBS(J16)%MOD_IND_END(J18) = J19
                  END IF
 4190          CONTINUE 
 4180       CONTINUE 
 4170    CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              IF ( PIM%OBS(J16)%MOD_IND_BEG(1) > 0 .AND. &
     &             PIM%OBS(J16)%MOD_IND_END(1) > 0 .AND. &
     &             PIM%OBS(J16)%MOD_IND_BEG(2) > 0 .AND. &
     &             PIM%OBS(J16)%MOD_IND_END(2) > 0       ) THEN
!
                   WRITE ( UNIT=LUN_MDU, FMT=130 ) J16, &
     &                 PIM%OBS(J16)%MOD_IND_BEG, &
     &                 PIM%OBS(J16)%MOD_IND_END, &
     &                 PIM%C_SOU(PIM%OBS(J16)%SOU_IND), &
     &                 PIM%C_STA(PIM%OBS(J16)%STA_IND(1)), &
     &                 PIM%C_STA(PIM%OBS(J16)%STA_IND(2)), &
     &                 MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                           PIM%STA(PIM%OBS(J16)%STA_IND(1))%MOD(PIM%OBS(J16)%MOD_IND_BEG(1))%TIM_BEG, -2 ), &
     &                 MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                           PIM%STA(PIM%OBS(J16)%STA_IND(1))%MOD(PIM%OBS(J16)%MOD_IND_END(1))%TIM_END, -2 ), &
     &                 MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                           PIM%STA(PIM%OBS(J16)%STA_IND(2))%MOD(PIM%OBS(J16)%MOD_IND_BEG(2))%TIM_BEG, -2 ), &
     &                 MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                           PIM%STA(PIM%OBS(J16)%STA_IND(2))%MOD(PIM%OBS(J16)%MOD_IND_END(2))%TIM_END, -2 )
 130               FORMAT ( 'Obs: ',I6,' MOD_BEG: ', I8, 1X, I8, &
     &                      ' MOD_END: ', I8, 1X, I8, &
     &                      ' C_SOU: ', A, &
     &                      ' C_STA: ', A8, 1X, A8, ' TIM_1: ', A30, 1X, A30, &
     &                      ' TIM_2: ', A21, 1X, A21 )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( IER, STR )
                        CALL ERR_LOG ( 7467, IUER, 'PIMA_INDX', 'Failure '// &
     &                      'in writing into file '// &
     &                       MDU_FILE(1:I_LEN(MDU_FILE))// &
     &                      ' -- error '//STR )
                        RETURN
                   END IF
                 ELSE
                   STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%OBS(J16)%TIM_BEG_IND), &
     &                                    -2 )
                   WRITE ( UNIT=LUN_MDU, FMT=135 ) J16, &
     &                     PIM%OBS(J16)%MOD_IND_BEG, &
     &                     PIM%OBS(J16)%MOD_IND_END, &
     &                     PIM%C_SOU(PIM%OBS(J16)%SOU_IND), &
     &                     PIM%C_STA(PIM%OBS(J16)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(J16)%STA_IND(2)), &
     &                     STR(1:22)
 135               FORMAT ( 'NO MODEL for obs ', I6, &
     &                      ' MOD_BEG: ', I4, 1X, I4, &
     &                      ' MOD_END: ', I4, 1X, I4, &
     &                      ' C_SOU: ', A, &
     &                      ' C_STA: ', A8, 1X, A8, &
     &                      ' TIM: ', A22 )
              END IF
         END IF
 4160 CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           CLOSE ( UNIT=LUN_MDU )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   PIMA_LOAD_VERA_MOD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_CODA_IFMOD_HEADER ( FINAM, INTMOD_TYPE, CIH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_CODA_IFMOD_HEADER
! *                                                                      *
! * # 27-FEB-2012 GET_CODA_IFMOD_HEADER v2.0 (c) L. Petrov 22-OCT-2012 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( CODA_IFMOD__HEADER ) :: CIH
      CHARACTER  FINAM*(*), INTMOD_TYPE*(*)
      INTEGER*4  IUER
      CHARACTER  BUF*1024, SESS*32, STR*128
      INTEGER*8  SIZE_I8
      INTEGER*4  IS, UNIX_DATE, SEEK_CUR, LUN, LN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ, LSEEK, FILE_INFO
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_CUR', SEEK_CUR, LN )
      IS = FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), UNIX_DATE, &
                       SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7469, IUER, 'GET_CODA_IFMOD_HEADER', &
     &         'Failure in attempt to learn size of file '// &
     &          FINAM(1:I_LEN(FINAM))//' -- '//STR )
           RETURN
      END IF
!
      IF ( SIZE_I8 < 512 ) THEN
           CALL ERR_LOG ( 7470, IUER, 'GET_CODA_IFMOD_HEADER', &
     &         'File '//FINAM(1:I_LEN(FINAM))//' is too short' )
           RETURN
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER < 0 ) THEN
           CALL ERR_LOG ( 7471, IUER, 'GET_CODA_IFMOD_HEADER', &
     &         'Failure in attempt to open CODA file '// &
     &          FINAM(1:I_LEN(FINAM))//' -- '//STR )
           RETURN
      END IF
!
      IF ( INTMOD_TYPE == PIMA__MOD_VERA2000 ) THEN
           IS = READ  ( %VAL(LUN), %REF(BUF), %VAL(400) )
           IF ( IS < 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7472, IUER, 'GET_CODA_IFMOD_HEADER', &
     &              'Failure in attempt to read CODA file '// &
     &               FINAM(1:I_LEN(FINAM))//' -- '//STR )
                RETURN
           END IF
           IF ( BUF(1:13) .NE. 'HEADDER_START' ) THEN
                CALL ERR_LOG ( 7473, IUER, 'GET_CODA_IFMOD_HEADER', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' does not '// &
     &              'conform CODA standard: '//BUF(1:13) )
                RETURN
           END IF
           IF ( BUF(389:399) .NE. 'HEADDER_END' ) THEN
                CALL ERR_LOG ( 7474, IUER, 'GET_CODA_IFMOD_HEADER', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has unexpected '// &
     &              'format' )
                RETURN
           END IF
!
           CIH%VERSION = BUF(40:42)//' '
           IF ( CIH%VERSION .NE. '1.0 ' ) THEN
                CALL CLRCH (  STR )
                CALL TRAN ( 13, BUF(40:42), STR(1:3) )
                CALL ERR_LOG ( 7475, IUER, 'GET_CODA_IFMOD_HEADER', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has unsupported '// &
     &              'version '//STR(1:3)//' while only version 1.0 '// &
     &              'is supported' )
                RETURN
           END IF
!
           IS = LSEEK ( %VAL(LUN), %VAL(1), %VAL(SEEK_CUR) )
           IS = READ  ( %VAL(LUN), CIH%EXP_NAME, %VAL(32) )
           IS = READ  ( %VAL(LUN), CIH%NPOLY, %VAL(4) )
           IS = READ  ( %VAL(LUN), CIH%VERS,  %VAL(4) )
           IS = READ  ( %VAL(LUN), CIH%NSS,   %VAL(4) )
           CALL ENDIAN_CNV_I4 ( CIH%NPOLY )
           CALL ENDIAN_CNV_I4 ( CIH%VERS  )
           CALL ENDIAN_CNV_I4 ( CIH%NSS   )
           CIH%NDAT = (SIZE_I8 - 445)/(61 + CIH%NSS*11*8)
           IF ( CIH%NDAT*(61 + CIH%NSS*11*8) .NE. (SIZE_I8 - 445) ) THEN
                WRITE ( 6, * ) 'CIH%NDAT*(61 + CIH%NSS*11*8) = ', CIH%NDAT*(61 + CIH%NSS*11*8)
                WRITE ( 6, * ) 'CIH%NSS*11*8 = ', CIH%NSS*11*8
                WRITE ( 6, * ) 'CIH%NPOLY= ', CIH%NPOLY, ' CIH%VERS= ', CIH%VERS, ' CIH%NSS= ', CIH%NSS
                WRITE ( 6, * ) 'CIH%NDAT= ', CIH%NDAT
                CALL ERR_LOG ( 7476, IUER, 'GET_CODA_IFMOD_HEADER', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has nonstadnard '// &
     &              'length' )
                RETURN
           END IF
           IS = LSEEK ( %VAL(LUN), %VAL(49), %VAL(SEEK_CUR) )
           IS = READ  ( %VAL(LUN), CIH%IND_ANT, %VAL(4) )
           CALL ENDIAN_CNV_I4    ( CIH%IND_ANT )
         ELSE IF ( INTMOD_TYPE == PIMA__MOD_VERA1000 ) THEN
           IS = LSEEK ( %VAL(LUN), %VAL(48), %VAL(SEEK_CUR) )
           IS = READ  ( %VAL(LUN), CIH%NSS, %VAL(4) )
           CALL ENDIAN_CNV_I4    ( CIH%NSS )
           CIH%NDAT = (SIZE_I8 - 56)/(72 + CIH%NSS*11*8)
           IF ( CIH%NDAT*(72 + CIH%NSS*11*8) .NE. (SIZE_I8 - 56) ) THEN
                WRITE ( 6, * ) 'CIH%NDAT*(61 + CIH%NSS*11*8) = ', CIH%NDAT*(61 + CIH%NSS*11*8)
                WRITE ( 6, * ) 'CIH%NSS*11*8 = ', CIH%NSS*11*8
                WRITE ( 6, * ) 'CIH%NDAT= ', CIH%NDAT
                CALL ERR_LOG ( 7477, IUER, 'GET_CODA_IFMOD_HEADER', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has nonstadnard '// &
     &              'length' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_CODA_IFMOD_HEADER  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_CODA_IFMOD_DATA ( PIM, INTMOD_TYPE, K_MOD, FINAM, &
     &                                 CIH, CID, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_CODA_IFMOD_DATA
! *                                                                      *
! * ## 27-FEB-2012 GET_CODA_IFMOD_DATA v2.0 (c) L. Petrov 22-OCT-2012 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( CODA_IFMOD__HEADER ) :: CIH
      TYPE     ( CODA_IFMOD__DATA   ) :: CID
      REAL*8,    ALLOCATABLE :: CIM_DATA(:)
 real*8  vec(8) ! %%%%%%%%%
      CHARACTER  FINAM*(*), INTMOD_TYPE*(*)
      INTEGER*4  K_MOD(PIM%NSTA), IUER
      CHARACTER  STR*128
      INTEGER*4  IS, SEEK_CUR, LUN, LN, LEN_DATA, J1, J2, J3, &
     &           IL, IL1, IL2, IL3, IL4, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ, LSEEK, LTM_DIF
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_CUR', SEEK_CUR, LN )
      ALLOCATE ( CIM_DATA(CIH%NSS*11) )
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7478, IUER, 'GET_CODA_IFMOD_DATA', &
     &         'Failure in attempt to open CODA file '// &
     &          FINAM )
           DEALLOCATE ( CIM_DATA )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      IF ( INTMOD_TYPE == PIMA__MOD_VERA2000 ) THEN
           IS = LSEEK ( %VAL(LUN), %VAL(445), %VAL(SEEK_CUR) )
           LEN_DATA = 445
         ELSE IF ( INTMOD_TYPE == PIMA__MOD_VERA1000 ) THEN
           IS = LSEEK ( %VAL(LUN), %VAL(52), %VAL(SEEK_CUR) )
           LEN_DATA = 52
      END IF
      IF ( IS .NE. LEN_DATA ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7479, IUER, 'GET_CODA_IFMOD_DATA', &
     &         'Failure in attempt to seek the CODA file '// &
     &          FINAM(1:I_LEN(FINAM))//' -- '//STR )
           DEALLOCATE ( CIM_DATA )
           RETURN
      END IF
      DO 410 J1=1,CIH%NDAT
         IF ( INTMOD_TYPE == PIMA__MOD_VERA2000 ) THEN
              IS = LSEEK ( %VAL(LUN), %VAL(1), %VAL(SEEK_CUR) )
!
              IS = READ  ( %VAL(LUN), CID, %VAL(60) )
              IF ( IS .NE. 60 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7481, IUER, 'GET_CODA_IFMOD_DATA', &
     &                 'Failure in attempt to read the CODA file '// &
     &                  FINAM(1:I_LEN(FINAM))//' -- '//STR )
                   DEALLOCATE ( CIM_DATA )
                   RETURN
              END IF
           ELSE IF ( INTMOD_TYPE == PIMA__MOD_VERA1000 ) THEN
              IS = LSEEK ( %VAL(LUN), %VAL(12), %VAL(SEEK_CUR) )
              IS = READ  ( %VAL(LUN), CID, %VAL(60) )
              IF ( IS .NE. 60 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7483, IUER, 'GET_CODA_IFMOD_DATA', &
     &                 'Failure in attempt to read the CODA file '// &
     &                  FINAM(1:I_LEN(FINAM))//' -- '//STR )
                   DEALLOCATE ( CIM_DATA )
                   RETURN
              END IF
         END IF
!
         IS = READ  ( %VAL(LUN), CIM_DATA, %VAL(8*11*CIH%NSS) )
         IF ( IS .NE. 8*11*CIH%NSS ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7482, IUER, 'GET_CODA_IFMOD_DATA', &
     &            'Failure in attempt to read the CODA file '// &
     &             FINAM(1:I_LEN(FINAM))//' -- '//STR )
              DEALLOCATE ( CIM_DATA )
              RETURN
         END IF
!
         CID%PDELAY = CIM_DATA(1)
         CID%GDELAY = CIM_DATA(1+CIH%NSS)
         CID%PRATE  = CIM_DATA(1+2*CIH%NSS)
         CID%GRATE  = CIM_DATA(1+3*CIH%NSS)
         CID%DDELAY = CIM_DATA(1+4*CIH%NSS)
         CID%DRATE  = CIM_DATA(1+5*CIH%NSS)
         CID%P2ND   = CIM_DATA(1+6*CIH%NSS)
         CID%G2ND   = CIM_DATA(1+7*CIH%NSS)
         CID%P3RD   = CIM_DATA(1+8*CIH%NSS)
         CID%G3RD   = CIM_DATA(1+9*CIH%NSS)
         CID%P4TH   = CIM_DATA(1+10*CIH%NSS)
!
         CALL ENDIAN_CNV_R8 ( CID%MJD_R8  )
         CALL ENDIAN_CNV_R8 ( CID%PP      )
         CALL ENDIAN_CNV_I4 ( CID%ANT_NUM )
         CALL ENDIAN_CNV_R8 ( CID%FAR_ROT )
         CALL ENDIAN_CNV_R8 ( CID%PDELAY  )
         CALL ENDIAN_CNV_R8 ( CID%GDELAY  )
         CALL ENDIAN_CNV_R8 ( CID%PRATE   )
         CALL ENDIAN_CNV_R8 ( CID%GRATE   )
         CALL ENDIAN_CNV_R8 ( CID%DDELAY  )
         CALL ENDIAN_CNV_R8 ( CID%DRATE   )
         CALL ENDIAN_CNV_R8 ( CID%P2ND    )
         CALL ENDIAN_CNV_R8 ( CID%G2ND    )
         CALL ENDIAN_CNV_R8 ( CID%P3RD    )
         CALL ENDIAN_CNV_R8 ( CID%G3RD    )
         CALL ENDIAN_CNV_R8 ( CID%P4TH    )
         CID%MJD_R8 = CID%MJD_R8 - PIM%UTC_MTAI/86400.0D0
! %%%%%%%%%%%%%%%%%%%%%%%5
!   write  ( 6 ,* ) ' sou= ', cid%sou_nam, ' mjd= ', cid%mjd_r8 ! %%%%%%%%%%%
!   write  ( 6 ,* ) ' sou= ', cid%sou_nam, ' CID%PDELAY = ', CID%PDELAY ! %%%%%%%%%%%
!   write  ( 6 ,* ) ' sou= ', cid%sou_nam, ' ant_num = ', cid%ant_num ! %%%%%%w
!   write  ( 6 ,* ) ' kk(1-4)= ', CID%GDELAY, CID%G2ND, CID%G3RD, CID%P4TH
!   write ( 6, * ) ' dat= ', cid%mjd_r8, &
!   &                        cid%pp, &
!   &                        cid%ant_num, &
!   &                        cid%far_rot, &
!   &                        cid%pdelay, &
!   &                        cid%gdelay, &
!   &                        cid%prate, &
!   &                        cid%grate, &
!   &                        cid%ddelay, &
!   &                        cid%drate, &
!   &                        cid%p2nd, &
!   &                        cid%g2nd, &
!   &                        cid%p3rd, &
!   &                        cid%g3rd, &
!   &                        cid%p4th
!         write  ( 6 ,* ) ' sou= ', cid%sou_nam, ' mjd= ', cid%mjd_r8 ! %%%%%%%%%%%
!         VEC(1) = CIM_DATA(1) ; CALL ENDIAN_CNV_R8 ( VEC(1) )
!         VEC(2) = CIM_DATA(2) ; CALL ENDIAN_CNV_R8 ( VEC(2) )
!         VEC(3) = CIM_DATA(3) ; CALL ENDIAN_CNV_R8 ( VEC(3) )
!         VEC(4) = CIM_DATA(4) ; CALL ENDIAN_CNV_R8 ( VEC(4) )
!!         VEC(5) = CIM_DATA(5) ; CALL ENDIAN_CNV_R8 ( VEC(5) )
!         VEC(6) = CIM_DATA(6) ; CALL ENDIAN_CNV_R8 ( VEC(6) )
!         VEC(7) = CIM_DATA(7) ; CALL ENDIAN_CNV_R8 ( VEC(7) )
!         VEC(8) = CIM_DATA(8) ; CALL ENDIAN_CNV_R8 ( VEC(8) )
!   write ( 6, * ) ' vec= ', vec(1:8) 
!  call pause ( 'sada' ) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        
! ------ Bypass "fill" values
!
         IF ( DABS(CID%GDELAY) > 1.0D0 ) GOTO 410
         IF ( CID%ANT_NUM .NE. CIH%IND_ANT ) THEN
              WRITE ( 6, * ) ' J1 ', INT2(J1), ' ANT_NUM = ', CID%ANT_NUM, &
     &                       ' IND_ANT = ', CIH%IND_ANT
              CALL ERR_LOG ( 7483, IUER, 'GET_CODA_IFMOD_DATA', &
     &            'Trap of internal control: ANT_NUM is not IND_ANT '// &
     &            'in file '//FINAM )
              DEALLOCATE ( CIM_DATA )
              RETURN
         END IF
!
         K_MOD(CIH%STA_IND) = K_MOD(CIH%STA_IND) + 1
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%TIM_BEG = &
     &          (CID%MJD_R8 - PIM%MJD_0)*86400.0D0 - PIM%TAI_0
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%TIM_END = &
     &           PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%TIM_BEG + &
     &           CID%PP
         DO 420 J2=1,PIM%NSOU
            IL = I_LEN(PIM%SOU(J2)%NAME)
            IF ( CID%SOU_NAM(1:IL) == PIM%SOU(J2)%NAME(1:IL) ) THEN
                 PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%SOU_IND = J2
                 GOTO 820
            END IF
 420     CONTINUE
 820     CONTINUE
         IF ( PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%SOU_IND == 0 ) THEN
              DO 430 J3=1,PIM%NSOU
                 IL1 = I_LEN(PIM%SOU(J3)%IVS_NAME)
                 IL2 = I_LEN(PIM%SOU(J3)%J2000_NAME)
                 IL3 = I_LEN(PIM%SOU(J3)%B1950_NAME)
                 IL4 = I_LEN(PIM%SOU(J3)%DB_NAME)
                 IF ( CID%SOU_NAM(1:IL1) == PIM%SOU(J3)%IVS_NAME   .OR. &
     &                CID%SOU_NAM(1:IL2) == PIM%SOU(J3)%J2000_NAME .OR. &
     &                CID%SOU_NAM(1:IL3) == PIM%SOU(J3)%B1950_NAME .OR. &
     &                CID%SOU_NAM(1:IL4) == PIM%SOU(J3)%DB_NAME         ) THEN
                      PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%SOU_IND = J3
                      GOTO 830
                  END IF
 430          CONTINUE
 830          CONTINUE
         END IF
!
         IF ( PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%SOU_IND == 0 ) THEN
              IF ( CID%SOU_NAM(1:I_LEN(CID%SOU_NAM)) .NE. 'NO_OBJECT' ) THEN
                   CALL ERR_LOG ( 7484, IUER, 'GET_CODA_IFMOD_DATA', &
     &                 'Source with name '//CID%SOU_NAM(1:I_LEN(CID%SOU_NAM))// &
     &                 ' defined in coda file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' has not been observed in experiment '//PIM%CONF%SESS_CODE )
                   DEALLOCATE ( CIM_DATA )
                   RETURN
              END IF
         END IF
!
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(0,1) = CID%GDELAY
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(1,1) = CID%GRATE
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(2,1) = CID%G2ND/2.0D0
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(3,1) = CID%G3RD/6.0D0
!
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PRAT_POL(0,1) = CID%GRATE
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PRAT_POL(1,1) = CID%G2ND
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PRAT_POL(2,1) = CID%G3RD/2.0D0
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PRAT_POL(3,1) = 0.0D0
!
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PDEL_POL(0,1) = &
     &           PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(0,1) 
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PDEL_POL(1,1) = &
     &           PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(1,1) 
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PDEL_POL(2,1) = &
     &           PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(2,1) 
         PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%PDEL_POL(3,1) = &
     &           PIM%STA(CIH%STA_IND)%MOD(K_MOD(CIH%STA_IND))%GDEL_POL(3,1) 
!
 410  CONTINUE
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7485, IUER, 'GET_CODA_IFMOD_DATA', &
     &         'Failure in attempt to close CODA file '// &
     &          FINAM )
           DEALLOCATE ( CIM_DATA )
           RETURN
      END IF
      DEALLOCATE ( CIM_DATA )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_CODA_IFMOD_DATA  !#!#
