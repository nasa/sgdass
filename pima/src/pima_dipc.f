#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_DIPC ( PIM, L_OPT, KEYWORD, VALUE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_DIPC
! *                                                                      *
! *  ### 15-DEC-2010   PIMA_DIPC   v1.1 (c)  L. Petrov  06-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  L_OPT, IUER
      CHARACTER  KEYWORD(PIM__MOPT)*(*), VALUE(PIM__MOPT)*(*)
      TYPE ( PIMA_DPC__TYPE ), POINTER ::  DPCS(:)
      CHARACTER  PCAL_DIR*128, STR*128, STR1*128, FILNAM*128, &
     &           FILS(PIM__MOBS)*128
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4, ALLOCATABLE ::  IND_FIL(:,:), STA_FLG(:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, IND_STA, N_MIS, LEV, L_FIL, IFIL, NO_TONES, &
     &           IL, IS, MJD_PT, KSTA_PC(PIM__MSTA), IFRQ, IAMB, &
     &           IND_FRQ(PIM__MFRQ), LAST_PT, KP, IER
      LOGICAL*1  FL_PC
      REAL*8     UTC_PT, TIM_PT, TIM_PC_BEG, TIM_PC_END, &
     &           TIM_SCA_BEG, TIM_SCA_END, TIM_MID, TIM_SPAN, &
     &           PCAL_AMP, PCAL_PHS, PCAL_RAT
      REAL*8     TIM(PIM__MUV), PHS(PIM__MUV), AMP(PIM__MUV), WEI(PIM__MUV)
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, CLOSEDIR
      INTEGER*4, EXTERNAL     :: I_LEN, ILEN, GET_FILE_FROM_DIR 
      REAL*8,    EXTERNAL     :: DOY_TO_TIM 
      REAL*4,    EXTERNAL     :: PHAS_CMPL_R4 
!
      IF ( L_OPT < 2 ) THEN
           CALL ERR_LOG ( 6411, IUER, 'PIMA_DIPC', 'There should be '// &
     &         'at least two options supplied for processing phase '// &
     &         'calibration from difx ascii tables' )
           RETURN 
      END IF
!
      DO 410 J1=1,PIM%NSTA
         PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL = .FALSE.
         PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE   = .FALSE.
         PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES   = 0
         PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%NPOI       = 0
         PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%NPOL       = 0
         PIM%STA(J1)%CABLE%CAB_AVAIL  = .FALSE.
         PIM%STA(J1)%CABLE%CABLE_SIGN = 0
         PIM%STA(J1)%CABLE%MEAN_CABLE = 0.0
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%PHAS    ) ) DEALLOCATE ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%PHAS )
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%AMPL    ) ) DEALLOCATE ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%AMPL )
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%FREQ    ) ) DEALLOCATE ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%FREQ )
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%RATE    ) ) DEALLOCATE ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%RATE )
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%SOU_IND ) ) DEALLOCATE ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%SOU_IND )
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8  ) ) DEALLOCATE ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8  )
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 ) ) DEALLOCATE ( PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 )
         IF ( ASSOCIATED ( PIM%STA(J1)%CABLE%CAB_DEL   ) ) DEALLOCATE ( PIM%STA(J1)%CABLE%CAB_DEL    )
 410  CONTINUE 
!
      DO 420 J2=1,2
         IF ( KEYWORD(J2) == 'pcal_dir:' ) THEN
              PCAL_DIR = VALUE(J2)
              DIR_DESC = FUNC_OPENDIR ( PCAL_DIR(1:I_LEN(PCAL_DIR))//CHAR(0) )
              IF ( DIR_DESC .EQ. 0 ) THEN
                   CALL ERR_LOG ( 6412, IUER, 'PIMA_DIPC', 'Cannot find '// &
     &                  'or cannot read directory '//PCAL_DIR )
                   RETURN 
                 ELSE 
                   IS = CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
           ELSE IF ( KEYWORD(J2) == 'num_tones:' ) THEN
              CALL CHIN ( VALUE(J2), NO_TONES )
              IF ( NO_TONES .LE. 0 ) THEN
                   CALL ERR_LOG ( 6413, IUER, 'PIMA_DIPC', 'Failure '// &
     &                 'in parsing the number of pcal tones: '// &
     &                  VALUE(J2)(1:I_LEN(VALUE(J2)))//' while an integer '// &
     &                  'number was expected' )
                   RETURN 
                 ELSE IF ( NO_TONES > PIM__MTON ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM__MTON, STR )
                   CALL ERR_LOG ( 6414, IUER, 'PIMA_DIPC', 'Too many '// &
     &                 'pcal tones: '//VALUE(J2)(1:I_LEN(VALUE(J2)))// &
     &                 ' -- more than PIM__MTON: '//STR )
                   RETURN 
              END IF
           ELSE 
              CALL ERR_LOG ( 6415, IUER, 'PIMA_DIPC', 'Keyword '// &
     &             KEYWORD(J2)(1:I_LEN(KEYWORD(J1)))//' is not supported '// &
     &            'for action DIPC' )
              RETURN 
         END IF
 420  CONTINUE 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%      write ( 6, * ) 'PCAL_DIR= ', PCAL_DIR(1:I_LEN(PCAL_DIR))  ! %%%%%%%%
!%      write ( 6, * ) 'NO_TONES= ', NO_TONES                     ! %%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      LEV = 0
      L_FIL = 0
      DO 430 J3=1,PIM__MOBS
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, PCAL_DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6416, IUER, 'PIMA_DIPC', 'Error in '// &
     &           'reading input directory '//PCAL_DIR(1:I_LEN(PCAL_DIR))// &
     &           '  '//FILNAM )
              RETURN 
         END IF
         IF ( LEV == 0 ) GOTO 830 ! End of work
         IL = ILEN(FILNAM) 
         IF ( IL < 7 ) GOTO 430
         IF ( FILNAM(IL-6:IL-2) .NE. 'PCAL_' ) GOTO 430
         IF ( L_FIL == PIM__MOBS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM__MOBS, STR )
              CALL ERR_LOG ( 6417, IUER, 'PIMA_DIPC', 'Too many pcal '// &
     &            'files, more than PIM__MOBS: '//STR )
              RETURN 
         END IF
         L_FIL = L_FIL + 1
         FILS(L_FIL) = FILNAM
 430  CONTINUE 
 830  CONTINUE 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%% write ( 6, * ) 'L_FIL = ', L_FIL ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL SORT_FAST_CH ( L_FIL, FILS )
!
      ALLOCATE ( DPCS(L_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6418, IUER, 'PIMA__DIPC', 'Failure to '// &
     &         'allocate memory for DPCS' )
           RETURN 
      END IF 
!
      ALLOCATE ( IND_FIL(PIM%NSCA,PIM%NSTA), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*PIM%NSCA*PIM%NSTA, STR )
           CALL ERR_LOG ( 6418, IUER, 'PIMA__DIPC', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for temporary array IND_FIL' )
           RETURN 
      END IF 
!
      ALLOCATE ( STA_FLG(PIM%NSCA,PIM%NSTA), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*PIM%NSCA*PIM%NSTA, STR )
           CALL ERR_LOG ( 6418, IUER, 'PIMA__DIPC', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for temporary array STA_FLG' )
           RETURN 
      END IF 
      IND_FIL = 0
      STA_FLG = 0
!
      DO 440 J4=1,L_FIL
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, 210 ) J4, L_FIL, FILS(J4)(1:I_LEN(FILS(J4))), CHAR(13)
 210          FORMAT ( 2X, I5, ' ( ', I5, ' ) File: ', A,A$ )
              CALL FLUSH ( 6 )
         END IF
         CALL ERR_PASS ( IUER, IER )
         CALL DIFX_PCAL_PARSE ( FILS(J4), DPCS(J4), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6419, IUER, 'PIMA_DIPC', 'Error in '// &
     &           'parsing DiFX PCal file '//FILS(J4) )
              RETURN 
         END IF
 440  CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           CALL CLRCH ( STR )
           WRITE ( 6, '(A)' ) STR(1:78)
           WRITE ( 6, 220) L_FIL
 220       FORMAT ( I4, ' Files have been processed' )
      END IF
!
      DO 450 J5=1,L_FIL
         TIM_PT = 0.0
         IND_STA = 0
         DO 460 J6=1,PIM%NSTA
             IF ( DPCS(J5)%STA_NAM == PIM%STA(J6)%ORIG_NAME ) THEN
                  IND_STA = J6
             END IF
 460     CONTINUE 
         IF ( IND_STA == 0 ) THEN
              CALL ERR_LOG ( 6420, IUER, 'PIMA_DIPC', 'Trap of '// &
     &            'internal control: station '//DPCS(J6)%STA_NAM// &
     &            ' did not observe in experiment '// &
     &            PIM%CONF%SESS_CODE )
              RETURN 
         END IF
         TIM_PC_BEG = DOY_TO_TIM ( PIM, DPCS(J5)%TIM_DOY_BEG(1) )
         TIM_PC_END = DOY_TO_TIM ( PIM, DPCS(J5)%TIM_DOY_BEG(DPCS(J5)%NPC) )
         DO 470 J7=1,PIM%NSCA
            TIM_SCA_BEG = PIM%TIM_R8(PIM%SCA(J7)%TIM_IND) 
            TIM_SCA_END = PIM%TIM_R8(PIM%SCA(J7)%TIM_IND + PIM%SCA(J7)%NUM_EPC - 1)
            FL_PC = .FALSE.
            IF ( ( TIM_PC_BEG .GE. TIM_SCA_BEG .AND. TIM_PC_END .LE. TIM_SCA_END ) .OR. &
     &           ( TIM_PC_BEG .LE. TIM_SCA_BEG .AND. TIM_PC_END .GE. TIM_SCA_BEG ) .OR. &
     &           ( TIM_PC_BEG .LE. TIM_SCA_END .AND. TIM_PC_END .GE. TIM_SCA_END ) .OR. &
     &           ( TIM_PC_BEG .LE. TIM_SCA_BEG .AND. TIM_PC_END .GE. TIM_SCA_END ) ) THEN
                 FL_PC = .TRUE.
            END IF 
            DO 480 J8=1,PIM%SCA(J7)%NBAS
               IF ( PIM%OBS(PIM%SCA(J7)%OBS_IND(J8))%STA_IND(1) == IND_STA .OR. &
     &              PIM%OBS(PIM%SCA(J7)%OBS_IND(J8))%STA_IND(2) == IND_STA      ) THEN 
                    STA_FLG(J7,IND_STA) = 1
                    IF ( FL_PC ) IND_FIL(J7,IND_STA) = J5
               END IF
 480        CONTINUE 
 470     CONTINUE 
 450  CONTINUE 
!
      N_MIS = 0
      DO 490 J9=1,PIM%NSTA
         KSTA_PC(J9) = 0
         DO 4100 J10=1,PIM%NSCA
            IF ( STA_FLG(J10,J9) == 1  .AND. IND_FIL(J10,J9) == 0 ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                      WRITE ( 6, 230 ) PIM%C_STA(J9), J10
 230                  FORMAT ( 'PIMA_DIPC  Station ', A, ' Scan ', I4, ' pcal is not defined' )
                 END IF
                 N_MIS = N_MIS + 1
               ELSE
                 KSTA_PC(J9) = KSTA_PC(J9) + 1
            END IF
 4100    CONTINUE 
         IF ( KSTA_PC(J9) .LE. 0 ) GOTO 490
         PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL = .TRUE.
         PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE   = .TRUE.
         PIM%STA(J9)%CABLE%CAB_AVAIL                   = .FALSE.
         PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES   = NO_TONES
         PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI       = KSTA_PC(J9) 
         PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOL       = 1
         ALLOCATE ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(NO_TONES,PIM%NFRQ,PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI) )
         ALLOCATE ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(NO_TONES,PIM%NFRQ,PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI,PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOL) )
         ALLOCATE ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(NO_TONES,PIM%NFRQ,PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI,PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOL) )
         ALLOCATE ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%RATE(NO_TONES,PIM%NFRQ,PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI,PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOL) )
         ALLOCATE ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%SOU_IND(PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI) )
         ALLOCATE ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8(PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI) )
         ALLOCATE ( PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4(PIM%STA(J9)%PCAL(PIM%CONF%FRQ_GRP)%NPOI) )
 490  CONTINUE 
!
      IF ( N_MIS > 0 .AND. PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, 240 ) N_MIS
 240       FORMAT ( 'PIMA_DIPC: phase calibration is missing for ', &
     &               I6, ' observations' )
           IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                CALL ERR_LOG ( 6422, IUER, 'PIMA_DIPC', 'There are '// &
     &              'observations without phase calibration in '// &
     &              'experiment '//PIM%CONF%SESS_CODE )
                RETURN 
           END IF
      END IF
!
      DO 4110 J11=1,PIM%NSTA
         KSTA_PC(J11) = 0
         IF ( PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%NPOI == 0 ) GOTO 4110
         DO 4120 J12=1,PIM%NSCA
            IF ( STA_FLG(J12,J11) == 0 ) GOTO 4120
            IFIL = IND_FIL(J12,J11)
            IF ( IFIL == 0 ) GOTO 4120
            KSTA_PC(J11) = KSTA_PC(J11) + 1
            DO 4130 J13=1,PIM%NFRQ
               IND_FRQ(J13) = 0
               DO 4140 J14=1,DPCS(IFIL)%NFREQ
                  IF ( DPCS(IFIL)%FREQ(J14,J13) .GE. PIM%FRQ(J13,PIM%NFRG)%FREQ .AND. &
     &                 DPCS(IFIL)%FREQ(J14,J13) .LE. PIM%FRQ(J13,PIM%NFRG)%FREQ + PIM%FRQ(J13,PIM%NFRG)%BAND_WIDTH ) THEN
                       IF ( IND_FRQ(J13) == 0 ) THEN
                            IND_FRQ(J13) = J14
                          ELSE 
                             IF ( DPCS(IFIL)%FREQ(J14,J13) < DPCS(IFIL)%FREQ(IND_FRQ(J13),J13) ) THEN
!
! ------------------------------- Select if this frequency is less than the 
! ------------------------------- previously selected
!
                                  IND_FRQ(J13) = J14
                             END IF
                       END IF
                  END IF
 4140          CONTINUE 
               IF ( IND_FRQ(J13) == 0 ) THEN
                    CALL CLRCH ( STR )
                    WRITE ( UNIT=STR(1:12), FMT='(F12.5)' ) PIM%FRQ(J13,PIM%NFRG)%FREQ*1.D-6
                    CALL ERR_LOG ( 6422, IUER, 'PIMA_DIPC', 'Trap of '// &
     &                  'internal control: cannot find pcal for frequency '// &
     &                   STR(1:I_LEN(STR))//' MHz in phase calibration '// &
     &                  'file '//FILS(IFIL) )
                    RETURN 
               END IF
               LAST_PT = 0
               KP  = 0
               TIM_PC_BEG = DOY_TO_TIM ( PIM, DPCS(IFIL)%TIM_DOY_BEG(1) )
               DO 4150 J15=1,DPCS(IFIL)%NPC   
                  TIM(J15) = (J15-1)*DPCS(IFIL)%TIM_AVR(1)
                  AMP(J15) = ABS ( DPCS(IFIL)%PCAL(IND_FRQ(J13),J15) )
                  PHS(J15) = PHAS_CMPL_R4 ( DPCS(IFIL)%PCAL(IND_FRQ(J13),J15) )
                  IF ( LAST_PT > 0 ) THEN
                       IAMB = IDNINT ( (PHS(J15)-PHS(LAST_PT))/PI2 )
                       PHS(J15) = PHS(J15) - IAMB*PI2
                  END IF
                  IF ( AMP(J15) < PIMA__PCAL_AMP_MIN ) THEN
                       WEI(J15) = 0.0D0
                     ELSE
                       WEI(J15) = 0.01D0/AMP(J15)
                       LAST_PT = J15
                       KP = KP + 1
                  END IF
 4150          CONTINUE 
!
               CALL ERR_PASS ( IUER, IER )
               CALL PCAL_MOD ( DPCS(IFIL)%NPC, TIM_PC_BEG, TIM, AMP, PHS, &
     &                         WEI, TIM_MID, TIM_SPAN, PCAL_AMP, &
     &                         PCAL_PHS, PCAL_RAT, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6423, IUER, 'PIMA_DIPC', 'Failure to'// &
     &                  ' in computing regression model of phase cal phases'// &
     &                  ' in experiment '// &
     &                   PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                  ' station '//PIM%C_STA(J11)// &
     &                  ' using DiFX pcal file '//FILS(IFIL) )
                    RETURN 
               END IF
!
               PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(1,J13,KSTA_PC(J11)) = DPCS(IFIL)%FREQ(IND_FRQ(J13),J13)
               PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(1,J13,KSTA_PC(J11),PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%NPOL) = PCAL_PHS
               PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(1,J13,KSTA_PC(J11),PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%NPOL) = PCAL_AMP
               PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%RATE(1,J13,KSTA_PC(J11),PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%NPOL) = PCAL_RAT
               PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%SOU_IND(KSTA_PC(J11)) = PIM%SCA(J11)%SOU_IND
               PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8(KSTA_PC(J11))  = TIM_PC_BEG + TIM_MID
               PIM%STA(J11)%PCAL(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4(KSTA_PC(J11)) = TIM_SPAN
 4130       CONTINUE 
 4120    CONTINUE 
 4110 CONTINUE 
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           write ( 6, '(A)' ) 'End of pima_dipc' 
      END IF
!
      DEALLOCATE ( STA_FLG )
      DEALLOCATE ( IND_FIL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_DIPC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIFX_PCAL_PARSE (  PCAL_FIL, DPC, IUER )
! ************************************************************************
! *                                                                      *
! *   Rpitine DIFX_PCAL_PARSE
! *                                                                      *
! * ### 14-DEC-2010 DIFX_PCAL_PARSE v1.0 (c)  L. Petrov  15-DEC-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  PCAL_FIL*(*)
      INTEGER*4  IUER
      INTEGER*4  MB, MIND
      PARAMETER  ( MB   = 1024 )
      PARAMETER  ( MIND = 2048 )
      CHARACTER  STR*128, STR1*128, STR2*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER, ALLOCATABLE :: BUF(:)*8192
      TYPE       DPC__STRU
          INTEGER*4  NPC
          INTEGER*4  NFREQ
          REAL*8,    POINTER :: FREQ(:,:)  => NULL()
          COMPLEX*8, POINTER :: PCAL(:,:)  => NULL()
          REAL*8,    POINTER :: TIM_DOY_BEG(:) => NULL()
          REAL*8,    POINTER :: TIM_AVR(:) => NULL()
          CHARACTER  STA_NAM*2
          CHARACTER  FILLER*6
      END TYPE   DPC__STRU
      TYPE ( DPC__STRU ) ::  DPC
!
      REAL*8     TIM_DOY, TIM_AVR
      REAL*4     PC_REAL, PC_IMAG
      INTEGER*4  J1, J2, LIND, IND(2,MIND), NB, LF, IFRQ, IER 
      INTEGER*4, EXTERNAL :: I_LEN, ILENB
!
      ALLOCATE ( BUF(MB), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MB*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 5311, IUER, 'DIFX_PCAL_PARSE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for a buffer' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( PCAL_FIL, MB, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5312, IUER, 'DIFX_PCAL_PARSE', 'Error in '// &
     &         'an attempt read the input file with pcal '//PCAL_FIL )
           RETURN 
      END IF
!
      CALL EXWORD ( BUF(1), MIND, LIND, IND, REG, IER )
      LF = (LIND-9)/4
      IF ( LF*4 + 9 .NE. LIND ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( LF, STR )
           CALL ERR_LOG ( 5313, IUER, 'DIFX_PCAL_PARSE', 'A trap '// &
     &         'of internal control in processing pcal file '// &
     &          PCAL_FIL(1:I_LEN(PCAL_FIL))//' -- the number of words '// &
     &         'is wrong: '//STR )
           RETURN 
      END IF
!
      ALLOCATE  ( DPC%FREQ(LF,NB), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*LF*NB, STR )
           CALL ERR_LOG ( 5314, IUER, 'DIFX_PCAL_PARSE', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array DPC%FREQ_PCAL' )
           RETURN 
      END IF
!
      ALLOCATE  ( DPC%PCAL(LF,NB), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LF*NB, STR )
           CALL ERR_LOG ( 5314, IUER, 'DIFX_PCAL_PARSE', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array DPC%FREQ_PCAL' )
           RETURN 
      END IF
!
      ALLOCATE  ( DPC%TIM_DOY_BEG(NB), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LF*NB, STR )
           CALL ERR_LOG ( 5315, IUER, 'DIFX_PCAL_PARSE', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array DPC%TIM_DOY_BEG' )
           RETURN 
      END IF
!
      ALLOCATE  ( DPC%TIM_AVR(NB), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NB, STR )
           CALL ERR_LOG ( 5316, IUER, 'DIFX_PCAL_PARSE', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array DPC%TIM_AVR' )
           RETURN 
      END IF
!
      DPC%NPC   = NB
      DPC%NFREQ = LF
!
      DO 410 J1=1,NB
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         DPC%STA_NAM = BUF(J1)(IND(1,1):IND(2,1))
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F11.7)', IOSTAT=IER ) DPC%TIM_DOY_BEG(J1)
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1,  STR2 )
              CALL ERR_LOG ( 5317, IUER, 'DIFX_PCAL_PARSE', 'Reading '// &
     &               'error in word 2: '//BUF(J1)(IND(1,2):IND(2,2))// &
     &               ' at the line '//STR(1:I_LEN(STR))// &
     &               ' when processing pcal file '//PCAL_FIL )
              RETURN 
         END IF
!
         READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F11.7)', IOSTAT=IER  ) DPC%TIM_AVR(J1)
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1,  STR2 )
              CALL ERR_LOG ( 5318, IUER, 'DIFX_PCAL_PARSE', 'Reading '// &
     &               'error in word 2: '//BUF(J1)(IND(1,2):IND(2,2))// &
     &               ' at the line '//STR(1:I_LEN(STR))// &
     &               ' when processing pcal file '//PCAL_FIL )
              RETURN 
         END IF
         DPC%TIM_AVR(J1) = DPC%TIM_AVR(J1)*86400.0D0
!
         LF = (LIND-9)/4
         IF ( LF*4 + 9 .NE. LIND ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( LF, STR )
              CALL ERR_LOG ( 5319, IUER, 'DIFX_PCAL_PARSE', 'A trap '// &
     &            'of internal control in processing pcal file '// &
     &             PCAL_FIL(1:I_LEN(PCAL_FIL))//' -- the number of words '// &
     &            'is wrong: '//STR )
              RETURN 
         END IF
!
         DO 420 J2=1,LF
            READ ( UNIT=BUF(J1)(IND(1,11+(J2-1)*4):IND(2,11+(J2-1)*4)), FMT='(I6)', IOSTAT=IER ) IFRQ
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( J2,  STR1 )
                 CALL CLRCH ( STR2 )
                 CALL INCH  ( J1,  STR2 )
                 CALL ERR_LOG ( 5320, IUER, 'DIFX_PCAL_PARSE', 'Reading '// &
     &               'error in word '//STR1(1:I_LEN(STR1))//': '// &
     &               BUF(J1)(IND(1,11+(J2-1)*4):IND(2,11+(J2-1)*4))// &
     &               ' at the line '//STR2(1:I_LEN(STR2))// &
     &               ' when processing pcal file '//PCAL_FIL )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J1)(IND(1,12+(J2-1)*4):IND(2,12+(J2-1)*4)), FMT='(E12.5)', IOSTAT=IER ) PC_REAL
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( J2,  STR1 )
                 CALL CLRCH ( STR2 )
                 CALL INCH  ( J1,  STR2 )
                 CALL ERR_LOG ( 5321, IUER, 'DIFX_PCAL_PARSE', 'Reading '// &
     &               'error in word '//STR1(1:I_LEN(STR1))//': '// &
     &               BUF(J1)(IND(1,12+(J2-1)*4):IND(2,12+(J2-1)*4))// &
     &               ' at the line '//STR2(1:I_LEN(STR2))// &
     &               ' when processing pcal file '//PCAL_FIL )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J1)(IND(1,13+(J2-1)*4):IND(2,13+(J2-1)*4)), FMT='(E12.5)', IOSTAT=IER ) PC_IMAG
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( J2,  STR1 )
                 CALL CLRCH ( STR2 )
                 CALL INCH  ( J1,  STR2 )
                 CALL ERR_LOG ( 5318, IUER, 'DIFX_PCAL_PARSE', 'Reading '// &
     &               'error in word '//STR1(1:I_LEN(STR1))//': '// &
     &               BUF(J1)(IND(1,13+(J2-1)*4):IND(2,13+(J2-1)*4))// &
     &               ' at the line '//STR2(1:I_LEN(STR2))// &
     &               ' when processing pcal file '//PCAL_FIL )
                 RETURN 
            END IF
!
            DPC%FREQ(J2,J1) = IFRQ*1.0D6
            DPC%PCAL(J2,J1) = CMPLX ( PC_REAL, PC_IMAG )
 420     CONTINUE 
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DIFX_PCAL_PARSE  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   DOY_TO_TIM ( PIM, TIM_DOY )
! ************************************************************************
! *                                                                      *
! *   Routine DOY_TO_TIM  computes time differences between              *
! *   the date TIM_DOY in the format of a float number of day_of_year    *
! *   elapsed since (PIM%MJD_0,PIM%TAI_0)
! *                                                                      *
! *  ### 16-DEC-2010   DOY_TO_TIM  v1.0 (c)  L. Petrov  16-DEC-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INCLUDE   'astro_constants.i'
      REAL*8     DOY_TO_TIM 
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4    I_YEAR4   
      PARAMETER  ( I_YEAR4 = 1461 ) ! number of days in the 4 year cycle
      INTEGER*4  NDAYS, K4_YEAR, L4_DAY, L4_YEAR, J1, MJD_NY, DAYS4(4)
      DATA DAYS4 / 0, 366, 731, 1096 /
      REAL*8     TIM_DOY
!
! --- K4_YEAR -- Number of four-year cycles elapsed from J2000.0
! --- L4_DAY  -- number of days elapsed from the beginning of a four-year cycle
!
      NDAYS = PIM%MJD_0 - J2000__MJD  
      K4_YEAR = (NDAYS+1)/I_YEAR4
      L4_DAY  = (NDAYS+1) - I_YEAR4*K4_YEAR
      IF ( L4_DAY .LE. 0 ) THEN
           K4_YEAR = K4_YEAR - 1
           L4_DAY  = L4_DAY + I_YEAR4
      END IF
!
      DO 410 J1=1,4
         IF ( L4_DAY .LT. DAYS4(J1) ) GOTO 810
         L4_YEAR = J1
  410 CONTINUE
  810 CONTINUE
!
! --- MJD data of the new year of the year of the date PIM%MJD_0
!
      MJD_NY = J2000__MJD + I_YEAR4*K4_YEAR + DAYS4(L4_YEAR)
!
      DOY_TO_TIM = (TIM_DOY-1.0D0)*86400.D0 - &
     &             ( (PIM%MJD_0 - MJD_NY)*86400.0D0 + PIM%TAI_0 ) - &
     &             PIM%UTC_MTAI
      RETURN
      END  FUNCTION   DOY_TO_TIM  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PCAL_MOD ( NP, TIM_PC_BEG, TIM, AMP, PHS, WEI, TIM_MID, &
     &                      TIM_SPAN, PCAL_AMP, PCAL_PHS, PCAL_RAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PCAL_MOD 
! *                                                                      *
! *  ### 19-DEC-2010    PCAL_MOD   v1.0 (c)  L. Petrov  19-DEC-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  NP, IUER
      REAL*8     TIM_PC_BEG, TIM(NP), AMP(NP), PHS(NP), WEI(NP), &
     &           TIM_MID, TIM_SPAN, PCAL_PHS, PCAL_AMP, PCAL_RAT
      INTEGER*4    PIMA__PCAL_DEG 
      PARAMETER  ( PIMA__PCAL_DEG = 5 )
      REAL*8     TIM_AVR, WW, TIM_MIN, TIM_MAX, MEAN_T, DR_VAL, SH_VAL, &
     &           DR_SIG, SH_SIG, LEG_EST(0:PIMA__PCAL_DEG), &
     &           LEG_ERR(0:PIMA__PCAL_DEG)
      INTEGER*4  J1, J2, KP, IND_LAST, IER
      REAL*8,    EXTERNAL :: LEGENDRE_POL 
!
      KP = 0
      TIM_AVR = 0.0D0
      TIM_MAX = 0.0D0
      TIM_MIN = -1.D9
      PCAL_AMP = 0.0D0
      WW = 0.0D0
      DO 410 J1=1,NP
         IF ( AMP(J1) < PIMA__PCAL_AMP_MIN ) THEN
              WEI(J1) = 0.0D0
            ELSE
              KP = KP + 1
              IND_LAST = J1
              WEI(J1) = 0.01D0/AMP(J1)
              TIM_AVR = TIM_AVR + TIM(J1)*WEI(J1)
              WW = WW + WEI(J1)
              TIM_MAX = TIM(J1)
              IF ( TIM_MIN < -86400.0D0 ) TIM_MIN = TIM(J1)
              PCAL_AMP = PCAL_AMP + AMP(J1)
         END IF
 410  CONTINUE 
      IF ( KP == 0 ) THEN
           TIM_SPAN = TIM(1)
           TIM_MID  = TIM(1)
           PCAL_PHS = 0.0D0
           PCAL_AMP = 2.0D0*PIMA__PCAL_AMP_MIN 
           CALL ERR_LOG  ( 0, IUER )
           RETURN 
      END IF
      TIM_AVR = TIM_AVR/WW
      IF ( KP == 1 ) THEN
           TIM_MID  = TIM_PC_BEG + TIM_AVR
           TIM_SPAN = MAX ( DABS(TIM_AVR-TIM_MIN), DABS(TIM_AVR-TIM_MAX) ) 
           PCAL_PHS = 0.0D0
           CALL ERR_LOG  ( 0, IUER )
           RETURN 
      END IF 
!
      PCAL_AMP = PCAL_AMP/KP
      CALL ERR_PASS ( IUER, IER )
      CALL RGRW8 ( NP, TIM, PHS, WEI, %VAL(0), MEAN_T, DR_VAL, SH_VAL, &
     &             DR_SIG, SH_SIG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6451, IUER, 'PIMA_DIPC', 'Error in an '// &
     &         'attempt to compute cofficients of linear '// &
     &         'regression over phase calibration phases' )
           RETURN 
      END IF
!
      PCAL_RAT = DR_VAL
      IF ( KP < 7 ) THEN
           PCAL_PHS = SH_VAL + DR_VAL*(TIM_AVR - MEAN_T)
           CALL ERR_LOG  ( 0, IUER )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL LEGENDRE_REGR ( IND_LAST, TIM, PHS, WEI, PIMA__PCAL_DEG, &
     &                     LEG_EST, LEG_ERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6451, IUER, 'PIMA_DIPC', 'Error in an '// &
     &         'attempt to compute cofficients of Legendre '// &
     &         'polynonials for phase calibration phases' )
           RETURN 
      END IF
!
      PCAL_PHS = 0.0D0
!!  write ( 6, *) 't1= ', tim(1), ' tn= ', tim(ind_last), ' ta= ', tim_avr ! %%%
      DO 420 J2=0,PIMA__PCAL_DEG
         PCAL_PHS = PCAL_PHS + LEG_EST(J2)*LEGENDRE_POL ( J2, &
     &                                     TIM(1), TIM(IND_LAST), TIM_AVR )
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PCAL_MOD !#!  
