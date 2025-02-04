      SUBROUTINE SIMUL_PARSE_COVZEN ( SIMUL, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMUL_PARSE_COVZEN 
! *                                                                      *
! * ### 08-SEP-2021 SIMUL_PARSE_COVZEN v2.2 (c) L. Petrov 01-DEC-2021 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'simul.i'
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      INTEGER*4  IVRB, IUER
      INTEGER*8  DIR_DESC
      INTEGER*4  MF, MP, MC, MIND
      PARAMETER  ( MF   = 2048 )
      PARAMETER  ( MP   = 512 )
      PARAMETER  ( MC   = 64*1024 )
      PARAMETER  ( MIND =  32 )
      REAL*8     TIM(MP)
      CHARACTER  C_FIL(MF)*128, FILNAM*128, BUF(MC)*128, &
     &           CN_STA(SIM__MSTA)*8, CG_MIS(SIM__MSTA)*8, &
     &           CM_MIS(SIM__MSTA)*8, MF_PAT*32, FIL_STA_NAM*8, &
     &           STR*4096, STR1*32, FIL_MF*128, FIL_BIN*128
      PARAMETER  ( MF_PAT = 'normal_notilt' )      
      LOGICAL*1  LEX
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, ISTA, IND_COR, &
     &           NC, NM, LEV, L_FIL, IS, NP, ID, IE, IL, LIND, IND(2,MIND), &
     &           LM_MIS, LG_MIS, LUN, NEL, N2, NO2, IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, GET_FILE_FROM_DIR, I_LEN, ILEN, LINDEX, LTM_DIF
!
      L_FIL = 0
      LEV   = 0
      CALL CLRCH ( FIL_MF ) 
      DO 410 J1=1,MF
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, SIMUL%CNF%ZEN_COV_DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 1481, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &            'reading input directory '//TRIM(SIMUL%CNF%ZEN_COV_DIR)// &
     &            '  '//FILNAM )
              RETURN 
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '~' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, TRIM(MF_PAT) ) .GT. 0 ) THEN
              FIL_MF = FILNAM
              GOTO 410
         END IF
         ID = LINDEX ( FILNAM, '_' ) 
         IE = LINDEX ( FILNAM, '.' ) 
         IF ( ID < 1 .OR. IE < 1 .OR. IE - ID < 3 ) GOTO 410
         CALL CLRCH ( FIL_STA_NAM )
         FIL_STA_NAM = FILNAM(ID+1:IE-1)
         CALL TRAN ( 11, FIL_STA_NAM, FIL_STA_NAM )
         IF ( INDEX ( FILNAM, 'zen_cov' ) .GT. 0 .AND. &
     &        INDEX ( FILNAM, '.txt'    ) .GT. 0 .AND. &
     &        LTM_DIF ( 0, SIMUL%NSTA, SIMUL%STA_NAM, FIL_STA_NAM  ) > 0 ) THEN
              L_FIL = L_FIL + 1
              IF ( L_FIL > MF ) THEN
                   CALL CLRCH ( STR1 ) 
                   CALL INCH  ( MF, STR1 )
                   CALL ERR_LOG ( 1482, IUER, 'SIMUL_PARSE_COVZEN', 'Too many '// &
     &                 'files in the input directory '//TRIM(SIMUL%CNF%ZEN_COV_DIR)// &
     &                 ' -- more than '//STR1 ) 
                   RETURN 
              END IF
              C_FIL(L_FIL) = FILNAM
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           CALL ERR_LOG ( 1483, IUER, 'SIMUL_PARSE_COVZEN', 'No zen_cov '// &
     &         'files has been found in the input in directory '// &
     &          SIMUL%CNF%ZEN_COV_DIR )
           RETURN 
      END IF
!
      IF ( ILEN(FIL_MF) == 0 ) THEN
           CALL ERR_LOG ( 1484, IUER, 'SIMUL_PARSE_COVZEN', 'Could not '// &
     &         'find mapping extra noise file with pattern '//TRIM(MF_PAT)// &
     &         ' in directory '//SIMUL%CNF%ZEN_COV_DIR )
           RETURN 
      END IF
!
      CALL SORT_CH ( L_FIL, C_FIL )
      LM_MIS = 0
      LG_MIS = 0
      DO 420 J2=1,SIMUL%NSTA
         IL = ILEN(SIMUL%STA_NAM(J2))
         DO 430 J3=1,L_FIL
            ID = LINDEX ( C_FIL(J3), '_' ) 
            IE = LINDEX ( C_FIL(J3), '.' ) 
            IF ( ID < 1 .OR. IE < 1 .OR. IE - ID < 3 ) GOTO 430
            FIL_STA_NAM = C_FIL(J3)(ID+1:IE-1)
            CALL TRAN ( 11, FIL_STA_NAM, FIL_STA_NAM )
            IF ( FIL_STA_NAM(1:IL) == SIMUL%STA_NAM(J2)(1:IL) ) THEN
                 FIL_BIN = C_FIL(J3)(1:ILEN(C_FIL(J3))-4)//'.bin'
                 INQUIRE ( FILE=FIL_BIN, EXIST=LEX )
                 IF ( .NOT. LEX ) THEN
                      LG_MIS = LG_MIS + 1
                      CG_MIS(LG_MIS) = SIMUL%STA_NAM(J2)
                 END IF
                 GOTO 420
            END IF
 430     CONTINUE 
         LM_MIS = LM_MIS + 1
         CM_MIS(LM_MIS) = SIMUL%STA_NAM(J2)
 420  CONTINUE 
      IF ( LM_MIS > 0 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL LIST_TO_LINE ( LM_MIS, CM_MIS, ',', STR )
           CALL INCH  ( LM_MIS, STR1 )
           CALL ERR_LOG ( 1485, IUER, 'SIMUL_PARSE_COVZEN', 'Could not '// &
     &         'find ascii covariance model files for '//TRIM(STR1)// &
     &         ' stations '//TRIM(STR)//' in directory '//SIMUL%CNF%ZEN_COV_DIR )
           RETURN 
      END IF
      IF ( LG_MIS > 0 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL LIST_TO_LINE ( LG_MIS, CG_MIS, ',', STR )
           CALL INCH  ( LG_MIS, STR1 )
           CALL ERR_LOG ( 1486, IUER, 'SIMUL_PARSE_COVZEN', 'Could not '// &
     &         'find binary covariance model files for '//TRIM(STR1)// &
     &         ' stations '//TRIM(STR)//' in directory '//SIMUL%CNF%ZEN_COV_DIR )
           RETURN 
      END IF
!
      SIMUL%N_COV = L_FIL
      ALLOCATE ( SIMUL%COV(SIMUL%N_COV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1487, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &         'allocation of dynamic memory for COV object' )
           RETURN 
      END IF
!
      DO 440 J4=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( C_FIL(J4), MP, BUF, NP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1488, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &            'reading covariance file '//C_FIL(J4) )
              RETURN 
         END IF
         IF ( BUF(1)(1:LEN(COVZEN__LABEL)) .NE. COVZEN__LABEL ) THEN
              CALL ERR_LOG ( 1489, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &            'parsing covariance file '//TRIM(C_FIL(J4))//' -- '// &
     &            'the first line is '//TRIM(BUF(1))//' while '// &
     &             COVZEN__LABEL//' was expected' )
              RETURN 
         END IF
!
         ISTA = 0
         DO 450 J5=2,NP
            CALL EXWORD ( BUF(J5), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )            
            IF ( BUF(J5)(IND(1,1):IND(2,1)) == 'Station:' ) THEN
                 ISTA = LTM_DIF ( 0, SIMUL%NSTA, SIMUL%STA_NAM, BUF(J5)(IND(1,2):IND(2,2)) )
!
! -------------- This station is not participating in this experiment.
! -------------- Let us bypass it.
!
                 IF ( ISTA < 1 ) GOTO 440
!
! -------------- Participated. Ok, proceed
!
                 SIMUL%COV(ISTA)%STA_NAM = BUF(J5)(IND(1,2):IND(2,2)) 
                 SIMUL%COV(ISTA)%FIL_COV = C_FIL(J4)
               ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) == 'Interval:' ) THEN
                 SIMUL%COV(ISTA)%INT_NAM = BUF(J5)(IND(1,2):IND(2,2)) 
               ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) == 'Rms:'  ) THEN
                 READ ( UNIT=BUF(J5)(IND(1,2):IND(2,2)), FMT='(F15.5)' ) SIMUL%COV(ISTA)%RMS 
                 SIMUL%COV(ISTA)%RMS = 1.D-12*SIMUL%COV(ISTA)%RMS 
               ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) == 'Num_cor:'  ) THEN
                 READ ( UNIT=BUF(J5)(IND(1,2):IND(2,2)), FMT='(I5)' ) SIMUL%COV(ISTA)%NP
                 ALLOCATE ( SIMUL%COV(ISTA)%DTIM(SIMUL%COV(ISTA)%NP), STAT=IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 1490, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &                    'allocation of dynamic memory for DTIM object' )
                      RETURN 
                 END IF
                 ALLOCATE ( SIMUL%COV(ISTA)%COR(SIMUL%COV(ISTA)%NP), STAT=IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 1491, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &                    'allocation of dynamic memory for COV object' )
                      RETURN 
                 END IF
               ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) == 'Corr:' ) THEN
                 READ ( UNIT=BUF(J5)(IND(1,2):IND(2,2)), FMT='(I5)'    ) IND_COR
                 READ ( UNIT=BUF(J5)(IND(1,4):IND(2,4)), FMT='(F15.5)' ) SIMUL%COV(ISTA)%DTIM(IND_COR+1)
                 READ ( UNIT=BUF(J5)(IND(1,6):IND(2,6)), FMT='(F15.5)' ) SIMUL%COV(ISTA)%COR(IND_COR+1)
            END IF
 450     CONTINUE 
!
         FIL_BIN =  C_FIL(J4)(1:ILEN(C_FIL(J4))-4)//'.bin'
         CALL ERR_PASS ( IUER, IER )
         CALL BINF_OPEN ( FIL_BIN, 'OLD', LUN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1492, IUER, 'SIMUL_PARSE_COVZEN', &
     &            'Error in opening covariance binary file '// &
     &             FIL_BIN )
              RETURN 
         END IF
!
         CALL CLRCH ( STR )
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', LEN(ACM__LABEL), STR, NEL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1493, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &            'reading the magic of the input covariance file '// &
     &             FIL_BIN )
              RETURN 
         END IF
         IF ( STR(1:LEN(ACM__LABEL)) .NE. ACM__LABEL ) THEN
              CALL TRAN ( 13, STR, STR )
              CALL ERR_LOG ( 1494, IUER, 'SIMUL_PARSE_COVZEN', 'Cannot '// &
     &            'recoginze magic in the input covariance file '// &
     &             TRIM(FIL_BIN)//' expected '//ACM__LABEL//' but got '// &
     &             STR )
              RETURN 
         END IF
!
         CALL RDBIN_ARRAY ( LUN, 'B1', 8,  STR, NEL,    IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', 8,  STR, NEL,    IER )
         CALL RDBIN_ARRAY ( LUN, 'B1', 8,  STR, NEL,    IER )
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  SIMUL%COV(ISTA)%NO,      NEL, IER )
         IF ( MOD(SIMUL%COV(ISTA)%NO,2) == 0 ) THEN
              N2 =  SIMUL%COV(ISTA)%NO/2
            ELSE
              N2 = (SIMUL%COV(ISTA)%NO+1)/2
         END IF
         CALL RDBIN_ARRAY ( LUN, 'R8', 1,  SIMUL%COV(ISTA)%INP_STP, NEL, IER )
         CALL RDBIN_ARRAY ( LUN, 'R8', 1,  SIMUL%COV(ISTA)%MOD_STP, NEL, IER )
         CALL RDBIN_ARRAY ( LUN, 'R8', 1,  SIMUL%COV(ISTA)%SIG_SQ_DIAG, NEL, IER )
         CALL RDBIN_ARRAY ( LUN, 'R8', 1,  SIMUL%COV(ISTA)%COV0, NEL, IER )
         NO2 =  SIMUL%COV(ISTA)%NO*(SIMUL%COV(ISTA)%NO+1)/2
     &        
         ALLOCATE ( SIMUL%COV(ISTA)%COVS(NO2),                   &
     &              SIMUL%COV(ISTA)%COR_OBS(SIMUL%COV(ISTA)%NO), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1495, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &            'allocation of dynamic memory for COVS and COR_OBS arrays for station '// &
     &             SIMUL%STA_NAM(ISTA)  )
              RETURN 
         END IF
         CALL RDBIN_ARRAY ( LUN, 'R8', NO2,                SIMUL%COV(ISTA)%COVS,    NEL, IER )
         CALL RDBIN_ARRAY ( LUN, 'R8', SIMUL%COV(ISTA)%NO, SIMUL%COV(ISTA)%COR_OBS, NEL, IER )
         CALL BINF_CLOSE  ( LUN, IER )
 440  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FIL_MF, MC, BUF, NM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1496, IUER, 'SIMUL_PARSE_COVZEN', 'Error in '// &
     &         'reading mapping function extra noise file '//FIL_MF )
           RETURN 
      END IF
!
      NC = 0
      DO 460 J6=1,SIMUL%NSTA
         IF ( SIMUL%COV(J6)%STA_NAM .NE. SIMUL%STA_NAM(J6) ) THEN
              NC = NC + 1
              CN_STA(NC) = SIMUL%STA_NAM(J6) 
              GOTO 460
         END IF
!
         SIMUL%COV(J6)%NE = 0
         DO 470 J7=1,NM
            IF ( BUF(J7)(1:8)   == 'Station:' .AND. &
     &           BUF(J7)(10:17) ==  SIMUL%STA_NAM(J6) ) THEN
                 SIMUL%COV(J6)%NE = SIMUL%COV(J6)%NE + 1
            END IF
 470     CONTINUE 
         IF ( SIMUL%COV(J6)%NE == 0 ) THEN
              CALL ERR_LOG ( 1497, IUER, 'SIMUL_PARSE_COVZEN', &
     &            'No data for station '//SIMUL%STA_NAM(J6)// &
     &            ' were found in the mapping function rms was '// &
     &            'found in file '//FIL_MF )
              RETURN 
         END IF
         ALLOCATE ( SIMUL%COV(J6)%EL_MF(SIMUL%COV(J6)%NE),  &
     &              SIMUL%COV(J6)%MF_RMS(SIMUL%COV(J6)%NE), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1498, IUER, 'SIMUL_PARSE_COVZEN', 'Error '// &
     &            'in attempt to allocate memory for EL_MF and MF_RMS '// &
     &            'arrays' )
              RETURN 
         END IF
!
         SIMUL%COV(J6)%NE = 0
         DO 480 J8=1,NM
            IF ( BUF(J8)(1:8)   == 'Station:' .AND. &
     &           BUF(J8)(10:17) ==  SIMUL%STA_NAM(J6) ) THEN
                 SIMUL%COV(J6)%NE = SIMUL%COV(J6)%NE + 1
                 READ ( UNIT=BUF(J8)(25:31), FMT='(F7.4)'  ) SIMUL%COV(J6)%EL_MF(SIMUL%COV(J6)%NE)
                 READ ( UNIT=BUF(J8)(43:49), FMT='(F7.2)'  ) SIMUL%COV(J6)%MF_RMS(SIMUL%COV(J6)%NE)
                 SIMUL%COV(J6)%EL_MF(SIMUL%COV(J6)%NE)  = SIMUL%COV(J6)%EL_MF(SIMUL%COV(J6)%NE)*DEG__TO__RAD
                 SIMUL%COV(J6)%MF_RMS(SIMUL%COV(J6)%NE) = 1.D-12*SIMUL%COV(J6)%MF_RMS(SIMUL%COV(J6)%NE)
            END IF
 480     CONTINUE 
 460  CONTINUE 
      IF ( NC > 0 ) THEN
           CALL CLRCH ( STR  ) 
           CALL LIST_TO_LINE ( NC, CN_STA, ' ', STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( NC, STR1 ) 
           CALL ERR_LOG ( 1499, IUER, 'SIMUL_PARSE_COVZEN', 'No covariance '// &
     &         'in directory '//TRIM(SIMUL%CNF%ZEN_COV_DIR)//' was found for '// &
     &          TRIM(STR1)//' stations: '//STR )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SIMUL_PARSE_COVZEN  !#!#
