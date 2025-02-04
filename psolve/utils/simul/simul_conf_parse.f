      SUBROUTINE SIMUL_CONF_PARSE ( SIMUL, FIL_PAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMUL_CONF_PARSE
! *                                                                      *
! * ### 01-DEC-2021  SIMUL_CONF_PARSE v1.0 (c) L. Petrov 01-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'simul.i'
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      CHARACTER  FIL_PAR*(*)
      INTEGER*4  IUER  
      LOGICAL*1  LEX
      INTEGER*4  MP, MIND
      PARAMETER  ( MP   = 128 ) 
      PARAMETER  ( MIND = 32  ) 
      CHARACTER  BUF(MP)*128, STR*128, STR1*128
      INTEGER*4  J1, J2, J3, NP, LIND, IND(2,MIND), N_SMP, IER
!
      INQUIRE ( FILE=FIL_PAR, EXIST=LEX )
      IF ( .NOT. LEX  ) THEN
           CALL ERR_LOG ( 7471, IUER, 'SIMUL_CONF_PARSE', 'Cannot find '// &
     &         'VLBI simulation parameter file '//FIL_PAR )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_PAR, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7472, IUER, 'SIMUL_CONF_PARSE', 'Error in reading '// &
     &         'VLBI simulation parameter file '//FIL_PAR )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(SIMPAR__LABEL)) == SIMPAR__LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 7473, IUER, 'SIMUL_CONF_PARSE', 'Cannot recoginize '// &
     &         'the first line of the VLBI simulation parameter file '// &
     &          TRIM(FIL_PAR)//' -- magic '//SIMPAR__LABEL//' was expected '// &
     &         'but got '//STR )
           RETURN 
      END IF
!
      N_SMP = 0
      DO 410 J1=2,NP
         IF ( BUF(J1)(1:1) == '#' .OR. BUF(J1)(1:1) == ' ' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, ' '//CHAR(0)//CHAR(9), IER ) 
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'VTD_CONF:' ) THEN
              SIMUL%CNF%VTD_CONF = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'VCAT_CONF:' ) THEN
              SIMUL%CNF%VCAT_CONF = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'REPO_IN:' ) THEN
              SIMUL%CNF%REPO_IN = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'REPO_OUT:' ) THEN
              SIMUL%CNF%REPO_OUT = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'STP_DIR:' ) THEN
              SIMUL%CNF%STP_DIR = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NERS_CONF:' ) THEN
              SIMUL%CNF%NERS_CONF = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SBT_TABLE:' ) THEN
              SIMUL%CNF%SBT_TABLE = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ZEN_COV_DIR:' ) THEN
              SIMUL%CNF%ZEN_COV_DIR = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'RH_MODE:' ) THEN
              SIMUL%CNF%RH_MODE = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GR_DEL_MODE:' ) THEN
              SIMUL%CNF%GR_DEL_MODE = BUF(J1)(IND(1,2):IND(2,2))
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'WHT_RMS:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F22.15)', IOSTAT=IER ) SIMUL%CNF%WHT_RMS 
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DIL_ZEN_COV:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F22.15)', IOSTAT=IER ) SIMUL%CNF%DIL_ZEN_COV 
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DIL_MAP_ERR:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F22.15)', IOSTAT=IER ) SIMUL%CNF%DIL_MAP_ERR 
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DIL_OBS_NOISE:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) SIMUL%CNF%DIL_OBS_NOISE
              N_SMP = N_SMP + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ISEED:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I10)', IOSTAT=IER ) SIMUL%CNF%ISEED
              N_SMP = N_SMP + 1
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 7474, IUER, 'SIMUL_CONF_PARSE', 'Error in parsing '// &
     &            'value '//BUF(J1)(IND(1,2):IND(2,2))//' of the keyword '// &
     &             BUF(J1)(IND(1,1):IND(2,1))//' at the '//TRIM(STR)//'-th line '// &
     &            ' of the VLBI simulation parameter file '//FIL_PAR )
              RETURN 
         END IF
 410  CONTINUE 
!
      IF ( N_SMP < M__SMP ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_SMP, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M__SMP, STR1 )
           CALL ERR_LOG ( 7475, IUER, 'SIMUL_CONF_PARSE', 'Only '//TRIM(STR)// &
     &        ' out of '//TRIM(STR1)//' keywrds were found in the VLBI '// &
     &        'simulation parameter file '//FIL_PAR )
           RETURN 
      END IF
      SIMUL%CNF%CONF_FILE = FIL_PAR
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE SIMUL_CONF_PARSE  !#!#
