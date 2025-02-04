      SUBROUTINE BNC_READ ( FILIN, ANC, LUN, FRQ_ARR, POL_ARR, ID_ARR,  &
     &                      NEP_ARR, OFF_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine BNC_READ 
! *                                                                      *
! *   read the contents of the binary file to                            *
! *                                                                      *
! *                                                                      *
! *  ### 27-JUL-2022     BNC_READ  v1.2 (c)  L. Petrov  27-JUL-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
!@IN_ATP@!      INCLUDE   'astro_constants.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      CHARACTER  FILIN*(*)
      CHARACTER  STR_DO_DATE*24,  STR_TSYS_DATE*24, STR_MET_DATE*24
      CHARACTER  STR_EPO_DATE*24, STR_UTCMTAI*5, STR*128, TIT*64
      INTEGER*4  IUER
      REAL*8     TIM_1ST, FRQ_ARR(ANC__MTPS)
      INTEGER*1  POL_ARR(ANC__MTPS)
      CHARACTER  ID_ARR(ANC__MTPS)*4
      INTEGER*4  NEP_ARR(ANC__MTPS)
      INTEGER*8  OFF_ARR(ANC__MTPS)
      INTEGER*4  LUN, NBT, IER 

!@@!      LUN = 18
!
! --- Open Binary File to read
!
      IUER = -1
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
! --- Read the contents of the binary file in the order they were 
!     written in the "BNC_WRITE" routine
!
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', LEN(BNC__LABEL), ANC%EXP_CODE,      &
     &                   NBT, IUER )
      IF ( NBT .NE. LEN(BNC__LABEL) ) THEN
         IUER = -1
         CALL ERR_LOG ( 5002, IUER, 'BNC_READ',                         &
     &           'Did not find a BNC magic at the begnning of '//FILIN )
         CALL EXIT ( 1 ) 
      END IF
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 16, ANC%EXP_CODE, NBT, IUER )        ! Experiment Code
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 8, ANC%STA_NAM,  NBT, IUER )         ! Station name
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 5, STR_UTCMTAI,  NBT, IUER )        ! UTC_MTAI
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_DO_DATE(1:24), NBT, IUER )   ! Initial DATA_ON block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_TSYS_DATE(1:24), NBT, IUER ) ! Initial TSYS block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_MET_DATE(1:24), NBT, IUER )  ! Initial METEO block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_DOO, NBT, IUER )         ! Number of DATA_ON (scans)
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPS, NBT, IUER )         ! Number of TP_Sensors
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TSYS, NBT, IUER )        ! Number of TSYS
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'R8', ANC%NUM_TPS, FRQ_ARR, NBT, IUER )
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_TPS, POL_ARR, NBT, IUER )
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, ID_ARR,  NBT, IUER )
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, NEP_ARR, NBT, IUER )
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_TPS, OFF_ARR, NBT, IUER )
! ---
      IER = IUER
! ---
      IUER = 0
      CALL DATE_TO_TIME ( STR_DO_DATE, ANC%MJD_DOO, ANC%TAI_DOO, IUER )
! ---
      IUER = 0
      CALL DATE_TO_TIME ( STR_TSYS_DATE, ANC%MJD_TSYS, ANC%TAI_TSYS, IUER )
! ---
      IUER = 0
      CALL DATE_TO_TIME ( STR_MET_DATE, ANC%MJD_MET, ANC%TAI_MET, IUER )
! ---
      CALL ERR_PASS (IER, IUER )      
! ---
      RETURN
      END  SUBROUTINE BNC_READ  !#!#
