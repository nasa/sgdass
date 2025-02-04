      SUBROUTINE PIMA_GET_VERA_TSYS ( PIM, VTD, IND_STA, TSYS_FINAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_VERA_TSYS parses VERA Tsys files and extracts    *
! *   information about phase calibration measurements.                  *
! *                                                                      *
! * ## 28-MAR-2006 PIMA_GET_VERA_TSYS v1.0 (c) L. Petrov 28-MAR-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      TYPE     ( PIM_TSYS__TYPE ) :: TSYS_TMP
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  TSYS_FINAM*(*)
      INTEGER*4  IND_STA, IUER
      REAL*8     EPS_TIME, TIM_MARGIN 
      PARAMETER  ( EPS_TIME   = 40.0D0  ) ! sec
      PARAMETER  ( TIM_MARGIN = 600.0D0 ) ! sec
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 4096 )
      CHARACTER  STR*128, SOU_NAM*8, SOU_NAM_ARR(MBUF)*8
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, J4, IND_SCA, NBUF, N_TSYS, IER
      REAL*8     TIM_TSYS, TAU_GR, TAU_PH, RATE_PH, AZ(2), ELEV(2), &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), FREQ_VTD
      REAL*8     START_TIME_HR, STOP_TIME_HR, TAI_START, TAI_STOP, TIM_SCA, &
     &           TIM_HR, TIME_MID(MBUF), TSYS_VAL(MBUF)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE  = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = PIM%REF_FREQ
      OBS_TYP%STATUS  = VTD__BND 
!
      INQUIRE ( FILE=TSYS_FINAM, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7241, IUER, 'PIMA_GET_VERA_TSYS', 'Cannot find '// &
     &         'VERA Tsys file '//TSYS_FINAM )
           RETURN 
      END IF
!    
      ALLOCATE ( BUF(MBUF) )
      CALL RD_TEXT ( TSYS_FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7242, IUER, 'PIMA_GET_VERA_TSYS', 'Cannot find '// &
     &         'VERA Tsys file '//TSYS_FINAM )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IND_SCA = 0
      N_TSYS  = 0
      CALL CLRCH ( SOU_NAM )
      DO 410 J1=1,NBUF
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
         IF ( BUF(J1)(1:11) == ' # *** SCAN' ) THEN
              IND_SCA  = 0
              CALL CHIN ( BUF(J1)(12:15), IND_SCA ) 
              IF ( IND_SCA < 1  .OR.  IND_SCA > 9999 ) THEN
                   CALL ERR_LOG ( 7243, IUER, 'PIMA_GET_VERA_TSYS', &
     &                 'Failure to decode scan index in parsing the '// &
     &                  STR(1:I_LEN(STR))//'th line '// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//' of the VERA Tsys '// &
     &                 'file '//TSYS_FINAM )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(26:36), FMT='(F11.5)', IOSTAT=IER ) START_TIME_HR
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7244, IUER, 'PIMA_GET_VERA_TSYS', &
     &                 'Failure to decode start time in parsing the '// &
     &                  STR(1:I_LEN(STR))//'th line '// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//' of the VERA Tsys '// &
     &                 'file '//TSYS_FINAM )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              TAI_START = START_TIME_HR*3600.0D0 - PIM%UTC_MTAI
!
              READ ( UNIT=BUF(J1)(42:52), FMT='(F11.4)', IOSTAT=IER ) STOP_TIME_HR
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7245, IUER, 'PIMA_GET_VERA_TSYS', &
     &                 'Failure to decode stop time in parsing the '// &
     &                  STR(1:I_LEN(STR))//'th line '// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//' of the VERA Tsys '// &
     &                 'file '//TSYS_FINAM )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              TAI_STOP = STOP_TIME_HR*3600.0D0 - PIM%UTC_MTAI
!
              SOU_NAM = BUF(J1)(61:68)
!
              IF ( IND_SCA > PIM%NSCA ) THEN
                   IER = -1
                   CALL ERR_LOG ( 7246, IER, 'PIMA_GET_VERA_TSYS', &
     &                 'Wrong scan index in the '// &
     &                  STR(1:I_LEN(STR))//'th line '// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//' of the VERA Tsys '// &
     &                 'file '//TSYS_FINAM )
                   GOTO 810
!@                   DEALLOCATE ( BUF )
!@                   RETURN 
              END IF
              TIM_SCA = PIM%TAI_0 + PIM%TIM_R8(PIM%SCA(IND_SCA)%TIM_IND)
              IF ( TAI_STOP < TAI_START ) TAI_STOP = TAI_STOP + 86400.0D0
              IF ( TIM_SCA .GE. TAI_START - EPS_TIME  .AND. &
     &             TIM_SCA .LE. TAI_STOP  + EPS_TIME        ) THEN
                   IF ( SOU_NAM .NE. PIM%SOU(PIM%SCA(IND_SCA)%SOU_IND)%NAME ) THEN
                        IER = -1
                        CALL ERR_LOG ( 7247, IUER, 'PIMA_GET_VERA_TSYS', &
     &                      'Wrong source name in the '// &
     &                       STR(1:I_LEN(STR))//'th line '// &
     &                       BUF(J1)(1:I_LEN(BUF(J1)))//' of the VERA Tsys '// &
     &                       'file '//TSYS_FINAM(1:I_LEN(TSYS_FINAM))// &
     &                       ' -- fits file has '// &
     &                       PIM%SOU(PIM%SCA(IND_SCA)%SOU_IND)%NAME  )
                   END IF
                 ELSE 
                   WRITE ( 6, * ) ' TIM_SCA   = ', TIM_SCA, ' TAI_START = ', TAI_START, ' TAI_STOP = ', TAI_STOP
                   WRITE ( 6, * ) ' TIM_SCA   = ', MJDSEC_TO_DATE ( PIM%MJD_0, TIM_SCA,   -2 )
                   WRITE ( 6, * ) ' TIM_START = ', MJDSEC_TO_DATE ( PIM%MJD_0, TAI_START, -2 )
                   WRITE ( 6, * ) ' TIM_STOP  = ', MJDSEC_TO_DATE ( PIM%MJD_0, TAI_STOP,  -2 )
                   IER = -1
                   CALL ERR_LOG ( 7248, IER, 'PIMA_GET_VERA_TSYS', &
     &                 'Wrong time range for scan '//BUF(J1)(12:15)// &
     &                 ' was detected in parsing '//STR(1:I_LEN(STR))// &
     &                 'th line '//BUF(J1)(1:I_LEN(BUF(J1)))// &
     &                 ' of the VERA Tsys file '// &
     &                 TSYS_FINAM(1:I_LEN(TSYS_FINAM))// &
     &                 ' -- out of range' )
              END IF
         END IF
!
         IF ( BUF(J1)(2:2)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
!
         N_TSYS = N_TSYS + 1
         READ ( UNIT=BUF(J1)(2:10), FMT='(F9.4)', IOSTAT=IER ) TIM_HR
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7249, IUER, 'PIMA_GET_VERA_TSYS', &
     &            'Error in decoding tiem tag of system temparature '// &
     &             BUF(J1)(2:10)//' in the '//STR(1:I_LEN(STR))//'th line '// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//' of the VERA Tsys '// &
     &            'file '//TSYS_FINAM )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
! ------ Convert time to amount of seconds elapsed from 
! ------ MJD_0, TAI_0
!
         TIME_MID(N_TSYS) = TIM_HR*3600.0D0 - PIM%UTC_MTAI
!
         READ ( UNIT=BUF(J1)(55:63), FMT='(F9.4)', IOSTAT=IER ) TSYS_VAL(N_TSYS)
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7250, IUER, 'PIMA_GET_VERA_TSYS', &
     &            'Error in decoding system temperature '//BUF(J1)(55:63)// &
     &            ' in the '//STR(1:I_LEN(STR))//'th line '// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//' of the VERA Tsys '// &
     &            'file '//TSYS_FINAM )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
         SOU_NAM_ARR(N_TSYS) = SOU_NAM
 410  CONTINUE 
 810  CONTINUE 
      DEALLOCATE ( BUF )
!
! --- Deallocate memory if it was previously allocated
!
      IF ( ASSOCIATED( PIM%STA(IND_STA)%TSYS(1)%TSYS ) ) THEN
           DEALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%TSYS )
           DEALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%TIME_MID_R8  )
           DEALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%TIME_SPAN_R4 )
           DEALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%SOU_IND )
           DEALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%AZ_R4 )
           DEALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%ELEV_R4 )
      END IF
      PIM%STA(IND_STA)%TSYS(1)%NPOI = N_TSYS 
      PIM%STA(IND_STA)%TSYS(1)%NPOL = PIM%NPOL
!
! --- Allocate dynamic memory for TSYS
!
      ALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%TSYS(PIM%NFRQ,PIM%STA(IND_STA)%TSYS(1)%NPOI,PIM%STA(IND_STA)%TSYS(1)%NPOL) )
      ALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%TIME_MID_R8 (PIM%STA(IND_STA)%TSYS(1)%NPOI) )
      ALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%TIME_SPAN_R4(PIM%STA(IND_STA)%TSYS(1)%NPOI) )
      ALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%SOU_IND(PIM%STA(IND_STA)%TSYS(1)%NPOI) )
      ALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%AZ_R4(PIM%STA(IND_STA)%TSYS(1)%NPOI) )
      ALLOCATE ( PIM%STA(IND_STA)%TSYS(1)%ELEV_R4(PIM%STA(IND_STA)%TSYS(1)%NPOI) )
!
      DO 420 J2=1,PIM%STA(IND_STA)%TSYS(1)%NPOI 
         DO 430 J3=1,PIM%NFRQ
            PIM%STA(IND_STA)%TSYS(1)%TSYS(J3,J2,1) = TSYS_VAL(J2)
 430     CONTINUE 
         PIM%STA(IND_STA)%TSYS(1)%TIME_MID_R8(J2) = TIME_MID(J2) - PIM%TAI_0 
         PIM%STA(IND_STA)%TSYS(1)%TIME_SPAN_R4(J2) = 0.0D0
         PIM%STA(IND_STA)%TSYS(1)%SOU_IND(J2) = 0
         DO 440 J4=1,PIM%NSOU
            IF ( SOU_NAM_ARR(J2) == PIM%SOU(J4)%NAME ) THEN
                 PIM%STA(IND_STA)%TSYS(1)%SOU_IND(J2) = J4
            END IF
 440     CONTINUE 
         IF ( PIM%STA(IND_STA)%TSYS(1)%SOU_IND(J2) == 0 ) THEN
              CALL ERR_LOG ( 7251, IUER, 'PIMA_GET_VERA_TSYS', &
     &            'Trap of internal control: source '//SOU_NAM_ARR(J2)// &
     &            'was not found in the experimet source list when '// &
     &            'processing the VERA Tsys file '//TSYS_FINAM )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( SOU_NAM_ARR(J2), PIM%C_STA(IND_STA), PIM%C_STA(IND_STA), &
     &                    PIM%MJD_0, &
     &                    PIM%TAI_0 + PIM%STA(IND_STA)%TSYS(1)%TIME_MID_R8(J2), &
     &                    OBS_TYP, VTD, TAU_GR, RATE_PH, DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7252, IUER, 'PIMA_GET_VERA_TSYS', 'Error in an '// &
     &            'attempt to compute theoretical path delay for Tsys '// &
     &            'measurement' )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_GET_AZEL ( PIM%C_SOU(PIM%STA(IND_STA)%TSYS(1)%SOU_IND(J2)), &
     &                       PIM%C_STA(IND_STA), PIM%C_STA(IND_STA), &
     &                       PIM%MJD_0, &
     &                       PIM%TAI_0 + PIM%STA(IND_STA)%TSYS(1)%TIME_MID_R8(J2), &
     &                       VTD, AZ, ELEV, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 7253, IUER, 'PIMA_GET_VERA_TSYS', 'Error in '// &
     &            'an attempt to compute azimuth and elevation delay ' )
              RETURN 
         END IF
!
         PIM%STA(IND_STA)%TSYS(1)%AZ_R4(J2)   = AZ(1)
         PIM%STA(IND_STA)%TSYS(1)%ELEV_R4(J2) = ELEV(1)
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_VERA_TSYS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEX_DATE_TO_MJDSEC ( STR_VEX_DATE, MJD, SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Roitine VEX_DATE_TO_MJDSEC transforms the time tag from VEX        *
! *   format in the form YYYYxxxMMHHSS.SSSSS where xxx is day of year    *
! *   to the pair (MJD,SEC).                                             *
! *   
! * ## 29-APR-2006 VEX_DATE_TO_MJDSEC  v1.0 (c) L. Petrov 29-APR-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  STR_VEX_DATE*(*)
      INTEGER*4  MJD, IUER
      REAL*8     SEC
      INTEGER*4  IDAY, IER
      CHARACTER  STR_USE*40, STR_NJ*40
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( ILEN(STR_VEX_DATE) < 7 ) THEN
           CALL ERR_LOG ( 411, IUER, 'VEX_DATE_TO_MJDSEC', 'Input string '// &
     &          STR_VEX_DATE(1:I_LEN(STR_VEX_DATE))//' is too short' )
           RETURN 
      END IF
      CALL CLRCH ( STR_USE )
      STR_USE = STR_VEX_DATE 
      STR_NJ  = STR_USE(1:4)//'.01.01_'//STR_USE(8:9)//'_'//STR_USE(10:11)// &
     &          STR_USE(12:)
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( STR_NJ, MJD, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 412, IUER, 'VEX_DATE_TO_MJDSEC', 'Wrong format '// &
     &         'of the input string '//STR_VEX_DATE )
           RETURN 
      END IF
!
      CALL CHIN ( STR_USE(5:7), IDAY )
      IF ( IDAY < 1 .OR. IDAY > 366 ) THEN
           CALL ERR_LOG ( 413, IUER, 'VEX_DATE_TO_MJDSEC', 'Wrong field '// &
     &         '"day of year" of the input string '//STR_VEX_DATE )
           RETURN 
      END IF
!
      MJD = MJD + IDAY - 1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VEX_DATE_TO_MJDSEC  !#!#
