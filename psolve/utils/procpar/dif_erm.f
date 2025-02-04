      PROGRAM    DIF_ERM
! ************************************************************************
! *                                                                      *
! *   Program DIF_ERM computes the differences in the EOP specified in   *
! *   two files in the form  of the Empirial Earth Rotation Model        *
! *   ERM format, computes statistics, and displays th differences.      *
! *                                                                      *
! *  ### 24-OCT-2021    DIF_ERM    v1.0 (c)  L. Petrov  09-NOV-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INCLUDE   'erm.i'
      INCLUDE   'getpar.i'
      INTEGER*4  MP, M_PAR, M_ERM, M_HEAD, M_HEO, MIND
      PARAMETER  ( MP     = 256*1024 )
      PARAMETER  ( M_PAR  =  1024 )
      PARAMETER  ( M_ERM  = 32768 )
      PARAMETER  ( M_HEAD =   516 )
      PARAMETER  ( M_HEO  =  8192 )
      PARAMETER  ( MIND   =    32 )
      TYPE ( ERM__TYPE  ) :: ERM(2)
      TYPE ( EOP__STRU  ) :: EOP(MP,2)
      TYPE ( HEO__STRUC ) :: HEO(M_HEO,2)
      CHARACTER  FIL_ERM(2)*128, FIL_HEO(2)*128, EOP_APR_FILE(2)*128, SOL_ID(2)*32
      CHARACTER  BUF1(MP)*128, BUF2(MP)*128, NAME_HEO(M_HEO,2)*6, &
     &           DATE_BEG_STR*30, DATE_END_STR*30
      CHARACTER  UNIT_STR*128, STR*128
      LOGICAL*1  FL_PLOT
      REAL*8     HEO_EPOCH_SEC(2)
      LOGICAL*1  LEX
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.0D-3 )
      REAL*8     TIM_STEP(2), TIM_BEG, TIM_END, TIM_CNS_BEG, TIM_CNS_END
      REAL*8     MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG, TAI_BEG, TAI_END, &
     &           WW, RMS, WRMS
      REAL*8     TIM(MP), VAL(MP,2), ERR(MP,2), DIF(MP), WEI(MP)
      INTEGER*4  J1, J2, J3, J4, J5, IDER, IL, NP(2), L_HEO(2), ICMP, IVRB, &
     &           IND_BEG(2), MJD_BEG, MJD_END, KP, UZT_MODEL, UZT_USE, IUER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IVRB = 0
      IDER = 0
      TIM_STEP = 2.0*86400.0D0
      FL_PLOT = .TRUE.
      DATE_BEG_STR = 'beg'
      DATE_END_STR = 'end'
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: erm1 erm2 icmp [-der] [-noplot] [date_beg] [date_end] [ivrb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_ERM(1) )
           CALL GETARG ( 2, FIL_ERM(2) )
           CALL GETARG ( 3, STR  )
           CALL CHIN   ( STR, ICMP )
           IF ( ICMP == 1 .OR. ICMP == 2 .OR. ICMP == 3 .OR. ICMP == 33 ) THEN
                CONTINUE 
              ELSE
                IUER = -1
                CALL ERR_LOG ( 8301, IUER, 'DIF_ERM', 'Error in parsing '// &
     &              'the third argment. A number 0, 1, 2, 3, or 33 was expected, '// &
     &              'but got '//STR )
                CALL EXIT ( 1 )
           END IF
           IF ( ICMP == 33 ) THEN
                ICMP = 3
                UZT_MODEL = UZT__RE2014
                UZT_USE   = UZT__SUBTRACT
              ELSE
                UZT_MODEL = UZT__NONE
                UZT_USE   = UZT__NONE
           END IF
           IF ( IARGC()  .GE. 4 ) THEN
                CALL GETARG ( 4, STR )
                CALL CHIN   ( STR, IDER )
           END IF
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR  )
                IF ( STR(1:4) == '-nop' ) THEN
                     FL_PLOT = .FALSE.
                END IF
           END IF
           IF ( IARGC()  .GE. 6 ) THEN
                CALL GETARG ( 6, DATE_BEG_STR  )
           END IF
           IF ( IARGC()  .GE. 7 ) THEN
                CALL GETARG ( 7, DATE_END_STR  )
           END IF
           IF ( IARGC() .GE. 8 ) THEN
                CALL GETARG ( 8, STR  )
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF
!
      DO 410 J1=1,2
         INQUIRE ( FILE=FIL_ERM(J1), EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              IUER = -1
              CALL ERR_LOG ( 8302, IUER, 'DIF_ERM', 'Did not find Empirial '// &
     &            'Earth Rotation Model file '//FIL_ERM(J1) )
              CALL EXIT ( 1 )
         END IF
         IL = ILEN(FIL_ERM(J1))
!
         FIL_HEO(J1) = FIL_ERM(J1)(1:IL-4)//'.heo'
         INQUIRE ( FILE=FIL_HEO(J1), EXIST=LEX )
         IF ( LEX ) THEN
              IUER = -1
              CALL READ_HEO ( FIL_HEO(J1), M_HEO, L_HEO(J1), HEO(1,J1), &
     &                        NAME_HEO(1,J1), HEO_EPOCH_SEC(J1), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 8303, IUER, 'DIF_ERM', 'Failure in reading high '// &
     &                 'frequency EOP file '//FIL_HEO(J1) )
                   CALL EXIT ( 1 )
              END IF
            ELSE
              L_HEO(J1) = 0
         END IF
!
         IUER = -1
         CALL PARSE_ERM ( FIL_ERM(J1), ERM(J1), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 8304, IUER, 'DIF_ERM', 'Error in parsing '// &
     &            'input 1st ERM file '//FIL_ERM(J1) )
              CALL EXIT ( 1 )
         END IF
 410  CONTINUE 
!
      DO 420 J2=1,2
         IF ( J2 == 2 .AND. FIL_ERM(2) == FIL_ERM(1) ) THEN
             CALL SETENV ( 'ERM_NO_VAL'//CHAR(0), 'YES'//CHAR(0), %VAL(1) )
         END IF
         IUER = -1
         CALL ERM_TO_EOP ( ERM(J2), TIM_STEP, UZT_MODEL, UZT_USE, &
     &                     MP, NP(J2), EOP(1,J2), L_HEO(J2), HEO(1,J2), &
     &                     HEO_EPOCH_SEC(J2), IVRB, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 8305, IUER, 'DIF_ERM', 'Error in computation '// &
     &            'the EOP series from the empirical Earth rotation model '// &
     &            'defined in '//FIL_ERM(J2) )
              CALL EXIT ( 1 )
         END IF
 420  CONTINUE 
!
      STR = MJDSEC_TO_DATE ( ERM(1)%MJD_REF_CNS, ERM(1)%TAI_REF_CNS, IUER )
      WRITE ( 6, '(A)' ) 'Mean epoch: '//STR(1:19)
      DO 430 J3=1,3
         IF ( .NOT. ( ICMP == 0 .OR. J3 == ICMP ) ) GOTO 430
         TIM_CNS_BEG = MAX ( (ERM(1)%MJD_BEG_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(1)%TAI_BEG_RANGE_CNS, &
     &                       (ERM(2)%MJD_BEG_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(2)%TAI_BEG_RANGE_CNS, &
     &                       (ERM(1)%MJD_BEG_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(1)%TAI_BEG_RANGE_CNS, &
     &                       (ERM(2)%MJD_BEG_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(2)%TAI_BEG_RANGE_CNS  &
     &                     )
         TIM_CNS_END = MIN ( (ERM(1)%MJD_END_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(1)%TAI_END_RANGE_CNS, &
     &                       (ERM(2)%MJD_END_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(2)%TAI_END_RANGE_CNS, &
     &                       (ERM(1)%MJD_END_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(1)%TAI_END_RANGE_CNS, &
     &                       (ERM(2)%MJD_END_RANGE_CNS - J2000__MJD)*86400.0D0 + ERM(2)%TAI_END_RANGE_CNS  &
     &                     ) 
!
         IF ( DATE_BEG_STR == 'beg' ) THEN
              TIM_BEG = TIM_CNS_BEG
            ELSE
              IUER = -1
              CALL DATE_TO_TIME ( DATE_BEG_STR, MJD_BEG, TAI_BEG, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 8306, IUER, 'DIF_ERM', 'Error in parsing '// &
     &                 'the 5th argument, start time '//DATE_BEG_STR )
                   CALL EXIT ( 1 )
              END IF
              TIM_BEG = (MJD_BEG - J2000__MJD)*86400.D0 + TAI_BEG
         END IF
!
         IF ( DATE_END_STR == 'end' ) THEN
              TIM_END = TIM_CNS_END
            ELSE
              IUER = -1
              CALL DATE_TO_TIME ( DATE_END_STR, MJD_END, TAI_END, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 8307, IUER, 'DIF_ERM', 'Error in parsing '// &
     &                 'the 6th argument, stop time '//DATE_END_STR )
                   CALL EXIT ( 1 )
              END IF
              TIM_END = (MJD_END - J2000__MJD)*86400.D0 + TAI_END
         END IF
!
         IF ( TIM_BEG < (ERM(1)%MJD_BEG - J2000__MJD)*86400.0D0 + ERM(1)%TAI_BEG ) THEN
              TIM_BEG = (ERM(1)%MJD_BEG - J2000__MJD)*86400.0D0 + ERM(1)%TAI_BEG 
         END IF
         IF ( TIM_BEG < (ERM(2)%MJD_BEG - J2000__MJD)*86400.0D0 + ERM(2)%TAI_BEG ) THEN
              TIM_BEG = (ERM(2)%MJD_BEG - J2000__MJD)*86400.0D0 + ERM(2)%TAI_BEG 
         END IF
!
         IF ( TIM_END > (ERM(1)%MJD_END - J2000__MJD)*86400.0D0 + ERM(1)%TAI_END - ERM(1)%TIME_EST_SPAN(J3) ) THEN
              TIM_END = (ERM(1)%MJD_END - J2000__MJD)*86400.0D0 + ERM(1)%TAI_END - ERM(1)%TIME_EST_SPAN(J3) 
         END IF
         IF ( TIM_END > (ERM(2)%MJD_END - J2000__MJD)*86400.0D0 + ERM(2)%TAI_END - ERM(2)%TIME_EST_SPAN(J3) ) THEN
              TIM_END = (ERM(2)%MJD_END - J2000__MJD)*86400.0D0 + ERM(2)%TAI_END - ERM(2)%TIME_EST_SPAN(J3)
         END IF
!
         IND_BEG(1) = IDNINT ( (TIM_BEG - ERM(1)%TIM(1,J3))/TIM_STEP(1) ) + 1
         IND_BEG(2) = IDNINT ( (TIM_BEG - ERM(2)%TIM(1,J3))/TIM_STEP(2) ) + 1
         IF ( IND_BEG(1) < 1 ) IND_BEG(1) = 1
         IF ( IND_BEG(2) < 1 ) IND_BEG(2) = 1
         KP         = IDNINT ( (TIM_END - TIM_BEG)/TIM_STEP(1) ) + 1
         IF ( TIM_BEG + (KP-1)*TIM_STEP(1) > TIM_END + TIM_EPS ) THEN
              TIM_END = TIM_END - TIM_STEP(1) 
         END IF
         IF ( IVRB > 0 ) THEN
              WRITE ( 6, * ) 'ICMP= ', J3, ' KP= ', KP, ' IND_BEG= ', IND_BEG
              WRITE ( 6, * ) 'TIM_ERM_BEG:  ', MJDSEC_TO_DATE ( ERM(1)%MJD_BEG, ERM(1)%TAI_BEG, IUER )
              WRITE ( 6, * ) 'TIM_MEAN_BEG: ', MJDSEC_TO_DATE ( ERM(1)%MJD_BEG_RANGE_CNS, ERM(1)%TAI_BEG_RANGE_CNS, IUER )
              WRITE ( 6, * ) 'TIM_RATE_BEG: ', MJDSEC_TO_DATE ( ERM(1)%MJD_BEG_RANGE_CNS, ERM(1)%TAI_BEG_RANGE_CNS, IUER )
              WRITE ( 6, * ) 'TIM_BEG:      ', TIM_TO_DATE    ( TIM_BEG, IUER )
!
              WRITE ( 6, * ) ' '
              WRITE ( 6, * ) 'TIM_ERM_END:  ', MJDSEC_TO_DATE ( ERM(1)%MJD_END, ERM(1)%TAI_END, IUER )
              WRITE ( 6, * ) 'TIM_MEAN_END: ', MJDSEC_TO_DATE ( ERM(1)%MJD_END_RANGE_CNS, ERM(1)%TAI_END_RANGE_CNS, IUER  )
              WRITE ( 6, * ) 'TIM_RATE_END: ', MJDSEC_TO_DATE ( ERM(1)%MJD_END_RANGE_CNS, ERM(1)%TAI_END_RANGE_CNS, IUER  )
              WRITE ( 6, * ) 'TIM_END:      ', TIM_TO_DATE    ( TIM_END, IUER )
         END IF
!
         DO 440 J4=1,KP
            TIM(J4) = (EOP(IND_BEG(1)-1+J4,1)%MJD_EOP - J2000__MJD)/JYEAR__DAYS
            IF ( J3 == 1 ) THEN
                 IF ( IDER == 0 ) THEN
                      VAL(J4,1) = EOP(IND_BEG(1)-1+J4,1)%YPL_V
                      VAL(J4,2) = EOP(IND_BEG(2)-1+J4,2)%YPL_V
                   ELSE IF ( IDER == 1 ) THEN
                      VAL(J4,1) = EOP(IND_BEG(1)-1+J4,1)%YPR_V
                      VAL(J4,2) = EOP(IND_BEG(2)-1+J4,2)%YPR_V
                   ELSE IF ( IDER == 2 ) THEN
                      VAL(J4,1) = EOP(IND_BEG(1)-1+J4,1)%YPQ_V
                      VAL(J4,2) = EOP(IND_BEG(2)-1+J4,2)%YPQ_V
                 END IF
                 ERR(J4,1) = EOP(IND_BEG(1)-1+J4,1)%YPL_E
                 ERR(J4,2) = EOP(IND_BEG(2)-1+J4,2)%YPL_E
              ELSE IF ( J3 == 2 ) THEN
                 IF ( IDER == 0 ) THEN
                      VAL(J4,1) = EOP(IND_BEG(1)-1+J4,1)%XPL_V
                      VAL(J4,2) = EOP(IND_BEG(2)-1+J4,2)%XPL_V
                   ELSE IF ( IDER == 1 ) THEN
                      VAL(J4,1) = EOP(IND_BEG(1)-1+J4,1)%XPR_V
                      VAL(J4,2) = EOP(IND_BEG(2)-1+J4,2)%XPR_V
                   ELSE IF ( IDER == 2 ) THEN
                      VAL(J4,1) = EOP(IND_BEG(1)-1+J4,1)%XPQ_V
                      VAL(J4,2) = EOP(IND_BEG(2)-1+J4,2)%XPQ_V
                 END IF
                 ERR(J4,1) = EOP(IND_BEG(1)-1+J4,1)%XPL_E
                 ERR(J4,2) = EOP(IND_BEG(2)-1+J4,2)%XPL_E
              ELSE IF ( J3 == 3 ) THEN
                 IF ( IDER == 0 ) THEN
                      VAL(J4,1) = -1.00273781191135448D0*EOP(IND_BEG(1)-1+J4,1)%U1_V
                      VAL(J4,2) = -1.00273781191135448D0*EOP(IND_BEG(2)-1+J4,2)%U1_V
                   ELSE IF ( IDER == 1 ) THEN
                      VAL(J4,1) = -1.00273781191135448D0*EOP(IND_BEG(1)-1+J4,1)%UTR_V
                      VAL(J4,2) = -1.00273781191135448D0*EOP(IND_BEG(2)-1+J4,2)%UTR_V
                   ELSE IF ( IDER == 2 ) THEN
                      VAL(J4,1) = -1.00273781191135448D0*EOP(IND_BEG(1)-1+J4,1)%UTQ_V
                      VAL(J4,2) = -1.00273781191135448D0*EOP(IND_BEG(2)-1+J4,2)%UTQ_V
                 END IF
                 ERR(J4,1) = EOP(IND_BEG(1)-1+J4,1)%U1_E
                 ERR(J4,2) = EOP(IND_BEG(2)-1+J4,2)%U1_E
            END IF
            DIF(J4) = VAL(J4,1) - VAL(J4,2)
            WEI(J4) = 1.0D0
 440     CONTINUE 
!
         MEAN_T = (ERM(1)%MJD_REF_CNS - ERM(1)%MJD_BEG)*86400.0D0 + &
     &            (ERM(1)%TAI_REF_CNS - ERM(1)%TAI_BEG)
         IF ( IVRB > 1 ) THEN
              WRITE ( 6, * ) ' MEAN_T= ', MEAN_T, ' KP= ', KP ! %%%
         END IF
         CALL RGRW8 ( KP, TIM, DIF, WEI, %VAL(0), MEAN_T, &
     &                DR_VAL, SH_VAL, DR_SIG, SH_SIG, IUER )
         WW =   0.0D0
         RMS =  0.0D0
         WRMS = 0.0D0
         DO 450 J5=1,KP
            RMS  = RMS  + ( DIF(J5) - (SH_VAL + DR_VAL*(TIM(J5)-MEAN_T)) )**2
            WRMS = WRMS + ( DIF(J5) - (SH_VAL + DR_VAL*(TIM(J5)-MEAN_T)) )**2/ERR(J5,1)**2
            WW   = WW   + 1.0D0/ERR(J5,1)**2
 450     CONTINUE 
         WRMS = SQRT ( WRMS/WW )
         RMS  = SQRT (  RMS/KP )
         WRITE ( 6, 110 ) J3, SH_VAL, SH_SIG, WRMS, &
     &                    J3, DR_VAL/JYEAR__DAYS/86400.0D0, DR_SIG/JYEAR__DAYS/86400.0D0, RMS 
 110     FORMAT ( 'E', I1, ') Shift: ', 1PD16.9, ' -+ ', 1PD10.3, ' rad    wrms: ', 1PD10.3 / &
     &            'E', I1, ') Drift: ', 1PD16.9, ' -+ ', 1PD10.3, ' rad/s   rms: ', 1PD10.3 )
!
         IF ( IDER == 0 ) THEN
              UNIT_STR = '(rad)'
           ELSE IF ( IDER == 1 ) THEN
              UNIT_STR = '(rad/s)'
           ELSE IF ( IDER == 2 ) THEN
              UNIT_STR = '(rad/s**2)'
         END IF
         IF ( FL_PLOT ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'EOP E'//TRIM(STR)// &
     &                            ' from '//TRIM(ERM(1)%SOL_ID)// &
     &                            ' and '//TRIM(ERM(2)%SOL_ID)//' '//UNIT_STR )
              CALL DIAGI_2 ( KP, TIM, VAL(1,1), KP, TIM, VAL(1,2), IUER )
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'Difference in EOP E'//TRIM(STR)// &
     &                            ' from '//TRIM(ERM(1)%SOL_ID)// &
     &                            ' minus '//TRIM(ERM(2)%SOL_ID)//' '//UNIT_STR )
              CALL DIAGI_1 ( KP, TIM, DIF, IUER )
         END IF
 430  CONTINUE 
!
      END  PROGRAM   DIF_ERM  !#!#
