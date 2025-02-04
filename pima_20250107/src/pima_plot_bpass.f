      SUBROUTINE PIMA_PLOT_BPASS ( PIM, STA, FILE_PLOT, IDEV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_PLOT_BPASS 
! *                                                                      *
! * ### 24-MAY-2006  PIMA_PLOT_BPASS  v3.1 (c) L. Petrov 07-MAR-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( DIAGI_STRU  ) :: DIA(2)
      REAL*8     FREQ(PIM__MCHN*PIM__MFRQ), AMPL(PIM__MCHN*PIM__MFRQ), &
     &           PHAS(PIM__MCHN*PIM__MFRQ)
      CHARACTER  STA(2)*(*), FILE_PLOT*(*)
      CHARACTER  PREF_NAME*128, COMMON_TIT*128, ZAG*128, UNIT*128, TITS(2)*128
      INTEGER*4  IDEV, IUER
      INTEGER*4  J1, J2, J3, J4, IP, KP, IBST, ILST, IOST, IPST, IWST, &
     &           IDEV_DEF, ICL1, ICL2, ICL3, ICODE, IB, NC, NR, &
     &           IND_STA(2), IFRQ, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4, ATAN_CS_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, LTM_DIF
!
      IF ( PIM%CONF%ACT_CODE == PIMA__BPLT_CODE ) THEN
           IF ( PIM%L_BASBPS == 0 ) THEN
                CALL ERR_LOG ( 7581, IUER, 'PIMA_PLOT_BPASS', 'No bandpass data '// &
     &              'were found' )
                RETURN 
           END IF 
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                WRITE ( 6, * ) 'Bandpass file: '//TRIM(PIM%BPASS(1)%FINAM)
           END IF
         ELSE IF ( PIM%CONF%ACT_CODE == PIMA__PPLT_CODE ) THEN
           IF ( PIM%L_STA_PBP== 0 ) THEN
                CALL ERR_LOG ( 7582, IUER, 'PIMA_PLOT_BPASS', 'No polariuzation '// &
     &              'bandpass data were found' )
                RETURN 
           END IF 
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                WRITE ( 6, * ) 'Polarization bandpass file: '//TRIM(PIM%PBP(1)%FINAM)
           END IF
      END IF
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV_DEF, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7583, IUER, 'PIMA_PLOT_BPASS', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
      IF ( IDEV == 0 ) THEN
           IDEV = IDEV_DEF
           PREF_NAME = '/tmp/'
         ELSE 
           IB = LINDEX ( FILE_PLOT, '/' ) - 1
           IF ( IB < 1 ) THEN
                PREF_NAME = './'
              ELSE 
                PREF_NAME = FILE_PLOT(1:IB)
           END IF
      END IF
!
      IND_STA(1) = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA(1) )
      IND_STA(2) = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA(2) )
      IF ( IND_STA(1) < 1 ) THEN
           CALL ERR_LOG ( 7584, IUER, 'PIMA_PLOT_BPASS', 'Station '// &
     &          STA(1)//' did not observed in experiment '// &
     &          PIM%CONF%SESS_CODE )
           RETURN
      END IF
      IF ( IND_STA(2) < 1 ) THEN
           CALL ERR_LOG ( 7585, IUER, 'PIMA_PLOT_BPASS', 'Station '// &
     &          STA(2)//' did not observed in experiment '// &
     &          PIM%CONF%SESS_CODE )
           RETURN
      END IF
!
      IP = 0
      IFRQ = 0
      DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 430 J3=1,PIM%NCHN
            IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
!
! -------------- Check the bandpass mask
!
                 IF ( PIM%BANDPASS_MASK(J3,J2,IND_STA(1),PIMA__MASK_BPAS) == 0 ) GOTO 430
                 IF ( PIM%BANDPASS_MASK(J3,J2,IND_STA(2),PIMA__MASK_BPAS) == 0 ) GOTO 430
            END IF
            IF ( PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) > PIMA__MIN_FRQ ) THEN
                 IP = IP + 1
                 IF ( PIM%CONF%ACT_CODE == PIMA__BPLT_CODE ) THEN
                      FREQ(IP) = PIM%BPASS(IND_STA(1))%FREQ(J3,J2) - &
     &                           PIM%BPASS(IND_STA(1))%FREQ(1,PIM%CONF%BEG_FRQ)
                      IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
                           AMPL(IP) = ABS( PIM%BPASS(IND_STA(1))%BPS(J3,J2) )* &
     &                                ABS( PIM%BPASS(IND_STA(2))%BPS(J3,J2) )
                         ELSE
                           AMPL(IP) = SQRT ( ABS( PIM%BPASS(IND_STA(1))%BPS(J3,J2) )* &
     &                                       ABS( PIM%BPASS(IND_STA(2))%BPS(J3,J2) )  )
                      END IF
                      IF ( IND_STA(2) == IND_STA(1) ) THEN
                           PHAS(IP) = PHAS_CMPL_R4 ( PIM%BPASS(IND_STA(1))%BPS(J3,J2) )
                         ELSE 
                           PHAS(IP) = PHAS_CMPL_R4 ( CONJG(PIM%BPASS(IND_STA(1))%BPS(J3,J2))* &
     &                                                     PIM%BPASS(IND_STA(2))%BPS(J3,J2)  )
                      END IF
                    ELSE IF ( PIM%CONF%ACT_CODE == PIMA__PPLT_CODE ) THEN
                      FREQ(IP) = PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) - PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
                      IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
                           AMPL(IP) = ABS( PIM%PBP(IND_STA(1))%CMPL(J3,J2) )* &
     &                                ABS( PIM%PBP(IND_STA(2))%CMPL(J3,J2) )
                         ELSE
                           AMPL(IP) = SQRT ( ABS( PIM%PBP(IND_STA(1))%CMPL(J3,J2) )* &
     &                                       ABS( PIM%PBP(IND_STA(2))%CMPL(J3,J2) )  )
                      END IF
                      IF ( IND_STA(2) == IND_STA(1) ) THEN
                           PHAS(IP) = PHAS_CMPL_R4 ( PIM%PBP(IND_STA(1))%CMPL(J3,J2) )
                         ELSE
                           PHAS(IP) = PHAS_CMPL_R4 ( CONJG(PIM%PBP(IND_STA(1))%CMPL(J3,J2))* &
     &                                                     PIM%PBP(IND_STA(2))%CMPL(J3,J2)  )
                      END IF
                 END IF
                 IF ( PHAS(IP) < -PI__NUM ) PHAS(IP) = PHAS(IP) + PI2
                 IF ( PHAS(IP) >  PI__NUM ) PHAS(IP) = PHAS(IP) - PI2
            END IF
 430    CONTINUE 
 420  CONTINUE 
      KP = IP
!
      DO 440 J4=1,2
         CALL NOUT ( SIZEOF(DIA(J4)), DIA(J4) )
         DIA(J4)%IDEV = IDEV
         DIA(J4)%NCLR = 2
         IF ( J4 == 1 ) THEN
              DIA(J4)%NPOI(1)   = KP
              DIA(J4)%ADR_X8(1) = LOC(FREQ)
              DIA(J4)%ADR_Y8(1) = LOC(PHAS)
              DIA(J4)%NPOI(2)   = 0
              DIA(J4)%ADR_X8(2) = 0
              DIA(J4)%ADR_Y8(2) = 0
            ELSE 
              DIA(J4)%NPOI(2)   = KP
              DIA(J4)%ADR_X8(2) = LOC(FREQ)
              DIA(J4)%ADR_Y8(2) = LOC(AMPL)
              DIA(J4)%NPOI(1)   = 0
              DIA(J4)%ADR_X8(1) = 0
              DIA(J4)%ADR_Y8(1) = 0
         END IF
         DIA(J4)%ADR_E8(1) = 0
         DIA(J4)%ADR_E8(2) = 0
         DIA(J4)%LER(1)    = .FALSE.
         DIA(J4)%LER(2)    = .FALSE.
         IF ( J4 == 1 ) THEN
              DIA(J4)%ICOL(1) = J4
              DIA(J4)%IBST(1) = 0
              DIA(J4)%ILST(1) = 2
              DIA(J4)%IOST(1) = 1
              DIA(J4)%IWST(1) = 1
              DIA(J4)%ICLR    = 1
              DIA(J4)%IPST(1) = 4
              DIA(J4)%ILST(1) = 1
            ELSE 
              DIA(J4)%ICOL(2) = J4
              DIA(J4)%IBST(2) = 0
              DIA(J4)%ILST(2) = 2
              DIA(J4)%IOST(2) = 1
              DIA(J4)%IWST(2) = 1
              DIA(J4)%ICLR    = 2
              DIA(J4)%IPST(2) = 4
              DIA(J4)%ILST(2) = 2
         END IF
!
         IF ( J4 == 1 ) THEN
              DIA(J4)%XMIN   = FREQ(1)  - (FREQ(KP)-FREQ(1))*DIAGI_FIE
              DIA(J4)%XMAX   = FREQ(KP) + (FREQ(KP)-FREQ(1))*DIAGI_FIE
              DIA(J4)%YMIN   = -PI__NUM 
              DIA(J4)%YMAX   =  PI__NUM
            ELSE
              DIA(J4)%XMIN   = FREQ(1)  - (FREQ(KP)-FREQ(1))*DIAGI_FIE
              DIA(J4)%XMAX   = FREQ(KP) + (FREQ(KP)-FREQ(1))*DIAGI_FIE
              DIA(J4)%YMIN   =  1.0
              DIA(J4)%YMAX   = -1.0
         END IF
         DIA(J4)%ARG_UNITS = 'Frequency in Hz'
!
         IF ( ILEN(FILE_PLOT) > 0 ) THEN
              DIA(J4)%NAME = FILE_PLOT
            ELSE 
              DIA(J4)%NAME   = 'bps_'//STA(1)(1:I_LEN(STA(1)))//'_'// &
     &                                 STA(2)(1:I_LEN(STA(2)))
         END IF
         DIA(J4)%ITRM   = 0
         IF ( ILEN(FILE_PLOT) == 0 ) THEN
              DIA(J4)%IBATCH = 0
            ELSE 
              DIA(J4)%IBATCH = 1
         END IF
         DIA(J4)%STATUS = DIA__DEF
         IF ( J4 == 1 ) THEN
              TITS(J4) = 'Bandpass phase for '// &
     &                    STA(1)(1:I_LEN(STA(1)))//'_'// &
     &                    STA(2)(1:I_LEN(STA(2)))
              DIA(J4)%ZAG  = TITS(J4)
            ELSE 
              TITS(J4) = 'Bandpass amplitude for '// &
     &                    STA(1)(1:I_LEN(STA(1)))//'_'// &
     &                    STA(2)(1:I_LEN(STA(2)))
              DIA(J4)%ZAG  = TITS(J4)
         END IF
 440  CONTINUE 
!
      NC = 1
      NR = 2
!
      COMMON_TIT = 'Bandpass for '// &
     &              STA(1)(1:I_LEN(STA(1)))//'_'// &
     &              STA(2)(1:I_LEN(STA(2)))
!
      ICODE = 0
      CALL ERR_PASS ( IUER, IER )
      CALL MULTI_DIAGI ( COMMON_TIT, 2, NC, NR, TITS, 0, &
     &                   ZAG, ZAG, PREF_NAME, DIA, ICODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7586, IUER, 'PIMA_PLOT_BPASS', 'Failure to '// &
     &         'make a bandpass plot' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_BPASS !#!# 
