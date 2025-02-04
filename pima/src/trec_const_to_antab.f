      SUBROUTINE TREC_CONST_TO_ANTAB ( PIM, STA_NAM, TREC, TATM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine TREC_CONST_TO_ANTAB
! *                                                                      *
! * ## 08-DEC-2017 TREC_CONST_TO_ANTAB v1.0 (c) L. Petrov 08-DEC-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  STA_NAM*(*)
      REAL*8     TREC, TATM
      CHARACTER  FILOUT*128
      INTEGER*4  IUER
      LOGICAL*1  LEX
      INTEGER*4  MP
      PARAMETER  ( MP = 64*1024 ) 
      REAL*8       ELEV_MIN
      PARAMETER  ( ELEV_MIN = 2.0D0*DEG__TO__RAD )
      CHARACTER  STR*512, STR1*512, OUT(MP)*512
      INTEGER*4  J1, J2, J3, J4, J5, ID, ISTA, NP, NOUT, IER
      REAL*8     TIM_SCA_BEG, TIM_SCA_END, TIM_SCA_MID, TSYS_OUT(PIM__MFRQ), ELEV
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      REAL*8,    EXTERNAL :: DEL_ISA
!
      ISTA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
      IF ( ISTA < 1 ) THEN
           CALL ERR_LOG ( 4331, IUER, 'TREC_CONST_TO_ANTAB', 'Wrong '// &
     &         'value of the 1st keyword: station '//TRIM(STA_NAM)// &
     &         ' did not observe in experiment '//PIM%CONF%SESS_CODE )
           RETURN 
      END IF
      CALL CLRCH ( STR  )
      CALL CLRCH ( STR1 )
      CALL TRAN  ( 12, PIM%CONF%SESS_CODE,      STR  )
      CALL TRAN  ( 12, PIM%STA(ISTA)%ORIG_NAME, STR1 )
      ID = LINDEX ( PIM%CONF_FILE, '/' )
      IF ( ID < 1 ) ID = 2
      FILOUT = PIM%CONF_FILE(1:ID-1)//'/'//TRIM(STR)//'_'//STR1(1:2)//'.ant'
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           NOUT = 0
           NOUT = NOUT + 1 ; OUT(NOUT) = '# LOG-ANTAB Format  Version of 2009.08.07'
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
           NOUT = NOUT + 1 ; OUT(NOUT) = '# Generator: PIMA (trec_const_to_antab) '//PIMA__LABEL
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
           WRITE ( UNIT=OUT(NOUT), FMT=110 ) TREC, TATM, GET_CDATE()
 110       FORMAT ( '# Generated using Trec= ', F6.1, ' Tatm= ', F6.1, ' K  on ', A )
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
           NOUT = NOUT + 1 ; OUT(NOUT) = 'STATION:  '//PIM%STA(ISTA)%IVS_NAME
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FILOUT, MP, OUT, NP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4332, IUER, 'TREC_CONST_TO_ANTAB', 'Error in '// &
     &              'reading existing antab file '//FILOUT )
                RETURN 
           END IF
           NOUT = NP
           DO 410 J1=1,NP
              IF ( OUT(J1)(1:9) == 'NUMB_FRQ:' ) THEN
                   NOUT = J1 - 2
              END IF
 410       CONTINUE 
           IF ( OUT(NOUT)(1:13) == '# Overwritten' ) NOUT = NOUT - 1
      END IF
      write ( 6, * ) 'filout= ', trim(filout), ' lex= ', lex, ' nout= ', nout  ! %%%%
!
      NOUT = NOUT + 1 ; OUT(NOUT) = '#'
      IF ( LEX ) THEN
           WRITE ( UNIT=OUT(NOUT), FMT=120 ) TREC, TATM, GET_CDATE()
 120       FORMAT ( '# Overwritten using Trec= ', F6.1, ' Tatm= ', F6.1, ' K  on ', A )
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
      END IF
      CALL CLRCH  ( STR )
      CALL INCH   ( PIM%NFRQ, STR(1:2) )
      CALL CHASHR ( STR(1:2) )
      NOUT = NOUT + 1 ; OUT(NOUT) = 'NUMB_FRQ:    '//STR(1:2)
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      NOUT = NOUT + 1 ; OUT(NOUT) = '#       Chan     IF_Freq     LO_Freq    Sky_freq   Pol'
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      DO 420 J2=1,PIM%NFRQ
         NOUT = NOUT + 1
         WRITE ( UNIT=OUT(NOUT), FMT=130 ) J2, 0.0D0, 0.0D0, &
     &                                     1.0D-6*PIM%FREQ_ARR(1,J2,PIM%CONF%FRQ_GRP), &
     &                                     'R'
 130     FORMAT ( 'FRQ:  ', 2X, I4, 2X, F10.2, 2X, F10.2, 2X, F10.2, 5X, A1 )
         CALL CHASHR ( OUT(NOUT)(9:12) )
 420  CONTINUE 
!
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      STR  = '#       Scan  Scan_name   Sou_name    TAI_Time_tag             Ts #01  Ts #02  Ts #03  Ts #04  Ts #05  Ts #06  Ts #07  Ts #08  Ts #09  Ts #10  Ts #11  Ts #12  Ts #13  Ts #14  Ts #15  Ts #16  Ts #17  Ts #18  Ts #19  Ts #20  Ts #21  Ts #22  Ts #23  Ts #24  Ts #25  Ts #26  Ts #27  Ts #28  Ts #29  Ts #30  Ts #31  Ts #32'
      NOUT = NOUT + 1 ; OUT(NOUT) = STR(1:61+8*PIM%NFRQ)
      NOUT = NOUT + 1 ; OUT(NOUT) = '# ' 
      CALL CLRCH  ( STR )
      CALL INCH   ( PIM%NSCA, STR )
      NOUT = NOUT + 1 ; OUT(NOUT) = '# NUMB_TSYS:   '//STR(1:I_LEN(STR))
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
!
      DO 430 J3=1,PIM%NSCA
         TIM_SCA_BEG = PIM%TIM_R8(PIM%SCA(J3)%TIM_IND)
         TIM_SCA_END = PIM%TIM_R8(PIM%SCA(J3)%TIM_IND + PIM%SCA(J3)%NUM_EPC)
         TIM_SCA_MID = (TIM_SCA_BEG + TIM_SCA_END)/2.0D0
         CALL CLRCH ( STR1 ) 
         STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_SCA_MID, IER )
         NOUT = NOUT +  1
         ELEV = -1.0D15
         DO 440 J4=1,PIM%SCA(J3)%NBAS
            IF ( PIM%OBS(PIM%SCA(J3)%OBS_IND(J4))%STA_IND(1) == ISTA ) THEN
                 ELEV = PIM%OBS(PIM%SCA(J3)%OBS_IND(J4))%ELEV(1)
               ELSE IF ( PIM%OBS(PIM%SCA(J3)%OBS_IND(J4))%STA_IND(2) == ISTA ) THEN
                 ELEV = PIM%OBS(PIM%SCA(J3)%OBS_IND(J4))%ELEV(2)
            END IF 
 440     CONTINUE 
!!         write ( 6, * ) 'allo= ', allocated ( pim%sca(j3)%obs_ind ), ' nbas= ', pim%sca(j3)%nbas, ' sha= ', shape ( pim%sca(j3)%obs_ind ) ! %%%%%%%%%%%%
         DO 450 J5=1,PIM%NFRQ
            IF ( ELEV > ELEV_MIN ) THEN
                 TSYS_OUT(J5) = TREC + TATM*DEL_ISA ( ELEV )/DEL_ISA ( P2I )
               ELSE IF ( ELEV > -1.D15 ) THEN
                 TSYS_OUT(J5) = TREC + TATM*DEL_ISA ( ELEV_MIN )/DEL_ISA ( P2I )
               ELSE 
                 TSYS_OUT(J5) = -1.0D0
            END IF 
 450     CONTINUE 
         WRITE ( OUT(NOUT), 140 ) J3, &
     &                            PIM%SCA(J3)%SCAN_NAME, &
     &                            PIM%C_SOU(PIM%SCA(J3)%SOU_IND), &
     &                            STR1(1:22), &
     &                            TSYS_OUT(1:PIM%NFRQ)
 140     FORMAT ( 'TSYS:   ',I4, 2X, A, 2X, A, 4X , A, 1X, 32(4X,F7.1) )
 430  CONTINUE 
!
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      NOUT = NOUT + 1 ; OUT(NOUT) = '# LOG-ANTAB Format  Version of 2009.08.07'
!      
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT ( NOUT, OUT, FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4333, IUER, 'TREC_CONST_TO_ANTAB', 'Error in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Output antab file is written: '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TREC_CONST_TO_ANTAB !#!#
