      SUBROUTINE PIMA_AMPL_OUT ( PIM, PIM_2ND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_AMPL_OUT  creates the fringe fitting output suitable *
! *   for the coarse amplitude analysis.                                 *
! *                                                                      *
! *  ### 29-MAR-2011  PIMA_AMPL_OUT  v1.0 (c) L. Petrov  01-APR-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM, PIM_2ND
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 128 )
      PARAMETER  ( MIND =  32 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, IND, &
     &           MJD_FRT, IDAY, IND_FRG, LUN, USER_SUP, LIND, &
     &           STA_IND(2), SWAP_STA, KFRQ, NUMB_BND, NBUF, &
     &           INDS(2,MIND), OBS_DB_IND, IND_OBS, IER
      CHARACTER  FILOUT*128, BUF(MBUF)*128, MK3_DBNM*16, DB_NAME*16, &
     &           STR*128
      LOGICAL*4  FL_BAS_REVERSE
      REAL*8     SEFD_BAS, TAI_FRT, UTC_FRT, SNR(2), EFF_DURA(2), &
     &           FRN_AMPL(2), TSYS1(PIM__MFRQ,2),  TSYS2(PIM__MFRQ,2), &
     &           GAIN(2,PIM__MBND), TSYS1_AVR(2), TSYS2_AVR(2), &
     &           GAIN_POLY, POLY_VAL
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      REAL*8,    EXTERNAL :: GET_APR_SEFD_BAS
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
!
      IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) == 0 ) THEN
           NUMB_BND = 1
         ELSE 
           NUMB_BND = 2
      END IF
!
      IF ( ILEN(PIM%CONF%MKDB_DESC_FILE) > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( PIM%CONF%MKDB_DESC_FILE, MBUF, BUF, NBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7891, IUER, 'PIMA_AMPL_OUT', 'Error in an '// &
     &              'attempt to read experiment description file '// &
     &               PIM%CONF%MKDB_DESC_FILE )
                RETURN
           END IF
!
           DO 410 J1=1,NBUF
              IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
              IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
              CALL EXWORD ( BUF(J1), MIND, LIND, INDS, CHAR(0)//CHAR(32)//CHAR(9), -2 )
              IF ( LIND < 2 ) GOTO 410
              IF ( BUF(J1)(INDS(1,1):INDS(2,1)) == 'MK3_DBNM:' ) THEN
                   CALL CLRCH ( MK3_DBNM )
                   MK3_DBNM = BUF(J1)(INDS(1,2):INDS(2,LIND))
                 ELSE IF ( BUF(J1)(INDS(1,1):INDS(2,1)) == 'DB_NAME:'  ) THEN
                   CALL CLRCH ( DB_NAME )
                   DB_NAME = BUF(J1)(INDS(1,2):INDS(2,LIND))
             END IF
 410       CONTINUE 
         ELSE 
           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0, IER )
           DB_NAME = STR(1:4)//STR(6:7)//STR(9:10)//'_'// &
     &               PIM%CONF%MKDB_OUTPUT_NAME
           MK3_DBNM = '??'
      END IF
!
      FILOUT = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'// &
     &         PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_amp.txt'
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7892, IUER, 'PIMA_AMPL_OUT', 'Error in an '// &
     &         'attempt to open output file '//FILOUT )
           RETURN
      END IF
!
      WRITE ( LUN, '(A)' ) '# PIMA AMPL output. Format version of 2011.03.31'
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'DB_NAME:  '//DB_NAME 
      WRITE ( LUN, '(A)' ) 'MK3_DBNM: '//MK3_DBNM
      WRITE ( LUN, '(A,I6)' ) 'NUMB_OBS: ', PIM%NOBS
!
      OBS_DB_IND = 0
      DO 420 J2=1,PIM%L_MKDB
         DO 430 J3=1,PIM%SCADB(J2)%NOBS
            IND_OBS = PIM%SCADB(J2)%OBS_IND(J3)
            IF ( IND_OBS == 0 ) GOTO 430
            IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 430
            OBS_DB_IND = OBS_DB_IND + 1
!
            MJD_FRT = PIM%MJD_0
            TAI_FRT = PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + PIM%OBS(IND_OBS)%FRT_OFFSET(1)
            UTC_FRT = TAI_FRT + PIM%UTC_MTAI
            IDAY = UTC_FRT/86400.0D0
            MJD_FRT = MJD_FRT + IDAY
            UTC_FRT = UTC_FRT - IDAY*86400.0D0
            SNR(1) = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,PIM%CONF%FRQ_GRP)/ &
     &                             PIM%OBS(IND_OBS)%NOISE(PIM%CONF%FRQ_GRP)
            FRN_AMPL(1) = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,PIM%CONF%FRQ_GRP)
            IF ( FRN_AMPL(1) < 0.0 ) FRN_AMPL(1) = 0.0
!
            USER_SUP = 0
            USER_SUP = IBSET ( USER_SUP, GOOD__SPS  )
            USER_SUP = IBSET ( USER_SUP, INIT__SPS  )
            IF ( SNR(1) < PIM%CONF%FRIB_SNR_DETECTION ) THEN
                 USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
            END IF
            EFF_DURA(1) = PIM%OBS(IND_OBS)%EFF_DUR(1)
!
            STA_IND(1) = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)) )
            STA_IND(2) = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)) )
            IF ( STA_IND(1) > STA_IND(2) ) THEN
                 SWAP_STA = STA_IND(1)
                 STA_IND(1) = STA_IND(2)
                 STA_IND(2) = SWAP_STA
                 FL_BAS_REVERSE = .TRUE.
               ELSE
                 FL_BAS_REVERSE = .FALSE.
            END IF
!
! --------- If the data were not processed with MKDB in TEXT of GVH mode,
! --------- then the station swapping should NOT be made
!
            FL_BAS_REVERSE = .FALSE.
!
            TSYS1     = 0.0
            TSYS1_AVR = 0.0
            TSYS2     = 0.0
            TSYS2_AVR = 0.0
!
            KFRQ = 0
            DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               KFRQ = KFRQ + 1
               IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%TSYS(PIM%CONF%FRQ_GRP)%AVAIL .AND. &
     &              PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP) > 0 ) THEN
!
                    IF ( FL_BAS_REVERSE ) THEN
                         TSYS1(KFRQ,2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J5,PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP),1)
                       ELSE 
                         TSYS1(KFRQ,1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J5,PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP),1)
                    END IF
               END IF
               IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%TSYS(PIM%CONF%FRQ_GRP)%AVAIL .AND. &
     &              PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP) > 0 ) THEN
                    IF ( FL_BAS_REVERSE ) THEN
                         TSYS1(KFRQ,1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J5,PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP),1)
                       ELSE 
                         TSYS1(KFRQ,2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J5,PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP),1)
                    END IF
               END IF
               TSYS1_AVR(1) = TSYS1_AVR(1) + TSYS1(KFRQ,1) 
               TSYS1_AVR(2) = TSYS1_AVR(2) + TSYS1(KFRQ,2) 
 450        CONTINUE
            IF ( KFRQ > 0 ) THEN
                 TSYS1_AVR(1) = TSYS1_AVR(1)/KFRQ
                 TSYS1_AVR(2) = TSYS1_AVR(2)/KFRQ
            END IF 
!@  write ( 6, * ) ' j2= ', j2, ' rev = ', fl_bas_reverse, &
!@     &    ' t1= ', pim%sta(pim%obs(j2)%sta_ind(1))%tsys%tsys(8,pim%obs(j2)%tsys_ind(1,pim%conf%frib_frq_grp),1), & ! %%%%
!@     &    ' t2= ', pim%sta(pim%obs(j2)%sta_ind(2))%tsys%tsys(8,pim%obs(j2)%tsys_ind(2,pim%conf%frib_frq_grp),1) ! %%%%%%%
!@  call pause ( 'asdas' ) ! %%%
!
! --------- Get antenna gain
!
            IND_FRG = PIM%CONF%FRQ_GRP
            GAIN = 0.0D0
            DO 460 J6=1,2
               IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%AVAIL ) THEN
                    IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%TYP(1,1) == 2 .AND. &
     &                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%NTAB > 0            ) THEN
!
! ---------------------- Compute the value of the polynomial that models
! ---------------------- elevation depedence of the gain
!
! ---------------------- The caveat: indexing of array GAIN%Y_VAL starts from 1
!
                         POLY_VAL = 0.0D0
                         DO 470 J7=1,PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%NTAB 
                            IF ( IS_R4_NAN ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%GAIN(J7,PIM%CONF%BEG_FRQ,1) ) ) THEN
                                 GAIN_POLY = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%Y_VAL(J7,PIM%CONF%BEG_FRQ,1) 
                               ELSE 
                                 GAIN_POLY = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%GAIN(J7,PIM%CONF%BEG_FRQ,1) 
                            END IF
                            IF ( PIM%CORR_NAME == 'VERA' .AND. J7 > 1 ) THEN
                                 POLY_VAL = POLY_VAL + GAIN_POLY* &
     &                                      (PIM%OBS(IND_OBS)%ELEV(J6)/DEG__TO__RAD)**(J7-2)
                               ELSE 
                                 POLY_VAL = POLY_VAL + GAIN_POLY* &
     &                                      (PIM%OBS(IND_OBS)%ELEV(J6)/DEG__TO__RAD)**(J7-1)
                            END IF
 470                     CONTINUE
!
! ---------------------- In the future we may find a more elegant way to deal with 
! ---------------------- this issue
!
                         IF ( FL_BAS_REVERSE ) THEN
                              IF ( J6 == 1 ) IND = 2
                              IF ( J6 == 2 ) IND = 1
                            ELSE 
                              IND = J6
                         END IF
                         GAIN(IND,1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J6))%GAIN(IND_FRG)%SENS(PIM%CONF%BEG_FRQ,1)*POLY_VAL
                    END IF
               END IF
 460        CONTINUE
!
            IF ( NUMB_BND > 1 ) THEN
                 EFF_DURA(2) = PIM_2ND%OBS(IND_OBS)%EFF_DUR(1) 
!
                 KFRQ = 0
                 DO 490 J9=PIM_2ND%CONF%BEG_FRQ,PIM_2ND%CONF%END_FRQ
                    KFRQ = KFRQ + 1
                    IF ( PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(1))%TSYS(IND_FRG)%AVAIL .AND. &
     &                   PIM_2ND%OBS(IND_OBS)%TSYS_IND(1,PIM_2ND%CONF%FRQ_GRP) > 0 ) THEN
!
                         IF ( FL_BAS_REVERSE ) THEN
                              TSYS2(KFRQ,2) = PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(1))%TSYS(IND_FRG)%TSYS(J9,PIM_2ND%OBS(IND_OBS)%TSYS_IND(1,PIM_2ND%CONF%FRQ_GRP),1)
                            ELSE 
                              TSYS2(KFRQ,1) = PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(1))%TSYS(IND_FRG)%TSYS(J9,PIM_2ND%OBS(IND_OBS)%TSYS_IND(1,PIM_2ND%CONF%FRQ_GRP),1)
                         END IF
                    END IF
                    IF ( PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(2))%TSYS(IND_FRG)%AVAIL .AND. &
     &                  PIM_2ND%OBS(IND_OBS)%TSYS_IND(2,PIM_2ND%CONF%FRQ_GRP) > 0 ) THEN
                        IF ( FL_BAS_REVERSE ) THEN
                             TSYS2(KFRQ,1) = PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(2))%TSYS(IND_FRG)%TSYS(J9,PIM_2ND%OBS(IND_OBS)%TSYS_IND(2,PIM_2ND%CONF%FRQ_GRP),1)
                           ELSE 
                             TSYS2(KFRQ,2) = PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(2))%TSYS(IND_FRG)%TSYS(J9,PIM_2ND%OBS(IND_OBS)%TSYS_IND(2,PIM_2ND%CONF%FRQ_GRP),1)
                        END IF
                    END IF
                    TSYS2_AVR(1) = TSYS2_AVR(1) + TSYS2(KFRQ,1) 
                    TSYS2_AVR(2) = TSYS2_AVR(2) + TSYS2(KFRQ,2) 
 490             CONTINUE
                 IF ( KFRQ > 0 ) THEN
                      TSYS2_AVR(1) = TSYS2_AVR(1)/KFRQ
                      TSYS2_AVR(2) = TSYS2_AVR(2)/KFRQ
                 END IF 
                 DO 4100 J10=1,2
                    IF ( PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%AVAIL ) THEN
                         IF ( PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%TYP(1,1) == 2 .AND. &
     &                        PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%NTAB > 0            ) THEN
!
! --------------------------- Compute the value of the polynomial that models
! --------------------------- elevation depedence of the gain
!
! --------------------------- The caveat: indexing of array GAIN%Y_VAL starts from 1
!
                              POLY_VAL = 0.0D0
                              DO 4110 J11=1,PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%NTAB 
                                 IF ( IS_R4_NAN ( PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%GAIN(J11,PIM_2ND%CONF%BEG_FRQ,1) ) ) THEN
                                      GAIN_POLY = PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%Y_VAL(J11,PIM_2ND%CONF%BEG_FRQ,1) 
                                    ELSE 
                                      GAIN_POLY = PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%GAIN(J11,PIM_2ND%CONF%BEG_FRQ,1) 
                                 END IF
                                 IF ( PIM%CORR_NAME == 'VERA' .AND. J11 > 1 ) THEN
                                      POLY_VAL = POLY_VAL + GAIN_POLY* &
     &                                          (PIM_2ND%OBS(IND_OBS)%ELEV(J10)/DEG__TO__RAD)**(J11-2)
                                    ELSE 
                                      POLY_VAL = POLY_VAL + GAIN_POLY* &
     &                                          (PIM_2ND%OBS(IND_OBS)%ELEV(J10)/DEG__TO__RAD)**(J11-1)
                                 END IF
 4110                         CONTINUE
!
! --------------------------- In the future we may find a more elegant way to deal with 
! --------------------------- this issue
!
                              IF ( FL_BAS_REVERSE ) THEN
                                   IF ( J10 == 1 ) IND = 2
                                   IF ( J10 == 2 ) IND = 1
                                 ELSE 
                                   IND = J10
                              END IF
                              GAIN(IND,2) = PIM_2ND%STA(PIM_2ND%OBS(IND_OBS)%STA_IND(J10))%GAIN(IND_FRG)%SENS(PIM_2ND%CONF%BEG_FRQ,1)*POLY_VAL
                         END IF
                    END IF
 4100            CONTINUE
               ELSE 
                 SNR(2)      = 0.0D0
                 FRN_AMPL(2) = 0.0D0
                 TSYS2_AVR   = 0.0D0
                 EFF_DURA(2) = 0.0D0
            END IF
!
            WRITE ( LUN, 110 ) OBS_DB_IND, &
     &                       PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND),    &
     &                       PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                       PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                       PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME, &
     &                       MJD_FRT, UTC_FRT, USER_SUP, &
     &                       PIM%OBS(IND_OBS)%ELEV(1), &
     &                       PIM%OBS(IND_OBS)%ELEV(2), &
     &                       PIM%OBS(IND_OBS)%AZ(1), &
     &                       PIM%OBS(IND_OBS)%AZ(2), &
     &                       EFF_DURA, SNR, FRN_AMPL, &
     &                       PIM%OBS(IND_OBS)%UVW(1:2)*VTD__C/PIM%REF_FREQ, &
     &                       TSYS1_AVR(1), TSYS1_AVR(2), &
     &                       TSYS2_AVR(1), TSYS2_AVR(2), &  
     &                       GAIN(1,1), GAIN(2,1),  &
     &                       GAIN(1,2), GAIN(2,2), IND_OBS
 110        FORMAT ( 'Obs: ', I6, 1X, A, 1X, A, '/', A, 1X, A, 6X, 1X, I5, &
     &                1X, F8.2, 2X, 1X, I11, &
     &                2(F8.5, 1X), 1X, 2(F8.5, 1X), &
     &                1X, 2(F7.2, 1X), 1X, &
     &                2(F8.2, 1X), 1X, 2(F8.6, 1X), 1X, &
     &                2(F12.1, 1X), 1X, 2(F7.1, 1X),  1X, 2(F7.1, 1X), &
     &                1X, 2(F7.5, 1X),  1X, 2(F7.5, 1X), 2X, I6 )
 430     CONTINUE 
 420  CONTINUE 
      CLOSE ( UNIT=LUN )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_AMPL_OUT: output file '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      CLOSE ( UNIT=LUN )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  PIMA_AMPL_OUT  !#!  
