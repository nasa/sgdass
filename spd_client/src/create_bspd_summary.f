      SUBROUTINE CREATE_BSPD_SUMMARY ( DIROUT, L_BSPD, FILBSPD, PREF_OUT, &
     &                                 IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CREATE_BSPD_SUMMARY 
! *                                                                      *
! * ## 24-JUN-2014 CREATE_BSPD_SUMMARY v1.3 (c) L. Petrov 24-NOV-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( SPD_DEL__TYPE  ) :: SPD_DEL
      INTEGER*4  L_BSPD, IUER
      CHARACTER  DIROUT*(*), FILBSPD(L_BSPD)*(*), PREF_OUT*(*)
      TYPE     ( SPD_DEL__TYPE  ) :: SPD_IN
      CHARACTER  BUF(SPD__M_STA+16)*128, STR_BEG*21, STR_END*21, FILOUT*128, &
     &           FILTMP*128, STR*128, POSTFIX*8, LONG_STR*32768
      INTEGER*4  J1, J2, J3, NF, IN, IP, IS, PID, N1, NREC, NOFF, IER
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, LINDEX, RENAME
!
      NF = 0
      DO 410 J1=1,L_BSPD
         CALL ERR_PASS ( IUER, IER )
         CALL SPD_3D_BIN_READ_HEAD ( FILBSPD(J1), SPD_DEL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6531, IUER, 'CREATE_BSPD_SUMMARY', 'Error '// &
     &            'in reading the input SPD file '//FILBSPD(J1) )
              RETURN 
         END IF
         IF ( J1 == 1 ) THEN
              NF = NF + 1; BUF(NF) = BSPD_SUMM__LABEL
              NF = NF + 1; BUF(NF) = '#'
              NF = NF + 1; BUF(NF) = 'Last_Update:      '//GET_CDATE()
              STR_BEG = MJDSEC_TO_DATE ( SPD_DEL%TIM%MJD_BEG, SPD_DEL%TIM%TAI_BEG, IER )
              STR_END = MJDSEC_TO_DATE ( SPD_DEL%TIM%MJD_END, SPD_DEL%TIM%TAI_END, IER )
              NF = NF + 1
              WRITE ( UNIT=BUF(NF), FMT='("Min_epoch:    ",4X,I5,1X,F7.1, 1X, A)' ) &
     &                SPD_DEL%TIM%MJD_BEG, SPD_DEL%TIM%TAI_BEG, STR_BEG(1:21)
              NF = NF + 1
              WRITE ( UNIT=BUF(NF), FMT='("Max_epoch:    ",4X,I5,1X,F7.1, 1X, A)' ) &
     &                SPD_DEL%TIM%MJD_END, SPD_DEL%TIM%TAI_END, STR_END(1:21)
!
              NF = NF + 1
              NREC = SPD_DEL%LAB%TOT_NUM_DEL
              WRITE ( UNIT=BUF(NF), FMT='("Num_Epoch:    ",I9)' ) NREC
!
              NF = NF + 1
              NOFF = SIZEOF(SPD_DEL%LAB) + SPD_DEL%LAB%LEN_TIM + &
     &               SPD_DEL%LAB%LEN_STA + SPD_DEL%LAB%LEN_MOD + &
     &               SPD_DEL%LAB%LEN_MET + SPD_DEL%LAB%LEN_ELV + &
     &               SPD_DEL%LAB%LEN_AZM
              WRITE ( UNIT=BUF(NF), FMT='("Delay_offset: ",I9)' ) NOFF
!
              NF = NF + 1
              WRITE ( UNIT=BUF(NF), FMT='("Delay_record_len: ",I5)' ) SPD_DEL%LAB%LEN_DEL
!
              NF = NF + 1
              WRITE ( UNIT=BUF(NF), FMT='("Num_Stations: ",I9)' ) L_BSPD
!
              NF = NF + 1
              IS = LINDEX ( PREF_OUT, '/' )
              IF ( ILEN(TRIM(PREF_OUT(IS+1:))) == 0 ) THEN
                   CALL ERR_LOG ( 6532, IUER, 'CREATE_BSPD_SUMMARY', 'Trap of internal '// &
     &                 'control: empty prefix to write in the summary file' ) 
                   RETURN
              END IF
              WRITE ( UNIT=BUF(NF), FMT='("Prefix:     ",6X,A)'  ) TRIM(PREF_OUT(IS+1:))
!
              NF = NF + 1
              WRITE ( UNIT=BUF(NF), FMT='("Sample_Interval: ",F8.1, " sec,  ", F8.6, " days")' ) &
     &                SPD_DEL%TIM%TIM_STEP, SPD_DEL%TIM%TIM_STEP/86400.0D0
!
              NF = NF + 1
              IF ( SPD_DEL%MOD%N_RFR .LE. 1 ) THEN
                   WRITE ( UNIT=BUF(NF), FMT='("Num_Refr:     ",4X,I1,2X,A)' ) &
     &                     SPD_DEL%MOD%N_RFR, SPD_DEL%MOD%SPD_TYPE(1)
                 ELSE IF ( SPD_DEL%MOD%N_RFR .GE. 2 ) THEN
                   WRITE ( UNIT=BUF(NF), FMT='("Num_Refr:     ",4X,I1,2X,A,2X,A)' ) &
     &                     SPD_DEL%MOD%N_RFR, SPD_DEL%MOD%SPD_TYPE(1), SPD_DEL%MOD%SPD_TYPE(2)
              END IF
              NF = NF + 1; BUF(NF) = '#'
              NF = NF + 1; BUF(NF) = '#  Description of the algorithm:'
              NF = NF + 1; BUF(NF) = '#'
!
              CALL LIB$MOVC3 ( SPD_DEL%MOD%LEN_TEXT, %REF(SPD_DEL%MOD%TEXT), %REF(LONG_STR) )
              IP = 1
              DO 420 J2=1,SPD_DEL%MOD%N_LINES
                 IN = INDEX ( LONG_STR(IP:SPD_DEL%MOD%LEN_TEXT), CHAR(0) ) + IP-1
                 NF = NF + 1
                 IF ( IN > IP ) THEN
                      WRITE ( BUF(NF), '(A)' ) '@@  '//LONG_STR(IP:IN-1)
                    ELSE 
                      WRITE ( BUF(NF), '(A)' ) '@@  '
                 END IF
                 IP = IN + 1
 420          CONTINUE 
              NF = NF + 1; BUF(NF) = '#'
              NF = NF + 1; BUF(NF) = '#  Description of numerical weather model:'
              NF = NF + 1; BUF(NF) = '#'
!
              CALL LIB$MOVC3 ( SPD_DEL%MET%LEN_TEXT, %REF(SPD_DEL%MET%TEXT), %REF(LONG_STR) )
              IP = 1
              DO 430 J3=1,SPD_DEL%MET%N_LINES
                 IN = INDEX ( LONG_STR(IP:SPD_DEL%MET%LEN_TEXT), CHAR(0) ) + IP-1
                 NF = NF + 1
                 IF ( IN > IP ) THEN
                      WRITE ( BUF(NF), '(A)' ) '%%  '//LONG_STR(IP:IN-1)
                    ELSE 
                      WRITE ( BUF(NF), '(A)' ) '%%  '
                 END IF
                 IP = IN + 1
 430          CONTINUE 
!
              NF = NF + 1; BUF(NF) = '#'
              N1 = NF + 1
            ELSE
              IF ( SPD_DEL%LAB%TOT_NUM_DEL .NE. NREC ) THEN
                   CALL ERR_LOG ( 6533, IUER, 'CREATE_BSPD_SUMMARY', 'Different '// &
     &                 'number of delays in files '//TRIM(FILBSPD(J1))//' and '// &
     &                  FILBSPD(1) )
                   RETURN
              END IF
         END IF
!
         NF = NF + 1
         WRITE ( UNIT=BUF(NF), FMT=160 ) &
     &                         SPD_DEL%STA%NAME, &
     &                         SPD_DEL%STA%PHI_GDT/DEG__TO__RAD, &
     &                         SPD_DEL%STA%LON/DEG__TO__RAD, &
     &                         SPD_DEL%STA%HEI_ELL, &
     &                         SPD_DEL%STA%HEI_GEOID
 160     FORMAT ( 'Station_name: ',A, ' Lat_gdt: ', F9.5, ' Lon: ', F9.5, &
     &           ' Hei_ell: ', F8.2, ' Hei_geo: ', F8.2 )
 410  CONTINUE 
!
      CALL SORT_FAST_CH ( NF-N1+1, BUF(N1) )
!
      FILOUT = DIROUT(1:I_LEN(DIROUT))//'/bspd_summary.txt'
!
! --- Get information about PID of this process
!
      PID = GETPID()
      WRITE ( UNIT=POSTFIX(1:8), FMT='(I8)' ) PID
      CALL BLANK_TO_ZERO ( POSTFIX(1:8) )
      FILTMP = DIROUT(1:I_LEN(DIROUT))//'/bspd_summary.txt__'//POSTFIX
!
! --- Write into the temporary file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NF, BUF, FILTMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6534, IUER, 'CREATE_BSPD_SUMMARY', 'Error '// &
     &         'in writing into the output file '//FILTMP )
           RETURN
      END IF
!
! --- Rename the temporary file to the output file
!
      IS = RENAME ( TRIM(FILTMP)//CHAR(0), TRIM(FILOUT)//CHAR(0) )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6535, IUER, 'CREATE_BSPD_SUMMARY', 'Error '// &
     &          TRIM(STR)//' in renaming '//TRIM(FILTMP)//' into '// &
     &          FILOUT )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CREATE_BSPD_SUMMARY  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BLANK_TO_UNDSCR ( STR )
      IMPLICIT   NONE 
      CHARACTER  STR*(*)
      INTEGER*4  J1
      DO 410 J1=1,LEN(STR)
         IF ( STR(J1:J1) == ' ' ) STR(J1:J1) = '_'
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  BLANK_TO_UNDSCR  !#!#
