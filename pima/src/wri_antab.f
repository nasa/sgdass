      SUBROUTINE WRI_ANTAB ( M_CHN, N_TSYS, N_FRQ, MJD_TSYS, UTC_TSYS, &
     &                      IF_FRQ, LO_FRQ, POL_FRQ, &
     &                      SCAN_NAME, SOURCE_NAME, TSYS, &
     &                      N_ONS, MJD_ONS, UTC_ONS, SOURCE_ONS, &
     &                      N_CAB, MJD_CAB, UTC_CAB, CAB, N_ATM, MJD_ATM, &
     &                      UTC_ATM, PRES, TEMP, HUMID, &
     &                      STA_NAM, PROG__LABEL, FILIN, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRI_ANTAB
! *                                                                      *
! *  ### 10-FEB-2008    WRI_ANTAB   v1.1 (c)  L. Petrov 05-SEP-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M_CHN, N_TSYS, N_FRQ, N_ONS, N_CAB, N_ATM, IUER
      INTEGER*4  MJD_TSYS(N_TSYS), MJD_ONS(2,N_ONS), MJD_CAB(N_CAB), &
     &           MJD_ATM(N_ATM)
      REAL*8     UTC_TSYS(N_TSYS), UTC_ONS(2,N_ONS), UTC_CAB(N_CAB), &
     &           CAB(N_CAB), UTC_ATM(N_ATM), IF_FRQ(M_CHN), &
     &           LO_FRQ(M_CHN), TSYS(M_CHN,N_TSYS), &
     &           PRES(N_ATM), TEMP(N_ATM), HUMID(N_ATM)
      CHARACTER  SCAN_NAME(N_TSYS)*(*), SOURCE_NAME(N_TSYS)*(*), &
     &           SOURCE_ONS(N_ONS)*(*), STA_NAM*(*), POL_FRQ(M_CHN)*(*), &
     &           PROG__LABEL*(*), &
     &           FILIN*(*), FILOUT*(*)
      CHARACTER  OUT*4096, STR*128, STR1*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, LUN, IL, N_CHN, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_UNIT, LINDEX
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( OUT ) 
           CALL INCH  ( IER, OUT ) 
           CALL ERR_LOG ( 2831, IUER, 'WRI_ANTAB', 'Error '//OUT(1:I_LEN(OUT))// &
     &         'during attempt to open output file '//FILOUT )
           RETURN 
      END IF
!
      STR = FILIN
      IL = LINDEX ( FILIN, '/' )
      STR = FILIN(IL+1:)
!
      WRITE ( LUN, '(A)' ) '# LOG-ANTAB Format  Version of 2009.08.07'
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A)' ) PROG__LABEL
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A)' ) '# Generated from log file '//STR(1:I_LEN(STR))// &
     &                     '  on '//GET_CDATE()
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A)' ) 'STATION:  '//STA_NAM
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A,I5)' ) 'NUMB_DATA_ON:  ', N_ONS
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A)' ) '#         Data_on start           Data_on end             Source'
      WRITE ( LUN, '(A)' ) '# '
      DO 410 J1=1,N_ONS
         CALL ERR_PASS ( IUER, IER )
         STR  = MJDSEC_TO_DATE ( MJD_ONS(1,J1), UTC_ONS(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) 'J1 = ', J1, ' MJD(1), UTC(1) = ', &
     &                        MJD_ONS(1,J1), UTC_ONS(1,J1)
              CALL ERR_LOG ( 2833, IUER, 'WRI_ANTAB', 'Wrong MJD,UTC' )
              RETURN 
         END IF
!
         IF ( MJD_ONS(2,J1) > 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              STR1 = MJDSEC_TO_DATE ( MJD_ONS(2,J1), UTC_ONS(2,J1), IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) 'J1 = ', J1, ' MJD(2), UTC(2) = ', &
     &                             MJD_ONS(2,J1), UTC_ONS(2,J1)
                   CALL ERR_LOG ( 2833, IUER, 'WRI_ANTAB', 'Wrong MJD,UTC' )
                   RETURN 
              END IF
            ELSE 
              STR1 = STR
         END IF
!
         WRITE  ( LUN, 110 ) STR(1:22), STR1(1:22), SOURCE_ONS(J1)
 110     FORMAT ( 'DATA_ON:  ', A, 2X, A, 2X, A )
 410  CONTINUE 
!
      IF ( N_CAB > 0 ) THEN
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A,I5)' ) 'NUMB_CAB: ', N_CAB
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) 'SIGN_CAB:   +1'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '#       Date                     Cable delay (sec)'
           WRITE ( LUN, '(A)' ) '# '
           DO 420 J2=1,N_CAB
              STR = MJDSEC_TO_DATE ( MJD_CAB(J2), UTC_CAB(J2), IER )
              WRITE ( UNIT=LUN, FMT=120 ) STR(1:22), CAB(J2)
 120          FORMAT ( 'CABLE:  ', A, 2X, 1PD12.5 )
 420       CONTINUE 
      END IF
!
      IF ( N_ATM > 0 ) THEN
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A,I5)' ) 'NUMB_METEO: ', N_ATM
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '#       Date                    Temp    Pres    Humid'
           WRITE ( LUN, '(A)' ) '# '
           DO 430 J3=1,N_ATM
              STR = MJDSEC_TO_DATE ( MJD_ATM(J3), UTC_ATM(J3), IER )
              WRITE ( UNIT=LUN, FMT=130 ) STR(1:22), TEMP(J3), PRES(J3), HUMID(J3)
 130          FORMAT ( 'METEO:  ', A, 2X, F5.1, 2X, F7.0, 2X, F5.1 )
 430       CONTINUE 
      END IF
!
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A,I5)' ) 'NUMB_FRQ: ', N_FRQ
!
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A)' ) '#       Chan     IF_Freq     LO_Freq    Sky_freq   Pol'
      WRITE ( LUN, '(A)' ) '# '
!
      DO 440 J4=1,N_FRQ
         WRITE ( UNIT=OUT, FMT=140 ) J4, IF_FRQ(J4), LO_FRQ(J4), &
     &                                   IF_FRQ(J4) + LO_FRQ(J4), POL_FRQ(J4)
 140     FORMAT ( 'FRQ:  ', 2X, I4, 2X, F10.2, 2X, F10.2, 2X, F10.2, 5X, A1 )
         CALL CHASHR ( OUT(9:12) )
         WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
 440  CONTINUE 
!
      WRITE ( LUN, '(A)' ) '# '
      OUT = '#       Scan  Scan_name   Sou_name    TAI_Time_tag             @'
      DO 450 J5=1,N_FRQ
         IL = ILEN(OUT)
         OUT(IL:) = 'Ts #00  @'
         CALL INCH      ( J5, OUT(IL+4:IL+5) )
         CALL CHASHR    (     OUT(IL+4:IL+5) )
         CALL BLANK_TO_ZERO ( OUT(IL+4:IL+5) )
 450  CONTINUE 
      IL = ILEN(OUT)
      OUT(IL:IL) = ' '
      WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
      WRITE ( LUN, '(A)' ) '# '
!
      WRITE ( LUN, '(A,I5)' ) 'NUMB_TSYS: ', N_TSYS
      WRITE ( LUN, '(A)' ) '# '
      DO 460 J6=1,N_TSYS
         DO 470 J7=1,M_CHN
            IF ( TSYS(J7,J6) > 0.0  .OR. TSYS(J7,J6) == -1.0D0 ) N_CHN = J7
 470     CONTINUE 
!
         OUT = MJDSEC_TO_DATE ( MJD_TSYS(J6), UTC_TSYS(J6), -2 )
         OUT = 'TSYS:   @@@@  '//SCAN_NAME(J6)(1:10)//'  '// &
     &                           SOURCE_NAME(J6)(1:10)//'  '// &
     &                           OUT(1:22)//'  @'
         CALL INCH   ( J6, OUT(9:12) )
         CALL CHASHR (     OUT(9:12) )
!
         IL = ILEN(OUT)
         DO 480 J8=1,N_FRQ
            OUT(IL:IL) = ' '
            WRITE ( UNIT=OUT(IL:IL+8), FMT='(F7.1," @")', IOSTAT=IER ) TSYS(J8,J6)
            IL = ILEN(OUT)
 480     CONTINUE 
         OUT(IL:IL) = ' '
         WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
 460  CONTINUE 
!
      WRITE ( LUN, '(A)' ) '# '
      WRITE ( LUN, '(A)' ) '# LOG-ANTAB Format  Version of 2009.08.07'
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_ANTAB  !#!#
