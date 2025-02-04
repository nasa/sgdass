      SUBROUTINE TPD_PUT ( TPD, IOBS, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine TPD_PUT
! *                                                                      *
! *  ### 07-NOV-2007    TPD_PUT    v1.2 (c)  L. Petrov  20-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'vtd.i'
      INCLUDE   'oborg.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'prfil.i'
      TYPE     ( TPD__TYPE     ) :: TPD
      CHARACTER  STR*128
      INTEGER*4  IOBS, J1, J2, IUER
      INTEGER*2  ICONT_I2, IERR_I2
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IOBS < 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR ) 
           CALL ERR_LOG ( 7351, IUER, 'TPD_PUT', 'Wrong observation '// &
     &         'index: '//STR )
           RETURN 
        ELSE IF ( IOBS == 1 ) THEN
           TPD%HEADER%UTC_M_TAI  = UTC_M_TAI 
           TPD%HEADER%NUTPSI_AVE = NUTPSI_AVE 
           TPD%HEADER%NUTEPS_AVE = NUTEPS_AVE 
           TPD%HEADER%NUTPSI_DIF = NUTPSI_DIF 
           TPD%HEADER%NUTEPS_DIF = NUTEPS_DIF 
           TPD%HEADER%TIME0      = TIME0
!
! -------- Put station dependent information
!
           DO 410 J1=1,TPD%HEADER%NSTA
              TPD%STA(J1)%IVS_NAME   = ISITN_CHR(J1)
              TPD%STA(J1)%VSITEC(1)  = VSITEC(1,J1) 
              TPD%STA(J1)%VSITEC(2)  = VSITEC(2,J1) 
              TPD%STA(J1)%VSITEC(3)  = VSITEC(3,J1) 
              TPD%STA(J1)%NVSITEC(1) = NVSITEC(1,J1) 
              TPD%STA(J1)%NVSITEC(2) = NVSITEC(2,J1) 
              TPD%STA(J1)%NVSITEC(3) = NVSITEC(3,J1) 
!
              TPD%STA(J1)%VSITEV(1) = VSITEV(1,J1) 
              TPD%STA(J1)%VSITEV(2) = VSITEV(2,J1) 
              TPD%STA(J1)%VSITEV(3) = VSITEV(3,J1) 
              TPD%STA(J1)%VAXOF     = VAXOF(J1)    
!
              ICONT_I2 = 0
              IF ( J1 == 1 ) ICONT_I2 = 1
              CALL GETCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
              IF ( IERR_I2 .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( INT4(IERR_I2), STR )
                   CALL ERR_LOG ( 8277, IUER, 'TPD_INIT', 'Error in '// &
     &                  'reading SITE namefile card: ' )
                   RETURN 
               END IF
               READ ( UNIT=STR(15:18), FMT='(A4)'    ) TPD%STA(J1)%CDP_NUMBER
               READ ( UNIT=STR(29:38), FMT='(F10.4)' ) TPD%STA(J1)%ECC_TRS(1)
               READ ( UNIT=STR(40:49), FMT='(F10.4)' ) TPD%STA(J1)%ECC_TRS(2)  
               READ ( UNIT=STR(51:57), FMT='(F10.4)' ) TPD%STA(J1)%ECC_TRS(3)  
!@               READ ( UNIT=STR(59:60), FMT='(A)' ) TPD%STA(J1)%ECC_TYPE
 410       CONTINUE 
!
! -------- Put source dependent information
!
           DO 420 J2=1,TPD%HEADER%NSOU
              TPD%SOU(J2)%IVS_NAME = ISTRN_CHR(J2)
              TPD%SOU(J2)%ALPHA = VSTARC(1,J2)
              TPD%SOU(J2)%DELTA = VSTARC(2,J2)
 420       CONTINUE 
        ELSE IF ( IOBS > TPD%HEADER%NOBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR(1:20) ) 
           CALL INCH  ( TPD%HEADER%NOBS, STR(101:120) ) 
           CALL ERR_LOG ( 7352, IUER, 'TPD_PUT', 'Wrong observation '// &
     &         'index: '//STR(1:I_LEN(STR(1:20)))//' -- exceeds the total '// &
     &         'number of observations in the session '//STR(101:110)// &
     &         ' stored in the TPD file: '//TPD%FILE_NAME )
           RETURN 
      END IF
!
      TPD%PARAM(IOBS)%AZIM(1) = AZ(1) 
      TPD%PARAM(IOBS)%AZIM(2) = AZ(2) 
      TPD%PARAM(IOBS)%ELEV(1) = ELEV(1) 
      TPD%PARAM(IOBS)%ELEV(2) = ELEV(2) 
!
      TPD%PARAM(IOBS)%UT1_M_TAI = UT1_M_TAI 
      TPD%PARAM(IOBS)%X_POLE    = X_POLE 
      TPD%PARAM(IOBS)%Y_POLE    = Y_POLE 
      TPD%PARAM(IOBS)%UT1_RATE  = UT1_RATE 
      TPD%PARAM(IOBS)%XP_RATE   = XP_RATE 
      TPD%PARAM(IOBS)%YP_RATE   = YP_RATE 
!
      TPD%DELAY(IOBS)%THEO = DT 
!
      TPD%DELAY(IOBS)%DER(1)  = BP(1,1,1) 
      TPD%DELAY(IOBS)%DER(2)  = BP(2,1,1)  
      TPD%DELAY(IOBS)%DER(3)  = BP(3,1,1)  
      TPD%DELAY(IOBS)%DER(4)  = BP(1,2,1)  
      TPD%DELAY(IOBS)%DER(5)  = BP(2,2,1)  
      TPD%DELAY(IOBS)%DER(6)  = BP(3,2,1)  
      TPD%DELAY(IOBS)%DER(7)  = SP(1,1)    
      TPD%DELAY(IOBS)%DER(8)  = SP(2,1)    
      TPD%DELAY(IOBS)%DER(9)  = ROTP(1,1)  
      TPD%DELAY(IOBS)%DER(10) = ROTP(2,1)  
      TPD%DELAY(IOBS)%DER(11) = ROTP(3,1)  
      TPD%DELAY(IOBS)%DER(12) = NUTP(1,1)  
      TPD%DELAY(IOBS)%DER(13) = NUTP(2,1)  
      TPD%DELAY(IOBS)%DER(14) = AP(1,1)    
      TPD%DELAY(IOBS)%DER(15) = AP(2,1)    
      TPD%DELAY(IOBS)%DER(16) = AGRAD_PART(1,1,1)  
      TPD%DELAY(IOBS)%DER(17) = AGRAD_PART(2,1,1)  
      TPD%DELAY(IOBS)%DER(18) = AGRAD_PART(1,2,1)  
      TPD%DELAY(IOBS)%DER(19) = AGRAD_PART(2,2,1)  
      TPD%DELAY(IOBS)%DER(20) = AXOFP(1,1)  
      TPD%DELAY(IOBS)%DER(21) = AXOFP(2,1)  
      TPD%DELAY(IOBS)%DER(22) = TROP_WZD(1) 
      TPD%DELAY(IOBS)%DER(23) = TROP_WZD(2) 
      TPD%DELAY(IOBS)%DER(24) = ATM_ZENDEL(1)
      TPD%DELAY(IOBS)%DER(25) = ATM_ZENDEL(2)
!
      IF ( TPD%HEADER%RATE_USE == SOLVE__YES ) THEN
           TPD%RATE(IOBS)%THEO = RT
!
           TPD%RATE(IOBS)%DER(1)  = BP(1,1,2) 
           TPD%RATE(IOBS)%DER(2)  = BP(2,1,2) 
           TPD%RATE(IOBS)%DER(3)  = BP(3,1,2) 
           TPD%RATE(IOBS)%DER(4)  = BP(1,2,2) 
           TPD%RATE(IOBS)%DER(5)  = BP(2,1,2) 
           TPD%RATE(IOBS)%DER(6)  = BP(3,2,2) 
           TPD%RATE(IOBS)%DER(7)  = SP(1,2) 
           TPD%RATE(IOBS)%DER(8)  = SP(2,2) 
           TPD%RATE(IOBS)%DER(9)  = ROTP(1,2) 
           TPD%RATE(IOBS)%DER(10) = ROTP(2,2) 
           TPD%RATE(IOBS)%DER(11) = ROTP(3,2) 
           TPD%RATE(IOBS)%DER(12) = NUTP(1,2) 
           TPD%RATE(IOBS)%DER(13) = NUTP(2,2) 
           TPD%RATE(IOBS)%DER(14) = AP(1,2) 
           TPD%RATE(IOBS)%DER(15) = AP(2,2) 
           TPD%RATE(IOBS)%DER(16) = AGRAD_PART(1,1,2) 
           TPD%RATE(IOBS)%DER(17) = AGRAD_PART(2,1,2) 
           TPD%RATE(IOBS)%DER(18) = AGRAD_PART(1,2,2) 
           TPD%RATE(IOBS)%DER(19) = AGRAD_PART(2,2,2) 
           TPD%RATE(IOBS)%DER(20) = AXOFP(1,2) 
           TPD%RATE(IOBS)%DER(21) = AXOFP(2,2) 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPD_PUT  !#!#
