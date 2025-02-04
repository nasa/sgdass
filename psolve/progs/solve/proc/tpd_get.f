      SUBROUTINE TPD_GET ( TPD, IOBS, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine TPD_GET
! *                                                                      *
! *  ### 07-NOV-2007    TPD_GET    v1.3 (c)  L. Petrov  20-MAY-2010 ###  *
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
!
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
           CALL ERR_LOG ( 7361, IUER, 'TPD_GET', 'Wrong observation '// &
     &         'index: '//STR )
           RETURN 
        ELSE IF ( IOBS == 1 ) THEN
           UTC_M_TAI  = TPD%HEADER%UTC_M_TAI 
           NUTPSI_AVE = TPD%HEADER%NUTPSI_AVE 
           NUTEPS_AVE = TPD%HEADER%NUTEPS_AVE 
           NUTPSI_DIF = TPD%HEADER%NUTPSI_DIF 
           NUTEPS_DIF = TPD%HEADER%NUTEPS_DIF 
           TIME0      = TPD%HEADER%TIME0 
!
! -------- Put station dependent information
!
           DO 410 J1=1,TPD%HEADER%NSTA
              IF ( ISITN_CHR(J1) .NE. TPD%STA(J1)%IVS_NAME ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 7362, IUER, 'TPD_GET', 'Site name '// &
     &                 'mismatch for the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'station. Socom name '//ISITN_CHR(J1)// &
     &                 ' , name saved in the TPD file '// &
     &                 TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))//' '// &
     &                 TPD%STA(J1)%IVS_NAME )
                   RETURN 
              END IF
!
              VSITEC(1,J1)  = TPD%STA(J1)%VSITEC(1)  
              VSITEC(2,J1)  = TPD%STA(J1)%VSITEC(2)  
              VSITEC(3,J1)  = TPD%STA(J1)%VSITEC(3)  
              NVSITEC(1,J1) = TPD%STA(J1)%NVSITEC(1) 
              NVSITEC(2,J1) = TPD%STA(J1)%NVSITEC(2) 
              NVSITEC(3,J1) = TPD%STA(J1)%NVSITEC(3) 
!
              VSITEV(1,J1)  = TPD%STA(J1)%VSITEV(1) 
              VSITEV(2,J1)  = TPD%STA(J1)%VSITEV(2) 
              VSITEV(3,J1)  = TPD%STA(J1)%VSITEV(3) 
              VAXOF(J1)     = TPD%STA(J1)%VAXOF     
!
              ICONT_I2 = 0
              IF ( J1 == 1 ) ICONT_I2 = 1
              CALL GETCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
              IF ( IERR_I2 .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( INT4(IERR_I2), STR )
                   CALL ERR_LOG ( 7363, IUER, 'TPD_GET', 'Error in '// &
     &                  'reading SITE namefile card: ' )
                   RETURN 
               END IF
               WRITE ( UNIT=STR(15:18), FMT='(A4)'    ) TPD%STA(J1)%CDP_NUMBER
               WRITE ( UNIT=STR(29:38), FMT='(F10.4)' ) TPD%STA(J1)%ECC_TRS(1)
               WRITE ( UNIT=STR(40:49), FMT='(F10.4)' ) TPD%STA(J1)%ECC_TRS(2)  
               WRITE ( UNIT=STR(51:60), FMT='(F10.4)' ) TPD%STA(J1)%ECC_TRS(3)  
!
               ICONT_I2 = 4
               CALL PUTCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
               IF ( IERR_I2 .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( INT4(IERR_I2), STR )
                    CALL ERR_LOG ( 7364, IUER, 'TPD_GET', 'Error in '// &
     &                  'writing SITE namefile card: '//STR )
                    RETURN 
               END IF
 410       CONTINUE 
!
! -------- Put source dependent information
!
           DO 420 J2=1,TPD%HEADER%NSOU
              IF ( ISTRN_CHR(J2) .NE. TPD%SOU(J2)%IVS_NAME ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 7365, IUER, 'TPD_GET', 'Source name '// &
     &                 'mismatch for the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'source. Socom name '//ISITN_CHR(J1)// &
     &                 ' , name saved in the TPD file '// &
     &                 TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))//' '// &
     &                 TPD%SOU(J2)%IVS_NAME )
                   RETURN 
              END IF
              VSTARC(1,J2) = TPD%SOU(J2)%ALPHA 
              VSTARC(2,J2) = TPD%SOU(J2)%DELTA 
 420       CONTINUE 
        ELSE IF ( IOBS > TPD%HEADER%NOBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR ) 
           CALL INCH  ( TPD%HEADER%NOBS, STR(101:120) ) 
           CALL ERR_LOG ( 7366, IUER, 'TPD_GET', 'Wrong observation '// &
     &         'index: '//STR(1:ILEN(STR(1:20)))//' -- exceeds the total '// &
     &         'number of observations in the session: '//STR(101:) )
           RETURN 
      END IF
!
      AZ(1)   = TPD%PARAM(IOBS)%AZIM(1) 
      AZ(2)   = TPD%PARAM(IOBS)%AZIM(2) 
      ELEV(1) = TPD%PARAM(IOBS)%ELEV(1) 
      ELEV(2) = TPD%PARAM(IOBS)%ELEV(2) 
!
      UT1_M_TAI = TPD%PARAM(IOBS)%UT1_M_TAI 
      X_POLE    = TPD%PARAM(IOBS)%X_POLE    
      Y_POLE    = TPD%PARAM(IOBS)%Y_POLE    
      UT1_RATE  = TPD%PARAM(IOBS)%UT1_RATE  
      XP_RATE   = TPD%PARAM(IOBS)%XP_RATE   
      YP_RATE   = TPD%PARAM(IOBS)%YP_RATE   
!
      DT = TPD%DELAY(IOBS)%THEO 
!
      BP(1,1,1)  = TPD%DELAY(IOBS)%DER(1)  
      BP(2,1,1)  = TPD%DELAY(IOBS)%DER(2)  
      BP(3,1,1)  = TPD%DELAY(IOBS)%DER(3)  
      BP(1,2,1)  = TPD%DELAY(IOBS)%DER(4)  
      BP(2,2,1)  = TPD%DELAY(IOBS)%DER(5)  
      BP(3,2,1)  = TPD%DELAY(IOBS)%DER(6)  
      SP(1,1)    = TPD%DELAY(IOBS)%DER(7)  
      SP(2,1)    = TPD%DELAY(IOBS)%DER(8)  
      ROTP(1,1)  = TPD%DELAY(IOBS)%DER(9)  
      ROTP(2,1)  = TPD%DELAY(IOBS)%DER(10) 
      ROTP(3,1)  = TPD%DELAY(IOBS)%DER(11) 
      NUTP(1,1)  = TPD%DELAY(IOBS)%DER(12) 
      NUTP(2,1)  = TPD%DELAY(IOBS)%DER(13) 
      AP(1,1)    = TPD%DELAY(IOBS)%DER(14) 
      AP(2,1)    = TPD%DELAY(IOBS)%DER(15) 
      AGRAD_PART(1,1,1)  = TPD%DELAY(IOBS)%DER(16) 
      AGRAD_PART(2,1,1)  = TPD%DELAY(IOBS)%DER(17) 
      AGRAD_PART(1,2,1)  = TPD%DELAY(IOBS)%DER(18) 
      AGRAD_PART(2,2,1)  = TPD%DELAY(IOBS)%DER(19) 
      AXOFP(1,1)    = TPD%DELAY(IOBS)%DER(20) 
      AXOFP(2,1)    = TPD%DELAY(IOBS)%DER(21) 
      TROP_WZD(1)   = TPD%DELAY(IOBS)%DER(22) 
      TROP_WZD(2)   = TPD%DELAY(IOBS)%DER(23) 
      ATM_ZENDEL(1) = TPD%DELAY(IOBS)%DER(24) 
      ATM_ZENDEL(2) = TPD%DELAY(IOBS)%DER(25) 
!
      IF ( TPD%HEADER%RATE_USE == SOLVE__YES ) THEN
           RT = TPD%RATE(IOBS)%THEO 
!
           BP(1,1,2) = TPD%RATE(IOBS)%DER(1)  
           BP(2,1,2) = TPD%RATE(IOBS)%DER(2)  
           BP(3,1,2) = TPD%RATE(IOBS)%DER(3)  
           BP(1,2,2) = TPD%RATE(IOBS)%DER(4)  
           BP(2,2,2) = TPD%RATE(IOBS)%DER(5)  
           BP(3,2,2) = TPD%RATE(IOBS)%DER(6)  
           SP(1,2)   = TPD%RATE(IOBS)%DER(7)  
           SP(2,2)   = TPD%RATE(IOBS)%DER(8)  
           ROTP(1,2) = TPD%RATE(IOBS)%DER(9)  
           ROTP(2,2) = TPD%RATE(IOBS)%DER(10) 
           ROTP(3,2) = TPD%RATE(IOBS)%DER(11) 
           NUTP(1,2) = TPD%RATE(IOBS)%DER(12) 
           NUTP(2,2) = TPD%RATE(IOBS)%DER(13) 
           AP(1,2) = TPD%RATE(IOBS)%DER(14) 
           AP(2,2) = TPD%RATE(IOBS)%DER(15) 
           AGRAD_PART(1,1,2) = TPD%RATE(IOBS)%DER(16) 
           AGRAD_PART(2,1,2) = TPD%RATE(IOBS)%DER(17) 
           AGRAD_PART(1,2,2) = TPD%RATE(IOBS)%DER(18) 
           AGRAD_PART(2,2,2) = TPD%RATE(IOBS)%DER(19) 
           AXOFP(1,2) = TPD%RATE(IOBS)%DER(20) 
           AXOFP(2,2) = TPD%RATE(IOBS)%DER(21) 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPD_GET  !#!#
