      PROGRAM    BAS_SPLINE
! ************************************************************************
! *                                                                      *
! *   Program BAS_SPLINE
! *                                                                      *
! *  ### 03-MAR-2005   BAS_SPLINE  v1.0 (c)  L. Petrov  03-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  MBUF, MBAS, MIND
      PARAMETER  ( MBUF = 1024*1024 )
      PARAMETER  ( MBAS =     4096 )
      PARAMETER  ( MIND =       32 )
      CHARACTER  FILCON*128, FILBAS*128, BUF(MBUF)*128, STRING*128, &
     &           STA(2)*8, REG*4
      INTEGER*4  L_SPE, ADR_SPE, SIZE_SPE, NBUF, LIND, IND(2,MIND), KBUF, &
     &           MJD_BEG, MJD_REF, MJD_END, J1, J2, J3, NP, IUER 
      REAL*8     TAI_BEG, TAI_REF, TAI_END, ERR_MAX, TIM(MBAS), &
     &           VAL(MBAS), ERR(MBAS), AVR, WW
      REAL*8     TIM_BEG, TIM_END, TIM_REF, WEI_SCL
      PARAMETER  ( ERR_MAX = 200.0  ) ! mm
      PARAMETER  ( WEI_SCL = 2.D-3  ) !  m
      COMMON     / GUGU / NBUF, KBUF, BUF
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'/' )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() .LT. 2 ) THEN
           WRITE ( 6, * ) 'bas_spline.e <file_control> <file_bas>' 
           CALL EXIT ( 1 ) 
         ELSE 
           CALL GETARG ( 1, FILCON )
           CALL GETARG ( 2, FILBAS )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILCON, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      L_SPE = 0
      ADR_SPE = 0
      MJD_BEG = 0
      MJD_END = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  .EQ. '*' ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -3 )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATE_BEG' ) THEN
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,2):IND(2,2)), MJD_BEG, TAI_BEG, -3 )
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATE_END' ) THEN
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,2):IND(2,2)), MJD_END, TAI_END, -3 )
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATE_REF' ) THEN
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,2):IND(2,2)), MJD_REF, TAI_REF, -3 )
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BASELINE' ) THEN
              IF ( LIND .LT. 3 ) THEN
                   CALL ERR_LOG ( 5701, -1, 'BAS_SPINE', 'Error in parsing '// &
     &                 'keyword BASELINE in control file: two qualifiers '// &
     &                 'should follow' )
                   CALL EXIT ( 2 ) 
              END IF
              STA(1) = BUF(J1)(IND(1,2):IND(1,2)+7) 
              STA(2) = BUF(J1)(IND(1,3):IND(1,3)+7) 
         END IF
!@
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SPLINE_POS' ) THEN
              KBUF = J1
              STRING = BUF(J1)(IND(2,1)+1:)
              IUER = -1
              write ( 6, * ) 'string= '//string
              CALL PARSE_SPE ( STRING, L_SPE, ADR_SPE, SIZE_SPE, IUER )
              IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
         END IF
 410  CONTINUE 
!
      TIM_BEG = 2000.0D0 + ( MJD_BEG - J2000__MJD )*1.0D0/365.25D0 + &
     &                     ( TAI_BEG - 43200.0D0)/(86400.0D0*365.25D0)
      TIM_END = 2000.0D0 + ( MJD_END - J2000__MJD )*1.0D0/365.25D0 + &
     &                     ( TAI_END - 43200.0D0)/(86400.0D0*365.25D0)
      TIM_REF = 2000.0D0 + ( MJD_REF - J2000__MJD )*1.0D0/365.25D0 + &
     &                     ( TAI_REF - 43200.0D0)/(86400.0D0*365.25D0)
!
      WRITE ( 6, * ) ' L_SPE   = ', L_SPE
      WRITE ( 6, * ) ' MJD_BEG = ', MJD_BEG, ' MJD_END = ', MJD_END
      WRITE ( 6, * ) ' TAI_BEG = ', TAI_BEG, ' TAI_END = ', TAI_END
      WRITE ( 6, * ) ' STA     =  >>', STA(1), '  >>',  STA(2)
      WRITE ( 6, * ) ' TIM_BEG = ', TIM_BEG, ' TIM_END=',TIM_END
      WRITE ( 6, * ) ' TIM_REF = ', TIM_REF
      WRITE ( 6, * ) ' filbas= '//filbas(1:i_len(filbas))
      IF ( L_SPE == 0 ) CALL EXIT ( 0 )
!
      IUER = -1
      CALL RD_TEXT ( FILBAS, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      NP = 0
      AVR = 0.0D0
      WW  = 0.0D0
      DO 420 J2=1,NBUF
         CALL BLANK_TO_UNDERSCORE ( TRIM(BUF(J2)(46:53)) )
         CALL BLANK_TO_UNDERSCORE ( TRIM(BUF(J2)(55:62)) )
         IF ( ( BUF(J2)(46:53) .EQ. STA(1) .AND.        &
     &          BUF(J2)(55:62) .EQ. STA(2)       ) .OR. &
              ( BUF(J2)(55:62) .EQ. STA(1) .AND.        &
     &          BUF(J2)(46:53) .EQ. STA(2)       )      ) THEN
              NP = NP + 1
              READ ( UNIT=BUF(J2)(35:44), FMT='(F10.5)' ) TIM(NP)
              READ ( UNIT=BUF(J2)(64:77), FMT='(F14.5)' ) VAL(NP)
              READ ( UNIT=BUF(J2)(78:83), FMT='(F6.4)'  ) ERR(NP)
              IF ( ERR(NP) > ERR_MAX .OR. &
     &             TIM(NP) < TIM_BEG .OR. &
     &             TIM(NP) > TIM_END      ) THEN
!
                   NP = NP - 1
                   GOTO 420
              END IF
!
              TIM(NP) = (TIM(NP) - 2000.0D0)*365.25D0*86400.0D0
              VAL(NP) = VAL(NP)*1.D-3
              ERR(NP) = ERR(NP)*1.D-3
!@              ERR(NP) = 1.D-3
              AVR = AVR + (VAL(NP)/ERR(NP))**2
              WW  = WW  + (1.D0/ERR(NP))**2
         END IF
 420  CONTINUE
!
      AVR = DSQRT ( AVR/WW )
      WRITE ( 6, * ) ' NP=',NP, ' NBUF=',NBUF
      WRITE ( 6, * ) ' AVR=',AVR
      DO 430 J3=1,NP
         VAL(J3) = VAL(J3) - AVR
 430  CONTINUE 
!
      IF ( NP .LE. 2 ) THEN
           WRITE ( 6, * ) ' Too few points at baseline ', STA(1), ' ', STA(2), &
     &                    ' -- ', NP
           CALL EXIT ( 1 ) 
      END IF
      IUER = -1
      CALL COMP_SPLINE ( L_SPE, %VAL(ADR_SPE), TIM_REF, WEI_SCL, NP, TIM, VAL, &
     &                   ERR, STA, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
      IUER = -1
      CALL COMP_LINE ( TIM_REF, WEI_SCL, NP, TIM, VAL, ERR, STA, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      END  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   CFREAD ( STRING )
      IMPLICIT   NONE 
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 128 )
      INTEGER*4  NBUF, KBUF
      CHARACTER  BUF(MBUF)*128
      COMMON     / GUGU / NBUF, KBUF, BUF
      CHARACTER  STRING*(*)
      INTEGER*2  CFREAD
      INTEGER*4  KK, J1
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      KK = KBUF
      DO 410 J1=KK,NBUF
         KBUF = KBUF + 1
         STRING = BUF(KBUF)
         IF ( STRING(1:1)  .NE. '*' ) GOTO 810
!!         IF ( ILEN(STRING) .GT. '*' ) GOTO 810
         IF ( ILEN(STRING) .GT. 0 ) GOTO 810
 410  CONTINUE
 810  CONTINUE 
      CFREAD = ILEN ( STRING )
      RETURN
      END  !#!  
