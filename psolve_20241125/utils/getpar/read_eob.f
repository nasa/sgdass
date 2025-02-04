      SUBROUTINE READ_EOB ( FILEOB, MHEAD, NHEAD, HEAD_BUF, MSES, NSES, EOP, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_EOB reads REOP file in getpar EOB format.             *
! *                                                                      *
! *  ### 03-JUN-2002    READ_EOB   v1.4 (c)  L. Petrov  29-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INTEGER*4  MHEAD, NHEAD, MSES, NSES, IUER
      CHARACTER  FILEOB*(*), HEAD_BUF(MHEAD)*(*)
      TYPE ( EOP__STRU ) ::  EOP(MSES)
!
      TYPE ( EOB__CHAR ) ::  EOB
      CHARACTER  STR*128, STR_EOB*512, FMT_DATE*10
      LOGICAL*4  LEX, FL_EOP
      INTEGER*4  LUN, IOS, LEN_EOP, LEN_EOB, J1, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
!
      INQUIRE ( FILE=FILEOB, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5031, IUER, 'READ_EOB', 'EOB file '// &
     &                    FILEOB(1:I_LEN(FILEOB))//' was not found' )
           RETURN
      END IF
      LEN_EOP = SIZEOF ( EOP(1) )
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILEOB, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5032, IUER, 'READ_EOB', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in an attempt to open input file '//FILEOB )
           RETURN
      END IF
      READ ( UNIT=LUN, FMT='(A)' ) STR
!
      IF ( STR(1:46) .EQ. SIG_EOB ) THEN
           FMT_DATE = STR(37:46)
         ELSE IF ( STR(1:46) .EQ. SIG_EOB1 ) THEN
           FMT_DATE = STR(37:46)
         ELSE 
           CALL ERR_LOG ( 5033, IUER, 'READ_EOB', 'File '// &
     &          FILEOB(1:I_LEN(FILEOB))//' is not in the EOB format. The '// &
     &          SIG_EOP//' was expected at the header' )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
      NSES   = 0
      NHEAD  = 0
      FL_EOP = .FALSE.
      LEN_EOB = SIZEOF( EOB )
      CALL CLRCH ( STR_EOB )
      DO 410 J1=1,1024*1024*1024
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR_EOB
         CALL LIB$MOVC3 ( LEN_EOB, %REF(STR_EOB), EOB )
         IF ( IOS .EQ. -1 ) THEN
              GOTO 810
            ELSE IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 5034, IUER, 'READ_EOB', 'Error '// &
     &             STR(1:I_LEN(STR))//' in reading file '//FILEOB )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
!
         IF ( ILEN(STR_EOB) .EQ.  0  ) GOTO 410
         IF ( EOB%FLAG(1:1)  .EQ. '#'  ) THEN
              IF ( .NOT. FL_EOP ) THEN
                   NHEAD = NHEAD + 1
                   IF ( NHEAD .GT. MHEAD ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( MHEAD, STR )
                        CALL ERR_LOG ( 5035, IUER, 'READ_EOB', 'Parameter '// &
     &                      'MHEAD: '//STR(1:I_LEN(STR))//' turned out to '// &
     &                      'be too small' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   HEAD_BUF(NHEAD) = STR_EOB
              END IF
              GOTO 410
         END IF
!
         FL_EOP = .TRUE. ! since header is pover and EOP section started
         NSES = NSES + 1
         IF ( NSES .GT. MSES ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MSES, STR )
              CALL ERR_LOG ( 5036, IUER, 'READ_EOB', 'Parameter MSES '// &
     &             STR(1:I_LEN(STR))//' turned out to be too small' )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
!
         IF ( EOB%SEP_01 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_02 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_03 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_04 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_05 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_06 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_07 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_08 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_09 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_10 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_11 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_12 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_13 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_14 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_15 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_16 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_17 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_18 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_19 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_20 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_21 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_22 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_23 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_24 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_25 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_26 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_27 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_28 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_29 .NE. BLANK__B1  .OR. &
     &        EOB%SEP_30 .NE. BLANK__B1        ) THEN
!
              CALL CLRCH ( STR )
              CALL INCH  (  J1, STR )
              CALL ERR_LOG ( 5037, IUER, 'READ_EOB', 'Line '// &
     &             STR(1:I_LEN(STR))//' of the file '//FILEOB(1:I_LEN(FILEOB))// &
     &             ' violates the GETPAR_EOB format' )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
!
! ------ Initialization
!
         CALL NOUT ( LEN_EOP, EOP(NSES) )
         EOP(NSES)%STATUS = 0
!
! ------ Read the dates, database and session names
!
         READ ( UNIT=EOB%MJD_EOP, FMT='(F12.6)' ) EOP(NSES)%MJD_EOP
         READ ( UNIT=EOB%MJD_NUT, FMT='(F12.6)' ) EOP(NSES)%MJD_NUT
         EOP(NSES)%FLAG   = EOB%FLAG
         EOP(NSES)%DBNAME = EOB%DBNAME
         EOP(NSES)%SCODE  = EOB%SCODE
!
! ------ Read EOP values
!
         IF ( EOB%XPL_V(1:3) .NE. '-0 ' .AND. EOB%XPL_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%XPL_V, FMT='(F8.6)' ) EOP(NSES)%XPL_V
              EOP(NSES)%XPL_V = EOP(NSES)%XPL_V*1.D3*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPL__GTP )
         END IF
!
         IF ( EOB%YPL_V(1:3) .NE. '-0 '  .AND. EOB%YPL_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%YPL_V, FMT='(F8.6)' ) EOP(NSES)%YPL_V
              EOP(NSES)%YPL_V = EOP(NSES)%YPL_V*1.D3*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPL__GTP )
         END IF
!
         IF ( EOB%U1_V(1:3) .NE. '-0 '  .AND. EOB%U1_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%U1_V, FMT='(F11.7)' ) EOP(NSES)%U1_V
              EOP(NSES)%U1_V = EOP(NSES)%U1_V*1.D3*MSEC__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, U1__GTP )
         END IF
!
         IF ( EOB%DPSI_V(1:3) .NE. '-0 ' .AND. EOB%DPSI_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%DPSI_V, FMT='(F8.3)' ) EOP(NSES)%DPSI_V
              EOP(NSES)%DPSI_V = EOP(NSES)%DPSI_V*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DPSI__GTP )
         END IF
!
         IF ( EOB%DEPS_V(1:3) .NE. '-0 ' .AND. EOB%DEPS_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%DEPS_V, FMT='(F8.3)' ) EOP(NSES)%DEPS_V
              EOP(NSES)%DEPS_V = EOP(NSES)%DEPS_V*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DEPS__GTP )
         END IF
!
         IF ( EOB%XPR_V(1:3) .NE. '-0 '  .AND. EOB%XPR_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%XPR_V, FMT='(F9.6)' ) EOP(NSES)%XPR_V
              EOP(NSES)%XPR_V = EOP(NSES)%XPR_V*1.D3*MAS__TO__RAD/86400.0
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPR__GTP )
         END IF
!
         IF ( EOB%YPR_V(1:3) .NE. '-0 '  .AND. EOB%YPR_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%YPR_V, FMT='(F9.6)' ) EOP(NSES)%YPR_V
              EOP(NSES)%YPR_V = EOP(NSES)%YPR_V*1.D3*MAS__TO__RAD/86400.0
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPR__GTP )
         END IF
!
         IF ( EOB%UTR_V(1:3) .NE. '-0 '  .AND. EOB%UTR_V(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%UTR_V, FMT='(F7.4)' ) EOP(NSES)%UTR_V
              EOP(NSES)%UTR_V = EOP(NSES)%UTR_V*MSEC__TO__RAD/86400.0
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, UTR__GTP )
         END IF
!
! ------ Now read formal uncertainties
!
         IF ( EOB%XPL_E(1:3) .NE. '-0 '  .AND. EOB%XPL_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%XPL_E, FMT='(F8.6)' ) EOP(NSES)%XPL_E
              EOP(NSES)%XPL_E = EOP(NSES)%XPL_E*1.D3*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPL__GTP )
           ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, XPL__GTP )
         END IF
!
         IF ( EOB%YPL_E(1:3) .NE. '-0 '  .AND. EOB%YPL_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%YPL_E, FMT='(F8.6)' ) EOP(NSES)%YPL_E
              EOP(NSES)%YPL_E = EOP(NSES)%YPL_E*1.D3*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPL__GTP )
           ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, YPL__GTP )
         END IF
!
         IF ( EOB%U1_E(1:3) .NE. '-0 '  .AND. EOB%U1_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%U1_E, FMT='(F9.7)' ) EOP(NSES)%U1_E
              EOP(NSES)%U1_E = EOP(NSES)%U1_E*1.D3*MSEC__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, U1__GTP )
           ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, U1__GTP )
         END IF
!
         IF ( EOB%DPSI_E(1:3) .NE. '-0 ' .AND. EOB%DPSI_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%DPSI_E, FMT='(F7.3)' ) EOP(NSES)%DPSI_E
              EOP(NSES)%DPSI_E = EOP(NSES)%DPSI_E*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DPSI__GTP )
           ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, DPSI__GTP )
         END IF
!
         IF ( EOB%DEPS_E(1:3) .NE. '-0 ' .AND. EOB%DEPS_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%DEPS_E, FMT='(F7.3)' ) EOP(NSES)%DEPS_E
              EOP(NSES)%DEPS_E = EOP(NSES)%DEPS_E*MAS__TO__RAD
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DEPS__GTP )
           ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, DEPS__GTP )
         END IF
!
         IF ( EOB%XPR_E(1:3) .NE. '-0 '  .AND. EOB%XPR_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%XPR_E, FMT='(F9.6)' ) EOP(NSES)%XPR_E
              EOP(NSES)%XPR_E = EOP(NSES)%XPR_E*1.D3*MAS__TO__RAD/86400.0
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPR__GTP )
           ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, XPR__GTP )
         END IF
!
         IF ( EOB%YPR_E(1:3) .NE. '-0 '  .AND. EOB%YPR_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%YPR_E, FMT='(F9.6)' ) EOP(NSES)%YPR_E
              EOP(NSES)%YPR_E = EOP(NSES)%YPR_E*1.D3*MAS__TO__RAD/86400.0
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPR__GTP )
           ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, YPR__GTP )
         END IF
!
         IF ( EOB%UTR_E(1:3) .NE. '-0 '  .AND. EOB%UTR_E(1:3) .NE. '***' ) THEN
              READ ( UNIT=EOB%UTR_E, FMT='(F7.4)' ) EOP(NSES)%UTR_E
              EOP(NSES)%UTR_E = EOP(NSES)%UTR_E*MSEC__TO__RAD/86400.0
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, UTR__GTP )
            ELSE
              EOP(NSES)%STATUS = IBCLR ( EOP(NSES)%STATUS, UTR__GTP )
         END IF
!
! ------ Read correlations
!
         READ ( UNIT=EOB%C_XY,  FMT='(F6.4)' ) EOP(NSES)%C_XY
         READ ( UNIT=EOB%C_XU,  FMT='(F6.4)' ) EOP(NSES)%C_XU
         READ ( UNIT=EOB%C_YU,  FMT='(F6.4)' ) EOP(NSES)%C_YU
         READ ( UNIT=EOB%C_PE,  FMT='(F6.4)' ) EOP(NSES)%C_PE
         READ ( UNIT=EOB%C_URX, FMT='(F6.4)' ) EOP(NSES)%C_URX
         READ ( UNIT=EOB%C_URY, FMT='(F6.4)' ) EOP(NSES)%C_URY
         READ ( UNIT=EOB%C_URU, FMT='(F6.4)' ) EOP(NSES)%C_URU
!
! ------ Read other paramters
!
         READ ( UNIT=EOB%DURA,  FMT='(F5.2)' ) EOP(NSES)%DURA
         EOP(NSES)%DURA = EOP(NSES)%DURA*3600.0D0
         IF ( EOB%WRMS == '*******' ) EOB%WRMS = '99999.9'
         READ ( UNIT=EOB%WRMS,  FMT='(F7.2)', IOSTAT=IER ) EOP(NSES)%WRMS
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  (  J1, STR )
              CALL ERR_LOG ( 5038, IUER, 'READ_EOB', 'Erro in reading wrms '// &
     &             EOB%WRMS//' from line '//STR(1:I_LEN(STR))// &
     &             ' of the file '//FILEOB )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
         EOP(NSES)%WRMS = EOP(NSES)%WRMS*1.D-12
         READ ( UNIT=EOB%NOBS,  FMT='(I6)' ) EOP(NSES)%NOBS
         IF ( FMT_DATE .GE. '2007.08.30' ) THEN
              EOP(NSES)%C_NET = EOB%C_NET
            ELSE 
              CALL CLRCH ( EOP(NSES)%C_NET )
         END IF
 410  CONTINUE
 810  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      CLOSE ( UNIT=LUN )
      RETURN
      END  SUBROUTINE  READ_EOB  !#!#
