      PROGRAM    BASPLO
! ************************************************************************
! *                                                                      *
! *   Program basplo reads the input bas-file (output of getpar), input  *
! *   file with the epochs of episodic site motions. Then it runs an     *
! *   unfinite loop and asks a user to enter the baseline name in        *
! *   format STATION1/STATION2. If the station name contains blanks,     *
! *   they should be replaced with underscore character _ .              *
! *   BASPLO  computes statistics of the baseline length repeatability,  *
! *   prints this statistics in the text screen and prints the plot of   *
! *   the baseline length evolution in the graphic window using DiaGI    *
! *   graphic package.                                                   *
! *                                                                      *
! *  ### 16-DEC-1999     BASPLO    v2.3 (c)  L. Petrov  17-SEP-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*4  MBUF, MIND, M_STA, M_EPC
      PARAMETER  ( MBUF = 256*1024, MIND=32, M_STA = 256, M_EPC = 64 )
      CHARACTER  FINAM*128, TITLE*128, BUF(MBUF)*256
      REAL*8     TIM(MBUF), VAL(MBUF), ERR(MBUF), WEI(MBUF), &
     &           DR_VAL, DR_SIG, TIM_EPC(M_EPC), SH_VAL(M_EPC), SH_SIG(M_EPC), &
     &           ERR_MAX_DEF, ERR_MAX
      REAL*8     TIM1(MBUF), VAL1(MBUF), ERR1(MBUF), EPOCH_ESM(M_STA), &
     &           WRMS, CHI, VAL_1ST, VAL_SUM, WW
      CHARACTER  FILESM*128, CE_STA(M_STA)*8
      CHARACTER  SAVE_DIR*128, BASFIL__LABEL*46, STR*32, COM*512, ST1*8, ST2*8, &
     &           BAS*17, REG*5, STR_NO_ESM*8
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//CHAR(32)//'/,' )
      PARAMETER  ( BASFIL__LABEL = &
     &             '# GETPAR_BAS format version 1.0  of 2001.05.25' )
      PARAMETER  ( ERR_MAX_DEF = 20000.0 ) ! mm
      LOGICAL*4  FL_NL(M_STA), FL_DETREND
      INTEGER*4  NBUF, NP, KP, IV(MBUF), LIND, IND(2,MIND), KP1, &
     &           ID, J1, J2, J3, LE_STA, LN, IP, IOS, BOXCAR_PAR, L_EPC, &
     &           IER, IUER
      INTEGER*4  COMMAND_ARGUMENT_COUNT, I_LEN, ILEN, READ_LINE
!
      BOXCAR_PAR = 0
      ERR_MAX = ERR_MAX_DEF
      CALL CLRCH ( FILESM )
      FL_DETREND = .FALSE.
!
      IF ( COMMAND_ARGUMENT_COUNT() .GE. 1 ) THEN
           CALL GETARG ( 1, FINAM )
           CALL GET_COMMAND ( COM, LN, IOS )
!
           IP = INDEX ( COM, '-e' )
           IF ( IP .GT. 0 ) THEN
                FILESM = COM(IP+2:)
                IF ( ILEN(FILESM) .EQ. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 711, IUER, 'BASPLO', 'A value should '// &
     &                   'follow switch -e in command line' )
                     CALL EXIT ( 1 )
                END IF
                CALL CHASHL ( FILESM )
                IP = INDEX ( FILESM, ' ' )
                IF ( IP .GT. 0 ) CALL CLRCH ( FILESM(IP:) )
           END IF
!
           IP = INDEX ( COM, '-b' )
           IF ( IP .GT. 0 ) THEN
                STR = COM(IP+2:)
                IF ( ILEN(STR) .EQ. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 712, IUER, 'BASPLO', 'A value should '// &
     &                   'follow switch -b in command line' )
                     CALL EXIT ( 1 )
                END IF
                CALL CHASHL ( STR )
                IP = INDEX ( STR, ' ' )
                IF ( IP .GT. 0 ) CALL CLRCH ( STR(IP:) )
                CALL CHIN ( STR, BOXCAR_PAR )
           END IF
!
           IP = INDEX ( COM, '-l' )
           IF ( IP .GT. 0 ) THEN
                STR = COM(IP+2:)
                IF ( ILEN(STR) .EQ. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 713, IUER, 'BASPLO', 'A value should '// &
     &                   'follow switch -l in command line' )
                     CALL EXIT ( 1 )
                END IF
                CALL CHASHL ( STR )
                IP = INDEX ( STR, ' ' )
                IF ( IP .GT. 0 ) CALL CLRCH ( STR(IP:) )
                IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR = STR(1:IP)//'.'
                READ ( UNIT=STR, FMT='(F10.5)', IOSTAT=IOS ) ERR_MAX
                IF ( IOS .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 714, IUER, 'BASPLO', 'Error in parsing '// &
     &                   'the value of the -l option' )
                     CALL EXIT ( 1 )
                END IF
           END IF
           IP = INDEX ( COM, '-d' )
           IF ( IP .GT. 0 ) THEN
                FL_DETREND = .TRUE.
           ENDIF
        ELSE
           WRITE ( 6, 110 )
 110       FORMAT ( 1X,'Usage: basplo bas_file [-e esm_file] [-b boxcar_val] ', &
     &                 '[-l value] [-deternd]' )
           CALL EXIT ( 1 )
      END IF
      CALL GETENVAR ( "SOLVE_NOESM", STR_NO_ESM )
      IF ( ILEN(STR_NO_ESM) > 0 ) CALL TRAN ( 11, STR_NO_ESM, STR_NO_ESM )
!
      IF ( ILEN(FILESM) .EQ. 0 ) THEN
!
! -------- Get directory name SAVE_DIR
!
           CALL GETENVAR ( 'PSOLVE_SAVE_DIR', STR )
           IF ( ILEN(STR) .GT. 0 ) THEN
                SAVE_DIR = STR
             ELSE
               SAVE_DIR = SOLVE_SAVE_DIR
           ENDIF
           ID = ILEN(SAVE_DIR)
           IF ( SAVE_DIR(ID:ID) .NE. '/' ) THEN
                ID = ID + 1
                SAVE_DIR(ID:ID) = '/'
           ENDIF
!
! -------- Build the name of the epoch of the episodic site motions
!
           FILESM = SAVE_DIR(1:ID)//'glo.esm'
      END IF
!
! --- Read the file with the epochs of the episodic site motion
!
      IUER = -1
      CALL RD_ESM ( FILESM, M_STA, LE_STA, CE_STA, EPOCH_ESM, FL_NL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 715, IUER, 'BASPLO', 'Error in reading the '// &
     &         'episodic site motion file '//FILESM )
           CALL EXIT ( 1 )
      END IF
      IF ( STR_NO_ESM .EQ. 'YES' ) LE_STA = 0
!
      IER = -1
      CALL RD_TEXT ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 716, IUER, 'BASPLO', 'Error in reading the '// &
     &         'file with baseline lenghts '//FINAM )
           CALL EXIT ( 1 )
      END IF
!
      IF ( BUF(1)(1:46) .NE. BASFIL__LABEL ) THEN
           IUER = -1
           CALL ERR_LOG ( 717, IUER, 'BASPLO', 'Input file '// &
     &          FINAM(1:I_LEN(FINAM))//' is not in '//BASFIL__LABEL// &
     &          ' format' )
           CALL EXIT ( 1 )
      END IF
!
! --- Infinite loop of requests to enter the baseline and plotting the results
!
 910  CONTINUE
!
! --- Request: please enter the baseline
!
      CALL CLRCH ( STR )
!!      WRITE ( 6, '(A$)' ) 'Baseline? '
!!      READ ( UNIT=5, FMT='(A)', IOSTAT=IOS ) STR
      IOS = READ_LINE ( 'Baseline? ', STR )
      IOS = 0
      IF ( IOS .NE. 0  .OR.  ILEN(STR) .LE. 0 ) CALL EXIT ( 0 )
!
! --- Parse the answer
!
      CALL EXWORD ( STR, MIND, LIND, IND, REG, -3 )
      IF ( LIND .LT. 2 ) GOTO 910
!
! --- Extract the name of the first and the second statin of the baseline
!
      ST1 = STR( IND(1,1):IND(2,1) )
      ST2 = STR( IND(1,2):IND(2,2) )
!
      IF ( ST1 .GT. ST2 ) THEN
           STR = ST1
           ST1 = ST2
           ST2 = STR
      END IF
      BAS = ST1//'/'//ST2
!
! --- Replace blanks with underscores
!
!      DO 410 J1=1,17
!         IF ( BAS(J1:J1) .EQ. '_' ) BAS(J1:J1) = ' '
! 410  CONTINUE
!
      TITLE = 'Evolution of '//BAS//' length (mm)'
!
! --- Read the buffer with the file and search for the records with this baseline
!
      NP = 0
      DO 420 J2=1,NBUF
         IF ( BUF(J2)(46:62) .EQ. BAS ) THEN
              NP = NP + 1
              READ ( UNIT=BUF(J2)(35:44), FMT='(F10.5)' ) TIM(NP)
              READ ( UNIT=BUF(J2)(64:77), FMT='(F14.7)' ) VAL(NP)
              READ ( UNIT=BUF(J2)(78:83), FMT='(F6.3)', IOSTAT=IER ) ERR(NP)
              IF ( IER .NE. 0 ) ERR(NP) = 999.99
              WEI(NP) = 1.D0/ERR(NP)
              IF ( ERR(NP) .LT. ERR_MAX ) THEN
                   IV(NP) = 1
                ELSE
                  IV(NP) = 0
              END IF
         END IF
 420  CONTINUE
      IF ( NP .LE. 0 ) THEN
           WRITE ( 6, * ) ' No sessions at this baseline at all, sorry'
           GOTO 910
      END IF
!
! --- Compute repeatability
!
      IUER = -1
      CALL REPEATABILITY ( BOXCAR_PAR, ST1, ST2, M_STA, M_EPC, LE_STA, CE_STA, &
     &                     EPOCH_ESM, NP, TIM, VAL, WEI, ERR, IV, L_EPC, &
     &                     KP, DR_VAL, DR_SIG, TIM_EPC, SH_VAL, SH_SIG, WRMS, &
     &                     CHI, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Print statustics
!
      WRITE ( 6, 120 )  BAS, SH_VAL(1)*1.D-6, WRMS, WRMS/SH_VAL(1)*1.D9
 120  FORMAT ( A,' Length= ',F7.1,' km   wrms= ',F5.1,' mm   ',F5.1,' ppb' )
      WRITE ( UNIT=TITLE(I_LEN(TITLE)+2:), FMT='("wrms=",F5.1," mm")' ) &
     &      WRMS
      WRITE ( 6, 130 )  BAS, SH_VAL(1), SH_SIG(1), DR_VAL, DR_SIG
 130  FORMAT ( A17, ' Len: ',F14.2,' -+ ',F5.2,' mm  Vel: ',F6.2, &
     &         ' -+ ',F5.2,' mm/yr' )
      WRITE ( 6, 140 )  ERR_MAX
 140  FORMAT ( 'Err_max = ',F8.1, ' mm' )
      IF ( L_EPC .GE. 1 ) WRITE ( 6, 150 ) 1, TIM_EPC(1), SH_VAL(2), SH_SIG(2)
      IF ( L_EPC .GE. 2 ) WRITE ( 6, 150 ) 2, TIM_EPC(2), SH_VAL(3), SH_SIG(3)
      IF ( L_EPC .GE. 3 ) WRITE ( 6, 150 ) 3, TIM_EPC(3), SH_VAL(4), SH_SIG(4)
      IF ( L_EPC .GE. 4 ) WRITE ( 6, 150 ) 4, TIM_EPC(4), SH_VAL(5), SH_SIG(5)
 150  FORMAT ( 1X,' Break ',I1,' epoch: ',F10.5,'  Jump =', &
     &         F9.2,' -+ ',F5.2, ' mm'  )
!
! --- Prepare the title of the plot
!
      CALL DIAGI_SETDEF ( -3, 'DIAGI_CTIT', TITLE )
      CALL DIAGI_SETDEF ( -3, 'DIAGI_UNIT', 'Time (years)' )
      CALL DIAGI_SETDEF ( -3, 'DIAGI_ILST', 1     )
      CALL DIAGI_SETDEF ( -3, 'DIAGI_IPST', 5     )
      CALL DIAGI_SETDEF ( -3, 'DIAGI_IBST', 2     )
      IF ( FL_DETREND .AND. KP > 0 ) THEN
           VAL_1ST = VAL(1)
           VAL_SUM = 0.0D0
           WW = 0.0D0
           DO 430 J3=1,KP
              VAL(J3) = VAL(J3) - (VAL_1ST + DR_VAL*(TIM(J3) - TIM(1)))
              VAL_SUM = VAL_SUM + VAL(J3)/ERR(J3)
              WW =  WW + 1.D0/ERR(J3)
 430       CONTINUE 
           VAL_SUM = VAL_SUM/WW
           VAL = VAL - VAL_SUM
      END IF
!
      IF ( BOXCAR_PAR .GT. 1 ) THEN
!
! -------- Apply boxcar filter
!
           CALL BOXCAR ( BOXCAR_PAR, KP, TIM, VAL, ERR, KP1, TIM1, VAL1, ERR1 )
           IF ( KP1 .GE. 1 ) THEN
                CALL DIAGI_1E ( KP1, TIM1, VAL1, ERR1, -3 )
              ELSE
                WRITE ( 6, '(A)' ) 'Too few points, sorry'
           END IF
        ELSE
!
! -------- Plot the baseline length
!
           CALL DIAGI_1E ( KP, TIM, VAL, ERR, -3 )
      END IF
      GOTO 910
!
      END  !#!  BASPLO  #!#
