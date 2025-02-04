      SUBROUTINE PD_PROC ( SPD, REQ_ID, REM_IP_STR, SOCK_FD, REM_FD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PD_PROC
! *                                                                      *
! *  ### 09-JAN-2015    PD_PROC   v1.2 (c)  L. Petrov  22-APR-2015  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'spd_common.i'
      TYPE     ( HEB__TYPE      ) :: HEB_3D
      TYPE     ( SPD_3D__TYPE   ) :: SPD
      TYPE     ( SPD_COM__TYPE  ) :: RECV_COM, SEND_COM
      TYPE     ( SPD_PDRQ__TYPE ) :: PDRQ
      TYPE     ( SPD_2P__TYPE   ),   POINTER :: SPD_2P(:)
      TYPE     ( SPD_4D__TYPE   ) :: SPD_4D
      REAL*8     EPS
      PARAMETER  ( EPS = 100.0 )
      CHARACTER  REQ_ID*(*), REM_IP_STR*(*)
      INTEGER*4  SOCK_FD, REM_FD, IUER
      INTEGER*4  MBUF, MFIL
      PARAMETER  ( MBUF = 2048 )
      PARAMETER  ( MFIL = 128*1024 )
      INTEGER*1  BUF(MBUF)
      INTEGER*8  DIR_DESC(16), MEL
      INTEGER*4  IL, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, L_FIL, IS, LEV, MJD_FIL, &
     &           IND_BEG, IND_END, IFMT, IND_TIM, ISTL, SIGALRM, ARG_LEN, &
     &           NMOD, IER
      REAL*4     EPS_LEV1, EPS_LEV2, EPS_LAT, EPS_LON, EPS_TIM
      PARAMETER  ( EPS_LEV1 = 0.001 )
      PARAMETER  ( EPS_LEV2 = 1.0 )
      PARAMETER  ( EPS_LAT  = 1.E-5 )
      PARAMETER  ( EPS_LON  = 3.E-5 )
      PARAMETER  ( EPS_TIM  = 1.0   )
      REAL*8     TAI_BEG, TAI_END, TAI_FIL, DIST, RLEV
      CHARACTER  STR*128, MESSAGE*256, STR_DATE_BEG*30, STR_DATE_END*30, &
     &           DATE_FIL*21, FILNAM*128, EXT*4, TEST_STR*16
      CHARACTER, ALLOCATABLE :: C_FIL(:)*128
      REAL*8     SPD__READ_TIMEOUT
      PARAMETER  ( SPD__READ_TIMEOUT   =  4.0D0 )
      INTEGER*4, EXTERNAL :: ALARM, GET_FILE_FROM_DIR, I_LEN, ILEN, SIGNAL, &
     &                       SOCK_READ_POLL, SOCK_WRITE, SPD_ALARM_PROC
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      EXT = '.heb'
      TEST_STR = 'none'
!
! --- Set signal for alarm
!
      CALL GET_SYSTEM_CONSTANT ( 'SIGALRM', SIGALRM, ARG_LEN )
      IS = SIGNAL ( %VAL(SIGALRM), SPD_ALARM_PROC )
      SUB_REM_FD = REM_FD
!
! --- Send command "sendpdrq"
!
      SEND_COM%VERB = 'sendpdrq'
      SEND_COM%LEN  = SIZEOF(PDRQ)
      IS = SOCK_WRITE ( REM_FD, SIZEOF(SEND_COM), SEND_COM, MESSAGE )
      IF ( IS .NE. SIZEOF(SEND_COM) ) THEN
           CALL SPD_LOG ( REQ_ID, 2311, 'E', 'PD_PROC', 'Error in sending '// &
     &                   'command to '//REM_IP_STR//' : '// &
     &                   MESSAGE(1:I_LEN(MESSAGE)) )
           CALL ERR_PASS ( 2311, IUER )
           RETURN 
      END IF
!
! --- Read the response: structure PDRQ -- the header of the dlay table
!
      CALL NOUT ( SIZEOF(PDRQ), PDRQ )
      IS = SOCK_READ_POLL ( REM_FD, SIZEOF(PDRQ), PDRQ, SIZEOF(PDRQ), &
     &                      SPD__READ_TIMEOUT, MESSAGE )
      IF ( INDEX ( MESSAGE, "Time out has expired" ) > 0 ) THEN
           CALL SPD_LOG ( REQ_ID, 2312, 'E', 'PD_PROC', &
     &         'spd_server did not get data from IP '//REM_IP_STR )
           CALL ERR_PASS ( 2312, IUER )
           RETURN 
        ELSE IF ( IS == -1 ) THEN
           CALL SPD_LOG ( REQ_ID, 2313, 'E', 'PD_PROC', 'Error in '// &
     &         'reading pdrq from '//REM_IP_STR// &
     &         ' : '//MESSAGE(1:I_LEN(MESSAGE)) )
           CALL ERR_PASS ( 2313, IUER )
           RETURN 
      END IF
!
! --- Parse the data structure
!
      IER = 0
      STR_DATE_BEG = MJDSEC_TO_DATE ( PDRQ%MJD_BEG, PDRQ%TAI_BEG, IER )
      IER = 0
      STR_DATE_END = MJDSEC_TO_DATE ( PDRQ%MJD_END, PDRQ%TAI_END, IER )
      CALL INCH ( PDRQ%NUM_POI, STR )
      CALL SPD_LOG ( REQ_ID, 2314, 'I', ' ', 'Received pd request for '// &
     &               STR(1:I_LEN(STR))//' points for [ '// &
     &               STR_DATE_BEG(1:23)//', '//STR_DATE_END(1:23)//' ]' )
!
! --- Allocate dynamic memory SPD_2P for the path delay table
!
      ALLOCATE ( SPD_2P(PDRQ%NUM_POI), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MFIL*SIZEOF(C_FIL(1)), STR )
           CALL SPD_LOG ( REQ_ID, 2315, 'E', 'PD_PROC', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memoiry for array SPD_2P' )
           CALL ERR_PASS ( 2315, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
!
! --- Send command "sendpdtb" -- request of the path delay table
!
      SEND_COM%VERB = 'sendpdtb'
      SEND_COM%LEN  = PDRQ%NUM_POI*SIZEOF(SPD_2P(1))
      IS = SOCK_WRITE ( REM_FD, SIZEOF(SEND_COM), SEND_COM, MESSAGE )
      IF ( IS .NE. SIZEOF(SEND_COM) ) THEN
           CALL SPD_LOG ( REQ_ID, 2316, 'E', 'PD_PROC', 'Error in sending '// &
     &                   'command to '//REM_IP_STR//' : '// &
     &                   MESSAGE(1:I_LEN(MESSAGE)) )
           CALL ERR_PASS ( 2316, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
!
! --- Read the input delay table
!
      CALL NOUT ( PDRQ%NUM_POI*SIZEOF(SPD_2P(1)), SPD_2P )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL CLRCH  ( STR ) 
      CALL INCH   ( PDRQ%NUM_POI*SIZEOF(SPD_2P(1)), STR )
      CALL SPD_LOG ( REQ_ID, 8001, 'I', 'PD_PROC', 'Run sock_read_poll of size '//STR )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IS = SOCK_READ_POLL ( REM_FD, PDRQ%NUM_POI*SIZEOF(SPD_2P(1)), SPD_2P, &
     &                      PDRQ%NUM_POI*SIZEOF(SPD_2P(1)), &
     &                      SPD__READ_TIMEOUT, MESSAGE )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL SPD_LOG ( REQ_ID, 8002, 'I', 'PD_PROC', 'Finished sock_read_poll' )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF ( INDEX ( MESSAGE, "Time out has expired" ) > 0 ) THEN
           CALL SPD_LOG ( REQ_ID, 2317, 'E', 'PD_PROC', &
     &         'spd_server did not get data from IP '//REM_IP_STR )
           CALL ERR_PASS ( 2317, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
        ELSE IF ( IS == -1 ) THEN
           CALL SPD_LOG ( REQ_ID, 2318, 'E', 'PD_PROC', 'Error in '// &
     &         'reading spd_2p from '//REM_IP_STR// &
     &         ' : '//MESSAGE(1:I_LEN(MESSAGE)) )
           CALL ERR_PASS ( 2318, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
!
! --- Set alarm -- to send "wait_1s " command every second
!
      IS = ALARM ( %VAL(SPD__ALARM_INT) )
!
! --- Check the style of the request: satellite data or point-to-point
!
      DO 410 J1=1,PDRQ%NUM_POI  
         DIST= DSQRT ( (SPD_2P(J1)%COO_EMI(1) - SPD_2P(J1)%COO_REC(1))**2 + &
     &                 (SPD_2P(J1)%COO_EMI(2) - SPD_2P(J1)%COO_REC(2))**2 + &
     &                 (SPD_2P(J1)%COO_EMI(3) - SPD_2P(J1)%COO_REC(3))**2   )
         IF ( DIST > 1.5*SPD__U_MAX ) THEN
              ISTL = SPD__SAT
            ELSE 
              ISTL = SPD__2P
         END IF
 410  CONTINUE 
!
! === Process the request
!
! --- Allocate memory for the array of file names with refrqctivity
!
      ALLOCATE ( C_FIL(MFIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MFIL*SIZEOF(C_FIL(1)), STR )
           CALL SPD_LOG ( REQ_ID, 2319, 'E', 'PD_PROC', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memoiry for array C_FIL' )
           CALL ERR_PASS ( 2319, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
!
! --- Read directory with refractivity and collect file names
!
      LEV = 0
      L_FIL = 0
      DO 420 J1=1,MFIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, SPD%CONF%RESP_DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL SPD_LOG ( REQ_ID, 2320, 'E', 'PD_PROC', 'Error in '// &
     &            'reading input directory '// &
     &            SPD%CONF%RESP_DIR(1:I_LEN(SPD%CONF%RESP_DIR))// &
     &            '  '//FILNAM )
              DEALLOCATE ( C_FIL )
              DEALLOCATE ( SPD_2P )
              CALL ERR_PASS ( 2320, IUER )
              RETURN
         END IF
         IF ( LEV == 0 ) GOTO 820 ! No more files remained
!
! ------ Check whether this wil is appropriate
!
         IF ( INDEX ( FILNAM, EXT )     < 1    ) GOTO 420
         IF ( INDEX ( FILNAM, '#' )     .GE. 1 ) GOTO 420
         IF ( INDEX ( FILNAM, '/refr/' ) < 1   ) GOTO 420
!
         IL = ILEN(FILNAM)
         IF ( IL < 22 ) GOTO 420
!
! ------ Extract the date from the file name
!
         DATE_FIL = FILNAM(IL-16:IL-13)//'.'//FILNAM(IL-12:IL-11)//'.'// &
     &              FILNAM(IL-10:IL-6)//':'//FILNAM(IL-5:IL-4)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TAI_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL SPD_LOG ( REQ_ID, 2321, 'E', 'PD_PROC', 'Unexpected '// &
     &            'format of file name '//FILNAM(1:I_LEN(FILNAM))// &
     &            ' was found in input directory '//SPD%CONF%RESP_DIR )
              DEALLOCATE ( C_FIL )
              DEALLOCATE ( SPD_2P )
              CALL ERR_PASS ( 2321, IUER )
              RETURN
         END IF
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > MFIL )  THEN
!
! ----------- Too many files...
!
              CALL CLRCH ( STR )
              CALL INCH  ( MFIL, STR )
              CALL SPD_LOG ( REQ_ID, 2322, 'E', 'PD_PROC', 'Too many files '// &
     &           'were found in directory '// &
     &           SPD%CONF%RESP_DIR(1:I_LEN(SPD%CONF%RESP_DIR))// &
     &            ' -- more than '//STR )
              DEALLOCATE ( C_FIL )
              DEALLOCATE ( SPD_2P )
              CALL ERR_PASS ( 2322, IUER )
              RETURN
         END IF
         C_FIL(L_FIL) = FILNAM 
 420  CONTINUE 
 820  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           CALL SPD_LOG ( REQ_ID, 2323, 'E', 'PD_PROC', 'No files with '// &
     &         'extension '//EXT(1:I_LEN(EXT))//' were found in the '// &
     &         'input directory '//SPD%CONF%RESP_DIR )
           DEALLOCATE ( C_FIL )
           DEALLOCATE ( SPD_2P )
           CALL ERR_PASS ( 2323, IUER )
           RETURN
         ELSE
!
! -------- Sort files alphabetically. That means chronological sorting
!
           CALL SORT_CH ( L_FIL, C_FIL )
      END IF
!
      IF ( L_FIL == 0 ) THEN
           CALL SPD_LOG ( REQ_ID, 2324, 'E', 'PD_PROC', 'No appropriate '// &
     &         'data files were found in the input directory '// &
     &         SPD%CONF%RESP_DIR )
           DEALLOCATE ( C_FIL )
           DEALLOCATE ( SPD_2P )
           CALL ERR_PASS ( 2324, IUER )
           RETURN
      END IF
!
! --- Search the statrt and stop files names that are neeeded for 
! --- processing this request
!
      IND_BEG = 0
      IND_END = 0
      DO 430 J3=1,L_FIL
         IL = ILEN(C_FIL(J3))
         DATE_FIL = C_FIL(J3)(IL-16:IL-13)//'.'//C_FIL(J3)(IL-12:IL-11)//'.'// &
     &              C_FIL(J3)(IL-10:IL-6)//':'//C_FIL(J3)(IL-5:IL-4)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TAI_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL SPD_LOG ( REQ_ID, 2325, 'E', 'PD_PROC', 'Unexpected '// &
     &            'format of file name '//FILNAM(1:I_LEN(FILNAM))// &
     &            ' was found in input directory '//SPD%CONF%RESP_DIR )
              DEALLOCATE ( C_FIL )
              DEALLOCATE ( SPD_2P )
              CALL ERR_PASS ( 2325, IUER )
              RETURN
         END IF
!
         IF ( (MJD_FIL*86400.0D0 + TAI_FIL) < (PDRQ%MJD_BEG*86400.0D0 + PDRQ%TAI_BEG) + EPS ) THEN
              IND_BEG = J3
         END IF 
         IF ( (MJD_FIL*86400.0D0 + TAI_FIL) > (PDRQ%MJD_END*86400.0D0 + PDRQ%TAI_END) - EPS ) THEN
              IF ( IND_END == 0 ) IND_END = J3
         END IF 
 430  CONTINUE 
!
      IF ( IND_BEG < 1 ) THEN
!
! -------- Did not find teh first file of the range
!
           IL = ILEN(C_FIL(1))
           DATE_FIL = C_FIL(1)(IL-16:IL-13)//'.'//C_FIL(1)(IL-12:IL-11)//'.'// &
     &                C_FIL(1)(IL-10:IL-6)//':'//C_FIL(1)(IL-5:IL-4)//':00.0'
           STR = MJDSEC_TO_DATE ( SPD_2P(1)%MJD, SPD_2P(1)%TAI, IUER )
           CALL SPD_LOG ( REQ_ID, 2326, 'E', 'PD_PROC', 'Begin date in the '// &
     &         'data '//STR(1:21)//' is before the first epoch of '// &
     &         'atmospheric data '//DATE_FIL )
           DEALLOCATE ( C_FIL  )
           DEALLOCATE ( SPD_2P )
           CALL ERR_PASS ( 2326, IUER )
           RETURN
      END IF
!
      IF ( IND_END < 1 ) THEN
!
! -------- Did not find  the last file of the range
!
           IL = ILEN(C_FIL(L_FIL))
           DATE_FIL = C_FIL(L_FIL)(IL-16:IL-13)//'.'// &
     &                C_FIL(L_FIL)(IL-12:IL-11)//'.'// &
     &                C_FIL(L_FIL)(IL-10:IL-6)//':'// &
     &                C_FIL(L_FIL)(IL-5:IL-4)//':00.0'
           IUER = -1
           STR = MJDSEC_TO_DATE ( SPD_2P(PDRQ%NUM_POI)%MJD, &
     &                            SPD_2P(PDRQ%NUM_POI)%TAI, IUER )
           CALL SPD_LOG ( REQ_ID, 2327, 'E', 'PD_PROC', 'End date in the '// &
     &         'data '//STR(1:21)//' is after the last epoch of '// &
     &         'atmospheric data '//DATE_FIL )
           DEALLOCATE ( C_FIL  )
           DEALLOCATE ( SPD_2P )
           CALL ERR_PASS ( 2327, IUER )
           RETURN
      END IF
!
      IF ( IND_BEG - 2 .GE. 1 ) THEN
           IND_BEG = IND_BEG - 2
         ELSE 
           IND_BEG = 1
      END IF
      IF ( IND_BEG + 2 .LE. L_FIL ) THEN
           IND_END = IND_END + 2
         ELSE 
           IND_END = L_FIL
      END IF    
!
      IND_TIM = 0
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Now read selectged data files with refractivity
!
      DO 440 J4=IND_BEG,IND_END
         IND_TIM = IND_TIM + 1
!
! ------ Read the C_FIL(J4) th file
!
         IUER = -1
         CALL READ_HEB ( C_FIL(J4), HEB_3D, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL SPD_LOG ( REQ_ID, 2328, 'E', 'PD_PROC', 'Error in reading '// &
     &            'input file with refractivity '//C_FIL(J4) )
              DEALLOCATE ( C_FIL )
              DEALLOCATE ( SPD_2P )
              CALL ERR_PASS ( 2328, IUER )
              RETURN
         END IF
         HEB_3D%TAI = HEB_3D%UTC
!
         IF ( IND_TIM == 1 ) THEN
!
! ----------- The first file? Then we need initialize SPD_4D data structure that
! ----------- keeps refractivity
!
              IUER = -1
              CALL SPD_4D_INIT ( SPD_4D, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL SPD_LOG ( REQ_ID, 2329, 'E', 'PD_PROC', 'Failure '// &
     &                 'in an attempt to initialize object SPD' )
                   DEALLOCATE ( C_FIL )
                   DEALLOCATE ( SPD_2P )
                   CALL ERR_PASS ( 2329, IUER )
                   RETURN
              END IF
              SPD_4D%DIMS(1) = HEB_3D%DIMS(1) - SPD__MDEG
              SPD_4D%DIMS(2) = HEB_3D%DIMS(2) - SPD__MDEG
              SPD_4D%DIMS(3) = HEB_3D%DIMS(3) - SPD__MDEG
              SPD_4D%DIMS(4) = IND_END - IND_BEG + 1
              SPD_4D%DIMS(5) = HEB_3D%DIMS(4)
!
! ----------- Allocate dynamic memory for %LEV array
!
              IF ( ASSOCIATED ( SPD_4D%LEV ) ) DEALLOCATE ( SPD_4D%LEV )
              ALLOCATE ( SPD_4D%LEV(1-SPD__MDEG:SPD_4D%DIMS(1)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD__MLEV+SPD_4D%DIMS(1)), STR )
                   CALL SPD_LOG ( REQ_ID, 2330, 'E', 'PD_PROC', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%LEV' )
                   DEALLOCATE ( C_FIL )
                   DEALLOCATE ( SPD_2P )
                   CALL SPD_4D_QUIT ( SPD_4D )
                   CALL ERR_PASS ( 2330, IUER )
                   RETURN
              END IF
!
! ----------- Allocate dynamic memory for %LON array
!
              IF ( ASSOCIATED ( SPD_4D%LON ) ) DEALLOCATE ( SPD_4D%LON )
              ALLOCATE ( SPD_4D%LON(1-SPD__MDEG:SPD_4D%DIMS(2)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD_4D%DIMS(2)+SPD__MDEG), STR )
                   CALL SPD_LOG ( REQ_ID, 2331, 'E', 'PD_PROC', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%LON' )
                   DEALLOCATE ( C_FIL  )
                   DEALLOCATE ( SPD_2P )
                   CALL SPD_4D_QUIT ( SPD_4D )
                   CALL ERR_PASS ( 2331, IUER )
                   RETURN
              END IF
!
! ----------- Allocate dynamic memory for %LAT array
!
              IF ( ASSOCIATED ( SPD_4D%LAT ) ) DEALLOCATE ( SPD_4D%LAT )
              ALLOCATE ( SPD_4D%LAT(1-SPD__MDEG:SPD_4D%DIMS(3)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD_4D%DIMS(3)+SPD__MDEG), STR )
                   CALL SPD_LOG ( REQ_ID, 2332, 'E', 'PD_PROC', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%LON' )
                   DEALLOCATE ( C_FIL )
                   DEALLOCATE ( SPD_2P )
                   CALL SPD_4D_QUIT ( SPD_4D )
                   CALL ERR_PASS ( 2332, IUER )
                   RETURN
              END IF
!
! ----------- Allocate dynamic memory for %TIM array
!
              IF ( ASSOCIATED ( SPD_4D%TIM ) ) DEALLOCATE ( SPD_4D%TIM )
              ALLOCATE ( SPD_4D%TIM(1-SPD__MDEG:SPD_4D%DIMS(4)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD_4D%DIMS(4)+SPD__MDEG), STR )
                   CALL SPD_LOG ( REQ_ID, 2333, 'E', 'PD_PROC', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%TIM' )
                   DEALLOCATE ( C_FIL )
                   DEALLOCATE ( SPD_2P )
                   CALL SPD_4D_QUIT ( SPD_4D )
                   CALL ERR_PASS ( 2333, IUER )
                   RETURN
              END IF
!
! ----------- Allocate dynamic memory for %REFR array with refractivity
!
              ALLOCATE ( SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1-SPD__MDEG:SPD_4D%DIMS(4),SPD_4D%DIMS(5)), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   MEL = INT8(SPD_4D%DIMS(1)+SPD__MDEG)*INT8(SPD_4D%DIMS(2)+SPD__MDEG)* &
     &                   INT8(SPD_4D%DIMS(3)+SPD__MDEG)*INT8(SPD_4D%DIMS(4)+SPD__MDEG)* &
     &                   INT8(SPD_4D%DIMS(5))
                   CALL IINCH8 ( MEL, STR )
                   CALL SPD_LOG ( REQ_ID, 2334, 'E', 'PD_PROC', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%REFR' )
                   DEALLOCATE ( C_FIL )
                   DEALLOCATE ( SPD_2P )
                   CALL SPD_4D_QUIT ( SPD_4D )
                   CALL ERR_PASS ( 2334, IUER )
                   RETURN
              END IF
!
! ----------- Initialization
!
              SPD_4D%LEV  = 0.0
              SPD_4D%LAT  = 0.0
              SPD_4D%LON  = 0.0
              SPD_4D%TIM  = 0.0
              SPD_4D%REFR = 0.0
              SPD_4D%STATUS = SPD__ALLO
              SPD_4D%MJD_0 = HEB_3D%MJD
              SPD_4D%TAI_0 = HEB_3D%TAI
         END IF
!
! ------ Load array of air refractivity into 3D section of
! ------ SPD_4D%REFR
!
         SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),IND_TIM,1:SPD_4D%DIMS(5)) = &
     &        HEB_3D%VAL(1:SPD_4D%DIMS(1)+SPD__MDEG,1:SPD_4D%DIMS(2)+SPD__MDEG,1:SPD_4D%DIMS(3)+SPD__MDEG,1:SPD_4D%DIMS(5))
         SPD_4D%TIM(IND_TIM) = (HEB_3D%MJD*86400.0D0 + HEB_3D%TAI) - &
     &                         (SPD_4D%MJD_0*86400.0D0 + SPD_4D%TAI_0)
 440  CONTINUE 
      DEALLOCATE ( C_FIL )
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'pd_2point reading data files: '//STR(1:27)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Build arrays of height (level)
!
      DO 450 J5=1,SPD_4D%DIMS(1)
         RLEV = J5-1 - (SPD__MLEV-1)/2
         SPD_4D%LEV(J5) = DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
 450  CONTINUE 
!
! --- Add arguments of extended knots for the height array
!
      DO 460 J6=0,1-SPD__MDEG,-1
         SPD_4D%LEV(J6) = SPD_4D%LEV(J6+1) - EPS_LEV1
 460  CONTINUE 
      DO 470 J7=SPD_4D%DIMS(1)+1,SPD_4D%DIMS(1)+SPD__MDEG
         SPD_4D%LEV(J7) = SPD_4D%LEV(J7-1) + EPS_LEV2
 470  CONTINUE 
!
! --- Build arrays of longitude
!
      DO 480 J8=1,SPD_4D%DIMS(2)
!
! ------ NB: the longitudinal grid has two more nodes: at 360deg and 360deg + 1step
!
         SPD_4D%LON(J8) = (J8-1)*PI2/(SPD_4D%DIMS(2) - 2)
 480  CONTINUE 
!
! --- Add arguments of extended knots for the longitude array
!
      DO 490 J9=0,1-SPD__MDEG,-1
         SPD_4D%LON(J9) = SPD_4D%LON(J9+1) - EPS_LON
 490  CONTINUE 
      DO 4100 J10=SPD_4D%DIMS(2)+1,SPD_4D%DIMS(2)+SPD__MDEG
         SPD_4D%LON(J10) = SPD_4D%LON(J10-1) + EPS_LON
 4100 CONTINUE 
!
! --- Biuld the latitude array
!
      DO 4110 J11=1,SPD_4D%DIMS(3)
         SPD_4D%LAT(J11) = -P2I + (J11-1)*PI__NUM/(SPD_4D%DIMS(3)-1) 
 4110 CONTINUE 
!
! --- Add arguments of extended knots for the lattidude array
!
      DO 4120 J12=0,1-SPD__MDEG,-1
         SPD_4D%LAT(J12) = SPD_4D%LAT(J12+1) - EPS_LAT
 4120 CONTINUE 
      DO 4130 J13=SPD_4D%DIMS(3)+1,SPD_4D%DIMS(3)+SPD__MDEG
         SPD_4D%LAT(J13) = SPD_4D%LAT(J13-1)  + EPS_LAT
 4130 CONTINUE 
!
! --- Add arguments of extended knots for the time array
!
      DO 4140 J14=0,1-SPD__MDEG,-1
         SPD_4D%TIM(J14) = SPD_4D%TIM(J14+1) - EPS_TIM
 4140 CONTINUE 
      DO 4150 J15=SPD_4D%DIMS(4)+1,SPD_4D%DIMS(4)+SPD__MDEG
         SPD_4D%TIM(J15) = SPD_4D%TIM(J15-1) + EPS_TIM
 4150 CONTINUE 
!
      IF ( PDRQ%TYP_IND(2) == 0 ) THEN
           NMOD = 1
         ELSE 
           NMOD = 2
      END IF
!
      DO 4160 J16=1,NMOD
!
! ------ Compuite 4D spline coefficients for each flavor of refractivity
!
         IUER = -1
         CALL BSPL4_4D_CMP ( SPD__MDEG, 0, SPD_4D%DIMS,    &
     &                       SPD_4D%LEV(1:SPD_4D%DIMS(1)), &
     &                       SPD_4D%LON(1:SPD_4D%DIMS(2)), &
     &                       SPD_4D%LAT(1:SPD_4D%DIMS(3)), &
     &                       SPD_4D%TIM(1:SPD_4D%DIMS(4)), &
     &                       SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1-SPD__MDEG:SPD_4D%DIMS(4),PDRQ%TYP_IND(J16)), &
     &                       IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL SPD_LOG ( REQ_ID, 2335, 'E', 'PD_PROC', 'Failure in an '// &
     &            'attempt to expand refractivity field into the '// &
     &            '4D B-spline basis' )
              CALL ERR_PASS ( 2335, IUER )
              DEALLOCATE ( SPD_2P )
              CALL SPD_4D_QUIT ( SPD_4D )
              RETURN
         END IF
 4160 CONTINUE 
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'pd_2point B-spline expansion: '//STR(1:27)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Compute path delay in point-to-point mode
!
      IUER = -1
      CALL COMP_PD_2POINT ( ISTL, NMOD, PDRQ%NUM_POI, SPD_4D, SPD_2P, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL SPD_LOG ( REQ_ID, 2336, 'E', 'PD_PROC', 'Failure in '// &
     &         'an attempt to compute path delays' )
           CALL ERR_PASS ( 2336, IUER )
           DEALLOCATE ( SPD_2P )
           CALL SPD_4D_QUIT ( SPD_4D )
           RETURN
      END IF
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'pd_2point path delay computation WALL time: '//STR(1:27)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Deallocate SPD_3D array
!
      CALL SPD_4D_QUIT ( SPD_4D )
!
! --- Cancel alarm
!
      IS = ALARM ( %VAL(0) )
!
! --- Send command "recvpdtb" -- take back your data
!
      SEND_COM%VERB = 'recvpdtb'
      SEND_COM%LEN  = PDRQ%NUM_POI*SIZEOF(SPD_2P(1))
      IS = SOCK_WRITE ( REM_FD, SIZEOF(SEND_COM), SEND_COM, MESSAGE )
      IF ( IS .NE. SIZEOF(SEND_COM) ) THEN
           CALL SPD_LOG ( REQ_ID, 2337, 'E', 'PD_PROC', 'Error in sending '// &
     &                   'command recvpdtb '//REM_IP_STR//' : '// &
     &                   MESSAGE(1:I_LEN(MESSAGE)) )
           DEALLOCATE ( SPD_2P )
           CALL ERR_PASS ( 2337, IUER )
           RETURN 
      END IF
!
! --- Send SPD_2P table with results
!
      IS = SOCK_WRITE ( REM_FD, PDRQ%NUM_POI*SIZEOF(SPD_2P(1)), SPD_2P, &
     &                  MESSAGE )
      IF ( IS .NE. PDRQ%NUM_POI*SIZEOF(SPD_2P(1)) ) THEN
           CALL SPD_LOG ( REQ_ID, 2338, 'E', 'PD_PROC', 'Error in sending '// &
     &                   'command to '//REM_IP_STR//' : '// &
     &                   MESSAGE(1:I_LEN(MESSAGE)) )
           CALL ERR_PASS ( 2338, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
!
! --- Wait for acknowledgement
!
      IS = SOCK_READ_POLL ( REM_FD, SIZEOF(RECV_COM), RECV_COM, SIZEOF(RECV_COM), &
     &                      SPD__READ_TIMEOUT, MESSAGE )
      IF ( INDEX ( MESSAGE, "Time out has expired" ) > 0 ) THEN
           CALL SPD_LOG ( REQ_ID, 2339, 'E', 'PD_PROC', &
     &         'spd_server did not get data from IP '//REM_IP_STR )
           CALL ERR_PASS ( 2339, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
        ELSE IF ( IS == -1 ) THEN
           CALL SPD_LOG ( REQ_ID, 2340, 'E', 'PD_PROC', 'Error in '// &
     &         'reading acknowledgement from '//REM_IP_STR// &
     &         ' : '//MESSAGE(1:I_LEN(MESSAGE)) )
           CALL ERR_PASS ( 2340, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
!
! --- Check whether that we have received is acknowledgement
!
      IF ( RECV_COM%VERB .NE. 'ack     ' ) THEN
           CALL SPD_LOG ( REQ_ID, 2341, 'E', 'PD_PROC', 'Error in '// &
     &         'reading acknowledgement from '//REM_IP_STR// &
     &         ' : received '//RECV_COM%VERB )
           CALL ERR_PASS ( 2341, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
!
! --- Write bye command -- good bye, my love, good bye!
!
      SEND_COM%VERB = 'bye     ' 
      SEND_COM%LEN  = 0
      IS = SOCK_WRITE ( REM_FD, SIZEOF(SEND_COM), SEND_COM, MESSAGE )
      IF ( IS .NE. SIZEOF(SEND_COM) ) THEN
           CALL SPD_LOG ( REQ_ID, 2342, 'E', 'PD_PROC', 'Error in '// &
     &         'sending command bye to '//REM_IP_STR//' : '// &
     &          MESSAGE(1:I_LEN(MESSAGE)) )
           CALL ERR_PASS ( 2342, IUER )
           DEALLOCATE ( SPD_2P )
           RETURN 
      END IF
      DEALLOCATE ( SPD_2P )
      CALL SPD_LOG ( REQ_ID, 2343, 'I', ' ', 'Successfully fulfilled pd request' )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PD_PROC !#!  
