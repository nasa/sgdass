      PROGRAM    PLOSER_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL PLOSER()
      END  PROGRAM  PLOSER_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLOSER()
! ************************************************************************
! *                                                                      *
! *   Program  PLOSER is for plotting station and/or source position     *
! *   evolution extracted from the spool file and saved in *.lso file.   *
! *                                                                      *
! *   It is assumed that the spool file has been parsed by GETPAR and    *
! *   the outpuit files xxxxx.lso and xxxxxx.lst have been created.      *
! *                                                                      *
! *   PLOSER reads xxxxx.lso and xxxxxx.lst files, creates the list of   *
! *   sources and stations and then asks users to hit the station or     *
! *   source name. Evolution XYZ of UEN coordinates of the station and   *
! *   evolution of right ascension and declination of the source is      *
! *   plotted by DiaGi program.                                          *
! *                                                                      *
! *   Estimates of station positions and/or source position with formal  *
! *   error exceeding the specified level are discarded.                 *
! *                                                                      *
! *   A summary about all sources and stations estimated in local mode   *
! *   is written in the output file.                                     *
! *                                                                      *
! *   Usage: ploser <solution_file> [<min_sess>]                         *
! *                                                                      *
! *   Where solution file is the name WITHOUT extension of the source    *
! *   and station files. PLOSER addes extension .lso and .lst and looks  *
! *   for these file.                                                    *
! *                                                                      *
! *   <min_sess> -- the minimal number of sessions in which the object   *
! *   (station or source) should participate. If the source (or station) *
! *   were observed in less than min_sess sessions, then that object     *
! *   will not be displayed. Default: min_sess=1 (all obsjects are       *
! *   displayed).                                                        *
! *                                                                      *
! *   F.e. ploser /tmp/cri33  will cause PLOSER to read files            *
! *   /tmp/cri33.lso (for source positions) and /tmp/cri33.lst           *
! *   (for station positions).                                           *
! *                                                                      *
! *    NB: ploser filters out bad points. It does not plot the point     *
! *        with very large formal uncertainty. Values of the uncertinty  *
! *        limits are set as parameters in the beginning of the program. *
! *                                                                      *
! *  ###  19-AUG-98     PLOSER    v3.0 (c)  L. Petrov  30-OCT-2004 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAX_ARC_SRC, MAX_ARC_STA, MAX_OBS
!@      PARAMETER  ( MAX_ARC_SRC = 768 )
      PARAMETER  ( MAX_ARC_SRC = 2048 )
      PARAMETER  ( MAX_ARC_STA = 160 )
      PARAMETER  ( MAX_OBS     = 2 )
      INCLUDE    'diagi.i'
      INCLUDE    'diagi_local.i'
      INCLUDE    'obser.i'
      TYPE      ( DIAGI_STRU    ) :: DIAGI_S
      TYPE      ( DBOBJ_O__STRU ) :: DBOBJ
      INTEGER*4  MBSOU, MBSTA, MWSOU, MWSTA, M_STA, M_SOU, M_SES
      PARAMETER  ( MWSOU=160, MWSTA=200, M_STA=216, M_SOU=4096, M_SES=4096 )
      PARAMETER  ( MBSOU=64*1024, MBSTA = 64*1024 )
      CHARACTER  BUFSOU(MBSOU)*(MWSOU), BUFSTA(MBSTA)*(MWSTA), &
     &           FISOU*80, FISTA*80, FISUM*80, C_STA(M_STA)*8, C_SOU(M_SOU)*8
      CHARACTER  OBJ(M_SOU)*8, MES*80, STR*80, STR1*80, SOLNAM*80
      INTEGER*4  K_STA(M_STA),  K_SOU(M_SOU), L_STA, L_SOU
      REAL*8     YB_STA(M_STA), YE_STA(M_STA), YB_SOU(M_SOU), YE_SOU(M_SOU)
      REAL*8     YYB_STA(M_STA), YYE_STA(M_STA), YYB_SOU(M_SOU), YYE_SOU(M_SOU)
      REAL*8     DR, SH, WRMS, RAD, RAD_0, VAL, VAL_0, FILL, TIME_SPAN
      REAL*8     T8_ARR(M_SES), X8_ARR(M_SES), E8_ARR(M_SES), W8_ARR(M_SES), &
     &           X2_ARR(M_SES), E2_ARR(M_SES), TR_ARR(2), XR_ARR(2)
      REAL*8       PI, PI2, P2I, RAD_MAS
      PARAMETER  ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) !
      PARAMETER  ( RAD_MAS = 180.D0*3600.D0*1.D3/PI )
      PARAMETER  ( FILL     = 0.02  )
      INTEGER*4  IL, J1, J2, J3, J4, J5, J6, J7, J8, J9, K1, K2, IP, ITP, NP, &
     &           IUER, IHR, IGR, IMN, NBSOU, NBSTA, NUMARG, M_BUT
      PARAMETER  ( M_BUT = 7 )
      CHARACTER  B_SGN*1, BUTTON_LET(M_BUT)*1, BUTTON_NAME(M_BUT)*24
      CHARACTER  GET_VERSION*54, PLOSER_VERS*54
      INTEGER*4  ISR, ISR_LAST, ISTA_LAST, ISTA, IER, NZ, LD, ICMP, IOS, NN, &
     &           N_STA, N_SOU, MIN_SES
      INTEGER*4  LTM_DIF, SELSTA_PLUS, I_LEN, LINDEX
      LOGICAL*4  LSEL_STA(M_STA), LSEL_SOU(M_SOU), FSTA, FSOU, FLAG, LEX
      REAL*8     SEC, DECL, MAX_RA, MAX_DC, MAX_PS, HW_GAUSS_FLT, HW__MIN
      PARAMETER  ( HW__MIN = 1.D-5 )
      LOGICAL*4  FL_BW
      DATA       ( BUTTON_LET(NN), BUTTON_NAME(NN), NN=1,M_BUT ) &
     &           / &
     &             ' ', '                        ', &
     &             '1', 'Station positions XYZ   ', &
     &             '2', 'Station positions UEN   ', &
     &             '3', 'Source coordinates      ', &
     &             'X', 'Exit                    ', &
     &             ' ', '                        ', &
     &             ' ', '                        ' &
     &           /
!
      PARAMETER  ( FL_BW  = .FALSE. ) 
      PARAMETER  ( MAX_RA =   5.0 ) ! max sigma for right_ascen*cos(delta) (mas)
      PARAMETER  ( MAX_DC =   5.0 ) ! max sigma for declination            (mas)
      PARAMETER  ( MAX_PS = 200.0 ) ! max sigma for station position       (mm)
      INTEGER*4  IARGC
!
      INCLUDE  'ploser_version.i'
      PLOSER_VERS = GET_VERSION()
!
      CALL SET_SIGNAL_CTRLC ( 1 )
      CALL CLRCH ( FISUM )
      FISUM = '/tmp/ploser.sum'
!
! --- Get arguments (of supplied)
!
      CALL CLRCH ( SOLNAM )
      NUMARG = IARGC ()
      IF ( NUMARG .GE. 1 ) THEN
           CALL GETARG ( 1, SOLNAM )
      END IF
      IF ( NUMARG .GE. 2 ) THEN
           CALL GETARG ( 2, STR )
           CALL CHIN   ( STR, MIN_SES )
         ELSE
           MIN_SES = 1
      END IF
      IF ( NUMARG .GE. 3 ) THEN
           CALL GETARG ( 3, STR )
           READ ( UNIT=STR, FMT='(F20.10)' ) HW_GAUSS_FLT
         ELSE
           HW_GAUSS_FLT = 0.0D0
      END IF
!
 910  CONTINUE
      CALL CLRCH ( FISOU  )
      CALL CLRCH ( FISTA  )
!
      IF ( NUMARG .LT. 1 ) THEN
           CALL CLRCH ( SOLNAM )
!
! -------- Request the filename
!
           WRITE ( 6, 100 )  PLOSER_VERS(1:I_LEN(PLOSER_VERS))
 100       FORMAT ( 1X,A,2X,' solution file? '$ )
           READ ( UNIT=5, FMT='(A)' ) SOLNAM
      END IF
!
! --- If user specified the file with extension we cut off the extension
!
      IP = LINDEX ( SOLNAM, '.' )
      IF ( IP .GT. 1 ) THEN
           CALL CLRCH ( SOLNAM(IP:) )
      END IF
!
! --- Form the input file for source positions and check whether the source
! --- file exists
!
      FISOU = SOLNAM(1:I_LEN(SOLNAM))//'.lso'
      INQUIRE ( FILE=FISOU, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           WRITE ( 6, * ) '$$$   Input file '//FISOU(1:I_LEN(FISOU))// &
     &            ' was not found   $$$'
           NUMARG = 0
           GOTO 910
      END IF
!
! --- Form the input file for station positions and check whether the source
! --- file exists
!
      FISTA = SOLNAM(1:I_LEN(SOLNAM))//'.lst'
      INQUIRE ( FILE=FISTA, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           WRITE ( 6, * ) '$$$   Input file '//FISTA(1:I_LEN(FISTA))// &
     &            ' was not found   $$$'
           NUMARG = 0
           GOTO 910
      END IF
!
! --- Reading input files. Information is put in buffers
!
      WRITE ( 6, * ) ' Reading '//FISOU(1:I_LEN(FISOU))//' ...'
      CALL RD_TEXT ( FISOU, MBSOU, BUFSOU, NBSOU, -3 )
      WRITE ( 6, * ) ' File '//FISOU(1:I_LEN(FISOU))//' has been read. ',NBSOU,' lines.'
!
      WRITE ( 6, * ) ' Reading '//FISTA(1:I_LEN(FISTA))//' ...'
      CALL RD_TEXT ( FISTA, MBSTA, BUFSTA, NBSTA, -3 )
      WRITE ( 6, * ) ' File '//FISTA(1:I_LEN(FISTA))//' has been read',NBSTA,' lines.'
!
! --- Parsing source file for gathering information about sources.
! --- We form list of source names, time range of observations, number of
! --- sessions which they participated in.
!
      L_SOU = 0
      DO 410 J1=1,NBSOU
         IF ( BUFSOU(J1)(1:7) .NE. 'SOU_LCO' ) GOTO 410
!
         IL = LTM_DIF ( 1, L_SOU, C_SOU, BUFSOU(J1)(11:18) )
         IF ( IL .LE. 0) THEN
              L_SOU = L_SOU + 1
              C_SOU(L_SOU) = BUFSOU(J1)(11:18)
              IL = L_SOU
              K_SOU(IL) = 0
              READ ( UNIT=BUFSOU(J1)(47:56), FMT='(F10.5)' ) YB_SOU(IL)
         END IF
!
! ------ Array YB_SOU contains the data of the first observation of the source.
! ------ Array YE_SOU contains the data of the last  observation of the source.
!
         K_SOU(IL) = K_SOU(IL) + 1
         READ ( UNIT=BUFSOU(J1)(47:56), FMT='(F10.5)' ) YE_SOU(IL)
 410  CONTINUE
!
! --- Parsing station file for gathering information about stations.
! --- We form list of station names, time range of observations, number of
! --- sessions which they participated in
!
      L_STA = 0
      DO 420 J2=1,NBSTA
         IF ( BUFSTA(J2)(1:7) .NE. 'STA_LCX' ) GOTO 420
!
         IF ( L_STA .GT. 0 ) THEN
              IL = LTM_DIF ( 1, L_STA, C_STA, BUFSTA(J2)(11:18) )
            ELSE
              IL = 0
         END IF
         IF ( IL .LE. 0) THEN
              L_STA = L_STA + 1
              C_STA(L_STA) = BUFSTA(J2)(11:18)
              IL = L_STA
              K_STA(IL) = 0
              READ ( UNIT=BUFSTA(J2)(47:56), FMT='(F10.5)' ) YB_STA(IL)
         END IF
!
! ------ Array YB_STA contains the data of the first observation at the station
! ------ Array YE_STA contains the data of the last  observation at the station
!
         K_STA(IL) = K_STA(IL) + 1
         READ ( UNIT=BUFSTA(J2)(47:56), FMT='(F10.5)' ) YE_STA(IL)
 420  CONTINUE
!
! --- Sorting stations. Sorted array of station names, number of sessions in
! --- which the stations have been participated are put in the fields of the
! --- data structure DBOBJ.
!
      DBOBJ%L_STA = L_STA
      DBOBJ%U_STA = L_STA
      DO 430 J3=1,L_STA
         OBJ(J3) = C_STA(J3)
 430  CONTINUE
      CALL SORT_CH ( L_STA, OBJ )
!
      IF ( L_STA .GT. MO_STA ) THEN
           WRITE ( 6, * ) ' L_STA=',L_STA,' MO_STA=',MO_STA
           WRITE ( 6, '(A)' ) 'The number of stations exceeded the limit'
           CALL EXIT ( 1 )
      END IF
!
      N_STA = L_STA
      L_STA = 0
      DO 440 J4=1,N_STA
         IP = LTM_DIF ( 1, N_STA, C_STA, OBJ(J4) )
         IF ( K_STA(IP) .GE. MIN_SES ) THEN
              L_STA = L_STA + 1
              DBOBJ%LIS_STA(L_STA) = L_STA
              DBOBJ%UIS_STA(L_STA) = L_STA
              DBOBJ%C_STA(L_STA)   = C_STA(IP)
              DBOBJ%KU_STA(L_STA)  = K_STA(IP)
              LSEL_STA(L_STA)      = .FALSE.
              YYB_STA(L_STA)       = YB_STA(IP)
              YYE_STA(L_STA)       = YE_STA(IP)
         END IF
 440  CONTINUE
!
! --- Sorting sources. Sorted array of source names, number of sessions in
! --- which the sources have been observed are put in the fields of the data
! --- structure DBOBJ.
!
      DBOBJ%L_SOU = L_SOU
      DBOBJ%U_SOU = L_SOU
      DO 450 J5=1,L_SOU
         OBJ(J5) = C_SOU(J5)
 450  CONTINUE
      CALL SORT_CH ( L_SOU, OBJ )
      N_SOU = L_SOU
      L_SOU = 0
      DO 460 J6=1,N_SOU
         IP = LTM_DIF ( 1, N_SOU, C_SOU, OBJ(J6) )
         IF ( K_SOU(IP) .GT. MIN_SES ) THEN
              L_SOU = L_SOU + 1
              DBOBJ%LIS_SOU(L_SOU) = L_SOU
              DBOBJ%UIS_SOU(L_SOU) = L_SOU
              DBOBJ%C_SOU(L_SOU)   = C_SOU(IP)
              DBOBJ%KU_SOU(L_SOU)  = K_SOU(IP)
              LSEL_SOU(L_SOU)      = .FALSE.
              YYB_SOU(L_SOU)       = YB_SOU(IP)
              YYE_SOU(L_SOU)       = YE_SOU(IP)
         END IF
 460  CONTINUE
!
! --- Write down summary
!
      OPEN ( UNIT=16, FILE=FISUM, STATUS='UNKNOWN' )
!
      WRITE ( 16, '(A)' ) 'Solution '//SOLNAM(1:I_LEN(SOLNAM))
      WRITE ( 16, '(A)' ) '---------------------------------------'// &
     &                    '---------------------------------------'
!
! --- First about stations
!
      DO 510 K1=1,L_STA
         IP = DBOBJ%LIS_STA(K1)
         WRITE ( 16, 110 ) K1, DBOBJ%C_STA(K1), DBOBJ%KU_STA(K1), &
     &                     YYB_STA(IP), YYE_STA(IP), YYE_STA(IP)-YYB_STA(IP)
 110     FORMAT ( 1X,'STA = ',I3,' >>',A,'<<  K_STA = ',I4,'  YEAR = ',F11.6, &
     &               ', ',F11.6,' | ',F5.2 )
 510  CONTINUE
!
      WRITE ( 16, '(A)' ) '---------------------------------------'// &
     &                    '---------------------------------------'
!
! --- Then about sources
!
      DO 520 K2=1,L_SOU
         IP = DBOBJ%LIS_SOU(K2)
         WRITE ( 16, 120 ) K2, DBOBJ%C_SOU(K2), DBOBJ%KU_SOU(K2), &
     &                     YYB_SOU(IP), YYE_SOU(IP), YYE_SOU(IP)-YYB_SOU(IP)
 120     FORMAT ( 1X,'SOU = ',I3,' >>',A,'<<  K_SOU = ',I4,'  YEAR = ',F11.6, &
     &               ', ',F11.6,' | ',F5.2 )
 520  CONTINUE
!
      WRITE ( 16, '(A)' ) '---------------------------------------'// &
     &                    '---------------------------------------'
!
      CLOSE ( UNIT=16 )
      WRITE ( 6, * ) ' Summary is written in the file '//FISUM(1:I_LEN(FISUM))
      WRITE ( 6, * ) ' '
!
! --- Dialogue with user
!
      ISR_LAST  = 0
      ISTA_LAST = 0
 920  CONTINUE
      IUER = -1
      ITP = 0
      CALL MULTI_DIAGI ( PLOSER_VERS(1:I_LEN(PLOSER_VERS))//'                '// &
     &                   'Select plot type', 0, 0, 0, ' ', M_BUT, BUTTON_NAME, &
     &                    BUTTON_LET, '/tmp/', %VAL(0), ITP, IUER )
      ITP = ITP-1
      CALL PGENDQ()
      IF ( ITP .LE. 0  .OR.  ITP .GE. 4 ) CALL EXIT ( 0 )
!      type *,' icode =',icode
!      type *,' sta_xyz/sta_ren/sou/exit  1/2/3/4 '
!      ACCEPT *,ITP
      IF ( ITP .EQ. 1  .OR.  ITP .EQ. 2 ) THEN
           FSTA = .TRUE.
           FSOU = .FALSE.
        ELSE IF ( ITP .EQ. 3 ) THEN
           FSTA = .FALSE.
           FSOU = .TRUE.
        ELSE
           STOP 'God bye!'
      END IF
!
! --- Loop of the object selection
!
 930  CONTINUE
!
! ------ Selection
!
         IF ( FSOU ) THEN
!
! ----------- Source selection
!
              CALL CLRCH  ( MES )
              MES = 'Source selection'
              IER = -1
              CALL SELSOU_PLUS ( MES, DBOBJ, 1, LSEL_SOU, ISR_LAST, ISR, IER )
              IF ( ISR .GT. 0 ) THEN
                   ISR_LAST = ISR
                   LSEL_SOU(ISR) = .TRUE.
              END IF
              IF ( ISR .EQ. 0 ) GOTO 920
              ICMP = 2
!!              type *,' isr = ',isr
           ELSE IF ( FSTA ) THEN
!
! ----------- Station selection
!
              CALL CLRCH ( MES )
              MES = 'Station selection'
              FLAG = SELSTA_PLUS ( MES, DBOBJ, ISTA_LAST, ISTA, -3 )
              IF ( FLAG ) THEN
                   ISTA_LAST = ISTA
                 ELSE
                   ISTA = 0
              END IF
              IF ( ISTA .EQ. 0 ) GOTO 920
              ICMP = 3
!!              type *,' ista = ',ista
         END IF
!
! ------ Cicle on the coordinate components
!
         DO 470 J7=1,ICMP
            NP = 0
!
! --------- Station
!
            IF ( FSTA ) THEN
!
! ------------ Scan buffer of station coordinate and gather information about
! ------------ station positions in the arrays T8_ARR, E8_ARR, W8_ARR
!
               DO 480 J8=1,NBSTA
                  IF ( J7 .EQ. 1                                       .AND. &
     &               BUFSTA(J8)(11:18) .EQ. DBOBJ%C_STA(ISTA)          .AND. &
     &               ( ITP .EQ. 1 .AND. BUFSTA(J8)(1:7) .EQ. 'STA_LCX' .OR. &
     &               ITP .EQ. 2 .AND. BUFSTA(J8)(1:7) .EQ. 'STA_LCU'  )   ) THEN
!
! ------------------ It is the line with station position of the selected
! ------------------ station: XYZ and UEN coordinates
!
                     NP = NP+1
                     READ ( UNIT=BUFSTA(J8)(47:56), FMT='(F10.5)' ) T8_ARR(NP)
                     READ ( UNIT=BUFSTA(J8)(62:76), FMT='(F5.7)' ) VAL
                     IF ( NP .EQ. 1 ) THEN
                          VAL_0 = VAL
                     END IF
                     X8_ARR(NP) = (VAL-VAL_0)
                     READ ( UNIT=BUFSTA(J8)(81:90), FMT='(F10.5)' ) E8_ARR(NP)
                     W8_ARR(NP) = 1.D0/E8_ARR(NP)
!
! ------------------ Filter out estimates with very high formal uncertainty
!
                     IF ( DABS ( E8_ARR(NP) ) .GT. MAX_PS ) NP = NP -1
                   ELSE IF ( J7 .EQ. 2                                 .AND. &
     &               BUFSTA(J8)(11:18) .EQ. DBOBJ%C_STA(ISTA)          .AND. &
     &               ( ITP .EQ. 1 .AND. BUFSTA(J8)(1:7) .EQ. 'STA_LCX' .OR. &
     &                 ITP .EQ. 2 .AND. BUFSTA(J8)(1:7) .EQ. 'STA_LCU' )  ) THEN
!
! ------------------ It is the line with station position of the selected
! ------------------ station: XYZ and UEN coordinates
!
                     NP = NP+1
                     READ ( UNIT=BUFSTA(J8)(47:56),  FMT='(F10.5)' ) T8_ARR(NP)
                     READ ( UNIT=BUFSTA(J8)(96:110), FMT='(F15.5)' ) VAL
                     IF ( NP .EQ. 1 ) THEN
                          VAL_0 = VAL
                     END IF
                     X8_ARR(NP) = (VAL-VAL_0)
                     READ ( UNIT=BUFSTA(J8)(115:124), FMT='(F10.5)' ) E8_ARR(NP)
                     W8_ARR(NP) = 1.D0/E8_ARR(NP)
!
! ------------------ Filter out estimates with very high formal uncertainty
!
                     IF ( DABS ( E8_ARR(NP) ) .GT. MAX_PS ) NP = NP -1
                   ELSE IF ( J7 .EQ. 3                                 .AND. &
     &               BUFSTA(J8)(11:18) .EQ. DBOBJ%C_STA(ISTA)          .AND. &
     &               ( ITP .EQ. 1 .AND. BUFSTA(J8)(1:7) .EQ. 'STA_LCX' .OR. &
     &                 ITP .EQ. 2 .AND. BUFSTA(J8)(1:7) .EQ. 'STA_LCU' )  ) THEN
!
! ------------------ It is the line with station position of the selected
! ------------------ station: XYZ and UEN coordinates
!
                     NP = NP+1
                     READ ( UNIT=BUFSTA(J8)(47:56),   FMT='(F10.5)' ) T8_ARR(NP)
                     READ ( UNIT=BUFSTA(J8)(130:144), FMT='(F15.5)' ) VAL
                     IF ( NP .EQ. 1 ) THEN
                     VAL_0 = VAL
                     END IF
                     X8_ARR(NP) = (VAL-VAL_0)
                     READ ( UNIT=BUFSTA(J8)(149:158), FMT='(F10.5)' ) E8_ARR(NP)
                     W8_ARR(NP) = 1.D0/E8_ARR(NP)
!
! ------------------ Filter out estimates with very high formal uncertainty
!
                     IF ( DABS ( E8_ARR(NP) ) .GT. MAX_PS ) NP = NP -1
                  END IF
 480          CONTINUE
            END IF ! FSTA
!
! --------- Source
!
            IF ( FSOU ) THEN
!
! ------------ Scan buffer of source coordinate and gather information about
! ------------ source positions in the arrays T8_ARR, E8_ARR, W8_ARR
!
               DO 490 J9=1,NBSOU
                  IF ( J7 .EQ. 1                               .AND. &
     &                 BUFSOU(J9)(11:18) .EQ. DBOBJ%C_SOU(ISR)       ) THEN
!
! ----------------- It is the line with source position of the selected
! ----------------- source. Let's get right ascension.
!
                    NP = NP+1
                    READ ( UNIT=BUFSOU(J9)(47:56), FMT='(F10.5)' ) T8_ARR(NP)
                    READ ( UNIT=BUFSOU(J9)(63:79), FMT='(I2,1X,I2,1X,F11.8)' ) &
     &                     IHR, IMN, SEC
                    RAD = (IHR + IMN/60.0D0 + SEC/3600.D0 )*PI/12.D0
                    IF ( NP .EQ. 1 ) THEN
                         RAD_0 = RAD
                    END IF
                    READ ( UNIT=BUFSOU(J9)(84:93), FMT='(F10.5)', IOSTAT=IOS ) VAL
                    IF ( IOS .NE. 0 ) VAL = 99999.9
                    X8_ARR(NP) = (RAD-RAD_0)*RAD_MAS
                    E8_ARR(NP) = VAL
                    W8_ARR(NP) = 1.D0/E8_ARR(NP)
                    READ ( UNIT=BUFSOU(J9)(100:116), &
     &                     FMT='(A1,I2,1X,I2,1X,F11.8)' ) B_SGN, IGR, IMN, SEC
                    DECL = (IGR + IMN/60.0D0 + SEC/3600.D0 )*PI/180.D0
!
! ----------------- Filter out data with uncertainty exceeding the limit
!
                    IF ( DABS ( E8_ARR(NP)*DCOS(DECL) ) .GT. MAX_RA ) NP = NP -1
                  ELSE IF ( J7 .EQ. 2                               .AND. &
     &                      BUFSOU(J9)(11:18) .EQ. DBOBJ%C_SOU(ISR)       ) THEN
!
! ----------------- It is the line with source position of the selected
! ----------------- source. Let's get declination
!
                    NP = NP+1
                    READ ( UNIT=BUFSOU(J9)(47:56), FMT='(F10.5)' ) T8_ARR(NP)
                    READ ( UNIT=BUFSOU(J9)(100:116), &
     &                     FMT='(A1,I2,1X,I2,1X,F11.8)' ) B_SGN, IGR, IMN, SEC
                    RAD = (IGR + IMN/60.0D0 + SEC/3600.D0 )*PI/180.D0
                    IF ( B_SGN .EQ. '-' ) RAD = -RAD
                    IF ( NP .EQ. 1 ) THEN
                         RAD_0 = RAD
                    END IF
                    READ ( UNIT=BUFSOU(J9)(121:130), FMT='(F10.5)', IOSTAT=IOS ) VAL
                    IF ( IOS .NE. 0 ) VAL = 99999.9
                    X8_ARR(NP) = (RAD-RAD_0)*RAD_MAS
                    E8_ARR(NP) = VAL
                    W8_ARR(NP) = 1.D0/E8_ARR(NP)
!
! ----------------- Filter out data with uncertainty exceeding the limit
!
                    IF ( DABS ( E8_ARR(NP) ) .GT. MAX_DC ) NP = NP -1
                END IF
 490          CONTINUE
            END IF ! FSOU
!
            IF ( NP .LE. 0 ) THEN
                 CALL PAUSE ( 'No data' )
                 GOTO 930
            END IF
!
            WRMS = 0.0D0
!
! --------- Calculation of the parameters of the linear trend for
! --------- residuals to be plotted
!
            CALL REGRW8    ( NP, T8_ARR, X8_ARR, W8_ARR, %VAL(0), DR, SH, -3 )
            TR_ARR(1) = T8_ARR(1)
            TR_ARR(2) = T8_ARR(NP)
            XR_ARR(1) = 0.0
            XR_ARR(2) = DR*(T8_ARR(NP)-T8_ARR(1))
            IF ( FSOU ) THEN
                 XR_ARR(1) = XR_ARR(1) - DR*( 2000.0D0 - T8_ARR(1) )
                 XR_ARR(2) = XR_ARR(2) - DR*( 2000.0D0 - T8_ARR(1) )
                 CALL SHIFT_ARR ( NP, T8_ARR, X8_ARR, SH, DR )
            END IF
            IF ( HW_GAUSS_FLT .GT. HW__MIN ) THEN
                 CALL SORT83  ( NP, T8_ARR, X8_ARR, E8_ARR )
                 CALL GAUSS_FILTER ( NP, HW_GAUSS_FLT, T8_ARR, X8_ARR, E8_ARR, &
     &                               X2_ARR, E2_ARR ) 
            END IF
!
! --------- Calculation of wrms of the residuals with respect to the
! --------- linear trend
!
            CALL DISP_WTR8 ( NP, T8_ARR, X8_ARR, W8_ARR, DR, SH, %VAL(0), WRMS, &
     &                       NZ, -3 )
!
! --------- Calcilation the index of the selected baseline in the
! --------- list of used baselines
!
            WRITE ( UNIT=STR, FMT='(F11.3)' ) WRMS
            CALL CHASHL ( STR )
            IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
!
! --------- Cleaning all fields of the data strucutre DIAGI_S
!
            LD = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
            CALL NOUT ( LD, DIAGI_S%IFIRST_FIELD )
            CALL CLRCH ( DIAGI_S%ZAG )
!
            CALL CLRCH ( STR1 )
            CALL GETENVAR ( 'DIAGI_SCREEN', STR1 )
            CALL TRAN   ( 11, STR1, STR1 )
            DIAGI_S%IDEV = IXS__DEF
            IF ( STR1(1:5) .EQ. 'SMALL' ) THEN
                 DIAGI_S%IDEV = 2
              ELSE IF ( STR1(1:3) .EQ. 'BIG' ) THEN
                 DIAGI_S%IDEV = 1
            END IF
!
! --------- Make a title of the plot in according the the type of the data
!
            IF ( FSTA .AND.  ITP .EQ.1  .AND.  J7 .EQ. 1 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_STA(ISTA)//' X-coordinate '// &
     &                        ' wrms='//STR(1:I_LEN(STR))//' mm'
                ELSE IF ( FSTA .AND.  ITP .EQ.1  .AND.  J7 .EQ. 2 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_STA(ISTA)//' Y-coordinate '// &
     &                        ' wrms='//STR(1:I_LEN(STR))//' mm'
                ELSE IF ( FSTA .AND.  ITP .EQ.1  .AND.  J7 .EQ. 3 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_STA(ISTA)//' Z-coordinate '// &
     &                        ' wrms='//STR(1:I_LEN(STR))//' mm'
                ELSE IF ( FSTA .AND.  ITP .EQ.2  .AND.  J7 .EQ. 1 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_STA(ISTA)//' U-coordinate '// &
     &                        ' wrms='//STR(1:I_LEN(STR))//' mm'
                ELSE IF ( FSTA .AND.  ITP .EQ.2  .AND.  J7 .EQ. 2 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_STA(ISTA)//' E-coordinate '// &
     &                        ' wrms='//STR(1:I_LEN(STR))//' mm'
                ELSE IF ( FSTA .AND.  ITP .EQ.2  .AND.  J7 .EQ. 3 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_STA(ISTA)//' N-coordinate '// &
     &                        ' wrms='//STR(1:I_LEN(STR))//' mm'
                ELSE IF ( FSOU .AND.  J7 .EQ. 1 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_SOU(ISR)//' Right ascension '// &
     &                        ' wrms='//STR(1:I_LEN(STR))//' mas'
                ELSE IF ( FSOU .AND. J7 .EQ. 2 ) THEN
                 DIAGI_S%ZAG = DBOBJ%C_SOU(ISR)//' Declination '// &
     &                         ' wrms='//STR(1:I_LEN(STR))//' mas'
            END IF
!
! --------- Filling other fields of the DIAGI data strucutre
!
            IF ( HW_GAUSS_FLT .GT. HW__MIN ) THEN
                 DIAGI_S%NCLR      = 3
               ELSE 
                 DIAGI_S%NCLR      = 2
            END IF
            DIAGI_S%ICLR      = 1
!
            DIAGI_S%NPOI(1)   = NP
            DIAGI_S%ADR_X8(1) = LOC(T8_ARR)
            DIAGI_S%ADR_Y8(1) = LOC(X8_ARR)
            DIAGI_S%ADR_E8(1) = LOC(E8_ARR)
            DIAGI_S%LER (1)   = .TRUE.
            DIAGI_S%ICOL(1)   = 1
            IF ( FL_BW ) DIAGI_S%ICOL(1) = 14
            DIAGI_S%IOST(1)   = 1
            DIAGI_S%IPST(1)   = 5
            DIAGI_S%IWST(1)   = 1
            DIAGI_S%IBST(1)   = 2
            IF ( FSOU  .AND.  HW_GAUSS_FLT .GT. HW__MIN ) THEN
                 DIAGI_S%IPST(1)   = 4
                 DIAGI_S%IBST(1)   = 2
            END IF
            DIAGI_S%ILST(1)   = 1
!
            DIAGI_S%NPOI(2)   = 2
            DIAGI_S%ADR_X8(2) = LOC(TR_ARR)
            DIAGI_S%ADR_Y8(2) = LOC(XR_ARR)
            DIAGI_S%ADR_E8(2) = 0
            DIAGI_S%LER (2)   = .FALSE.
            DIAGI_S%ICOL(2)   = 14
            DIAGI_S%IOST(2)   = 1
            DIAGI_S%IPST(2)   = 1
            DIAGI_S%IWST(2)   = 1
            DIAGI_S%IBST(2)   = 0
            DIAGI_S%ILST(2)   = 2
            IF ( HW_GAUSS_FLT .GT. HW__MIN ) THEN
                 DIAGI_S%NPOI(3)   = NP
                 DIAGI_S%ADR_X8(3) = LOC(T8_ARR)
                 DIAGI_S%ADR_Y8(3) = LOC(X2_ARR)
                 DIAGI_S%ADR_E8(3) = LOC(E2_ARR)
                 DIAGI_S%LER (3)   = .TRUE.
                 DIAGI_S%ICOL(3)   = 9
                 IF ( FL_BW ) DIAGI_S%ICOL(3) = 13
                 DIAGI_S%IOST(3)   = 0
                 DIAGI_S%IPST(3)   = 4
                 DIAGI_S%IWST(3)   = 3
                 DIAGI_S%IBST(3)   = 2
                 DIAGI_S%IBST(3)   = 4
                 IF ( FL_BW ) DIAGI_S%IBST(3) = 0
                 DIAGI_S%ILST(3)   = 3
            END IF
!
            TIME_SPAN    = ( T8_ARR(NP) - T8_ARR(1) )
            DIAGI_S%XMIN = T8_ARR(1)    - FILL*TIME_SPAN
            DIAGI_S%XMAX = T8_ARR(NP)   + FILL*TIME_SPAN
!
            DIAGI_S%YMIN =  1.0  ! To force Diagi to calculate
            DIAGI_S%YMAX = -1.0  ! boundaries anew
!
            DIAGI_S%YMIN = -10.0  !
            DIAGI_S%YMAX =  10.0  !
!
            DIAGI_S%ITRM      = 0
            DIAGI_S%STATUS    = DIA__DEF
!
! --------- IV. Calling the main routine of DiaGI for plot
!
            IER = -1
            CALL DIAGI     ( DIAGI_S, IER )
 470     CONTINUE
      GOTO 930
!
! --- That is all, guys!
!
      END  SUBROUTINE  PLOSER  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SHIFT_ARR ( NP, T8_ARR, X8_ARR, SH, DR )
      IMPLICIT   NONE 
      INTEGER*4  NP
      REAL*8     T8_ARR(NP), X8_ARR(NP), SH, DR
      REAL*8     SHIFT
      INTEGER*4  J1
!
      SHIFT = DR*( 2000.0D0 - T8_ARR(1) )
      DO 410 J1=1,NP
         X8_ARR(J1) = X8_ARR(J1) - SHIFT - SH
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  SHIFT_ARR
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAUSS_FILTER ( M, HW, DAT, ARR1, SIG1, ARR2, SIG2 )
      IMPLICIT   NONE 
      INTEGER*4  M
      REAL*8     HW, DAT(M), ARR1(M), SIG1(M), ARR2(M), SIG2(M)
      REAL*8     WIN, WIN_SUM, EXP_VAR
      INTEGER*4  J1, J2
!
      DO 410 J1=1,M
         WIN_SUM    = 0.0D0
         ARR2(J1)   = 0.0D0
         SIG2(J1)   = 0.0D0
         DO 420 J2=1,M
            EXP_VAR = -( (DAT(J2) - DAT(J1))/HW )**2
            IF ( EXP_VAR .LT. -40.0D0 ) EXP_VAR = -40.0D0
            WIN_SUM = WIN_SUM + DEXP(EXP_VAR)
            ARR2(J1) = ARR2(J1) + ARR1(J2)*DEXP(EXP_VAR)
            SIG2(J1) = SIG2(J1) + (SIG1(J2)*DEXP(EXP_VAR))**2
 420     CONTINUE 
         ARR2(J1) = ARR2(J1)/WIN_SUM
         SIG2(J1) = DSQRT ( SIG2(J1) )/WIN_SUM
 410  CONTINUE 
!      
      RETURN
      END  SUBROUTINE  GAUSS_FILTER 
