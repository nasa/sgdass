      PROGRAM CHECK_STA_LIST
! ************************************************************************
! *                                                                      *
! *   PRogram CHECK_STA_LIST
! *                                                                      *
! * ### 03-JUL-2015  CHECK_STA_LIST   v1.0 (c) L. Petrov 03-JUL-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER  MALO_STA_LIST*128, TRIAL_STA_LIST*128
      CHARACTER  MALO_STA_BUF(MALO__MSTA)*128, TRIAL_STA_BUF(MALO__MSTA)*128
      CHARACTER  D_STA(MALO__MSTA)*8, M_STA(MALO__MSTA)*16
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      CHARACTER  STR*128
      INTEGER*4  MIND
      PARAMETER  ( MIND  = 32 )
      REAL*8     EDGE_SEC, COO(3), PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC 
      REAL*8     DIST, DIST_MIN
      INTEGER*4  N_STA, N_TRI, LIND, IND(2,MIND), IOS(3), IND_STA_MIN, &
     &           L_STA, J1, J2, J3, IUER
      REAL*8,    EXTERNAL :: DP_VV_V 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
!      MALO_STA_LIST  = '/progs/malo_20150601/share/loading.sta'
!      TRIAL_STA_LIST = '/tmp/g.1'
      EDGE_SEC = 1.0D0
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: check_sta_list  sta_fil [loading_sta_file]'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, TRIAL_STA_LIST )
           IF ( IARGC() > 1 ) THEN
                CALL GETARG ( 2, MALO_STA_LIST )
              ELSE 
                MALO_STA_LIST  = 'loading.sta'
           END IF 
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( MALO_STA_LIST, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4301, IUER, 'CHECK_STA_LIST', 'Cannot find '// &
     &         'MALO station list for pre-computed loading '//MALO_STA_LIST )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( MALO_STA_LIST, MALO__MSTA, MALO_STA_BUF, N_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4302, IUER, 'CHECK_STA_LIST', 'Cannot find '// &
     &         'MALO station list for pre-computed loading '//MALO_STA_LIST )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( TRIAL_STA_LIST, MALO__MSTA, TRIAL_STA_BUF, N_TRI, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4303, IUER, 'CHECK_STA_LIST', 'Cannot find '// &
     &         'MALO station list for pre-computed loading '//TRIAL_STA_LIST )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4304, IUER, 'CHECK_STA_LIST', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4305, IUER, 'CHECK_STA_LIST', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INP_STA ( MAL(1), MALO_STA_LIST, EDGE_SEC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4306, IUER, 'CHECK_STA_LIST', 'Failure in '// &
     &         'an attempt to load station file '// &
     &          MALO_STA_LIST )
           CALL EXIT ( 1 )
      END IF
!
      L_STA = 0
      DO 410 J1=1,N_TRI
         IF ( TRIAL_STA_BUF(J1)(1:1) == '#' ) GOTO 410
         IF ( TRIAL_STA_BUF(J1)(1:14) == 'SITLIST Format' ) GOTO 410
         CALL EXWORD ( TRIAL_STA_BUF(J1), MIND, LIND, IND, &
     &                 CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( LIND < 4 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4307, IUER, 'CHECK_STA_LIST', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'th line of the '// &
     &            'input file '//TRIAL_STA_LIST(1:I_LEN(TRIAL_STA_LIST))// &
     &            ' -- the line should have at least four words' )
              CALL EXIT ( 1 )
         END IF
         L_STA = L_STA + 1
         D_STA(L_STA) = TRIAL_STA_BUF(J1)(IND(1,1):IND(2,1))
!
! ------ Read station coordinates
!
         READ ( UNIT=TRIAL_STA_BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.3)', &
     &          IOSTAT=IOS(1) ) COO(1)
         READ ( UNIT=TRIAL_STA_BUF(J1)(IND(1,3):IND(2,3)), FMT='(F12.3)', &
     &          IOSTAT=IOS(2) ) COO(2)
         READ ( UNIT=TRIAL_STA_BUF(J1)(IND(1,4):IND(2,4)), FMT='(F12.3)', &
     &          IOSTAT=IOS(3) ) COO(3)
         IF ( IOS(1) .NE. 0  .OR.  IOS(2) .NE.0  .OR.  IOS(3) .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4308, IUER, 'CHECK_STA_LIST', 'Error during '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the input '// &
     &            'file '//TRIAL_STA_LIST(1:I_LEN(TRIAL_STA_LIST))// &
     &            ' -- wrong format of station coordinates' )
              CALL EXIT ( 1 )
         END IF
         CALL REF_ELL ( 1, COO, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC )
         IF ( H_ELL .LT. MALO__HEIGHT_MIN ) THEN
              M_STA(L_STA) = 'BELOW_GEOID'
              GOTO 410
         END IF
         IF ( H_ELL .GT. MALO__HEIGHT_MAX ) THEN
              M_STA(L_STA) = 'ABOVE_GEOID'
              GOTO 410
         END IF
!
         CALL CLRCH ( M_STA(L_STA) )
         M_STA(L_STA) = 'NOT_FOUND'
         DIST_MIN = 3.0D0*REA__WGS84
         IND_STA_MIN = 1
         DO 420 J2=1,MAL(1)%NSTA
            DIST = DSQRT ( (COO(1) - MAL(1)%STA(J2)%COO(1))**2 + &
     &                     (COO(2) - MAL(1)%STA(J2)%COO(2))**2 + & 
     &                     (COO(3) - MAL(1)%STA(J2)%COO(3))**2   &
     &                   )
            IF ( DIST < DIST_MIN ) THEN
                 DIST_MIN = DIST
                 IND_STA_MIN  = J2
            END IF
  420    CONTINUE 
         IF ( DIST_MIN < MALO__RD_AREA ) THEN
              M_STA(L_STA) = MAL(1)%STA(IND_STA_MIN)%NAME(1:8)
         END IF
  410 CONTINUE 
!
      DO 430 J3=1,L_STA
         WRITE ( 6, 110 ) D_STA(J3), M_STA(J3)
 110     FORMAT ( A, 2X, A )
 430  CONTINUE 
!      
      END  PROGRAM CHECK_STA_LIST  !#!  
