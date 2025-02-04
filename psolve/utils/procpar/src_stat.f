      PROGRAM    SRC_STAT
! ************************************************************************
! *                                                                      *
! *   Program SRC_STAT
! *                                                                      *
! *  ### 11-JAN-2005    SRC_STAT   v1.2 (c)  L. Petrov  23-JUL-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'getpar.i'
      INTEGER*4  MBUF, MSOU, MSES
      PARAMETER  ( MBUF = 64*1024*1024 )
      PARAMETER  ( MSOU = 64*1024 )
      PARAMETER  ( MSES = 32*1024 )
      TYPE ( SOURCE_CAT__TYPE ) :: SOUCAT(MSOU)      
      CHARACTER  FILCAT*128, FILIN*128, FILOUT*128
      CHARACTER  C_SOU(MSOU)*10, C_SOU_SRT(MSOU)*10, CAT_SOU(MSOU)*128
      CHARACTER  B_SOU(MSOU)*10, C_SES(MSES)*10
      CHARACTER  STR*32, STR1*7, STR2*7, NAME*10
      CHARACTER, ALLOCATABLE :: BUF(:)*128, OUT(:)*128
      LOGICAL*4  FL_JNAME, FL_SRC_POST2021, FL_SRC_POST2024
      INTEGER*4  KOBS_LIM
      PARAMETER  ( KOBS_LIM = 1 )
      INTEGER*4, ALLOCATABLE :: K_SOU_IND(:,:)
      INTEGER*4  NBUF, J1, J2, J3, J4, K_SOU_COU(MSOU), K_SOU_SES(MSOU), &
     &           KOBS, KREC, L_SOU, LC_SOU, IS, ISES, &
     &           IND, LOUT, MODE, IC, L_SES, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
!@      FILCAT = '/vlbi/solutions/2005f_astro/2005f_astro_cat.txt'
!@      FILCAT = '/vlbi/solve/save_files/source.names'
!
      FL_JNAME = .FALSE.
!
      IF ( IARGC () < 3  ) THEN
           WRITE ( 6, * ) 'usage: src_stat <spool_file> <filcat> <output_file>'
           CALL EXIT ( 1 ) 
         ELSE  
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILCAT )
           CALL GETARG ( 3, FILOUT )
      END IF
      ALLOCATE ( K_SOU_IND(MSOU,MSES), BUF(MBUF), OUT(MBUF) )
!
      IUER = -1
      CALL RD_TEXT ( FILIN, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      IUER = -1
      CALL READ_SOU ( FILCAT, MSOU, LC_SOU, SOUCAT, CAT_SOU, MODE, IUER )
!
      CALL NOUT_I4 ( MSOU, K_SOU_COU )
      CALL NOUT_I4 ( MSOU, K_SOU_SES )
!
      L_SOU = 0
      L_SES = 0
      FL_SRC_POST2021 = .FALSE.
      DO 410 J1=1,NBUF
         IF ( INDEX ( BUF(J1), 'Listing_Options:' ) > 0 ) THEN
              IF ( INDEX ( BUF(J1), 'SRC_STAT POST2021' ) > 0 )  FL_SRC_POST2021 = .TRUE.
              IF ( INDEX ( BUF(J1), 'SRC_STAT POST2024' ) > 0 )  FL_SRC_POST2024 = .TRUE.
         END IF 
         IF ( BUF(J1)(1:9) .NE. 'SRC_STAT:' ) GOTO 410
!
         IF ( FL_SRC_POST2024 ) THEN
              ISES = LTM_DIF ( 1, L_SES, C_SES, BUF(J1)(74:83) )
            ELSE IF ( FL_SRC_POST2021 ) THEN
              ISES = LTM_DIF ( 1, L_SES, C_SES, BUF(J1)(65:74) )
            ELSE 
              ISES = LTM_DIF ( 1, L_SES, C_SES, BUF(J1)(50:59) )
         END IF
         IF ( ISES .LE. 0 ) THEN
              L_SES = L_SES + 1
              IF ( FL_SRC_POST2024 ) THEN
                   C_SES(L_SES) = BUF(J1)(74:83) 
                ELSE IF ( FL_SRC_POST2021 ) THEN
                   C_SES(L_SES) = BUF(J1)(65:74) 
                 ELSE
                   C_SES(L_SES) = BUF(J1)(50:59) 
              END IF
              ISES = L_SES
         END IF
!
         IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
              CALL CHIN ( BUF(J1)(33:38), KOBS )
              CALL CHIN ( BUF(J1)(40:44), KREC )
            ELSE
              CALL CHIN ( BUF(J1)(22:25), KOBS )
              CALL CHIN ( BUF(J1)(27:30), KREC )
         END IF
         IF ( KOBS .LT. KOBS_LIM ) GOTO 410
!
         NAME = BUF(J1)(12:19)//'  '
         IF ( FL_JNAME ) THEN
              IC = LTM_DIF ( 1, LC_SOU, CAT_SOU, NAME )
              IF ( IC .GT. 0 ) THEN
                   NAME = SOUCAT(IC)%J2000_NAME
                 ELSE 
                   NAME = 'B_'//BUF(J1)(12:19)
                   IF ( NAME == 'B_0218+357' ) NAME = 'J0221+355A'
                   IF ( NAME == 'B_1422+231' ) NAME = 'J1424+225A'
                   IF ( NAME == 'B_1830-21A' ) NAME = 'J1833-210A'
                   IF ( NAME == 'B_2015-274' ) NAME = 'J2015-013A'
                   IF ( NAME == 'B_2105+420' ) NAME = 'J2107+421A'
              END IF
         END IF
         IF ( FL_SRC_POST2024 ) THEN
              NAME = BUF(J1)(22:31)
         END IF
!
         IS = LTM_DIF ( 1, L_SOU, C_SOU, NAME )
         IF ( IS .LE. 0 ) THEN
              L_SOU = L_SOU + 1
              C_SOU(L_SOU) = NAME
              B_SOU(L_SOU) = BUF(J1)(12:19)
              IS = L_SOU
         END IF
!
         K_SOU_COU(IS) = K_SOU_COU(IS) + KOBS
         K_SOU_SES(IS) = K_SOU_SES(IS) + 1
         K_SOU_IND(IS,K_SOU_SES(IS)) = J1
 410  CONTINUE 
      WRITE ( 6, * ) 'L_SOU = ', L_SOU, ' L_SES=',L_SES
!
      CALL LIB$MOVC3 ( L_SOU*LEN(C_SOU(1)), C_SOU, C_SOU_SRT )
      CALL SORT_CH   ( L_SOU, C_SOU_SRT )
!
      LOUT = 1
      OUT(LOUT) = '# Statistics of sources observed in astro/geo VLBA sessions. Format version of 2024.07.23'
      LOUT = LOUT + 1
      OUT(LOUT) = '# Generated on '//GET_CDATE()
      LOUT = LOUT + 1
      CALL INCH ( L_SES, STR(1:5) ) 
      CALL CHASHR ( STR(1:5) )
      OUT(LOUT) = '# The total number of observing sessions: '//STR(1:5)
      LOUT = LOUT + 1
      OUT(LOUT) = '# Generated on '//GET_CDATE()
      CALL INCH ( L_SOU, STR(1:5) ) 
      CALL CHASHR ( STR(1:5) )
      OUT(LOUT) = '# The total number of observed sources:   '//STR(1:5)
      CALL CHASHR ( STR(1:5) )
      LOUT = LOUT + 1
      OUT(LOUT) = '# '
      LOUT = LOUT + 1
      OUT(LOUT) = '# Description:'
      LOUT = LOUT + 1
      OUT(LOUT) = '# '
      LOUT = LOUT + 1
      OUT(LOUT) = '#    SES records:'
      LOUT = LOUT + 1
      OUT(LOUT) = '#'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field  11:20  A10 IAU source name (when available)'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field  22:29  A8  IVS name'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field  41:45  I5  The total number of sessions with this source'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field  55:59  I5  The total number of observations of this source'
      LOUT = LOUT + 1
      OUT(LOUT) = '#'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    SOU records:'
      LOUT = LOUT + 1
      OUT(LOUT) = '#'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   11:20  A10    IAU source name (when available)'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   22:29  A8     IVS name'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   43:48  I6     The number of used observations'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   49:54  I6     The number of detected observations'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   56:60  I6     The number of correlated observations'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   63:72  F10.3  The rms fit of group delay in ps'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   75:77  I3     The number of used scans'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   79:81  I3     The number of correlated scans'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   84:93  A10    IVS database name'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field   96:103 A8     IVS session name'
      LOUT = LOUT + 1
      OUT(LOUT) = '#    Field  106:128  A19   Nominal middle date of the observing session'
      LOUT = LOUT + 1
      OUT(LOUT) = '#       '
!
      DO 420 J2=1,L_SOU
         LOUT = LOUT + 1
         OUT(LOUT) = '#--------'
         LOUT = LOUT + 1
         IND = LTM_DIF ( 0, L_SOU, C_SOU, C_SOU_SRT(J2) )
         CALL INCH ( K_SOU_SES(IND), STR1 )
         CALL INCH ( K_SOU_COU(IND), STR2 )
         CALL CHASHR ( STR1 )
         CALL CHASHR ( STR2 )
!
         OUT(LOUT) = 'SES_STAT: '//C_SOU(IND)//' '//B_SOU(IND)//'  '// &
     &               'Ses: '//STR1//'  Obs: '//STR2
         LOUT = LOUT + 1
         OUT(LOUT) = '#'
         DO 430 J3=1,K_SOU_SES(IND)
            LOUT = LOUT + 1
            OUT(LOUT) = 'SOU_STAT:          '//BUF(K_SOU_IND(IND,J3))(10:)
            IF ( FL_JNAME ) THEN
                 OUT(LOUT)(11:20) = C_SOU(IND)
            END IF
 430     CONTINUE 
 420  CONTINUE 
!
      IUER = -1
      CALL WR_TEXT ( LOUT, OUT, FILOUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      WRITE ( 6, * ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
      DEALLOCATE ( BUF )
      DEALLOCATE ( OUT )
      DEALLOCATE ( K_SOU_IND )
!
      END  PROGRAM   SRC_STAT  !#!#
