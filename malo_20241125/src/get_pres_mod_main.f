      PROGRAM GET_PRES_MOD_MAIN
! ************************************************************************
! *                                                                      *
! *   Program GET_PRES_MOD_MAIN computes atmospheric pressure using the  *
! *   output of numerical weather models for the provided dataset with   *
! *   dates 3D positions.                                                *
! *                                                                      *
! * ### 14-FEB-2018 GET_PRES_MOD_MAIN  v1.1 (c) L. Petrov 19-FEB-2018 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT     NONE 
      INCLUDE     'astro_constants.i'
      INCLUDE     'malo.i'
      INCLUDE     'heb.i'
      TYPE       ( MALO__TYPE ), POINTER :: MAL(:)
      TYPE       ( HEB__TYPE  )          :: HEB_G
      INTEGER*4    MEPC, MIND
      PARAMETER  ( MEPC = 16*1024*1024 )
      PARAMETER  ( MIND = 64  )
      REAL*8     POS_HLP(3,MEPC), TIM(MEPC), PRES_MEA(MEPC), PRES_MOD(MEPC)
      INTEGER*4  NEPC, J1, J2, J3, J4, IND(2,MIND), MJD(MEPC), LIND, IUER 
      INTEGER*4  MJD_BEG, MJD_END, NB, NE, ND, NO, IVRB, IND_BEG, IND_END
      REAL*8     TIM_BEG, TIM_END, TIM_INT
      REAL*8       EPS
      PARAMETER  ( EPS = 1.0D-6 )
      CHARACTER  FILP*128, FILO*128, DIR_HEB*128, FIL_IGH*128, &
     &           STR*128, STR1*128, STR2*128, YEAR*4
      CHARACTER  BUF(MEPC)*64, OUT(MEPC)*80
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A) ' ) 'Usage: get_pres_main year'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, YEAR )
      END IF
      FILP    = '/t0/pressure/pressure_2006_2017.txt'
      DIR_HEB = '/imsl/heb/geosfpit'
      FIL_IGH = '/progs/malo_20170618/share/geosfpit_height_above_geoid.heb'
      FILO    = '/tmp/pressure_'//YEAR//'.txt'
      FILO    = '/tmp/test3_pressure_'//YEAR//'.txt'
      TIM_INT = 3*3600.0D0
      IVRB    = 1
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 3801, -2, 'GET_PRES_MOD_MAIN', 'Error in an attempt '// &
     &         'to allocate memory for object MALO' )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 3802, -2, 'GET_PRES_MOD_MAIN', 'Error in an attempt '// &
     &         'to initialize MALO object' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILP, MEPC, BUF, NEPC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3803, IUER, 'GET_PRES_MOD_MAIN', 'Error in reading '// &
     &         'input measurement pressure file '//FILP )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 3804, IUER, 'GET_PRES_MOD_MAIN', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      NB = 1
      NE = NEPC
      MJD_BEG = -1
      IND_BEG = -1
!
      ND = 0 
      NO = 0
      DO 410 J1=1,NEPC
         IF ( BUF(J1)(1:4) .NE. YEAR ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(32)//CHAR(9), IUER )
         CALL DATE_TO_TIME ( BUF(J1)(IND(1,1):IND(2,1)), MJD(J1), TIM(J1), IUER )
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT=* ) POS_HLP(1,J1)
         READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT=* ) POS_HLP(2,J1)
         READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=* ) POS_HLP(3,J1)
         POS_HLP(2,J1) = POS_HLP(2,J1)*DEG__TO__RAD
         POS_HLP(3,J1) = POS_HLP(3,J1)*DEG__TO__RAD
         IF ( POS_HLP(2,J1) < 0.0 ) POS_HLP(2,J1) = POS_HLP(2,J1) + PI2
         READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT=* ) PRES_MEA(J1)
         IF ( MJD_BEG == -1 ) THEN
              MJD_BEG = MJD(J1)
              TIM_BEG = INT((TIM(J1) + EPS)/TIM_INT)*TIM_INT
              IF ( TIM_BEG .GE. 86400.0D0 - EPS ) THEN
                   TIM_BEG = TIM_BEG - 86400.0D0
                   MJD_BEG = MJD_BEG + 1
              END IF
              IND_BEG = J1
         END IF
         IF ( (MJD(J1) - MJD_BEG)*86400.0D0 + (TIM(J1) - TIM_BEG ) > TIM_INT ) THEN
              IND_END = J1 - 1
!
              TIM_END = TIM_BEG + TIM_INT
              IF ( TIM_END .GE. 86400.0D0 - EPS ) THEN
                   TIM_END = TIM_END - 86400.0D0
                   MJD_END = MJD_BEG + 1
                ELSE
                   MJD_END = MJD_BEG
              END IF
!
              IUER = -1
              STR1 = MJDSEC_TO_DATE ( MJD(IND_BEG), TIM(IND_BEG), IUER )
              IUER = -1
              STR2 = MJDSEC_TO_DATE ( MJD(IND_END), TIM(IND_END), IUER )
!
              IF ( IND_END .GE. IND_BEG .AND. IND_BEG > 0 ) THEN
                   IUER = -1
                   CALL GET_PRES_MOD ( IND_END - IND_BEG + 1, MJD(IND_BEG), &
     &                                 TIM(IND_BEG), POS_HLP(1,IND_BEG), &
     &                                 MJD_BEG, TIM_BEG, MJD_END, TIM_END, TIM_INT, &
     &                                 DIR_HEB, HEB_G, MAL, &
     &                                 PRES_MOD(IND_BEG), IVRB, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 3804, IUER, 'GET_PRES_MOD_MAIN', &
     &                                 'Failure to compute pressure model value '// &
     &                                 'for the range of epochs '//STR1(1:19)// &
     &                                 ' to '//STR2(1:19) )
                        CALL EXIT ( 1 )
                   END IF
              END IF
              DO 420 J2=IND_BEG,IND_END
                 NO = NO + 1
                 WRITE ( UNIT=OUT(NO), FMT=110 ) BUF(J2)(1:46), PRES_MEA(J2), 0.01*PRES_MOD(J2)
 110             FORMAT ( A, 2X, F6.1, 2X, F6.1 )
 420          CONTINUE 
              IND_BEG = J1
              MJD_BEG = MJD_END
              TIM_BEG = TIM_END
              ND = ND + 1
              IF ( MOD(ND,16) == 0 ) THEN
                   IUER = -1
                   CALL WR_TEXT ( NO, OUT, FILO, IUER )
                   WRITE ( 6, * ) 'Epoch '//STR1(1:19)//'  '//STR2(1:19)
                   WRITE ( 6, * ) '  Written into the output file '//TRIM(FILO)
              END IF
         END IF
 410  CONTINUE 
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FILO, IUER )
      WRITE ( 6, * ) 'Written into the output file '//TRIM(FILO)
!
      RETURN
      END  PROGRAM  GET_PRES_MOD_MAIN  !#!#  
