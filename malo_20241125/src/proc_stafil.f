      PROGRAM    PROC_STAFIL
! ************************************************************************
! *                                                                      *
! *   Program PROC_STAFIL
! *                                                                      *
! * ## 03-NOV-2014   PROC_STAFIL   v1.0 (c)  L. Petrov  03-NOV-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4    MLON, MLAT, MSTA, MIND
      PARAMETER  ( MLON = 43200 )
      PARAMETER  ( MLAT = 21600 )
      PARAMETER  ( MSTA = 8192  )
      PARAMETER  ( MIND = 256   )
      INTEGER*2  HEI_ARR(MLON,MLAT)
      TYPE     ( HEB__TYPE    ) :: HEB_GEOID_BSPL
      CHARACTER  FIL_STA*128, GTOPO_FIL*128, GEOID_BSPL_FIL*128, FILOUT*128
      CHARACTER  MODE_STR*32, STR*128, BUF(MSTA)*128, OUT(MSTA)*512, MODE*128
      CHARACTER  REG*3, STA_NAM*9, CUSTOMER_NAME*128, SITE_NAME*128
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//'@' ) 
      REAL*8     LAT, LON, GEH, HEI_OH, COO(3), LON_STEP, LAT_STEP
      REAL*8     EXC_SQR_WGS84
      PARAMETER  ( EXC_SQR_WGS84 = 2.0D0*FLAT__WGS84 - FLAT__WGS84**2 )
      INTEGER*4  J1, J2, J3, J4, J5, LUN, IND(2,MIND), IR, LIND, NB, NO, &
     &           IND_LAT, IND_LON, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ
      REAL*8,    EXTERNAL :: GET_GEOID
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, * ) 'Usage: proc_stafil filin mode gtopo_file '// &
     &                    'geoid_bspl_fil out_fil'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FIL_STA )
           CALL GETARG ( 2, MODE_STR  )
           CALL GETARG ( 3, GTOPO_FIL )
           CALL GETARG ( 4, GEOID_BSPL_FIL )
           CALL GETARG ( 5, FILOUT )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_STA, MSTA, BUF, NB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4301, IUER, 'PROC_STAFIL', 'Failure in '// &
     &         'an attempt to read input station file '//FIL_STA )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL BINF_OPEN ( GTOPO_FIL, 'OLD', LUN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           IUER = -2
           CALL ERR_LOG ( 4303, IUER, 'PROC_STAFIL', 'Failure in '// &
     &         'an attempt to open the input file '// &
     &          GTOPO_FIL(1:I_LEN(GTOPO_FIL))//' -- '//STR )
           CALL EXIT ( 1 )
      END IF
!
      IR = READ ( %VAL(LUN), HEI_ARR, %VAL(2*MLON*MLAT) )
      IF ( IR .NE. 2*MLON*MLAT ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           IUER = -2
           CALL ERR_LOG ( 4304, -2, 'PROC_STAFIL', 'Failure in '// &
     &         'an attempt to read contents of data file from '//GTOPO_FIL )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL BINF_CLOSE ( LUN, IUER )
!
      LAT_STEP = PI__NUM/MLAT
      LON_STEP = PI2/MLON
!
      NO = 0
      NO = NO + 1; OUT(NO) = '# Station file  GE-01  Format version of 2014.11.03'
      NO = NO + 1; OUT(NO) = '# '
      NO = NO + 1; OUT(NO) = '# Last modificaiton date: '//GET_CDATE()
      NO = NO + 1; OUT(NO) = '# '
      CALL CLRCH ( STR )
      CALL INCH  ( NB, STR )
      NO = NO + 1; OUT(NO) = '# Total number of stations: '//STR(1:I_LEN(STR))
      NO = NO + 1; OUT(NO) = '#'
      NO = NO + 1; OUT(NO) = '# Station          X          Y          Z     Latitude  Longitude H_ort   H_ell  Custmoer name                                         Site name            '
      NO = NO + 1; OUT(NO) = '#                  m          m          m       deg       deg       m       m  '
      NO = NO + 1; OUT(NO) = '#'
      DO 410 J1=1,NB
         IF ( ILEN(BUF(J1)) .LT.  1  ) GOTO 410 ! skip empty line
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410 ! skip comment line
!
         IUER = -1
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IUER )
         IF ( LIND < 5 ) THEN
              CALL CLRCH   ( STR )
              CALL INCH    ( J1, STR )
              IUER = -2
              CALL ERR_LOG ( 4306, -2, 'PROC_STAFIL', 'Error in parsing '// &
     &            ' the input file: '//FIL_STA(1:I_LEN(FIL_STA))// &
     &            ' -- too few words on line '//STR )
              CALL EXIT ( 1 )
         END IF
         STA_NAM = BUF(J1)(IND(1,1):IND(2,1))
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F30.7)' ) LAT
         READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F30.7)' ) LON
         LAT = LAT*DEG__TO__RAD
         LON = LON*DEG__TO__RAD
         IF ( LON < 0.0 ) LON = LON + PI2
         CUSTOMER_NAME = BUF(J1)(IND(1,4):IND(2,4)) 
         CALL CHASHL ( CUSTOMER_NAME )
         SITE_NAME = BUF(J1)(IND(1,5):IND(2,5)) 
         CALL CHASHL ( SITE_NAME )
!
         IUER = -1
         GEH = GET_GEOID ( LAT, LON, GEOID_BSPL_FIL, HEB_GEOID_BSPL, IUER )
         IF ( IUER .NE. 0  ) THEN
              CALL CLRCH   ( STR )
              CALL INCH    ( J1, STR )
              IUER = -2
              CALL ERR_LOG ( 4307, IUER, 'PROC_STAFIL', 'Error in '// &
     &            'computing geoid height above the reference ellipsoid' )
              CALL EXIT ( 1 )
         END IF
!
         IND_LAT = NINT ( (LAT+P2I)/LAT_STEP ) + 1
         IND_LON = NINT ( LON/LON_STEP ) + 1
         HEI_OH = HEI_ARR(IND_LON,IND_LAT)
         IF ( HEI_OH < -9000 ) HEI_OH = 0.0D0
!
         COO(1) = (REA__WGS84/DSQRT(1.D0 - EXC_SQR_WGS84*DCOS(LAT)**2) + HEI_OH + GEH)* &
     &            DCOS(LON)*DCOS(LAT)
         COO(2) = (REA__WGS84/DSQRT(1.D0 - EXC_SQR_WGS84*DCOS(LAT)**2) + HEI_OH + GEH)* &
     &            DSIN(LON)*DCOS(LAT)
         COO(3) = (REA__WGS84*(1.D0 - EXC_SQR_WGS84)/DSQRT(1.D0 - EXC_SQR_WGS84*DCOS(LAT)**2) &
     &                        + HEI_OH + GEH)*DSIN(LAT)
         NO = NO +  1
         WRITE ( UNIT=OUT(NO), FMT=110 ) STA_NAM, COO, LAT/DEG__TO__RAD, LON/DEG__TO__RAD, &
     &                                   HEI_OH, HEI_OH + GEH, CUSTOMER_NAME(1:52), SITE_NAME(1:32)
 110     FORMAT ( A, 2X, 3(F10.1, 1X), 2X, F9.5, 1X, F10.5, 1X, F6.1, 1X, F6.1, &
     &            2X, A, 2X, A )
 410  CONTINUE 
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FILOUT, IUER )
      IF ( IUER .NE. 0  ) THEN
           IUER = -1
           CALL ERR_LOG ( 4308, IUER, 'PROC_STAFIL', 'Error in '// &
     &         'an attempt to write in the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  PROC_STAFIL  !#!  
