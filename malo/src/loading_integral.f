      PROGRAM    LOADING_INTEGRAL_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL LOADING_INTEGRAL()
      END  PROGRAM  LOADING_INTEGRAL_LAUNCH !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LOADING_INTEGRAL()
! ************************************************************************
! *                                                                      *
! *   Program LOADING_INTEGRAL reads an input file with loading          *
! *   displacements in netCDF format, input land-sea mask and            *
! *   it comptues several average quanities:                             *
! *   1) Average up dispalcements over the whole Earth.                  *
! *   2) Average up dispalcements over the land.                         *
! *   3) Average up dispalcements over the sea.                          *
! *   4) Average up dispalcements over the sea at |latitudes| < 66deg.   *
! *                                                                      *
! * ### 04-FEB-2015 LOADING_INTEGRAL v2.1 (c) L. Petrov  13-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LS
      CHARACTER  FILIN*128, FILLS*128, FILOUT*128, WAV_NAM(MALO__MWAV)*4, &
     &           STR*128, WORD(8)*6, FILTMP*128, INTERNET_HOSTNAME*64, &
     &           SYSNAME*128, HARDWARE*128, TMP_DIR*128, COM*1024
      INTEGER*4  MODE, NLON, NLAT, IVEC, ICMP, IFRQ, MJD, J1, J2, J3, &
     &           IS, ID, IL, NTHR, IUER
      INTEGER*8  MEL
      LOGICAL*1  FL_BZIP2
      REAL*8     TAI, INTG(8), LAT, AREA
      REAL*4,    ALLOCATABLE, TARGET :: DSP_ARR(:) 
      REAL*4,    POINTER     :: DSPL(:,:,:)
      REAL*4     VAL_MIN, VAL_MAX
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, OMP_GET_THREAD_NUM, SYSTEM
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: loading_integral mode nc-file fls-mask '
           CALL EXIT ( 1 )
        ELSE 
           CALL GETARG ( 1, STR    )
           CALL CHIN   ( STR, MODE )
           IF ( MODE == 1 ) THEN
                CONTINUE 
              ELSE 
                CALL ERR_LOG ( 6201, IUER, 'LOADING_INTEGRAL', 'Wrong mode: '// &
     &               STR(1:I_LEN(STR))//' only mode 1 is supported' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 2, FILIN )
           CALL GETARG ( 3, FILLS )
      END IF      
      IL = ILEN(FILIN)
      IF ( IL < 8 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6202, IUER, 'LOADING_INTEGRAL', 'Name of '// &
     &         'the input filfe '//TRIM(FILIN)//' is too short' )
           CALL EXIT ( 1 )
      END IF
      IF ( FILIN(IL-6:IL) == '.nc.bz2' ) THEN
           CALL GETINFO_HOST ( INTERNET_HOSTNAME )
           IF ( INTERNET_HOSTNAME == 'localhost' ) THEN
                CALL GETINFO_SYSTEM ( SYSNAME, INTERNET_HOSTNAME, HARDWARE )
           END IF
           ID = LINDEX ( FILIN, '/' ) 
           IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &          INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &          INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &          INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &          INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'        ) THEN
                TMP_DIR = '/dev/shm'
             ELSE IF ( INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a' ) THEN
                TMP_DIR = '/imls/oper_temp'
             ELSE 
                TMP_DIR = '/tmp'
           END IF
           FILTMP = TRIM(TMP_DIR)//'/'//FILIN(ID:IL-7)//'.nc'
!
! -------- Honor environemnet variable OMP_NUM_THREADS.
! -------- We limit the number of threads for lbzip2
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL CHIN ( STR, NTHR )
                IF ( NTHR < 1 ) NTHR = 1
                CALL CLRCH ( STR ) 
                CALL INCH  ( NTHR, STR )
                STR = '-n '//STR
              ELSE
!
! ------------- ... or do not use any limit when the variable is not set up
!
                CALL CLRCH ( STR )
           END IF
!
           IF ( OMP_IN_PARALLEL() ) THEN
                COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
              ELSE 
                COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dsfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
           END IF
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
!
! ------------- lbzip2 may fail because of "Cannot allocate memory". As a desperate
! ------------- attempt we try once more with using bzip2 and with only one thread 
! ------------- and a small block size
!
                CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
                COM = 'bzip2 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
                IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           END IF
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6203, IUER, 'LOADING_INTEGRAL', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                CALL EXIT ( 1 )
           END IF
           FL_BZIP2 = .TRUE.
         ELSE IF ( FILIN(IL-2:IL) == '.nc' ) THEN
           FILTMP = FILIN
           FL_BZIP2 = .FALSE.
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6204, IUER, 'LOADING_INTEGRAL', 'Name of '// &
     &         'the input filfe '//TRIM(FILIN)//' does not have '// &
     &         'extension .nc or .nc.bz2' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read land-sea mask
!
      IUER = -1
      CALL READ_HEB ( FILLS, HEB_LS, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6202, IUER, 'LOADING_INTEGRAL', 'Error in '// &
     &         'an attempt to read heb file with land-sea '// &
     &         'mask '//FILLS )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
           CALL EXIT ( 1 )
      END IF   
!
! --- Alocate memory for displacementrs
!
      MEL = INT8(3)*HEB_LS%DIMS(1)*HEB_LS%DIMS(2)
      ALLOCATE ( DSP_ARR(MEL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*MEL, STR )
           IUER = -1
           CALL ERR_LOG ( 6203, IUER, 'LOADING_INTEGRAL', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory' )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
           CALL EXIT ( 1 )
      END IF
!
! --- Read array of loadint displacemetnd into the one-dimensional array DSP_ARR
!
      IUER = -1
      CALL READ_LOADING_NC ( FILTMP, MEL, NLON, NLAT, IVEC, ICMP, &
     &                       IFRQ, MJD, TAI, DSP_ARR, WAV_NAM, IUER )
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6204, IUER, 'LOADING_INTEGRAL', 'Error in '// &
     &         'reading input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
      IF ( NLON == HEB_LS%DIMS(1) .OR. NLAT == HEB_LS%DIMS(2) ) THEN
           CONTINUE 
         ELSE
           WRITE ( 6, * ) ' NLON,NLAT= ', NLON, NLAT
           WRITE ( 6, * ) ' HEB_LS%DIM= ', HEB_LS%DIMS
           IUER = -1
           CALL ERR_LOG ( 6205, IUER, 'LOADING_INTEGRAL', 'Dimensions of '// &
     &         'loading file ans land/water mask file do not match' )
           CALL EXIT ( 1 )
      END IF
!
! --- Reshape this array into the 3D array DSPL
!
      CALL C_F_POINTER ( C_LOC(DSP_ARR), DSPL, [NLON,NLAT,3] )
!@      FILOUT = '/tmp/boo'
!@      IUER = -1
!@      DSPL = 1000.0*DSPL
!@      VAL_MIN =  1.0
!@      VAL_MAX = -1.0
!@      CALL PLOT_GRID_R4 ( 1, 7, 37, 1, NLON, NLAT, DSPL(1,1,1), &
!@     &                    'Up load', 'mm', VAL_MIN, VAL_MAX, FILOUT, IUER )
!
! --- Compute integrals over the Earth or some portion of the Earth
!
      INTG = 0 
      DO 410 J1=1,NLAT
         LAT = -P2I + (J1-1)*PI__NUM/(NLAT-1)
         DO 420 J2=1,NLON
            AREA = (PI2/NLON)*(PI__NUM/(NLAT-1))*DCOS(LAT)
            INTG(1) = INTG(1) + DSPL(J2,J1,1)*AREA
            INTG(5) = INTG(5) +               AREA
!
            INTG(2) = INTG(2) + DSPL(J2,J1,1)*AREA*HEB_LS%VAL(J2,J1,1,1)
            INTG(6) = INTG(6) +               AREA*HEB_LS%VAL(J2,J1,1,1)
!
            INTG(3) = INTG(3) + DSPL(J2,J1,1)*AREA*(1.0 - HEB_LS%VAL(J2,J1,1,1))
            INTG(7) = INTG(7) +               AREA*(1.0 - HEB_LS%VAL(J2,J1,1,1))
!
            IF ( DABS(LAT) .LE. 66.0D0*DEG__TO__RAD ) THEN
                 INTG(4) = INTG(4) + DSPL(J2,J1,1)*AREA*(1.0 - HEB_LS%VAL(J2,J1,1,1))
                 INTG(8) = INTG(8) +               AREA*(1.0 - HEB_LS%VAL(J2,J1,1,1))
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
! --- Print the output string
!
      IUER = -1
      STR = MJDSEC_TO_DATE ( MJD, TAI, IUER )
      DO 430 J3=1,4
         WRITE ( UNIT=WORD(J3), FMT='(F6.3)' ) 1000.0D0*INTG(J3)/INTG(J3+4)
         IF ( INDEX ( WORD(J3), '*' ) > 0 ) THEN
              WRITE ( UNIT=WORD(J3), FMT='(F6.2)' ) 1000.0D0*INTG(J3)/INTG(J3+4)
         END IF
 430  CONTINUE 
      WRITE ( 6, 110 ) STR(1:19), ((MJD - J2000__MJD)*86400.0D0 + &
     &                             (TAI - 43200))/YEAR__TO__SEC + 2000.0D0, &
     &                             WORD(1:4)
 110  FORMAT ( 'Date: ', A, 1X, F13.8, &
     &         ' Whole_dspl: ',     A, &
     &         ' Land_dspl: ',      A, &
     &         ' Sea_dspl: ',       A, &
     &         ' Sea_66deg_dspl: ', A, ' mm' )
!
      END  SUBROUTINE  LOADING_INTEGRAL  !#!#
