      PROGRAM    GTOPO30_ASSEMBLE
! ************************************************************************
! *                                                                      *
! *   Program GTOPO30_ASSEMBLE reads original tiles with GTOPO30 model,  *
! *   assemble themin one lat-long array and writes this array into      *
! *   the output file.                                                   *
! *                                                                      *
! * ### 04-OCT-2012  GTOPO30_ASSEMBLE v1.0 (c) L. Petrov 05-OCT-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N_G30, NLON, NLAT, M_HDR
      PARAMETER  ( N_G30 = 33 )
      PARAMETER  ( NLON = 43200 )
      PARAMETER  ( NLAT = 21600 )
      PARAMETER  ( M_HDR = 64   )
      CHARACTER  DIR_G30*128, FILNAM*128, FIL_DEM(N_G30)*128, &
     &           FIL_HDR(N_G30)*128, FILOUT*128, C_HDR(M_HDR)*128, STR*128
      INTEGER*8  DIR_DESC(32)
      REAL*8     LON_MIN, LAT_MAX, LAT_STEP, LON_STEP
      INTEGER*2  HEI_GLO(NLON,NLAT)
      INTEGER*4  IS, LEV, J1, J2, J3, J4, J5, J6, LD, LH, L_HDR, &
     &           KLON, KLAT, FILL_VALUE, IND_LAT, IND_LON, LUN, IUER
      LOGICAL*1  LEX
      INTEGER*8  IR, IW
      INTEGER*2, ALLOCATABLE :: HEI_DEM(:,:)
      INTEGER*8, EXTERNAL :: READ, WRITE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR 
!
      DIR_G30 = '/g1/gtopo30'
      FILOUT  = DIR_G30(1:I_LEN(DIR_G30))//'/gtopo30.dat'
!
      LEV = 0
      LD  = 0
      LH  = 0
!
      DO 410 J1=1,1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_G30, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 4101, -2, 'GTOPO30_ASSEMBLE', 'Error in '// &
     &           'reading input directory '//DIR_G30(1:I_LEN(DIR_G30))// &
     &           '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, 'ANT' ) > 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '.DEM' ) > 0 ) THEN
              LD = LD + 1 
              FIL_DEM(LD) = FILNAM
         END IF
         IF ( INDEX ( FILNAM, '.HDR' ) > 0 ) THEN
              LH = LH + 1 
              FIL_HDR(LH) = FILNAM
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      CALL SORT_CH ( LH, FIL_HDR )
      CALL SORT_CH ( LD, FIL_DEM )
!
      write ( 6, * ) ' lh= ', lh, ' ld= ', ld ! %%%
      HEI_GLO = 0
      DO 420 J2=1,LD
!         WRITE ( 6, 110 ) J2, FIL_DEM(J2)(1:I_LEN(FIL_DEM(J2))), &
!     &                        FIL_HDR(J2)(1:I_LEN(FIL_HDR(J2)))
! 110     FORMAT ( 'I2= ',I3, ' Fd: ', A , ' Fh: ', A ) 
!
         IUER = -1
         CALL RD_TEXT ( FIL_HDR(J2), M_HDR, C_HDR, L_HDR, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 4103, -2, 'GTOPO30_ASSEMBLE', 'Error in '// &
     &           'attempt to read input header file '//FIL_HDR(J2)  )
              CALL EXIT ( 1 )
         END IF
         DO 430 J3=1,L_HDR
            IF ( C_HDR(J3)(1:5) == 'NROWS' ) THEN
                 READ ( UNIT=C_HDR(J3)(13:18), FMT='(I6)' ) KLAT
               ELSE IF ( C_HDR(J3)(1:5) == 'NCOLS' ) THEN
                 READ ( UNIT=C_HDR(J3)(13:18), FMT='(I6)' ) KLON
               ELSE IF ( C_HDR(J3)(1:6) == 'NODATA' ) THEN
                 READ ( UNIT=C_HDR(J3)(13:19), FMT='(I7)' ) FILL_VALUE
               ELSE IF ( C_HDR(J3)(1:6) == 'ULXMAP' ) THEN
                 READ ( UNIT=C_HDR(J3)(13:32), FMT='(F20.16)' ) LON_MIN
               ELSE IF ( C_HDR(J3)(1:6) == 'ULYMAP' ) THEN
                 READ ( UNIT=C_HDR(J3)(13:32), FMT='(F20.16)' ) LAT_MAX
               ELSE IF ( C_HDR(J3)(1:4) == 'XDIM' ) THEN
                 READ ( UNIT=C_HDR(J3)(13:32), FMT='(F20.16)' ) LON_STEP
               ELSE IF ( C_HDR(J3)(1:4) == 'YDIM' ) THEN
                 READ ( UNIT=C_HDR(J3)(13:32), FMT='(F20.16)' ) LAT_STEP
            END IF
 430     CONTINUE 
         LON_MIN = IDNINT ( (LON_MIN - LON_STEP/2)*1000)/1000
         LAT_MAX = IDNINT ( (LAT_MAX + LAT_STEP/2)*1000)/1000
!
         ALLOCATE ( HEI_DEM(KLON,KLAT), STAT=IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 2*KLON*KLAT, STR )
              CALL ERR_LOG ( 4104, -2, 'GTOPO30_ASSEMBLE', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes dynamic '// &
     &            'memory' )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL BINF_OPEN ( FIL_DEM(J2), 'OLD', LUN, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 4105, -2, 'GTOPO30_ASSEMBLE', 'Failure in an attempt '// &
     &            'to open the input file '//FIL_DEM(J2)(1:I_LEN(FIL_DEM(J2)))// &
     &            ' -- '//STR )
              CALL EXIT ( 1 )
         END IF
!
         IR = READ ( %VAL(LUN), HEI_DEM, %VAL(2*KLON*KLAT) )
         IF ( IR .NE. 2*KLON*KLAT ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 4106, -2, 'GTOPO30_ASSEMBLE', 'Failure in an attempt '// &
     &           'to read contensts of data file '//FIL_DEM(J2) )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL BINF_CLOSE ( LUN, IUER )
!
         IF ( INDEX ( FIL_DEM(J2), 'E120S60.DEM' ) > 0 ) THEN
!
! ----------- Fix a bug in E120S60.DEM: two columns near 180deg meridian 
! ----------- wee see, which is wrong
!
              HEI_DEM(KLON,1:KLAT)   = HEI_DEM(KLON-2,1:KLAT) 
              HEI_DEM(KLON-1,1:KLAT) = HEI_DEM(KLON-2,1:KLAT) 
         END IF
!         
!         WRITE ( 6, * ) 'J2= ', INT2(J2), ' KLON/KLAT= ', KLON, KLAT, &
!     &                  ' Max: ', LAT_MAX, LON_MIN
!
         DO 440 J4=1,KLAT
            IND_LAT = IDNINT( 1 + (90.0 - LAT_MAX)/LAT_STEP ) + J4-1
            DO 450 J5=1,KLON
               IND_LON = IDNINT( 1 + (LON_MIN - 180.0)/LON_STEP ) + J5-1 + NLON/2
               IF ( IND_LON > NLON ) IND_LON = IND_LON - NLON 
               IF ( IND_LON < 1    ) IND_LON = IND_LON + NLON 
               CALL ENDIAN_CNV_I2 ( HEI_DEM(J5,J4) )
               HEI_GLO(IND_LON,NLAT+1-IND_LAT) = HEI_DEM(J5,J4)
 450        CONTINUE 
 440     CONTINUE 
!
         DEALLOCATE ( HEI_DEM )
 420  CONTINUE 
!
! --- Fix a bug in GTOPO30: all points with lat -90deg were considered as sea.
!
      HEI_GLO(1:NLON,1) = HEI_GLO(1:NLON,2)
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( LEX ) IS = UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
      IUER = -1
      CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4107, -2, 'GTOPO30_ASSEMBLE', 'Failure in '// &
     &         'an attempt to open the output file '// &
     &          FIL_DEM(J2)(1:I_LEN(FIL_DEM(J2)))//' -- '//STR )
           CALL EXIT ( 1 )
      END IF
!
      IW = WRITE ( %VAL(LUN), HEI_GLO, %VAL(2*NLON*NLAT) )
      IF ( IW .NE. 2*NLON*NLAT ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4108, -2, 'GTOPO30_ASSEMBLE', 'Failure in '// &
     &         'an attempt to write contents of data file into '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL BINF_CLOSE ( LUN, IUER )
      WRITE ( 6, '(A)' ) 'Output file '//FILOUT(1:I_LEN(FILOUT))
!
      END  PROGRAM  GTOPO30_ASSEMBLE  !#!#
