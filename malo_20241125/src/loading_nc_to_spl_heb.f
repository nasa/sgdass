      PROGRAM    LOADING_NC_TO_SPL_HEB
! ************************************************************************
! *                                                                      *
! *   Program  LOADING_NC_TO_SPL_HEB  reads the input global loading     *
! *   displacement field in netCDF format expands the fields over 2D     *
! *   B-spline basis and writes the expansion coefficients in HEB        *
! *   format. Optionally, loading_nc_to_spl_heb performs compression     *
! *   of the output file.                                                *
! *                                                                      *
! *   This program assumes the input displacement file name obeys        *
! *   the following convention: ttt_ooooooo_YYYYMMDD_hhmm.nc      or     *
! *                             ttt_ooooooo_YYYYMMDD_hhmm.nc.bz2, where  *
! *                                                                      *
! *   ttt     -- model type: atm, lws, nto, or toc                       *
! *   ooooooo -- model name: merra2, geosfp or geosfpit                  *
! *   YYYY    -- year   as an integer number                             *
! *   MM      -- month  as an integer number                             *
! *   DD      -- day    as an integer number                             *
! *   hh      -- hour   as an integer number                             *
! *   mm      -- minute as an integer number                             *
! *                                                                      *
! *   Usage: loading_nc_to_spl_heb nc-file heb_file [compr]              *
! *                                                                      *
! *          nc-file   --  input file with the global displacement       *
! *                        field in netCDF format.                       *
! *                                                                      *
! *          heb_file  --  name of the output file with expansion        *
! *                        coefficients.                                 *
! *                                                                      *
! *          compr     --  compression method. If omitted,               *
! *                        no compression is used                        *
! *                        Supported methods:                            *
! *                                                                      *
! *                           gzip                                       *
! *                           bzip2                                      *
! *                           pbzip2                                     *
! *                           pbzip2_p1                                  *
! *                           lbzip2                                     *
! *                           lbzip2_p1                                  *
! *                           lbzip2_1                                   *
! *                           lbzip2_1p1 (recommended)                   *
! *                                                                      *
! * # 19-NOV-2016 LOADING_NC_TO_SPL_HEB v1.1 (c) L. Petrov 22-MAY-2017 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'heb.i'
      INTEGER*4  MC, MS
      PARAMETER  ( MC = 8192 )
      PARAMETER  ( MS = 8192 )
      TYPE     ( HEB__TYPE  ) :: HEB_DSPL
      CHARACTER  FILIN*128, FILOUT*128, WAV_NAM*4, STR*128
      CHARACTER  TITLE*128, INSTITUTION*128, HISTORY*128, REFERENCE*128, &
     &           SOURCE(MS)*128, COMMENT(MC)*128, LOAD_TYPE*8, &
     &           LOAD_MODEL*64, COMPR_STR*16, COMPR_COM_STR*18, COM_STR*256
      REAL*4,    ALLOCATABLE :: LON(:), LAT(:), BSPL_2D(:,:,:), DSPL_2D_R4(:,:,:)
      INTEGER*4  NLON, NLAT, IVEC, ICMP, IFRQ, MJD, LC, LS, IUER 
      INTEGER*4  J1, J2, J3, ID, IU
      REAL*8     TAI
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: loading_nc_to_spl_heb nc-file heb_file [compr]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILOUT )
           IF ( ILEN(FILOUT) > 8 ) THEN
                IF ( FILOUT(ILEN(FILOUT)-7:ILEN(FILOUT)) == '.heb.bz2' ) THEN
!
! ------------------ Take out extension .bz2
!
                   
                     CALL CLRCH ( FILOUT(ILEN(FILOUT)-3:) )
                END IF
                IF ( FILOUT(ILEN(FILOUT)-6:ILEN(FILOUT)) == '.heb.gz' ) THEN
!
! ------------------ Take out extension .gz
!
                   
                     CALL CLRCH ( FILOUT(ILEN(FILOUT)-2:) )
                END IF
           END IF
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, COMPR_STR )
           END IF
      END IF
!
! --- Check compression method
!
      IF ( ILEN(COMPR_STR) == 0 ) THEN
           CONTINUE 
          ELSE IF ( COMPR_STR == 'none' ) THEN
           CONTINUE 
           CALL CLRCH ( COMPR_COM_STR )
         ELSE IF ( COMPR_STR == 'gzip' ) THEN
           COMPR_COM_STR = 'gzip -1 -f '
         ELSE IF ( COMPR_STR == 'bzip2' ) THEN
           COMPR_COM_STR = 'bzip2 -9 -f '
         ELSE IF ( COMPR_STR == 'pbzip2' ) THEN
           COMPR_COM_STR = 'pbzip2 -r -m1024 -S4096 -9 -f '
         ELSE IF ( COMPR_STR == 'pbzip2_p1' ) THEN
           COMPR_COM_STR = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
         ELSE IF ( COMPR_STR == 'lbzip2' ) THEN
           COMPR_COM_STR = 'lbzip2 -9 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_p1' ) THEN
           COMPR_COM_STR = 'lbzip2 -9 -n1 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_1' ) THEN
           COMPR_COM_STR = 'lbzip2 -1 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_1p1' ) THEN
           COMPR_COM_STR = 'lbzip2 -1 -n1 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_2p1' ) THEN
           COMPR_COM_STR = 'lbzip2 -2 -n1 -f '
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6311, IUER, 'LOADING_NC_TO_SPL_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR_STR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
! --- Inquire loading file: get the dimension and epoch
!
      IUER = -1
      CALL INQ_LOADING_NC ( FILIN, NLON, NLAT, MJD, TAI, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6312, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure in '// &
     &         'an attempt to read the header of the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate dynamic memory
!
      ALLOCATE ( DSPL_2D_R4(NLON,NLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(4)*INT8(3)*INT8(NLON)*INT8(NLAT), STR )
           IUER = -1
           CALL ERR_LOG ( 6313, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array DSPL_2D_R4' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( BSPL_2D(1-MALO__MDEG:NLON+2,1-MALO__MDEG:NLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(8)*INT8(3)*INT8(NLON+2+MALO__MDEG)*INT8(NLAT+MALO__MDEG), STR )
           IUER = -1
           CALL ERR_LOG ( 6314, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array BSPL_2D' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( LON(NLON+2), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(NLON+2), STR )
           IUER = -1
           CALL ERR_LOG ( 6315, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( LAT(NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(NLAT), STR )
           IUER = -1
           CALL ERR_LOG ( 6316, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON' )
           CALL EXIT ( 1 )
      END IF
!
! --- Extract auxulliary information from the displacement field, 
! --- in particular, the model desciption and the dataset descrition
!
      IUER = -1
      CALL READ_LOADING_NC_INFO ( FILIN, TITLE, INSTITUTION, &
     &                            MS, LS, SOURCE, HISTORY, REFERENCE, &
     &                            MC, LC, COMMENT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6317, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure in '// &
     &         'an attempt to read the data from the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the displacement field
!
      IUER = -1
      CALL READ_LOADING_NC ( FILIN, INT8(3)*INT8(NLON)*INT8(NLAT), NLON, NLAT, &
     &                       IVEC, ICMP, IFRQ, MJD, TAI, DSPL_2D_R4, WAV_NAM, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6318, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure in '// &
     &         'an attempt to read the data from the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Copy the displacemnt field in the array BSPL_2D with different
! --- dimensions. The contents if this file will be
!
      BSPL_2D = 0.0
      BSPL_2D(1:NLON,1:NLAT,1:3) = DSPL_2D_R4(1:NLON,1:NLAT,1:3)
!
! --- Expand the array BSPL_2D over longitude 360deg and one more node 
! --- beyond 360 deg in order to ensure clean interpolation
!
      BSPL_2D(NLON+1:NLON+1,1:NLAT,1:3) = BSPL_2D(1:1,1:NLAT,1:3) 
      BSPL_2D(NLON+2:NLON+2,1:NLAT,1:3) = BSPL_2D(2:2,1:NLAT,1:3) 
!
      DO 410 J1=1,NLON+2
         LON(J1) = 0.0 + (J1-1)*PI2/NLON
 410  CONTINUE 
!
      DO 420 J2=1,NLAT
         LAT(J2) = -P2I + (J2-1)*PI__NUM/(NLAT-1)
 420  CONTINUE
!
! --- Expand the diplsacement field into the 2D Bspline basis
!
      DO 430 J3=1,3
         IUER = -1
         CALL BSPL4_2D_CMP ( MALO__MDEG, 0, NLON+2, NLAT, LON, LAT, &
     &                       BSPL_2D(1-MALO__MDEG,1-MALO__MDEG,J3), IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6319, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure in '// &
     &              'an attempt to compute coefficients of the 2D '// &
     &              'interpolating spline for the displacement field' )
              CALL EXIT ( 1 )
         END IF
 430  CONTINUE 
!
! --- Prepare HEB header
!
      HEB_DSPL%MJD     = MJD
      HEB_DSPL%UTC     = TAI
      HEB_DSPL%TAI     = TAI
      HEB_DSPL%DIMS(1) = NLON+2+MALO__MDEG
      HEB_DSPL%DIMS(2) = NLAT+MALO__MDEG
      HEB_DSPL%DIMS(3) = 3
      HEB_DSPL%DIMS(4) = 1
      HEB_DSPL%DATA_OFFSET = HEB__HDS
      HEB_DSPL%ENDIAN      = HEB__LE
      HEB_DSPL%DATA_TRANSFORM = HEB__SCOF
      HEB_DSPL%FILL_VALUE     = 1.0E15
      HEB_DSPL%OFFSET         = 0.0
      HEB_DSPL%SCALE_FACTOR   = 0.00002
      HEB_DSPL%DATA_COMPRESSION = HEB__NONE
      HEB_DSPL%SDS_NAME       = 'B-spline expansion of site displacements'
      HEB_DSPL%UNITS          = 'meter'
      HEB_DSPL%DATA_FORMAT    = HEB__I2
      HEB_DSPL%MIN_VALUE      = MINVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
      HEB_DSPL%MAX_VALUE      = MAXVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
      HEB_DSPL%VALID_RANGE(1) =  -0.096D0
      HEB_DSPL%VALID_RANGE(2) =   0.096D0
      HEB_DSPL%PROD_DATE_TIME = GET_CDATE()
!
      HEB_DSPL%FILE_NAME      = FILOUT
      HEB_DSPL%HISTORY        = HISTORY
      HEB_DSPL%SOURCE         = SOURCE(1)
      HEB_DSPL%TITLE          = TITLE
      HEB_DSPL%PROD_NAME      = TITLE
      HEB_DSPL%INSTITUTION    = INSTITUTION
      HEB_DSPL%REFERENCES     = REFERENCE
      HEB_DSPL%VERSION_ID     = MALO__LABEL
!
      WRITE ( UNIT=HEB_DSPL%COMMENT(1), FMT=110 ) NLON, NLAT, 2, MALO__MDEG
 110  FORMAT ( 'Nlon= ', I6, ' Nlat= ', I6, ' Lon_overlap= ', I1, ' Mdeg= ', I1 )
!
! --- Extract the loading type and the fluid model from the file name
!
      ID = LINDEX ( FILIN, '/' )
      LOAD_TYPE  = FILIN(ID+1:ID+3)
      LOAD_MODEL = FILIN(ID+5:)
      IU = INDEX ( LOAD_MODEL, '_' )
      IF ( IU < 1 ) IU = ILEN(LOAD_MODEL)
      CALL CLRCH ( LOAD_MODEL(IU:) )
      HEB_DSPL%COMMENT(2) = 'Loading type:  '//LOAD_TYPE
      HEB_DSPL%COMMENT(3) = 'Loading model: '//LOAD_MODEL
      HEB_DSPL%SOURCE     = LOAD_MODEL
!
! --- Write down the output file in HEB format
!
      IUER = -1
      CALL WRITE_HEB ( HEB_DSPL, BSPL_2D, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6320, IUER, 'LOADING_NC_TO_SPL_HEB', 'Failure '// &
     &         'to write loading displacements into the output '// &
     &         'file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(COMPR_COM_STR) .NE. 0 ) THEN
!
! -------- Now compress the output file 
!
           COM_STR = COMPR_COM_STR(1:I_LEN(COMPR_COM_STR))//' '// &
     &               FILOUT(1:I_LEN(FILOUT))
           CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
      END IF
!
      DEALLOCATE ( DSPL_2D_R4 )
      DEALLOCATE ( BSPL_2D    )
      DEALLOCATE ( LAT        )
      DEALLOCATE ( LON        )
      CALL EXIT ( 0 )
      END  PROGRAM    LOADING_NC_TO_SPL_HEB  !#!  
