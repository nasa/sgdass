      PROGRAM    LOADING_HEB_TO_SPL_HEB_LAUNCH
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
      CALL LOADING_HEB_TO_SPL_HEB()
      END  PROGRAM  LOADING_HEB_TO_SPL_HEB_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  LOADING_HEB_TO_SPL_HEB()
! ************************************************************************
! *                                                                      *
! *   Program  LOADING_HEB_TO_SPL_HEB  reads the input global loading    *
! *   displacement field in HEB format expands the fields over 2D        *
! *   B-spline basis and writes the expansion coefficients in HEB        *
! *   format. Optionally, loading_heb_to_spl_heb performs compression    *
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
! *                           lbzip2_2p1                                 *
! *                           lbzip2_1p1 (recommended)                   *
! *                                                                      *
! * # 21-FEB-2017 LOADING_HEB_TO_SPL_HEB v2.1 (c) L. Petrov 03-JUN-2017 # *
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
     &           LOAD_MODEL*64, COMPR_STR*16, COMPR_COM_STR*16, COM_STR*256, &
     &           NUM_THR_STR*128, HEB_SDS_NAME*1024
      REAL*4,    ALLOCATABLE :: LON(:), LAT(:), BSPL_2D(:,:,:), DSPL_2D_R4(:,:,:)
      INTEGER*4  NLON, NLAT, IVEC, ICMP, IFRQ, MJD, LC, LS, NTHR, NFRQ, IUER 
      INTEGER*4  J1, J2, J3, J4, J5, J6, IND3_SECT(2), IND4_SECT(2), &
     &           ID, IU, IS, SEEK_SET, ARG_LN, LUN, IVRB
      INTEGER*8  TOT_LEN_I8, OFFS_I8, LEN_I8, OFFSET_RET
      REAL*4     LOAD_MIN, LOAD_MAX
      REAL*8     TAI
      LOGICAL*1  LEX  
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, OMP_GET_MAX_THREADS, UNLINK 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*8, EXTERNAL :: LSEEK
!
      IVRB = 1
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: loading_heb_to_spl_heb heb_loa heb_spl [compr]'
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
           COMPR_COM_STR= 'lbzip2 -9 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_p1' ) THEN
           COMPR_COM_STR= 'lbzip2 -9 -n1 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_1' ) THEN
           COMPR_COM_STR= 'lbzip2 -1 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_1p1' ) THEN
           COMPR_COM_STR= 'lbzip2 -1 -n1 -f '
         ELSE IF ( COMPR_STR == 'lbzip2_2p1' ) THEN
           COMPR_COM_STR = 'lbzip2 -2 -n1 -f '
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6311, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR_STR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
      CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
      IF ( ILEN(NUM_THR_STR) == 0 ) THEN
           NTHR = 1
         ELSE 
           CALL CHIN ( NUM_THR_STR, NTHR )
      END IF
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
      NTHR = OMP_GET_MAX_THREADS()
!
! --- Read input file with the displacement field
!
      IUER = -1
      CALL READ_HEB_HEADER ( FILIN, HEB_DSPL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6312, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &         'Failed to read the header of the input file with '// &
     &         'displacements '//FILIN )
           CALL EXIT ( 1 )
      END IF
      HEB_DSPL%TAI = HEB_DSPL%UTC
      HEB_SDS_NAME = HEB_DSPL%SDS_NAME
      NLON = HEB_DSPL%DIMS(1)
      NLAT = HEB_DSPL%DIMS(2)
      NFRQ = HEB_DSPL%DIMS(4)
      IF ( NFRQ == 1 ) THEN
           IUER = -1
           CALL READ_HEB ( FILIN, HEB_DSPL, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6313, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &              'Failed to read input file with displacements '//FILIN )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( IVRB .GE. 2 ) WRITE ( 6, * ) 'NFRQ= ', NFRQ
!
! --- Allocate dynamic memory
!
      ALLOCATE ( BSPL_2D(1-MALO__MDEG:NLON+2,1-MALO__MDEG:NLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(8)*INT8(3)*INT8(NLON+2+MALO__MDEG)*INT8(NLAT+MALO__MDEG), STR )
           IUER = -1
           CALL ERR_LOG ( 6314, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Failure to '// &
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
           CALL ERR_LOG ( 6315, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Failure to '// &
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
           CALL ERR_LOG ( 6316, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LAT' )
           CALL EXIT ( 1 )
      END IF
!
! --- Copy the displacemnt field in the array BSPL_2D with different
! --- dimensions. The contents if this file will be
!
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
      DO 430 J3=1,NFRQ
         IF ( NFRQ > 1 ) THEN
              IND3_SECT(1) = 1
              IND3_SECT(2) = 3
              IND4_SECT(1) = J3
              IND4_SECT(2) = J3
!
              IUER = -1
              CALL READ_HEB_SECT ( FILIN, HEB_DSPL, IND3_SECT, IND4_SECT, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   IUER = -1
                   CALL ERR_LOG ( 6317, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &                 'Failure to read the '//STR(1:I_LEN(STR))// &
     &                 ' th section of the input heb file '//FILIN )
                   CALL EXIT ( 1 )
              END IF
              IF ( IVRB .GE. 2 ) WRITE ( 6, * ) 'READ_HEB_SECT J3= ', J3 ; CALL FLUSH ( 6 )
         END IF
!
         BSPL_2D = 0.0
         BSPL_2D(1:NLON,1:NLAT,1:3) = HEB_DSPL%VAL(1:NLON,1:NLAT,1:3,1)
!
! ------ Expand the array BSPL_2D over longitude 360deg and one more node 
! ------ beyond 360 deg in order to ensure clean interpolation
!
         BSPL_2D(NLON+1:NLON+1,1:NLAT,1:3) = BSPL_2D(1:1,1:NLAT,1:3) 
         BSPL_2D(NLON+2:NLON+2,1:NLAT,1:3) = BSPL_2D(2:2,1:NLAT,1:3) 
!
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&      PRIVATE ( J4, IUER ),     &
!$OMP&      SCHEDULE ( STATIC )
         DO 440 J4=1,3
            IUER = -1
            CALL BSPL4_2D_CMP ( MALO__MDEG, 0, NLON+2, NLAT, LON, LAT, &
     &                          BSPL_2D(1-MALO__MDEG,1-MALO__MDEG,J4), IUER )
            IF ( IUER .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6318, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 2D '// &
     &                 'interpolating spline for the displacement field' )
                 CALL EXIT ( 1 )
            END IF
 440     CONTINUE 
!$OMP    END PARALLEL DO
         IF ( J3 == NFRQ ) THEN
              DEALLOCATE ( HEB_DSPL%VAL )
         END IF
!
! ------ Prepare HEB header for the output
!
         HEB_DSPL%DIMS(1) = NLON+2+MALO__MDEG
         HEB_DSPL%DIMS(2) = NLAT+MALO__MDEG
         HEB_DSPL%DIMS(3) = 3
         HEB_DSPL%DIMS(4) = NFRQ
         HEB_DSPL%DATA_OFFSET = HEB__HDS
         HEB_DSPL%ENDIAN      = HEB__LE
         HEB_DSPL%DATA_TRANSFORM = HEB__SCOF
         HEB_DSPL%FILL_VALUE     = 1.0E15
         HEB_DSPL%OFFSET         = 0.0
         HEB_DSPL%SCALE_FACTOR   = 0.00002
         HEB_DSPL%DATA_COMPRESSION = HEB__NONE
         ID = INDEX ( HEB_SDS_NAME, '|' )
         IF ( ID == 0 ) ID = 1
         HEB_DSPL%SDS_NAME       = 'B-spline expansion '//HEB_SDS_NAME(ID:)
         HEB_DSPL%UNITS          = 'meter'
         HEB_DSPL%MIN_VALUE      = MINVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
         HEB_DSPL%MAX_VALUE      = MAXVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
         HEB_DSPL%VALID_RANGE(1) =  -0.096D0
         HEB_DSPL%VALID_RANGE(2) =   0.096D0
         HEB_DSPL%PROD_DATE_TIME = GET_CDATE()
         HEB_DSPL%VERSION_ID     = MALO__LABEL
!
         WRITE ( UNIT=HEB_DSPL%COMMENT(1), FMT=110 ) NLON, NLAT, 2, MALO__MDEG
 110     FORMAT ( 'Nlon= ', I6, ' Nlat= ', I6, ' Lon_overlap= ', I1, ' Mdeg= ', I1 )
!
! ------ Extract the loading type and the fluid model from the file name
!
         ID = LINDEX ( FILIN, '/' )
         LOAD_TYPE  = FILIN(ID+1:ID+3)
         LOAD_MODEL = FILIN(ID+5:)
         IU = INDEX ( LOAD_MODEL, '_' )
         IF ( IU < 1 ) IU = ILEN(LOAD_MODEL)
         CALL CLRCH ( LOAD_MODEL(IU:) )
         HEB_DSPL%COMMENT(2) = 'Loading type:  '//LOAD_TYPE
         IF ( NFRQ > 1 ) HEB_DSPL%COMMENT(2) = TRIM(HEB_DSPL%COMMENT(2))//' harmonic variations'
         IF ( HEB_DSPL%SOURCE(1:5) == 'GRACE' ) LOAD_MODEL = 'GRACE' 
         IF ( LOAD_MODEL == 'e' ) LOAD_MODEL = 'GRACE'
         HEB_DSPL%COMMENT(3) = 'Loading model: '//LOAD_MODEL
         HEB_DSPL%SOURCE     = LOAD_MODEL
!
         IF ( NFRQ == 1 ) THEN
!
! ---------- Write down the output file in HEB format
!
             IUER = -1
             HEB_DSPL%DATA_FORMAT = HEB__I2
             CALL WRITE_HEB ( HEB_DSPL, BSPL_2D, FILOUT, IUER )
             IF ( IUER .NE. 0 ) THEN
                  IUER = -1
                  CALL ERR_LOG ( 6319, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &                'Failure to write loading displacements into the output '// &
     &                'file '//FILOUT )
                  CALL EXIT ( 1 )
             END IF
           ELSE
             IF ( J3 == 1 ) THEN
!
! --------------- Check weather the output file exists
!
                  INQUIRE ( FILE=FILOUT, EXIST=LEX )
                  IF ( LEX ) THEN
!
! -------------------- Exists? Then remove it
!
                       IS = UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) ) 
                       IF ( IS .NE. 0 ) THEN
                            CALL CLRCH  ( STR ) 
                            CALL GERROR ( STR )
                            CALL ERR_LOG ( 6320, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &                          'Error in an attempt to remove old output file '// &
     &                           FILOUT(1:I_LEN(FILOUT))//' '//STR )
                            RETURN 
                       END IF 
                  END IF
!
! --------------- Write down the header of the output file in HEB format
!
                  IUER = -1
                  HEB_DSPL%STATUS  = HEB__HDON
                  CALL WRITE_HEB ( HEB_DSPL, %VAL(0), FILOUT, IUER )
                  IF ( IUER .NE. 0 ) THEN
                       IUER = -1
                       CALL ERR_LOG ( 6320, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &                     'Failure to write the header of loading displacements '// &
     &                     'into the output file '//FILOUT )
                       CALL EXIT ( 1 )
                  END IF
                  LOAD_MIN = MINVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
                  LOAD_MAX = MAXVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
                  TOT_LEN_I8 = HEB__HDS_V1
                ELSE
                  LOAD_MIN = MINVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
                  LOAD_MAX = MAXVAL(BSPL_2D(1-MALO__MDEG:NLON+1,1-MALO__MDEG:NLAT-1,1:3))
             END IF
!
! ---------- Open the output file in the append mode
!
             CALL BINF_OPEN ( FILOUT, 'APPEND', LUN, IUER )
             IF ( IUER .NE. 0 ) THEN
                  IUER = -2
                  CALL ERR_LOG ( 6321, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Failure '// &
     &                'in an attempt to re-open the output '// &
     &                'file '//FILOUT )
                  CALL EXIT ( 1 )
             END IF
!
! ---------- Write the slice of the displacement field for this component,
! ---------- this wave into the output file
!
             OFFS_I8 = TOT_LEN_I8
             LEN_I8  = SIZEOF(BSPL_2D)
             TOT_LEN_I8 = TOT_LEN_I8 + LEN_I8
!
             IUER = -1
             CALL BIG_WRITE ( LUN, OFFS_I8, LEN_I8, BSPL_2D, IUER )
             IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH ( STR )
                  CALL INCH  ( J3, STR )
                  IUER = -2
                  CALL ERR_LOG ( 6322, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Failure '// &
     &                'in an attempt to append displacement for the '// &
     &                 STR(1:I_LEN(STR))//' th wave to the end of output '// &
     &                'file '//FILOUT )
                  CALL EXIT ( 1 )
             END IF
!
! ---------- Close the output file
!
             IUER = -1
             CALL BINF_CLOSE ( LUN, IUER )
             IF ( IUER .NE. 0 ) THEN
                  IUER = -2
                  CALL ERR_LOG ( 6323, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &                'Failure in an attempt to close the output '// &
     &                'file '//FILOUT )
                  CALL EXIT ( 1 )
             END IF
             IF ( J3 == NFRQ ) THEN
                  IF ( IVRB .GE. 2 ) WRITE ( 6, * ) 'TOT_LEN_I8= ', TOT_LEN_I8
!
! --------------- Update the header section. Open the output file once again
!
                  IUER = -1
                  CALL BINF_OPEN ( FILOUT, 'APPEND', LUN, IUER )
                  IF ( IUER .NE. 0 ) THEN
                       write ( 6, * ) 'iuer = ', iuer , ' lun= ', lun
                       IUER = -2
                       CALL ERR_LOG ( 6324, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &                     'Failure in an attempt to re-open the output '// &
                 &         'file '//FILOUT )
                       CALL EXIT ( 1 )
                  END IF
!
! --------------- Position to the beginning of the output file
!
                  OFFSET_RET = LSEEK( %VAL(LUN), %VAL(0), %VAL(SEEK_SET) )
                  IF ( OFFSET_RET .NE. 0 ) THEN
                       write ( 6, * ) 'offset_ret= ', offset_ret ! %%%
                       IUER = -2
                       CALL ERR_LOG ( 6325, IUER, 'LOADING_HEB_TO_SPL_HEB', &
                 &         'Failure in an attempt to position to the beginning '// &
                 &         'the output file '//FILOUT )
                       CALL EXIT ( 1 )
                  END IF
                  IF ( IVRB .GE. 2 ) WRITE ( 6, * ) 'OFFSET_RET= ', OFFSET_RET
!
! --------------- Re-write the header
!
                  HEB_DSPL%MIN_VALUE = LOAD_MIN
                  HEB_DSPL%MAX_VALUE = LOAD_MAX
                  HEB_DSPL%STATUS  = HEB__HDON
                  IUER = -1
                  CALL WRITE_HEB ( HEB_DSPL, %VAL(0), FILOUT, IUER )
                  IF ( IUER .NE. 0 ) THEN
                       IUER = -2
                       CALL ERR_LOG ( 6326, IUER, 'LOADING_HEB_TO_SPL_HEB', 'Failure '// &
                 &         'in an attempt to re-write the header of the output '// &
                 &         'file '//FILOUT )
                       CALL EXIT ( 1 )
                  END IF
!
! --------------- Position to the end of the output file
!
                  CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
                  OFFSET_RET = LSEEK( %VAL(LUN), %VAL(TOT_LEN_I8), %VAL(SEEK_SET) )
                  IF ( OFFSET_RET .NE. TOT_LEN_I8 ) THEN
                       CALL CLRCH  ( STR )
                       CALL GERROR ( STR )
                       IUER = -2
                       CALL ERR_LOG ( 6327, IUER, 'LOADING_HEB_TO_SPL_HEB', &
     &                     'Failure in position the file into the end of '// &
     &                     'the output file' )
                       CALL EXIT ( 1 )
                  END IF
!
! --------------- Finally, close the file
!
                  CALL BINF_CLOSE ( LUN, IUER )
                  IF ( IUER .NE. 0 ) THEN
                       IUER = -2
                       CALL ERR_LOG ( 6328, IUER, 'LOADING_HEB_TO_SPL_HEB', &
                 &         'Failure in an attempt to close the output '// &
                 &         'file '//FILOUT )
                       CALL EXIT ( 1 )
                  END IF
             END IF 
         END IF
 430  CONTINUE 
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
      DEALLOCATE ( BSPL_2D    )
      DEALLOCATE ( LAT        )
      DEALLOCATE ( LON        )
      CALL EXIT ( 0 )
      END  SUBROUTINE  LOADING_HEB_TO_SPL_HEB  !#!#
