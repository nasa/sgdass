      PROGRAM    MODIS_LW_GEN
! ************************************************************************
! *                                                                      *
! *   Program MODIS_LW_GEN reads a set of files in HDF format with       *
! *   MOD44W product, parses them and generates the output file with     *
! *   water-land mask at the equal-angle latitude/longitude grid in heb  *
! *   format.                                                            *
! *                                                                      *
! *  ### 29-DEC-2015  MODIS_LW_GEN v1.0 (c)  L. Petrov  29-DEC-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MOD_INFO__TYPE ) :: MOD_INFO
      TYPE     ( HEB__TYPE      ) :: HEB_LS, HEB_MOD
      INTEGER*8  DIR_DESC(16)
      INTEGER*4  M_FIL, MLON, MLAT
      PARAMETER  ( M_FIL =   1024 )
      PARAMETER  ( MLON  = 172800 )
      PARAMETER  ( MLAT  =  86401 )
      CHARACTER  DIR_MOD*128, FILNAM*128, FILOUT*128, FIL_HDF(M_FIL)*128, &
     &           STR*128, NUM_THR_STR*128
      INTEGER*4  LEV, IS, IL, L_FIL, NTHR, J1, J2, J3, J4, IUER
!!      INTEGER*8, EXTERNAL :: READ, WRITE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, OMP_GET_MAX_THREADS
!
      DIR_MOD = '/s0/mod44w/'
      FILOUT  = '/s0/mod44w/mod44w_water.heb'
!
      LEV = 0
      L_FIL = 0
      DO 410 J1=1,1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_MOD, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 4401, -2, 'MODIS_LW_GEN', 'Error in '// &
     &           'reading input directory '//DIR_MOD(1:I_LEN(DIR_MOD))// &
     &           '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IL =  ILEN(FILNAM) 
         IF ( IL < 8 ) GOTO 410
         IF ( INDEX ( FILNAM(IL-3:IL), '.hdf' ) > 0 ) THEN
              L_FIL = L_FIL + 1 
              FIL_HDF(L_FIL:) = FILNAM
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           CALL ERR_LOG ( 4402, -2, 'MODIS_LW_GEN', 'No hdf files were found '// &
     &           'in the input directory '//DIR_MOD )
           CALL EXIT ( 1 )
         ELSE 
           CALL SORT_CH ( L_FIL, FIL_HDF )
      END IF
      WRITE ( 6, '(I4,A)') L_FIL, ' hdf files were found in the input directory' 
      HEB_LS%DIMS(1) = MLON
      HEB_LS%DIMS(2) = MLAT
      HEB_LS%DIMS(3) = 1
      HEB_LS%DIMS(4) = 1
      HEB_MOD%DATA_FORMAT = HEB__I1
      ALLOCATE ( HEB_LS%VAL1(HEB_LS%DIMS(1),HEB_LS%DIMS(2),HEB_LS%DIMS(3),HEB_LS%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR ) 
           CALL IINCH8 ( HEB_LS%DIMS(1)*HEB_LS%DIMS(2)*HEB_LS%DIMS(3)*HEB_LS%DIMS(4), &
     &                   STR )
           IUER = -1
           CALL ERR_LOG ( 4443, IUER, 'MODIS_LW_GEN', 'Failure '// &
               'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for HEB_LS%VAL' )
           RETURN
      END IF
      IUER = -1
      CALL LW_INIT ( HEB_LS, IUER )
!
      CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
      IF ( ILEN(NUM_THR_STR) == 0 ) THEN
           NTHR = 1
         ELSE 
           CALL CHIN ( NUM_THR_STR, NTHR )
      END IF
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
      NTHR = OMP_GET_MAX_THREADS()
!!      NTHR = 1 ! parallelization does not work
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J2, MOD_INFO, IUER ) &
!$OMP&   FIRSTPRIVATE ( HEB_MOD ) &
!$OMP&   SCHEDULE ( STATIC )
      DO 420 J2=1,L_FIL
         IUER = -1
!$OMP    CRITICAL (PARSE)
         CALL PARSE_MOD44W ( FIL_HDF(J2), HEB_MOD, MOD_INFO, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 4404, IUER, 'MODIS_LW_GEN', 'Error in parsing '// &
     &            'input HDF file '//FIL_HDF(J2) )
              CALL EXIT ( 1 )
         END IF
         WRITE ( 6, 210 ) FIL_HDF(J2)(1:I_LEN(FIL_HDF(J2))), &
     &                    MOD_INFO%IND_HOR_TILE, MOD_INFO%IND_VER_TILE
 210     FORMAT ( 'File: ', A, ' Tiles (h/v): ', I4, 1X, I4 )
!$OMP     END CRITICAL (PARSE)
!
         IUER = -1
         CALL UPDATE_LW_MODIS ( HEB_LS, HEB_MOD, MOD_INFO, IUER )
!$OMP    CRITICAL (ERROR_CHECK)
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 4404, IUER, 'MODIS_LW_GEN', 'Error in updating '// &
     &            'land-water mask whne processing contents of file '// &
     &             FIL_HDF(J2) )
              CALL EXIT ( 1 )
         END IF
!$OMP    END CRITICAL (ERROR_CHECK)
 420  CONTINUE 
!$OMP END PARALLEL DO
!
      IUER = -1
      HEB_LS%SDS_NAME    = 'Original Water mask at native resolution'
      HEB_LS%UNITS       = 'd/m'
      HEB_LS%PROD_NAME   = 'MOD44W'
      HEB_LS%FILE_NAME   =  FILOUT
      HEB_LS%HISTORY     = 'Converted from HDF files with modis_lw_gen'
      HEB_LS%SOURCE      = 'MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN Grid'
      HEB_LS%TITLE       = 'Land/Water mask from MOD44W'
      HEB_LS%INSTITUTION = 'NASA'
      HEB_LS%COMMENT(1)  = '1 for land and 0 for water. Original fill valeus are interpreted as water'
      HEB_LS%REFERENCES  = 'http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/qaFlagPage?cgi?sat=terra&ver=C5'
      HEB_LS%PROD_DATE_TIME = '2009-07-31T17:28:14.000Z'
      HEB_LS%VERSION_ID     = '5.1.0'
      HEB_LS%MJD            = 51544       
      HEB_LS%UTC            = 0.0D0
      HEB_LS%TAI            = 0.0D0
      HEB_LS%DIMS(1)        = MLON
      HEB_LS%DIMS(2)        = MLAT
      HEB_LS%DIMS(3)        = 1
      HEB_LS%DIMS(4)        = 1
      HEB_LS%DATA_FORMAT      = HEB__I1
      HEB_LS%DATA_TRANSFORM   = HEB__NONE 
      HEB_LS%DATA_COMPRESSION = HEB__NONE 
      HEB_LS%MIN_VALUE        = 0.0
      HEB_LS%MAX_VALUE        = 1.0
      HEB_LS%VALID_RANGE(1)   = 0.0
      HEB_LS%VALID_RANGE(2)   = 1.0
      HEB_LS%FILL_VALUE       = MOD_INFO%FILL_VALUE
      HEB_LS%OFFSET           = 0.0
      HEB_LS%SCALE_FACTOR     = 1.0
      IUER = -1
      CALL WRITE_HEB ( HEB_LS, HEB_LS%VAL1, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4405, -2, 'MODIS_LW_GEN', 'Failure in writing '// &
     &         'into the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      END  PROGRAM    MODIS_LW_GEN  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARSE_MOD44W ( FILIN, HEB_MOD, MOD_INFO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_MOD44W parses input data file with land-water mask   *
! *   HDF format and puts results of parsing in HEB_MOD and MOD_INFO     *
! *   data structures.                                                   *
! *                                                                      *
! *  ### 29-DEC-2015  PARSE_MOD44W  v1.0 (c) L. Petrov  29-DEC-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'hdf4.inc'
      CHARACTER  FILIN*(*)
      TYPE     ( HEB__TYPE      ) :: HEB_MOD
      TYPE     ( MOD_INFO__TYPE ) :: MOD_INFO
      INTEGER*4  IUER
      INTEGER*4    META__M_BUF 
      PARAMETER  ( META__M_BUF = 128000 )
      INTEGER*1  META_BUF(META__M_BUF)
      CHARACTER  DS_NAME*64, DA_NAME*64, STR*128
      INTEGER*4  SD_ID, SDS_ID, SDS_IND, SDA_IND, DS_RANK, DS_DIMS(32), &
     &           DS_DTYPE, DS_NATTR, DA_DTYPE, DA_COUNT, &
     &           START(8), STRIDE(8), EDGES(8), IS, ISTAT, SAT_IND, &
     &           LEN_META, IER 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      DS_NAME = 'water_mask'
!
! --- Open the input file and get the file id: SD_ID
!
      SD_ID = SFSTART ( FILIN, HDF4__DFACC_READ )
      IF ( SD_ID == HDF4__FAIL  ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4431, IUER, 'PARSE_MOD44W', 'Error in '// &
     &         'opening input file '//FILIN )
           RETURN
      END IF
!
! --- Find the SDS index DS_NAME
!
      SDS_IND = SFN2INDEX ( SD_ID, DS_NAME  )
      IF ( SDS_IND < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4432, IUER, 'PARSE_MOD44W', 'Cannot '// &
     &         'find dataset '//DS_NAME(1:I_LEN(DS_NAME))// &
     &         ' in input file '//FILIN )
           IS = SFEND ( SD_ID )
           RETURN
      END IF
!
! --- Select the SDS
!
      SDS_ID = SFSELECT ( SD_ID, SDS_IND )
!
! --- Get information about the SDS: rank, dimensions, data type,
! --- the number of attributes
!
      IS = SFGINFO ( SDS_ID, DS_NAME, DS_RANK, DS_DIMS, DS_DTYPE, &
     &               DS_NATTR )
      IF ( IS < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4433, IUER, 'PARSE_MOD44W', 'Error '// &
     &         'in retrieval information about the dataset '// &
     &         DS_NAME(1:I_LEN(DS_NAME))//' in the input file '//FILIN )
           IS = SFEND ( SD_ID )
           RETURN
      END IF
!
! --- Get _FillValue attribute
!
      SDA_IND = SFFATTR ( SDS_ID, '_FillValue' )
      IF ( SDA_IND < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4434, IUER, 'PARSE_MOD44W', 'Cannot '// &
     &         'find attribute _FillValue for dataset '// &
     &         DS_NAME(1:I_LEN(DS_NAME))//' in the input file '//FILIN )
           IS = SFEND ( SD_ID )
           RETURN
      END IF
      IS = SFRNATT ( SDS_ID, SDA_IND, MOD_INFO%FILL_VALUE )
      IF ( IS < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4435, IUER, 'PARSE_MOD44W', 'Failure '// &
     &         'in reading attribute _FillValue for dataset '// &
     &          DS_NAME(1:I_LEN(DS_NAME))//' in the input file '//FILIN )
           RETURN
      END IF
!
! --- Find core metadata
!
      SAT_IND = SFFATTR ( SD_ID, 'StructMetadata.0' )
      IF ( SAT_IND < 0 ) THEN
           CALL ERR_LOG ( 4436, IUER, 'PARSE_MOD44W', 'Cannot '// &
              'find attribute StructMetadata.0 in the input file '// &
               FILIN )
           RETURN
      END IF
!
! --- Read struct metadata
!
      IS = SFRCATT ( SD_ID, SAT_IND, META_BUF, %VAL(SIZEOF(META_BUF)) )
      IF ( IS < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4437, IUER, 'PARSE_MOD44W', 'Failure '// &
               'in reading global attribute StructMetadata.0 '// &
               'in the input file '//FILIN )
           RETURN
      END IF
!
! --- Lean the lenght of the buffer with core metadata
!
      IS = SFGAINFO ( SD_ID, SAT_IND, DA_NAME, DA_DTYPE, LEN_META )
!
! --- Parse the core metadata. Results of parsing are put in MOD_INFO
!
      CALL ERR_PASS ( IUER, IER )
      CALL MODIS_PARSE_COREDATA ( LEN_META, META_BUF, MOD_INFO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4438, IUER, 'PARSE_MOD44W', 'Failure '// &
               'to parse global attribute StructMetadata.0 '// &
               'in the input file '//FILIN )
           RETURN
      END IF
!
      SAT_IND = SFFATTR ( SD_ID, 'CoreMetadata.0' )
!
! --- Read core metadata
!
      IS = SFRCATT ( SD_ID, SAT_IND, META_BUF, %VAL(SIZEOF(META_BUF)) )
      IF ( IS < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4439, IUER, 'PARSE_MOD44W', 'Failure '// &
               'in reading global attribute CoreMetadata.0 '// &
               'in the input file '//FILIN )
           RETURN
      END IF
!
! --- Lean the lenght of the buffer with core metadata
!
      IS = SFGAINFO ( SD_ID, SAT_IND, DA_NAME, DA_DTYPE, LEN_META )
!
! --- Parse the core metadata. Results of parsing are put in AS2A
!
      CALL ERR_PASS ( IUER, IER )
      CALL MODIS_PARSE_COREDATA ( LEN_META, META_BUF, MOD_INFO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4440, IUER, 'PARSE_MOD44W', 'Failure '// &
               'to parse global attribute coremetdata CoreMetadata.0 '// &
               'in the input file '//FILIN )
           RETURN
      END IF
!
      SAT_IND = SFFATTR ( SD_ID, 'ArchiveMetadata.0' )
!
! --- Read core metadata
!
      IS = SFRCATT ( SD_ID, SAT_IND, META_BUF, %VAL(SIZEOF(META_BUF)) )
      IF ( IS < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4441, IUER, 'PARSE_MOD44W', 'Failure '// &
               'in reading global attribute ArchiveMetadata.0 '// &
               'in the input file '//FILIN )
           RETURN
      END IF
!
! --- Lean the lenght of the buffer with core metadata
!
      IS = SFGAINFO ( SD_ID, SAT_IND, DA_NAME, DA_DTYPE, LEN_META )
!
! --- Parse the core metadata. Results of parsing are put in AS2A
!
      CALL ERR_PASS ( IUER, IER )
      CALL MODIS_PARSE_COREDATA ( LEN_META, META_BUF, MOD_INFO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4442, IUER, 'PARSE_MOD44W', 'Failure '// &
               'to parse global attribute ArchiveMetadata.0 '// &
               'in the input file '//FILIN )
           RETURN
      END IF
      IF ( ASSOCIATED ( HEB_MOD%VAL1 ) ) THEN
           DEALLOCATE ( HEB_MOD%VAL1 )
      END IF
!
! --- Allocate memory in HEB_MOD for the data
!
      HEB_MOD%DIMS(1) = DS_DIMS(1)
      HEB_MOD%DIMS(2) = DS_DIMS(2)
      HEB_MOD%DIMS(3) = 1
      HEB_MOD%DIMS(4) = 1
      HEB_MOD%DATA_FORMAT = HEB__I1
      ALLOCATE ( HEB_MOD%VAL1(HEB_MOD%DIMS(1),HEB_MOD%DIMS(2),HEB_MOD%DIMS(3),HEB_MOD%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR ) 
           CALL IINCH8 ( HEB_MOD%DIMS(1)*HEB_MOD%DIMS(2)*HEB_MOD%DIMS(3)*HEB_MOD%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 4443, IUER, 'PARSE_MOD44W', 'Failure '// &
               'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for HEB_MOD%VAL' )
           RETURN
      END IF
      START(1)  = 0
      START(2)  = 0
      STRIDE(1) = 1
      STRIDE(2) = 1
      EDGES(1)  = DS_DIMS(1)
      EDGES(2)  = DS_DIMS(2)
!
! --- Read the dataset into HEB_MOD%VAL
!
      IS = SFRDATA ( SDS_ID, START, STRIDE, EDGES, HEB_MOD%VAL1 )
      IF ( IS < 0 ) THEN
           ISTAT = HEPRNT ( 1 )
           CALL ERR_LOG ( 4444, IUER, 'PARSE_MOD44W', 'Failure '// &
     &         'in attempt to read dataset '// &
     &          DS_NAME(1:I_LEN(DS_NAME))//' in the input file '//FILIN )
           RETURN
      END IF
!
      IS = SFEND ( SD_ID )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PARSE_MOD44W  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MODIS_PARSE_COREDATA ( LEN_META, META_BUF, MOD_INFO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MODIS_PARSE_COREDATA                                       *
! *                                                                      *
! * ## 04-JUN-2010 MODIS_PARSE_COREDATA v2.0 (c) L. Petrov 30-DEC-2015 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'hdf4.inc'
      TYPE     ( MOD_INFO__TYPE ) :: MOD_INFO
      INTEGER*4  LEN_META, IUER
      INTEGER*1  META_BUF(LEN_META)
      CHARACTER  STR*512
      INTEGER*4  MIND, MBUF
      PARAMETER  ( MIND = 256   )
      PARAMETER  ( MBUF = 2048 )
      CHARACTER, ALLOCATABLE :: COR(:)*1024
      CHARACTER  REG*10
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'='//'"'//'\'//'('//')'//','//':' )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IB, IP, IND(2,MIND), LIND, &
     &           IND_2ND(2,MIND), LIND_2ND, L_COR, ORB_NUM, &
     &           N_NOD, L_NOD, B_IND, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: UTC_TO_TAI
!
      ALLOCATE ( COR(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MBUF*LEN(COR(1)) )
           CALL ERR_LOG ( 2411, IUER, 'MODIS_PARSE_COREDATA', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         ' for the buffer for coredump' )
           RETURN
      END IF
      IB = 1
      L_COR = 0
      DO 410 J1=1,LEN_META
         IF ( META_BUF(J1) == 10 .OR. J1 == LEN_META ) THEN
              IF ( J1 > IB ) THEN
!
! ---------------- Extract a line from the buffer
!
                   L_COR = L_COR + 1
                   CALL CLRCH ( COR(L_COR) )
                   CALL LIB$MOVC3 ( J1-IB-1, META_BUF(IB+1), %REF(COR(L_COR)) )
                   IB = J1 + 1
              END IF
        END IF
 410  CONTINUE
!!   write ( 6, * ) 'L_COR= ', L_COR
!
      DO 420 J2=1,L_COR
         CALL EXWORD ( COR(J2), MIND, LIND, IND, REG, -2 )
         IF ( LIND < 2 ) GOTO 420
         IF ( COR(J2)(IND(1,1):IND(2,1)) == 'UpperLeftPointMtrs' ) THEN
              READ ( UNIT=COR(J2)(IND(1,2):IND(2,2)), FMT='(F16.5)' ) MOD_INFO%ULP(1)
              READ ( UNIT=COR(J2)(IND(1,3):IND(2,3)), FMT='(F16.5)' ) MOD_INFO%ULP(2)
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'LowerRightMtrs' ) THEN
              READ ( UNIT=COR(J2)(IND(1,2):IND(2,2)), FMT='(F16.5)' ) MOD_INFO%LRP(1)
              READ ( UNIT=COR(J2)(IND(1,3):IND(2,3)), FMT='(F16.5)' ) MOD_INFO%LRP(2)
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'ProjParams' ) THEN
              READ ( UNIT=COR(J2)(IND(1,2):IND(2,2)), FMT='(F16.5)' ) MOD_INFO%RAD_PROJ
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'OBJECT' .AND. &
     &               COR(J2)(IND(1,2):IND(2,2)) == 'NORTHBOUNDINGCOORDINATE' ) THEN
              CALL EXWORD ( COR(J2+2), MIND, LIND_2ND, IND_2ND, REG, -2 )
              READ ( UNIT=COR(J2+2)(IND_2ND(1,2):IND_2ND(2,2)), FMT='(F16.5)' ) MOD_INFO%NOR_LAT
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'OBJECT' .AND. &
     &               COR(J2)(IND(1,2):IND(2,2)) == 'SOUTHBOUNDINGCOORDINATE' ) THEN
              CALL EXWORD ( COR(J2+2), MIND, LIND_2ND, IND_2ND, REG, -2 )
              READ ( UNIT=COR(J2+2)(IND_2ND(1,2):IND_2ND(2,2)), FMT='(F16.5)' ) MOD_INFO%SOU_LAT
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'OBJECT' .AND. &
     &               COR(J2)(IND(1,2):IND(2,2)) == 'EASTBOUNDINGCOORDINATE' ) THEN
              CALL EXWORD ( COR(J2+2), MIND, LIND_2ND, IND_2ND, REG, -2 )
              READ ( UNIT=COR(J2+2)(IND_2ND(1,2):IND_2ND(2,2)), FMT='(F16.5)' ) MOD_INFO%EAS_LON
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'OBJECT' .AND. &
     &               COR(J2)(IND(1,2):IND(2,2)) == 'WESTBOUNDINGCOORDINATE' ) THEN
              CALL EXWORD ( COR(J2+2), MIND, LIND_2ND, IND_2ND, REG, -2 )
              READ ( UNIT=COR(J2+2)(IND_2ND(1,2):IND_2ND(2,2)), FMT='(F16.5)' ) MOD_INFO%WES_LON
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'OBJECT' .AND. &
     &               COR(J2)(IND(1,2):IND(2,2)) == 'CHARACTERISTICBINSIZE' ) THEN
              CALL EXWORD ( COR(J2+2), MIND, LIND_2ND, IND_2ND, REG, -2 )
              READ ( UNIT=COR(J2+2)(IND_2ND(1,2):IND_2ND(2,2)), FMT='(F16.5)' ) MOD_INFO%PIXEL_SIZE
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'VALUE'                .AND. &
     &               COR(J2)(IND(1,2):IND(2,2)) == 'HORIZONTALTILENUMBER'       ) THEN
              CALL EXWORD ( COR(J2+7), MIND, LIND_2ND, IND_2ND, REG, -2 )
              READ ( UNIT=COR(J2+7)(IND_2ND(1,2):IND_2ND(2,2)), FMT='(I3)' ) MOD_INFO%IND_HOR_TILE
           ELSE IF ( COR(J2)(IND(1,1):IND(2,1)) == 'VALUE'                .AND. &
     &               COR(J2)(IND(1,2):IND(2,2)) == 'VERTICALTILENUMBER'         ) THEN
              CALL EXWORD ( COR(J2+7), MIND, LIND_2ND, IND_2ND, REG, -2 )
              READ ( UNIT=COR(J2+7)(IND_2ND(1,2):IND_2ND(2,2)), FMT='(I3)' ) MOD_INFO%IND_VER_TILE
         END IF
 420  CONTINUE 
      DEALLOCATE ( COR ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MODIS_PARSE_COREDATA  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LW_INIT ( HEB_LS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LW_INIT initilizes land water mask. It sets the area       *
! *   with latitude < -85 deg as land and sets everything else as water. *
! *                                                                      *
! *  ### 05-JAN-2016     LW_INIT   v1.0 (c)  L. Petrov  05-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      TYPE     ( HEB__TYPE      ) :: HEB_LS
      INTEGER*4  IUER
      REAL*8     PHI_MIN
      PARAMETER  ( PHI_MIN = -85.0D0**DEG__TO__RAD )
      REAL*8     PHI
      INTEGER*4  J1, J2
!
      HEB_LS%VAL1 = MALO__WATER_VAL
      DO 410 J1=1,HEB_LS%DIMS(2)
         PHI = -P2I + (J1-1)*PI__NUM*(HEB_LS%DIMS(2)-1)
         IF ( PHI < PHI_MIN ) THEN
              DO 420 J2=1,HEB_LS%DIMS(1)
                 HEB_LS%VAL1(J2,J1,1,1) = MALO__LAND_VAL
 420          CONTINUE 
         END IF 
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LW_INIT !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPDATE_LW_MODIS ( HEB_LS, HEB_MOD, MOD_INFO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine UPDATE_LW_MODIS updates the water-land mask for the        *
! *   contribution from one MODIS swath.                                 *
! *                                                                      *
! * ### 29-DEC-2015 UPDATE_LW_MODIS v1.0 (c) L. Petrov  05-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE      ) :: HEB_LS, HEB_MOD
      TYPE     ( MOD_INFO__TYPE ) :: MOD_INFO
      INTEGER*4  IUER 
      REAL*8     PHI, LAM, DCOS_PHI, PHI_STEP, PHIG, LAMG, LAM_STEP, DIST
      REAL*8     PHI_MAX, SCL
      PARAMETER  ( PHI_MAX = 85.0D0*DEG__TO__RAD )
      PARAMETER  ( SCL     = 1.250D0 )
      INTEGER*4  IND_PHI, IND_LAM, IND_LAMG, J1, J2, J3, NP, ITURN
      REAL*8,    EXTERNAL :: ARC_LEN_AD 
!
      PHI_STEP = PI__NUM/(HEB_LS%DIMS(2) - 1)
      LAM_STEP = PI2/HEB_LS%DIMS(1)
      DO 410 J1=1,HEB_MOD%DIMS(2)
         PHI = (MOD_INFO%ULP(2) - (J1-0.5D0)*MOD_INFO%PIXEL_SIZE)/MOD_INFO%RAD_PROJ
         DCOS_PHI = DCOS(PHI)
         IND_PHI = IDNINT( (PHI + P2I)/PHI_STEP ) + 1
         IF ( IND_PHI < 1 ) IND_PHI = 1
         IF ( IND_PHI > HEB_LS%DIMS(2) ) IND_PHI = HEB_LS%DIMS(2) 
         IF ( PHIG > PHI_MAX ) GOTO 410
         DO 420 J2=1,HEB_MOD%DIMS(1)
            IF ( HEB_MOD%VAL1(J2,J1,1,1) == 0 ) THEN
                 LAM = (MOD_INFO%ULP(1) + (J2-0.5D0)*MOD_INFO%PIXEL_SIZE)/MOD_INFO%RAD_PROJ/ &
     &                 DCOS_PHI 
                 ITURN = LAM/PI2
                 LAM = LAM - ITURN*PI2
                 IF ( LAM .GE.  PI2   ) LAM = LAM - PI2
                 IF ( LAM .LT.  0.0D0 ) LAM = LAM + PI2
                 IND_LAM = IDNINT(LAM/LAM_STEP) + 1
                 IF ( IND_LAM < 1              ) IND_LAM = IND_LAM + HEB_LS%DIMS(1)
                 IF ( IND_LAM > HEB_LS%DIMS(1) ) IND_LAM = IND_LAM - HEB_LS%DIMS(1)
!
                 NP = IDNINT(1.0/DCOS_PHI) + 1
                 PHIG = (IND_PHI-1)*PHI_STEP - P2I
                 LAMG = (IND_LAM-1)*LAM_STEP
!
                 DO 430 J3=-NP,NP
                    DIST = ARC_LEN_AD ( LAMG + J3*LAM_STEP, PHIG, LAMG, PHIG )*MOD_INFO%RAD_PROJ
                    IF ( DIST < SCL*MOD_INFO%PIXEL_SIZE ) THEN
                         IND_LAMG = IND_LAM + J3
                         IF ( IND_LAMG < 1              ) IND_LAMG = IND_LAMG + HEB_LS%DIMS(1)
                         IF ( IND_LAMG > HEB_LS%DIMS(1) ) IND_LAMG = IND_LAMG - HEB_LS%DIMS(1)
                         HEB_LS%VAL1(IND_LAMG,IND_PHI,1,1) = MALO__LAND_VAL
                         IF ( PHIG > 0.0 .AND. J1 == HEB_MOD%DIMS(1) ) THEN
                              HEB_LS%VAL1(IND_LAMG,IND_PHI-1,1,1) = MALO__LAND_VAL
                         END IF
                    END IF
 430             CONTINUE 
            END IF 
 420     CONTINUE 
 410  CONTINUE 
      DEALLOCATE ( HEB_MOD%VAL1 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE UPDATE_LW_MODIS  !#!  
