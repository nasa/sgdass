      PROGRAM    WRITE_LS_PGM
! ************************************************************************
! *                                                                      *
! *   Program WRITE_LS_PGM  reads the land/sea mask in PGM format and    *
! *   writes it into HEB or netCDF format.                               *
! *                                                                      *
! * ### 11-OCT-2012  WRITE_LS_PGM   v2.0 (c) L. Petrov  08-MAR-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'heb.i'
      CHARACTER  FILPGM*128, FILOUT*128, FILPLO*128, TITLE*128, DTYP*8
      CHARACTER  STR*128
      TYPE      ( HEB__TYPE  ) :: LS
      INTEGER*4  MLON, MLAT
      PARAMETER  ( MLAT =  8192 )
      PARAMETER  ( MLON = 16384 )
      REAL*4     HEI_ARR(MLON,MLAT), ADD_OFFSET_R4, SCALE_R4, VALID_RANGE(2)
      LOGICAL*1  FL_PLOT, FL_UNITS_DEG
      INTEGER*4  J1, J2
      INTEGER*4  IDEV, IPAL, ISCL, IPRC, NLON, NLAT, ID, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
!      FILPGM = '/d2/gtopo30/ls_mask_edited_d2047.pgm'
!      FILOUT = '/d2/gtopo30/ls_mask_edited_d2047.nc'
!      DTYP   = 'fls_mask' 
!      FL_PLOT = .TRUE.
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: write_ls_pgm data_type pgm_file filout [plot] [deg]'
           CALL EXIT ( 0 )
         ELSE
           CALL GETARG ( 1, DTYP )
           CALL GETARG ( 2, FILPGM )
           CALL GETARG ( 3, FILOUT )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR )
                IF ( STR == 'plot' ) THEN
                     FL_PLOT = .TRUE.
                   ELSE 
                     WRITE ( 6, '(A)' ) 'Unsupported 4th argument: '// &
     &                                   STR(1:I_LEN(STR))
                END IF
              ELSE 
                FL_PLOT = .FALSE.
           END IF
           FL_UNITS_DEG = .FALSE.
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 4, STR )
                IF ( STR == 'deg' ) THEN
                     FL_UNITS_DEG = .TRUE.
                END IF
           END IF
      END IF
!
      IUER = -1
      CALL READ_PGM ( FILPGM, NLAT, NLON, HEI_ARR, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IDEV = 1
      IPRC = 1
      IF ( DTYP == 'bls_mask' ) THEN
           IPAL = 1
           ISCL = 1
           TITLE = 'Land/sea mask 0 -- sea, 1 -- land'
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
           VALID_RANGE(1) = 0.0
           VALID_RANGE(2) = 1.0
        ELSE IF ( DTYP == 'tls_mask' ) THEN
           IPAL = 1
           ISCL = 3
           TITLE = 'Land/sea mask 0 -- sea, 1 -- land, 2 -- sea/land'
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
           VALID_RANGE(1) = 0.0
           VALID_RANGE(2) = 2.0
        ELSE IF ( DTYP == 'fls_mask' ) THEN
           IPAL = 1
           ISCL = 1
           TITLE = 'Land/sea mask [0.0,1.0] -- a fraction of land'
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 0.01
           HEI_ARR = 100.0*HEI_ARR
           VALID_RANGE(1) = 0.0
           VALID_RANGE(2) = 1.0
        ELSE IF ( DTYP == 'dig_elev' ) THEN
           IPAL = 7
           ISCL = 14
           TITLE = 'Digital elevation' 
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
           VALID_RANGE(1) = -500.0
           VALID_RANGE(2) = 9000.0
        ELSE 
           WRITE ( 6, * ) 'Unknown data type '//DTYP
           CALL EXIT ( 1 )
      END IF
!
      ID = LINDEX ( FILOUT, '/' ) + 1
      IF ( INDEX ( FILOUT, '.heb' ) == ILEN(FILOUT)-3 ) THEN
           IUER = -1
           LS%SDS_NAME    = TITLE
           LS%UNITS       = 'm'
           LS%PROD_NAME   = 'G3TOPO'
           LS%FILE_NAME   =  FILOUT(ID:)
           LS%HISTORY     = 'Downscaled by program gtoto30_regrid'
           LS%SOURCE      = 'a global digital elevation model (DEM)'
           LS%TITLE       = 'Digital elevation averaged over the pixel area' 
           LS%INSTITUTION = 'Astrogeo Center'
           LS%REFERENCES  = 'http://webmap.ornl.gov/wcsdown/dataset.jsp?ds_id=10003'
           LS%PROD_DATE_TIME = 'n/a'
           LS%VERSION_ID     = '3'
           LS%MJD            = 51544       
           LS%UTC            = 0.0D0
           LS%TAI            = 0.0D0
           LS%DIMS(1)        = NLON
           LS%DIMS(2)        = NLAT
           LS%DIMS(3)        = 1
           LS%DIMS(4)        = 1
           LS%DATA_FORMAT      = HEB__R4
           LS%DATA_TRANSFORM   = HEB__NONE 
           LS%DATA_COMPRESSION = HEB__NONE 
           LS%MIN_VALUE        = MINVAL ( RESHAPE ( HEI_ARR, [NLON*NLAT] ) )
           LS%MAX_VALUE        = MAXVAL ( RESHAPE ( HEI_ARR, [NLON*NLAT] ) )
           LS%VALID_RANGE(1)   = VALID_RANGE(1)
           LS%VALID_RANGE(2)   = VALID_RANGE(2)
           LS%FILL_VALUE       = -9999.0
           LS%OFFSET           = 0.0
           LS%SCALE_FACTOR     = 1.0
!
           IUER = -1
           CALL WRITE_HEB ( LS, HEI_ARR, FILOUT, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE
           IUER = -1
           CALL WRITE_LS_MASK_NC ( DTYP, NLON, NLAT, HEI_ARR, ADD_OFFSET_R4, &
     &                             SCALE_R4, FILOUT, FL_UNITS_DEG, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      IF ( FL_PLOT ) THEN
           FILPLO = '/tmp/boo'
           IUER = -1
           CALL PLOT_GRID_R4 ( IDEV, IPAL, ISCL, IPRC, NLON, NLAT, HEI_ARR, &
     &                         TITLE, ' ', 1.0, -1.0, FILPLO, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END IF 
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_PGM ( FILPGM, NLAT, NLON, HEI_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_PGM 
! *                                                                      *
! *  ### 10-OCT-2012    READ_PGM   v1.0 (c)  L. Petrov  10-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILPGM*(*)
      REAL*4     HEI_ARR(*)
      INTEGER*4  NLAT, NLON, IUER 
      CHARACTER  STR*128, STR_ARR(4)*128
      INTEGER*4  UNIX_DATE, IS, LUN, IB, IE, J1, J2, J3, MAX_VAL, IP, IER
      INTEGER*8  SIZE_I8, IR
      INTEGER*1, ALLOCATABLE :: BUF(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, INDEX_I1 
      INTEGER*8, EXTERNAL :: READ
      INTEGER*4  I, J, M, LOCC
      LOCC(I,J,M) = I + (J-1)*M
!
      IS = FILE_INFO ( FILPGM(1:I_LEN(FILPGM))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6411, IUER, 'READ_PGM', 'Failure in an attempt '// &
     &                   'to collect information about input file '//FILPGM )
           RETURN 
      END IF
!
!!  write ( 6, * ) ' SIZE_I8= ', SIZE_I8
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILPGM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6412, IUER, 'READ_PGM', 'Failure in an attempt '// &
     &                   'to open input file '//FILPGM )
           RETURN 
      END IF
!
      ALLOCATE ( BUF(SIZE_I8), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZE_I8, STR )
           CALL ERR_LOG ( 6413, IUER, 'READ_PGM', 'Failure in an attempt '// &
     &                   'to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                   ' of dynamic memory for array BUF' )
           RETURN 
      END IF
      IR = READ ( %VAL(LUN), BUF, %VAL(SIZE_I8) )
      IF ( IR .NE. SIZE_I8 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6414, IUER, 'READ_PGM', 'Failure in an attempt '// &
     &                   'to read input file '//FILPGM(1:I_LEN(FILPGM))// &
     &                   ' -- '//STR )
           RETURN 
      END IF
!
      CALL BINF_CLOSE ( LUN, IER )
!
      IB = 1
      DO 410 J1=1,4
         IE = INDEX_I1 ( BUF(IB), CHAR(10), %VAL(SIZE_I8-IB+2) ) + IB-2
         IF ( IE < IB .OR. IE > SIZE_I8  ) THEN
              CALL ERR_LOG ( 6415, IUER, 'READ_PGM', 'Failure in parsing'// &
     &                      ' input file '//FILPGM(1:I_LEN(FILPGM))// &
     &                      ' cannot find the first string' )
              RETURN 
         END IF
         CALL CLRCH  ( STR_ARR(J1) ) 
         CALL MEMCPY ( %REF(STR_ARR(J1)), BUF(IB:IE), %VAL(IE-IB+1) )
         IB = IE + 2
 410  CONTINUE 
!
      IP = INDEX ( STR_ARR(3), ' ' )
      CALL CHIN ( STR_ARR(3)(1:IP-1), NLON    )
      CALL CHIN ( STR_ARR(3)(IP+1:),  NLAT    )
      CALL CHIN ( STR_ARR(4),         MAX_VAL )
!
      IF ( MAX_VAL .NE. 255 ) THEN
           CALL ERR_LOG ( 6416, IUER, 'READ_PGM', 'Failure in parsing'// &
     &                   ' input file '//FILPGM(1:I_LEN(FILPGM))// &
     &                   ' -- unsupported value of max_val: '//STR_ARR(4) )
           RETURN 
      END IF
!
      DO 420 J2=1,NLAT
         DO 430 J3=1,NLON
            IF ( BUF(IB) .GT. 0 ) THEN
                 HEI_ARR(LOCC(J3,NLAT+1-J2,NLON)) = FLOAT(BUF(IB))/255.
               ELSE IF ( BUF(IB) .LT. 0 ) THEN
                 HEI_ARR(LOCC(J3,NLAT+1-J2,NLON)) = (256.0 + FLOAT(BUF(IB)))/255.0
               ELSE 
                 HEI_ARR(LOCC(J3,NLAT+1-J2,NLON)) = 0.0 
            END IF
            IB = IB + 1
 430     CONTINUE 
 420  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE READ_PGM  !#!#
