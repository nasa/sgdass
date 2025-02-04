      PROGRAM    GEOS_GEN_EH
! ************************************************************************
! *                                                                      *
! *   Program GEOS_GEN_EH generates a file in HEB format with ellipsoid  *
! *   height of 2D grid-points used in GMAO numerical weather model.     *
! *   It reads the dataset PHIS -- surface geopotential, reads EGM96     *
! *   geopotential, compute gravity field at the grid that has geodetic  *
! *   latitudes and longitudes and ellipsoidal heights = 0. Then         *
! *   from the differences of geopotential provided by PHIS and the      *
! *   geopotential at the geoid it computes geometric height of the grid *
! *   point above the ellipsoid.                                         *
! *                                                                      *
! *  ### 07-FEB-2013  GEOS_GEN_EH  v1.1 (c)  L. Petrov  01-AUG-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_PHIS, HEB_GPT
      INTEGER*4  MP, MDEG, MDIMS
      PARAMETER  ( MP     = 64*1024 )
      PARAMETER  ( MDEG   =     360 )
      CHARACTER  EGM96_FIL*128, FILOUT*128, GEOS_TIN*128, STR*128
      CHARACTER  BUF(MP)*128
      COMPLEX*16 GPT(0:MDEG,0:MDEG)
      REAL*8     VAL1, VAL2
      REAL*4     VAL_MIN, VAL_MAX
      LOGICAL*1  FL_GEOID, FL_GEOID_ONLY
      INTEGER*4  J1, J2, J3, J4, NP, L_LON, L_LAT, IND_1, IND_2, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      FL_GEOID      = .FALSE.
      FL_GEOID_ONLY = .FALSE.
!!      EGM96_FIL = '/progs/malo_20170618/share/egm96_to360.ascii'
      EGM96_FIL = '/t0/imls_devel_model/egm96_to360.ascii'
!
!      GEOS_TIN  = '/g3/geos_constants/DAS.ops.asm.const_2d_asm_Nx.GEOS572.00000000_0000.V01.nc4'
!!      FILOUT    = '/tmp/ge_ellipsoid_height.heb'
!
!!      GEOS_TIN  = '/g3/geos_constants/MERRA300.prod.assim.const_2d_asm_Nx.00000000.hdf'
!!      GEOS_TIN  = '/g3/geos_constants/DAS.ops.asm.const_2d_asm_Nx.GEOS572.00000000_0000.V01.nc4'
!!      GEOS_TIN  = '/g3/geos_constants/geosfpit_GEOS.fp.asm.const_2d_asm_Nx.00000000_0000.V01.nc4'
!!      GEOS_TIN  = '/g3/geos_constants/DAS.fpit.asm.const_2d_asm_Nx.GEOS591.00000000_0000.V01.nc4'
      GEOS_TIN  = '/t0/imls_devel_model/GEOS.it.asm.asm_const_0hr_glo_L576x361_slv.GEOS5294.1998-01-01T0000.V01.nc4'
!!      GEOS_TIN  = '/s0/geos_constants/MERRA2_101.const_2d_asm_Nx.00000000.nc4'
!!      GEOS_TIN  = '/t0/7km/orig/const/c1440_NR.const_2d_asm_Nx.20060101.nc4'
!
      IF ( FL_GEOID ) THEN
!!           FILOUT    = '/tmp/merra_ellipsoid_height.heb'
!!           FILOUT    = '/tmp/geosfp_ellipsoid_height.heb'
!!           FILOUT    = '/tmp/7km_ellipsoid_height.heb'
!!           FILOUT    = '/tmp/geosit_ellipsoid_height.heb'
!!           FILOUT    = '/tmp/geosit_ellipsoid_height.heb'
         ELSE 
!!           FILOUT    = '/tmp/geosfp_height_above_geoid.heb'
!!           FILOUT    = '/tmp/merra_height_above_geoid.heb'
!!           FILOUT    = '/tmp/geos57_height_above_geoid.heb'
!!           FILOUT    = '/tmp/geosfp_height_above_geoid.heb'
!!           FILOUT    = '/tmp/geosfpit_height_above_geoid.heb'
!!           FILOUT    = '/tmp/merra2_height_above_geoid.heb'
           FILOUT    = '/tmp/geosit_geoid_height.heb'
      END IF
!
      IUER = -1
      CALL RD_TEXT ( EGM96_FIL, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1801, IUER, 'GEOS_GEN_EH', 'Failure in '// &
     &         'an attempt to read file '//EGM96_FIL )
           CALL EXIT ( 1 )
      END IF
!
      GPT = (0.0, 0.0)
      DO 410 J1=1,NP
         READ ( UNIT=BUF(J1)(1:4), FMT='(I4)' ) IND_1
         READ ( UNIT=BUF(J1)(5:8), FMT='(I4)' ) IND_2
         READ ( UNIT=BUF(J1)(10:28), FMT='(D19.12)' ) VAL1
         READ ( UNIT=BUF(J1)(30:48), FMT='(D19.12)' ) VAL2
         GPT(IND_1,IND_2) = DCMPLX ( VAL1, VAL2 )
         GPT(IND_2,IND_1) = DCMPLX ( VAL1, VAL2 )
 410  CONTINUE 
!
      IUER = -1
      CALL READ_NETCDF_PHIS ( GEOS_TIN, L_LON, L_LAT, HEB_PHIS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1802, IUER, 'GEOS_GEN_EH', 'Failure in '// &
     &         'an attempt to read and parse file '//GEOS_TIN )
           CALL EXIT ( 1 )
      END IF
!
      HEB_GPT%DIMS = HEB_PHIS%DIMS 
!@      IF ( FL_GEOID_ONLY ) THEN
!@           HEB_GPT%DIMS(1) = 1440 
!@           HEB_GPT%DIMS(2) = 721
!@      END IF 
      ALLOCATE ( HEB_GPT%VAL(HEB_GPT%DIMS(1),HEB_GPT%DIMS(2),1,1), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_PHIS%DIMS(1)*HEB_PHIS%DIMS(2), STR )
           CALL ERR_LOG ( 1803, IUER, 'GEOS_GEN_EH', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memore for array HEB%VAL' )
           RETURN
      END IF
      HEB_GPT%STATUS  = HEB__ALLO
!
      IF ( FL_GEOID ) THEN
           IUER = -1
           CALL COMP_GPT_HEB ( MDEG, GPT, HEB_GPT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 1804, IUER, 'GEOS_GEN_EH', 'Failure in '// &
     &              'an attempt to compute geopotential at the reference '// &
     &              'ellipsoid' )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           HEB_GPT%VAL = 0.0
      END IF      
!
! --- Shift geoid from [0, 360], to [-180, 180]
!
      CALL GRID_2D_SHIFT_180_R4 ( INT(HEB_GPT%DIMS(1),KIND=4), &
     &                            INT(HEB_GPT%DIMS(2),KIND=4), &
     &                            HEB_GPT%VAL )
!
! --- HEB%PHIS keeps the geopotential. Dividing it by the *nominal*
! --- gravity acceleration, we get the height above the geoid.
! --- Adding the height of the geoid wrt the reference ellipsoid,
! --- we transform HEB%PHIS to the elevation above the WGS84 reference ellipsoid
!
      IF ( FL_GEOID ) THEN
           HEB_PHIS%VAL = HEB_PHIS%VAL/ACCREF__MAPL + HEB_GPT%VAL
         ELSE 
           HEB_PHIS%VAL = HEB_PHIS%VAL/ACCREF__MAPL
      END IF
!
! --- Generic parameters
!
      HEB_PHIS%UTC = 0.0
      HEB_PHIS%TAI = 0.0
      HEB_PHIS%MJD = J2000__MJD
      HEB_PHIS%DIMS(1) = L_LON
      HEB_PHIS%DIMS(2) = L_LAT
      HEB_PHIS%DIMS(3) =     1
      HEB_PHIS%DIMS(4) =     1
      HEB_PHIS%DATA_OFFSET = HEB__HDS
      HEB_PHIS%ENDIAN      = HEB__LE
      HEB_PHIS%DATA_TRANSFORM = HEB__NONE
      HEB_PHIS%FILL_VALUE     = 1.0E15
      HEB_PHIS%OFFSET         = 0.0
      HEB_PHIS%SCALE_FACTOR   = 1.0
      HEB_PHIS%DATA_COMPRESSION = HEB__NONE
      IF ( FL_GEOID ) THEN
           HEB_PHIS%SDS_NAME       = 'height above the WGS84 ellipsoid'
         ELSE 
           HEB_PHIS%SDS_NAME       = 'height above the WGS84 geoid'
      END IF
      HEB_PHIS%UNITS          = 'm'
      HEB_PHIS%DATA_FORMAT    = HEB__R4
      HEB_PHIS%MIN_VALUE      = MINVAL(HEB_PHIS%VAL)
      HEB_PHIS%MAX_VALUE      = MAXVAL(HEB_PHIS%VAL)
      HEB_PHIS%VALID_RANGE(1) = HEB_PHIS%MIN_VALUE      
      HEB_PHIS%VALID_RANGE(2) = HEB_PHIS%MAX_VALUE      
      HEB_PHIS%Prod_date_time = GET_CDATE()
!
! --- Write "height above the ellipsoid: in the output file
!
      IUER = -1
      CALL WRITE_HEB ( HEB_PHIS, HEB_PHIS%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
     &         'in an attempt to write into output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, '(A)' ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
!
!      HEB_PHIS%VAL = HEB_PHIS%VAL/9.80665D0 + 
!      IUER = -1
!      HEB_PHIS%VAL = HEB_PHIS%VAL/9.80665D0
!      HEB_GPT%VAL  = HEB_GPT%VAL
!
!
! --- Shift again to make geoid from [-180, 180] to [0, 360]
!
      CALL GRID_2D_SHIFT_180_R4 ( INT(HEB_GPT%DIMS(1),KIND=4), &
     &                            INT(HEB_GPT%DIMS(2),KIND=4), &
     &                            HEB_GPT%VAL )
      IF ( FL_GEOID ) THEN
           STR = '/tmp/foo'
           VAL_MIN =  1.0
           VAL_MAX = -1.0
           CALL PLOT_GRID_R4 ( 1, 7, 0, 1, INT(HEB_GPT%DIMS(1),KIND=4), &
     &                         INT(HEB_GPT%DIMS(2),KIND=4), &
     &                         HEB_GPT%VAL, 'EGM96 geoid height', 'm^2/s^2', &
     &                         VAL_MIN, VAL_MAX, STR, IUER )
      END IF
!      CALL PLOT_GRID_R4 ( 1, 7, 0, 1, L_LON, L_LAT, &
!     &                    HEB_PHIS%VAL, 'PHIS', 'm^2/s^2', STR, IUER )
!
!
      HEB_GPT%UTC = 0.0
      HEB_GPT%TAI = 0.0
      HEB_GPT%MJD = J2000__MJD
      HEB_GPT%DATA_OFFSET = HEB__HDS
      HEB_GPT%ENDIAN      = HEB__LE
      HEB_GPT%DATA_TRANSFORM = HEB__NONE
      HEB_GPT%FILL_VALUE     = 1.0E15
      HEB_GPT%OFFSET         = 0.0
      HEB_GPT%SCALE_FACTOR   = 1.0
      HEB_GPT%DATA_COMPRESSION = HEB__NONE
      HEB_GPT%SDS_NAME       = 'geoid height above the WGS84 ellipsoid'
      HEB_GPT%TITLE          = 'geoid height above the WGS84 ellipsoid'
      HEB_GPT%PROD_NAME      = 'EGM96 Earth gravity field'
      HEB_GPT%UNITS          = 'm'
      HEB_GPT%VERSION_ID     = '1'
      HEB_GPT%DATA_FORMAT    = HEB__R4
      HEB_GPT%MIN_VALUE      = MINVAL(HEB_GPT%VAL)
      HEB_GPT%MAX_VALUE      = MAXVAL(HEB_GPT%VAL)
      HEB_GPT%VALID_RANGE(1) = HEB_GPT%MIN_VALUE      
      HEB_GPT%VALID_RANGE(2) = HEB_GPT%MAX_VALUE      
      HEB_GPT%Prod_date_time = GET_CDATE()
!
! --- Write "height above the ellipsoid: in the output file
!
      IF ( FL_GEOID ) THEN
           FILOUT = '/tmp/geoid.heb'
           IUER = -1
           CALL WRITE_HEB ( HEB_GPT, HEB_GPT%VAL, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7206, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
     &              'in an attempt to write into output file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
           WRITE ( 6, * ) 'Wrote file '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_NETCDF_PHIS ( FILIN, L_LON, L_LAT, HEB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_NETCDF_PHIS
! *                                                                      *
! * ### 11-JAN-2013  READ_NETCDF_PHIS v1.0 (c) L. Petrov 02-FEB-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      INTEGER*4  L_LON, L_LAT, IUER
      CHARACTER  FILIN*128
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*4  IS, NCID, ID_DIM_LON, ID_DIM_LAT, &
     &                     ID_VAR_LAT, ID_VAR_LON, ID_VAR_PHIS
      REAL*8     TIM_BEG, TIM_END, SWAP
      CHARACTER  STR*8192, STR_DAT*32, STR_TIM*32, DATE_STR*32
      INTEGER*4  J1, J2, J3, MJD_BEG, MJD_END, ID, IP, ILON, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      HEB%STATUS = HEB__UNDF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
!
! --- Learn the ID of dimensions 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           IS = NF_INQ_DIMID ( NCID, 'XDim:EOSGRID', ID_DIM_LON )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1821, IUER, 'READ_NETCDF_PHIS', 'Error in an '// &
     &         'attempt to open the netcf file with latent flux '// &
     &          FILIN(1:I_LEN(FILIN))//' NF_OPEN: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'lat'
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           IS = NF_INQ_DIMID ( NCID, 'YDim:EOSGRID', ID_DIM_LAT )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1822, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, L_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1823, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         'getting the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, L_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1824, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         'getting the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL NOUT ( SIZEOF(HEB), HEB )
      HEB%STATUS  = HEB__UNDF
      HEB%DIMS(1) = L_LON
      HEB%DIMS(2) = L_LAT
      HEB%DIMS(3) = 1
      HEB%DIMS(4) = 1
!
      ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4), STR )
           CALL ERR_LOG ( 1825, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memore for array HEB%VAL' )
           RETURN
      END IF
      HEB%STATUS  = HEB__ALLO
!
! --- Learn the ID of the variable "PHIS"
!
      IS = NF_INQ_VARID ( NCID, 'PHIS', ID_VAR_PHIS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1826, IUER, 'READ_NETCDF_PHIS', 'Variable '// &
     &         '"PHIS" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_PHIS, HEB%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1827, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         'getting the values of the variable "PHIS" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB%STATUS  = HEB__LOAD
!
      CALL CLRCH ( HEB%PROD_NAME )
      IP = LINDEX ( FILIN, '/' ) + 1
      ID = LINDEX ( FILIN, '.' ) - 1
      IF ( ID < IP ) ID = IP
      ID = LINDEX ( FILIN(1:ID), '.' ) - 1
      IF ( ID < IP ) ID = IP
      HEB%PROD_NAME  = FILIN(IP:ID)
!
      CALL CLRCH ( HEB%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'history', HEB%HISTORY )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB%HISTORY )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1828, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'source', HEB%SOURCE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB%SOURCE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1829, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB%SOURCE, HEB%SOURCE )
!
      CALL CLRCH ( HEB%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'title', HEB%TITLE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB%TITLE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1830, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution', HEB%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB%INSTITUTION )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1831, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'references', HEB%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB%REFERENCES )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1832, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         'an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'comment', STR )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'Comment', STR )
      END IF
      STR = HEB%VERSION_ID 
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 1833, IUER, 'READ_NETCDF_PHIS', 'Error in '// &
     &         'an attempt to read attribute VersionID: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%FILE_NAME )
      HEB%FILE_NAME  = FILIN(IP:)
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_NETCDF_PHIS  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COMP_GPT_HEB ( DEG, GPT, HEB_GPT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_GPT_HEB 
! *                                                                      *
! *  ### 07-FEB-2013  COMP_GPT_HEB  v1.1 (c)  L. Petrov 27-AUG-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_GPT
      INTEGER*4  DEG, IUER
      COMPLEX*16 GPT(0:DEG,0:DEG)
      REAL*8     REA, FLAT, GM, OMEGA, EXC_SQ, ACC_EQU, GRV_LAT 
      PARAMETER  ( REA    = REA__WGS84  )
      PARAMETER  ( FLAT   = FLAT__WGS84 )
      PARAMETER  ( GM     = GM__EGM96   )
      PARAMETER  ( OMEGA  = OMEGA__EGM96  )
      PARAMETER  ( EXC_SQ = 2*FLAT - FLAT**2 )
      PARAMETER  ( ACC_EQU = ACC_EQU__WGS84 ) ! Equatorial gravity acc.
      PARAMETER  ( GRV_LAT = GRV_LAT__WGS84 ) ! D(ACC_EQU)/D(phi)
      REAL*8     LAT, LAT_GCN, LON, DST, G_ACC
      COMPLEX*16 GPT_USE(0:DEG,0:DEG)
      INTEGER*4  J1, J2, J3, J4, IER
      ADDRESS__TYPE :: FSH
      REAL*8,    EXTERNAL :: SPHE_COMP_VAL 
      ADDRESS__TYPE, EXTERNAL :: SPHE_INIT 
!
      CALL ERR_PASS ( IUER, IER )
      FSH = SPHE_INIT ( -1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1841, IUER, 'COMP_GPT_HEB', 'Error in an attempt '// &
     &         'to initialize FSH' )
           RETURN 
      END IF
      DO 410 J1=1,HEB_GPT%DIMS(2)
         LAT = -P2I + (J1-1)*PI__NUM/(HEB_GPT%DIMS(2)-1)
         IF ( J1 == 1 .OR. J1 == HEB_GPT%DIMS(2)   ) THEN
              LAT_GCN = LAT
            ELSE 
              LAT_GCN = DATAN ( (1.0D0-EXC_SQ)*DTAN(LAT) )
         END IF
         DO 420 J2=1,HEB_GPT%DIMS(1)
            LON = (J2-1)*PI2/HEB_GPT%DIMS(1)
            DST = REA*DSQRT ( (1.D0 + (EXC_SQ**2 - 2.D0*EXC_SQ)*DSIN(LAT)**2)/ &
     &                        (1.D0 - EXC_SQ*DSIN(LAT)**2) )
            DO 430 J3=0,DEG
               DO 440 J4=0,DEG
                  GPT_USE(J4,J3) = ((REA/DST)**J4)*GPT(J4,J3)
 440           CONTINUE 
 430        CONTINUE 
!
! --------- Compute the geopotential at the ellipsoid with respect to the geocenter
!
            CALL ERR_PASS ( IUER, IER )
            HEB_GPT%VAL(J2,J1,1,1) = GM/DST* &
     &            ( 1.0D0 + SPHE_COMP_VAL ( %VAL(FSH), DEG, DEG, LAT_GCN, LON, &
     &                                      1, 1, GPT_USE,  IER ) ) &
     &            + 0.5D0*OMEGA**2*DST**2*DCOS(LAT_GCN)**2
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1842, IUER, 'COMP_GPT_HEB', 'Error in '// &
     &               'computing geopotential' )
                 RETURN 
            END IF
!
! --------- Compue G_ACC -- gravity accelration at the reference ellipsid WGS84
!
            G_ACC = ACC_EQU* (1.D0 + GRV_LAT*DSIN(LAT)**2 ) / &
     &                DSQRT  (1.D0 - EXC_SQ*DSIN(LAT)**2  )
!
! --------- Compute the height above the reference ellipsoid that we need to
! --------- climb in order to compensate the difference between the gepotential
! --------- at a given point and the reference geopotential 
!
            HEB_GPT%VAL(J2,J1,1,1) = (HEB_GPT%VAL(J2,J1,1,1) - W0__IAU2004)/G_ACC
 420     CONTINUE 
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_GPT_HEB  !#!  
