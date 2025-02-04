      PROGRAM    SC_GEN
! ************************************************************************
! *                                                                      *
! *   Program  SC_GEN  computes the sample correction for the spherical  *
! *   harmonics of the global pressure field of a given degree and given *
! *   order. The sampling correction is the residual displacement at     *
! *   that hadmonics due to undersamnpling the coastal line. Dimension:  *
! *   m/Pa.                                                              *
! *                                                                      *
! * ### 03-FEB-2016      SC_GEN     v2.0 (c)  L. Petrov  07-FEB-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LS_BT, HEB_OUT
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      CHARACTER  FIL_LS_BT*128, FIL_LS_SAM*128, FIL_LOVE*128, &
     &           WISDOM_FILE*128, FIL_OUT*128, STR_DEG*3, STR_ORD*3, &
     &           STR*128, STR1*128, MODE_STR*16
      CHARACTER    SC__VERSION*30
      PARAMETER  ( SC__VERSION = 'SC_GEN   Version of 2016.02.03' )
      INTEGER*4    MIND
      PARAMETER  ( MIND = 8 )
      REAL*4,    ALLOCATABLE :: LON_FULL(:), LON_SAM(:), LAT_FULL(:), LAT_SAM(:)     
      INTEGER*4  J1, J2, J3, J4, LIND, IND(2,MIND), DEG, ORD, DEG_FULL, DEG_SAM, &
     &           IVRB, NUM_THR, ID, IUER 
      REAL*8     TIM_R8 
      LOGICAL*1  FL_FULL
      INTEGER*8  FSH
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      INTEGER*8, EXTERNAL :: SPHE_INIT_PLAN 
      REAL*8,    EXTERNAL :: WALL_TIMER
!
!     $MALO_DIR/bin/sc_gen /s0/temp/load/ocean_ls_bt_d10799_d2699.heb $MALO_DIR/share/Love_load_32768_cm.dat 1 0 '/s0/sc_load/ocean_*' sam
!     /s0/sc_load> $MALO_DIR/bin/sc_gen /s0/temp/load/ocean_ls_bu_d2699.heb $MALO_DIR/share/Love_load_32768_cm.dat 1 0 /s0/sc_load/mod44w_sc_by_21599_2699.heb sam
!
      WISDOM_FILE = '/progs/malo_20151228/share/malo_fftw_plan_16thr.wis'
      NUM_THR = 16
      IVRB = 2
!
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: fil_ls_bt fil_love deg ord heb_out sam/full'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_LS_BT )
           CALL GETARG ( 2, FIL_LOVE  )
           CALL GETARG ( 3, STR_DEG   )
           CALL CHIN   ( STR_DEG, DEG )
           CALL GETARG ( 4, STR_ORD   )
           CALL CHIN   ( STR_ORD, ORD )
           CALL GETARG ( 5, FIL_OUT   )
           CALL GETARG ( 6, MODE_STR  )
      END IF
      IF ( DEG < 0 .OR. DEG > MALO__MDIM ) THEN
           IUER = -1
           CALL ERR_LOG ( 6401, IUER, 'SC_GEN', 'Wrong degree '//STR )
           CALL EXIT ( 1 )
      END IF
      IF ( ORD < 0 .OR. ORD > DEG ) THEN
           IUER = -1
           CALL ERR_LOG ( 6402, IUER, 'SC_GEN', 'Wrong degree '//STR )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Started procrssing SC for '// &
     &                    ' degree: '//STR_DEG(1:I_LEN(STR_DEG))// &
     &                    ' order: '//STR_ORD(1:I_LEN(STR_ORD))
           CALL FLUSH ( 6 )
      END IF 
      IF ( MODE_STR == 'full' ) THEN
           FL_FULL = .TRUE.
         ELSE IF ( MODE_STR == 'sam' ) THEN
           FL_FULL = .FALSE.
         ELSE 
           CALL ERR_LOG ( 6403, IUER, 'SC_GEN', 'Wrong the 6th argument '// &
     &          MODE_STR(1:I_LEN(MODE_STR))//' expected full or sam' ) 
           CALL EXIT ( 1 )
      END IF
      TIM_R8 = WALL_TIMER ( %VAL(0) )
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6404, IUER, 'SC_GEN', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6405, IUER, 'SC_GEN', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialization of the spherical harmonics package
!
      IUER = -1
      FSH = SPHE_INIT_PLAN ( WISDOM_FILE, 1, 2.0D0, NUM_THR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6406, IUER, 'SC_GEN', 'Error in an attempt to '// &
     &         'initialize FSH object for spherical harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
!
      MAL(1)%CONF%LOVE_FILE = FIL_LOVE
      IUER = -1
      CALL READ_LOVE ( MAL(1), IUER ) 
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6407, IUER, 'SC_GEN', 'Error in '// &
     &         'an attempt to read a file with Love numbers '// &
     &          MAL(1)%CONF%LOVE_FILE )
           CALL EXIT ( 1 )
      END IF   
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Reading full ls mask...'
           CALL FLUSH ( 6 )
      END IF 
      IUER = -1
      CALL READ_HEB ( FIL_LS_BT, HEB_LS_BT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6408, IUER, 'SC_GEN', 'Failed to read full '// &
     &         'blackman truncated land/sea mask'//FIL_LS_BT )
           CALL EXIT ( 1 )
      END IF
      DEG_FULL = HEB_LS_BT%DIMS(1)/4 - 1 
!
      CALL EXWORD ( HEB_LS_BT%COMMENT(1), MIND, LIND, IND, CHAR(0)//CHAR(32), IUER )
      IF ( LIND < 5 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6409, IUER, 'SC_GEN', 'Trap of internal control: '// &
     &         'COMMENT(1) has less than 5 words in the truncated land/sea '// &
     &         'mask '//FIL_LS_BT )
           CALL EXIT ( 1 )
      END IF
!
      CALL CHIN ( HEB_LS_BT%COMMENT(1)(IND(1,5):IND(2,5)), DEG_SAM )
      IF ( DEG > DEG_SAM ) THEN
           IUER = -1
           CALL ERR_LOG ( 6410, IUER, 'SC_GEN', 'The third parameters DEG '// &
     &         'is to large: more than the degree of the sampling land/sea '// &
     &         'mask '//FIL_LS_SAM )
           CALL EXIT ( 1 )
      END IF
      IF ( FL_FULL ) DEG_SAM = DEG_FULL
!
      HEB_OUT%DIMS(1) = (DEG_SAM+1)*4
      HEB_OUT%DIMS(2) = (DEG_SAM+1)*2 + 1
      HEB_OUT%DIMS(3) = 3
      IF ( ORD == 0 ) THEN
           HEB_OUT%DIMS(4) = 1
         ELSE 
           HEB_OUT%DIMS(4) = 2
      END IF
!
      IF ( INDEX ( FIL_OUT, '*' ) > 0 ) THEN
           ID = LINDEX ( FIL_OUT, '*' ) - 1
           CALL CHASHR ( STR_DEG )
           CALL CHASHR ( STR_ORD )
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG_FULL, STR  )
           CALL INCH  ( DEG_SAM,  STR1 )
           FIL_OUT = FIL_OUT(1:ID)//'sc_'//STR(1:I_LEN(STR))//'_'// &
     &               STR1(1:I_LEN(STR1))//'_'//STR_DEG//'_'//STR_ORD// &
     &               '.heb'
           CALL BLANK_TO_ZERO ( FIL_OUT(1:I_LEN(FIL_OUT)) )
           WRITE ( 6, * ) 'Output file will be '//FIL_OUT(1:I_LEN(FIL_OUT)) 
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Compute sampling correction...'
           CALL FLUSH ( 6 )
      END IF 
!
      IUER = -1
      CALL COMP_SC ( DEG_SAM, DEG, ORD, %VAL(FSH), MAL(1), HEB_LS_BT, &
     &               HEB_OUT, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6411, IUER, 'SC_GEN', 'Failure in '// &
     &         'computation of the sampling correction' )
           CALL EXIT ( 1 )
      END IF
!
      HEB_OUT%DATA_OFFSET      = HEB__HDS
      HEB_OUT%ENDIAN           = HEB__LE
      HEB_OUT%DATA_TRANSFORM   = HEB__NONE
      HEB_OUT%FILL_VALUE       = 1.0E15
      HEB_OUT%OFFSET           = 0.0
      HEB_OUT%SCALE_FACTOR     =  1.0
      HEB_OUT%DATA_COMPRESSION = HEB__NONE
      HEB_OUT%SDS_NAME         = 'Sampler correction for loading in the ocean'
      HEB_OUT%UNITS            = 'm/Pa'
      HEB_OUT%DATA_FORMAT      = HEB__R4
      HEB_OUT%VALID_RANGE(1)   = HEB_OUT%MIN_VALUE        
      HEB_OUT%VALID_RANGE(2)   = HEB_OUT%MAX_VALUE        
      HEB_OUT%PROD_DATE_TIME   = GET_CDATE()
      HEB_OUT%COMMENT(1)       = 'Pressure spherical harmonic expansion'// &
     &                           ' degree: '//STR_DEG(1:I_LEN(STR_DEG))// &
     &                           ' order: '//STR_ORD(1:I_LEN(STR_ORD))
      CALL CLRCH ( STR )
      CALL INCH  ( DEG_FULL, STR )
      HEB_OUT%COMMENT(2)       = 'Degree of the full dataset '//STR(1:I_LEN(STR))
      CALL CLRCH ( STR )
      CALL INCH8 ( DEG_SAM, STR )
      HEB_OUT%COMMENT(3)       = 'Degree of the sampling dataset '//STR(1:I_LEN(STR))
!
      HEB_OUT%FILE_NAME        = FIL_OUT
      HEB_OUT%HISTORY          = ' '
      HEB_OUT%SOURCE           = 'Model data' 
      HEB_OUT%TITLE            = HEB_OUT%SDS_NAME
      HEB_OUT%PROD_NAME        = HEB_OUT%SDS_NAME
      HEB_OUT%INSTITUTION      = 'Astrogeo Center'
      HEB_OUT%REFERENCES       = 'http://astrogeo.org/malo/'
      HEB_OUT%VERSION_ID       = SC__VERSION
      HEB_OUT%MJD              = J2000__MJD
      HEB_OUT%UTC              = 0.0
      HEB_OUT%TAI              = 0.0
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Write sampling correction...'
           CALL FLUSH ( 6 )
      END IF 
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6412, IUER, 'SC_GEN', 'Failure in '// &
     &         'writing sampling correction into the output file '// &
     &          FIL_OUT )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Wrote output file '// &
     &                    FIL_OUT(1:I_LEN(FIL_OUT))
           TIM_R8 = WALL_TIMER ( %VAL(2) )
           WRITE ( 6, 120 ) STR_DEG, STR_ORD, TIM_R8
 120       FORMAT ( 1X,'Computation of sampling correction of degree ', A, &
     &                 ' order ',A, ' took ', F8.1, ' sec' )
           CALL FLUSH ( 6 )
      END IF 
!
      END  PROGRAM   SC_GEN !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COMP_SC ( DEG_SAM, DEG, ORD, FSH, MALO, HEB_LS_BT, &
     &                     HEB_OUT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_SC
! *                                                                      *
! *  ### 03-FEB-2016    COMP_SC    v1.0 (c)  L. Petrov  03-FEB-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( SPHE_TYPE  ) :: FSH
      TYPE     ( HEB__TYPE  ) :: HEB_LS_BT, HEB_OUT
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  DEG_SAM, DEG, ORD, IVRB, IUER
      REAL*8     YC, YS, LAT, LON
      REAL*8,    ALLOCATABLE :: MTY_FUN(:,:,:), MTY_SPH(:,:,:,:,:), &
     &           DSPL_R8(:,:,:,:), PLT(:), COS_LON(:), SIN_LON(:)
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, IR, DEG_FULL, &
     &           NLON_SAM, NLAT_SAM, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      DEG_FULL = HEB_LS_BT%DIMS(1)/4 - 1
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Compute ls-mask*Y ...'
           CALL FLUSH ( 6 )
      END IF 
!
      ALLOCATE ( MTY_FUN(HEB_LS_BT%DIMS(1),HEB_LS_BT%DIMS(2),HEB_OUT%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*INT8(2)*HEB_LS_BT%DIMS(1)*HEB_LS_BT%DIMS(2), STR )
           CALL ERR_LOG ( 6421, IUER, 'COMP_SC', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array MTY_FUN' )
           RETURN 
      END IF
      ALLOCATE ( MTY_SPH(2,0:DEG_FULL,0:DEG_FULL,2,HEB_OUT%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*INT8(2)**INT8(2)*INT8(2)*INT8(DEG_FULL+1)**2, STR )
           CALL ERR_LOG ( 6422, IUER, 'COMP_SC', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array MTY_SPH' )
           RETURN 
      END IF
      ALLOCATE ( PLT(HEB_LS_BT%DIMS(2)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*HEB_LS_BT%DIMS(2), STR )
           CALL ERR_LOG ( 6423, IUER, 'COMP_SC', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array PLT' )
           RETURN 
      END IF
      ALLOCATE ( COS_LON(HEB_LS_BT%DIMS(1)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*HEB_LS_BT%DIMS(1), STR )
           CALL ERR_LOG ( 6424, IUER, 'COMP_SC', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array COS_LON' )
           RETURN 
      END IF
      ALLOCATE ( SIN_LON(HEB_LS_BT%DIMS(1)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*HEB_LS_BT%DIMS(1), STR )
           CALL ERR_LOG ( 6425, IUER, 'COMP_SC', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array SIN_LON' )
           RETURN 
      END IF
!
      MTY_FUN = 0.0D0
      IR = (DEG_FULL+1)/(DEG_SAM+1)
      DO 410 J1=1,HEB_LS_BT%DIMS(2)
         LAT = -P2I + (J1-1)*( PI__NUM/(HEB_LS_BT%DIMS(2)-1) )
         IF ( DEG > 0 ) THEN
              CALL ERR_PASS ( IUER, IER ) 
              CALL SPHE_LEG_F ( FSH, DEG, 1, 1, LAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6426, IUER, 'COMP_SC', 'Error in '// &
     &                 'computing the value of Legendre polynomial' )
                   RETURN 
              END IF
              PLT(J1) = FSH%PLT(ORD,DEG)
            ELSE
              PLT(J1) = 1.0D0
         END IF
 410  CONTINUE 
      DO 420 J2=1,HEB_LS_BT%DIMS(1)
         LON = (J2-1)*PI2/HEB_LS_BT%DIMS(1)
         COS_LON(J2) = DCOS(ORD*LON)
         SIN_LON(J2) = DSIN(ORD*LON)
 420  CONTINUE 
      DO 430 J3=1,HEB_LS_BT%DIMS(2)
         DO 440 J4=1,HEB_LS_BT%DIMS(1)
            IF ( DEG == 0 .AND. ORD == 0 ) THEN
                 MTY_FUN(J4,J3,1) = HEB_LS_BT%VAL(J4,J3,1,1) 
               ELSE
                 YC   = PLT(J3)*COS_LON(J4)
                 MTY_FUN(J4,J3,1) = YC*HEB_LS_BT%VAL(J4,J3,1,1) 
                 IF ( HEB_OUT%DIMS(4) == 2 ) THEN
                      YS   = PLT(J3)*SIN_LON(J4)
                      MTY_FUN(J4,J3,2) = YS*HEB_LS_BT%VAL(J4,J3,1,1) 
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      IF ( ASSOCIATED ( FSH%AJ   ) ) DEALLOCATE ( FSH%AJ )
      IF ( ASSOCIATED ( FSH%F1   ) ) DEALLOCATE ( FSH%F1 )
      IF ( ASSOCIATED ( FSH%F2   ) ) DEALLOCATE ( FSH%F2 )
      IF ( ASSOCIATED ( FSH%F3   ) ) DEALLOCATE ( FSH%F3 )
      IF ( ASSOCIATED ( FSH%PL   ) ) DEALLOCATE ( FSH%PL )
      IF ( ASSOCIATED ( FSH%PLT  ) ) DEALLOCATE ( FSH%PLT )
      IF ( ASSOCIATED ( FSH%DPLT ) ) DEALLOCATE ( FSH%DPLT )
      IF ( ASSOCIATED ( FSH%MSIN ) ) DEALLOCATE ( FSH%MSIN )
      IF ( ASSOCIATED ( FSH%MCOS ) ) DEALLOCATE ( FSH%MCOS )
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Compute the 1st direct SHT ... '
           CALL FLUSH ( 6 )
      END IF 
      CALL ERR_PASS ( IUER, IER )
      CALL SPHE_DIR_2NN ( FSH, INT(HEB_LS_BT%DIMS(2)-1,KIND=4), MTY_FUN(1,1,1), &
     &                    DEG_FULL, DEG_FULL, 1, 1, MTY_SPH(1,0,0,1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6427, IUER, 'COMP_SC', 'Failure in computation'// &
     &         'of spherical harmonics transform' )
           RETURN 
      END IF
      IF ( HEB_OUT%DIMS(4) == 2 ) THEN
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, * ) GET_CDATE()//'  Compute the 2nd direct SHT ... '
                CALL FLUSH ( 6 )
           END IF 
!
           CALL ERR_PASS ( IUER, IER )
           CALL SPHE_DIR_2NN ( FSH, INT(HEB_LS_BT%DIMS(2)-1,KIND=4), MTY_FUN(1,1,2), &
     &                         DEG_FULL, DEG_FULL, 1, 1, MTY_SPH(1,0,0,1,2), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6428, IUER, 'COMP_SC', 'Failure in computation'// &
     &               'of spherical harmonics transform' )
                RETURN 
           END IF
      END IF
!
      DEALLOCATE ( MTY_FUN )
!
      NLON_SAM = (DEG_SAM+1)*4
      NLAT_SAM = (DEG_SAM+1)*2 + 1
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Scale with Love numbers ...'
           CALL FLUSH ( 6 )
      END IF 
!
      DO 450 J5=0,DEG_FULL     ! order
         DO 460 J6=J5,DEG_FULL ! degree
            DO 470 J7=1,2      ! 1: cos, 2: sin
               MTY_SPH(J7,J6,J5,2,1) = MTY_SPH(J7,J6,J5,1,1) * &
     &                                   3.0D0*MALO%LOVE(J6,MALO__L)/(2*J6+1)/ &
     &                                   (MALO__DENS*MALO__GRAV)
               MTY_SPH(J7,J6,J5,1,1) = MTY_SPH(J7,J6,J5,1,1) * &
     &                                   3.0D0*MALO%LOVE(J6,MALO__H)/(2*J6+1)/ &
     &                                   (MALO__DENS*MALO__GRAV)
               MTY_SPH(J7,J5,J6,1,1) = MTY_SPH(J7,J6,J5,1,1) 
               MTY_SPH(J7,J5,J6,2,1) = MTY_SPH(J7,J6,J5,2,1) 
!
               IF ( HEB_OUT%DIMS(4) == 2 ) THEN
                    MTY_SPH(J7,J6,J5,1,2) = MTY_SPH(J7,J6,J5,1,2) * &
     &                                        3.0D0*MALO%LOVE(J6,MALO__H)/(2*J6+1)/ &
     &                                        (MALO__DENS*MALO__GRAV)
                    MTY_SPH(J7,J6,J5,2,2) = MTY_SPH(J7,J6,J5,1,2) * &
     &                                        3.0D0*MALO%LOVE(J6,MALO__L)/(2*J6+1)/ &
     &                                        (MALO__DENS*MALO__GRAV)
!
                    MTY_SPH(J7,J5,J6,1,2) = MTY_SPH(J7,J6,J5,1,2) 
                    MTY_SPH(J7,J5,J6,2,2) = MTY_SPH(J7,J6,J5,2,2) 
               END IF
 470        CONTINUE 
 460     CONTINUE 
 450  CONTINUE 
!
      ALLOCATE ( DSPL_R8(NLON_SAM,NLAT_SAM,3,HEB_OUT%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*INT8(3)*INT8(2)*INT8(NLON_SAM)*INT8(NLAT_SAM), STR )
           CALL ERR_LOG ( 6429, IUER, 'COMP_SC', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array DSPL_R8' )
           RETURN 
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) GET_CDATE()//'  Compute the 1st inverse vector SHT ...'
           CALL FLUSH ( 6 )
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPHE_INV_2NN_VEC ( FSH, DEG_FULL, DEG_SAM, MTY_SPH(1,0,0,1,1), &
     &                        NLAT_SAM, DSPL_R8(1,1,1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6430, IUER, 'COMP_SC', 'Failure in computation '// &
     &         'of spherical harmonics transform' )
           RETURN 
      END IF
!
      IF ( HEB_OUT%DIMS(4) == 2 ) THEN
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, * ) GET_CDATE()//'  Compute the 2nd inverse vector SHT ...'
                CALL FLUSH ( 6 )
          END IF 
!
          CALL ERR_PASS ( IUER, IER )
          CALL SPHE_INV_2NN_VEC ( FSH, DEG_FULL, DEG_SAM, MTY_SPH(1,0,0,1,2), &
     &                            NLAT_SAM, DSPL_R8(1,1,1,2), IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6431, IUER, 'COMP_SC', 'Failure in computation '// &
     &             'of spherical harmonics transform' )
               RETURN 
          END IF
      END IF
      DEALLOCATE ( MTY_SPH  )
!
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0  ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH8 ( INT8(4)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)* &
     &                          HEB_OUT%DIMS(3)*HEB_OUT%DIMS(4), STR )
           IUER = -1
           CALL ERR_LOG ( 6432, IUER, 'COMP_SC', 'Error in an attempt '// &
     &         'to reserve '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for output array HEB_OUT' )
           CALL EXIT ( 1 )
      END IF
!
      HEB_OUT%MIN_VALUE = DSPL_R8(1,1,1,1) 
      HEB_OUT%MAX_VALUE = DSPL_R8(1,1,1,1) 
      DO 480 J8=1,HEB_OUT%DIMS(4)
         DO 490 J9=1,HEB_OUT%DIMS(3)
            DO 4100 J10=1,HEB_OUT%DIMS(2)
                DO 4110 J11=1,HEB_OUT%DIMS(1)
                   HEB_OUT%VAL(J11,J10,J9,J8) = DSPL_R8(J11,J10,J9,J8) 
                   IF ( HEB_OUT%VAL(J11,J10,J9,J8) < HEB_OUT%MIN_VALUE ) THEN
                        HEB_OUT%MIN_VALUE = HEB_OUT%VAL(J11,J10,J9,J8) 
                   END IF
                   IF ( HEB_OUT%VAL(J11,J10,J9,J8) > HEB_OUT%MAX_VALUE ) THEN
                        HEB_OUT%MAX_VALUE = HEB_OUT%VAL(J11,J10,J9,J8) 
                   END IF
 4110           CONTINUE 
 4100       CONTINUE 
 490     CONTINUE 
 480  CONTINUE 
!
      DEALLOCATE ( DSPL_R8 )
      DEALLOCATE ( COS_LON )
      DEALLOCATE ( SIN_LON )
      DEALLOCATE ( PLT     )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_SC  !#!  
