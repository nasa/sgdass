      PROGRAM  MERRANV_TO_DQTUV
! ************************************************************************
! *                                                                      *
! *   Program MERRANV_TO_DQTUV reads a given file with results of        *
! *   assimilation run of MERRA model at native resolution, extacts      *
! *   six datasets:                                                      *
! *   1) DELP (d) -- pressure_thickness                                  *
! *   2) QV (q)   -- specific_humidity                                   *
! *   3) T (t)    -- air_temperature                                     *
! *   4) U (u)    -- eastward_wind                                       *
! *   5) V (v)    -- northward_wind                                      *
! *                                                                      *
! *   encodes them in HEB format and writes in the output                *
! *   subdirectories directory six files: one file per parameter.        *
! *   Six subdirectories under the output are specifed:                  *
! *   /d, /q, /t, /u, /v                                                 *
! *                                                                      *
! *   Compression can be specified optinoally.                           *
! *                                                                      *
! *   Ussage: merranv_to_dgqtuv input_file output_directory [compr]      *
! *                                                                      *
! * ### 04-FEB-2013  MERRANV_TO_DQTUV v2.1 (c) L. Petrov 22-MAY-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INTEGER*4    MLON, MLON2, MLAT, MLEV, MTIM, MTIM2, MP
      PARAMETER  ( MLON  =  540 )
      PARAMETER  ( MLON2 =  576 )
      PARAMETER  ( MLAT  =  361 )
      PARAMETER  ( MLEV  =   72 )
      PARAMETER  ( MTIM  =    4 )
      PARAMETER  ( MTIM2 =    4 )
      PARAMETER  (   MP  =    5 )
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*4,    ALLOCATABLE :: DELP(:,:,:,:), T(:,:,:,:), QV(:,:,:,:), &
     &                          U(:,:,:,:),    V(:,:,:,:)
      LOGICAL*1  LEX
      INTEGER*8  DIR_DESC, IP, IS
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      INTEGER*4  J1, J2, J3, LLON, LTIM, IUER
      REAL*8     TIM_STEP, UTC_0
      CHARACTER  FILIN*128, FILOUT*128, DIROUT*128, DIR*128, STR*128, &
     &           COMPR*16, COMPR_COM*64, COM_STR*256, SUBDIR(MP)*1
      DATA       SUBDIR /   &
     &                 'd', &
     &                 'q', &
     &                 't', &
     &                 'u', &
     &                 'v'  &
     &           /
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
      INTEGER*4, EXTERNAL :: MKDIR, I_LEN, ILEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      COMPR = 'none'
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage merranv_to_dqtuv filin output_directory [compr]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, DIROUT )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, COMPR ) 
           END IF
      END IF
      IF ( COMPR == 'none' ) THEN
           CONTINUE 
           CALL CLRCH ( COMPR_COM )
         ELSE IF ( COMPR == 'gzip' ) THEN
           COMPR_COM = 'gzip -1 -f '
         ELSE IF ( COMPR == 'bzip2' ) THEN
           COMPR_COM = 'bzip2 -9 -f '
         ELSE IF ( COMPR == 'pbzip2' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -f '
         ELSE IF ( COMPR == 'pbzip2_p1' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
         ELSE IF ( COMPR == 'lbzip2' ) THEN
           COMPR_COM= 'lbzip2 -9 -f '
         ELSE IF ( COMPR == 'lbzip2_p1' ) THEN
           COMPR_COM= 'lbzip2 -9 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_1' ) THEN
           COMPR_COM= 'lbzip2 -1 -f '
         ELSE IF ( COMPR == 'lbzip2_1p1' ) THEN
           COMPR_COM= 'lbzip2 -1 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_2p1' ) THEN
           COMPR_COM = 'lbzip2 -2 -n1 -f '
         ELSE 
           CALL ERR_LOG ( 7201, -2, 'MERRANV_TO_DQTUV', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
      IF ( INDEX ( FILIN, 'MERRA2' ) > 0 ) THEN
           LTIM = MTIM2
           LLON = MLON2
         ELSE
           LTIM = MTIM
           LLON = MLON
      END IF
!
! --- Allocate memory for arrays with datasets
!
      ALLOCATE ( DELP(LLON,MLAT,MLEV,LTIM) )
      ALLOCATE ( T(LLON,MLAT,MLEV,LTIM)    )
      ALLOCATE ( QV(LLON,MLAT,MLEV,LTIM)   )
      ALLOCATE ( U(LLON,MLAT,MLEV,LTIM)    )
      ALLOCATE ( V(LLON,MLAT,MLEV,LTIM)    )
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7201, -2, 'MERRANV_TO_DQTUV', 'Cannot find '// &
     &         'input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7202, -2, 'MERRANV_TO_DQTUV', 'Output '// &
     &         'directory '//DIROUT(1:I_LEN(DIROUT))//' does not exist' )
           CALL EXIT ( 1 )
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check whether the output subdirectory exists
!
      DO 410 J1=1,MP
         DIR = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(J1)
         DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
         IF ( DIR_DESC .LE. 0 ) THEN
              IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
              IF ( IS .EQ. -1 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7203, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &                 'in an attempt to create output directory '//DIR )
                   CALL EXIT ( 1 )
              END IF
            ELSE 
              IP = CLOSEDIR ( %VAL(DIR_DESC) )
         END IF
 410  CONTINUE 
!
! --- Parse the input file
!
      IUER = -1
      CALL READ_MERRANV_DQTUV ( FILIN, LLON, MLAT, MLEV, LTIM, &
     &                          DELP, QV, T, U, V, HEB, TIM_STEP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7204, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &         'in an attempt to parse the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
      UTC_0 = HEB%UTC
      DO 420 J2=1,LTIM
!
! ------ Generic parameters
!
         HEB%UTC     = UTC_0 + (J2-1)*TIM_STEP
         HEB%TAI     = UTC_0 + (J2-1)*TIM_STEP
         HEB%DIMS(1) = LLON
         HEB%DIMS(2) = MLAT
         HEB%DIMS(3) = MLEV
         HEB%DIMS(4) =    1
         HEB%DATA_OFFSET = HEB__HDS
         HEB%ENDIAN      = HEB__LE
         HEB%DATA_TRANSFORM = HEB__NONE
         HEB%FILL_VALUE     = 1.0E15
         HEB%OFFSET         = 0.0
         HEB%SCALE_FACTOR   = 1.0
         HEB%DATA_COMPRESSION = HEB__NONE
!
         IUER = -1
         STR = MJDSEC_TO_DATE ( HEB%MJD, HEB%UTC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7205, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &            'in an attempt to parse the input file '//FILIN )
              CALL EXIT ( 1 )
         END IF
!
! ------ Update headers for "pressure thickness" variable
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)//'/'//SUBDIR(1)//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
!
         HEB%SDS_NAME       = 'pressure_thickness'
         HEB%UNITS          = 'Pa'
         HEB%DATA_FORMAT    = HEB__I2
         HEB%MIN_VALUE      = MINVAL(DELP)
         HEB%MAX_VALUE      = MAXVAL(DELP)
         HEB%VALID_RANGE(1) = 0.0
         HEB%VALID_RANGE(2) = 6400.0
         HEB%OFFSET         = 3200.0
         HEB%SCALE_FACTOR   = 6400.0/64000.0
!
! ------ Write "pressure thickness variable"
!
         IUER = -1
         CALL WRITE_HEB ( HEB, DELP(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7205, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
!
! ------ Specific humidity
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(2)//'/'//SUBDIR(2)//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
         HEB%SDS_NAME     = 'specific_humidity'
         HEB%UNITS        = 'dimensionless'
         HEB%DATA_FORMAT  = HEB__I2
         HEB%VALID_RANGE(1) = EXP(-34.0)
         HEB%VALID_RANGE(2) = EXP(-2.0)
         HEB%MIN_VALUE      = MAX ( MINVAL(QV), HEB%VALID_RANGE(1) )
         HEB%MAX_VALUE      = MIN ( MAXVAL(QV), HEB%VALID_RANGE(2) )
         HEB%DATA_TRANSFORM = HEB__LOG
         HEB%OFFSET         = -18.0
         HEB%SCALE_FACTOR   = 32.0/64000.0
         HEB%DIMS(3)        = MLEV
!
!!  call check_qv ( heb%dims(1), heb%dims(2), heb%dims(3), qv(1,1,1,j2) ) ! %%%%%
!
         IUER = -1
         CALL WRITE_HEB ( HEB, QV(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7205, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
!
! ------ Air temperature
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(3)//'/'//SUBDIR(3)//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
         HEB%SDS_NAME       = 'air_temperature'
         HEB%UNITS          = 'K'
         HEB%DATA_FORMAT    = HEB__I2
         HEB%VALID_RANGE(1) =   0.0
         HEB%VALID_RANGE(2) = 512.0
         HEB%MIN_VALUE      = MINVAL(T)
         HEB%MAX_VALUE      = MAXVAL(T)
         HEB%DATA_TRANSFORM = HEB__SCOF
         HEB%OFFSET         = 256.0
         HEB%SCALE_FACTOR   = 512.0/64000.0
         HEB%DIMS(3)        = MLEV
!
         IUER = -1
         CALL WRITE_HEB ( HEB, T(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7205, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
!
! ------ Eastward wind
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(4)//'/'//SUBDIR(4)//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
         HEB%SDS_NAME     = 'eastward_wind'
         HEB%UNITS        = 'm/s'
         HEB%DATA_FORMAT  = HEB__I2
         HEB%VALID_RANGE(1) = -256.0
         HEB%VALID_RANGE(2) =  256.0
         HEB%MIN_VALUE      = MINVAL(U)
         HEB%MAX_VALUE      = MAXVAL(U)
         HEB%DATA_TRANSFORM = HEB__SCOF
         HEB%OFFSET         = 0.0
         HEB%SCALE_FACTOR   = 512.0/64000.0
         HEB%DIMS(3)        = MLEV
!
         IUER = -1
         CALL WRITE_HEB ( HEB, U(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7205, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
!
! ------ North-ward wind
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(5)//'/'//SUBDIR(5)//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
         HEB%SDS_NAME     = 'northward_wind'
         HEB%UNITS        = 'm/s'
         HEB%DATA_FORMAT  = HEB__I2
         HEB%VALID_RANGE(1) = -256.0
         HEB%VALID_RANGE(2) =  256.0
         HEB%MIN_VALUE      = MINVAL(V)
         HEB%MAX_VALUE      = MAXVAL(V)
         HEB%DATA_TRANSFORM = HEB__SCOF
         HEB%OFFSET         = 0.0
         HEB%SCALE_FACTOR   = 512.0/64000.0
         HEB%DIMS(3)        = MLEV
!
         IUER = -1
         CALL WRITE_HEB ( HEB, V(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7205, -2, 'MERRANV_TO_DQTUV', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
 420  CONTINUE 
!
      END  PROGRAM  MERRANV_TO_DQTUV  !#!  
!
! ------------------------------------------------------------------------
!
      subroutine check_qv ( nlon, nlat, nlev, qv ) ! %%%%%
      implicit   none 
      integer*8  nlon, nlat, nlev
      real*4     qv(nlon,nlat,nlev)
      real*4     qv_lim
      parameter  ( qv_lim = 1.0e-12 )
      integer*4  j1, j2, j3
   write ( 6, * ) 'Start qv check'
      do 410 j1=1,nlev
         do 420 j2=1,nlat
            do 430 j3=1,nlon
               if ( j3 == 51 .and. j2 == 163 ) then
                    write ( 6, 120 ) j3, j2, j1, qv(j3,j2,j1)  !%%%%%%%%%%%%%%%%%%%%
 120                format ( 'INDS: lat/lon/lev ', i3, 1x, i3, 1x, i3, ' qv= ', 1pe14.7 ) ! %%%%
               end if
               if ( qv(j3,j2,j1) < qv_lim ) then
                    write ( 6, 110 ) -180 + (j3-1)*0.666667, -90 + (j2-1)*0.5, j3, j2, j1, qv(j3,j2,j1)  !%%%%%%%%%%%%%%%%%%%%
 110                format ( 'lon: ', f8.3, ' lat: ', f7.3, ' ind: lat/lon/lev ', i3, 1x, i3, 1x, i3, ' qv= ', 1pe14.7 ) ! %%%%
               end if
 430        continue 
 420     continue 
 410  CONTINUE 
      return
      end  !#!  
