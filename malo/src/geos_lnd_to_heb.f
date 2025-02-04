      PROGRAM    GEOS_LND_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program GEOS_LND_TO_HEB  reads GEOS files with suffix LND,         *
! *   extract parameter: twland  and writes it in                        *
! *   subdirectory twland of the output directory. The output            *
! *   is written in HEB format.                                          *
! *                                                                      *
! * ### 01-MAY-2013  GEOS_LND_TO_HEB v2.3 (c) L. Petrov 22-MAY-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_TWLAND, HEB_TWLAND_720
      INTEGER*4    MPAR
      PARAMETER  ( MPAR =    1 )
      CHARACTER    FILIN*128, DIROUT*128, FILOUT*128, STR*128, &
     &             DIR*128, SUBDIR(MPAR)*8, COM_STR*256, COMPR*16, &
     &             COMPR_COM*64
      DATA         SUBDIR /            &
     &                     'twland  ' &
     &                    /
      LOGICAL*1  LEX
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      INTEGER*4  J1, J2, J3, MJD_START, ID, L_TIM, IUER
      REAL*8     TIM_STEP, UTC_START 
      INTEGER*8  DIR_DESC, IP, IS
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, MKDIR
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: geos_lnd_to_heb file_in dirout [compr]'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, DIROUT )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, COMPR ) 
              ELSE 
                COMPR = 'none'
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
           CALL ERR_LOG ( 7101, -2, 'GEOS_LND_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7102, -2, 'GEOS_LND_TO_HEB', 'Cannot find '// &
     &         ' input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7103, -2, 'GEOS_LND_TO_HEB', 'Output '// &
     &         'directory '//DIROUT(1:I_LEN(DIROUT))//' does not exist' )
           CALL EXIT ( 1 )
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check whether the output subdirectory exists
!
      DO 410 J1=1,MPAR
         DIR = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(J1)
         DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
         IF ( DIR_DESC .LE. 0 ) THEN
              IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
              IF ( IS .EQ. -1 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7104, -2, 'GEOS_LND_TO_HEB', 'Failure '// &
     &                 'in an attempt to create output directory '//DIR )
                   CALL EXIT ( 1 )
              END IF
            ELSE 
              IP = CLOSEDIR ( %VAL(DIR_DESC) )
         END IF
 410  CONTINUE 
!
      IUER = -1
      CALL READ_GEOS_LND ( FILIN, HEB_TWLAND, TIM_STEP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7105, -2, 'GEOS_LND_TO_HEB', 'Failure in '// &
     &         'parsing input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
      IF ( HEB_TWLAND%DIMS(1) == 540 .AND. HEB_TWLAND%DIMS(2) == 361 ) THEN
           CALL HEB_FROM_540_TO_720 ( HEB_TWLAND, HEB_TWLAND_720, IUER )
           DEALLOCATE ( HEB_TWLAND%VAL )
           HEB_TWLAND = HEB_TWLAND_720
           ALLOCATE ( HEB_TWLAND%VAL(720,360,1,HEB_TWLAND%DIMS(4)), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7106, -2, 'GEOS_LND_TO_HEB', 'Failure in '// &
     &              'allocating memory for HEB_TWLAND%VAL' )
                CALL EXIT ( 1 )
           END IF
           HEB_TWLAND%VAL = HEB_TWLAND_720%VAL
           DEALLOCATE ( HEB_TWLAND_720%VAL )
        ELSE IF ( HEB_TWLAND%DIMS(1) == 576 .AND. HEB_TWLAND%DIMS(2) == 361 ) THEN
           CALL HEB_LON_REGRID  ( 720, HEB_TWLAND, HEB_TWLAND_720, IUER )
           DEALLOCATE ( HEB_TWLAND%VAL )
           HEB_TWLAND = HEB_TWLAND_720
           ALLOCATE ( HEB_TWLAND%VAL(720,360,1,HEB_TWLAND%DIMS(4)), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7106, -2, 'GEOS_LND_TO_HEB', 'Failure in '// &
     &              'allocating memory for HEB_TWLAND%VAL' )
                CALL EXIT ( 1 )
           END IF
           HEB_TWLAND%VAL = HEB_TWLAND_720%VAL
           DEALLOCATE ( HEB_TWLAND_720%VAL )
      END IF
!
! --- Generic parameters
!
      HEB_TWLAND%DATA_OFFSET = HEB__HDS
      HEB_TWLAND%ENDIAN      = HEB__LE
      HEB_TWLAND%DATA_TRANSFORM = HEB__NONE
      HEB_TWLAND%FILL_VALUE     = 1.0E15
      HEB_TWLAND%DATA_COMPRESSION = HEB__NONE
      HEB_TWLAND%SDS_NAME       = 'Total water store in land reservoirs'
      HEB_TWLAND%UNITS          = 'Pa'
      HEB_TWLAND%DATA_FORMAT    = HEB__R4
      HEB_TWLAND%MIN_VALUE      = MINVAL(HEB_TWLAND%VAL)
      HEB_TWLAND%MAX_VALUE      = MAXVAL(HEB_TWLAND%VAL)
      HEB_TWLAND%VALID_RANGE(1) = HEB_TWLAND%MIN_VALUE      
      HEB_TWLAND%VALID_RANGE(2) = HEB_TWLAND%MAX_VALUE      
      HEB_TWLAND%OFFSET         = 0.0
      HEB_TWLAND%SCALE_FACTOR   = 1.0
!
! --- Write "Total water store in land reservoirs"
!
      MJD_START = HEB_TWLAND%MJD
      UTC_START = HEB_TWLAND%UTC
      L_TIM = HEB_TWLAND%DIMS(4)
      HEB_TWLAND%DIMS(3) = 1
      HEB_TWLAND%DIMS(4) = 1
      DO 420 J2=1,L_TIM
         HEB_TWLAND%UTC = UTC_START + (J2-1)*TIM_STEP
         HEB_TWLAND%MJD = MJD_START
         IF ( HEB_TWLAND%UTC > 86400.0D0 ) THEN
              HEB_TWLAND%UTC = HEB_TWLAND%UTC - 86400.0D0 
              HEB_TWLAND%MJD = MJD_START + 1
         END IF
         HEB_TWLAND%TAI = HEB_TWLAND%UTC
!
         IUER = -1
         STR = MJDSEC_TO_DATE ( HEB_TWLAND%MJD, HEB_TWLAND%UTC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7107, -2, 'GEOS_LND_TO_HEB', 'Failure '// &
     &            'in an attempt to parse the date '//FILIN )
              CALL EXIT ( 1 )
         END IF
!
! ------ Update headers for "Total water store in land reservoirs" variable
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)(1:I_LEN(SUBDIR(1)))// &
     &            '/'//SUBDIR(1)(1:I_LEN(SUBDIR(1)))//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
         ID = LINDEX ( FILOUT, '/' )
         HEB_TWLAND%FILE_NAME = FILOUT(ID+1:)
!
         IUER = -1
         CALL WRITE_HEB ( HEB_TWLAND, HEB_TWLAND%VAL(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7108, -2, 'GEOS_LND_TO_HEB', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '//FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
 420  CONTINUE 
!
      CALL EXIT ( 0 )
      END  PROGRAM  GEOS_LND_TO_HEB  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_FROM_540_TO_720 ( HEB_IN, HEB_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HEB_FROM_540_TO_720 
! *                                                                      *
! * ## 06-MAY-2013 HEB_FROM_540_TO_720 v1.0 (c) L. Petrov 06-MAY-2013 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_IN, HEB_OUT
      REAL*4     VAL_MIN
      PARAMETER  ( VAL_MIN = 0.001 )
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, IND_LON, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      HEB_OUT = HEB_IN
      HEB_OUT%DIMS(1) = 720
      HEB_OUT%DIMS(2) = 360
      HEB_OUT%DIMS(3) =   1
      HEB_OUT%DIMS(4) = HEB_IN%DIMS(4)
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3)*HEB_OUT%DIMS(4), STR )
           CALL ERR_LOG ( 5411, IUER, 'HEB_FROM_540_TO_720', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' of dynamic memory '// &
     &         'for array HEB_OUT' )
           RETURN 
      END IF
!
      HEB_OUT%VAL = 0.0
!
      DO 410 J1=1,HEB_OUT%DIMS(4)
         DO 420 J2=1,HEB_OUT%DIMS(2)
            IND_LON = 1
            DO 430 J3=1,HEB_OUT%DIMS(1)
               IF ( MOD(J3,4) == 1 ) THEN
                    IF ( J3 > 1 ) IND_LON = IND_LON + 3
                    HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON,J2,1,J1)
                  ELSE IF ( MOD(J3,4) == 2 ) THEN
                    IF ( HEB_IN%VAL(IND_LON,J2,1,J1)   > VAL_MIN .AND. &
        &                HEB_IN%VAL(IND_LON+1,J2,1,J1) > VAL_MIN       )  THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = 1./3.* HEB_IN%VAL(IND_LON,J2,1,J1) + &
        &                                          2./3.* HEB_IN%VAL(IND_LON+1,J2,1,J1)
                       ELSE IF ( HEB_IN%VAL(IND_LON,J2,1,1)   > VAL_MIN ) THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON,J2,1,J1)
                       ELSE IF ( HEB_IN%VAL(IND_LON+1,J2,1,J1)   > VAL_MIN ) THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON+1,J2,1,J1)
                    END IF
                  ELSE IF ( MOD(J3,4) == 3 ) THEN
                    IF ( HEB_IN%VAL(IND_LON+1,J2,1,J1) > VAL_MIN .AND. &
        &                HEB_IN%VAL(IND_LON+2,J2,1,J1) > VAL_MIN       )  THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = 2./3.* HEB_IN%VAL(IND_LON+1,J2,1,J1) + &
        &                                          1./3.* HEB_IN%VAL(IND_LON+2,J2,1,J1)
                       ELSE IF ( HEB_IN%VAL(IND_LON+1,J2,1,J1)   > VAL_MIN ) THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON+1,J2,1,J1)
                       ELSE IF ( HEB_IN%VAL(IND_LON+2,J2,1,J1)   > VAL_MIN ) THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON+2,J2,1,J1)
                    END IF
                  ELSE IF ( MOD(J3,4) == 0 ) THEN
                    HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON+2,J2,1,J1)
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      IF ( HEB_OUT%FILE_NAME(1:1) == '/' ) HEB_OUT%FILE_NAME = HEB_OUT%FILE_NAME(2:)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  HEB_FROM_540_TO_720  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_LON_REGRID ( LON_DIM_NEW, HEB_IN, HEB_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HEB_LON_REGRID
! *                                                                      *
! * ### 26-NOV-2013  HEB_LON_REGRID  v1.0 (c) L. Petrov 26-NOV-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_IN, HEB_OUT
      REAL*4     VAL_MIN
      PARAMETER  ( VAL_MIN = 0.001 )
      INTEGER*4  LON_DIM_NEW, IUER
      CHARACTER  STR*128
      REAL*8     LON, FRAC
      INTEGER*4  J1, J2, J3, IND_LON_OLD, IND_LON_NEXT, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      HEB_OUT = HEB_IN
      HEB_OUT%DIMS(1) = LON_DIM_NEW
      HEB_OUT%DIMS(2) = HEB_IN%DIMS(2) - 1
      HEB_OUT%DIMS(3) =   1
      HEB_OUT%DIMS(4) = HEB_IN%DIMS(4)
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3)*HEB_OUT%DIMS(4), STR )
           CALL ERR_LOG ( 5411, IUER, 'HEB_FROM_576_TO_720', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' of dynamic memory '// &
     &         'for array HEB_OUT' )
           RETURN 
      END IF
!
      HEB_OUT%VAL = 0.0
!
      DO 410 J1=1,HEB_OUT%DIMS(4)
         DO 420 J2=1,HEB_OUT%DIMS(2)
            DO 430 J3=1,HEB_OUT%DIMS(1)
               LON = (J3-1)*PI2/LON_DIM_NEW
!
! ------------ Find IND_LON_OLD -- the index of the old grid that 
! ------------ the just preceeds or coinicides with the J3-th grid point of the new grid
!
!              ind_lon_old     ind_lon_next
!                        |     |
!                        v     v
!       
!                     ___|___x_|____|  old_grid
!                            ^
!                            |
!                     new grid element
!
               IND_LON_OLD = IDINT ( LON/PI2*HEB_IN%DIMS(1) ) + 1
!
! ------------ Frac is a fraction of longitude difference to the IND_LON_OLD to
! ------------ the old longitude grid step
!
               FRAC = (LON - (IND_LON_OLD-1)*PI2/HEB_IN%DIMS(1))/PI2*HEB_IN%DIMS(1)
               IF ( FRAC < 0.01 ) THEN
!
! ----------------- Case 1): the new grid point fall exactly on a old grid point
!
                    HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON_OLD,J2,1,J1)
                  ELSE 
!
! ----------------- Case 1): the new grid point fall between old grid points.
! ----------------- Let us use linear interpolation in a case if both points
! ----------------- are on land. If one point on land and another is on an ocean,
! ----------------- us the value at the closest land point 
!
                    IND_LON_NEXT = IND_LON_OLD + 1
                    IF ( IND_LON_NEXT > HEB_IN%DIMS(1) ) IND_LON_NEXT = 1 
                    IF ( HEB_IN%VAL(IND_LON_OLD,J2,1,J1)  > VAL_MIN .AND. &
        &                HEB_IN%VAL(IND_LON_NEXT,J2,1,J1) > VAL_MIN        )  THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = (1.0D0 - FRAC)*HEB_IN%VAL(IND_LON_OLD,J2,1,J1) + &
     &                                                      FRAC *HEB_IN%VAL(IND_LON_NEXT,J2,1,J1) 
                       ELSE IF ( HEB_IN%VAL(IND_LON_OLD,J2,1,J1)  > VAL_MIN ) THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON_OLD,J2,1,J1)
                       ELSE IF ( HEB_IN%VAL(IND_LON_NEXT,J2,1,J1)  > VAL_MIN ) THEN
                         HEB_OUT%VAL(J3,J2,1,J1) = HEB_IN%VAL(IND_LON_NEXT,J2,1,J1)
                    END IF
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      IF ( HEB_OUT%FILE_NAME(1:1) == '/' ) HEB_OUT%FILE_NAME = HEB_OUT%FILE_NAME(2:)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  HEB_LON_REGRID  !#!#
