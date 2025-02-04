      PROGRAM    GEN_AAM_LAUNCH
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
      CALL GEN_AAM()
      END  PROGRAM  GEN_AAM_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  GEN_AAM()
! ************************************************************************
! *                                                                      *
! *   Program GEN_AAM
! *                                                                      *
! *  ### 30-JUL-2015    GEN_AAM    v2.0 (c)  L. Petrov  30-NOV-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ) :: MAL
      TYPE     ( HEB__TYPE  ) :: HEB_D, HEB_T, HEB_Q, HEB_IEH, HEB_OEH, &
     &                           HEB_LS, HEB_U, HEB_V, GEOID_BSPL_HEB
      INTEGER*4    MO
      PARAMETER  ( MO = 64 )
      CHARACTER  DIR_HEB*128, OBS_DATE*128, FIL_IGH*128, FIL_OGH*128, &
     &           OUT_PREF*128, FIL_GEOID*128, FIL_LS_MASK*128, FIL_OUT*128, &
     &           FIL_GEOID_BSPL*128, CAL_DATE*19, DIR_OUT*128, &
     &           FINAM_DELP*128, DATA_SRC*128, COMPR*16, COMPR_COM*64, &
     &           COM_STR*256, OUT(MO)*128, FILOUT*128, TEST_STR*8, STR*128
      REAL*8     LAT_GDT, LON, TIM, IMOM_NOIB(3), IMOM_IB(3), HMOM(3)
      INTEGER*8  DIR_DESC
      INTEGER*4  ID, IL, DEG, J1, J2, NO, IUER
      LOGICAL*1  EX_DELP, EX_W
      CHARACTER  GEN_AAM__LABEL*32
      PARAMETER  ( GEN_AAM__LABEL = 'GEN_AAM  Vers  1.2 of 2015.12.08' )
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, LINDEX
      REAL*8,    EXTERNAL :: GET_GEOID, WALL_TIMER 
!
      TEST_STR = 'none'
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_aam heb-dir date inp_grid_height '// &
     &                        'output_grid_height output_grid_ls_mask output_pref'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIR_HEB     )
           CALL GETARG ( 2, OBS_DATE    )
           CALL GETARG ( 3, FIL_IGH     )
           CALL GETARG ( 4, FIL_OGH     )
           CALL GETARG ( 5, FIL_LS_MASK )
           CALL GETARG ( 6, OUT_PREF    )
      END IF
      IF ( TEST_STR == 'timer' ) THEN
           TIM = WALL_TIMER ( %VAL(0) )
      END IF
!
      ID = LINDEX ( OUT_PREF, '/' )
      DIR_OUT = OUT_PREF(1:ID)
      DIR_DESC = OPENDIR ( DIR_OUT(1:I_LEN(DIR_OUT))//CHAR(0) )
      IF ( DIR_DESC > 0 ) THEN 
           DIR_DESC = CLOSEDIR ( %VAL(DIR_DESC) )
         ELSE
           IUER = -1
           CALL ERR_LOG ( 4601, IUER, 'GEN_AAM', 'Wrong 5th argument: '// &
     &         'output directory '//DIR_OUT(1:I_LEN(DIR_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      FINAM_DELP = DIR_HEB(1:I_LEN(DIR_HEB))//'/'//OBS_DATE(1:4)// &
     &            '/d/d_'//OBS_DATE(1:I_LEN(OBS_DATE))//'.heb.bz2'
      INQUIRE ( FILE=FINAM_DELP, EXIST=EX_DELP )
      IF ( .NOT. EX_DELP ) THEN
           IL = ILEN(FINAM_DELP)
           CALL CLRCH ( FINAM_DELP(IL-3:) )
           INQUIRE ( FILE=FINAM_DELP, EXIST=EX_DELP )
      END IF
!
      IUER = -1
      CALL READ_DQTUV_HEB ( FINAM_DELP, HEB_D, HEB_Q, HEB_T, HEB_U, HEB_V, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4602, IUER, 'GEN_AAM', 'Error in an attempt '// &
     &                   'to read numerical weather model for date '//OBS_DATE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_IGH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4603, IUER, 'GEN_AAM', 'Error in an attempt '// &
     &        'to find the file with geoid undulations '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_OGH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4604, IUER, 'GEN_AAM', 'Error in an attempt '// &
     &        'to find the file with digital elevations above geoid '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_MODEL_FILE ( FIL_LS_MASK, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4605, IUER, 'GEN_AAM', 'Error in an attempt '// &
     &        'to find the file with land sea mask '//FIL_LS_MASK )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_IGH, HEB_IEH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           IUER = -1
           CALL ERR_LOG ( 4606, IUER, 'GEN_AAM', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_OGH, HEB_OEH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           IUER = -1
           CALL ERR_LOG ( 4607, IUER, 'GEN_AAM', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_LS_MASK, HEB_LS, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           IUER = -1
           CALL ERR_LOG ( 4608, IUER, 'GEN_AAM', 'Error in reading '// &
     &         'heb-file '//FIL_LS_MASK )
           CALL EXIT ( 1 )
      END IF
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '("Read input data time: ", F8.3)' ) WALL_TIMER ( %VAL(2) )
      END IF
!
      IUER = -1
      CALL MALO_COMP_AAM ( HEB_D, HEB_T, HEB_Q, HEB_U, HEB_V, HEB_IEH, &
     &                     HEB_OEH, HEB_LS, MAL, IMOM_NOIB, IMOM_IB, &
     &                     HMOM, TEST_STR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4609, IUER, 'GEN_AAM', 'Error in an attempt '// &
     &              'to compute surface atmospheric pressure' )
           CALL EXIT ( 1 )
      END IF
!      
      NO = 1; OUT(NO) = AAM__FMT
      NO = NO+1; OUT(NO) = '# '
      NO = NO+1; OUT(NO) = '# Atmospheric angular momentum computed by using numerical weather model'
      NO = NO+1; OUT(NO) = '# '
      NO = NO+1; OUT(NO) = '# Author:                        Leonid Petrov'
      NO = NO+1; OUT(NO) = '# URL:                           http://aam.earthrotation.net'
      NO = NO+1; OUT(NO) = '# Created by                     '//GEN_AAM__LABEL 
      NO = NO+1; OUT(NO) = '# Created on                     '//GET_CDATE()
      NO = NO+1; OUT(NO) = '# Original data file name:       '//HEB_D%FILE_NAME
      NO = NO+1; OUT(NO) = '# Original data product name:    '//HEB_D%PROD_NAME
      NO = NO+1; OUT(NO) = '# Original data history:         '//HEB_D%HISTORY
      NO = NO+1; OUT(NO) = '# Original data source:          '//HEB_D%SOURCE
      NO = NO+1; OUT(NO) = '# Original data title:           '//HEB_D%TITLE
      NO = NO+1; OUT(NO) = '# Original data institution:     '//HEB_D%INSTITUTION
      NO = NO+1; OUT(NO) = '# Original data production time: '//HEB_D%PROD_DATE_TIME
      NO = NO+1; OUT(NO) = '# '
      STR = MJDSEC_TO_DATE ( HEB_D%MJD, HEB_D%UTC, IUER )
      NO = NO+1
      WRITE ( UNIT=OUT(NO), FMT=110 ) STR(1:19), HEB_D%MJD, HEB_D%UTC
 110  FORMAT ( 'AAM date:                  ',A, 2X, I5, 2X, F7.1 )
      NO = NO+1
      WRITE ( UNIT=OUT(NO), FMT=120 ) IMOM_NOIB
 120  FORMAT ( 'AAM matter part no IB:     ', 3(1PE12.5,2X), ' kg/m^2' )
      NO = NO+1
      WRITE ( UNIT=OUT(NO), FMT=130 ) IMOM_IB
 130  FORMAT ( 'AAM matter part    IB:     ', 3(1PE12.5,2X), ' kg/m^2' )
      NO = NO+1
      WRITE ( UNIT=OUT(NO), FMT=140 ) HMOM
 140  FORMAT ( 'AAM motion part:           ', 3(1PE12.5,2X), ' kg/(s*m^2)' )
!
      NO = NO+1
      WRITE ( UNIT=OUT(NO), FMT=150 ) MALO__X1_MAT_COEF*IMOM_NOIB(1)  + &
     &                                MALO__X1_MOT_COEF*HMOM(1),   &
     &                                MALO__X1_MAT_COEF*IMOM_NOIB(2)  + &
     &                                MALO__X1_MOT_COEF*HMOM(2),   &
     &                                MALO__X3_MAT_COEF*IMOM_NOIB(3)  + &
     &                                MALO__X3_MOT_COEF*HMOM(3)
 150  FORMAT ( 'Excitation function no IB: ', 3(1PE12.5,2X), ' 1/s' )
!
      NO = NO+1
      WRITE ( UNIT=OUT(NO), FMT=160 ) MALO__X1_MAT_COEF*IMOM_IB(1)  + &
     &                                MALO__X1_MOT_COEF*HMOM(1),   &
     &                                MALO__X1_MAT_COEF*IMOM_IB(2)  + &
     &                                MALO__X1_MOT_COEF*HMOM(2),   &
     &                                MALO__X3_MAT_COEF*IMOM_IB(3)  + &
     &                                MALO__X3_MOT_COEF*HMOM(3)
 160  FORMAT ( 'Excitation function    IB: ', 3(1PE12.5,2X), ' 1/s' )
!
      FILOUT = OUT_PREF(1:I_LEN(OUT_PREF))//OBS_DATE(1:I_LEN(OBS_DATE))//'.txt'
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4610, IUER, 'GEN_AAM', 'Error in an attempt '// &
     &              'to write the AAM in the output file' )
           CALL EXIT ( 1 )
      END IF
!
      CALL EXIT ( 0 )
      END  SUBROUTINE  GEN_AAM  !#!#
