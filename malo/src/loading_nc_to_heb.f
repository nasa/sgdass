      PROGRAM    LOADING_NC_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program  LOADING_NC_TO_HEB
! *                                                                      *
! * ### 06-JUN-2016 LOADING_NC_TO_HEB v1.0 (c) L. Petrov 06-JUN-2016 ### *
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
     &           SOURCE(MS)*128, COMMENT(MC)*128
      INTEGER*4  NLON, NLAT, IVEC, ICMP, IFRQ, MJD, LC, LS, IUER 
      INTEGER*4  J1, J2
      REAL*8     TAI
      REAL*4,    ALLOCATABLE :: DSP_ARR_R4(:,:,:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: loading_nc_to_heb nc-file heb_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILOUT )
      END IF
!
      IUER = -1
      CALL INQ_LOADING_NC ( FILIN, NLON, NLAT, MJD, TAI, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6381, IUER, 'LOADING_NC_TO_HEB', 'Failure in '// &
     &         'an attempt to read the header of the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( DSP_ARR_R4(NLON,NLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(4)*INT8(3)*INT8(NLON)*INT8(NLAT), STR )
           IUER = -1
           CALL ERR_LOG ( 6382, IUER, 'LOADING_NC_TO_HEB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array DSP_ARR_R4' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_LOADING_NC_INFO ( FILIN, TITLE, INSTITUTION, &
     &                            MS, LS, SOURCE, HISTORY, REFERENCE, &
     &                            MC, LC, COMMENT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6383, IUER, 'LOADING_NC_TO_HEB', 'Failure in '// &
     &         'an attempt to read the data from the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!!
!      WRITE ( 6, * ) 'TITLE:       '//TITLE(1:I_LEN(TITLE))
!      WRITE ( 6, * ) 'INSTITUTION: '//INSTITUTION(1:I_LEN(INSTITUTION))
!      WRITE ( 6, * ) 'REFERENCE:   '//REFERENCE(1:I_LEN(REFERENCE))
!      WRITE ( 6, * ) 'HISTORY:     '//HISTORY(1:I_LEN(HISTORY))
!      DO 410 J1=1,LS
!         WRITE ( 6, '(A)' ) SOURCE(J1)(1:I_LEN(SOURCE(J1)))
! 410  CONTINUE 
!      DO 420 J2=1,LC
!         WRITE ( 6, '(A)' ) COMMENT(J2)(1:I_LEN(COMMENT(J2)))
! 420  CONTINUE 
!      WRITE ( 6, * ) '-------------------'
!      WRITE ( 6, * ) 'Date: '//MJDSEC_TO_DATE ( MJD, TAI, IUER )
!!
!
      IUER = -1
      CALL READ_LOADING_NC ( FILIN, INT8(3)*INT8(NLON)*INT8(NLAT), NLON, NLAT, &
     &                       IVEC, ICMP, IFRQ, MJD, TAI, DSP_ARR_R4, WAV_NAM, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6384, IUER, 'LOADING_NC_TO_HEB', 'Failure in '// &
     &         'an attempt to read the data from the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      HEB_DSPL%MJD     = MJD
      HEB_DSPL%UTC     = TAI
      HEB_DSPL%TAI     = TAI
      HEB_DSPL%DIMS(1) = NLON
      HEB_DSPL%DIMS(2) = NLAT 
      HEB_DSPL%DIMS(3) = 3
      HEB_DSPL%DIMS(4) = 1
      HEB_DSPL%DATA_OFFSET = HEB__HDS
      HEB_DSPL%ENDIAN      = HEB__LE
      HEB_DSPL%DATA_TRANSFORM = HEB__SCOF
      HEB_DSPL%FILL_VALUE     = 1.0E15
      HEB_DSPL%OFFSET         = 0.0
      HEB_DSPL%SCALE_FACTOR   = 0.00002
      HEB_DSPL%DATA_COMPRESSION = HEB__NONE
      HEB_DSPL%SDS_NAME       = 'Site displacement '
      HEB_DSPL%UNITS          = 'meter'
      HEB_DSPL%DATA_FORMAT    = HEB__I2
      HEB_DSPL%MIN_VALUE      = MINVAL(DSP_ARR_R4)
      HEB_DSPL%MAX_VALUE      = MAXVAL(DSP_ARR_R4)
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
      IUER = -1
      CALL WRITE_HEB ( HEB_DSPL, DSP_ARR_R4, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6385, IUER, 'LOADING_NC_TO_HEB', 'Failure '// &
     &         'to write loading displacements into the output '// &
     &         'file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      CALL EXIT  ( 0 )
      END  PROGRAM    LOADING_NC_TO_HEB  !#!  
