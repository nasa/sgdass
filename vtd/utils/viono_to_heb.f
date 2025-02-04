       PROGRAM    VIONO_TO_HEB_MAIN
       IMPLICIT   NONE 
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL VIONO_TO_HEB()
       END  PROGRAM  VIONO_TO_HEB_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE  VIONO_TO_HEB()
! ************************************************************************
! *                                                                      *
! *   Program  VIONO_TO_HEB  extracts TEC data from the ionospheric      *
! *   model for a given data and writes them in HEB format.              *
! *                                                                      *
! *  ### 01-MAR-2018  VIONO_TO_HEB  v1.0 (c)  L. Petrov 01-MAR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'viono.i'
      CHARACTER  FILV*128, DIRO*128, FILO*128, DATE_REQ*21, STR*128
      CHARACTER  OUT*2048
      TYPE     ( IONO__TYPE ) :: VIO
      REAL*4,    ALLOCATABLE  :: VIO_TEC(:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, MJD_REQ, IVRB, IND_REQ, NLON, &
     &           NLAT, LUN, IC, IND_LON, IND_LAT, IUER
      LOGICAL*1  LEX
      REAL*4     MIN_VALUE, MAX_VALUE, VALID_RANGE(2), FILL_VAL
      REAL*8     UTC_REQ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, WRITE
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      VALID_RANGE(1) =    0.0
      VALID_RANGE(2) =  999.0
      FILL_VAL       =   -1.0
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Viono_to_heb viono_file date output_dir'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILV     )
           CALL GETARG ( 2, DATE_REQ )
           CALL GETARG ( 3, DIRO     )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_REQ, MJD_REQ, UTC_REQ, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4401, IUER, 'VIONO_TO_HEB', 'Error in pareing the '// &
     &         'request date '//DATE_REQ )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL VIO_GET_HEADER ( FILV, VIO, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4402, IUER, 'VIONO_TO_HEB', 'Error in reading the '// &
     &         ' header of the ionospheric model file '//FILV )
           CALL EXIT ( 1 ) 
      END IF
      IND_REQ = IDNINT( ( (MJD_REQ*86400.0D0 + UTC_REQ) - (VIO%HEADER%MJD_BEG*86400.0D0 + VIO%HEADER%UTC_BEG) )/ &
     &                     VIO%HEADER%TIM_STEP ) + 1
      IF ( IND_REQ < 1 ) THEN
           IUER = -1
           STR = MJDSEC_TO_DATE ( VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG, IUER )
           IUER = -1
           CALL ERR_LOG ( 4403, IUER, 'VIONO_TO_HEB', 'Requested date '// &
     &          TRIM(DATE_REQ)//' is earlier than '//STR(1:19)//' -- the '// &
     &         'first date of the file with ionosphere model '//FILV )
           CALL EXIT ( 1 ) 
      END IF 
      IF ( IND_REQ > VIO%HEADER%NEPC ) THEN
           IUER = -1
           STR = MJDSEC_TO_DATE ( VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG + &
     &                                                VIO%HEADER%NEPC*VIO%HEADER%TIM_STEP, IUER )
           IUER = -1
           CALL ERR_LOG ( 4404, IUER, 'VIONO_TO_HEB', 'Requested date '// &
     &          TRIM(DATE_REQ)//' is later than '//STR(1:19)//' -- the '// &
     &         'last date of the file with ionosphere model '//FILV )
           CALL EXIT ( 1 ) 
      END IF 
!
!      write ( 6, * ) 'mur: ', MJD_REQ, UTC_REQ
!      write ( 6, * ) 'mo:  ', VIO%HEADER%MODEL
!      write ( 6, * ) 'au:  ', VIO%HEADER%AUTHOR
!      write ( 6, * ) 'mut: ', VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG, VIO%HEADER%TIM_STEP
!      write ( 6, * ) 'lpt: ', VIO%HEADER%NLON, VIO%HEADER%NLAT, VIO%HEADER%NEPC
      write ( 6, * ) 'llm: ', VIO%HEADER%LON_MIN, VIO%HEADER%LAT_MIN
!      write ( 6, * ) 'inr: ', IND_REQ
!
      IUER = -1
      CALL VTD_GET_IONO ( FILV, MJD_REQ, UTC_REQ, 1, VIO, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4405, IUER, 'VIONO_TO_HEB', 'Error in reading '// &
     &         'global TEC from file '//TRIM(FILV)//' on epoch '//DATE_REQ )
           RETURN 
      END IF
      NLON = IDNINT ( PI2/VIO%HEADER%LON_STEP ) + 1
      NLAT = IDNINT ( PI__NUM/VIO%HEADER%LAT_STEP ) + 1
      ALLOCATE ( VIO_TEC(NLON,NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL CLRCH ( STR )
           CALL IINCH ( 4*NLON*NLAT, STR )
           CALL ERR_LOG ( 4405, IUER, 'VIONO_TO_HEB', 'Error in an attempt '// &
     &         'to allocate '//TRIM(STR)//' bytes of dynamic memory '// &
     &         'for array VIO_TEC' )
           CALL EXIT ( 1 ) 
      END IF
      VIO_TEC = -1.00
!
      MIN_VALUE = 1.0E15
      MAX_VALUE =-1.0E15
      DO 410 J1=1,VIO%HEADER%NLAT
         IND_LAT = IDNINT((VIO%HEADER%LAT_MIN + P2I + (J1-1)*VIO%HEADER%LAT_STEP)/VIO%HEADER%LAT_STEP ) + 1
         DO 420 J2=1,VIO%HEADER%NLON
            IND_LON = IDNINT((VIO%HEADER%LON_MIN + (J2-1)*VIO%HEADER%LON_STEP)/VIO%HEADER%LON_STEP ) + (NLON-1)/2 + 1
            VIO_TEC(IND_LON,IND_LAT) = VIO%HEADER%SCALE*VIO%TEC_VAL(J2,J1,1)
            IF ( VIO_TEC(IND_LON,IND_LAT) > 0 ) THEN
                 MIN_VALUE = MIN ( MIN_VALUE, VIO_TEC(IND_LON,IND_LAT) )
                 MAX_VALUE = MAX ( MAX_VALUE, VIO_TEC(IND_LON,IND_LAT) )
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      OUT = 'HEB Format version of 2013.01.30'
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR, FMT='("Dims:   ",I18,1X,I18,1X,I18,1X,I18)' ) NLON, NLAT, 1, 1
      CALL CHASHL ( STR(19:) )
      OUT = TRIM(OUT)//CHAR(10)//STR
      OUT = TRIM(OUT)//CHAR(10)//'Data_Format:      R4'
      OUT = TRIM(OUT)//CHAR(10)//'Endian:           LE'
      OUT = TRIM(OUT)//CHAR(10)//'Data_Compression: none'
      OUT = TRIM(OUT)//CHAR(10)//'Data_Offset:      2048'
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR, FMT='("Data_Length:      ",I18)' ) 4*NLON*NLAT
      CALL CHASHL ( STR(19:) )
      OUT = TRIM(OUT)//CHAR(10)//STR
      OUT = TRIM(OUT)//CHAR(10)//'Data_Transform:   none'
      WRITE ( UNIT=STR, FMT='("Fill_Value:       ", F15.8)' ) FILL_VAL
      CALL CHASHL ( STR(19:) )
      OUT = TRIM(OUT)//CHAR(10)//STR
!      OUT = TRIM(OUT)//CHAR(10)//'Fill_Value:       -1.0'
      OUT = TRIM(OUT)//CHAR(10)//'Offset:           0.0'
      OUT = TRIM(OUT)//CHAR(10)//'Scale_factor:     1.0'
      OUT = TRIM(OUT)//CHAR(10)//'SDS_name:         ionosphere TEC'
      OUT = TRIM(OUT)//CHAR(10)//'Title:            Ionosphere TEC'
      OUT = TRIM(OUT)//CHAR(10)//'Units:            TECU'
      OUT = TRIM(OUT)//CHAR(10)//'File_name:        '//TRIM(FILV)
      OUT = TRIM(OUT)//CHAR(10)//'Prod_name:        '//VIO%HEADER%MODEL
      OUT = TRIM(OUT)//CHAR(10)//'Institution:      '//VIO%HEADER%AUTHOR
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR, FMT='("MJD, Time:   ", 5X,I5,1X,F7.1,A)' ) MJD_REQ, UTC_REQ
      OUT = TRIM(OUT)//CHAR(10)//STR
      WRITE ( UNIT=STR, FMT='("Min_Value:        ", 1PE14.7,A)' ) MIN_VALUE
      OUT = TRIM(OUT)//CHAR(10)//STR
      WRITE ( UNIT=STR, FMT='("Max_Value:        ", 1PE14.7,A)' ) MAX_VALUE
      OUT = TRIM(OUT)//CHAR(10)//STR
      WRITE ( UNIT=STR, FMT='("Valid_Range:      ", 1PE14.7,1X,1PE14.7,A)' ) VALID_RANGE
      OUT = TRIM(OUT)//CHAR(10)//STR
      OUT = TRIM(OUT)//CHAR(10)
!
      CALL CLRCH ( STR )
      STR = MJDSEC_TO_DATE ( MJD_REQ, UTC_REQ )
      FILO = TRIM(DIRO)//'/vio_'//STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)//STR(15:16)//STR(18:19)//'.heb'
!      write ( 6, * ) 'filo= ', trim(filo)
!      write ( 6, * ) 'out= ',  trim(out)
      INQUIRE ( FILE=FILO, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL UNLINK ( TRIM(FILO)//CHAR(0) )
      END IF
!
      IUER = -1
      CALL BINF_OPEN ( FILO, 'NEW', LUN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4406, IUER, 'VIONO_TO_HEB', 'Error in an '// &
     &         'to open for writing the output file '//FILO )
           CALL EXIT ( 1 )
      END IF
!
      IC = WRITE ( %VAL(LUN), %REF(OUT), %VAL(LEN(OUT)) )
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           IUER = -1
           CALL ERR_LOG ( 4407, IUER, 'VIONO_TO_HEB', 'Error in '// &
     &         'writing the header of the output file '//TRIM(FILO)// &
     &         ' -- '//STR )
           CALL EXIT ( 1 )
      END IF
      IF ( IC .NE. LEN(OUT) ) THEN
           CALL CLRCH   ( STR )
           IUER = -1
           CALL ERR_LOG ( 4408, IUER, 'VIONO_TO_HEB', 'Error in '// &
     &         'writing the header of the output file '//TRIM(FILO)// &
     &         ' -- not all bytes have been written' )
           CALL EXIT ( 1 )
      END IF
!
      IC = WRITE ( %VAL(LUN), %REF(VIO_TEC), %VAL(4*NLON*NLAT) )
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           IUER = -1
           CALL ERR_LOG ( 4409, IUER, 'VIONO_TO_HEB', 'Error in '// &
     &         'writing the data section of the output file '//TRIM(FILO)// &
     &         ' -- '//STR )
           CALL EXIT ( 1 )
      END IF
      IF ( IC .NE. 4*NLON*NLAT ) THEN
           CALL CLRCH   ( STR )
           IUER = -1
           CALL ERR_LOG ( 4410, IUER, 'VIONO_TO_HEB', 'Error in '// &
     &         'writing the data section of the output file '//TRIM(FILO)// &
     &         ' -- not all bytes have been written' )
           CALL EXIT ( 1 )
      END IF
      CALL BINF_CLOSE ( LUN, IUER )
      WRITE ( 6, '(A)' ) 'Written output file: '//TRIM(FILO)
!
      END  SUBROUTINE  VIONO_TO_HEB  !#!#
