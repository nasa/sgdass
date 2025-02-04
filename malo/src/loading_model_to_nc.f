      PROGRAM    LOADING_MODEL_TO_NC
! ************************************************************************
! *                                                                      *
! *   Program LOADING_MODEL_TO_NC converts the model of site             *
! *   displacements from HEB to NETCDF format.                           *
! *                                                                      *
! * ## 30-MAY-2013 loading_model_to_nc v2.0 (c) L. Petrov 18-FEB-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      TYPE     ( MALO__TYPE ), POINTER :: MALO(:)
      CHARACTER  FILIN*128, FILOUT*128, STR*128
      INTEGER*4  IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: loading_model_to_netcdf input_heb_file '// &
     &                    'output_netcdf_file' 
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILOUT )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6701, IUER, 'LOADING_MODEL_TO_NC', 'Failure '// &
     &         'in an attempt to read input file in HEB-format '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate malo object
!
      ALLOCATE ( MALO(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6702, IUER, 'LOADING_MODEL_TO_NC', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize malo object
!
      IUER = -1
      CALL MALO_INIT ( MALO(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6703, IUER, 'LOADING_MODEL_TO_NC', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL HEB_MODEL_TO_NC ( HEB, MALO(1), FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'LOADING_MODEL_TO_NC', 'Failure '// &
     &         'in an attempt to transform the model to netcdf format and '// &
     &         'write it to file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  LOADING_MODEL_TO_NC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_MODEL_TO_NC ( HEB, MALO, FILNC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HEB_MODEL_TO_NC
! *                                                                      *
! * ### 31-MAY-2013  HEB_MODEL_TO_NC  v2.0 (c) L. Petrov 18-FEB-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FILNC*(*)
      INCLUDE   'netcdf.inc'
      INTEGER*4  IUER
      REAL*8     LAT_RANGE(2), LON_RANGE(2), FRQ_RANGE(2), LAT_STEP, LON_STEP, &
     &           PHAS_ARR(MALO__MWAV), FREQ_ARR(MALO__MWAV)
      REAL*4     DSPL_RANGE(2), VALID_DSPL_RANGE(2)
      REAL*4,    ALLOCATABLE :: LON_ARR(:), LAT_ARR(:), FRQ_ARR(:), DSPL_ARR(:,:,:,:,:)
      CHARACTER  STR*128, CAR_ARR(3)*5, CMP_ARR(2)*5, C_WAV(MALO__MWAV)*4
      INTEGER*4  NCID, IS, ID_VAR(5)
      INTEGER*4  ID_DIM_LON, ID_DIM_LAT, ID_DIM_CMP, ID_DIM_FREQ, &
     &           ID_DIM_CAR, DIM_VEC(6)
      INTEGER*4  ID_VAR_LON, ID_VAR_LAT, ID_VAR_CMP, ID_VAR_CAR, &
     &           ID_VAR_FREQ, ID_VAR_PHAS, ID_VAR_DSPL
      INTEGER*4  ID_DIM_LENCMP
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  M_LON, M_LAT, M_CMP, M_CAR, M_FRQ, I_CMP, I_FRQ, IDM(2), ICN(2), &
     &           J1, J2, J3, J4, J5, J6, J7, LIND, IND(2,MIND), IER
      INTEGER*4, EXTERNAL :: I_LEN, ADD_CLIST 
!
      IF ( LEN(HEB%COMMENT(2)) < 8 ) THEN
           CALL ERR_LOG ( 6811, IUER, 'HEB_MODEL_TO_NC', 'Trap of internal '// &
     &         'control: comment(2) in the input heb file '//TRIM(HEB%FILE_NAME)// &
     &         ' is too short' ) 
           RETURN
      END IF
      CALL EXWORD ( HEB%COMMENT(2), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
      IF ( LIND < 4 ) THEN
           CALL ERR_LOG ( 6812, IUER, 'HEB_MODEL_TO_NC', 'Trap of internal '// &
     &         'control: comment(2) in the input heb file '//TRIM(HEB%FILE_NAME)// &
     &         ' has less than 4 words' ) 
           RETURN
      END IF 
      CALL CHIN ( HEB%COMMENT(2)(IND(1,4):IND(2,4)), MALO%CONF%MODEL_CODE )
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_MODC_PARSE ( MALO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6813, IUER, 'HEB_MODEL_TO_NC', 'Failure to parse '// &
     &         'regression model code '//HEB%COMMENT(2)(IND(1,4):IND(2,4)) )
           RETURN
      END IF
!
      M_FRQ = 0
      DO 410 J1=1,MALO%NMDC
         IF ( MALO%MODC(J1)%TYP == MALO__COS .OR. &
     &        MALO%MODC(J1)%TYP == MALO__SIN      ) THEN
              CALL ERR_PASS ( IUER, IER )
              IS = ADD_CLIST ( MALO__MWAV, M_FRQ, C_WAV, MALO%MODC(J1)%WAV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6814, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &                 'an attempt to update list C_WAV' ) 
                   RETURN
              END IF
              IF ( IS == 1 ) THEN
                   FRQ_RANGE(1) = MALO%MODC(J1)%FRQ
                   FRQ_RANGE(2) = MALO%MODC(J1)%FRQ
                 ELSE 
                   FRQ_RANGE(1) = MIN ( FRQ_RANGE(1), MALO%MODC(J1)%FRQ )
                   FRQ_RANGE(2) = MAX ( FRQ_RANGE(2), MALO%MODC(J1)%FRQ )
              END IF 
         END IF
 410  CONTINUE 
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_CREATE ( FILNC, 0, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6815, IUER, 'HEB_MODEL_TO_NC', 'Error in an attempt '// &
     &         'to create the output netcf file '//FILNC(1:I_LEN(FILNC))// &
     &         ' NF_CREATE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Declare six dimensions: up-east-noth, cos-sin, freq, lon, lat, lencmp
!
      M_CAR = 3
      IS = NF_DEF_DIM ( NCID, 'up-east-north', M_CAR, ID_DIM_CAR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6816, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new dimension cos-sin: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      M_CMP = 2
      IS = NF_DEF_DIM ( NCID, 'cos-sin', M_CMP, ID_DIM_CMP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6817, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new dimension cos-sin: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'freq', M_FRQ, ID_DIM_FREQ )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6818, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new dimension freq: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      M_LON = HEB%DIMS(2)
      IS = NF_DEF_DIM ( NCID, 'lon', M_LON, ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6819, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new dimension lon: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      M_LAT = HEB%DIMS(3)
      IS = NF_DEF_DIM ( NCID, 'lat', M_LAT, ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6820, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new dimension lat: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'lencmp', 5, ID_DIM_LENCMP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6821, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new dimension lencmp: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      DIM_VEC(1) = ID_DIM_LON   
      DIM_VEC(2) = ID_DIM_LAT   
      DIM_VEC(3) = ID_DIM_FREQ
      DIM_VEC(4) = ID_DIM_CMP   
      DIM_VEC(5) = ID_DIM_CAR   
!
! --- Declare 6 variables: cmp, freq, phas, lon, lat, disp
!
      IS = NF_DEF_VAR ( NCID, 'lon', NF_FLOAT, 1, DIM_VEC(1), ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6822, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new variable lon: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'lat', NF_FLOAT, 1, DIM_VEC(2), ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6823, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new variable lat: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'freq', NF_DOUBLE, 1, DIM_VEC(3), ID_VAR_FREQ )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6824, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new variable freq: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'phas', NF_DOUBLE, 1, DIM_VEC(3), ID_VAR_PHAS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6825, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new variable phas: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IDM(1) = ID_DIM_LENCMP
      IDM(2) = ID_DIM_CMP
      IS = NF_DEF_VAR ( NCID, 'cmp', NF_CHAR, 2, IDM, ID_VAR_CMP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6826, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new variable time: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IDM(1) = ID_DIM_LENCMP
      IDM(2) = ID_DIM_CAR
      IS = NF_DEF_VAR ( NCID, 'car', NF_CHAR, 2, IDM, ID_VAR_CAR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6827, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new variable time: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'disp', NF_FLOAT, 5, DIM_VEC, ID_VAR_DSPL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6828, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to create new variable disp: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Keep variables IDs
!
      ID_VAR(1) = ID_VAR_DSPL 
      ID_VAR(2) = ID_VAR_CMP
      ID_VAR(3) = ID_VAR_FREQ
      ID_VAR(4) = ID_VAR_LAT    
      ID_VAR(5) = ID_VAR_LON    
!
      LAT_RANGE(1) = -90.0
      LAT_RANGE(2) =  90.0
!
      LON_RANGE(1) =  0.0
      LON_RANGE(2) =  360.0*(1.0D0 - 1.D0/M_LON)
!
      ALLOCATE ( DSPL_ARR(M_LON,M_LAT,M_FRQ,M_CMP,M_CAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_LON*M_LAT*MALO__MFRQ*M_CMP*M_CAR, STR )
           CALL ERR_LOG ( 6829, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dymanic memory for array DSPL_ARR' )
           RETURN
      END IF
      DSPL_ARR = 0.0
!
      DSPL_RANGE(1) = HEB%VAL(1,1,1,1)
      DSPL_RANGE(2) = HEB%VAL(1,1,1,1)
      ALLOCATE ( LON_ARR(M_LON) )
      ALLOCATE ( LAT_ARR(M_LAT) )
      DO 420 J2=1,MALO%NMDC
         IF ( MALO%MODC(J2)%TYP  == MALO__COS  .OR.  MALO%MODC(J2)%TYP == MALO__SIN ) THEN
              I_FRQ = ADD_CLIST ( MALO__MWAV, M_FRQ, C_WAV, MALO%MODC(J2)%WAV, IER )
              IF ( MALO%MODC(J2)%TYP == MALO__COS ) THEN
                   I_CMP = 1
                ELSE IF ( MALO%MODC(J2)%TYP == MALO__COS ) THEN
                   I_CMP = 2
              END IF
              FREQ_ARR(I_FRQ) = MALO%MODC(J2)%FRQ
              PHAS_ARR(I_FRQ) = MALO%MODC(J2)%PHS
!
              DO 430 J3=1,M_CAR
                 DO 440 J4=1,M_LAT 
                    DO 450 J5=1,M_LON
                       DSPL_ARR(J5,J4,I_FRQ,I_CMP,J3) = HEB%VAL(J3,J5,J4,J2)
                       IF ( DSPL_ARR(J5,J4,I_FRQ,I_CMP,J3) < DSPL_RANGE(1) ) THEN
                            DSPL_RANGE(1) = DSPL_ARR(J5,J4,I_FRQ,I_CMP,J3) 
                       END IF
                       IF ( DSPL_ARR(J5,J4,I_FRQ,I_CMP,J3) > DSPL_RANGE(2) ) THEN
                            DSPL_RANGE(2) = DSPL_ARR(J5,J4,I_FRQ,I_CMP,J3) 
                       END IF
 450                CONTINUE
 440             CONTINUE
 430          CONTINUE
         END IF
 420  CONTINUE
!!   write ( 6, * ) ' dspl_range = ', dspl_range ! %%%%%
!
      VALID_DSPL_RANGE(1) = -1.0 ! 1m
      VALID_DSPL_RANGE(2) =  1.0 ! 1m
!
! --- Attributes for lattitude
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_LAT, 'units', &
     &                         LEN('degrees_north'), 'degrees_north' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6830, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_LAT, 'long_name', &
     &                         LEN('Latitude'), 'Latitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6831, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_DOUBLE ( NCID, ID_VAR_LAT, 'actual_range', NF_DOUBLE, &
     &                         2, LAT_RANGE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6832, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Attributes for longitude
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_LON, 'units', &
     &                         LEN('degrees_east'), 'degrees_east' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6833, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_LON, 'long_name', &
     &                         LEN('Longitude'), 'Longitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6834, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
      IS = NF_PUT_ATT_DOUBLE ( NCID, ID_VAR_LON, 'actual_range', NF_DOUBLE, &
     &                         2, LON_RANGE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6835, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Attributes for cartesian components
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_CAR, 'long_name', &
     &                         LEN('Cartesian Component: up, east or north'), &
     &                             'Cartesian Component: up, east or north' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6836, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Attributes for cos/sin components
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_CMP, 'long_name', &
     &                         LEN('Component: either cosine or sine'), &
     &                             'Component: either cosine or sine' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6837, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Attributes for frequencies
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_FREQ, 'units', &
     &                         LEN('rad/sec'), 'rad/sec' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6838, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_FREQ, 'long_name', &
     &                         LEN('Frequency'), 'Frequency' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6839, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_DOUBLE ( NCID, ID_VAR_FREQ, 'actual_range', NF_DOUBLE, &
     &                         2, FRQ_RANGE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6840, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Attributes for displacements
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_DSPL, 'units', &
     &                         LEN('meter'), 'meter' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6841, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR_DSPL, 'long_name', &
     &           LEN('Displacement caused by loading'), &
     &               'Displacement caused by loading'   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6842, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_DSPL, 'actual_range', NF_FLOAT, &
     &                       2, DSPL_RANGE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6843, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_DSPL, 'valid_range', NF_FLOAT, &
     &                       2, VALID_DSPL_RANGE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6844, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &          NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6845, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &          NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_DSPL, 'scale_factor', NF_FLOAT, &
     &                        1, 1.0 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6846, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_DSPL, 'add_offset', NF_FLOAT, &
     &                        1, 0.0 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6847, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_DSPL, 'missing_value', NF_FLOAT, &
     &                       1, 1.E14 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6848, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'History', &
     &                       I_LEN(HEB%HISTORY), HEB%HISTORY )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6849, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'Source', &
     &                       I_LEN(HEB%SOURCE), HEB%SOURCE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6850, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'Title', &
     &                       I_LEN(HEB%TITLE), HEB%TITLE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6851, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'Institution', &
     &                       I_LEN(HEB%INSTITUTION), HEB%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6852, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'References', &
     &                       I_LEN(HEB%REFERENCES), HEB%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6853, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'ProductionDateTime', &
     &                       I_LEN(HEB%PROD_DATE_TIME), HEB%PROD_DATE_TIME )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6854, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'LongName', &
     &                       I_LEN(HEB%PROD_NAME), HEB%PROD_NAME )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6855, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write the attribute '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Enough hassle with attributes! It is so boring...
!
      IS = NF_ENDDEF ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6856, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' NF_ENDDEF for output file '//FILNC(1:I_LEN(FILNC))// &
     &         ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      LAT_STEP = 180.0/M_LAT
      DO 460 J6=1,M_LAT
         LAT_ARR(J6) = -90.0 + LAT_STEP*(J6-1)
 460  CONTINUE
!
      LON_STEP = 360.0/M_LON
      DO 470 J7=1,M_LON
         LON_ARR(J7) =  0.0 + LON_STEP*(J7-1)
 470  CONTINUE
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LON, LON_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6857, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable LON '//&
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LAT,  LAT_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6858, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable LAT '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Write the first element of array CMP_ARR 
!
      IDM(1) = 1
      IDM(2) = 1  
      ICN(1) = 5
      ICN(2) = 1
!
! --- Write the first element of array CMP_ARR 
!
      CMP_ARR(1) = 'cos  '
      CMP_ARR(2) = 'sin  '
!
      IS = NF_PUT_VARA_TEXT  ( NCID, ID_VAR_CMP, IDM, ICN, CMP_ARR(1) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6859, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable CMP '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Write the second element of array CMP_ARR 
!
      IDM(2) = 2  
      IS = NF_PUT_VARA_TEXT  ( NCID, ID_VAR_CMP, IDM, ICN, CMP_ARR(2) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6860, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable CMP '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      CAR_ARR(1) = 'up   '
      CAR_ARR(2) = 'east '
      CAR_ARR(3) = 'north'
!
      IDM(2) = 1  
      IS = NF_PUT_VARA_TEXT  ( NCID, ID_VAR_CAR, IDM, ICN, CAR_ARR(1) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6861, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable CMP '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Write the second element of array CMP_ARR 
!
      IDM(2) = 2  
      IS = NF_PUT_VARA_TEXT  ( NCID, ID_VAR_CAR, IDM, ICN, CAR_ARR(2) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6862, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable CAR '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Write the third element of array CMP_ARR 
!
      IDM(2) = 3  
      IS = NF_PUT_VARA_TEXT  ( NCID, ID_VAR_CAR, IDM, ICN, CAR_ARR(3) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6863, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable CAR '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_VAR_DOUBLE ( NCID, ID_VAR_FREQ, FREQ_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6864, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable FREQ '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_VAR_DOUBLE ( NCID, ID_VAR_PHAS, PHAS_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6865, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable FREQ '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_DSPL, DSPL_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6866, IUER, 'HEB_MODEL_TO_NC', 'Error in '// &
     &         ' an attempt to write variable DSPL '// &
     &         NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
! --- Uph! Close file and go home to drink hot tea (or cold bear)
!
      IS = NF_CLOSE ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6867, IUER, 'HEB_MODEL_TO_NC', 'Error in an attempt '// &
     &         'to close the output netcf file '//FILNC(1:I_LEN(FILNC))// &
     &         ' NF_CLOSE: '//NF_STRERROR(IS) )
           DEALLOCATE ( LON_ARR    )
           DEALLOCATE ( LAT_ARR    )
           RETURN
      END IF
!
      DEALLOCATE ( DSPL_ARR )
      DEALLOCATE ( LON_ARR  )
      DEALLOCATE ( LAT_ARR  )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  HEB_MODEL_TO_NC  !#!#
