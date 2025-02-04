      SUBROUTINE READ_IONEX_FILE ( FILIN, IONEX_BUF, NION, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_IONEX_FILE. I did not find source code for            *
! *   a subroutine that would decompress .Z files which were compress    *
! *   with an archaic Unix routine. Therefore I had to ivoke gzip        *
! *   in a temprary directory. Damn it!                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     FILIN ( CHARACTER ) -- Name of the input file.                   *
! *                                                                      *
! * _________________________ Ouptut parameters: _______________________ *
! *                                                                      *
! * IONEX_BUF ( CHARACTER ) -- Text buffer with uncompressed contents    *
! *                            of the input file.                        *
! *      NION ( INTEGER*4 ) -- The number of lines in the buffer.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *                                                                      *
! * ### 07-MAY-2010 READ_IONEX_FILE  v1.1 (c)  L. Petrov 07-FEB-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      CHARACTER  FILIN*(*), IONEX_BUF(M__IOB)*(*)
      INTEGER*4  NION, IUER
      INTEGER*1, ALLOCATABLE ::  CMPR_BUF(:), UNCMPR_BUF(:)
      CHARACTER  STR*128, PID_STR*8, FIL_TMP*128
      LOGICAL*1  LEX
      INTEGER*4  J1, J2, UNIX_DATE, LUN, IS, IR, PID, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GETPID, SYSTEM
!
      CALL CLRCH ( PID_STR )
      PID = GETPID()
      CALL INCH ( PID, PID_STR )
      CALL CHASHR ( PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
      IF ( FILIN(ILEN(FILIN)-1:ILEN(FILIN)) .EQ. '.Z'   .OR. &
     &     FILIN(ILEN(FILIN)-1:ILEN(FILIN)) .EQ. '.z'   .OR. &
     &     FILIN(ILEN(FILIN)-2:ILEN(FILIN)) .EQ. '.gz'       ) THEN
!
! -------- Made a temporary file name
!
           FIL_TMP = '/tmp/gti_'//PID_STR
!
! -------- Check, whehter temporary file file exist. If yes, remove it
!
           INQUIRE ( FILE=FIL_TMP, EXIST=LEX )
           IF ( LEX ) CALL UNLINK ( FIL_TMP(1:I_LEN(FIL_TMP))//CHAR(0) )
!
! -------- Uncompress file by invoking gzip
!
           STR = 'gzip -dc '//FILIN(1:I_LEN(FILIN))//' > '//FIL_TMP
           IS = SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 4321, IUER, 'READ_IONEX_FILE', 'Failure '// &
     &              'to decompress input IONEX file '//FILIN )
                RETURN 
           END IF
!
! -------- Read the file into the buffer
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( FIL_TMP, M__IOB, IONEX_BUF, NION, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4322, IUER, 'READ_IONEX_FILE', 'Failure '// &
     &              'to read decompressed input IONEX file '//FILIN )
                CALL UNLINK ( FIL_TMP(1:I_LEN(FIL_TMP))//CHAR(0) )
                RETURN 
           END IF
!
! -------- Remove remporary file
!
           CALL UNLINK ( FIL_TMP(1:I_LEN(FIL_TMP))//CHAR(0) )
         ELSE
!
! -------- Read the file into the buffer
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FILIN, M__IOB, IONEX_BUF, NION, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4323, IUER, 'READ_IONEX_FILE', 'Failure '// &
     &              'to read decompressed input IONEX file '//FILIN )
                CALL UNLINK ( FIL_TMP(1:I_LEN(FIL_TMP))//CHAR(0) )
                RETURN 
           END IF
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_IONEX_FILE  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARSE_IONEX_FILE ( NION, IONEX_BUF, VIO, FL_SOUTH_POLE, &
     &                              FL_NORTH_POLE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_IONEX_FILE  parses the buffer that contains the      *
! *   ionosphere total electron contents map in IONEX format and loads   *
! *   its contetns into the fields of object VIO. VIO will have          *
! *   the data from the header of input files and the body of files:     *
! *   the total electron contents values at the global grid. The global  *
! *   grid may or may not have the data for the north and/or south       *
! *   poles.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      NION ( INTEGER*4 ) -- The number of lines in the buffer.        *
! * IONEX_BUF ( CHARACTER ) -- Text buffer with uncompressed contents    *
! *                            of the input file.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      VIO ( IONO__TYPE ) -- Data structure that keeps the data        *
! *                            related to ionosphere TEC maps.           *
! *                            It contains the header that describes     *
! *                            the dataset and the data.                 *
! * FL_SOUTH_POLE ( LOGICAL*1 ) -- Flag: if true, then the TEC at        *
! *                                south pole is present.                *
! * FL_NORTH_POLE ( LOGICAL*1 ) -- Flag: if true, then the TEC at        *
! *                                north pole is present.                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 07-MAY-2010  PARSE_IONEX_FILE v1.0 (c) L. Petrov 07-MAY-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'viono.i'
      TYPE ( IONO__TYPE ) :: VIO
      INTEGER*4  NION, IUER
      CHARACTER  IONEX_BUF(NION)*(*)
      CHARACTER  STR_DAT*19
      INTEGER*4  M__I2, M__R8
      PARAMETER  ( M__I2 = 16 )
      LOGICAL*1  FL_SOUTH_POLE, FL_NORTH_POLE
      REAL*8     LAT_MIN, LAT_MAX, LAT_STEP, LON_MIN, LON_MAX, LON_STEP, &
     &           ARR_R8(M__I2)
      CHARACTER  STR*128
      INTEGER*4  LAT_SIGN
      INTEGER*4  J1, J2, J3, J4, INTRV, I2_ARR(M__I2), IEXP, IND_EPC, &
     &           IND_LAT, IND_LON, LAST_LON, NUM_RECS, LAST_REC, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initialization
!
      IF ( VIO%STATUS_VAL == VIO__ALLO .OR. &
     &     VIO%STATUS_VAL == VIO__READ      ) THEN
           DEALLOCATE ( VIO%TEC_VAL )
      END IF
      CALL NOUT ( SIZEOF(VIO), VIO )
!
! --- Check the first line for IONEX magic
!
      IF ( IONEX_BUF(1)(21:35) .NE. 'IONOSPHERE MAPS' ) THEN
           CALL ERR_LOG ( 4331, IUER, 'PARSE_IONEX_FILE', 'Unrecognized '// &
     &         'format if the input IONEX file -- the line does not '// &
     &         'contains magic IONOSPHERE MAPS' )
           RETURN 
      END IF
!
      FL_SOUTH_POLE = .FALSE.
      FL_NORTH_POLE = .FALSE.
      VIO%HEADER%MISSING = VIONO__MISSING 
      LAT_SIGN = 1
!
! --- Parse the file contents
!
      DO 410 J1=2,NION
         IF ( IONEX_BUF(J1)(61:65) == 'PGM /' ) THEN
!
! ----------- Get information about the model name and author name
!
              VIO%HEADER%MODEL  = IONEX_BUF(J1)(1:16) 
              VIO%HEADER%AUTHOR = IONEX_BUF(J1)(21:36) 
           ELSE IF ( IONEX_BUF(J1)(61:78) == 'EPOCH OF FIRST MAP' ) THEN
!
! ----------- Parse epoch of the first map
!
              STR_DAT = IONEX_BUF(J1)(3:6)//'_'// &
     &                  IONEX_BUF(J1)(11:12)//'_'// &
     &                  IONEX_BUF(J1)(17:18)//'_'// &
     &                  IONEX_BUF(J1)(23:24)//'_'// &
     &                  IONEX_BUF(J1)(29:30)//':'// &
     &                  IONEX_BUF(J1)(35:36)//':00'
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( STR_DAT, VIO%HEADER%MJD_BEG, &
     &                            VIO%HEADER%UTC_BEG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4332, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure to decode the first epoch date '//STR_DAT )
                   RETURN 
              END IF
           ELSE IF ( IONEX_BUF(J1)(61:68) == 'INTERVAL' ) THEN
!
! ----------- Parse the interval between TEC maps
!
              READ ( UNIT=IONEX_BUF(J1)(1:6), FMT='(I6)', IOSTAT=IER ) INTRV
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4333, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure in decoding the value of INTERVAL '// &
     &                  IONEX_BUF(J1)(1:6) )
                   RETURN 
              END IF
              VIO%HEADER%TIM_STEP = INTRV
           ELSE IF ( IONEX_BUF(J1)(61:77) == '# OF MAPS IN FILE' ) THEN
!
! ----------- Get the number of TEC maps
!
              READ ( UNIT=IONEX_BUF(J1)(1:6), FMT='(I6)', IOSTAT=IER ) VIO%HEADER%NEPC
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4334, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure in decoding the value of # OF MAPS IN FILE '// &
     &                  IONEX_BUF(J1)(1:6) )
                   RETURN 
              END IF
              IF ( VIO%HEADER%NEPC == 13 ) THEN
!
! ---------------- Eliminate the devil's dozen!
!
                   VIO%HEADER%NEPC = 12
              END IF
              IF ( VIO%HEADER%NEPC == 25 ) THEN
                   VIO%HEADER%NEPC = 24
              END IF
           ELSE IF ( IONEX_BUF(J1)(61:78) == 'HGT1 / HGT2 / DHGT' ) THEN
!
! ----------- Get the hieght of the TEC map  (is it relevant at all?)
!
              READ ( UNIT=IONEX_BUF(J1)(1:20), FMT='(2X,3F6.1)', IOSTAT=IER ) ARR_R8(1:3)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4335, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure in decoding the value of HGT1 '// &
     &                  IONEX_BUF(J1)(1:20) )
                   RETURN 
              END IF
              VIO%HEADER%HEIGHT = ARR_R8(1)*1.D3 ! transform it in meters 
           ELSE IF ( IONEX_BUF(J1)(61:78) == 'LAT1 / LAT2 / DLAT' ) THEN
!
! ----------- Get information about grid: latitude
!
              READ ( UNIT=IONEX_BUF(J1)(1:20), FMT='(2X,3F6.1)', IOSTAT=IER ) &
     &               LAT_MIN, LAT_MAX, LAT_STEP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4336, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure in decoding the value of LAT1 '// &
     &                  IONEX_BUF(J1)(1:20) )
                   RETURN 
              END IF
              IF ( DABS( LAT_MIN - 90.0D0 ) < DABS(LAT_STEP)/2.0D0 ) FL_NORTH_POLE = .TRUE.
              IF ( DABS( LAT_MAX - 90.0D0 ) < DABS(LAT_STEP)/2.0D0 ) FL_NORTH_POLE = .TRUE.
              IF ( DABS( LAT_MIN + 90.0D0 ) < DABS(LAT_STEP)/2.0D0 ) FL_SOUTH_POLE = .TRUE.
              IF ( DABS( LAT_MAX + 90.0D0 ) < DABS(LAT_STEP)/2.0D0 ) FL_SOUTH_POLE = .TRUE.
              VIO%HEADER%LAT_MIN = MIN ( LAT_MIN, LAT_MAX )*DEG__TO__RAD
              LAT_MAX = MAX ( LAT_MIN, LAT_MAX )*DEG__TO__RAD
              IF ( LAT_STEP > 0 ) THEN
                   LAT_SIGN = 1
                   VIO%HEADER%LAT_STEP = LAT_STEP*DEG__TO__RAD
                ELSE
                   LAT_SIGN = -1
                   VIO%HEADER%LAT_STEP = -LAT_STEP*DEG__TO__RAD
              END IF
              VIO%HEADER%NLAT = IDNINT( (LAT_MAX - VIO%HEADER%LAT_MIN)/ &
     &                                   VIO%HEADER%LAT_STEP ) + 1
              IF ( .NOT. FL_NORTH_POLE .AND. LAT_MAX >  80.0D0 ) THEN
                   VIO%HEADER%NLAT = VIO%HEADER%NLAT + 1
              END IF
              IF ( .NOT. FL_SOUTH_POLE .AND. LAT_MIN < -80.0D0 ) THEN
                   VIO%HEADER%NLAT = VIO%HEADER%NLAT + 1
                   VIO%HEADER%LAT_MIN = VIO%HEADER%LAT_MIN - VIO%HEADER%LAT_STEP
              END IF
           ELSE IF ( IONEX_BUF(J1)(61:78) == 'LON1 / LON2 / DLON' ) THEN
!
! ----------- Get information about grid: longitude 
!
              READ ( UNIT=IONEX_BUF(J1)(1:20), FMT='(2X,3F6.1)', IOSTAT=IER ) &
     &               LON_MIN, LON_MAX, LON_STEP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4337, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure in decoding the value of LON1 '// &
     &                  IONEX_BUF(J1)(1:20) )
                   RETURN 
              END IF
              VIO%HEADER%LON_MIN  = MIN ( LON_MIN, LON_MAX )*DEG__TO__RAD
              LON_MAX = MAX ( LON_MIN, LON_MAX )*DEG__TO__RAD
              VIO%HEADER%LON_STEP = LON_STEP*DEG__TO__RAD
              VIO%HEADER%NLON = IDNINT( (LON_MAX - VIO%HEADER%LON_MIN)/ &
     &                                  VIO%HEADER%LON_STEP ) + 1
           ELSE IF ( IONEX_BUF(J1)(61:68) == 'EXPONENT' ) THEN
!
! ----------- Get the scaling factor that should be applided to raw numbers
!
              READ ( UNIT=IONEX_BUF(J1)(1:6), FMT='(I6)', IOSTAT=IER ) IEXP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4338, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure in decoding the value of EXPONENT'// &
     &                  IONEX_BUF(J1)(1:6) )
                   RETURN 
              END IF
              VIO%HEADER%SCALE = 10.0D0**(IEXP)
           ELSE IF ( IONEX_BUF(J1)(61:76) == 'START OF TEC MAP' ) THEN
!
! ----------- Get the TEC map index
!
              READ ( UNIT=IONEX_BUF(J1)(1:6), FMT='(I6)', IOSTAT=IER ) IND_EPC
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4339, IUER, 'PARSE_IONEX_FILE', &
     &                 'Failure in decoding the value of START OF TEC MAP'// &
     &                  IONEX_BUF(J1)(1:6) )
                   RETURN 
              END IF
              IF ( IND_EPC > VIO%HEADER%NEPC ) GOTO 810 !  Devil's doezen
              IF ( IND_EPC == 1 ) THEN
!
! ---------------- First index? Allocate memory
!
                   ALLOCATE ( VIO%TEC_VAL(VIO%HEADER%NLON,VIO%HEADER%NLAT,VIO%HEADER%NEPC), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 2*VIO%HEADER%NLON*VIO%HEADER%NLAT*VIO%HEADER%NEPC, STR )
                        CALL ERR_LOG ( 4340, IUER, 'PARSE_IONEX_FILE', &
     &                      'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                      ' bytes of dynamic memory for TEC maps' )
                        RETURN 
                   END IF
              END IF
              VIO%STATUS_VAL = VIO__ALLO 
!
! ----------- Initialize the latitude index. Specially consider the case
! ----------- whether the map contains northern and/or south poles
!
              IF ( LAT_SIGN == 1 ) THEN
                   IND_LAT = 0
                   IF ( .NOT. FL_SOUTH_POLE ) IND_LAT = IND_LAT + 1
                 ELSE 
                   IND_LAT = VIO%HEADER%NLAT + 1
!!                   IF ( .NOT. FL_NORTH_POLE ) IND_LAT = IND_LAT - 1
              END IF
           ELSE IF ( IONEX_BUF(J1)(61:80) == 'LAT/LON1/LON2/DLON/H' ) THEN
!
! ----------- Get the TEC map
!
! ----------- Get the number of records that we have to parse.
! ----------- NB: the last record may be incomplete
!
              NUM_RECS = VIO%HEADER%NLON/M__I2
              IF ( NUM_RECS*M__I2 < VIO%HEADER%NLON ) THEN
                   LAST_REC = VIO%HEADER%NLON - NUM_RECS*M__I2 
                   NUM_RECS = NUM_RECS + 1
                 ELSE 
                   LAST_REC = M__I2
              END IF
              IND_LAT = IND_LAT + LAT_SIGN
              IND_LON = 1
              DO 420 J2=1,NUM_RECS
                 IF ( J2 < NUM_RECS ) THEN
                      LAST_LON = IND_LON + M__I2 - 1
                    ELSE 
                      LAST_LON = IND_LON + LAST_REC - 1
                 END IF
!
! -------------- Read the record
!
                 READ ( UNIT=IONEX_BUF(J1+J2)(1:80), FMT='(16I5)', IOSTAT=IER ) &
     &                  VIO%TEC_VAL(IND_LON:LAST_LON,IND_LAT,IND_EPC)
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J1+J2, STR )
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG  ( 4341, IER, 'PARSE_IONEX_FILE', &
     &                    'Failure in reading line '// &
     &                     STR(1:I_LEN(STR))//' of the input IONEX '// &
     &                    'file with TEC model: '// &
     &                     IONEX_BUF(J1+J2)(1:80) )
!
! ------------------- Set default TEC: zero
!
                      VIO%TEC_VAL(IND_LON:LAST_LON,IND_LAT,IND_EPC) = -1
                 END IF 
                 IND_LON = IND_LON + M__I2
 420          CONTINUE 
              IF ( .NOT. FL_SOUTH_POLE .AND. LAT_MIN < -80.0D0 ) THEN
                   VIO%TEC_VAL(1:VIO%HEADER%NLON,1,IND_EPC) = VIONO__MISSING 
              END IF
              IF ( .NOT. FL_NORTH_POLE .AND. LAT_MAX >  80.0D0 ) THEN
                   VIO%TEC_VAL(1:VIO%HEADER%NLON,VIO%HEADER%NLAT,IND_EPC) = VIONO__MISSING 
              END IF
           ELSE IF ( IONEX_BUF(J1)(61:76) == 'START OF RMS MAP' ) THEN
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
      VIO%STATUS_VAL = VIO__READ
!
!      write ( 6, * ) ' VIO%HEADER%HEIGHT   = ', VIO%HEADER%HEIGHT 
!      write ( 6, * ) ' VIO%HEADER%LAT_MIN  = ', VIO%HEADER%LAT_MIN 
!      write ( 6, * ) ' VIO%HEADER%LAT_STEP = ', VIO%HEADER%LAT_STEP
!      write ( 6, * ) ' VIO%HEADER%LON_MIN  = ', VIO%HEADER%LON_MIN 
!      write ( 6, * ) ' VIO%HEADER%LON_STEP = ', VIO%HEADER%LON_STEP
!      write ( 6, * ) ' VIO%HEADER%SCALE    = ', VIO%HEADER%SCALE    
!      write ( 6, * ) ' VIO%HEADER%NLON     = ', VIO%HEADER%NLON
!      write ( 6, * ) ' VIO%HEADER%NLAT     = ', VIO%HEADER%NLAT    
!      write ( 6, * ) ' fl_poles= ', FL_NORTH_POLE, FL_SOUTH_POLE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PARSE_IONEX_FILE  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GTI_CREATE ( VIO, VIO_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GTI_CREATE  creates the output file with the ionosphere    *
! *   total electron contents maps and writes there the header.          *
! *   GTI_CREATE does not write the cody of the ionosphere total         *
! *   electron map.                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VIO ( IONO__TYPE ) -- Data structure that keeps the data        *
! *                            related to ionosphere TEC maps.           *
! *                            It contains the header that describes     *
! *                            the dataset and the data.                 *
! * VIO_FILE ( CHARACTER  ) -- Name of the output file with contents of  *
! *                            of ionospher TEC maps.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 08-MAY-2010   GTI_CREATE  v1.0 (c)  L. Petrov  08-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      TYPE     ( IONO__TYPE ) :: VIO
      CHARACTER  VIO_FILE*(*)
      INTEGER*4  IUER
      LOGICAL*1  LEX
      CHARACTER  STR*128
      INTEGER*4  NEPC_SAVE, IS, LUN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, UNLINK, WRITE
!
! --- Initialization
!
      VIO%HEADER%LABEL = VIONO__LABEL
      NEPC_SAVE = VIO%HEADER%NEPC
      VIO%HEADER%NEPC = 0
!
! --- Check whether the output file exists. If yes, then remove it
!
      INQUIRE ( FILE=VIO_FILE, EXIST=LEX )
      IF ( LEX ) THEN
           IS = UNLINK ( VIO_FILE(1:I_LEN(VIO_FILE))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4351, IUER, 'GTI_CREATE', 'Failure '// &
     &              'to remove exising binary ionosphere file '// &
     &               VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
                VIO%HEADER%NEPC = NEPC_SAVE 
                RETURN 
           END IF
      END IF
!
! --- Open the output file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( VIO_FILE, 'NEW', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4352, IUER, 'GTI_CREATE', 'Failure '// &
     &         'to open existing binary ionosphere file '//VIO_FILE )
           VIO%HEADER%NEPC = NEPC_SAVE 
           RETURN 
      END IF
!
! --- Write the header
!
      IS = WRITE ( %VAL(LUN), VIO%HEADER, %VAL(SIZEOF(VIO%HEADER)) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4353, IUER, 'GTI_CREATE', 'Failure '// &
     &         'to write the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           VIO%HEADER%NEPC = NEPC_SAVE 
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(VIO%HEADER) ) THEN
           CALL ERR_LOG ( 4354, IUER, 'GTI_CREATE', 'Failure '// &
     &         'to Write the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- not all bytes have '// &
     &         'been written: no space left at disk?' )
           VIO%HEADER%NEPC = NEPC_SAVE 
           RETURN 
      END IF
!
! --- Close the output file
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4355, IUER, 'GTI_CREATE', 'Failure '// &
     &         'to close binary ionosphere file '//VIO_FILE )
           VIO%HEADER%NEPC = NEPC_SAVE 
           RETURN 
      END IF
      VIO%HEADER%NEPC = NEPC_SAVE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE GTI_CREATE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GTI_APPEND ( VIO, VIO_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GTI_APPEND  writes the body of the object VIO that        *
! *   contains total electron contents maps defiend at a global grid     *
! *   to the file with TEC in binary VIONO format. Contents of the       *
! *   file header is updated as well.                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VIO ( IONO__TYPE ) -- Data structure that keeps the data        *
! *                            related to ionosphere TEC maps.           *
! *                            It contains the header that describes     *
! *                            the dataset and the data.                 *
! * VIO_FILE ( CHARACTER  ) -- Name of the output file with contents of  *
! *                            of ionospher TEC maps.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 08-MAY-2010   GTI_APPEND  v1.1 (c)  L. Petrov  02-DEC-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      TYPE     ( IONO__TYPE ) :: VIO
      CHARACTER  VIO_FILE*(*)
      INTEGER*4  IUER
      LOGICAL*1  LEX
      CHARACTER  STR*128
      TYPE     ( IONO__TYPE ) :: VIO_GLOB
      INTEGER*4  NEPC_SAVE, LUN, SEEK_SET, SEEK_END, LN, REC_LEN, IER
      REAL*8     TIM_DIF 
      INTEGER*8  IS, OFFS
      LOGICAL*1  FL_EPOCH_OVR
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ, WRITE
      INTEGER*8,     EXTERNAL :: LSEEK64
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
!                
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, LN )
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_END', SEEK_END, LN )
!
! --- Check whether the binary ionosphere TEC maps file exists.
!
      INQUIRE ( FILE=VIO_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4361, IUER, 'GTI_APPEND', 'Trap of internal '// &
     &         'control: cannot find binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
!
! --- Open the file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( VIO_FILE, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4362, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to open existing binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
      END IF

! --- Read the header
!
      IS = READ ( %VAL(LUN), VIO_GLOB%HEADER, %VAL(SIZEOF(VIO_GLOB%HEADER)) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4363, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to read the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(VIO%HEADER) ) THEN
           CALL ERR_LOG ( 4364, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to read  the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- not all bytes have '// &
     &         'been written: no space left at disk?' )
           RETURN 
      END IF
!
!      IF ( VIO_GLOB%HEADER%MODEL .NE. VIO%HEADER%MODEL ) THEN
!           CALL ERR_LOG ( 4365, IUER, 'GTI_APPEND', 'Input IONEX '// &
!     &         'file has a different model than the global '// &
!     &         'binary ionosphere file '//VIO_FILE )
!           RETURN 
!      END IF
!
! --- Check whether the global header consistent with the header of the 
! --- local file
!
      IF ( VIO_GLOB%HEADER%NLON .NE. VIO%HEADER%NLON ) THEN
           CALL ERR_LOG ( 4366, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different NLON parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%NLAT .NE. VIO%HEADER%NLAT ) THEN
           CALL ERR_LOG ( 4367, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different NLAT parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%TIM_STEP .NE. VIO%HEADER%TIM_STEP ) THEN
           CALL ERR_LOG ( 4368, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different TIM_STEP parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%SCALE .NE. VIO%HEADER%SCALE ) THEN
           CALL ERR_LOG ( 4369, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different SCALE parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%LON_MIN .NE. VIO%HEADER%LON_MIN ) THEN
           CALL ERR_LOG ( 4370, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different LON_MIN parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%LAT_MIN .NE. VIO%HEADER%LAT_MIN ) THEN
           CALL ERR_LOG ( 4371, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different LAT_MIN parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%LON_STEP .NE. VIO%HEADER%LON_STEP ) THEN
           CALL ERR_LOG ( 4372, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different LON_STEP parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%LAT_STEP .NE. VIO%HEADER%LAT_STEP ) THEN
           CALL ERR_LOG ( 4373, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different LAT_STEP parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
      IF ( VIO_GLOB%HEADER%HEIGHT .NE. VIO%HEADER%HEIGHT ) THEN
           CALL ERR_LOG ( 4374, IUER, 'GTI_APPEND', 'Input IONEX '// &
     &         'file has a different HEIGHT parameter than the global '// &
     &         'binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
!
! --- Get the time difference between the last epoch of the global map
! --- and the first epoch of the local ionosphere map (in seconds )
!
      TIM_DIF = (VIO%HEADER%MJD_BEG - VIO_GLOB%HEADER%MJD_BEG)*86400.0D0 + &
     &          (VIO%HEADER%UTC_BEG - VIO_GLOB%HEADER%UTC_BEG) - &
     &          VIO_GLOB%HEADER%NEPC*VIO%HEADER%TIM_STEP
!
! --- Check whether the first epoch of the local ionosphere map just follow
! --- the last epoch of the global map
!
      IF ( TIM_DIF > (-1.0-SHR__VIO)*VIO%HEADER%TIM_STEP .AND. &
     &     TIM_DIF < (-1.0+SHR__VIO)*VIO%HEADER%TIM_STEP       ) THEN
           FL_EPOCH_OVR = .TRUE.
         ELSE IF ( TIM_DIF > -SHR__VIO*VIO%HEADER%TIM_STEP .AND. &
     &             TIM_DIF <  SHR__VIO*VIO%HEADER%TIM_STEP       ) THEN
           FL_EPOCH_OVR = .FALSE.
         ELSE
           CALL CLRCH  ( STR )
           write ( 6, * ) ' VIO%HEADER%TIM_STEP = ', VIO%HEADER%TIM_STEP, ' VIO_GLOB%HEADER%NEPC= ', VIO_GLOB%HEADER%NEPC, ' VIO%HEADER%NEPC= ', VIO%HEADER%NEPC
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) TIM_DIF 
           CALL ERR_LOG ( 4376, IUER, 'GTI_APPEND', 'The difference '// &
     &         'between the last epoch of the global binary ionosphere '// &
     &         'file and the input file in the IONEX format is '// &
     &         'greater than acceptable: '//STR(1:15)//' seconds' )
           RETURN 
      END IF
!
! --- Update the global header
!
      IF ( .NOT. FL_EPOCH_OVR ) THEN
           VIO_GLOB%HEADER%NEPC = VIO_GLOB%HEADER%NEPC     + VIO%HEADER%NEPC 
         ELSE
           VIO_GLOB%HEADER%NEPC = VIO_GLOB%HEADER%NEPC - 1 + VIO%HEADER%NEPC 
      END IF
!
! --- Position the file into its beginning
!
#ifdef ADR_32BIT
      IS = LSEEK64 ( %VAL(LUN), %VAL(INT8(0)), %VAL(SEEK_SET) )
#else
      IS = LSEEK   ( %VAL(LUN), %VAL(0), %VAL(SEEK_SET) )
#endif
!
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4377, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to seek the beginning of binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
      END IF
!
! --- Write updated global header
!
      IS = WRITE ( %VAL(LUN), VIO_GLOB%HEADER, %VAL(SIZEOF(VIO_GLOB%HEADER)) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4378 , IUER, 'GTI_APPEND', 'Failure '// &
     &         'to write the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(VIO%HEADER) ) THEN
           CALL ERR_LOG ( 4379, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to write   the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- not all bytes have '// &
     &         'been written: no space left at disk?' )
           RETURN 
      END IF
!
! --- Position the global ionosphere file to the end of file
!
#ifdef ADR_32BIT
      IS = LSEEK64 ( %VAL(LUN), %VAL(INT8(0)), %VAL(SEEK_END) )
#else
      IS = LSEEK   ( %VAL(LUN), %VAL(0), %VAL(SEEK_END) )
#endif
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4380, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to seek the end of binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
      END IF
!
! --- Write the data records
!
      IF ( .NOT. FL_EPOCH_OVR ) THEN
            REC_LEN = 2*VIO%HEADER%NLON*VIO%HEADER%NLAT*VIO%HEADER%NEPC
            IS = WRITE ( %VAL(LUN), VIO%TEC_VAL, %VAL(REC_LEN) )
          ELSE
            REC_LEN = 2*VIO%HEADER%NLON*VIO%HEADER%NLAT*(VIO%HEADER%NEPC-1)
            IS = WRITE ( %VAL(LUN), VIO%TEC_VAL(1,1,2), %VAL(REC_LEN) )
      END IF
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4381, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to write the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
        ELSE IF ( IS .NE. REC_LEN ) THEN
           CALL ERR_LOG ( 4382, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to write the data record into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- not all bytes have '// &
     &         'been written: no space left at disk?' )
           RETURN 
      END IF
!
! --- Close the output file
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4383, IUER, 'GTI_APPEND', 'Failure '// &
     &         'to close binary ionosphere file '//VIO_FILE )
           VIO%HEADER%NEPC = NEPC_SAVE 
           RETURN 
      END IF

      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GTI_APPEND  !#!#
