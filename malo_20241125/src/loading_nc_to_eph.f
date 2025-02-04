      PROGRAM    LOADING_NC_TO_EPH
! ************************************************************************
! *                                                                      *
! *   Program  LOADING_NC_TO_EPH  computes the displacements caused by   *
! *   mass loading for the list of stations. The input for is the        *
! *   global loading displacement field in netCDF format. Program        *
! *   LOADING_NC_TO_EPH  the coefficients of expansion of the            *
! *   displacement field into 2D B-spline basis and then interpolates    *
! *   the displacement field for the positions of the specified          *
! *   stations. The results are written in EPHEDISP format.              *
! *                                                                      *
! *   Usage: loading_nv_to_eph nc_file station_file eph_file             *
! *                                                                      *
! *          nc-file      -- file with the global displacements field in *
! *                          netCDF format.                              *
! *                                                                      *
! *          station_file -- list of stations with names and Cartesian   *
! *                          coordinates in SITLIST format.              *
! *                                                                      *
! *          eph_file     -- name of the output file with station        *
! *                          displacements in EPHEDISP (ascii) format.   *
! *                                                                      *
! * ### 06-JUN-2016 LOADING_NC_TO_EPH v1.0 (c) L. Petrov 19-NOV-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo_local.i'
      INTEGER*4  MC, MS
      PARAMETER  ( MC = 8192 )
      PARAMETER  ( MS = 8192 )
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      CHARACTER  FILIN*128, FILSTA*128, FILOUT*128, WAV_NAM*4, STR*128
      CHARACTER  TITLE*128, INSTITUTION*128, HISTORY*128, REFERENCE*128, &
     &           SOURCE(MS)*128, COMMENT(MC)*128, FILDSC*128, FILCOM*128, &
     &           FILFMT*128, TMPDIR*128, SYSNAME*128, NODENAME*128, &
     &           HARDWARE*128, PID_STR*5, STR_TIME_INTR*28, &
     &           STR_TIME_READ*28, STR_TIME_WRIT*28
      REAL*4,    ALLOCATABLE :: DSPL_2D_R4(:,:,:)
      REAL*4,    ALLOCATABLE :: LON(:), LAT(:), BSPL_2D(:,:,:)
      REAL*8,    ALLOCATABLE :: DSPL_STA(:,:)
      REAL*8     TAI
      LOGICAL*1  FL_NC, FL_HEB, FL_TIMER
      INTEGER*4  NLON, NLAT, IVEC, ICMP, IFRQ, MJD, LC, LS, IUER 
      INTEGER*4  J1, J2, J3, J4, J5, IL, IND_LON, IND_LAT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8, IXMN4, GETPID
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      REAL*8,    EXTERNAL :: VAL_2D_BSPL
      REAL*4,    EXTERNAL :: VAL_2D_BSPL4
!
      FL_TIMER = .FALSE.
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: loading_nc_to_eph nc-file station_file eph_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILSTA  )
           CALL GETARG ( 3, FILOUT )
      END IF
!
#ifdef LINUX
       TMPDIR = '/dev/shm'
#else
       TMPDIR = '/tmp'
#endif
!
! --- Get PID
!
      CALL INCH     ( GETPID(), PID_STR )
      CALL CHASHR   (           PID_STR )
      CALL BLANK_TO_ZERO (      PID_STR )
!
      FILFMT = MALO_SHARE//'/ephedisp_format.txt'
      FILDSC = TRIM(TMPDIR)//'/dsc_'//PID_STR//'.txt'
      FILCOM = TRIM(TMPDIR)//'/com_'//PID_STR//'.txt'
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6271, IUER, 'LOADING_NC_TO_EPH', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize MALO object
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6272, IUER, 'LOADING_NC_TO_EPH', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read and parse input file with station names
!
      IUER = -1
      CALL MALO_INP_STA ( MAL(1), FILSTA, 0.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6273, IUER, 'LOADING_NC_TO_EPH', 'Failure in '// &
     &         'an attempt to read the header of the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Inquire loading file: get the dimension and epoch
!
      IUER = -1
      CALL INQ_LOADING_NC ( FILIN, NLON, NLAT, MJD, TAI, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6274, IUER, 'LOADING_NC_TO_EPH', 'Failure in '// &
     &         'an attempt to read the header of the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Memory allocation
!
      ALLOCATE ( DSPL_2D_R4(NLON,NLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(4)*INT8(3)*INT8(NLON)*INT8(NLAT), STR )
           IUER = -1
           CALL ERR_LOG ( 6275, IUER, 'LOADING_NC_TO_EPH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array DSPL_2D_R4' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( BSPL_2D(1-MALO__MDEG:NLON+2,1-MALO__MDEG:NLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(8)*INT8(3)*INT8(NLON+2+MALO__MDEG)*INT8(NLAT+MALO__MDEG), STR )
           IUER = -1
           CALL ERR_LOG ( 6276, IUER, 'LOADING_NC_TO_EPH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array BSPL_2D' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( LON(NLON+2), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(NLON+2), STR )
           IUER = -1
           CALL ERR_LOG ( 6277, IUER, 'LOADING_NC_TO_EPH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( LAT(NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(NLAT), STR )
           IUER = -1
           CALL ERR_LOG ( 6278, IUER, 'LOADING_NC_TO_EPH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( DSPL_STA(3,MAL(1)%NSTA), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR   )
           CALL IINCH8 ( INT8(3)*INT8(8)*INT8(MAL(1)%NSTA), STR )
           IUER = -1
           CALL ERR_LOG ( 6279, IUER, 'LOADING_NC_TO_EPH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON' )
           CALL EXIT ( 1 )
      END IF
!
! --- Extract auxulliary information from the displacement field, 
! --- in particular, the model desciption and the dataset descrition
!
      CALL WALL_TIMER ( %VAL(0) ) 
      IUER = -1
      CALL READ_LOADING_NC_INFO ( FILIN, TITLE, INSTITUTION, &
     &                            MS, LS, SOURCE, HISTORY, REFERENCE, &
     &                            MC, LC, COMMENT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6280, IUER, 'LOADING_NC_TO_EPH', 'Failure in '// &
     &         'an attempt to read the data from the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the displacement field
!
      IUER = -1
      CALL READ_LOADING_NC ( FILIN, INT8(3)*INT8(NLON)*INT8(NLAT), NLON, NLAT, &
     &                       IVEC, ICMP, IFRQ, MJD, TAI, DSPL_2D_R4, WAV_NAM, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6281, IUER, 'LOADING_NC_TO_EPH', 'Failure in '// &
     &         'an attempt to read the data from the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      CALL WALL_TIMER ( STR_TIME_READ ) 
      CALL WALL_TIMER ( %VAL(0) ) 
!
! --- Copy the displacemnt field in the array BSPL_2D with different
! --- dimensions. The contents if this file will be
!
      BSPL_2D = 0.0
      BSPL_2D(1:NLON,1:NLAT,1:3) = DSPL_2D_R4(1:NLON,1:NLAT,1:3)
!
! --- Expand the array BSPL_2D over longitude 360deg and one more node 
! --- beyond 360 deg in order to ensure clean interpolation
!
      BSPL_2D(NLON+1:NLON+1,1:NLAT,1:3) = BSPL_2D(1:1,1:NLAT,1:3) 
      BSPL_2D(NLON+2:NLON+2,1:NLAT,1:3) = BSPL_2D(2:2,1:NLAT,1:3) 
!
      DO 410 J1=1,NLON+2
         LON(J1) = 0.0 + (J1-1)*PI2/NLON
 410  CONTINUE 
!
      DO 420 J2=1,NLAT
         LAT(J2) = -P2I + (J2-1)*PI__NUM/(NLAT-1)
 420  CONTINUE
!
! --- Expand the diplsacement field into the 2D Bspline basis
!
      DO 430 J3=1,3
         IUER = -1
         CALL BSPL4_2D_CMP ( MALO__MDEG, 0, NLON+2, NLAT, LON, LAT, &
     &                       BSPL_2D(1-MALO__MDEG,1-MALO__MDEG,J3), IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6282, IUER, 'LOADING_NC_TO_EPH', 'Failure in '// &
     &              'an attempt to compute coefficients of the 2D '// &
     &              'interpolating spline for the displacement field' )
              CALL EXIT ( 1 )
         END IF
 430  CONTINUE 
!
! --- Compute the displacement field by running the interpolation
!
      DO 440 J4=1,MAL(1)%NSTA
         IND_LON = IXMN4 ( NLON+2, LON, SNGL(MAL(1)%STA(J4)%LON)     )
         IND_LAT = IXMN4 ( NLAT,   LAT, SNGL(MAL(1)%STA(J4)%LAT_GDT) )
         DO 450 J5=1,3
            DSPL_STA(J5,J4) = VAL_2D_BSPL4 ( SNGL(MAL(1)%STA(J4)%LON), SNGL(MAL(1)%STA(J4)%LAT_GDT), &
     &                                       NLON+2, NLAT, MALO__MDEG, IND_LON, IND_LAT, &
     &                                       LON, LAT, BSPL_2D(1-MALO__MDEG,1-MALO__MDEG,J5) )
 450    CONTINUE 
 440  CONTINUE 
      CALL WALL_TIMER ( STR_TIME_INTR ) 
!
      MAL(1)%NTIM = 1
      MAL(1)%MJD_BEG = MJD
      MAL(1)%TAI_BEG = TAI
      MAL(1)%MJD_END = MJD
      MAL(1)%TAI_END = TAI
!
! --- Write the comments into the temporary file
!
      IUER = -1
      CALL WR_TEXT ( LC, COMMENT, FILCOM, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6283, IUER, 'LOADING_NC_TO_EPH', 'Error in '// &
     &         'an attempt to write into the temporary file '//FILCOM )
           CALL EXIT ( 1 )
      END IF
!
! --- Write the model description into the temporary file
!
      IUER = -1
      CALL WR_TEXT ( LS-1, SOURCE, FILDSC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6284, IUER, 'LOADING_NC_TO_EPH', 'Error in '// &
     &         'an attempt to write into the temporary file '//FILDSC )
           CALL EXIT ( 1 )
      END IF
!
! --- Write the station displacements into the EPHEDISP file
!
      CALL WALL_TIMER ( %VAL(0) ) 
      IUER = -1
      CALL MALO_EPHEDISP_WRITE ( MAL(1), DSPL_STA, MALO__LABEL, FILOUT, &
     &                           FILDSC, FILCOM, FILFMT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6285, IUER, 'LOADING_NC_TO_EPH', 'Failure in '// &
     &         'writing the output file in EPHEDISP format' )
           CALL EXIT ( 1 )
      END IF
      CALL WALL_TIMER ( STR_TIME_WRIT ) 
!
! --- Remove temporary file
!
      CALL UNLINK ( TRIM(FILCOM)//CHAR(0) )
      CALL UNLINK ( TRIM(FILDSC)//CHAR(0) )
!
      IF ( FL_TIMER ) THEN
           WRITE ( 6, '(A)' ) 'Wall time reading:       '//TRIM(STR_TIME_READ)
           WRITE ( 6, '(A)' ) 'Wall time interpolating: '//TRIM(STR_TIME_INTR)
           WRITE ( 6, '(A)' ) 'Wall time writing:       '//TRIM(STR_TIME_WRIT)
      END IF
      CALL EXIT  ( 0 )
      END  PROGRAM    LOADING_NC_TO_EPH  !#!  
