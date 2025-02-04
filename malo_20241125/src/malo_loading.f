      FUNCTION   MALO_LOADING ( MALO_TASK, MALO, FSH, DEG_SPHE, &
     &                          MALO_OUTPUT, COMPR_COM, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_LOADING 
! *                                                                      *
! * ###  24-OCT-2012  MALO_LOADING  v2.1 (c) L. Petrov  27-MAY-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      INTEGER*4  MALO_LOADING 
      TYPE     ( SPHE_TYPE  ) :: FSH
      TYPE     ( MALO__TYPE ) :: MALO, MALO_AVR
      CHARACTER  MALO_TASK*(*), FILIN*128, MALO_OUTPUT*(*), COMPR_COM*(*)
      INTEGER*4  DEG_SPHE, IVRB, IUER
!
      CHARACTER  STR*128, SGN_LAT*1, PREF*128, EXT*8, FILOUT*128, &
     &           COM_STR*512, MODE*4
      REAL*8,    ALLOCATABLE :: SPH_MD(:,:,:,:), &
     &                          DSP_ARR(:,:,:), DSP_ARR3(:,:,:)
      REAL*8     LAT_VAL, LON_VAL, RES(3)
      REAL*4, ALLOCATABLE ::  ARR_R4(:,:) ! %%%%%%%%%%%%%%%%%%%%%%%%%% 
      INTEGER*4  J1, J2, J3, J4, J5, NORM, IPHS, IER, IDEV, NLAT, NLON, &
     &           MLAT, MLON, MD, MR, IP, IND_WC, IND_FRQ
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*1  LEX
      REAL*8,    EXTERNAL :: SPHE_COMP_VAL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      MALO_LOADING = 0
      IF ( MALO%SPH_STATUS .NE.  MALO__LOAD ) THEN
           CALL ERR_LOG ( 6131, IUER, 'MALO_LOADING', 'Trap of internal '// &
     &         'control: spherical harmonics have not been loaded' )
           RETURN 
      END IF   
      IF ( MALO%NTIM .NE. 1 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( MALO%NTIM, STR )
           CALL ERR_LOG ( 6132, IUER, 'MALO_LOADING', 'Trap of internal '// &
     &         'control: MALO%NTIM = '//STR(1:I_LEN(STR))//' while '// &
     &         'only 1 is supported' ) 
           RETURN 
      END IF
      MALO%MJD_END = MALO%MJD_BEG 
      MALO%TAI_END = MALO%TAI_BEG 
!
! --- Check whether the name has astrisk
!
      IND_WC = INDEX ( MALO_OUTPUT, '*' ) 
      IF ( IND_WC > 1 ) THEN
           PREF = MALO_OUTPUT(1:IND_WC-1)
           EXT =  MALO_OUTPUT(IND_WC+1:)
         ELSE 
           CALL CLRCH ( PREF )
           CALL CLRCH ( EXT  )
           IP = LINDEX ( MALO_OUTPUT, '.' ) 
           IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY .AND. &
     &          MALO%CONF%STATION_FINAM == 'NONE' ) THEN
                PREF = MALO_OUTPUT(1:IP-1)
                EXT = '.loa'
             ELSE IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY .AND. &
     &                 MALO%CONF%STATION_FINAM .NE. 'NONE' ) THEN
                PREF = MALO_OUTPUT(1:IP-1)
                EXT = '.eph'
             ELSE 
                IF ( IP > 1 ) THEN
                     EXT = MALO_OUTPUT(IP:)
                END IF
           END IF
           IF ( EXT == '.nc' ) THEN
                MODE = 'grid'
              ELSE IF ( EXT == '.loa' ) THEN
                MODE = 'stat'
              ELSE IF ( EXT == '.eph' ) THEN
                MODE = 'stat'
              ELSE IF ( EXT == '.heb' ) THEN
                MODE = 'grid'
              ELSE 
                CALL ERR_LOG ( 6133, IUER, 'MALO_LOADING', 'Unsupported '// &
     &              'output file name extension '//EXT(1:I_LEN(EXT))// &
     &              ' . Supported extnesions: .nc, .loa, .eph, .heb' )
                RETURN 
           END IF
      END IF 
!
      NORM = 1
      IPHS = 1
!
! --- Build the output file name
!
      IF ( IND_WC > 1 ) THEN
           STR = MJDSEC_TO_DATE ( MALO%MJD_ARR(1), MALO%TAI_ARR(1), -2 )
           FILOUT = PREF(1:I_LEN(PREF))//STR(1:4)//STR(6:7)//STR(9:10)// &
     &              '_'//STR(12:13)//STR(15:16)//EXT
         ELSE 
           FILOUT = MALO_OUTPUT
      END IF 
!
      IF ( MALO%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
           IND_FRQ = MALO%TAI_ARR(1)
           CALL INCH ( IND_FRQ, STR(1:2) )
           CALL CHASHR        ( STR(1:2) )
           CALL BLANK_TO_ZERO ( STR(1:2) )
           FILOUT = PREF(1:I_LEN(PREF))//'_frq'//STR(1:2)//EXT
      END IF
      IF ( IVRB .GE. 5 ) THEN
           WRITE ( 6, * ) ' filout = ', filout(1:i_len(filout)) ! %%%
      END IF
!
      IF ( MALO_TASK == 'load_update' ) THEN
           INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
           IF ( LEX ) THEN
                IF ( IVRB .GE. 4 ) THEN
                     WRITE ( 6, '(A)' ) 'MALO: File '//FILOUT(1:I_LEN(FILOUT))// &
     &                                  ' exists. Skipping' 
                END IF
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
      END IF
      IF ( MALO_TASK == 'load_d1_create' .OR. &
     &     MALO_TASK == 'load_d1_update'      ) THEN
!
! -------- In a case if compute correction for D1-term, we 
! -------- unscale spherical harmonic of degree one
! -------- and use the differences betwween Love numbers 
! -------- Total Earth minus solid Earth only, which exactly 1.
!
           MALO%SPH(1:2,0:1,1,MALO__H,1) = 1.0D0/MALO%LOVE(1,MALO__H)* &
     &                                           MALO%SPH(1:2,0:1,1,MALO__H,1)
           MALO%SPH(1:2,0:1,1,MALO__L,1) = 1.0D0/MALO%LOVE(1,MALO__L)* &
     &                                           MALO%SPH(1:2,0:1,1,MALO__L,1)
           MALO%SPH(1:2,0:2,2,MALO__H,1) = 0.0D0
           MALO%SPH(1:2,0:2,2,MALO__L,1) = 0.0D0
!
           MALO%SPH(1:2,1,0,MALO__H,1)  = MALO%SPH(1:2,0,1,MALO__H,1)  
           MALO%SPH(1:2,1,0,MALO__L,1)  = MALO%SPH(1:2,0,1,MALO__L,1)  
      END IF
      IF ( MALO_TASK == 'load_cf_create' .OR. &
     &     MALO_TASK == 'load_cf_update'      ) THEN
!
! -------- Correct spherical harmomics of degree 1 for a case when
! -------- displacements in cf frame are computed.
!
           MALO%SPH(1:2,0:1,1,MALO__H,1) = (1.0D0 + MALO%LOVE(1,MALO__H))/MALO%LOVE(1,MALO__H)* &
     &                                      MALO%SPH(1:2,0:1,1,MALO__H,1)
           MALO%SPH(1:2,0:1,1,MALO__L,1) = (1.0D0 + MALO%LOVE(1,MALO__L))/MALO%LOVE(1,MALO__L)* &
     &                                      MALO%SPH(1:2,0:1,1,MALO__L,1)
           MALO%SPH(1:2,1,0,MALO__H,1)   = MALO%SPH(1:2,0,1,MALO__H,1)  
           MALO%SPH(1:2,1,0,MALO__L,1)   = MALO%SPH(1:2,0,1,MALO__L,1)  
      END IF
!
      IF ( MODE == 'grid' ) THEN
!
! -------- Globlal, grid mode
!
           NLAT = 2*(MALO%CONF%OUTPUT_GRID_DEG+1)+1
           NLON = 4*(MALO%CONF%OUTPUT_GRID_DEG+1)
           ALLOCATE ( DSP_ARR(3,NLON,NLAT), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*NLON*NLAT, STR )
                CALL ERR_LOG ( 6135, IUER, 'MALO_LOADING', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                RETURN 
           END IF
           IF ( IVRB .GE. 4 ) THEN
                CALL WALL_TIMER ( %VAL(0) ) 
           END IF
!
#ifdef MALO_GRID_DIRECT  ! Pre Feb-2015 code. It is very slow
           DO 410 J1=1,NLAT
              LAT_VAL = -P2I + (J1-1)*PI__NUM/(NLAT-1)
              DO 420 J2=1,NLON
                 LON_VAL = (J2-1)*PI2/NLON
                 IER = -1
                 IF ( MALO_TASK == 'load_create'    .OR. &
     &                MALO_TASK == 'load_update'    .OR. &
     &                MALO_TASK == 'load_cf_create' .OR. &
     &                MALO_TASK == 'load_cf_update'      ) THEN
!
                      CALL SPHE_COMP_VEC ( FSH, DEG_SPHE, DEG_SPHE, LAT_VAL, &
     &                                     LON_VAL, NORM, IPHS, &
     &                                     MALO%SPH(1,0,0,1,1), DSP_ARR(1,J2,J1), &
     &                                     IER )
                   ELSE IF ( MALO_TASK == 'load_d1_create' .OR. &
     &                       MALO_TASK == 'load_d1_update'      ) THEN
                      CALL SPHE_COMP_VEC ( FSH, DEG_SPHE, 2, LAT_VAL, &
     &                                     LON_VAL, NORM, IPHS, &
     &                                     MALO%SPH(1,0,0,1,1), DSP_ARR(1,J2,J1), &
     &                                     IER )
                 END IF
  420         CONTINUE 
  410      CONTINUE 
#else
!
! -------- Determine MR -- the multiplicative factor output resolution
!
           MR = 4*(DEG_SPHE+1)/NLON
           IF ( MR*NLON < 4*(DEG_SPHE+1) ) MR = MR + 1
           IF ( MR < 1 ) THEN
                WRITE ( 6, * ) 'DEG_SPHE= ', DEG_SPHE, ' NLON= ', NLON
                CALL ERR_LOG ( 6136, IUER, 'MALO_LOADING', 'Trap of internal '// &
     &              'control: multiresolution factor is less than 2. '// &
     &              'Please check dimnesions' )
                RETURN 
           END IF
           MLON = NLON*MR
           MLAT = (NLAT-1)*MR + 1
           MD   = MLON/4 - 1
!
           ALLOCATE ( DSP_ARR3(MLON,MLAT,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*MLON*(MLAT-1), STR )
                CALL ERR_LOG ( 6137, IUER, 'MALO_LOADING', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                RETURN 
           END IF
!
! -------- Create the extended copy of scaled spherical harmonics
!
           ALLOCATE ( SPH_MD(2,0:MD,0:MD,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*2*2*(MD+1)*(MD+1), STR )
                CALL ERR_LOG ( 6138, IUER, 'MALO_LOADING', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                RETURN 
           END IF
           SPH_MD = 0.0D0
           DO 410 J1=0,DEG_SPHE
              DO 420 J2=0,J1
                 SPH_MD(1:2,J2,J1,1) = MALO%SPH(1:2,J2,J1,1,1)
                 SPH_MD(1:2,J2,J1,2) = MALO%SPH(1:2,J2,J1,2,1)
                 SPH_MD(1:2,J1,J2,1) = MALO%SPH(1:2,J2,J1,1,1)
                 SPH_MD(1:2,J1,J2,2) = MALO%SPH(1:2,J2,J1,2,1)
 420          CONTINUE 
 410       CONTINUE 
!
! -------- Compute the inverse vector spherical harmonics tranfrom.
! -------- The output is written in array DSP_ARR3 that is MLON*MLAT
!
           IF ( MALO_TASK == 'load_create'    .OR. &
     &          MALO_TASK == 'load_update'    .OR. &
     &          MALO_TASK == 'load_cf_create' .OR. &
     &          MALO_TASK == 'load_cf_update'      ) THEN
!
                CALL ERR_PASS ( IUER, IER )
                CALL SPHE_INV_2NN_VEC ( FSH, MD, MD, SPH_MD, MLAT, DSP_ARR3, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6139, IUER, 'MALO_LOADING', 'Failure '// &
     &                   'in an attempt to perform inverse vector spherical '// &
     &                    'harmonic transform' )
                     RETURN 
                END IF
              ELSE IF ( MALO_TASK == 'load_d1_create' .OR. &
     &                  MALO_TASK == 'load_d1_update'      ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL SPHE_INV_2NN_VEC ( FSH, MD, 2, SPH_MD, MLAT, DSP_ARR3, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6140, IUER, 'MALO_LOADING', 'Failure '// &
     &                   'in an attempt to perform inverse vector spherical '// &
     &                    'harmonic transform' )
                     RETURN 
                END IF
           END IF
!
           IF ( IVRB .GE. 8 ) THEN
                write ( 6, * ) 'malo_loading 287 mr = ', mr, ' nlon, nlat= ', nlon, nlat
                allocate ( arr_r4(nlon,nlat) )
                arr_r4(1:nlon,1:nlat) = 1000*dsp_arr3(1:nlon,1:nlat,1) 
                call plot_grid_r4 ( 1, 7, 0, 1, nlon, nlat, arr_r4, 'Up loading', 'mm', &
     &                              '/tmp/foo', IER )
                deallocate ( arr_r4 )
           END IF
           DO 430 J3=1,NLAT
              DO 440 J4=1,NLON
                 DSP_ARR(1,J4,J3) = DSP_ARR3(1+MR*(J4-1),1+MR*(J3-1),1)
                 DSP_ARR(2,J4,J3) = DSP_ARR3(1+MR*(J4-1),1+MR*(J3-1),2)
                 DSP_ARR(3,J4,J3) = DSP_ARR3(1+MR*(J4-1),1+MR*(J3-1),3)
 440          CONTINUE 
 430       CONTINUE 
!
           DEALLOCATE ( DSP_ARR3 )
           DEALLOCATE ( SPH_MD   )
#endif
!
           IF ( IVRB .GE. 4 ) THEN
                CALL WALL_TIMER ( STR ) 
                WRITE ( 6, * ) 'MALO_LOADING: Timing results: '//STR(1:I_LEN(STR))
           END IF
           IF ( EXT == '.loa' ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL LOA_WRI ( NLON, NLAT, MALO%MJD_ARR(1), &
     &                         MALO%TAI_ARR(1), DSP_ARR, FILOUT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6141, IUER, 'MALO_LOADING', 'Failure to '// &
     &                   'write loading displacements into the output file '// &
     &                    FILOUT )
                     RETURN 
                END IF
              ELSE IF ( EXT == '.nc' ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL WRITE_LOADING_NC ( NLON, NLAT, 1, 0, MALO%MJD_ARR(1), &
     &               MALO%TAI_ARR(1), DSP_ARR, %VAL(0), MALO__LABEL, 'NO', &
     &               MALO%CONF%LOA_FINAM_DESCR, MALO%CONF%LOA_FINAM_COMM, &
     &               FILOUT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6142, IUER, 'MALO_LOADING', 'Failure to '// &
     &                   'write loading displacements into the output file '// &
     &                    FILOUT )
                     RETURN 
                END IF
              ELSE IF ( EXT == '.heb' ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL WRITE_LOADING_HEB ( NLON, NLAT, MALO%MJD_ARR(1), &
     &               MALO%TAI_ARR(1), DSP_ARR, MALO__LABEL, 'NO', &
     &               MALO%CONF%LOA_FINAM_DESCR, MALO%CONF%LOA_FINAM_COMM, &
     &               FILOUT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6143, IUER, 'MALO_LOADING', 'Failure to '// &
     &                   'write loading displacements into the output file '// &
     &                    FILOUT )
                     RETURN 
                END IF
           END IF
!
           DEALLOCATE ( DSP_ARR )
           MALO_LOADING = 1
         ELSE 
!
! -------- Station mode
!
           IF ( MALO%STA_STATUS .NE. MALO__LOAD .AND. &
     &          MALO%STA_STATUS .NE. MALO__COMP       ) THEN
                WRITE ( 6, * ) ' MALO%STA_STATUS = ', MALO%STA_STATUS
                CALL ERR_LOG ( 6144, IUER, 'MALO_LOADING', 'Trap of internal '// &
     &              'control: malo status is neither '// &
     &              '"MALO__LOAD", nor "MALO__COMP" ' )
                RETURN 
           END IF
           ALLOCATE ( DSP_ARR(3,MALO%NSTA,1), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*MALO%NSTA, STR )
                CALL ERR_LOG ( 6145, IUER, 'MALO_LOADING', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                RETURN 
           END IF
           IF ( IVRB .GE. 4 ) THEN
                CALL WALL_TIMER ( %VAL(0) ) 
           END IF
           DO 450 J5=1,MALO%NSTA
              IF ( MOD(J5,100) == 0 .AND. IVRB .GE. 5 ) THEN
                   WRITE ( 6, 110 ) J5, MALO%NSTA, J5*100.0/MALO%NSTA, &
     &                               CHAR(13)
 110               FORMAT ( 'Loading for station ', I8, ' ( ', I8, ' )  ', &
     &                       F6.2,'% ',A$ )
                   CALL FLUSH ( 6 ) 
              END IF
              CALL ERR_PASS ( IUER, IER )
              IF ( MALO_TASK == 'load_create'    .OR. &
     &             MALO_TASK == 'load_update'    .OR. &
     &             MALO_TASK == 'load_cf_create' .OR. &
     &             MALO_TASK == 'load_cf_update'      ) THEN
                   CALL SPHE_COMP_VEC ( FSH, DEG_SPHE, DEG_SPHE, MALO%STA(J5)%LAT_GDT, &
     &                                  MALO%STA(J5)%LON, NORM, IPHS, &
     &                                  MALO%SPH(1,0,0,1,1), DSP_ARR(1,J5,1), IER )
                 ELSE IF ( MALO_TASK == 'load_d1_create' .OR. &
     &                     MALO_TASK == 'load_d1_update'      ) THEN
!
! ---------------- Compute expansion up to degree 2.
! ---------------- NB: we set priviosly MALO%SPH of degree 2 to zero. It was zero for degree 0
! ---------------- from the very beginning.
! ---------------- So, effectively, we compute contribution of D1 term only.
! ---------------- As of 2014.09.29 SPHE_COMP_VEC had a bug and computed N-comp
! ---------------- incorrectly when expansion of degree 1 was used, but it works
! ---------------- correctly for other degrees.
!
                   CALL SPHE_COMP_VEC ( FSH, DEG_SPHE, 2, MALO%STA(J5)%LAT_GDT, &
     &                                  MALO%STA(J5)%LON, NORM, IPHS, &
     &                                  MALO%SPH(1,0,0,1,1), DSP_ARR(1,J5,1), IER )
              END IF
 450       CONTINUE 
           IF ( IVRB .GE. 4 ) THEN
                CALL WALL_TIMER ( STR ) 
                WRITE ( 6, * ) 'MALO_LOADING: Timing results: '//STR(1:I_LEN(STR))
           END IF
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MALO_EPHEDISP_WRITE ( MALO, DSP_ARR, MALO__LABEL, FILOUT, &
     &                                MALO%CONF%LOA_FINAM_DESCR, &
     &                                MALO%CONF%LOA_FINAM_COMM, &
     &                                MALO%CONF%EPHEDISP_FINAM_FMT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6146, IUER, 'MALO_LOADING', 'Failure to '// &
     &              'write the output file '//FILOUT )
                RETURN 
           END IF
           DEALLOCATE ( DSP_ARR )       
           MALO_LOADING = 1
      END IF
!
      IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! -------- Now compress the output file 
!
           COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &               FILOUT(1:I_LEN(FILOUT))
           CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
      END IF
      IF ( IVRB .GE. 1 ) THEN
           IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
                WRITE ( 6, '(A)' ) 'MALO_LOADING: Wrote file '//FILOUT(1:I_LEN(FILOUT))//'.bz2'
              ELSE 
                WRITE ( 6, '(A)' ) 'MALO_LOADING: Wrote file '//FILOUT(1:I_LEN(FILOUT))
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   MALO_LOADING  !#!#
