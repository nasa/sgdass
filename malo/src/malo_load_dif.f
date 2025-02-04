      PROGRAM    MALO_LOAD_DIF
! ************************************************************************
! *                                                                      *
! *   Program MALO_LOAD_DIF
! *                                                                      *
! * ### 21-NOV-2012  MALO_LOAD_DIF v1.0 (c)  L. Petrov  21-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER  FIL1*128, FIL2*128, STR*128, TITLE*80, &
     &           C1_TXT(MALO__LOA_LTXT)*(MALO__LOA_LSTR), &
     &           C2_TXT(MALO__LOA_LTXT)*(MALO__LOA_LSTR)
      CHARACTER  WAV_NAM*8
      INTEGER*4  MLON, MLAT
      PARAMETER  ( MLON = 4*(MALO__NDEG+1)     )
      PARAMETER  ( MLAT = 2*(MALO__NDEG+1) + 1 )
      INTEGER*4  NLON1, NLAT1, MJD1, L1_TXT, DATA_OFFS1
      INTEGER*4  NLON2, NLAT2, MJD2, L2_TXT, DATA_OFFS2
      INTEGER*4  J1, J2, J3, J4, IE, IPAR, IDEV, IPAL, ISCL, IPRC, IUER
      REAL*8     TAI1, TAI2
      REAL*4     VAL_MIN, VAL_MAX
      INTEGER*4  IVEC, ICMP, IFRQ, MJD
      REAL*4,    ALLOCATABLE :: DSP1_ARR(:,:,:), DSP2_ARR(:,:,:), DIF(:,:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_load_dif file1 fil2 par'
           CALL EXIT ( 0 ) 
         ELSE 
           CALL GETARG ( 1, FIL1 ) 
           CALL GETARG ( 2, FIL2 ) 
           CALL GETARG ( 3, STR  ) 
           CALL CHIN ( STR, IPAR )
      END IF
!
      IE = LINDEX ( FIL1, '.' )
      IF ( FIL1(IE+1:) == 'loa' ) THEN
           IUER = -1
           CALL LOA_INQ ( FIL1, NLON1, NLAT1, MJD1, TAI1, MALO__LOA_LTXT, &
     &                    L1_TXT, C1_TXT, DATA_OFFS1, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, * ) ' --- 1st file '//FIL1(1:I_LEN(FIL1))
           STR = MJDSEC_TO_DATE ( MJD1, TAI1, IUER )
           WRITE ( 6, '(A)' ) '1st date: '//STR(1:21)
           DO 410 J1=1,L1_TXT
              WRITE ( 6, '(A)' ) C1_TXT(J1)(1:I_LEN(C1_TXT(J1)))
 410       CONTINUE 
!
           IUER = -1
           CALL LOA_INQ ( FIL2, NLON2, NLAT2, MJD2, TAI2, MALO__LOA_LTXT, &
     &                    L2_TXT, C2_TXT, DATA_OFFS2, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, * ) ' ' 
           WRITE ( 6, * ) ' --- 2nd file '//FIL2(1:I_LEN(FIL2))
           STR = MJDSEC_TO_DATE ( MJD2, TAI2, IUER )
           WRITE ( 6, '(A)' ) '2nd date: '//STR(1:21)
           DO 420 J2=1,L2_TXT
              WRITE ( 6, '(A)' ) C2_TXT(J2)(1:I_LEN(C2_TXT(J2)))
 420       CONTINUE 
!
           IF ( NLON1 .NE. NLON2 .OR. NLAT1 .NE. NLAT2 ) THEN
                WRITE ( 6, * ) 'Different dimensions. Cannot make comparison' 
                CALL EXIT ( 1 ) 
           END IF
!
           ALLOCATE ( DSP1_ARR(3,NLON1,NLAT1) )
           ALLOCATE ( DSP2_ARR(3,NLON1,NLAT1) )
           ALLOCATE ( DIF(NLON1,NLAT1) )
!
           IUER = -1
           CALL LOA_READ ( FIL1, NLON1, NLAT1, DATA_OFFS1, DSP1_ARR, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
           IUER = -1
           CALL LOA_READ ( FIL2, NLON2, NLAT2, DATA_OFFS2, DSP2_ARR, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
        ELSE IF ( FIL1(IE+1:) == 'nc' ) THEN
           ALLOCATE ( DSP1_ARR(MLON,MLAT,3) )
           IUER = 0
           CALL READ_LOADING_NC ( FIL1, INT8(MLON)*INT8(MLAT), NLON1, NLAT1, IVEC, ICMP, &
     &                            IFRQ, MJD1, TAI1, DSP1_ARR, WAV_NAM, IUER )
!
           DEALLOCATE ( DSP1_ARR )
           ALLOCATE ( DSP1_ARR(NLON1,NLAT1,3) )
           ALLOCATE ( DSP2_ARR(NLON1,NLAT1,3) )
           ALLOCATE ( DIF(NLON1,NLAT1) )
           CALL READ_LOADING_NC ( FIL1, INT8(3)*INT8(NLON1)*INT8(NLAT1), NLON1, NLAT1, &
     &                            IVEC, ICMP, IFRQ, MJD1, TAI1, DSP1_ARR, WAV_NAM, IUER )
           CALL READ_LOADING_NC ( FIL2, INT8(3)*INT8(NLON1)*INT8(NLAT1), NLON1, NLAT1, &
     &                            IVEC, ICMP, IFRQ, MJD1, TAI1, DSP2_ARR, WAV_NAM, IUER )
      END IF
!
      DIF = 0.0
      DO 430 J3=1,NLAT1
         DO 440 J4=1,NLON1
            IF ( IPAR == 1 .OR. IPAR == 2 .OR. IPAR == 3 ) THEN
                 IF ( FIL1(IE+1:) == 'loa' ) THEN
                      DIF(J4,J3) = (DSP2_ARR(IPAR,J4,J3) - DSP1_ARR(IPAR,J4,J3))*1000.0
                   ELSE 
                      DIF(J4,J3) = (DSP2_ARR(J4,J3,IPAR) - DSP1_ARR(J4,J3,IPAR))*1000.0
                 END IF
            END IF
            IF ( J3 == NLAT1 ) DIF(J4,J3) = 0.0
 440     CONTINUE 
 430  CONTINUE 
!
      IDEV = 1
      ISCL = 0
      IPAL = 7
      IPRC = 1
      STR = '/tmp/foo'
      IF  ( IPAR == 1 ) THEN
            TITLE = 'Differences in Up direction'
          ELSE IF  ( IPAR == 2 ) THEN
            TITLE = 'Differences in East direction'
          ELSE IF  ( IPAR == 3 ) THEN
            TITLE = 'Differences in North direction'
          ELSE 
            TITLE = 'Plot'
      END IF
   write ( 6, * ) ' mm-3'  ! %%%%%%%%%%%
      IF ( IPAR == 1 .OR. IPAR == 2 .OR. IPAR == 3 ) THEN
           IUER = -1
           VAL_MIN =  1.0
           VAL_MAX = -1.0
           CALL PLOT_GRID_R4 ( IDEV, IPAL, ISCL, IPRC, NLON1, NLAT1, DIF, &
     &                         TITLE, 'mm', VAL_MIN, VAL_MAX, STR, IUER )
      END IF
!
      END  PROGRAM   MALO_LOAD_DIF  !#!  
