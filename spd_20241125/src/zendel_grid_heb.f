      PROGRAM    ZENDEL_GRID_HEB 
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'spd.i'
      TYPE     ( HEB__TYPE    ) :: HEB_3D, HEBO
      TYPE     ( SPD_2P__TYPE ), POINTER :: SPD_2P(:)
      TYPE     ( SPD_4D__TYPE ) :: SPD_4D
      INTEGER*4    M_FIL, M_BUF
      PARAMETER  ( M_FIL  = 128*1024 )
      PARAMETER  ( M_BUF  = 1024*1024 )
      REAL*8     EPS
      PARAMETER  ( EPS = 100.0 )
      CHARACTER  FIL_RESP*128, FIL_ZEND*128, BUF(M_BUF)*256, OUT(M_BUF)*256, &
     &           DATE_FIL*21, FILNAM*128, STR*128, TEST_STR*16, EXT*4
      CHARACTER    ZENDEL_GRID_HEB__LABEL*38
      PARAMETER  ( ZENDEL_GRID_HEB__LABEL = 'ZENDEL_GRID_HEB  version of 2015.09.10' )
      REAL*4     EPS_LEV1, EPS_LEV2, EPS_LAT, EPS_LON, EPS_TIM
      PARAMETER  ( EPS_LEV1 = 0.001 )
      PARAMETER  ( EPS_LEV2 = 1.0 )
      PARAMETER  ( EPS_LAT  = 1.E-5 )
      PARAMETER  ( EPS_LON  = 3.E-5 )
      PARAMETER  ( EPS_TIM  = 1.0   )
      INTEGER*8  DIR_DESC(16), MEL
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           J12, J13, J14, J15, J16, J17, J18, &
     &           NB, LEV, L_FIL, ID, IL, IS, N_DEL, NOUT, &
     &           IVRB, MJD_BEG, MJD_END, MJD_FIL, IFMT, ISTL, &
     &           IND_BEG, IND_END, IND_TIM, DIMS(3), INDS(3), IUER
      REAL*4     ARGS(3)
      REAL*8     TAI_BEG, TAI_END, TAI_FIL, RLEV, DIST
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, GET_FILE_FROM_DIR
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR
      INTEGER*4, EXTERNAL :: IXMN4
      REAL*4,    EXTERNAL :: VAL_3D_BSPLE3J_R4
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: resp_file zendel_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_RESP )
           CALL GETARG ( 2, FIL_ZEND )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_RESP, HEB_3D, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4411, IUER, 'ZENDEL_GRID_HEB', 'Failure in reading '// &
     &         'input file '//FIL_RESP )
           CALL EXIT ( 1 )
      END IF
      HEB_3D%TAI = HEB_3D%UTC
      CALL SPD_4D_INIT ( SPD_4D, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4412, IUER, 'ZENDEL_GRID_HEB', 'Failure in an '// &
     &         'attempt to initialize object SPD' )
           CALL EXIT ( 1 )
      END IF
      SPD_4D%DIMS(1) = HEB_3D%DIMS(1) - SPD__MDEG
      SPD_4D%DIMS(2) = HEB_3D%DIMS(2) - SPD__MDEG
      SPD_4D%DIMS(3) = HEB_3D%DIMS(3) - SPD__MDEG
      SPD_4D%DIMS(4) = 1
      SPD_4D%DIMS(5) = HEB_3D%DIMS(4)
!
      IF ( ASSOCIATED ( SPD_4D%LEV ) ) DEALLOCATE ( SPD_4D%LEV )
      ALLOCATE ( SPD_4D%LEV(1-SPD__MDEG:SPD_4D%DIMS(1)+SPD__MDEG), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(SPD__MLEV+SPD_4D%DIMS(1)), STR )
           IUER = -2
           CALL ERR_LOG ( 4413, IUER, 'ZENDEL_GRID_HEB', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPD_4D%LEV' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ASSOCIATED ( SPD_4D%LON ) ) DEALLOCATE ( SPD_4D%LON )
      ALLOCATE ( SPD_4D%LON(1-SPD__MDEG:SPD_4D%DIMS(2)+SPD__MDEG), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(SPD_4D%DIMS(2)+SPD__MDEG), STR )
           IUER = -2
           CALL ERR_LOG ( 4417, IUER, 'ZENDEL_GRID_HEB', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPD_4D%LON' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ASSOCIATED ( SPD_4D%LAT ) ) DEALLOCATE ( SPD_4D%LAT )
      ALLOCATE ( SPD_4D%LAT(1-SPD__MDEG:SPD_4D%DIMS(3)+SPD__MDEG), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(SPD_4D%DIMS(3)+SPD__MDEG), STR )
           IUER = -2
           CALL ERR_LOG ( 4416, IUER, 'ZENDEL_GRID_HEB', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPD_4D%LAT' )
           CALL EXIT ( 1 )
      END IF
!

      ALLOCATE ( SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1,SPD_4D%DIMS(5)), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           MEL = INT8(SPD_4D%DIMS(1)+SPD__MDEG)*INT8(SPD_4D%DIMS(2)+SPD__MDEG)* &
     &           INT8(SPD_4D%DIMS(3)+SPD__MDEG)*INT8(SPD_4D%DIMS(4)+SPD__MDEG)* &
     &           INT8(SPD_4D%DIMS(5))
           CALL IINCH8 ( MEL, STR )
           IUER = -2
           CALL ERR_LOG ( 4419, IUER, 'ZENDEL_GRID_HEB', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array REFR' )
           CALL EXIT ( 1 )
      END IF
      SPD_4D%LEV  = 0.0
      SPD_4D%LAT  = 0.0
      SPD_4D%LON  = 0.0
      SPD_4D%REFR = 0.0
      SPD_4D%STATUS = SPD__ALLO
      SPD_4D%MJD_0 = HEB_3D%MJD
      SPD_4D%TAI_0 = HEB_3D%TAI
!
      IND_TIM = 1
      SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),IND_TIM,1:SPD_4D%DIMS(5)) = &
     &     HEB_3D%VAL(1:SPD_4D%DIMS(1)+SPD__MDEG,1:SPD_4D%DIMS(2)+SPD__MDEG,1:SPD_4D%DIMS(3)+SPD__MDEG,1:SPD_4D%DIMS(5))
!
! --- Build arrays of levels
!
      DO 460 J6=1,SPD_4D%DIMS(1)
         RLEV = J6-1 - (SPD__MLEV-1)/2
         SPD_4D%LEV(J6) = DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
 460  CONTINUE 
!
! --- Add arguments of extended knots for the height array
!
      DO 470 J7=0,1-SPD__MDEG,1
         SPD_4D%LEV(J7) = SPD_4D%LEV(J7+1) - EPS_LEV1
 470  CONTINUE 
      DO 480 J8=SPD_4D%DIMS(1)+1,SPD_4D%DIMS(1)+SPD__MDEG
         SPD_4D%LEV(J8) = SPD_4D%LEV(J8-1) + EPS_LEV2
 480  CONTINUE 
!
! --- Build arrays of longitude
!
      DO 490 J9=1,SPD_4D%DIMS(2)
!
! ------ NB: the longitudinal grid has two more nodes: at 360deg and 360deg + 1step
!
         SPD_4D%LON(J9) = (J9-1)*PI2/(SPD_4D%DIMS(2) - 2)
 490  CONTINUE 
!
! --- Add arguments of extended knots for the longitude array
!
      DO 4100 J10=0,1-SPD__MDEG,-1
         SPD_4D%LON(J10) = SPD_4D%LON(J10+1) - EPS_LON
 4100 CONTINUE 
      DO 4110 J11=SPD_4D%DIMS(2)+1,SPD_4D%DIMS(2)+SPD__MDEG
         SPD_4D%LON(J11) = SPD_4D%LON(J11-1) + EPS_LON
 4110 CONTINUE 
!
! --- Biuld the latitude array
!
      DO 4120 J12=1,SPD_4D%DIMS(3)
         SPD_4D%LAT(J12) = -P2I + (J12-1)*PI__NUM/(SPD_4D%DIMS(3)-1) 
 4120 CONTINUE 
!
! --- Add arguments of extended knots for the lattidude array
!
      DO 4130 J13=0,1-SPD__MDEG,-1
         SPD_4D%LAT(J13) = SPD_4D%LAT(J13+1) - EPS_LAT
 4130 CONTINUE 
      DO 4140 J14=SPD_4D%DIMS(3)+1,SPD_4D%DIMS(3)+SPD__MDEG
         SPD_4D%LAT(J14) = SPD_4D%LAT(J14-1)  + EPS_LAT
 4140 CONTINUE 
!
      IUER = -1
      CALL BSPL4_3D_CMP ( SPD__MDEG, 0, SPD_4D%DIMS,    &
     &                    SPD_4D%LEV(1:SPD_4D%DIMS(1)), &
     &                    SPD_4D%LON(1:SPD_4D%DIMS(2)), &
     &                    SPD_4D%LAT(1:SPD_4D%DIMS(3)), &
     &                    SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1,3), &
     &                    IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4420, IUER, 'ZENDEL_GRID_HEB', 'Failure in an '// &
     &         'attempt to expand refractivity field into the '// &
     &         '3D B-spline basis' )
           CALL EXIT ( 1 )
      END IF
!
      DIMS(1:3) = SPD_4D%DIMS(1:3)
      HEBO = HEB_3D
      HEBO%DIMS(1) = SPD_4D%DIMS(2)
      HEBO%DIMS(2) = SPD_4D%DIMS(3)
      HEBO%DIMS(3) = 1
      HEBO%DIMS(4) = 1
      ALLOCATE ( HEBO%VAL(HEBO%DIMS(1),HEBO%DIMS(2),HEBO%DIMS(3),HEBO%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4421, IUER, 'ZENDEL_GRID_HEB', 'Failure in an '// &
     &         'attempt to expand refractivity field into the '// &
     &         '3D B-spline basis' )
           CALL EXIT ( 1 )
      END IF
!
      DO 4150 J15=1,SPD_4D%DIMS(3) ! Lat
         DO 4160 J16=1,SPD_4D%DIMS(2) ! Lon
            INDS(1) = IXMN4 ( SPD_4D%DIMS(1), SPD_4D%LEV(1), 0.0E0 )
            INDS(2) = J16
            INDS(3) = J15
            IF ( INDS(2) == SPD_4D%DIMS(2) ) INDS(2) = J16-1
            IF ( INDS(3) == SPD_4D%DIMS(1) ) INDS(3) = J15-1
            ARGS(1) = 0.0E0
            ARGS(2) = SPD_4D%LON(J16)
            ARGS(3) = SPD_4D%LAT(J15)
            IF ( J15 == 1 ) ARGS(3) = ARGS(3) + 0.01*(SPD_4D%LAT(2) - SPD_4D%LAT(1))
            IF ( J16 == 1 ) ARGS(2) = ARGS(2) + 0.01*(SPD_4D%LON(2) - SPD_4D%LON(1))
            HEBO%VAL(J16,J15,1,1) = VAL_3D_BSPLE3J_R4 ( ARGS, DIMS, INDS, &
     &               SPD_4D%LEV, SPD_4D%LON, SPD_4D%LAT, &
     &               SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1,3) )
            HEBO%VAL(J16,J15,1,1) = 1.0E12*HEBO%VAL(J16,J15,1,1)/299792458.E0
 4160    CONTINUE 
 4150 CONTINUE 
!
      IUER = -1
      HEBO%SDS_NAME  = 'Zenith path delay at the sea level'
      HEBO%TITLE     = HEBO%SDS_NAME
      HEBO%PROD_NAME = HEBO%SDS_NAME
      HEBO%UNITS = 'ps'
      HEBO%PROD_DATE_TIME = GET_CDATE()
      CALL HEB_SETMIN ( HEBO, HEBO%VAL, HEBO%MIN_VALUE )
      CALL HEB_SETMAX ( HEBO, HEBO%VAL, HEBO%MAX_VALUE )
      HEBO%VALID_RANGE(1) = HEBO%MIN_VALUE
      HEBO%VALID_RANGE(2) = HEBO%MAX_VALUE
!
      IUER = -1
      CALL WRITE_HEB ( HEBO, HEBO%VAL, FIL_ZEND, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6713, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &         'to write the output file '//FIL_ZEND )
           CALL EXIT ( 1 )
      END IF
      END  !#!  
