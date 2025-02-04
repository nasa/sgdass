      SUBROUTINE OMCT_TO_SPR ( MALO, HEB_LS, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine OMCT_TO_SPR 
! *                                                                      *
! * ###  21-MAR-2014    OMCT_TO_SPR   v1.0 (c) L. Petrov 21-MAR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_LS
      INTEGER*4  IVRB, IUER
      CHARACTER  STR*128
      INTEGER*8  FSH
      REAL*8     PHI, LAM, LON, LAT, LONJ, LATJ, DIST, WEI, SUM_PAR, SUM_WEI, DP
      REAL*8,    ALLOCATABLE :: SPR(:,:), SPH(:,:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           NLON, NLAT, MDEG, NDEG, JLON, JLAT, IER
      LOGICAL*1  FL_PLOT
      PARAMETER  ( FL_PLOT = .FALSE. )
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*8, EXTERNAL :: SPHE_INIT 
      REAL*8,    EXTERNAL :: SPHE_COMP_VAL, ARC_LEN_AD
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      NLON = HEB_LS%DIMS(1)
      NLAT = HEB_LS%DIMS(2)
      MDEG = NLON/4-1
      NDEG = MALO%ORD_SPHE
!
      ALLOCATE ( SPR(NLON,NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*NLON*NLAT, STR )
           CALL ERR_LOG ( 6612, IUER, 'OMCT_TO_SPR', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of memory for array SPH' )
           RETURN 
      END IF
!
      ALLOCATE ( SPH(2,0:MDEG,0:MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*2*(MDEG+1)**2, STR )
           CALL ERR_LOG ( 6612, IUER, 'OMCT_TO_SPR', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of memory for array SPH' )
           RETURN 
      END IF
      DO 410 J1=1,MALO%NTIM
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, '(A,I2)' ) 'MALO '//GET_CDATE()//' started  malo_clean for epoch ', J1
              CALL FLUSH ( 6 )
         END IF
         SPH = 0.0D0
         DO 420 J2=0,NDEG
            DO 430 J3=0,NDEG
               SPH(1:2,J3,J2) = MALO%SPH(1:2,J3,J2,MALO__K,J1) 
 430        CONTINUE 
 420     CONTINUE 
!
         CALL ERR_PASS ( IUER, IER )
         CALL MALO_CLEAN_SH ( NLON, NLAT, MDEG, NDEG, HEB_LS%VAL, SPH, SPR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6613, IUER, 'OMCT_TO_SPR', 'Failure in '// &
     &            'an attempt to regird the OMCT dataset' )
              RETURN 
         END IF
!
         MALO%SPR(1:NLON,1:NLAT,J1) = SPR(1:NLON,1:NLAT)
         IF ( FL_PLOT ) THEN
              STR = 'OMCT bottom pressure' 
              CALL ERR_PASS ( IUER, IER )
              CALL PLOT_GRID_R4 ( 1, 7, 0, 1, NLON, NLAT, MALO%SPR(1,1,J1), STR, &
     &                            'Pa', 1.0, -1.0, '/tmp/foo', IER )
         END IF
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, '(A,I2)' ) 'MALO '//GET_CDATE()//' finished malo_clean for epoch ', J1
              CALL FLUSH ( 6 )
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( SPH )
      DEALLOCATE ( SPR )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  OMCT_TO_SPR   !#!#
