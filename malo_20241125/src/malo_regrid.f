      SUBROUTINE MALO_REGRID ( DEG, METH, MALO_IN, MALO_MEAN, MALO_OUT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_REGRID
! *                                                                      *
! * ### 22-OCT-2012    MALO_REGRID   v1.0 (c) L. Petrov  23-OCT-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, IVRB, IUER
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO_IN, MALO_MEAN, MALO_OUT
      CHARACTER  METH*(*)
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 3 )
      REAL*4,    ALLOCATABLE :: BCF(:,:), LAT_ARR(:), LON_ARR(:)
      REAL*4     NP_SPR, ARGS(2)
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, INDS(2), IER
      REAL*4,    EXTERNAL :: VAL_2D_BSPL4 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN4 
!
      ALLOCATE ( BCF(1-MDEG:MALO_IN%NLON+1,1-MDEG:MALO_IN%NLAT+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(MALO_IN%NLON+1+MDEG)*(MALO_IN%NLAT+1+MDEG), STR )
           CALL ERR_LOG ( 6711, IUER, 'MALO_REGRID', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for array BCF' )
           RETURN 
      END IF
!
      ALLOCATE ( LAT_ARR(MALO_IN%NLAT+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(MALO_IN%NLAT+1), STR )
           CALL ERR_LOG ( 6712, IUER, 'MALO_REGRID', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for array LAT_ARR' )
           RETURN 
      END IF
!
      ALLOCATE ( LON_ARR(MALO_IN%NLON+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(MALO_IN%NLON+1), STR )
           CALL ERR_LOG ( 6713, IUER, 'MALO_REGRID', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for array LON_ARR' )
           RETURN 
      END IF
      LAT_ARR(1:MALO_IN%NLAT) = MALO_IN%LAT(1:MALO_IN%NLAT) 
      LON_ARR(1:MALO_IN%NLON) = MALO_IN%LON(1:MALO_IN%NLON) 
      LAT_ARR(MALO_IN%NLAT+1:MALO_IN%NLAT+1) = P2I
      LON_ARR(MALO_IN%NLON+1:MALO_IN%NLON+1) = PI2
!
      MALO_OUT = MALO_IN
      MALO_OUT%NLON = 4*(DEG+1)
      MALO_OUT%NLAT = 2*(DEG+1)
      MALO_OUT%LAT     => NULL()
      MALO_OUT%LON     => NULL()
      MALO_OUT%MJD_ARR => NULL()
      MALO_OUT%TAI_ARR => NULL()
      MALO_OUT%SPR     => NULL()
!
      ALLOCATE ( MALO_OUT%LON(MALO_OUT%NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH  ( 4*MALO_OUT%NLON, STR )
           CALL ERR_LOG ( 6714, IUER, 'MALO_REGRID', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO_OUT%LON' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO_OUT%LAT(MALO_OUT%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO_OUT%NLAT, STR )
           CALL ERR_LOG ( 6715, IUER, 'MALO_REGRID', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO_OUT%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO_OUT%MJD_ARR(MALO_OUT%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO_OUT%NTIM, STR )
           CALL ERR_LOG ( 6716, IUER, 'MALO_REGRID', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO_OUT%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO_OUT%TAI_ARR(MALO_OUT%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO_OUT%NTIM, STR )
           CALL ERR_LOG ( 6717, IUER, 'MALO_REGRID', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO_OUT%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO_OUT%SPR(MALO_OUT%NLON,MALO_OUT%NLAT,MALO_OUT%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*INT8(MALO_OUT%NLON)*INT8(MALO_OUT%NLAT)*INT8(MALO_OUT%NTIM), STR )
           CALL ERR_LOG ( 6718, IUER, 'MALO_REGRID', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO_OUT%LAT' )
           RETURN 
      END IF 
!
      MALO_OUT%MJD_ARR(1:MALO_OUT%NTIM) = MALO_IN%MJD_ARR(1:MALO_OUT%NTIM) 
      MALO_OUT%TAI_ARR(1:MALO_OUT%NTIM) = MALO_IN%TAI_ARR(1:MALO_OUT%NTIM) 
!
      DO 410 J1=1,MALO_IN%NTIM
         IF ( IVRB .GE. 1 ) THEN
              WRITE ( 6, 110 ) J1, MALO_IN%NTIM, CHAR(13)
 110          FORMAT ( '  Regridding epoch ', I5, ' ( ', I5, ' )  ',A$ )
         END IF
         IF ( METH == 'spline' ) THEN
              NP_SPR = 0.0D0
              DO 420 J2=1,MALO_IN%NLAT
                 DO 430 J3=1,MALO_IN%NLON
                    BCF(J3,J2) = (MALO_IN%SPR(J3,J2,J1) - MALO_MEAN%SPR(J3,J2,1))
                    IF ( J2 == MALO_IN%NLAT ) THEN
                         NP_SPR = NP_SPR + 2.D0*(MALO_IN%SPR(J3,J2,J1)   - MALO_MEAN%SPR(J3,J2,1))  - &
     &                                          (MALO_IN%SPR(J3,J2-1,J1) - MALO_MEAN%SPR(J3,J2-1,1) )
                    END IF
 430             CONTINUE 
                 BCF(MALO_IN%NLON+1,J2) = BCF(1,J2) 
                 IF ( J2 == MALO_IN%NLAT ) THEN
!
! ------------------- Restore surface pressure at the northern pole
!
                      NP_SPR = NP_SPR/MALO_IN%NLON
                      DO 440 J4=1,MALO_IN%NLON+1
                         BCF(J4,MALO_IN%NLAT+1) = NP_SPR
 440                  CONTINUE 
                 END IF
 420          CONTINUE 
!
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_2D_CMP ( MDEG, 0, MALO_IN%NLON+1, MALO_IN%NLAT+1, &
     &                            LON_ARR, LAT_ARR, BCF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6719, IUER, 'MALO_REGRID', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 2-dimensional '// &
     &                 'interpolation B-spline interpolation' )
                   RETURN 
              END IF
              IF ( J1 == 1 ) THEN
                   DO 450 J5=1,MALO_OUT%NLAT
                      MALO_OUT%LAT(J5) = -P2I + (J5-1)*PI__NUM/MALO_OUT%NLAT
 450               CONTINUE 
                   DO 460 J6=1,MALO_OUT%NLON
                      MALO_OUT%LON(J6) = (J6-1)*PI2/MALO_OUT%NLON
 460               CONTINUE 
              END IF
!
              DO 470 J7=1,MALO_OUT%NLAT
                 ARGS(2) = MALO_OUT%LAT(J7) 
                 IF ( J7 == 1             ) ARGS(2) = ARGS(2) + PI2/MALO_OUT%NLAT/1000.0D0
                 IF ( J7 == MALO_OUT%NLAT ) ARGS(2) = ARGS(2) - PI2/MALO_OUT%NLAT/1000.0D0
                 INDS(2) = IXMN4 ( MALO_IN%NLAT+1, LAT_ARR, ARGS(2) )
!$OMP PARALLEL DO &
!$OMP&           PRIVATE ( J8 ), FIRSTPRIVATE ( ARGS, INDS ), &
!$OMP&           SCHEDULE ( GUIDED )
                 DO 480 J8=1,MALO_OUT%NLON
                    ARGS(1) = MALO_OUT%LON(J8) 
                    IF ( J8 == 1             ) ARGS(1) = ARGS(1) + PI2/MALO_OUT%NLON/1000.0D0
                    IF ( J8 == MALO_OUT%NLON ) ARGS(1) = ARGS(1) - PI2/MALO_OUT%NLON/1000.0D0
                    INDS(1) = IXMN4 ( MALO_IN%NLON+1, LON_ARR, ARGS(1) )
                    MALO_OUT%SPR(J8,J7,J1) = VAL_2D_BSPL4 ( ARGS(1), ARGS(2), &
     &                                                      MALO_IN%NLON+1, MALO_IN%NLAT+1, &
     &                                                      MDEG, INDS(1), INDS(2), &
     &                                                      LON_ARR, LAT_ARR, BCF )
 480            CONTINUE 
!$OMP END PARALLEL DO
 470          CONTINUE 
           ELSE IF ( METH == 'expand' ) THEN
              DO 490 J9=1,MALO_OUT%NLAT
                 INDS(1) = (J9-1+0.0001)/ ( FLOAT(MALO_OUT%NLAT)/FLOAT(MALO_IN%NLAT) ) + 1
                 DO 4100 J10=1,MALO_OUT%NLON
                    INDS(2) = (J10-1+0.0001)/( FLOAT(MALO_OUT%NLON)/FLOAT(MALO_IN%NLON) ) + 1
                    MALO_OUT%SPR(J10,J9,J1) = MALO_IN%SPR(INDS(2),INDS(1),J1) 
 4100            CONTINUE 
 490          CONTINUE 
         END IF
 410  CONTINUE 
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 120 ) MALO_IN%NTIM
 120       FORMAT  ( 'Regridded all ', I5, ' epochs        ' )
      END IF
      DEALLOCATE ( LON_ARR )
      DEALLOCATE ( LAT_ARR )
      DEALLOCATE ( BCF     )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_REGRID  !#!#
      
