      SUBROUTINE WRITE_LS_MASK_PGM ( DTYP, NLON, NLAT, HEI_ARR, FILOUT, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine WRITE_LS_MASK_PGM
! *                                                                      *
! * ### 10-OCT-2012 WRITE_LS_MASK_PGM v1.0 (c) L. Petrov 10-OCT-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INTEGER*4  NLON, NLAT, IUER
      REAL*4     HEI_ARR(NLON,NLAT)
      CHARACTER  DTYP*(*), FILOUT*(*), STR*32768
      INTEGER*4  RANGE(2)
      INTEGER*4  IS, NCID,   VECDIM(2), &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_LS, &
     &           ID_VAR_LS,  ID_VAR_LAT, ID_VAR_LON
      INTEGER*4  J1, J2, J3, LUN, IP, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, GET_UNIT
!
      LUN = GET_UNIT()
!
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6241, IUER, 'WRITE_LS_MASK_PGM', 'Failure '// &
     &         'in attempt to open the output file '//FILOUT )
           RETURN 
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) 'P2'
!
      IF ( DTYP == 'bls_mask' ) THEN
           STR = '# Land_sea mask: 0 -- sea, 1 -- land'
           RANGE(1) = 0
           RANGE(2) = 1
        ELSE IF ( DTYP == 'tls_mask' ) THEN
           STR = '# Land_sea mask: 0 -- sea, 1 -- land, 2 -- land and sea'
           RANGE(1) =  0
           RANGE(2) =  2
        ELSE IF ( DTYP == 'fls_mask' ) THEN
           STR = '# Land_sea mask: 0 -- totally sea, 100 -- totally land, (0, 100) -- share of land'
           RANGE(1) = 0
           RANGE(2) = 100
        ELSE IF ( DTYP == 'dig_elev' ) THEN
           STR = '# Elevation with respect WGS84' 
           RANGE(1) = -407
           RANGE(2) = 8752
        ELSE IF ( DTYP == 'el_area' ) THEN
           STR = '# Elevation averaged over pixel areas with respect to WGS84' 
           RANGE(1) = -407
           RANGE(2) = 8752
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:I_LEN(STR))
!
      CALL CLRCH ( STR )
      CALL INCH ( NLON, STR(1:5) ) 
      CALL INCH ( NLAT, STR(7:11) ) 
      CALL CHASHR ( STR(1:5) )
      CALL CHASHR ( STR(7:11) )
!
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:I_LEN(STR))
!
      WRITE ( UNIT=LUN, FMT='(I4)' ) RANGE(2)
!
      DO 410 J1=NLAT,1,-1
         IP = 1
         DO 420 J2=1,NLON
            IF ( DTYP == 'bls_mask' .OR. DTYP == 'tls_mask' ) THEN
                 WRITE ( UNIT=STR(IP:IP+1), FMT='(I2)' ) NINT ( HEI_ARR(J2,J1) )
                 IP = IP + 2
              ELSE IF ( DTYP == 'fls_mask' ) THEN
                 WRITE ( UNIT=STR(IP:IP+3), FMT='(I4)' ) NINT ( 100*HEI_ARR(J2,J1) )
                 IP = IP + 4
              ELSE IF ( DTYP == 'dig_elev' ) THEN
                 WRITE ( UNIT=STR(IP:IP+4), FMT='(I5)' ) NINT ( HEI_ARR(J2,J1) )
                 IP = IP + 5
              ELSE IF ( DTYP == 'dig_elev' ) THEN
                 WRITE ( UNIT=STR(IP:IP+4), FMT='(I5)' ) NINT ( HEI_ARR(J2,J1) )
                 IP = IP + 5
            END IF
 420     CONTINUE 
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:I_LEN(STR))
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRITE_LS_MASK_PGM  !#!#
