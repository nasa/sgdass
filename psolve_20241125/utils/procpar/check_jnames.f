      PROGRAM    CHECK_JNAMES
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      INTEGER*4  MP
      PARAMETER  ( MP = 64*1024 )
      TYPE       ( SOURCE_CAT__TYPE ) :: CAT(MP)
      CHARACTER  FILCAT*128, FILWRO*128, C_SOU(MP)*8, WRO(MP)*128, &
     &           J2000_NAME*10, B1950_NAME*8
      INTEGER*4  L_CAT, MODE, J1, J2, NW, IPAR, IUER
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6,  * ) 'Usage: check_jnames filcat'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILCAT )
      END IF
      IPAR   = 2
      FILWRO = '/apr/sou/non_canonical_jname.txt'
!
      IUER = -1
      CALL READ_SOU ( FILCAT, MP, L_CAT, CAT, C_SOU, MODE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      IUER = -1
      CALL RD_TEXT ( FILWRO, MP, WRO, NW, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      DO 410 J1=1,L_CAT
         IF ( CAT(J1)%CALIB .NE. 0  .AND.  CAT(J1)%CALIB .NE. 4 ) THEN
              CALL SOUCOO_TO_NAME ( CAT(J1)%ALP, CAT(J1)%DEL, J2000_NAME, B1950_NAME )
              IF ( CAT(J1)%J2000_NAME .NE. J2000_NAME ) THEN
                   DO 420 J2=1,NW
                      IF ( CAT(J1)%J2000_NAME == WRO(J2)(11:20) ) GOTO 410
 420               CONTINUE 
                   WRITE ( 6, 110 ) CAT(J1)%J2000_NAME, J2000_NAME, CAT(J1)%IVS_NAME
 110               FORMAT ( 'CAT_name: ',A, ' || Jname: ', A, 4X, ' IVS_Name: ', A  )
              END IF
         END IF
 410  CONTINUE 
!
      END  !#!# CHECK_JNAMES  #!#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SOUCOO_TO_NAME ( ALP_J2000, DEL_J2000, J2000_NAME, B1950_NAME )
! ************************************************************************
! *                                                                      *
! *   Routine  SOUCOO_TO_NAME computes the source names using  right     *
! *   ascension and declination at J2000.0 epochs. It generates two      *
! *   names:                                                             *
! *   1) J2000_NAME -- 10 characters long IAU J-name                     *
! *   2) B1950_NAME --  8 characters long IAU B-name                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  ALP_J2000 ( REAL*8    ) -- Right ascension in J2000 (in rad).       *
! *  DEL_J2000 ( REAL*8    ) -- Declination in J2000 (in rad).           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * J2000_NAME ( CHARACTER ) -- 10 characters long IAU J-name            *
! * B1950_NAME ( CHARACTER ) --  8 characters long IAU B-name            *
! *                                                                      *
! * ## 03-MAY-2002  SOUCOO_TO_NAME  v1.0 (c)  L. Petrov  17-JUL-2004 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     ALP_J2000, DEL_J2000
      CHARACTER  J2000_NAME*10, B1950_NAME*8
      REAL*8     ALP_B1950, DEL_B1950, S_J2000(3), S_B1950(3), RD
      REAL*8      PI, PI2, P2I, RAD_TO_MAS, RAD_TO_DEG
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi number
      PARAMETER  ( RAD_TO_MAS = 180.0D0*3600.D0*1.D3/PI )
      PARAMETER  ( RAD_TO_DEG = 180.0D0/PI )
      CHARACTER  DEL_STR*14, ALP_STR*15
      INTEGER*4  IER
!
      S_J2000(1) = DCOS(DEL_J2000)*DCOS(ALP_J2000)
      S_J2000(2) = DCOS(DEL_J2000)*DSIN(ALP_J2000)
      S_J2000(3) = DSIN(DEL_J2000)
      CALL J2000_TO_B1950 ( S_J2000, S_B1950 )
!
      CALL DECPOL ( 3, S_B1950, RD, ALP_B1950, DEL_B1950, IER )
!
      CALL RH_TAT ( ALP_B1950, 1, ALP_STR, IER )
      WRITE ( UNIT=DEL_STR(1:10), FMT='(F10.7)' ) DABS(DEL_B1950*RAD_TO_DEG)
      CALL CHASHL ( DEL_STR )
      IF ( DABS(DEL_B1950*RAD_TO_DEG) .LT. 10.0D0 ) DEL_STR = '0'//DEL_STR(1:9)
      IF ( DEL_B1950 .LT. 0.0D0 ) DEL_STR = '-'//DEL_STR(1:9)
      IF ( DEL_B1950 .GE. 0.0D0 ) DEL_STR = '+'//DEL_STR(1:9)
!
      B1950_NAME = ALP_STR(2:3)//ALP_STR(5:6)//DEL_STR(1:3)//DEL_STR(5:5)
!
      CALL RH_TAT ( ALP_J2000, 5, ALP_STR, IER )
      CALL RG_TAT ( DEL_J2000, 5, DEL_STR, IER )
      CALL CHASHL ( ALP_STR )
      J2000_NAME = 'J'//ALP_STR(1:2)//ALP_STR(4:5)//DEL_STR(1:3)//DEL_STR(5:6)
      IF ( J2000_NAME(6:6) .EQ. ' ' ) J2000_NAME(6:6) = '+'
      RETURN
      END  SUBROUTINE SOUCOO_TO_NAME
