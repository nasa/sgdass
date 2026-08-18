      PROGRAM    CHECK_BNAMES
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      INTEGER*4  MP
      PARAMETER  ( MP = 64*1024 )
      TYPE       ( SOURCE_CAT__TYPE ) :: CAT(MP)
      CHARACTER  FILCAT*128, FILWRO*128, C_SOU(MP)*8, &
     &           J2000_NAME*10, B1950_NAME*8, WRO(MP)*128
      INTEGER*4  L_CAT, MODE, J1, J2, NW, IPAR, IUER
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6,  * ) 'Usage: check_bnames filcat'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILCAT )
      END IF
      IPAR   = 2
      FILWRO = '/apr/sou/non_canonical_bname.txt'
!
      IUER = -1
      CALL RD_TEXT ( FILWRO, MP, WRO, NW, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      IUER = -1
      CALL READ_SOU ( FILCAT, MP, L_CAT, CAT, C_SOU, MODE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      DO 410 J1=1,L_CAT
         IF ( CAT(J1)%CALIB .NE. 0  .AND.  CAT(J1)%CALIB .NE. 4 ) THEN
              CALL SOUCOO_TO_NAME ( CAT(J1)%ALP, CAT(J1)%DEL, J2000_NAME, B1950_NAME )
              IF ( CAT(J1)%IVS_NAME .NE. B1950_NAME ) THEN
                   DO 420 J2=1,NW
                      IF ( CAT(J1)%IVS_NAME == WRO(J2)(1:8) ) GOTO 410
 420               CONTINUE 
                   WRITE ( 6, 110 ) CAT(J1)%IVS_NAME, B1950_NAME
 110               FORMAT ( 'IVS_name: ',A, ' || Bname: ', A )
              END IF
         END IF
 410  CONTINUE 
!
      END  !#!# CHECK_BNAMES  #!#!
