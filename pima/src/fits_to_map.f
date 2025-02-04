       PROGRAM    FITS_TO_MAP_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL FITS_TO_MAP()
       END  PROGRAM  FITS_TO_MAP_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE FITS_TO_MAP
! ************************************************************************
! *                                                                      *
! *   Program FITS_TO_MAP prints the map either on the terminal or in    *
! *   the file in the Postscript, gif, or png format.                    *
! *                                                                      *
! *  ###  29-JAN-2007  FITS_TO_MAP v1.2 (c)  L. Petrov  13-OCT-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      CHARACTER  FINAM*256
      INTEGER*4  MARG
      PARAMETER  ( MARG = 32 )
      CHARACTER  ARGS(MARG)*256, FILOUT*128
      REAL*8     SIGLEV, BOX_SIZE
      LOGICAL*4  FL_IMA, FL_CC
      INTEGER*4  J1, J2, IDEV, ICLR, ISIZE, IBEAM, ITHICK, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: GET_HR_CDATE*29
!
      CALL SET_SIGNAL_CTRLC ( 2 )
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: fits_to_map -o output_file_or_XW '// &
     &                    '[-box value] [-size code] [-color code] '// &
     &                    '[-lev value] [-beam code] [-thick code] '// &
     &                    'fits_map_file'
           CALL EXIT ( 0 )
         ELSE
!
! -------- Get arguments
!
           DO 410 J1=1,IARGC()
              CALL GETARG ( J1, ARGS(J1) )
 410       CONTINUE 
!
! -------- Set defaults
!
           BOX_SIZE = 0.0D0
           SIGLEV   = 5.0D0
           ISIZE    = 3
           ICLR     = -1
           IBEAM    = 0
           ITHICK   = 1
           IDEV     = 1
!
! -------- Parse arguments
!
           DO 420 J2=1,IARGC()
              IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-o' ) THEN
                   FILOUT = ARGS(J2+1)
                   IF ( ILEN(FILOUT) == 0 ) THEN
                        CALL ERR_LOG ( 1401, -2, 'FITS_TO_MAP', 'No value '// &
     &                      'of qualifier -o was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( FILOUT(1:3) == '/XW' .OR. &
     &                  FILOUT(1:2) == 'XW'       ) THEN
!@                        CALL CLRCH ( FILOUT )
                        IDEV = 1
                      ELSE IF ( ILEN(FILOUT) > 4 ) THEN
                        IF ( FILOUT(ILEN(FILOUT)-2:ILEN(FILOUT)) == '/PS' .OR. &
     &                       FILOUT(ILEN(FILOUT)-2:ILEN(FILOUT)) == '/ps'      ) THEN
                             IDEV = 2
                             CALL CLRCH ( FILOUT(ILEN(FILOUT)-2:) )
                          ELSE IF ( FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/GIF' .OR. &
     &                       FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/gif'      ) THEN
!
                             IDEV = 3
                             CALL CLRCH ( FILOUT(ILEN(FILOUT)-3:) )
                          ELSE IF ( FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/PNG' .OR. &
     &                       FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/ong'      ) THEN
!
                             IDEV = 4
                             CALL CLRCH ( FILOUT(ILEN(FILOUT)-3:) )
                          ELSE IF ( FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/BIN' .OR. &
     &                       FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/bin'      ) THEN
!
                             IDEV = 5
                             CALL CLRCH ( FILOUT(ILEN(FILOUT)-3:) )
                          ELSE
                             IDEV = 2
                             IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                                  IDEV = 3
                               ELSE IF ( INDEX ( FILOUT, '.png' ) > 0 ) THEN
                                  IDEV = 4
                               ELSE IF ( INDEX ( FILOUT, '.txt' ) > 0 ) THEN
                                  IDEV = 5
                             END IF
                        END IF
                      ELSE 
                        IDEV = 2
                        IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                             IDEV = 3
                          ELSE IF ( INDEX ( FILOUT, '.png' ) > 0 ) THEN
                             IDEV = 4
                        END IF
                   END IF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-box' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1402, -2, 'FITS_TO_MAP', 'No value '// &
     &                      'of qualifier -box was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( INDEX ( ARGS(J2+1), '.' ) .LE. 0 ) THEN
                        ARGS(J2+1) = ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//'.0'
                   END IF
                   READ ( UNIT=ARGS(J2+1), FMT='(F10.5)' ) BOX_SIZE
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-lev' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1403, -2, 'FITS_TO_MAP', 'No value '// &
     &                      'of qualifier -lev was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( INDEX ( ARGS(J2+1), '.' ) .LE. 0 ) THEN
                        ARGS(J2+1) = ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//'.0'
                   END IF
                   READ ( UNIT=ARGS(J2+1), FMT='(F10.5)' ) SIGLEV
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-col' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1404, -2, 'FITS_TO_MAP', 'No value '// &
     &                      'of qualifier -col was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( ARGS(J2+1)(1:4) == 'band'  .OR.  &
     &                  ARGS(J2+1)(1:4) == 'BAND'        ) THEN
                        ICLR = -1
                      ELSE IF ( ARGS(J2+1)(1:4) == 'grey' .OR. &
     &                          ARGS(J2+1)(1:4) == 'GREY' .OR. &
     &                          ARGS(J2+1)(1:2) == 'bw'   .OR. &
     &                          ARGS(J2+1)(1:2) == 'BW'        ) THEN
                        ICLR = 0
                      ELSE 
                        CALL CHIN ( ARGS(J2+1), ICLR )
                        IF ( ICLR < -1 .OR. ICLR > 128 ) ICLR = 0
                   END IF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-size' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1405, -2, 'FITS_TO_MAP', 'No value '// &
     &                      'of qualifier -size was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   CALL CHIN ( ARGS(J2+1), ISIZE )
                   IF ( ISIZE < 0 .OR. ISIZE > 5 ) ISIZE = 3
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-beam' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1406, -2, 'FITS_TO_MAP', 'No value '// &
     &                      'of qualifier -beam was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   CALL CHIN ( ARGS(J2+1), IBEAM )
                   IF ( ISIZE < 0 .OR. &
     &                  ( IBEAM > 5  .AND. &
     &                    ( IBEAM .LT. 11 .OR. IBEAM .GT. 14 ) ) ) THEN
                        IBEAM = 0
                   END IF
                 ELSE IF ( ARGS(J2)(1:4) == '-thi' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1406, -2, 'FITS_TO_MAP', 'No value '// &
     &                      'of qualifier -beam was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   CALL CHIN ( ARGS(J2+1), ITHICK )
                   IF ( ITHICK <   1 ) ITHICK =   1
                   IF ( ITHICK > 200 ) ITHICK = 200
              END IF
 420       CONTINUE 
           FINAM = ARGS(IARGC())
      END IF
!
      FL_IMA = .TRUE.
      IF ( IBEAM == 0 ) THEN
           FL_CC  = .FALSE.
         ELSE 
           FL_CC  = .TRUE.
      END IF
      IF ( IDEV == 5 ) THEN
           FL_IMA  = .FALSE.
           FL_CC   = .TRUE.
      END IF
!
! --- Get the image
!
      IUER = -1
      CALL GET_FITS_MAP ( FINAM, FL_CC, FL_IMA, MAP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1407, -2, 'FITS_TO_MAP', 'Failure in an '// &
     &         'attempt to read the fits file with image '//FINAM )
           CALL EXIT ( 1 )
      END IF
!
! --- Generate the plot
!
      IF ( IDEV == 5 ) THEN
           CALL GEN_MAPTXT ( MAP, FINAM, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 1408, -2, 'FITS_TO_MAP', 'Failure in an '// &
     &              'attempt to generate the output table with clean componets' )
                CALL EXIT ( 1 )
           END IF
         ELSE
           IUER = -1
           CALL GEN_MAPPLOT ( MAP, IDEV, ICLR, BOX_SIZE, SIGLEV, ISIZE, IBEAM, &
     &                        ITHICK, FINAM, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 1409, -2, 'FITS_TO_MAP', 'Failure in an '// &
     &              'attempt to generate the output picture' )
                CALL EXIT ( 1 )
           END IF
           IF ( IBEAM == -13 ) THEN
                IDEV  =  3
                IBEAM = 13
                FILOUT = FILOUT(3:)
                IUER  = -1
                CALL GEN_MAPPLOT ( MAP, IDEV, ICLR, BOX_SIZE, SIGLEV, ISIZE, IBEAM, &
     &                             ITHICK, FINAM, FILOUT, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1410, -2, 'FITS_TO_MAP', 'Failure in an '// &
     &                   'attempt to generate the output picture' )
                     CALL EXIT ( 1 )
                END IF
           END IF
      END IF
!
      CALL EXIT ( 0 )
      END  SUBROUTINE  FITS_TO_MAP  !#!#
