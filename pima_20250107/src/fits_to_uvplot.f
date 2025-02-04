       PROGRAM    FITS_TO_UVPLOT_MAIN
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
       CALL FITS_TO_UVPLOT()
       END  PROGRAM  FITS_TO_UVPLOT_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE FITS_TO_UVPLOT()
! ************************************************************************
! *                                                                      *
! *   Program FITS_TO_UVPLOT  computes the scan-averaged uv coordinates  *
! *   of the projected baseline to the plane tangential to the source    *
! *   direction either on the terminal or in the file in the Postscript  *
! *   or gif format. This plot provides information about the            *
! *   UV coverage.                                                       *
! *                                                                      *
! * ### 29-JAN-2007  FITS_TO_UVPLOT v1.0  (c) L. Petrov 29-JAN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      CHARACTER  FINAM*256
      INTEGER*4  MARG
      PARAMETER  ( MARG = 32 )
      CHARACTER  ARGS(MARG)*256, FILOUT*128
      INTEGER*4  J1, J2, IDEV, ICLR, ISIZE, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: fits_to_uvplot [-o ouput_file_or_XW]'// &
     &                    '[-size code] [-color code] uvs_fits_file'
           CALL EXIT ( 0 )
         ELSE
           DO 410 J1=1,IARGC()
              CALL GETARG ( J1, ARGS(J1) )
 410       CONTINUE 
!
           ISIZE  =  4
           ICLR   = -1
           IDEV   =  1
!
           DO 420 J2=1,IARGC()
              IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-o' ) THEN
                   FILOUT = ARGS(J2+1)
                   IF ( ILEN(FILOUT) == 0 ) THEN
                        CALL ERR_LOG ( 1601, -2, 'FITS_TO_UVPLOT', 'No value '// &
     &                      'of qualifier -o was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( FILOUT(1:3) == '/XW' .OR. &
     &                  FILOUT(1:2) == 'XW'       ) THEN
                        CALL CLRCH ( FILOUT )
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
                          ELSE
                             IDEV = 2
                             IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                                  IDEV = 3
                             END IF
                        END IF
                      ELSE 
                        IDEV = 2
                        IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                             IDEV = 3
                        END IF
                   END IF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-col' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1602, -2, 'FITS_TO_UVPLOT', 'No value '// &
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
                        IF ( ICLR < 0 .OR. ICLR > 128 ) ICLR = 0
                   END IF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-size' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1603, -2, 'FITS_TO_UVPLOT', 'No value '// &
     &                      'of qualifier -size was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   CALL CHIN ( ARGS(J2+1), ISIZE )
                   IF ( ISIZE < 0 .OR. ISIZE > 4 ) ISIZE = 3
              END IF
 420       CONTINUE 
           FINAM = ARGS(IARGC())
      END IF
!
      VIS%SKY_FRQ => NULL()
      VIS%MJD     => NULL()
      VIS%TAI     => NULL()
      VIS%VIS     => NULL()
      VIS%UV      => NULL()
      VIS%WEI     => NULL()
      VIS%IND_BAS => NULL()
      VIS%INT_TIM => NULL()
!
      IUER = -1
      CALL GET_FITS_VIS ( FINAM, VIS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1604, -2, 'FITS_TO_UVPLOT', 'Failure in an '// &
     &         'attempt to read the fits file with visibility data '// &
     &          FINAM )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GEN_UVPLOT ( VIS, IDEV, ICLR, ISIZE, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1605, -2, 'FITS_TO_UVPLOT', 'Failure in an '// &
     &         'attempt to generate the output uv-plot' )
           CALL EXIT ( 1 )
      END IF
      CALL EXIT ( 0 )
      END  SUBROUTINE  FITS_TO_UVPLOT  !#!#
