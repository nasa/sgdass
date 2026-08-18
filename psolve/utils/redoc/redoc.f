      PROGRAM    REDOC
! ************************************************************************
! *                                                                      *
! *   Program redoc treansforms SOLVE documentation which is in rdc      *
! *   format to one of the output formats: .txt, .html or .syn formats.  *
! *                                                                      *
! *   File in rdc format contains                                        *
! *     1) text,                                                         *
! *     2) meta-tags:  #{  #}                                            *
! *     3) section-tags:  #/ #\  or ##/ ##\ or ###/ ###\                 *
! *     4) synopsys-tags: #( #)                                          *
! *                                                                      *
! *   Program REDOC transforms the input file to another format.         *
! *   The output filw doesn't have tags. Tags are used for appropriate   *
! *   formatting.                                                        *
! *                                                                      *
! *  ### 16-MAY-2000     REDOC     v1.1 (c)  L. Petrov  10-OCT-2000 ###  *
! *  pet  2000.10.10  renamed .bat to .syn                               *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MBUF, M_SECT, M_META
      PARAMETER  ( MBUF = 8192, M_SECT = 256, M_META = 16 )
      INTEGER*4  NUMARG, LBUFIN, LBUFOUT, IUER
      CHARACTER  FILIN*128, FILTYP*8, FILOUT*128, BUFIN(MBUF)*128, &
     &           BUFOUT(MBUF)*128
      INTEGER*4  LIN_SECT(2,M_SECT), COL_SECT(2,M_SECT), LEV_SECT(M_SECT)
      INTEGER*4  LIN_META(2,M_META), COL_META(2,M_META)
      INTEGER*4  ROW_HDSE(M_SECT),   ROW_HDME(M_META), &
     &           COL_HDSE(2,M_SECT), COL_HDME(2,M_META)
      INTEGER*4  L_SECT, L_META
      LOGICAL*4  LEX
      INTEGER*4  IARGC, I_LEN
!
      NUMARG = IARGC ()
      IF ( NUMARG .LT. 3 ) THEN
!
! -------- Not enough arguments
!
           WRITE ( 6, * ) 'Usage: redoc <file_name> <output_type> <output_file>'
           CALL EXIT()
         ELSE
           CALL GETARG (  1, FILIN   )
           CALL GETARG (  2, FILTYP  )
           CALL GETARG (  3, FILOUT  )
           CALL TRAN   ( 11, FILTYP, FILTYP )
      END IF
!
! --- Inquire: does the file exist?
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7201, -1, 'REDOC', 'Input file '// &
     &                    FILIN(1:I_LEN(FILIN))//' has not been found ' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the file
!
      IUER = -1
      CALL RD_TEXT ( FILIN, MBUF, BUFIN, LBUFIN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7202, -1, 'REDOC', 'Error in reading input file '// &
     &          FILIN )
           CALL EXIT ( 2 )
      END IF
!
! --- POarse the file. Extract section and meta tags.
!
      IUER = -1
      CALL REDOC_PARSE ( MBUF, LBUFIN, BUFIN, &
     &     M_SECT, L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &     M_META, L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 4 )
!
! --- Make acttual transformation
!
      IUER = -1
      IF ( FILTYP(1:3) .EQ. 'TXT' ) THEN
           CALL REDOC_TXT ( MBUF, LBUFIN, BUFIN, LBUFOUT, BUFOUT, &
     &          L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &          L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
         ELSE IF ( FILTYP(1:3) .EQ. 'SYN' ) THEN
           CALL REDOC_SYN ( MBUF, LBUFIN, BUFIN, LBUFOUT, BUFOUT, &
     &          L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &          L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
         ELSE IF ( FILTYP(1:4) .EQ. 'HTML' ) THEN
           CALL REDOC_HTML ( MBUF, LBUFIN, BUFIN, LBUFOUT, BUFOUT, &
     &          L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &          L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
         ELSE
           CALL ERR_LOG ( 7203, -1, 'REDOC', 'Output filetype '//FILTYP// &
     &         'is not supported. One of txt, syn, html expected' )
           CALL EXIT ( 3 )
      END IF
      IF ( IUER .NE. 0 ) CALL EXIT ( 5 )
!
! --- REmove the output file if exist
!
      CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
!
! --- Write down the output into the file
!
      IUER = -1
      CALL WR_TEXT ( LBUFOUT, BUFOUT, FILOUT, IUER )
!
      END  !#!  REDOC  #!#
