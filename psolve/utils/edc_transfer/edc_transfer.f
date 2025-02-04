      PROGRAM    EDC_TRANSFER
! ************************************************************************
! *                                                                      *
! *   Program EDC_TRANSFER os for transformation of in imput external    *
! *   decimation file from one format to another, EDC_TRANSFER reads     *
! *   the input file in either ascii or binary format and writes the     *
! *   contents of the the external decimation file to either binary or   *
! *   ascii format.                                                      *
! *                                                                      *
! *  ### 25-OCT-2007  EDC_TRANSFER  v1.0 (c) L. Petrov  26-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      TYPE     ( EDC__TYPE     ) :: EDC
      CHARACTER  OP_CODE*32, FILIN*128, FILOUT*128, EDC_DIR*128, DB_NAME*10
      INTEGER*4  IP, J1, J2, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: edc_transfer -to_ascii|-to_binary '// &
     &                        '{input_file} {output_dir}'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, OP_CODE )
           CALL GETARG ( 2, FILIN   )
           CALL GETARG ( 3, FILOUT  )
      END IF
!
      IP = LINDEX ( FILIN, '/' ) 
      IF ( IP .LE. 0 ) THEN
           EDC_DIR = '.'
         ELSE IF ( IP == 1 ) THEN
           EDC_DIR = '/'
           FILIN = FILIN(2:)
         ELSE 
           EDC_DIR = FILIN(1:IP-1)
           FILIN = FILIN(IP+1:)
      END IF
      DB_NAME = FILIN
      IP = INDEX ( DB_NAME, '.' ) 
      IF ( IP > 0 ) CALL CLRCH ( DB_NAME(IP:) )
!
      IUER = -1
      CALL EDC_READ ( DB_NAME, EDC_DIR, EDC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7701, -2, 'EDC_TRANSFER', 'Error in an attempt '// &
     &         'to read input external decimation file' )
           CALL EXIT ( 1 )
      END IF
!
      IP = LINDEX ( FILOUT, '/' ) 
      IF ( IP .LE. 0 ) THEN
           EDC_DIR = '.'
        ELSE IF ( IP == 1 ) THEN
           EDC_DIR = '/'
           FILOUT = FILOUT(2:)
        ELSE 
           EDC_DIR = FILOUT(1:IP-1)
           FILOUT = FILOUT(IP+1:)
      END IF
!
      IF ( OP_CODE == '-to_ascii' ) THEN
           IUER = -1
           CALL EDC_WRITE ( EDC, EDC_DIR, EDC__ASC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7702, -2, 'EDC_TRANSFER', 'Error in an '// &
     &             'attempt to write input external decimation file' )
                CALL EXIT ( 1 )
           END IF
         ELSE IF ( OP_CODE == '-to_binary' ) THEN
           IUER = -1
           CALL EDC_WRITE ( EDC, EDC_DIR, EDC__BIN, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7703, -2, 'EDC_TRANSFER', 'Error in an '// &
     &             'attempt to write input external decimation file' )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           CALL ERR_LOG ( 7704, -2, 'EDC_TRANSFER', 'Unsupported operation '// &
     &         'code: '//OP_CODE(1:I_LEN(OP_CODE))//' -- one of '// &
     &         '-to_binary or -to_ascii was expeceted' )
           CALL EXIT ( 1 )
      END IF
!
      END PROGRAM   EDC_TRANSFER  !#!#
