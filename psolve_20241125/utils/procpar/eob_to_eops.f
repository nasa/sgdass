      PROGRAM    EOB_TO_EOPS
! ************************************************************************
! *                                                                      *
! *   Program  EOB_TO_EOPS  reformts file with the Earth Orientation     *
! *   Parameters series from Solve getpar EOB format to the IVS EOPS     *
! *   (ro the same EOPI) format.                                         *
! *                                                                      *
! *  ### 02-MAR-2001  EOB_TO_EOPS  v2.0 (c)  L. Petrov  04-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INTEGER*4    MSES, MHEAD
      PARAMETER  ( MSES = MAX_ARCS, MHEAD = 512 )
      CHARACTER  FILEOB*128, FILEOPS*128, HELP_DIR*128, EOPS_HELP*128
      CHARACTER  HEAD_BUF(MHEAD)*128, GET_CDATE*19
      TYPE ( EOP__STRU ) ::  EOP(MSES)
      INTEGER*4  NUMARG, NSES, NHEAD, NHELP, J1, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      NUMARG = IARGC ()
      IF ( NUMARG .LT. 2 ) THEN
           WRITE ( 6, * ) ' Usage: eob_to_eops <input_eob> <output_eops>'
           CALL EXIT()
         ELSE
           CALL GETARG ( 1, FILEOB  )
           CALL GETARG ( 2, FILEOPS )
      END IF
      WRITE ( 6, * ) 'Reading EOB file...'
!
! --- Read the input file in EOB format
!
      IUER = -1
      CALL READ_EOB ( FILEOB, MHEAD, NHEAD, HEAD_BUF, MSES, NSES, EOP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Get directory for master files
!
      CALL GETENV ( 'PSOLVE_HELP_DIR', HELP_DIR )
      IF ( ILEN(HELP_DIR) .LE. 0 ) THEN
           HELP_DIR = SOLVE_HELP_DIR
      END IF
      IF ( HELP_DIR(I_LEN(HELP_DIR):I_LEN(HELP_DIR)) .NE. '/' ) THEN
           HELP_DIR = HELP_DIR(1:I_LEN(HELP_DIR))//'/'
      END IF
!
      EOPS_HELP = HELP_DIR(1:I_LEN(HELP_DIR))//EOPS__HELP_FILE
!
! --- Read help-file with additional information about eops file. It
! --- will be treated as comments
!
      IUER = -1
      CALL RD_TEXT  ( EOPS_HELP, MHEAD-NHEAD, HEAD_BUF(NHEAD+1), NHELP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6701, -2, 'EOB_TO_EOPS', 'Error in an attempt '// &
     &         'to read the EOPS help file '//EOPS_HELP )
           CALL EXIT ( 1 )
      END IF
      NHEAD = NHEAD + NHELP
!
! --- Add comment prefix if needed
!
      DO 410 J1=1,NHEAD
         IF ( HEAD_BUF(J1)(1:1) .NE. '#' ) HEAD_BUF(J1) = '# '//HEAD_BUF(J1)
 410  CONTINUE
!
! --- Add trailing lines of the header session
!
      NHEAD = NHEAD + 1
      HEAD_BUF(NHEAD) = '# Processed by eob_to_eops at '//GET_CDATE()
      NHEAD = NHEAD + 1
      HEAD_BUF(NHEAD) = '# '
!
! --- Write down the file in EOPS format
!
      IUER = -1
      CALL WRITE_EOPS ( FILEOPS, NHEAD, HEAD_BUF, NSES, EOP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, * ) 'EOPS file is created'
!
      END  !#!  EOB_TO_EOPS  #!#
