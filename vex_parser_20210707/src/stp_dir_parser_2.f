      SUBROUTINE  STP_DIR_PARSER_2 ( DIR_STP, STP, IUER )
!
! ***************************************************************************
! *                                                                         *
! *   Routine STP_DIR_PARSER parses the VLBI station parameters files in a  *
! *   given directory.                                                      *
! *   N.B: - See also stp_dir_parser.f                                      *
! *        - Compared to the aforementioned routine, stp_dir_parser.f, here *
! *          we call on stp_fil_parser.f to parse the station parameters,   *
! *          as opposed to iteratively doing that here.                     *
! *                                                                         *
! *   INPUT:                                                                *
! *            DIR_STP    =  STP File Directory          { CHAR }           *
! *                                                                         *
! *            IUER      =  Error Handler                { INT, OPT }       *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                         Default, IUER = -1                              *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            STP       =  Parsed Object                { DERIVED TYPE }   *
! *                         For more on the parsed data, see stp.i, and     *
! *                         edit it accordingly to include more data        *
! *                         blocks.                                         *
! *                                                                         *
! *  ### 16-NOV-2020  STP_DIR_PARSER_2  v1.0 (c)  N. Habana  16-NOV-2020 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'stp.i'
      INCLUDE     'astro_constants.i'
      TYPE ( STP__TYPE ) :: STP
      CHARACTER   DIR_STP*(*)
      CHARACTER   FIL_STP*128, STR*128
      INTEGER*4   L_FIL, L_STP, LEV, IS
      INTEGER*4   DIR_DESC(16)
      CHARACTER   DELIM*5                       ! Deliminator for STP file
      CHARACTER   DELIM_DIR*4                   ! Deliminator for STP directory
      INTEGER*4   MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4   MAXL_STRING                   ! Max. String length
      PARAMETER   ( MAXL_STRING = 256 )           
      PARAMETER   ( MP = 128*1024 )             
      PARAMETER   ( MIND = 128 )                 
      PARAMETER   ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' ) 
      PARAMETER   ( DELIM_DIR =  CHAR(0)//CHAR(32)//CHAR(9)//'/' ) 
      CHARACTER   BUF(MP)*(MAXL_STRING)         ! Read File
      INTEGER*4   NP, LIND, IND(2,MIND), LN
      INTEGER*4   IUER, IER
      INTEGER*4   I0, I1
      INTEGER*4   J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10
      INTEGER*4   K0, K1, K2
      INTEGER*4   NDATES(3), NUMI(2)
      REAL*8      NUMR(2)
      CHARACTER   STR2(2)*32
      LOGICAL*1   FL_IVS, FL_ID, FL_UD, FL_COO, FL_MNT, FL_SAZ, FL_SEL
      LOGICAL*1   FL_AAZ, FL_AEL, FL_DAZ, FL_DEL, FL_TAZ, FL_TEL
      LOGICAL*1   FL_EMIN, FL_EMAX, FL_AZR, FL_REC, FL_PRE, FL_POST
      LOGICAL*1   FL_HAZ, FL_HEL
      INTEGER*4,  EXTERNAL :: ILEN, GET_FILE_FROM_DIR
#ifdef GNU
      INTEGER*4,  EXTERNAL :: STP_COMPAR_TSYS, STP_COMPAR_GAIN
#else
      INTEGER*2,  EXTERNAL :: STP_COMPAR_TSYS, STP_COMPAR_GAIN
#endif
!
! --- CLEAN STP
!
      IF ( STP%STATUS .NE. STP__INIT ) CALL STP_CLEAN_2 ( STP )
!
! --- Initialise the file count and directory level
!
      L_FIL = 0
      LEV   = 0
      L_STP = 0
!
! --- Read the files in the directory
!
      DO 210 K1 = 1, STP__MFIL
! ------
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_STP, FIL_STP )
         IF ( IS .NE. 0 ) THEN
            IUER = -2
            CALL ERR_LOG ( 5501, IUER, 'STP_DIR_PARSER',                &
     &              'Error in reading input directory '//               &
     &              TRIM(DIR_STP)//' '//FIL_STP )
            CALL EXIT ( 1 )
         END IF
! ------
         IF ( LEV == 0 ) GOTO 220 ! Have all the files in the directory
! ------
!
! ------ Bypass temporary files created by emacs
!
         IF ( INDEX ( FIL_STP, '#' ) > 0 ) GOTO 210
         IF ( INDEX ( FIL_STP, '~' ) > 0 ) GOTO 210
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > STP__MFIL ) THEN
            IUER = -2
            CALL CLRCH ( STR )
            CALL INCH  ( STP__MFIL, STR )
            CALL ERR_LOG ( 5502, IUER, 'STP_DIR_PARSER',                &
     &              'Too many files were found in the input directory'  &
     &              //TRIM(DIR_STP)//' more than STP__MFIL '            &
     &              //TRIM(STR)//'. Please raise STP__MFIL' )
             CALL EXIT ( 1 )
         END IF
!
! ------ Check if the file is an stp file.  
! ------ N.B: - Such files will have extension ".stp" and thus be
!               longer than 4 characters in name.
!
         CALL EXWORD ( FIL_STP, MIND, LIND, IND, DELIM_DIR, IUER )
         LN = IND(2,LIND) - IND(1,LIND)
         IF ( (LN .GT. 4)                                        .AND.  &
     &        (FIL_STP(IND(2,LIND)-3:IND(2,LIND)) .EQ. '.stp') ) THEN
            L_STP = L_STP + 1
            STP%C_FIL(L_STP) = FIL_STP
         END IF
 210  CONTINUE
!
 220  CONTINUE
!
! --- Sort the files and allocate them, given the directory contains stp 
!     files
!
      STP%NSTA = L_STP
      IF ( STP%NSTA > 0 ) THEN
         CALL SORT_FAST_CH ( STP%NSTA, STP%C_FIL ) 
!
         ALLOCATE ( STP%STA(STP%NSTA),  STAT = IER )
!
! ------ Parse the stp parameters for all files.
!
         DO 510 K2 = 1, STP%NSTA
           CALL STP_FIL_PARSER ( STP%STA(K2), STP%C_FIL(K2), IUER )

 510     CONTINUE
      ELSE
         CALL ERR_LOG ( 5524, IUER, 'STP_DIR_PARSER',                   &
     &            'There are no STP files in '//DIR_STP//'.' )
         RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE ! STP_DIR_PARSER_2  !#!
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE STP_CLEAN_2 ( STP )
      IMPLICIT   NONE
      INCLUDE   'stp.i'
      TYPE ( STP__TYPE ) :: STP
!
      IF ( ASSOCIATED ( STP%STA ) ) DEALLOCATE ( STA%STP )

      CALL CLRCH ( STP%C_FIL )
      STP%NSTA    = 0
      STP%STATUS  = STP__INIT
!
      RETURN
      END SUBROUTINE ! STP_CLEAN_2    !#!#!
