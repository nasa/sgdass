      SUBROUTINE TRP_INIT ( FL_GVF, TRP_DIR, N_FIL_TRP, ADR_TRP_FIL_BUF, &
     &                      STS_TRP_FIL, ADR_TRP_SES_BUF, STS_TRP_SES, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  TRP_INIT  examines contents of directory TRP_DIR,         *
! *   extracts files with extensions .spm and .trp that are supposedly   *
! *   have vales of the external tropospheric path delay, check their    *
! *   headers and stores the following information: 1) the total number  *
! *   of valid files with external tropospheric path delay; 2) names of  *
! *   files with external tropospheric path delays; 3) names of VLBI     *
! *   sessions that corresponds to files with external tropospheric path *
! *   delays.                                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          FL_GVF  ( LOGICAL*4 ) -- Flag, whether the database will be *
! *                                   in GVF format or not.              *
! *         TRP_DIR  ( CHARACTER ) -- Directory name with files with     *
! *                                   external tropospheric path delays. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       N_FIL_TRP ( INTEGER*4 ) -- The total number of files with      *
! *                                  external atmospheric path delay.    *
! * ADR_TRP_FIL_BUF ( INTEGER*8 ) -- The address of the text buffer that *
! *                                  keeps names of files with external  *
! *                                  atmospheric path delay. The buffer  *
! *                                  is sorted in such a way that the    *
! *                                  i-th line of ADR_TRP_FIL_BUF        *
! *                                  corresponds to the i-th line of     *
! *                                  ADR_TRP_SES_BUF.                    *
! *     STS_TRP_FIL ( INTEGER*4 ) -- Status of the the text buffer that  *
! *                                  keeps names of files with external  *
! *                                  atmospheric path delay. Possible    *
! *                                  values: UNDF__TRP, ALLO__TRP, and   *
! *                                  LOAD__TRP.                          *
! * ADR_TRP_SES_BUF ( INTEGER*8 ) -- The address of the text buffer that *
! *                                  keeps names of VLBI exeriments that *
! *                                  corresponds to files with external  *
! *                                  atmospheric path delay. The buffer  *
! *                                  is sorted in alphabetic order.      *
! *     STS_TRP_SES ( INTEGER*4 ) -- Status of the the text buffer that  *
! *                                  keeps names of files with external  *
! *                                  atmospheric path delay. Possible    *
! *                                  values: UNDF__TRP, ALLO__TRP, and   *
! *                                  LOAD__TRP.                          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 or IUER=-2 --     *
! *                                  in the case of error the message    *
! *                                  will be put on stdout.              *
! *                           Output: If input value IUER == -2, the     *
! *                                   variable is mot modified,          *
! *                                   otherwise IUER=0 in the case of    *
! *                                   successful completion and non-zero *
! *                                   in the case of error.              *
! *                                                                      *
! *  ### 07-FEB-2008    TRP_INIT   v1.0 (c)  L. Petrov  07-FEB-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'trp.i'
      LOGICAL*4  FL_GVF
      CHARACTER  TRP_DIR*(*)
      ADDRESS__TYPE :: ADR_TRP_FIL_BUF, ADR_TRP_SES_BUF
      INTEGER*4  N_FIL_TRP, STS_TRP_FIL, STS_TRP_SES, IUER
      CHARACTER  FINAM*128, STR*128
      CHARACTER, ALLOCATABLE :: TRP_FIL_BUF(:)*128, TRP_SES_BUF(:)*10
      SAVE       TRP_FIL_BUF, TRP_SES_BUF
      INTEGER*4  J1, J2, J3, J4, J5, LEV, IND_FIL, IS, LUN, IER
      ADDRESS__TYPE :: DIR_DESC(16)
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN, GET_UNIT
!
      N_FIL_TRP = 0
      STS_TRP_FIL = UNDF__TRP
      WRITE ( 6, '(A)' ) '  Read directory with external tropospheric path delay files'
!
! --- First pass: determine the number of valid files with external 
! --- atmospheric path delay path 
!
      LEV = 0
      DO 410 J1=1,1024*1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, TRP_DIR, FINAM )
         IF ( LEV == 0 ) GOTO 810
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 8131, IUER, 'TRP_INIT', 'Error in reading '// &
     &            'directory '//TRP_DIR(1:I_LEN(TRP_DIR))//' -- '//FINAM )
              RETURN 
         END IF
         IF ( ILEN(FINAM) < 5 ) GOTO 410
         IF ( INDEX ( FINAM, '#' ) > 0 ) GOTO 410
         IF ( INDEX ( FINAM, '~' ) > 0 ) GOTO 410
!
! ------ Only files with specific extnensions are counted for!
!
         IF ( FINAM(ILEN(FINAM)-3:ILEN(FINAM)) == '.spm' .OR. &
     &        FINAM(ILEN(FINAM)-3:ILEN(FINAM)) == '.trp'      ) THEN
              N_FIL_TRP = N_FIL_TRP + 1
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( N_FIL_TRP == 0 ) THEN
           CALL ERR_LOG ( 8132, IUER, 'TRP_INIT', 'No files with the '// &
     &         'external atmospheric path delays were found in the '// &
     &         'directory '//TRP_DIR(1:I_LEN(TRP_DIR))//' NB: only '// &
     &         'files wiht extensions .spm and .trp are considered' )
           RETURN 
      END IF
!
! --- Allocate memory for the buffer with names of external atmospheric 
! --- path delay files
!
      ALLOCATE ( TRP_FIL_BUF(N_FIL_TRP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( N_FIL_TRP*128, STR )
           CALL ERR_LOG ( 8133, IUER, 'TRP_INIT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic menory for the '// &
     &         'buffer of atmospheric path delay files' )
           RETURN 
      END IF
      STS_TRP_FIL = ALLO__TRP
!
! --- Get names of exterinal atmospheric path delay files and put them
! --- into the buffer TRP_FIL_BUF
!
      LEV = 0
      IND_FIL = 0
      DO 420 J2=1,1024*1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, TRP_DIR, FINAM )
         IF ( LEV == 0 ) GOTO 820
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 8134, IUER, 'TRP_INIT', 'Error in reading '// &
     &            'directory '//TRP_DIR(1:I_LEN(TRP_DIR))//' -- '//FINAM )
              RETURN 
         END IF
         IF ( ILEN(FINAM) < 5 ) GOTO 420
         IF ( INDEX ( FINAM, '#' ) > 0 ) GOTO 420
         IF ( INDEX ( FINAM, '~' ) > 0 ) GOTO 420
         IF ( FINAM(ILEN(FINAM)-3:ILEN(FINAM)) == '.spm' .OR. &
     &        FINAM(ILEN(FINAM)-3:ILEN(FINAM)) == '.trp'      ) THEN
              IND_FIL = IND_FIL + 1
              TRP_FIL_BUF(IND_FIL) = FINAM
         END IF
 420  CONTINUE 
 820  CONTINUE 
      STS_TRP_FIL = LOAD__TRP
      ADR_TRP_FIL_BUF = LOC(TRP_FIL_BUF)
!
! --- Allocate memory for the array with sessions names
!
      ALLOCATE ( TRP_SES_BUF(N_FIL_TRP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( N_FIL_TRP*10, STR )
           CALL ERR_LOG ( 8135, IUER, 'TRP_INIT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for the '// &
     &         'buffer of atmospheric path delay files' )
           RETURN 
      END IF
      STS_TRP_SES = ALLO__TRP
      LUN = GET_UNIT()
!
! --- Cucle over all files with external atmospheric path delay in 
! --- the directory, read their header anda extract hte session name
!
      DO 430 J3=1,N_FIL_TRP
!
! ------ Open the external atmospheric path file
!
         OPEN ( UNIT=LUN, FILE=TRP_FIL_BUF(J3), STATUS='OLD', IOSTAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IER, STR )
              CALL ERR_LOG ( 8136, IUER, 'TRP_INIT', 'Failure to open '// &
     &            'file '//TRP_FIL_BUF(J3)(1:I_LEN(TRP_FIL_BUF(J3)))//' '// &
     &            'Errir: '//STR )
              RETURN 
         END IF
!
! ------ Check the first line
!
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) STR
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IER, STR )
              CALL ERR_LOG ( 8137, IUER, 'TRP_INIT', 'Failure to read '// &
     &            'file '//TRP_FIL_BUF(J3)(1:I_LEN(TRP_FIL_BUF(J3)))//' '// &
     &            'Error: '//STR )
              RETURN 
         END IF
!
! ------ It hsould have a valid label
!
         IF ( STR(1:LEN(TRP__LABEL)) == TRP__LABEL ) THEN
              CONTINUE 
            ELSE IF ( STR(1:LEN(TRP__LABEL)) == TRP__LABEL_V10 ) THEN
              CONTINUE 
            ELSE 
              CALL ERR_LOG ( 8138, IUER, 'TRP_INIT', 'File '// &
     &             TRP_FIL_BUF(J3)(1:I_LEN(TRP_FIL_BUF(J3)))//' has wrong '// &
     &            'format: the first line is '//STR(1:I_LEN(STR))// &
     &            ' while '//TRP__LABEL//' was expected' )
              RETURN 
         END IF
!
! ------ Read other lines...
!
         DO 440 J4=1,1024*1024
             READ ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) STR
             IF ( IER == -1 ) THEN
                  WRITE ( 6, * ) 'Line: ', J4
                  CALL ERR_LOG ( 8139, IUER, 'TRP_INIT', 'File '// &
     &                 TRP_FIL_BUF(J3)(1:I_LEN(TRP_FIL_BUF(J3)))//' ended '// &
     &                'prematurely' )
                  RETURN 
             END IF
             IF ( IER .NE. 0 ) THEN
                  CALL CLRCH ( STR )
                  CALL INCH  ( IER, STR )
                  CALL ERR_LOG ( 8140, IUER, 'TRP_INIT', 'Failure to read '// &
     &                'file '//TRP_FIL_BUF(J3)(1:I_LEN(TRP_FIL_BUF(J3)))//' '// &
     &                'Error: '//STR )
                 RETURN 
             END IF
!
! ---------- ... till we find either experiment name or the database name
!
! ---------- Put temporarily the exprement name in the first 10 characeters 
! ---------- of the TRP_FIL_BUF buffer
!
!
             IF ( FL_GVF .AND. STR(1:1) == 'E' ) THEN
                  TRP_FIL_BUF(J3) = STR(4:13)//TRP_FIL_BUF(J3) 
                  GOTO 840
                ELSE IF ( .NOT. FL_GVF .AND. STR(1:1) == 'H' ) THEN
                  TRP_FIL_BUF(J3) = STR(4:13)//TRP_FIL_BUF(J3) 
                  GOTO 840
             END IF
 440     CONTINUE 
 840     CONTINUE 
         CLOSE ( UNIT=LUN )
 430  CONTINUE 
!
! --- Now sort buffer TRP_FIL_BUF in alphabetic order of experiment names
!
      CALL SORT_FAST_CH ( N_FIL_TRP, TRP_FIL_BUF )
!
! --- And the last: remove the session name from TRP_FIL_BUF and put it 
! --- into TRP_SES_BUF
!
      DO 450 J5=1,N_FIL_TRP
         TRP_SES_BUF(J5)(1:10) = TRP_FIL_BUF(J5)(1:10)
         TRP_FIL_BUF(J5) = TRP_FIL_BUF(J5)(11:)
         CALL CLRCH ( TRP_FIL_BUF(J5)(119:128) )
 450  CONTINUE 
      ADR_TRP_SES_BUF = LOC(TRP_SES_BUF)
      STS_TRP_SES = LOAD__TRP
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TRP_INIT  !#!  
