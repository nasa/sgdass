       PROGRAM    GET_FREQ_TABLE_FROM_FITS_MAIN
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
       CALL GET_FREQ_TABLE_FROM_FITS()
       END  PROGRAM  GET_FREQ_TABLE_FROM_FITS_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE  GET_FREQ_TABLE_FROM_FITS()
! ************************************************************************
! *                                                                      *
! *   Program GET_FREQ_TABLE_FROM_FITS reads the input file, extacts     *
! *   infroamtion about observed intemediate frequencies (IFs) and       *
! *   writes down a table with five columns in stdout. Columns:          *
! *                                                                      *
! *   1) IF start in MHz;                                                *
! *   2) IF end   in MHz (IF end is always greate IF start);             *
! *   3) IF bandwidrht in MHz;                                           *
! *   4) The number of spectral channels;                                *
! *   5) Spectral resoluiton in MHz.                                     *
! *                                                                      *
! * ## 15-NOV-2011 GET_FREQ_TABLE_FROM_FITS v2.0 L. Petrov 04-FEB-2022 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      CHARACTER  FIL*128
      REAL*8     FRQ_BEG(PIM__MFRQ), FRQ_END(PIM__MFRQ), FRQ_WID(PIM__MFRQ), CHN_WID
      INTEGER*4  NFRQ, NCHN, J1, IUER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: get_freq_table_from_fits {fits_file}'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FIL )
      END IF
      CALL PIMA_INIT ( PIM )
!
      PIM%L_FIL = 1
      ALLOCATE ( PIM%FILE(1) )
      PIM%FILE(1)%NAME = FIL
!
      IUER = -1
      CALL GET_FRQ_TAB ( PIM, NFRQ, NCHN, FRQ_BEG, FRQ_END, FRQ_WID, CHN_WID, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 410 J1=1,NFRQ
         WRITE ( 6, 110 ) 1.D-6*FRQ_BEG(J1), 1.D-6*FRQ_END(J1), &
     &                    1.D-6*FRQ_WID(J1), NCHN, 1.D-6*CHN_WID
 110     FORMAT ( F10.3, 1X, F10.3, 2X, F8.3, 2X, I5, 2X, F9.6 )
 410  CONTINUE 
      END  SUBROUTINE  GET_FREQ_TABLE_FROM_FITS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_FRQ_TAB ( PIM, NFRQ, NCHN, FRQ_BEG, FRQ_END, FRQ_WID, &
     &                         CHN_WID, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_FRQ_TAB parses input FITS files and extracts          *
! *   information about observed frequencies.                            *
! *                                                                      *
! *  ### 23-JAN-2009  GET_FRQ_TAB  v2.0 (c)  L. Petrov  04-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  NFRQ, NCHN, IUER
      REAL*8     FRQ_BEG(PIM__MFRQ), FRQ_END(PIM__MFRQ), FRQ_WID(PIM__MFRQ), CHN_WID
      CHARACTER  TABLE_NAME*32, STR*128, STR1*128, EXT_NAME*32
      REAL*8,    ALLOCATABLE :: FREQ(:,:), BAND_WIDTH(:,:), CHAN_WIDTH(:,:)
      INTEGER*4, ALLOCATABLE :: SIDE_BAND(:,:), BB_SP_CHAN_IND(:,:)
      INTEGER*4  J1, J2, J3, IND_FRQ_TAB, IND_FRQ_NFRQ, &
     &           IND_FRQ_BFRQ, IND_FRQ_BWID, IND_FRQ_CWID, IND_FRQ_SBND, &
     &           IND_FRQ_IBBC, IND_FRQ_REF, IP, NAXIS2, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFITS_OPEN ( PIM%FILE(1)%NAME, PIM%FILE(1)%FITS_DESC, 'OLD', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7811, IUER, 'GET_FRQ_TAB', 'Error in an attempt '// &
     &         'to open FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      CALL ERR_PASS       ( IUER, IER )
      CALL FFITS_GET_KEYP ( PIM%FILE(1)%FITS_DESC, PIM__MHDR, PIM__MKWD, &
     &                      PIM%FILE(1)%M_KWD, PIM%FILE(1)%L_HDR, &
     &                      PIM%FILE(1)%L_KWD, PIM%FILE(1)%KEY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7812, IUER, 'GET_FRQ_TAB', 'Error in an attempt '// &
     &         'to get keys from FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      IND_FRQ_TAB  = 0
      IND_FRQ_NFRQ = 0
      IND_FRQ_REF  = 0
      IND_FRQ_BFRQ = 0
      IND_FRQ_BWID = 0
      IND_FRQ_CWID = 0
      IND_FRQ_SBND = 0
      IND_FRQ_IBBC = 0
!
      DO 410 J1=1,PIM%FILE(1)%L_HDR
         CALL CLRCH ( EXT_NAME )
         DO 420 J2=1,PIM%FILE(1)%L_KWD(J1)
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:8) == 'EXTNAME ' ) THEN
                 EXT_NAME = PIM%FILE(1)%KEY(J2,J1)(11:)
            END IF
            IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "NAXIS2  =" ) > 0 ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(19:30), &
     &                  FMT='(I12)' ) NAXIS2
            END IF
            IF ( EXT_NAME(1:11) == "'FREQUENCY'" ) THEN
                 IND_FRQ_TAB = J1
                 PIM%FILE(1)%NFRG = NAXIS2
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "NO_BAND " ) > 0 ) THEN
                      IND_FRQ_NFRQ = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "REF_FREQ" ) > 0 ) THEN
                      IND_FRQ_REF  = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'BANDFREQ'" ) > 0 .OR. &
     &                INDEX ( PIM%FILE(1)%KEY(J2,J1), "'IF FREQ         '" ) > 0 ) THEN
                      IND_FRQ_BFRQ = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'TOTAL_BANDWIDTH'"  ) > 0 .OR. &
     &                INDEX ( PIM%FILE(1)%KEY(J2,J1), "'TOTAL BANDWIDTH '" ) > 0 ) THEN
                      IND_FRQ_BWID = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'CH_WIDTH'" ) > 0 .OR. &
     &                INDEX ( PIM%FILE(1)%KEY(J2,J1), "'CH WIDTH        '" ) > 0 ) THEN
                      IND_FRQ_CWID = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'SIDEBAND'" ) > 0 .OR. &
     &                INDEX ( PIM%FILE(1)%KEY(J2,J1), "'SIDEBAND        '" ) > 0 ) THEN
                      IND_FRQ_SBND = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'BB_CHAN '" ) > 0 ) THEN
                      IND_FRQ_IBBC = J2
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( IND_FRQ_NFRQ == 0 ) THEN
           CALL ERR_LOG ( 7145, IUER, 'PIMA_INDX', 'Keyword NO_BAND '// &
     &         'was not found in the FREQUENCY table in reading file '// &
     &          PIM%FILE(1)%NAME )
           RETURN
      END IF
      IF ( IND_FRQ_BFRQ == 0 ) THEN
           CALL ERR_LOG ( 7146, IUER, 'PIMA_INDX', 'Keyword BANDFREQ '// &
     &         'was not found in the FREQUENCY table in reading file '// &
     &          PIM%FILE(1)%NAME )
           RETURN
      END IF
      IF ( IND_FRQ_BWID == 0 ) THEN
           CALL ERR_LOG ( 7147, IUER, 'PIMA_INDX', 'Keyword TOTAL_BANDWIDTH '// &
     &         'was not found in the FREQUENCY table in reading file '// &
     &          PIM%FILE(1)%NAME )
           RETURN
      END IF
      IF ( IND_FRQ_CWID == 0 ) THEN
           CALL ERR_LOG ( 7148, IUER, 'PIMA_INDX', 'Keyword CH_WIDTH '// &
     &         'was not found in the FREQUENCY table in reading file '// &
     &          PIM%FILE(1)%NAME )
           RETURN
      END IF
      IF ( IND_FRQ_SBND == 0 ) THEN
           CALL ERR_LOG ( 7149, IUER, 'PIMA_INDX', 'Keyword SIDEBAND '// &
     &         'was not found in the FREQUENCY table in reading file '// &
     &          PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      TABLE_NAME = 'ARRAY_GEOMETRY'
!
! --- Get the reference frequency of the first file
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_R8 ( PIM, 1, TABLE_NAME, 'REF_FREQ', &
     &                       PIM%REF_FREQ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7105, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the value of reference frequency REF_FREQ' )
           RETURN
      END IF
!
! --- Get the spectral channel width
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_R8 ( PIM, 1, TABLE_NAME, 'CHAN_BW', &
     &                       PIM%CHAN_BW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7106, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the value of reference frequency CHAN_BW' )
           RETURN
      END IF
!
! --- Get the number of spectral channels in one IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_I4 ( PIM, 1, TABLE_NAME, 'NO_CHAN', &
     &                       PIM%NCHN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7107, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the the number of spectral channels' )
           RETURN
      END IF
!
      STR = PIM%FILE(1)%KEY(IND_FRQ_NFRQ,IND_FRQ_TAB)(10:)
      CALL CHASHL ( STR )
      IP = INDEX ( STR, ' ' )
      IF ( IP .LE. 1 ) THEN
           CALL ERR_LOG ( 7159, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the number frequencies from the FREQUENCY table '// &
     &         'in FITS-IDI file '// &
     &          PIM%FILE(1)%NAME(1:I_LEN(PIM%FILE(1)%NAME))//' '// &
     &          PIM%FILE(1)%KEY(IND_FRQ_NFRQ,IND_FRQ_TAB) )
           RETURN
      END IF
!
      READ ( UNIT=STR(1:IP-1), FMT='(I4)', IOSTAT=IER ) PIM%FILE(1)%NFRQ
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' STR = ', STR(1:I_LEN(STR))
           CALL ERR_LOG ( 7160, IUER, 'PIMA_INDX', 'Failure to decode '// &
     &         'the number frequencies from the FREQUENCY table '// &
     &         'in FITS-IDI file '// &
     &         PIM%FILE(1)%NAME(1:I_LEN(PIM%FILE(1)%NAME))//' '// &
     &         PIM%FILE(1)%KEY(IND_FRQ_NFRQ,IND_FRQ_TAB) )
           RETURN
      END IF
!
      IF ( PIM%FILE(1)%NFRQ > PIM__MFRQ ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM%FILE(1)%NFRQ, STR  )
           CALL INCH  ( PIM__MFRQ,         STR1 )
           CALL ERR_LOG ( 7161, IUER, 'PIMA_INDX', 'Too many '// &
               'frequencies: '//STR(1:I_LEN(STR))//' -- more than '// &
     &         ' PIM__MFRQ: '//STR1(1:I_LEN(STR1))// &
     &         '. Consider to increase parameter PIM__MFRQ' )
           RETURN
      END IF
!
      IF ( PIM%FILE(1)%NFRG > PIM__MFRG ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM%FILE(1)%NFRG, STR  )
           CALL INCH  ( PIM__MFRG,         STR1 )
           CALL ERR_LOG ( 7162, IUER, 'PIMA_INDX', 'Too many '// &
               'frequency groups: '//STR(1:I_LEN(STR))//' -- more than '// &
     &         ' PIM__MFRG: '//STR1(1:I_LEN(STR1))// &
     &         '. Consider to increase parameter PIM__MFRG' )
           RETURN
      END IF
!
      ALLOCATE ( FREQ(PIM%FILE(1)%NFRQ,PIM%FILE(1)%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, STR )
           CALL ERR_LOG ( 7164, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &         'temporary array FREQ' )
           RETURN
      END IF
!
      ALLOCATE ( BAND_WIDTH(PIM%FILE(1)%NFRQ,PIM%FILE(1)%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, STR )
           CALL ERR_LOG ( 7165, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &         'temporary array BAND_WIDTH' )
           RETURN
      END IF
!
      ALLOCATE ( CHAN_WIDTH(PIM%FILE(1)%NFRQ,PIM%FILE(1)%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, STR )
           CALL ERR_LOG ( 7166, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &         'temporary array CHAN_WIDTH' )
           RETURN
      END IF
!
      ALLOCATE ( SIDE_BAND(PIM%FILE(1)%NFRQ,PIM%FILE(1)%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, STR )
           CALL ERR_LOG ( 7167, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &         'temporary array SIDE_BAND' )
           RETURN
      END IF
!
      ALLOCATE ( BB_SP_CHAN_IND(PIM%FILE(1)%NFRQ,PIM%FILE(1)%NFRG), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, STR )
           CALL ERR_LOG ( 7168, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &         'temporary array BB_SP_CHAN_IND' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_FRQ_TAB, 1, &
     &                   PIM%FILE(1)%KEY(IND_FRQ_BFRQ,IND_FRQ_TAB), &
     &                   PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, FREQ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 1, STR )
           CALL ERR_LOG ( 7169, IUER, 'PIMA_INDX', 'Error in '// &
     &         'getting bandwidth frequency array for the '// &
     &          STR(1:I_LEN(STR))//'-th UV data of the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_FRQ_TAB, 1, &
     &           PIM%FILE(1)%KEY(IND_FRQ_BWID,IND_FRQ_TAB), &
     &           PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, BAND_WIDTH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 1, STR )
           CALL ERR_LOG ( 7170, IUER, 'PIMA_INDX', 'Error in '// &
     &         'getting array of frequency bandwidth for the '// &
     &          STR(1:I_LEN(STR))//'-th UV data of the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_FRQ_TAB, 1, &
     &           PIM%FILE(1)%KEY(IND_FRQ_CWID,IND_FRQ_TAB), &
     &           PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, CHAN_WIDTH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 1, STR )
           CALL ERR_LOG ( 7171, IUER, 'PIMA_INDX', 'Error in '// &
     &         'getting array of frequency bandwidthfor the '// &
     &          STR(1:I_LEN(STR))//'-th UV data of the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETI4 ( PIM%FILE(1)%FITS_DESC, IND_FRQ_TAB, 1, &
     &           PIM%FILE(1)%KEY(IND_FRQ_SBND,IND_FRQ_TAB), &
     &           PIM%FILE(1)%NFRQ*PIM%FILE(1)%NFRG, SIDE_BAND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 1, STR )
           CALL ERR_LOG ( 7172, IUER, 'PIMA_INDX', 'Error in '// &
     &         'getting array of frequency bandwidthf or the '// &
     &          STR(1:I_LEN(STR))//'-th UV data of the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
      CHN_WID = CHAN_WIDTH(1,1)
      NFRQ = PIM%FILE(1)%NFRQ
      NCHN = PIM%NCHN
!
      DO 430 J3=1,NFRQ
         FRQ_BEG(J3) = PIM%REF_FREQ + FREQ(J3,1)
         FRQ_END(J3) = PIM%REF_FREQ + FREQ(J3,1) + NCHN*CHN_WID
         FRQ_WID(J3) = NCHN*CHN_WID
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_FRQ_TAB  !#!#
