      SUBROUTINE WRITE_DIACORR ( COR_TYP, B3DOBJ, INCFIL, EXCFIL, N_PAR, &
     &                           COV_MAT, PAR_LIS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRITE_DIACORR  computes elements of the estimates of the  *
! *   correlation matrix between adjustments obtained by LSQ in SOLVE    *
! *   and writes them down in CORLxx file.                               *
! *                                                                      *
! *   WRITE_DIACORR computes elements of the correlations matrix which   *
! *   correspond to the parameters listed in include-parameters file     *
! *   INCFIL and are not listed in exclude-parameters file EXCFIL.       *
! *   Wild-card symbols * and  ? are supported in INCFIL or EXCFIL.      *
! *   If EXCFIL is an empty file then ESCFIL is not been examined.       *
! *                                                                      *
! *   A header to CORLxx file is written if it is still empty.           *
! *                                                                      *
! *   Two correlation types are supported: GLO_GLO and LOC_LOC.          *
! *                                                                      *
! *   WRITE_DIACORR supports both FAST__B1B3D and FAST__NONE modes.      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * COR_TYP ( CHARACTER ) -- Correlation type. One of                    *
! *                       GLO_GLO -- correlations between global         *
! *                                  parameters. Applicable for global   *
! *                                  runs only.                          *
! *                       LOC_LOC -- correlations between local          *
! *                                  parameters. Local parameters are    *
! *                                  parameters which are neither global *
! *                                  nor segmented.                      *
! *                                  Parameters are considered as         *
! *                                  a) local in non-fast mode of        *
! *                                     independent solution;            *
! *                                  b) local and global in fast mode of *
! *                                     independent solution;            *
! *                                  c) local and global in non-fast     *
! *                                     mode of global solution;         *
! *                                  d) local global and segmented in    *
! *                                     fast mode of global solution;    *
! *  B3DOBJ ( RECORD    ) -- Object with data structure for B3D          *
! *                          extension of SOLVE.                         *
! *  INCFIL ( CHARACTER ) -- Name of the file with the list of           *
! *                          parameters names which should be included   *
! *                          in the list of parameters between which     *
! *                          correlations are to be computed and written.*
! *  EXCFIL ( CHARACTER ) -- Name of the file with the list of           *
! *                          parameters names which should be included   *
! *                          in the list of parameters between which     *
! *                          correlations are to be computed and written.*
! *   N_PAR ( INTEGER*4 ) -- Number of parameters in the covariance      *
! *                          matrix.                                     *
! * COV_MAT ( REAL*8    ) -- Covariance matrix of the adjustments.       *
! * PAR_LIS ( CHARACTER ) -- List of parameters to have been estimated.  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  pet 2001.05.18  Added support of binary format of the correlation   *
! *                  file.                                               *
! *  pet 2001.05.22  Made lenght of all binary records to be a multiple  *
! *                  of 8 bytes.                                         *
! *  pet 2003.08.22  Fixed a bug: M_GPA is of INTEGER*2 type and       *
! *                  should be wrapped with  INT4()                      *
! *                                                                      *
! * ###  06-OCT-99   WRITE_DIACORR   v2.2 (c) L. Petrov 22-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      INCLUDE   'corel.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  N_PAR, IUER
      CHARACTER  COR_TYP*(*), INCFIL*(*), EXCFIL*(*), PAR_LIS(N_PAR)*(*), &
     &           INCBUF(M_GPA)*20, EXCBUF(M_GPA)*20, FILOUT*256, &
     &           START_DATE*64, STOP_DATE*64, OUT*128, CHAR_LEN8*128
      REAL*8     COV_MAT(*)
      LOGICAL*4  LEX, FL_PAR(M_GPA)
      INTEGER*4  N_INC, N_EXC, N_COR, IND_MAT(M_GPA), IND_COR(M_GPA), &
     &           STAT_BLOCK(12), IS, IL, I11, IP, LUN_CRL, L_BUF, &
     &           J1, J2, J3, J4, J5, J6, IER
      REAL*8     SIG_SQ1, SIG_SQ2, COR, FJDOBS, LJDOBS, VAL__MIN
      PARAMETER  ( VAL__MIN = 1.D-48 )
      BYTE         B_BUF(M_BBUF)
      CHARACTER  JD_TO_DATE*23
      LOGICAL*1  TITLE_ANON, FL_GLOBAL_L1
      INTEGER*4, EXTERNAL :: UNIT_TO_FILDESC, GET_UNIT, IFIND_PL, &
     &                       ILEN, I_LEN, LTM_DIF, FOR_STAT
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INTEGER*4   I, J, I_LEN8
      INTEGER*8   LOCC
      LOCC(I,J) = INT8(min(I,J)) +(INT8(max(I,J))*INT8(max(I,J)-1))/2
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      I_LEN8(CHAR_LEN8) = 8*(I_LEN(CHAR_LEN8)/8) + &
     &                    MAX ( 8, MOD(I_LEN(CHAR_LEN8),8) )
!
      IF ( COR_TYP .NE. 'GLO_GLO'  .AND. &
     &     COR_TYP .NE. 'LOC_LOC'         ) THEN
           CALL ERR_LOG ( 8801, IUER, 'WRITE_DIACORR', 'Correlation type '// &
     &                    COR_TYP//' is not supported by WRITE_DIACORR '// &
     &                   'GLO_GLO or LOC_LOC was expected' )
           RETURN
      END IF
!
      IF ( CRES_STYLE .EQ. CRES__PRE98 ) THEN
           TITLE_ANON = .FALSE.
         ELSE
           TITLE_ANON = .TRUE.
      END IF
!
! --- Check: does INCFIL file exist
!
      INQUIRE ( FILE=INCFIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8802, IUER, 'WRITE_DIACORR', 'INCFIL: '// &
     &          INCFIL(1:I_LEN(INCFIL))//' was not found' )
           RETURN
      END IF
!
! --- Read INCFIL
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( INCFIL, M_GPA, INCBUF, N_INC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8803, IUER, 'WRITE_DIACORR', 'Error in '// &
     &         'attempt to read include parameters file '//INCFIL )
           RETURN
      END IF
      IF ( ILEN(EXCFIL) .GT. 0 ) THEN
!
! -------- Check: does EXCFIL file exist
!
           INQUIRE ( FILE=EXCFIL, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 8804, IUER, 'WRITE_DIACORR', 'EXCFIL: '// &
     &               EXCFIL(1:I_LEN(EXCFIL))//' was not found' )
                RETURN
           END IF
!
! -------- Read EXCFIL
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( EXCFIL, M_GPA, EXCBUF, N_EXC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8805, IUER, 'WRITE_DIACORR', 'Error in '// &
     &              'attempt to read include parameters file '//EXCFIL )
                RETURN
           END IF
        ELSE
           N_EXC = 0
      END IF
!
! --- Match parameters from INC-list
!
      DO 410 J1=1,N_PAR
         FL_PAR(J1) = .FALSE. ! initialize the flag
         IL = LTM_DIF ( 3, N_INC, INCBUF, PAR_LIS(J1) )
         IF ( IL .GT. 0 ) THEN
              FL_PAR(J1) = .TRUE. ! Set flag to TRUE if found
         END IF
 410  CONTINUE
!
! --- Match parameters from EXC-list
!
      IF ( N_EXC .GT. 0 ) THEN
           DO 420 J2=1,N_PAR
              IL = LTM_DIF ( 3, N_EXC, EXCBUF, PAR_LIS(J2) )
              IF ( IL .GT. 0 ) THEN
                   FL_PAR(J2) = .FALSE. ! Set flag to FALSE if found
              END IF
 420       CONTINUE
      END IF
!
! --- Form the list of the parameters to be dealt with.
! --- N_COR   -- number of parameters eligible for computation of correlations
! --- IND_COR -- array indeces of the parameters in the parameter list PAR_LIS
! --- IND_MAT -- array indeces of the parameters in the covariance matrix
! ---            COV_MAT
!
      N_COR = 0
      DO 430 J3=1,N_PAR
         IF ( FL_PAR(J3) ) THEN
              IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ---------------- Seach in the INF_GLO list
!
                   IP = IFIND_PL ( B3DOBJ%N_GLO, B3DOBJ%INF_GLO, J3 )
                   IF ( IP .GT. 0 ) THEN
                        N_COR = N_COR + 1
                        IND_COR(N_COR) = J3
                        IND_MAT(N_COR) = IP
                   END IF
                ELSE IF ( FAST_MODE .EQ. F__B1B3D  .AND. &
     &                    COR_TYP .EQ. 'LOC_LOC'         ) THEN
!
! ---------------- Search in the INF_LOC list
!
                   IP = IFIND_PL ( B3DOBJ%N_LOC, B3DOBJ%INF_LOC, J3 )
                   IF ( IP .GT. 0 ) THEN
                        N_COR = N_COR + 1
                        IND_COR(N_COR) = J3
                        IND_MAT(N_COR) = IP
                   END IF
                ELSE
!
! ---------------- The J3-th parameter is sutable.
!
                   N_COR = N_COR + 1
                   IND_COR(N_COR) = J3
                   IND_MAT(N_COR) = J3
              END IF
         END IF
 430  CONTINUE
!
! --- Get the free logical unit for Fortran I/O
!
      LUN_CRL = GET_UNIT ()
!
! --- Open the output file
!
      CALL CLRCH ( FILOUT )
      FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'CORL'//PRE_LETRS
      OPEN ( UNIT=LUN_CRL, FILE=FILOUT, STATUS='UNKNOWN', ACCESS='APPEND', &
     &       IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL ERR_LOG ( 8806, IUER, 'WRITE_DIACORR', 'Error in '// &
     &         'openning output correlation file '//FILOUT )
           RETURN
      END IF
!
! --- Check its length
!
      IS = FOR_STAT ( FILOUT, STAT_BLOCK )
      IF ( STAT_BLOCK(8) .EQ. 0 ) THEN
!
! -------- Length of the output CORL file is zero -- add the format label
!
           FL_GLOBAL_L1 = .TRUE.
           IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
                WRITE ( LUN_CRL, '(A)' ) ASCII__CRL
              ELSE IF ( COROUT_FORM .EQ. CRL__BIN ) THEN
                CALL CLRCH ( OUT )
                OUT = BINARY__CRL
                CALL ERR_PASS ( IUER, IER )
                CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), OUT(1:48), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8807, IUER, 'WRITE_DIACORR', 'Error '// &
     &                   'in an attempt to write the first binary record to '// &
     &                   'the output correlation file '//FILOUT )
                     RETURN
                END IF
              ELSE
                CALL CLRCH ( OUT )
                CALL INCH  ( COROUT_FORM, OUT )
                CALL ERR_LOG ( 8808, IUER, 'WRITE_DIACORR', 'Trap of '// &
     &              'internal control: wrong galue of COROUT_FORM: '//OUT )
                RETURN
           END IF
!
! -------- ... and a general header
!
           CALL SOLUTION_IDENT ( COROUT_FORM, LUN_CRL, '# ', FL_GLOBAL_L1, &
     &                           TITLE_ANON )
      END IF
!
! --- Add the specific header
!
      WRITE ( OUT, '(A)' ) '# '
      IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
           WRITE ( LUN_CRL, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
      WRITE ( OUT, '(A)' ) '# Type:            '//COR_TYP//' Correlations'
      IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
           WRITE ( LUN_CRL, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
      WRITE ( OUT, '(A)' ) '# '
      IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
           WRITE ( LUN_CRL, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
      IF ( COR_TYP .EQ. 'LOC_LOC' ) THEN
!
! -------- Add more line in the specific header fo LOC_LOC correlations
!
           CALL OBSTM ( FJDOBS, LJDOBS )
           CALL CLRCH ( START_DATE )
           CALL CLRCH ( STOP_DATE  )
!
           WRITE ( UNIT=START_DATE(1:16), FMT='(F16.8)' ) FJDOBS
           WRITE ( UNIT=STOP_DATE(1:16),  FMT='(F16.8)' ) LJDOBS
           START_DATE(21:) = JD_TO_DATE ( FJDOBS, -3 )
           STOP_DATE(21:)  = JD_TO_DATE ( LJDOBS, -3 )
!
           WRITE ( LUN_CRL, '(A)' ) '# Database:       '//B3DOBJ%DBNAME_MES
           IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
                WRITE ( LUN_CRL, '(A)' ) OUT(1:I_LEN(OUT))
             ELSE
               CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), &
     &              OUT(1:I_LEN8(OUT)), -3 )
           END IF
!
           WRITE ( LUN_CRL, '(A)' ) '# Start_date:     '//START_DATE(1:43)
           IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
                WRITE ( LUN_CRL, '(A)' ) OUT(1:I_LEN(OUT))
             ELSE
               CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), &
     &              OUT(1:I_LEN8(OUT)), -3 )
           END IF
!
           WRITE ( LUN_CRL, '(A)' ) '# Stop_date:      '//STOP_DATE(1:43)
           IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
                WRITE ( LUN_CRL, '(A)' ) OUT(1:I_LEN(OUT))
             ELSE
               CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), &
     &              OUT(1:I_LEN8(OUT)), -3 )
           END IF
!
           WRITE ( LUN_CRL, '(A)' ) '# '
           IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
                WRITE ( LUN_CRL, '(A)' ) OUT(1:I_LEN(OUT))
             ELSE
               CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), &
     &              OUT(1:I_LEN8(OUT)), -3 )
           END IF
      END IF
!
      L_BUF = 0
      IF ( N_COR .GT. 1 ) THEN
           IF ( COROUT_FORM .EQ. CRL__BIN  ) THEN
!
! ------------- Write down the number of records and their type
!
                CALL CLRCH ( OUT )
                OUT(1:4) = '$ CH'
                CALL LIB$MOVC3 ( 4, N_COR, %REF(OUT(5:8)) )
                CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), OUT(1:8), -3 )
!
                DO 440 J4=1,N_COR
                   WRITE ( UNIT=OUT, FMT=110, IOSTAT=I11 ) &
     &                     IND_COR(J4), PAR_LIS(IND_COR(J4))
 110               FORMAT ( I5, 1X, A20, 2X )
                   CALL ERR_PASS ( IUER, IER )
                   CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), OUT(1:32), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8809, IUER, 'WRITE_DIACORR', &
     &                      'Error in attempt to write in correlation file '// &
     &                       FILOUT )
                        RETURN
                   END IF
 440            CONTINUE
!
! ------------- Write down the number of records and their type
!
                CALL CLRCH ( OUT )
                OUT(1:4) = '$ CR'
                CALL LIB$MOVC3 ( 4, (N_COR*(N_COR-1))/2, %REF(OUT(5:8)) )
                CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN_CRL), OUT(1:8), -3 )
           END IF
!
! -------- Computation of correlations
!
           DO 450 J5=1,N_COR-1
              SIG_SQ1 = COV_MAT( LOCC(IND_MAT(J5),IND_MAT(J5)) )
              IF ( SIG_SQ1 .LT. VAL__MIN ) SIG_SQ1 = VAL__MIN
              DO 460 J6=J5+1,N_COR
                 SIG_SQ2 = COV_MAT( LOCC(IND_MAT(J6),IND_MAT(J6)) )
                 IF ( SIG_SQ2 .LT. VAL__MIN ) SIG_SQ2 = VAL__MIN
!
! -------------- COR is a correlation coefficient
!
                 COR = COV_MAT( LOCC(IND_MAT(J5),IND_MAT(J6)) )/ &
     &                 DSQRT ( SIG_SQ1*SIG_SQ2 )
!
! -------------- ... and print it
!
                 IF ( COROUT_FORM .EQ. CRL__ASC ) THEN
                      WRITE ( UNIT=LUN_CRL, FMT=120, IOSTAT=I11 ) &
     &                        IND_COR(J5), IND_COR(J6), &
     &                        PAR_LIS(IND_COR(J5)), PAR_LIS(IND_COR(J6)), COR
 120                  FORMAT ( I5, 1X, I5, 2X, '"', A20, '"', &
     &                                      2X, '"', A20, '"', 2X, F12.9 )
                      IF ( I11 .NE. 0 ) THEN
                           CLOSE ( UNIT=LUN_CRL )
                           CALL ERR_LOG ( 8810, IUER, 'WRITE_DIACORR', &
     &                         'Error in writing output correlation file '// &
     &                          FILOUT )
                           RETURN
                      END IF
                   ELSE IF ( COROUT_FORM .EQ. CRL__BIN ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL PUT_BBUF ( LUN_CRL, M_BBUF, L_BUF, B_BUF, &
     &                                IND_MAT(J5), IND_MAT(J6), COR, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8811, IUER, 'WRITE_DIACORR', &
     &                         'Error in attempt to write buffer in file '// &
     &                          FILOUT )
                           RETURN
                      END IF
                 END IF
 460          CONTINUE
 450       CONTINUE
           IF ( COROUT_FORM .EQ. CRL__BIN  .AND.  L_BUF .NE. 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL WRBIN_RECORD ( UNIT_TO_FILDESC(LUN_CRL), L_BUF, B_BUF, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8812, IUER, 'WRITE_DIACORR', 'Error in '// &
     &                   'attempt to write buffer in file '//FILOUT )
                     RETURN
                END IF
           END IF
           L_BUF = 0
      END IF
      CLOSE ( UNIT=LUN_CRL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_DIACORR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PUT_BBUF ( LUN, M_BUF, L_BUF, B_BUF, IND1, IND2, COR, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilary routine PUT_BBUF adds the element to the binary buffer    *
! *   and flush it to the disk if the buffer is full.                    *
! *                                                                      *
! *  ### 18-MAY-2001    PUT_BBUF   v1.0 (c)  L. Petrov  18-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LUN, M_BUF, L_BUF, IND1, IND2, IUER
      REAL*8     COR
      BYTE       B_BUF(M_BUF)
      TYPE      BUF_ELEMENT
          INTEGER*2  IND1_I2
          INTEGER*2  IND2_I2
          REAL*4     COR_R4
      END TYPE  BUF_ELEMENT  ! BUF_ELEMENT !
      TYPE ( BUF_ELEMENT ) ::  B_ELM
      INTEGER*4  UNIT_TO_FILDESC
      INTEGER*4  IER
!
      IF ( L_BUF .EQ. M_BUF ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL WRBIN_RECORD ( UNIT_TO_FILDESC(LUN), M_BUF, B_BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8821, IUER, 'PUT_BBUF', 'Error in attempt '// &
     &              'to write buffer in disk' )
                RETURN
           END IF
           L_BUF = 0
      END IF
!
      B_ELM%IND1_I2 = IND1
      B_ELM%IND2_I2 = IND2
      B_ELM%COR_R4  = COR
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         type *,' b_elm.ind1_i2=',b_elm.ind1_i2, ! %%%
!     #          ' b_elm.ind2_i2=',b_elm.ind2_i2, ! %%%
!     #          ' b_elm.cor_r4 =',b_elm.cor_r4   ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL LIB$MOVC3 ( 8, B_ELM, B_BUF(L_BUF+1) )
      L_BUF = L_BUF + 8
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PUT_BBUF  #!#
