      SUBROUTINE WRITE_GLCORR ( B3DOBJ, INCFIL, EXCFIL, NCOM_PAR, NGLO_PAR, &
     &                          COMPAR_LIS, GLOPAR_LIS, LGPAR_LIS, &
     &                          GG_COVMAT, LG_COVMAT, LL_COVMAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRITE_GLCORR  computes elements of the estimates of the   *
! *   correlation matrix between adjustments of global and local         *
! *   parameters obtained by LSQ in global run SOLVE and writes them     *
! *   down in the output CORLxx file.                                    *
! *                                                                      *
! *   WRITE_GLCORR  computes elements of the correlations matrix which   *
! *   correspond to parameters lists in include-parameters file INCFIL   *
! *   and not listed in exclude-parameters file EXCFIL. Wild-card        *
! *   symbols * and ? are supported in INCFIL or EXCFIL. If the file     *
! *   EXCFIL is an empty then ESCFIL is not been examined.               *
! *                                                                      *
! *   A header to CORLxx file is written if it is still empty.           *
! *                                                                      *
! *   WRITE_GLCORR supports both FAST__B1B3D and FAST__NONE modes.       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     B3DOBJ ( RECORD    ) -- Object with data structure for B3D       *
! *                             extension of SOLVE.                      *
! *     INCFIL ( CHARACTER ) -- Name of the file with the list of        *
! *                             parameters names which should be         *
! *                             included in the list of parameters       *
! *                             between which correlations are to be     *
! *                             computed and written.                    *
! *     EXCFIL ( CHARACTER ) -- Name of the file with the list of        *
! *                             parameters names which should be         *
! *                             included in the list of parameters       *
! *                             between which correlations are to be     *
! *                             computed and written.                    *
! *   NCOM_PAR ( INTEGER*8 ) -- Total number of parameters in this       *
! *                             session including global and local.      *
! *   NGLO_PAR ( INTEGER*8 ) -- Number of parameters of this session     *
! *                             which were treated as global parameters. *
! * COMPAR_LIS ( CHARACTER ) -- List of all parameters of this session   *
! *                             to have been estimated in PROC order.    *
! *                             Dimension: NCOM_PAR.                     *
! * GLOPAR_LIS ( CHARACTER ) -- List of parameters of this session which *
! *                             which were treated as global parameters. *
! *                             Dimension: NGLO_PAR.                     *
! *  LGPAR_LIS ( CHARACTER ) -- List of all parameters of this session   *
! *                             to have been estimated in BACK order:    *
! *                             first all local parameters then all      *
! *                             global parameters. Not used in           *
! *                             FAST__B1B3D mode. Dimension: NCOM_PAR.   *
! *  GG_COVMAT ( REAL*8    ) -- Covariance matrix of global-global       *
! *                             parameters which were supplied by this   *
! *                             session. Not used in FAST__NONE mode.    *
! *  LG_COVMAT ( REAL*8    ) -- Covariance matrix of global-local        *
! *                             parameters which were supplied by this   *
! *                             session. Not used in FAST__NONE mode.    *
! *  LL_COVMAT ( REAL*8    ) -- In FAST__B1B3D mode: covariance matrix   *
! *                                of local-local parameters.            *
! *                             In FAST__NONE mode: covariance matrix    *
! *                                of local+global parameters, where     *
! *                                global are parameters supplied by     *
! *                                this session and treated as global.   *
! *                                The matrix contains first local then  *
! *                                global parameters in the order        *
! *                                determined in LGPAR_LIS array.        *
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
! * ###  08-OCT-99   WRITE_GLCORR   v1.2 (c)  L. Petrov 12-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  IUER
      INTEGER*8  NCOM_PAR, NGLO_PAR
      CHARACTER  INCFIL*(*), EXCFIL*(*), &
     &           COMPAR_LIS(NCOM_PAR)*(*), GLOPAR_LIS(NGLO_PAR)*(*), &
     &           LGPAR_LIS(NCOM_PAR)*(*), &
     &           INCBUF(M_GPA)*20, EXCBUF(M_GPA)*20, FILOUT*256, &
     &           START_DATE*64, STOP_DATE*64
      REAL*8     GG_COVMAT(*), LG_COVMAT(B3DOBJ%N_LOC,B3DOBJ%N_GLO), &
     &           LL_COVMAT(*)
      LOGICAL*4  LEX, FL_PAR(M_GPA)
      INTEGER*4  NG_COR, NL_COR, N_INC, N_EXC, &
     &           INDG_MAT(M_GPA), INDG_COR(M_GPA), &
     &           INDL_MAT(M_GPA), INDL_COR(M_GPA), &
     &           STAT_BLOCK(12), IS, IL, I11, IP, J1, J2, J3, J4, J5, IER
      REAL*8     SIG_SQ1, SIG_SQ2, COR, FJDOBS, LJDOBS
      CHARACTER  JD_TO_DATE*23
      LOGICAL*1  TITLE_ANON, FL_GLOBAL_L1
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, IFIND_PL, FOR_STAT
      INTEGER*4  I, J
      INTEGER*8  LOCC
      LOCC(I,J) = INT8(min(I,J)) +(INT8(max(I,J))*INT8(max(I,J)-1))/2
!
! --- Check: does INCFIL file exist
!
      INQUIRE ( FILE=INCFIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8801, IUER, 'WRITE_GLCORR', 'INCFIL: '// &
     &          INCFIL(1:I_LEN(INCFIL))//' was not found' )
           RETURN
      END IF
!
! --- Read INCFIL
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( INCFIL, M_GPA, INCBUF, N_INC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8802, IUER, 'WRITE_GLCORR', 'Error in '// &
     &         'attempt to read include parameters file '//INCFIL )
           RETURN
      END IF
      IF ( ILEN(EXCFIL) .GT. 0 ) THEN
!
! -------- Check: does EXCFIL file exist
!
           INQUIRE ( FILE=EXCFIL, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 8803, IUER, 'WRITE_GLCORR', 'EXCFIL: '// &
     &               EXCFIL(1:I_LEN(EXCFIL))//' was not found' )
                RETURN
           END IF
!
! -------- Read EXCFIL
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( EXCFIL, M_GPA, EXCBUF, N_EXC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8804, IUER, 'WRITE_GLCORR', 'Error in '// &
     &              'attempt to read include parameters file '//EXCFIL )
                RETURN
           END IF
        ELSE
           N_EXC = 0
      END IF
!
! --- Match parameters from INC-list
!
      DO 410 J1=1,NCOM_PAR
         FL_PAR(J1) = .FALSE. ! initialize the flag
         IL = LTM_DIF ( 3, N_INC, INCBUF, COMPAR_LIS(J1) )
         IF ( IL .GT. 0 ) THEN
              FL_PAR(J1) = .TRUE. ! Set flag to TRUE if found
         END IF
 410  CONTINUE
!
! --- Match parameters from EXC-list
!
      IF ( N_EXC .GT. 0 ) THEN
           DO 420 J2=1,NCOM_PAR
              IL = LTM_DIF ( 3, N_EXC, EXCBUF, COMPAR_LIS(J2) )
              IF ( IL .GT. 0 ) THEN
                   FL_PAR(J2) = .FALSE. ! Set flag to FALSE if found
              END IF
 420       CONTINUE
      END IF
!
      IF ( CRES_STYLE .EQ. CRES__PRE98 ) THEN
           TITLE_ANON = .FALSE.
         ELSE
           TITLE_ANON = .TRUE.
      END IF
!
! --- Form the list of the parameters to be dealt with.
! --- NG_COR   -- number of global parameters eligible for computation
! ---             of correlations
! --- NL_COR   -- number of local parameters eligible for computation
! ---             of correlations
! --- INDL_COR -- array indeces of the local parameters in the parameter
! ---             list COMPAR_LIS
! --- INDL_MAT -- array indeces of the local parameters in the covariance
! ---             matrix LL_COVMAT
! --- INDG_COR -- array indeces of the global parameters in the parameter
! ---             list COMPAR_LIS
! --- INDG_MAT -- array indeces of the local parameters in the covariance
! ---             matrix GG_COVMAT (in FAST__B1B3D mode) or in
! ---                    LL_COVMAT (in FAST__NONE mode)
!
      NG_COR = 0
      NL_COR = 0
      DO 430 J3=1,NCOM_PAR
         IF ( FL_PAR(J3) ) THEN
              IF ( FAST_MODE .EQ. F__B1B3D  ) THEN
!
! ---------------- Search in the INF_GLO list
!
                   IP = IFIND_PL ( B3DOBJ%N_GLO, B3DOBJ%INF_GLO, J3 )
                   IF ( IP .GT. 0 ) THEN
                        NG_COR = NG_COR + 1
                        INDG_COR(NG_COR) = J3
                        INDG_MAT(NG_COR) = IP
                   END IF
!
! ---------------- Search in the INF_LOC list
!
                   IP = IFIND_PL ( B3DOBJ%N_LOC, B3DOBJ%INF_LOC, J3 )
                   IF ( IP .GT. 0 ) THEN
                        NL_COR = NL_COR + 1
                        INDL_COR(NL_COR) = J3
                        INDL_MAT(NL_COR) = IP
                   END IF
                ELSE IF ( FAST_MODE .EQ. F__NONE ) THEN
!
! ---------------- Non fast mode.
! ---------------- Search the J3-th parameter in the list of global paramters
!
                   IL = LTM_DIF ( 0, NGLO_PAR, GLOPAR_LIS, COMPAR_LIS(J3) )
                   IF ( IL .GT. 0 ) THEN
!
! --------------------- We have found. J3-th parameter is a global parameter
!
                        NG_COR = NG_COR + 1
                        INDG_COR(NG_COR) = J3
                        INDG_MAT(NG_COR) = LTM_DIF ( 0, NCOM_PAR, LGPAR_LIS, &
     &                                               COMPAR_LIS(J3) )
                      ELSE
!
! --------------------- We haven't found. It means that the J3-th parameter
! --------------------- is a local parameter
!
                        NL_COR = NL_COR + 1
                        INDL_COR(NL_COR) = J3
                        INDL_MAT(NL_COR) = LTM_DIF ( 0, NCOM_PAR, LGPAR_LIS, &
     &                                               COMPAR_LIS(J3) )
                   END IF
              END IF
         END IF
 430  CONTINUE
!
! --- Open the output file
!
      CALL CLRCH ( FILOUT )
      FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'CORL'//PRE_LETRS
      OPEN ( UNIT=11, FILE=FILOUT, STATUS='UNKNOWN', ACCESS='APPEND', &
     &       IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL ERR_LOG ( 8806, IUER, 'WRITE_GLCORR', 'Error in '// &
     &         'openning output correlation file '//FILOUT )
           RETURN
      END IF
!
! --- Check its length
!
      IS = FOR_STAT ( FILOUT, STAT_BLOCK )
      IF ( STAT_BLOCK(8) .EQ. 0 ) THEN
!
! -------- Length of the output CORL file is zero -- add a general header
!
           FL_GLOBAL_L1 = .TRUE.
           CALL SOLUTION_IDENT ( CRL__ASC, 11, '# ', FL_GLOBAL_L1, TITLE_ANON )
      END IF
!
! --- Add the specific header
!
      WRITE ( 11, '(A)' ) '# '
      WRITE ( 11, '(A)' ) '# Type:           GLO_LOC Correlations'
!
! --- Add more line in the specific header fo LOC_LOC correlations
!
      CALL OBSTM ( FJDOBS, LJDOBS )
      CALL CLRCH ( START_DATE )
      CALL CLRCH ( STOP_DATE  )
!
      WRITE ( UNIT=START_DATE(1:16), FMT='(F16.8)' ) FJDOBS
      WRITE ( UNIT=STOP_DATE(1:16),  FMT='(F16.8)' ) LJDOBS
      START_DATE(21:) = JD_TO_DATE ( FJDOBS, -3 )
      STOP_DATE(21:)  = JD_TO_DATE ( LJDOBS, -3 )
      WRITE ( 11, '(A)' ) '# Database:       '//B3DOBJ%DBNAME_MES
      WRITE ( 11, '(A)' ) '# Start_date:     '//START_DATE(1:43)
      WRITE ( 11, '(A)' ) '# Stop_date:      '//STOP_DATE(1:43)
      WRITE ( 11, '(A)' ) '# '
!
      IF ( NG_COR .GT. 1 .AND. NL_COR .GT. 1 ) THEN
!
! -------- Computation of correlations
!
           DO 440 J4=1,NG_COR
              IF ( FAST_MODE .EQ. F__B1B3D  ) THEN
                   SIG_SQ1 = GG_COVMAT( LOCC(INDG_MAT(J4),INDG_MAT(J4)) )
                 ELSE IF ( FAST_MODE .EQ. F__NONE ) THEN
                   SIG_SQ1 = LL_COVMAT( LOCC(INDG_MAT(J4),INDG_MAT(J4)) )
              END IF
!
              DO 450 J5=1,NL_COR
                 SIG_SQ2 = LL_COVMAT( LOCC(INDL_MAT(J5),INDL_MAT(J5)) )
!
! -------------- COR is a correlation coefficient
!
                 IF ( FAST_MODE .EQ. F__B1B3D  ) THEN
                      COR = LG_COVMAT( INDL_MAT(J5), INDG_MAT(J4) )/ &
     &                         DSQRT ( SIG_SQ1*SIG_SQ2 )
                    ELSE IF ( FAST_MODE .EQ. F__NONE ) THEN
                      COR = LL_COVMAT( LOCC(INDL_MAT(J5), INDG_MAT(J4)) )/ &
     &                         DSQRT ( SIG_SQ1*SIG_SQ2 )
                 END IF
!
! -------------- ... and print it
!
                 WRITE ( UNIT=11, FMT=110, IOSTAT=I11) &
     &                   INDG_COR(J4), INDL_COR(J5), &
     &                   COMPAR_LIS(INDG_COR(J4)), COMPAR_LIS(INDL_COR(J5)), COR
 110             FORMAT ( I5, 1X, I5, 2X, '"', A20, '"', &
     &                                2X, '"', A20, '"', 2X, F12.9 )
                 IF ( I11 .NE. 0 ) THEN
                      CLOSE ( UNIT=11 )
                      CALL ERR_LOG ( 8807, IUER, 'WRITE_GLCORR', &
     &                    'Error in writing output correlation file '//FILOUT )
                      RETURN
                 END IF
 450          CONTINUE
 440       CONTINUE
      END IF
      CLOSE ( UNIT=11 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_GLCORR  #!#
