      SUBROUTINE BACK_MAIN ( M3, MODE, N_ARR1, ARR1, ARR2, B3DOBJ, &
     &                       B1B3DOBJ, IRNSV, IX1T3, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  BACK_MAIN PROGRAM SPECIFICATION
!
! 1.1 Do a back solution.
!
! 1.2 REFERENCES:
!
! 2.  BACK_MAIN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'prfil.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'precm.i'
      INCLUDE 'baccm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: back
!       CALLED SUBROUTINES: pmcmb, manip,mxcmb,cvrnc
!
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   IPARM1(10,M_GPA), IPARM2(10,M_GPA), IPARM3(10,M_GPA)
      COMMON     / PARAM / IPARM1, IPARM2, IPARM3
      CHARACTER   LPARM1(M_GPA)*20, LPARM2(M_GPA)*20, LPARM3(M_GPA)*20
      EQUIVALENCE (IPARM1,LPARM1), (IPARM2,LPARM2), (IPARM3,LPARM3)
      CHARACTER    SAVNAM*(NAME_SIZE)
      COMMON     / NAMARC / SAVNAM
!
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*8  M3
      INTEGER*4  MODE, N_ARR1, IUER
      INTEGER*2  IRNSV(2), IX1T3(*)
      CHARACTER  STR*16, STR1*16, FINAM_FAST*40
      INTEGER*2  NUMDB, LDBNAM(5,15), IDBV(15), k1
      INTEGER*4  IDBE(15)
      INTEGER*4  JS, JSIG, JB, JA, IER, MEM_STAT_OLD
      LOGICAL*2  KBIT
      REAL*8     ARR1(M3,2), ARR2(M3)
      INTEGER*4  IVER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Comments about memory:
! --- 1) ARR1(*,2)  and  ARR2 are equivalent in non-FAST mode
! --- 2) ARR1(*,2)  is not defined if MODE=2,3 and fast_mode = F_B1B3D
!
!CCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940201  Implement dynamic memory allocation for large matrices
!   mwh  970218  Added stuff for B1D   algorithm
!   pet  970228  Added stuff for B1B3D algorithm
!   pet  970304  Added writing FAST file in B1B3D mode
!   pet  970307  Added FAST_BYPASS
!   pet  980313  Added writing down covariance matrices in debug mode
!   pet  980710  Changed the place where arc file is read in fast mode --
!                moved reading from mxcmb to back. Added formal parameters:
!                B3DOBJ, B1B3DOBJ
!   pet  980730  Added a trap of internal control for handling the case when
!                wrong arc-file has been read.
!   pet  990305  Heaviliy rewrote. Added support of NO TRAIN mode. Added
!                support of universal error handler IUER
!   pet  990406  Added call of TIM_GET
!   pet  990418  Changed a bit error message 8521
!   pet  1999.10.11  Added support of computation of GLO_LOC and LOC_LOC
!                    correlation by using post OCT99 scheme.
!   pet  2001.05.04  Fixed a bug: list IPARM3 has wrong dimension
!
!CCCCC
!
! 5.  BACK_MAIN PROGRAM STRUCTURE
!
!
      CALL CLRCH ( FINAM_FAST )
      FINAM_FAST = PRE_SCR_DIR(1:PRE_SD_LEN)//'FAST'//PRE_LETRS
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
!
! --- Formatting the string with database name
!
      CALL CLRCH ( STR )
      CALL LIB$MOVC3 ( 10, LDBNAM, STR  )
      IVER = INT4 ( IDBV(1) )
      STR(12:) = '<'
      CALL INCH ( IVER, STR(13:) )
      STR( I_LEN(STR)+1: ) = '>'
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, 210 )  str(1:16), fast_mode, fast_dbg
 210       format ( 1X,' BACK:   session ',a,'  fast_mode=',i4, &
     &                   ' fast_dbg=',i4 )
      END IF
      IF ( ( MODE .EQ. 1  .OR.  MODE .EQ. 2 )  .AND.FAST_MODE .EQ. &
     &     F__B1B3D                              ) THEN
!
! -------- Check of consistency of the arc-file name
!
           IF ( STR(1:16) .NE. B3DOBJ%DBNAME_MES ) THEN
                CALL CLRCH ( STR1 )
                CALL INCH  ( IARCRC, STR1 )
                CALL ERR_LOG ( 8521, IUER, 'BACK_MAIN', 'Trap '// &
     &              'of internal control. Database name to be processed: '//STR// &
     &              ' and the database name: '//B3DOBJ%DBNAME_MES// &
     &              'read from arc-file '//SAVNAM(1:I_LEN(SAVNAM))// &
     &              ' are not the same. This may have happened since user '// &
     &              'modified control file before recovering and new '// &
     &              'version of the arc file become shorter. Alas! SOLVE is '// &
     &              'not perfect yet. What''s to do? First of all, don''t '// &
     &              'hang your nose! You can try to change control file '// &
     &              'back. Solve saved position in control file with the '// &
     &              'last processed session: '//STR1(1:I_LEN(STR1))// &
     &              ' You can add comments-lines to fill control file in '// &
     &              'order to match the database name in the '// &
     &               STR1(1:I_LEN(STR1))//'-th line with the arc-file '//SAVNAM(1:I_LEN(SAVNAM)) )
               RETURN
           END IF
         ELSE IF ( FAST_MODE .NE. F__B1B3D ) THEN
           B3DOBJ%DBNAME_MES = STR(1:16)
      END IF
!
! --- Putting in the array ARR submatrices which we will need further.
! --- arc-file is read in non-fast modes when MODE = 1,2
!
      CALL MXCMB ( MODE, N_ARR1, IARCS, IGLBLS, ARR1, ARR2, B3DOBJ, B1B3DOBJ )
!
      JS   = 1
      JSIG = 1 +   M_GPA
      JB   = 1 + 2*M_GPA
      JA   = 1 + 3*M_GPA
!
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD  ) THEN
!
! -------- Calculation of adjustments for local parameters and their covariance
! -------- matrix for FULL case
!
           CALL MANIP ( ARR1(JA,1), ARR1(JB,1) )
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'BACK-02' )
                CALL TIM_INIT()
           END IF
         ELSE IF ( FAST_MODE .EQ. F__B1D ) THEN
!
! -------- Calculation of adjustments for local parameters and their covariance
! -------- matrix for B1D case
!
           CALL BACK_B1D ( IARCS, IGLBLS, JA, JB, M3, ARR1 )
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'BACK-02' )
                CALL TIM_INIT()
           END IF
         ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Calculation of adjustments for local and segmented parameters and
! -------- their covariance matrices for B1B3D case
!
           CALL ERR_PASS   ( IUER, IER )
           CALL BACK_B1B3D ( FAST_COV, FAST_DBG, B3DOBJ, B1B3DOBJ, ARR1(JA,1), &
     &                       ARR1(JB,1), ARR1(JSIG,1), ARR1(JS,1), IER )
!
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8522, IUER, 'BACK_MAIN', &
     &              'Error during calculation estimates and covariance '// &
     &              'matrix of local and segmented parameters of the '// &
     &              'session '//b3dobj%dbname_mes//' Arc-file was read '// &
     &              'from the file '//SAVAF )
                RETURN
           END IF
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'BACK-02' )
                CALL TIM_INIT()
           END IF
           IF ( TRAIN ) THEN
!
! ------------- Write down the results: vector of estimats, their standard
! ------------- deviations and covariance matrix. (We don't need do it in
! ------------- NO TRAIN mode ;-) )
!
                CALL USE_NRMFIL ( ARR1, NPARM1, 'OWC' )
           END IF
!
! -------- Special trick: We set memory status to the value "undefined".
! -------- It prevents writing normal vectors and normal matrices on disk
!
           MEM_STAT_OLD = B3DOBJ%MEM_STAT
           B3DOBJ%MEM_STAT = F__UND
!
! -------- Rewrite some fields of B3DOBJ in fast-file. It is done for CRES
! -------- since B3DOBJ contains corrected theoreticals and they are
! -------- necessary for calculations of residuals
!
           CALL ERR_PASS  ( IUER, IER )
           CALL WRNOR_B3D ( FINAM_FAST, B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8523, IUER, 'BACK_MAIN', &
     &              'Error during writing file '// &
     &               FINAM_FAST(1:I_LEN(FINAM_FAST))//' while session '// &
     &               B3DOBJ%DBNAME_MES//' was processing' )
                RETURN
           END IF
!
! -------- Restore memory status
!
           B3DOBJ%MEM_STAT = MEM_STAT_OLD
      END IF
!
      IF ( CORLN  .OR.  I_ARCNAME(1:3) .EQ. 'CGM' ) THEN
!
! -------- Calculate the correlations in old mode  ( pre OCT99 ) if thats
! -------- what we're supposed to do before we rearrange the matrix
! -------- back into PROC order
!
           CALL USE_PARFIL ( 'ORC'      )
           CALL CVRNC      ( ARR1, ARR2 )
      ENDIF
!
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD  .OR. &
     &     FAST_MODE .EQ. F__B1D                                      )  THEN
!
! -------- Now we have to change order of parameters. Now the order is the
! -------- following:
! -------- first:  local paramaters of the session
! -------- then:   global parameters supplied by this session
! -------- CRES and ADJST expects to see parameters in PROC order
!
! -------- Re-arrange the matrix now so that SOLVE will find what it
! -------- expects (But it expects to see matrix in PROC order).
! -------- We put local-local covariance matrix at the end of ARR1 (we keep in
! -------- mind that we sized ARR1 TWICE more). The results go to ARR2
!
           CALL AMATX  ( ARR1(JA,2), ARR1(JA,1), &
     &                   ARR1(JB,2), ARR1(JB,1), &
     &                   ARR1(JS,2), ARR1(JS,1), IX3T1, NPARM3 )
!
! -------- Calculate variances of the adjustments using covariance matrix
!
           CALL SIGMAS ( ARR1(JA,2), ARR1(JSIG,2), NPARM1 )
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'BACK-02' )
                CALL TIM_INIT()
           END IF
!
           IF ( TRAIN ) THEN
!
! ------------- Write down adjustments and covariance matrix. (We don't need
! ------------- do it in NO TRAIN mode ;-) )
!
                CALL USE_NRMFIL ( ARR1(JS,2), NPARM1, 'OWC' )
           END IF
!
           IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! ------------- DEBUG-mode: writing down covariance matricx and vectors of the
! ------------- estimatrs
!
                ier = -1
                call matview_w ( '/tmp/cov_full.mat', 3, nparm1, &
     &               nparm1, arr2(ja),'Full covariance matrix', '()', &
     &               1, 1, ier )
                ier = -1
                call matview_w ( '/tmp/cov_full.vec', 1, nparm1, 1, &
     &               arr2(jb), 'Full vector '//'of estimates', '()', 1, 1, ier )
           END IF
      END IF
!
      IF ( COR_LL_FLAG ) THEN
!
! -------- Computation of local-local correlations
!
           CALL ERR_PASS ( IUER, IER )
           IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                CALL WRITE_DIACORR ( 'LOC_LOC', B3DOBJ, COR_LL_INCFIL, &
     &               COR_LL_EXCFIL, NPARM1, %VAL(B1B3DOBJ%AD_BI0), &
     &               LPARM1, IER )
              ELSE IF ( FAST_MODE .EQ. F__NONE ) THEN
                CALL WRITE_DIACORR ( 'LOC_LOC', B3DOBJ, COR_LL_INCFIL, &
     &               COR_LL_EXCFIL, NPARM1, ARR1(JA,2), LPARM1, IER )
           END IF
!
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8524, IUER, 'BACK_MAIN', 'Error in '// &
     &              'computation of LOC_LOC correlations while session '// &
     &               B3DOBJ%DBNAME_MES//' was processing' )
                RETURN
           END IF
      END IF
!
      IF ( COR_GL_FLAG ) THEN
!
! -------- Computation of global-local correlations
!
           CALL ERR_PASS ( IUER, IER )
           IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                CALL WRITE_GLCORR ( B3DOBJ, COR_GL_INCFIL, COR_GL_EXCFIL, &
     &               NPARM1, NPARM2, LPARM1, LPARM2, LPARM3, &
     &               %VAL(B1B3DOBJ%AD_W00), %VAL(B1B3DOBJ%AD_WI0), &
     &               %VAL(B1B3DOBJ%AD_BI0), IER )
              ELSE IF ( FAST_MODE .EQ. F__NONE ) THEN
                CALL WRITE_GLCORR ( B3DOBJ, COR_GL_INCFIL, COR_GL_EXCFIL, &
     &               NPARM1, NPARM2, LPARM1, LPARM2, LPARM3, &
     &               %VAL(0), %VAL(0), ARR1(JA,1), IER )
           END IF
!
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8525, IUER, 'BACK_MAIN', 'Error in '// &
     &              'computation of GLO_LOC correlations while session '// &
     &               B3DOBJ%DBNAME_MES//' was processing' )
                RETURN
           END IF
      END IF
!
      CALL USE_COMMON ( 'ORC' )
!
! --- Setting value of the solution run code
!
      IRNCD(1)=IRNSV(1)
      IRNCD(2)=IRNSV(2)
      CALL USE_COMMON ( 'OWC' )
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) CALL END_MN
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! -------- Debugging printout
!
           if ( fast_mode .eq. f__b1b3d ) WRITE ( 6, * ) ' g =', b3dobj%n_glo, &
     &         ' l=',b3dobj%n_loc,' sg =',(b3dobj%nbs-1)*b3dobj%sb +b3dobj%sx, &
     &         ' rcond = ',rcond
      END IF
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'BACK-03' )
           CALL TIM_INIT()
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  BACK_MAIN  #!#
