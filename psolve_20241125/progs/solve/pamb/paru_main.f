      SUBROUTINE PARU_MAIN ( VER_PAMB, PAMB_VER, FL_BATCH, N_OBS, IDBF, IDB2, &
     &           ML_OBSER, MA_OBSER, &
     &           DBOBJ, NCREC, OBSHLD, OBSSCA, OBSSTA, OBSBAS, RES, PAMBI, &
     &           PLACE, B3DOBJ, B1B3DOBJ, RST, CHIOBJ, SCAINF, EQUMEM, &
     &           PARU_FIL, INC_VERS, GVH, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  PARU_MAIN   takes parameters of PARU-propgram           *
! *   (PARU = Phase Ambiguity Resolution Utility) in the mode of screen  *
! *   form, invokes PARU-compiler, compiles PARU-program and then        *
! *   executes PARU-program.                                             *
! *                                                                      *
! *     Phase delay residuals and ambiguity values are updated in fields *
! *   of PAMBI data structure. PARU_MAIN doesn't update ambiguity and    *
! *   elimination status of the observations in oborg are itself, but    *
! *   such update may be done in during execution of PARU-program.       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  VER_PAMB ( CHARACTER ) -- Label with version of PAMB.               *
! *  PAMB_VER ( INTEGER*4 ) -- Verbosity mode.                           *
! *  FL_BATCH ( LOGICAL*4 ) -- Flag of batch mode.                       *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *  ML_OBSER ( INTEGER*4 ) -- The length (in bytes) of the grabbed      *
! *                            dynamic memory.                           *
! *  MA_OBSER ( INTEGER*4 ) -- The first address of the grabbed memory.  *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *    OBSSCA ( RECORD    ) -- Data structure which keeps scan-dependent *
! *                            information about the session.            *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  INC_VERS ( INTEGER*4 ) -- Output version increment. It is 1 if      *
! *                            PARU updates the database with version    *
! *                            increment, and 0 otherwise.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *    PLACE  ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *       RST ( RECORD    ) -- Data structure keeping the statistics     *
! *                            of postfit residuals.                     *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and       *
! *                            their mathematical expectations.          *
! *    SCAINF ( RECORD    ) -- Data structure which keeps a) values of   *
! *                            parameters which control work of algorithm*
! *                            SCADAM; b) result of work of algorithm    *
! *                            SCADAM. Values  of ARF will be written in *
! *                            this data structure.                      *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
! *  PARU_FIL ( CHARACTER ) -- File name of the PARU-program.            *
! *       GVH ( GVH__STRU ) -- Object with the contents of the database  *
! *                            in GVF format.                            *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  16-MAR-98    PARU_MAIN   v1.3  (c)  L. Petrov  24-JUL-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'fast.i'
      INCLUDE   'pamb.i'
      INCLUDE   'gvh.i'
      INCLUDE   'equmem.i'
      INTEGER*4  ML_OBSER, MA_OBSER, IDBF, N_OBS, PAMB_VER, INC_VERS, IUER
      INTEGER*4  IDB2
      TYPE ( NCREC__STRU ) ::  NCREC
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( STA_O__STRU ) ::  OBSSTA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      TYPE ( PAMBI__STRU ) ::  PAMBI(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      TYPE ( PAR__STRU ) ::  PAR
      TYPE ( SCAINF__STRU ) ::  SCAINF
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      TYPE ( GVH__STRU    ) ::  GVH
!
      CHARACTER  VER_PAMB*(*), PARU_FIL*(*), PARU_DIR*255, &
     &           PARU_PRG(PARU_MPRG)*255, PARU_SCRATCH*255, WORK_DIR*255, &
     &           STR*255
      LOGICAL*4  F_QUIT, FL_BATCH
      INTEGER*4  J1, IER
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN
!
! --- Get name for the directory where PARU programms assumed to reside.
!
      CALL CLRCH  ( PARU_DIR )
      CALL GETENVAR ( 'PARU_DIR', PARU_DIR )
      IF ( ILEN(PARU_DIR) .GT. 0 ) THEN
           IF ( PARU_DIR(ILEN(PARU_DIR):ILEN(PARU_DIR)) .NE. '/' ) THEN
                PARU_DIR(ILEN(PARU_DIR)+1:) = '/'
           END IF
      END IF
!
! --- Get name for working files
!
      CALL CLRCH  ( PARU_SCRATCH )
      CALL CLRCH  ( WORK_DIR     )
      WORK_DIR = SOLVE_WORK_DIR
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', STR )
!
! --- Creatrion of the file name for PARU-file in scratch directory
!
      IF ( ILEN(STR) .GT. 0 ) WORK_DIR = STR
      IF ( STR(I_LEN(STR):I_LEN(STR)) .EQ. '/' ) THEN
           PARU_SCRATCH = STR(1:I_LEN(STR))//'PARU'
        ELSE
           PARU_SCRATCH = STR(1:I_LEN(STR))//'/PARU'
      END IF
!
! --- Getting file name of default PARU-programms
!
      DO 410 J1=1,PARU_MPRG
         CALL CLRCH ( PARU_PRG(J1) )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
         STR = 'PARU_PRG_'//STR
         CALL GETENVAR ( STR, PARU_PRG(J1) )
 410  CONTINUE
!
! --- Getting name of PARU-prgramms and parameters of compilation and execution
! --- of PARU program
!
      IF ( FL_BATCH ) THEN
           F_QUIT = .FALSE.
         ELSE 
           CALL PARU_MENU ( VER_PAMB, DBOBJ%NAME, PARU_DIR, PARU_PRG, &
     &                      PARU_SCRATCH, PARU_FIL, PAMB_VER, F_QUIT )
      END IF
!
      IF ( F_QUIT ) THEN
!
! -------- User desided to quit... Good bye!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE (  6, '(A)' ) ' PAR program '//PARU_FIL(1:I_LEN(PARU_FIL))// &
     &            ' is being compiled'
      END IF
!
! --- Compilation of PARU-programm
!
      CALL ERR_PASS ( IUER, IER )
      CALL PARU_COMPILE ( PARU_FIL, PAR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6901, IUER, 'PARU_MAIN', 'Error of compilation '// &
     &         'from PARU language of '//PARU_FIL )
           RETURN
      END IF
!
      IF ( PAMB_VER .GE. 3 ) THEN
!
! -------- Printing dump of object code of PAR program
!
           CALL PARU_DUMP ( PAR )
      END IF
!
      IF ( PAMB_VER .GE. 1 ) THEN
           IF ( .NOT. FL_BATCH ) WRITE (  6, '(A)' ) ' ___________ '
           WRITE (  6, '(A)' ) ' PAR program '//PARU_FIL(1:I_LEN(PARU_FIL))// &
     &            ' is being executed'
      END IF
!
! --- Execution of PARU-programm
!
      CALL ERR_PASS  ( IUER, IER )
      CALL PARU_EXEC ( PAR, PAMB_VER, N_OBS, IDBF, IDB2, ML_OBSER, MA_OBSER, &
     &                 DBOBJ, NCREC, OBSHLD, OBSSCA, OBSSTA, OBSBAS, RES, &
     &                 PAMBI, PLACE, B3DOBJ, B1B3DOBJ, RST, CHIOBJ, SCAINF, &
     &                 EQUMEM, GVH, INC_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6902, IUER, 'PARU_MAIN', 'Error of execution of '// &
     &         'PARU program '//PARU_FIL )
           RETURN
      END IF
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE (  6, '(A)' ) ' PAR program '//PAR%FINAM(1:I_LEN(PAR%FINAM))// &
     &            ' finished successfully'
           IF ( .NOT. FL_BATCH ) THEN
                 WRITE (  6, '(A)' ) ' 같같같같같� '
                 CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PARU_MAIN  #!#
