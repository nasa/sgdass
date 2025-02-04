      SUBROUTINE PARU_EXEC ( PAR, PAMB_VER, N_OBS, IDBF, IDB2, &
     &           ML_OBSER, MA_OBSER, &
     &           DBOBJ, NCREC, OBSHLD, OBSSCA, OBSSTA, OBSBAS, RES, PAMBI, &
     &           PLACE, B3DOBJ, B1B3DOBJ, RST, CHIOBJ, SCAINF, EQUMEM, &
     &           GVH, INC_VERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARU_EXEC  executes PARU-program. PARU-programms is the   *
! *   stream of procedures with parameters for phase VLBI delay          *
! *   ambiguity resolution. PARU_EXEC doesn't store solution itself --   *
! *   PARU procedures should do it directly.                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  PAMB_VER ( INTEGER*4 ) -- Verbosity mode for PARU-executor (NB: not *
! *                            for procedures called by PARU-executor!)  *
! *       PAR ( RECORD    ) -- Data structure which contains "object     *
! *                              code" of PARU-program: stream of        *
! *                              functions to be executed and their      *
! *                              parameters record.                      *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session with quality code more than 0     *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  INC_VERS ( INTEGER*4 ) -- Output version increment. It is 1 if      *
! *                            PARU updates the database with version    *
! *                            increment, and 0 otherwise.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  ML_OBSER ( INTEGER*4 ) -- The lentgh (in bytes) of the grabbed      *
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
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs     *
! *                            of postfit residuals.                     *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and       *
! *                            their mathematical expectations.          *
! *       GVH ( GVH__STRU ) -- Object with the contents of the database  *
! *                            in GVF format.                            *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  18-MAR-1998   PARU_EXEC   v2.4  (c) L. Petrov 11-JUN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'socom.i'
      INCLUDE   'precm.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'fast.i'
      INCLUDE   'obser.i'
      INCLUDE   'oborg.i'
      INCLUDE   'pamb.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      INCLUDE   'equmem.i'
      INTEGER*4  N_OBS, PAMB_VER, IDBF, ML_OBSER, MA_OBSER, INC_VERS, IUER
      INTEGER*2  IDB2
      TYPE ( PAR__STRU     ) ::  PAR
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( NCREC__STRU   ) ::  NCREC
      TYPE ( HLD_O__STRU   ) ::  OBSHLD
      TYPE ( SCA_O__STRU   ) ::  OBSSCA(*)
      TYPE ( STA_O__STRU   ) ::  OBSSTA(*)
      TYPE ( BAS_O__STRU   ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU   ) ::  RES(N_OBS)
      TYPE ( PAMBI__STRU   ) ::  PAMBI(N_OBS)
      TYPE ( PLACE__STRU   ) ::  PLACE
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( RST_O__STRU   ) ::  RST
      TYPE ( CHIACC__STRU  ) ::  CHIOBJ
      TYPE ( SCAINF__STRU  ) ::  SCAINF
      TYPE ( EQUMEM__STRU  ) ::  EQUMEM
      TYPE ( GVH__STRU     ) ::  GVH
      TYPE ( VCAT__TYPE    ) ::  VCAT
!
      INTEGER*4  IFUN, J1, J3, J4, J5, J6, J7, J8, J9, ILV, IACT_FREEZE, &
     &           IVRB, IER
      INTEGER*4  IPAR1, IPAR2, IPAR3, IPAR7
      LOGICAL*4  LPAR1, LPAR2, LPAR3, LPAR4
      REAL*8                   RPAR3, RPAR4, RPAR5, RPAR6
      INTEGER*2  IDATYP_NEW, IDATYP_WEI, I2
      CHARACTER  STR*128, STR1*32, DBNAME*16, VALUE*32, OUT*384, &
     &           ELIM_TYP__SAVE*2
      CHARACTER  VTD_CONF_FILE*128
      LOGICAL*4  ELIM_MOD__SAVE, ELIM_AMB__SAVE, ELIM_ION__SAVE, ELIM_CNF__SAVE
      LOGICAL*4  PSTA, FSTA
      REAL*8     ELIM_CUT__SAVE, ELIM_THR__SAVE, ELIM_MSR__SAVE, MARGIN, &
     &           INIT_WEI, WEIGR_BAS_SAVE(MO_BAS), WEIPH_BAS_SAVE(MO_BAS)
      INTEGER*4  QUALCODE_GOOD_LIM__SAVE, ELIM_UPD__SAVE, ELIM_VRB__SAVE
      INTEGER*4  IOBS_USED, IOBS_SPAC, KAMB, OPCODE, IP, IPN, IPB, IPE, N_AMB
      LOGICAL*4  FL_SUPR 
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LTM_DIF
!
! --- Initialization
!
      PSTA = .FALSE.
      FSTA = .FALSE.
!
! --- Check of validity of PARU object code
!
      IF ( PAR%STATUS .NE. PAR__COMPLETE ) THEN
           CALL ERR_LOG ( 6961, IUER, 'PARU_EXEC', 'Compilation PAR program '// &
     &         'was no successful' )
           RETURN
      END IF
      IF ( PAR%N_PRC .EQ. 0 ) THEN
           CALL ERR_LOG ( 6962, IUER, 'PARU_EXEC', 'No one procedure '// &
     &         'was found in PAR program '//PAR%FINAM )
           RETURN
      END IF
!
      CALL CLRCH ( DBNAME )
      DBNAME = DBOBJ%NAME
!
! --- Cycle on PARU-procedures
!
      DO 410 J1=1,PAR%N_PRC
!
! ------ Getting function name
!
         IFUN = PAR%IFUN(J1)
         CALL CLRCH ( STR1 )
         CALL INCH  ( J1, STR1 )
!
         IF ( PAMB_VER .GE. 1 ) THEN
              WRITE (  6, * ) 'PAR: '//STR1(1:I_LEN(STR1))//'-th '// &
     &                        'procedure is being executed'
              WRITE ( 23, * ) 'PAR: '//STR1(1:I_LEN(STR1))//'-th '// &
     &                        'procedure is being executed'
         END IF
!
! ------ Calling appropriate routines for phase ambiguity resolution
!
         IF ( PAR_FUN(IFUN)         .EQ. 'INITIALIZE  ' ) THEN
!
! =========== INITIALIZE
!             ~~~~~~~~~~
!
              IF ( PAR%CKWD(1,J1)(1:3) .EQ. 'YES' ) THEN
                   LPAR1 = .TRUE.
                 ELSE
                   LPAR1 = .FALSE.
              END IF
!
              IF ( PAR%CKWD(2,J1)(1:3) .EQ. 'YES' ) THEN
                   LPAR2 = .TRUE.
                 ELSE
                   LPAR2 = .FALSE.
              END IF
!
              IF ( PAR%CKWD(3,J1)(1:3) .EQ. 'YES' ) THEN
                   LPAR3 = .TRUE.
                 ELSE
                   LPAR3 = .FALSE.
              END IF
              RPAR4 = PAR%RKWD(4,J1)*1.D-12
!
! ----------- Update of oborg area for the additional obserbles for S-band
!
              CALL ERR_PASS   ( IUER, IER )
              CALL PAMB_INIT  ( IDB2, DBNAME, DBOBJ, OBSHLD, OBSBAS, CHIOBJ, &
     &                          PAMBI, LPAR1, LPAR2, LPAR3, RPAR4, IOBS_USED, &
     &                          IOBS_SPAC, IER )
              IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6963, IUER, 'PARU_EXEC', 'Error during '// &
     &                'attempt to initialize phase delays and their '// &
     &                'ambiguities while database '//DBNAME(1:I_LEN(DBNAME))// &
     &                ' was being processed' )
                  RETURN
              END IF
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Phase delays have been initialized'
                   WRITE (  6, '(A,I5,A)' ) '      ',IOBS_USED, &
     &                                      ' observations are in use'
                   WRITE ( 23, * ) '     Phase delays have been initialized'
                   WRITE ( 23, '(A,I5,A)' ) '      ',IOBS_USED, &
     &                                      ' observations are in use'
                   IF ( IOBS_SPAC .EQ. 0 ) THEN
                        WRITE (  6, '(A)' ) '     No phase delays were rejected'
                        WRITE ( 23, '(A)' ) '     No phase delays were rejected'
                     ELSE
                        WRITE (  6, '(I5,A)' ) IOBS_SPAC, ' phase delays '// &
     &                       'were rejected (inconsistent ambiguity spacing)'
                        WRITE ( 23, '(I5,A)' ) IOBS_SPAC, ' phase delays '// &
     &                       'were rejected (inconsistent ambiguity spacing)'
                   END IF
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'SET_SOLTYP  ' ) THEN
!
! =========== SET_SOLTYP
!             ~~~~~~~~~~
!
               VALUE = PAR%CKWD(1,J1)
               ILV = I_LEN ( VALUE )
               IDATYP_NEW = -999
               DO 420 I2=FIRST__DTP,LAST__DTP
                  OUT = DATYP__ABR(1+I2*6:6+I2*6)
                  IF ( OUT(1:ILV) .EQ. VALUE(1:ILV) ) THEN
                       IDATYP_NEW = I2
                  END IF
 420           CONTINUE
               IF ( IDATYP_NEW .EQ. -999 ) THEN
                    CALL ERR_LOG ( 6964, IUER, 'PARU_EXEC', 'Error of '// &
     &                  'internal control: VALUE(1:ILV) >>'// &
     &                   VALUE(1:ILV)//'<<' )
                    RETURN
               END IF
!
               IF ( IDATYP_NEW .EQ. IDATYP ) THEN
                    WRITE (  6, * ) '     Solution type remained unchanged'
                    WRITE ( 23, * ) '     Solution type remained unchanged'
                  ELSE
                    IF ( SUPMET == SUPMET__META  .AND.  &
     &                   PAR%CKWD(2,J1)(1:3) ==  'YES'  ) THEN
!
                         CALL ACS_OBSFIL ( 'O' )
                         DO 430 J3=1,NUMOBS
                            CALL USE_OBSFIL ( IOBSFIL, J3, 'R' )
                            FL_SUPR = BTEST ( USER_SUP, INT4(IDATYP) )
                            IF ( FL_SUPR ) THEN
                                 USER_SUP = IBSET ( USER_SUP, INT4(IDATYP_NEW) )
                              ELSE
                                 USER_SUP = IBCLR ( USER_SUP, INT4(IDATYP_NEW) )
                            END IF
                            CALL USE_OBSFIL ( IOBSFIL, J3, 'W' )
 430                     CONTINUE
                         CALL ACS_OBSFIL ( 'C' )
!
                         WRITE (  6, * ) '     Suppression status has been transferred'
                         WRITE ( 23, * ) '     Suppression status has been transferred'
                  END IF
!
                  IDATYP = IDATYP_NEW
!
! --------------- Switching mode of calculation of the covariance matrix
! --------------- to "FULL"
!
                  FAST_COV = F__FUL
                  CALL USE_GLBFIL_4 ( 'OWC' )
!
! --------------- Solution type was changed. Write down SOCOM.
!
                  CALL USE_COMMON ( 'OWC' )
                  CALL PARCN()
!
                  WRITE (  6, * ) '     New solution type: "'//VALUE(1:ILV)// &
     &                            '" was set'
                  WRITE ( 23, * ) '     New solution type: "'//VALUE(1:ILV)// &
     &                            '" was set'
                  WRITE (  6, FMT='(A)' ) '      Database '//DBNAME// &
     &                                    ' is being read'
                  WRITE ( 23, FMT='(A)' ) '      Database '//DBNAME// &
     &                                    ' is being read'
!
! --------------- Scanning the observations, and calculation some statistics
! --------------- of the sessions, building the lists of the objects.
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL DB_SCAN  ( DBNAME, IDB2, IDBF, N_OBS, DBOBJ, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 6965, IUER, 'PARU_EXEC', 'Error during '// &
     &                     'initialization of data structure detected while '// &
     &                     'database '//DBNAME//' was processing' )
                      RETURN
                  END IF
                  IF ( DBOBJ%L_SCA .LE. 2 ) THEN
                       CALL ERR_LOG ( 6966, IUER, 'PARU_EXEC', 'Trap of internal '// &
     &                     'control: too few scans were found while '// &
     &                     'database '//DBNAME//' was processing' )
                       RETURN 
                  END IF
                  IF ( DBOBJ%L_STA .LT. 2 ) THEN
                       WRITE ( 6, * ) ' DBOBJ%L_STA = ', DBOBJ%L_STA
                       CALL ERR_LOG ( 6967, IUER, 'PARU_EXEC', 'Trap of '// &
     &                      'internal control: too few stations were found '// &
     &                      'while database '//DBNAME//' was processing' )
                       RETURN 
                  END IF
!
                  WRITE (  6, FMT='(A)'  ) '      Database '//DBNAME// &
     &                                     ' has been read'
                  WRITE ( 23, FMT='(A)'  ) '      Database '//DBNAME// &
     &                                     ' has been read'
                  WRITE (  6, '(A,I5,A)' ) '      ',DBOBJ%U_OBS, &
     &                                     ' observations are in use'
                  WRITE ( 23, '(A,I5,A)' ) '      ',DBOBJ%U_OBS, &
     &                                     ' observations are in use'
!
! --------------- Making initial LSQ solution. Estimates and FULL covariance
! --------------- matrix are calculaterd. Postfit residuals and their statistics
! --------------- are also calculated and stored in temporary data structures
!
                  ELIM_VRB__SAVE = ELIM_VRB
                  ELIM_VRB = PAMB_VER
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL SOL_INIT ( ELIM_VRB, 0, 1, N_OBS, DBOBJ%L_STA, &
     &                            DBOBJ%L_SCA, IDB2, IDBF, PLACE, B3DOBJ, &
     &                            B1B3DOBJ, OBSHLD, DBOBJ, NCREC, OBSSCA, &
     &                            OBSSTA, OBSBAS, RES, RST, CHIOBJ, EQUMEM, &
     &                            IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 6968, IUER, 'PARU_EXEC', 'Error during '// &
     &                     'attempt to obtain initial solution while '// &
     &                     'database '//DBNAME//' was processing' )
                       RETURN
                  END IF
                  ELIM_VRB = ELIM_VRB__SAVE
!
! --------------- Printing status message
!
                  CALL DATYP_SHOW ( IDATYP, OUT )
                  IF ( PAMB_VER .GE. 1 ) THEN
                       WRITE (  6, FMT='(A)' )'     Solution "'// &
     &                          OUT(1:I_LEN(OUT))//'" was updated'
                       WRITE ( 23, FMT='(A)' )'     Solution "'// &
     &                          OUT(1:I_LEN(OUT))//'" was updated'
                  END IF
               END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'CLOPA       ' ) THEN
!
! =========== CLOPA
!             ~~~~~
!
! ----------- Elimination permanent phase delay ambiguity for S-band using
! ----------- CLOPA algorithm
!
              IPAR1 = -1
              ILV = I_LEN( PAR%CKWD(1,J1) )
              CALL TRAN ( 11, BAND_STR(1)(1:ILV), STR )
              IF ( PAR%CKWD(1,J1)(1:ILV) .EQ. STR(1:ILV) ) IPAR1 = 1
              CALL TRAN ( 11, BAND_STR(2)(1:ILV), STR )
              IF ( PAR%CKWD(1,J1)(1:ILV) .EQ. STR(1:ILV) ) IPAR1 = 2
!
              IPAR2 = PAR%IKWD(2,J1)
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Permanent ambiguity elimination '// &
     &                             'algorithm CLOPA is running for '// &
     &                             BAND_STR(IPAR1)
                   WRITE ( 23, '(A)' ) '     Permanent ambiguity elimination '// &
     &                             'algorithm CLOPA is running for '// &
     &                             BAND_STR(IPAR1)
              END IF
!
! ----------- Elimination of permanent ambiguity
!
              CALL ERR_PASS  ( IUER, IER )
              CALL CLOPA     ( DBOBJ%L_OBS, DBOBJ, OBSBAS, RES, PAMBI, &
     &                         IPAR1, IPAR2, KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6969, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'attempt to apply phase delay ambiguity resolution ' // &
     &                 'algorithm CLOPA for X-band while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Permanent ambiguity elimination '// &
     &                             'algorithm CLOPA finished for '// &
     &                              BAND_STR(IPAR1)
                   WRITE ( 23, '(A)' ) '     Permanent ambiguity elimination '// &
     &                             'algorithm CLOPA finished for '// &
     &                              BAND_STR(IPAR1)
                   WRITE (  6, * ) '    ', KAMB, ' ambiguities were changed'
                   WRITE ( 23, * ) '    ', KAMB, ' ambiguities were changed'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'MIXOB       ' ) THEN
!
! =========== MIXOB
!             ~~~~~
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Ambiguity resolution algorithm '// &
     &                             'MIXOB is running'
                   WRITE ( 23, * ) '     Ambiguity resolution algorithm '// &
     &                             'MIXOB is running'
              END IF
!
! ----------- Making phase delay residuals and resolving phase delay ambiguities
! ----------- using MIXOB algorithm
!
              CALL ERR_PASS   ( IUER, IER )
              CALL PAMB_MARES ( 1, N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, %VAL(0), &
     &                          PAMBI, KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6970, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'making phase delay residuals and resolving '// &
     &                 'ambiguities using MIXOB algorithm while database '// &
     &                  DBNAME//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Ambiguity resolution algorithm '// &
     &                             'MIXOB finished'
                   WRITE ( 23, * ) '     Ambiguity resolution algorithm '// &
     &                             'MIXOB finished'
                   WRITE (  6, * ) '    ', KAMB, ' ambiguities were changed'
                   WRITE ( 23, * ) '    ', KAMB, ' ambiguities were changed'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'OSCRO       ' ) THEN
!
! =========== OSCRO
!             ~~~~~
!
              IPAR1 = -1
!
              ILV = I_LEN( PAR%CKWD(1,J1) )
              CALL TRAN ( 11, BAND_STR(1)(1:ILV), STR )
              IF ( PAR%CKWD(1,J1)(1:ILV) .EQ. STR(1:ILV) ) IPAR1 = 1
              CALL TRAN ( 11, BAND_STR(2)(1:ILV), STR )
              IF ( PAR%CKWD(1,J1)(1:ILV) .EQ. STR(1:ILV) ) IPAR1 = 2
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Ambiguity resolution algorithm '// &
     &                             'OSCRO is running for '//BAND_STR(IPAR1)
                   WRITE ( 23, '(A)' ) '     Ambiguity resolution algorithm '// &
     &                             'OSCRO is running for '//BAND_STR(IPAR1)
              END IF
!
! ----------- Resolving phase delay ambiguties for X-band using OSCRO algorithm
!
              CALL ERR_PASS  ( IUER, IER )
              CALL OSCRO_SES ( DBOBJ%L_OBS, DBOBJ, OBSBAS, RES, PAMBI, IPAR1, &
     &                         KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6971, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'attempt to apply phase delay ambiguity resolution ' // &
     &                 'algorithm OSCRO for X-band while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Ambiguity resolution algorithm '// &
     &                             'OSCRO finished for '//BAND_STR(IPAR1)
                   WRITE ( 23, '(A)' ) '     Ambiguity resolution algorithm '// &
     &                             'OSCRO finished for '//BAND_STR(IPAR1)
                   WRITE (  6, * ) '    ', KAMB, ' ambiguities were changed'
                   WRITE ( 23, * ) '    ', KAMB, ' ambiguities were changed'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'UPWEI       ' ) THEN
!
! =========== UPWEI
!             ~~~~~
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Weight update procedure UPWEI '// &
     &                                 'is running '
                   WRITE ( 23, '(A)' ) '     Weight update procedure UPWEI '// &
     &                                 'is running '
              END IF
!
! ----------- Setting parameter list for UPWEI
!
              CALL CLRCH ( STR )
              STR = 'NO'
              IF ( PAR%CKWD(1,J1)(1:8) .EQ. 'BASELINE' ) THEN
                   STR = 'BA'
                ELSE IF ( PAR%CKWD(1,J1)(1:6) .EQ. 'GLOBAL' ) THEN
                   STR = 'GL'
              ENDIF
!
              IPAR2 = PAR%IKWD(2,J1)
              RPAR3 = PAR%RKWD(3,J1)
              IF ( PAR%CKWD(4,J1)(1:3) .EQ. 'YES' ) THEN
                   LPAR4 = .TRUE.
                 ELSE
                   LPAR4 = .FALSE.
              END IF
              RPAR5 = PAR%RKWD(5,J1)*1.D-12
              RPAR6 = PAR%RKWD(6,J1)*1.D-12
              IPAR7 = PAR%IKWD(7,J1)
!
! ----------- Weight update
!
              CALL ERR_PASS   ( IUER, IER )
              CALL PARU_UPWEI ( STR(1:2), IPAR2, RPAR3, LPAR4, RPAR5, RPAR6, &
     &             IPAR7, IDB2, IDBF, DBOBJ%L_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &             OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &             PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6972, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'weight update while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Procedure UPWEI finished weight '// &
     &                                 'update '
                   WRITE ( 23, '(A)' ) '     Procedure UPWEI finished weight '// &
     &                                 'update '
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'SAVE        ' ) THEN
!
! =========== SAVAMB
!             ~~~~~~
!
              IF ( PAR%CKWD(2,J1)(1:3) .EQ. 'YES' ) THEN
                   LPAR1 = .TRUE.
                 ELSE
                   LPAR1 = .FALSE.
              END IF
!
              IF ( PAR%CKWD(3,J1)(1:3) .EQ. 'YES' ) THEN
                   LPAR2 = .TRUE.
                 ELSE
                   LPAR2 = .FALSE.
              END IF
!
              IF ( PAR%CKWD(4,J1)(1:12) .EQ. 'THIS_VERSION' ) THEN
                   INC_VERS =  0 ! to keep version counter in the weight file to the datbase version
                 ELSE 
                   INC_VERS = -2 ! to set version counter in the weight file to 0 
              ENDIF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Saving ambiguities and '// &
     &                             'ionosphere correction is running '
                   WRITE ( 23, '(A)' ) '     Saving ambiguities and '// &
     &                             'ionosphere correction is running '
              END IF
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PAMB_SAVE ( IDBF, N_OBS, DBOBJ, OBSBAS, RES, PAMBI, LPAR1, &
     &                         LPAR2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6973, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'saving ambiguities and ionsphere correction '// &
     &                 'occurred while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Saving ambiguities and '// &
     &                             'ionosphere correction finished'
                   WRITE ( 23, '(A)' ) '     Saving ambiguities and '// &
     &                             'ionosphere correction finished'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'ELIM        ' ) THEN
!
! =========== ELIM
!             ~~~~
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Procedure for outliers '// &
     &                                 'elimination ELIM is running '
                   WRITE ( 23, '(A)' ) '     Procedure for outliers '// &
     &                                 'elimination ELIM is running '
              END IF
!
! ----------- Setting parameter list for ELIM
!
              CALL CLRCH ( STR )
              STR = 'NO'
              IF ( PAR%CKWD(1,J1)(1:8) .EQ. 'BASELINE' ) THEN
                   STR = 'BA'
                ELSE IF ( PAR%CKWD(1,J1)(1:6) .EQ. 'GLOBAL' ) THEN
                   STR = 'GL'
              ENDIF
!
! ----------- Saving global values
!
              ELIM_MOD__SAVE = ELIM_MOD
              ELIM_TYP__SAVE = ELIM_TYP
              ELIM_CUT__SAVE = ELIM_CUT
              ELIM_THR__SAVE = ELIM_THR
              ELIM_MSR__SAVE = ELIM_MSR
              QUALCODE_GOOD_LIM__SAVE = QUALCODE_GOOD_LIM
              ELIM_UPD__SAVE = ELIM_UPD
              ELIM_VRB__SAVE = ELIM_VRB
              ELIM_AMB__SAVE = ELIM_AMB
              ELIM_ION__SAVE = ELIM_ION
              ELIM_CNF__SAVE = ELIM_CNF
!
! ----------- Putting parameters of elimination to global variablres which
! ----------- manage the procedure of outlier elimination
!
              ELIM_MOD          = .TRUE.
              ELIM_TYP          = STR(1:2)
              ELIM_CUT          = PAR%RKWD(2,J1)
              ELIM_THR          = PAR%RKWD(3,J1)*1.D-12
              QUALCODE_GOOD_LIM = PAR%IKWD(4,J1)
              ELIM_UPD          = PAR%IKWD(5,J1)
              ELIM_VRB          = PAR%IKWD(6,J1)
              ELIM_MSR          = PAR%RKWD(7,J1)*1.D-12
              ELIM_CNF          = .FALSE.
!
! ----------- Outrlier elimination
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PARU_ELIM ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, &
     &                         DBOBJ%L_STA, OBSHLD, DBOBJ, NCREC, &
     &                         OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &                         PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6974, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'outlier elimination occurred while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
! ----------- Restoring global values
!
              ELIM_MOD = ELIM_MOD__SAVE
              ELIM_TYP = ELIM_TYP__SAVE
              ELIM_CUT = ELIM_CUT__SAVE
              ELIM_THR = ELIM_THR__SAVE
              ELIM_MSR = ELIM_MSR__SAVE
              QUALCODE_GOOD_LIM = QUALCODE_GOOD_LIM__SAVE
              ELIM_UPD = ELIM_UPD__SAVE
              ELIM_VRB = ELIM_VRB__SAVE
              ELIM_AMB = ELIM_AMB__SAVE
              ELIM_ION = ELIM_ION__SAVE
              ELIM_CNF = ELIM_CNF__SAVE
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Procedure for outliers '// &
     &                                 'elimination ELIM finished'
                   WRITE ( 23, '(A)' ) '     Procedure for outliers '// &
     &                                 'elimination ELIM finished'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'MILE        ' ) THEN
!
! =========== MILE
!             ~~~~
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Procedure for restoration of '// &
     &                             'suppressed observations MILE is running'
                   WRITE ( 23, '(A)' ) '     Procedure for restoration of '// &
     &                             'suppressed observations MILE is running'
              END IF
!
! ----------- Setting parameter list for ELIM
!
              CALL CLRCH ( STR )
              STR = 'NO'
              IF ( PAR%CKWD(1,J1)(1:8) .EQ. 'BASELINE' ) THEN
                   STR = 'BA'
                ELSE IF ( PAR%CKWD(1,J1)(1:6) .EQ. 'GLOBAL' ) THEN
                   STR = 'GL'
              ENDIF
!
! ----------- Saving global values
!
              ELIM_MOD__SAVE = ELIM_MOD
              ELIM_TYP__SAVE = ELIM_TYP
              ELIM_CUT__SAVE = ELIM_CUT
              ELIM_THR__SAVE = ELIM_THR
              ELIM_MSR__SAVE = ELIM_MSR
              QUALCODE_GOOD_LIM__SAVE = QUALCODE_GOOD_LIM
              ELIM_UPD__SAVE = ELIM_UPD
              ELIM_VRB__SAVE = ELIM_VRB
              ELIM_AMB__SAVE = ELIM_AMB
              ELIM_ION__SAVE = ELIM_ION
              ELIM_CNF__SAVE = ELIM_CNF
!
! ----------- Putting parameters of elimination to global variablres which
! ----------- manage the procedure of eoutlier elimination
!
              ELIM_MOD          = .FALSE.
              ELIM_TYP          = STR(1:2)
              ELIM_CUT          = PAR%RKWD(2,J1)
              ELIM_THR          = PAR%RKWD(3,J1)*1.D-12
              QUALCODE_GOOD_LIM = PAR%IKWD(4,J1)
              ELIM_UPD          = PAR%IKWD(5,J1)
              ELIM_VRB          = PAR%IKWD(6,J1)
              ELIM_AMB          = .FALSE.
              IF ( PAR%CKWD(7,J1)(1:3) .EQ. 'YES' ) THEN
                   ELIM_AMB     = .TRUE.
              END IF
              ELIM_ION          = .FALSE.
              IF ( PAR%CKWD(8,J1)(1:3) .EQ. 'YES' ) THEN
                   ELIM_ION     = .TRUE.
              END IF
              ELIM_CNF          = .FALSE.
              ELIM_MSR__SAVE    = PAR%RKWD(9,J1)*1.D-12
!
              DBOBJ%F_AMB = ELIM_AMB
              DBOBJ%F_ION = ELIM_ION
              DBOBJ%F_AMB_CHANGED = .FALSE.
!
! ----------- Restoration of previously suppressed observations
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PARU_ELIM ( IDB2, IDBF, DBOBJ%L_OBS, DBOBJ%L_SCA, &
     &             DBOBJ%L_STA, &
     &             OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &             PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6975, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'restoration previously suppressed pbservations '// &
     &                 'occurred while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
! ----------- Restoring global values
!
              ELIM_MOD = ELIM_MOD__SAVE
              ELIM_TYP = ELIM_TYP__SAVE
              ELIM_CUT = ELIM_CUT__SAVE
              ELIM_THR = ELIM_THR__SAVE
              ELIM_MSR = ELIM_MSR__SAVE
              QUALCODE_GOOD_LIM = QUALCODE_GOOD_LIM__SAVE
              ELIM_UPD = ELIM_UPD__SAVE
              ELIM_VRB = ELIM_VRB__SAVE
              ELIM_AMB = ELIM_AMB__SAVE
              ELIM_ION = ELIM_ION__SAVE
              ELIM_CNF = ELIM_CNF__SAVE
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, '(A)' ) '     Procedure for restoration '// &
     &                                 'suppressed observations MILE finished'
                   WRITE ( 23, '(A)' ) '     Procedure for restoration '// &
     &                                 'suppressed observations MILE finished'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'SCATIE      ' ) THEN
!
! =========== SCATIE
!             ~~~~~~
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Ambiguity resolution algorithm '// &
     &                             'SCATIE is running'
                   WRITE ( 23, * ) '     Ambiguity resolution algorithm '// &
     &                             'SCATIE is running'
              END IF
!
              CALL ERR_PASS  ( IUER, IER )
              CALL SCATIE_DO ( DBOBJ, OBSSCA, OBSBAS, PAMBI, KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6976, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'phase delay ambiguity resolution by SCATIE '// &
     &                 'algorithm occurred while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '    ', KAMB,' ambiguities were changed'
                   WRITE ( 23, * ) '    ', KAMB,' ambiguities were changed'
                   WRITE (  6, * ) '     Ambiguity resolution algorithm '// &
     &                             'SCATIE finished'
                   WRITE ( 23, * ) '     Ambiguity resolution algorithm '// &
     &                             'SCATIE finished'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'STATIONS    ' ) THEN
!
! =========== STATIONS
!             ~~~~~~~~
!
              IPB = 1
!
! ----------- Initialization of participation status
!
              DO 440 J4=1,DBOBJ%L_STA
                 SCAINF%P_STA(J4) = .FALSE.
 440          CONTINUE
!
! ----------- Screening the list of station participating in SCADAM
!
              DO 450 J5=1,I_LEN(PAR%CKWD(1,J1))
                 IPN = INDEX ( PAR%CKWD(1,J1)(IPB:), ',' ) + IPB-1
                 IF ( IPN .LE. IPB-1 ) THEN
                      IPE = I_LEN ( PAR%CKWD(1,J1) )
                   ELSE
                      IPE = IPN -1
                 END IF
!
! -------------- Get a word
!
                 CALL CLRCH ( STR )
                 STR = PAR%CKWD(1,J1)(IPB:IPE)
                 CALL CHASHL ( STR )
                 DO 460 J6=1,8
                    IF ( STR(J6:J6) .EQ. '_' ) STR(J6:J6) = ' '
 460             CONTINUE
!
                 IF ( J5 .EQ. 1   .AND.  STR(1:3) .EQ. 'ALL' ) THEN
!
! ------------------- Special keyvalue "ALL" -- all stations
!
                      DO 470 J7=1,DBOBJ%L_STA
                         SCAINF%P_STA(J7) = .TRUE.
 470                  CONTINUE
                      PSTA = .TRUE.
                      GOTO 850
                 END IF
!
! -------------- Search for the station STR in the list of participated
! -------------- stations
!
                 IP = LTM_DIF ( 0, DBOBJ%L_STA, DBOBJ%C_STA, STR(1:8) )
                 IF ( IP .LE. 0 ) THEN
                      CALL LIST_TO_LINE ( DBOBJ%L_STA, DBOBJ%C_STA, ', ', OUT )
                      CALL ERR_LOG ( 6977, IUER, 'PARU_EXEC', 'Station '// &
     &                     STR(1:8)//' specified in PARTICIPATION list '// &
     &                    'is not found in the list of participated '// &
     &                    'stations: '//OUT(1:I_LEN(OUT)) )
                      RETURN
                 END IF
!
! -------------- Setting bit of participation
!
                 SCAINF%P_STA(IP) = .TRUE.
                 PSTA = .TRUE.
!
                 IF ( IPN .LE. IPB-1 ) THEN
                      GOTO 850
                    ELSE
                      IPB = IPN + 1
                 END IF
 450          CONTINUE
 850          CONTINUE
!
              STR = PAR%CKWD(2,J1)(1:8)
              DO 480 J8=1,8
                 IF ( STR(J8:J8) .EQ. '_' ) STR(J8:J8) = ' '
 480          CONTINUE
!
! ----------- Search for the station STR in the list of participated
! ----------- stations
!
              IP = LTM_DIF ( 0, DBOBJ%L_STA, DBOBJ%C_STA, STR(1:8) )
              IF ( IP .LE. 0 ) THEN
                   CALL LIST_TO_LINE ( DBOBJ%L_STA, DBOBJ%C_STA, ', ', OUT )
                   CALL ERR_LOG ( 6978, IUER, 'PARU_EXEC', 'Station '// &
     &                     STR(1:8)//' specified in FIDUCIAL list '// &
     &                    'is not found in the list of participated '// &
     &                    'stations: '//OUT(1:I_LEN(OUT)) )
                   RETURN
              END IF
!
! ----------- We found
!
              SCAINF%FID_STA = IP
              FSTA = .TRUE.
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'SCADAM      ' ) THEN
!
! =========== SCADAM
!             ~~~~~~
!
              IF ( .NOT. PSTA ) THEN
                   CALL ERR_LOG ( 6979, IUER, 'PARU_EXEC', 'List of '// &
     &                 'participating stations has not been specified. '// &
     &                 'SCADAM cannot run without specification of the list '// &
     &                 'of participating stations. Use function STATIONS '// &
     &                 'before function SCADAM' )
                   RETURN
              END IF
!
              IF ( .NOT. FSTA ) THEN
                   CALL ERR_LOG ( 6980, IUER, 'PARU_EXEC', 'Fiducial station '// &
     &                 'has not been specified. SCADAM cannot run without '// &
     &                 'specification of the fiducial station. Use function '// &
     &                 'STATIONS before function SCADAM' )
                   RETURN
              END IF
!
! ----------- Gathering parameters for SCADAM algorithm
!
              IPAR1           = PAR%IKWD(1,J1)
              SCAINF%XGR_LIM  = PAR%RKWD(2,J1)*1.D-12
              SCAINF%SGR_LIM  = PAR%RKWD(3,J1)*1.D-12
              SCAINF%XPH_LIM  = PAR%RKWD(4,J1)*1.D-12
              SCAINF%SPH_LIM  = PAR%RKWD(5,J1)*1.D-12
              SCAINF%DEFRG    = PAR%RKWD(6,J1)
              SCAINF%ARFMS    = PAR%RKWD(7,J1)
              SCAINF%FRZTR    = PAR%RKWD(8,J1)*3600.D0
              SCAINF%ARFFLO   = PAR%RKWD(9,J1)
              SCAINF%SPL_SPAN = PAR%RKWD(10,J1)*3600.D0
              SCAINF%SPL_CNST = PAR%RKWD(11,J1)/3600.D0
!
              IF ( PAR%CKWD(12,J1)(1:3) .EQ. 'YES' ) THEN
                   SCAINF%MSC_CONTROL = .TRUE.
                 ELSE
                   SCAINF%MSC_CONTROL = .FALSE.
              END IF
              SCAINF%ARF_TYPE = PAR%IKWD(13,J1)
              SCAINF%PLOT_INI = 0
              SCAINF%PLOT_FIN = 0
              IF ( IPAR1 .GE. 2 ) SCAINF%PLOT_FIN = 1
              IF ( IPAR1 .GE. 3 ) SCAINF%PLOT_INI = 1
!
! ----------- Call SCADAM algorithm
!
              CALL ERR_PASS  ( IUER, IER )
              CALL SCADAM_DO ( IPAR1, DBOBJ, OBSSCA, OBSBAS, PAMBI, SCAINF, &
     &                         KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6981, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'ambiguity resolution using SCADAM algorithm '// &
     &                 'occurred while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '    ', KAMB,' ambiguities were changed'
                   WRITE (  6, * ) '     Ambiguity resolution algorithm '// &
     &                             'SCADAM is running'
                   WRITE ( 23, * ) '    ', KAMB,' ambiguities were changed'
                   WRITE ( 23, * ) '      Ambiguity resolution algorithm '// &
     &                             'SCADAM is running'
              END IF
!
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'FREEZE_AMB  ' ) THEN
!
! =========== FREEZE_AMB
!             ~~~~~~~~~~
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Ambiguities are being frozen'
                   WRITE ( 23, * ) '     Ambiguities are being frozen'
              END IF
!
! ----------- Freezing/unfreezeing supression status of  phase delay
! ----------- observables
!
              IACT_FREEZE = -999
              IF ( PAR%CKWD(1,J1)(1:3) .EQ. 'X-B' ) THEN
                   IACT_FREEZE = 21
                ELSE IF ( PAR%CKWD(1,J1)(1:3) .EQ. 'S-B' ) THEN
                   IACT_FREEZE = 22
                ELSE IF ( PAR%CKWD(1,J1)(1:3) .EQ. 'BOT' ) THEN
                   IACT_FREEZE = 23
              END IF
!
! ----------- Freezeing ambiguities
!
              CALL ERR_PASS    ( IUER, IER )
              CALL PAMB_FREEZE ( IDBF, N_OBS, IACT_FREEZE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6982, IUER, 'PARU_EXEC', 'Error during '// &
     &                  'attempt to freeze suppression status '// &
     &                  'of phase delay observables while database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Ambiguities have been successfully '// &
     &                             'frozen'
                   WRITE ( 23, * ) '     Ambiguities have been successfully '// &
     &                             'frozen'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'UNFREEZE_AMB' ) THEN
!
! =========== FREEZE_AMB
!             ~~~~~~~~~~
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Ambiguities are being unfrozen'
                   WRITE ( 23, * ) '     Ambiguities are being unfrozen'
              END IF
!
! ----------- Freezing/unfreezeing supression status of  phase delay
! ----------- observables
!
              IACT_FREEZE = -999
              IF ( PAR%CKWD(1,J1)(1:3) .EQ. 'X-B' ) THEN
                   IACT_FREEZE = 31
                ELSE IF ( PAR%CKWD(1,J1)(1:3) .EQ. 'S-B' ) THEN
                   IACT_FREEZE = 32
                ELSE IF ( PAR%CKWD(1,J1)(1:3) .EQ. 'BOT' ) THEN
                   IACT_FREEZE = 33
              END IF
!
! ----------- Freezeing ambiguities
!
              CALL ERR_PASS    ( IUER, IER )
              CALL PAMB_FREEZE ( IDBF, N_OBS, IACT_FREEZE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6983, IUER, 'PARU_EXEC', 'Error during '// &
     &                  'attempt to unfreeze suppression status '// &
     &                  'of phase delay observables while database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   RETURN
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     Ambiguities have been successfully '// &
     &                             'unfrozen'
                   WRITE ( 23, * ) '     Ambiguities have been successfully '// &
     &                             'unfrozen'
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'SET_GR_WEI' ) THEN
              INIT_WEI = PAR%RKWD(1,J1)
              DO 490 J9=1,DBOBJ%L_BAS
                 OBSHLD%WEIGR_BAS(J9) = 1.D0/(INIT_WEI*1.D-12)
                 WEIGR_BAS_SAVE(J9) = OBSHLD%WEIGR_BAS(J9) 
                 WEIPH_BAS_SAVE(J9) = OBSHLD%WEIPH_BAS(J9)
 490          CONTINUE 
!
! ----------- Refresh weights
!
              CALL ERR_PASS ( IUER, IER )
              CALL REFRESH_WEI ( OBSHLD, DBOBJ, OBSBAS, WEIGR_BAS_SAVE, &
     &                           WEIPH_BAS_SAVE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6984, IUER, 'PARU_EXEC', 'Error duiring '// &
     &                 'attempt to refresh weights while database '// &
     &                  DBOBJ%NAME//' was processing' )
                   RETURN
              END IF
!
! ----------- Write them down in NAMFIL
!
              CALL ERR_PASS ( IUER, IER )
              CALL IO_WGT   ( 2, IDB2, DBOBJ, WEIGR_BAS_SAVE, WEIPH_BAS_SAVE, &
     &                        IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6985, IUER, 'PARU_EXEC', 'Error duiring '// &
     &                 'putting weights to NAMFIL while database '// &
     &                  DBOBJ%NAME//' was processing' )
                   RETURN
              END IF
!
! ----------- Making initial LSQ solution. Estimates and FULL covariance
! ----------- matrix are calculaterd. Postfit residuals and their statistics
! ----------- are also calculated and stored in temporary data structures
!
              ELIM_VRB__SAVE = ELIM_VRB
              ELIM_VRB = PAMB_VER
!
              CALL ERR_PASS ( IUER, IER )
              CALL SOL_INIT ( ELIM_VRB, 0, 1, N_OBS, DBOBJ%L_STA, &
     &                        DBOBJ%L_SCA, IDB2, IDBF, PLACE, B3DOBJ, &
     &                        B1B3DOBJ, OBSHLD, DBOBJ, NCREC, OBSSCA, &
     &                        OBSSTA, OBSBAS, RES, RST, CHIOBJ, EQUMEM, &
     &                       IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6986, IUER, 'PARU_EXEC', 'Error during '// &
     &                 'attempt to obtain initial solution while '// &
     &                'database '//DBNAME//' was processing' )
                  RETURN
              END IF
!
              ELIM_VRB = ELIM_VRB__SAVE
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     All weights wer set to ', INIT_WEI, &
     &                             ' ps '
                   WRITE ( 23, * ) '     All weights wer set to ', INIT_WEI, &
     &                             ' ps '
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'UPDATE_DB   ' ) THEN
              IF ( PAR%CKWD(1,J1)(1:12) == 'THIS_VERSION' ) THEN
                   OPCODE = 1
!?                   INC_VERS = 0
                 ELSE IF ( PAR%CKWD(1,J1)(1:12) == 'NEXT_VERSION' ) THEN
                   OPCODE = 2
!?                   INC_VERS = 1
                 ELSE IF ( PAR%CKWD(1,J1)(1:11) == 'NEXT_IF_1ST' ) THEN
                   OPCODE = 3
!?                   INC_VERS = -1
                 ELSE IF ( PAR%CKWD(1,J1)(1:12) == 'RESTORE     ' ) THEN
                   CALL ERR_LOG ( 6987, IUER, 'PARU_EXEC', 'Unsupported '// &
     &                 'value RESTORE of the qualifier OPERTAION for '// &
     &                 'the keyword UPDATE_DO' )
                   RETURN 
                 ELSE 
                   OPCODE = 0
              END IF
!
              CALL GETENVAR ( 'VCAT_CONF', STR )
              IF ( ILEN(STR) > 0 ) THEN
                   VCAT_CONF_FILE = STR
                 ELSE
                   VCAT_CONF_FILE = SOLVE_SAVE_DIR//'/vcat.conf'
               END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6988, IUER, 'PARU_EXEC', 'Error in '// &
     &                 'an attempt to read VCAT configuration file '// &
     &                  VTD_CONF_FILE )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_RELEASE ( GVH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6989, IUER, 'PARU_EXEC', 'Error in '// &
     &                 'an attempt to release memory used by GVH' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              IF ( OPCODE == 1 .OR. OPCODE == 2 .OR. OPCODE == 3 ) THEN
                   CALL UPTDB_DO ( GVH, VCAT, OPCODE, IER )
                 ELSE IF ( OPCODE == 4 ) THEN
!@                   CALL UPTDB_SUP ( GVH, IER )
                   CONTINUE
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6990, IUER, 'PARU_EXEC', 'Failure to '// &
                       'update the database '//DBNAME_CH )
                   RETURN 
              END IF
            ELSE IF ( PAR_FUN(IFUN) .EQ. 'GROUP_AMB   ' ) THEN
              IF ( PAR%CKWD(3,J1)(1:12) == 'GOOD_ONLY   ' ) THEN
                   OPCODE = 1
                 ELSE IF ( PAR%CKWD(3,J1)(1:12) == 'BAD_ONLY    ' ) THEN
                   OPCODE = 2
                 ELSE IF ( PAR%CKWD(3,J1)(1:12) == 'ALL_DATA    ' ) THEN
                   OPCODE = 3
                 ELSE 
                   OPCODE = 0
              END IF
              MARGIN = PAR%RKWD(4,J1)
              IVRB = PAR%IKWD(1,J1)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL PARU_GRAMB ( DBOBJ, OBSSCA, OBSBAS, RES, OPCODE, MARGIN, &
     &                          IVRB, N_AMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6991, IUER, 'PARU_EXEC', 'Failure to '// &
                       'update ambiguities in the database '//DBNAME_CH )
                   RETURN 
              END IF
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) '     ', N_AMB, ' Group delay ambiguities were resolved' 
                   WRITE ( 23, * ) '     ', N_AMB, ' Group delay ambiguities were resolved' 
              END IF
            ELSE
!
! =========== ???
!             ~~~
!
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 6992, IUER, 'PARU_EXEC', STR(1:I_LEN(STR))// &
     &            '-th procedure in PAR program '//PAR_FUN(IFUN)// &
     &            ' is not supported by PARU_EXEC' )
              RETURN
         END IF
         IF ( PAMB_VER .GE. 1 ) THEN
              WRITE (  6, * ) 'PAR: '//STR1(1:I_LEN(STR1))//'-th '// &
     &                        'procedure ',PAR_FUN(IFUN),' has been '// &
     &                        'successfully executed'
              WRITE ( 23, * ) 'PAR: '//STR1(1:I_LEN(STR1))//'-th '// &
     &                        'procedure ',PAR_FUN(IFUN),' has been '// &
     &                        'successfully executed'
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   PARU_EXEC  !#!#
