      SUBROUTINE OPA_ACTION ( IVRB, SOLVE_INIT, OPA, OPC_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  OPA_ACTION  interprets fields OPA.ACT, OPA.IACT and       *
! *   executes a set of OPA actions. Actions are executed in the order   *
! *   of values OPA__xxx. Status field and action field are updated upon *
! *   successful completion of each action. Operational analysis         *
! *   control file is updated in the case of successful completion and   *
! *   therefore OPA.ACT, OPA.STS fields are stored in the file.          *
! *                                                                      *
! *   If field OPA.IACT is OPA__ALL then all actions with value          *
! *   OPA.ACT(xxx) = "+" are executed. If OPA.IACT is OPA__xxx and       *
! *   OPA.ACT(OPA__xxx) = "+" then only action OPA__xxx is executed.     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       IVRB ( INTEGER*4 ) -- verbosity level.                         *
! *                          IVRB = 0 -- silent mode. Only error         *
! *                                 messages are printed at screen.      *
! *                          IVRB = 1 -- normal mode. Progress status is *
! *                                 printed at screen.                   *
! * SOLVE_INIT ( CHARACTER ) -- Solve user initials.                     *
! *   OPC_FILE ( CHARACTER ) -- Operational analysis control file.       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     OPA ( RECORD         ) -- Data structure which keeps settings    *
! *                               of OPA and current information related *
! *                               to processing this session.            *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 07-SEP-2000   OPA_ACTION  v1.5 (c)  L. Petrov  09-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'opa.i'
      TYPE ( OPA__STRU ) ::  OPA
      CHARACTER  SOLVE_INIT*2, OPC_FILE*(*)
      INTEGER*4  IVRB, IUER
      INTEGER*4  IER, IER_BEG
      LOGICAL*4  FL_EOPK
!
      CALL ERR_PASS ( IUER, IER_BEG )
      FL_EOPK = .FALSE. ! set flag EOP Kalman fileter has not been yet applied
!
! --- Call a procedure which whoul execute an action
!
      IF (   OPA%ACT(OPA__SUP) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__SUP ) ) THEN
!
! -------- Action: create superfile
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action create superfile '// &
     &                       'for database '//OPA%DB_NAME//' is being '// &
     &                       'executed... '
           CALL ERR_PASS ( IUER, IER )
           CALL OPA_SUP  ( SOLVE_INIT, OPA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4181, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to create a superfile for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__SUP) = '+'
           OPA%ACT(OPA__SUP) = 'D'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action create superfile '// &
     &                       'for database '//OPA%DB_NAME//' is executed '// &
     &                       'successfully'
      END IF
!
      IF (   OPA%ACT(OPA__GAL) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__GAL ) ) THEN
!
! -------- Action: update global arc-file
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update global '// &
     &                       'arc-list for database '//OPA%DB_NAME//' is '// &
     &                       'being executed... '
           CALL ERR_PASS ( IUER, IER )
           CALL OPA_GAL  ( OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4182, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to ipdate global arc-list for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__GAL) = '+'
           OPA%ACT(OPA__GAL) = 'D'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update global '// &
     &                       'arc-list for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
      END IF
!
      IF (   OPA%ACT(OPA__BAW) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__BAW ) ) THEN
!
! -------- Action: update baseline-dependent weights
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make baseline '// &
     &                       'weights for database '//OPA%DB_NAME//' is '// &
     &                       'being executed... '
           CALL ERR_PASS ( IUER, IER )
           CALL OPA_BAW  ( SOLVE_INIT, OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4183, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to create a baseline weights for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__BAW) = '+'
           OPA%ACT(OPA__BAW) = 'D'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make baseline '// &
     &                       'weights for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
      END IF
!
      IF (   OPA%ACT(OPA__STW) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__STW ) ) THEN
!
! -------- Action: update station-dependent weights
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make site '// &
     &                       'weights for database '//OPA%DB_NAME//' is '// &
     &                       'being executed... '
           CALL ERR_PASS ( IUER, IER )
           CALL OPA_STW  ( SOLVE_INIT, OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4184, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to create a site weights for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__STW) = '+'
           OPA%ACT(OPA__STW) = 'D'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make site '// &
     &                       'weights for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
      END IF
!
      IF (   OPA%ACT(OPA__STN) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__STN ) ) THEN
!
! -------- Action: make a standalone Solve solution
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make new standalone '// &
     &                       'solution for database '//OPA%DB_NAME//' is '// &
     &                       'being executed..'
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_STANDALONE ( SOLVE_INIT, OPA, OPC_FILE, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4185, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to make standalone solution for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__STN) = '+'
           OPA%ACT(OPA__STN) = 'D'
           IF ( OPA%STS(OPA__SNX) .EQ. 'N' ) OPA%STS(OPA__SNX) = '?'
           IF ( OPA%ACT(OPA__SNX) .EQ. 'N' ) OPA%ACT(OPA__SNX) = '?'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make '// &
     &                       'standalone solution for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
      END IF
!
      IF (   OPA%ACT(OPA__EOS) .EQ. '+'             .AND. &
     &       OPA%SESSION_TYPE(1:7) .EQ. 'DIURNAL'   .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__EOS ) ) THEN
!
! -------- Action: make two Solve EOP solutions, run EOP Kalman filetr and
! -------- update EOPS file
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update EOPS '// &
     &                       'for database '//OPA%DB_NAME//' is '// &
     &                       'being executed... '
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_EOPS   ( SOLVE_INIT, OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4186, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to update EOPS file for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__EOS) = '+'
           OPA%ACT(OPA__EOS) = 'D'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update '// &
     &                       'EOPS for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
           OPA%STS(OPA__EOK) = '+'
           OPA%ACT(OPA__EOK) = 'D'
           FL_EOPK = .TRUE.
      END IF
!
      IF (   OPA%ACT(OPA__EOS) .EQ. '+'             .AND. &
     &       OPA%SESSION_TYPE(1:9) .EQ. 'INTENSIVE'    .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__EOS ) ) THEN
!
! -------- Action: make Solve EOPI solution and update the EOPI file
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update EOPI '// &
     &                       'for database '//OPA%DB_NAME//' is '// &
     &                       'being executed... '
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_EOPI   ( SOLVE_INIT, OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4186, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to update EOPI file for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__EOS) = '+'
           OPA%ACT(OPA__EOS) = 'D'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update '// &
     &                       'EOPI for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
      END IF
!
      IF (   OPA%ACT(OPA__EOM) .EQ. '+'                .AND. &
     &       OPA%SESSION_TYPE(1:9) .EQ. 'INTENSIVE'    .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__EOM ) ) THEN
!
! -------- Action: make two Solve EOP solutions, and update EOPM file
!
! -------- Check whether the standalone solutions has been run. If not,
! -------- we have to run it, since we need to learn information about
! -------- the total number of baselines and their names
!
           IF ( OPA%ACT(OPA__STN) .NE. 'D' ) THEN
!
! ------------- Action: make a standalone Solve solution
!
                IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'We need first to run a standalone solution'
                IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make new standalone '// &
     &                            'solution for database '//OPA%DB_NAME//' is '// &
     &                            'being executed..'
                CALL ERR_PASS   ( IUER, IER )
                CALL OPA_STANDALONE ( SOLVE_INIT, OPA, OPC_FILE, IVRB, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4187, IUER, 'OPA_ACTION', 'Error '// &
     &                   'in attempt to make standalone solution for '// &
     &                   'database '//OPA%DB_NAME )
                     GOTO 810
                END IF
!
                OPA%STS(OPA__STN) = '+'
                OPA%ACT(OPA__STN) = 'D'
           END IF
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update EOPM '// &
     &                       'for database '//OPA%DB_NAME//' is '// &
     &                       'being executed... '
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_EOPM   ( SOLVE_INIT, OPA, OPC_FILE, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4188, IUER, 'OPA_ACTION', 'Error in '// &
     &              'attempt to update EOPM file for database '// &
     &               OPA%DB_NAME )
                GOTO 810
           END IF
           OPA%STS(OPA__EOM) = '+'
           OPA%ACT(OPA__EOM) = 'D'
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update '// &
     &                       'EOPM for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
      END IF
!
      IF (   OPA%ACT(OPA__EOK) .EQ. '+'  .AND.  .NOT. FL_EOPK  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__EOK ) ) THEN
!
! -------- Action run EOP Kalman Filter and update erp-file
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update EOPK '// &
     &                       'for database '//OPA%DB_NAME// &
     &                       ' is being executed... '
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_EOPK   ( SOLVE_INIT, OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4189, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to update EOPK file for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action update '// &
     &                       'EOPK for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
           OPA%STS(OPA__EOK) = '+'
           OPA%ACT(OPA__EOK) = 'D'
      END IF
!
      IF (   OPA%ACT(OPA__SNR) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__SNR ) ) THEN
!
! -------- Action to make SNR analysis
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make SNR analysis '// &
     &          'for database '//OPA%DB_NAME//' is being executed... '
           CALL ERR_PASS    ( IUER, IER )
           CALL OPA_SNRANAL ( SOLVE_INIT, OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4190, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to make SNR analysis for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action make SNR '// &
     &                       'analysis for database '//OPA%DB_NAME// &
     &                       ' is executed successfully'
           OPA%STS(OPA__SNR) = '+'
           OPA%ACT(OPA__SNR) = 'D'
      END IF
!
      IF (   OPA%ACT(OPA__VDB) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__VDB ) ) THEN
!
! -------- Action to make SNR analysis
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action insert a '// &
     &                       'solution listing into VDB for database '// &
     &                        OPA%DB_NAME//' is being executed... '
           CALL ERR_PASS ( IUER, IER )
           CALL OPA_VDB  ( SOLVE_INIT, OPA, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4191, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to make SNR analysis for database '//OPA%DB_NAME )
                GOTO 810
           END IF
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action insert a '// &
     &                       'solution listing into VDB for database '// &
     &                        OPA%DB_NAME//' is executed successfully'
           OPA%STS(OPA__VDB) = '+'
           OPA%ACT(OPA__VDB) = 'D'
      END IF
!
      IF (   OPA%ACT(OPA__SBD) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__SBD ) ) THEN
!
! -------- Action submit a pair of databases to the IVS Data Center
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Launching dclient ...'
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_SUBMIT ( OPA, OPA__SBD, IVRB, IER )
           IF ( IER .EQ. OPA__CAN ) THEN
                WRITE ( *, '(A)' ) 'Database '//OPA%DB_NAME//' will not be '// &
     &                             'submitted to the IVS Data Center'
             ELSE IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4192, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to submit a pair of databases '//OPA%DB_NAME )
                GOTO 810
             ELSE IF ( IER .EQ. 0  ) THEN
                OPA%STS(OPA__SBD) = '+'
                OPA%ACT(OPA__SBD) = 'D'
                IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action submit '// &
     &                            'a pair of databases '//OPA%DB_NAME// &
     &                            ' is executed successfully'
           END IF
      END IF
!
      IF (   OPA%ACT(OPA__SBE) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__SBE ) ) THEN
!
! -------- Action submit EOPS file to the IVS Data Center
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Launching dclient ...'
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_SUBMIT ( OPA, OPA__SBE, IVRB, IER )
           IF ( IER .EQ. OPA__CAN ) THEN
                WRITE ( *, '(A)' ) 'EOPS series will not be submitted to '// &
     &                             'the IVS Data Center'
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
              ELSE IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4193, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to submit a EOPS series '//OPA%DB_NAME )
                GOTO 810
              ELSE IF ( IER .EQ. 0 ) THEN
                OPA%STS(OPA__SBE) = '+'
                OPA%ACT(OPA__SBE) = 'D'
                IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action submit '// &
     &                            'series of EOP is executed successfully'
           END IF
      END IF
!
      IF (   OPA%ACT(OPA__SBM) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__SBM ) ) THEN
!
! -------- Action submit EOPM file to the IVS Data Center
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Launching dclient ...'
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_SUBMIT ( OPA, OPA__SBM, IVRB, IER )
           IF ( IER .EQ. OPA__CAN ) THEN
                WRITE ( *, '(A)' ) 'EOPM series will not be submitted to '// &
     &                             'the IVS Data Center'
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
              ELSE IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4194, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to submit a EOPM series '//OPA%DB_NAME )
                GOTO 810
              ELSE IF ( IER .EQ. 0 ) THEN
                OPA%STS(OPA__SBM) = '+'
                OPA%ACT(OPA__SBM) = 'D'
                IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action submit '// &
     &                            'series of EOPM is executed successfully'
           END IF
      END IF
!
      IF (   OPA%ACT(OPA__SNX) .EQ. '+'  .AND. &
     &     ( OPA%IACT .EQ. OPA__ALL  .OR.  OPA%IACT .EQ. OPA__SNX ) ) THEN
!
! -------- Action submit Sinex listing to the IVS Data Center
!
           IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Launching dclient ...'
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_SUBMIT ( OPA, OPA__SNX, IVRB, IER )
           IF ( IER .EQ. OPA__CAN ) THEN
                WRITE ( *, '(A)' ) 'Sinex listing from database '//OPA%DB_NAME// &
     &                         ' will not be submitted to the IVS Data Center'
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
              ELSE IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4195, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &              'to submit a Sinex listing from the database '// &
     &               OPA%DB_NAME )
                GOTO 810
              ELSE IF ( IER .EQ. 0 ) THEN
                OPA%STS(OPA__SNX) = '+'
                OPA%ACT(OPA__SNX) = 'D'
                IF ( IVRB .GE. 1 ) WRITE ( *, '(A)' ) 'Action submit '// &
     &                            'Sinex listing is executed successfully'
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
 810  CONTINUE
!
! --- Write OPC file on disk once more
!
      CALL WRITE_OPC ( OPC_FILE, OPA, IER_BEG )
      IF ( IER_BEG .NE. 0 ) THEN
           CALL ERR_LOG ( 4196, IUER, 'OPA_ACTION', 'Error in attempt '// &
     &         'to write the current status of experiment '//OPA%DB_NAME// &
     &         ' in file '//OPC_FILE )
           RETURN
      END IF
!
      RETURN
      END  !#!  OPA_ACTION  #!#
