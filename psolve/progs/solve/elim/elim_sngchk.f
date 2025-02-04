      SUBROUTINE ELIM_SNGCHK ( DBOBJ, N_OBS, L_SCA, OBSBAS, OBSSCA, &
     &                         SNGCHK_CMP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ELIM_SNGCHK  makes singularity check for the normal       *
! *   matrix using statistics collected during processing the database   *
! *   when the normal matrix was build and estimation model.             *
! *   It makes the following checks:                                     *
! *     1) Does the number of USED in estimation observations of either  *
! *        source exceed the specified limit (SNGCHK_SOUMIN) if source   *
! *        dependent parameters for this source are estimated.           *
! *     2) Does the number of USED in estimation observations of either  *
! *        station exceed the specified limit (SNGCHK_STAMIN).           *
! *     3) Does the number of USED in estimation observations of the     *
! *        baseline exceed the specified limit (SNGCHK_BASMIN) if        *
! *        baseline-dependent parameters such as baseline-dependent      *
! *        clocks are estimated for this baseline.                       *
! *                                                                      *
! *   After making test, ELIM_SNGCHK makes singularity check action      *
! *   defined by SNGCHK_ACTION from ../include/glbc4.i                   *
! *                                                                      *
! *   SNGCHK generates completion code which is used by other routines.  *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *   N_OBS ( INTEGER*4 ) -- Total number of observations in the         *
! *                          session.                                    *
! *   L_SCA ( INTEGER*4 ) -- Number of common scans.                     *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *      R_SOU ( INTEGER*4 ) -- Number of sources to be deselected from  *
! *                             estimation.                              *
! *    RIS_SOU ( INTEGER*4 ) -- List of the sources which status         *
! *                             "estimate at least one coordinate" is    *
! *                             forcible tuened off. Arrays contains the *
! *                             indices of the source names in the array *
! *                             ISTRN_CHR from prfil.i  Length of the    *
! *                             list is R_SOU elements.                  *
! *      R_STA ( INTEGER*4 ) -- Number of stations to be deselected      *
! *    RIS_STA ( INTEGER*4 ) -- List of the sources to be deselected.    *
! *                             Arrays contains the indices of the       *
! *                             station names in the array ISITN_CHR     *
! *                             from prfil.i. Length of the list is      *
! *                             R_STA elements.                          *
! *      R_BAS ( INTEGER*4 ) -- Number of baselines to be deselected     *
! *    RIS_BAS ( INTEGER*4 ) -- List of the baselines to be deselected.  *
! *                             Arrays contains the indices of the       *
! *                             station names in the array ?? from       *
! *                             prfil.i Length of the list is R_BAS      *
! *                             elements.                                *
! * SNGCHK_CMP ( INTEGER*4 ) -- Completion code of the routine.          *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
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
! *  ### 12-JUL-1998  ELIM_SNGCHK   v1.1 (c) L. Petrov 12-JUL-1998  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INTEGER*4  N_OBS, L_SCA, SNGCHK_CMP, IUER
      INTEGER*4  R_SOU, RIS_SOU(MO_SOU), R_STA, RIS_STA(MO_STA), R_BAS, &
     &           RIS_BAS(MO_BAS)
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(L_SCA)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      INTEGER*4  J1, ISCA, N_SCA, IER
      CHARACTER  STR*20
      INTEGER*2  INT2_ARG
      LOGICAL*4, EXTERNAL :: USE_TERM_COLOR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Make singularity check test
!
      CALL ERR_PASS ( IUER, IER )
      CALL SNGCHK ( DBOBJ, -1, R_SOU, RIS_SOU, R_STA, RIS_STA, R_BAS, &
     &              RIS_BAS, SNGCHK_CMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6651, IUER, 'ELIM_SNGCHK', 'Error during '// &
     &         'singularity check after elimination of the outlier' )
           RETURN
      END IF
      IF ( USE_TERM_COLOR() ) CALL SET_COLOR ( 0 ) ! set color
!
      IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__CONT ) THEN
           CONTINUE
         ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__BACK ) THEN
!
! -------- Elimination of one outlier led to elimination of the set of
! -------- other observations and the number of parameters has been
! -------- changed. Therefore we make full updates of the solution,
! -------- calculation of residuals and their statistics.
!
! -------- Recalculation of the lists of used observations in DBOBJ
! -------- since reparameterization may change status "used"
! -------- observations
!
           N_SCA = L_SCA
           CALL ERR_PASS ( IUER, IER )
           CALL SESTAT_INIT ( DBOBJ, 2, DBOBJ%NAME, DBOBJ%IDATYP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6652, IUER, 'ELIM_SNGCHK', 'Error during '// &
     &              'initialization of DBOBJ data structures after '// &
     &              'reparameterization' )
                RETURN
           END IF
!
! -------- Cycle over all observations of the session to update statistics
!
           DO 410 J1=1,N_OBS
              ISCA = OBSBAS(J1)%IND_SCA
!
! ----------- Update of suppression code for possible change of the
! ----------- deselection status of the source/baseline/station
!
              CALL SUPSTAT_UPD ( OBSSCA(ISCA)%ISTAR, OBSBAS(J1)%ISITE, &
     &                           OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                           OBSBAS(J1)%AUTO_SUP )
!
! ----------- Update of lists and statistics for the J1-th obsevation
!
              CALL ERR_PASS   ( IUER, IER )
              CALL SESTAT_OBS ( DBOBJ, OBSSCA(ISCA)%ISTAR, OBSBAS(J1)%ISITE, &
     &                          SUPMET, OBSBAS(J1)%SUPSTAT(1), &
     &                          OBSBAS(J1)%UACSUP, OBSBAS(J1)%AUTO_SUP, &
     &                          OBSBAS(J1)%USER_SUP, OBSBAS(J1)%USER_REC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6653, IUER, 'ELIM_SNGCHK', 'Error during '// &
     &                 'calculation anew of DBOBJ data structures after '// &
     &                 'reparameterization//reparameterization when the '// &
     &                  STR(1:I_LEN(STR))//'-th observation was processed' )
                   RETURN
              END IF
 410       CONTINUE
!
! -------- Final sorting the lists
!
           CALL ERR_PASS   ( IUER, IER )
           CALL SESTAT_DBS ( DBOBJ, N_SCA, 0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6654, IUER, 'ELIM_SNGCHK', 'Error during '// &
     &              'initialization of DBOBJ data structures after '// &
     &              'reparameterization' )
                RETURN
           END IF
         ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__STOP ) THEN
           CALL ERR_LOG ( 6655, IUER, 'ELIM_SNGCHK', 'Singurity check '// &
     &         'detected possible singularity. Execution is terminated in '// &
     &         'accordance with user action for singularity test: STOP' )
           RETURN
         ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__SKIP ) THEN
           CONTINUE
         ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__FAIL ) THEN
           CALL ERR_LOG ( 6656, IUER, 'ELIM_SNGCHK', 'Trap of internal '// &
     &         'control: failure to determine completion status of SNGCHK. '// &
     &         'Please send verbose bug report to Leonid Petrov '// &
     &         '(pet@leo.gsfc.nasa.gov)' )
      END IF  !   SNGCHK_CMP
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ELIM_SNGCHK  #!#
