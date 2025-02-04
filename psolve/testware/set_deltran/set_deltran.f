      PROGRAM    SET_DELTRAN
! ************************************************************************
! *                                                                      *
! *   Program SET_DELTRAN  creates an intermediate file for importing    *
! *   delays from the database to the current Solve scratch area         *
! *   provided that this database and current Solve scratch area belong  *
! *   to the same experiment. This trick is used for investigation of    *
! *   subtle differences between two versions (revisions) of a database  *
! *   file obtained by different fringing.                               *
! *                                                                      *
! *   set_deltran reads a pair of X/S databases and puts in the          *
! *   intermediate file some information: group and phase delays, delay  *
! *   date, suppression status etc.                                      *
! *                                                                      *
! *   This intermediate file is later used by program USE_DELTRAN which  *
! *   is invoked by Solve in so-called user program mode.                *
! *                                                                      *
! *   Usage: set_deltran <database_name>'                                *
! *                                                                      *
! *  ### 11-DEC-2001  SET_DELTRAN  v1.1 (c)  L. Petrov  28-DEC-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'delay_transfer.i'
      CHARACTER  DBX*10, DBS*10, FILOUT*128, SCR_DIR*128
      INTEGER*4  LEN_DTR, MOBS, IP, NOBS
      PARAMETER  ( MOBS = 64*1024 )
      TYPE ( DT__STRU ) ::  DTR(MOBS)
      INTEGER*4  LUN, J1, IS
      INTEGER*4  IARGC, I_LEN, ILEN
!
      IF ( IARGC () .GE. 1 ) THEN
           CALL GETARG ( 1, DBX )
           IF ( DBX(1:1) .NE. '$' ) DBX = '$'//DBX
           DBS = DBX
           DBS(9:9) = 'S'
         ELSE
           WRITE ( 6, '(A)' ) 'Usage: set_deltran <database_name>'
           CALL EXIT ( 1 )
      END IF
!
! --- Learn the name of the scatch directory
!
      CALL CLRCH ( SCR_DIR )
      CALL GETENVAR ( 'SCRATCH_DIR', SCR_DIR )
      IF ( ILEN(SCR_DIR) .EQ. 0 ) THEN
           SCR_DIR = SCRATCH_DIR
      END IF
      IS = I_LEN(SCR_DIR)
      IF ( SCR_DIR(IS:IS) .EQ. '/' ) SCR_DIR(IS:IS) = ' '
      IS = I_LEN(SCR_DIR)
!
! --- Set filename
!
      FILOUT   = SCR_DIR(1:IS)//'/delay_transfer.bin'
!
! --- Learn the length of DTR data structure
!
      LEN_DTR = LOC( DTR(1)%LAST_FIELD ) - LOC( DTR(1)%GRDEL_X ) + 2
!
! --- Read X-band database
!
      WRITE ( 6, '(A,A,A)' ) 'Reading ', DBX, '  ...'
      CALL READ_DB ( 1, DBX,  MOBS,  NOBS, DTR, -3 )
!
! --- Read S-band database
!
      WRITE ( 6, '(A,A,A)' ) 'Reading ', DBS, '  ...'
      CALL READ_DB ( 2, DBS,  NOBS,  IP,   DTR, -3 )
!
      WRITE ( 6, '(A,A)' ) 'Writing in ', FILOUT(1:I_LEN(FILOUT))
!
! --- Open the output binary file
!
      CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, -3 )
!
! --- Write the number of observations
!
      CALL WRBIN_RECORD ( LUN, 4, NOBS, -3 )
!
! --- Write records for each obsercation
!
      DO 410 J1=1,NOBS
         CALL WRBIN_RECORD ( LUN, LEN_DTR, DTR(J1), -3 )
 410  CONTINUE
      CALL BINF_CLOSE ( LUN, -3 )
      WRITE ( 6, '(A)' ) 'Done'
!
      END  !#!  SET_DELTRAN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_DB ( IMODE, DB_NAME, MOBS, NOBS, DTR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_DB reads database and puts it into the record DT.    *
! *                                                                      *
! *  ### 11-DEC-2001    READ_DB    v1.0 (c)  L. Petrov  11-DEC-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE    'delay_transfer.i'
      INTEGER*4   IMODE, MOBS, NOBS, IUER
      CHARACTER   DB_NAME*(*)
!
      CHARACTER   STR*80, STR2*80
      CHARACTER   BL(2)*8, SOUR*8, QC_STR*2, DATE_STR*10
      INTEGER*2   KERR, I2, UACSUP
      INTEGER*4   IVER, UTC_I4(6), NAMB, NAMB_S, IQC, MJD, J1, J2
      REAL*8      TAU_GR, TAU_SIG, GAMB_SP, GAMB_SP_S, ARR_R8(64), &
     &            SEC, SEC_TAG, UTC_TAG, EPS_SEC
      PARAMETER  ( EPS_SEC = 0.1D0 )
      TYPE ( DT__STRU ) ::  DTR(MOBS)
      REAL*8      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! PI number
!
      INTEGER*2  INT2_ARG
!
! --- Openning database for the X- band
!
      IVER = 0
      CALL KAI ( INT2(1), INT2(0), INT2(0), INT2(6), DB_NAME, INT2(IVER), &
     &          'same      ', STR, I2, STR, KERR )
      IF ( KERR .NE. 0 ) THEN
           CALL CLRCH ( STR2 )
           CALL INCH  ( INT4(KERR), STR2 )
           CALL ERR_LOG ( 7901, IUER, 'READ_DB', 'Error KAI for database '// &
     &          DB_NAME//' KERR='//STR2 )
           RETURN
      ENDIF
!
! --- Reading the preface records of the database
!
      CALL MVREC ( INT2(1), INT2(1), INT2(1), KERR )
      IF ( KERR .NE. 0 ) THEN
           CALL CLRCH ( STR2 )
           CALL INCH  ( INT4(KERR), STR2 )
           CALL ERR_LOG ( 7902, IUER, 'READ_DB', 'Error KAI for database '// &
     &          DB_NAME//' KERR='//STR2 )
           RETURN
      ENDIF
!
! --- Getting the number of observations in hte databse for S-band
!
      CALL DBH_GETI4 ( 'NUMB OBS', NOBS, 1, 1, 1, -3 )
      DO 410 J1=1,NOBS
!
! ------ Reading record for J1-th observation
!
         CALL MVREC ( INT2(2), INT2(1), INT2(1), KERR )
         IF ( KERR .EQ. 1 ) GOTO 810  !  End of file ecountered
         IF ( KERR .NE. 0 ) THEN
              CALL CLRCH ( STR2 )
              CALL INCH  ( INT4(KERR), STR2 )
              WRITE ( 6, * ) ' J1=',J1
              CALL ERR_LOG ( 7903, IUER, 'READ_DB', 'Error KAI for '// &
     &            'database '//DB_NAME//'  KERR='//STR2 )
              RETURN
         ENDIF
         CALL DBH_GETCH ( 'BASELINE', BL,      2, 1,    -3 )
         CALL DBH_GETCH ( 'STAR ID ', SOUR,    1, 1,    -3 )
         CALL DBH_GETI4 ( 'UTC TAG4', UTC_I4,  6, 1, 1, -3 )
         CALL DBH_GETR8 ( 'SEC TAG ', SEC_TAG, 1, 1, 1, -3 )
         IF ( IMODE .EQ. 1 ) THEN
              WRITE ( UNIT=DATE_STR(1:10), FMT='(I4,".",I2,".",I2)' ) UTC_I4(1), &
     &                UTC_I4(2), UTC_I4(3)
              CALL  DATE_TO_TIME ( DATE_STR(1:10)//'-00:00:00.0', DTR(J1)%MJD, &
     &                             SEC, -3 )
!
              DTR(J1)%UTC_TAG= UTC_I4(4)*3600.0D0 + UTC_I4(5)*60.0D0 + SEC_TAG
!
              DTR(J1)%BAS_NAME = BL(1)//BL(2)
              DTR(J1)%SOU_NAME = SOUR
         END IF
!
         CALL DBH_GETCH ( 'QUALCODE', QC_STR,  1, 1,    -3 )
         CALL CHIN      (  QC_STR, IQC )
         IF ( IMODE .EQ. 1 ) THEN
              CALL DBH_GETI2 ( 'UACSUP  ', UACSUP,    1, 1, 1, -3 )
              CALL DBH_GETR8 ( 'GPDLAMBG', GAMB_SP,   1, 1, 1, -3 )
              CALL DBH_GETI4 ( '# AMBIG ', NAMB,      1, 1, 1, -3 )
!
              CALL DBH_GETR8 ( 'GRPAMB_S', GAMB_SP_S, 1, 1, 1, -3 )
              CALL DBH_GETI4 ( '#GAMBG_S', NAMB_S,    1, 1, 1, -3 )
         END IF
         CALL DBH_GETR8 ( 'DEL OBSV', ARR_R8,  2, 1, 1, -3 )
         TAU_GR = (ARR_R8(1) + ARR_R8(2))*1.D-6
         CALL DBH_GETR8 ( 'DELSIGMA', TAU_SIG, 1, 1, 1, -3 )
!
         IF ( IMODE .EQ. 1 ) THEN
              DTR(J1)%GRDEL_X    = TAU_GR
              DTR(J1)%GRSIG_X    = TAU_SIG
              DTR(J1)%GRDAMBSP_X = GAMB_SP
              DTR(J1)%NAMBSP_X   = NAMB
              DTR(J1)%GRDAMBSP_S = GAMB_SP_S
              DTR(J1)%NAMBSP_S   = NAMB_S
              DTR(J1)%QC_X       = IQC
              DTR(J1)%UACSUP     = UACSUP
            ELSE
!                 type *,' j1=',j1 ! %%
              WRITE ( UNIT=DATE_STR(1:10), FMT='(I4,".",I2,".",I2)' ) &
     &                UTC_I4(1), UTC_I4(2), UTC_I4(3)
              CALL  DATE_TO_TIME ( DATE_STR(1:10)//'-00:00:00.0', MJD, &
     &                             SEC, -3 )
              UTC_TAG= UTC_I4(4)*3600.0D0 + UTC_I4(5)*60.0D0 + SEC_TAG
              DO 420 J2=1,MOBS
                 IF ( DTR(J2)%MJD .EQ. MJD                           .AND. &
     &                DABS( DTR(J2)%UTC_TAG - UTC_TAG ) .LT. EPS_SEC .AND. &
     &                DTR(J2)%BAS_NAME(1:8)  .EQ. BL(1)              .AND. &
     &                DTR(J2)%BAS_NAME(9:16) .EQ. BL(2)              .AND. &
     &                DTR(J2)%SOU_NAME       .EQ. SOUR                    ) THEN
!
                      IF ( IMODE .EQ. 2 ) THEN
                           DTR(J2)%GRDEL_S    = TAU_GR
                           DTR(J2)%GRSIG_S    = TAU_SIG
                           DTR(J2)%QC_S       = IQC
                      END IF
                      GOTO 420
                 END IF
 420          CONTINUE
         END IF
 410  CONTINUE
 810  CONTINUE
!
      CALL FINIS ( INT2(0) )
!
      RETURN
      END  !#!  READ_DB  #!#
