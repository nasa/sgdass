      PROGRAM    SUR_SKED_LAUNCH
      IMPLICIT   NONE 
      CHARACTER  STR*128
      INTEGER*8    STACK_SIZE_IN_GIGABYTES, STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( STACK_SIZE_IN_GIGABYTES = 2 )
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*4, EXTERNAL :: ILEN
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL GETENVAR ( 'GOMP_STACKSIZE', STR )
      IF ( ILEN(STR) == 0 ) THEN
            CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      END IF
!
      CALL SUR_SKED_MAIN()
      END  PROGRAM  SUR_SKED_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_SKED_MAIN()
! ************************************************************************
! *                                                                      *
! *   Program SUR_SKED_MAIN is the main unit for scheduing survey style  *
! *   observing sessions at small networks.                              *
! *                                                                      *
! * ### 10-OCT-2005  SUR_SKED_MAIN  v2.8 (c) L. Petrov  27-SEP-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE ), POINTER :: SUR(:)
      TYPE     ( VTD__TYPE ), POINTER :: VTD(:)
      CHARACTER  EXP_FIL*128, STR*128, ANS*128
      INTEGER*4  IVRB, IVRB__DEF, IUER
!
      INTEGER*8    STACK_SIZE_IN_GIGABYTES, STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( STACK_SIZE_IN_GIGABYTES = 4 )
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IARGS
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      IVRB__DEF = 4
!
      CALL SET_SIGNAL_CTRLC ( 1 )
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) SUR__LABEL
           WRITE ( 6, '(A)' ) 'Usage: sur_sked <sur_control_file} {verbosity_level}'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, EXP_FIL )
           IF ( IARGC() .GE. 2 ) THEN
                CALL GETARG ( 2, STR     )
                CALL CHIN   ( STR, IVRB  )
              ELSE
                IVRB = IVRB__DEF
           END IF
      END IF
!
      CALL NERS_VERSION ( 'NERS__LABEL', ANS )
      IF ( ANS .NE. NERS__LABEL ) THEN
           CALL ERR_LOG ( 1681, IUER, 'SUR_SKED_MAIN', 'Trap of internal '// &
     &         'control: sur_sked was compiled against '//NERS__LABEL// &
     &         ', but linked against '//TRIM(ANS)//'. Please recompile sur_sked' )
           RETURN 
      END IF
!
      ALLOCATE ( SUR(1) )
      ALLOCATE ( VTD(1) )
      CALL NOUT ( SIZEOF(SUR(1)), SUR )
      CALL NOUT ( SIZEOF(VTD(1)), VTD )
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) SUR__LABEL
      END IF
!
! --- Parse configuration file
!
      IUER = -1
      CALL SUR_SKED_CONF ( EXP_FIL, SUR, VTD, IVRB, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IF ( SUR(1)%ALGORITHM == 'FRINGE_SEARCH_01' .OR. &
     &     SUR(1)%ALGORITHM == 'FRINGE_SEARCH_02'      ) THEN
!
! ======== Fringe survey IVRB
!
! -------- Get the first observation
!
           IUER = -1
           CALL SUR_FIND_FIRST ( SUR, VTD, SUR(1)%MJD_START, &
     &                           SUR(1)%TAI_START+SUR(1)%PRESES_INTERVAL, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! -------- Get second and all further scans
!
           IUER = -1
           CALL SUR_FIND_SEQ   ( SUR, VTD, SUR(1)%MJD_STOP, SUR(1)%TAI_STOP, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE IF ( SUR(1)%ALGORITHM == 'ASTROMET_01'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_02'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_03'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_04'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_05'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_06'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_07'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_11'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_12'   .OR.  &
     &             SUR(1)%ALGORITHM == 'ASTROMET_13'   .OR.  &
     &             SUR(1)%ALGORITHM == 'GEODETIC_01'   .OR.  &
     &             SUR(1)%ALGORITHM == 'GEODETIC_02'   .OR.  &
     &             SUR(1)%ALGORITHM == 'GEODETIC_03'   .OR.  &
     &             SUR(1)%ALGORITHM == 'GNSS_01'       .OR.  &
     &             SUR(1)%ALGORITHM == 'GNSS_02'       .OR.  &
     &             SUR(1)%ALGORITHM == 'SPACECRAFT_01' .OR.  &
     &             SUR(1)%ALGORITHM == 'IMAGING_01'    .OR.  &
     &             SUR(1)%ALGORITHM == 'IMAGING_S1'          ) THEN
!
! -------- Astrometric or geodetic schedule
!
           IUER = -1
           CALL SUR_ASTROMET ( SUR, VTD, IVRB, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END IF
!
! --- Print results
!
      IUER = -1
      CALL SUR_PRINT_RES ( SUR, VTD, IVRB, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( SUR(1)%ALGORITHM == 'ASTROMET_01'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_02'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_03'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_04'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_05'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_06'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_07'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_11'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_12'   .OR.  &
     &     SUR(1)%ALGORITHM == 'ASTROMET_13'   .OR.  &
     &     SUR(1)%ALGORITHM == 'GEODETIC_01'   .OR.  &
     &     SUR(1)%ALGORITHM == 'GEODETIC_02'   .OR.  &
     &     SUR(1)%ALGORITHM == 'GEODETIC_03'   .OR.  &
     &     SUR(1)%ALGORITHM == 'GNSS_01'       .OR.  &
     &     SUR(1)%ALGORITHM == 'GNSS_02'       .OR.  &
     &     SUR(1)%ALGORITHM == 'SPACECRAFT_01' .OR.  &
     &     SUR(1)%ALGORITHM == 'IMAGING_01'    .OR.  &
     &     SUR(1)%ALGORITHM == 'IMAGING_S1'          ) THEN
!
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * )
                WRITE ( 6, '(A)' ) 'Compute statistics'
           END IF
!
           IUER = -1
           CALL SUR_ASTRO_STAT ( SUR, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 1685, -2, 'SUR_SKED_MAIN', 'Error in attempt '// &
     &              'to create the file with the schedule statistics' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, * ) 'sur_sked: Output plan file: ', SUR(1)%OUT_PLAN(1:I_LEN(SUR(1)%OUT_PLAN))
           WRITE ( 6, * ) 'sur_sked: Output vex  file: ', SUR(1)%OUT_VEX(1:I_LEN(SUR(1)%OUT_VEX))
           WRITE ( 6, * ) 'sur_sked: Output stat file: ', SUR(1)%OUT_STAT(1:I_LEN(SUR(1)%OUT_STAT))
           WRITE ( 6, * ) 'sur_sked: Output list file: ', SUR(1)%OUT_SOU_LIST(1:I_LEN(SUR(1)%OUT_SOU_LIST))
           WRITE ( 6, * ) 'sur_sked: Output key  file: ', SUR(1)%OUT_KEY(1:I_LEN(SUR(1)%OUT_KEY))
           WRITE ( 6, * ) 'sur_sked: Output ast  file: ', SUR(1)%OUT_AST(1:I_LEN(SUR(1)%OUT_AST))
           WRITE ( 6, * ) 'sur_sked: Number of scans:  ', SUR(1)%L_SCN
           WRITE ( 6, * ) 'sur_sked: Number of prim.:  ', SUR(1)%L_SCN_SO1
           WRITE ( 6, * ) 'sur_sked: Number of secnd.: ', SUR(1)%L_SCN_SO2
           WRITE ( 6, * ) 'sur_sked: Number of cals:   ', SUR(1)%L_SCN_CAL
           WRITE ( 6, * ) 'sur_sked: successful termination'
      END IF
      CALL EXIT ( 0 )
      END  SUBROUTINE  SUR_SKED_MAIN  !#!#
