      PROGRAM  MDLPL
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  MDLPL PROGRAM SPECIFICATION
!
! 1.1 MDLPL PLOTS THE ATMOSPHERIC MODELS DETERMINED BY SOLVE
!
! 1.2 REFERENCES:
!
! 2.  MDLPL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'mdlcm.i'
      INTEGER*2     LDBNAM(5,15), IDBV(15)
      INTEGER*4     IDBE(15)
      CHARACTER     CDBNAM(15)*10, DBNAME*16, HFEOP_CMP_FINAM*8
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: first,secnd,third,hard_reset,menu
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 ISTART(MAX_ARC_STA), INUMBER(MAX_ARC_STA)
      INTEGER*2 ISTARTC(MAX_ARC_STA), INUMBERC(MAX_ARC_STA)
      INTEGER*2 ISTART_EOP(3), INUMBER_EOP(3), IEXT_I2
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4 J1, IMODE, IUER, I_LEN
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      CHARACTER    STR*54
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! 4.  HISTORY
!
!  WHO   WHEN     WHAT
!  JWR  88.05.06  Logic error for multiple dbs fixed.
!  JWR  88.06.06  SOCAL call modified.
!  KDB  90.04.25  Plot earth orientation parameters
!  jwr  94.06.29  Batch features added.
!  pet  97.10.31  Support of MDLPL-extension added
!  pet  98.05.21  Added new feature: MDLPL_EXT doesn't print boxes of
!                 deselected stations.
!  pet  98.07.06  Added call of SET_PATHS in order to allow to use
!                 MDLPL_EXT even if envoronment variables PGPLOT_DIR,
!                 PGPLOT_FONT have not been set up
!  pet  1999.07.27   Added support of MDLPL_PLUS
!
! 5.  MDLPL PROGRAM STRUCTURE
!
!C
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL PRE_PROG()
      CALL SET_PATHS()
      INCLUDE 'mdlpl_version.i' ! Set revision date of the current version
      CALL CURLIB_SET_TERM ( PTR_CH(PRE_SCR_DIR(:PRE_SD_LEN)//'term'// &
     &                       PRE_LETRS//CHAR(0)) )
      CALL START_MN()
      BATCH_CONTROL = .FALSE.
      RUN_ID        = '10000'
      USER_INITIALS = PRE_LETRS
      CALL USE_BUFFER ( IEXT_I2, INT2(1), 'ORC' )
!
! --- Open and read the site and star names from PARFIL and get SOCOM.
!
      CALL USE_PARFIL ( 'ORC' )
      CALL USE_COMMON ( 'ORC' )
      CALL USE_GLBFIL_4  ( 'ORC' )
!
 910  CONTINUE
      IF ( IEXT_I2 .EQ. 0 ) THEN
!
! -------- Interact to get the users desires.
!
           CALL MENU()
         ELSE IF ( IEXT_I2 .EQ. 1 ) THEN
!
! -------- Declaring compare hf EOP model
!
           HFEOP_CMP_FILE_NAME = HFEOP_CMP_DEF
           HFEOP_CMP_FINAM     = HFEOP_CMP_DEF
      END IF
!
      CALL FIRST ( ISTART, INUMBER, ISTARTC, INUMBERC, &
     &             ISTART_EOP, INUMBER_EOP, IWHICH, BTC )
!
      CALL SECND ( ISTART, INUMBER, ISTARTC, INUMBERC, ISTART_EOP, &
     &             INUMBER_EOP )
      IF ( IEXT_I2 .EQ. 0 ) THEN
           CALL THIRD ( INUMBERC )
           CALL HARD_RESET()
         ELSE IF ( IEXT_I2 .EQ. 1 ) THEN
!
! -------- MDLPL-extension
!
           CALL CLRCH ( DBNAME )
           CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
           DO 410 J1=1,NUMDB
              IF ( KBIT(IDBSEL,INT2(J1)) ) THEN
!
! ---------------- Forming the string with database name
!
                   DBNAME = CDBNAM(J1)
                   DBNAME(12:) = '<'
                   CALL INCH ( INT4( IDBV(J1) ), DBNAME(13:) )
                   DBNAME( I_LEN(DBNAME)+1: ) = '>'
              END IF
 410       CONTINUE
!
           IMODE = 2
           IUER  = -1
           CALL MDLPL_EXT ( IMODE, DBNAME, HFEOP_CMP_FINAM, WEB_DIR, &
     &          MDLPL_IPS_PAG, MDLPL_IPC_PAG, MDLPL_FL_EOPMOD, MDLPL_FL_CLF, &
     &          MDLPL_FL_FRAME, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 9200, -1, 'MDLPL', 'Errors '// &
     &              'occurred during attempt to display plots' )
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
!
! -------- Store parameters which might have been changed
!
           CALL USE_GLBFIL_4  ( 'OWC' )
!
           IF ( IMODE .EQ. 0 ) THEN
                IEXT_I2 = 0
                CALL START_MN()
                GOTO 910
           END IF
      END IF
!
      CALL USE_GLBFIL_4  ( 'OWC' )
      CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
!
      END  !#!  MDLPL  #!#
