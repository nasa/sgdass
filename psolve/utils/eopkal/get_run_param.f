      SUBROUTINE GET_RUN_PARM ( LINFILE, LOUTFILE, LERRFILE, LDUBFILE, &
     &                          LLODFILE, LPLOTFILE, LINFILE_TYPE, &
     &                          LSTSFILE, NUM_EOP, &
     &                          KUSE_RATE, KMON, KNEOS_IRIS, KUT1S, KLOD, &
     &                          KPLOT, KPM_ANNUAL, KPM_LINEAR, KUT_ANNUAL, &
     &                          KUT_SEMI, SIG1_MAX, SIG_SCALE, TIME_STEP, &
     &                          IDATE_START, IDATE_END )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      CHARACTER*64 LINFILE, LOUTFILE, LERRFILE, LDUBFILE  ! Various files
      CHARACTER*64 LPLOTFILE
      CHARACTER*64 LLODFILE
      CHARACTER*10 linfile_type             ! type of input file: IERS, EOPJMG
      INTEGER*2 num_eop                     ! number of EOP parameters we use
!     Updated to specificaly type integers which
!-------------------------------------------------
      LOGICAL*2 kuse_rate(3)                  ! Do we use rate info?
      LOGICAL*2 kmon                          ! monitor progress
      LOGICAL*2 kneos_iris                    ! use only EOP stuff
      LOGICAL*2 kut1s                         ! ut1s removed and added
      LOGICAL*2 klod                          ! output lod?
      LOGICAL*2 kplot                         ! output plot file?
!
! The following few describe the filter model.
!
      LOGICAL*2 kpm_annual                    !Include annual term in PM?
      LOGICAL*2 kpm_linear                    !Include linear term
      LOGICAL*2 kut_annual                    !Include annual term in UT1
      LOGICAL*2 kut_semi                      !Include seasonal term in UT1
!
! End of the filter model.
!
      REAL*8        sig1_max          ! maximum value for this
      REAL*8        sig_scale         ! Scale input formal errors by this
      INTEGER*4 idate_start,idate_end       ! limits of data we use
      REAL*8     time_step            ! mod file spacing in days
      LOGICAL*4  FL_INT, FL_INP, FL_OUT, FL_STS
!
      CHARACTER*6 LEOP(3)/"X-Pole", "Y-Pole", "UT1"/
      INTEGER*2   I, J1, NUMARG
!
! local variables
!
      LOGICAL*4 KEXIST                      ! Does file exist?
      CHARACTER LCHAR*1                     ! Holds Y/N response.
      CHARACTER LDUM*80                     ! dummy string buffer
      CHARACTER GET_CDATE*19
      INTEGER*2 IYEAR, IMONTH, IDAY
      CHARACTER STR*128, PID_STR*8, FIL*128, LSTSFILE*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      INTEGER*4 IP, IU
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
#ifdef INTEL
      INTEGER*4, EXTERNAL :: IARGC
#endif
      INTEGER*4, EXTERNAL :: GETPID, I_LEN
!
!  pet  2004.10.28  Changed file names: added PID and username
!
      NUMARG = IARGC ()
      IF ( NUMARG .EQ. 1 ) THEN
           CALL GETARG ( 1, STR )
           IF ( STR(1:2) .EQ. '-h'      .OR.  STR(1:2)  .EQ.  '-H' .OR. &
     &          STR(1:6) .EQ. '--help' ) THEN
!
                WRITE ( 6, * ) 'Usage: eopkal -i <input_file> '// &
     &                         '-o <output_file> -s <status_file>'
                WRITE ( 6, * ) ' '
                WRITE ( 6, * ) '      <input_file> may be in EOB, EOPS or '// &
     &                         'EOPJMG format'
                WRITE ( 6, * ) '      <putput_file> is in ERP format '
                CALL EXIT ( 0 )
           END IF
      END IF
!
      IP = GETPID ()
      CALL INCH   ( IP,    PID_STR )
      CALL CHASHR (        PID_STR ) 
      CALL BLANK_TO_ZERO ( PID_STR )
      CALL GETINFO_USER  ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      IU = I_LEN(USER_NAME)
      IF ( IU .EQ. 1 ) USER_NAME = '1'
!
      IF ( NUMARG .GE. 6 ) THEN
           FL_INT = .FALSE.
           FL_INP = .FALSE.
           FL_OUT = .FALSE.
           FL_STS = .FALSE.
           DO 410 J1=1,3
              CALL GETARG ( (J1-1)*2+1, STR )
              CALL GETARG ( (J1-1)*2+2, FIL )
              IF ( STR(1:2) .EQ. '-i' ) THEN
                   FL_INP = .TRUE.
                   LINFILE = FIL
                 ELSE IF ( STR(1:2) .EQ. '-o' ) THEN
                   FL_OUT = .TRUE.
                   LOUTFILE = FIL
                 ELSE IF ( STR(1:2) .EQ. '-s' ) THEN
                   FL_STS = .TRUE.
                   LSTSFILE = FIL
              END IF
 410       CONTINUE
           IF ( .NOT. FL_INP ) THEN
                WRITE ( *, * ) 'Input file was not supplied. Check command line'
                CALL EXIT ( 2 )
           END IF
           IF ( .NOT. FL_OUT ) THEN
                WRITE ( *, * ) 'Output file was not supplied. Check command '// &
     &                         'line'
                CALL EXIT ( 2 )
           END IF
           IF ( .NOT. FL_STS ) THEN
                WRITE ( *, * ) 'Status file was not supplied. Check '// &
     &                         'command line'
                CALL EXIT ( 2 )
           END IF
           INQUIRE ( FILE=LINFILE, EXIST=KEXIST )
           IF ( .NOT. KEXIST ) THEN
                IF ( .NOT. KEXIST ) WRITE ( *, * ) 'Input EOP file '// &
     &                LINFILE(1:I_LEN(LINFILE))//'was not found. '
                CALL EXIT ( 2 )
           END IF
           LCHAR = 'Y'
         ELSE
!
! -------- Interactive case
!
           FL_INT = .TRUE.
!
! -------- Set up input, output files.
!
           KEXIST = .FALSE.
           DO WHILE (.NOT. KEXIST )
              WRITE ( *, * ) 'Input file name: '
              READ  ( *, '(A)' ) LINFILE
              IF ( LINFILE .EQ. "quit" .OR. LINFILE .EQ. "QUIT" ) THEN
                   WRITE ( *, * ) "Stopping at user request."
              ENDIF
              INQUIRE ( FILE=LINFILE, EXIST=KEXIST )
              IF ( .NOT. KEXIST ) WRITE ( *, * ) "File not found. "// &
     &                                           "Please try again."
          END DO
!
          WRITE ( *, * ) 'Output file name: '
          READ  ( *, '(A)' ) LOUTFILE
          LSTSFILE = '/tmp/eopkal_'//USER_NAME(1:IU)//'_'//PID_STR//'.sts'
          WRITE( * , * ) "Use default setup (Y/N): "
          READ ( *, * ) LCHAR
      END IF
!
! --- Open input file
!
      OPEN ( 1, FILE=LINFILE, STATUS="OLD" )
!
! --- See what kind of file input file is:
!
      READ ( 1, '(A80)' ) LDUM
      IF ( LDUM(1:12) .EQ. "yyyymmddhhmm" ) THEN
           LINFILE_TYPE = "EOPJMG"
        ELSE IF ( LDUM(1:32) .EQ. '# GETPAR_EOB format version 2.0 ' ) THEN
           LINFILE_TYPE = "EOPB"
           READ ( 1, '(A80)' ) LDUM
        ELSE IF ( LDUM(1:46) .EQ. '# GETPAR_EOB format version 2.1  of 2007.08.30' ) THEN
           LINFILE_TYPE = "EOPB"
           READ ( 1, '(A80)' ) LDUM
        ELSE IF ( LDUM(1:1) .NE. '#' .AND. INDEX ( LDUM, "IERS" ) .NE. 0 ) THEN
           LINFILE_TYPE = "IERS"
        ELSE
           LINFILE_TYPE = "BEATS ME"
           WRITE ( *,* ) 'Unknown type of file '//LINFILE(1:I_LEN(LINFILE))
           WRITE ( *,* ) 'First line: '//LDUM(1:I_LEN(LDUM))
           CALL EXIT ( 5 )
      ENDIF
!
! --- Open output file
!
      OPEN ( UNIT=2, FILE=LOUTFILE, STATUS='UNKNOWN' )
!
! --- Write status: start in LSTSFILE
!
      OPEN ( UNIT=3, FILE=LSTSFILE, STATUS='UNKNOWN' )
      WRITE ( 3, '(A)' ) "EOPKAL: started at "//GET_CDATE()
      CLOSE ( UNIT = 3 )
!
! --- Now start setting up how to do the run.
!
      IF ( LCHAR .EQ. "y" .OR. LCHAR .EQ. "Y" ) THEN
           LERRFILE  = "/tmp/eop_kal_"//USER_NAME(1:IU)//'_'//PID_STR//".err"
           LDUBFILE  = "/tmp/eop_kal_"//USER_NAME(1:IU)//'_'//PID_STR//".dub"
           LPLOTFILE = "/tmp/eop_kal_"//USER_NAME(1:IU)//'_'//PID_STR//".plt"
           LLODFILE  = "/tmp/lod_"//USER_NAME(1:IU)//'_'//PID_STR//".dat"
           WRITE ( *, * ) "Error  file is: ", LERRFILE
           WRITE ( *, * ) "Double file is: ", LDUBFILE
           WRITE ( *, * ) "Plot file is:   ", LPLOTFILE
           WRITE ( *, * ) "LOD  file is:   ", LLODFILE
           WRITE ( *, * ) "EOP type  is:   ", LINFILE_TYPE
           KLOD = .TRUE.
           KPLOT = .TRUE.
           KMON = .FALSE.
           KNEOS_IRIS  = .FALSE.
           SIG1_MAX    = 5.0
           SIG_SCALE   = 1.5
           IDATE_START = 0
!
!                     xYYYYMMDD
!
           IDATE_END = 999999999
           WRITE ( *, * ) "Using all data with nominal constriants."
           WRITE ( *, * ) "Mod file spacing is 1 day."
           TIME_STEP = 1.D0
           KUT1S = .TRUE.
!
! -------- Use ut1_rates if we have them and if we want to use
!
           IF ( LINFILE_TYPE .EQ. "EOPJMG" ) THEN
                NUM_EOP = 4
                KUSE_RATE    = .FALSE.
                KUSE_RATE(3) = .TRUE.
              ELSE IF ( LINFILE_TYPE .EQ. "EOPB" ) THEN
                NUM_EOP = 3
                KUSE_RATE    = .FALSE.
                KUSE_RATE(3) = .FALSE.
              ELSE
                NUM_EOP = 3
           ENDIF
         ELSE
!
! -------- Not default
!
           WRITE ( *, * ) 'Error file name:  '
           READ  ( *, '(A)' ) LERRFILE
!
           WRITE ( *, * ) 'Double file name:  '
           READ  ( *, '(A)' ) ldubfile
           WRITE ( *, * ) "Make lod file (Y/N)? "
           READ ( *, * )  LCHAR
           KLOD = LCHAR .EQ. "y" .OR. LCHAR .EQ. "Y"
           IF ( KLOD ) THEN
                WRITE ( *, * ) "Enter in lod file name: "
                READ ( *, *  ) LLODFILE
           ENDIF
!
           WRITE ( *, * ) "Make plot file (Y/N)? "
           READ  ( *, * ) LCHAR
           KPLOT = LCHAR .EQ. "y" .OR. LCHAR .EQ. "Y"
           IF ( KPLOT ) THEN
                WRITE ( *, * ) "Enter in plot file name: "
                READ ( *, * ) LPLOTFILE
           ENDIF
!
           WRITE ( *, * ) "Monitor Progress?"
           READ ( *, * ) LCHAR
           KMON = LCHAR .EQ. "y" .OR. LCHAR .EQ. "Y"
!
           WRITE ( *, * ) "Use only NAVNET, Polaris, IRIS (y/n)? "
           READ ( *, * ) LCHAR
           KNEOS_IRIS = LCHAR .EQ. "y" .OR. LCHAR .EQ. "Y"
!
           WRITE ( *, * ) "Sig1 uncertainty? (99 no constraints, 5 is nominal)"
           READ ( *, * ) SIG1_MAX
!
           WRITE ( *, * ) "Rescale  formal errors by how much? "
           READ  ( *, * ) SIG_SCALE
!
           WRITE ( *, * ) "Enter in starting date (YYYY MM DD): "
           READ  ( *, * ) IYEAR, IMONTH, IdAY
           IDATE_START = ( IYEAR*10000 + IMONTH*100 + IDAY )
!
           WRITE ( *, * ) "Enter in ending date (YYYY MM DD): "
           WRITE ( *, * ) " (end date <= start date uses all data) "
           READ  ( *,  * ) IYEAR, IMONTH, IdAY
           IDATE_END = IYEAR*10000 + IMONTH*100 + IDAY
!
           IF ( IDATE_END .LE. IDATE_START ) THEN
                WRITE ( *, * ) "Ending date before starting date."
                WRITE ( *, *  )"Assuming you want to go to end of data."
!
! ------------- Put on an extra 9 for year 2000 problem.
!                           xYYYYMMDD
                idate_end = 999999999
           ENDIF
!
           WRITE ( *, * ) "Do UT1S pre-smoothing (Y/N): "
           READ ( *, * ) LCHAR
           KUT1S = LCHAR .EQ. "y" .OR. LCHAR .EQ. "Y"
!
           TIME_STEP = 0.0
           DO WHILE ( TIME_STEP .GT. 1.0 .OR. TIME_STEP .LT. 0.05 )
              WRITE ( *, * ) "Spacing of mod file in days: (0.05-1)"
              READ  ( *, * ) TIME_STEP
              TIME_STEP = INT(TIME_STEP*100.)/100.
           END DO
!
! -------- Set up to use x_dot,y_dot,ut1_dot
!
           NUM_EOP=3
           KUSE_RATE=.FALSE.
           IF ( LINFILE_TYPE .EQ. "EOPJMG" ) THEN
!
! ------------- At the moment only have option for UT1.
! ------------- (EOPJMG only gives UT1 rates.)
!
                DO I = 3, 3
                   WRITE ( *, * ) "Are you going to use ", leop(i), " rates? "
                   READ ( *, * ) LCHAR
                   KUSE_RATE ( I )  = LCHAR .EQ. "y" .OR. LCHAR .EQ. "Y"
                   IF ( KUSE_RATE(I) ) NUM_EOP = NUM_EOP + 1
               ENDDO
           ENDIF
      ENDIF
!
      KPM_ANNUAL = .FALSE.
      KPM_LINEAR = .FALSE.
      KUT_ANNUAL = .FALSE.
      KUT_SEMI   = .FALSE.
      KMON       = .TRUE.
!
      RETURN
      END  !#!  GET_RUN_PARM  #!#
