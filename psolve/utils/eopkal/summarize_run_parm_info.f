      SUBROUTINE summarize_run_parm_info(linfile,loutfile, &
     & lerrfile,ldubfile, linfile_type, &
     & kuse_rate,kmon,kneos_iris,kut1s, &
     & kpm_annual,kpm_linear,kut_annual,kut_semi, &
     & sig1_max,sig_scale,time_step, &
     & idate_start,idate_end,idate_first,idate_last, &
     &     num_rejects, num_doubles,num_tot)
!
!
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER LDUM*256
!
      character*64 linfile,loutfile,lerrfile,ldubfile  !various files
      CHARACTER*10 linfile_type              ! IRIS, EOPJMG, etc.
      LOGICAL*2 kuse_rate(3)                  ! Do we use rate info?
      LOGICAL*2 kmon                          ! monitor progress
!     Updated to specificaly type integers which
!-------------------------------------------------
      LOGICAL*2 kneos_iris                    ! use only EOP stuff
      LOGICAL*2 kut1s                         ! ut1s removed and added
! The following few describe the filter model.
      LOGICAL*2 kpm_annual                    !Include annual term in PM?
      LOGICAL*2 kpm_linear                    !Include linear term
      LOGICAL*2 kut_annual                    !Include annual term in UT1
      LOGICAL*2 kut_semi                      !Include seasonal term in UT1
      REAL*8        sig1_max          ! maximum value for this
      REAL*8        sig_scale         ! Scale input formal errors by this
      INTEGER*4 idate_start,idate_end       ! limits of data we use
      INTEGER*4  idate_first,idate_last     ! actual limits of data.
      REAL*8     time_step            ! mod file spacing in days
      INTEGER*4 num_rejects                 ! number of points rejected
      INTEGER*4 num_doubles                 ! number of simultaneous measurements
      INTEGER*4 num_tot                     ! total number of measurements
!
! local variables
      CHARACTER*6 LEOP(3)/"X-Pole", "Y-Pole", "UT1"/
      INTEGER*4 i                           ! Counter
!
      WRITE ( LDUM, '(A)' ) "# -----Summary of eop_kal run-------"
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '(A)' ) "# "
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '(A)'   ) "# Input file:   "//LINFILE
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '(A)'   ) "# Format of the input file: "//LINFILE_TYPE
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '(A,I8)' ) "# First date read in: ", IDATE_FIRST
      CALL OUTPUT_LDUM ( LDUM )
!
!
      WRITE ( LDUM, '(A,I8)' ) "# Last date read  in: ", IDATE_LAST
      CALL OUTPUT_LDUM ( LDUM )
!
!
      WRITE ( LDUM, '(A,I6)' ) "# Total # read in:    ", NUM_TOT
      CALL OUTPUT_LDUM ( LDUM )
!
!
      WRITE ( LDUM, '(A,I6)' ) "#       # rejects:    ", NUM_REJECTS
      CALL OUTPUT_LDUM ( LDUM )
!
!
      WRITE ( LDUM, '(A,I6)' ) "#       # doubles:    ", NUM_DOUBLES
      CALL OUTPUT_LDUM ( LDUM )
!
!
      WRITE ( LDUM, '(A,A)' ) "# Output file: ", LOUTFILE
      CALL OUTPUT_LDUM ( LDUM )
!
!
      WRITE ( LDUM, '(A,A)' ) "# Reject file: ", LERRFILE
      CALL OUTPUT_LDUM ( LDUM )
!
!
      WRITE ( LDUM, '(A,A)' ) "# Double file: ", LDUBFILE
      CALL OUTPUT_LDUM ( LDUM )
!
! --- Output characteristics of run.
!
      IF ( KNEOS_IRIS ) THEN
           WRITE ( LDUM, '(A)' ) "# Used only Polaris, IRIS, NAVNET and NEOS."
         ELSE
           WRITE ( LDUM, '(A)' ) "# Used all experiment types."
      ENDIF
      CALL OUTPUT_LDUM ( LDUM )
!
!
      IF ( KUT1S ) then
           WRITE ( LDUM, '(A)' ) "# Removed UT1S before smoothing"
         ELSE
           WRITE ( LDUM, '(A)' ) "# Did not remove UT1S before smoothing"
      ENDIF
      CALL OUTPUT_LDUM ( LDUM )
!
      DO I=1,3
         IF ( KUSE_RATE(I) ) THEN
              WRITE ( LDUM, '(A,A,A)' ) "# Used ",LEOP(I), " rates."
              CALL OUTPUT_LDUM ( LDUM )
         ENDIF
      END DO
!
      WRITE ( LDUM, '("# Input formal errors rescaled by: ", f8.2)' ) sig_scale
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '("# Sig1_max cutoff value:           ", f8.2)' ) sig1_max
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '("# Mod_file spacing in days:        ", f8.2)' ) time_step
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '(A)' ) "# Filter had the following characteristics:"
      CALL OUTPUT_LDUM ( LDUM )
!
      WRITE ( LDUM, '(A)' ) "# Normal UT1+LOD and PM Stochastic model"
      CALL OUTPUT_LDUM ( LDUM )
!
      IF ( KPM_ANNUAL ) THEN
           WRITE ( LDUM, '(A)' ) "# Annual term included in PM model"
           CALL OUTPUT_LDUM ( LDUM )
      ENDIF
!
      IF ( KPM_LINEAR ) THEN
           WRITE ( LDUM, '(A)' ) "# Linear term included in PM model"
           CALL OUTPUT_LDUM ( LDUM )
      ENDIF
!
      IF ( KUT_ANNUAL ) THEN
           WRITE ( LDUM, '(A)' ) "# Annual term included in UT model"
           CALL OUTPUT_LDUM ( LDUM )
      ENDIF
!
      IF ( KUT_SEMI ) THEN
           WRITE ( LDUM, '(A)' ) "# Semi-annual term included in UT model"
           CALL OUTPUT_LDUM ( LDUM )
      ENDIF
!
      RETURN
      END  !#!  SUMMARIZE_RUN_PARAM_INFO  #!#
