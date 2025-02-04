      SUBROUTINE GETENVAR ( NAME, VALUE ) 
      IMPLICIT   NONE 
      CHARACTER  NAME*(*), VALUE*(*)
#ifdef GNU
      INTRINSIC  GETENV
#endif
#if defined (INTEL) || defined (SUN)
      CALL GETENV_ ( NAME, VALUE ) 
#else
      CALL GETENV ( NAME, VALUE ) 
#endif      
      RETURN
      END  SUBROUTINE GETENVAR 
!
! ---------------------------------------------------------------------------
!
      FUNCTION UNIT_TO_FILDESC ( UNIT )
! ************************************************************************
! *                                                                      *
! *   Function  UNIT_TO_FDESC returns Unix file descriptor of the opened *
! *   file associated with Fortran I/O unit. If the file is not opened   *
! *   then  UNIT_TO_FILDESC returns -1 .                                 *
! *                                                                      *
! * ### 05-JUL-2003  UNIT_TO_FILDESC  v1.1 (c) L. Petrov 06-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      INTEGER*4  UNIT_TO_FILDESC, UNIT
      INTEGER*4  FNUM, GETFD, IERR
#ifdef GNU
      INTRINSIC  FNUM
#endif
#ifdef INTEL
      CALL PXFFILENO_ ( UNIT, UNIT_TO_FILDESC, IERR )
      IF ( IERR .NE. 0 ) UNIT_TO_FILDESC = -1
#endif
#if defined (HPUX) || defined (GNU)
      UNIT_TO_FILDESC = FNUM ( UNIT )
#endif
#ifdef SUN
      WRITE ( 6, * ) 'Trap of internal control: function getfd is missing from '
      WRITE ( 6, * ) 'libF77.a library. Blame Sun!'
      CALL EXIT ( 1 ) 
      UNIT_TO_FILDESC = -1
#endif
      RETURN
      END  FUNCTION  UNIT_TO_FILDESC 
!
! ------------------------------------------------------------------------
!
      FUNCTION   INT8_TO_I4 ( IARG_8 )
      INTEGER*4  INT8_TO_I4 
      INTEGER*8  IARG_8 
      INT8_TO_I4 = IARG_8
      RETURN
      END  FUNCTION  INT8_TO_I4 
!
! ---------------------------------------------------------------------------
!
#if defined INTEL
      FUNCTION   FOR_ETIME ( TARG_R4 )
      REAL*4     FOR_ETIME
      REAL*4     TARG_R4(2)
      FOR_ETIME = ETIME ( TARG_R4 )
      RETURN
      END  FUNCTION   FOR_ETIME 
!
! ---------------------------------------------------------------------------
!
      FUNCTION   ETIME ( TARG_R4 )
      REAL*4     ETIME
      REAL*4     TARG_R4(2)
      ETIME = ETIME_ ( TARG_R4 )
      RETURN
      END  FUNCTION   ETIME 
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE FDATE ( DATE_CHR )
      CHARACTER  DATE_CHR*(*)
      CALL FDATE_ ( DATE_CHR ) 
      RETURN
      END  SUBROUTINE FDATE  
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE FLUSH ( LUNIT )
      INTEGER*4  LUNIT
      CALL FLUSH_ ( LUNIT ) 
      RETURN
      END  SUBROUTINE  FLUSH  
!
! ---------------------------------------------------------------------------
!
      FUNCTION   IARGC ()
      IMPLICIT   NONE 
      INTEGER*4  IARGC, IARGC_
      IARGC = IARGC_()
      RETURN
      END  FUNCTION   IARGC
!
! ---------------------------------------------------------------------------
!
      FUNCTION   RAN ( ISEED )
      IMPLICIT   NONE 
      REAL*4     RAN, RAN_
      INTEGER*4  ISEED
      RAN = RAN_ ( ISEED )
      RETURN
      END  FUNCTION  RAN
!
! ---------------------------------------------------------------------------
!
      FUNCTION   FTELL ( LUN )
      IMPLICIT   NONE 
      INTEGER*4  FTELL, FTELL_, LUN
      FTELL = FTELL_ ( LUN )
      RETURN
      END  FUNCTION  FTELL
!
! ------------------------------------------------------------------------
!
      FUNCTION   SECNDS ( ARG_R4 ) 
      REAL*4     SECNDS, ARG_R4
      REAL*4     SECNDS_
      SECNDS = SECNDS_ ( ARG_R4 )
      RETURN
      END  FUNCTION  SECNDS 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE IDATE ( IM, ID, IY )
      INTEGER*4  IM, ID, IY
      CALL IDATE_ ( IM, ID, IY )
      RETURN
      END  SUBROUTINE IDATE
#endif
!
! -------------------------------------------------------------------------
!
#if defined (SUN) || defined (HPUX) || defined (GNU)
      FUNCTION   FOR_ETIME ( TARG_R4 )
      REAL*4     FOR_ETIME
      REAL*4     TARG_R4(2)
#ifdef GNU
      REAL*4,    INTRINSIC :: ETIME
#endif
      FOR_ETIME = ETIME ( TARG_R4 )
      RETURN
      END  FUNCTION   FOR_ETIME 
#endif
!
! ------------------------------------------------------------------------
!
#if defined (SUN) || defined (GNU)
      FUNCTION   INT1 ( IARG_4 )
      INTEGER*1  INT1
      INTEGER*4  IARG_4
      INT1 = IARG_4
      RETURN
      END  FUNCTION  INT1
!
! ------------------------------------------------------------------------
!
      FUNCTION  INT4 ( IARG_2 )
      INTEGER*1 INT4
      INTEGER*2 IARG_2
      INT4 = IARG_2
      RETURN
      END  FUNCTION  INT4
#endif
!
! ---------------------------------------------------------------------------
!
      FUNCTION   FOR_STAT ( FILE_NAME, STATB )
      INTEGER*4  FOR_STAT 
      INTEGER*4  STATB(16) 
      CHARACTER  FILE_NAME*(*)
#ifdef GNU
      INTEGER*4, INTRINSIC :: STAT
#else 
      INTEGER*4  STAT, STAT_
#endif
#if defined (INTEL) || defined (SUN)
      FOR_STAT = STAT_ ( FILE_NAME, STATB )
#else
      FOR_STAT = STAT  ( FILE_NAME, STATB )
#endif
      RETURN
!
! ---------------------------------------------------------------------------
!
      END  FUNCTION  FOR_STAT
      FUNCTION   FOR_LSTAT ( FILE_NAME, STATB )
      INTEGER*4  FOR_LSTAT 
      INTEGER*4  STATB(16) 
      CHARACTER  FILE_NAME*(*)
#ifdef GNU
      INTEGER*4, INTRINSIC :: LSTAT
#else 
      INTEGER*4  LSTAT, LSTAT_
#endif
#if defined (INTEL) || defined (SUN)
      FOR_LSTAT = LSTAT_ ( FILE_NAME, STATB )
#else
      FOR_LSTAT = LSTAT  ( FILE_NAME, STATB )
#endif
      RETURN
      END  FUNCTION  FOR_LSTAT
!
! ---------------------------------------------------------------------------
!
#ifdef INTEL
      SUBROUTINE GETARG ( IPAR, CPAR )
      INTEGER*4  IPAC
      CHARACTER  CPAR*(*)
      CALL GETARG_ ( IPAR, CPAR ) 
      RETURN
      END  SUBROUTINE  GETARG 
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE GERROR ( STR )
      IMPLICIT   NONE 
      CHARACTER  STR*(*)
      INTEGER*2  IR
      INTEGER*4  IS
      INTEGER*4  GETLASTERROR_, STRERROR
!
      IR = GETLASTERROR_()            !  Get the code of the last error
      IS = STRERROR ( %VAL(IR) )      !  Get system message related to IR
      CALL STRNCPY ( STR, %VAL(IS) )  !  Copy the system message to STR
!
      RETURN
      END  SUBROUTINE  GERROR 
#endif
#ifdef SUN
      SUBROUTINE GERROR ( STR ) 
      IMPLICIT   NONE 
      CHARACTER  STR*(*)
      CALL GERROR_ ( STR ) 
      RETURN
      END  SUBROUTINE  GERROR
!
! ------------------------------------------------------------------------
!
      FUNCTION   LOC__SUN$$_STR ( STR )
      INTEGER*4  LOC__SUN$$_STR
      CHARACTER  STR*(*)
      LOC__SUN$$_STR = LOC(STR)
      RETURN
      END  FUNCTION   LOC__SUN$$_STR  !#!#
#endif
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FOR_QSORT ( ARR, NEL, EL_SIZE, COMP_FUNC )
#if defined (INTEL) || defined (SUN)
      INTEGER*1  ARR
      INTEGER*4  NEL
      INTEGER*4  EL_SIZE
      INTEGER*2, EXTERNAL :: COMP_FUNC 
      CALL QSORT ( ARR, NEL, EL_SIZE, COMP_FUNC )
#else
      INTEGER*1  ARR
      INTEGER*4  NEL
      INTEGER*4  EL_SIZE
      INTEGER*8  NEL_I8, EL_SIZE_I8
      INTEGER*4  NEL_I4, EL_SIZE_I4
      INTEGER*4, EXTERNAL :: COMP_FUNC 
#ifdef ADR_64BIT
      NEL_I8 = NEL
      EL_SIZE_I8 = EL_SIZE
      CALL QSORT ( %VAL(LOC(ARR)), %VAL(NEL_I8), %VAL(EL_SIZE_I8), COMP_FUNC )
#else
      NEL_I4 = NEL
      EL_SIZE_I4 = EL_SIZE
      CALL QSORT ( %VAL(LOC(ARR)), %VAL(NEL_I4), %VAL(EL_SIZE_I4), COMP_FUNC )
#endif      
#endif      
      RETURN
      END  SUBROUTINE  FOR_QSORT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FOR_QSORT8 ( ARR, NEL, EL_SIZE, COMP_FUNC )
#if defined (INTEL) || defined (SUN)
      INTEGER*1  ARR
      INTEGER*4  NEL
      INTEGER*4  EL_SIZE
      INTEGER*2, EXTERNAL :: COMP_FUNC 
      WRITE ( 6, * ) 'FOR_QSORT8 is not implmented for INTEL'
      CALL EXIT ( 1 )
#else
      INTEGER*1  ARR
      INTEGER*8  NEL, EL_SIZE
      INTEGER*4, EXTERNAL :: COMP_FUNC 
      CALL QSORT ( %VAL(LOC(ARR)), %VAL(NEL), %VAL(EL_SIZE), COMP_FUNC )
#endif      
      RETURN
      END  SUBROUTINE  FOR_QSORT8  !#!#
