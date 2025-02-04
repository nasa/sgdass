      SUBROUTINE SESTAT_INIT ( DBOBJ, IP, DBNAME, IDATYP, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure  SESTAT_INIT  initialize data structure for collecting   *
! *   statistics for the database. It is assumed that SESTAT_INIT is     *
! *   called before the the looop for scanning observations in the       *
! *   database.                                                          *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      IP ( INTEGER*4 ) -- Mode switch.                                *
! *                          IP=1 -- all fields of DBOBJ are to be       *
! *                                  initialized;                        *
! *                          IP=2 -- only lists of objects and number of *
! *                                  observations should be initialized, *
! *                                  other fields being untouched.       *
! *  DBNAME ( CHARACTER ) -- Database name.                              *
! *  IDATYP ( INTEGER*2 ) -- Code of the solution type.                  *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  06-JUL-98   SESTAT_INIT  v1.2  (c)  L. Petrov  13-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      CHARACTER  DBNAME*16
      INTEGER*2  IDATYP
      INTEGER*4  IP, IUER, LEN_DBOBJ, LEN2_DBOBJ, I_LEN
      CHARACTER  STR*80, STR1*80
!
! --- Test of data structure length
!
      LEN_DBOBJ = (LOC(DBOBJ%LAST_FIELD) - LOC(DBOBJ%FIRST_FIELD)) + 4
      IF ( LEN_DBOBJ .NE. ML_DBOBJ ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( ML_DBOBJ, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_DBOBJ, STR1 )
           CALL ERR_LOG ( 8401, IUER, 'SESTAT_INIT', 'Internal error: '// &
     &         'Declared size of DBOBJ data structure (obser.i) '// &
     &          STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &         'actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- Zeroing DBOBJ
!
      IF ( IP .EQ. 1 ) THEN
!
! -------- Full initiliazation
!
           CALL NOUT ( ML_DBOBJ, DBOBJ )
           DBOBJ%NAME   = DBNAME
           DBOBJ%STATUS = DBOBJ__UNF
           DBOBJ%IDATYP = IDATYP
           DBOBJ%F_AMB  = .FALSE.
           DBOBJ%F_ION  = .FALSE.
           DBOBJ%F_AMB_CHANGED = .FALSE.
        ELSE IF ( IP .EQ. 2 ) THEN
!
! -------- Partial initialization
!
           LEN2_DBOBJ = (LOC(DBOBJ%MARKER_2) - LOC(DBOBJ%MARKER_1)) + 4
!
! -------- Zero all stuff between MARKER_1 and MARKER_2
!
           CALL NOUT ( LEN2_DBOBJ, DBOBJ%MARKER_1 )
        ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( IP, STR )
           CALL ERR_LOG ( 8402, IUER, 'SESTAT_INIT', 'Wrong value of '// &
     &         'paramater IP :'//STR(1:I_LEN(STR))//' -- one of 1, 2 '// &
     &         ' was expected' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SESTAT_INIT  #!#
