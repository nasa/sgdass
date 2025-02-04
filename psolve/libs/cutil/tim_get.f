      SUBROUTINE TIM_GET ( NAME )
! ************************************************************************
! *                                                                      *
! *   Subroutine TIM_GET formats information about amount of CPU and     *
! *   phyisical time elepased from the moment when procedure TIM_INIT    *
! *   was executed. It wites this informatiuon in the file               *
! *   $WORK_DIR/TIMRxx, were xx are SOLVE user initials. If writes this  *
! *   information in the screen too. The name of the SOLVE component is  *
! *   a part of the output line.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   NAME   ( CHARACTER ) -- Name of the component (program). Any       *
! *                           string up to 8 symbols long.               *
! *                           Comment: if the symbol of name is "-" then *
! *                           it is dropped and the name of the session  *
! *                           is not printed.                            *
! *                                                                      *
! *  ###  05-APR-99    TIM_GET     v1.7  (c)  L. Petrov 12-NOV-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'precm.i'
      CHARACTER  NAME*(*)
      CHARACTER  NAME_COMP*80
      INTEGER*2  LDBNAM(5,15), IDBV(15), NUMD_I2
      INTEGER*4  IDBE(15), DBVER, I40
      CHARACTER  CDBNAM(15)*10, DBNAME*16, DBNAME_MES*32, TIMING_FILE*160, &
     &           CUR_SESS*8, TOT_SESS*8, OUT_TERM*80, OUT_FILE*80, &
     &           WORK_DIR*160, STR_CPU*11, STR_ELP*11, DATE_STR*19, MODE*1
      CHARACTER  GET_CDATE*19
      EQUIVALENCE ( CDBNAM,LDBNAM(1,1))
      REAL*8     TCPU, TELP
      INTEGER*4  HOU_CPU, MIN_CPU, SEC_CPU, TIC_CPU, &
     &           HOU_ELP, MIN_ELP, SEC_ELP, TIC_ELP
      LOGICAL*4  F_HYPHEN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( FAST_MODE .EQ. F__NONE ) THEN
!
! -------- non-fast mode: small letter
!
           CALL TRAN ( 12, ISLTY2, MODE )
         ELSE
!
! -------- Fast mode: LARGE letter
!
           CALL TRAN ( 11, ISLTY2, MODE )
      END IF
      IF ( ICHAR(MODE) .EQ. 0 ) MODE = ' '
!
      CALL CLRCH ( NAME_COMP )
      IF ( NAME(1:1) .EQ. '-' ) THEN
           F_HYPHEN = .TRUE.  ! set flag
           NAME_COMP = NAME(2:)
         ELSE
           F_HYPHEN = .FALSE. ! lift flag
           NAME_COMP = NAME
      END IF
!
! --- Learn the database name and its version
!
      NUMD_I2 = 1
      CALL DBPOX ( NUMD_I2, LDBNAM, IDBV, IDBE )
      DBNAME = CDBNAM(1)
      DBVER  = IDBV(1)
!
! --- Forming field DBNAME_MES
!
      CALL CLRCH ( DBNAME_MES )
      DBNAME_MES = DBNAME
      DBNAME_MES(12:) = '<'
      CALL INCH ( DBVER, DBNAME_MES(13:) )
      DBNAME_MES( I_LEN(DBNAME_MES)+1: ) = '>'
!
! --- Set the lines with current session number and total number of sessions
!
      CALL CLRCH ( CUR_SESS )
      CALL CLRCH ( TOT_SESS )
      IF ( NARCS .GT. INT2(1) ) THEN
           CALL INCH   ( IARCNM, CUR_SESS )
           CALL CHASHL ( CUR_SESS )
           CALL INCH   ( NARCS,  TOT_SESS )
           CALL CHASHL ( TOT_SESS )
           TOT_SESS = '('//TOT_SESS(1:I_LEN(TOT_SESS))//')'
      END IF
!
! --- Get information about CPU and physical elapsed time
!
      CALL TIM_TP ( 2, TCPU, TELP, ' ' )
!
! --- Decoding CPU time
!
      HOU_CPU = TCPU/3600
      MIN_CPU = (TCPU - HOU_CPU*3600.0)/60
      SEC_CPU = (TCPU - HOU_CPU*3600.0 - MIN_CPU*60.0)
      TIC_CPU = (TCPU - HOU_CPU*3600.0 - MIN_CPU*60.0 -  SEC_CPU)*100
      IF ( TIC_CPU .GE. 100 ) TIC_CPU = 99
!C
      STR_CPU = 'hh:mm:ss.tt'
      CALL INCH   ( HOU_CPU, STR_CPU(1:2) )
      CALL CHASHR (          STR_CPU(1:2) )
      CALL BLANK_TO_ZERO (   STR_CPU(1:2) )
!
      CALL INCH   ( MIN_CPU, STR_CPU(4:5) )
      CALL CHASHR (          STR_CPU(4:5) )
      CALL BLANK_TO_ZERO (   STR_CPU(4:5) )
!
      CALL INCH   ( SEC_CPU, STR_CPU(7:8) )
      CALL CHASHR (          STR_CPU(7:8) )
      CALL BLANK_TO_ZERO (   STR_CPU(7:8) )
!
      CALL INCH   ( TIC_CPU, STR_CPU(10:11) )
      CALL CHASHR (          STR_CPU(10:11) )
      CALL BLANK_TO_ZERO (   STR_CPU(10:11) )
!
! --- Decoding elapsed physical time
!
      HOU_ELP = TELP/3600
      MIN_ELP = (TELP - HOU_ELP*3600.0)/60
      SEC_ELP = (TELP - HOU_ELP*3600.0 - MIN_ELP*60.0)
      TIC_ELP = (TELP - HOU_ELP*3600.0 - MIN_ELP*60.0 -  SEC_ELP)*100
      IF ( TIC_ELP .GE. 100 ) TIC_ELP = 99
!C
      STR_ELP = 'hh:mm:ss.tt'
      CALL INCH   ( HOU_ELP, STR_ELP(1:2) )
      CALL CHASHR (          STR_ELP(1:2) )
      CALL BLANK_TO_ZERO (   STR_ELP(1:2) )
!
      CALL INCH   ( MIN_ELP, STR_ELP(4:5) )
      CALL CHASHR (          STR_ELP(4:5) )
      CALL BLANK_TO_ZERO (   STR_ELP(4:5) )
!
      CALL INCH   ( SEC_ELP, STR_ELP(7:8) )
      CALL CHASHR (          STR_ELP(7:8) )
      CALL BLANK_TO_ZERO (   STR_ELP(7:8) )
!
      CALL INCH   ( TIC_ELP, STR_ELP(10:11) )
      CALL CHASHR (          STR_ELP(10:11) )
      CALL BLANK_TO_ZERO (   STR_ELP(10:11) )
!
! --- Generate a status line for terminal
!
      CALL CLRCH ( OUT_TERM )
      CALL CLRCH ( OUT_FILE )
      IF ( ILEN(TOT_SESS) .EQ. 0 ) THEN
           OUT_TERM = NAME_COMP(1:8)//'  '// &
     &                DBNAME_MES(1:16)//'  '//STR_CPU(2:)//' / '//STR_ELP// &
     &                ' '//MODE
         ELSE
           OUT_TERM = NAME_COMP(1:12)//'  '// &
     &                CUR_SESS(1:I_LEN(CUR_SESS))// &
     &                TOT_SESS(1:I_LEN(TOT_SESS))//'  '// &
     &                DBNAME_MES(1:16)//'  '//STR_CPU//' / '//STR_ELP// &
     &                ' '//MODE
      END IF
!
! --- Get curremt date and time
!
      DATE_STR = GET_CDATE ()
!
! --- Generate a status line for file
!
      WRITE ( UNIT=OUT_FILE, FMT=110 ) NAME_COMP, IARCNM, DBNAME_MES(1:16), &
     &        TCPU, TELP, DATE_STR(12:19), MODE
 110  FORMAT ( '| ',A8,' | ',I4,' | ',A16,' | ',F10.2,' | ',F10.2,' | ', &
     &          A,' | ',A )
!
      IF ( F_HYPHEN ) THEN
           OUT_TERM(10:38) = '                             '
           OUT_FILE(21:36) = '                '
      END IF
!
! --- Set the file name where timing information will be written
!
      CALL CLRCH ( WORK_DIR )
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
      IF ( ILEN(WORK_DIR) .EQ. 0 ) THEN
           WORK_DIR = SOLVE_WORK_DIR
      END IF
!
! --- form a timing file name
!
      CALL CLRCH ( TIMING_FILE )
      TIMING_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'/'//'TIMR'//PRE_LETRS
      TIMING_FILE = WORK_DIR(1:I_LEN(WORK_DIR))
      TIMING_FILE(I_LEN(WORK_DIR)+1:I_LEN(WORK_DIR)+5) = '/TIMR'
      TIMING_FILE(I_LEN(WORK_DIR)+6:I_LEN(WORK_DIR)+7) = PRE_LETRS
!
! --- Write timing line in TIMING file
!
      OPEN  ( UNIT=40, FILE=TIMING_FILE, STATUS='UNKNOWN', ACCESS='APPEND', &
     &        IOSTAT=I40 )
      WRITE ( UNIT=40, FMT='(A)', IOSTAT=I40 ) OUT_FILE(1:I_LEN(OUT_FILE))
      CLOSE ( UNIT=40, IOSTAT=I40 )
!
! --- Write status line in the screen
!
!@     WRITE ( 6, FMT='(A)' ) ' '//OUT_TERM(1:77)
      WRITE ( 6, FMT='(A,A)' ) ' ', OUT_TERM(1:77)
!
      RETURN
      END  !#!  TIM_GET  #!#
