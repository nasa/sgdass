      SUBROUTINE SETUP()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETUP PROGRAM SPECIFICATION
!
! 1.1 Set up input CGM file name, mode (add or subtract) and CGM to
!      be added or subtracted.
!
! 1.2 REFERENCES:
!
! 2.  SETUP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'addcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: adder
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*63 CGMIN,FNAME,DIR,THINGIN
      CHARACTER*3 ADDSUB
      CHARACTER*6 INTTODECIMAL
      INTEGER*2 INAME(8),ERROR,I,IDUM,LETRS,LENGTH
!
      INTEGER*2 DECIMALTOINT,ICHCM,ICHMV,LENGTH_DIR,LENGTH_NAME,TRIMLEN
      LOGICAL*2 KBIT
!
      INTEGER*2 IRUNST(70)
      CHARACTER*140 STRING
      EQUIVALENCE (IRUNST(1),STRING)
!
      EQUIVALENCE (FNAME,INAME(1))
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SETUP PROGRAM STRUCTURE
!
      STDALN=PRE_IP(1).NE.0
!
      IF(.NOT.STDALN) THEN
        STRING=' '
        CALL USE_BUFFER( IRUNST, INT2(66), 'ORC' )
        CGMINN=STRING(1:64)
        THINGN=STRING(65:128)
        ADORSB='ADD'
        IF(IRUNST(65).EQ.0) ADORSB='SUB'
        ARORCG='ARC'
        IF(IRUNST(66).EQ.0) ARORCG='CGM'
      ELSE
!
!  SET UP PRELUDE COMMON BY HAND FOR NOW
!
        LETRS=2H??
        CALL SETUP_PRELUDE(LETRS )
        CALL SET_TESTV(.FALSE. )
        CALL SET_SPOOL(.FALSE. )
        CALL MAKE_PIPES()
!
        CALL SPLITSTRING(STRING,CGMIN,STRING )
        CALL SPLITSTRING(STRING,CGMIN,STRING )
        CALL SPLITSTRING(STRING,CGMIN,STRING )
        CALL SPLITSTRING(STRING,ADDSUB,STRING )
        CALL SPLITSTRING(STRING,THINGIN,STRING )
        ARORCG='CGM'
!
        IF(CGMIN.EQ.' '.OR.THINGIN.EQ.' '.OR. &
     &     (ADDSUB.NE.'ADD'.AND.ADDSUB.NE.'SUB'))THEN
        call start_mn()
        call &
     &       addstr_f("ADDER: ADDER of cgms " )
        call nl_mn()
        call &
     &       addstr_f("Usage: ADDER,CGMIN,ADDSUB,THINGIN" )
        call nl_mn()
        call &
     &       addstr_f("Where: CGMIN   is the input CGM file name" )
        call nl_mn()
        call &
     &       addstr_f("       ADDSUB  is ADD or SUB for type of operation" )
        call nl_mn()
        call &
     &       addstr_f("       THINGIN is the name of the CGM to be added or " )
        call &
     &       addstr_f("subtracted" )
        call nl_mn()
        call end_mn()
          STOP
        ENDIF
!
        CGMINN=CGMIN
!
        THINGN=THINGIN
!
        ADORSB=ADDSUB
      ENDIF
!
!      IF(STDALN.OR..NOT.KBIT(PRE_IBATCH,4)) then
!       call start_mn
!        call addstr_f("[ADDER 93.07.08]")
!       call refresh_mn
!       call end_mn
!      endif
!
      RETURN
      END
