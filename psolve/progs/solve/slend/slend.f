      PROGRAM SLEND
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!   SLEND TERMINATES MULTI-SOLVE AND OF'S MULTI-SOLVE PROGRAMS
!
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
      INCLUDE 'fclib.i'
!
      LOGICAL*2 KBIT
      INTEGER*2 I
!
      INTEGER*4 I4P0
      DATA I4P0 /0/
!
      CALL PRE_PROG()
      INCLUDE 'slend_version.i' ! Set revision date of the current version
!
!   BLANK SCREEN
!
      IF(KSCREEN) THEN
        call start_mn()
        CALL SETCR_MN(I4P0,I4P0 )
        CALL clear_mn()
      ENDIF
!
!   BLANK SCREEN
!
 1000 CONTINUE
      IF(KSCREEN) then
        call addstr_f("solve done" )
        call nl_mn()
        call refresh_mn()
        call end_mn()
      endif
      IF ( KBIT( PRE_IP(2), INT2(6)) ) THEN
           CALL CURLIB_SET_TERM ( PTR_CH(PRE_SCR_DIR(:PRE_SD_LEN)//'term'// &
     &                            PRE_LETRS//CHAR(0)) )
      ENDIF
!
!   FINISHED
!
      CALL REMOVE_SOLVE_LOCK()  
      CALL END_PROG()
      END
