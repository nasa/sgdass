      SUBROUTINE IONX(JNSTA,LDBNAM,IDBVER,JSITN,JSITI,LCOMM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE 'solve.i'
!
! 1.  IONX PROGRAM SPECIFICATION
!
! 1.1  SUBROUTINE IONX INTERACTS WITH THE USER TO SET UP OPTIONS FOR
!      IONOSPHERE CALIBRATIONS.
!
! 1.2 REFERENCES:
!
! 2.  IONX PROGRAM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
      INTEGER*2 LDBNAM(5),JSITN(4,MAX_ARC_STA),JSITI(MAX_ARC_STA)
      INTEGER*2 JNSTA,IDBVER
!
!     LDBNAM - Data base names.
!     JSITN  - Stations status.
!     JSITI  - Contains the final status.
!     JNSTA -  Number of stations in current data base.
!     IDBVER - Data base versions.
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LCOMM
!
!     LCOMM - INDICATE WHETHER USER WANTS TO SEE THE NEXT DATA BASE,
!             RETURN TO THE OPTIONS MENU OR PERFORM LEAST SQUARES.
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: corrn
!     CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 FOUND, KBIT, AVAIL
      CHARACTER*4 CCHAR
      INTEGER*2 I,ITEST,K,L
      integer*4 ichr,ix,iy,iy1
      EQUIVALENCE (ICHR,CCHAR)
      character*80 bufstr
!
      INTEGER*4 I4P0, I4P14, I4P15, I4P26, I4P32
      DATA I4P0 /0/, I4P14 /14/, I4P15 /15/, I4P26 /26/, I4P32 /32/
      INTEGER*2  INT2_ARG
!
! 4.  HISTORY
!  WHO  WHEN   WHAT
!
!     MALLAMA. JAN. 18, 1983. BASED ON CABLX
!
!     MALLAMA, FEBRUARY 1984. ADDED DISPLAY OF DATA BASE NAME.
!
!     JRR, 841116 : IONX extensively rewritten to handle new ionospheric
!                   correction values and new flag settings.
!     KDB, 860428 : Added code allowing user to return to options menu
!                   or see next data base.
!     KDB, 860505 : Added code allowing user to refresh screen or
!                   initiate least squares.
!     KDB  950810   Allow 32 sites.
!     pet  2000.09.15  Corrected bugs: the previous version was screwed up.
!
!
! 5.  IONX PROGRAM STRUCTURE
!
!     1. CLEAR THE SCREEN
!
 50   CONTINUE
      CALL SETCR_MN(I4P0,I4P0 )
      CALL CLEAR_MN()
!
!  2. WRITE STATUS OF IONOSPHERE CORRECTIONS ON SCREEN.
!
      WRITE(bufstr,1000) LDBNAM,IDBVER
 1000 FORMAT(4X,'IONOSPHERE CAL FOR ',5A2,I3)
      call addstr_f(bufstr )
      call nl_mn()
      call nl_mn()
      call addstr_f("   site    available  INCLUDEd" )
      call nl_mn()
      call addstr_f("   ----    ---------  --------" )
      call nl_mn()
!
      DO I = 1,JNSTA
        iy = i + 3
!       list status for all stations
        CALL SETCR_MN(I4P0,iy )
        WRITE(bufstr,1001) (JSITN(K,I),K=1,4)
 1001   FORMAT(1X,4A2)
        call addstr_f(bufstr )
        call nl_mn()
        CALL SETCR_MN(I4P15,iy )
        IF(KBIT( JSITI(I), INT2(1) ) .AND. .NOT.KBIT( JSITI(I), INT2(2) )) THEN
!         write GION is available
          CALL SETCR_MN ( 15, IY )
          call addstr_f("GION " )
        ENDIF
        IF(.NOT.KBIT( JSITI(I), INT2(1) ) .AND. KBIT( JSITI(I), INT2(2) )) THEN
!         write PHION is available
          CALL SETCR_MN ( 15, IY )
          call addstr_f("PHION" )
        ENDIF
        IF(KBIT( JSITI(I), INT2(1) ) .AND. KBIT( JSITI(I), INT2(2) )) THEN
!         write both are available
          CALL SETCR_MN ( 15, IY )
          call addstr_f("BOTH " )
        ENDIF
        IF(.NOT.KBIT( JSITI(I), INT2(1) ).AND..NOT.KBIT( JSITI(I), INT2(2))) &
     &     THEN
!         write neither available
          CALL SETCR_MN ( 15, IY )
          call addstr_f(" NO  " )
        ENDIF
        IF(KBIT( JSITI(I), INT2(4) ) .AND. .NOT.KBIT( JSITI(I), INT2(5))) THEN
!         write GION to be applied
          CALL SETCR_MN ( 26, IY )
          call addstr_f("GION " )
        ENDIF
        IF(.NOT.KBIT( JSITI(I), INT2(4) ) .AND. KBIT( JSITI(I), INT2(5))) THEN
!         write PHION to be applied
          CALL SETCR_MN ( 26, IY )
          call addstr_f("PHION" )
        ENDIF
        IF(.NOT.KBIT( JSITI(I), INT2(4) ).AND..NOT.KBIT( JSITI(I), &
     &     INT2(5)))THEN
!         write none to be applied
          CALL SETCR_MN ( 26, IY )
          call addstr_f(" NO  " )
        ENDIF
        CALL SETCR_MN(I4P32,iy )
      ENDDO
!
! 3. WRITE OUT CONTROL LINE
!
      call nl_mn()
      call nl_mn()
      call &
     &     addstr_f("     (A)LL     RETURN TO (O)PTIONS     (N)EXT DATA BASE" )
      call nl_mn()
!
!
! 4. NOW SEE WHAT THE USER WANTS TO DO.
!
      ITEST = 0
      iy1 = jnsta + 5
      CALL SETCR_MN(I4P14,iy )
  99  CONTINUE
      DO WHILE(ITEST.EQ.0)
!       DO BEGIN interacting
        CALL SENKR_MN(IX,IY,ICHR )
        IF (CCHAR(4:4).EQ.'R') GO TO 50
        IF (CCHAR(4:4).EQ.'Q') THEN
           LCOMM = 2
           GO TO 99999
        ENDIF
        IF (CCHAR(4:4).EQ.'O' .OR. (IY.EQ.5+JNSTA .AND. &
     &    IX.GE.15 .AND. IX.LE.33 .AND. CCHAR(4:4) .EQ. ' ')) THEN
          CALL SETCR_MN(I4P0,I4P0 )
          CALL CLEAR_MN()
          LCOMM = 0
          GO TO 99999
        ENDIF
        IF (CCHAR(4:4).EQ.'N' .OR. (CCHAR(4:4) .EQ. ' '.AND. &
     &    IY.EQ.5+JNSTA .AND. IX.GE.39 .AND. IX.LE.54)) THEN
          CALL SETCR_MN(I4P0,I4P0 )
          CALL CLEAR_MN()
          LCOMM = 1
          GO TO 99999
        ENDIF
        FOUND = .FALSE.
!
        AVAIL = .FALSE.
        DO  I=1,JNSTA
!         search for availability
          IF(KBIT( JSITI(I), INT2(1) ) .OR. KBIT( JSITI(I), INT2(2))) &
     &       AVAIL = .TRUE.
        ENDDO
        IF(.NOT.AVAIL) THEN
!         not available
          CALL SETCR_MN(I4P14,iy1 )
          FOUND = .TRUE.
        ENDIF
        IF (FOUND) GO TO 99
!
        IF (IY.GE.4 .AND. IY.LE.3+JNSTA .AND. CCHAR(4:4).EQ.' ') THEN
!         toggle
          L = IY - 3
          CALL SETCR_MN(I4P26,IY )
          IF(KBIT( JSITI(L), INT2(1) ).AND..NOT.KBIT( JSITI(L), INT2(2) )) THEN
!           only GION available
            IF(KBIT( JSITI(L), INT2(4) )) THEN
!             remove GION
              CALL SBIT( JSITI(L), INT2(4), INT2(0) )
              CALL SETCR_MN ( 26, IY )
              call addstr_f(" NO  " )
              GO TO 998
            ENDIF
            IF(.NOT.KBIT( JSITI(L), INT2(4) )) THEN
!             apply GION
              CALL SBIT( JSITI(L), INT2(4), INT2(1) )
              CALL SETCR_MN ( 26, IY )
              call addstr_f("GION " )
              GO TO 998
            ENDIF
          ENDIF
          IF(.NOT.KBIT( JSITI(L), INT2(1) ).AND.KBIT( JSITI(L), INT2(2) )) THEN
!           only PHION available
            IF(KBIT( JSITI(L), INT2(5))) THEN
!             remove PHION
              CALL SBIT( JSITI(L), INT2(5), INT2(0) )
              CALL SETCR_MN ( 26, IY )
              call addstr_f(" NO  " )
              GO TO 998
            ENDIF
            IF(.NOT.KBIT( JSITI(L), INT2(5))) THEN
!             apply PHION
              CALL SBIT( JSITI(L), INT2(5), INT2(1) )
              CALL SETCR_MN ( 26, IY )
              call addstr_f("PHION" )
              GO TO 998
            ENDIF
          ENDIF
          IF(KBIT( JSITI(L), INT2(1) ).AND.KBIT( JSITI(L), INT2(2) )) THEN
!           both available
            IF(KBIT( JSITI(L), INT2(4) )) THEN
!             remove GION, apply PHION
              CALL SBIT( JSITI(L), INT2(4), INT2(0) )
              CALL SBIT( JSITI(L), INT2(5), INT2(1) )
              CALL SETCR_MN ( 26, IY )
              call addstr_f("PHION" )
              GO TO 998
            ENDIF
            IF(KBIT( JSITI(L), INT2(5))) THEN
!             remove both
              CALL SBIT( JSITI(L), INT2(4), INT2(0) )
              CALL SBIT( JSITI(L), INT2(5), INT2(0) )
              CALL SETCR_MN ( 26, IY )
              call addstr_f(" NO  " )
              GO TO 998
            ENDIF
            IF(.NOT.KBIT( JSITI(L), INT2(4) ).AND..NOT.KBIT( JSITI(L), &
     &         INT2(5))) THEN
!             apply GION
              CALL SBIT( JSITI(L), INT2(4), INT2(1) )
              CALL SETCR_MN ( 26, IY )
              call addstr_f("GION" )
              GO TO 998
            ENDIF
          ENDIF
  998     CONTINUE
          CALL SETCR_MN(I4P26,IY )
          FOUND = .TRUE.
        ENDIF
        IF (FOUND) GO TO 99
!
        IF (CCHAR(4:4).EQ.'A' .OR. (CCHAR(4:4) .EQ. ' '.AND. &
     &    IY.EQ.5+JNSTA .AND. IX.GE.5 .AND. IX.LE.9)) THEN
!         letter all
          DO L=1,JNSTA
!           reset all flags
            IY = L + 3
            CALL SETCR_MN(I4P26,IY )
            CALL REFRESH_MN ( )
            IF(KBIT( JSITI(L), INT2(1) ).AND..NOT.KBIT( JSITI(L), INT2(2))) &
     &         THEN
!             only GION available
              IF(KBIT( JSITI(L), INT2(4) )) THEN
!               remove GION
                CALL SBIT( JSITI(L), INT2(4), INT2(0) )
                CALL SETCR_MN ( 26, IY )
                call addstr_f(" NO" )
                call refresh_mn()
                GO TO 999
              ENDIF
              IF(.NOT.KBIT( JSITI(L), INT2(4) )) THEN
!               apply GION
                CALL SBIT( JSITI(L), INT2(4), INT2(1) )
                CALL SETCR_MN ( 26, IY )
                call addstr_f("GION" )
                call refresh_mn()
                GO TO 999
              ENDIF
            ENDIF
            IF(.NOT.KBIT( JSITI(L), INT2(1) ).AND.KBIT( JSITI(L), INT2(2))) &
     &         THEN
!             only PHION available
              IF(KBIT( JSITI(L), INT2(5))) THEN
!               remove PHION
                CALL SBIT( JSITI(L), INT2(5), INT2(0) )
                CALL SETCR_MN ( 26, IY )
                call addstr_f(" NO" )
                call refresh_mn()
                GO TO 999
              ENDIF
              IF(.NOT.KBIT( JSITI(L), INT2(5))) THEN
!               apply PHION
                CALL SBIT( JSITI(L), INT2(5), INT2(1) )
                CALL SETCR_MN ( 26, IY )
                call addstr_f("PHION" )
                call refresh_mn()
                GO TO 999
              ENDIF
            ENDIF
            IF(KBIT( JSITI(L), INT2(1) ).AND.KBIT( JSITI(L), INT2(2) )) THEN
!             both available
              IF(KBIT( JSITI(L), INT2(4) )) THEN
!               remove GION, apply PHION
                CALL SBIT( JSITI(L), INT2(4), INT2(0) )
                CALL SBIT( JSITI(L), INT2(5), INT2(1) )
                CALL SETCR_MN ( 26, IY )
                call addstr_f("PHION" )
                call refresh_mn()
                GO TO 999
              ENDIF
              IF(KBIT( JSITI(L), INT2(5))) THEN
!               remove both
                CALL SBIT( JSITI(L), INT2(4), INT2(0) )
                CALL SBIT( JSITI(L), INT2(5), INT2(0) )
                CALL SETCR_MN ( 26, IY )
                call addstr_f(" NO" )
                call refresh_mn()
                GO TO 999
              ENDIF
              IF(.NOT.KBIT( JSITI(L), INT2(4) ) .AND..NOT.KBIT( JSITI(L), &
     &           INT2(5))) THEN
!               apply GION
                CALL SBIT( JSITI(L), INT2(4), INT2(1) )
                CALL SETCR_MN ( 26, IY )
                call addstr_f("GION" )
                call refresh_mn()
                GO TO 999
              ENDIF
            ENDIF
  999       CONTINUE
          ENDDO
          CALL SETCR_MN(I4P14,iy1 )
          CALL REFRESH_MN ( )
          FOUND = .TRUE.
        ENDIF
      ENDDO
!
99999 RETURN
      END
