      SUBROUTINE SCOR_LKEYALL ( ICALIBNO, QDCAL, CHANGEALLGOAL, &
     &                          IHIGHYIND )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCOR_LKEYALL PROGRAM SPECIFICATION
!
! 1.1
!     PRINTS KEY LISTING THE MEANINGS OF SYMBOLS USED TO INDICATE
!     WHETHER A CALIBRATION IS UNAVAILABLE (-), JUST AVAILABLE (V) OR
!     AVAILABLE AND APPLIED TO A STATION (P).  THE SUBROUTINE ALSO
!     PRINTS ONE OR TWO LINES ALLOWING THE USER TO CHANGE A
!     CALIBRATION FOR ALL STATIONS AT ONCE.
!
! 1.2 REFERENCES:
!
! 2.  SCOR_LKEYALL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*1 CHANGEALLGOAL
      CHARACTER*8 QDCAL(*)
      integer*2 icalibno,ihighyind
!
! CHANGEALLGOAL - Specifies whether calibration will be changed to
!                 available (V) or applied (P)
! ICALIBNO - Number of calibrations used
! IHIGHYIND - Y coordinate of bottom valid field for changing
! QDCAL - List of calibration names
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: selcor
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      integer*2 trimlen,ikeyx,ikeyy,ifrstallline,ifrstchrxind
      integer*2 ifldlenind,ifrstloxall,ifldlenall,noinline,ilim1
      integer*2 ilim2,k,i
      integer*4 ix,iy
      CHARACTER*64 BUFR
!!      DATA IKEYX/16/, IKEYY/27/  ! was
      DATA IKEYX/0/, IKEYY/20/
      DATA IFRSTALLLINE/28/
      DATA IFRSTCHRXIND/13/
      DATA IFLDLENIND/5/
      DATA IFRSTLOXALL/16/
      DATA IFLDLENALL/13/
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SCOR_LKEYALL PROGRAM STRUCTURE
!
!     PRINT KEY
!
!      IX = IKEYX
!      IY = IKEYY
!      CALL setcr_mn (IX, IY)
!      call addstr_f  ( "V = Available but applied")
!
       CALL SETCR_MN ( 62, 2   )
       CALL ADDSTR_F ( 'Legend:'           )
       CALL SETCR_MN ( 62, 4   )
       CALL ADDSTR_F ( '"V" Available but' )
       CALL SETCR_MN ( 62, 5   )
       CALL ADDSTR_F ( '    NOT applied  ' )
       CALL SETCR_MN ( 62, 7   )
       CALL ADDSTR_F ( '"P" Applied'       )
       CALL SETCR_MN ( 62, 9  )
       CALL ADDSTR_F ( '"-" Not applied'   )
!
!      IX = IX + 4
!      IY = IY + 1
!      CALL setcr_mn (IX, IY)
!      call addstr_f(" NOT APPLIED")
!
!      IX = IX - 4
!      IY = IY + 2
!      CALL setcr_mn (IX, IY)
!      call addstr_f("  P = Applied")
!
!      IY = IY + 2
!      CALL setcr_mn (IX, IY)
!      call addstr_f("  - = Not available")
!
!     PRINT LINES FOR CHANGING A CALIBRATION FOR ALL STATIONS AT ONCE
!
!
      CALL SETCR_MN ( 40, INT4(IHIGHYIND)+1 )
      CALL SCOR_ALLGOAL (CHANGEALLGOAL, ICALIBNO )
!
      IF (ICALIBNO .LE. 5) THEN
        NOINLINE = ICALIBNO
      ELSE
        NOINLINE = 5
      END IF
!
      ILIM1 = 1
      ILIM2 = IFLDLENALL - 1
      DO K = 1,64
        BUFR(K:K) = ' '
      END DO
!
      DO 60 I = 1, NOINLINE
        WRITE (BUFR(ILIM1:ILIM2), 1090) I, QDCAL(I)
 1090   FORMAT (I2, ")", 1X, A8)
        WRITE (BUFR(ILIM1:ILIM1), 1100)
 1100   FORMAT ("(")
        ILIM1 = ILIM1 + IFLDLENALL
        ILIM2 = ILIM2 + IFLDLENALL
 60   CONTINUE
!
      IX = IFRSTLOXALL
      IY = IFRSTALLLINE
!!      CALL setcr_mn (IX, IY)
!!      call addstr_f(bufr(:trimlen(bufr)))
!
      IF (ICALIBNO .GE.6) THEN
        if (icalibno.le.10) then
          noinline = icalibno
        else
          noinline = 10
        endif
        ILIM1 = 1
        ILIM2 = IFLDLENALL - 1
        DO K = 1,64
          BUFR(K:K) = ' '
        END DO
        DO 70 I = 6, noinline
          WRITE (BUFR(ILIM1:ILIM2), 1090) I, QDCAL(I)
          WRITE (BUFR(ILIM1:ILIM1), 1100)
          ILIM1 = ILIM1 + IFLDLENALL
          ILIM2 = ILIM2 + IFLDLENALL
 70     CONTINUE
        IX = IFRSTLOXALL
        IY = IFRSTALLLINE + 1
!!        CALL setcr_mn (IX, IY)
!!      call addstr_f(bufr(:trimlen(bufr)))
      END IF
      IF (ICALIBNO .GE.11) THEN
        ILIM1 = 1
        ILIM2 = IFLDLENALL - 1
        DO K = 1,64
          BUFR(K:K) = ' '
        END DO
        DO 71 I = 11, ICALIBNO
          WRITE (BUFR(ILIM1:ILIM2), 1090) I, QDCAL(I)
          WRITE (BUFR(ILIM1:ILIM1), 1100)
          ILIM1 = ILIM1 + IFLDLENALL
          ILIM2 = ILIM2 + IFLDLENALL
 71     CONTINUE
        IX = IFRSTLOXALL
        IY = IFRSTALLLINE + 2
!!        CALL setcr_mn (IX, IY)
!!      call addstr_f(bufr(:trimlen(bufr)))
      END IF
!
!     POSITION CURSOR SO THAT THE USER CAN CONVENIENTLY INPUT CHANGES
!     FOR INDIVIDUAL STATIONS, THE ONLY COMMANDS WHICH MUST BE ENTERED
!     BY CURSOR.
!
      IX = IFRSTCHRXIND + (ICALIBNO/2) * IFLDLENIND
      IY = IHIGHYIND + 1
      CALL setcr_mn (IX, IY )
!
      RETURN
      END
