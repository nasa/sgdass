      SUBROUTINE SCORF_LKYALL ( ITPCAL, QDCAL, IC_RANGE, CHANGEALLGOAL, &
     &                          IHIGHYIND )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCORF_LKYALL PROGRAM SPECIFICATION
!
! 1.1
!     PRINTS KEY LISTING THE MEANINGS OF SYMBOLS USED TO INDICATE
!     WHETHER A CALIBRATION IS AVAILABLE (V) OR
!     AVAILABLE AND APPLIED TO A STATION (P).  THE SUBROUTINE ALSO
!     PRINTS ONE OR TWO LINES ALLOWING THE USER TO CHANGE A
!     CALIBRATION FOR ALL STATIONS AT ONCE.
!     ALSO PRINTS COMMAND FOR ADDING A FLYBY CALIBRATION.
!     ALSO PRINTS COMMANDS FOR GOING TO NEXT/PREVIOUS SET OF CALIBS.
!
! 1.2 REFERENCES:
!
! 2.  SCORF_LKYALL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*1 CHANGEALLGOAL
      CHARACTER*8 QDCAL(*)
      integer*2 ITPCAL,ihighyind,ic_range
!
! CHANGEALLGOAL - Specifies whether calibration will be changed to
!                 available (V) or applied (P)
! ITPCAL - Number of calibrations on this page
! IHIGHYIND - Y coordinate of bottom valid field for changing
! QDCAL - List of calibration names
! IC_RANGE - since the list of flyby calibrations will exceed
!    a page, need to know which subset we're working with.  Identify
!    first member and count out as many as needed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: selcorf
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*64 BUFR
      INTEGER*2 I,IFLDLENALL,IFLDLENIND,IFRSTALLLINE,IFRSTCHRXIND, &
     &   IFRSTLOXALL, ILIM1,ILIM2,ILOXADDCAL,ILOXBCKCAL, &
     &   ILOXFWDCAL,IYADDCAL,IYBCKCAL,IYFWDCAL,K,NOINLINE,trimlen
      INTEGER*4  IKEYX, IKEYY
      integer*4 ix,iy
      DATA IKEYX/63/, IKEYY/11/  ! was
!!      DATA IKEYX/0/, IKEYY/12/
      DATA IFRSTALLLINE/27/
      DATA IFRSTCHRXIND/13/
      DATA IFLDLENIND/5/
      DATA IFRSTLOXALL/16/
      DATA IFLDLENALL/13/
      DATA ILOXADDCAL /63/
      DATA ILOXBCKCAL /63/
      DATA ILOXFWDCAL /63/
      DATA IYADDCAL /9/
      DATA IYBCKCAL /7/
      DATA IYFWDCAL /5/
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!   KDB   7/18/91 CREATED
!   KDB   8/10/95 Finish changing for 32 sites.
!
! 5.  SCORF_LKYALL PROGRAM STRUCTURE
!
!
!     PRINT KEY
!
!      IX = IKEYX
!      IY = IKEYY
!      CALL setcr_mn (IX, IY)
!      call addstr_f("V = AVAILABLE BUT")
!C
!      IX = IX + 4
!      IY = IY + 1
!      CALL setcr_mn (IX, IY)
!      call addstr_f("NOT APPLIED")
!C
!      IX = IX - 4
!      IY = IY + 2
!      CALL setcr_mn (IX, IY)
!      call addstr_f("P = APPLIED")
!
!     PRINT LINES FOR CHANGING A CALIBRATION FOR ALL STATIONS AT ONCE
!
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
      CALL SETCR_MN ( 40, INT4( IHIGHYIND)+1 )
      CALL SCOR_ALLGOAL (CHANGEALLGOAL, ITPCAL )
!
!
      IF (ITPCAL .LE. 5) THEN
        NOINLINE = ITPCAL
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
        WRITE (BUFR(ILIM1:ILIM2), 1090) I, QDCAL(IC_RANGE - 1 + I)
 1090   FORMAT (I2, ")", 1X, A8)
        WRITE (BUFR(ILIM1:ILIM1), 1100)
 1100   FORMAT ("(")
        ILIM1 = ILIM1 + IFLDLENALL
        ILIM2 = ILIM2 + IFLDLENALL
 60   CONTINUE
!
      IX = IFRSTLOXALL
      IY = IFRSTALLLINE
!      CALL setcr_mn (IX, IY)
!      call addstr_f(bufr(:trimlen(bufr)))
!
      IF (ITPCAL .GE.6) THEN
        ILIM1 = 1
        ILIM2 = IFLDLENALL - 1
        DO K = 1,64
          BUFR(K:K) = ' '
        END DO
        DO 70 I = 6, ITPCAL
          WRITE (BUFR(ILIM1:ILIM2), 1090) I, QDCAL(IC_RANGE - 1 + I)
          WRITE (BUFR(ILIM1:ILIM1), 1100)
          ILIM1 = ILIM1 + IFLDLENALL
          ILIM2 = ILIM2 + IFLDLENALL
 70     CONTINUE
        IX = IFRSTLOXALL
        IY = IFRSTALLLINE + 1
!        CALL setcr_mn (IX, IY)
!        call addstr_f(bufr(:trimlen(bufr)))
      END IF
!
!     POSITION CURSOR SO THAT THE USER CAN CONVENIENTLY INPUT CHANGES
!     FOR INDIVIDUAL STATIONS, THE ONLY COMMANDS WHICH MUST BE ENTERED
!     BY CURSOR.
!
      IX = IFRSTCHRXIND + (ITPCAL/2) * IFLDLENIND
      IY = IHIGHYIND + 1
!      CALL setcr_mn (IX, IY)
!
      RETURN
      END  !#!  SCORF_LKYALL  #!#
