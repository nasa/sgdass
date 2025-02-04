      SUBROUTINE GET_EPOCH ( TJD, NCNT )
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 im, id, iy, ihr, imin
!-----END of imp added lines.
!
!
! 1.  GET_EPOCH PROGRAM SPECIFICATION
!
! 1.1 Read clock or atmosphere epochs from the terminal.
!
!     Note Bene: It should be modified in 2020 year!
!
! 1.2 REFERENCES:
!
! 2.  GET_EPOCH INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NCNT
!
! NCNT - Screen line at which to leave the cursor
!
! 2.3 OUTPUT Variables:
!
      REAL*8 TJD
!
! TJD - Julian date of the specified epoch
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: stflg
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      REAL*8      FJLDY
      CHARACTER   BUFSTR*80 
      INTEGER*4   NC4, IX4, IY4, IOS, ICH4
      REAL*8      FJD_BEG, FJD_END, TIM_HOUR
      INTEGER*4,  EXTERNAL :: I_LEN, ILEN
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2004.10.29  Added more diagnostic in the case of wrong input
!   pet   2005.03.17  Added support of ability to enter epoch as a fraction of &
!                     hours
!
!
! 5.  GET_EPOCH PROGRAM STRUCTURE
!
! --- Set the default values
!
      IM = 0
      ID = 0
      IY = 0
      IHR = 0
      IMIN = 0
      TJD = 0.D0
!
!---- Read the input from the keyboard
!
 210  CONTINUE
      CALL GETSTR_F ( BUFSTR )
      IF  ( ILEN(BUFSTR) .EQ. 0 ) BUFSTR = '0 0 0 0 0'
      IF ( INDEX ( BUFSTR, '.' ) > 0 ) THEN
           READ ( BUFSTR, FMT='(F8.4)', IOSTAT=IOS ) TIM_HOUR
           IF ( IOS .NE. 0 ) THEN
                WRITE ( 6, '(A)' ) 'BUFSTR  >>'//BUFSTR(1:I_LEN(BUFSTR))//'<<'
                CALL ADDSTR_F ( "$$$  Error in decoding your input. "// &
     &                          "Hit <return> and try again. $$$   " )
                CALL SENKR_MN ( IX4, IY4, ICH4 )
                CALL NL_MN()
                CALL ADDSTR_F ( "?" )
                GOTO 210
           END IF
           CALL OBSTM ( FJD_BEG, FJD_END )
           TJD = FJD_BEG + TIM_HOUR/24.0D0
         ELSE 
           READ ( BUFSTR, FMT=*, IOSTAT=IOS ) IY, IM, ID, IHR, IMIN
           IF ( IOS .NE. 0  ) THEN
                WRITE ( 6, '(A)' ) 'BUFSTR  >>'//BUFSTR(1:I_LEN(BUFSTR))//'<<'
                CALL ADDSTR_F ( "$$$  Error in decoding your input. "// &
     &                          "Hit <return> and try again. $$$   " )
                CALL SENKR_MN ( IX4, IY4, ICH4 )
                CALL NL_MN()
                CALL ADDSTR_F ( "?" )
                GOTO 210
           END IF
!
! -------- Check whether year and month are zero
!
           IF ( IY .EQ. 0 .AND. IM .EQ. 0 ) THEN
                NC4 = NCNT
                CALL SETCR_MN ( 0, NC4 )
                CALL CLRTOBOT_MN()
                RETURN
           END IF
           IF ( IY .LT. 72 .AND. IY .GT. 20 ) THEN
                CALL ADDSTR_F ( "$$$  The YEAR is not correct. Hit <return> "// &
     &                          "and try again. $$$   " )
                CALL SENKR_MN ( IX4, IY4, ICH4 )
                CALL NL_MN()
                CALL ADDSTR_F ( "?" )
                GOTO 210
           END IF
!
           IF ( IM .LT. 0  .OR.  IM .GT. 12 ) THEN
                CALL ADDSTR_F ( "$$$  The NUMBER OF A MONTH is not correct. "// &
     &                          "Hit <return> and try again. $$$   " )
                CALL SENKR_MN ( IX4, IY4, ICH4 )
                CALL NL_MN()
                CALL ADDSTR_F ( "?" )
                GOTO 210
           END IF
           IF ( ID .LT. 0  .OR.  ID .GT. 31 ) THEN
                CALL ADDSTR_F ( "$$$  The NUMBER OF A DAY is not correct. "// &
     &                          "Hit <return> and try again. $$$   " )
                CALL SENKR_MN ( IX4, IY4, ICH4 )
                CALL NL_MN()
                CALL ADDSTR_F ( "?" )
                GOTO 210
           END IF
           IF ( IHR .LT. 0  .OR.  IHR .GT. 23 ) THEN
                CALL ADDSTR_F ( "$$$  The HOUR is not correct. Hit <return> and "// &
     &                          "try again. $$$   " )
                CALL SENKR_MN ( IX4, IY4, ICH4 )
                CALL NL_MN()
                CALL ADDSTR_F ( "?" )
                GOTO 210
           END IF
           IF ( IMIN .LT. 0  .OR.  IMIN .GT. 59 ) THEN
                CALL ADDSTR_F ( "$$$  The MINUTE is not correct. Hit <return> and "// &
     &                          "try again. $$$   " )
                CALL SENKR_MN ( IX4, IY4, ICH4 )
                CALL NL_MN()
                CALL ADDSTR_F ( "?" )
                GOTO 210
           END IF
!
! -------- Calculate the julian date, and fracture of the day
!
           IF ( IM .EQ. 0 ) THEN
                TJD = IM
             ELSE
                TJD = FJLDY ( IM, ID, IY )
           END IF
           TJD=TJD + IHR/24.0D0 + IMIN/1440.0D0
      END IF
      RETURN
      END  SUBROUTINE  GET_EPOCH
