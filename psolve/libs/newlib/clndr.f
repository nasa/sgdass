      SUBROUTINE CLNDR ( IYEAR , MONTH , IDAY , IPMON , IPDAY )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      integer*2 iday0, idayr, iyear, month, iday, ipmon, index
      integer*2 ipday, jpday, jpmon
!     IMPLICT NONE and type declerations added by hand. jwr August 2002.
!
      INCLUDE 'param.i'
!     Convert YMD to printable month, day-of-week TAC 760911
!     Y2K fixes              KDB 981027
!
!-----CLNDR IS A CALENDER CONVERSION PROGRAM WHICH CONVERTS YEAR-
!     MONTH-DAY DATES INTO PRINTABLE MONTH AND DAY-OF-WEEK:
!         IPMON = 'PRINTABLE' MONTH ( e.g. "JAN." )
!         IPDAY = 'PRINTABLE' DAY OF WEEK ( e.g. "MON." )
!              ( IPMON AND IPDAY BOTH IN <2A2>  OR <A2,A1> FORMATS)
!
!-----NOTE THAT IF MONTH = 0 ON ENTRY ( AND HENCE THE INPUT IS IN YEAR
!     AND DAY-OF-THE-YEAR FORMAT ), MONTH WILL BE CHANGED TO THE
!     CORRECT MONTH AND IDAY WILL BE CHANGED TO THE DAY-OF-THE-MONTH
!     PRIOR TO RETURN. IF THE IDAY IS 'INVALID' ( E.G.  33 DEC 1976 ),
!     IT WILL BE 'CORRECTED' ( E.G.  02 JAN 1977 ) PRIOR TO RETURN.
!     YOU SHOULD THEREFORE BE VERY CAUTIOUS OF INSERTING INTEGERS
!     (RATHER THAN VARIABLES) INTO YOUR CALL.
!     E.G.      CALL CLNDR [ 1976 , 12 , 33 ... ]      MIGHT CAUSE
!                      DISASTROUS RESULTS ) !!!!!!!!!!!!!!!!!!!
!
!****************************************************************************
!********            ACHTUNG !!    WARNING !!    ATTENCIONE !!       ********
!********   REVISED 11 SEPT 76 BY TAC 'CUZ THE RETURNED DAY OF WEEK  ********
!********               FOR  IYEAR = 1977 WAS INCORRECT !            ********
!********                                                            ********
!****************************************************************************
!
      DIMENSION JPDAY (14) , IPDAY (2) , JPMON (24) , IPMON (2)
      CHARACTER*28 JPDAY_CHAR
      EQUIVALENCE (JPDAY,JPDAY_CHAR)
      CHARACTER*48 JPMON_CHAR
      EQUIVALENCE (JPMON,JPMON_CHAR)
!
      DATA JPDAY_CHAR /'SUMOTUWETHFRSAN N ESD URI T '/
      DATA JPMON_CHAR /'JAFEMAAPMAJUJUAUSEOCNODEN.B.R.R.Y.N.L.G.P.T.V.C. &
     &'/
!
!     The input year may be expressed as the full four digits or as the last
!     two digits.   (For the
!     record, all known callers use the 4 digit format, but the 2 digit
!     case, introduced by TAC on 9/11/76 will be retained.  The years past 1900
!     case is unexpected and will be flagged as an error.)
!     Internally, clndr needs the year expressed in the full four digits.
!     Convert the input year to this format if the year is in two digits.
!     (Convert iyear itself and return the new value.)
!
      if (iyear.ge.0 .and. iyear.le.99) then
!       Input year is expressed as last 2 digits. Convert to 4 digits.
        if (iyear.ge.Y2K_START_YEAR) then !year in 1900s.
          iyear = iyear + 1900
        else !year in 2000s
          iyear = iyear + 2000
        endif
      endif
!
      if(iyear.gt.99 .and. iyear .lt.200) then
!        value passed as year - 1900 (year is in the 2000s)
        write (*, &
     &       "('bad year input to newlib/new5/clndr: ',i5)")iyear
        stop 'bad year input to newlib/new5/clndr'
      endif
!
!-----WHICH FORMAT IS THE ENTRY IN?
!     I.E. YEAR AND DAY-OF-YEAR OR YEAR-MONTH-DAY ?
!
      IDAYR = 0
      IF ( MONTH ) 1 , 1 , 2
!
!-----DAY-OF-YEAR FORMAT, SO DECODE THE MONTH AND CHANGE IDAY TO DAY-
!     OF-THE-MONTH:
!
  1   IDAYR = IDAY
      CALL YMDAY ( IYEAR , IDAYR , MONTH , IDAY )
      GO TO 9
!
!-----ALREADY IN YEAR-MONTH-DAY FORMAT:
!
  2   IDAYR = IDAY + IDAY0 ( IYEAR , MONTH )
!
!-----NOW FIGURE OUT THE DAY OF THE WEEK:
!
!************* THE FOLLOWING LINE WAS INCORRRECT & REVISED 11 SEPT 76 BY TAC
! 9   INDEX = MOD (IYEAR + IDAYR - ((IYEAR - 1)/ 4) + 4 , 7) + 1
!         NOTE                   ^ <----CHANGES-----> ^
  9   INDEX = MOD (IYEAR + IDAYR + ((IYEAR - 1)/ 4) + 5 , 7) + 1
      IPDAY (1) = JPDAY ( INDEX )
      IPDAY (2) = JPDAY ( INDEX +7 )
!
!-----FILL IN THE MONTH AND RETURN:
!
      IPMON (1) = JPMON ( MONTH )
      IPMON (2) = JPMON ( MONTH + 12 )
      RETURN
      END
