!!Obsrv
!
!     Common space for the observation array, which holds all the
!     information needed to sort the observation set and eliminate
!     duplicates.  Takes the place of keeping the observation set in
!     memory. This common also holds the working space used to get
!     values into the observation array.
!
!     Programmers:
!       Gregg Cooke        90.02.08  Creation.
!
!     Modifications:
!       Brent Archinal     92.09.30  MAXOB changed from 8000 to 20000.
!       Brent Archinal     93.01.21  "nobs" spec moved to gen_outdb -
!                                    the only place it is used.
!       Melvin White       94.01.31  Changed "dpv" to "dpvp" and "dpvd".
!                                    Added "rdpvp" and "rdpvd".
!       Brent Archinal     95.02.21  MAXOB changed from 20000 to 32768.
!                                    Type changed from i*2 to i*4.
!       Brent Archinal     95.03.24  Added fifth R*8 element to "iobs"
!                                    to hold info for secondary dup.
!                                    elimination criteria.  rdpvp and
!                                    rdpvd dropped.
!       "     "            98.03.13  rdpvp and rdpvd actually dropped.
!                                    Converted to handle I*4 number
!                                    of observations.  MAXOB raised
!                                    from 32768 to 100001.
!
!     *** If changed, remember to re-"make" dbgutil library. ***
!
!     Parameters:
!
!     MAXOB is the maximum number of observations the program can
!     handle.  (Actually it is one plus the maximum as sort_obs uses
!     the last space as a buffer.  BA, 95.03.24).
!
      integer*4 MAXOB
      parameter (MAXOB = 100001)
!
!     Specifications:
!
!     IOBS -- The format of the observation array is as follows:
!
!  iobs i4bs dobs
!  2b   4b   8b     #      Type  Description
!  --   --   --    --      ---   --------------------------------------
!   1   *1    1     1      I*4   Index (current observation number)
!   2
!  *3    2          2      I*2   Baseline code
!  *4               3      I*2   Source code
!  *5    3    2     4      I*2   Frequency
!  *6               5      I*2   Quality code
!  *7    4          6      I*2   Pass/fail flag
!   8               7      I*2   <empty>
!   9    5   *3     8      R*8   Time
!  10
!  11    6
!  12
!  13    7   *4     9      R*8   Primary duplicate elim criterion
!  14
!  15    8
!  16
!  17    9   *5     10     R*8   Secondary duplicate elim criterion
!  18
!  19   10
!  20
!
!  To access any of the above variables, look for the "*" number to the
!  left of the item desired, and use that element of the array name
!  at the top of the column.  E.g. use IOBS to access I*2 variables,
!  I4BS to access I*4 variables, and DOBS to access R*8 variables.
!
!     IUTC  -- The current observation time.
!     USEC  -- The seconds part of the current observation time.
!     ISTR  -- The current source.
!     IBAS  -- The current baseline.
!     DPVP  -- The current primary elimination criterion.
!     DPVD  -- The current secondary duplicate elimination criterion.
!     DFRQ  -- The current frequency.
!     LQLC  -- The current quality code.
!     JCHAN -- The current number of channels.
!     NUM   -- The total number of observations in the observations array.
!
      character cbas*16, cstr*8
      integer*2 iobs(20,MAXOB), iutc(5), istr(4), ibas(8), &
     &          lqlc, jchan
      integer*4 num, i4bs(10,MAXOB)
      real*8 dobs(5,MAXOB), dpvp, dpvd, dfrq, usec
      equivalence (iobs,dobs), (iobs,i4bs), (cbas,ibas), (cstr,istr)
!
!     Common Space:
!
      common /obsrv/ iobs, iutc, istr, ibas, dpvp, dfrq, usec, &
     &               lqlc, jchan, num, dpvd
!
