      SUBROUTINE PARMINFOE(IROW,JPARM,MPAR,IWDS,PTYPE,CNVRT,APRIORE)
      IMPLICIT NONE
!
! 1.  PARMINFOE PROGRAM SPECIFICATION
!
! 1.1 Get information about the parameters so that special processing can
!     be done for the eop parameters.  The information is returned
!     in an array whose elements correspond to the input list
!     of parameter names.
!
! 1.2 REFERENCES:
!
! 2.  PARMINFOE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IROW,MPAR,IWDS
      INTEGER*2 JPARM(IWDS,MPAR)
!
! IROW - index to desired parameter
! JPARM - input list of parameter names
! MPAR - maximum number of parameters
! IWDS - number of words in each parameter name
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*3 PTYPE(MPAR)
      REAL*8 APRIORE(MPAR),CNVRT(MPAR)
!
!     PTYPE -type of parameter (e.g., WOB for wobble, UT1 for ut1)
!     CNVRT - factor for converting from the internal Solve units
!            (e.g., the A matrix units)
!            to the output units expected by the users.
!     APRIORE - aprioris for selected parameters.
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*20 CPARM
      logical*2 intrp_rates
      real*8 trot_cur,fjldy, &
     &       rotap_cur(4),ratap(3)
      character*1 eop_type,eop_order
      integer*2 iyr_eop,imon_eop,idy_eop,ihr_eop,imin_eop,idum2(2),jct
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb  960626  Created.
!   kdb  960703  Recognize station parameters.
!
! 5.  PARMINFOE PROGRAM STRUCTURE
!
!     Don't bother resetting this parameter's values if they have
!     already been set.
!
      if (ptype(irow).ne.'   ') return
!
!      Determine the parameter type and conversion factor.
!
      cparm = ' '
      write(cparm,"(10A2)") (JPARM(JCT,IROW),JCT=1,IWDS)
      if (index(cparm,"WOBBLE").ne.0 .or. &
     &    index(cparm,"NUTATION").ne.0) then
!       Radians to mas
        CNVRT(irow) = (180.0D0/PI__NUM) * 3600.0D0 * 1000.D0
        if (index(cparm,"WOBBLE").ne.0) then
          ptype(irow) = 'WOB'
        else
          ptype(irow) = 'NUT'
        endif
      else if (index(cparm,"UT1-TAI").ne.0) then
!       Seconds to ms
        CNVRT(irow) = 1.0D3
        ptype(irow) = 'UT1'
      else if (index(cparm,"COMPONENT").eq.12 .and. &
     &         (cparm(10:10).eq.'X'.or.cparm(10:10).eq.'Y'.or. &
     &          cparm(10:10).eq.'Z')) then
!       No conversion needed - site position parameter
        CNVRT(irow) = 1.0D0
        ptype(irow) = 'STA'
      else
!       No conversion needed
        CNVRT(irow) = 1.0D0
        ptype(irow) = 'OTH'
      endif
!
!     Eop aprioris must be calculated on the fly, because they are not
!     carried in the prfil.  (But only bother with the aprioris in the
!     old estimation style, and only do the offsets and rates.)
!
      intrp_rates = .true.
      if (ptype(irow).eq.'WOB'.or.ptype(irow).eq.'UT1') then
        if ((ptype(irow).eq.'WOB'.and.eop_style(1).eq.0) .or. &
     &      (ptype(irow).eq.'UT1'.and.eop_style(2).eq.0)) then
!         old estimation style
          cparm = ' '
          write(cparm,"(10A2)") (JPARM(JCT,IROW),JCT=1,IWDS)
          eop_type = cparm(1:1)
          eop_order = cparm(10:10)
          if (eop_order.eq.'0'.or.eop_order.eq.'1') then
!           Generate the epoch
            read(cparm(11:12),"(i2)") iyr_eop
            read(cparm(13:14),"(i2)") imon_eop
            read(cparm(15:16),"(i2)") idy_eop
            read(cparm(17:18),"(i2)") ihr_eop
            read(cparm(19:20),"(i2)") imin_eop
            trot_cur = fjldy(imon_eop,idy_eop,iyr_eop)
            trot_cur = trot_cur + &
     &        (ihr_eop * 60.0D0 + imin_eop) / 1440.0D0
            if(KEROT) then
              CALL INTRP_EOMOD(TROT_CUR,ROTAP_CUR(1), &
     &           ROTAP_CUR(2),ROTAP_CUR(3),ROTAP_CUR(4), &
     &           INTRP_RATES,RATAP(3),RATAP(1),RATAP(2), &
     &           IDUM2)
            else
              CALL INTRP_EOVR(TROT_CUR,ROTAP_CUR(1), &
     &          ROTAP_CUR(2),ROTAP_CUR(3),ROTAP_CUR(4), &
     &          INTRP_RATES,RATAP(3),RATAP(1),RATAP(2), &
     &          IDUM2)
            endif
!           Pull out the proper apriori
            if (eop_order.eq.'0') then !offset
              if (eop_type.eq.'X') then
                apriore(irow) = rotap_cur(3)
              else if (eop_type.eq.'Y') then
                apriore(irow) = rotap_cur(4)
              else
                apriore(irow) = -1.0D0 * rotap_cur(1)
              endif
            else !rate
              if (eop_type.eq.'X') then
                apriore(irow) = ratap(1)
              else if (eop_type.eq.'Y') then
                apriore(irow) = ratap(2)
              else
                apriore(irow) = -1.0D0 * ratap(3)
              endif
            endif
          endif
        endif
      endif
!
      RETURN
      END
