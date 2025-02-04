      SUBROUTINE OUT_LST ( IND_COV, JPARM, J_NAME, A2, BVECT, TVCT, &
     &                     COV_END, ELEMENTS, MPAR, IWDS, IARCNM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OUT_LST PROGRAM SPECIFICATION
!
! 1.1 Print out a list of parameters in matrix form.
!
! 1.2 REFERENCES:
!
! 2.  OUT_LST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 ELEMENTS
      INTEGER*2 MPAR,IWDS
      INTEGER*2 IND_COV(MPAR),JPARM(IWDS,MPAR)
      INTEGER*2 COV_END,IARCNM
      REAL*8 A2(ELEMENTS),BVECT(MPAR),TVCT(MPAR)
      CHARACTER*(*) J_NAME
!
! A2, BVECT - Sub_sets of ARC matrix
! COV_END - Total number of elements
! ELEMENTS - Not used
! IARCNM - Current arc number
! IND_COV - Covariance index list
! IWDS - Length in words of parameter names
! J_NAME - Name of input ARC file
! JPARM - Parameter list
! MPAR - Maximum number of parameters
! TVCT - Vector consisting of diagonal elements of A2
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'baccm.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      REAL*8  APRIOR(M_GPA)
      COMMON /APRIORI/aprior
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: covmm
!       CALLED SUBROUTINES: prnt_vect
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 INDX4
      INTEGER*2 I,II,J,JJ,K,L
      INTEGER*2 IDUM(10)
      INTEGER*2 JTOT, MON, DAY, YEAR
      INTEGER*4 IOS
      logical*2 equal
      CHARACTER DUM*20,XS1*1,XS2*1
      character*24 datestr
      CHARACTER*20 CPARM
      REAL*8 CNVRT(MPAR)
      character*3 ptype(mpar)
      logical*2 intrp_rates
      real*8 trot_prev,trot_cur,tolerance,fjldy, &
     &       rotap_cur(4),ratap(3)
      character*1 eop_type,eop_order
      integer*2 iyr_eop,imon_eop,idy_eop,ihr_eop,imin_eop,idum2(2)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb  960621  Fix units on wobble, ut1-tai and nutation parameters.
!   kdb  960624  Print totals for wobble and ut1-tai old-style offset and
!                rate parameters.
!   kdb  960627  Fix incompatibilities between the format of CVRFxx and the
!                format expected by corel. (The incompatibilities
!                produce a bad CORLxx file.)
!                First, place a false date in CVRFxx so that corel can
!                properly process CVRFxx and not shift the information by one
!                row in CORLxx.  Also add an extra blank line between the
!                parameter list and parameter matrix.
!   kdb  960820  Fix arc_j range error by changing aprior declaration from
!                max_sta to max_par.
!
!
! 5.  OUT_LST PROGRAM STRUCTURE
!
!   first write header
!
       datestr = ' '
       if (aprior(1).ne.0) then
         call mdyjl(mon,day,year,idum,trot(1) )
         write(datestr,1107) year,mon,day
1107     format('Date = ',I2,'/',I2,'/',I2)
       else
!        Set up a false date so that corel will find CVRFxx in the expected
!        format and not have trouble reading it. KDB 6/27/96
         datestr = 'Date = 00/00/00'
       endif
      JTOT=COV_END
      WRITE ( 88, 1101, IOSTAT=IOS) &
     &        IARCNM, JTOT, J_NAME, IARCNM, JTOT, J_NAME, DATESTR
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1101 FORMAT('1',1X,I5,1X,I5,1X,A/ &
     &       ' ',1X,I5,1X,I5,1X,A,1X,A/)
!
! If we're doing stations, write the apriori position of the
!   reference station and the date as found in TROT
!
       call use_glbfil_4('ORC' )
       if ( aprior(1) .ne. 0 ) then
            do i=1,numsta
               if ( equal( isitn(1,i), INT2(1), fixed_sta(1), INT2(1), &
     &              INT2(8)) ) then
                   write ( 88, 1103, IOSTAT=IOS) (fixed_sta(j),j=1,4), &
     &             vsitec(1,i)
                   CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), &
     &                         INT2(0) )
                   WRITE ( 88, 1104, IOSTAT=IOS ) (fixed_sta(j),j=1,4), &
     &             vsitec(2,i)
                   CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), &
     &                         INT2(0) )
                   WRITE ( 88, 1105, IOSTAT=IOS ) (fixed_sta(j),j=1,4), &
     &             vsitec(3,i)
                   CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), &
     &                         INT2(0) )
1103               format(5X,'0 ',4A2,' X COMPONENT',47X,D23.16)
1104               format(5X,'0 ',4A2,' Y COMPONENT',47X,D23.16)
1105               format(5X,'0 ',4A2,' Z COMPONENT',47X,D23.16)
               endif
            enddo
       endif
!
! ---- Some variables may need unit conversions.  Determine the specific
! ---- parameters and conversions.
!
       DO I=1,COV_END
         cparm = ' '
         write(cparm,"(10A2)") (JPARM(J,IND_COV(I)),J=1,IWDS)
         if (index(cparm,"WOBBLE").ne.0 .or. &
     &       index(cparm,"NUTATION").ne.0) then
!          Radians to mas
           CNVRT(i) = (180.0D0/PI__NUM) * 3600.0D0 * 1000.D0
           if (index(cparm,"WOBBLE").ne.0) then
             ptype(i) = 'WOB'
           else
             ptype(i) = 'NUT'
           endif
         else if (index(cparm,"UT1-TAI").ne.0) then
!          Seconds to ms
           CNVRT(i) = 1.0D3
           ptype(i) = 'UT1'
         else
!          No conversion needed
           CNVRT(i) = 1.0D0
           ptype(i) = 'OTH'
         endif
       ENDDO
!
!      Eop aprioris must be calculated on the fly, because they are not
!      carried in the prfil.  (But only bother with the aprioris in the
!      old estimation style, and only do the offsets and rates.)
!
       intrp_rates = .true.
       trot_prev = 0.0D0
       tolerance = 1.0D0/86400.0D0 !1 second
       DO I=1,COV_END
         if (ptype(i).eq.'WOB'.or.ptype(i).eq.'UT1') then
           if ((ptype(i).eq.'WOB'.and.eop_style(1).eq.0) .or. &
     &         (ptype(i).eq.'UT1'.and.eop_style(2).eq.0)) then
!            old estimation style
             cparm = ' '
             write(cparm,"(10A2)") (JPARM(J,IND_COV(I)),J=1,IWDS)
             eop_type = cparm(1:1)
             eop_order = cparm(10:10)
             if (eop_order.eq.'0'.or.eop_order.eq.'1') then
!              Generate the epoch
               read(cparm(11:12),"(i2)") iyr_eop
               read(cparm(13:14),"(i2)") imon_eop
               read(cparm(15:16),"(i2)") idy_eop
               read(cparm(17:18),"(i2)") ihr_eop
               read(cparm(19:20),"(i2)") imin_eop
               trot_cur = fjldy(imon_eop,idy_eop,iyr_eop)
               trot_cur = trot_cur + &
     &           (ihr_eop * 60.0D0 + imin_eop) / 1440.0D0
               if (dabs(trot_cur - trot_prev) .gt. tolerance) then
!                new epoch - get all values that apply to this epoch
                 if(KEROT) then
                   CALL INTRP_EOMOD(TROT_CUR,ROTAP_CUR(1), &
     &                  ROTAP_CUR(2),ROTAP_CUR(3),ROTAP_CUR(4), &
     &                  INTRP_RATES,RATAP(3),RATAP(1),RATAP(2), &
     &                  IDUM2 )
                  else
                    CALL INTRP_EOVR(TROT_CUR,ROTAP_CUR(1), &
     &                   ROTAP_CUR(2),ROTAP_CUR(3),ROTAP_CUR(4), &
     &                   INTRP_RATES,RATAP(3),RATAP(1),RATAP(2), &
     &                   IDUM2 )
                  endif
               endif
!              Pull out the proper apriori
               if (eop_order.eq.'0') then !offset
                 if (eop_type.eq.'X') then
                   aprior(i) = rotap_cur(3)
                 else if (eop_type.eq.'Y') then
                   aprior(i) = rotap_cur(4)
                 else
                   aprior(i) = -1.0D0 * rotap_cur(1)
                 endif
               else !rate
                 if (eop_type.eq.'X') then
                   aprior(i) = ratap(1)
                 else if (eop_type.eq.'Y') then
                   aprior(i) = ratap(2)
                 else
                   aprior(i) = -1.0D0 * ratap(3)
                 endif
               endif
             endif
           endif
         endif
       ENDDO
!
!
       DO I=1,COV_END
          IF ( IND_COV(I) .NE. 0 ) THEN
               WRITE ( 88, 1102, IOSTAT=IOS ) I, (JPARM(J,IND_COV(I)),J=1,IWDS), &
     &                 TVCT(IND_COV(I))*CNVRT(I)*CNVRT(I), &
     &                 BVECT(IND_COV(I))*CNVRT(I), &
     &                 (BVECT(IND_COV(I))+aprior(i))*CNVRT(I)
               CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), &
     &                     INT2(0) )
 1102         FORMAT(1X,I5,1X,10A2,1X,3D23.16)
          ENDIF
       ENDDO
!
!      The corel algorithm needs an extra blank line here if there are fewer
!      than 4 parameters, because corel will try to read parameters 4+
!      whether they are there or not and will
!      pick up the first parameter matrix line without this blank line.
!
       IF ( JTOT.LE.3 ) THEN
            WRITE ( 88, "(' ')", IOSTAT=IOS )
            CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
       ENDIF
!
!   now write out the matrix of parameters
!
      DO I=2,COV_END
         IF ( IND_COV(I) .NE. 0 ) THEN
              DO J=1,I-1
                 VECTR(J) = A2 ( INDX4(IND_COV(I),IND_COV(J)))*CNVRT(I)*CNVRT(J)
              ENDDO
              CALL PRNT_VECT ( VECTR, INT2(I-1), I )
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  OUT_LST  #!#
