      SUBROUTINE menu ( laction, idatyp, iwt_mode, num_wts, kpause_reway, &
     &                  kfall_back, knew_update, max_iter, chi_tol, &
     &                  wt_floor, wt_ceiling, DBNAME )
!
! modified JMGipson 97OCT30
!  New menu possibilities:
!  1.) Maximum number of iterations.
!  2.) Chi-square tolerance.
!  3.) Experimental update mode for new weights.
!  4.) Flag for automatically going from one mode to another.
! Also, substantially changed the way the screen is written
! to make it easier to modify.
!
! pet  21-JAN-98  Added database name printing at the header line of hte menu.
!                 Made some cosmetic improvements in appearence of the screen
!                 form.
! pet  03-FEB-98  Substituted hard-coded test of solution type by DATYP_INQ
! pet  16-FEB-99  Added stuff for the new menu item: H  -- getting on-line
!                 help
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      character*1 laction
      integer*2 idatyp
      integer*2 iwt_mode,num_wts
      logical*2 kpause_reway
      logical*2 kfall_back             !fall back from baseline-->station-->global
      logical*2 knew_update            !knew_update mode.
      integer*2  max_iter              !Maximum number of iterations
      double precision chi_tol         !Chi square tolerance.
      double precision wt_floor(2)
      double precision wt_ceiling(2)
      CHARACTER  DBNAME*(*)
      LOGICAL*4  DATYP_INQ
!
! present user menu
!
! input:
!     idatyp from socom
!     okay   .true. if error constants on all baseline are equal for idatyp
!
! output:
!     laction:  what the user wants, see menu display for meanings
!
!      modifications
!
!      kdb 970204 Bring up (e)nter particular values option under new reway.
!                 Rename Z (zero) to R (reinitialize) for backwards
!                    compatibility.  Then rename R (read from namfil) to N.
!      kdb 970610 New option, P, to turn off pauses in iterative listing.
!      pet 980121
!
!
      character*4 cchar
      integer*4 ichar4
      character*5 ltype
      character*8 lkind(0:2)/"Global","Station","Baseline"/
      CHARACTER   BUFSTR*80, STR*54, GET_VERSION*54
      integer*4 ix,iy
      integer*4 i4p0, i4p4, i4p6, i4p40
      integer*4 irow4,icol4
      equivalence (ichar4,cchar)
      data i4p0, i4p4, i4p6, i4p40 /0,4,6,40/
!
      CHARACTER*1 get_action_char
!
!
!
!
      character*38 lcol0
      character*38 lcol1(10)
      character*38 lcol2(10)
!
!
      integer*2 irow
      INTEGER*2 ibeg
!
! data statement for first column.
!         What appears on screen
      data &
     &lcol0/  "On line (H)elp                      " /
!
      data &
     &lcol1/  "(I)terate to unity                  ", &
     &   "(U)pdate weights once               ", &
     &   "(R)e-initialize errors to zero      ", &
     &   "re-read the (N)amfil weights        ", &
     &   "Good (C)hoice values: 10 ps 100 fs/s", &
     &   "(E)nter specific values             ", &
     &   "Change to (G)lobal weights          ", &
     &   "Change to (S)tation weights         ", &
     &   "Change to (B)aseline weights        ", &
     &   "(D)isplay current weights           "/
!
! data statement for second column.
      data &
     &lcol2/  "(M)aximum iterations:               ", &
     &   "(T)olerance for convergence:        ", &
     &   "(P)ause in iteration:               ", &
     &   "(F)all back mode:                   ", &
     &   "e(X)perimental new-weights:         ", &
     &   "f(L)oor                             ", &
     &   "Ceiling (%)                         ", &
     &   "Find Baseline co(V)ariance ", &
     &   "(A)bort without storing weights     ", &
     &   "Return to (O)ptin storing weights   "/
      INTEGER*4   ILEN, I_LEN
      INTEGER*2  INT2_ARG
!
! Write up the header.
!
      call setcr_mn(i4p0,i4p0 )
      call clear_mn()
!
      ltype='Group'
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) LTYPE = 'Phase'
!
!      call setcr_mn(i4p40,i4p0)
!      call addstr_f("MYWAY Ver. 97.11.24")
!
      call setcr_mn ( 1, 0    )
      call addstr_f ( lcol0               )
      call setcr_mn ( 40, 0   )
      call addstr_f ( "(algorithm MYWAY)" )
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      write(bufstr,1) DBNAME, ltype, lkind(iwt_mode), num_wts
1     &
     & FORMAT(" ",A, 1x, a5," weights in use, kind of weights,number: ",a8,i6)
      call nl_mn()
      call nl_mn()
      call addstr_f(bufstr )
!
!
! The screen is composed of two columns.  We write them up one at a time.
!
! The first column starts at 1 (First character is blank).
      icol4=1
! write up 10 rows, starting at row 4.
      do irow=1,10
        irow4=(irow-1)*2+4
        call setcr_mn(icol4,irow4 )
        call addstr_f(lcol1(irow) )
      end do
!
!
! before writing up the second column we need to do some modifications.
      ibeg=32
      write(lcol2(1)(ibeg:38),'(i2)')   max_iter
      write(lcol2(2)(ibeg:38),'(f6.4)') chi_tol
!
! --- Improvment of the appeareance of CHI_TOL
!
      IF ( LCOL2(2)(32:32) .EQ. ' ' ) LCOL2(2)(32:32) = '0'
      IF ( LCOL2(2)(ILEN(LCOL2(2)):ILEN(LCOL2(2))) .EQ. '0') &
     &     LCOL2(2)(ILEN(LCOL2(2)):ILEN(LCOL2(2)))  =   ' '
      IF ( LCOL2(2)(ILEN(LCOL2(2)):ILEN(LCOL2(2))) .EQ. '0') &
     &     LCOL2(2)(ILEN(LCOL2(2)):ILEN(LCOL2(2)))  =   ' '
      IF ( LCOL2(2)(ILEN(LCOL2(2)):ILEN(LCOL2(2))) .EQ. '0') &
     &     LCOL2(2)(ILEN(LCOL2(2)):ILEN(LCOL2(2)))  =   ' '
!
      write(lcol2(6), &
     & '("f(L)oor     ", f6.1, " ps ", f6.1, " fs/s")') &
     & wt_floor
      IF ( LCOL2(6)(16:16) .EQ. ' ' ) LCOL2(6)(16:16) = '0'
      IF ( LCOL2(6)(26:26) .EQ. ' ' ) LCOL2(6)(26:26) = '0'
!
      write(lcol2(7), &
     & '("Ceiling (%) ", i6, " ps ", i6, " fs/s")') &
     & nint(wt_ceiling(1)),nint(wt_ceiling(2))
!
      if(kpause_reway) then
         lcol2(3)(ibeg:38) = "Yes"
      else
         lcol2(3)(ibeg:38) = "No"
      endif
      if(kfall_back) then
         lcol2(4)(ibeg:38) = "On"
      else
         lcol2(4)(ibeg:38) = "Off"
      endif
      if(knew_update) then
         lcol2(5)(ibeg:38) = "On"
      else
         lcol2(5)(ibeg:38) = "Off"
      endif
!
!
! The second column starts at 40
      icol4=40
! write up 10 rows, starting at row 4.
      do irow=1,10
        irow4=(irow-1)*2+4
        call setcr_mn(icol4,irow4 )
        call addstr_f(lcol2(irow) )
      end do
!
!
! wait for user input.
!!      call setcr_mn(i4p6,i4p4)
      call setcr_mn ( 51, 22 )
!
10    continue
      call senkr_mn(ix,iy,ichar4 )
!
!
      if(cchar(4:4) .ne. ' ') then
        call casefold(cchar )
        do irow=1,10
          laction= get_action_char(lcol1(irow))  !if action character from first column, return
          IF(cCHAR(4:4) .EQ. laction) return
!
!
          laction= get_action_char(lcol2(irow))  !if action character from second column, return
          IF(cCHAR(4:4) .EQ. laction) return
        end do
!
        LACTION = GET_ACTION_CHAR(LCOL0)  ! if action character from the zero-th line
        IF ( CCHAR(4:4) .EQ. LACTION) RETURN
      else if(mod(iy,2) .eq. 1 .or. iy .lt. 4) then
        goto 10                        !ignore odd rows (which are blank) and header rows.
      else
        irow=(iy-4)/2+1
        if(ix .le. 40) then
           laction=get_action_char(lcol1(irow))
        else
           laction=get_action_char(lcol2(irow))
        endif
        return
      endif
      goto 10                         !Unknown case. Should never get here.
      end
      CHARACTER*1 function get_action_char(lstring)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
      CHARACTER*(*) lstring
      INTEGER*2 ind
!
      ind=INDEX(lstring,"(")
!
!
      IF(ind .EQ. 0 .OR. ind.eq.LEN(lstring)) then
        get_action_char= " "
      else
        get_action_char=lstring(ind+1:ind+1)
      endif
      return
      end
