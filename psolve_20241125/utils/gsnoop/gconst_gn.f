      SUBROUTINE GCONST_GN (NUM_MASTER,MASTER_LIST,CONSTRAINT_VEL, &
     &                      CONST_VEL_SPECIFIC)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GCONST_GN PROGRAM SPECIFICATION
!
! 1.1 Parse the CONSTRAINTS section of the control file (specifically the
!     VELOCITIES keyword) to find stations that have been constrained in
!     the solution to the reference frame (e.g., NUVEL-1A) for insufficient
!     data.  Set the output constraint status bit array (CONSTRAINT_VEL)
!     to show that these sites have received a constraint of some kind.
!     (Another sub will set the same array to show other constraints.)
!     Also set the corresponding bits in CONST_VEL_SPECIFIC to show the sites
!     have been constrained specifically via $CONSTRAINTS VELOCITIES.
!     (CONSTRAINT_VEL tracks all velocity constraints on a site.
!      CONST_VEL_SPECIFIC just tracks the $CONSTRAINTS VELOCITIES constraint.)
!
!
! 1.2 REFERENCES:
!
! 2.  GCONST_GN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT and OUTPUT Variables:
!
!     constraint_vel - bit array for the sites' XYZ velocity parameters (in the
!                      order of the gsnoop master site list).  Bit on means
!                      the corresponding site's velocity parameters are
!                      constrained.  All velocity constraints (including ones
!                      from $SUPPRESSION) are tracked here.
!     MASTER_LIST - master site list from covariance file
!     NUM_MASTER - number of sites in master list
!     CONST_VEL_SPECIFIC - bit array for whether the $CONSTRAINTS VELOCITIES
!                          constraint itself has been applied.
!
      integer*2 num_master
      character*8 master_list(*)
      integer*2 constraint_vel(*)
      integer*2 CONST_VEL_SPECIFIC(*)
!
! 2.4 COMMON BLOCKS USED
!
      INCLUDE 'solve.i'
!
! 2.5 SUBROUTINE INTERFACE
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*80 TOKEN
      character*128 strinh
      INTEGER*2 LENGTH,IDUM,I,n,j
      integer*2 istasp,ict,kct,lct
      logical*2 vel_found
      character*80 qstr,cbuffer
      character*8 csite1,csite2
      integer*2 stasup(4,max_sta)
      character*8 stasup_chr(max_sta)
      equivalence (stasup(1,1),stasup_chr(1))
      LOGICAL*2 XYZ_GENERAL,UEN_GENERAL,KCON_SIGMA
      REAL*8 XYZ_CONSTRAINT(3), UEN_CONSTRAINT(3)
      INTEGER*2 XYZ_EXCEPT(STA_BIT_WORDS), UEN_EXCEPT(STA_BIT_WORDS)
      CHARACTER*128 STRINH_TEST
      CHARACTER*80 TOKEN_TEST
      INTEGER*4 IUER
      INTEGER*2 IEXCEPT
      LOGICAL*2 UEN_CONSTRAINED,XYZ_CONSTRAINED,CONSTRAINT_SET
      logical*2 kbit
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 CFREAD,TRIMLEN,VELCON(STA_BIT_WORDS)
      LOGICAL*2 CFEOF
      REAL*8 DEFVELCNST(3),ALTVELCNST(3)
!
! 4.  HISTORY
!
!   written 4/17/95 by kdb (based on solve/batch/gconst.f)
!
!   modified:
!
!   kdb  4/19/95 fix memory protection error
!   kdb  11/12/97 move solve.i from /data15 to /data18
!   kdb   4/22/99 relative include reference, now that gsnoop has joined the
!                 standard source directory tree.
!   kdb  10/26/00 range checking is being enabled, so convert arguments from (1)
!                 to (*).
!   kdb   1/02/01 Allow for new f_solve $CONSTRAINTS VELOCITIES keyword formats
!                 implemented in December 2000 (specifies XYZ and UEN
!                 constraints independently).  This will also be reported
!                 by a new solve/batch routine, gstavelcnst.
!                 (But retain backwards compatibility for older control files.)
!   kdb   9/20/01 Pass up bit array to specifically track whether the
!                 $CONSTRAINTS VELOCITIES keyword has been applied.
!
!
!
! 5.  GCONST_GN PROGRAM STRUCTURE
!
! Initialize some things
!
      istasp = 0
      do i=1,STA_BIT_WORDS
        velcon(i) = 0
      enddo
      do i=1,3
        defvelcnst(i) = -1.d0
      enddo
      do i=1,3
        altvelcnst(i) = -1.d0
      enddo
      XYZ_GENERAL = .false.
      UEN_GENERAL = .false.
      do i=1,3
        xyz_constraint(i) = 0.0d0
        uen_constraint(i) = 0.0d0
      enddo
      do i=1,STA_BIT_WORDS
        XYZ_EXCEPT(i) = 0
        UEN_EXCEPT(i) = 0
      enddo
      kcon_sigma = .false.
!
      vel_found = .false.
!
! Loop through the CONSTRAINTS section, looking for the VELOCITIES keyword
!
      LENGTH=CFREAD(STRINH)
      DO WHILE(STRINH(1:1) .eq.' ' .and. .not.CFEOF(IDUM))
        DO WHILE (TRIMLEN(STRINH) .gt. 0)
          CALL SPLITSTRING(STRINH,TOKEN,STRINH )
          if (TOKEN .eq. 'VELOCITIES') then
            if (vel_found) then
              qstr = 'CONSTRAINTS VELOCITIES keyword found twice'
              call asnl(qstr )
              qstr = 'Not coded to handle this'
              call asnl(qstr )
              qstr = 'Hit any key to continue'
              call asnl(qstr )
              call getstr_f(cbuffer )
            end if
            strinh_test = strinh
            call splitstring(strinh_test,token_test,strinh_test )
            if (token_test .eq. 'XYZ' .or. token_test .eq. 'UEN') then
!             new 12/2000 format of $CONSTRAINTS VELOCITIES keyword
!               in which XYZ and UEN constraints can be specified independently
!              (But for the time being Gsnoop is allowed to assume that if one
!               XYZ component is constrained, all are AND if one UEN component
!               is constrained, all are.
!             note: returned values are:
!                kcon_sigma - indicates that constraint was set using
!                   SIGMA phrase of keyword.  (Not used by gsnoop.)
!                xyz_general - indicates whether constraint will be applied
!                     (true) or not (false) for general list of stations
!                xyz_constraint - values of constraint in x, y, z components
!                stasup_chr - list of stations that are exceptions to one
!                  general rule or another, NOT NECESSARILY the general rule
!                  for $CONSRAINTS VELOCITIES.  To determine if a site is in
!                  stasup_chr because it's an exception to the $CONSTRAINTS
!                  VELOCITIES rule, check the corresponding bit in xyz_except.
!                xyz_except  - bit i indicates whether site i is (true) or
!                      is not (false) an exception to the general
!                      $CONSTRAINTS VELOCITIES rule.  The sites' bits are in
!                      the order given in stasup_chr.
!                uen_general, uen_constraint, uen_except - same as xyz
!                istasp - number of stations that are exceptions to general
!                   rule
!                iuer - error return
!
              call gstavelcnst('VELOCITIES',KCON_SIGMA, &
     &             XYZ_GENERAL,XYZ_CONSTRAINT,XYZ_EXCEPT, &
     &             UEN_GENERAL,UEN_CONSTRAINT,UEN_EXCEPT, &
     &             STASUP_CHR,ISTASP,TOKEN,STRINH,IUER )
!
!             The sinex output reports XYZ parameters.
!             So the sinex output must report if a constraint is applied to the
!             XYZ components for a given site.  (By user request, Gsnoop
!             is allowed to assume that if a constraint is applied at one
!             component, it's applied to all components.)  So the three
!             components at a site will be tracked and reported together.
!             This information is tracked by the site's bit within
!             CONSTRAINT_VEL.  So set these bits now.
!
!             Run over sites in the gsnoop master list.
!
              do ict = 1,num_master
!
!               See if this site is in the exception list, and if so, get
!               its pointer in the exception list.
!
                csite1 = master_list(ict)
                iexcept = 0
                do kct = 1,istasp
                  csite2 = stasup_chr(kct)
                  if (csite1.eq.csite2) then
                    iexcept = kct
                  end if
                enddo
!
!               Now see if the specified XYZ/UEN constraints were intended
!               to be applied to this site or not.
!
                if (iexcept.eq.0) then
!                 This is a general site.  Use the general flag.
                  uen_constrained = uen_general
                  xyz_constrained = xyz_general
                else
!                 This site is in the exception list.  But the exception list
!                 may contain sites that are exceptions to rules other than
!                 the $CONSTRAINTS VELOCITIES keyword.   So check the bit
!                 arrays that tell whether the site is an exception to this
!                 rule or not.
                  if (kbit(uen_except,iexcept)) then
!                   The site is an exception to this rule.  Don't follow the
!                   general setting.
                    uen_constrained = .not. uen_general
                  else
!                   The site is not an exception to this rule.  Follow the
!                   general setting.
                    uen_constrained = uen_general
                  endif
                  if (kbit(xyz_except,iexcept)) then
                    xyz_constrained = .not. xyz_general
                  else
                    xyz_constrained = xyz_general
                  endif
                endif
!
!               The specified constraints are unlikely to be all zeroes, which
!               would make no sense under the new syntax.  But check just in
!               case (for example if a user wanted to quickly turn off the
!               constraint by zeroing it out and not modifying the syntax).
!               If the constraints are all zero, there is actually no
!               constraint.
!
                constraint_set = .false.
                do kct = 1,3
                  if(uen_constraint(kct).gt.0.0D0) constraint_set = .true.
                enddo
                if (.not.constraint_set) uen_constrained = .false.
!
                constraint_set = .false.
                do kct = 1,3
                  if(xyz_constraint(kct).gt.0.0D0) constraint_set = .true.
                enddo
                if (.not.constraint_set) xyz_constrained = .false.
!
!               Constraining any UEN component will constrain all XYZ
!               components.  And recall that if one XYZ component is
!               constrained, all are assumed to be constrained.
!               So if the site is constrained for any component in the UEN
!               and/or XYZ systems, record that it's constrained in XYZ.
!
                if (uen_constrained .or. xyz_constrained) then
                   call sbit( CONSTRAINT_VEL, ict, INT2(1) )
                   call sbit( CONST_VEL_SPECIFIC, ict, INT2(1) )
                endif
              end do
            else
!
!             original format for $CONSTRAINTS VELOCITIES keyword
!             (note: VELOCITIES NO falls under this path.)
!             This specifies the constraints as UEN, then applies the
!             appropriate XYZ constraints.
!             (Note: by user request, snoop is allowed to assume that if one UEN
!              component is constrained, all are.)
!
              call gvelcnst(defvelcnst,altvelcnst,velcon,stasup, &
     &             istasp,token,strinh )
!
              if (defvelcnst(1).gt.-1.d0) then
!
!               User chose yes case, which applies a constraint to all sites
!               (either the same constraint to all sites or an alternate
!               constraint to selected sites.)
!
                do ict = 1,num_master
                  call sbit( CONSTRAINT_VEL, ict, INT2(1) )
                  call sbit( CONST_VEL_SPECIFIC, ict, INT2(1) )
                end do
              else
!
!               User chose no.  That is, he chose not to apply any constraint to
!               most sites.  However, the user may have specified exceptions
!               which will be constrained.
!
                if (istasp.ne.0) then
!
!                 there were exceptions
!
                  do ict = 1,num_master
                    csite1 = master_list(ict)
                    do kct = 1,istasp
                      write (csite2,"(4a2)") (stasup(lct,kct),lct=1,4)
                      if (csite1.eq.csite2) then
                        call sbit( CONSTRAINT_VEL, ict, INT2(1) )
                        call sbit( CONST_VEL_SPECIFIC, ict, INT2(1) )
                      end if
                    end do
                  end do
                end if
              end if
            endif
            vel_found = .true.
          ENDIF
        ENDDO
!
! Read next record
!
        LENGTH=CFREAD(STRINH)
      ENDDO
!
      CALL CFUNRD(LENGTH,STRINH )
!
      RETURN
      END
