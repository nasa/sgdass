      SUBROUTINE GSUPRS_GN(STACMP,NUM_MASTER,MASTER_LIST, &
     &                     CONSTRAINT_POS,CONSTRAINT_VEL, &
     &                     VELOCITY_TIE_STATUS, NUMVELGRP)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
! 1.  GSUPRS_GN PROGRAM SPECIFICATION
!
! 1.1 Parse SUPPRESSION section of the control file to determine which
!     stations' position and/or velocity (XYZ) parameters are constrained.
!     Set bit arrays accordingly .
!     Also return velocity_tie_status for each site
!           (0 for independent velocity,
!            positive for tied velocity (actual number gives the group,
!              that is, all sites with a value of n are tied together))
!
! 1.2 REFERENCES:
!
! 2.  GSUPRS_GN INTERFACE
!
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
!     stacmp - type of parameters being estimated (XYZ or UEN)
!     MASTER_LIST - master site list
!     NUM_MASTER - number of sites in master list
!
      integer*2 num_master
      character*8 master_list(*)
      CHARACTER*(*) STACMP
!
! 2.3 OUTPUT Variables:
!
!     constraint_pos, constraint_vel - tracks whether any of the constraints
!        of interest, either from $SUPPRESSION or $CONSTRAINTS section,
!        are applied to the site.
!     velocity_tie_status - tells whether or not a given site is in a
!         velocity tie group (if so, gives # of group) or not (zero)
!     numvelgrp - number of velocity tie groups, or zero if no groups
      INTEGER*2 CONSTRAINT_POS(*),CONSTRAINT_VEL(*)
      INTEGER*2 VELOCITY_TIE_STATUS(*)
      INTEGER*2 NUMVELGRP
!
! 2.4 COMMON BLOCKS USED
!
!
! 2.5 SUBROUTINE INTERFACE
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*128 STRING,TOKEN,OLD_TOKEN
      INTEGER*2 LENGTH,IDUM,ITOKEN(40),BLANK
      LOGICAL*2 LDUM,KBIT
      INTEGER*2 NUMSTAGRP,ICT,ISTASP,DEFVEL,DEFCMP,JCT, &
     &          CMPSUP(STA_BIT_WORDS,4),VELSUP(STA_BIT_WORDS,4), &
     &          KCT,IPOINT,LCT
      CHARACTER*8 CSITE1,CSITE2,CSITE_AT,CSITE_TO
      INTEGER*2 STATIES(MAX_STA),VELTIES(MAX_STA),DATSTA(4),DTOSTA(4), &
     &          STASUP(4,MAX_STA)
      CHARACTER*8 STASUP_CHR(MAX_STA)
      EQUIVALENCE ( STASUP, STASUP_CHR )
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 CFREAD,TRIMLEN
      LOGICAL*2 CFEOF
!
      EQUIVALENCE (TOKEN,ITOKEN(1))
!
      DATA BLANK/2H  /
!
! 4.  HISTORY
!
!   written 4/13/95 by kdb (based on solve/batch/gsuprs)
!
!   modified:
!
!   kdb  4/19/95 fix memory protection error
!   kdb  11/12/97 move solve.i from /data15 to /data18
!   kdb   4/22/99 relative include reference, now that gsnoop has joined the
!                 standard source directory tree.
!   kdb  10/26/00 range checking is being enabled, so convert arguments from (1)
!                 to (*).
!   kdb  9/19/01  return velocity_tie_status.
!   kdb   4/28/06 Gveltie (a solve/batch subroutine) now treats stasup as a
!                 character variable.  Update the gveltie call.
!
!
! 5.  GSUPRS_GN PROGRAM STRUCTURE
!
      numstagrp=0
      numvelgrp=0
      DEFVEL=0
      DEFCMP=0
      ISTASP = 0
      do ict=1,max_sta
        staties(ict)=0
        velties(ict)=0
      enddo
      DO JCT=1,3
        DO ICT=1,STA_BIT_WORDS
          CMPSUP(ICT,JCT)=0
          VELSUP(ICT,JCT)=0
        ENDDO
      ENDDO
      DO ICT=1,STA_BIT_WORDS
        CMPSUP(ICT,4)=0
      ENDDO
!
      DO ICT=1,4
        DATSTA(ICT)=BLANK
        DTOSTA(ICT)=BLANK
      ENDDO
!
      LENGTH=CFREAD(STRING)
      DO WHILE(STRING(1:1).EQ.' '.AND..NOT.CFEOF(IDUM))
        DO WHILE(TRIMLEN(STRING).GT.0)
          CALL SPLITSTRING(STRING,TOKEN,STRING )
!
! 'VELOCITIES' KEYWORD
!
          OLD_TOKEN = TOKEN
          IF(TOKEN.EQ.'VELOCITIES') THEN
            CALL GVELSP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING )
          ELSE IF(TOKEN.EQ.'VELOCITY_ORIGIN') THEN
            CALL gvlosp(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING,LDUM )
          ELSE IF(TOKEN.EQ.'VELOCITY_TIE') THEN
            CALL gveltie(numvelgrp,velties,stasup_chr,istasp,token,string, &
     &           defvel,velsup,defcmp,cmpsup )
          ELSE IF(TOKEN.EQ.'STATION_TIE') THEN
            CALL gstatie(numstagrp,staties,stasup,istasp,token,string, &
     &           defvel,velsup,defcmp,cmpsup )
          ELSE IF(TOKEN.EQ.'DIRECTION') THEN
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            CALL UNDSCR(TOKEN )
            CALL CHAR2HOL( TOKEN, DATSTA, INT2(1), INT2(8) )
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            CALL UNDSCR(TOKEN )
            CALL CHAR2HOL( TOKEN, DTOSTA, INT2(1), INT2(8) )
            WRITE (CSITE_AT,"(4A2)") (DATSTA(LCT),LCT=1,4)
            WRITE (CSITE_TO,"(4A2)") (DTOSTA(LCT),LCT=1,4)
          ELSE IF(TOKEN.EQ.'STATIONS') THEN
            CALL GSTASP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  STACMP,TOKEN,STRING )
          ELSE IF(TOKEN.EQ.'STATION_ORIGIN') THEN
            CALL GORISP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING )
          ENDIF
!
!         The first istasp slots of stasup will now contain sites that are to
!         be handled as exceptions to at least one of the $SUPPRESSION section
!         keywords of interest.   All keywords but the direction keyword will
!         have a status array whose first istasp slots will correspond to the
!         exceptional sites.  Slot istasp + 1 of the keywords' arrays will give
!         the status of all other sites.  So loop through the master
!         site list, finding the array position that applies to each site, then
!         mark the site as constrained if any of the keywords applied to it.
!
!         The direction keyword sets a pair of sites, so these sites can be
!         marked as constrained when they are located in the master loop.
!
!         Also record the velocity tie status of each site
!                (0 for no tie OR # of group to which it belongs).
!
          do ict = 1,num_master
            csite1 = master_list(ict)
            ipoint = 0
            do kct = 1,istasp
              write (csite2,"(4a2)") (stasup(lct,kct),lct=1,4)
              if (csite1.eq.csite2) ipoint = kct
            end do
            if (ipoint.eq.0) ipoint = istasp+1
!
!           VELOCITIES keyword: suppresses Up, East or North velocities.
!           Gsnoop is trying to determine which XYZ velocity parameters
!           are constrained,
!           so if any velocity component (U,E or N) is suppressed at a site,
!           gsnoop will consider all of that site's parameters constrained.
!
            if (old_token.eq.'VELOCITIES') then
              if (kbit(velsup(1,1),ipoint).or.kbit(velsup(1,2),ipoint).or. &
     &        kbit(velsup(1,3),ipoint))call sbit( constraint_vel, ict, &
     &        INT2(1))
            end if
!
!           VELOCITY_ORIGIN keyword: a site is either suppressed or not for all
!           XYZ velocity parameters.
!
            if (old_token.eq.'VELOCITY_ORIGIN') then
              if (kbit(velsup(1,4),ipoint))call sbit( constraint_vel, ict, &
     &         INT2(1) )
            end if
!
!           VELOCITY_TIE keyword:  if a site is tied to a group, all of its
!              XYZ velocity parameters are constrained.
!
            if (old_token.eq.'VELOCITY_TIE') then
              if (velties(ipoint).ne.0)call sbit( constraint_vel, ict, &
     &        INT2(1))
              velocity_tie_status(ict) = velties(ipoint)
            end if
!
!           STATIONS keyword: suppresses XYZ or UEN stations.
!
!           UEN case:
!             As above, gsnoop deals with XYZ station parameters, so if any UEN
!             position is suppressed, all gsnoop station parameters are
!             constrained.
!           XYZ case:
!             By user request, gsnoop will assume that if any
!             XYZ parameter is suppressed, the user will have suppressed all
!             of them.
!
            if (old_token.eq.'STATIONS') then
              if (kbit(cmpsup(1,1),ipoint).or.kbit(cmpsup(1,2),ipoint).or. &
     &        kbit(cmpsup(1,3),ipoint))call sbit( constraint_pos, ict, &
     &        INT2(1))
            end if
!
!           STATION_ORIGIN keyword: a site is either suppressed or not for all
!           XYZ site parameters.
!
            if (old_token.eq.'STATION_ORIGIN') then
              if (kbit(cmpsup(1,4),ipoint))call sbit( constraint_pos, ict, &
     &         INT2(1) )
            end if
!
!           STATION_TIE keyword:  if a site is tied to a group, all of its
!              XYZ parameters are constrained.
!
            if (old_token.eq.'STATION_TIE') then
              if (staties(ipoint).ne.0)call sbit( constraint_pos, ict, &
     &        INT2(1))
            end if
!
!           DIRECTION keyword:
!           Both sites in the DIRECTION specification will
!           be constrained.  Note that this keyword affects velocities.
!
            if (old_token.eq.'DIRECTION') then
              IF (CSITE1 .EQ. CSITE_AT .OR. CSITE1 .EQ. CSITE_TO)call &
     &         sbit(constraint_vel, ict, INT2(1) )
            end if
          end do
        ENDDO
        LENGTH=CFREAD(STRING)
      ENDDO
!
      CALL CFUNRD(LENGTH,STRING )
!
      RETURN
      END
