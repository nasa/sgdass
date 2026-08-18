      SUBROUTINE CONS_LEVS(CONTROL_PATH,ICONLEN, &
     &                     NUM_MASTER,MASTER_LIST, &
     &                     CONSTRAINT_POS,CONSTRAINT_VEL, &
     &                     CONST_VEL_SPECIFIC,VELOCITY_TIE_STATUS, &
     &                     NUMVELGRP)
!
!     purpose: parse a batch control file to determine which stations are
!              constrained.  Return:
!                constraint_pos, _vel - bit arrays corresponding to the
!                  input site list where a bit turned on indicates a site is
!                  constrained (by any of various $CONSTRAINT or $SUPPRESSION
!                  position/velocity constraints)
!                CONST_VEL_SPECIFIC - bit array indicating whether site is
!                  constrained by the $CONSTRAINTS VELOCITIES keyword.
!                velocity_tie_status - numeric array indicating whether site is
!                  in velocity tie group or not
!                    (If so, gives # of group.  If not, set to zero.
!                     This is from the $SUPPRESSION VELOCITY_TIE keyword.
!                numvelgrp - # of velocity tie groups
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
! 2.2 INPUT Variables:
!
!     CONTROL_PATH, iconlen - path to control file and length of path
!     MASTER_LIST - master site list
!     NUM_MASTER - number of sites in master list
!
!
      character*157 CONTROL_PATH
      integer*2 iconlen,num_master
      character*8 master_list(*)
!
! 2.3 OUTPUT Variables:
!
!     CONSTRAINT_POS - bit array indicating whether or not a site's XYZ
!                      position parameters are constrained.  (Either all or
!                      none are.)   Bit on means site is constrained.
!                      Tracks various constraints from the $SUPPRESSION and
!                      $CONSTRAINTS section of the batch control file.
!     CONSTRAINT_VEL - equivalent of CONSTRAINT_POS for XYZ velocities.
!     CONST_VEL_SPECIFIC  - bit array tracking whether or not sites are
!                      specifically
!                      constrained by the $CONSTRAINTS VELOCITIES keyword.
!     VELOCITY_TIE_STATUS - numeric array indicating whether site is
!                           in velocity tie group or not.
!                           If so gives # of group.  If not, set to zero.
!                           This is from the $SUPPRESSION VELOCITY_TIE keyword.)
!     NUMVELGRP - number of velocity tie groups
!
      integer*2 constraint_pos(*), constraint_vel(*), CONST_VEL_SPECIFIC(*)
      integer*2 velocity_tie_status(*)
      integer*2 numvelgrp
!
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*128 TOKEN,STRING
      INTEGER*2 LENGTH,IDUM,CFREAD,ict
      LOGICAL*2 CFEOF
      CHARACTER*3 STACMP
!
! 4.  HISTORY
!
!   written 4/13/95 by kdb, based on solve/batch/ctrlfl.f
!
!   modified:
!
!   kdb  4/19/95 fix memory protection error
!   kdb 10/26/00 range checking is being enabled, so convert arguments from (1)
!                to (*).
!   kdb  9/19/01 Return bit array telling whether sites are constrained
!                specifically by $CONSTRAINTS VELOCITIES keyword and whether
!                sites are in $SUPPRESSION VELOCITY_TIE group or not.
!   kdb  4/28/06 Change control_path declaration from *63 to *157 to match
!                the declaration in the corresponding variable in gsnoop common.
! 5.  cons_levs PROGRAM STRUCTURE
!
!     Initialize
!
      CALL CFOPEN(CONTROL_PATH(1:ICONLEN) )
!
      DO ICT = 1,NUM_MASTER
        CALL SBIT( CONSTRAINT_POS, ICT, INT2(0) )
        CALL SBIT( CONSTRAINT_VEL, ICT, INT2(0) )
        CALL SBIT( CONST_VEL_SPECIFIC, ICT, INT2(0) )
        velocity_tie_status(ict) = 0
      END DO
      numvelgrp = 0
!
      STACMP = ' '
!
!     Read first record, and then loop until end of file
!     The only records of interest are the $CONSTRAINTS and $SUPPRESSION
!     section records (plus the $FLAGS section, to set some information
!     needed to parse the other sections).
!
      LENGTH=CFREAD(STRING)
      DO WHILE (.NOT. CFEOF(IDUM))
        CALL SPLITSTRING(STRING,TOKEN,STRING )
!
!       Handle $FLAGS section
!
        IF(TOKEN.EQ.'$FLAGS') THEN
          CALL GFLAGS_GN(STACMP )
!
!       Handle $SUPPRESSION section
!
        ELSE IF(TOKEN.EQ.'$SUPPRESSION') THEN
          CALL GSUPRS_GN(STACMP,NUM_MASTER,MASTER_LIST, &
     &                   CONSTRAINT_POS,CONSTRAINT_VEL, &
     &                   VELOCITY_TIE_STATUS, NUMVELGRP )
!
!       Handle $CONSTRAINTS section
!
        ELSE IF(TOKEN.EQ.'$CONSTRAINTS') THEN
          CALL GCONST_GN(NUM_MASTER,MASTER_LIST,CONSTRAINT_VEL, &
     &                   CONST_VEL_SPECIFIC )
        ENDIF
!
        LENGTH=CFREAD(STRING)
      ENDDO
!
      CLOSE (92)
!
      RETURN
      END
