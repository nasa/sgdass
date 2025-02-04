      SUBROUTINE ID_COV_TYPE(CONTROL_PATH,ICONLEN, &
     &                       COVS_WANTED,CONS_OVERRIDE)
!
!     purpose: parse a batch control file to determine the type of covariances
!              desired.
!
      IMPLICIT NONE
!
!
! 2.2 INPUT Variables:
!
!     CONTROL_PATH, iconlen - path to control file and length of path
!
      character*63 CONTROL_PATH
      integer*2 iconlen
!
! 2.3 OUTPUT Variables:
!
!     COVS_WANTED - TYPE OF COVARIANCES DESIRED
!             bit n set - type n desired, where the bit types are:
!                1 - Site positions
!                2 - Site velocities
!                3 - wobble offsets
!                4 - ut1 offset
!     CONS_OVERRIDE - -1 if no override
!                     0-2 to override to that value
!                        (The meanings of the values are given in the Sinex
!                              output specifications:
!                                0 = fixed (approximate)
!                                1 = significant
!                                2 = free or loose)
!
      integer*2 covs_wanted,cons_override
!
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*128 TOKEN,STRING
      INTEGER*2 LENGTH,IDUM,CFREAD,IERR,INUM
      LOGICAL*2 CFEOF
      CHARACTER*10 PSCOPE
      CHARACTER*3 PPARM
      character*79 qstr
      INTEGER*4  IOS
!
! 4.  HISTORY
!
!   written 7/8/96 by kdb
!
! 5.  id_cov_type PROGRAM STRUCTURE
!
!     Initialize
!
      CALL CFOPEN(CONTROL_PATH(1:ICONLEN))
!
!     Read first record, and then loop until end of file
!     The only record of interest is the $OUTPUT section's COVARIANCES record.
!     Pull the scope of the covariances (e.g., by arc, cgm) plus the parameters
!     desired (eop, sites).
!
      PSCOPE = ' '
      PPARM = ' '
      LENGTH=CFREAD(STRING)
      DO WHILE (.NOT. CFEOF(IDUM))
        CALL SPLITSTRING(STRING,TOKEN,STRING)
        IF(TOKEN.EQ.'$OUTPUT') THEN
!*********
          LENGTH=CFREAD(STRING)
          DO WHILE(STRING(1:1).EQ.' '.AND..NOT.CFEOF(IDUM))
            CALL SPLITSTRING(STRING,TOKEN,STRING)
            IF (TOKEN.EQ.'COVARIANCES') THEN
              CALL SPLITSTRING(STRING,TOKEN,STRING)
              IF (TOKEN.EQ.'YES') THEN
                CALL SPLITSTRING(STRING,PSCOPE,STRING)
                CALL SPLITSTRING(STRING,PPARM,STRING)
!               The dbname scope has an extra field (the version attached).
!               If that's the option given, read one more time to get the
!               parameters desired.
                READ(PPARM,*,IOSTAT=IOS ) INUM
                IERR = IOS
                IF (IERR.EQ.0) CALL SPLITSTRING(STRING,PPARM,STRING)
              ENDIF
            ENDIF
            LENGTH=CFREAD(STRING)
          ENDDO
          CALL CFUNRD(LENGTH,STRING)
!***************
        ENDIF
        LENGTH=CFREAD(STRING)
      ENDDO
!
      CLOSE (92)
!
!     Now analyze the scope and requested parameters to determine which
!     parameters should be printed in the covariance output.  Note that not
!     all COVARIANCES specifications are supported.
!
      if (pscope.eq.'CGM'.and.PPARM.eq.'STA') then
!       Set up to handle site positions and velocities, with the
!       constraints set by code later.
        covs_wanted = 3
        cons_override = -1
      else if (pscope.eq.'BY_ARC'.and.PPARM.eq.'ALL')then
!       Set up to handle site positions and velocities and the eop offsets,
!       with weak constraints.
        covs_wanted = 15
        cons_override = 2
      else
        write(qstr,"('Unsupported covariance setup in control file:')")
        call asnl(qstr)
        write(qstr,"(A,1x,A)") pscope,pparm
        call asnl(qstr)
        write(qstr,"('Covariance output not possible.')")
        call asnl(qstr)
        write(qstr,'("Any key to continue")')
        call asnl(qstr)
        call getstr_f(qstr)
        covs_wanted = 0
        cons_override = -1
      endif
!
      RETURN
      END
