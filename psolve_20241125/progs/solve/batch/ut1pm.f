      SUBROUTINE UT1PM ( ORIENT, UT1FLG, ROTFLG, NUMSTA, ISTAD, VSITEC, &
     &                   NRTARC, NROT, TROT, ROTAP, IBIT, ISITN, &
     &                   CONSTRAINT_BITS, IEND, EOPMID, EOP_EPOCH_MJD, &
     &                   EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI  )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'astro_constants.i'
!
! 1.  UT1PM PROGRAM SPECIFICATION
      LOGICAL*2 L4TOL2
!
! 1.1 Pick rotation epoch and which ones to solve for.
!
! 1.2 REFERENCES:
!
! 2.  UT1PM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) ORIENT,UT1FLG,ROTFLG
      INTEGER*2 NUMSTA,ISTAD(1),NRTARC,ISITN(4,*)
      INTEGER*2 CONSTRAINT_BITS
      INTEGER*4 EOP_EPOCH_MJD, IEND
      REAL*8    ROTAP(MAX_ROT,4), TROT(*), VSITEC(3,*), EOP_EPOCH_SEC 
      REAL*8    TSTART_SEC_TAI, TEND_SEC_TAI
      INTEGER*2 EOPMID
!
! CONSTRAINT_BITS - Flags for application of constraints
! IEND - Number of last observation
! ISTAD - Station data flag
! ISITN - Array of station names
! NRTARC - Earth orientation epoch
! NUMSTA - Total number of stations
! ORIENT - Earth orientation flag (FIXED or FREE?)
! ROTAP - Rotation information. Refer to socom.i
! ROTFLG - Earth orientation components override
! TROT - Compete Julian Date for up to max_rot rotation epochs
! UT1FLG - Earth orientation flag
! VSITEC - Site coordinates for station being processed
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 NROT,IBIT(3)
!
! IBIT - Set bits to 1 or 0
! NROT - Number of rotation eopchs currently in use
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'oborg.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: glsta
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I1, I2, IGLSTA, IDUM2(2), TIMMIN
      REAL*8    DX, DY, DZ, TSTART, TEND, DDUM, TIMFRACT
      REAL*8    EOP_EPOCH_JD, SEC_START, SEC_STOP, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI
      INTEGER*4 TIMINT, MJD_START, MJD_STOP, IUER
      LOGICAL*2, EXTERNAL :: KBIT
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD 
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb  951207  Integer*4 number of observations.
!   jwr  970414  New logic for 'all offset style' of hf eop estimation
!   pet  980923  Corrected an old bug: previous version set EOP epoch to the
!                nearest noon when the nearest midnight was specified.
!                Added support of calculation EOP at the end of the session and
!                at the nearest noon.
!   pet  2000.10.05  Added checking whether EOPMOD has correct value
!   pet  2021.07.12  Added support of EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI
!
! 5.  UT1PM PROGRAM STRUCTURE
!
      NROT=1
      CALL OBSTM ( TSTART, TEND ) ! get time of nominal start and end of session
!
      IBIT(1)=0
      IBIT(2)=0
      IBIT(3)=0
      TROT(1)  = 0.0
      TIMINT   = 0.0
      TIMFRACT = 0.0
      TIMMIN   = 0.0
      TIMFRACT = 0.0
!
      TSTART_SEC_TAI = (TSTART - 2451544.5D0)*86400.D0 + 32.184
      TEND_SEC_TAI   = (TEND   - 2451544.5D0)*86400.D0 + 32.184
!
! --- Check validity range of the EOP estimiation
!
      IF ( EOP_BEFORE_SEC_TAI > -1.D10 ) THEN
           IF ( TSTART_SEC_TAI  > EOP_BEFORE_SEC_TAI ) THEN
                NROT = 0
!
! ------------- The experiment start epoch is after the boundadry epoch --
! ------------- we will not estimate EOP for that experiment
!
                RETURN 
           END IF
      END IF
      IF ( EOP_AFTER_SEC_TAI   > -1.D10 ) THEN
           IF ( TSTART_SEC_TAI < EOP_AFTER_SEC_TAI ) THEN
!
! ------------- The experiment start epoch is before the boundadry epoch --
! ------------- we will not estimate EOP for that experiment
!
                NROT = 0
                RETURN 
           END IF
      END IF
!
      IF ( EOPMID .EQ. 0 ) THEN ! Use the starting epoch
           TROT(1) = TSTART + 32.184D0/86400.0D0
        ELSE IF ( EOPMID .EQ. 1 ) THEN ! Use the middle of the eop epoch
           TROT(1)  = ( (TSTART+TEND) / 2.0D0) + (.05D0 / 1440.D0) 
           TIMINT   = TROT(1) 
           TIMFRACT = TROT(1) - TIMINT
           TIMMIN   = TIMFRACT * 1440.D0
           TIMFRACT = TIMMIN / 1440.D0
           TROT(1)  = TIMINT + TIMFRACT + 32.184D0/86400.0D0
           CALL INTRP_EOVR ( TROT(1), ROTAP(1,1), ROTAP(1,2), ROTAP(1,3), &
     &          ROTAP(1,4), FALSE__L2, DDUM, DDUM, DDUM, IDUM2 )
         ELSE IF ( EOPMID .EQ. 2 ) THEN ! The first noon as the epoch.
           TROT(1) = INT ( TSTART + 0.499999D0 ) + 1.0D0 + 32.184D0/86400.0D0
           CALL INTRP_EOVR  ( TROT(1), ROTAP(1,1), ROTAP(1,2), ROTAP(1,3), &
     &          ROTAP(1,4), FALSE__L2, DDUM, DDUM, DDUM, IDUM2 )
         ELSE IF ( EOPMID .EQ. 3 ) THEN ! The first midnight as the epoch.
           TROT(1) = INT ( TSTART + 0.499999D0 ) + 0.5D0 + 32.184D0/86400.0D0
           CALL INTRP_EOVR  ( TROT(1), ROTAP(1,1), ROTAP(1,2), ROTAP(1,3), &
     &          ROTAP(1,4), FALSE__L2, DDUM, DDUM, DDUM, IDUM2 )
         ELSE IF ( EOPMID .EQ. 4 ) THEN ! The last epoch
           TROT(1) = TEND + 32.184D0/86400.0D0
         ELSE IF ( EOPMID .EQ. 5 ) THEN ! The last epoch
           CALL JD_TO_MJD_SEC ( TSTART, MJD_START, SEC_START )
           CALL JD_TO_MJD_SEC ( TEND,   MJD_STOP,  SEC_STOP  )
           IF ( EOP_EPOCH_SEC + 32.184D0 > SEC_START ) THEN
                TROT(1) = MJD_SEC_TO_JD ( MJD_START, EOP_EPOCH_SEC + 32.184D0 )
              ELSE
                TROT(1) = MJD_SEC_TO_JD ( MJD_START+1, EOP_EPOCH_SEC + 32.184D0 )
           END IF
         ELSE IF ( EOPMID .EQ. 6 ) THEN ! The last epoch
           TROT(1) = J2000__JD + (EOP_EPOCH_SEC + 32.184D0)/86400.0D0
         ELSE IF ( EOPMID .EQ. 7 ) THEN ! The last epoch
           TROT(1) = MJD_SEC_TO_JD ( EOP_EPOCH_MJD, EOP_EPOCH_SEC + 32.184D0 )
         ELSE
           CALL FERR ( INT2(451), &
     &         'ut1pm(BATCH) Wrong value of EOPMOD varaible: '// &
     &         'value of the qualifier of UT1/PM keyword. Should be one of '// &
     &         'START, MIDDLE, NOON, MIDNIGHT, END. Please check your '// &
     &         'control file', INT2(0), INT2(0) )
      ENDIF
!
      IF ( ORIENT .EQ. 'Y'   .AND. UT1FLG .EQ. 'Y' .AND.ROTFLG .EQ. &
     &    '   ' .AND. .NOT. (KBIT( CONSTRAINT_BITS, INT2(1))) ) THEN
           IBIT(1)=1
           IBIT(2)=1
           IBIT(3)=1
           CALL GLSTA ( IGLSTA, I1, I2, NUMSTA, ISTAD, ISITN )
!
           IF ( IGLSTA .LT. 2 ) THEN
                IBIT(1)=0
                IBIT(2)=0
                IBIT(3)=0
             ELSE IF ( IGLSTA .EQ. 2 ) THEN
                DX=ABS(VSITEC(1,I1)-VSITEC(1,I2))
                DY=ABS(VSITEC(2,I1)-VSITEC(2,I2))
                DZ=ABS(VSITEC(3,I1)-VSITEC(3,I2))
                IF ( DX .GT. DY  .AND. DX .GT. DZ ) THEN
                     IBIT(2)=0
                  ELSE IF ( DY .GT. DX  .AND. DY .GT. DZ ) THEN
                     IBIT(1)=0
                  ELSE
                     IBIT(3)=0
                ENDIF
           ENDIF
        ELSE IF ( ROTFLG .NE. '   ' .AND. UT1FLG .EQ. 'Y'  .AND. &
     &            .NOT. (KBIT(CONSTRAINT_BITS, INT2(1)))         ) THEN
           IF ( ROTFLG(1:1) .EQ. 'X' ) IBIT(1)=1
           IF ( ROTFLG(2:2) .EQ. 'Y' ) IBIT(2)=1
           IF ( ROTFLG(3:3) .EQ. 'U' ) IBIT(3)=1
        ELSE IF (KBIT( CONSTRAINT_BITS, INT2(1)) .AND. ORIENT.EQ.'Y') THEN
           IBIT(1)=1
           IBIT(2)=1
           IBIT(3)=1
      ENDIF
!
      RETURN
      END  !#!  UT1PM  #!#
