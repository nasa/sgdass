      SUBROUTINE INTORCON ( ITYPE, IX, IMINX_INT, IMAXX_INT, IMINX_CON )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  INTORCON PROGRAM SPECIFICATION
!
! 1.1 Allow setting values associated with estimating constrained
!     rates (either interval/epochs or constraints)
!
! 1.2 REFERENCES:
!
! 2.  INTORCON INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      integer*2 itype
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rmflg
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
!
      INTEGER*2 JKONT, ichr, jchar,curtyp, &
     & I, KK, K, JJ, II, ILOC, KBITN, KERR, &
     & LEPS, LPSI, L, NLTIDE, KKK, IIROT, JERR, NLCOR, ICOD, J, &
     & III, ICOUNT, LP, ITYP, IMIN, IH, ID, IM, IYR, IROTT, IDIR, &
     & IREGB, IREGA, ITTYP, IPOS
      INTEGER*4 IX, IY, IXD, IYD, IDUM, ICH, ILLOC, NCNT, IPOS4, NLREL, &
     &          IONF(3), IMINX_CON, IMINX_INT, IY1
      INTEGER*4  I_LEN
      character*4 cch
      character*2 cchar
      equivalence ( ichr,cchar)
      equivalence ( ich,cch)
!
      LOGICAL*2 IP,KBIT,TOG_OK,kston
      CHARACTER*3 RES_FLAG,COR_FLAG
      CHARACTER*2 MAM(2)
      character*79 bufstr
!
!
      INTEGER*2 IDSP(16),KKBUF(188)
      INTEGER*2 NSTPLT(3),NSOLVE(3)
      INTEGER*2 IRC(2,3)
      INTEGER*2 IFIRST_LINE(2),ILAST_LINE(2),IDISP,ICHOICE, &
     &  IMINX_TOG(3), &
     &  IMAXX_CON,IMAXX_INT,IMAXX_TOG(3)
      CHARACTER*3 CDISP(3)
      CHARACTER*10 EOP_DISP_A1(2)
!
!     Scheduling of SLVEB moved here, from PROC:
!     Specifications follow (5 lines)
!
!     Modified so that select baselines and select
!     data bases can be passed into 'RMFLG'.
!     Data type for solution put under character (:).
!
!     2004.05.05:jwr: I_LEN put in buffer reads.
!
      REAL*8 REG
      INTEGER*2 IREG(2)
      EQUIVALENCE (REG,IREG,IREGA),(IREG(2),IREGB)
      REAL*8 FJDOBS,LJDOBS,RANGE_LEFT,TOLERANCE
      INTEGER*2 IBIT,ITYP_START,ITYP_STOP
      INTEGER*4 I4P0, I4P1, I4P5, I4P3, I4P13, I4P20, I4P33, &
     &   I4P14, I4P2, I4P58, I4P7, I4P69
      DATA      I4P0, I4P1, I4P5, I4P3, I4P13, I4P20, I4P33, &
     &   I4P14, I4P2, I4P58, I4P7, &
     &  I4P69/ 0, 1, 5, 3, 13, 20, 33, 14, 2, 58, 7, 69 /
!
!
      DATA ifirst_line/1,6/,ilast_line/3,8/
      DATA NSTPLT/2HST,2HPL,2HT /,NSOLVE/2HSO,2HLV,2HE /
      DATA IRC/2HXW,2HOB,2HYW,2HOB,2HUT,2H1 /
      DATA EOP_DISP_A1 /"X/Y WOBBLE","UT1-TAI   "/
      DATA MAM /"ma"," m"/
!
!     Partial soft coding for the location of the earth orientation fields
!
      DATA IMINX_TOG /13,40,57/, IMAXX_TOG /37,54,76/
      DATA IONF /35,52,74/
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!  mwh  940802  Created
!
! 5.  INTORCON PROGRAM STRUCTURE
!
!
      curtyp = itype
650   CONTINUE
      call setcr_mn(I4P0,I4P0 )
      call clear_mn()
!
!     Set up polar motion and UT1 flags.
      WRITE(bufstr,25)
      call addstr_f(bufstr(:70) )
 25   FORMAT("Polar motion and UT1 parameters")
      call setcr_mn(I4P0,I4P1 )
      DO 300 ITTYP = 1,2 !running over x/y wobble, then UT1
!
!
!         Dealing with the earth orientation parameterization style where
!         at least an unconstrained offset and a global rate will be
!         estimated.  In addition the user may estimate rates associated
!         with individual epochs of equal length, plus a diurnal and/or
!         semi-diurnal sine wave.
!
!         first line - offset and global rate
!
            if (kbit( eopa1_choice(ittyp), INT2(1) )) then
                  WRITE(bufstr,74) EOP_DISP_A1(ITTYP)
            else
                  WRITE(bufstr,75) EOP_DISP_A1(ITTYP)
            endif
            call addstr_f(bufstr )
            call nl_mn()
   74     FORMAT (A10,":",2X,"unconstrained offset ON                ")
   75     FORMAT (A10,":",2X,"unconstrained offset and global rate ON")
!
!         second line - fields for toggling sines and rates
!
            DO I = 1,3
              IF (KBIT(EOPA1_CHOICE(ITTYP),I)) THEN
                CDISP(I) = 'ON '
              ELSE
                CDISP(I) = 'OFF'
              END IF
            END DO
            IF (ITTYP .EQ. 1) THEN !wobble
              WRITE(bufstr,76) (CDISP(I),I=1,3)
  76          FORMAT(13X,"(&) constrained rates ",A3,2X, &
     &                   "($) diurnal ",A3,2X, &
     &                   "(%) semi-diurnal ",A3)
            ELSE !ut1
              WRITE(bufstr,77) (CDISP(I),I=1,3)
  77          FORMAT(13X,"(@) constrained rates ",A3,2X, &
     &                   "(<) diurnal ",A3,2X, &
     &                   "(>) semi-diurnal ",A3)
            END IF
            call addstr_f(bufstr )
            call nl_mn()
!
!         third line - rates values
!
            IF (KBIT( EOPA1_CHOICE(ITTYP), INT2(1) )) THEN
              WRITE(bufstr,78) NROT_A1(ITTYP), &
     &            ROT_INTERVAL(ITTYP)* 24.0D0,SEOCNST(ITTYP),MAM(ITTYP)
  78        FORMAT (13X,"# Epochs ",I3,"     Interval ",F6.3," hrs", &
     &          "      Constraint ",F5.2,1X,A2,"s/day")
            ELSE
              WRITE(bufstr,"(79X)")
            END IF
            call addstr_f(bufstr )
            call nl_mn()
            call nl_mn()
            call nl_mn()
300    continue
        ittyp=2
        call setcr_mn(I4P0,I4P20 )
        call addstr_f("Return to (L)ast page" )
!
      if (curtyp.eq.1) then
        call setcr_mn(I4P14,I4P2 )
      else
        call setcr_mn(I4P14,I4P7 )
      endif
  660 CALL senkr_mn(IX,IY,ICH)
      cchar(1:1) = cch(4:4)
      if (cchar(1:1).eq.'L'.or.(cchar(1:1).eq.' '.and.iy.eq.20)) then
        return
      endif
!
!     Redefine commands to toggle rates and sines within style 1 of the earth
!     orientation parameterization: the part of the code which processes these
!     commands expects them to be expressed as a blank given at a specific
!     x & y coordinate
!
      IF (CCHAR(1:1).EQ.'&' .or. CCHAR(1:1).EQ.'$' .or. &
     &    CCHAR(1:1).EQ.'%'  .or. CCHAR(1:1).EQ.'@' .or. &
     &    CCHAR(1:1).EQ.'<' .or. CCHAR(1:1).EQ.'>') THEN
!
!
          IF (CCHAR(1:1).EQ.'&') THEN !wobble constrained rates
            IY = IFIRST_LINE(1) + 1
            IX = IONF(1)
          ELSE IF (CCHAR(1:1).EQ.'$') THEN !wobble diurnal
            IY = IFIRST_LINE(1) + 1
            IX = IONF(2)
          ELSE IF (CCHAR(1:1).EQ.'%') THEN !wobble semi-diurnal
            IY = IFIRST_LINE(1) + 1
            IX = IONF(3)
          ELSE IF (CCHAR(1:1).EQ.'@') THEN !ut1 constrained rates
            IY = IFIRST_LINE(2) + 1
            IX = IONF(1)
          ELSE IF (CCHAR(1:1).EQ.'<') THEN !ut1 diurnal
            IY = IFIRST_LINE(2) + 1
            IX = IONF(2)
          ELSE IF (CCHAR(1:1).EQ.'>') THEN !ut1 semi-diurnal
            IY = IFIRST_LINE(2) + 1
            IX = IONF(3)
          END IF
        CCHAR = "  "
      END IF !converting symbols to toggle rates and sines
!
!     Change values within the current polar motion or UT1 parameterization
!     style. (E.g., flip the order flags for style 0, change the number of
!     epochs for style 1)
!
!
!     First determine if we are dealing with x-wobble, y-wobble or UT1.
!
      ITTYP = 1
      DO ITYP = 1,2
        IF (IY .GE. IFIRST_LINE(ITYP) .and. &
     &      IY .LE. ILAST_LINE(ITYP) ) ITTYP = ITYP
      END DO
      ityp = ittyp
!
!     Now we can figure out which parameterization scheme we're dealing
!     with, and how to interpret the screen
!
        IF (IY .EQ. IFIRST_LINE(ITTYP) + 1) THEN
!
!         The user wants to toggle one of the sines or the constrained rate
!         breaks
!
          ICHOICE = 0
          DO I = 1,3
            IF (IX .GE. IMINX_TOG(I) .AND. IX .LE. &
     &         IMAXX_TOG(I))ICHOICE = I
          END DO
          IF (ICHOICE .NE. 0) THEN !picked a valid choice
!
!           First do what the user is asking
!
            IF (KBIT(EOPA1_CHOICE(ITYP),ICHOICE)) THEN
              IDISP = 0
              CDISP(1) = 'OFF'
            ELSE
              IDISP = 1
              CDISP(1) = 'ON '
            END IF
            CALL SBIT(EOPA1_CHOICE(ITYP),ICHOICE,IDISP )
            CALL setcr_mn(IONF(ICHOICE),IY )
            WRITE(bufstr,726) CDISP(1)
  726       FORMAT (A3)
            call addstr_f(bufstr(:3) )
!
            IF (ICHOICE .EQ. 1) THEN !rates are a bit more complicated
              call setcr_mn(I4P0,iy-I4P1 )
              if (idisp.eq.1) then
                write(bufstr,74) eop_disp_a1(ityp)
              else
                write(bufstr,75) eop_disp_a1(ityp)
              endif
              call addstr_f(bufstr )
              CALL SBIT( CONSTRAINT_BITS, INT2(ITYP+3), IDISP )
              CALL setcr_mn(I4P0,IY+I4P1 )
              IF (IDISP .EQ. 0) THEN !must also blank out interval etc. info
                WRITE(bufstr,'(79X)')
              ELSE !must display interval etc. info
                WRITE(bufstr,78) NROT_A1(ITYP), &
     &            ROT_INTERVAL(ITYP)* 24.0D0,SEOCNST(ITYP),MAM(ITYP)
              END IF
              call addstr_f(bufstr )
              call nl_mn()
            END IF
!
!           In addition, this interlocking feature will turn off rates if
!             the user has turned on a sine or turn off sines if the user
!             has turned on rates. Do this because rate plus sine solutions
!             aren't done,
!             and this will save the user a few steps.
!
            IF (IDISP .EQ. 1) THEN !the user is turning something on
              IF (ICHOICE .EQ. 1) THEN !rates on, so sines off
                DO I = 2,3
                  CALL SBIT( EOPA1_CHOICE(ITYP), I, INT2(0) )
                  CALL setcr_mn(IONF(I),IY )
                  call addstr_f("OFF" )
                END DO
              ELSE !sine(s) on, so rates off
                CALL SBIT( EOPA1_CHOICE(ITYP), INT2(1), INT2(0) )
                CALL setcr_mn(IONF(1),IY )
                call addstr_f("OFF" )
                CALL SBIT( CONSTRAINT_BITS, INT2(ITYP+3), INT2(0) )
                CALL setcr_mn(I4P0,IY+I4P1 )
                WRITE(bufstr,'(79X)')
                call addstr_f(bufstr )
              END IF  !rates on vs sines on
            END IF !turning on something, so turn off other type
!
!           place the cursor somewhere useful
!
            IF (ICHOICE .EQ. 1) THEN
              CALL setcr_mn(IMINX_INT,IY +I4P1 )
            ELSE
              CALL setcr_mn(IONF(5-ICHOICE),IY )
            END IF
            GO TO 660
          ELSE
            GO TO 660 !on rate/sine toggle line, but not in proper field
          END IF
        ELSE IF (IY .EQ. IFIRST_LINE(ITTYP) + 2) THEN !# epochs or constraint
!
!         The user wants to change a value associated with estimating
!         constrained rates (either the number of epochs or the constraint)
!
          IF ( KBIT( EOPA1_CHOICE(ITYP), INT2(1) )  ) THEN
!
!           These values are only valid if constrained rates are being
!           estimated
!
            IF (IX .GE. IMINX_INT  .AND. IX .LE. IMAXX_INT) THEN
!
!             Changing the number of epochs
!
 795          CALL setcr_mn(I4P13,IY+1)
              call addstr_f("New interval in hrs?                " )
              CALL setcr_mn(I4P33,IY+1 )
              call getstr_f(bufstr )
              READ(bufstr,*,ERR=795) ROT_INTERVAL(ITYP)
              if (rot_interval(ityp).le.0) goto 795
              ROT_INTERVAL(ITYP) = ROT_INTERVAL(ITYP)/24.0D0 !converted to day
              CALL OBSTM(FJDOBS,LJDOBS )
!
!             Count up the number of intervals by adding on interval lengths
!             until the resulting epochs no longer fall within the experiment
!             (or fall too close to the end to make a new epoch worthwhile).
!             (Specifically, if an epoch would fall less than 10% of the
!             interval size away from the end of the experiment, leave it out.)
!
              NROT_A1(ITYP) = 1  !epoch at beginning of experiment
              TOLERANCE = ROT_INTERVAL(ITYP) * 0.1D0
              RANGE_LEFT =  (LJDOBS - FJDOBS) - ROT_INTERVAL(ITYP)
              DO WHILE (RANGE_LEFT .GT. TOLERANCE)
!               add another epoch
                NROT_A1(ITYP) = NROT_A1(ITYP) + 1
                RANGE_LEFT = RANGE_LEFT - ROT_INTERVAL(ITYP)
              END DO
              CALL setcr_mn(I4P0,IY )
              WRITE(bufstr,78) NROT_A1(ITYP), &
     &          ROT_INTERVAL(ITYP)* 24.0D0,SEOCNST(ITYP),MAM(ITYP)
              call addstr_f(bufstr )
              call nl_mn()
              bufstr = ' '
              call addstr_f(bufstr )
              CALL setcr_mn(IMINX_CON+I4P3,IY )
              GO TO 660
            ELSE IF (IX .GE. IMINX_CON ) THEN
!
!             Changing the constraint
!
 796          CALL setcr_mn(IMINX_CON,IY+1)
              WRITE(bufstr,'("New value (",A2,"s/day)?",8X)') MAM(ITYP)
              call addstr_f(bufstr )
              CALL setcr_mn(IMINX_CON+I4P20,IY+1 )
              call getstr_f(bufstr )
              READ ( BUFSTR(1:I_LEN(BUFSTR)), *, ERR=796 ) SEOCNST(ITYP)
              CALL setcr_mn(I4P0,IY )
              WRITE(bufstr,78) NROT_A1(ITYP), &
     &          ROT_INTERVAL(ITYP)* 24.0D0,SEOCNST(ITYP),MAM(ITYP)
              call addstr_f(bufstr )
              call nl_mn()
              bufstr = ' '
              call addstr_f(bufstr )
              if (ityp.eq.1) then
                call setcr_mn(I4P14,I4P7 )
              else
                call setcr_mn(I4P14,I4P2 )
              endif
              GO TO 660
            ELSE !mistake
              GO TO 660 !on interval/constraint line, but not in a valid field
            END IF
          ELSE !constrained rates not being estimated
            GO TO 660
          END IF
        END IF
        goto 660
!
      RETURN
      END
