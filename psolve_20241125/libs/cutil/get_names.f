      SUBROUTINE GET_NAMES ( LNAME, ISTR_LEN, MAX_PARM, NPARM, KSHORT, KGLOBAL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  GET_NAMES PROGRAM SPECIFICATION
!
! 1.1 Return parameter names from SOLVE solution, in NRMFIL
!     matrix order. Assumes that SOCOM and PRFIL are in
!     memory already. (gets user parameters from global file)
!
!     Treatment of the user partials depend on formal argument KGLOBAL
!     and variable KGLOBONLY passed via $PSOLVE_ROOT/include/glbcm
!
!     1) KGLOBAL= .TRUE., KGLOBONLY ignored  -- USRGxx file is read which
!        holds global user_globals which are in CGM already. All these
!        parameters are added to the parameters list.
!     2) KGLOBAL= .FALSE., KGLOBONLY = .TRUE.  -- USRPxx file is read which
!        holds user globals from this session only. Only global user parameters
!        (marked as 'G' at the position 22:22) are added to the list of
!        parameters.
!     3) KGLOBAL= .FALSE., KGLOBONLY = .FALSE. -- USRPxx file is read which
!        holds user globals from this session only. All user partials: global
!        and "for the session only" are added to the list of parameters.
!
!
! 1.2 REFERENCES:
!
! 2.  PARMS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
!
      INTEGER*4 MAX_PARM !  maximum number of parameters
      LOGICAL*2 KSHORT   !  kshort=true return as 20 chars, else 32chars
      LOGICAL*2 KGLOBAL  !  return global or arc parameters?
      INTEGER*2 ISTR_LEN !  length of string in characters
!
! 2.3 OUTPUT Variables:
!
      character*(istr_len) lname(*)   !  parameters which are returned.
      INTEGER*4 NPARM                 !  number of parameters.
!
! 2.4 COMMON BLOCKS USED
!
      INCLUDE 'erm.i'
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
! 3.  LOCAL VARIABLES
!
!
! flag to see uf USRG exists
      LOGICAL*4   KEXIST
!
      INTEGER*2   IOFCMP, KBITN, IEPOCH, USER_PARM(40)
      INTEGER*4   IOS, NBITS
      INTEGER*2   IEOP, IXYU, IORD
      REAL*8      RJD8
      CHARACTER   CBUF*32, CBUF6(6)*32, CDUM*80, FNAME*(NAME_SIZE)
      EQUIVALENCE (CDUM,USER_PARM)
      INTEGER*2   I, J, IAORD, ID, IHR, IM, IMIN, INEXTS, IP, IROTT, ISTA, &
     &            KSTA, JSTA, ITERM, ITIME, ITYP, IY, JATM, JCLOCK, JNUT, &
     &            JSTR, JTYPE, JXYZ, K, LP, M, IT
      INTEGER*2 IGRAD, ISTART, IIN_OUT
      CHARACTER   LCMP(6)*1, XYU_LABEL(3)*3, LEOP(3)*(8)
      DATA        XYU_LABEL /'X W','Y W','UT1'/
!
      INTEGER*4   IXYU_START  (2),          IXYU_STOP    (2)
      DATA        IXYU_START / 1, 3 /,      IXYU_STOP  / 2, 3 /
      DATA LEOP   / "X WOBBLE", "Y WOBBLE", "UT1-TAI" /
!
      CHARACTER   LSD(2)*12     / "Diurnal", "Semi-diurnal" /
      CHARACTER   LSINCOS(2)*3  / "Sin", "Cos"/
      data lcmp   /"X","Y","Z","U","E","N"/
      character*10 ltide_name(3)/"Love #l","Love #h","Lag Angle"/
      character*20 lnutation(2)
      data lnutation/"LONGITUDE NUTATION","OBLIQUITY NUTATION"/
      character*3 lnut_short(2)
      data lnut_short/"Lon","Obl"/
      character*10 lnut_long(2)
      data lnut_long/"Lon. Nut.","Obl. Nut."/
      character*9 lnut_type(2)/"Constant","Slope/Yr"/
      character*9 lnut_term(6)
      data lnut_term/"Principal","9.3 Year","Annual","S. Annual", &
     & "122 Day","13.7 Day"/
      character*9 lin_out(2)/"In Phase ","Out Phase"/
      character*2 lhfeop(4)/"UC","US","PC","PS"/
!
      INTEGER*2   POS_USED(3,MAX_STA)
      CHARACTER   LET*3
      character*2 lgrad_short(2)/"NG","EG"/
      character*10 lgrad_long(2)/"North Grad","East Grad"/
      LOGICAL*4   FL_STA_USED, FL_SOU_USE
      INTEGER*4   J1, J2, J3, J4, J5, J6, J7, IHPE, ISPE, IDEG, IHEO, INOD, ICMP, IND
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      LOGICAL*4,  EXTERNAL :: CHECK_STABIT
      LOGICAL*2,  EXTERNAL :: KBIT
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, LTM_DIF
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   WEH  850225  Created
!   JWR  851216  Replace masking logic with KBIT logic
!   WEH & JWR
!        860110  Added nutation time series logic
!   MK   861105  Added continued atmospheres and atmosphere rates
!   MWH  910306  Modify to handle episodic site motion
!   JMG  960503  Fix bug: only try to open USRGxx if it exists.
!   JMG  960917  Get rid of all hollerith!
!   jwr  970206  Bug in converting clock epochs from real to ymdhm fixed.
!   pet  970421  Added socom-extension and substitutet trot --> eop
!   pet  970423  Added detours for the case of uniform segmentations
!   pet  970428  Added new scheme of clock break handling
!   pet  970429  Fixed bug: for the case of uniform segments added test of bit
!                arrays LATM, LCLK
!   pet  971201  Added logic for bypassing deselected station
!   pet  980124  Restored a piece of archaic code for calculation clock epoch
!                when clock segments are not uniform for comparison purposes
!   pet  990111  Improved comments and error message
!   pet  990706  Removed unused variables
!   pet  2000.10.26  fixed a bug: variable TROT(1) keeps time epoch for UT1 and
!                    polar motion modelled by low degree polynomial,
!                    not TROT_A1 as it was coded previously
!   pet  2002.05.30  changed a little bit syntax of the paramter of station
!                    coordinate: in the case if the station had an epicodic
!                    motion the new format is "ssssssss cyymmdd-COO" where
!                    ssssssss -- station name
!                    c        -- component: X, Y or Z
!                    yymmdd   -- date
!   pet  2002.10.02  Changed format of parameter name for the case of episodic
!                    site motion
!   pet  2005.03.01  Added support of harmonic site position variations and
!                    spline parameterization for site positions
!   pet  2006.01.18  Added support of estimation of parameters related to &
!                    the Earth Rotation Model
!   pet  2006.05.30  Added support of estimation of parameters related to 
!                    the Harmonic variations in the Earth Orientation
!   pet  2022.08.23  Added support of estimation of estimation of ionospheric scale
!
! 5.  GET_NAMES  PROGRAM STRUCTURE
!
!
      IF ( SOCOM_PLUS_FIRST .NE. SPL__DONE ) THEN
           WRITE ( 6, * ) 'GET_NAMES: socom_plus has not been initialized'
           IND = -2147483486
           CBUF6(IND) = 'NONE'
           STOP 'Abnormal termination'
      END IF
!
! --- Setting bit field for station's deselection status
!
      CALL SET_STABIT ( INT2(2) )
      CALL NOUT_I2 ( 3*INT4(MAX_STA), POS_USED )
!
!---- Handle all site related parameters
!
      NPARM=0
      DO ISTA = 1,NUMSTA ! This loop runs over all site related information
!
! ------ Move the site name into IBUF
!
         CALL CLRCH ( CBUF )
         CBUF(1:8) = ISITN_CHR(ISTA)
         INEXTS=9
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
         IF ( .NOT. CHECK_STABIT ( ISTA ) ) GOTO 810
!
! ------ Check site coordinates
!
         IOFCMP=KBITN( IUEN, INT2(1) )*3
         DO JXYZ = 1,3 ! Running over X,Y,Z coordinates!
            CBUF(10:10) = LCMP(JXYZ+IOFCMP)
            IF ( KBIT(LSITEC(1,JXYZ), ISTA) ) THEN
                 IF ( PSITED(ISTA).NE.0 ) THEN
                      DO I=1,PWCNUM(1)
!
! ---------------------- Move the component name to IBUF.
!
                         CALL MDYJL ( IM, ID, IY, IT, PWCEP(I) )
                         WRITE ( UNIT=CBUF(11:20), FMT='(3I2,"-COO")') &
     &                           IY, IM, ID
                         CALL BLANK_TO_ZERO ( CBUF(11:16) )
                         CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, &
     &                                    CBUF )
                      ENDDO
                   ELSE
                      IF ( VSITED(ISTA) .EQ. 0 ) THEN
                           CBUF(12:20)="COMPONENT"
                         ELSE
                           CALL MDYJL ( IM, ID, IY, IT, VSITED(ISTA) )
                           WRITE ( CBUF(11:20), '(3I2,"-POS")' ) IY, IM, ID
                           CALL BLANK_TO_ZERO ( CBUF(11:16) )
                      ENDIF
                      CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                 END IF
            END IF
         END DO
!
! ------ Station velocities
!
         DO JXYZ = 1,3 !Running over X,Y,Z coordinates!
            CBUF(10:20) = LCMP(JXYZ+IOFCMP)//" VELOCITY"
            IF ( KBIT( LSITEV(1,JXYZ), ISTA) ) THEN
                 CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
            END IF      
         END DO
!
! ------ Diurnal radial sign and cosine
!
         IF ( KBIT( IUEN, INT2(2) ) ) THEN
              CBUF(10:11)="U "
              IF ( KSHORT ) THEN
                   CBUF(12:20)="DIURNAL S"
                 ELSE
                   CBUF(12:32)=" COMPONENT DIURNAL SIN"
              ENDIF
              CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              IF ( KSHORT ) THEN
                   CBUF(12:20)="DIURNAL C"
                 ELSE
                   CBUF(12:32)=" COMPONENT DIURNAL COS"
              ENDIF
              CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
         ENDIF
!
         IF ( FL_HPESOL ) THEN
!
! ----------- Special trick in orer to overcome a crude mistake in desiging
! ----------- Solve: if a station was modeled with eposidic motion(s),
! ----------- it enters the station list more than once.
!
              FL_STA_USED = .FALSE.
              IF ( ISTA > 1 ) THEN
                   DO KSTA=1,ISTA-1
                      IF ( ISITN_CHR(KSTA) == ISITN_CHR(ISTA) ) FL_STA_USED = .TRUE.
                   END DO
              END IF
!
              DO IHPE = 1,L_HPE ! Running over Harmonics
                 DO JXYZ = 1,3 ! Running over X,Y,Z coordinates!
                    IF ( HPESOL(ISTA,IHPE)%FL_EST  .AND. .NOT. FL_STA_USED ) THEN
!
! ---------------------- Cosine component
!
                         CBUF(10:10) = LCMP(JXYZ)
                         CBUF(11:12) = 'C '
                         CBUF(13:20) = HPESOL(ISTA,IHPE)%NAME
                         CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                         IF ( HPESOL(ISTA,IHPE)%IND_EQU(JXYZ,N__COS) == -1 ) THEN
                              HPESOL(ISTA,IHPE)%IND_EQU(JXYZ,N__COS) = NPARM
                         END IF
!
! ---------------------- Sine component
!
                         CBUF(10:10) = LCMP(JXYZ)
                         CBUF(11:12) = 'S '
                         CBUF(13:20) = HPESOL(ISTA,IHPE)%NAME
                         CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                         IF ( HPESOL(ISTA,IHPE)%IND_EQU(JXYZ,N__SIN) == -1 ) THEN
                              HPESOL(ISTA,IHPE)%IND_EQU(JXYZ,N__SIN) = NPARM
                         END IF
                    END IF
                 ENDDO
              ENDDO
         END IF
!
         IF ( FL_SPESOL ) THEN
              DO ISPE = 1,L_SPE ! Running over stations with spline parameterization
                 DO JXYZ = 1,3 ! Running over X,Y,Z coordinates!
                    IF ( SPESOL(ISPE)%IND_STA == ISTA  .AND. &
     &                   KGLOBAL ) THEN
!
                         DO INOD = 1-SPESOL(ISPE)%DEGREE,SPESOL(ISPE)%L_NOD-1
                            IF ( SPESOL(ISPE)%USED(INOD) ) THEN
                                 CBUF(10:10) = LCMP(JXYZ)
                                 CBUF(11:16) = 'BSPLN '
                                 CALL INCH ( INOD, CBUF(17:20) )
                                 CALL CHASHR     ( CBUF(17:20) )
                                 CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                                 IF ( SPESOL(ISPE)%IND_EQU(INOD,JXYZ) == -1 ) THEN
                                      SPESOL(ISPE)%IND_EQU(INOD,JXYZ) = NPARM
                                 END IF
                            END IF
                         END DO
                       ELSE IF ( SPESOL(ISPE)%IND_STA == ISTA  .AND.  &
     &                           .NOT. KGLOBAL                 .AND.  &
     &                           SPESOL(ISPE)%IND_NOD > 0             ) THEN
                         DO IDEG = -SPESOL(ISPE)%DEGREE,SPESOL(ISPE)%CORR_IND
                            CBUF(10:10) = LCMP(JXYZ)
                            CBUF(11:16) = 'BSPLN '
                            CALL INCH ( SPESOL(ISPE)%IND_NOD+IDEG, CBUF(17:20) )
                            CALL CHASHR                          ( CBUF(17:20) )
                            CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                            IF ( SPESOL(ISPE)%IND_EQU(SPESOL(ISPE)%IND_NOD+IDEG,JXYZ) == -1 ) THEN
                                 SPESOL(ISPE)%IND_EQU(SPESOL(ISPE)%IND_NOD+IDEG,JXYZ) = NPARM
                            END IF
                         END DO
                    END IF
                 ENDDO
              ENDDO
         END IF
!
!
! ------ Check axis offset
!
         IF ( KBIT(LAXOF(1),ISTA) ) THEN
!
! ----------- Check to see if this station was done before.  True for episodic
! ----------- motion. If so, don't estimatad a new axis offset.
!
              DO JSTA=1,ISTA-1
                 IF ( ISITN_CHR(JSTA) .EQ. ISITN_CHR(ISTA) ) GOTO 10
              END DO
              CBUF(10:20)="AXIS OFFSET"
              CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
         END IF
10    CONTINUE
!
      IF ( ISTA .LE. MAX_ARC_STA ) THEN
!
! ===== Check the clocks.
!
        IF ( UNF_CLO ) THEN
!
! -------- Case of uniform clock intervals.
!
! -------- Test: Does the clock of current station are being estimated?
!
           NBITS = 0
           IF ( NUMCLK(ISTA) .GT. 0 ) THEN
              IF ( KBIT(ICLSTA(1,1+ICLSTR(ISTA)), ISTA) ) THEN
!
! ------------- It is cunninng place! How to know will this station participate
! ------------- in estimation of clock? To test bit ICLSTA? True. But not
! ------------- sifficient. To test NUMCLK(ISTA)? Yes. But it is again
! ------------- unsuficient! So ADPAR from cutil thoughtless reinitilize them.
! ------------- The right answer is: you should test ALL bits of arrays ICLSTA,
! ------------- LCLK!! Oh, my Gosh!
!
                DO JCLOCK = 1, NUMCLK(ISTA)
                   K = JCLOCK + ICLSTR(ISTA)
                   IF ( KBIT( ICLSTA(1,K), ISTA) ) THEN
                      DO IORD = 1,ICLMAX
                         IF ( KBIT(LCLK(K),IORD) ) THEN
                              NBITS = NBITS + 1
                         END IF
                      END DO
                   END IF
                END DO
              END IF
           END IF
           IF ( NBITS .GT. 0 ) THEN
!
! ----------- First, global polinom, then clock breaks. We use another letter
! ----------- for clock breaks to simplify logic of MAPO_PARAM (from PROC)
!
              DO 410 J1=0,NUM_BRK(ISTA)
                 IF ( J1 .EQ. 0 ) THEN
                      LET = 'Clk'
                      CALL EPOC ( IM, ID, IY, IHR, IMIN, JDATE_CLO(1) )
                   ELSE
                      LET = 'Brk'
                      CALL EPOC ( IM, ID, IY, IHR, IMIN, JDATE_BRK(J1,ISTA) )
                 END IF
                 DO 420 J2=0,NPL_CLO
!
! ----------------- Form time tag
!
                    IF ( KSHORT ) THEN
                         WRITE ( CBUF(9:32), '(A,I1,5I2.2)'  ) LET(1:1), &
     &                   J2, IY, IM, ID, IHR, IMIN
                      ELSE
                         WRITE ( CBUF(10:32), FMT = &
     &                  '(A,I2,X,I2,"/",I2,"/",I2,I3,":",I2)') LET(1:3), &
     &                   J2, IY, IM, ID, IHR, IMIN
                    ENDIF
!
! ----------------- Add parameter to the list of parameters
!
                    CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
 420             CONTINUE
 410          CONTINUE
!
! ----------- Then, segments
!
              DO 430 J3=2,NUM_CLO
!
! -------------- Form time tag
!
                 CALL EPOC ( IM, ID, IY, IHR, IMIN, JDATE_CLO(J3) )
                 IF ( KSHORT ) THEN
                      WRITE ( CBUF(9:32), '("c",I1,5I2.2)') &
     &                0, IY, IM, ID, IHR, IMIN
                   ELSE
                      WRITE ( CBUF(10:32), FMT = &
     &                '("Clk",I2,X,I2,"/",I2,"/",I2,I3,":",I2)') &
     &                 0, IY, IM, ID, IHR, IMIN
                 ENDIF
!
! -------------- Add parameter to the list of parameters
!
                 CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
 430          CONTINUE
           END IF
          ELSE ! .NOT UNF_CLO
           DO JCLOCK = 1, NUMCLK(ISTA) ! Running over the clocks for this station!
!
! ----------- Construct the Y,M,D,H,M for this clock
!
              K = JCLOCK + ICLSTR(ISTA)
              IF ( KBIT( ICLSTA(1,K), ISTA) ) THEN
!!                 CALL EPOC ( IM, ID, IY, IHR, IMIN, FJDCL(K) )
!
! ---------------- Piece of archaic code left here for compatibilty purposes
!
                   RJD8=.5+AINT(FJDCL(K)-.5+2D-5)
                   CALL MDYJL(IM,ID,IY,ITIME,RJD8 )
                   RJD8=RJD8-2D-5
                   IHR=(FJDCL(K)-RJD8)*24.0D0 + .1D-7
                   IMIN = ((FJDCL(K)-RJD8) - IHR/24.0D0)*1440.0D0 + 0.1D-7
!
! -------------- Check for polynomials
!
                 DO IORD = 1,ICLMAX ! Running over the maximum degree polynomial
                    IF ( KBIT(LCLK(K),IORD) ) THEN
!
! ------------------- Compute the degree of the parameter
!
                      LP = IORD-1
!
! ------------------- Check If Sin or Cos is on and is continued diurnal epoch
!
                      IF ( KSHORT ) THEN
                           WRITE ( CBUF(9:32), '("C",I1,5I2.2)') &
     &                     IORD-1, IY, IM, ID, IHR, IMIN
                        ELSE
                           WRITE ( CBUF(10:32), FMT = &
     &                           '("Clk",I2,X,I2,"/",I2,"/",I2,I3,":",I2)') &
     &                            IORD-1, IY, IM, ID, IHR, IMIN
                      ENDIF
                      IF ( KBIT( LCLK(K), INT2(13)) .AND. KSHORT ) &
     &                     CBUF(9:9)="c"
!
! ------------------- Diurnal sine or cosine
!
                      IF ( KBIT( LCLK(K), INT2(16) ) .AND. &
     &                     KBIT( LCLK(K), INT2(14) )       ) THEN
                         IF ( KSHORT ) THEN
                              WRITE ( CBUF(9:32), '(" C",5i2.2)') &
     &                        IY, IM, ID, IHR, IMIN
                           ELSE
                              WRITE ( CBUF(10:32), &
     &                        '("Cos",3x,i2,"/",i2,"/",i2,i3,":",i2)') &
     &                        IY, IM, ID, IHR, IMIN
                         ENDIF
                        ELSE IF ( KBIT( LCLK(K), INT2(15) ) .AND. &
     &                            KBIT( LCLK(K), INT2(14) )       ) THEN
                         IF ( KSHORT ) THEN
                              WRITE ( CBUF(9:32), '(" s",5I2.2)') &
     &                        IY, IM, ID, IHR, IMIN
                           ELSE
                              WRITE ( CBUF(10:32), &
     &                       '("Sin",3X,I2,"/",I2,"/",I2,I3,":",I2)') &
     &                        IY, IM, ID, IHR, IMIN
                         ENDIF
                      ENDIF
                      CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                   END IF
                 END DO
!
! -------------- Diurnal sine and cosine clocks
!
                 IF ( .not. KBIT( LCLK(K), INT2(14)) ) THEN ! not a continued diurnal!
                    DO M=1,2
                       IF ( KBIT( LCLK(K), INT2(14+M)) ) THEN
                          IF ( M .EQ. 2 ) THEN
                             IF ( KSHORT ) THEN
                                  WRITE(CBUF(9:32), &
     &                            '(" C",5I2.2)')IY, IM, ID, IHR, IMIN
                               ELSE
                                  WRITE(CBUF(10:32), &
     &                           '("Cos",3x,i2,"/",i2,"/",i2,i3,":",i2)') &
     &                            IY, IM, ID, IHR, IMIN
                             ENDIF
                            ELSE
                             IF ( KSHORT ) THEN
                                  WRITE ( CBUF(9:32), '(" S",5I2.2)') &
     &                            IY, IM, ID, IHR, IMIN
                               ELSE
                                  WRITE ( CBUF(10:32), &
     &                            '("Sin",3x,i2,"/",i2,"/",i2,i3,":",i2)') &
     &                            IY, IM, ID, IHR, IMIN
                             ENDIF
                          ENDIF
                          CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, &
     &                                    CBUF )
                       END IF
                    END DO
                 END IF
              ENDIF
           END DO
        END IF
!
! ===== Atmospheres
!
        IF ( UNF_ATM ) THEN
!
! ---------- Case of uniform segments
!
             NBITS = 0
!
! ---------- It is cunninng place! How to know will this station participate
! ---------- in estimation of atmosphere? To test bit IATSTR? True. But not
! ---------- sufficient. To test NUMATM(ISTA)? Yes. But it is again
! ---------- unsuficient! So ADPAR from cutil thoughtless reinitilize them.
! ---------- The right answer is: you should test ALL bits of arrays LATM!!
! ---------- Oh, my Gosh!
!
             IF ( NUMATM(ISTA) .GT. 0 ) THEN
                  DO JATM = IATSTR(ISTA)+1, IATSTR(ISTA)+NUMATM(ISTA)
                     DO IAORD = 0,1
                        IF ( KBIT(LATM(1,IAORD+1),JATM) ) THEN
                             NBITS = NBITS + 1
                        END IF
                     END DO
                  END DO
             END IF
!
             IF ( NBITS .GT. 0 ) THEN
                  DO 440 J4=1,NUM_ATM
!
! ------------------ Form time tag
!
                     CALL EPOC ( IM, ID, IY, IHR, IMIN, JDATE_ATM(J4) )
                     IF ( KSHORT ) THEN
                          WRITE ( CBUF(9:32), '("a",I1,5I2.2)') &
     &                            0, IY, IM, ID, IHR, IMIN
                       ELSE
                          WRITE ( CBUF(10:32), FMT = &
     &                           '("Atm",I2,X,I2,"/",I2,"/",I2,I3,":",I2)') &
     &                            0, IY, IM, ID, IHR, IMIN
                     ENDIF
                     IF ( KSHORT .AND. J4 .EQ. 1 ) CBUF(9:9) = 'A'
!
! ------------------ Add parameter to the list of parameters
!
                     CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
 440              CONTINUE
             END IF
           ELSE
!
! ---------- Non-uniform segments
!
             DO JATM = IATSTR(ISTA)+1, IATSTR(ISTA)+NUMATM(ISTA)
                DO IAORD = 0,1
                   IF ( KBIT(LATM(1,IAORD+1),JATM) ) THEN
                        CALL EPOC ( IM, ID, IY, IHR, IMIN, TATM(JATM) )
                        IF ( KSHORT ) THEN
                             WRITE ( CBUF(9:32), '("A",i1,5i2)') &
     &                       IAORD, IY, IM, ID, IHR, IMIN
                          ELSE
                             WRITE ( CBUF(10:32), &
     &                       '("Atm ",i1,i2,"/",i2,"/",i2,i3,":",i2)') &
     &                       IAORD, IY, IM, ID, IHR, IMIN
                       ENDIF
                       IF ( KBIT(LATM(1,3),JATM) .AND. KSHORT ) CBUF(9:9)="a"
                       CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                  END IF
                END DO
            END DO
        END IF
!
! ===== Gradients
!
        CBUF(9:9)=" "
        DO JATM = 1, NUMGRAD(ISTA)
           IF ( KBIT(LGRAD(1), JATM) ) THEN
                CALL EPOC ( IM, ID, IY, IHR, IMIN, TGRAD(JATM) )
                DO IGRAD=1,2
                   IF ( KSHORT ) THEN
                      WRITE ( cbuf(9:32), '(A2,5I2.2)' ) LGRAD_SHORT(IGRAD), &
     &                IY, IM, ID, IHR, IMIN
                     ELSE
                      WRITE ( CBUF(9:32), &
     &                '(A10,2X,I2,"/",I2,"/",I2,I3,":",I2)') &
     &                LGRAD_LONG(IGRAD), IY, IM, ID, IHR, IMIN
                   ENDIF
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                END DO
           END IF
        END DO
       END IF
 810  CONTINUE
      END DO ! This end the site loop.
!
! === Check star coordinates
!
      INEXTS=1
      DO JSTR = 1,NUMSTR
         CBUF=ISTRN_CHR(JSTR)
         IF ( KBIT(LSTAR(1,1), JSTR) ) THEN ! Right Ascension turned on
              CBUF(10:32)="RIGHT ASCENSION"
              CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
         END IF
         IF ( KBIT(LSTAR(1,2),JSTR) ) THEN  ! Declination turned on
              CBUF(10:32)="DECLINATION"
              CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
         END IF
      END DO
!
! === Check star proper motions
!
      INEXTS=1
      DO JSTR = 1,NUMSTR
         CBUF=ISTRN_CHR(JSTR)
         IF ( KBIT(LPROP(1,1), JSTR) ) THEN ! Right Ascension turned on
             CBUF(10:32) = "RIGHT ASC VELO "
             CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
         END IF
         IF ( KBIT(LPROP(1,2),JSTR) ) THEN ! Declination turned on
              CBUF(10:32) = "DEC VELO"
              CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
         END IF
      END DO
!
      IF ( FL_EHEO ) THEN
           DO IHEO = 1, L_EHEO ! Running over HEO constituents
              IF ( HEOSOL(IHEO)%FL_EST(HEO__E1E2) ) THEN
                   CBUF = 'HEO C  EP '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                   CBUF = 'HEO S  EP '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              END IF
              IF ( HEOSOL(IHEO)%FL_EST(HEO__E3) ) THEN
                   CBUF = 'HEO C  E3 '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                   CBUF = 'HEO S  E3 '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              END IF
!
              IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E1E2) ) THEN
                   CBUF = 'HEO CV EP '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                   CBUF = 'HEO SV EP '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              END IF
              IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E3) ) THEN
                   CBUF = 'HEO CV E3 '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                   CBUF = 'HEO SV E3 '//HEOSOL(IHEO)%NAME(1:10)
                   CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              END IF
           END DO
      END IF
!
      IF ( FL_EERM ) THEN
           IF ( KGLOBAL ) THEN
!
! ------------- In the case of the first session we count all ERM parameters
! ------------- over entier run -- similar to global mode
!
                DO ICMP=1,3
!
! ---------------- CGM-style -- all ERM parameters
!
                   DO INOD = 1-EERM%DEGREE(ICMP),EERM%NKNOTS(ICMP)-1
                      CALL CLRCH ( CBUF )
                      WRITE ( CBUF, '("ERM ",I1,4X,I5,5X)' ) ICMP, INOD
                      CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                   END DO
                END DO
              ELSE
                DO ICMP=1,3
                   IF ( IND_EERM_NOD(ICMP) > 0 ) THEN
!
! --------------------- Non CGM-style -- Only estimatable ERM parameters
! --------------------- are counted
!
                        DO INOD = IND_EERM_NOD(ICMP)-EERM%DEGREE(ICMP), &
     &                            IND_EERM_NOD(ICMP)+EERM_OVR(ICMP)
                           CALL CLRCH ( CBUF )
                           WRITE ( CBUF, '("ERM ",I1,4X,I5,5X)' ) ICMP, INOD
                           CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, &
     &                                     CBUF )
                        END DO
                    END IF
                END DO
           END IF
      END IF
!
! === UT1/PM
!
! --- Begin eop section
!
      DO IEOP = 1,2 !pm or ut1
!
! ------ Old polynomial parameterizaton
!
         IF ( EOP_STYLE(IEOP) .EQ. EOP__POLY ) THEN ! polynomials
            DO IXYU = IXYU_START(IEOP), IXYU_STOP(IEOP) !start-stop
               DO IEPOCH = 1,NROT
                   DO IORD = 1,4 !running 1st to 4th order
                      IF ( IROTT(IEPOCH,IXYU,IORD,LROT) .EQ.1 ) THEN ! flag is on
                         CALL EPOC ( IM, ID, IY, IHR, IMIN, TROT(1) )
                         WRITE ( CBUF, &
     &                   '(A8,I2,5I2.2)')LEOP(IXYU), (IORD-1), IY, IM, ID, IHR, IMIN
                         CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, &
     &                                   CBUF )
                      ENDIF !flag is on
                  ENDDO ! running 1st to 4th order
              ENDDO ! running the rotation epochs
            ENDDO ! start-stop
         ENDIF ! polynomials
!
! ------ The two new segmented parameterization styles
!
         IF ( EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS .OR. &
     &        EOP_STYLE(IEOP) .EQ. EOP__SEGS_ONLY          ) THEN ! new segmented styles
!
              DO IXYU = IXYU_START(IEOP), IXYU_STOP(IEOP) ! start-stop
                 IF ( EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS ) THEN !There's a global rate
                    CALL EPOC ( IM, ID, IY, IHR, IMIN, TROT_A1 )
                    WRITE ( CBUF, '(A3,"GRate 1",5I2.2)') &
     &              XYU_LABEL(IXYU), IY, IM, ID, IHR, IMIN
                    CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                 ENDIF ! There's a global rate
!
! -------------- Handle the segments.
!
                 IF ( UNF_EOP .AND.  NROT_A1(IEOP) .GT. 0 ) THEN
!
! ----------------- Case of uniform segments
!
                    DO IEPOCH = 1, NUM_EOP ! running over the segment epochs
                       CALL EPOC ( IM, ID, IY, IHR, IMIN, JDATE_EOP(IEPOCH) )
                       WRITE ( CBUF, '(A8," 0",5I2.2)') &
     &                 LEOP(IXYU), IY, IM, ID, IHR, IMIN
                       CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                    ENDDO ! running over the segment epochs
                  ELSE
                    DO IEPOCH = 1, NROT_A1(IEOP) !running over the segment epochs
                       CALL EPOC( IM, ID, IY, IHR, IMIN, (TROT_A1+ &
     &                      ROT_INTERVAL(IEOP)*(IEPOCH-1)) )
                       WRITE(CBUF,'(A8," 0",5I2.2)') LEOP(IXYU),IY,IM,ID,IHR, &
     &                 IMIN
                       CALL PARM_ADD(LNAME,ISTR_LEN,NPARM,MAX_PARM,CBUF )
                    ENDDO !running over the segment epochs
                 END IF
             ENDDO ! start-stop
         ENDIF ! new segmented styles
!
! ------ Handle the new configuration of the sine wave style.
! ------ With this style now all eighteen parameters are always turned on.
!
         IF ( EOP_STYLE(IEOP) .EQ. EOP__SINE ) THEN ! It's the sine wave configuration
              CALL EPOC ( IM, ID, IY, IHR, IMIN, TROT_A1 )
              DO IXYU = IXYU_START(IEOP), IXYU_STOP(IEOP) ! start-stop
                 WRITE ( CBUF6(1),'(A8," 0",5I2.2    )') &
     &                   LEOP(IXYU), IY, IM, ID, IHR, IMIN
                 WRITE ( CBUF6(2),'(A8," 1",5I2.2    )') &
     &                   LEOP(IXYU), IY, IM, ID, IHR, IMIN
                 WRITE ( CBUF6(2), '(A2,"DiurnalSin  ")' ) XYU_LABEL(IXYU)
                 WRITE ( CBUF6(2), '(A2,"DiurnalCos  ")' ) XYU_LABEL(IXYU)
                 WRITE ( CBUF6(2), '(A2,"Semi-diSin  ")' ) XYU_LABEL(IXYU)
                 WRITE ( CBUF6(2), '(A2,"Semi-diCos  ")' ) XYU_LABEL(IXYU)
                 DO I = 1,6
                    CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF6(I) )
                 ENDDO
              ENDDO !start-stop
         ENDIF !It's the sine wave configuration
!
      ENDDO !x&y pole or ut1
!
! --- end EOP SECTION
!
! --- Relativity
!
      IF ( LREL.NE.0 ) THEN
           CBUF = "Gamma"
           CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
      ENDIF
!
! --- Earth tides
!
! --- Globals first
!
      IF ( ITDGLB .EQ. 0 ) then !Global values being estimated!
          DO J=1,3
             IF ( LTIDE(1,J) .NE. 0 ) THEN
                  CBUF = LTIDE_NAME(J)
                  CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
             END IF
          END DO
        ELSE
          DO ISTA=1,NUMSTA
!
! ---------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
             IF ( .NOT. CHECK_STABIT ( ISTA ) ) GOTO 820
             DO JTYPE = 1,3
                IF ( KBITN(LTIDE(1,JTYPE), ISTA) .NE. 0 ) THEN
                     CBUF = ISITN_CHR(ISTA)//" "//ltide_name(j)
                     CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                END IF
             ENDDO
 820         CONTINUE
          END DO
      END IF
!
! --- Precession
!
      IF ( LPREC.NE.0 ) THEN
           CBUF = "Precession Constant"
           CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
      END IF
!
! --- Nutation - offsets
!
      DO JNUT = 1,2  ! Running of Dpsi and Deps
         IF ( KBIT(LNUT,JNUT) ) THEN
              CBUF = LNUTATION(JNUT)
              CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
         END IF
      END DO
!
! --- Nutation - Time Series
!
      IF ( LNUT(2).NE.0  .OR. LNUT(3).NE.0 ) THEN
           DO ITYP = 1,2 ! Run over longitude and obliquity
              IF ( KSHORT ) THEN
                   CBUF = LNUT_SHORT(ITYP)
                   ISTART=5
                ELSE
                   CBUF = LNUT_LONG(ITYP)
                   ISTART = 12
              ENDIF
              DO ITERM = 1,6        !Run over the six possible terms
                 IF ( KBIT( LNUT(ITYP+1), ITERM) ) THEN
                      DO IIN_OUT=1,2
                         CBUF(ISTART:32) = LNUT_TERM(ITERM)//LIN_OUT(IIN_OUT)
                         CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, &
     &                                   CBUF )
                      END DO
                 END IF
              END DO ! Run over the six possible terms
            END DO   ! Run over longitude and obliquity
      END IF
!
! --- Additional nutation parameters
!
! --- Longitude/Obliquity
!
      DO ITYP=1,2
         IF ( ( ITYP .EQ. 1 .AND. NFLPSI .GT. 0 ) .OR. &
     &        ( ITYP .EQ. 2 .AND. NFLEPS .GT. 0 )      ) THEN
            IF ( KSHORT ) THEN
                 CBUF = LNUT_SHORT(ITYP)
                 ISTART=5
              ELSE
                 CBUF=LNUT_LONG(ITYP)
                 ISTART=12
            ENDIF
            DO I = 1,210
               IF ( KBIT(FLPSI,I) ) THEN
                  IF ( I .LE. 2 ) THEN
                       CBUF(ISTART:32)=LNUT_TYPE(I)
                     ELSE IF(I .LE. 4) THEN
                       WRITE ( CBUF(ISTART:32), '("FCN",F7.2,X,A3,X)') &
     &                 FCNPER, LSINCOS(5-I)
                     ELSE
                       WRITE ( CBUF(ISTART:32), '("CALC",I4,2x,a9)') &
     &                         ((I-4)+1)/2, LIN_OUT(MOD(I,INT2(2))+1)
                  ENDIF
                  CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
               END IF
            ENDDO
         ENDIF
      ENDDO
!
! --- Baseline dependent clocks
!
      IF ( LOGBCL ) THEN
         DO I=1,NUMSTA
!
! --------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
            IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 830
            IP=I+1
            DO J=IP,NUMSTA
!
! ------------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
               IF ( .NOT. CHECK_STABIT ( J ) ) GOTO 840
!
               IF ( KBIT(ICLOCK(1,I), J ) .OR. KBIT(ICLOCK(1,J),I) ) THEN
                    CBUF = ISITN_CHR(I)//"--"//ISITN_CHR(J)//" Clocks"
                    CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
               END IF
 840           CONTINUE
            END DO
 830        CONTINUE
         END DO
      END IF
!
      IF ( .NOT. KGLOBAL ) THEN
           IF ( IOS_EST == IOS__SES ) THEN
                CBUF = 'IOS_SES '//DBNAME_CH
                CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              ELSE IF ( IOS_EST == IOS__STA ) THEN
                DO 450 J5=1,NUMSTA
                   IF ( CHECK_STABIT ( INT2(J5) ) ) THEN
                        CBUF = 'IOS_STA '//ISITN_CHR(J5)
                        CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                   END IF
 450            CONTINUE 
              ELSE IF ( IOS_EST == IOS__BAS ) THEN
                DO 460 J6=1,NUMSTA
!
! ---------------- Check STABIT bit fields to bypass deselcted station
!
                   IF ( CHECK_STABIT ( INT2(J6) ) ) THEN
                        IP = J6 + 1
                        DO 470 J7=IP,NUMSTA
!
! ------------------------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
                           IF ( CHECK_STABIT ( INT2(J7) ) ) THEN
                                IF ( KBIT(ICLOCK(1,J6),J7) .OR. KBIT(ICLOCK(1,J7),J6) ) THEN
                                     CBUF = 'IOB_'//ISITN_CHR(J6)//ISITN_CHR(J7)
                                     CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                                END IF
                           END IF
 470                    CONTINUE
                   END IF
 460            CONTINUE
           END IF
      END IF
!
! --- High-frequency earth orientation parameters (tidal components)
!
      IF ( KHFEOP.EQ.2 .OR. KHFEOP.EQ.3 ) THEN
           DO I=1,NUM_SDE_UT1
              DO J=1,2
                 WRITE ( CBUF, '(A2,6I3)' ) LHFEOP(J), (SDE_ARG(K,I),K=1,6)
                 CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              ENDDO
           ENDDO
!
           DO I=NUM_SDE_UT1+1,NUM_SDE_UT1+NUM_SDE_XY
              DO J=3,4
                 WRITE(CBUF,'(A2,6I3)') LHFEOP(J),(SDE_ARG(K,I),K=1,6)
                 CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
              ENDDO
           ENDDO
      ENDIF
!
      IF ( SOU_ADM_FLAG .NE.  SOUADM__NO ) THEN
!
! ------ Count the number of parameters for source structure admittance
!
         DO J=1,NUMSTR !running over the sources
!
! --------- Check, whether the J-th source was selected
!
!@          IF ( SUPMET == SUPMET__META ) THEN
!@               FL_SOU_USE = BTEST ( SOU_USE(J), INT4(IDATYP) )
!@             ELSE 
                 FL_SOU_USE = KBIT (  ISRSEL(1), J ) 
!@          END IF
            IF ( FL_SOU_USE ) THEN
!
! -------------- Yes, it was selected
!
                 IF ( SOU_ADM_FLAG == SOUADM__GLB_ALL ) THEN
!
! ------------------- Admittance is computed for all sources. 
!
                      CBUF = 'GLB_SOU_ADM ALL'
                      CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                      GOTO 860
                    ELSE IF ( SOU_ADM_FLAG == SOUADM__LCL_ALL .AND. &
     &                       .NOT. KGLOBAL                          ) THEN
                      CBUF = 'LCL_SOU_ADM ALL'
                      CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                      GOTO 860
                    ELSE 
!
! ------------------- Admittance is computed for a list of sources
!
                      IF ( IND_SOU_ADM(1) > 0 ) THEN
!
! ------------------------ Search the J-th sources in the list
!
                           IP = LTM_DIF ( 1, IND_SOU_ADM(2)-IND_SOU_ADM(1)+1, &
     &                                    SRCSUP(IND_SOU_ADM(1)), &
     &                                    ISTRN_CHR(J) )
                           IF ( IP > 0 ) THEN
!
! ----------------------------- The J-th source was found in the list
!
                                IF ( SOU_ADM_FLAG == SOUADM__GLB_LIST_NO ) THEN
                                     CBUF = 'GLB_SOU_ADM '//ISTRN_CHR(J)
                                     CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                                   ELSE IF ( SOU_ADM_FLAG == SOUADM__LCL_LIST_NO .AND. &
     &                                       .NOT. KGLOBAL ) THEN
                                     CBUF = 'LCL_SOU_ADM '//ISTRN_CHR(J)
                                     CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                                END IF
                              ELSE 
                                IF ( SOU_ADM_FLAG == SOUADM__GLB_LIST_YES ) THEN
                                     CBUF = 'GLB_SOU_ADM '//ISTRN_CHR(J)
                                     CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                                   ELSE IF ( SOU_ADM_FLAG == SOUADM__LCL_LIST_YES .AND. &
     &                                       .NOT. KGLOBAL )  THEN
                                     CBUF = 'LCL_SOU_ADM '//ISTRN_CHR(J)
                                     CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                                END IF
                           END IF
                         ELSE IF ( IND_SOU_ADM(1) == 0                 .AND. &
     &                             SOU_ADM_FLAG == SOUADM__GLB_LIST_NO     ) THEN
                           CBUF = 'GLB_SOU_ADM '//ISTRN_CHR(J)
                           CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                         ELSE IF ( IND_SOU_ADM(1) == 0                 .AND. &
     &                             SOU_ADM_FLAG == SOUADM__LCL_LIST_NO .AND. &
     &                             .NOT. KGLOBAL                           ) THEN
                           CBUF = 'LCL_SOU_ADM '//ISTRN_CHR(J)
                           CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
                        END IF ! Exception list
                   END IF ! Admittance flag
              END IF ! fl_sou_use
         ENDDO ! sources
 860     CONTINUE 
      END IF
!
! --- Handle user-defined parameters (from USER_PARTIAL feature)
!
      IF ( KUSER_PART ) THEN
           IF ( KGLOBAL ) THEN
                FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
             ELSE
                FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRP'//PRE_LETRS
           ENDIF
!
! -------- Test of existence of file with user partials
!
           INQUIRE ( FILE=FNAME, EXIST=KEXIST )
           IF ( KEXIST ) THEN
                OPEN ( 66, FILE=FNAME, STATUS='OLD', IOSTAT=IOS )
                CALL FERR ( INT2(IOS), "GET_NAMES: Opening "//FNAME, &
     &                      INT2(0), INT2(0) )
!
! ------------- Reading the number of user partials
!
                READ ( 66, *, IOSTAT = IOS ) NUM_USER_PART
                IF ( NUM_USER_PART .LT. 0 ) IOS = 1
                CALL FERR ( INT2(IOS), "GET_NAMES: Reading "//FNAME, &
     &                      INT2(0), INT2(0) )
!
                DO I=1,NUM_USER_PART
                   READ ( 66, '(A)', IOSTAT = IOS ) CDUM
                   CALL FERR ( INT2(IOS), "GET_NAMES: Reading "//FNAME, &
     &                         INT2(0), INT2(0) )
                   CBUF=CDUM(1:20)
!
! ---------------- Are reading in only global parameters form USRG, or
! ---------------- or are reading in only the global parameters from USRP
!
                   IF ( KGLOBAL .OR. (.NOT. KGLOBAL .AND. KGLOBONLY ) ) THEN
                        IF ( CDUM(22:22) .EQ. 'G' ) THEN
                             CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, &
     &                                       MAX_PARM, CBUF )
                           ELSE
                             IF ( KGLOBAL ) THEN
!
! ------------------------------- If reading in USRG, they should all be global
! ------------------------------- parameters.
!
                                  CALL FERR ( INT2(7734), &
     &                                'GET_NAMES: Trap of '// &
     &                                'internal control: not global '// &
     &                                'paramater: "'//CDUM(1:22)//'" was '// &
     &                                'found in the file of global '// &
     &                                'user parameters '//FNAME, INT2(0), INT2(0) )
                                  STOP 'Abnormal termination'
                             END IF
                         ENDIF
                      ELSE
                         CALL PARM_ADD ( LNAME, ISTR_LEN, NPARM, &
     &                                   MAX_PARM, CBUF )
                    ENDIF
                ENDDO
!
                CLOSE ( 66, IOSTAT = IOS )
                CALL FERR ( INT2(IOS), "Closing USRG"//PRE_LETRS, &
     &                      INT2(0), INT2(0) )
             ELSE
                NUM_USER_PART=0
           ENDIF
      ENDIF
!
 999  CONTINUE
      RETURN
      END  !#!  GET_NAMES  #!#
