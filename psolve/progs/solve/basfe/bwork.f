      SUBROUTINE BWORK ( LSCRD, XOFFST, MONAM, MOTYPE, ARR, LBUF, IPTR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  BWORK PROGRAM SPECIFICATION
!
! 1.1 Contains most of the code formerly in the BASFE main program.
!     If GLOBL mode and stations are not carried, then use only
!     the stations which are in the current arc.  These are at the
!     top of the PARFIL list.
!
! 1.2 REFERENCES:
!
! 2.  BWORK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 LSCRD(3,MAX_STA)
      INTEGER*2 MONAM(5,MAX_STA),MOTYPE(MAX_STA)
      REAL*8 XOFFST(3,MAX_STA),ARR(*)
!
! LSCRD - Array of site coordinate parameters for each component of
!          each station
! MONAM - Array of monument names for each station
! MOTYPE - Array of monument types for each station
! XOFFST - Array of axis offsets for each station
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'buff4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: basfe
!       CALLED SUBROUTINES: gtlhv,spart,cvmml
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 KP, IM, ID, IYR, ITIME, IA(3), IB(3)
      INTEGER*2 M, J, I, KK, NSITES, JPX, IPX
      INTEGER*2 ITYPE, IOS, XR(MAX_STA)
      CHARACTER FNAME*(NAME_SIZE)
      LOGICAL*2 KBIT, EQUAL
      REAL*8    SPAR(6,11), BVMAG, APX, APY, APZ
      REAL*8    XOFFSG(3,MAX_STA)
      REAL*8    ZSITE(3,2), BC(11), BSIG(11), CMT(3,3), DATAN2Z
      REAL*8    TMPMAT2(12), TMPMAT1(6)
      REAL*8    EARTH_RAD
      CHARACTER VERSION_STR*54, STR*128, STR1*128, GET_VERSION*54
      INTEGER*2  INT2_NE
      PARAMETER  ( INT2_NE = 2HNE )
      INTEGER*4 I4M1
      INTEGER*4 I4P4, I4P0, I4P1
      DATA  I4P4, I4P0, I4P1 / 4, 0, 1 /
      INTEGER*4 IPTR, JA
      CHARACTER LBUF(*)*(*)
      INTEGER*2 INX, IXX(12), IAV(3), IBV(3)
      EQUIVALENCE (IXX,IAV)
      EQUIVALENCE (IXX(4),IBV)
      EQUIVALENCE (IXX(7),IA)
      EQUIVALENCE (IXX(10),IB)
      EQUIVALENCE (TMPMAT2(7),TMPMAT1(1))
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-9 )
      INTEGER*4   PAGEWID, PAGELEN, MIN_PAGEWID, IOSTAT
      LOGICAL*4   DATYP_INQ
      INTEGER*4   I_LEN
!
      DATA I4M1 / -1 /
      DATA MIN_PAGEWID / 80 /
!
      DATA CMT/9*0.0D0/
      DATA EARTH_RAD / 6.378137D6 /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  870603  Took out eccentricity code which estimated the
!                positions of the VLBI reference points (since SDBH
!                now estimates the positions of the monuments).
!                Revised so that BWORK only prints out values and
!                scaled sigmas for left-hand spherical length
!                and monument to monument vector magnitude,
!                horizontal and vertical values.
!   EJH  870613  Some variables changed to double integer for
!                implementation of 384 parameters.
!   AEE  911219  Changed 'WC' to 'OWC' in use_buffer call and removed
!                calls to use_buffer that was just for closing, since
!                it is now done in reada.f file.
!   JLR   921215 replaced -1J with I4M1
!   pet   980415 forced bwork to bypass deselected baselines and not to print
!                baseline statistics for the baselines which were not in
!                solution
!   pet   980612 Disabled beeping
!   pet   980921 Removed a hidden option ' ' which irritated users
!   pet   980921 Fixed a bug: MDLPL after NOV-97 required USER_BUFFER what
!                was missed.
!   pet   990404 Rewrote comments. Changed the list of arguments.
!   pet   990419 Changed logic. Baseline information in global mode printed
!                ONLY if position of both statin have been adjusted. The same
!                for velocities.
!   pet   2000.03.22  Added support of the case when FC_GWINW  returns 0
!                     (X-server doesn't provide correct information about
!                      the current window)
!   pet   2000.08.08  Fixed a bug: program was terminated abnormally when the
!                     physical wifth of the screen exceeded dimension of LBUF
!
!   pet  2001.01.11   Changed the way how scaled factor or m-sigmas is computed.
!                     Renamed "Scaled sigma" --> "m-sgima" and renamed
!                     "Sigma" --> "a-sigma"
!   pet  2002.05.31   Made some cosmetic changes in the header
!   pet  2003.12.09   Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!   pet  2020.12.30   Fixed a bug: deleection status was checked for unsorted baselines
!
! 5.  BWORK PROGRAM STRUCTURE
!
!CCCCC
      JA = 3*M_GPA
      IF ( NSCNT1 .EQ. 0 ) THEN
           RETURN
      END IF
      CALL GET_TERMSIZE ( PAGELEN, PAGEWID )
      PAGEWID = PAGEWID - 1
      IF ( PAGEWID .LT. 1 ) THEN
           CALL GETENVAR ( 'COLUMNS', STR )
           READ ( UNIT=STR, IOSTAT=IOSTAT, FMT='(I4)' ) PAGEWID
           IF ( PAGEWID .LE. 0 ) PAGEWID = MIN_PAGEWID
      END IF
      IF ( PAGEWID .GT. LEN(LBUF(1)) ) THEN
           PAGEWID = LEN(LBUF(1))
      END IF
!
! --- Initialize IPTR and set up top of LBUF
!
      IPTR=1
      INX=0
!
! --- Read saved CRES output from file to be used to scroll
! --- back display
!
      IF ( KSCREEN  .AND.  .NOT. KBATCH ) THEN
           FNAME = PRE_SCR_DIR(:PRE_SD_LEN)//'CRBF'//PRE_LETRS
           OPEN ( 34, FILE=FNAME, IOSTAT=IOSTAT )
           IOS = IOSTAT
           CALL FERR ( IOS, "BASFE(bwork): Opening CRES display buffer", INT2(0), &
     &          INT2(0) )
           DO WHILE ( .TRUE. )
              READ ( 34, '(A)', END=50, IOSTAT=IOSTAT ) LBUF(IPTR)
              IOS = IOSTAT
              CALL FERR ( IOS, "BASFE(BWORK) Reading CRES display buffer", &
     &             INT2(0), INT2(0) )
              IPTR=IPTR+1
           ENDDO
50         CONTINUE
           CLOSE ( 34, IOSTAT=IOSTAT )
           IOS = IOSTAT
           CALL FERR ( IOS, "BASFE(BWORK) Closing CRES display file", INT2(0), &
     &          INT2(0) )
      ENDIF
!
      NSITES = NUMSTA
      DO 410 KK=1,NSITES
!
! ------ Calculate the monument offsets in geocentric coordinates
!
         ITYPE = MOTYPE(KK)
         IF ( ITYPE .EQ. INT2_NE ) THEN
              CALL KROT ( APX, APY, APZ, XOFFST(3,KK), XOFFST(2,KK), &
     &                    XOFFST(1,KK), XOFFSG(1,KK), XOFFSG(2,KK), &
     &                    XOFFSG(3,KK) )
              GOTO 410
         END IF
!
         DO 420 I=1,3
            XOFFSG(I,KK) = XOFFST(I,KK)
 420     CONTINUE
 410  CONTINUE
!
! --- Print headings
!
      IF ( KSPOOL .AND. KFULLOUT ) THEN
           IF ( NSCNT1 .GT. 0 ) THEN
!
! ------------- Baseline coordinate
!
                WRITE ( STR1, 2020 ) IRNCD
                CALL BLANK_TO_ZERO ( STR1(36:44) )
                WRITE ( 23, '(A)' )  STR1(1:44)
              ELSE
!
! ------------- Baseline velocity
!
                WRITE ( STR1, 2120 ) IRNCD
                CALL BLANK_TO_ZERO ( STR1(46:54) )
                WRITE ( 23, '(A)' )  STR1(1:54)
           ENDIF
!
           CALL MDYJL ( IM, ID, IYR, ITIME, PREPOCH )
           IF ( IYR .GT. 69 ) THEN
                IYR = IYR+1900
              ELSE
                IYR = IYR+2000
           END IF
           WRITE ( UNIT=STR(1:10), FMT='(I4,".",I2,".",I2)' ) IYR, IM, ID
           CALL BLANK_TO_ZERO ( STR(1:10) )
!
           WRITE ( 23, 2021 ) STR(1:10)
           WRITE ( 23, 2012 )
           WRITE ( 23, 2024 )
           WRITE ( 23, 2025 )
         ELSE IF ( KSPOOL  .AND.  NSCNT1 .LT. 0 ) THEN
           WRITE ( STR1, 2120 ) IRNCD
           CALL BLANK_TO_ZERO ( STR1(46:54) )
           WRITE ( 23, '(A)' )  STR1(1:54)
         ELSE IF ( KSPOOL  .AND.  NSCNT1 .GT. 0 ) THEN
           WRITE ( STR1, 2130 ) IRNCD
           CALL BLANK_TO_ZERO ( STR1(36:45) )
           WRITE ( 23, '(A)' )  STR1(1:45)
      END IF
!
      IF ( KSCREEN  .AND.  KFULLOUT  .AND.  KBIT( PRE_IP(2), INT2(6)) ) THEN
!
! -------- Print a long title on the screen
!
           CALL NL_MN()
           CALL NL_MN()
           IPTR=IPTR+1
           VERSION_STR = GET_VERSION ( )
           WRITE ( LBUF(IPTR), "(32X,A)") &
     &             VERSION_STR(1:I_LEN(VERSION_STR))
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
!
           CALL NL_MN()
           CALL NL_MN()
           CALL CLRCH ( LBUF(IPTR+1) )
           IPTR=IPTR+2
           WRITE ( LBUF(IPTR), 2010 ) IRNCD
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
!
           CALL NL_MN()
           CALL NL_MN()
           CALL NL_MN()
           CALL CLRCH ( LBUF(IPTR+1) )
           CALL CLRCH ( LBUF(IPTR+2) )
           IPTR=IPTR+3
           WRITE ( LBUF(IPTR), 2011 )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
!
           CALL NL_MN()
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), 2012 )
           CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
!
           CALL NL_MN()
           CALL NL_MN()
           CALL NL_MN()
           CALL CLRCH ( LBUF(IPTR+1) )
           CALL CLRCH ( LBUF(IPTR+2) )
           IPTR=IPTR+3
           WRITE ( LBUF(IPTR), 2014 )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
!
           CALL NL_MN()
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), 2015 )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
      END IF
!
 2010 FORMAT(6X,"Baseline information for run ",I5,"-",I4 )
 2020 FORMAT("1     Baseline information for run ",I5,"-",I4 )
 2120 FORMAT("1     Baseline velocity information for run ",I5,"-",I4)
 2130 FORMAT("1     Baseline information for run ",I5,"-",I4, &
     &           " (mm)     Length  a-sigma      Length  a-sigma  ", &
     &           "Trans   a-sigma    Vert   a-sigma" )
 2011 FORMAT(6X,"Monument to monument values")
 2021 FORMAT(6X,"Monument to monument values at epoch  ",A )
 2012 FORMAT(6X,'Baseline vector components: Length, Vertical and ', &
     &          'Transverse components' )
 2014 FORMAT(23X," Vector Mag(mm)  Length(mm) ", &
     &           ' Horizontal(mm) Vertical(mm) ')
 2024 FORMAT(47X,"Vector mag  a-sigma   Length   a-sigma Horizontal ", &
     &           "a-sigma Vertical a-sigma" )
 2015 FORMAT ( 29X, "& a-sigma      & a-sigma   & a-sigma   & a-sigma" )
 2025 FORMAT ( 52X, "(mm)", 4X, " (mm)", 8X, "(mm)", 4X, " (mm)", &
     &          3X, "(mm)", 5X, " (mm)", 4X, "(mm)", 5X, " (mm)"  )
!
! --- Process the baselines.
!
      CALL XRSORT ( ISITN, NSITES, XR )
      IPX = NSITES - 1
!
      DO 900 I=1,IPX
         JPX = I+1
         DO 800 J=JPX,NSITES
            IF ( KGLOBALS ) THEN
!
! -------------- We print baseline information in global mode ONLY if poisition
! -------------- of both stations have been estimated
!
                 IF ( NSCNT1 .GT. 0 .AND. &
     &                ( .NOT. KBIT ( LSITEC(1,1),XR(I)) .OR. &
     &                  .NOT. KBIT ( LSITEC(1,2),XR(I)) .OR. &
     &                  .NOT. KBIT ( LSITEC(1,2),XR(I)) .OR. &
     &                  .NOT. KBIT ( LSITEC(1,1),XR(J)) .OR. &
     &                  .NOT. KBIT ( LSITEC(1,2),XR(J)) .OR. &
     &                  .NOT. KBIT ( LSITEC(1,3),XR(J))      ) ) GOTO 800
!
! -------------- The same for velocities
!
                 IF ( NSCNT1 .LT. 0 .AND. &
     &                ( .NOT. KBIT ( LSITEV(1,1),XR(I)) .OR. &
     &                  .NOT. KBIT ( LSITEV(1,2),XR(I)) .OR. &
     &                  .NOT. KBIT ( LSITEV(1,2),XR(I)) .OR. &
     &                  .NOT. KBIT ( LSITEV(1,1),XR(J)) .OR. &
     &                  .NOT. KBIT ( LSITEV(1,2),XR(J)) .OR. &
     &                  .NOT. KBIT ( LSITEV(1,3),XR(J))      ) ) GOTO 800
              ELSE IF ( .NOT. KGLOBALS ) THEN
!
! -------------- In the case that statistics is gathered not for the global
! -------------- solution, we check, whether this baseline was deselected or not. 
! -------------- If yes, we bypass this baseline
!
                 IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------------- Case of phase delay solution
!
                      IF ( .NOT. KBIT (IBLSEL_P(1,XR(J)),XR(I)) .AND. &
     &                     .NOT. KBIT (IBLSEL_P(1,XR(I)),XR(J))       ) THEN
                           GOTO 800
                      END IF
                    ELSE
!
! ------------------- Case of group delay solution
!
                      IF ( .NOT. KBIT (IBLSEL_G(1,XR(J)),XR(I)) .AND. &
     &                     .NOT. KBIT (IBLSEL_G(1,XR(I)),XR(J))       ) THEN
                           GOTO 800
                      END IF
                 END IF
            END IF  ! KGLOBALS
!
            BC(1) = VSITE1(1,XR(J)) - VSITE1(1,XR(I))
            BC(2) = VSITE1(2,XR(J)) - VSITE1(2,XR(I))
            BC(3) = VSITE1(3,XR(J)) - VSITE1(3,XR(I))
!
! --------- Compute baseline length
!
            BC(4) = DSQRT(BC(1)*BC(1) + BC(2)*BC(2) + BC(3)*BC(3))
!
! --------- Compute baseline cylindrical radius
!
            BC(7) = DSQRT(BC(1)*BC(1) + BC(2)*BC(2))
!
! --------- Compute baseline declination
!
            BC(6) = DATAN2Z(BC(3),BC(7))
!
! --------- Compute baseline longitude
!
            BC(8) = DATAN2Z(BC(2),BC(1))
!
! --------- Compute baseline hour angle
!
            BC(5) = DATAN2Z(VSITE1(2,xr(I)),VSITE1(1,XR(I))) - BC(8)
!
! --------- Compute the baseline vector in L,H,V (Trask) coordinates
! --------- and store in BC(9-11) and the partial derivatives of
! --------- L,H,V with respect to the geocentric X,Y,Z of the two
! --------- stations and store at the end of SPAR.
!
            IF ( NSCNT1 .GT. 0  .AND. &
     &           DSQRT ( VSITE1(1,XR(I))**2 + VSITE1(2,XR(I))**2 + &
     &                   VSITE1(3,XR(I))**2 ) < EARTH_RAD/2.0D0 ) THEN
                 WRITE ( 6, * ) ' I = ', I,' XR(I) = ', XR(I)
                 WRITE ( 23, * ) 'VSITE1(1:3,XR(I)) = ', VSITE1(1:3,XR(I)) 
                 CALL ERR_LOG ( 2701, -2, 'BWORK', 'Trap of internal '// &
     &               'control: wrong value of site positions: '// &
     &               ' the site '//ISITN_CHR(XR(I))//' is'// &
     &               ' in the hell!' )
!
! -------------- Last two lines are in order to deliberatrely to cause 
! -------------- a crash. The crash will show the calls stack
!

                 IOS = -32700
                 BC(1) = VSITEC(1,XR(IOS))
            END IF
!
            IF ( NSCNT1 .GT. 0  .AND. &
                 DSQRT ( VSITE1(1,XR(J))**2 + VSITE1(2,XR(J))**2 + &
     &                   VSITE1(3,XR(J))**2 ) < EARTH_RAD/2.0D0 ) THEN
                 CALL ERR_LOG ( 2702, -2, 'BWORK', 'Trap of internal '// &
     &               'control: wrong value of site positions: '// &
     &               ' the site '//ISITN_CHR(XR(J))//' is'// &
     &               ' in the hell!' )
!
! -------------- Last two lines are in order to deliberatrely to cause 
! -------------- a crash. The crash wiull show the calls stack
!
                 IOS = -32700
                 BC(1) = VSITEC(1,XR(IOS))
            END IF
!
            CALL GTLHV ( VSITEC(1,XR(I)), VSITEC(1,XR(J)), BC(1), BC(9), SPAR )
!
! --------- Calculate the partials for the baseline formal errors
!
            CALL SPART ( SPAR, BC, VSITE1(1,XR(I)) )
!
! --------- Calculate the baseline formal errors and the cartesian cov. matrix
!
            CALL CVMML ( ARR(JA+1), SPAR, XR(I), XR(J), BSIG, CMT, LSCRD )
!
! --------- Currently cmt is turned off see lines of code with carcov
! --------- staring in col 1
!
! --------- Skip if baseline not adjsted
!
            IF ( BSIG(1) .LT. EPS ) GOTO 800
!
! --------- Convert to degrees and hours, arc-seconds and time seconds for pri
!
            BC(8)   = BC(8) * 180.0D0/PI__NUM
            BSIG(8) = BSIG(8) * 180.0D0/PI__NUM
            BSIG(8) = BSIG(8) * 3600.0D0
            BC(6)   = BC(6) * 180.0D0/PI__NUM
            BSIG(6) = BSIG(6) * 180.0D0/PI__NUM
            BSIG(6) = BSIG(6) * 3600.0D0
            BC(5)   = BC(5) * 12.0D0/PI__NUM
            BSIG(5) = BSIG(5) * 12.0D0/PI__NUM
            BSIG(5) = BSIG(5) * 3600.0D0
            IF ( BC(5) .LT. -12 ) BC(5) = BC(5) + 24.0D0
            IF ( BC(5) .GT.  12 ) BC(5) = BC(5) - 24.0D0
!
! --------- Other calculations
!
            BVMAG = DSQRT ( BC(1)**2 + BC(2)**2 + BC(3)**2 ) ! Old eccen calc
!
! --------- Print baseline information.  Print station and monument names,
! --------- and the monument to monument values (vector magnitude, length,
! --------- horizontal, vertical and their values).
!
            IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                 IPTR=IPTR+1
                 IF ( LMONUMENTS(XR(I))(1:1) .EQ. CHAR(0) ) THEN
                      CALL CLRCH ( LMONUMENTS(XR(I)) )
                 END IF
                 IF ( LMONUMENTS(XR(J))(1:1) .EQ. CHAR(0) ) THEN
                      CALL CLRCH ( LMONUMENTS(XR(J)) )
                 END IF
                 WRITE ( LBUF(IPTR), 2016 ) (ISITN(KP,XR(I)),KP=1,4), &
     &                                      (MONUMENTS(KP,XR(I)),KP=1,5), &
     &                                      BVMAG*1.D3, &
     &                                      BC(9)*1.D3, &
     &                                      BC(10)*1.D3, &
     &                                      BC(11)*1.D3
                 CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                 CALL NL_MN()
                 IPTR=IPTR+1
!
                 WRITE ( LBUF(IPTR), 2017 ) (ISITN(KP,XR(J)),KP=1,4), &
     &                                      (MONUMENTS(KP,XR(J)),KP=1,5), &
     &                                      BSIG(9)*1.D3, &
     &                                      BSIG(9)*1.D3, &
     &                                      BSIG(10)*1.D3, &
     &                                      BSIG(11)*1.D3
                 CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                 CALL NL_MN()
                 CALL NL_MN()
           END IF
!
           IF ( KSPOOL .AND. ISITN_CHR(XR(I)) .NE. ISITN_CHR(XR(J)) )THEN
               IF ( LMONUMENTS(XR(I))(1:1) .EQ. CHAR(0) ) THEN
                    CALL CLRCH ( LMONUMENTS(XR(I)) )
               END IF
               IF ( LMONUMENTS(XR(J))(1:1) .EQ. CHAR(0) ) THEN
                    CALL CLRCH ( LMONUMENTS(XR(J)) )
               END IF
!
                WRITE(23,2026) (ISITN(KP,XR(I)),KP=1,4), &
     &                         (MONUMENTS(KP,XR(I)),KP=1,5), &
     &                         (ISITN(KP,XR(J)),KP=1,4), &
     &                         (MONUMENTS(KP,XR(J)),KP=1,5), &
     &                         BVMAG*1.D3, &
     &                         BSIG(9)*1.D3, &
     &                         (BC(KP)*1.D3,BSIG(KP)*1.D3,KP=9,11)
           ENDIF
!
 2016      FORMAT(4A2,1X,5A2,1X,"to ",2F15.2,2F12.2)
 2017      FORMAT(" ",4A2,1X,5A2,3X,  2F15.2,2F12.2)
 2026      FORMAT(1X,4A2,1X,5A2," to ",4A2,1X,5A2, &
     &            F15.2,F6.2, &
     &            F15.2,F6.2, &
     &            F10.2,F7.2, &
     &            F11.2,F7.2)
!
! -------- Handle offset monuments, for instance the laser monuments
! -------- Note that eccentricity information is required for both stations
! -------- but, that offset monument info is required for only one.
! -------- If offset is available for only one, then the VLBI monument at
! -------- one station is connected to the offset monument at the other.
!
           IF ( MOTYPE(XR(I)).NE.0 .OR.  MOTYPE(XR(J)).NE.0 ) THEN
!
! ------------- Let's do the offset monuments
!
                IF ( MOTYPE(XR(I)) .NE. 0 .AND. MOTYPE(XR(J)) .NE. 0) THEN
!
! ------------------ Offsets at both ends case
!
                     DO M=1,3
!
! --------------------- Handle monuments
!
                        BC(M) = BC(M) + (XOFFSG(M,XR(J)) - XOFFSG(M,XR(I)))
                        ZSITE(M,1) = VSITE1(M,XR(I)) + XOFFSG(M,XR(I))
                        ZSITE(M,2) = VSITE1(M,XR(J)) + XOFFSG(M,XR(J))
                     END DO  ! Handle monuments
                END IF  ! Offsets at both ends case
!
                IF ( MOTYPE(XR(I)) .NE. 0 .AND. MOTYPE(XR(J)) .EQ. 0 ) THEN
!
! ------------------ Offset at only the Ith station
!
                     DO M=1,3
!
! --------------------- Handle monuments
!
                        BC(M) = BC(M) - XOFFSG(M,XR(I))
                        ZSITE(M,1) = VSITE1(M,XR(I)) + XOFFSG(M,XR(I))
                        ZSITE(M,2) = VSITE1(M,XR(J))
                     END DO  ! Handle monuments
!
! ------------------ For the J-th station use the name of the VLBI mark.
!
                     IF ( LMONUMENTS(XR(J))(1:1) .EQ. CHAR(0) ) THEN
                          CALL CLRCH ( LMONUMENTS(XR(J)) )
                     END IF
!
                     DO M=1,5
                        MONAM(M,XR(J)) = MONUMENTS(M,XR(J))
                     END DO
                END IF ! Offset at only the Ith station
!
                IF ( MOTYPE(XR(I)) .EQ. 0 .AND. MOTYPE(XR(J)) .NE. 0 ) THEN
!
! ------------------ Offset at only the Jth station
!
                     DO M=1,3
!
! --------------------- Monument offsets
!
                        BC(M) = BC(M) + XOFFSG(M,XR(J))
                        ZSITE(M,1) = VSITE1(M,XR(I)) + XOFFSG(M,XR(I))
                        ZSITE(M,2) = VSITE1(M,XR(J)) + XOFFSG(M,XR(J))
                     END DO ! Monument offsets
!
! ------------------ For the I-th station use the name of the VLBI mark.
!
                     IF ( LMONUMENTS(XR(I))(1:1) .EQ. CHAR(0) ) THEN
                          CALL CLRCH ( LMONUMENTS(XR(I)) )
                     END IF
!
                     DO M=1,5
                        MONAM(M,XR(I)) = MONUMENTS(M,XR(I))
                     END DO
                END IF ! Offset at only the Jth station
!
                BC(4) = DSQRT ( BC(1)**2 + BC(2)**2 + BC(3)**2 )
                CALL GTLHV ( VSITEC(1,XR(I)), VSITEC(1,XR(J)), BC(1), BC(9), &
     &                       SPAR )
!
! ------------- Print monument-to-monument information.  Print station
! ------------- name and monument names, and the monument-to-monument
! ------------- vector magnitude.
!
                IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                     IPTR=IPTR+2
                     WRITE ( LBUF(IPTR), 2016 ) ( ISITN(KP,XR(I)),KP=1,4), &
     &                                          ( MONAM(KP,XR(I)),KP=1,5), &
     &                                            BC(4)*1.D3
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
!
                     IPTR=IPTR+1
                     WRITE ( LBUF(IPTR), 2017 ) ( ISITN(KP,XR(J)),KP=1,4), &
     &                                          ( MONAM(KP,XR(J)),KP=1,5), &
     &                                            BSIG(9)*1.D3
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
                     CALL NL_MN()
                END IF
!
                IF ( KSPOOL .AND..NOT. EQUAL( ISITN(1,XR(I)), INT2(1), &
     &               ISITN(1,XR(J)), INT2(1), INT2(8)) ) THEN
!
                     WRITE ( 23, 2026 ) ( ISITN(KP,XR(I)), KP=1,4 ), &
     &                                  ( MONAM(KP,XR(I)), KP=1,5), &
     &                                  ( ISITN(KP,XR(J)), KP=1,4), &
     &                                  ( MONAM(KP,XR(J)), KP=1,5), &
     &                                    BC(4)*1.D3, BSIG(9)*1.D3
!
                ENDIF
            END IF  ! Let's do the offset monuments
  800    CONTINUE ! j
  900 CONTINUE  ! i
      IF ( KSPOOL ) THEN
           WRITE ( 23, '(A)' ) ""
      END IF
!
      RETURN
      END  !#!  BWORK  #!#
