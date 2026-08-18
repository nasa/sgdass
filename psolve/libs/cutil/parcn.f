      SUBROUTINE PARCN()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PARCN PROGRAM SPECIFICATION
!
! 1.1 Look at the parameter flags from SETFL, count them,
!     and determine NPARAM, NSPARM, IPSTP and NSOURC
!
! 1.2 REFERENCES:
!
! 2.  PARCN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   ICMP, INOD, IHEO, IP, J5, J6, J7
      INTEGER*2   IEOP, ISTA, NUMEP, IXYZ, IUHR, JCLOCK, IORD, IATM, JATM
      INTEGER*2   J, NRT, JTIDE, I, K, IP1, JSTA, IROTT, IHPE, ISPE, KBITN
      INTEGER*2   INT2_ARG
      LOGICAL*4   FL_USE
      INTEGER*4   INT4
      LOGICAL*2,  EXTERNAL :: KBIT
      LOGICAL*4,  EXTERNAL :: CHECK_STABIT
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, LTM_DIF
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  851116  Removed all mask logic and inserted structures
!   JWR  860108  Added nutation time series logic
!   MK   861105  Added continued atmospheres and atmosphere rates
!   EJH  880218  Converted to ftn77, added logic for automatic clock parms
!   PET  970428  Added support of the special case of uniform segments and
!                new scheme of clock break handling
!   pet  971201  Added logic for bypassing deselected station
!   pet  2001.09.04  Fixed the bug: the previous version computed WRONG
!                    number of parameters if axis offset was estimated for
!                    the station with eposidic or pise-wise linear motion
!   pet  2003.08.15  Added support of array NPAR_GRAD_STA -- the number &
!                    of parameters before atmosphere gradients of the ith &
!                    station or zero if atmsophere gradients are not estimated
!   pet  2003.08.25  Added support of the varaible NPARAM_AFTER_BASCL -- &
!                    parameter counter just after baseline dependent clocks
!                    section
!   pet  2005.03.01  Added support of harmonic site position variations and
!                    spline parameterization for site positions
!   pet  2006.01.27  Added support of estimation of parameters related to &
!                    the Earth Rotation Model
!   pet  2007.08.09  Added support of estimation of source structure delay &
!                    admittance
!   pet  2022.08.21  Added support of estimation of estimation of ionospheric scale
!
! 5.  PARCN PROGRAM STRUCTURE
!
!     Count the parameter flags
!
      NPARAM    = 0
      NSPARM(1) = 0
      ICLMAX    = 0
!
      CALL NOUT_I2 ( INT4(MAX_ARC_STA), NPAR_GRAD_STA )
!
! --- Setting bit field for station's deselection status
!
      CALL SET_STABIT ( INT2(2) )
!
      DO ISTA=1,NUMSTA !running over the stations
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
         IF ( .NOT. CHECK_STABIT ( ISTA ) ) GOTO 810
!
! ------ First check the station coordinates
!
         NUMEP = 1
         IF ( PSITED(ISTA).NE.0 ) NUMEP = PWCNUM(1)
         DO IXYZ = 1,3
            IF ( KBIT(LSITEC(1,IXYZ),ISTA) ) NPARAM = NPARAM + NUMEP
         ENDDO
!
! ------ Next check the station velocities
!
         DO IXYZ = 1,3
            IF ( KBIT(LSITEV(1,IXYZ),ISTA) ) NPARAM = NPARAM + 1
         ENDDO
!
! ------ Diurnal radial sin and cosine
!
         IF ( KBIT( IUEN, INT2(2) ) ) NPARAM = NPARAM + 2
!
! ------ Harmonics site position variations
!
         IF ( FL_HPESOL ) THEN
              DO IHPE = 1,L_HPE ! Running over Harmonics
                 IF ( HPESOL(ISTA,IHPE)%FL_EST ) THEN
                      NPARAM = NPARAM + 6
                 END IF
              ENDDO
         END IF
!
! ------ B-spline coefficients of site motion
!
         IF ( FL_SPESOL ) THEN
              DO ISPE = 1,L_SPE ! Running over sites with spline parameterization
                 IF ( SPESOL(ISPE)%IND_STA == ISTA ) THEN
                      DO INOD=1-SPESOL(ISPE)%DEGREE,SPESOL(ISPE)%L_NOD-1
                         IF ( SPESOL(ISPE)%USED(INOD) ) THEN
                              NPARAM = NPARAM + 3
                         END IF
                      END DO
                 END IF
              END DO
         END IF
!
! ====== Check the axis offset.
!
         IF ( KBIT(LAXOF(1),ISTA) ) THEN
!
! ----------- Check to see if this station was done before. True for episdic
! ----------- motion. If so, don't estimate a new axis offset.
!
              DO JSTA=1,ISTA-1
                 IF ( ISITN_CHR(JSTA) .EQ. ISITN_CHR(ISTA) ) GOTO 820
              END DO
              NPARAM=NPARAM+1
 820         CONTINUE
         END IF
!
! ====== Check the clocks
!
         IF ( ISTA .LE. MAX_ARC_STA ) THEN
            IF ( UNF_CLO ) THEN
!
! ------------- Case of uniform clock intervals
!
                IF ( NUMCLK(ISTA) .GT. 0 ) THEN
                   IF ( KBIT(ICLSTA(1,1+ICLSTR(ISTA)), ISTA) ) THEN
!
! ------------------- Recalculate ICLMAX
!
                      DO IORD = 0,4 ! running over 0th to 4 order
!
! ---------------------- We test max degree for the first segment. We assume
! ---------------------- that max degree should be the same for all intervals
!
                         IF ( KBIT( LCLK(1+ICLSTR(ISTA)), INT2(IORD+1)) ) THEN
                              IF ( IORD+1 .GT. ICLMAX ) ICLMAX = IORD+1
                         ENDIF
                      ENDDO
!
                      NPL_CLO = ICLMAX - 1
!
                      IF ( NPL_CLO .GT. 2 ) THEN
                           WRITE ( 6, * )  '$$$ Error detected in PARCN: ' // &
     &                                     'Attempt to use the degree of ' // &
     &                                     'clock polinomial > 2 in the ' // &
     &                                     'case of uniform segments.'
                           WRITE ( 6, * ) ' This case is not more supported. '// &
     &                                    'If you REALLY need to disable ' //&
     &                                    'uniform mode by setting '// &
     &                                    'environment variable '// &
     &                                    'UNF_DISABLE = "YES"'
                           CALL EXIT ( 1 )
                      END IF
!
                      NPARAM = NPARAM + NPL_CLO  ! contributions of global polinom
                      NPARAM = NPARAM + NUM_BRK(ISTA)*(NPL_CLO+1)  ! clock breaks
                      NPARAM = NPARAM + NUM_CLO  ! contributions of segments
                   END IF
                END IF
              ELSE
                IUHR = 0
                DO WHILE ( IUHR .LT. NUMCLK(ISTA) )
!
! ---------------- Running over the clock epochs for this station
!
                   IUHR = IUHR + 1
                   JCLOCK = IUHR + ICLSTR(ISTA)
                   IF ( KBIT(ICLSTA(1,JCLOCK),ISTA) ) THEN
!
! ------------------- Check for the polynomial coefficients
!
                      DO IORD = 0,4 ! running over 0th to 4 order
                         IF ( KBIT( LCLK(JCLOCK), INT2(IORD+1)) ) THEN ! it's on
                              NPARAM = NPARAM+1
                              IF ( IORD+1 .GT. ICLMAX ) ICLMAX = IORD+1
                         ENDIF
                      ENDDO
!
! ------------------- Now check for diurnal and make sure it is not continued.
! ------------------- Also use the fact that diurnals always come in pairs
!
                      IF (      KBIT( LCLK(JCLOCK), INT2(15)) .AND..NOT. &
     &                     KBIT(LCLK(JCLOCK), INT2(14))       ) NPARAM = NPARAM+2
!
                   ENDIF
                ENDDO
            END IF
!
! ========= Check the atmosphere epochs
!
            IF ( UNF_ATM ) THEN
                 IF ( NUMATM(ISTA) .GT. 0 ) THEN
                      NPARAM = NPARAM + NUM_ATM   ! contributions of segments
                 END IF
               ELSE
                 IATM = 0
                 DO WHILE (IATM.LT.NUMATM(ISTA))
!
! ----------------- Running over the atmosphere epochs
!
                    IATM = IATM+1
                    JATM = IATM+IATSTR(ISTA)
                    IF ( KBIT(LATM(1,1),JATM) ) NPARAM = NPARAM+1
                    IF ( KBIT(LATM(1,2),JATM) ) NPARAM = NPARAM+1
                ENDDO
            END IF
!
! --------- Check the gradient epochs
!
            IATM = 0
            DO WHILE ( IATM .LT. NUMGRAD(ISTA) )
!
! ------------ running over the gradient epochs
!
               IATM = IATM+1
               IF ( KBIT(LGRAD(1), IATM) ) THEN
                    IF ( IATM .EQ. 1 ) NPAR_GRAD_STA(ISTA) = NPARAM
                    NPARAM = NPARAM+2
               END IF
            ENDDO
         ENDIF
!
! ------ Store in NSPARM for the next site the total number of
! ------ parameters accumulated upto the start of that site.
!
 810     CONTINUE
         IF ( ISTA+1 .LE. MAX_ARC_STA ) NSPARM(ISTA+1) = NPARAM
      ENDDO
!
! --- Put in NSPARM the total number of site related parameters
!
      NSLAST = NPARAM
!
! --- Count the source coordinate parameters
!
      DO J=1,NUMSTR !running over the sources
          IF(KBIT(LSTAR(1,1),J)) NPARAM = NPARAM+1
          IF(KBIT(LSTAR(1,2),J)) NPARAM = NPARAM+1
      ENDDO
!
! --- Count the source proper motion parameters
!
      DO J=1,NUMSTR !running over the sources
          IF(KBIT(LPROP(1,1),J)) NPARAM = NPARAM+1
          IF(KBIT(LPROP(1,2),J)) NPARAM = NPARAM+1
      ENDDO
!
      NSOURC = NPARAM-NSLAST
      IPSTP  = NPARAM
!
      IF ( FL_EHEO ) THEN
           DO IHEO = 1, L_EHEO ! Running over HEO constituents
              IF ( HEOSOL(IHEO)%FL_EST(HEO__E1E2) ) THEN
                   NPARAM = NPARAM + 2
              END IF
              IF ( HEOSOL(IHEO)%FL_EST(HEO__E3) ) THEN
                   NPARAM = NPARAM + 2
              END IF
              IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E1E2) ) THEN
                   NPARAM = NPARAM + 2
              END IF
              IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E3) ) THEN
                   NPARAM = NPARAM + 2
              END IF
           END DO
      END IF
!
      IF ( FL_EERM ) THEN
           DO ICMP=1,3
              IF ( IND_EERM_NOD(ICMP) > 0 ) THEN
                   NPARAM = NPARAM + EERM%DEGREE(ICMP) + 1 + EERM_OVR(ICMP)
              END IF
           END DO
      END IF
!
! --- Count the earth rotation parameters
!
      DO IEOP = 1,2 !running over x/y wobble, then UT1 style choices
!
        IF ( EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS ) THEN
            IF ( IEOP.EQ.1 ) NPARAM = NPARAM + 2  ! for x and y pole global rate
            IF ( IEOP.EQ.2 ) NPARAM = NPARAM + 1  ! for UT1 global rate
        ENDIF
!
        IF ( EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS .OR. &
     &       EOP_STYLE(IEOP) .EQ. EOP__SEGS_ONLY           ) THEN
             IF ( UNF_EOP ) THEN
                  IF ( IEOP.EQ.1 ) NPARAM = NPARAM + NUM_EOP*2
                  IF ( IEOP.EQ.2 ) NPARAM = NPARAM + NUM_EOP
               ELSE
                  IF ( IEOP.EQ.1 ) NPARAM = NPARAM + NROT_A1(IEOP)*2 !for x and y pole break epochs
                  IF ( IEOP.EQ.2 ) NPARAM = NPARAM + NROT_A1(IEOP)   !for UT1 break epochs
             ENDIF
        ENDIF
!
! ----- Sine parameterization has 18 parameters: offset, rate, 24-hr sine and cosine,
! ----- 12-hr sine and cosine times 3 coordiantes.
!
        IF ( EOP_STYLE(IEOP) .EQ. EOP__SINE ) THEN
             IF ( IEOP.EQ.1 ) NPARAM = NPARAM+12
             IF ( IEOP.EQ.2 ) NPARAM = NPARAM+ 6
        ENDIF
!
        IF ( EOP_STYLE(IEOP) .EQ. EOP__POLY .AND. NROT > 0 ) THEN ! old style - individually specified epochs
             NRT = 0
             DO WHILE (NRT.LT.NROT) !running over the rotation epochs
                NRT = NRT+1
                IF ( IEOP .EQ. 1) THEN !X/Y wobble
                     DO IXYZ = 1,2 !scanning x-pole, then y-pole
                        DO IORD = 0,3
                           IF ( IROTT ( NRT, IXYZ, INT2(IORD+1), LROT) .EQ. 1 )NPARAM=NPARAM+1
                        ENDDO !orders
                     ENDDO ! scanning x-pole, then y-pole
                   ELSE ! UT1
                     DO IORD = 0,3
                        IF ( IROTT ( NRT, INT2(3), INT2(IORD+1), LROT) .EQ. 1 )NPARAM=NPARAM+1
                     ENDDO ! orders
                END IF ! end handling for x/y vs.
!                      ! UT1 (within old style scheme)
             ENDDO ! running over epochs
        END IF ! old style scheme
      END DO ! running over x/y wobble, then UT1 style choices
!
! === Test relativity flag
!
      IF(LREL.NE.0) NPARAM = NPARAM + 1
!
! === Test earth tide parameters
!     If ITDGLB = 1 then the parameters are site dependent
!
      IF ( ITDGLB .EQ. 0 ) THEN !tide parameters are site independent
           DO JTIDE = 1,3
              IF ( LTIDE(1,JTIDE) .NE. 0 ) NPARAM = NPARAM + 1
           ENDDO
        ELSE ! site dependent earth tide parameters
           DO ISTA = 1,NUMSTA !checking the stations
!
! ----------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
              IF ( .NOT. CHECK_STABIT ( ISTA ) ) GOTO 830
              DO JTIDE = 1,3
                 IF(KBIT(LTIDE(1,JTIDE),ISTA)) NPARAM = NPARAM+1
              ENDDO
 830          CONTINUE
           ENDDO
      ENDIF
!
! === Test the precession constant parameter
!
      IF ( LPREC .NE. 0 ) NPARAM = NPARAM + 1
!
! --- Test for all types of nutation parameters
!
! --- Check for daily DPSI and DEPS
!
      DO I=1,2
         IF ( KBITN(LNUT(1),I) .EQ. 1 ) NPARAM = NPARAM+1
      ENDDO
!
! --- Check for time series terms. Remember LNUT(2) keeps track
! --- of longitude terms and LNUT(3) keeps track of obliquity
! --- terms.  The in-phase and out-of-phase terms are estimated
! --- separately and are two distict parameters. However, they are
! --- turned on in pairs and there is only one flag to keep track
! --- of a given pair.
!
      DO I=1,6 !run over the 6 possible pairs periods
          DO K=2,3 !run over longitude and obliquity
              IF(KBIT(LNUT(K),I)) NPARAM = NPARAM + 2
          ENDDO
      ENDDO
!
! --- Additional nutation terms
!
      NPARAM=NPARAM+NFLPSI+NFLEPS
!
! --- Test baseline-dependent clock parameters
! --- 6/26/86 KDB changed do loop range to make ICLOCK handling
! --- consistent through SOLVE
!
      IF ( LOGBCL ) THEN !there are some baseline dependent clocks
           DO ISTA = 1,NUMSTA-1 !running the station list
!
! ----------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
              IF ( .NOT. CHECK_STABIT ( ISTA ) ) GOTO 840
!
              IP1 = ISTA + 1
              DO JSTA = IP1,NUMSTA
!
! -------------- Check STABIT_P or STABIT_G bit fields to bypass deselcted
! -------------- station
!
                 IF ( .NOT. CHECK_STABIT ( JSTA ) ) GOTO 850
!
                 IF ( KBIT ( ICLOCK(1,ISTA), JSTA ) .OR. &
     &                KBIT ( ICLOCK(1,JSTA), ISTA )      ) NPARAM = NPARAM + 1
 850             CONTINUE
              ENDDO
 840          CONTINUE
          ENDDO
      ENDIF
      NPARAM_AFTER_BASCL = NPARAM
!
! --- Handling ionospheric scale parameters
!
      IF ( IOS_EST == IOS__SES ) THEN
           NPARAM = NPARAM + 1
         ELSE IF ( IOS_EST == IOS__STA ) THEN
           DO 450 J5=1,NUMSTA
              IF ( CHECK_STABIT ( INT2(J5) ) ) THEN
                   NPARAM = NPARAM + 1
              END IF
 450       CONTINUE 
         ELSE IF ( IOS_EST == IOS__BAS ) THEN
           DO 460 J6=1,NUMSTA
!
! ----------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
              IF ( CHECK_STABIT ( INT2(J6) ) ) THEN
                   IP = J6 + 1
                   DO 470 J7=IP,NUMSTA
!
! ------------------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
                      IF ( CHECK_STABIT ( INT2(J7) ) ) THEN
                           IF ( KBIT(ICLOCK(1,J6),J7) .OR. KBIT(ICLOCK(1,J7),J6) ) THEN
                                NPARAM = NPARAM + 1
                           END IF
                      END IF
 470               CONTINUE
              END IF
 460       CONTINUE
      END IF
!
! --- Handle high-frequency EOP parameters here
!
      IF ( KHFEOP .EQ. 2  .OR.  KHFEOP .EQ. 3 ) THEN
           NPARAM = NPARAM + (NUM_SDE_UT1 + NUM_SDE_XY)*2
      ENDIF
!
      IF ( SOU_ADM_FLAG .NE.  SOUADM__NO ) THEN
!
! -------- Count the number of parameters for source structure admittance
!
           DO J=1,NUMSTR !running over the sources
!
! ----------- Check, whether the J-th source was selected
!
!@              IF ( SUPMET == SUPMET__META ) THEN
!@                   FL_USE = BTEST ( SOU_USE(J), INT4(IDATYP) )
!@                ELSE 
                   FL_USE = KBIT (  ISRSEL(1), J ) 
!@              END IF
              IF ( FL_USE ) THEN
!
! ---------------- Yes, it was selected
!
                   IF ( SOU_ADM_FLAG == SOUADM__GLB_ALL ) THEN
!
! --------------------- Admittance is computed for all sources. Count it.
!
                        NPARAM = NPARAM + 1
                        GOTO 860
                      ELSE IF ( SOU_ADM_FLAG == SOUADM__LCL_ALL ) THEN
!
! --------------------- Admittance is computed for all sources. Count it.
!
                        NPARAM = NPARAM + 1
                        GOTO 860
                      ELSE 
!
! --------------------- Admittance is computed for a list of sources
!
                        IF ( IND_SOU_ADM(1) > 0 ) THEN
!
! -------------------------- Search the J-th sources in the list
!
                             IP = LTM_DIF ( 1, IND_SOU_ADM(2)-IND_SOU_ADM(1)+1, &
     &                                      SRCSUP(IND_SOU_ADM(1)), &
     &                                      ISTRN_CHR(J) )
                             IF ( IP > 0 ) THEN
!
! ------------------------------- The J-th source was found in the list
!
                                  IF ( SOU_ADM_FLAG == SOUADM__GLB_LIST_NO ) THEN
                                       NPARAM = NPARAM + 1
                                     ELSE IF ( SOU_ADM_FLAG == SOUADM__LCL_LIST_NO ) THEN
                                       NPARAM = NPARAM + 1
                                  END IF
                                ELSE 
                                  IF ( SOU_ADM_FLAG == SOUADM__GLB_LIST_YES ) THEN
                                       NPARAM = NPARAM + 1
                                    ELSE IF ( SOU_ADM_FLAG == SOUADM__LCL_LIST_YES ) THEN
                                       NPARAM = NPARAM + 1
                                  END IF
                             END IF 
                           ELSE IF ( IND_SOU_ADM(1) == 0 .AND. &
     &                               SOU_ADM_FLAG == SOUADM__GLB_LIST_NO ) THEN
                             NPARAM = NPARAM + 1
                           ELSE IF ( IND_SOU_ADM(1) == 0 .AND. &
     &                               SOU_ADM_FLAG == SOUADM__LCL_LIST_NO ) THEN
                             NPARAM = NPARAM + 1
                        END IF ! The list is defined
                   END IF ! Admittance flag
              END IF ! fl_use
         ENDDO ! sources
 860     CONTINUE 
      END IF ! Source admittance
!
! --- Check for user_defined parameters ( set up with USER_PARTIAL feature)
!
      IF ( KUSER_PART ) THEN
           NPARAM = NPARAM + NUM_USER_PART
      ENDIF
!
! --- See if the number of parameters has changed since the last
! --- PARCN was run.
!
      IPSTP = NPARAM - IPSTP
!
      RETURN
      END  !#!  PARCN  #!#
