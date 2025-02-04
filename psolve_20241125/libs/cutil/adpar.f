      SUBROUTINE ADPAR()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ADPAR PROGRAM SPECIFICATION
!
! 1.1 Add parameters from IPARM2 to SOLVE Common. Only sites
!     and sources can be added.
!
! 1.2 REFERENCES:
!
! 2.  ADPAR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'q_socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'q_prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbc2.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: lists
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*4     DATYP_INQ
      LOGICAL*2     KBIT, EQUAL, KNAM, KEP, KESM, STARC, l2hold
      INTEGER*2     ICORD(3), IRADC(2), I, J, K, L, KBITN
      INTEGER*2     IMATCH, II, LDBNAM(5,15), IDBVER(15), NUMDD
      INTEGER*4     IUER
      CHARACTER     LDBNAM_C*10, ERRSTR*120, STR*128, STR1*32, STR2*32, STR3*32, STR4*32
      EQUIVALENCE ( LDBNAM, LDBNAM_C )
      LOGICAL*4     FL_SOU_NOCHECK, FL_STA_NOCHECK
      INTEGER*4,    EXTERNAL :: ILEN, I_LEN
!
      DATA ICORD/2HX ,2HY ,2HZ /,IRADC/2HRA,2HDC/
      REAL*8        FJD_BEG, FJD_END
!CCCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   911112 Enhanced a priori error messages passed to ferr.
!   :94.01.24:jwr: Improved error messages.
!   :96.06.04:kdb: Improved error message for condition 9091
!                  (apriori site mismatch).
!   :97.10.10:pet: Commented out a vile code ("setting up atmosphere
!                  and clock epoch"). It causes severe bug ADR_OCT97
!   :97.12.10:pet: Added logic for bypassing deselected stations and
!                  deseleced source in database
!   :98.02.03:pet: Substituted hard-coded test of solution type by DATYP_INQ
!   :98.11.07:pet: Corrected a bug in logic: the previus version was trying
!                  to add a deselected source to the global list of sources
!   2002.09.26 pet Added logic for keeping Julian dates of first and lasr
!                  observation of each object, station and source
!   2002.12.25 pet Added updated of variatlbes GLO_FJDOBS_MIN, GLO_FJDOBS_MAX
!   2003.04.29 pet Fixed a bug: the previous version did not recognized
!                  the source as a global parameter, if only source proper
!                  motion was estimated without estimation of source position
!   2003.05.15 jwr 5 logical expression in subrouine calls replaced with the
!                  variable l2hold. (Adds confidance in the '-I2' conversion.)
!   2010.04.17 pet Added support of the kludge variable ADPAR_SOU_NOCHECK to
!                  disable source apriori consistency
!   2015.02.21 pet Added more verbose output in a case of inconsistency
!   2021.06.02 pet Added support of JNAME
!   2022.03.03 pet Impoved an error message "Too many"
!
!CCCCCC
!
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      CALL OBSTM ( FJD_BEG, FJD_END )
      IF ( KBIT( IUEN, INT2(1) )  .NEQV.  KBIT( Q_IUEN, INT2(1) ) ) THEN
           CALL FERR ( INT2(9045), 'XYZ - UEN conflict', INT2(0), INT2(0) )
      END IF
!
! --- Comment: variables WITH prefix Q_ -- from ARC originated socom/prfil block
! --- Comment: variables WITHOUT prefix Q_ -- from global originated socom/prfil
! --- block
!
      FL_SOU_NOCHECK = .FALSE.
      CALL GETENVAR ( 'ADPAR_SOU_NOCHECK', STR )
      IF ( STR == 'YES' ) FL_SOU_NOCHECK = .TRUE.
!
      FL_STA_NOCHECK = .FALSE.
      CALL GETENVAR ( 'ADPAR_STA_NOCHECK', STR )
      IF ( STR == 'YES' ) FL_STA_NOCHECK = .TRUE.
!
      IF ( KBIT( Q_IUEN, INT2(2) ) ) CALL SBIT ( IUEN, INT2(2), INT2(1) )
!
! ---- Copy global minimal and maximal observations date
!
!      GLO_FJDOBS_MIN = Q_GLO_FJDOBS_MIN
!      GLO_FJDOBS_MAX = Q_GLO_FJDOBS_MAX
!
! --- Stations
!
      DO 1000 I=1,Q_TOTSTA
!
! ------ We don't delete stations that are not turned on in the arc,
! ------ so that the baselines to the reference station can still be calculated.
!
! ------ Check for same station in the current solution
!
         IMATCH = 0
         KNAM   = .FALSE.
         KEP    = .FALSE.
         KESM   = Q_VSITED(I) .NE. 0
!
         IF ( DATYP_INQ ( Q_IDATYP, PHASE__DTP ) ) THEN
              IF ( .NOT. KBIT ( Q_STABIT_P, I ) ) GOTO 1000
            ELSE
              IF ( .NOT. KBIT ( Q_STABIT_G, I ) ) GOTO 1000
         END IF
!
         DO J=1,TOTSTA
            IF ( Q_ISITN_CHR(I) .EQ. ISITN_CHR(J) ) THEN
!
! -------------- J-th station is already in global parfil
!
                 KESM = KESM .OR. (VSITED(J).NE.0)
                 KNAM = .TRUE.
                 IF ( VSITED(J) .EQ. Q_VSITED(I) .AND. &
     &                PSITED(J) .EQ. Q_PSITED(I)       ) THEN
!
                      KEP = .TRUE.
                      IF ( IMATCH .EQ. 0 ) IMATCH=J
                 ENDIF
!
                 IF ( KEP ) THEN
!
! ------------------- We update counters if
! ------------------- 1) The station did not have episodic motion (VSITED=0)
! ------------------- 2) The station is in the same episodic motion range
!
                      STA_FJD_END(J) = FJD_END
                      STA_FJD_MID(J) = (STA_FJD_MID(J)*NSES_STA(J) + &
     &                          (FJD_BEG + FJD_END)/2.0D0 )/DBLE(NSES_STA(J)+1)
                      NSES_STA(J) = NSES_STA(J) + 1
                 END IF
             ENDIF
         ENDDO
!
! ------ Add it if it isn't there
!
         IF ( .NOT. KEP ) THEN
              TOTSTA=TOTSTA+1
              IF ( TOTSTA .GT. MAX_STA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( INT4(MAX_STA), STR )
                   CALL FERR ( INT2(9092), 'CUTIL/ADPAR: Too many stations, '// &
     &                 'more than '//TRIM(STR), INT2(0), INT2(0) )
              ENDIF
              J=TOTSTA
!
              DO K=1,3
                 CALL SBIT ( LSITEC(1,K), J, INT2(0) )
                 CALL SBIT ( LSITEV(1,K), J, INT2(0) )
                 VSITEC(K,J) = Q_VSITEC(K,I)
                 VSITEV(K,J) = Q_VSITEV(K,I)
              ENDDO
!
              VAXOF(J)  = Q_VAXOF(I)
              VSITED(J) = Q_VSITED(I)
              PSITED(J) = Q_PSITED(I)
!
              DO K=1,4
                 ISITN(K,J) = Q_ISITN(K,I)
              ENDDO
!
              DO K=1,5
                 MONUMENTS(K,J)=Q_MONUMENTS(K,I)
              ENDDO
!
              STA_FJD_BEG(J) = FJD_BEG
              STA_FJD_END(J) = FJD_END
              STA_FJD_MID(J) = ( FJD_BEG + FJD_END )/2.0D0
              NSES_STA(J) = 1
         ENDIF
!
         IF ( .NOT. KNAM .OR. (.NOT.KEP) ) THEN
              NUMSTA = NUMSTA+1
         ENDIF
!
! ------ Turn on the appropriate bits
!
750      CONTINUE
         IF ( IMATCH .GT. 0 ) J = IMATCH
         DO K=1,3
!
! --------- Set coordinate bit
!
            L2HOLD = KBIT ( Q_LSITEC(1,K), I ) .OR. KBIT ( LSITEC(1,K), J )
            CALL KSBIT ( LSITEC(1,K), J, L2HOLD )
!
! --------- Set velocity bit (unless we're adding second set of parms for
! --------- the same station)
!
            IF ( .NOT. KNAM .OR. .NOT. KESM ) THEN
                 L2HOLD = KBIT ( Q_LSITEV(1,K), I ) .OR. KBIT ( LSITEV(1,K), J )
                 CALL KSBIT ( LSITEV(1,K), J, L2HOLD )
            ENDIF
!
! --------- Check apriori
!
            STARC = KCSTA
            DO II=1,NACSTA
               IF ( EQUAL ( ISELAR(IACSTA+(II-1)*4), INT2(1), ISITN(1,J), &
     &              INT2(1), INT2(8) ) ) THEN
                    STARC = .NOT.STARC
               ENDIF
            ENDDO
!
            IF ( STARC .AND. DABS(Q_VSITEC(K,I)-VSITEC(K,J)) .GT. 0.001 .AND. &
     &           .NOT. Q_SIMULATION_TEST .AND. .NOT. FL_STA_NOCHECK ) THEN
!
                 CALL DBPOX ( NUMDD, LDBNAM, IDBVER, IDBEND )
                 WRITE ( 6, '("CUTIL/ADPAR: Fatal error!")' )
                 WRITE ( 6, '("Inconsistent apriori positions at session ", &
     &                          a10, " Ver. ",i4)') ldbnam_c, idbver(1)
                 WRITE ( 6, '(1x,4A2,1x,A2)') ( ISITN(L,J),L=1,4 ), ICORD(K)
                 WRITE ( 6, '(" global value ",F15.4," meters")') VSITEC(K,J)
                 WRITE ( 6, '(" session value",F15.4," meters")') Q_VSITEC(K,I)
                 WRITE ( 6, &
     &                   '(" Difference   ",F15.4," meters")')VSITEC(K,j)-q_VSITEC(K,i)
                 WRITE ( ERRSTR, '("Inconsistent apriori positions at ", A10, &
     &                  " Ver. ",i4,1x,4A2,1x,A2)' ) LDBNAM_C, IDBVER(1), &
     &                  (ISITN(L,J),L=1,4), ICORD(K)
!!                 CALL FERR ( INT2(9091), 'CUTIL/ADPAR: '//ERRSTR, INT2(0), &
!!     &                INT2(0) )
            ENDIF
!
            IF ( DABS(Q_VSITEV(K,I)-VSITEV(K,J)).GT. 0.001 .AND. &
     &           .NOT. Q_SIMULATION_TEST .AND. .NOT. FL_STA_NOCHECK ) THEN
!
                 CALL DBPOX(NUMDD,LDBNAM,IDBVER,IDBEND )
                 WRITE ( 6, '("CUTIL/ADPAR: Fatal error!")')
                 WRITE ( 6, '("Inconsistent apriori velocity at session ",a10, &
     &                        " Ver. ",i4)') LDBNAM_C, IDBVER(1)
                 WRITE ( 6, * ) &
     &                  'Set environment variable ADPAR_STA_NOCHECK YES', &
     &                  ' if you need to override this check'
                 WRITE ( ERRSTR, '("Inconsistent apriori velocity at session ", &
     &                              a10, " Ver. ",i4)') LDBNAM_C, IDBVER(1)
                 WRITE ( 6, '(1x,4A2,1x,A2)')(ISITN(L,J),L=1,4),ICORD(K)
                 WRITE ( 6, '(" global value ",F15.4," meter/yr")')   VSITEV(K,j)
                 WRITE ( 6, '(" session value",F15.4," meter/yr")') Q_VSITEV(K,i)
                 WRITE ( 6, &
     &                        '(" Difference   ",F15.4," meter/yr")')VSITEV(K,j) - Q_VSITEV(K,I)
                 CALL FERR ( INT2(9093), 'CUTIL/ADPAR: '//ERRSTR, INT2(0), &
     &                INT2(0) )
           ENDIF
         ENDDO
!
! ------ Set axis offset bit
!
!!!!     CALL KSBIT(LAXOF(1),J,KBIT(Q_LAXOF(1),I).OR.KBIT(LAXOF(1),J) )
!c
         l2hold = KBIT(Q_LAXOF(1),I) .OR. KBIT(LAXOF(1),J)
         CALL KSBIT(LAXOF(1) ,J ,l2hold )
!
! ------ Check apriori
!
         IF ( DABS(Q_VAXOF(I)-VAXOF(J)) .GT. 0.001 .AND. &
     &        .NOT. Q_SIMULATION_TEST .AND. .NOT. FL_STA_NOCHECK ) THEN
!
              CALL DBPOX ( NUMDD, LDBNAM, IDBVER, IDBEND )
              WRITE ( 6, '("CUTIL/ADPAR: Fatal error!")')
              WRITE ( 6, '("Inconsistent apriori axis offset at session ", &
     &                A10, " Ver. ",I4)' ) LDBNAM_C, IDBVER(1)
              WRITE ( ERRSTR, '("Inconsistent apriori axis offset at session ", &
     &                A10, " Ver. ",I4)' ) LDBNAM_C, IDBVER(1)
              WRITE ( 6, * ) &
     &                  'Set environment variable ADPAR_STA_NOCHECK YES', &
     &                  ' if you need to override this check'
              WRITE ( 6, '(1X,4A2)') (ISITN(L,J),L=1,4)
              WRITE ( 6, '(" global value ",F15.4," meters")' ) VAXOF(J)
              WRITE ( 6, '(" session value",F15.4," meters")' ) Q_VAXOF(I)
              WRITE ( 6, '(" Difference   ",F15.4," meters")') &
     &                    VAXOF(J)-Q_VAXOF(I)
              CALL FERR ( INT2(9094), 'CUTIL/ADPAR '//ERRSTR, INT2(0), &
     &             INT2(0) )
         ENDIF
!
! ------ Check for site dependent Earth Tides
!
         IF ( ITDGLB .NE. 0   .AND.   Q_ITDGLB .NE. 0 ) THEN
              DO K=1,3
                 IF(KBIT(Q_LTIDE(1,K),I)) CALL SBIT( LTIDE(1,K), J, INT2(1) )
              ENDDO
         ENDIF
1000  CONTINUE ! End of cycle over stations
!
! --- Stars & quasars & other sources
!
      DO I=1,Q_NUMSTR
!
! ------ Check: was the I-th source selected in solution?
!
         IF ( .NOT. KBIT ( Q_ISRSEL(1), I ) ) GOTO 2000 ! source was deleceted
!
! ------ Check: are we estimating coordinates or proper motion of the
! ------ I-th source?
!
         DO K=1,2
            IF ( KBIT ( Q_LSTAR(1,K),I) ) GOTO 1500 ! Yes, we are
            IF ( KBIT ( Q_LPROP(1,K),I) ) GOTO 1500 ! Yes, we are
         ENDDO
!
! ------ Alas, we don't estmate neither right ascension nor declination.
! ------ We cease dealing this sourcde and going to the next source
!
         GOTO 2000
!
! ------ Check for an already existing star
!
1500     CONTINUE
         DO J=1,NUMSTR
            IF ( EQUAL( Q_ISTRN(1,I), INT2(1), ISTRN(1,J), INT2(1), INT2(8)) &
     &            )THEN
                 SRC_FJD_END(J) = FJD_END
                 SRC_FJD_MID(J) = ( SRC_FJD_MID(J)*NSES_SRC(J) + &
     &                              (FJD_BEG+FJD_END)/2.0D0 )/ &
     &                            DBLE(NSES_SRC(J)+1)
                 NSES_SRC(J) = NSES_SRC(J) + 1
                 GOTO 1750
            END IF
         ENDDO
!
! ------ Add it to the list, since it isn't there already
!
         NUMSTR=NUMSTR+1
         IF ( NUMSTR .GT. MAX_SRC ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(MAX_SRC), STR )
              CALL FERR ( INT2(9095), 'CUTIL/ADPAR: Too many sources, '// &
     &            'more than '//TRIM(STR), INT2(0), INT2(0) )
         ENDIF
         J=NUMSTR
         DO K=1,4
            ISTRN(K,J)=Q_ISTRN(K,I)
         ENDDO
         JNAME(J) = Q_JNAME(I)
         DO K=1,2
            CALL SBIT ( LSTAR(1,K), J, INT2(0) )
            CALL SBIT ( LPROP(1,K), J, INT2(0) )
            VSTARC(K,J)=Q_VSTARC(K,I)
         ENDDO
!
         SRC_FJD_BEG(J) = FJD_BEG
         SRC_FJD_END(J) = FJD_END
         SRC_FJD_MID(J) = ( FJD_BEG + FJD_END )/2.0D0
         NSES_SRC(J) = 1
!
! ------ Turn on the appropriate bits
!
1750     CONTINUE
         DO K=1,2
!
! --------- Set bits
!
!!!!!       CALL KSBIT ( LSTAR(1,K), J, KBIT(Q_LSTAR(1,K),I)  .OR. KBIT(LSTAR(1,K), J ) )
            L2HOLD = KBIT(Q_LSTAR(1,K),I)  .OR. KBIT(LSTAR(1,K), J )
            CALL KSBIT ( LSTAR(1,K), J, L2HOLD )
!
!!!!!       CALL KSBIT ( LPROP(1,K), J, KBIT(Q_LPROP(1,K),I)  .OR. KBIT(LPROP(1,K), J ) )
            L2HOLD = KBIT(Q_LPROP(1,K),I)  .OR. KBIT(LPROP(1,K), J )
            CALL KSBIT ( LPROP(1,K), J, L2HOLD )
!
! --------- Check a prioiris
!
            IF ( DABS(Q_VSTARC(K,I)-VSTARC(K,J)) .GT. 5.D-9 .AND. &
     &           .NOT. FL_SOU_NOCHECK ) THEN
                 CALL RH_TAT (   VSTARC(1,J), 6, STR1, IUER )
                 CALL RG_TAT (   VSTARC(2,J), 6, STR2, IUER )
                 CALL RH_TAT ( Q_VSTARC(1,I), 6, STR3, IUER )
                 CALL RG_TAT ( Q_VSTARC(2,I), 6, STR4, IUER )
                 CALL DBPOX(NUMDD,LDBNAM,IDBVER,IDBEND )
                 WRITE ( 6, '("CUTIL/ADPAR: Fatal error!")')
                 WRITE ( 6, '( &
     &              "Inconsistent apriori source positions at session ",a10, &
     &              " Ver. ",i4)') ldbnam_c,idbver(1)
                 WRITE ( 6, * ) &
     &                  'Set up environment variable ADPAR_SOU_NOCHECK YES', &
     &                  ' if you need to override this check'
                 WRITE ( 6, 9656 ) ISTRN_CHR(J), IRADC(K), &
     &                 ( Q_VSTARC(K,I)-VSTARC(K,J))*180.0D0*3.6D6/PI__NUM
9656             FORMAT( ' ',A,' ',A2,' Difference ',F15.4,' mas'/  &
     &                  'Set environment variable ADPAR_SOU_NOCHECK YES', &
     &                  ' if you need to override this check' )
                 WRITE ( ERRSTR, 9657 ) ISTRN_CHR(J), IRADC(K), &
     &                 ( Q_VSTARC(K,I)-VSTARC(K,J))*180.0D0*3.6D6/PI__NUM
 9657            FORMAT( ' ',A,' ',A2,' Difference ',F15.4,' mas' )
                 WRITE ( 6, 9666 ) ISTRN_CHR(J), STR1(1:I_LEN(STR1)), STR2(1:I_LEN(STR2)), &
     &                                           STR3(1:I_LEN(STR3)), STR4(1:I_LEN(STR4))
                 CALL FERR ( INT2(9096), ERRSTR, INT2(0), INT2(0) )
 9666            FORMAT ( 'Source: ',A, ' CGM positions: ', A, 2X, A/ &
     &                    13X,'Apriori positions: ', A, 2X, A )
            ENDIF
         ENDDO
2000     CONTINUE
      ENDDO  ! end of cycle over sources
!
! --- Handle other parameters
! --- Relativity
!
      IF ( Q_LREL .NE. 0 ) LREL=Q_LREL
!
! --- Global Earth Tides
!
      IF ( ITDGLB .EQ. 0  .AND. Q_ITDGLB .EQ. 0 ) THEN
           DO I=1,3
              IF ( Q_LTIDE(1,I) .NE. 0 ) LTIDE(1,I)=Q_LTIDE(1,I)
           ENDDO
      ENDIF
!
      IF ( ITDGLB .EQ. 0  .NEQV. Q_ITDGLB .EQ. 0 ) THEN
           CALL FERR ( INT2(9097), &
     &         'CUTIL/ADPAR: Local-global earth tides conflict', INT2(0), &
     &          INT2(0) )
      ENDIF
!
! --- Precesion
!
!      IF ( Q_LPREC .NE. 0 ) LPREC=Q_LPREC
!      IF ( DABS(VPREC-Q_VPREC) .GT. 1D-4 ) THEN
!           WRITE ( 6, * ) 'Global  value   VPREC = ',  VPREC
!           WRITE ( 6, * ) 'Session value Q_VPREC = ',Q_VPREC
!           WRITE(6,9989) Q_VPREC-VPREC
!9989       FORMAT ( ' Difference in a priori precession is too big: ',D20.10 )
!           WRITE ( ERRSTR, 9989 ) Q_VPREC-VPREC
!           CALL FERR ( INT2(9098), ERRSTR, INT2(0), INT2(0) )
!      ENDIF
!
! --- Nutation Offset
!
      DO J=1,2
         IF ( KBIT(Q_LNUT,J) ) CALL SBIT ( LNUT, J, INT2(0) )
      ENDDO
!
! --- Nutation Time Series
!
      DO J=2,3
         DO I=1,6
            IF ( KBIT(Q_LNUT(J),I) ) CALL SBIT( LNUT(J), I, INT2(1) )
            IF ( DABS(VNUT(J-1,I)-Q_VNUT(J-1,I)) .GT. 5.D-13 ) THEN
                 WRITE(6,9990) J-1,I,VNUT(J-1,I)-Q_VNUT(J-1,I)
9990             FORMAT(' Nutation in phase a priori error',2I5,D20.10)
                 call ferr ( INT2(9099), &
     &               'CUTIL/ADPAR: Nutation in phase error', INT2(0), INT2(0) )
            ENDIF
            IF ( DABS(VNUTOP(J-1,I)-Q_VNUTOP(J-1,I)) .GT. 5.D-13 ) THEN
                 WRITE(6,9991) J-1, I, VNUTOP(J-1,I)-Q_VNUTOP(J-1,I)
9991             FORMAT( ' Nutation out of phase a priori error', 2I5, D20.10 )
                 CALL FERR ( INT2(9100), &
     &               'CUTIL/ADPAR: Nutation out of phase error', INT2(0), &
     &                INT2(0) )
           ENDIF
         ENDDO
      ENDDO
!
! --- Additional nutation terms
!
      IF ( Q_NFLPSI .NE. 0  .OR.  Q_NFLEPS .NE. 0 ) THEN
           NFLPSI=0
           NFLEPS=0
           DO I=1,216
              IF ( KBIT ( Q_FLPSI, I ) ) CALL SBIT ( FLPSI, I, INT2(1) )
              IF ( KBIT ( Q_FLEPS, I ) ) CALL SBIT ( FLEPS, I, INT2(1) )
              NFLPSI = NFLPSI + KBITN ( FLPSI, I )
              NFLEPS = NFLEPS + KBITN ( FLEPS, I )
           ENDDO
      ENDIF
!
      RETURN
      END  !#!  ADPAR  #!#
