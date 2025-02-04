      FUNCTION IONO_AMB ( OBS, GAMBX, GAMBS, LUSE, LSUP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  IONO_AMB  eliminates group delay ambiguities from group   *
! *   ionosphere corrections and add them to X-band group delays.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       OBS  ( RECORD    ) -- Data structure which contains            *
! *                             band-independent information: time of    *
! *                             observation, baseline, lists of objects, *
! *                             status flags etc.                        *
! *      LUSE ( LOGICAL*4  ) -- Flag: whether to take into account       *
! *                             not used observations. (.TRUE. means     *
! *                             take).                                   *
! *                                                                      *
! *      LSUP ( LOGICAL*4  ) -- If TRUE then observations which are      *
! *                             changed for re-distribution of           *
! *                             ambiguities are suppressed.              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <IONO_AMB> ( INTEGER*4 ) -- Counter of observations for which        *
! *                             correction to group delay ionosphere due *
! *                             to errors in determination group delay   *
! *                             ambiguities for X-band were applied.     *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *      GAMBX ( RECORD    ) -- Data structures for group delay          *
! *                             ambiguity resolution software, for       *
! *                             X-band.                                  *
! *      GAMBS ( RECORD    ) -- Data structures for group delay          *
! *                             ambiguity resolution software, for       *
! *                             S-band.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                          Input:  switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  15-AUG-97    IONO_AMB   v3.4 (c)  L. Petrov  31-JUL-2001  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'gamb.i'
      TYPE ( OBS__STRU ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMBX, GAMBS
      LOGICAL*4  LUSE, LSUP
      INTEGER*4  IONO_AMB, IUER, M_SEG
      PARAMETER  ( M_SEG = 64 )
      REAL*8       GAMB__GIMS, GAMB__GIMX, GION__EPS, GAV_LIM
      PARAMETER  ( GAMB__GIMS = 10.D-9  )  !  Separation segments for S-band
      PARAMETER  ( GAMB__GIMX =  2.D-9  )  !  Separation segments for X-band
      PARAMETER  ( GION__EPS  =  1.D-15 )  !  Minimal acceptable value of GION
      PARAMETER  ( GAV_LIM    =  1.D-20 )  !  Minimal baseline average which
!                                          !  Is still not considered as zero
      INTEGER*4  L_SEG, LIS_SEG(M_SEG)
      REAL*8     GIONX_SP, GIONS_SP, AV_SEG(M_SEG), DOBS_TAU_XS
      REAL*8     GAV_BAS(MG_BAS), GAV_STA(MG_STA), CLS, TOL_RANGE
      PARAMETER ( TOL_RANGE = 0.25 ) ! Tolerance range for misclosure
      CHARACTER  CBAST*17, STR*32
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, LMX, IMX, I_AMB, ICN_AMB, &
     &           IBS
      INTEGER*4  IND_STA(MG_STA), JMP_BAS(MG_BAS), IST1, IST2, NST1, NST2
      INTEGER*4  IFIND_PL
!C
      IONO_AMB = 0
!
! --- Let's calculate spacings for group ionosphere time delay due to
! --- possible errors in group delay ambiguity determinations
!
      GIONS_SP = MIN ( GAMBX%GAMB_SP, GAMBS%GAMB_SP )*GAMBX%FREQ_GR**2/ &
     &           ABS ( GAMBS%FREQ_GR**2 - GAMBX%FREQ_GR**2)
!
      GIONX_SP = MIN ( GAMBX%GAMB_SP, GAMBS%GAMB_SP )*GAMBS%FREQ_GR**2/ &
     &           ABS ( GAMBS%FREQ_GR**2 - GAMBX%FREQ_GR**2)
!
! --- Further analysis will be done for each baseline separately
!
      CALL NOUT_R8 ( MG_BAS, GAV_BAS )
      CALL NOUT_R8 ( MG_STA, GAV_STA )
      DO 410 J1=1,OBS%L_BAS
!
! ------ Split array of group ionosphere corrections for S-band at segementes
!
         L_SEG = 0
         DO 420 J2=1,OBS%NOBS                            ! Cycle on all observ.
!
! --------- We bypass the j2-observation if
! --------- 1) ionosphere calibration has not been calculated for it
! --------- 2) observation was not used ( and LUSE flag us true )
!
            IF ( DABS( GAMBX%GION_TAU(J2) ) .LT. GION__EPS ) GOTO 420
            IF ( LUSE .AND. .NOT. (GAMBX%USE(J2) .AND. GAMBS%USE(J2))) GOTO 420
!
            IF ( OBS%IBA(J2) .EQ. OBS%LIS_BAS(J1) ) THEN ! of j1-th baseline
                 IF ( L_SEG .EQ. 0 ) THEN
!
! ------------------- First observation -- put at the first segment
!
                      L_SEG = 1
                      LIS_SEG(L_SEG)=1                   ! Initilize counter and
                      AV_SEG(L_SEG) = GAMBS%GION_TAU(J2) ! ... average
                    ELSE
!
! ------------------- If this observation is not the first we try all segements
!
                      DO 430 J3=1,L_SEG
                         IF ( ABS( GAMBS%GION_TAU(J2) - AV_SEG(J3) ) .LT. &
     &                   GAMB__GIMS ) THEN
!
! --------------------------- Difference between observation and average is
! --------------------------- at acceptable level. Put it to segment. Update
! --------------------------- counter and average group delay ionosphere
! --------------------------- correction
!
                              LIS_SEG(J3) = LIS_SEG(J3) + 1
                              AV_SEG(J3) = ( AV_SEG(J3)*(LIS_SEG(J3)-1) + &
     &                                       GAMBS%GION_TAU(J2) ) /LIS_SEG(J3)
                              GOTO 420
                         END IF
 430                  CONTINUE
!
! ------------------- No one segment suited. Let's create a new one.
!
                      L_SEG = L_SEG + 1
                      IF ( L_SEG .GT. M_SEG ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( M_SEG, STR )
                           CALL ERR_LOG ( 7751, IUER, 'IONO_AMB', 'Trap of '// &
     &                         'internal control in processing baseline '// &
     &                          CBAST( OBS, OBS%LIS_BAS(J1) )// &
     &                         ' : to many segments M_SEG='//STR )
                           RETURN
                      END IF
                      LIS_SEG(L_SEG)=1
                      AV_SEG(L_SEG) = GAMBS%GION_TAU(J2)
                 END IF
            END IF
 420     CONTINUE
         IF ( L_SEG .EQ. 0 ) GOTO 410
!
! ------ Now -- search of the segment which contains maximum number of
! ------ observation. It will be master segment
!
         LMX = 0
         IMX = 0
         DO 440 J4=1,L_SEG
            IF ( LIS_SEG(J4) .GT. LMX ) THEN
                 LMX = LIS_SEG(J4)
                 IMX = J4         ! Index of master segment in list of segments
            END IF
 440     CONTINUE
!
! ------ Calculate ICN_AMB -- permanent number of group delay ambiguities for
! ------ X-band which caused permanent shift values of group delay ionosophere
! ------ correction for S-band.
!
         ICN_AMB = NINT ( (AV_SEG(IMX))/ GIONS_SP )
!
! ------ Store average group delay ionopshere correction for the band S
! ------ (but the value which it will have after applying correction, including
! ------ permanent shift)
!
         GAV_BAS(J1) = AV_SEG(IMX) - ICN_AMB*GIONS_SP
!
         IF ( OBS%IT .GT. 4 ) THEN
              WRITE ( 6, 120 )  J1, CBAST ( OBS, OBS%LIS_BAS(J1) ), ICN_AMB, &
     &                      AV_SEG(IMX)*1.D9, L_SEG
 120          FORMAT ( 1X,' %%%  IONO_AMB: ',I3,' (',A,') ICN_AMB=',I6, &
     &                    ' AVG=',F12.2,' (',I3,')' )
         END IF
!
         GAMBX%SH_BAS(J1) = GAMBX%SH_BAS(J1) + ICN_AMB*GAMBX%GAMB_SP* &
     &                      GAMBX%FREQ_GR**2/ &
     &                      ABS ( GAMBS%FREQ_GR**2 - GAMBX%FREQ_GR**2)
!
         DO 450 J5=1,OBS%NOBS
!
            IF ( OBS%IBA(J5) .EQ. OBS%LIS_BAS(J1) ) THEN
!
! -------------- Calculate I_AMB -- number of group delay ambiguity for
! -------------- X-band which caused shift value of group delay ionosophere
! -------------- correction for S-band for this observation.
!
                 IF ( DABS( GAMBX%GION_TAU(J5) ) .GT. GION__EPS ) THEN
                     I_AMB = ICN_AMB + NINT ((GAMBS%GION_TAU(J5) - AV_SEG(IMX))/ &
     &                       GIONS_SP)
                   ELSE
                     I_AMB = ICN_AMB
                 END IF
                 IF ( I_AMB .NE. 0 ) THEN
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!            type *,' IONO_AMB   j5= ',j5,' i_amb =',i_amb,                 ! %%
!     #     ' av = ',av_seg(imx)*1.d9,' s_ion = ',gambs.gion_tau(j5)*1.d9   ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------------------- Ambiguity is not zero? We should apply correction.
!
! ------------------- Correct X-band O-C for group delay.
!
                      GAMBX%OCT(J5) = GAMBX%OCT(J5) + I_AMB*GAMBX%GAMB_SP
                      GAMBX%JMP(J5) = GAMBX%JMP(J5) + I_AMB*GAMBX%GAMB_SP
!
! ------------------- Then make "raw" ionosphere contaminated O-C for
! ------------------- X- and S- band observable (without previous correction
! ------------------- due to ionosphere)
!
                      GAMBX%OCT(J5) = GAMBX%OCT(J5) + GAMBX%GION_TAU(J5)
                      GAMBS%OCT(J5) = GAMBS%OCT(J5) + GAMBS%GION_TAU(J5)
!
! ------------------- ... To produce the difference observed delays for X-band
! ------------------- minus S-band
!
                      DOBS_TAU_XS   = GAMBX%OCT(J5) - GAMBS%OCT(J5)
!
! ------------------- New recalculate group ionosphere deleay...
!
                      GAMBX%GION_TAU(J5) = -DOBS_TAU_XS * GAMBS%FREQ_GR**2/ &
     &                                (GAMBX%FREQ_GR**2 - GAMBS%FREQ_GR**2)
                      GAMBS%GION_TAU(J5) = -DOBS_TAU_XS * GAMBX%FREQ_GR**2/ &
     &                                (GAMBX%FREQ_GR**2 - GAMBS%FREQ_GR**2)
!
! ------------------- ... and at last apply it to group delay O-C
!
                      GAMBX%OCT(J5) = GAMBX%OCT(J5) - GAMBX%GION_TAU(J5)
                      GAMBS%OCT(J5) = GAMBS%OCT(J5) - GAMBS%GION_TAU(J5)
!
! ------------------- And, of course, don't forget update counter of our
! ------------------- virtues
!
                      IF ( I_AMB .NE. ICN_AMB ) THEN
                           IONO_AMB = IONO_AMB + 1
!
                           IF ( LSUP ) THEN
!
! ----------------------------- Suppress observations
!
                                GAMBX%USE(J5) = .FALSE.
                                GAMBS%USE(J5) = .FALSE.
                           END IF
                      END IF
                 END IF
            END IF
 450     CONTINUE
!
 410  CONTINUE
!
! --- II. Test: Isn't it just a situation, when the remnant from closure
! ---     of average triangles for group delay ionosphere calibration for
! ---     the S-band is a multiple of the group delay ambiguity spacing
! ---     constant for the X-band? If it is just the case then we should
! ---     redistribute additional clock jumps between baselines at the band X
! ---     in order to reduce the remnants from closure of triangles of
! ---     of average triangles for group delay ionosphere calibration for
! ---     the S-band to the value less than the group delay ambiguity spacing
! ---     constant. Permanent group delay ambiguity for each baseline at X-band
! ---     is determined and applied. Group delay ionosphere calibration for both
! ---     bands is recalculated.
!
      IF ( OBS%L_BAS .GT. 3 ) THEN
!
! -------- Clearing array of scanned stations
!
           CALL NOUT_I4 ( MG_STA, IND_STA )
           CALL NOUT_I4 ( MG_STA, JMP_BAS )
           DO 460 J6=1,OBS%L_STA
!
! ----------- Search of the first not-scanned station
!
              IF ( IND_STA(J6) .EQ. 0 ) THEN
!
! ---------------- We take this station as a fidicial.
!
                   GAV_STA(J6)=0.0D0
                   IND_STA(J6) = 1 ! Set up the flag "scanned"
!
! ---------------- And now scan all baselines
!
                   DO 470 J7=1,OBS%L_BAS
                      IF ( DABS(GAV_BAS(J7)) .LT. GAV_LIM ) GOTO 470
!
! ------------------- Find IST1, IST2 -- indices stations of J7-th baseline
! ------------------- in the list of stations
!
                      CALL NBAST ( OBS%LIS_BAS(J7), NST1, NST2 )
                      IST1 = IFIND_PL ( OBS%L_STA, OBS%LIS_STA, NST1 )
                      IST2 = IFIND_PL ( OBS%L_STA, OBS%LIS_STA, NST2 )
!
! ------------------- Test: have they been already scanned?
!
                      IF ( IND_STA(IST1) .EQ. 1  .AND. &
     &                     IND_STA(IST2) .EQ. 0        ) THEN
!
! ------------------------ The first station has been scanned but the second --
! ------------------------ has not. Determine stations shift for the second one.
!
                           GAV_STA(IST2) = GAV_BAS(J7) + GAV_STA(IST1)
                           IND_STA(IST2) = 1 ! Set up the flag "scanned"
                           JMP_BAS(J7)   = 0
                        ELSE IF ( IND_STA(IST1) .EQ. 0  .AND. &
     &                            IND_STA(IST2) .EQ. 1        ) THEN
!
! ------------------------ The second station has been scanned but the first --
! ------------------------ has not. Determine the stations shift for the
! ------------------------ unscanned station.
!
                           GAV_STA(IST1) = GAV_STA(IST2) - GAV_BAS(J7)
                           IND_STA(IST1) = 1 ! Set up the flag "scanned"
                           JMP_BAS(J7)   = 0
                        ELSE IF ( IND_STA(IST1) .EQ. 1  .AND. &
     &                            IND_STA(IST2) .EQ. 1        ) THEN
!
! ------------------------ Both stations have been already scanned. Find
! ------------------------ a difference of average group delay ionosphere
! ------------------------ calibrations on the base of scanned stations and
! ------------------------ compare it with the baseline average inosphere
! ------------------------ calibration obtained for this baseline. Entire
! ------------------------ part of the division is just the constant group
! ------------------------ delay ambiguity for all observations of the
! ------------------------ baseline which we are looking for.
!
                           CLS = (GAV_BAS(J7) - &
     &                            (GAV_STA(IST2) - GAV_STA(IST1)) )/ GIONS_SP
                           JMP_BAS(J7)=NINT( CLS )
!
! ------------------------ But we apply this correction only if a misclosure
! ------------------------ is within a tolerance range.
!
                           IF ( IABS(JMP_BAS(J7)) .EQ. 1  .AND. &
     &                          DABS( CLS - JMP_BAS(J7) ) .GT. TOL_RANGE ) THEN
                                JMP_BAS(J7)=0
                           END IF
                           GAMBX%SH_BAS(J7) = GAMBX%SH_BAS(J7) + &
     &                                        JMP_BAS(J7)*GAMBX%FREQ_GR**2/ &
     &                           ABS ( GAMBS%FREQ_GR**2 - GAMBX%FREQ_GR**2)
                           IF ( OBS%IT .GT. 3 ) THEN
                                WRITE (  6, 190 ) CBAST (OBS, OBS%LIS_BAS(J7)), &
     &                                            JMP_BAS(J7), CLS - JMP_BAS(J7)
                                WRITE ( 23, 190 ) CBAST (OBS, OBS%LIS_BAS(J7)), &
     &                                            JMP_BAS(J7), CLS - JMP_BAS(J7)
  190                           FORMAT ( 2X,'&&&  IONO_AMB:     Cor BAS ',A, &
     &                                   ': JMP=',I7,' MIS_CLS = ',F8.2 )
                           END IF
                      END IF
  470              CONTINUE
              END IF
  460      CONTINUE
!
! -------- Correct O-C
!
           DO 480 J8=1,OBS%NOBS
              IBS = IFIND_PL ( OBS%L_BAS, OBS%LIS_BAS, OBS%IBA(J8) )
              IF ( JMP_BAS(IBS) .NE. 0 ) THEN
!
! ---------------- Ambiguity is not zero? We should apply correction.
!
! ---------------- Correct X-band O-C for group delay.
!
                   GAMBX%OCT(J8) = GAMBX%OCT(J8) + JMP_BAS(IBS)*GAMBX%GAMB_SP
                   GAMBX%JMP(J8) = GAMBX%JMP(J8) + JMP_BAS(IBS)*GAMBX%GAMB_SP
                   IF ( DABS( GAMBX%GION_TAU(J8) ) .GT. GION__EPS ) THEN
!
! ------------------- Then make "raw" ionosphere contaminated O-C for
! ------------------- X- and S- band observable (without previous correction
! ------------------- due to ionosphere)
!
                      GAMBX%OCT(J8) = GAMBX%OCT(J8) + GAMBX%GION_TAU(J8)
                      GAMBS%OCT(J8) = GAMBS%OCT(J8) + GAMBS%GION_TAU(J8)
!
! ------------------- ... To produce the difference observed delays for X-band
! ------------------- minus S-band
!
                      DOBS_TAU_XS   = GAMBX%OCT(J8) - GAMBS%OCT(J8)
!
! ------------------- New recalculate group ionosphere deleay...
!
                      GAMBX%GION_TAU(J8) = -DOBS_TAU_XS * GAMBS%FREQ_GR**2/ &
     &                                   (GAMBX%FREQ_GR**2 - GAMBS%FREQ_GR**2)
                      GAMBS%GION_TAU(J8) = -DOBS_TAU_XS * GAMBX%FREQ_GR**2/ &
     &                                   (GAMBX%FREQ_GR**2 - GAMBS%FREQ_GR**2)
!
! ------------------- ... and at last apply it to group delay O-C
!
                      GAMBX%OCT(J8) = GAMBX%OCT(J8) - GAMBX%GION_TAU(J8)
                      GAMBS%OCT(J8) = GAMBS%OCT(J8) - GAMBS%GION_TAU(J8)
                   END IF
              END IF
  480      CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  IONO_AMB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PROP_AMB ( OBS, GAMB_FROM, GAMB_TO )
! ************************************************************************
! *                                                                      *
! *   Ancillary  routine PROP_AMB  transfers clock jumps from GAMB data  *
! *   structure to another. And it also updates O-C for time delay.      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       OBS  ( RECORD    ) -- Data structure which contains            *
! *                             band-independent information: time of    *
! *                             observation, baseline, lists of objects, *
! *                             status flags etc.                        *
! *  GAMB_FROM ( RECORD    ) -- Data structures for group delay          *
! *                             ambiguity resolution software, for one   *
! *                             band. Jumps in O-C for group delay       *
! *                             will be copied from this data structure. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    GAMB_TO ( RECORD    ) -- Data structures for group delay          *
! *                             ambiguity resolution software, for one   *
! *                             band. Jumps and O-C for group delay      *
! *                             will be updated for this data structure. *
! *                                                                      *
! *  ###  14-AUG-97    PROP_AMB    v1.0  (c)  L. Petrov  16-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'gamb.i'
      TYPE ( OBS__STRU ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMB_FROM, GAMB_TO
      INTEGER*4  J1
!
      DO 410 J1=1,OBS%NOBS
         GAMB_TO%JMP(J1) = GAMB_TO%JMP(J1) + GAMB_FROM%JMP(J1)
         GAMB_TO%OCT(J1) = GAMB_TO%OCT(J1) + GAMB_FROM%JMP(J1)
 410  CONTINUE
!
      RETURN
      END  !#!  PROP_AMB  #!#
