      SUBROUTINE NAMFILE_UPDATE ( IDX, IDS, CDATE, F_METCAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NAMFILE_UPDATE makes update of NAMFIL-files for            *
! *   calculation ionosphere and transferring atmosphere and other       *
! *   calibration form X-band to S-band database.                        *
! *   Routine IONA (from IONO) was taken as base for this routine.       *
! *                                                                      *
! *   Ionosphere status only for selected station will be changed.       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      IDX ( INTEGER*2 ) -- index X-band database in scratch file.     *
! *      IDS ( INTEGER*2 ) -- index S-band database in scratch file.     *
! *    CDATE ( CHARACTER ) -- Date of the lcurrent version GAMB.         *
! * F_METCAL ( LOGICAL*4 ) -- Flag. If .TRUE. then CLCT, CALI and MDAT   *
! *                           cards will be copied from  X-band to       *
! *                           S-band.                                    *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                          Input:  switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                          Output: 0 in the case of successful         *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-AUG-97  NAMFILE_UPDATE  v2.0 (c) L. Petrov  28-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'ioncm.i'
      INCLUDE   'oborg.i'
      INCLUDE   'obors.i'
      INCLUDE   'socom.i'
      INCLUDE   'glbc4.i'
!
      CHARACTER  CDATE*8
      LOGICAL*4  F_METCAL
      INTEGER*2  IDX, IDS
      INTEGER*4  IUER
!
      INTEGER*2  ICONT, NBLX, NBLS
      REAL*8     FLX ( 4, MAX_ARC_BSL ), FLS ( 4, MAX_ARC_BSL )
      CHARACTER  JBUF*70, JBUF_CLCT*70, CALI_BUF*120, NAMSIT_CHR*8, &
     &           JBUF_MDAT(0:MAX_ARC_STA)*70, &
     &           JBUF_CALS(0:MAX_ARC_STA)*70
      INTEGER*2  LDBNAM(5,15), IDBV(15)
      INTEGER*4  IDBE(15)
      INTEGER*2  I, II, K, KK, ICALAPP, ICALSTAT, IICAL, NAMBL(8), NAMSIT(4), &
     &           NMET, JERR, KERR
      INTEGER*4  IOS
      INTEGER*2  NCAL, NFCAL, IDUM1, NCON, N_CALS, IMODE, J06, J07, ISTA, J1 !, J2
      INTEGER*2  KBLX(2,MAX_ARC_BSL), KBLS(2,MAX_ARC_BSL)
      LOGICAL*2  EQUAL
      LOGICAL*4  CHECK_STABIT
!
! --- Set status of station selection bits
!
      CALL SET_STABIT ( INT2(2) )
!
! --- Extraction version numbers and other information from NAMFIL
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
!
! --- Calculate number of baslines in X-band database. Make certain the
! --- database does not have more than MAX_ARC_BSL baselines.
!
      ICONT = 1
      KERR  = 0
      NBLX  = 0
      DO WHILE ( KERR .EQ. 0 )
         CALL GETCARD ( IDX, 'REWT', ICONT, JBUF, KERR )
         ICONT = 0
         IF ( KERR .EQ. 0 ) NBLX = NBLX + 1
      END DO
!
      IF ( NBLX .GT. MAX_ARC_BSL ) THEN ! Too many X-band baselines
           CALL ERR_LOG ( 7781, IUER, 'NAMFIL_UPDATE', 'NAMFIL '// &
     &         'contains more than MAX_ARC_BSL baselines' )
           RETURN
      END IF ! too many X-band baselines
!
! --- Put IONO card in NAMFIL
!
      WRITE ( JBUF, 216 ) ( LDBNAM(I,IDS),I=1,5), IDBV(IDS), CDATE
  216 FORMAT ( "IONO", 1X, 5A2, I4, 1X, A8 )
      CALL PUTCARD ( IDX, 'IONO', INT2(1), JBUF, KERR )
      IF ( KERR .NE. 0 ) THEN
           CALL ERR_LOG ( 7782, IUER, 'NAMFIL_UPDATE', 'Error in '// &
     &         'writing IONO card in NAMFIL')
           RETURN
      END IF
!
! --- Get the X-band reweight information. (Jim Ryan 82-4-8)
!
      IF ( NBLX.GT.0 ) THEN !get per baseline information
           ICONT = 1
           DO II=1,NBLX !running over the baselines
              CALL GETCARD ( IDX, 'REWT', ICONT, JBUF, KERR )
              IF ( KERR.NE.0 ) THEN
                   CALL ERR_LOG ( 7783, IUER, 'NAMFIL_UPDATE', '!!' )
                   RETURN
              END IF
              READ ( JBUF, 220, IOSTAT=IOS ) NAMBL, ( FLX(K,II),K=1,4 )
  220         FORMAT ( 5X, 4A2, 1X, 4A2, 4F10.2 )
              IF ( IOS.NE.0 ) THEN
                   CALL ERR_LOG ( 7784, IUER, 'NAMFIL_UPDATE', &
     &                 'Error while reading NAMFIL REWT card' )
                   RETURN
              END IF
!
! ----------- Convert units back to seconds and seconds per second.
!
              FLX(1,II) = FLX(1,II)*1.D12
              FLX(2,II) = FLX(2,II)*1.D15
              FLX(3,II) = FLX(3,II)*1.D12
              FLX(4,II) = FLX(4,II)*1.D15
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!            type *,'ii=',ii,' flx(1,ii)=',flx(1,ii),' (3,ii)=',flx(1,ii) ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ----------- Get the station numbers for this baseline
!
              DO KK=1,NUMSTA ! Station check
                 IF ( EQUAL ( NAMBL(1), INT2(1), ISITN(1,KK), INT2(1), &
     &                        INT2(8) ) ) KBLX(1,II)=KK
                 IF ( EQUAL ( NAMBL(5), INT2(1), ISITN(1,KK), INT2(1), &
     &                        INT2(8) ) ) KBLX(2,II)=KK
              END DO ! Station check
           END DO ! Running over the baselines
      END IF ! Get the per baseline information
!
! --- Update ionosphere calibration status in NAMFIL for X-band.
!
      KERR  = 0
      ICONT = 1
      DO WHILE (KERR.EQ.0)
         CALL GETCARD ( IDX, 'CALS', ICONT, JBUF, KERR )
         IF ( KERR .EQ. 0 ) THEN ! Good card found
              ICONT = 0
              READ ( JBUF, 232, IOSTAT=IOS ) NAMSIT, IICAL, ICALSTAT, ICALAPP
  232         FORMAT (5X,4A2,1X,3I7)
              IF ( IOS.NE.0 ) THEN
  405              CALL ERR_LOG ( 7785, IUER, 'NAMFIL_UPDATE', &
     &                 'Error while reading NAMFIL CALS card' )
                   RETURN
              END IF
!
! ----------- Decode station name ...
!
              WRITE ( UNIT=NAMSIT_CHR, FMT='(4A2)' ) (NAMSIT(II), II=1,4)
!
! ----------- ... and find an index of the statation NAMSIT in the list of
! ----------- stations held in socom
!
              ISTA = 1 ! In order to prevent "out of range" error
              DO 406 J06=1,NUMSTA
                 IF ( NAMSIT_CHR .EQ. ISITN_CHR(J06) ) ISTA = J06
 406          CONTINUE
!
! ----------- Check: was the satition selected?
!
              IF ( CHECK_STABIT ( ISTA ) ) THEN
!
! ---------------- Set GION if the station was selected
!
                   CALL SBIT ( IICAL, INT2(1), INT2(1) )
!
! ---------------- Apply GION if the station was selected
!
                   CALL SBIT ( IICAL, INT2(4), INT2(1) )
                   CALL SBIT ( IICAL, INT2(5), INT2(0) )
              END IF
!
              WRITE ( JBUF, 1232 ) NAMSIT, IICAL, ICALSTAT, ICALAPP
 1232         FORMAT ( "CALS ", 4A2, 1X, 3I7 )
              CALL PUTCARD ( IDX, 'CALS', INT2(4), JBUF, JERR )
              IF ( JERR .NE. 0 ) THEN
                   CALL ERR_LOG ( 7786, IUER, 'NAMFIL_UPDATE', &
     &                 'Error while writing NAMFIL CALS card' )
                   RETURN
              END IF
         END IF ! Good card found
      END DO
!
! --- Make certain the S-band data base does not have more than MAX_ARC_BSL
! --- baselines.
!
      ICONT = 1
      KERR  = 0
      NBLS  = 0
      DO WHILE ( KERR .EQ. 0 )
         CALL GETCARD ( IDS, 'REWT', ICONT, JBUF, KERR )
         ICONT = 0
         IF ( KERR.EQ.0 ) NBLS = NBLS + 1
      END DO
!
      IF ( NBLS .GT. MAX_ARC_BSL ) THEN ! Too many S-band baselines
           CALL ERR_LOG ( 7787, IUER, 'NAMFIL_UPDATE', &
     &         'NAMFIL contains more than MAX_ARC_BSL baselines.' )
           RETURN
      END IF ! Too many S-band baselines
!
! --- Update for ion-calibrated S-band
!
      WRITE ( JBUF, 216 ) ( LDBNAM(I,IDX),I=1,5 ), IDBV(IDX), CDATE
      CALL PUTCARD ( IDS, 'IONO', INT2(1), JBUF, KERR )
      IF ( KERR .NE. 0 ) THEN
           CALL ERR_LOG ( 7788, IUER, 'NAMFIL_UPDATE', &
     &         'NAMFIL contains more than MAX_ARC_BSL baselines.' )
           RETURN
      ENDIF
!
! --- Get the S-band reweight information.
!
      IF ( NBLS.GT.0 ) THEN ! Get per baseline information
           ICONT = 1
           DO II=1,NBLS ! Running over the baselines
              CALL GETCARD ( IDS, 'REWT', ICONT, JBUF, KERR )
              IF ( KERR .NE. 0 ) THEN
                   CALL ERR_LOG ( 7789, IUER, 'NAMFIL_UPDATE', &
     &                 'Error while reading REWT card from NAMFIL for '// &
     &                 'for S-band' )
                   RETURN
              END IF
!
              READ ( JBUF, 220, IOSTAT=IOS ) NAMBL,(FLS(K,II),K=1,4)
!
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 7790, IUER, 'NAMFIL_UPDATE', &
     &                 'Error while reading REWT card from NAMFIL for S-band' )
                   RETURN
              END IF
!
! ----------- Convert units back to seconds and seconds per second.
!
              FLS(1,II) = FLS(1,II)*1.D12
              FLS(2,II) = FLS(2,II)*1.D15
              FLS(3,II) = FLS(3,II)*1.D12
              FLS(4,II) = FLS(4,II)*1.D15
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!            type *,'ii=',ii,' fls(1,ii)=',fls(1,ii),' (3,ii)=',fls(1,ii) ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ----------- Get the station numbers for this baseline
!
              DO KK=1,NUMSTA ! Station check
                 IF ( EQUAL ( NAMBL(1), INT2(1), ISITN(1,KK), &
     &                        INT2(1), INT2(8) ) ) KBLS(1,II)=KK
                 IF ( EQUAL ( NAMBL(5), INT2(1), ISITN(1,KK), &
     &                        INT2(1), INT2(8) ) ) KBLS(2,II)=KK
              END DO         ! Station check
           END DO ! Running over the baselines
      END IF ! Get the per baseline information
!
! --- Update ionosphere calibration status in NAMFIL for S-band.
!
      KERR  = 0
      ICONT = 1
      DO WHILE ( KERR .EQ. 0 )
         CALL GETCARD ( IDS, 'CALS', ICONT, JBUF, KERR )
         IF ( KERR.EQ.0 ) THEN !Good card found
              ICONT = 0
              READ ( JBUF, 232) NAMSIT, IICAL, ICALSTAT, ICALAPP
!
! ----------- Decode station name ...
!
              WRITE ( UNIT=NAMSIT_CHR, FMT='(4A2)' ) (NAMSIT(II), II=1,4)
!
! ----------- ... and find an index of the statation NAMSIT in the list of
! ----------- stations held in socom
!
              ISTA = 1 ! In order to prevent "out of range" error
              DO 407 J07=1,NUMSTA
                 IF ( NAMSIT_CHR .EQ. ISITN_CHR(J07) ) ISTA = J07
 407          CONTINUE
!
! ----------- Check: was the satition selected?
!
              IF ( CHECK_STABIT ( ISTA ) ) THEN
!
! ---------------- Set GION
!
                   CALL SBIT ( IICAL, INT2(1), INT2(1) )
!
! ---------------- Apply GION
!
                   CALL SBIT ( IICAL, INT2(4), INT2(1) )
                   CALL SBIT ( IICAL, INT2(5), INT2(0) )
              END IF
!
              WRITE ( JBUF, 1232 ) NAMSIT, IICAL, ICALSTAT, ICALAPP
              CALL PUTCARD ( IDS, 'CALS', INT2(4), JBUF, JERR )
!
              IF ( JERR .NE. 0 ) THEN
                   CALL ERR_LOG ( 7791, IUER, 'NAMFIL_UPDATE', &
     &                 'Error while writing CALS card from NAMFIL for S-band' )
                   RETURN
              END IF
         END IF ! Good card found
      END DO
!
      IF ( F_METCAL ) THEN
!
! -------- Transferring calibration data. Firstly read some NAMFIL cards
!
           CALL GETCARD ( IDX, 'CLCT', INT2(1), JBUF_CLCT, JERR )
           IF ( JERR .NE. INT2(0) ) THEN
                CALL ERR_LOG ( 7792, IUER, 'NAMFIL_UPDATE', &
     &              'Error reading CLCT card from NAMFIL for X-band' )
                RETURN
           END IF
!
           READ ( JBUF_CLCT, "(4X,4(1X,I3),50X)", IOSTAT=IOS ) NCAL, NFCAL, &
     &                                                         IDUM1, NCON
           IF ( IOS .NE. INT2(0) ) THEN
                CALL ERR_LOG ( 7793, IUER, 'NAMFIL_UPDATE', &
     &              'Error reading CLCT card from NAMFIL for X-band' )
                RETURN
           END IF
!
           CALL GET_CLN_CARD ( IDX, 'CALI', NCAL, CALI_BUF, JERR )
           IF ( JERR .NE. INT2(0) ) THEN
                CALL ERR_LOG ( 7794, IUER, 'NAMFIL_UPDATE', &
     &              'Error reading CLCT card from NAMFIL for X-band' )
                RETURN
           END IF
!
! -------- Then read CALS cards in array JBUF_CALS
!
           IMODE=1
           DO 410 J1=1,MAX_ARC_STA
              CALL GETCARD ( IDX, 'CALS', IMODE, JBUF_CALS(J1), JERR )
              IF ( JERR .EQ. 1 ) THEN
                   N_CALS = J1-1
                   GOTO 810
                ELSE IF ( JERR .NE. 0 ) THEN
                   WRITE ( 6, * ) ' JERR = ',JERR,' J1=',J1
                   CALL ERR_LOG ( 7795, IUER, 'NAMFIL_UPDATE', &
     &                 'Error reading CALS card from NAMFIL for X-band' )
                   RETURN
              END IF
              IMODE =0
 410       CONTINUE
           N_CALS = MAX_ARC_STA
!
! -------- Now N_CALS contains the number of CALS cards
!
 810       CONTINUE
!
! -------- This segment borrowed from cres/secnd.f
!
! -------- Reading all MDAT cards in array JBUF_MDAT
!
           CALL GETCARD ( IDX, 'MDAT', INT2(1), JBUF_MDAT(0), JERR )
           IF ( JERR .NE. 0 ) THEN
                CALL ERR_LOG ( 7796, IUER, 'NAMFIL_UPDATE', &
     &              'Error reading MDAT card from NAMFIL for X-band' )
                RETURN
           END IF
           READ ( JBUF_MDAT(0), '(5X,I3)', IOSTAT=IOS ) NMET
           IF ( IOS .NE. 0 ) THEN
                CALL ERR_LOG ( 7797, IUER, 'NAMFIL_UPDATE', &
     &              'Error reading MDAT card from NAMFIL for X-band' )
                RETURN
           END IF
!
           DO I=1,NMET
              CALL GETCARD ( IDX, 'MDAT', INT2(0), JBUF_MDAT(I), JERR )
              IF ( JERR .NE. 0 ) THEN
                   WRITE ( 6, * ) ' I=',I,' NMET=',NMET,' JERR = ',JERR
                   CALL ERR_LOG ( 7798, IUER, 'NAMFIL_UPDATE', &
     &                 'Error reading MDAT card from NAMFIL for X-band' )
                   RETURN
              END IF
           END DO
!
! -------- Then writing
! -------- ~~~~~~~~~~~~
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                          !!
!  Writing calibration data remained unresolved problem to 19-AUG-97.      !!
!  Changing calibration status spoils solution.                            !!
!                                                                          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!           CALL PUTCARD      ( IDS, 'CLCT', 1, JBUF_CLCT, JERR )
!           IF ( JERR .NE. 0 ) THEN
!               CALL ERR_LOG ( INT4(7798), IUER, 'NAMFIL_UPDATE',
!     #              'Error writing CLCT card to NAMFIL for S-band' )
!                RETURN
!           END IF
!C
!           CALL PUT_CLN_CARD ( IDS, 'CALI', 1, NCAL, CALI_BUF, JERR)
!           IF ( JERR .NE. 0 ) THEN
!               CALL ERR_LOG ( INT4(7799), IUER, 'NAMFIL_UPDATE',
!     #              'Error writing CLCI card to NAMFIL for S-band' )
!                RETURN
!           END IF
!C
!C -------- Writing CALS cards
!C
!           IMODE = 1
!           DO 420 J2=1,N_CALS
!              CALL PUTCARD ( IDS, 'CALS', IMODE, JBUF_CALS(J2), JERR )
!              IF ( JERR .NE. 0 ) THEN
!                  CALL ERR_LOG ( INT4(7800), IUER, 'NAMFIL_UPDATE',
!     #                 'Error writing CALS card to NAMFIL for S-band' )
!                   RETURN
!              END IF
!              IMODE = 0
! 420       CONTINUE
!
! -------- Then writing all MDAT cards in array JBUF_MDAT
!
           CALL PUTCARD ( IDS, 'MDAT', INT2(1), JBUF_MDAT(0), JERR )
           IF ( JERR .NE. 0 ) THEN
                CALL ERR_LOG ( 7801, IUER, 'NAMFIL_UPDATE', &
     &              'Error writing MDAT card to NAMFIL for S-band' )
                RETURN
           END IF
!
           DO I=1,NMET
              CALL PUTCARD ( IDS, 'MDAT', INT2(0), JBUF_MDAT(I), JERR )
              IF ( JERR .NE. 0 ) THEN
!                   TYPE *,' I=',I,' NMET=',NMET,' JERR = ',JERR
!                  CALL ERR_LOG ( 7802, IUER, 'NAMFIL_UPDATE',
!     #                 'Error writing MDAT card to NAMFIL for S-band' )
!                   RETURN
              END IF
           ENDDO
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  NAMFILE_UPDATE  #!#
