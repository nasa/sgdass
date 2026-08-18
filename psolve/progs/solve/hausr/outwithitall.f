      SUBROUTINE   OUTWITHITALL()
      IMPLICIT     NONE
!
! 1.  OUTWITHITALL PROGRAM SPECIFICATION
!
! 1.1 Writes baseline statistics, sort baselines, and check to see if both
!     stations are globl or not.
!
! 1.2 REFERENCES:
!
! 2.  OUTWITHITALL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
      INCLUDE 'hausr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: hausr
!       CALLED SUBROUTINES: hsort_key, glbl_or_lcl
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4     NRECORDS, IDSIG, IRSIG
      REAL*8        DSIG_SUM_PRI, RSIG_SUM_PRI, WEIGHTED_EPOCH, WEIGHT_SUM, &
     &              SOU_WEI_EPO
      INTEGER*4     OBSU_SUM(3), NSCA_TOT_SUM, NSCA_USED_SUM
!
!   type & dimension:  local
!
      REAL*8       CHISQR_CUM(3), DSIG_CUM, RSIG_CUM, WRMS_SUM(2), FACT_SUM(2)
      INTEGER*2    I, J, NTMP(22), BASELINE_NAME(8), IBUF(30), SOURCE_NAME(4)
      INTEGER*4    K
      CHARACTER    KIND*5, JNAME_STR*10, BNAME_STR*8, STR*128
      EQUIVALENCE  ( NTMP(1),  OBSU_SUM(1) ), &
     &             ( NTMP(7),  WRMS_SUM(1) ), &
     &             ( NTMP(15), FACT_SUM(1) )
      LOGICAL*4,   EXTERNAL :: IS_R8_NAN, IS_R4_NAN
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4. HISTORY
!
!  WHO  WHEN         WHAT
!  pet  03-APR-99    Replaced call hsort     with  hsort_key
!  pet  30-NOV-2004  Replaced call hsort_key with  sort_ch
!  pet  2006.05.02   Fixed a bug: the old version crashed when residuals 
!                    were too big
!  pet  2010.11.06   Changed the wording in the listing from local/globl &
!                    to LclBas, GlbBas, LocSou, GloSou in order to make &
!                    listing grepable
!  pet  2021.06.01   Replaced call sort_ch with SORT_FAST_CH
!  pet  2024.07.09   Added suppport of scan statistics and weighted mean epoch
!  pet  2024.07.09   Added support for computation of scan statistics and weighted epoch
!                    for a given source
!
! 5. OUTWITHITALL PROGRAM STRUCTURE
!
!   header for baseline statistics
!
      IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
           WRITE ( 23, 5601 )
 5601      FORMAT(/"1Baseline Statistics Summary "/ &
     &             "      Baseline        # W.Obs      W.RMS Del  Ch/ndf Del    W.RMS Rate Ch/ndf Rat   Status"/ &
     &             "                     used   total     ps                        fs/s   " &
     &             /)
        ELSE IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
           WRITE ( 23, 5602 )
 5602      FORMAT(/"1Baseline Statistics Summary "/ &
     &             "      Baseline        # W.Obs      W.RMS Del  Ch/ndf Del    W.RMS Rate Ch/ndf Rat   Status"/ &
     &             "                     used   total     ps                        fs/s   " &
     &             /)
         ELSE 
           WRITE ( 23, 5603 )
 5603      FORMAT("1Baseline Statistics Summary ",/, &
     &            "      Baseline        # W.Obs    W.RMS Del   N.R.D.  W.RMS Rate", &
     &            "   N.R.R. Status",/, &
     &            "                     used/total     ps                   fs/s   " &
     &            /)
      END IF
!
! --- Print out baseline statistics sort the baselines
!
      IF ( IBNDX .GT. 1 ) THEN
           CALL SORT_FAST_CH ( INT4(IBNDX), BASELINES_CHR )
      END IF
!
! --- Flip through the baselines
!
      DO I = 1, IBNDX
         DO J = 1, 8
            BASELINE_NAME(J) = BASELINES(J,I)
         END DO
         DO J = 1, 22
            NTMP(J) = BASELINES(8+J,I)
         END DO
!
! ------ Test to determine if BOTH stations in this baseline are global
!
         Call GLBL_OR_LCL ( INT2(1), BASELINE_NAME, KIND )
!
         IF ( DABS(FACT_SUM(1) ) .GE. 1.D-38 ) THEN
              DSIG_CUM = DSQRT( WRMS_SUM(1)/FACT_SUM(1) )
            ELSE
              DSIG_CUM = 0.D0
         ENDIF
         IF ( DABS(FACT_SUM(2) ) .GE. 1.D-38 ) THEN
              RSIG_CUM = DSQRT( WRMS_SUM(2)/FACT_SUM(2) )
            ELSE
              RSIG_CUM = 0.D0
         ENDIF
!
         IF ( OBSU_SUM(1) .EQ. 0.0D0 ) THEN
              CHISQR_CUM(1)=0.0D0
            ELSE
              CHISQR_CUM(1) = DSQRT( WRMS_SUM(1)/IABS(OBSU_SUM(1)) )
         ENDIF
!
         IF ( OBSU_SUM(3) .EQ. 0 ) THEN
              CHISQR_CUM(2)=0.0D0
           ELSE
              CHISQR_CUM(2) = DSQRT( WRMS_SUM(2)/IABS(OBSU_SUM(3)) )
         ENDIF
!
         IF ( DABS(DSIG_CUM) < 2.D9 ) THEN
              IDSIG = IDNINT(DSIG_CUM)
              DSIG_SUM_PRI = DSIG_CUM
            ELSE IF ( DSIG_CUM < -2.D-9 ) THEN
              IDSIG = -2000000000
              DSIG_SUM_PRI = -99999.999
            ELSE IF ( DSIG_CUM >  2.D-9 ) THEN
              IDSIG =  2000000000
              DSIG_SUM_PRI = 999999.999
         END IF
!
         IF ( DABS(RSIG_CUM) < 2.D9 ) THEN
              IRSIG = IDNINT(RSIG_CUM)
              RSIG_SUM_PRI = RSIG_CUM
            ELSE IF ( RSIG_CUM < -2.D-9 ) THEN
              IRSIG = -2000000000
              RSIG_SUM_PRI = -99999.999
            ELSE IF ( RSIG_CUM >  2.D-9 ) THEN
              IRSIG =  2000000000
              RSIG_SUM_PRI = 999999.999
         END IF
!
         IF ( KIND == 'local' ) THEN
              STR = 'LocBas'
            ELSE
              STR = 'GloBas'
         END IF
         IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_COV .EQ. F__SEG ) THEN
              IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
                   WRITE (23,5502) ( BASELINE_NAME(J), J=1,8), &
     &                               OBSU_SUM(1), OBSU_SUM(2), DSIG_SUM_PRI, &
     &                               CHISQR_CUM(1), RSIG_SUM_PRI, CHISQR_CUM(2), STR(1:6)
 5502             FORMAT ( 1X, 4A2, 1X, 4A2, I7, 1X ,I7, 1X, F10.3, 5X, F7.3, 4X, &
     &                     F10.3, 4X, F7.3, 3X, A6 )
                 ELSE
                   WRITE (23,5503) ( BASELINE_NAME(J), J=1,8), &
     &                               OBSU_SUM(1), OBSU_SUM(2), IDSIG, &
     &                               CHISQR_CUM(1), IRSIG, CHISQR_CUM(2), STR(1:6)
 5503              FORMAT ( 1X,4A2,"-",4A2,I7,"/",I6,1X,I5,5X,F7.3,4X, &
     &                 I6,4X,F7.3,3X,A6 )
              END IF
            ELSE
              WRITE (23,5513) (BASELINE_NAME(J), J=1,8), &
     &                         OBSU_SUM(1), OBSU_SUM(2), IDNINT(DSIG_CUM), &
     &                         IDNINT(RSIG_CUM), STR(1:6)
 5513         FORMAT ( 1X,4A2,"-",4A2,I7,"/",I6,1X,I5,5X,'  N/A  ',4X, &
     &                 I6, 4X, '  N/A  ', 3X, A6 )
         END IF
      END DO
!
! --- Print out source statistics
! --- header for source statistics
!
      IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
           WRITE ( 23, 5621 )
 5621      FORMAT ( /"1Source Statistics Summary "/ &
     &               "     B-name    J-name        Number of obs  W.RMS Del Chi/ndf Del     ", &
     &               "W.RMS Rat Chi/ndf Rat  Status"/ &
     &               "                              used   total     ps                        fs/s"/ )
         ELSE IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
           WRITE ( 23, 5622 )
 5622      FORMAT ( /"1Source Statistics Summary "/ &
     &               "     B-name    J-name        Number of obs      W.RMS     Chi/ndf Number of scans" &
     &               " Mid Epoch  Status"/ &
     &               "                              used   total         ps                used   total" )
         ELSE 
            WRITE ( 23, 5623 )
 5623       FORMAT("1Source Statistics Summary ",/, &
     &             "       Source         # W.Obs    W.RMS Del   N.R.D.  W.RMS Rate", &
     &             "   N.R.R. Status",/, &
     &             "                     used/total     ps                   fs/s   " / )
      END IF
!
! --- Sort the source list
!
      IF ( ISNDX .GT. 1 ) THEN
           CALL SORT_FAST_CH ( ISNDX, SOURCES_CHR )
      END IF
!
      DO K = 1, ISNDX
         DO J = 1, 4
            SOURCE_NAME(J) = SOURCES(J, K)
         END DO
         DO J = 1, 22
            NTMP(J) = SOURCES(4+J, K)
         END DO
         CALL MEMCPY ( BNAME_STR, SOURCE_NAME )
         CALL MEMCPY ( JNAME_STR, SOURCES(27,K) )
         CALL MEMCPY ( NSCA_TOT_SUM,   SOURCES(36,K), %VAL(4) )
         CALL MEMCPY ( NSCA_USED_SUM,  SOURCES(38,K), %VAL(4) )
         CALL MEMCPY ( WEIGHTED_EPOCH, SOURCES(40,K), %VAL(8) )
         CALL MEMCPY ( WEIGHT_SUM,     SOURCES(44,K), %VAL(8) )
         IF ( WEIGHT_SUM > 1.0D0 ) THEN
              SOU_WEI_EPO = WEIGHTED_EPOCH/WEIGHT_SUM
            ELSE 
              SOU_WEI_EPO = 0.0D0
         END IF
!
         CALL GLBL_OR_LCL ( INT2(0), SOURCE_NAME, KIND )
!
         IF ( IS_R8_NAN(FACT_SUM(1)) ) FACT_SUM(1) = 1.0D-40
         IF ( IS_R8_NAN(FACT_SUM(2)) ) FACT_SUM(2) = 1.0D-40
!
         IF ( DABS(FACT_SUM(1)) .GE. 1.0D-38 ) THEN
              DSIG_CUM = DSQRT ( WRMS_SUM(1)/FACT_SUM(1) )
            ELSE
              DSIG_CUM = 0.D0
         ENDIF
         IF ( DABS(FACT_SUM(2)) .GE. 1.0D-38 ) THEN
              RSIG_CUM = DSQRT( WRMS_SUM(2)/FACT_SUM(2) )
            ELSE
              RSIG_CUM = 0.D0
         ENDIF
!
         IF ( OBSU_SUM(1) .EQ. 0.0D0 ) THEN
              CHISQR_CUM(1)=0.0D0
            ELSE
              CHISQR_CUM(1) = DSQRT( WRMS_SUM(1)/IABS(OBSU_SUM(1)) )
         ENDIF
         IF ( OBSU_SUM(3) .EQ. 0.0D0 ) THEN
              CHISQR_CUM(2)=0.0D0
            ELSE
              CHISQR_CUM(2) = DSQRT( WRMS_SUM(2)/IABS(OBSU_SUM(3)) )
         ENDIF
!
         IF ( IS_R8_NAN(DSIG_CUM) ) DSIG_CUM = 4.0D9
         IF ( IS_R8_NAN(RSIG_CUM) ) RSIG_CUM = 4.0D9
!
         IF ( DABS(DSIG_CUM) < 2.D9 ) THEN
              IDSIG = IDNINT(DSIG_CUM)
              DSIG_SUM_PRI = DSIG_CUM
            ELSE IF ( DSIG_CUM < -2.D-9 ) THEN
              IDSIG = -2000000000
              DSIG_SUM_PRI = -99999.9
            ELSE IF ( DSIG_CUM >  2.D-9 ) THEN
              IDSIG =  2000000000
              DSIG_SUM_PRI = 999999.9
         END IF
!
         IF ( DABS(RSIG_CUM) < 2.D9 ) THEN
              IRSIG = IDNINT(RSIG_CUM)
              RSIG_SUM_PRI = RSIG_CUM
            ELSE IF ( RSIG_CUM < -2.D-9 ) THEN
              IRSIG = -2000000000
              RSIG_SUM_PRI = -99999.9
            ELSE IF ( RSIG_CUM >  2.D-9 ) THEN
              IRSIG =  2000000000
              RSIG_SUM_PRI = 999999.9
         END IF
!
         IF ( KIND == 'local' ) THEN
              STR = 'LocSou'
            ELSE
              STR = 'GloSou'
         END IF
         IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
              WRITE ( 23, 5541 ) ( SOURCE_NAME(J), J=1,4 ), JNAME_STR, &
     &                OBSU_SUM(1), OBSU_SUM(2), DSIG_SUM_PRI, &
     &                CHISQR_CUM(1), RSIG_SUM_PRI, CHISQR_CUM(2), STR(1:6)
 5541         FORMAT ( 5X, 4A2, 2X, A10, 2X, I7, 1X, I7, 1X, F10.3, 5X, F7.3, 4X, &
     &                 F10.3, 4X, F7.3, 3X, A6 )
           ELSE IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
              WRITE ( 23, 5542 ) ( SOURCE_NAME(J), J=1,4 ), JNAME_STR, &
     &                OBSU_SUM(1), OBSU_SUM(2), DSIG_SUM_PRI, &
     &                CHISQR_CUM(1), NSCA_USED_SUM, NSCA_TOT_SUM, SOU_WEI_EPO, STR(1:6)
 5542         FORMAT ( 5X, 4A2, 2X, A10, 2X, I7, 1X, I7, 1X, F10.3, 5X, F7.3, 2X, &
     &                 I6, 2X, I6, 2X, F8.3, 2X, A6 )
           ELSE
              WRITE ( 23, 5543 ) ( SOURCE_NAME(J), J=1,4 ), &
     &                OBSU_SUM(1), OBSU_SUM(2), IDSIG, &
     &                CHISQR_CUM(1), IRSIG , CHISQR_CUM(2), STR(1:6)
 5543         FORMAT ( 5X, 4A2, 5X, I7, "/", I6, 1X, I5, 5X, F7.3, 4X, &
     &                 I6, 4X, F7.3, 3X, A6 )
         END IF
      END DO
!
      RETURN
      END  !#!  OUTWITHITALL  #!#
