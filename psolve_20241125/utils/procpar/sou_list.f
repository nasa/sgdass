      PROGRAM    SOU_LIST_MAIN
! ************************************************************************
! *                                                                      *
! *  Program SOU_LIST_MAIN
! *                                                                      *
! *  ### 20-JUL-1998  SOU_LIST_MAIN v3.2 (c) L. Petrov  10-APR-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FILSPL*128, FILGOOD*128, FILBAD*128, FILNON*128, FILALL*128, &
     &           MODE_STR*16
      INTEGER*4  MN_SES, MN_OBSTOT, MN_OBSSES, MN_OBS, NUMARG
      REAL*8     MN_YEARS
!
      CALL CLRCH ( FILSPL  )
      CALL CLRCH ( FILGOOD )
      CALL CLRCH ( FILBAD  )
      CALL CLRCH ( FILNON  )
      CALL CLRCH ( FILALL  )
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: sou_list splool_file nosou|heo|proper'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILSPL   )
           CALL GETARG ( 2, MODE_STR )
      END IF
!
      IF ( MODE_STR == 'nosou' ) THEN
!
! -------- Choice for selection of no-sources
!
           MN_SES    = 1
           MN_OBSSES = 1
           MN_OBSTOT = 3
           MN_OBS    = 3
           MN_YEARS  = -1.0
        ELSE IF ( MODE_STR == 'heo' ) THEN
!
! -------- Choice for HEO
!
           MN_SES    = 2
           MN_OBSSES = 20
           MN_OBSTOT = 1048576
           MN_OBS    = 40
           MN_YEARS  = 3.0D0
        ELSE IF ( MODE_STR == 'proper' ) THEN
!
! -------- Choice for proper motion
!
           MN_SES    = 8
           MN_OBSSES = 8
           MN_OBSTOT = 1048576
           MN_OBS    = 128
           MN_YEARS  = 4.0D0
        ELSE
           WRITE ( 6, * ) 'Unsupported mode '//TRIM(MODE_STR)
           CALL EXIT ( 1 )
      END IF
!
      FILGOOD = '/tmp/list_good.sou'
      FILBAD  = '/tmp/list_bad.sou'
      FILNON  = '/tmp/list_non.sou'
      FILALL  = '/tmp/list_all.sou'
!
      CALL       SOU_LIST ( MN_SES, MN_OBSSES, MN_OBSTOT, MN_OBS, MN_YEARS, &
     &                      FILSPL, FILGOOD, FILBAD, FILNON, FILALL )
      END   !#!  SOU_LIST_MAIN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SOU_LIST ( MN_SES, MN_OBSSES, MN_OBSTOT, MN_OBS, MN_YEARS, &
     &                      FILSPL, FILGOOD, FILBAD, FILNON, FILALL )
! ************************************************************************
! *                                                                      *
! *   Routine SOU_LIST  reads the spool file of the global solution and  *
! *   creates three files with source lists in BATOPT format:            *
! *   1) FILGOOD with list of sources which passed the test of good      *
! *      sources:                                                        *
! *      if ( total number of session   >= MN_SES .AND.                  *
! *           total number of good obs. >= MN_OBS ) the sourece is good. *
! *      if ( total number of session <MN_SES .AND.                      *
! *           total number of good obs>= MN_OBSTOT ) the source is good. *
! *      If source had less than MN_OBSSES good observation in the       *
! *      particular session we don't count these observations.           *
! *   2) FILBAD  with list of sources which have less good observations  *
! *      than the limit, but more than 1.                                *
! *   3) FILNON  with list of sources which have no good observations    *
! *      at all.                                                         *
! *                                                                      *
! *  ###  22-JUL-98    SOU_LIST   v3.1 (c)  L. Petrov  20-JUN-2005  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*4  MN_SES, MN_OBSTOT, MN_OBSSES, MN_OBS
      REAL*8     MN_YEARS
      INTEGER*4  M_SOU, NK
      PARAMETER  ( M_SOU = 64*1024 ) ! Max number of sources
      PARAMETER  (    NK = 8     ) ! Number of columns in the outout
      CHARACTER  FILSPL*(*), FILGOOD*(*), FILBAD*(*), FILNON*(*), FILALL*(*)
      CHARACTER  STR*200, FILTMP*80
      CHARACTER  SOU1(M_SOU)*8, SOU2(M_SOU)*8, SOU3(M_SOU)*8, C_SOU(M_SOU)*8
      CHARACTER  CDAT_BEG(M_SOU)*10,  CDAT_END(M_SOU)*10
      LOGICAL*4  FL_SOU
      INTEGER*4  NOBS_SOU(M_SOU), NSES_SOU(M_SOU), L_SOU, NS_OBS, &
     &           N1, N2, N3, MJD_BEG, MJD_END, J1, J2, IP
      REAL*8     TAI_BEG, TAI_END, SPAN_YEAR
      INTEGER*4  SRC_LISTING_STYLE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      OPEN ( UNIT=11, FILE=FILSPL,  STATUS='OLD' )
      OPEN ( UNIT=22, FILE=FILGOOD, STATUS='UNKNOWN' )
      OPEN ( UNIT=33, FILE=FILBAD,  STATUS='UNKNOWN' )
      OPEN ( UNIT=44, FILE=FILNON,  STATUS='UNKNOWN' )
      OPEN ( UNIT=55, FILE=FILALL,  STATUS='UNKNOWN' )
      WRITE ( 55, '(A)' ) '# Source statistics from '//FILSPL(1:I_LEN(FILSPL))
!
! --- Temporary file
!
      FILTMP = '/tmp/0.0'
      CALL SYSTEM ( 'rm -f '//FILTMP )
!
      L_SOU = 0
      FL_SOU = .FALSE.
      CDAT_BEG = '          '
      CDAT_END = '          '
!
      SRC_LISTING_STYLE = SRC_LONG_SPOOL__FMT
      DO 410 J1=1,1024*1024*1024
         READ ( UNIT=11, FMT='(A)', END=810 ) STR
         IF ( MOD(J1,10000) .EQ. 0 ) THEN
              WRITE ( 6, FMT='("    line ",I7,"   ",A$)' ) J1, CHAR(13)
              CALL FLUSH ( 6 )
         END IF
         IF ( STR(1:17) .EQ. ' Listing_Options:' ) THEN
              IF ( INDEX ( STR, 'SRC_STAT LONG'  ) > 0 ) THEN
                   SRC_LISTING_STYLE = SRC_LONG_SPOOL__FMT
              END IF
              IF ( INDEX ( STR, 'SRC_STAT SHORT' ) > 0 ) THEN
                   SRC_LISTING_STYLE = SRC_SHORT_SPOOL__FMT
              END IF
              IF ( INDEX ( STR, 'SRC_STAT POST2021' ) > 0 ) THEN
                   SRC_LISTING_STYLE = SRC_POST2021_SPOOL__FMT
              END IF
              IF ( INDEX ( STR, 'SRC_STAT POST2024' ) > 0 ) THEN
                   SRC_LISTING_STYLE = SRC_POST2024_SPOOL__FMT
              END IF
         END IF
!
         IF ( STR(1:18) .EQ. ' Source Statistics' ) THEN
              FL_SOU = .TRUE.
         END IF
         IF ( STR(1:1) .NE. ' ' .AND. &
     &        STR(1:9) .NE. 'SRC_STAT:' ) FL_SOU = .FALSE.
         NS_OBS = 0
         IF ( FL_SOU ) THEN
              IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
                   IF ( STR(1:9) .EQ. 'SRC_STAT:' ) THEN
                        READ ( UNIT=STR(34:38), FMT='(I5)' ) NS_OBS
                        IF ( L_SOU .EQ. 0 ) THEN
                             L_SOU = L_SOU + 1
                             C_SOU(L_SOU) = STR(12:19)
                             NSES_SOU(L_SOU) = 0
                             NOBS_SOU(L_SOU) = 0
                             IP = L_SOU
                           ELSE
                             IP = LTM_DIF ( 1, L_SOU, C_SOU, STR(12:19) )
                             IF ( IP .LE. 0 ) THEN
                                  L_SOU = L_SOU + 1
                                  C_SOU(L_SOU) = STR(12:19)
                                  NSES_SOU(L_SOU) = 0
                                  NOBS_SOU(L_SOU) = 0
                                  IP = L_SOU
                             END IF
                        END IF
                        IF ( NS_OBS .GE. MN_OBSSES ) THEN
                             NSES_SOU(IP) = NSES_SOU(IP) + 1
                             NOBS_SOU(IP) = NOBS_SOU(IP) + NS_OBS
                             IF ( ILEN(CDAT_BEG(IP)) == 0 ) THEN
                                  CDAT_BEG(IP) = STR(87:96)
                                  CDAT_END(IP) = STR(87:96)
                                ELSE 
                                  CDAT_BEG(IP) = MIN ( CDAT_BEG(IP), STR(87:96) )
                                  CDAT_END(IP) = MAX ( CDAT_END(IP), STR(87:96) )
                             END IF
                        END IF
                   END IF
                 ELSEIF ( SRC_LISTING_STYLE == SRC_PRE2004_SPOOL__FMT ) THEN
                   IF ( STR(23:23) .EQ. '/' ) THEN
                        READ ( UNIT=STR(18:22), FMT='(I5)' ) NS_OBS
                        IF ( L_SOU .EQ. 0 ) THEN
                             L_SOU = L_SOU + 1
                             C_SOU(L_SOU) = STR(6:13)
                             NSES_SOU(L_SOU) = 0
                             NOBS_SOU(L_SOU) = 0
                             IP = L_SOU
                           ELSE
                             IP = LTM_DIF ( 1, L_SOU, C_SOU, STR(6:13) )
                             IF ( IP .LE. 0 ) THEN
                                  L_SOU = L_SOU + 1
                                  C_SOU(L_SOU) = STR(6:13)
                                  NSES_SOU(L_SOU) = 0
                                  NOBS_SOU(L_SOU) = 0
                                  IP = L_SOU
                             END IF
                        END IF
                        IF ( NS_OBS .GE. MN_OBSSES ) THEN
                             NSES_SOU(IP) = NSES_SOU(IP) + 1
                             NOBS_SOU(IP) = NOBS_SOU(IP) + NS_OBS
                        END IF
                   END IF
                 ELSE IF ( SRC_LISTING_STYLE == SRC_SHORT_SPOOL__FMT .OR. &
     &                     SRC_LISTING_STYLE == SRC_LONG_SPOOL__FMT     ) THEN
                   IF ( STR(1:9) .EQ. 'SRC_STAT:' ) THEN
                        READ ( UNIT=STR(22:25), FMT='(I4)' ) NS_OBS
                        IF ( L_SOU .EQ. 0 ) THEN
                             L_SOU = L_SOU + 1
                             C_SOU(L_SOU) = STR(12:19)
                             NSES_SOU(L_SOU) = 0
                             NOBS_SOU(L_SOU) = 0
                             IP = L_SOU
                           ELSE
                             IP = LTM_DIF ( 1, L_SOU, C_SOU, STR(12:19) )
                             IF ( IP .LE. 0 ) THEN
                                  L_SOU = L_SOU + 1
                                  C_SOU(L_SOU) = STR(12:19)
                                  NSES_SOU(L_SOU) = 0
                                  NOBS_SOU(L_SOU) = 0
                                  IP = L_SOU
                             END IF
                        END IF
                        IF ( NS_OBS .GE. MN_OBSSES ) THEN
                             NSES_SOU(IP) = NSES_SOU(IP) + 1
                             NOBS_SOU(IP) = NOBS_SOU(IP) + NS_OBS
                             IF ( ILEN(CDAT_BEG(IP)) == 0 ) CDAT_BEG(IP) = STR(50:53)//'.'//STR(54:55)//'.'//STR(56:57)
                             CDAT_END(IP) = STR(50:53)//'.'//STR(54:55)//'.'//STR(56:57)
                        END IF
                   END IF
                 ELSE IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
                   IF ( STR(1:9) .EQ. 'SRC_STAT:' ) THEN
                        READ ( UNIT=STR(33:38), FMT='(I6)' ) NS_OBS
                        IF ( L_SOU .EQ. 0 ) THEN
                             L_SOU = L_SOU + 1
                             C_SOU(L_SOU) = STR(12:19)
                             NSES_SOU(L_SOU) = 0
                             NOBS_SOU(L_SOU) = 0
                             IP = L_SOU
                           ELSE
                             IP = LTM_DIF ( 1, L_SOU, C_SOU, STR(12:19) )
                             IF ( IP .LE. 0 ) THEN
                                  L_SOU = L_SOU + 1
                                  C_SOU(L_SOU) = STR(12:19)
                                  NSES_SOU(L_SOU) = 0
                                  NOBS_SOU(L_SOU) = 0
                                  IP = L_SOU
                             END IF
                        END IF
                        IF ( NS_OBS .GE. MN_OBSSES ) THEN
                             NSES_SOU(IP) = NSES_SOU(IP) + 1
                             NOBS_SOU(IP) = NOBS_SOU(IP) + NS_OBS
                             IF ( ILEN(CDAT_BEG(IP)) == 0 ) CDAT_BEG(IP) = STR(96:105)
                             CDAT_END(IP) = STR(96:105)
                        END IF
                   END IF
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
!
      N1 = 0
      N2 = 0
      N3 = 0
      DO 420 J2=1,L_SOU
         IF ( ILEN(CDAT_BEG(J2)) > 0 .AND. ILEN(CDAT_END(J2)) > 0 ) THEN 
              CALL DATE_TO_TIME ( CDAT_BEG(J2), MJD_BEG, TAI_BEG, -2 )
              CALL DATE_TO_TIME ( CDAT_END(J2), MJD_END, TAI_END, -2 )
              SPAN_YEAR = (MJD_END - MJD_BEG)/365.25D0
            ELSE 
              SPAN_YEAR = 0.0D0
         END IF
!
         IF ( ( NSES_SOU(J2) .GE. MN_SES  .AND.  NOBS_SOU(J2) .GE. MN_OBS .AND. SPAN_YEAR > MN_YEARS ) .OR. &
     &        ( NSES_SOU(J2) .LT. MN_SES  .AND.  NOBS_SOU(J2) .GE. MN_OBSTOT  .AND. SPAN_YEAR > MN_YEARS ) ) &
     &   THEN
!
! ----------- Source with good history
!
              N1 = N1 + 1
              SOU1(N1) = C_SOU(J2)
              WRITE ( UNIT=22, FMT='(A)' ) SOU1(N1)
            ELSE IF ( NOBS_SOU(J2) .EQ. 0 .OR. &
     &                NOBS_SOU(J2) .EQ. 1 .OR. &
     &                NOBS_SOU(J2) .EQ. 2      ) THEN
!
! ----------- No observations
!
              N3 = N3 + 1
              SOU3(N3) = C_SOU(J2)
              WRITE ( UNIT=44, FMT='(A)' ) SOU3(N3)
            ELSE
!
! ----------- Source with bad history
!
              N2 = N2 + 1
              SOU2(N2) = C_SOU(J2)
              WRITE ( UNIT=33, FMT='(A)' ) SOU2(N2)
         END IF
!
         WRITE ( UNIT=55, FMT='(A,"  ",I7,"  ",I7)' ) C_SOU(J2), NSES_SOU(J2), &
     &                                                           NOBS_SOU(J2)
 420  CONTINUE
      CLOSE ( 11 )
      CLOSE ( 22 )
      CLOSE ( 33 )
      CLOSE ( 44 )
      CLOSE ( 55 )
!
      WRITE ( 6, * ) '  '
      WRITE ( 6, * ) ' Spool file:         ', FILSPL
      WRITE ( 6, * ) ' Session limit     = ', MN_SES
      WRITE ( 6, * ) ' Limit of obs/ses  = ', MN_OBSSES
      WRITE ( 6, * ) ' Observation limit = ', MN_OBS, MN_OBSTOT
      WRITE ( 6, * ) ' Good  sources:  N1= ', N1
      WRITE ( 6, * ) ' Bad   sources:  N2= ', N2
      WRITE ( 6, * ) ' No    sources:  N3= ', N3
      WRITE ( 6, * ) ' Total sources:  NN= ', N1+N2+N3,'   (', L_SOU,') '
!
! --- Sorting
!
      CALL SYSTEM ( 'sort '//FILGOOD(1:I_LEN(FILGOOD))//' > '// &
     &               FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      CALL SYSTEM ( 'mv '//FILTMP(1:I_LEN(FILTMP))//' '// &
     &                     FILGOOD(1:I_LEN(FILGOOD))//CHAR(0)  )
!
      CALL SYSTEM ( 'sort '//FILBAD(1:I_LEN(FILBAD))//' > '// &
     &               FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      CALL SYSTEM ( 'mv '//FILTMP(1:I_LEN(FILTMP))//' '// &
     &                     FILBAD(1:I_LEN(FILBAD))//CHAR(0)  )
!
      CALL SYSTEM ( 'sort '//FILNON(1:I_LEN(FILNON))//' > '// &
     &               FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      CALL SYSTEM ( 'mv '//FILTMP(1:I_LEN(FILTMP))//' '// &
     &                     FILNON(1:I_LEN(FILNON))//CHAR(0)  )
!
      CALL SYSTEM ( 'sort '//FILALL(1:I_LEN(FILALL))//' > '// &
     &               FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      CALL SYSTEM ( 'mv '//FILTMP(1:I_LEN(FILTMP))//' '// &
     &                     FILALL(1:I_LEN(FILALL))//CHAR(0)  )
!
      CALL WRITE_SOURCE_LIST ( NK, FILGOOD, N1, SOU1 )
      CALL WRITE_SOURCE_LIST ( NK, FILBAD,  N2, SOU2 )
      CALL WRITE_SOURCE_LIST ( NK, FILNON,  N3, SOU3 )
!
      WRITE ( 6, * ) ' List of good sources: '//FILGOOD(1:I_LEN(FILGOOD))
      WRITE ( 6, * ) ' List of bad  sources: '//FILBAD(1:I_LEN(FILBAD))
      WRITE ( 6, * ) ' List of non  sources: '//FILNON(1:I_LEN(FILNON))
      WRITE ( 6, * ) ' List of all  sources: '//FILALL(1:I_LEN(FILALL))
!
      RETURN
      END  !#!  SOU_LIST  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_SOURCE_LIST ( NK, FIL, NS, SOU )
! ************************************************************************
! *                                                                      *
! *   Auxillry routine  WRITE_SOURCE_LIST  writes a source list          *
! *   in BATOPT format. The list is has NK columns.                      *
! *                                                                      *
! * ### 22-JUL-1998 WRITE_SOURCE_LIST v1.1 (c) L. Petrov 22-JUN-2011 ### *
! *                                                                      *
! ************************************************************************
      INTEGER*4  NK, NS
      CHARACTER  FIL*(*), SOU(NS)*8, OUT*(512*1024)
!
      OPEN ( UNIT=22, FILE=FIL, STATUS='OLD' )
      CALL CLRCH ( OUT )
!
      IB=1
      DO 420 J2=1,NS
         READ ( UNIT=22, FMT='(A)' ) SOU(J2)
         OUT(IB:) = SOU(J2)//' '
         IB = IB + (8+1)
 420  CONTINUE
      CLOSE ( UNIT=22 )
!
      OPEN ( UNIT=22, FILE=FIL, STATUS='UNKNOWN' )
      IB=1
      DO 430 J3=1,NS
         IE = IB + NK*(8+1) - 2
         IF ( J3*NK .LT. NS ) THEN
              WRITE ( UNIT=22, FMT='(A)' ) '      '//OUT(IB:IE)//' \'
           ELSE
              WRITE ( UNIT=22, FMT='(A)' ) '      '//OUT(IB:IE)
         END IF
         IB = IE + 2
         IF ( J3*NK .GE. NS ) GOTO 830
 430  CONTINUE
 830  CONTINUE
      CLOSE ( UNIT=22 )
      RETURN
      END  !#!  WRITE_SOURCE_LIST   #!#
