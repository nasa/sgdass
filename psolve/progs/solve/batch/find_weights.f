      FUNCTION FIND_WEIGHTS ( DBNAME, VER, LSITN, NUMSTA, LF_WEI, &
     &                        WEIGHT_FILE, WEIGHTS, WEIGHT_TYPE_UR, CONSTANTS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      LOGICAL*2 FIND_WEIGHTS 
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! 1.  FIND_WEIGHTS PROGRAM SPECIFICATION
!
!
! 1.1 Look in file for new re-weight constants for this arc. Weight file
!      format is free-format with the following values in the following
!      order:
!
!   I. weight_type_ur A (for weighting by arc)
!
!         YYMMMDDXX database name
!         version number
!         group delay reweight (picoseconds)
!         group delay solution delay rate reweight (femtosec/sec)
!         phase delay reweight (picoseconds)
!         phase delay solution delay rate reweight (femtosec/sec)
!
!   II. weight_type_ur S (for weighting by site)
!
!    database1 ver sitenam1 #####.## #####.## #####.## #####.## ...
!                       sitenam4 #####.## #####.## #####.## #####.##
!    database1 ver sitenam5 #####.## #####.## #####.## #####.## ...
!                       sitenam8 #####.## #####.## #####.## #####.##
!               for as many sites as needed
!    database2 ver sitenam1 #####.## #####.## #####.## #####.## ...
!                       sitenam4 #####.## #####.## #####.## #####.##
!           and so on.
!
!   (the specific field sizes are:
!
!    col 1-9:   YYMMMDDXX database name
!    col 10-13: version number
!
!    1. site name of 8 characters
!    2. group delay solution, delay reweight (picoseconds)
!    3. group delay solution, rate reweight (femtoseconds/second)
!    4. phase delay solution, delay reweight (picoseconds)
!    5. phase delay solution, rate reweight (femtoseconds/second)
!         (all four reweight values have fortran format f8.2
!
!   III. weight_type_ur B (for weighting by baseline)
!
!    datebase1 ver sitenam1/sitenam2  #####.##  #####.##  #####.##  #####.##
!    col 1-9:   YYMMMDDXX database name
!    col 10-13: version number
!    col 15-22: station1
!    col 24-31: station1
!    col 33-41: F8.2  group delay solution, delay reweight (picoseconds)
!    col 43-51: F8.2  group delay solution, rate  reweight (femtoseconds/second)
!    col 53-61: F8.2  phase delay solution, delay reweight (picoseconds)
!    col 63-71: F8.2  phase delay solution, rate  reweight (femtoseconds/second)
!
! 1.2 REFERENCES:
!
! 2.  FIND_WEIGHTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER DBNAME*(*), WEIGHT_FILE(*)*(*), WEIGHTS*1
      INTEGER*2 VER, LSITN(4,*), NUMSTA
!
! DBNAME - Database name
! VER - Database version number
! WEIGHT_FILE - Name of file containing new weights
! lsitn - list of site names
! weights - significance of weights in solution
!      (e.g., U for use weights if possible, R for absolutely require them)
! numsta - number of sites in this arc
!
! 2.3 OUTPUT Variables:
!
      REAL*8    CONSTANTS(4,MAX_ARC_BSL)
      CHARACTER WEIGHT_TYPE_UR*(*)
!
! weight_type_ur - type of weighting (A = by arc, S = by site)
! CONSTANTS - New weights read from file
! FIND_WEIGHTS - True if weights found for this arc, false otherwise
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER  DBNAML*9, CDUM*200, CCDUM*200, FIELD1*80, FIELD2*80, &
     &           FILE_TYPE_CHECK*80, THIS_VAL*80, THIS_SITE*8, &
     &           SITE_NAMES(MAX_ARC_STA)*8, CSITN*8, STR*80, STRF*80, &
     &           JBUF*72, JBUF_READ*72
      INTEGER*2  IERR, IVER, DECIMALTOINT, ILEN1, ILEN2, TRIMLEN, ICT, ICT1, &
     &           NSITES, IPTR, JCT, IGP, IST, IDR, NBLINES, IBL, &
     &           IWT_MODE, IPAR_I2, IFLEN
      INTEGER*4  LF_WEI, LU_FLOORS
      REAL*8     RNUM_CHECK
      LOGICAL*2  LOOP, FLOOR_FLAG, PHASE_FLAG, FIRST_ARC
      REAL*8     SORT_CONSTANTS(4,MAX_ARC_STA), SITE_CONSTANTS(4,MAX_ARC_STA), &
     &           EXTERN_FORM(2,MAX_ARC_STA), INTERN_FORM(2,MAX_ARC_BSL), &
     &           FLOOR_BLINE, FLOOR_SITE
      INTEGER*4  IOS, J1, J2, J3
      CHARACTER  FLOOR_FILE*63, ERRBUF*255
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
      DATA FIRST_ARC /.TRUE./
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   KDB  950804  Allow free-format weight file
!   KDB  970204  New site weighting feature.
!   KDB  970226  Special option to: if 1+ site group delay weights > site_floor,
!                set min bline group delay weight to bline_floor,
!                where the floors are read from a file
!   pet  980208  Hided stealth-bug (ref. code)
!   pet  2000.03.29   Added support of a "by_baseline" weight file type.
!                     Improved comments and error messages
!   pet  2000.11.29   Fixed the bug: the previous version computed the number
!                     of baslines incorrectly. As a result it reached the end
!                     of weight file in attempt to get weights from the last
!                     one-baseline session
!
! 5.  FIND_WEIGHTS PROGRAM STRUCTURE
!
      FIND_WEIGHTS = .FALSE.
      DO 410 J1=1,LF_WEI
         OPEN ( 65, FILE=WEIGHT_FILE(J1), IOSTAT=IOS )
         CALL FERR ( INT2(IOS), 'FIND_WEIGHTS: Error in opening weight file '// &
     &               WEIGHT_FILE(J1), INT2(0), INT2(0) )
!
! ------ Removing possible non-numeric leading character in the input 
! ------ database name since the weight file's database names are required 
! ------ to leave off the leading database symbol
!
         DBNAML=DBNAME
         IF ( INDEX ( '0123456789', DBNAME(1:1)) .EQ. 0 ) DBNAML=DBNAME(2:)
!
! ------ Serach for this db and ver
!
  121    CONTINUE
!
         READ ( 65, '(A)', IOSTAT=IOS ) CDUM
         CALL FERR ( INT2(IOS), 'FIND_WEIGHTS: Error in first reading of '// &
     &               'weight file '//WEIGHT_FILE(J1), INT2(0), INT2(0) )
!
! ------ ignore comments lines
!
         IF ( CDUM(1:1)  .EQ. '*' ) GOTO 121
         IF ( CDUM(1:1)  .EQ. '#' ) GOTO 121
         IF ( ILEN(CDUM) .EQ. 0   ) GOTO 121
         STR = CDUM
!
! ------ Split the line onto words
!
         CALL SPLITSTRING ( CDUM, FIELD1, CDUM )
         CALL SPLITSTRING ( CDUM, FIELD2, CDUM )
         ILEN1 = TRIMLEN  ( FIELD1 )
         ILEN2 = TRIMLEN  ( FIELD2 )
         IVER = DECIMALTOINT ( FIELD2(1:ILEN2), IERR )
         CALL FERR ( IERR, 'FIND_WEIGHTS: Error in first decoding of '// &
     &       'version number in weight file '//WEIGHT_FILE(J1), &
     &        INT2(0), INT2(0) )
         IF ( FIELD1(1:ILEN1) .EQ. DBNAML  .AND.  IVER .EQ. VER ) GOTO 250
         GOTO 121
!
! ------ We found it!!!
!
  250    CONTINUE
         CLOSE ( 65, IOSTAT=IOS )
!
! ------ Determine the type of weight file (by (A)rc or by (S)ite or by
! ------ (B)aselines).
! ------ Do so by looking at the line being currently processed.
! ------ If the next thing is a number, it's a by Arc file.
! ------ Otherwise, assume the next thing is a site name and that
! ------ it's a by Site file or by Baseline file.
!
         STRF = STR
         CCDUM = CDUM
         CALL SPLITSTRING ( CCDUM, FILE_TYPE_CHECK, CCDUM )
         READ ( FILE_TYPE_CHECK, *, IOSTAT=IOS, ERR=265 ) RNUM_CHECK
         IF ( RNUM_CHECK .LT. 0 ) IOS = 1
 265     CONTINUE
         IERR = IOS
         IF ( IERR .EQ. 0 ) THEN
              WEIGHT_TYPE_UR = 'A'
            ELSE
              WEIGHT_TYPE_UR = 'S'
         ENDIF
!
         IF ( STR(23:23) .EQ. '/' ) THEN
              WEIGHT_TYPE_UR = 'B'
         ENDIF
!
         IF ( WEIGHT_TYPE_UR .EQ. 'A' ) THEN
!
! ----------- If weighting by arc, the weight line applicable to this arc just
! ----------- has four values. Read them and return.
!
              READ ( CDUM, *, IOSTAT=IOS ) ( CONSTANTS(ICT,1), ICT=1,4 )
              CALL FERR ( INT2(IOS), 'FIND_WEIGHTS: Error in decoding '// &
     &                   'by arc weights from file '//WEIGHT_FILE(J1), &
     &                    INT2(0), INT2(0) )
              FIND_WEIGHTS = .TRUE.
           ELSE IF ( WEIGHT_TYPE_UR .EQ. 'B' ) THEN
!
! ----------- Weights of baseline type
!
              DO 420 J2=1,(NUMSTA*(NUMSTA-1))/2
                 IF ( J2 .EQ. 1 ) THEN
                      IPAR_I2 = 1
                    ELSE
                      IPAR_I2 = 0
                 END IF
!
                 CALL GETCARD ( INT2(1), 'REWT', IPAR_I2, JBUF_READ, IERR )
                 IF ( IERR .EQ. 1 ) GOTO 810
                 IF ( IERR .NE. 0 ) THEN
                      WRITE ( 6, * ) ' IERR=',IERR
                      CALL FERR ( INT2(1042), &
     &                    'FIND_WEIGHTS: Error in attempt to read'// &
     &                    ' weights from NAMFIL ', INT2(0), INT2(0) )
                      GOTO 300
                 END IF
!
! -------------- Check: whether the line from weight file corresponds to
! -------------- the current superfile
!
                 IF ( STR(1:13) .NE. STRF(1:13) ) THEN
!
! ------------------- Wow: we have already read the line in weight file for the
! ------------------- next superfile
!
                      CALL FERR ( INT2(1044), 'FIND_WEIGHTS: Weight '// &
     &                    ' weight file '//WEIGHT_FILE(J1)//' contains '// &
     &                    'not all baselines which are in the '// &
     &                    'superfile '//STRF(1:13), INT2(0), INT2(0) )
                      GOTO 300
                 END IF
!
! -------------- Replace undsercores with blanks and make other things in order
! -------------- to generate a line for NAMEFIL
!
!@U                 CALL UNDSCR ( STR(15:31) )
                 STR(23:23) = '-'
                 JBUF = 'REWT '//STR(15:)
!
! -------------- Put this line in NAMEFIL
!
                 CALL PUTCARD ( INT2(1), 'REWT', INT2(4), JBUF, IERR )
                 IF ( IERR .NE. 0 ) THEN
                      WRITE ( 6, * ) ' IERR=',IERR
                      CALL FERR ( INT2(1046), 'FIND_WEIGHTS: Error in '// &
     &                    'attempt to put weights in NAMFIL', INT2(0), INT2(0) )
                      GOTO 300
                 END IF
!
                 IF ( J2 .EQ. (NUMSTA*(NUMSTA-1))/2 ) GOTO 810
!
! -------------- If not all baselines have been processed, try to read the
! -------------- next line
!
 710             CONTINUE
                 READ ( 65, '(A)', IOSTAT=IOS ) STR
                 IF ( IOS .EQ. -1 ) THEN
!
! ------------------- Wow! The file is ended here. Let's figure out whether
! ------------------- we have processed the last baseline in the session
!
                      CALL GETCARD ( INT2(1), 'REWT', IPAR_I2, JBUF_READ, IERR )
                      IF ( IERR .EQ. 1 ) GOTO 810 ! oh, yes!
                      IF ( IERR .NE. 0 ) THEN
                           WRITE ( 6, * ) ' IERR=',IERR
                           CALL FERR ( INT2(1048), &
     &                         'FIND_WEIGHTS: Error in attempt '// &
     &                         'to read weights from NAMFIL ', INT2(0), INT2(0) )
                           GOTO 300
                      END IF
                      WRITE ( 6, * ) ' IERR=',IERR,' IOS=',IOS
                      CALL FERR ( INT2(1050), 'FIND_WEIGHTS: premature end '// &
     &                    'of weight file '//WEIGHT_FILE(J1)// &
     &                    ' it contains not all baseline weights for the '// &
     &                    'superfile '//STRF(1:13), INT2(0), INT2(0) )
                      GOTO 300
                 END IF
                 IF ( IOS .NE. 0 ) THEN
                      WRITE ( 6, * ) ' IOS=',IOS
                      CALL FERR ( INT2(1052), 'FIND_WEIGHTS: Error in '// &
     &                    'reading weight file by_baseline. Line '//STR// &
     &                    ' weight file '//WEIGHT_FILE(J1), INT2(0), INT2(0) )
                      GOTO 300
                 END IF
!
! -------------- If the line is a comment line -- read wights file once more
!
                 IF ( STR(1:1)  .EQ. '*' ) GOTO 710
                 IF ( STR(1:1)  .EQ. '#' ) GOTO 710
                 IF ( ILEN(STR) .EQ.  0  ) GOTO 710
 420          CONTINUE
!
! ----------- Happy end
!
 810          CONTINUE
              FIND_WEIGHTS = .TRUE.
              GOTO 300
           ELSE IF ( WEIGHT_TYPE_UR .EQ. 'S' ) THEN
!
! ---------- If weighting by site, there are one or more lines in this file
! ---------- for the current arc, where each line has up to four sites and each
! ---------- site has four values.  Process each line for the desired arc.
!
              NSITES = 0
              DO WHILE ( FIELD1(1:ILEN1) .EQ. DBNAML  .AND.  IVER .EQ. VER )
!
! -------------- The current weight file line applies to the desired arc.
! -------------- Process it.
!
                 LOOP = .TRUE.
                 DO WHILE ( LOOP )
                    CALL SPLITSTRING ( CDUM, THIS_SITE, CDUM )
!
! ----------------- The check for .00 gets past a defect in the initial site weight
! ----------------- files in which two extra .00's were appended to all lines to
! ----------------- take care of lines which lacked rate values.  (Defective site
! ----------------- weight files were produced at the request of a user who needed
! ----------------- preliminary weight file before testing was completed.)
!
                    IF ( ILEN(THIS_SITE) .EQ. 0  .OR.  THIS_SITE .EQ. '.00' ) THEN
                         LOOP = .FALSE.
                      ELSE
                         NSITES = NSITES + 1
                         DO ICT = 1,8
!
! ------------------------- Remove underscores put into the site names
! ------------------------- in the weight file for to keep the free formatted
! ------------------------- fields.
!
                            IF ( THIS_SITE(ICT:ICT) .EQ. '_' ) THEN
                                 THIS_SITE(ICT:ICT) = ' '
                            END IF
                         END DO
!
                         SITE_NAMES(NSITES) = THIS_SITE
                         DO ICT = 1,4
                            CALL SPLITSTRING ( CDUM, THIS_VAL, CDUM  )
                            READ ( THIS_VAL, *, IOSTAT=IOS, ERR=280) &
     &                             SORT_CONSTANTS(ICT,NSITES)
 280                        CALL FERR ( INT2(IOS), 'FIND_WEIGHTS: Error in '// &
     &                          'decoding '//'by_site weights from file', &
     &                           INT2(0), INT2(0) )
                        ENDDO
                    ENDIF
                 ENDDO
!
! -------------- Get the next line
!
                 READ ( 65, '(A)', IOSTAT=IOS, END=290 ) CDUM
                 CALL FERR ( INT2(IOS), 'FIND_WEDIGHTS: Error in reading weight '// &
     &                      'file for next by_site', INT2(0), INT2(0) )
                 CALL SPLITSTRING ( CDUM, FIELD1, CDUM )
                 CALL SPLITSTRING ( CDUM, FIELD2, CDUM )
                 ILEN1 = TRIMLEN(FIELD1)
                 ILEN2 = TRIMLEN(FIELD2)
                 IVER = DECIMALTOINT ( FIELD2(1:ILEN2), IERR )
                 CALL FERR ( IERR, 'FIND_WEIGHTS: second+ decoding of weight '// &
     &                       'file version number', INT2(0), INT2(0) )
                 GOTO 295
 290             CONTINUE
                 ILEN1 = 3
                 FIELD1 = 'EOF'
 295         CONTINUE
           ENDDO
           FIND_WEIGHTS = .TRUE.
!
! -------- Make sure each site in this arc is represented in the weight line.
! -------- At the same time, reorder the weight values for the sites.
! -------- The weights were entered in the weight file alphabetically
! -------- (or possibly even in some random order, since some weight files
! -------- are manually generated), but they should match the parfil and namfil
! -------- order in the internal variables.
!
           DO ICT = 1, NUMSTA
              WRITE ( CSITN, "(4A2)" ) (LSITN(ICT1,ICT),ICT1=1,4)
              IPTR = 0
              DO JCT = 1,NSITES
                 IF ( SITE_NAMES(JCT) .EQ. CSITN ) IPTR = JCT
              ENDDO
              IF ( IPTR .EQ. 0 ) FIND_WEIGHTS = .FALSE.
              DO JCT = 1,4
                 IF ( IPTR .EQ. 0 ) THEN
                      SITE_CONSTANTS(JCT,ICT) = 0.0D0
                   ELSE
                      SITE_CONSTANTS(JCT,ICT) = SORT_CONSTANTS ( JCT, IPTR )
                 ENDIF
              ENDDO
           ENDDO
!
! -------- Special option: if a file exists and has non-zero values,
! -------- flag case where 1+ sites has a group delay weight >
! -------- site floor value so the code can later force all baseline weights
! -------- to a baseline floor value if they fall below it..
!
! -------- But first get the site and baseline floors from a file, if this
! -------- is the first arc.
!
           IF ( FIRST_ARC ) THEN
                FIRST_ARC  = .FALSE.
                FLOOR_FILE = "/data1/solve_files/weight_floors"
                IFLEN = TRIMLEN ( FLOOR_FILE )
                LU_FLOORS = GET_UNIT()
                OPEN ( LU_FLOORS, FILE=FLOOR_FILE(1:IFLEN), IOSTAT=IOS, &
     &                 STATUS='OLD', ACCESS='SEQUENTIAL', FORM='FORMATTED' )
 405            CONTINUE
                IERR = IOS
                IF ( IERR .NE. 0 .AND. IERR .NE. 908 ) THEN
                     WRITE ( ERRBUF, "('FIND_WEIGHTS: error ',I5, &
     &                       ' opening weight floor file ',A)" ) IERR, &
     &                       FLOOR_FILE(1:IFLEN)
                     CALL FERR ( INT2(710), ERRBUF, INT2(0), INT2(0) )
                  ELSE IF (IERR.EQ.908) THEN
                     FLOOR_SITE = 0.0d0
                     FLOOR_BLINE = 0.0d0
                  ELSE
                     READ ( LU_FLOORS, *, IOSTAT=IOS ) FLOOR_SITE, FLOOR_BLINE
                     IERR = IOS
                     IF ( IERR .NE. 0 ) THEN
                          WRITE ( ERRBUF, "('FIND_WEIGHTS: error ',I5, &
     &                           ' reading weight floor file ')" ) IERR
                          CALL FERR ( INT2(720), ERRBUF, INT2(0), INT2(0) )
                     ENDIF
                 ENDIF
                 CLOSE ( LU_FLOORS )
           ENDIF  !  FIRST_ARC
!
! -------- Now see if this arc falls into this case
!
           IF ( FLOOR_SITE .EQ. 0.0D0  .AND.  FLOOR_BLINE .EQ. 0.0D0 ) THEN
                FLOOR_FLAG = .FALSE.
              ELSE
                PHASE_FLAG = .FALSE.
                FLOOR_FLAG = .FALSE.
                DO IST = 1,NUMSTA
                   IF ( SITE_CONSTANTS(3,IST) .NE. 0.0D0 .OR. &
     &                  SITE_CONSTANTS(4,IST) .NE. 0.0D0      ) PHASE_FLAG = .TRUE.
                   IF ( SITE_CONSTANTS(1,IST) .GT. FLOOR_SITE ) FLOOR_FLAG = .TRUE.
                ENDDO
                IF ( PHASE_FLAG ) FLOOR_FLAG = .FALSE.
           ENDIF
!
! -------- Finally, Solve deals internally with baseline weights, not site
! -------- weights.  Convert the site weights to baseline weights.
!
           IWT_MODE = 1 ! flag to produce site weights
           DO IGP = 0,2,2 ! Group, then phase
              DO IST = 1,NUMSTA
                 DO IDR = 1,2
                    EXTERN_FORM(IDR,IST) = SITE_CONSTANTS(IGP+IDR,IST)
                 ENDDO
              ENDDO
!
              CALL WTS_OTH_BL ( EXTERN_FORM, NUMSTA, IWT_MODE, INTERN_FORM )
              NBLINES = (NSITES*(NSITES-1))/2
              DO IBL = 1,NBLINES
                 DO IDR = 1,2
                    CONSTANTS(IGP+IDR,IBL) = DSQRT(INTERN_FORM(IDR,IBL))
                 ENDDO
              ENDDO
           ENDDO
!
! -------- Now set all baseline weights to at least the baseline floor,
! -------- if this arc is the special case
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Alas, there is somewhere a stealth-bug in BATCH. Sometimes it corrupts
! variable floor_flag and it become TRUE instead of FALSE. That lead to
! setting constants(1,*) to indefined value.  I didn't find it. I merely
! commented out it and that cured SOLVE.   pet 08-FEB-98 17:54:06
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        if (floor_flag) then
!          do ibl = 1,nblines
!            if (constants(1,ibl).lt.floor_bline)
!     .          constants(1,ibl) = floor_bline
!          enddo
!        endif
!!!!!!!!!
         ENDIF  !  WEIGHT_TYPE_UR
 410  CONTINUE 
!
      IOS = 0
 300  CONTINUE
      IERR = IOS
!
      RETURN
      END  !#!  FIND_WEIGHTS  #!#
