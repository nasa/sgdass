      SUBROUTINE SAVE_WEIGHTS ( DBNAME, VER, WEIGHT_FILE, INTERNAL_CONSTANTS, &
     &                          APPEND_FLAG, WEIGHT_TYPE_MA, NBLINE, NSITE, &
     &                          LSITN, INC_VERS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SAVE_WEIGHTS PROGRAM SPECIFICATION
!
! 1.1 Place weights in weight file, according to the specified format given
!     in weight_type_ma.
!
!  The three weight_file formats are:
!
!  I. for weighting by arc
!
!    col 1-9:   YYMMMDDXX database name
!    col 10-13: version number
!    remaining columns contain four free-format numbers:
!    1. group delay reweight (picoseconds)
!    2. group delay solution delay rate reweight (femtoseconds/second)
!    3. phase delay reweight (picoseconds)
!    4. phase delay solution delay rate reweight (femtoseconds/second)
!
! II. for weighting by site
!
!    datebase1 ver sitenam1 #####.## #####.## #####.## #####.## ...
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
!       for each site group,
!    1. site name (8 characters)
!    2. group delay solution, delay reweight (picoseconds)
!    3. group delay solution, rate reweight  (femtoseconds/second)
!    4. phase delay solution, delay reweight (picoseconds)
!    5. phase delay solution, rate reweight  (femtoseconds/second)
!         (all four reweight values have fortran format f8.2)
!
! III. for weighting by baselines
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
!
! 1.2 REFERENCES:
!
! 2.  SAVE_WEIGHTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INTEGER*2 CHARS_PER_SITE_GROUP, MAX_BUFFER
      PARAMETER (CHARS_PER_SITE_GROUP = 45)
      PARAMETER (MAX_BUFFER = CHARS_PER_SITE_GROUP*MAX_ARC_STA)
!
! 2.2 INPUT Variables:
!
      CHARACTER DBNAME*(*), WEIGHT_FILE*(*), APPEND_FLAG*(*), &
     &          WEIGHT_TYPE_MA*(*)
      INTEGER*2 VER, NBLINE, NSITE, LSITN(4,*)
      INTEGER*4 IOS
      REAL*8    INTERNAL_CONSTANTS(4,MAX_ARC_BSL)
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
#ifdef GNU
      INTEGER*4, EXTERNAL :: COMPAR_CH8
#else
      INTEGER*2, EXTERNAL :: COMPAR_CH8
#endif
!
! APPEND_FLAG - True if we should append to weight-file
! INTERNAL_CONSTANTS - Weights to be stored in internal solve format
!    (expressed as baseline weights, regardless of the true nature
!       (arc, baseline or site) of the weights)
! DBNAME - Name of database
! VER - Database version number
! WEIGHT_FILE - Name of file to store weights in
! weight_type_ma - type of weights being produced
!         (S for weighting by site, A for weighting by arc)
! nbline - number of baselines
! nsite  - number of sites (from socom)
! lsitn  - list of site names (from parfil)
!
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prces
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
!
!     a buffer size of 1741 allows max_arc_sta = 32 sites.
!
      CHARACTER  SORT_BUFFER*(MAX_BUFFER), OUT_BUFFER*(MAX_BUFFER+13), &
     &           HEADER*13, DBNAML*10, CSITN_UND*8, JBUF*80
      INTEGER*2  IERR, IWT_MODE, IGP, IBL, IDR, IST, ISTART_OUT, &
     &           IEND_OUT, IVAL, NUM_IN_ROW, NUM_FULL_ROWS, ISTART_IN, &
     &           IEND_IN, NUM_LEFT, ICT, IUND, IPAR, J1
      INTEGER*4  INC_VERS
      REAL*8     INTERN_FORM(2,MAX_ARC_BSL), EXTERN_FORM(2,MAX_ARC_STA), &
     &           SITE_CONSTANTS(4,MAX_ARC_BSL), DPDUM2(2)
      INTEGER*4, EXTERNAL :: I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  970204      New site weighting feature.
!   pet  2000.03.29  Added support of a "by_baseline" weight format.
!                    Improved comments and error messages
!
! 5.  SAVE_WEIGHTS PROGRAM STRUCTURE
!
! remove possbile non-numeric leading character
!
      DBNAML=DBNAME
      IF ( INDEX ( '0123456789', DBNAME(1:1)).EQ.0 ) DBNAML = DBNAME(2:)
!
! --- Weights are carried internally in solve as baseline weights.
! --- If site weights are desired, they must be generated from the baseline
! --- weights,
! --- (Arc weights are okay as is.  The single set of weights for the
! --- current arc is stored redundantly in every set of baseline weight
! --- variables, and can be pulled from the first set.)
! --- (Baseline weights are even better as is. Only slight format change is
! ---  done before copying them in NAMFILE)
!
      IF ( WEIGHT_TYPE_MA .EQ. 'S' ) THEN
!
! -------- Generate the site weights.  The subroutine that does this only
! -------- handles two of the four weights at a time, so call twice,
! -------- once for the group weights and once for the phase weights.
!
           IWT_MODE = 1 ! Flag to produce site weights
           DO IGP = 0,2,2 ! Group, then phase
              DO IBL = 1,NBLINE  ! Baslines
                 DO IDR = 1,2
                    INTERN_FORM(IDR,IBL) = INTERNAL_CONSTANTS(IGP+IDR,IBL)
                 ENDDO
              ENDDO
!
              CALL WTS_BL_OTH ( INTERN_FORM, IWT_MODE, NSITE, EXTERN_FORM, &
     &                          DPDUM2 )
              DO IST = 1,NSITE
                 DO IDR = 1,2
                    SITE_CONSTANTS(IGP+IDR,IST) = EXTERN_FORM(IDR,IST)
                 ENDDO
              ENDDO
           ENDDO
!
! -------- Sort the sites alphabetically.
!
! -------- Set up sort buffer
!
           ISTART_OUT = 1
           IEND_OUT = CHARS_PER_SITE_GROUP
           DO IST = 1,NSITE
!
! ----------- Replace blanks in the site names with underscores
!
              WRITE ( CSITN_UND, "(4A2)", IOSTAT=IOS) &
     &               (LSITN(IVAL,IST),IVAL = 1,4)
              CALL FERR ( INT2(IOS), 'SAVE_WEIGHTS: error in setting up for '// &
     &            'blank to underscore conversion', INT2(0), INT2(0) )
              DO IUND = 1,8
                 IF (CSITN_UND(IUND:IUND).EQ.' ') CSITN_UND(IUND:IUND) = '_'
              ENDDO
              WRITE ( SORT_BUFFER(ISTART_OUT:IEND_OUT), 90, IOSTAT=IOS) &
     &                CSITN_UND, (SITE_CONSTANTS(IVAL,IST),IVAL=1,4)
 90           FORMAT ( 1X, A8, 4(1X,F8.2) )
             CALL FERR ( INT2(IOS), 'SSAVE_WEIGHTS: error in setting up '// &
     &           'buffer to sort by_site weights', INT2(0), INT2(0) )
              IF ( IST .LT. NSITE ) THEN
                   ISTART_OUT = ISTART_OUT + CHARS_PER_SITE_GROUP
                   IEND_OUT = IEND_OUT + CHARS_PER_SITE_GROUP
              ENDIF
           ENDDO
!
! -------- Now sort.
!
           CALL FOR_QSORT  ( %REF(SORT_BUFFER), INT4(NSITE), &
     &                       INT4(CHARS_PER_SITE_GROUP), COMPAR_CH8 )
      ENDIF
!
! --- Open the weight file and write out the weights
!
      CALL FTN_OPEN ( INT2(65), WEIGHT_FILE, APPEND_FLAG )
      IF ( WEIGHT_TYPE_MA .EQ. 'A' ) THEN ! Weighting by arc
!
! -------- The same arc reweights are stored for every baseline.
! -------- Take the first baseline's values.
!
           WRITE ( 65, 91, IOSTAT=IOS ) DBNAML, VER, &
     &             (INTERNAL_CONSTANTS(IVAL,1),IVAL=1,4)
 91        FORMAT ( A10, I3, 4(2X,F10.2) )
           CALL FERR ( INT2(IOS), 'SAVE_WEIGHTS: error in writing by_arc '// &
     &                 'weight file', INT2(0), INT2(0) )
         ELSE IF ( WEIGHT_TYPE_MA .EQ. 'B' ) THEN ! Weighting by baseline
!
! -------- "BY_BASELINE" mode. Here we copy contents of REWT NAMEFIL card with
! -------- some transformation:
! -------- a) add prefix: database_name and version
! -------- b) remove prefix REWT
! -------- c) replace balnks in tation name with underscores
! -------- d) replace "-" as a separator bwetween station name with "/"
!
           DO 410 J1=1,NBLINE
              IF ( J1 .EQ. 1 ) THEN
                   IPAR = 1
                 ELSE
                   IPAR = 0
              END IF
!
! ----------- Read REWT card
!
              CALL GETCARD ( INT2(1), 'REWT', IPAR, JBUF, IERR )
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1, ' IERR=',IERR
                   CALL FERR ( INT2(1054), &
     &                 'SAVE_WEIGHTS: Error in getting REWT '// &
     &                 'card from NAMFIL', INT2(0), INT2(0) )
                   RETURN
              END IF
!
! ----------- Replace blanks with underscores
!
              DO IUND = 6,22
                 IF ( JBUF(IUND:IUND) .EQ. ' ' ) JBUF(IUND:IUND) = '_'
              ENDDO
!
! ----------- Replace "-" with "/" in station names delimiter
!
              JBUF(14:14) = '/'
!
! ----------- Write down the line in the output weight file
!
              IF ( INC_VERS == -2 ) THEN
                   WRITE  ( 65, 110, IOSTAT=IOS ) DBNAML, 0,            JBUF(6:62)
                ELSE 
                   WRITE  ( 65, 110, IOSTAT=IOS ) DBNAML, VER+INC_VERS, JBUF(6:62)
              END IF
 110          FORMAT ( A10, I3, 1X, A )
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(1056), 'SAVE_WEIGHTS: Error in writing '// &
     &                 'a record in the output weight file '// &
     &                  WEIGHT_FILE(1:I_LEN(WEIGHT_FILE)), INT2(0), INT2(0) )
                   RETURN
               END IF
 410       CONTINUE
!
! -------- Write down a session delimiter after processing the last baseline
!
           WRITE  ( 65, FMT='(A)', IOSTAT=IOS ) '*'
         ELSE IF ( WEIGHT_TYPE_MA .EQ. 'S' ) THEN ! Weighting by site
!
! -------- For each group of four sites, until all sites are taken care of,
! -------- write a line to the site file with the current database name and
! -------- version number, followed by four groups of site info (where each
! -------- group has a site name followed by four reweight values).
!
! -------- Set up the part which is common to all lines (database name
! -------- and version)
!
           IF ( INC_VERS == -2 ) THEN
                WRITE ( HEADER, 92, IOSTAT=IOS ) DBNAML, 0
              ELSE
                WRITE ( HEADER, 92, IOSTAT=IOS ) DBNAML, VER+INC_VERS
           END IF
 92        FORMAT(A10,I3)
           CALL FERR ( INT2(IOS), 'SAVE_WEIGHTS Error in setting up by_site '// &
     &                'weight file buffer header ', INT2(0), INT2(0) )
!
! -------- Dump the full rows of 4 sites
!
           NUM_IN_ROW = 4
           NUM_FULL_ROWS = NSITE/NUM_IN_ROW
           IF ( NUM_FULL_ROWS .GT. 0 ) THEN
                ISTART_IN = 1
                IEND_IN  = CHARS_PER_SITE_GROUP * 4
                IEND_OUT = 13 + CHARS_PER_SITE_GROUP * 4
                DO ICT = 1,NUM_FULL_ROWS
                   OUT_BUFFER = HEADER//SORT_BUFFER(ISTART_IN:IEND_IN)
                   WRITE ( 65, '(A)', IOSTAT=IOS ) OUT_BUFFER(1:IEND_OUT)
                   CALL FERR ( INT2(IOS), 'SAVE_WEIGHTS: Error in writing '// &
     &                 'by_site full row', INT2(0), INT2(0) )
                   ISTART_IN = ISTART_IN + CHARS_PER_SITE_GROUP * 4
                   IEND_IN = IEND_IN + CHARS_PER_SITE_GROUP * 4
                ENDDO
              ELSE
                ISTART_IN = 1
            END IF
!
! --------- Dump the final, partially filled row, if there is one
!
            IF ( NSITE/NUM_IN_ROW*NUM_IN_ROW .NE. NSITE ) THEN
                 NUM_LEFT = NSITE - NUM_FULL_ROWS * NUM_IN_ROW
                 IF ( ISTART_IN .EQ. 1 ) THEN
                      IEND_IN = CHARS_PER_SITE_GROUP * NUM_LEFT
                    ELSE
                      IEND_IN = IEND_IN - CHARS_PER_SITE_GROUP * (4 - NUM_LEFT)
                 ENDIF
                 OUT_BUFFER = HEADER//SORT_BUFFER(ISTART_IN:IEND_IN)
                 IEND_OUT = 13 + CHARS_PER_SITE_GROUP * NUM_LEFT
                 WRITE ( 65, '(A)', IOSTAT=IOS ) OUT_BUFFER(1:IEND_OUT)
                 CALL FERR ( INT2(IOS), 'SAVE_WEIGHTS: Error in writing '// &
     &                'by_site partial row', INT2(0), INT2(0) )
             END IF
         ELSE
             CALL FERR ( INT2(1822), &
     &           'SAVE_WEIGHTS: unsupported weighting type: '//'WEIGHT_TYPE_MA', &
     &            INT2(0), INT2(0) )
      END IF ! WEIGHT_TYPE_MA
!
! --- Write an end-of-file mark and close the file
!
      ENDFILE ( 65, IOSTAT=IOS )
      CALL FERR ( INT2(IOS), 'SAVE_WEIGHTS: error in marking EOF '// &
     &            WEIGHT_FILE, INT2(0), INT2(0) )
      CLOSE ( 65, IOSTAT=IOS )
      CALL FLUSH ( 65 )
      CALL FERR ( INT2(IOS), 'Closing'//WEIGHT_FILE, INT2(0), INT2(0) )
!
      RETURN
      END  !#!  SAVE_WEIGHTS  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   COMPAR_CH8 ( STA1, STA2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine COMPAR_CH8 is used for comparison of two        *
! *   strings of 8 characters long.                                      *
! *                                                                      *
! * ### 14-OCT-2017   COMPAR_CH8   v1.0 (c)  L. Petrov  14-OCT-2017  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  STA1*8, STA2*8
#ifdef GNU
      INTEGER*4  COMPAR_CH8
#else
      INTEGER*2  COMPAR_CH8
#endif
!
      IF ( STA1 > STA2 ) THEN
           COMPAR_CH8 =  1
         ELSE IF ( STA1 < STA2 ) THEN
           COMPAR_CH8 = -1
         ELSE
           COMPAR_CH8 =  0
      END IF
!
      RETURN
      END  FUNCTION  COMPAR_CH8  !#!#
