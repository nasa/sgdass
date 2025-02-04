!!Cntrl
!
!     Common space to keep all the control file values.
!
!     Programmers:
!       Gregg Cooke        90.02.08  Creation.
!     Modifications:
!       Brent Archinal     90.12.12  Changed to Beta version no. 2,
!                                    and MAXCFL changed to 16;
!                                    after correcting time selection
!                                    and output of multiple databases,
!                                    changing name to dbedit, and adding
!                                    EPHEMERIS flag.
!       Brent Archinal     91.02.08  Changed to Beta version no. 3,
!                                    due to duplicate elimination fix
!                                    in sort_obs.f (by DGG).
!       Brent Archinal     91.02.27  Changed to Beta version no. 4,
!                                    due to fix of "24 hour" UTC TAG
!                                    problem in put_ephem.f.
!       "                  91.04.30  Changed to operational version 1.0,
!                                    call to datsv added to dbedit.f.
!       Brent Archinal     91.07.25  Size of Uflag fixed several places,
!                                    call to phist_ephut changed in
!                                    gen_outdb, phist_outdb fixes.
!                                    Version 1.1.
!       Darin Miller       91.11.19  Changed variable "ut1nm" to
!                                    "dut1nm" to avoid redundant
!                                     common variable in apriori
!                                    file dbedit_ut1com.i, and changed the
!                                    size to 255.
!       Darin Miller       91.12.13  Added dirflg, iform, and hform.
!       Brent Archinal     92.01.07  Version 2.0.  Many changes,
!                                    mostly to read correl files, but
!                                    also to run calc, have multiple
!                                    users, and fix bugs.
!       Brent Archinal     92.04.09  Version 2.1.  "open_disk" rewritten
!                                    to read correl file names properly.
!                                    Parameters moved to param.i.  Got
!                                    working on HP9000/700 system.
!       Brent Archinal     92.10.06  Changes made to handle ~20000 obs.
!                                    Also fix to read_ddbuf and fixes
!                                    to minimize number of units opened.
!       "     "            92.12.28  Version 2.3.
!       Melvin White       94.02.08  Version 3.0.  Changed "kill" to
!                                    "pkill" and "dkill".  Changed
!                                    "high" to "phigh" and "dhigh".
!                                    Changed MAXCFL to 17.
!                                    Added "idpcp" and "idpcd".
!       Brent Archinal     95.02.21  Version 3.1.
!       Brent Archinal     97.02.12  Version 3.2.
!       "     "            98.03.23  Version 4.0.
!       "     "          1999.05.18  Version 4.1.
!       "     "          1999.08.04  Version 5.0.  Added "mk4flg".
!       "     "          2000.05.12  Version 5.1.
!       Leonid Petrov    2000.05.19  Revision 5.2. Added ephem_put, db_format,
!                                    dec_mode
!       Leonid Petrov    2000.05.19  Revision 6.0 Added NOINPUT_LCODE_FILE
!       Leonid Petrov    2001.06.15  Revision 6.6 Added SCAN_CHECK
!       Leonid Petrov    2001.06.18  Revision 6.6 Added NOINPUT_OBSER_FILE
!       Leonid Petrov    2001.06.22  Revision 6.6 Increased MAXCFL from 18 to 20
!
!     Parameters:
!
!     MAXHDR and MAXCFL are the number of header blocks and the maximum
!     number of data flags per header block that will be found in this
!     program's control file.  REVISION is, of course, the current revision
!     number of this program (actual revision number is defined in
!     dbedit_version.i )
!
      INTEGER*2  MAXHDR, MAXCFL
      PARAMETER  ( MAXHDR = 4   )
      PARAMETER  ( MAXCFL = 20  )
      INTEGER*4    IFORM, HFORM
      CHARACTER  REVISION*6
!
!     Specifications:
!
!     VERBOSE -- Flag used to control the amount of output created
!                by the program.
!     CFDCB   -- Control file unit number.
!     HEADER  -- Buffer used to keep track of control file flags.
!     CFLIST  -- List of control file flags used to initialize the
!                control file reader.
!     PKILL   -- Primary duplicate elimination criterion.
!     DKILL   -- Secondary duplicate elimination criterion.
!     PHIGH   -- Sense of primary duplicate elimination criterion.
!     DHIGH   -- Sense of secondary duplicate elimination criterion.
!     IDPCP   -- Counter for number of duplicate observations eliminated
!                by primary duplicate elimination criterion.
!     IDPCD   -- Counter for number of duplicate observations eliminated
!                by secondary duplicate elimination criterion.
!     SORT    -- Option flag specifying how the sort is done.
!     NCHANX  -- Number of X-band channels for input filter.
!     NCHANS  -- Number of S-band channels for input filter.
!     XBEG    -- Beginning of time window.  Used for input and output filters.
!     XEND    -- Ending of time window.  Used for input abd output filters.
!     INWHIST -- Input time interval window history entry.
!     OTWHIST -- Output time window history entry.
!     REJCT   -- Option flag specifying that rejected observations are to
!                be noted in the rejects file.
!     REJCTPATH -- Full pathname of rejects file.
!     REJCTWND -- Option flag specifying that time window rejects are
!                 to be included in rejects file.
!     DECALC  -- Option flag specifying what to do with CALC lcodes.
!     SKELKY  -- Keyname of skeleton database.
!     SKLVR   -- Version number of skeleton database.
!     EPHKY   -- Keyname of ephemeris database.
!     EPHVR   -- Version number of ephemeris database.
!     DUT1NM   -- Pathname to the UT1-polar motion file.
!     UT1SER  -- Series designation of UT1PM data used.
!     UPDTSS  -- Option flag specifying that the site/source catalog is
!                to be updated.
!     ALTSSC  -- Alternative site/source catalog pathname and prefix.
!     USRNAM  -- User's name, 10 characters.
!     MEDIA   -- Reference number of input data source.
!     DIRFLG  -- Flag indicating if an directory input source has ever
!                been specified.
!     IFORM   -- Number of FRNGE files in IEEE format.
!     HFORM   -- Number of FRNGE files in HP format.
!     KYNM    -- Current database keyname.  Used for both input and output.
!     IVER    -- Version of current database.
!     PATH    -- Full pathname of directory to use for disk-based data source.
!     TAPE    -- Tape driver to use for tape-based data source.
!     STAT    -- Output database status.
!     LETR    -- Character used in keyname generation for current
!                output database.
!     HIST    -- History entry used for current output database description.
!     BAND    -- Band designation of output database.
!     MK4FLG  -- Flag indicating if Mark IV correlator data is to be
!                read from a directory, as an input source.
!     EPHEM_PUT -- Flag indicating if epehemeris should be put in the
!                  output database
!     DB_FORMAT -- Format of the output database
!     DEC_MODE  -- if .TRUE. then binary interger and float data are coded in
!                  DEC mode
!
      CHARACTER PKILL*1, DKILL*1, SORT*1,SKELKY*10,USRNAM*10,KYNM*10, &
     &          PATH*80, EPHKY*10, TAPE*80, STAT_CH*1, LETR*1, HIST*80, &
     &          REJCTPATH*80, HEADER*16, INWHIST*80, OTWHIST*80, &
     &          ALTSSC*80, CBAND*2, DUT1NM*255, UT1SER*4, &
     &          CFLIST(MAXHDR,MAXCFL)*16, DB_FORMAT*8, &
     &          INPUT_LCODE_FILE*128, NOOUTPUT_LCODE_FILE*128, &
     &          NOINPUT_OBSER_FILE*128
!    #
      INTEGER*4 IDPCP, IDPCD
      INTEGER*2 NCHANX, NCHANS, MEDIA, IVER, BAND, CFDCB, &
     &          LKYNM(5), LHIST(40), SKLVR, EPHVR
      INTEGER*4 VERBOSE
      LOGICAL*1 PHIGH, DHIGH, DECALC, REJCT, REJCTWND, UPDTSS, DIRFLG, &
     &          MK4FLG, EPHEM_PUT, DEC_MODE, APRIO_PUT, UT1PM_PUT, &
     &          SCAN_CHECK
      REAL*8    XBEG, XEND
      EQUIVALENCE (KYNM,LKYNM), (LHIST,HIST), (CBAND,BAND)
!
!     Common Space:
!
      COMMON /CNTRL/ DKILL, SORT, SKELKY, USRNAM, KYNM, PATH, &
     &               TAPE, STAT_CH, LETR, EPHKY, HIST, REJCTPATH, HEADER, &
     &               INWHIST, OTWHIST, ALTSSC, DUT1NM, UT1SER, CFLIST, &
     &               NCHANX, NCHANS, MEDIA, IVER, BAND, VERBOSE, &
     &               CFDCB, SKLVR, EPHVR, DHIGH, DECALC, REJCT, &
     &               REJCTWND, UPDTSS, XBEG, XEND, DIRFLG, MK4FLG, &
     &               HFORM, IFORM, PKILL, IDPCP, IDPCD, PHIGH, &
     &               EPHEM_PUT, DB_FORMAT, DEC_MODE, REVISION, &
     &               APRIO_PUT, UT1PM_PUT, INPUT_LCODE_FILE, &
     &               NOOUTPUT_LCODE_FILE, NOINPUT_OBSER_FILE, SCAN_CHECK
!
