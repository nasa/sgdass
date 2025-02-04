!
! === Control file for REPAB
! 
!
! --- SECTION: STATUS OF LEAFING THROUGH BASELINE PAGES
!
      CHARACTER  REPAB__LABEL*38
      PARAMETER  ( REPAB__LABEL = 'REPA Control file.  Format version of ' )
! 
! --- SECTION: DATABASE
! --- data structure for REPA parameters (one record per database)
!
      TYPE      REPA_EXP
         CHARACTER IVS_CODE*6                 ! IVS code of current experiment
         INTEGER*4 BAND_NUM                   ! # of loaded bands
         CHARACTER BAND_NAME(MAX_DBS)*13      ! database filenames of loaded bands of experiment
         CHARACTER BAND_KIND(MAX_DBS)*1       ! kind of bands (X or S)
         INTEGER*4 BAND_RECS(MAX_DBS,2)       ! # of first and last record in residual work file
      END TYPE  REPA_EXP
!
! --- SECTION: BUTTONS
! --- max. number of buttons per property page is 10 (including 'next page', 'exit')
! --- in case of adding new buttons change the following numbers and arrays
!
      INTEGER*4  PAGES_M                             ! # of property pages
      PARAMETER  ( PAGES_M = 3 )
      INTEGER*4  MPB_NUM                             ! max. # of button names
      PARAMETER  ( MPB_NUM = 10 )
      INTEGER*4  PAG_NUM(PAGES_M)                    ! buttons per property page (# of nonempty elements)
      DATA PAG_NUM /9,8,8/
!
! --- multi_diagi button names - property pages
!
      CHARACTER  BUT_MAIN(MPB_NUM,PAGES_M)*80             ! set of button names
      DATA BUT_MAIN /'Delay Residuals ', &        ! #1  (page 1)
     &               'Rate Residuals ', &         ! #2  (page 1)
     &               'Del. Residuals vs. Elev. St.1', &     ! #3  (page 1)
     &               'Del. Residuals vs. Elev. St.2', &     ! #4  (page 1)
     &               'Del. Residuals vs. Azim. St.1', &     ! #5  (page 1)
     &               'Del. Residuals vs. Azim. St.2', &     ! #6  (page 1)
     &               'Next Property Page', &              ! #7  (page 1)
     &               'Band Page (not available)', &       ! #8  (page 1)
     &               'Return to OPTIN (Exit)', &          ! #9  (page 1)
     &               ' ', &                               ! #10 (page 1)
!
     &               'Delay Residuals (corsig)', &        ! #1  (page 2)
     &               'Rate Residuals (corsig)', &         ! #2  (page 2)
     &               'SNR vs. Elevations St.1', &         ! #3  (page 2)
     &               'SNR vs. Elevations St.2', &         ! #4  (page 2)
     &               'Phase Delays', &                    ! #5  (page 2)
     &               'Next Property Page', &              ! #6  (page 2)
     &               'Band Page (not available)', &       ! #7  (page 2)
     &               'Return to OPTIN (Exit)', &          ! #8  (page 2)
     &               ' ', &                               ! #9  (page 2)
     &               ' ', &                               ! #10 (page 2)
!
     &               'Temperature', &                     ! #1  (page 3)
     &               'Pressure', &                        ! #2  (page 3)
     &               'Humidity', &                        ! #3  (page 3)
     &               'Cable Calibrations', &              ! #4  (page 3)
     &               'Group Ionos. corrections', &        ! #5  (page 3)
     &               'Next Property Page', &              ! #6  (page 3)
     &               'Band Page (not available)', &       ! #7  (page 3)
     &               'Return to OPTIN (Exit)', &          ! #8  (page 3)
     &               ' ', &                               ! #9  (page 3)
     &               ' ' &                                ! #10 (page 3)
     &              /
!
! --- multi_diagi button short cuts
!
      CHARACTER  BUT_SHORT(MPB_NUM,PAGES_M)*1
      DATA BUT_SHORT /'1','2','3','4','5','6','N','B','E',' ', &
     &                '1','2','3','4','5','N','B','E',' ',' ', &
     &                '1','2','3','4','5','N','B','E',' ',' '/
!
! --- property codes
!
      CHARACTER  PROPER(MPB_NUM,PAGES_M)*6       ! property codes
      DATA PROPER /'DELFUL', &                   ! #1  (page 1) 'Gr.Delay Residuals (fulsig)'
     &             'RATFUL', &                   ! #2  (page 1) 'Rate Residuals (fulsig)'
     &             'GDELE1', &                   ! #3  (page 1) 'Gr.Del.(fulsig) vs. Elev. St.1'
     &             'GDELE2', &                   ! #4  (page 1) 'Gr.Del.(fulsig) vs. Elev. St.2'
     &             'GDELA1', &                   ! #5  (page 1) 'Gr.Del.(fulsig) vs. Azim. St.1'
     &             'GDELA2', &                   ! #6  (page 1) 'Gr.Del.(fulsig) vs. Azim. St.2'
     &             'NEXTPG', &                   ! #7  (page 1) 'Next Property Page'
     &             'CHBAND', &                   ! #8  (page 1) 'Band Page'
     &             'EXIT  ', &                   ! #9  (page 1) 'Return to OPTIN (Exit)
     &             ' ', &                        ! #10 (page 1)
!
     &             'DELCOR', &                   ! #1  (page 2) 'Gr.Delay Residuals (corsig)'
     &             'RATCOR', &                   ! #2  (page 2) 'Rate Residuals (corsig)'
     &             'SNELE1', &                   ! #3  (page 2) 'SNR vs. Elevations St.1'
     &             'SNELE2', &                   ! #4  (page 2) 'SNR vs. Elevations St.2'
     &             'PHADEL', &                   ! #5  (page 2) 'Phase Delays'
     &             'NEXTPG', &                   ! #6  (page 2) 'Next Property Page'
     &             'CHBAND', &                   ! #7  (page 2) 'Band Page'
     &             'EXIT  ', &                   ! #8  (page 2) 'Return to OPTIN (Exit)'
     &             ' ', &                        ! #9  (page 2)
     &             ' ', &                        ! #10 (page 2)
!
     &             'TEMPER', &                   ! #1  (page 3) 'Temperature'
     &             'PRESSU', &                   ! #2  (page 3) 'Pressure'
     &             'HUMIDI', &                   ! #3  (page 3) 'Humidity'
     &             'CABLED', &                   ! #4  (page 3) 'Cable Calibrations'
     &             'GRIONO', &                   ! #5  (page 3) 'Gr. Ionos. correc. errors.'
     &             'NEXTPG', &                   ! #6  (page 3) 'Next Property Page'
     &             'CHBAND', &                   ! #7  (page 3) 'Band Page'
     &             'EXIT  ', &                   ! #8  (page 3) 'Return to OPTIN (Exit)'
     &             ' ',      &                   ! #9  (page 3)
     &             ' '       &                   ! #10 (page 3)
     &            /
!
! --- abbreviations for solve data types
!
!%%%
      CHARACTER  IDATYP_ABBR(19)*16              ! data types
      DATA IDATYP_ABBR /' GRPRAT  (csg) #', &
     &                  ' PHSRAT  (csg) #', &
     &                  ' SNBRAT  (csg) #', &
     &                  ' GRPONL  (csg) #', &
     &                  ' PHSONL  (csg) #', &
     &                  ' SNBONL  (csg) #', &
     &                  ' RATONL  (csg) #', &
     &                  ' G_GXS   (csg) #', &
     &                  ' PX_GXS  (csg) #', &
     &                  ' PS_GXS  (csg) #', &
     &                  ' PS_GX   (csg) #', &
     &                  ' PX_GS   (csg) #', &
     &                  ' PS_GX   (csg) #', &
     &                  ' PS_GS   (csg) #', &
     &                  ' P_PXS   (csg) #', &
     &                  ' GX      (csg) #', &
     &                  ' GS      (csg) #', &
     &                  ' PX      (csg) #', &
     &                  ' PS      (csg) #'  &
     &                 /
!
! --- SECTION: PLOTS
!
      INTEGER*4  PPPL_MAX                     ! max. # of plots per page (MultiDiaGi)
      PARAMETER  ( PPPL_MAX = 36 )            ! prefere square numbers
      INTEGER*4  M_CLR                        ! # of plot colours including zero/sigma lines
      PARAMETER  ( M_CLR = 9 )                ! and connecting lines
!
! --- SECTION: USER PARAMETER FILE
!
      CHARACTER  COL_KEY(M_CLR)*11            ! colour keywords for parameter file
      DATA COL_KEY / 'COLOUR_GOOD', 'COLOUR_RECV', 'COLOUR__BAD', &
     &               'COLOUR_ZERO', 'COLOUR_SIG1', 'COLOUR_SIG2', &
     &               'COLOUR_CONN', 'COLOUR_MET1', 'COLOUR_MET2' /
      INTEGER*4  COL_ATR(M_CLR)               ! colour attributes
      DATA COL_ATR / 1, 3, 13, 14, 29, 29, 21, 22, 20 /  ! default values
!
      CHARACTER  COL_NAM(M_CLR)*10            ! colour names
      DATA COL_NAM / 'green','red','black','grey','grey', &
     &               'grey','purple','yellow','green'/   ! default values
!
      CHARACTER  STY_KEY(M_CLR)*11            ! point style keywords for parameter file
      DATA STY_KEY / 'PTNSTY_GOOD', 'PTNSTY_RECV', 'PTNSTY__BAD', &
     &               'PTNSTY_ZERO', 'PTNSTY_SIG1', 'PTNSTY_SIG2', &
     &               'PTNSTY_CONN', 'PTNSTY_MET1', 'PTNSTY_MET2' /
      INTEGER*4  PNT_STY(M_CLR)               ! point styles
      DATA PNT_STY / 5, 5, 5, 1, 1, 1, 3, 5, 5 /         ! default values
!
      CHARACTER  BAD_KEY*11                   ! show "bad" observations keyword for parameter file
      DATA BAD_KEY / 'DISPLAY_BAD' /
      LOGICAL*4     SHOW_BAD                  ! flag for bad observations display (yes=.TRUE.)
      DATA SHOW_BAD / .TRUE. /                           ! default value
!
      CHARACTER  NUM_KEY*11                   ! max. # of plots per page (MultiDiaGi)
      DATA NUM_KEY / 'NUMBER_PLOT' /
      INTEGER*4  PPPL_M                       ! max. # of plots per page (MultiDiaGi)
!                                             ! (not greater than PPPL_MAX !!)
      DATA PPPL_M / 25 /                      ! default value
!
      CHARACTER  PAG_KEY(2)*11                ! previous/next baseline keywords for parameter file
!                                             ! Prefere PgUp/PgDn keybord buttons!
!                                             ! (other keys are possible)
!                                             ! Codes can be different on different computers!!
!                                             ! Use the program diagi_key to find the codes.
      DATA PAG_KEY / 'PREV_____BL', 'NEXT_____BL' /
      INTEGER*4 PAGE_UPDN(2)                  ! decimal codes of PgUp/PgDn keybord buttons
      DATA PAGE_UPDN / 221, 220 /             ! default values (dec. codes of PgUp/PgDn keys)
!
! --- SECTION: OBSERVATIONS
!
      INTEGER*4  BOBS_MAX                     ! max. # observations per baseline
!C    PARAMETER  ( BOBS_MAX = 1000 )
      PARAMETER  ( BOBS_MAX = 500 )           ! note! 24 * 60min/3min = 480 scans in 24h
      INTEGER*4  BASL_MAX                     ! max. # of baselines
      PARAMETER  ( BASL_MAX = 300 )
!
!------------------------------------------------------------------------------
! --- SECTION: USER FUNCTION BUTTONS AND KEYS
!
      INTEGER*4  FUNC_NUM                     ! max. # of user function buttons
      PARAMETER  ( FUNC_NUM = 20 )
      CHARACTER  FUNC_BUTT(FUNC_NUM)*8        ! button names of user functions
!
! --- !! Do not change the order !!
!
      DATA FUNC_BUTT / 'SHIFT-DN', &             ! (01) single shift down
     &                 'SUP--REC', &             ! (02) single suppress/recover
     &                 'SHIFT-UP', &             ! (03) single shift up
     &                 'NORMMODE', &             ! (04) normal mode
     &                 '--INFO--', &             ! (05) get information
     &                 'GR-SUPPR', &             ! (06) group suppress
     &                 'GR-RECOV', &             ! (07) group recover
     &                 'SHIFT-GR', &             ! (08) group shift
     &                 'PNT-CONN', &             ! (09) connecting lines
     &                 '--INIT--', &             ! (10) disconnect
     &                 'INP-CONN', &             ! (11) connecting lines (user input)
     &                 'AMBRESET', &             ! (12) ambiguity reset
     &                 ' ',' ',' ',' ',' ',' ',' ',' ' /
!
      CHARACTER  FUNC_KEY(FUNC_NUM)*1         ! keys for user functions
      DATA FUNC_KEY /',', &                      ! (01) single shift down
     &               '#', &                      ! (02) single suppress/recover
     &               '.', &                      ! (03) single shift up
     &               'z', &                      ! (04) normal mode
     &               '<', &                      ! (05) get information
     &               '0', &                      ! (06) group suppress
     &               '1', &                      ! (07) group recover
     &               '*', &                      ! (08) group shift
     &               '+', &                      ! (09) connecting lines
     &               '-', &                      ! (10) disconnect
     &               '~', &                      ! (11) connecting lines (user input)
     &               '%', &                      ! (12) ambiguity reset
     &               ' ',' ',' ',' ',' ',' ',' ',' ' /
!
! --- SECTION: HELPFILE
!
      CHARACTER  HELPFILE*8
      DATA HELPFILE / 'repa.hlp' /
