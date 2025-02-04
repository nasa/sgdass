      SUBROUTINE DBI_COMBLIST ( DBI, LT_DBS, CT_DBS, LH_DBS, CH_DBS, &
     &                          LI_DBS, CI_DBS, LG_DBS, CG_DBS, LN_DBS, CN_DBS, &
     &                          L_DBS, C_DBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DBI_COMBLIST  reads 5 lists:                              *
! *    CT_DBS -- list of database filenames in the IVS Data Center;      *
! *    CH_DBS -- list of the database filenames (with path) in the       *
! *              local catalogue system;                                 *
! *    CI_DBS -- list of the database filenames (without path) in the    *
! *              incoming directory;                                     *
! *    CG_DBS -- get_list of the database file names which user wants    *
! *              to retrieve. The elements of this list may contain      *
! *              elements with wildcard symbols;                         *
! *    CN_DBS -- noget_list of the database file names which should not  *
! *              be retrieved. The elements of this list may contain     *
! *              elements with wildcard symbols;                         *
! *   and creates list C_DBS of the databases filenames which are to be  *
! *   downloaded:                                                        *
! *        -- in the IVS Data Center database list  AND                  *
! *        -- not in the current catalogue system   AND                  *
! *        -- not in the incoming directory         AND                  *
! *        -- in the get_list                       AND                  *
! *        -- not in the noget_list.                                     *
! *                                                                      *
! *  Comments:                                                           *
! *  a) extension .gz in the database filenames in CT_DBS list is not    *
! *     taken into account;                                              *
! *  b) database version number is not taken into account when the       *
! *     database filename lists CT_DBS, CH_DBS and CI_DBS are compared,  *
! *     but it is taken into account when lists CT_DBS, CG_DBS and       *
! *     CN_DBS are compared.                                             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DBI ( RECORD    ) -- Object with data structure for keeping       *
! *                         configuration of DB_IMPORT utility.          *
! * LT_DBS ( INTEGER*4 ) -- Numbers of elements in the CT_DBS list.      *
! * CT_DBS ( CHARACTER ) -- Database filename in the IVS Data Center.    *
! *                         Dimension: LT_DBS.                           *
! * LH_DBS ( INTEGER*4 ) -- Numbers of elements in the CH_DBS list.      *
! * CH_DBS ( CHARACTER ) -- Database filename list in the local VLBI     *
! *                         database catalogue system.                   *
! *                         Dimension: LH_DBS.                           *
! * LI_DBS ( INTEGER*4 ) -- Numbers of elements in the CI_DBS list.      *
! * CI_DBS ( CHARACTER ) -- Database filename list in the incoming       *
! *                         directory.                                   *
! *                         Dimension: LT_DBS.                           *
! * LG_DBS ( INTEGER*4 ) -- Numbers of elements in get_list.             *
! * CG_DBS ( CHARACTER ) -- Get_list. Contains database filenames which  *
! *                         user wants to retrieve. It may contain       *
! *                         elements with wildcad symbol.                *
! *                         Dimension: LG_DBS.                           *
! * LN_DBS ( INTEGER*4 ) -- Numbers of elements in noget_list.           *
! * CN_DBS ( CHARACTER ) -- Noget_list. Contains database filenames      *
! *                         which user doesn't want to retrieve. It may  *
! *                         contain elements with wildcad symbol.        *
! *                         Dimension: LN_DBS.                           *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  L_DBS ( INTEGER*4 ) -- Numbers of database filenames to be          *
! *                         downloaded.                                  *
! *  C_DBS ( CHARACTER ) -- List of the database filenames which is to   *
! *                         be downloaded. Dimension: L_DBS.             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 18-OCT-2000  DBI_COMBLIST  v1.0 (c) L. Petrov  18-OCT-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  LT_DBS, LH_DBS, LI_DBS, LG_DBS, LN_DBS, L_DBS, IUER
      CHARACTER  CT_DBS(LT_DBS)*(*), CH_DBS(LH_DBS)*(*), CI_DBS(LI_DBS)*(*), &
     &           CG_DBS(LG_DBS)*(*), CN_DBS(LN_DBS)*(*), C_DBS(M_DBS)*(*)
      CHARACTER  DBT_FILE*20, DBT_NAME*9, DBH_NAME*9, DBI_NAME*9, STR*32
      INTEGER*4  IV, IG, J1, J2, J3, J4, J5
      LOGICAL*4  FL_GET, FL_NOGET, MATCH_WILD
      INTEGER*4  I_LEN
!
      L_DBS = 0
      IF ( LT_DBS .EQ. 0 ) THEN
!
! -------- If the IVS Data Center filename ist is empty then nothing to do.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Scan database fdielnames from the IVS Data Center
!
      DO 410 J1=1,LT_DBS
!
! ------ Extact DBT_NAME -- database name and
! ------        DBT_FILE -- database filename (without path and extension)
!
         CALL CLRCH ( DBT_NAME )
         CALL CLRCH ( DBT_FILE )
         DBT_NAME = CT_DBS(J1)
         DBT_FILE = CT_DBS(J1)
!
! ------ Discard extension .gz if found
!
         IG = INDEX ( DBT_FILE, '.gz' )
         IF ( IG .GT. 0 ) CALL CLRCH ( DBT_FILE(IG:) )
!
! ------ Replace "_" with blank
!
         IF ( DBT_NAME(9:9) .EQ. '_' ) DBT_NAME(9:9) = ' '
         IF ( LH_DBS .GT. 0 ) THEN
!
! ----------- Compare DBT_BAME with all datbase filenames from the list of the
! ----------- local catalogue system
!
              DO 420 J2=1,LH_DBS
!
! -------------- Only database names are compared
!
                 IV = INDEX ( CH_DBS(J2), '_V' )
                 IF ( IV .GT. 0 ) THEN
                      CALL CLRCH ( DBH_NAME )
                      DBH_NAME = CH_DBS(J2)(IV-9:IV-1)
                      IF ( DBH_NAME(9:9) .EQ. '_' ) DBH_NAME(9:9) = ' '
                      IF ( DBH_NAME .EQ. DBT_NAME ) GOTO 410 ! Coincide -- off!
                 END IF
 420          CONTINUE
         END IF
!
         IF ( LI_DBS .GT. 0 ) THEN
!
! ----------- Compare DBT_NAME with all database fileanmes in the incoming
! ----------- directory
!
              DO 430 J3=1,LI_DBS
!
! -------------- We don't take into account datbase fielnams whcih are
! -------------- compressed -- they may appear due to failure to download them
! -------------- in the previouis attempt to run DB_IMPORT
!
                 IG = INDEX ( CI_DBS(J3), '.gz' )
                 IF ( IG .LE. 0 ) THEN
!
! ------------------- Only database names are compared
!
                      CALL CLRCH ( DBI_NAME )
                      DBI_NAME = CI_DBS(J3)
                      IF ( DBI_NAME(9:9) .EQ. '_' ) DBI_NAME(9:9) = ' '
                      IF ( DBI_NAME .EQ. DBT_NAME ) GOTO 410
                 END IF
 430          CONTINUE
         END IF
!
         IF ( LG_DBS .GT. 0 ) THEN
!
! ----------- Compare DBT_FILE with all elements of get_list with using
! ----------- wildcard symbols
!
              FL_GET = .FALSE.
              DO 440 J4=1,LG_DBS
                 IF ( MATCH_WILD ( DBT_FILE, CG_DBS(J4)(1:I_LEN(CG_DBS(J4))) ) ) &
     &           FL_GET = .TRUE. ! found!
 440          CONTINUE
              IF ( .NOT. FL_GET ) GOTO 410 ! If not found -- off!
         END IF
!
         IF ( LN_DBS .GT. 0 ) THEN
!
! ----------- Compare DBT_FILE with all elements of noget_list with using
! ----------- wildcard symbols
!
              FL_NOGET = .FALSE.
              DO 450 J5=1,LN_DBS
                 IF ( MATCH_WILD ( DBT_FILE, CN_DBS(J5)(1:I_LEN(CN_DBS(J5))) ) ) &
     &           FL_NOGET = .TRUE.
 450          CONTINUE
              IF ( FL_NOGET ) GOTO 410 ! Found? -- off!
         END IF
!
! ------ All checks are over. Add the ths database fielname to the output list
!
         L_DBS = L_DBS + 1
         IF ( L_DBS .GT. M_DBS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_DBS, STR )
              CALL ERR_LOG ( 5761, IUER, 'DBI_COMBLIST', 'Too many files '// &
     &            'for downloading. Parameter M_DBS '//STR(1:I_LEN(STR))// &
     &            ' is too small' )
              RETURN
         END IF
         C_DBS(L_DBS) = CT_DBS(J1)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!             type *,' l_dbs=',l_dbs,'  c_dbs  >>',                     ! %%%%
!     #                c_dbs(l_dbs)(1:i_len(c_dbs(l_dbs))),'<<  '       ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
410   CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DBI_COMBLIST  #!#
