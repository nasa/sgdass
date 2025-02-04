      SUBROUTINE GVH_WRI_DB ( FMT, GVH, VCAT, REPO, DB_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_WRI_DB  writes a database with an astro/geo VLBI      *
! *   experiment eith in binary GVF in ascii vgosda format.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FMT     ( CHARACTER  ) -- Output database format.                    *
! *                           'a' or 'vda' or 'VDA' or '-vda' --         *
! *                               plain ascii vgosda format.             *
! *                           'b' or 'gvf' or 'GVF' or '-gvf' --         *
! *                               binary gvf format.                     *
! * GVH     ( GVH__STRU  ) -- Data structure which keeps internal        *
! *                           information related to the database of an  *
! *                           astro/geo VLBI experiment.                 *
! *                                                                      *
! * VCAT    ( VCAT__TPYE ) -- Data structure that keeps configuration of *
! *                           VLBI database catalogue.                   *
! *                                                                      *
! * REPO    ( CHARACTER  ) -- repostitary name for a binary file in GVF  *
! *                           format. The repositaries are defined in    *
! *                           vcat (VLBI catalogue configuraiton file).  *
! *                           This parameter is is ignored if the        *
! *                           output file is in vgodsa format.           *
! *                                                                      *
! * DB_NAME ( CHARACTER  ) -- meaning of this parameter depends on       *
! *                           the output file format.                    *
! *                           If the output file is in vgosda format,    *
! *                           then this parameter specifies the fill     *
! *                           path database name.                        *
! *                           If the output file is in gvf format, then
! *                           this parameter difines the database name   *
! *                           in form YYYYMMDD_S_vVVV.env or             *
! *                           YYYYMMDD-S_vVVV.env , where YYYY -- year,  *
! *                           MM -- month of the year as an interger     *
! *                           number, DD -- day of the month, S siffix   *
! *                           in a range from a to z, VVV -- an integer  *
! *                           database version. The 9-th character is    *
! *                           underscore for a observation database and  *
! *                           hyphen for a simulation database.          *
! *                           Shorten form YYYYMMDD-S_vVVV implues       *
! *                           YYYYMMDD-S_vVVV.env                        *
! *                           Short form YYYYMMDD-S implues              *
! *                           YYYYMMDD-S_v001.env                        *
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
! * ### 15-JUN-2020    GVH_WRI_DB    v1.0 (c) L. Petrov  17-JUN-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( VCAT__TYPE ) :: VCAT
      CHARACTER  FMT*(*), REPO*(*), DB_NAME*(*)
      INTEGER*4  IUER
      INTEGER*4  M__ENV
      PARAMETER  ( M__ENV = 32 )
      CHARACTER  OUT_FMT*2, FILOUT*128, &
     &           STR*128, ENV_BUF(M__ENV)*128, FULL_ENV_NAME*128, &
     &           VERS_STR*3, EXP_CODE*32, DB_SESS_CODE*10, DB_SUFFIX*1
      INTEGER*4  IB, IE, J1, J2, VERS, K_SEG, OPCODE, IND_REP, DIMS(2), IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( FMT == 'b' .OR. FMT == 'GVF ' .OR. FMT == 'gvf' .OR. FMT == '-gvf' ) THEN
           CALL CLRCH ( EXP_CODE )
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'EXP_CODE', 0, 0, LEN(EXP_CODE), &
     &                       DIMS(1), DIMS(2), %REF(EXP_CODE), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7111, IUER, 'GVH_WRI_DB', 'Error in '// &
     &              'getting lcode EXP_CODE' )
               RETURN 
           END IF
!
           IF ( ILEN(DB_NAME) == 10 ) THEN
                DB_SESS_CODE  = DB_NAME
                DB_SUFFIX = DB_NAME(10:10)
                VERS_STR  = '001'
                FULL_ENV_NAME = DB_NAME(1:10)//'_v'//VERS_STR//'.env'
              ELSE IF ( ILEN(DB_NAME) == 15 ) THEN
                VERS_STR = DB_NAME(13:15)
                DB_SESS_CODE  = DB_NAME(1:10)
                DB_SUFFIX = DB_NAME(10:10)
                FULL_ENV_NAME = DB_NAME(1:10)//'_v'//VERS_STR//'.env'
                CALL CHIN ( VERS_STR, VERS )
                IF ( DB_NAME(11:12) .NE. '_v' ) THEN
                     CALL ERR_LOG ( 7112, IUER, 'GVH_WRI_DB', 'Malformed '// &
     &                   'database name '//TRIM(DB_NAME)//' -- fields 11:12 '// &
     &                   'should be _v' )
                     RETURN 
                END IF
                IF ( VERS < 1 .OR. VERS > 999 ) THEN
                     CALL ERR_LOG ( 7113, IUER, 'GVH_WRI_DB', 'Malformed '// &
     &                   'database name '//TRIM(DB_NAME)//' -- version field '// &
     &                   'should be a positive integer number' )
                     RETURN 
                END IF
              ELSE IF ( ILEN(DB_NAME) == 19 ) THEN
                VERS_STR = DB_NAME(13:15)
                DB_SESS_CODE  = DB_NAME(1:10)
                DB_SUFFIX     = DB_NAME(10:10)
                FULL_ENV_NAME = DB_NAME(1:10)//'_v'//VERS_STR//'.env'
                CALL CHIN ( VERS_STR, VERS )
                IF ( DB_NAME(11:12) .NE. '_v' ) THEN
                     CALL ERR_LOG ( 7114, IUER, 'GVH_WRI_DB', 'Malformed '// &
     &                   'database name '//TRIM(DB_NAME)//' -- fields 11:12 '// &
     &                   'should be _v' )
                     RETURN 
                END IF
                IF ( DB_NAME(16:19) .NE. '.env' ) THEN
                     CALL ERR_LOG ( 7115, IUER, 'GVH_WRI_DB', 'Malformed '// &
     &                   'database name '//TRIM(DB_NAME)//' -- fields 16:19 '// &
     &                   'should be .env' )
                     RETURN 
                END IF
                IF ( VERS < 1 .OR. VERS > 999 ) THEN
                     CALL ERR_LOG ( 7116, IUER, 'GVH_WRI_DB', 'Malformed '// &
     &                   'database name '//TRIM(DB_NAME)//' -- version field '// &
     &                   'should be a positive integer number' )
                     RETURN 
                END IF
           END IF
!
           IND_REP = 0
           DO 410 J1=1,VCAT%NREPS
              IF ( VCAT%GVF_REP_NAME(J1) == REPO ) THEN
                   IND_REP = J1
              END IF
 410       CONTINUE 
           IF ( IND_REP == 0 ) THEN
                CALL ERR_LOG ( 7117, IUER, 'GVH_WRI_DB', 'GVF repository '// &
     &               REPO//' was not defined in the VCAT configuration '// &
     &               'file '//VCAT%CONF_FILE )
                RETURN 
           END IF
         ELSE IF ( FMT == 'a'   .OR. FMT == 'VDA'  .OR. &
     &             FMT == 'vda' .OR. FMT == '-vda'      ) THEN
           IF ( INDEX ( DB_NAME, '.vda' ) > 0 ) THEN
                FILOUT = DB_NAME
              ELSE 
                FILOUT = TRIM(DB_NAME)//'.vda'
           END IF
           GVH%FILEENV = FILOUT
      END IF
!
      K_SEG = 0
      DO 420 J2=1,GVH__MSEG
         IF ( GVH%TOCS(J2)%NTOC < 1 ) GOTO 420
         IF ( J2 == 1 ) THEN
              OPCODE = GVH__CRT
            ELSE
              OPCODE = GVH__APP
         END IF
!
         K_SEG = K_SEG + 1
         IF ( J2 == 1 .AND. FMT == 'b' .OR. FMT == 'GVF ' .OR. FMT == 'gvf' .OR. FMT == '-gvf' ) THEN
              ENV_BUF(K_SEG) = TRIM(REPO)//' MAN fr1 '//VERS_STR//' bgv '//TRIM(DB_SUFFIX)//' '//TRIM(EXP_CODE)
              FILOUT = TRIM(VCAT%GVF_DB_DIR(IND_REP))//'/'// &
     &                      DB_SESS_CODE(1:9)// &
     &                      TRIM(DB_SUFFIX)//'_'// &
     &                      TRIM(EXP_CODE)// &
     &                      '_fr1_v'//VERS_STR//'.bgv'
            ELSE IF ( J2 == 2 .AND. FMT == 'b' .OR. FMT == 'GVF ' .OR. FMT == 'gvf' .OR. FMT == '-gvf' ) THEN
              ENV_BUF(K_SEG) = TRIM(REPO)//' OPT fr2 '//VERS_STR//' bgv '//TRIM(DB_SUFFIX)//' '//TRIM(EXP_CODE)
              FILOUT = TRIM(VCAT%GVF_DB_DIR(IND_REP))//'/'// &
     &                      DB_SESS_CODE(1:9)// &
     &                      TRIM(DB_SUFFIX)//'_'// &
     &                      TRIM(EXP_CODE)// &
     &                      '_fr2_v'//VERS_STR//'.bgv'
            ELSE IF ( J2 == 3 .AND. FMT == 'b' .OR. FMT == 'GVF ' .OR. FMT == 'gvf' .OR. FMT == '-gvf' ) THEN
              ENV_BUF(K_SEG) = TRIM(REPO)//' MAN cl1 '//VERS_STR//' bgv '//TRIM(DB_SUFFIX)//' '//TRIM(EXP_CODE)
              FILOUT = TRIM(VCAT%GVF_DB_DIR(IND_REP))//'/'// &
     &                      DB_SESS_CODE(1:9)// &
     &                      TRIM(DB_SUFFIX)//'_'// &
     &                      TRIM(EXP_CODE)// &
     &                      '_cl1_v'//VERS_STR//'.bgv'
            ELSE IF ( J2 == 4 .AND. FMT == 'b' .OR. FMT == 'GVF ' .OR. FMT == 'gvf' .OR. FMT == '-gvf' ) THEN
              ENV_BUF(K_SEG) = TRIM(REPO)//' OPT sl1 '//VERS_STR//' bgv '//TRIM(DB_SUFFIX)//' '//TRIM(EXP_CODE)
              FILOUT = TRIM(VCAT%GVF_DB_DIR(IND_REP))//'/'// &
     &                      DB_SESS_CODE(1:9)// &
     &                      TRIM(DB_SUFFIX)//'_'// &
     &                      TRIM(EXP_CODE)// &
     &                      '_sl1_v'//VERS_STR//'.bgv'
            ELSE IF ( J2 == 5 .AND. FMT == 'b' .OR. FMT == 'GVF ' .OR. FMT == 'gvf' .OR. FMT == '-gvf' ) THEN
              ENV_BUF(K_SEG) = TRIM(REPO)//' OPT th1 '//VERS_STR//' bgv '//TRIM(DB_SUFFIX)//' '//TRIM(EXP_CODE)
              FILOUT = TRIM(VCAT%GVF_DB_DIR(IND_REP))//'/'// &
     &                      DB_SESS_CODE(1:9)// &
     &                      TRIM(DB_SUFFIX)//'_'// &
     &                      TRIM(EXP_CODE)// &
     &                      '_th1_v'//VERS_STR//'.bgv'
         END IF
         GVH%FILENAME(J2) = FILOUT
         IF ( FMT == 'b' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_WRITE_BGV ( GVH, J2, GVH__CRT, FILOUT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7118, IUER, 'GVH_WRI_DB', 'Error in '// &
     &                 'attempt to write the output database file '//FILOUT )
                   RETURN
              END IF
            ELSE IF ( FMT == 'a'   .OR. FMT == 'VDA'  .OR. &
     &                FMT == 'vda' .OR. FMT == '-vda'      ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_PREGET ( GVH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7119, IUER, 'GVH_WRI_DB', 'Error in an '// &
     &                 'attempt to execute GVH_PREGET' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_WRITE_AGV ( GVH, J2, OPCODE, FILOUT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7120, IUER, 'GVH_WRI_DB', 'Error in '// &
     &                 'attempt to write the output database file '//FILOUT )
                   RETURN
              END IF
         END IF
 420  CONTINUE
!
      IF ( FMT == 'b' .OR. FMT == 'GVF ' .OR. FMT == 'gvf' .OR. FMT == '-gvf' ) THEN
           GVH%FILEENV = TRIM(VCAT%GVF_ENV_DIR(IND_REP))//'/'// &
     &                 TRIM(FULL_ENV_NAME)
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT ( K_SEG, ENV_BUF, GVH%FILEENV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7121, IUER, 'GVH_WRI_DB', 'Error in '// &
     &              'attempt to write the output envelop file '//GVH%FILEENV )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVH_WRI_DB  !#!#
