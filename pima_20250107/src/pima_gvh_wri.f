      SUBROUTINE PIMA_GVH_WRI ( PIM, GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GVH_WRI 
! *                                                                      *
! * ### 15-JUL-2009  PIMA_GVH_WRI  v1.2 (c)  L. Petrov  11-JUN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( VCAT__TYPE ) :: VCAT
      INTEGER*4  IUER
      INTEGER*4  M__ENV
      PARAMETER  ( M__ENV = 32 )
      CHARACTER  OUT_FMT*2, GEN_DB_NAME*10, OUTPUT_NAME*128, &
     &           STR*128, ENV_BUF(M__ENV)*128, ENV_FINAM*128, REPO*3
      INTEGER*4  IB, IE, J1, J2, N_BAND, OPCODE, IND_REP, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      OUT_FMT = 'b'
      CALL ERR_PASS ( IUER, IER )
      CALL VCAT_GET_CONF ( PIM%CONF%MKDB_VCAT_CONFIG, VCAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7671, IUER, 'PIMA_GVH_WRI', 'Error in parsing '// &
     &         'VCAT configuration file '//PIM%CONF%MKDB_VCAT_CONFIG )
           RETURN 
      END IF
      CALL CLRCH ( REPO )
      CALL GETENVAR ( 'PIMAVAR_REPO', REPO )
      IF ( ILEN(REPO) == 0 ) REPO = PIMA__VCAT_REPO_OBS 
!
      IND_REP = 0
      DO 410 J1=1,VCAT%NREPS
         IF ( VCAT%GVF_REP_NAME(J1) == REPO ) THEN
              IND_REP = J1
         END IF
 410  CONTINUE 
      IF ( IND_REP == 0 ) THEN
           CALL ERR_LOG ( 7672, IUER, 'PIMA_GVH_WRI', 'GVF repository '//REPO// &
     &         ' was not defined in the VCAT configuration file '// &
     &          PIM%CONF%MKDB_VCAT_CONFIG )
           RETURN 
      END IF
!
      IER = -1
      STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%UTC_MTAI, IER )
      GEN_DB_NAME = STR(1:4)//STR(6:7)//STR(9:10)//'_'//PIM%CONF%MKDB_SUFFIX
!
      DO 420 J2=1,4
         IF ( J2 == 1 ) THEN
              OPCODE = GVH__CRT
            ELSE
              OPCODE = GVH__APP
         END IF
!
         IF ( OUT_FMT == 'a'  .OR.  OUT_FMT == 'ab' ) THEN
              OUTPUT_NAME = VCAT%GVF_DB_DIR(IND_REP)(1:I_LEN(VCAT%GVF_DB_DIR(IND_REP)))//'/'// &
     &                      GEN_DB_NAME(1:I_LEN(GEN_DB_NAME))//'.agv'
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_WRITE_AGV ( GVH, J2, OPCODE, OUTPUT_NAME, IER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7672, IUER, 'PIMA_GVH_WRI', 'Error in '// &
     &                 'attempt to write output database file '//OUTPUT_NAME )
                   CALL EXIT ( 1 )
              END IF
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                  WRITE ( 6, '(A)' ) 'PIMA_GVH_WRI: Output file: '// &
     &                                OUTPUT_NAME(1:I_LEN(OUTPUT_NAME))
              END IF
         END IF
!
         IF ( OUT_FMT == 'b' .OR. OUT_FMT == 'ab' ) THEN
              IF ( J2 == 1 ) THEN
                   ENV_BUF(J2) = 'SYS MAN fr1   1 bgv '// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//' '// &
     &                           PIM%CONF%SESS_CODE
                   OUTPUT_NAME = VCAT%GVF_DB_DIR(IND_REP)(1:I_LEN(VCAT%GVF_DB_DIR(IND_REP)))//'/'// &
     &                           GEN_DB_NAME(1:8)//'_'// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//'_'// &
     &                           PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                           '_fr1_v001.bgv'
                 ELSE IF ( J2 == 2 ) THEN
                   ENV_BUF(J2) = 'SYS OPT fr2   1 bgv '// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//' '// &
     &                           PIM%CONF%SESS_CODE
                   OUTPUT_NAME = VCAT%GVF_DB_DIR(IND_REP)(1:I_LEN(VCAT%GVF_DB_DIR(IND_REP)))//'/'// &
     &                           GEN_DB_NAME(1:8)//'_'// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//'_'// &
     &                           PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                           '_fr2_v001.bgv'
                 ELSE IF ( J2 == 3 ) THEN
                   ENV_BUF(J2) = 'SYS MAN cl1   1 bgv '// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//' '// &
     &                           PIM%CONF%SESS_CODE
                   OUTPUT_NAME = VCAT%GVF_DB_DIR(IND_REP)(1:I_LEN(VCAT%GVF_DB_DIR(IND_REP)))//'/'// &
     &                           GEN_DB_NAME(1:8)//'_'// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//'_'// &
     &                           PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                           '_cl1_v001.bgv'
                 ELSE IF ( J2 == 4 ) THEN
                   ENV_BUF(J2) = 'SYS OPT sl1   1 bgv '// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//' '// &
     &                           PIM%CONF%SESS_CODE
                   OUTPUT_NAME = VCAT%GVF_DB_DIR(IND_REP)(1:I_LEN(VCAT%GVF_DB_DIR(IND_REP)))//'/'// &
     &                           GEN_DB_NAME(1:8)//'_'// &
     &                           PIM%CONF%MKDB_SUFFIX(1:I_LEN(PIM%CONF%MKDB_SUFFIX))//'_'// &
     &                           PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                           '_sl1_v001.bgv'
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_WRITE_BGV ( GVH, J2, GVH__CRT, OUTPUT_NAME, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7673, IUER, 'PIMA_GVH_WRI', 'Error in '// &
     &                 'attempt to write output database file '//OUTPUT_NAME )
                   CALL EXIT ( 1 )
              END IF
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_GVH_WRI: Output file: '//OUTPUT_NAME(1:I_LEN(OUTPUT_NAME))
              END IF
         END IF
 420  CONTINUE
!
      IF ( OUT_FMT == 'b' .OR. OUT_FMT == 'ab' ) THEN
           ENV_FINAM = VCAT%GVF_ENV_DIR(IND_REP)(1:I_LEN(VCAT%GVF_ENV_DIR(IND_REP)))//'/'// &
     &                 GEN_DB_NAME(1:I_LEN(GEN_DB_NAME))//'_v001.env'
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT ( 4, ENV_BUF, ENV_FINAM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7674, IUER, 'PIMA_GVH_WRI', 'Error in '// &
     &              'attempt to write output envelope file '//OUTPUT_NAME )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_GVH_WRI: Output file: '// &
     &                         ENV_FINAM(1:I_LEN(ENV_FINAM))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GVH_WRI  !#!#
