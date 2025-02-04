      SUBROUTINE SOLUTION_IDENT ( IFMT, LUN, PREF, FL_GLOBAL_L1, TITLE_ANON )
! ************************************************************************
! *                                                                      *
! *     Routine SOLUTION_IDENT writes information about where, when,     *
! *   by whom and how solution was obtained at logical unit LUN.         *
! *   If IFMT .EQ. CRL__ASC then this information is written in ASCII    *
! *   format. If IFMT .EQ. CRL__BIN, them this information is written in *
! *   binary format using binio library.                                 *
! *                                                                      *
! *     Field "Analyst" will not be written if logical variable          *
! *   TITLE_ANON is .TRUE.                                               *
! *                                                                      *
! *   pet  2002.12.25  Moved definition of spool format label to         *
! *                    solve.i                                           *
! *   pet  2003.08.22  Made modifications in order to prevent memory     *
! *                    leakage.                                          *
! *                                                                      *
! * ### 04-MAY-1998  SOLUTION_IDENT  v5.4 (c) L. Petrov 28-APR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'precm.i'
      INCLUDE   'socom.i'
      INTEGER*4  IFMT
      INTEGER*4  LUN
      LOGICAL*1  TITLE_ANON, FL_GLOBAL_L1
      CHARACTER  PREF*(*)
      CHARACTER  USER_NAME*80, USER_REALNAME*80, USER_E_ADDRESS*80
      CHARACTER  SYSNAME*80, NODENAME*80, HARDWARE*80, EXEC_DIR*80, &
     &           CONTROL_FILE*128, CHAR_LEN8*128
      CHARACTER  OUT*512
      CHARACTER  GET_CDATE*19
      INTEGER*4, EXTERNAL :: UNIT_TO_FILDESC, ILEN, I_LEN
      INTEGER*4  I_LEN8
      I_LEN8(CHAR_LEN8) = 8*(I_LEN(CHAR_LEN8)/8) + &
     &                    MAX ( 8, MOD(I_LEN(CHAR_LEN8),8) )
!
! --- Getting information about user and about system
!
      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL GETINFO_SYSTEM ( SYSNAME,   NODENAME,      HARDWARE       )
      IF ( ILEN(USER_REALNAME) .EQ. 0 ) USER_REALNAME = USER_NAME
!
! --- Getting information about directory where execetubales were located
!
      CALL GETENVAR ( 'PSOLVE_DIR', EXEC_DIR )
      IF ( ILEN(EXEC_DIR) .EQ. 0 ) THEN
           EXEC_DIR = SOLVE_PROG_DIR
      END IF
!
! --- Now printing collected information
!
      CALL CLRCH ( OUT )
      WRITE ( OUT, 110 ) PREF, CENTER_ABR, CENTER_FULL_NAME
 110  FORMAT ( A,'Analysis center: ',A,' -- ',A,' ' )
      IF ( IFMT .EQ. CRL__ASC ) THEN
           WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &          OUT(1:I_LEN8(OUT)), -3 )
         ELSE
           CALL CLRCH ( OUT )
           CALL INCH  ( IFMT, OUT )
           CALL ERR_LOG ( 3733, -1, 'SOLUTION_IDENT', 'Trap of internal '// &
     &         'control: IFMT='//OUT )
      END IF
!
      IF ( .NOT. TITLE_ANON ) THEN
           WRITE ( OUT, 120 ) PREF, USER_REALNAME(1:I_LEN(USER_REALNAME)), &
     &                              USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))
 120       FORMAT ( A,'Analyst:         ',A,' ( ',A,' )' )
           IF ( IFMT .EQ. CRL__ASC ) THEN
                WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
              ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &               OUT(1:I_LEN8(OUT)), -3 )
           END IF
      END IF
!
      CALL TRAN ( 13, NODENAME, NODENAME )
      CALL TRAN ( 13, HARDWARE, HARDWARE )
      CALL TRAN ( 13, SYSNAME,  SYSNAME  )
!
      WRITE ( OUT, 130 ) PREF, NODENAME(1:I_LEN(NODENAME)), &
     &                         HARDWARE(1:I_LEN(HARDWARE)), &
     &                         SYSNAME(1:I_LEN(SYSNAME))
 130  FORMAT ( A,'Machine:         ',A,'  ',A,'  ',A )
      IF ( IFMT .EQ. CRL__ASC ) THEN
           WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &          OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
      WRITE ( OUT, 140 ) PREF, EXEC_DIR(1:I_LEN(EXEC_DIR))
 140  FORMAT ( A,'Executables:     ',A )
      IF ( IFMT .EQ. CRL__ASC ) THEN
           WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &          OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
      WRITE ( OUT, 150 ) PREF, PRE_LETRS
 150  FORMAT ( A,'Solve initials:  ',A )
      IF ( IFMT .EQ. CRL__ASC ) THEN
           WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &          OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
      IF ( KBATCH ) THEN
           WRITE ( OUT, 160 ) PREF, SOLUID_CHR(1:I_LEN(SOLUID_CHR))
           IF ( IFMT .EQ. CRL__ASC ) THEN
                WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
              ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &               OUT(1:I_LEN8(OUT)), -3 )
           END IF
      END IF
 160  FORMAT ( A,'Solution ID:     ',A )
!
      IF ( KBATCH ) THEN
           CALL GET_CONTROL_FILE ( CONTROL_FILE )
           WRITE ( OUT, 170 ) PREF, CONTROL_FILE(1:I_LEN(CONTROL_FILE))
         ELSE
           WRITE ( OUT, 170 ) PREF, 'Interactive solution'
      END IF
 170  FORMAT ( A,'Control file:    ',A )
!
      WRITE ( OUT, 180 ) PREF, SPOOL__FMT
 180  FORMAT ( A,'Spool format:    ',A )
      IF ( IFMT .EQ. CRL__ASC ) THEN
           WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &          OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
! --- These two sections of code are equivalent. But the HP FORTRAN90 2.5.1
! --- leaks memory in executing the previous statement
!
!@      OUT = PREF//'Local time:      '//GET_CDATE()
      WRITE ( OUT, 190 ) PREF, GET_CDATE()
 190  FORMAT ( A,'Local time:      ',A )
!
      IF ( IFMT .EQ. CRL__ASC ) THEN
           WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
           CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &          OUT(1:I_LEN8(OUT)), -3 )
      END IF
!
      IF ( .NOT. FL_GLOBAL_L1 ) THEN
!
! -------- This session-dependent stuff is not relevant global solutions
!
           WRITE  ( OUT, 1100 ) PREF, CORRTYPE
 1100      FORMAT ( A,'Correlator type: ',A )
           IF ( IFMT .EQ. CRL__ASC ) THEN
                WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
              ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), &
     &               OUT(1:I_LEN8(OUT)), -3 )
           END IF
!
           IF ( EXPSERNO .GT. 0 ) THEN
                WRITE ( OUT, 1110 ) PREF, EXP_NUM
 1110           FORMAT ( A,'Serial number:   ',I5 )
                IF ( IFMT .EQ. CRL__ASC ) THEN
                     WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
                  ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                     CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), OUT(1:I_LEN8(OUT)), &
     &                                   -2 )
                END IF
           END IF
!
           IF ( ILEN(EXP_CODE) .GT. 0 ) THEN
                WRITE ( OUT, 1120 ) PREF, EXP_CODE(1:I_LEN(EXP_CODE))
 1120           FORMAT ( A,'Experiment code: ',A )
                IF ( IFMT .EQ. CRL__ASC ) THEN
                     WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
                  ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                     CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), OUT(1:I_LEN8(OUT)), &
     &                                   -2 )
                END IF
           END IF
!
           IF ( ILEN(ENV_FINAM) .GT. 0 ) THEN
                WRITE ( OUT, 1130 ) PREF, MK3_DBNM(1:I_LEN(MK3_DBNM))
 1130           FORMAT ( A,'Mark-3 db_name:  ',A )
              ELSE 
                WRITE ( OUT, 1130 ) PREF, DBNAME_CH(1:I_LEN(DBNAME_CH))
           END IF
           IF ( IFMT .EQ. CRL__ASC ) THEN
                 WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
               ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), OUT(1:I_LEN8(OUT)), &
     &                              -2 )
           END IF
!
           IF ( ILEN(EXP_DESC) .GT. 0 ) THEN
                WRITE ( OUT, 1140 ) PREF, EXP_DESC(1:I_LEN(EXP_DESC))
 1140           FORMAT ( A,'Experiment desc: ',A )
                IF ( IFMT .EQ. CRL__ASC ) THEN
                     WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
                  ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                     CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), OUT(1:I_LEN8(OUT)), &
     &                                   -2 )
                END IF
           END IF
!
           IF ( ILEN(REC_MODE) .GT. 0 ) THEN
                WRITE ( OUT, 1150 ) PREF, REC_MODE(1:I_LEN(REC_MODE))
 1150           FORMAT ( A,'Recording mode:  ',A )
                IF ( IFMT .EQ. CRL__ASC ) THEN
                     WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
                  ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                     CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), OUT(1:I_LEN8(OUT)), &
     &                                   -2 )
                END IF
           END IF
!
           IF ( ILEN(PI_NAME) .GT. 0 ) THEN
                WRITE ( OUT, 1160 ) PREF, PI_NAME(1:I_LEN(PI_NAME))
 1160           FORMAT ( A,'Agency/PI name:  ',A )
                IF ( IFMT .EQ. CRL__ASC ) THEN
                     WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
                  ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                     CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), OUT(1:I_LEN8(OUT)), &
     &                                   -2 )
                END IF
           END IF
!
           IF ( ILEN(CORRELATOR_NAME ) .GT. 0 ) THEN
                WRITE ( OUT, 1170 ) PREF, &
     &                              CORRELATOR_NAME(1:I_LEN(CORRELATOR_NAME))
 1170           FORMAT ( A,'Correlator:      ',A )
                IF ( IFMT .EQ. CRL__ASC ) THEN
                     WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
                  ELSE IF ( IFMT .EQ. CRL__BIN ) THEN
                     CALL WRBIN_STRING ( UNIT_TO_FILDESC(LUN), OUT(1:I_LEN8(OUT)), &
     &                                   -2 )
                END IF
           END IF
      END IF
!
      RETURN
      END  !#!  SOLUTION_IDENT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_CONTROL_FILE ( STR )
! ************************************************************************
! *                                                                      *
! *   Auxiliary  routine  GET_CONTROL_FILE copies control file from      *
! *   glbc4 to the output string OUT.                                    *
! *                                                                      *
! * ###  07-OCT-99  GET_CONTROL_FILE  v1.0 (c) L. Petrov  07-OCT-99  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
      CHARACTER  STR*(*)
      CALL CLRCH ( STR )
      STR = CONTROL_FILE
      RETURN
      END  !#!  GET_CONTROL_FILE  #!#
