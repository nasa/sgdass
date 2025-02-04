      PROGRAM    SPOOL_BITE
! ************************************************************************
! *                                                                      *
! *   Program  SPOOL_BITE is for extraction of a small portion from      *
! *   Solve spool file. Since Solve spool files are very large, they may *
! *   have more than a million lines, a set of tools is necessary for    *
! *   working with such kind of files. SPOOL_BITE is one of the tools.   *
! *   It "bites" the file and returns a portion.                         *
! *                                                                      *
! *   Usage:  spool_bite spool_file (-d <database> | -glo | -bas |       *
! *           -ove |  -sou ) [-o <output_file>]                          *
! *                                                                      *
! *     where spool_file -- is the name of the spool file which Solve    *
! *   generated. Then one of the option should follow:                   *
! *                                                                      *
! *     -d <database>  -- extract the portion of spool file with listing *
! *                       for this database. Database name should be in  *
! *                       the same syntax as it appears in arc-file.     *
! *                                                                      *
! *     -glo           -- section of the spool file with the estimates   *
! *                       of global parameters will be extracted and     *
! *                       written to the output file.                    *
! *                                                                      *
! *     -bas           -- section of the spool file with baseline        *
! *                       statistics will be extracted and written into  *
! *                       the output file.                               *
! *                                                                      *
! *     -ove           -- section of the spool file with overall         *
! *                       statistics will be extracted and written into  *
! *                       the output file.                               *
! *                                                                      *
! *     -sou           -- section of the spool file with source          *
! *                       statistics will be extracted and written into  *
! *                       the output file.                               *
! *                                                                      *
! *     -o <output_file>  -- sets the name of the output file. If the    *
! *                          output_file is omitted then SPOOL_BITE      *
! *                          write its output in /tmp/out.spl . It the   *
! *                          output filename is '-' then correlations    *
! *                          are written in standard output device       *
! *                          (screen).                                   *
! *                                                                      *
! *  ### 29-SEP-2000   SPOOL_BITE  v1.6 (c)  L. Petrov  31-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MARG
      PARAMETER  ( MARG = 10 )
      INTEGER*4  NARG, IO, NOUT, LUN, M_PRE, N_PRE, J1, J2, J3, J4
      PARAMETER  ( M_PRE = 32 )
      LOGICAL*4  LEX, GL_WAS,  RQ_GLO, RQ_OVE, RQ_BAS, RQ_SOU, RQ_DBS, &
     &           FL_NEXT_SKIP, FL_GLO, FL_OVE, FL_BAS, FL_SOU, FL_DBS, &
     &           FL_USER_PROG, FL_USER_BUFF, FL_PART_PROG, FL_RUN
      CHARACTER  FILSPL*128, CARG(MARG)*128, FILOUT*128, DBNAME*10, STR*256, &
     &           PRE(M_PRE)*256
      INTEGER*4  IARGC, ILEN, I_LEN
!
      INCLUDE 'spool_bite_version.i' ! Set revision date of the current version
!
! --- Initialization
!
      CALL CLRCH ( FILSPL )
      CALL CLRCH ( DBNAME )
      CALL CLRCH ( FILOUT )
      RQ_GLO = .FALSE.
      RQ_BAS = .FALSE.
      RQ_OVE = .FALSE.
      RQ_SOU = .FALSE.
      RQ_DBS = .FALSE.
      FILOUT = '/tmp/out.spl'
!
      NARG = IARGC()
!
! --- Parse arguments line
!
      IF ( NARG .LT. 2 ) THEN
           WRITE ( 6, * ) 'spool_bite spool_file (-d <database> '// &
     &            '| -g | -bas | -ove | -sou ) [-o <output_file>]'
           CALL EXIT ( 1 )
         ELSE
!
! -------- Get all arguments and put them into array CARR
!
           DO 410 J1=1,MARG
              CALL CLRCH ( CARG(J1) )
              IF ( J1 .LE. NARG ) THEN
                   CALL GETARG ( J1, CARG(J1) )
              END IF
 410       CONTINUE
!
           FILSPL=CARG(1)
!
! -------- Parse argument list, starting from the second argument
!
           FL_NEXT_SKIP = .FALSE.
           DO 420 J2=2,MIN(NARG,MARG)
              IF ( FL_NEXT_SKIP ) THEN
                   FL_NEXT_SKIP = .FALSE.
                   GOTO 420
              END IF
!
              IF ( CARG(J2)(1:2) .EQ. '-g' ) THEN
                   RQ_GLO = .TRUE.
                 ELSE IF ( CARG(J2)(1:4) .EQ. '-bas' ) THEN
                   RQ_BAS = .TRUE.
                 ELSE IF ( CARG(J2)(1:4) .EQ. '-ove' ) THEN
                   RQ_OVE = .TRUE.
                 ELSE IF ( CARG(J2)(1:4) .EQ. '-sou' ) THEN
                   RQ_SOU = .TRUE.
                 ELSE IF ( CARG(J2)(1:2) .EQ. '-d' ) THEN
                   RQ_DBS = .TRUE.
                   IF ( J2 .EQ. MARG ) THEN
                        WRITE ( 7, '(A)' ) 'spool_bite.e: too many switches'
                        CALL EXIT ( 1 )
                   END IF
                   DBNAME=CARG(J2+1)
                   IF ( ILEN(DBNAME) .EQ. 0 ) THEN
                        WRITE ( 7, '(A)' ) 'spool_bite.e: database name '// &
     &                                     'must be specified after -d switch'
                        CALL EXIT ( 1 )
                   END IF
                   IF ( DBNAME(1:1) .NE. '$' ) DBNAME = '$'//DBNAME
                   FL_NEXT_SKIP = .TRUE.
                 ELSE IF ( CARG(J2)(1:2) .EQ. '-o' ) THEN
                   IF ( J2 .EQ. MARG ) THEN
                        WRITE ( 7, '(A)' ) 'spool_bite.e: too many switches'
                        CALL EXIT ( 1 )
                   END IF
!
                   FILOUT=CARG(J2+1)
                   IF ( ILEN(FILOUT) .EQ. 0 ) THEN
                        WRITE ( 7, '(A)' ) 'spool_bite.e: output fiel name '// &
     &                                     'must be specified after -o switch'
                        CALL EXIT ( 1 )
                   END IF
                   FL_NEXT_SKIP = .TRUE.
                 ELSE
                   WRITE ( 7, '(A)' ) 'Switch '//CARG(J2)(1:I_LEN(CARG(J2)))// &
     &                                ' is not supported'
                   CALL EXIT ( 1 )
              END IF
 420       CONTINUE
      END IF
!
! --- Check whether the spool file exists
!
      INQUIRE ( FILE=FILSPL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           WRITE ( 7, '(A)' ) 'Spool file '//FILSPL(1:I_LEN(FILSPL))// &
     &                       ' was not found'
           CALL EXIT ( 1 )
      END IF
!
! --- Open spool file
!
      OPEN ( UNIT=11, FILE=FILSPL, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           WRITE ( 7, '(A,I5,A)' ) 'Error ',IO,' in opening spool file '// &
     &                              FILSPL(1:I_LEN(FILSPL))
           CALL EXIT ( 1 )
      END IF
      IF ( FILOUT(1:1) .EQ. '-' ) THEN
           LUN = 6
         ELSE
           LUN = 22
!
! -------- Open output file
!
           OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IO )
           IF ( IO .NE. 0 ) THEN
                WRITE ( 7, '(A,I5,A)' ) 'Error ',IO,' in opening output file '// &
     &                             FILOUT(1:I_LEN(FILOUT))
                CALL EXIT ( 1 )
           END IF
      END IF
      WRITE ( LUN, '(A)' ) '# excerpt from '//FILSPL(1:I_LEN(FILSPL))
!
      NOUT   = 1
      N_PRE  = 0
      GL_WAS = .FALSE.
      FL_GLO = .FALSE.
      FL_OVE = .FALSE.
      FL_BAS = .FALSE.
      FL_SOU = .FALSE.
      FL_DBS = .FALSE.
      FL_RUN = .FALSE.
!
      FL_USER_PROG = .FALSE.
      FL_USER_BUFF = .FALSE.
      FL_PART_PROG = .FALSE.
!
! --- Infintie loop for reading spool file
!
      DO 430 J3=1,99999999
         READ ( UNIT=11, FMT='(A)', IOSTAT=IO ) STR
         IF ( IO .EQ. -1 ) THEN
              GOTO 830
            ELSE IF ( IO .NE. 0 ) THEN
              WRITE ( 7, '(A,I5,A,I7,A)' ) 'Error ',IO,' in reading the ',J3, &
     &               '-th line of the spool file '//FILSPL(1:I_LEN(FILSPL))
              CALL EXIT ( 1 )
         END IF
!
! ------ Print line counter
!
         IF ( LUN .NE. 6 ) THEN
              IF ( MOD(J3,10000) .EQ. 0 ) THEN
                   WRITE ( 6, FMT='("    line ",I7,"   ",A$)' ) J3, CHAR(13)
              END IF
         END IF
!
! ------ Gather informatuion to preabmle
!
         IF ( STR(1:4) .EQ. '1Run' ) THEN
              IF ( FL_RUN ) N_PRE = 0
              FL_RUN = .TRUE.
         END IF
         IF ( STR(1:7) .EQ. 'ENTER :' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:12) .EQ. 'Run done on ' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:15) .EQ. 'Run started at ' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:3) .EQ. 'ID ' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:10) .EQ. 'USER_PROG:' ) THEN
              IF ( .NOT. FL_USER_PROG ) THEN
                   N_PRE = N_PRE + 1
                   PRE(N_PRE) = STR
                   FL_USER_PROG = .TRUE.
             END IF
         END IF
!
         IF ( STR(1:10) .EQ. 'USER_BUFF:' ) THEN
              IF ( .NOT. FL_USER_BUFF ) THEN
                   N_PRE = N_PRE + 1
                   PRE(N_PRE) = STR
                   FL_USER_BUFF = .TRUE.
              END IF
         END IF
!
         IF ( STR(1:15) .EQ. 'USER_PART_PROG:' ) THEN
              IF ( .NOT. FL_PART_PROG ) THEN
                   N_PRE = N_PRE + 1
                   PRE(N_PRE) = STR
                   FL_PART_PROG = .TRUE.
              END IF
         END IF
!
         IF ( STR(1:17) .EQ. ' Analysis center:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:9) .EQ. ' Analyst:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:9) .EQ. ' Machine:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:13) .EQ. ' Executables:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:16) .EQ. ' Solve initials:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:13) .EQ. ' Solution ID:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:14) .EQ. ' Spool format:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
         IF ( STR(1:12) .EQ. ' Local time:' ) THEN
              N_PRE = N_PRE + 1
              PRE(N_PRE) = STR
         END IF
!
! ------ Look for signatures in the spool file
!
         IF ( .NOT. GL_WAS  .AND.  STR(1:16) .EQ. ' ---------------' ) THEN
              FL_GLO = .TRUE.
              GL_WAS = .TRUE.
         END IF
         IF ( STR(1:4) .EQ. '1Run' ) THEN
              FL_GLO = .FALSE.
!
! ----------- Check: isn't it time to stop scanning spool file?
!
              IF (      RQ_GLO  .AND. &
     &            .NOT. RQ_DBS  .AND. &
     &            .NOT. RQ_BAS  .AND. &
     &            .NOT. RQ_OVE  .AND. &
     &            .NOT. RQ_SOU         ) GOTO 830
              IF (  RQ_DBS       .AND. &
     &              FL_DBS       .AND. &
     &             .NOT. RQ_BAS  .AND. &
     &             .NOT. RQ_OVE  .AND. &
     &             .NOT. RQ_SOU        ) GOTO 830
         END IF
         IF ( STR(1:4) .EQ. '1Ove' ) THEN
              FL_OVE = .TRUE.
         END IF
         IF ( FL_OVE .AND. STR(1:4) .EQ. '1Bas' ) THEN
              FL_OVE = .FALSE.
              FL_BAS = .TRUE.
         END IF
         IF ( STR(1:4) .EQ. '1Sou' ) THEN
              FL_BAS = .FALSE.
              FL_SOU = .TRUE.
         END IF
         IF ( STR(1:14) .EQ. '1  CGM Summary' ) THEN
              FL_SOU = .FALSE.
         END IF
         IF ( RQ_DBS ) THEN
              IF ( STR(1:10) .EQ. ' Data base' .AND. &
     &             STR(12:21) .EQ. DBNAME ) THEN
                   FL_DBS = .TRUE.
              END IF
         END IF
!
         IF ( ( RQ_GLO .AND.  FL_GLO ) .OR. &
     &        ( RQ_DBS .AND.  FL_DBS ) .OR. &
     &        ( RQ_BAS .AND.  FL_BAS ) .OR. &
     &        ( RQ_OVE .AND.  FL_OVE ) .OR. &
     &        ( RQ_SOU .AND.  FL_SOU )      ) THEN
!
              NOUT = NOUT + 1
              IF ( NOUT .EQ. 2  .AND.  N_PRE .GT. 0 ) THEN
                   WRITE ( LUN, '(A)' ) ' '
                   NOUT = NOUT + 1
                   DO 440 J4=1,N_PRE
                      WRITE ( LUN, '(A)' ) PRE(J4)(1:I_LEN(PRE(J4)))
                      NOUT = NOUT + 1
 440               CONTINUE
                   WRITE ( LUN, '(A)' ) ' '
                   NOUT = NOUT + 1
              END IF
!
              WRITE ( LUN, '(A)' ) STR(1:I_LEN(STR))
         END IF
 430  CONTINUE
 830  CONTINUE
      CLOSE ( UNIT=11 )
      CLOSE ( UNIT=LUN )
      IF ( LUN .NE. 6 ) THEN
           WRITE ( 6, '(I7,A)' ) NOUT,' lines are written in the output file '// &
     &                           FILOUT(1:I_LEN(FILOUT))
      END IF
      END  !#!  SPOOL_BITE  #!#
