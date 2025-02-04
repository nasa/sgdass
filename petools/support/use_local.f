      PROGRAM    USE_LOCAL
! ************************************************************************
! *                                                                      *
! *   Program USE_LOCAL creates the output file on the basis the input   *
! *   template file and input local file. 2 formats are supported:       *
! *   FORTRAN and csh (C-shell). USE_LOCAL copies contents of the        *
! *   template file to the output file except meta-commands. If it       *
! *   finds met-commands -- lines starting from character @ it looks     *
! *   for local file and substitutes the value of parameters defined in  *
! *   meta-command by values defined in local file.                      *
! *                                                                      *
! *   USE_LOCAL is for smart customization. Template file contains       *
! *   generic data. Local file contains local preferences.               *
! *                                                                      *
! *   Format of meta-commands which can be specified in template files:  *
! *                                                                      *
! *   @<type_of_the_value><max_length>@ <parameter_name>                 *
! *                                                                      *
! *     where <type_of_the_value> is                                     *
! *               V for non-character values                             *
! *               C for character values                                 *
! *           <max_length> - maximal length of the value (in characters) *
! *                          Must be specified for values of character   *
! *                          type. Should not be specified for           *
! *                          non-character types of values.              *
! *                          If the actual length of the value excluding *
! *                          delimiters exceeds max_length then an error *
! *                          message is generated unless max_length = 0  *
! *                          (max_length=0 allows arbitrary length of    *
! *                          the values).                                *
! *          <parameter_name> - name of the parameter.                   *
! *                                                                      *
! *   Local file consist of lines with definitions and comments.         *
! *   Lines starting with non-blank symbol are considered as comments.   *
! *   Format of definitions lines is                                     *
! *     <parameter_name> = <parameter_value>                             *
! *                                                                      *
! *   USE_LOCAL first parses local file. Then  it scans template file.   *
! *   If it finds a meta-command then it finds parameter name in local   *
! *   file and generates the output command:                             *
! *                                                                      *
! *   a) PARAMETER ( <parameter_name> = <parameter_value> )              *
! *                                                                      *
! *   b) CHARACTER <parameter_name>*length                               *
! *      PARAMETER ( <parameter_name> = <parameter_value> )              *
! *                                                                      *
! *   c) set <parameter_name> = <parameter_value>                        *
! *                                                                      *
! *   in according with type of the value and the current format.        *
! *                                                                      *
! * Usage: use_local <template_file> <local_file> <format> <output_file> *
! *                                                                      *
! *  Format should be a string either FORTRAN or csh                     *
! *                                                                      *
! *  Example of template file:                                           *
! *                                                                      *
! *   &V&  MAX_PAR                                                       *
! *   &C3& CENTER_ABR                                                    *
! *   &C0& DELOG_DIR                                                     *
! *                                                                      *
! *  Example of a local file:                                            *
! *                                                                      *
! *      MAX_PAR    = 16384                                              *
! *      CENTER_ABR = 'GSF'                                              *
! *      DELOG_DIR  = '/box1/wxcb/'                                      *
! *                                                                      *
! * ### 22-MAY-2000     USE_LOCAL   v1.2 (c) L. Petrov  03-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MBUF, MPAR
      PARAMETER  ( MBUF = 4096 )
      PARAMETER  ( MPAR =  256 )
      CHARACTER  TEMPL_FILE*128, LOCAL_FILE*128, OUTPUT_FILE*128, FMT_OUT*8
      CHARACTER  BUFT(MBUF)*128, BUFL(MBUF)*128, BUFO(MBUF)*128, TYP*1, &
     &           PARTEMPL*128, PARNAM(MPAR)*128, PARVAL(MPAR)*128, &
     &           PARUSED(MPAR)*128, PARMISSED(MPAR)*128, STR*16, COM*1, &
     &           TEMP_STR*11
      INTEGER*4  NBUF_TEMPL, NBUF_LOCAL, NUMARG, LOUT, J1, J2, J3, J4, IO, &
     &           IERR, IB, CLEN_BEG, CLEN_END, CLEN, NPAR, NUSED, NMISSED, &
     &           LERR, IPAR, IP, I22
#ifdef HPUX
      PARAMETER  ( LERR = 7 )
#else
      PARAMETER  ( LERR = 6 )
#endif
      LOGICAL*4  WAS_BLANK
      INTEGER*4  IARGC
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, FIND_ELEM
!
! --- Parse a command line
!
      NUMARG = IARGC ()
      IF ( NUMARG .EQ. 4 ) THEN
           CALL GETARG ( 1, TEMPL_FILE  )
           CALL GETARG ( 2, LOCAL_FILE  )
           CALL GETARG ( 3, FMT_OUT     )
           CALL GETARG ( 4, OUTPUT_FILE )
        ELSE
           WRITE ( LERR, FMT='(A)' ) ' Usage: use_local <template_file> '// &
     &            '<local_file> <format> <output_file>'
           CALL EXIT_PROG ( 1 )
      END IF
      IF ( FMT_OUT .EQ. 'FORTRAN'  .OR.  FMT_OUT .EQ. 'csh' ) THEN
         ELSE
           WRITE ( LERR, 110 ) FMT_OUT(1:I_LEN(FMT_OUT))
 110       FORMAT ( 'CREATE_TEMPLATE: wrong format: ',A,' specified. '/ &
     &              'Only FORTRAN or csh are supported' )
           CALL EXIT_PROG ( 2 )
      END IF
!
! --- Reading template file
!
      CALL READ_FILE ( TEMPL_FILE, MBUF, BUFT, NBUF_TEMPL, IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( LERR, '(A)' ) 'USE_LOCAL: Error in reading template '// &
     &             'file '//TEMPL_FILE(1:I_LEN(TEMPL_FILE))
           CALL EXIT_PROG ( 3 )
      END IF
!
! --- Reading local file
!
      CALL READ_FILE ( LOCAL_FILE, MBUF, BUFL, NBUF_LOCAL, IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( LERR, '(A)' ) 'USE_LOCAL: Error in reading template '// &
     &             'file '//LOCAL_FILE(1:I_LEN(LOCAL_FILE))
           CALL EXIT_PROG ( 4 )
      END IF
!
! --- Parse local file
!
      CALL PARSE_LOCAL ( LOCAL_FILE, NBUF_LOCAL, BUFL, MPAR, NPAR, PARNAM, &
     &                   PARVAL )
!
! --- Writing preabmle
!
      IF ( FMT_OUT .EQ. 'FORTRAN' ) THEN
           COM = '!'
           BUFO(1) = '! '
         ELSE IF ( FMT_OUT .EQ. 'csh' ) THEN
           COM = '#'
           BUFO(1) = '#!/bin/csh'
      END IF
!
      BUFO(2) = COM//' This file is generated automatically by '// &
     &          'use_local.f from '
      BUFO(3) = COM//'      template_file: '//TEMPL_FILE(1:I_LEN(TEMPL_FILE))
      BUFO(4) = COM//'      local customization file: '// &
     &                  LOCAL_FILE(1:I_LEN(LOCAL_FILE))
      BUFO(5) = COM//' '
      BUFO(6) = COM//'      PLEASE DON''T EDIT THIS FILE! '
      BUFO(7) = COM//'      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
      BUFO(8) = COM//' '
      LOUT = 8
!
! --- Start reading template file
!
      NUSED = 0
      NMISSED = 0
      DO 410 J1=1,NBUF_TEMPL
         IF ( BUFT(J1)(1:1) .EQ. '&'  .AND.  BUFT(J1)(1:2) .NE. '& ' ) THEN
!
! ----------- This is a metacommand
!
              CLEN     = 0
              CLEN_BEG = 0
              CLEN_END = 0
              WAS_BLANK = .FALSE.
              IB = 0
!
! ----------- Parse a metacommand: find ther type, max-length, position where
! ----------- the field of parameter name starts
!
              DO 420 J2=2,ILEN(BUFT(J1))
                 IF ( BUFT(J1)(J2:J2) .EQ. 'V' .AND. .NOT. WAS_BLANK ) THEN
                      TYP = 'V'
                    ELSE IF ( BUFT(J1)(J2:J2) .EQ. 'C'  .AND. &
     &                        .NOT. WAS_BLANK ) THEN
                      TYP = 'C'
                      CLEN_BEG = J2+1 ! save initial position of max-length
                    ELSE IF ( BUFT(J1)(J2:J2) .EQ. '&'  .AND. &
     &                        .NOT. WAS_BLANK ) THEN
                      CLEN_END = J2-1 ! save the final position of max-length
                      IF ( TYP .EQ. 'C' ) THEN
!
! ------------------------ Extract max_length
!
                           IF ( CLEN_END .LT. CLEN_BEG ) THEN
                                WRITE ( LERR, 110 ) J1, &
     &                                     TEMPL_FILE(1:I_LEN(TEMPL_FILE)), &
     &                                     BUFT(J1)(1:I_LEN(BUFT(J1)))
 120                            FORMAT ( 'CREATE_INCLDE: error at the line ', &
     &                                    I4,' of template file '/A/A/ &
     &                                    ' Length specificator after type', &
     &                                    ' code has not been supplied' )
                                CALL EXIT_PROG ( 5 )
                           END IF
                           TEMP_STR = '           '
                           TEMP_STR = BUFT(J1)(CLEN_BEG:CLEN_END)
                           READ ( TEMP_STR, '(I11)', IOSTAT=IO ) CLEN
                           IF ( IO .NE. 0 ) THEN
                                WRITE ( LERR, 120 ) J1, &
     &                                     TEMPL_FILE(1:I_LEN(TEMPL_FILE)), &
     &                                     BUFT(J1)(1:I_LEN(BUFT(J1)))
 130                            FORMAT ( 'CREATE_INCLDE: error at the line ',I4, &
     &                              ' of template file '/A/A/ &
     &                              ' Wrong format of length specificator ', &
     &                              'after the type code has not been ', &
     &                              'supplied' )
                                CALL EXIT_PROG ( 6 )
                           END IF
                      END IF
                    ELSE IF ( BUFT(J1)(J2:J2) .EQ. ' ' .OR. &
     &                        BUFT(J1)(J2:J2) .EQ. CHAR(9) ) THEN
                      WAS_BLANK = .TRUE.
                    ELSE
                      IF ( WAS_BLANK ) THEN
                           IB = J2
                           GOTO 820
                      END IF
                 END IF
 420          CONTINUE
 820          CONTINUE
              IF ( IB .EQ. 0 ) GOTO 410
              IF ( IB .EQ. ILEN(BUFT(J1)) ) THEN
                   WRITE ( LERR, 140 ) J1, TEMPL_FILE(1:I_LEN(TEMPL_FILE)), &
     &                              BUFT(J1)(1:I_LEN(BUFT(J1)))
 140               FORMAT ( 'CREATE_INCLDE: error at the line ',I4, &
     &                      ' of template file '/A/A/ &
     &                      ' No parameter name has been supplied' )
                   CALL EXIT_PROG ( 7 )
              END IF
!
! ----------- Extract parameter name in the template file
!
              CALL CLRCH ( PARTEMPL )
              PARTEMPL = BUFT(J1)(IB:)
!
! ----------- Find the index of the parameter in the PARNAM whcih keeps the
! ----------- parameter names taken from local file
!
              IPAR = FIND_ELEM ( NPAR, PARNAM, PARTEMPL )
              IF ( IPAR .LE. 0 ) THEN
!
! ---------------- We didn't find? Schade... Well, put the parameter name
! ---------------- in the list of missed parameter names.
!
                   NMISSED = NMISSED + 1
                   CALL CLRCH ( PARMISSED(NMISSED) )
                   PARMISSED(NMISSED) = PARTEMPL
                   GOTO 410
              END IF
!
! ----------- Now look: have we use this parameter? Has it been already defined
! ----------- in template file?
!
              IP = FIND_ELEM ( NUSED, PARUSED, PARTEMPL )
              IF ( IP .GT. 0 ) THEN
                   WRITE ( LERR, 150 ) PARTEMPL(1:I_LEN(PARTEMPL)), &
     &                              LOCAL_FILE(1:I_LEN(LOCAL_FILE))
 150               FORMAT ( 'USE_LOCAL: Parameter ',A,' was find twice ', &
     &                      'in the local file '/A )
                   CALL EXIT_PROG ( 8 )
              END IF
!
! ----------- WEll. Now ttime came to make a substitution
!
              IF ( TYP .EQ. 'V' ) THEN
!
! ---------------- Non-character data.
!
                   LOUT = LOUT + 1
                   BUFO(LOUT) = '      PARAMETER ( '// &
     &                          PARTEMPL(1:I_LEN(PARTEMPL))// &
     &                          ' = '//PARVAL(IPAR)(1:I_LEN(PARVAL(IPAR)))// &
     &                          ' ) ! Local customization'
                 ELSE IF ( TYP .EQ. 'C' ) THEN
!
! ---------------- Character data
!
                   IF ( CLEN .GT. 0 ) THEN
!
! --------------------- Check the length of th string
!
                        IF ( ILEN(PARVAL(IPAR))-2 .GT. CLEN ) THEN
                             WRITE ( LERR, 160 ) PARTEMPL(1:I_LEN(PARTEMPL)), &
     &                                        LOCAL_FILE(1:I_LEN(LOCAL_FILE)), &
     &                                     PARVAL(IPAR)(1:I_LEN(PARVAL(IPAR))), &
     &                                     CLEN
 160                         FORMAT ( 'USE_LOCAL: Parameter ',A, &
     &                                ' in local file '/A/ &
     &                                ' is too long: ',A/ &
     &                                ' Longer than the limit of ',I3, &
     &                                ' characters' )
                             CALL EXIT_PROG ( 9 )
                        END IF
                   END IF
!
! ---------------- Check starting and final delimters
!
                   IF ( PARVAL(IPAR)(1:1) .NE. '"' .AND. &
     &                  PARVAL(IPAR)(1:1) .NE. "'"       ) THEN
!
                        WRITE ( LERR, 170 ) PARTEMPL(1:I_LEN(PARTEMPL)), &
     &                                   LOCAL_FILE(1:I_LEN(LOCAL_FILE)), &
     &                                   PARVAL(IPAR)(1:I_LEN(PARVAL(IPAR)))
 170                    FORMAT ( 'USE_LOCAL: Parameter ',A, &
     &                           ' in local file '/A/ &
     &                           ' is of character type, but its first', &
     &                           ' symbol is neither " nor ''  :'/A )
                        CALL EXIT_PROG ( 10 )
                   END IF
!
                   IF ( PARVAL(IPAR)(1:1) .NE. &
     &                  PARVAL(IPAR)(I_LEN(PARVAL(IPAR)):I_LEN(PARVAL(IPAR))) ) &
     &                  THEN
!
                        WRITE ( LERR, 180 ) PARTEMPL(1:I_LEN(PARTEMPL)), &
     &                                   LOCAL_FILE(1:I_LEN(LOCAL_FILE)), &
     &                                   PARVAL(IPAR)(1:I_LEN(PARVAL(IPAR)))
 180                    FORMAT ( 'USE_LOCAL: Parameter ',A, &
     &                           ' in local file '/A/ &
     &                           ' is of character type, but its last', &
     &                           ' delimiter is not the same'/ &
     &                           ' as the first delimiter: '/A )
                        CALL EXIT_PROG ( 11 )
                   END IF
!
                   IF ( FMT_OUT .EQ. 'FORTRAN' ) THEN
!
! --------------------- Fortran mode. Two lines: the first line with
! --------------------- definition of the variable with actual length
!
                        LOUT = LOUT + 1
                        WRITE ( UNIT=STR, FMT='(I11)' ) ILEN(PARVAL(IPAR))-2
                        CALL CHASHL ( STR )
                        BUFO(LOUT) = '      CHARACTER '// &
     &                                PARTEMPL(1:I_LEN(PARTEMPL))//'*'// &
     &                               STR(1:I_LEN(STR))
                   END IF
!
                   IF ( FMT_OUT .EQ. 'FORTRAN' ) THEN
!
! --------------------- The second line of FOTRAN mode is the definition
! --------------------- itself
!
                        LOUT = LOUT + 1
                        BUFO(LOUT) = '      PARAMETER ( '// &
     &                             PARTEMPL(1:I_LEN(PARTEMPL))// &
     &                             ' = '//PARVAL(IPAR)(1:I_LEN(PARVAL(IPAR)))// &
     &                             ' ) ! Local customization'
                      ELSE IF ( FMT_OUT .EQ. 'csh' ) THEN
!
! --------------------- C-shell mode
!
                        LOUT = LOUT + 1
                        BUFO(LOUT) = 'set '// &
     &                             PARTEMPL(1:I_LEN(PARTEMPL))// &
     &                             ' = '//PARVAL(IPAR)(1:I_LEN(PARVAL(IPAR)))// &
     &                             ' # Local customization'
                   END IF
              END IF
            ELSE
!
! ----------- Non-meta command. Mererly copy oit in the output file
!
              LOUT = LOUT + 1
              BUFO(LOUT) = BUFT(J1)
         END IF
 410  CONTINUE
!
      IF ( NMISSED .GT. 0 ) THEN
!
! -------- We have missed parameters. Generate an error message
!
           WRITE ( LERR, 190 ) NMISSED, TEMPL_FILE(1:I_LEN(TEMPL_FILE)), &
     &                       LOCAL_FILE(1:I_LEN(LOCAL_FILE))
 190       FORMAT ( 'USE_LOCAL: ',I4,' parameters defined in template ', &
     &              'file '/A/ &
     &              'were not found in the local file '/A )
!
! -------- .. print a list of missed parameters
!
           DO 430 J3=1,NMISSED
              WRITE ( LERR, '(A)' ) '   '//PARMISSED(J3)(1:I_LEN(PARMISSED(J3)))
 430       CONTINUE
           WRITE ( LERR, 1000 ) LOCAL_FILE(1:I_LEN(LOCAL_FILE))
 1000      FORMAT ( 1X/'Look at definitions of new parameters in ', &
     &              '$PETOOLS_ROOT/release/history.lcl'/ &
     &              1X/'and update your local customization file ',A )
           CALL EXIT_PROG ( 13 )
      END IF
!
! --- Open the output file ...
!
      OPEN ( UNIT=22, FILE=OUTPUT_FILE, STATUS='UNKNOWN', IOSTAT=I22 )
      IF ( I22 .NE. 0 ) THEN
           WRITE ( LERR, 1110 ) OUTPUT_FILE(1:I_LEN(OUTPUT_FILE))
 1110      FORMAT ( 'USE_LOCAL: Error in attempt to open output file ', &
     &               A )
           CALL EXIT_PROG ( 12 )
      END IF
!
! --- ... and write down the output to the output file
!
      DO 440 J4=1,LOUT
         WRITE ( 22, FMT='(A)', IOSTAT=I22 ) BUFO(J4)(1:I_LEN(BUFO(J4)))
         IF ( I22 .NE. 0 ) THEN
              WRITE ( LERR, 1120 ) OUTPUT_FILE(1:I_LEN(OUTPUT_FILE))
 1120         FORMAT ( 'USE_LOCAL: Error in wrinting into output file ', &
     &               A )
              CALL EXIT_PROG ( 13 )
         END IF
 440  CONTINUE
      CLOSE ( UNIT=22 )
!
! --- That's all
!
      CALL EXIT_PROG ( 0 )
      END  !#!  USE_LOCAL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARSE_LOCAL ( INPUT_FILE, NBUF, BUFL, MPAR, NPAR, PARNAM, &
     &                         PARVAL )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_LOCAL parses local file with definitions in the      *
! *   format <parameter_name> = <parameter_value> and fills arrays       *
! *   PARNAM and PARVAL.                                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * INPUT_FILE ( CHARACTER ) -- File name of the input file.             *
! *       NBUF ( INTEGER*4 ) -- Number of lines in the input file.       *
! *       BUFL ( CHARACTER ) -- Character arrays whcih keeps content     *
! *                             of the input file. Length of the array:  *
! *                             NBUF.                                    *
! *       MPAR ( INTEGER*4 ) -- Maximal allowed number of parameters.    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       NPAR ( INTEGER*4 ) -- Number of parameters found in local      *
! *                             file.                                    *
! *     PARNAM ( CHARACTER ) -- Arrays of parameters name. Length of the *
! *                             array: NBUF.                             *
! *     PARVAL ( CHARACTER ) -- Arrays of parameters values. Length of   *
! *                             the array: NBUF.                         *
! *                                                                      *
! *  ### 24-MAY-2000  PARSE_LOCAL  v1.1 (c)  L. Petrov  02-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NBUF, MPAR, NPAR
      CHARACTER  INPUT_FILE*(*), BUFL(NBUF)*(*), PARNAM(MPAR)*(*), &
     &           PARVAL(MPAR)*(*)
      CHARACTER  TAB*1
      PARAMETER  ( TAB = CHAR(9) )
      INTEGER*4  LERR
#ifdef HPUX
      PARAMETER  ( LERR = 7 )
#else
      PARAMETER  ( LERR = 6 )
#endif
      EXTERNAL   ILEN
      INTEGER*4  J1, IP, IELEM, FIND_ELEM, ILEN, I_LEN
!
      NPAR = 0
      DO 410 J1=1,NBUF
         IF ( BUFL(J1)(1:1) .NE. ' ' ) GOTO 410
         IF ( ILEN(BUFL(J1)) .EQ. 0  ) GOTO 410
         CALL CHASHL ( BUFL(J1) )
         IP = INDEX ( BUFL(J1), '=' )
         IF ( IP .LE. 0 ) THEN
              WRITE ( LERR, 210 ) J1, INPUT_FILE(1:I_LEN(INPUT_FILE)), &
     &                             BUFL(J1)(1:I_LEN(BUFL(J1)))
 210          FORMAT ( 'USE_LOCAL: error at the line ',I4, &
     &                 ' of local file ',A/A/ &
     &                 ' No = character was found' )
              CALL EXIT_PROG ( 101 )
            ELSE IF ( IP .EQ. 1 ) THEN
              WRITE ( LERR, 220 ) J1, INPUT_FILE(1:I_LEN(INPUT_FILE)), &
     &                             BUFL(J1)(1:I_LEN(BUFL(J1)))
 220          FORMAT ( 'USE_LOCAL: error at the line ',I4, &
     &                 ' of local file ',A/A/ &
     &                 ' Character is the first character of the line' )
              CALL EXIT_PROG ( 102 )
         END IF
         NPAR = NPAR + 1
         PARNAM(NPAR) = BUFL(J1)(1:IP-1)
!
         IELEM = FIND_ELEM ( NPAR-1, PARNAM, BUFL(J1)(1:IP-1) )
         IF ( IELEM .GT. 0 ) THEN
              WRITE ( LERR, 230 ) BUFL(J1)(1:IP-1), &
     &                         INPUT_FILE(1:I_LEN(INPUT_FILE))
 230          FORMAT ( 'USE_LOCAL: Parameter ',A,' was find twice ', &
     &                 'in the local file '/A )
              CALL EXIT_PROG ( 103 )
         END IF
         CALL CLRCH ( BUFL(J1)(1:IP) )
         CALL CHASHL ( BUFL(J1) )
         PARVAL(NPAR) = BUFL(J1)(1:)
         IF ( ILEN(PARVAL(NPAR)) .LE. 0 ) THEN
              WRITE ( LERR, 240 ) J1, INPUT_FILE(1:I_LEN(INPUT_FILE)), &
     &                             BUFL(J1)(1:I_LEN(BUFL(J1)))
 240          FORMAT ( 'CREATE_INCLDE: error at the line ',I4, &
     &                 ' of local file ',A/A/ &
     &                 ' Empty value of the parameter ' )
              CALL EXIT_PROG ( 104 )
         END IF
 410  CONTINUE
      RETURN
      END  !#!  PARSE_LOCAL  #!#
!
! ---------------------------------------------------------------------------
!
#if defined INTEL
      FUNCTION   IARGC ()
      IMPLICIT   NONE
      INTEGER*4  IARGC, IARGC_
      IARGC = IARGC_()
      RETURN
      END  FUNCTION   IARGC
#endif
!
! ---------------------------------------------------------------------------
!
#if defined INTEL || defined SUN
      SUBROUTINE EXIT_PROG ( IARG_I4 )
      IMPLICIT   NONE
      INTEGER*4  IARG_I4
      CALL EXIT_ ( IARG_I4 )
      RETURN
      END  SUBROUTINE EXIT_PROG
#endif
!
! ---------------------------------------------------------------------------
!
#if defined GNU || defined SUN
      SUBROUTINE EXIT_PROG ( IARG_I4 )
      IMPLICIT   NONE
      INTEGER*4  IARG_I4
      CALL EXIT ( IARG_I4 )
      RETURN
      END  SUBROUTINE EXIT_PROG
#endif
!
! ---------------------------------------------------------------------------
!
#if defined HPUX
      SUBROUTINE EXIT_PROG ( IARG_I4 )
      IMPLICIT   NONE
      INTEGER*4  IARG_I4
      CALL EXIT ( IARG_I4 )
      RETURN
      END  SUBROUTINE EXIT_PROG
#endif
!
! ------------------------------------------------------------------------
!
#if defined INTEL
      SUBROUTINE GETARG ( IPAR, CPAR )
      INTEGER*4  IPAC
      CHARACTER  CPAR*(*)
      CALL GETARG_ ( IPAR, CPAR )
      RETURN
      END  SUBROUTINE  GETARG !#!#
#endif
      
