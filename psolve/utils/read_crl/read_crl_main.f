      PROGRAM    READ_CRL_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  READ_CRL_MAIN  reads Solve correlation file in binary     *
! *   format and writes it to the output file in ascii format.           *
! *   This program also serves as an example of manipluation with Solve  *
! *   binary corrlation files.                                           *
! *                                                                      *
! *   Usage:  read_crl  <correlation_file_name> [<output_file>]'         *
! *                                                                      *
! *      where <correlation_file_name> is a correlation file name        *
! *           in binary format. Solve wreites down correlations in file  *
! *           $WORK_DIR/CORLxx where xx are Solve user initials.         *
! *                                                                      *
! *      if output_file is omiited of it is '-' then correlations are    *
! *           written in standard output device (screen).                *
! *                                                                      *
! *  ### 18-MAY-2001 READ_CRL_MAIN v1.0 (c)  L. Petrov  31-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'corel.i'
      CHARACTER  FILSPL*128, FILOUT*128, C_PARM*20, COMMENT*80
!
      TYPE ( CORL__STRU ) ::  CORL_REC
      INTEGER*4  J1, I11, ICOND, NUMARG, IND1, IND2, STATB(12), IS, IUER
      LOGICAL*4  LEX
      REAL*8     CORR
#ifdef INTEL
      INTEGER*4, EXTERNAL :: IARGC
#endif
      INTEGER*4, EXTERNAL :: FOR_STAT, I_LEN, ILEN, READ_CRL
!
! --- Get run-time arguments
!
      CALL CLRCH ( FILSPL )
      CALL CLRCH ( FILOUT )
      NUMARG = IARGC ()
      IF ( NUMARG .GE. 1 ) THEN
           CALL GETARG ( 1, FILSPL )
           INQUIRE ( FILE=FILSPL, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 8900, -1, 'READ_CRL_MAIN', 'Tyu-u-u-u! '// &
     &              'Correlation file '//FILSPL(1:I_LEN(FILSPL))// &
     &              ' was not found' )
                CALL EXIT ( 2 )
           END IF
!
! -------- Lean file length
!
           IS = FOR_STAT ( FILSPL, STATB )
           IF ( STATB(8) .EQ. 0 ) THEN
                CALL ERR_LOG ( 8990, -1, 'READ_CRL_MAIN', 'Correlation file '// &
     &               FILSPL(1:I_LEN(FILSPL))//' has zero lenght.' )
                CALL EXIT ( 2 )
           END IF
           IF ( NUMARG .EQ. 2 ) THEN
                CALL GETARG ( 2, FILOUT )
           END IF
         ELSE
           WRITE ( 6, * ) 'Usage: read_crl  <correlation_file_name> [<output_file>]'
           CALL EXIT ( 1 )
      END IF
!
! --- Open output file
!
      IF ( ILEN(FILOUT) .GT. 0  .AND. FILOUT(1:1) .NE. '-' ) THEN
           OPEN ( UNIT=6, FILE=FILOUT, STATUS='UNKNOWN' )
      END IF
!
! --- Initialize CORL_REC data structure. INIT_CRL  checks input file,
! --- reserve space for intenral buffers and initliaze fields of CORL_REC
!
      IUER = -1
      CALL INIT_CRL ( FILSPL, CORL_REC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Write down a format label
!
      CALL CLRCH ( COMMENT )
      COMMENT = ASCII__CRL
      WRITE ( 6, '(A)' ) COMMENT(1:I_LEN(COMMENT))
      DO 410 J1=1,1024*1024*1024
!
! ------ Now read the record from correlation fiel
!
         IUER = -1
         ICOND = READ_CRL ( CORL_REC, COMMENT, C_PARM, IND1, IND2, CORR, IUER )
         IF ( IUER .NE. 0 ) THEN
              WRITE ( 6, * ) ' iuer=',iuer ! %%%
              WRITE ( 6, * ) ' J1=',J1
              CALL EXIT ( 1 )
         END IF
!
! ------ Record may one of types: COMM_CRL, NPAR__CRL, NCOR__CRL, CPAR__CRL,
! ------ CORR__CRL and END__CRL
!
         IF ( ICOND .EQ. COMM__CRL ) THEN
!
! ----------- It returned a comments line in second argument
!
              WRITE ( 6, '(A)' ) COMMENT(1:I_LEN(COMMENT))
           ELSE IF ( ICOND .EQ. CPAR__CRL ) THEN
!
! ----------- It returned parameter name in the list of correlation parameters
! ----------- in the 3-rd argument. NB: this list may be smaller than the
! ----------- Solve parameters list since correlations may be computed only
! ----------- for a subset of parameters. Therefore, the index of a parameter
! ----------- in correlation parameters list does not necessary correspond to
! ----------- the index of a parametr in Solve parameters list.
!
!              WRITE ( 6, '(A)' ) C_PARM(1:I_LEN(C_PARM))
              CONTINUE
           ELSE IF ( ICOND .EQ. NPAR__CRL ) THEN
!
! ----------- It returned the number of parameters in the 4-th argument
!
              CONTINUE
           ELSE IF ( ICOND .EQ. NCOR__CRL ) THEN
!
! ----------- It returned the total number of correlations in the 4-th argument
!
              CONTINUE
           ELSE IF ( ICOND .EQ. CORR__CRL ) THEN
!
! ----------- It returns indeces of the correlation in the 4 and 5 th argument
! ----------- in Solve argument list as well as the value of a corrlation
! ----------- in the 6-th argument. Table CORL__REC.IXREF_PAR references the
! ----------- index of a parameter in Sovle parameters list to the index
! ----------- of a parameter in the list of parameters in correlation file.
!
              WRITE ( UNIT=6, FMT=120, IOSTAT=I11 )  IND1, IND2, &
     &                CORL_REC%C_PAR(CORL_REC%IXREF_PAR(IND1)), &
     &                CORL_REC%C_PAR(CORL_REC%IXREF_PAR(IND2)), CORR
 120          FORMAT ( I5, 1X, I5, 2X, '"', A20, '"', &
     &                             2X, '"', A20, '"', 2X, F12.9 )
           ELSE IF ( ICOND .EQ. END__CRL ) THEN
!
! ----------- It returned status: end of file is reached.
!
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Now we close CORL_REC buffer. Internal buffers are closed and
! --- status "not initializes" is set.
!
      CALL CLOSE_CRL ( CORL_REC )
      END  !#!  READ_CRL_MAIN  #!#
