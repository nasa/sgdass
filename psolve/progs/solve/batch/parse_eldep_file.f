      SUBROUTINE PARSE_ELDEP_FILE ( FILNAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_ELDEP_FILE parses the file in ELDEP format that      *
! *   specifies additive elevation-dependent re-weighting parameters     *
! *   for the list of stations.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FILNAM ( CHARACTER ) -- Name of the file with elevation-dependent   *
! *                          re-weighting parameters for the list of     *
! *                          stations.                                   *
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
! * ### 03-DEC-2007 PARSE_ELDEP_FILE v1.0 (c) L. Petrov  03-DEC-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'glbc3.i'
      CHARACTER  FILNAM*128
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 1024 )
      PARAMETER  ( MIND =   32 )
      CHARACTER  BUF(MBUF)*128, ELDEP__LABEL*36, REG*3
      PARAMETER  ( ELDEP__LABEL = 'ELDEP  Format version of 2007.12.03 ' )
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER  STR*128
      REAL*8     SIG
      LOGICAL*4  LEX
      INTEGER*4  NBUF, IND(2,MIND), LIND, J1, J2, IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=FILNAM, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8161, IUER, 'PARSE_ELDEP_FILE', 'Cannot find '// &
     &         'the file with elevation- and station- dependent '// &
     &         'reweighting parameters '//FILNAM )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILNAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8162, IUER, 'PARSE_ELDEP_FILE', 'Error in reading '// &
     &         'the file with elevation- and station- dependent '// &
     &         'reweighting parameters '//FILNAM )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(ELDEP__LABEL)) == ELDEP__LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 8163, IUER, 'PARSE_ELDEP_FILE', 'Wrong format of '// &
     &         'the file with elevation- and station- dependent '// &
     &         'reweighting parameters '//FILNAM(1:I_LEN(FILNAM))// &
     &         ' the first line is '//BUF(1)(1:I_LEN(BUF(1)))//' but '// &
     &         ELDEP__LABEL//' was expected' )
           RETURN 
      END IF
!
      CALL USE_GLBFIL_3 ( 'OR' ) 
      CALL NOUT_R8 ( INT4(MAX_STA), RWT_EL_STA )
      N_RWT_STA  = 0
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         CALL VTD_NAME_REPAIR ( BUF(J1)(1:8) )
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
!@U         CALL UNDSCR ( BUF(J1)(1:8) )
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.6)', IOSTAT=IER ) SIG
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8164, IUER, 'PARSE_ELDEP_FILE', 'Error in '// &
     &            'reading the file with elevation- and station- dependent '// &
     &            'reweighting parameters '//FILNAM(1:I_LEN(FILNAM))// &
     &            ' -- failure to parse line '//STR(1:I_LEN(STR))// &
     &            ' word2: '//BUF(J1)(IND(1,2):IND(2,2))//' a REAL*8 '// &
     &            'number was expected' )
              RETURN 
         END IF
!
         IF ( ISTASP > 0 ) THEN
              DO 420 J2=1,ISTASP
                 IF ( STASUP(J2) == BUF(J1)(1:8) ) THEN
                      CALL SBIT ( CMPSUP, INT2(J2), INT2(RWT_EL_STA__CNB) )
                      RWT_EL_STA(J2) = SIG
                      GOTO 420
                 END IF
 420          CONTINUE 
         END IF
         ISTASP = ISTASP + 1
         IF ( ISTASP > MAX_STA ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(MAX_STA), STR )
              CALL ERR_LOG ( 8165, IUER, 'PARSE_ELDEP_FILE', 'Trap of '// &
     &            'internal control: ISTASP from glbc3.i exceeded '// &
     &            'MAX_STA '//STR )
              RETURN 
         END IF
         STASUP(ISTASP) = BUF(J1)(1:8) 
         CALL SBIT ( CMPSUP(1,RWT_EL_STA__CNB), ISTASP, INT2(1) )
         RWT_EL_STA(ISTASP) = SIG
 410  CONTINUE 
!
      CALL USE_GLBFIL_3 ( 'WC' ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PARSE_ELDEP_FILE  !#!#
