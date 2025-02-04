      SUBROUTINE INIT_CRL ( FILSPL, CORL_REC, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  INIT_CRL  initilaizes data structure CORL_REC. It      *
! *   also checks whether the file FILSPL is a valid correlation file    *
! *   in binary correlation format.                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    FILSPL ( CHARACTER ) -- Filename of the correlation file. It is   *
! *                            assumed that the correlation file has     *
! *                            been created by Solve using qualifier     *
! *                            BINARY.                                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  CORL_REC ( RECORD    ) -- Data structure which keeps internal       *
! *                            buffers for parsing correlation file.     *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 18-MAY-2001    INIT_CRL    v1.0 (c) L. Petrov  01-JUN-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'corel.i'
      CHARACTER  FILSPL*(*)
      INTEGER*4  IUER
!
      TYPE ( CORL__STRU ) ::  CORL_REC
      CHARACTER  STR*80
      LOGICAL*4  LEX
      INTEGER*4  IO, IC, LEN_FR, LEN_CORL
      PARAMETER  ( LEN_FR = LEN(BINARY__CRL)+4 ) ! Length of the first record
      INTEGER*4  UNIT_TO_FILDESC, GET_UNIT, I_LEN, READ
!
! --- Initialization of the record
!
      LEN_CORL = LOC(CORL_REC%LAST_MARKER) - LOC(CORL_REC%FIRST_MARKER) + 4
      CALL NOUT ( LEN_CORL, CORL_REC )
!
! --- Set status: undefined and save correlation file
!
      CORL_REC%STATUS = UND__CRL
      CORL_REC%FILSPL = FILSPL
!
! --- Check file
!
      INQUIRE ( FILE=FILSPL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8901, IUER, 'INIT_CRL', 'Correlation file '// &
     &          FILSPL(1:I_LEN(FILSPL))//' was not found' )
           RETURN
      END IF
!
! --- Get Fortran physical unit and open the correlation file
!
      CORL_REC%LUN = GET_UNIT ()
      OPEN ( UNIT=CORL_REC%LUN, FILE=FILSPL, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 8902, IUER, 'INIT_CRL', 'Error '//STR(1:I_LEN(STR))// &
     &         'in attempt to open file with correlation coefficents '//FILSPL )
           RETURN
      END IF
!
! --- Read the forst physical record of the correlation file
!
      IC = READ ( %VAL(UNIT_TO_FILDESC(CORL_REC%LUN)), %REF(STR), %VAL(LEN_FR) )
      IF ( IC .GT. 0  .AND. IC .LT. LEN_FR ) THEN
           CALL ERR_LOG ( 8903, IUER, 'INIT_CRL', 'File '// &
     &          FILSPL(1:I_LEN(FILSPL))//' is not in Binary CRL_SPOOL Format '// &
     &         'since it is too short' )
           CLOSE ( UNIT=CORL_REC%LUN )
           RETURN
      END IF
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 8904, IUER, 'INIT_CRL', 'Error "'// &
     &          STR(1:I_LEN(STR))//'" in attempt to read the first physical '// &
     &         'record of the file with correlation coefficents '//FILSPL )
           CLOSE ( UNIT=CORL_REC%LUN )
           RETURN
      END IF
!
! --- Check whether the first physical record contains the format label
!
      IF ( STR(5:LEN_FR) .NE. BINARY__CRL ) THEN
!           WRITE ( 6, '(A)' ) ' FIRST_RECORD: >>',STR(5:len_fr),'<<' ! %%%
           CALL ERR_LOG ( 8905, IUER, 'INIT_CRL', 'File '// &
     &          FILSPL(1:I_LEN(FILSPL))//' is not in Binary CRL_SPOOL format' )
           CLOSE ( UNIT=CORL_REC%LUN )
           RETURN
      END IF
!
! --- All checks are done. Set status: initialized
!
      CORL_REC%STATUS = INIT__CRL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INIT_CRL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CLOSE_CRL ( CORL_REC )
! ************************************************************************
! *                                                                      *
! *   Routine  CLOSE_CRL  closes correlation file in binary format and   *
! *   sets status of CORL_REC recrd "undefined".                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  CORL_REC ( RECORD    ) -- Data structure which keeps internal       *
! *                            buffers for parsing correlation file.     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 18-MAY-2001   CLOSE_CRL   v1.0 (c)  L. Petrov  18-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'corel.i'
!
      TYPE ( CORL__STRU ) ::  CORL_REC
!
      CLOSE ( UNIT=CORL_REC%LUN )
      CORL_REC%L_BUF = 0
      CALL CLRCH ( CORL_REC%FILSPL )
      CORL_REC%STATUS = UND__CRL
!
      RETURN
      END  !#!  CLOSE_CRL #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION READ_CRL ( CORL_REC, COMMENT, C_PARM, IND1, IND2, CORR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_CRL  reads the next portion of correlation file in   *
! *   Solve binary format and returns some piece of information. Type    *
! *   and meaning of READ_CRL depends on previous calls and contents of  *
! *   the itself. READ_CRL returns completion code. Its value points on  *
! *   the type of information retrieved from the file.                   *
! *                                                                      *
! *   COMM__CRL -- correlation comment. Character line is returned in    *
! *                output argument COMMENT.                              *
! *   CPAR__CRL -- parameter name. Character line is returned in output  *
! *                argument C_PARM. The index of the parameter in the    *
! *                Solve parameters list is returned in output argument  *
! *                IND1. NB: the total number of Solve parameters may be *
! *                less than the number of parameters for which          *
! *                correlations were computed.                           *
! *   NPAR__CRL -- total number of parameters for which correlations     *
! *                were computed and put in the correlation file.        *
! *   NCOR__CRL -- total number of correlations which were computed.     *
! *   CORR__CRL -- correlation coefficient for a pair of parameters.     *
! *                The coefficient is returned in output argument CORR.  *
! *                The indices are returned in output arguments IND1 and *
! *                IND2. They are the indices in Solve parameters list.  *
! *    END__CRL -- end of file has been reached. No information is       *
! *                returned.                                             *
! *                                                                      *
! *   Constants with postfix __CRL and data structure CORL_REC are       *
! *   defined in $PSOLVE_ROOT/progs/solve/include/corel.i                *
! *                                                                      *
! *   Data structure contains some auxiliary information which can be    *
! *   useful. Its contains depends on return code since information is   *
! *   put there through consecutive calls.                               *
! *                                                                      *
! *   COREL_REC.FILSPL  -- (all return codes) Name of the correlation    *
! *                        file.                                         *
! *   COREL_REC.M_PAR   -- (CPAR__CRL, NPAR__CRL, NCOR__CRL, CORR__CRL,  *
! *                         END__CRL) total number of parameters.        *
! *   COREL_REC.C_PAR   -- (NPAR__CRL, NCOR__CRL, CORR__CRL, END_CRL) -- *
! *                        table of parameters name for which            *
! *                        correlations were computed. NB: this table is *
! *                        NOT necessarily the table of Solve parameters,*
! *                        since correlations may be computed for        *
! *                        a subset of Solve parameters.                 *
! *   COREL_REC.IXREF_PAR -- (NPAR__CRL, NCOR__CRL, CORR__CRL, END_CRL)  *
! *                          -- Cross corresponding table from Solve     *
! *                          parameters list to COREL_REC.C_PAR          *
! *                                                                      *
! *   Thus, if return code is CORR_CRL, then the names of parameters for *
! *   which a correlation coefficient is computed are                    *
! *   COREL_REC.C_PAR(COREL_REC.IXREF_PAR(IND1)) and                     *
! *   COREL_REC.C_PAR(COREL_REC.IXREF_PAR(IND2)) respectively.           *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * COMMENT ( CHARACTER ) -- Comment line. Defined when return code is   *
! *                          COMM__CRL.                                  *
! *  C_PARM ( CHARACTER ) -- Parameter name. Defined when return code is *
! *                          COMM__CRL.                                  *
! *    IND1 ( INTEGER*4 ) -- If the return code is NPAR__CRL then total  *
! *                          number of parameters. If the return code is *
! *                          NCOR_CRL then total number of correlations. *
! *                          If the return code is CORR__CRL then index  *
! *                          of the first parameter for which            *
! *                          the correlation coefficient was computed.   *
! *    IND2 ( INTEGER*4 ) -- If the return code is CORR__CRL then index  *
! *                          of the second parameter for which           *
! *                          correlation was computed.                   *
! *    CORR ( REAL*8    ) -- If the return code is CORR__CRL then        *
! *                          a correlation coefficient.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  CORL_REC ( RECORD    ) -- Data structure which keeps internal       *
! *                            buffers for parsing correlation file.     *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 18-MAY-2001    READ_CRL   v1.0 (c)  L. Petrov  18-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'corel.i'
      INTEGER*4  READ_CRL, IND1, IND2, IUER
!
      TYPE ( CORL__STRU ) ::  CORL_REC
      CHARACTER  C_PARM*(*), COMMENT*(*)
      REAL*8     CORR
      BYTE       ARRAY(M_BBUF)
      CHARACTER  STR*80
      INTEGER*2  IND1_I2, IND2_I2
      REAL*4     CORR_R4
      INTEGER*4  NBT, IPAR, IER
      INTEGER*4  I_LEN, UNIT_TO_FILDESC
!
! --- Initialization of outpuit arguments
!
      CALL CLRCH ( COMMENT )
      CALL CLRCH ( C_PARM  )
      IND1 = 0
      IND2 = 0
      CORR = 0.0D0
!
! --- Operation depends on the previous status
!
      IF ( CORL_REC%STATUS .NE. INIT__CRL  .AND. &
     &     CORL_REC%STATUS .NE. CPAR__CRL  .AND. &
     &     CORL_REC%STATUS .NE. CORR__CRL  .AND. &
     &     CORL_REC%STATUS .NE. END__CRL   .AND. &
     &     CORL_REC%STATUS .NE. CPAR__CRL        ) THEN
!
           CALL ERR_LOG ( 8911, IUER, 'READ_CRL', 'Sructure CORL_REC '// &
     &         ' was not initialized' )
           RETURN
         ELSE IF ( CORL_REC%STATUS .EQ. INIT__CRL ) THEN
!
! -------- The previous operation was intialization or retrieving a comment.
! -------- Then retrieve a comment line
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( UNIT_TO_FILDESC(CORL_REC%LUN), M_BBUF, ARRAY, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 8912, IUER, 'READ_CRL', 'Error "'// &
     &               STR(1:I_LEN(STR))//'" while reading the file '// &
     &               CORL_REC%FILSPL )
                CLOSE ( UNIT=CORL_REC%LUN )
                RETURN
           END IF
!
! -------- Copy comment line to the output argument
!
           CALL LIB$MOVC3 ( MIN(LEN(COMMENT),NBT), ARRAY, %REF(COMMENT) )
           IF ( COMMENT(1:1) .EQ. '#' ) THEN
                READ_CRL = COMM__CRL
              ELSE IF ( COMMENT(1:4) .EQ. '$ CH' ) THEN
!
! ------------- It was a section delimieter. Get the total number of parameters
!
                CALL LIB$MOVC3 ( 4, %REF(COMMENT(5:8)), CORL_REC%M_PAR )
                IF ( CORL_REC%L_PAR .LT. 0  .OR. CORL_REC%L_PAR .GT. M_GPA) THEN
                     CALL ERR_LOG ( 8913, IUER, 'READ_CRL', 'Trap of '// &
     &                   'internal control in reading correlation file '// &
     &                    CORL_REC%FILSPL(1:I_LEN(CORL_REC%FILSPL))// &
     &                   ' -- the number of parameters: '//STR(1:I_LEN(STR))// &
     &                   ' is out of range' )
                     CLOSE ( UNIT=CORL_REC%LUN )
                     RETURN
                END IF
!
! ------------- Initialize buffer ounter and cross-reference table
!
                CORL_REC%L_PAR = 0
                CALL NOUT_I4 ( M_GPA, CORL_REC%IXREF_PAR )
                IND1 = CORL_REC%M_PAR
                READ_CRL = NPAR__CRL
                CORL_REC%STATUS = CPAR__CRL
                CALL CLRCH ( COMMENT )
             ELSE
                CALL TRAN ( 13, COMMENT, COMMENT )
                CALL ERR_LOG ( 8914, IUER, 'READ_CRL', 'Trap of internal '// &
     &              'control in reading correlation file '// &
     &               CORL_REC%FILSPL(1:I_LEN(CORL_REC%FILSPL))// &
     &              ' -- comment line "'//COMMENT(1:I_LEN(COMMENT))// &
     &              '" starts not from #' )
                CLOSE ( UNIT=CORL_REC%LUN )
                RETURN
           END IF
         ELSE IF ( CORL_REC%STATUS .EQ. CPAR__CRL ) THEN
!
! -------- The previous operation was retrieving parameter name.
! -------- Do it again
!
           IF ( CORL_REC%L_PAR .EQ. CORL_REC%M_PAR ) THEN
!
! ------------- Aga. We have already retreived tha last parameter name.
! ------------- Then should be section delimeter. Read it.
!
                READ_CRL = NCOR__CRL
                CORL_REC%STATUS = CORR__CRL
                CALL ERR_PASS ( IUER, IER )
                CALL RDBIN_RECORD ( UNIT_TO_FILDESC(CORL_REC%LUN), M_BBUF, ARRAY, NBT, IER)
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH  ( STR )
                     CALL GERROR ( STR )
                     CALL ERR_LOG ( 8915, IUER, 'READ_CRL', 'Error "'// &
     &                    STR(1:I_LEN(STR))//'" while reading the file '// &
     &                    CORL_REC%FILSPL )
                     CLOSE ( UNIT=CORL_REC%LUN )
                     RETURN
                END IF
!
! ------------- Estract the string and the number of correlations
!
                CALL LIB$MOVC3 ( 4, ARRAY(1), %REF(STR) )
                CALL LIB$MOVC3 ( 4, ARRAY(5), CORL_REC%M_COR  )
                IF ( CORL_REC%M_COR .LT. 0  .OR.  CORL_REC%M_COR .GT. &
     &               (INT8(M_GPA-1)*INT8(M_GPA-2))/2 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( CORL_REC%M_COR, STR )
                     CALL ERR_LOG ( 8916, IUER, 'READ_CRL', 'Trap of '// &
     &                   'internal control in reading correlation file '// &
     &                    CORL_REC%FILSPL(1:I_LEN(CORL_REC%FILSPL))// &
     &                   ' -- number of correlation is out of range: '// &
     &                    STR )
                     CLOSE ( UNIT=CORL_REC%LUN )
                     RETURN
                END IF
!
! ------------- Initiliaze correlation buffer
!
                CORL_REC%L_COR = 0
                CORL_REC%M_BUF = 0
                CORL_REC%L_BUF = 0
                IND1 = CORL_REC%M_COR
                IND2 = 0
              ELSE
!
! ------------- Retrieve the next parameter name
!
                CORL_REC%L_PAR = CORL_REC%L_PAR + 1
                CALL ERR_PASS ( IUER, IER )
                CALL RDBIN_RECORD ( UNIT_TO_FILDESC(CORL_REC%LUN), M_BBUF, ARRAY, NBT, IER)
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH  ( STR )
                     CALL GERROR ( STR )
                     CALL ERR_LOG ( 8917, IUER, 'READ_CRL', 'Error "'// &
     &                    STR(1:I_LEN(STR))//'" while reading the file '// &
     &                    CORL_REC%FILSPL )
                     CLOSE ( UNIT=CORL_REC%LUN )
                     RETURN
                END IF
                IF ( NBT .NE. 32 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH ( NBT, STR )
                     CALL ERR_LOG ( 8918, IUER, 'READ_CRL', 'Trap of '// &
     &                   'internal control in reading correlation file '// &
     &                    CORL_REC%FILSPL(1:I_LEN(CORL_REC%FILSPL))// &
     &                   ' -- parameter name line length is not 32 bytes, '// &
     &                   'but '//STR )
                     CLOSE ( UNIT=CORL_REC%LUN )
                     RETURN
                END IF
!
! ------------- Extract the index of the parameter in the correlation matrix...
!
                CALL CLRCH ( STR )
                CALL LIB$MOVC3 ( 5, ARRAY, %REF(STR) )
                CALL CHIN      ( STR(1:5), IPAR )
                IF ( IPAR .LT. 0  .OR.  IPAR .GT. M_GPA ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IPAR, STR )
                     CALL ERR_LOG ( 8919, IUER, 'READ_CRL', 'Trap of '// &
     &                   'internal control in reading correlation file '// &
     &                    CORL_REC%FILSPL(1:I_LEN(CORL_REC%FILSPL))// &
     &                   ' -- index of the parameter: '//STR(1:I_LEN(STR))// &
     &                   ' is out of range' )
                     CLOSE ( UNIT=CORL_REC%LUN )
                     RETURN
                END IF
!
! ------------- ... and put it in a cross referece table
!
                CORL_REC%IXREF_PAR(IPAR) = CORL_REC%L_PAR
                CALL LIB$MOVC3 ( 20, ARRAY(7), &
     &                           %REF(CORL_REC%C_PAR(CORL_REC%L_PAR)))
!
! ------------- Extract parameter name from the logical record
!
                CALL LIB$MOVC3 ( 20, ARRAY(7), %REF(C_PARM) )
                IND1     = CORL_REC%L_PAR
                READ_CRL = CPAR__CRL
           END IF
         ELSE IF ( CORL_REC%STATUS .EQ. CORR__CRL ) THEN
!
! -------- The previous operation was retrieving a correlation coefficent
! -------- Do it again
!
           IF ( CORL_REC%M_COR .EQ. CORL_REC%L_COR  ) THEN
!
! ------------- We have already retrieved all correlation ceofficients.
! ------------- Nothing to do.
!
                READ_CRL = END__CRL
                CORL_REC%STATUS = END__CRL
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
!
           IF ( CORL_REC%M_BUF .EQ. CORL_REC%L_BUF ) THEN
!
! ------------- Buffer counter saied that all correlation from the buffer have
! ------------- bean already retrieved. Read a buffer from disk.
!
                CALL ERR_PASS ( IUER, IER )
                CALL RDBIN_RECORD ( UNIT_TO_FILDESC(CORL_REC%LUN), M_BBUF, CORL_REC%B_BUF, &
     &                              CORL_REC%M_BUF, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8920, IUER, 'READ_CRL', 'Error in '// &
     &                   'physical readiing a record from correlation file '// &
     &                    CORL_REC%FILSPL )
                     RETURN
                END IF
!
! ------------- Initialize a budder counter
!
                CORL_REC%L_BUF = 0
           END IF
!
! -------- Extract the correlation coefficient and indeces in a parameter
! -------- list from the buffer
!
           CALL LIB$MOVC3 ( 2, CORL_REC%B_BUF(CORL_REC%L_BUF+1), IND1_I2 )
           CALL LIB$MOVC3 ( 2, CORL_REC%B_BUF(CORL_REC%L_BUF+3), IND2_I2 )
           CALL LIB$MOVC3 ( 4, CORL_REC%B_BUF(CORL_REC%L_BUF+5), CORR_R4 )
           CORR = CORR_R4
           IND1 = IND1_I2
           IND2 = IND2_I2
!
! -------- Increment buffer counter
!
           CORL_REC%L_COR = CORL_REC%L_COR + 1
           CORL_REC%L_BUF = CORL_REC%L_BUF + 8
           READ_CRL = CORR__CRL
         ELSE IF ( CORL_REC%STATUS .EQ. END__CRL ) THEN
!
! -------- Nothing to do!
!
           READ_CRL = END__CRL
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_CRL  #!#
