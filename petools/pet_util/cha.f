      FUNCTION ILEN ( STR )
! ************************************************************************
! *                                                                      *
! *   Function ILEN returns the position of the last character of the    *
! *   string STR which is not blank or binary zero. If the string        *
! *   contains only blanks and/or binary zeroes, then ILEN=0             *
! *                                                                      *
! *  ### 17-JAN-1989               v1.0 (c)  L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  ILEN
      CHARACTER STR*(*)
      INTEGER*4  J1
!
      ILEN=0
      DO 410 J1=LEN(STR),1,-1
         ILEN=J1
         IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) RETURN
 410  CONTINUE
      ILEN=0
      RETURN
      END  !#!  ILEN  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION I_LEN ( STR )
! ************************************************************************
! *                                                                      *
! *   Function I_LEN returns the position of the last character of the   *
! *   string STR which is not blank or binary zero. If the string        *
! *   contains only blanks and/or binary zeroes, then I_LEN=1            *
! *                                                                      *
! *   In the case if the string is empty (only blanks and/or binary      *
! *   zeroes) then
! *                                                                      *
! *   STR(1:I_LEN(STR)) returns the character blank or binary zero       *
! *   STR(1:ILEN(STR)) -- causes abnormal termination due to range chack *
! *                       ( STR(1:0) is not permitted )                  *
! *                                                                      *
! *  ### 17-JAN-1989     I_LEN     v1.0 (c)  L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  I_LEN
      CHARACTER STR*(*)
      INTEGER*4  J1
!
      I_LEN=1
      DO 410 J1=LEN(STR),1,-1
         I_LEN=J1
         IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) RETURN
 410  CONTINUE
      I_LEN=1
!
      RETURN
      END  !#!  ILEN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CLRCH ( STR )
! ************************************************************************
! *                                                                      *
! *   Blanking the string STR: all characters of the string CLRCH get    *
! *   the value blank (decimal code 32).                                 *
! *                                                                      *
! *  ### 17-JAN-1988     CLRCH     v1.0 (c)  L. Petrov  10-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  STR*(*)
      INTEGER*4  J1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
      EXTERNAL   MEMSET
!
!      DO 410 J1=1,LEN(STR)
!         STR(J1:J1)=' '
! 410  CONTINUE
#ifdef SUN
      CALL MEMSET ( %VAL(LOC__SUN$$_STR(STR)), %VAL(ICHAR(' ')), %VAL(LEN(STR)) )
#else
      CALL MEMSET ( %REF(STR), %VAL(ICHAR(' ')), %VAL(LEN(STR)) )
#endif
      RETURN
      END  !#!  CLRCH  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT ( NBYTES, ARRAY )
! ************************************************************************
! *                                                                      *
! *   Routine NOUT fills NBYTES bytes of the array ARRAY with binary     *
! *   zero.                                                              *
! *                                                                      *
! *  ### 17-JAN-1989      NOUT     v1.0 (c)  L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NBYTES
      INTEGER*1  ARRAY(NBYTES)
      INTEGER*4  J1
!
!@      DO 410 J1=1,NBYTES
!@         ARRAY(J1)=0
!@ 410  CONTINUE
      IF ( NBYTES > 0 ) THEN
           CALL BZERO ( ARRAY, %VAL(NBYTES) )
      END IF
      RETURN
      END  !#!  NOUT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION LIB$SCANC ( STR, TAB, IVAL )
! ************************************************************************
! *                                                                      *
! *   Function LIB$SCANC emulates the function with the same name        *
! *   provided by VAX/VMS. It scans the string STR and checks every      *
! *   character of STR against the table TAB. The table TAB is a byte    *
! *   array with dimension 0:255. LIB$SCANC tries every character of the *
! *   string STR as the input in the table TAB: TAB(STR(IP:IP)) (where   *
! *   IP is the position of the character under investigation) and       *
! *   checks the result against IVAL. If the results is IVAL then it     *
! *   returns position of the tested character. If TAB(STR(IP:IP)) is    *
! *   not IVAL for evey IP, then LIB$SCANC returns zero.                 *
! *                                                                      *
! * ________________________ Input paramters: __________________________ *
! *                                                                      *
! *         STR ( CHARACTER ) -- The string under investigation.         *
! *         TAB ( INTEGER*1 ) -- The table of codes. Dimenstion: (0,255) *
! *        IVAL ( INTEGER*4 ) -- The value of the code.                  *
! *                                                                      *
! * ________________________ Output parameters. ________________________ *
! *                                                                      *
! * <LIB$SCANC> (INTEGER*4  ) -- Position in the string of the symbol    *
! *                              with code  not equal to IVAL or zero.   *
! *                                                                      *
! *  ### 16-DEC-1996    LIB$SCANC   v1.0 (c) L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  STR*(*)
      INTEGER*4  LIB$SCANC
      INTEGER*1  TAB(0:255), IVAL
      INTEGER*4  J1, ICH
!
      DO 410 J1=1,LEN(STR)
         LIB$SCANC = J1
         ICH = ICHAR( STR(J1:J1) )
         IF ( ICH .LT. 0 ) ICH=256+ICH
         IF ( TAB(ICH) .EQ. IVAL ) RETURN
 410  CONTINUE
      LIB$SCANC = 0
      RETURN
      END  !#!  LIB$SCANC  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION LIB$SPANC ( STR, TAB, IVAL )
! ************************************************************************
! *                                                                      *
! *   Function LIB$SPANC emulates the function with the same name        *
! *   provided by VAX/VMS. It scans the string STR and checks every      *
! *   character of STR against the table TAB. The table TAB is a byte    *
! *   array with dimension 0:255. LIB$SPANC tries every character of the *
! *   string STR as the input in the table TAB: TAB(STR(IP:IP)) (where   *
! *   IP is the position of the character under investigation) and       *
! *   checks the result against IVAL. If the results is not IVAL then    *
! *   it returns position of the character which is not IVAL.            *
! *   If TAB(STR(IP:IP)) is IVAL for evey IP, then LIB$SPANC returns     *
! *   zero.                                                              *
! *                                                                      *
! * ________________________ Input paramters: __________________________ *
! *                                                                      *
! *         STR ( CHARACTER ) -- The string under investigation.         *
! *         TAB ( INTEGER*1 ) -- The table of codes. Dimenstion: (0,255) *
! *        IVAL ( INTEGER*4 ) -- The value of the code.                  *
! *                                                                      *
! * ________________________ Output parameters. ________________________ *
! *                                                                      *
! * <LIB$SPANC> (INTEGER*4  ) -- Position in the string of the symbol    *
! *                              with code  not equal to IVAL or zero.   *
! *                                                                      *
! *  ### 16-DEC-1996    LIB$SPANC   v1.0 (c) L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LIB$SPANC
      CHARACTER  STR*(*)
      INTEGER*1  TAB(0:255), IVAL
      INTEGER*4  ICH, J1
!
      DO 410 J1=1,LEN(STR)
         LIB$SPANC = J1
         ICH = ICHAR( STR(J1:J1) )
         IF ( ICH .LT. 0 ) ICH=256+ICH
         IF ( TAB(ICH) .NE. IVAL ) RETURN
 410  CONTINUE
      LIB$SPANC = 0
      RETURN
      END  !#!  LIB$SPANC  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   LIB$SKPC ( SUBSTR, STR )
! ************************************************************************
! *                                                                      *
! *   Function LIB$SKPC emulates the the function with the same name     *
! *   provided by VAX/VMS. It seeks the the first character of SUBSTR in *
! *   the string STR and returns the position of the first character STR *
! *   which is not equal to SUBSTR(1:1). If it fails to find such        *
! *   a character it returns zero.                                       *
! *                                                                      *
! *  ### 16-DEC-1996    LIB$SKPC   v1.0 (c)  L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LIB$SKPC
      CHARACTER  SUBSTR*(*), STR*(*)
      INTEGER*4  J1
!
      DO 410 J1=1,LEN(STR)
         LIB$SKPC = J1
         IF ( STR(J1:J1) .NE. SUBSTR(1:1) ) RETURN
 410  CONTINUE
      LIB$SKPC = 0
      RETURN
      END  !#!  LIB$SKPC  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LIB$MOVC3 ( LENB, BYTE_FROM, BYTE_TO )
! ************************************************************************
! *                                                                      *
! *   Function LIB$MOVC3 emulates the the function with the same name    *
! *   provided by VAX/VMS. Routine  LIB$MOVC3  copies  LENB bytes from   *
! *   BYTE_FROM to BYTE_TO.                                              *
! *                                                                      *
! *  ###  22-DEC-96   LIB$MOVC3    v3.0  (c)  L. Petrov 27-JUN-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LENB
      INTEGER*1  BYTE_FROM(LENB), BYTE_TO(LENB)
      EXTERNAL   MEMCPY
!
      IF ( LENB .LE. 0 ) RETURN
      CALL MEMCPY ( BYTE_TO, BYTE_FROM, %VAL(LENB) )
!
      RETURN
      END  !#!  LIB$MOVC3  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LIB$MOVC8 ( LENB_I8, BYTE_FROM, BYTE_TO )
! ************************************************************************
! *                                                                      *
! *   Function LIB$MOVC8 emulates the the function with the same name    *
! *   provided by VAX/VMS. Routine  LIB$MOVC8  copies  LENB bytes from   *
! *   BYTE_FROM to BYTE_TO.                                              *
! *                                                                      *
! *  ###  22-DEC-96   LIB$MOVC8    v3.1  (c)  L. Petrov 17-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  LENB_I8
      INTEGER*1  BYTE_FROM(LENB_I8), BYTE_TO(LENB_I8)
      EXTERNAL   MEMCPY
!
      IF ( LENB_I8 .LE. 0 ) RETURN
      CALL MEMCPY ( BYTE_TO, BYTE_FROM, %VAL(LENB_I8) )
!
      RETURN
      END  !#!  LIB$MOVC3  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_STRING0 ( BYTE_ARRAY, STR )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine GET_STRING0 tranform C-style null-terminated     *
! *   string to Fortran-style character string with trailing blanks.     *
! *                                                                      *
! *  ###  04-MAY-98   GET_STRING0   v1.0 (c)  L. Petrov  04-MAY-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*1  BYTE_ARRAY(*)
      CHARACTER  STR*(*)
      INTEGER*4  J1
!
      CALL CLRCH ( STR )
      DO 410 J1=1,LEN(STR)
         IF ( BYTE_ARRAY(J1) .EQ. 0 ) RETURN
         STR(J1:J1) = CHAR(BYTE_ARRAY(J1))
 410  CONTINUE
!
      RETURN
      END  !#!  GET_STRING0  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION LINDEX ( STR, SUBSTR )
! ************************************************************************
! *                                                                      *
! *     Routine  LINDEX  eximines string STR and search for the          *
! *   substring SUBSTR there. If the sustring is encountered more than   *
! *   omce in the string STR than it locates the LAST appearence of the  *
! *   sustring. It returns the index of the symbol in the string STR     *
! *   where the sustring is found. It works analogouly to the built-in   *
! *   finction of INDEX but detects the last appearance of the substring.*
! *   It returns 0 if the substring has not been found.                  *
! *                                                                      *
! *     Examples:                                                        *
! *                                                                      *
! *     STR=DOGCATFISHCAT       SUBSTR=CAT      INDEX=4   LINDEX=11      *
! *     STR=DOGCATFISHCAT       SUBSTR=FISH     INDEX=7   LINDEX=7       *
! *     STR=DOGCATFISHCAT       SUBSTR=BIRD     INDEX=0   LINDEX=0       *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  LINDEX
        CHARACTER STR*(*), SUBSTR*(*)
        INTEGER*4  L, KC, J1
!
        L=LEN(STR)
        LINDEX=0
        KC=1
        DO 410 J1=1,L
           KC=INDEX ( STR( (LINDEX+1):L ), SUBSTR )
           IF(KC.EQ.0) GOTO 810
           LINDEX=LINDEX+KC
           IF( LINDEX.GE.L ) GOTO 810
  410   CONTINUE
  810   RETURN
        END  !#!  LINDEX  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION FINDEX ( STR, SUBSTR )
! ************************************************************************
! *                                                                      *
! *   Function FINDEX finds the first occurrence of the suvstring SUBSTR *
! *   in string STR. It has the same fucntionailty as Fortran intrinsic  *
! *   INDEX. It may be called instead of INDEX in order to cicrumvent    *
! *   strong type checking.                                              *
! *                                                                      *
! *  ### 09-JUN-2009     FINDEX    v1.0 (c)  L. Petrov  09-JUN-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  FINDEX
      CHARACTER  STR*(*), SUBSTR*(*)
      FINDEX = INDEX ( STR, SUBSTR )
      RETURN
      END  FUNCTION  FINDEX  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   UNINDEX ( STR, SUBSTR )
! ************************************************************************
! *                                                                      *
! *   Routine UNINDEX returss the index of first occurence counted from  *
! *   the left edge of the string STR when the substring SUBSTR does not *
! *   coinside with the portion of the string STR. It is analogue of     *
! *   INDEX, but it reverses the meaning of the operation.               *
! *                                                                      *
! *  ### 18-MAR-1991    UNINDEX    v1.0 (c)  L. Petrov  02-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  UNINDEX
      CHARACTER  STR*(*), SUBSTR*(*)
      INTEGER*4  L1, L2, J1
      L1 = LEN(STR)
      L2 = LEN(SUBSTR)
!
      UNINDEX = 0
      DO 410 J1=1,L1-L2+1
         IF ( STR(J1:J1+L2-1) .NE. SUBSTR ) THEN
              UNINDEX = J1
              RETURN
         END IF
 410  CONTINUE
!
      RETURN
      END  !#!  UNINDEX  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE BLANK_TO_ZERO ( STR )
! ************************************************************************
! *                                                                      *
! *     Routine  BLANK_TO_ZERO  replaces blanks or binary zeroes with    *
! *   text zeroes (code=48) in the string STR.                           *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*)
        INTEGER*4  J1
!
        DO 410 J1=1,LEN(STR)
           IF ( STR(J1:J1) .EQ. ' '     ) STR(J1:J1) = '0'
           IF ( STR(J1:J1) .EQ. CHAR(0) ) STR(J1:J1) = '0'
  410   CONTINUE
        RETURN
        END  !#!  BLANK_TO_ZERO  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE ZERO_TO_BLANK ( STR )
! ************************************************************************
! *                                                                      *
! *     Routine  ZERO_TO_BLANK  replaces text zeroes (code=48) by blanks *
! *   (code=32) in the in the string STR.                                *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*)
        INTEGER*4  J1
!
        DO 410 J1=1,LEN(STR)
           IF ( STR(J1:J1) .EQ. '0' ) STR(J1:J1)=' '
  410   CONTINUE
        RETURN
        END  !#!  ZERO_TO_BLANK  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE BZERO_TO_BLANK ( STR )
! ************************************************************************
! *                                                                      *
! *     Routine  BZERO_TO_BLANK  replaces binary zeroes (code=0) with    *
! *   blanks (code=32) in the in the string STR.                         *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*)
        INTEGER*4  J1
!
        DO 410 J1=1,LEN(STR)
           IF ( STR(J1:J1) .EQ. CHAR(0) ) STR(J1:J1)=' '
  410   CONTINUE
        RETURN
        END  !#!  BZERO_TO_BLANK  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE STR_BLANK_TRAILER ( STR )
! ************************************************************************
! *                                                                      *
! *     Routine  BZERO_TO_BLANK  replaces binary zeroes (code=0) with    *
! *   blanks (code=32) in the in the string STR.                         *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*)
        INTEGER*4  J1
        LOGICAL*1  FL_BLANK
!
        FL_BLANK = .FALSE.
        DO 410 J1=1,LEN(STR)
           IF ( STR(J1:J1) .EQ. CHAR(0) ) FL_BLANK = .TRUE.
           IF ( FL_BLANK ) THEN
                STR(J1:J1) = ' '
           END IF
  410   CONTINUE
        RETURN
        END  !#!  STR_BLANK_TRAILER #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE INCH ( INTEG, CH )
! ************************************************************************
! *                                                                      *
! *     Routine  INCH  encodes INTEG of the type INTEGER*4  to the       *
! *   character string  CH. The string is adjusted to the left edge and  *
! *   trailing blanks are add to the right edge if necessary.            *
! *                                                                      *
! *     If the string  CH  has too few space for all digits than it is   *
! *   filled by asterisks.                                               *
! *                                                                      *
! *     |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|                                 *
! *     |  INTEGER  ---->  CHARACTER   |                                 *
! *     |______________________________|                                 *
! *                                                                      *
! *     Calls:    CLRCH, PROBE_W, LW$STR, NUM$ARG, LEN, LIB$SKPC         *
! *                                                                      *
! *  ###  08-JAN-91      INCH     v1.0     (c) L. Petrov 16-FEB-96  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  CH*(*), STR*11
        INTEGER*4  INTEG
        INTEGER*4  L, IE
        INTEGER*4, EXTERNAL :: ILEN
!
        L=LEN(CH)
        CALL CLRCH ( CH )
!
        WRITE ( UNIT=STR(1:11), FMT='(I11)', ERR=710 ) INTEG
        CALL CHASHL ( STR )
        IF ( ILEN(STR).GT.L ) GOTO 710
        CH=STR
        GOTO 810
!
  710   CONTINUE
        IE=1
        CH='************'
  810   CONTINUE
!        IF ( NUM$ARG().GE.3 ) THEN
!             IF ( PROBE_W ( 1, 4, IERR) ) IERR=IE
!        END IF
        RETURN
        END  !#!  INCH  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE INCH8 ( INTEG_I8, CH )
! ************************************************************************
! *                                                                      *
! *     Routine  INCH  encodes INTEG of the type INTEGER*4  to the       *
! *   character string  CH. The string is adjusted to the left edge and  *
! *   trailing blanks are add to the right edge if necessary.            *
! *                                                                      *
! *     If the string  CH  has too few space for all digits than it is   *
! *   filled by asterisks.                                               *
! *                                                                      *
! *     |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|                                 *
! *     |  INTEGER  ---->  CHARACTER   |                                 *
! *     |______________________________|                                 *
! *                                                                      *
! *     Calls:    CLRCH, PROBE_W, LW$STR, NUM$ARG, LEN, LIB$SKPC         *
! *                                                                      *
! *  ###  08-JAN-91      INCH     v1.0     (c) L. Petrov 16-FEB-96  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  CH*(*), STR*19
        INTEGER*8  INTEG_I8
        INTEGER*4  L, IE
        INTEGER*4, EXTERNAL :: ILEN
!
        L=LEN(CH)
        CALL CLRCH ( CH )
!
        WRITE ( UNIT=STR(1:19), FMT='(I19)', ERR=710 ) INTEG_I8
        CALL CHASHL ( STR )
        IF ( ILEN(STR).GT.L ) GOTO 710
        CH=STR
        GOTO 810
!
  710   CONTINUE
        IE=1
        CH='************'
  810   CONTINUE
!        IF ( NUM$ARG().GE.3 ) THEN
!             IF ( PROBE_W ( 1, 4, IERR) ) IERR=IE
!        END IF
        RETURN
        END  !#!  INCH  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE IINCH ( INTEG, STR )
! ************************************************************************
! *                                                                      *
! *     Routine  IINCH  encodes INTEG of the type INTEGER*4  to the      *
! *   character string  CH. The string is adjusted to the left edge and  *
! *   trailing blanks added to the right edge if necessary. Decimal      *
! *   digits are grouped by 3 and are separated by a blank.              *
! *                                                                      *
! *   Examples:                                                          *
! *                                                                      *
! *      124                                                             *
! *      56 893                                                          *
! *      -1 234 895                                                      *
! *                                                                      *
! *   If the string  CH  has too few space for all digits than it is     *
! *   filled by asterisks.                                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  INTEG ( INTEGER*4 ) -- a number to be encoded.                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    STR ( CHARACTER ) -- Output string.                               *
! *                                                                      *
! *  ###  10-JAN-1996    IINCH    v1.1  (c) L. Petrov  08-FEB-1996  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*), PRM*20
!        LOGICAL    PROBE_W, PRESENT
!        INTEGER*4  IERR, IER
        INTEGER*4  INTEG, IL, LN
        INTEGER*4, EXTERNAL :: ILEN
!
! ----- Usual transformation INTEGER*4 ---> CHARACTER
!
        CALL CLRCH ( PRM )
        CALL INCH  ( INTEG, PRM )
!
! ----- Check completion codes
!
        LN=LEN(STR)
        IF ( LN .GT. 20 ) LN = 20
!        IF ( IER .EQ. 2 ) THEN
!             IF ( PRESENT ( IERR, 3  ) .AND. PROBE_W ( 1, 4, IERR ) )
!     #            IERR=1
!             RETURN
!        END IF
!        IF ( IER .EQ. 1 ) THEN
!             CALL REPEAT ( '*', LN, STR )
!             IF ( PRESENT ( IERR, 3  ) .AND. PROBE_W ( 1, 4, IERR ) )
!     #            IERR=1
!             RETURN
!        END IF
        IF ( PRM(1:1) .EQ. '*' ) THEN
!
! ---------- There were an error? Fill the string by * and quit
!
             CALL REPEAT ( '*', LN, STR )
             RETURN
        END IF
!
        CALL CLRCH ( STR )
        IL=ILEN(PRM)
!
! ----- Adjust the string at right edge
!
        CALL CHASHR ( PRM )
        STR = PRM(18:20)
!
! ----- Give additional space for a blank delimiter
!
        IF ( IL.GT.3  ) STR = PRM(15:17)//' '//STR
        IF ( IL.GT.6  ) STR = PRM(12:14)//' '//STR
        IF ( IL.GT.9  ) STR = PRM(9:11) //' '//STR
        IF ( ( IL .GT. 3   .AND.   IL   .EQ. LN ) .OR. &
     &       ( IL .GT. 6   .AND.   IL-1 .EQ. LN ) .OR. &
     &       ( IL .GT. 9   .AND.   IL-2 .EQ. LN )      ) THEN
!
! ---------- Oh! It was too few space
!
             CALL REPEAT ( '*', LN, STR )
!             IF ( PRESENT ( IERR, 3  ) .AND. PROBE_W ( 1, 4, IERR ) )
!     #            IERR=1
             RETURN
        END IF
        CALL CHASHL ( STR )
        IF ( STR(1:2) == '- ' ) STR = STR(1:1)//STR(3:)
        RETURN
        END  !#!  IINCH  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE IINCH8 ( INTEG, STR )
! ************************************************************************
! *                                                                      *
! *     Routine  IINCH8  encodes INTEG of the type INTEGER*8  to the     *
! *   character string  CH. The string is adjusted to the left edge and  *
! *   trailing blanks added to the right edge if necessary. Decimal      *
! *   digits are grouped by 3 and are separated by a blank.              *
! *                                                                      *
! *   Examples:                                                          *
! *                                                                      *
! *      124                                                             *
! *      56 893                                                          *
! *      -1 234 895                                                      *
! *                                                                      *
! *   If the string  CH  has too few space for all digits than it is     *
! *   filled by asterisks.                                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  INTEG ( INTEGER*8 ) -- a number to be encoded.                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    STR ( CHARACTER ) -- Output string.                               *
! *                                                                      *
! *  ###  27-JAN-2005    IINCH8   v1.0  (c) L. Petrov  27-JAN-2005  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*), PRM*25
        INTEGER*8  INTEG
        INTEGER*4  IL, LN
        INTEGER*4, EXTERNAL :: ILEN
!
! ----- Usual transformation INTEGER*8 ---> CHARACTER
!
        CALL CLRCH ( PRM )
        WRITE ( UNIT=PRM, FMT='(I20)' ) INTEG
!
! ----- Check completion codes
!
        LN=LEN(STR)
        IF ( LN .GT. LEN(PRM) ) LN = LEN(PRM)
        IF ( PRM(1:1) .EQ. '*' ) THEN
!
! ---------- There were an error? Fill the string by * and quit
!
             CALL REPEAT ( '*', LN, STR )
             RETURN
        END IF
!
        CALL CLRCH ( STR )
        IL=ILEN(PRM)
!
! ----- Adjust the string at right edge
!
        CALL CHASHR ( PRM )
        STR = PRM(23:25)
!
! ----- Give additional space for a blank delimiter
!
        IF ( IL .GT.  3 ) STR = PRM(20:22)//' '//STR
        IF ( IL .GT.  6 ) STR = PRM(17:19)//' '//STR
        IF ( IL .GT.  9 ) STR = PRM(14:16) //' '//STR
        IF ( IL .GT. 12 ) STR = PRM(11:13) //' '//STR
        IF ( IL .GT. 15 ) STR = PRM( 8:10) //' '//STR
        IF ( IL .GT. 18 ) STR = PRM( 5: 7) //' '//STR
        IF ( ( IL .GT. 3   .AND.   IL   .EQ. LN ) .OR. &
     &       ( IL .GT. 6   .AND.   IL-1 .EQ. LN ) .OR. &
     &       ( IL .GT. 9   .AND.   IL-2 .EQ. LN )      ) THEN
!
! ---------- Oh! It was too few space
!
             CALL REPEAT ( '*', LN, STR )
        END IF
        CALL CHASHL ( STR )
        IF ( STR(1:2) == '- ' ) STR = STR(1:1)//STR(3:)
        RETURN
        END  !#!  IINCH8  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CHIN ( CH, INTEG )
! ************************************************************************
! *                                                                      *
! *     Routine  CHIN  decodes the string CH of character type to the    *
! *   integer number INTEG of INTEGER*4 type.                            *
! *                                                                      *
! *     |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|                                 *
! *     | CHARACTER  ---->  INTEGER    |                                 *
! *     |______________________________|                                 *
! *                                                                      *
! *   If the string cannot be interperted as an integer number then      *
! *   INTEG = -1 111 111 111 .                                           *
! *                                                                      *
! *    Calls: NUM$ARG, PROBE_R, LR$STR, IFOR_MEN .                       *
! *                                                                      *
! *  ###  16-JAN-89      CHIN      v1.0  (c)  L. Petrov  28-OCT-93  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
!!        INTEGER*4 CHIN
        CHARACTER CH*(*), CHH*12
        INTEGER*4  INTEG
        INTEGER*4  IO
        INTEGER*4, EXTERNAL :: ILEN
!
        CALL CLRCH ( CHH )
        CHH=CH
        IF ( ILEN(CHH) .EQ. 0 ) THEN
!!             CHIN=-2
             INTEG= -1111111111
             RETURN
        END IF
        READ ( UNIT=CHH, FMT='(I12)', IOSTAT=IO ) INTEG
        IF ( IO .NE. 0 ) THEN
!!             CHIN=-4
             INTEG= -1111111111
             RETURN
        END IF
!
!!        CHIN=0
        RETURN
        END  !#!  CHIN  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CHIN8 ( CH, I8 )
! ************************************************************************
! *                                                                      *
! *     Routine  CHIN  decodes the string CH of character type to the    *
! *   integer number INTEG of INTEGER*8 type.                            *
! *                                                                      *
! *     |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|                                 *
! *     | CHARACTER  ---->  INTEGER*8  |                                 *
! *     |______________________________|                                 *
! *                                                                      *
! *   If the string cannot be interperted as an integer number then      *
! *   I8 = -1 111 111 111 111 111 111                                    *
! *                                                                      *
! *  ###  16-JAN-89     CHIN8      v1.0  (c)  L. Petrov  28-OCT-93  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER CH*(*), CHH*24
        INTEGER*8  I8
        INTEGER*4  IO
        INTEGER*4, EXTERNAL :: ILEN
!
        CALL CLRCH ( CHH )
        CHH=CH
        IF ( ILEN(CHH) .EQ. 0 ) THEN
             I8 = -111111111111111111_8
             RETURN
        END IF
        READ ( UNIT=CHH, FMT='(I24)', IOSTAT=IO ) I8
        IF ( IO .NE. 0 ) THEN
             I8 = -111111111111111111_8
             RETURN
        END IF
!
        RETURN
        END  SUBROUTINE  CHIN8  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CHASHL ( STR )
! ************************************************************************
! *                                                                      *
! *   Routine   CHASHL  adjusts string STR to the left edge. Thus,       *
! *   the first symbol of STR which is not a blank or a binary zero      *
! *   becomes the first symbol of the string STR becomes. Trailing       *
! *   blanks are added to the right edge of the string if needed.        *
! *                                                                      *
! *  ###  21-DEC-90      CHASHL    v1.0  (c)  L. Petrov  21-DEC-90  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*)
        INTEGER*4  LN, J1, IB
!
        LN=LEN(STR)
!
! ----- Search for the fist non-blankl symbol
!
        DO 410 J1=1,LN
           IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) THEN
                IB=J1
                GOTO 810
           END IF
 410    CONTINUE
        RETURN
!
 810    CONTINUE
        IF ( IB .EQ. 1 ) RETURN
!
! ----- Shift IB-1 sybols to the left
!
        STR(1:LN-(IB-1))=STR(IB:LN)
!
! ----- Replacing symbols at the right edge by blanks
!
        CALL CLRCH ( STR(LN-(IB-1)+1:) )
!
        RETURN
        END  !#!  CHASHL  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CHASHR ( STR )
! ************************************************************************
! *                                                                      *
! *   Routine   CHASHR  adjusts string STR at the right edge. If the     *
! *   last character of STR was a blank or binary zero, it shifts the    *
! *   string STR to the right edge at N characters in such a way that    *
! *   the last character of STR will become a non-blank and a non-binary *
! *   zero character. N blanks are added to the left edge of the string. *
! *                                                                      *
! *  ###  21-DEC-1990    CHASHR  v1.0  (c)  L. Petrov  21-DEC-1990  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*)
        INTEGER*4  LN, J1, IE
        INTEGER*4, EXTERNAL :: ILEN
!
        LN=LEN(STR)
        IE=ILEN(STR)
        IF ( IE .EQ. 0  ) RETURN
        IF ( LN .EQ. IE ) RETURN
!
        STR(1+(LN-IE):LN)=STR(1:IE)
        CALL CLRCH ( STR(1:(LN-IE)) )
        RETURN
        END  !#!  CHASHR  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION BCHAR ( STR )
! ************************************************************************
! *                                                                      *
! *     Byte function  BCHAR  returns the code of the first symbol of    *
! *   the string STR as a signed number of byte type.                    *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*1  BCHAR
        CHARACTER STR*(*)
        INTEGER*4  K
!
        K=ICHAR( STR(1:1) )
        IF ( K .GT. 127 ) K = K-256
        BCHAR=K
        RETURN
        END  !#!  BCHAR  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE DETAB ( STR, OUT )
! ************************************************************************
! *                                                                      *
! *   Routine  DETAB  replaced symbols tabulations (code=9) in the       *
! *   input string STR with the appropriate number of symbols blank      *
! *   (code=32) in the ouput string STR. NB: the effective length of the *
! *   the string OUT (position of the last character which is not        *
! *   a blank and not a binary zero) may change.                         *
! *                                                                      *
! *  ###  21-DEC-1990    CHASHR  v1.0  (c)  L. Petrov  21-DEC-1990  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER STR*(*), OUT*(*), TAB
        PARAMETER  ( TAB = CHAR(9) )  ! Tabulation symbol
        INTEGER*4  NS, LIM, K, J1
        INTEGER*4, EXTERNAL :: ILEN
!
        CALL CLRCH ( OUT ) !  Clear the input line
        NS = ILEN ( STR )
        IF ( NS.LE.0 ) RETURN
        LIM=LEN ( OUT )    !  Get declared string length
!
        K=0
        DO 410 J1=1,NS
           IF ( STR(J1:J1) .EQ. TAB ) THEN
                K=(K/8)*8+8
             ELSE
               K=K+1
               IF( K.GT.LIM ) RETURN
               OUT(K:K)=STR(J1:J1)
           END IF
  410   CONTINUE
        RETURN
        END  !#!  DETAB  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE REPEAT ( SUBSTR, NC, STR )
! ************************************************************************
! *                                                                      *
! *     Routine REPEAT  puts substring SUBSTR consecuently NC times in   *
! *   the output string STR.                                             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  SUBSTR ( CHARACTER ) -- Input substring.                            *
! *      NC ( INTEGER*4 ) -- Number of repetitions.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     STR ( CHARACTER ) -- Output sustring. Only first NC*LEN(SUBSTR)  *
! *                          symbols are changed in the sustring STR.    *
! *                                                                      *
! *  ###  23-JUL-91      REPEAT    v1.0  (c)  L. Petrov  23-JUL-91  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  SUBSTR*(*), STR*(*)
        INTEGER*4  K1, K2, NC, NB, J1
!
        IF ( NC .LT. 0 ) RETURN
        K1=LEN(SUBSTR)
        K2=LEN(STR)
        NB=1
        DO 410 J1=1,NC
           STR(NB:)=SUBSTR
           NB=NB+K1
           IF ( NB.GT.K2 ) RETURN
  410   CONTINUE
        RETURN
        END  !#!  REPEAT  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION LTM_DIF ( IREG, KSTR, TEXT, SAMPLE )
! ************************************************************************
! *                                                                      *
! *     Routine  LTM_DIF  scans a character array TEXT which contains    *
! *   KSTR strings and check, does it contain the string SAMPLE.         *
! *   If yes, then it returns the index of the element in the array      *
! *   TEXT which coincides with the examined string SAMPLE. If more than *
! *   one line in TEXT matches to SAMPLE then the index of the first     *
! *   occurrence is returned.                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    IREG ( INTEGER*4, OPT ) -- Mode switcher:                         *
! *                      IREG=0 -- (default) Only the substring SAMPLE   *
! *                                without trailing blanks is examined,  *
! *                                but not the whole string.             *
! *                      IREG=1 -- Trainling balnks are taken into       *
! *                                account in both TEXT and SAMPLE.      *
! *                                If TEXT and SAMPLE have different     *
! *                                length, a match is establshed if the  *
! *                                portion of either TEXT or SAMPLE that *
! *                                is longer has only blanks.            *
! *                      IREG=2 -- Wild card symbols * and ? in SAMPLE   *
! *                                are supported. It is assumed that no  *
! *                                line in TEXT contains * or ?.         *
! *                                Trailing blanks are ignored in both   *
! *                                TEXT and SAMPLE.                      *
! *                      IREG=3 -- Wild card symbols * and ? in TEXT     *
! *                                are supported. It is assumed that     *
! *                                SAMPLE doesn't contains * or ?.       *
! *                                Trailing blanks are ignored in both   *
! *                                TEXT and SAMPLE.                      *
! *                      IREG not 0, 1, 2 or 3 is interpreted as 0.      *
! *    KSTR ( INTEGER*4 ) -- Number of elements in the array TEXT.       *
! *    TEXT ( CHARACTER ) -- Text array. the length is KSTR.             *
! *  SAMPLE ( CHARACTER ) -- Examined string.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  <LTM_DIF> ( INTEGER*4 ) -- Index of the element TEXT which          *
! *                             coincides with SAMPLE. If the TEXT       *
! *                             contains more than one elements which    *
! *                             coincides with sample the index of the   *
! *                             first element is returned.               *
! *                                                                      *
! *  ###  21-JAN-1992    LTM_DIF   v1.4  (c)  L. Petrov 26-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  LTM_DIF
        INTEGER*4  IREG, KSTR
        CHARACTER  TEXT(KSTR)*(*), SAMPLE*(*)
        INTEGER*4  IRG, ILT, ILS, IL, J1
        LOGICAL*4, EXTERNAL :: MATCH_WILD
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
        IF ( LOC(IREG) .EQ. 0 ) THEN
             IRG=0
          ELSE
             IRG=IREG
        END IF
!
        IF ( IRG .LT. 0  .OR.  IRG .GT. 3 ) IRG = 0
        IF ( KSTR .LE. 0 ) THEN
             LTM_DIF = -1
             RETURN
        END IF
!
! ----- Compute IL -- the length of the strings under comparison
!
        ILT=LEN(TEXT(1))
        IF ( IRG .NE. 1 ) ILS = ILEN(SAMPLE)
        IF ( IRG .EQ. 1 ) ILS = LEN(SAMPLE)
        IL = MIN ( ILT, ILS )
        IF ( IL .EQ. 0 ) THEN
             LTM_DIF=-1
             RETURN
        END IF
        LTM_DIF=0
!
! ----- SCan text array
!
        DO 410 J1=1,KSTR
           IF ( IRG.EQ.0 .OR. IRG.EQ.1 ) THEN
!
! ------------- Exact comparison
!
                IF ( TEXT(J1)(1:IL) .EQ. SAMPLE(1:IL) ) THEN
                     IF ( IRG == 0 ) THEN
                          LTM_DIF = J1
                          RETURN
                        ELSE IF ( IRG == 1 .AND. ILT > ILS ) THEN
!
! ----------------------- Now check for three cases if exact comparsion was requested
!
                          IF ( ILEN(TEXT(J1)(IL+1:)) == 0 ) THEN
                               LTM_DIF = J1
                               RETURN
                          END IF
                        ELSE IF ( IRG == 1 .AND. ILS > ILT ) THEN
                          IF ( ILEN(SAMPLE(IL+1:)) == 0 ) THEN
                               LTM_DIF = J1
                               RETURN
                          END IF
                        ELSE IF ( IRG == 1 .AND. ILS == ILT ) THEN
                          LTM_DIF = J1
                          RETURN
                     END IF
                END IF
             ELSE IF ( IRG.EQ.2 ) THEN
!
! ------------- Comparion including wild card symbols in SAMPLE
!
                IF ( MATCH_WILD ( TEXT(J1)(1:I_LEN(TEXT(J1))), &
     &                            SAMPLE(1:ILS) ) ) THEN
                     LTM_DIF=J1
                     RETURN
                END IF
!                ILF=STR$MATCH_WILD ( TEXT(J1)(1:IL), SAMPLE(1:IL) )
!              IF ( ILF.EQ.STR$_MATCH ) THEN
!                     LTM_DIF=J1
!                     RETURN
!                END IF
             ELSE IF ( IRG.EQ.3 ) THEN
!
! ------------- Comparion including wild card symbols in TEXT(J1)
!
                IF ( MATCH_WILD ( SAMPLE(1:ILS), &
     &                            TEXT(J1)(1:I_LEN(TEXT(J1))) ) ) THEN
                     LTM_DIF=J1
                     RETURN
                END IF
           END IF
  410   CONTINUE
        RETURN
        END  !#!  LTM_DIF #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE EXWORD ( STR, MIND, LIND, IND, REG, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine EXWPRD scans the input line and finds words. A word is   *
! *   a substring separated by one or more delimiter symbols. The string *
! *   REG contains the list of delmiters: any character of the string    *
! *   REG is consider as a delmiter. EXWORD returns the total number of  *
! *   words, LIND, and the array of the indeces of the first and the     *
! *   last character of the word: IND. IND(1,I) is the position of the   *
! *   beginning of the I-th word, IND(2,I) is the position of the end of *
! *   the I-th word.                                                     *
! *                                                                      *
! *   If the string does not have delimiters, then LIND=1, IND(1,1)=1    *
! *   adn IND(2,1)=LEN(STR)                                              *
! *                                                                      *
! *     Example:                                                         *
! *              CHARACTER REG*5, STR*20                                 *
! *              INTEGER   IND(2,4)                                      *
! *              STR='WORD BOOK/  PHRASE  '                              *
! *              CALL  EXWORD ( STR, 4, LIND, IND, REG(1:2), STR, -1 )   *
! *                                                                      *
! *     Result:                                                          *
! *                                                                      *
! *              LIND=3                                                  *
! *               IND(1,1)=1    IND(2,1)=4                               *
! *               IND(1,2)=6    IND(2,2)=9                               *
! *               IND(1,3)=13   IND(2,3)=18                              *
! *                                                                      *
! * ________________________ Input paramters: __________________________ *
! *                                                                      *
! *   STR ( CHARACTER ) -- The string under investigation. NB: trailing  *
! *                        blanks and binary zeroes are significant.     *
! *  MIND ( INTEGER*4 ) -- The maximal number of words which the string  *
! *                        might have.                                   *
! *   REG ( CHARACTER, OPT ) -- The string which contains delimiters    *
! *                             which separate words. If REG is omitted, *
! *                             then the delimiter blank and a binary    *
! *                             zero are to be used. NB: trailing        *
! *                             blanks and binary zeroes are significant.*
! *                                                                      *
! * ________________________ Output parameters. ________________________ *
! *                                                                      *
! *  LIND ( INTEGER*4 ) -- The total number of found words.              *
! *   IND ( INTEGER*4 ) -- Array of dimenstion (2,MIND). The I-th raw    *
! *                        of this array contains the index of the       *
! *                        beginning and the end of the I-th word.       *
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
! *      IUER=0  --  sucessfull completion.                              *
! *      IUER=1  --  Wrong type of the first argument.                   *
! *      IUER=2  --  Argument MIND is unacessible for reading.           *
! *      IUER=3  --  Argument LIND is unacessible wor writing.           *
! *      IUER=4  --  Array  IND is unacessible wor writing.              *
! *      IUER=5  --  Wrong type of  the REG argument.                    *
! *      IUER=6  --  Parsing  is not completed: the total number of      *
! *                  words exceeds MIND.                                 *
! *                                                                      *
! *  ###  27-FEB-1992    EXWORD   v5.3  (c)  L. Petrov  15-AUG-2005 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*), REG*(*), OGR*255, ST*10
        INTEGER*4  MIND, LIND, IND(2,MIND), IUER, I_LEN ! , IER
        INTEGER*1  TAB(0:255)
        INTEGER*4  LN, LR, IP, J1, J2
        INTEGER*4, EXTERNAL :: LIB$SPANC, LIB$SCANC
#if defined (GNU) || defined (SUN)
        INTEGER*1, EXTERNAL :: INT1
#endif
!
        IF ( LOC(REG) .EQ. 0 ) THEN
             LR=2
             OGR(1:LR)=CHAR(0)//' '
          ELSE
             LR=LEN(REG)
             IF ( LR .LE. 0 ) THEN
                  LR=2
                  OGR(1:LR)=CHAR(0)//' '
                ELSE
                  OGR(1:LR)=REG
             END IF
        END IF
        LIND=0
!
! ----- Zeroing the delimeters table
!
        CALL NOUT ( 256, TAB )
!
! ----- Set the delimeters table
!
        DO 410 J1=1,LR
           TAB(ICHAR(OGR(J1:J1)))=1
  410   CONTINUE
        LN=LEN(STR)  !  string length
!
! ----- find the first character which is not a delimiter
!
        IP=LIB$SPANC ( STR, TAB, INT1(1) )
        IF ( IP.EQ.0 ) THEN
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
!
! ----- Cycle over the maximal number of words
!
        DO 420 J2=1,MIND
           IND(1,J2)=IP
!
! -------- Search for a delimiter
!
           IP=(IP-1) + LIB$SCANC ( STR(IP:), TAB, INT1(1) )
           IF ( IP .EQ. (IND(1,J2)-1) ) THEN
!
! ------------- Delimiter was not found
!
                IND(2,J2)=LN
                LIND=J2
                CALL ERR_LOG ( 0, IUER )
                RETURN
             ELSE
!
! ------------- Thanks God, we found a delimiter
!
                IND(2,J2)=IP-1
           END IF
!
! -------- Search for the character whcih is not a delimiter
!
           IP=(IP-1) + LIB$SPANC ( STR(IP:), TAB, INT1(1) )
           IF ( IP.EQ.IND(2,J2) ) THEN
!
! ------------- Alas, we did not find a character whcih is not a delimiter :-(
!
                LIND=J2
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
  420   CONTINUE
!
! ----- the number of wrods in STR is greater than MIND
!
        LIND=MIND
        CALL CLRCH   ( ST )
        CALL INCH    ( MIND, ST )
        CALL ERR_LOG ( 6, IUER, 'EXWORD', 'Too many words. Argument MIND '// &
     &                 'is too small.  MIND='//ST(1:I_LEN(ST)) )
        RETURN
        END  !#!  EXWORD  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BLANK_TO_UNDERSCORE ( STR )
      IMPLICIT   NONE
      CHARACTER  STR*(*)
      INTEGER*4  J1
      IF ( LEN(STR) .LE. 0 ) RETURN
      DO 410 J1=1,LEN(STR)
         IF ( STR(J1:J1) == ' ' ) STR(J1:J1) = '_'
 410  CONTINUE
      RETURN
      END  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UNDERSCORE_TO_BLANK ( STR )
      CHARACTER  STR*(*)
      DO 410 J1=1,LEN(STR)
         IF ( STR(J1:J1) .EQ. '_' ) STR(J1:J1) = ' '
 410  CONTINUE
      RETURN
      END  !#!  UNDERSCORE_TO_BLANK  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   INDEX_I1 ( STR, SUBSTR )
      IMPLICIT   NONE 
      CHARACTER  STR*(*), SUBSTR*(*)
      INTEGER*4  INDEX_I1
!
      INDEX_I1 = INDEX ( STR, SUBSTR )
!
      RETURN
      END  FUNCTION   INDEX_I1  !#!#
