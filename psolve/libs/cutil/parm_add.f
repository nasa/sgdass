      SUBROUTINE PARM_ADD ( LNAME, ISTR_LEN, NPARM, MAX_PARM, CBUF )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 istr_len
!-----END of imp added lines.
!
      INTEGER*4  NPARM, MAX_PARM
      CHARACTER  STR*32
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*4, EXTERNAL :: I_LEN
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      CHARACTER*(ISTR_LEN) LNAME(*),CBUF
      IF ( NPARM .LT. MAX_PARM ) THEN
           NPARM=NPARM+1
           LNAME(NPARM)=CBUF
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( MAX_PARM, STR )
           CALL ERR_LOG ( 8481, -3, 'PARM_ADD', 'Failure to add a new '// &
     &         'parameter '//CBUF(1:I_LEN(CBUF))//' in the list: '// &
               'the number of parameters exceeded the limit MAX_PAR '// &
     &         STR )
           CALL EXIT ( 1 )
      ENDIF
      RETURN
      END
