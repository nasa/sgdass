      SUBROUTINE READ_LOVE ( MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   You should not interpret the name of this routine verbatim. :-)    *
! *   READ_LOVE  reads the file with load Love numbers and extracts      *
! *   the k Love numbers of degrees in the range [0, IDEG]. These        *
! *   Love numbers are put in the output array LOVE_K.                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    MALO ( MALO__TYPE     ) -- Data structure that keeps parameters   *
! *                               relevant to MALO (MAss LOading)        *
! *                               package.                               *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ### 05-JAN-2005    READ_LOVE  v3.0 (c)  L. Petrov  22-NOV-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  IUER
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      REAL*8     H2, L2, K2
      INTEGER*4  KDEG, J1, IFMT, NBUF, LIND, IND(2,MIND), IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( ASSOCIATED ( MALO%LOVE ) ) THEN
           DEALLOCATE ( MALO%LOVE )
      END IF
!
      ALLOCATE ( MALO%LOVE(0:MALO__MDIM,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*3*(MALO__MDIM+1), STR )
           CALL ERR_LOG ( 6261, IUER, 'READ_LOVE', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'Love vector' )
           RETURN 
      END IF
      MALO%NLOVE = 0
      MALO%LOVE = 0.0
!
! --- Allocate memory for the temporary buffer
!
      ALLOCATE ( BUF(MALO__MDIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( LEN(BUF(1))*MALO__MDIM, STR )
           CALL ERR_LOG ( 6261, IUER, 'READ_LOVE', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'buffer BUF' )
           RETURN 
      END IF
!
! --- Read the input file to the temporary buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( MALO%CONF%LOVE_FILE, MALO__MDIM, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2912, IUER, 'READ_LOVE', 'Error in an attempt '// &
     &         'to read Love file '//MALO%CONF%LOVE_FILE )
           DEALLOCATE  ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(LOVE_NUMBERS__LABEL)) .EQ. LOVE_NUMBERS__LABEL ) THEN
           IFMT = 1
         ELSE IF ( BUF(1)(1:LEN(LOVE_NUMBERS__LABEL)) .EQ. LOVE_NUMBERS__LABEL_01 ) THEN
           IFMT = 2
         ELSE 
           CALL ERR_LOG ( 2913, IUER, 'READ_LOVE', 'The input file '// &
     &          MALO%CONF%LOVE_FILE(1:I_LEN(MALO%CONF%LOVE_FILE))// &
     &          ' is not in loading Love numbers format. Sorry, no love ;-(' )
           DEALLOCATE  ( BUF )
           RETURN 
      END IF
!
! --- Parse the first IDEG+1 non-comments lines
!
      KDEG = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  .EQ. '*' ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '!' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
!
! ------ Parse the record
!
         READ ( UNIT=BUF(J1)(IND(1,1):IND(2,1)),  FMT='(I6)'     ) KDEG
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)),  FMT='(F17.10)' ) MALO%LOVE(KDEG,MALO__H)
         READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)),  FMT='(F17.10)' ) MALO%LOVE(KDEG,MALO__L)
         READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)),  FMT='(F17.10)' ) MALO%LOVE(KDEG,MALO__K)
         MALO%NLOVE = KDEG
 410  CONTINUE 
!
      DEALLOCATE  ( BUF )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  READ_LOVE  !#!#
