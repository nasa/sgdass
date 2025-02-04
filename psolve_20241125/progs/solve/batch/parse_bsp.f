      SUBROUTINE PARSE_BSP ( STRING, L_BSP, ADR_BSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_BSP 
! *                                                                      *
! *  ### 15-MAR-2005   PARSE_BSP   v1.0 (c)  L. Petrov  15-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'bsp.i'
      CHARACTER  STRING*(*)
      ADDRESS__TYPE :: ADR_BSP
      INTEGER*4  L_BSP, IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 1024 )
      PARAMETER  ( MIND =   32 )
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128, NEXT*16, NAME_EXPAND*128, REG*4
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'\' )
      TYPE ( BSPSTA__TYPE ), ALLOCATABLE :: BSP(:)
      SAVE   BSP
      INTEGER*2  LN
      INTEGER*4  IOS, J1, J2, J3, J4, J5, IP, IL, IAT, NBUF, &
     &           LIND, IND(2,MIND), IER
      LOGICAL*4  FL_BSP, LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*2, EXTERNAL :: CFREAD
!
      STR = STRING
      CALL CHASHL  ( STR ) 
      IF ( STR(1:5) .EQ. 'NONE ' ) THEN
!
! -------- Nothing to do
!
           L_BSP = 0
           ADR_BSP = 0
           LN = CFREAD ( STRING )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Aloocate memory for the command buffer
!
      ALLOCATE ( BUF(MBUF), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL IINCH ( IOS, STR ) 
           CALL ERR_LOG ( 8751, IUER, 'PARSE_BSP', 'Error in an attempt to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes' )
           RETURN 
      END IF
!
! --- Read the control file line by line till we end of continuation. Expand
! --- @-names and read their context into the buffer
!
      NBUF = 1
      BUF(NBUF) = STRING
      DO 410 J1=2,MBUF
!
! ------ Read the next line from the control file
!
         LN = CFREAD ( STRING )
         IF ( ILEN(STRING) ==  0  ) GOTO 410
         IF ( STRING(1:1)  == '*' ) GOTO 410
         NBUF = NBUF + 1
         BUF(NBUF) = STRING
         IF ( STRING(ILEN(STRING):ILEN(STRING)) .NE.  '\' ) THEN
!
! ----------- Aga. The last character is not continuation. This means that
! ----------- we reached the end of the sequence of continuation lines. Good!
!
              GOTO 810
         END IF
         IAT = INDEX ( BUF(NBUF), '@' )
         IF ( IAT .GT. 0 ) THEN
!
! ----------- Expand the name and expand the buffer
!
              NAME_EXPAND = BUF(NBUF)(IAT+1:)
              IP = INDEX ( NAME_EXPAND, ' ' ) 
              CALL CLRCH ( NAME_EXPAND(IP:) )
!
! ----------- Read the file into the buffer
!
              CALL ERR_PASS ( IUER, IER )
              CALL BUFFER_EXPAND ( MBUF, NBUF, BUF, IAT, NAME_EXPAND, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8752, IUER, 'PARSE_BSP', 'Failure to'// &
     &                 ' expand line @'//NAME_EXPAND(1:I_LEN(NAME_EXPAND))// &
     &                 ' in processing keyword SPLINE_DISPLACEMENTS' ) 
                   DEALLOCATE ( BUF ) 
                   RETURN 
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      CALL CLRCH ( STRING )
!
! --- First pass: determine the number of stations with apriori B-spline models
!
      L_BSP = 0
      DO 420 J2=1,NBUF
         IF ( INDEX ( BUF(J2), 'BSP' ) > 0 ) THEN
              L_BSP = L_BSP + 1
         END IF
 420  CONTINUE 
      IF ( L_BSP .EQ. 0 ) THEN
           CALL ERR_LOG ( 8753, IUER, 'PARSE_BSP', 'No BSP qualifiers '// &
     &         ' were found in processing the keyword SPLINE_DISPLACEMENTS' ) 
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
      IF ( L_BSP .GT. M__BSP ) THEN
           CALL ERR_LOG ( 8754, IUER, 'PARSE_BSP', 'To many NAME '// &
     &         'qualifiers were found in processing the keyword '// &
     &         'SPLINE_DISPLACEMENTS. Check parameter M__BSP in '// &
     &         '$PSOLVE_ROOT/include/solve.i' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
! --- Allocate memory for HPE object
!
      ALLOCATE ( BSP(L_BSP), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8755, IUER, 'PARSE_BSP', 'Failure to '// &
     &         'allocate in memory object BSP' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
      CALL NOUT ( L_BSP*SIZEOF(BSP(1)), BSP)
!
! --- The second pass. Parse the keywords
!
      L_BSP = 0
      FL_BSP = .FALSE.
      NEXT = 'BSP'
      DO 430 J3=1,NBUF
         IL = I_LEN(BUF(J3))
         IF ( BUF(J3)(IL:IL) .EQ. '\' ) THEN
              CALL CLRCH ( BUF(J3)(IL:IL) )
         END IF
!
         IL = ILEN(BUF(J3))
         IF ( IL .LE. 0 ) GOTO 430
!
! ------ Parse the line into words
!
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, REG, -3 )
         DO 440 J4=1,LIND
            IF ( NEXT == 'BSP' ) THEN
                 IF ( .NOT. FL_BSP ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'BSP' ) THEN
                           FL_BSP  = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8756, IUER, 'PARSE_BSP', 'Error '// &
     &                         'in parsing keyword SPLINE_DISPLACEMENTS: '// &
     &                         'the qualifier BSP was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( BSP ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE
                      L_BSP = L_BSP + 1
                      BSP(L_BSP)%FILE_NAME = BUF(J3)(IND(1,J4):IND(2,J4)) 
                      INQUIRE ( FILE=BSP(L_BSP)%FILE_NAME, EXIST=LEX )
                      IF ( .NOT. LEX ) THEN
                           CALL ERR_LOG ( 8757, IUER, 'PARSE_BSP', 'Error '// &
     &                         'in parsing keyword SPLINE_DISPLACEMENTS: '// &
     &                         'the file '//BUF(J3)(IND(1,J4):IND(2,J4))// &
     &                         '" was found' )
                           DEALLOCATE ( BSP ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      FL_BSP  = .FALSE.
                 END IF
               ELSE 
                 CALL ERR_LOG ( 8758, IUER, 'PARSE_BSP', 'Unknown qualifier '// &
     &                BUF(J3)(IND(1,J4):IND(2,J4))//'" was found in '// &
     &               'an attempt to parse keyword SPLINE_DISPLACEMENTS ' )
                 DEALLOCATE ( BSP ) 
                 DEALLOCATE ( BUF ) 
                 RETURN 
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      DO 450 J5=1,L_BSP
         CALL ERR_PASS ( IUER, IER )
         CALL READ_BSP ( BSP(J5), IER ) 
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8759, IUER, 'PARSE_BSP', 'Error in an attempt '// &
     &            'to read file with coefficients of expansion of site '// &
     &            'displacements with B-spline basis '//BSP(L_BSP)%FILE_NAME )
              DEALLOCATE ( BSP ) 
              RETURN 
         END IF
 450  CONTINUE 
!
      ADR_BSP = LOC(BSP)
      DEALLOCATE ( BUF ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_BSP 
