      PROGRAM    EOP_FCS_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL EOP_FCS_MAIN()
      END  PROGRAM  EOP_FCS_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOP_FCS_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  EOP_FCS_MAIN
! *                                                                      *
! *  ### 02-MAR-2016  EOP_FCS_MAIN   v1.1 (c) L. Petrov 03-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
!
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      TYPE     ( MALO__EOP_TYPE ) :: EOP
      TYPE     ( NERS__TYPE     ) :: NERS
      CHARACTER  FILHEO*128, FILOUT*128, STR*128
      INTEGER*4  J1, J2, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL EOP_INIT ( EOP )
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: eop_fcs config_file filout [verbosity]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, EOP%CNF_FILE )
           CALL GETARG ( 2, FILOUT       )
           CALL GETARG ( 3, STR          )
           IF ( IARGC() .GE. 3 ) THEN
                CALL CHIN   ( STR, EOP%IVRB   )
              ELSE
                EOP%IVRB = 0
           END IF 
      END IF 
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6601, IUER, 'EOP_FCS_MAIN', 'Error in an attempt '// &
     &         'to allocate memory for object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6602, IUER, 'EOP_FCS_MAIN', 'Error in an attempt '// &
     &         'to initialize object EOP_FCS_MAIN' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL PARSE_EOP_CONF ( EOP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6603, IUER, 'EOP_FCS_MAIN', 'Error in parsing '// &
     &         'configuration file '//EOP%CNF_FILE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_LOAD_EOP ( MAL(1), EOP, NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6604, IUER, 'EOP_FCS_MAIN', 'Error in loading '// &
     &         'the EOP object' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL EOP_FCS ( MAL(1), EOP, NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6605, IUER, 'EOP_FCS_MAIN', 'Error in an attempt '// &
     &         'to generate EOP forecaset' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL EOP_FCS_WRI ( NERS, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6606, IUER, 'EOP_FCS_MAIN', 'Error in an attempt '// &
     &         'to write the EOP forecaset into the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      IF ( EOP%IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      CALL FCS_QUIT ( NERS )
!
      END  SUBROUTINE  EOP_FCS_MAIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLO_EOP ( EOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PLO_EOP
! *                                                                      *
! *  ### 07-MAR-2016     PLO_EOP   v1.0 (c)  L. Petrov  07-MAR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE    ( MALO__EOP_TYPE ) :: EOP
      INTEGER*4  IUER
      INTEGER*4  MIND, MP
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( MP = 128*1024 )
      CHARACTER  STR*128
      REAL*8     T1(MP), X1(MP), E1(MP), T2(MP), X2(MP), E2(MP)
      INTEGER*4  LIND, IND(2,MIND), J1, J2, J3, IS1, IS2, ICMP, N1, N2, LN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ_LINE
!
 710  CONTINUE 
        LN = READ_LINE ( 'Enter new is1, is2, cmp >> ', STR )
        CALL EXWORD ( STR, MIND, LIND, IND, CHAR(32)//CHAR(0)//CHAR(9)//',', IER )
        IF ( LIND < 3 ) THEN
             WRITE ( 6, * ) 'Too few words: ', LIND
             GOTO 710
        END IF
        CALL CHIN ( STR(IND(1,1):IND(2,1)), IS1 )
        CALL CHIN ( STR(IND(1,2):IND(2,2)), IS2 )
        CALL CHIN ( STR(IND(1,3):IND(2,3)), ICMP )
!
        IF ( IS1 < 1 .OR. IS1 > M__EOPS ) THEN
             WRITE ( 6, * ) 'is1 is out of range'
             GOTO 710
        END IF
!
        IF ( IS2 < 1 .OR. IS2 > M__EOPS ) THEN
             WRITE ( 6, * ) 'is2 is out of range'
             GOTO 710
        END IF
!
        IF ( ICMP < 1 .OR. ICMP > 6 ) THEN
             WRITE ( 6, * ) 'icmp is out of range'
             GOTO 710
        END IF
!
        N1 = EOP%EOPS(IS1)%NP
        DO 410 J1=1,N1
           T1(J1) = EOP%EOPS(IS1)%SER(J1)%TIM
           IF ( ICMP .LE. 3 ) THEN
                X1(J1) = EOP%EOPS(IS1)%SER(J1)%E(ICMP)
              ELSE 
                X1(J1) = EOP%EOPS(IS1)%SER(J1)%ER(ICMP-3)
           END IF
 410    CONTINUE 
!
        N2 = EOP%EOPS(IS2)%NP
        DO 420 J2=1,N2
           T2(J2) = EOP%EOPS(IS2)%SER(J2)%TIM
           IF ( ICMP .LE. 3 ) THEN
                X2(J2) = EOP%EOPS(IS2)%SER(J2)%E(ICMP)
              ELSE 
                X2(J2) = EOP%EOPS(IS2)%SER(J2)%ER(ICMP-3)
           END IF
 420    CONTINUE 
        CALL DIAGI_2 ( N1, T1, X1, N2, T2, X2, IER)
      GOTO 710
      RETURN
      END  SUBROUTINE PLO_EOP  !#!  
