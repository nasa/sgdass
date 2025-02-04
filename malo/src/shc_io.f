      SUBROUTINE SHC_WRITE ( DEG, SHC_FMT, NUM_SPH, SPH, MJD, TAI, L_TXT, &
     &                       C_TXT, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SHC_WRITE writes down spherical harmonic coefficients      *
! *   into an output file FILOUT in a binary SHC-format.                 *
! *                                                                      *
! *  ### 21-SEP-2012   SHC_WRITE   v2.0 (c)  L. Petrov  21-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INTEGER*4  DEG, NUM_SPH, L_TXT, MJD, IUER
      REAL*8     SPH(2,0:DEG,0:DEG,NUM_SPH), TAI
      CHARACTER  SHC_FMT*(*), C_TXT(L_TXT)*(*), FILOUT*(*)
      REAL*4,    ALLOCATABLE :: ARR_R4(:,:)
      REAL*8,    ALLOCATABLE :: ARR_R8(:,:)
      CHARACTER  STR*128, BUF(MALO__SHC_LTXT)*(MALO__SHC_LSTR)
      INTEGER*8, EXTERNAL :: WRITE
      INTEGER*4  LUN, J1, J2, J3, J4, IP, IS, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( SHC_FMT == 'REAL*8' ) THEN
           CONTINUE
         ELSE IF ( SHC_FMT == 'REAL*4' ) THEN
           CONTINUE
         ELSE 
           CALL ERR_LOG ( 5311, IUER, 'SHC_WRITE', 'Unsupported SHC format: '// &
    &          STR(1:I_LEN(STR))//' while REAL*8 or REAL*4 were expected' )
           RETURN 
      END IF
!
      WRITE ( UNIT=BUF(1)(1:13), FMT='("DEGREE: ",  I5)' ) DEG
      BUF(2) = 'FORMAT: '//SHC_FMT
      WRITE ( UNIT=BUF(3)(1:10), FMT='("NUM_SET: ", I1)' ) NUM_SPH
      WRITE ( UNIT=BUF(4)(1:10), FMT='("MJD: ",     I5)' ) MJD
      WRITE ( UNIT=BUF(5)(1:12), FMT='("TAI: ",   F7.1)' ) TAI
!
      IF ( L_TXT > 0 ) THEN
           DO 410 J1=1,L_TXT
              BUF(J1+5) = 'COMMENT: '//C_TXT(J1)
 410       CONTINUE 
      END IF
      DO 420 J2=1,L_TXT 
         IP = ILEN(BUF(J2))
         IF ( IP < LEN(BUF(J2)) ) CALL CLRCH ( BUF(J2)(IP+1:) )
         BUF(J2)(MALO__SHC_LSTR:MALO__SHC_LSTR) = CHAR(10)
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5312, IUER, 'SHC_WRITE', 'Failure in an attempt '// &
     &         'to open the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- '//STR )
           RETURN 
      END IF
!
      IS = WRITE ( %VAL(LUN), %REF(SPHE__LABEL//CHAR(10)), &
     &             %VAL(LEN(SPHE__LABEL)+1) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5313, IUER, 'SHC_WRITE', 'Failure in an attempt '// &
     &         'to write the 1st line in the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
        ELSE IF ( IS .NE. LEN(SPHE__LABEL)+1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5314, IUER, 'SHC_WRITE', 'Failure in an attempt '// &
     &         'to write the 1st line in the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- not all bytes have been written' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
!
      IS = WRITE ( %VAL(LUN), %REF(BUF), %VAL(MALO__SHC_LTXT*MALO__SHC_LSTR) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5315, IUER, 'SHC_WRITE', 'Failure in an attempt '// &
     &         'to write a line with comment in the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
        ELSE IF ( IS .NE. MALO__SHC_LTXT*MALO__SHC_LSTR ) THEN
           CALL ERR_LOG ( 5316, IUER, 'SHC_WRITE', 'Failure in an attempt '// &
     &         'to write a line with comment in the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- not all bytes have been written' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
!
      IF ( SHC_FMT == 'REAL*4' ) THEN
           ALLOCATE ( ARR_R4(2,0:DEG) )
         ELSE IF ( SHC_FMT == 'REAL*8' ) THEN
           ALLOCATE ( ARR_R8(2,0:DEG) )
      END IF
      DO 430 J3=1,NUM_SPH
         DO 440 J4=0,DEG
            IF ( SHC_FMT == 'REAL*8' ) THEN
                 ARR_R8(1:2,0:J4) = SPH(1:2,J4,0:J4,J3)
                 IS = WRITE ( %VAL(LUN), ARR_R8, %VAL(8*2*(J4+1)) )
                 IF ( IS .NE. 8*2*(J4+1) ) THEN
                      CALL CLRCH  ( STR )
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 5317, IUER, 'SHC_WRITE', 'Failure in an attempt '// &
     &                    'to write a data record in the output file '// &
     &                    FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
                      IER = 0
                      CALL BINF_CLOSE ( LUN, IER )
                      RETURN 
                 END IF
               ELSE IF ( SHC_FMT == 'REAL*4' ) THEN
                 ARR_R4(1:2,0:J4) = SPH(1:2,J4,0:J4,J3)
                 IS = WRITE ( %VAL(LUN), ARR_R4, %VAL(4*2*(J4+1)) )
                 IF ( IS .NE. 4*2*(J4+1) ) THEN
                      CALL CLRCH  ( STR )
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 5318, IUER, 'SHC_WRITE', 'Failure in an attempt '// &
     &                    'to write a data record in the output file '// &
     &                     FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
                      IER = 0
                      CALL BINF_CLOSE ( LUN, IER )
                      RETURN 
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
      IF ( SHC_FMT == 'REAL*4' ) THEN
           DEALLOCATE ( ARR_R4 )
         ELSE IF ( SHC_FMT == 'REAL*8' ) THEN
           DEALLOCATE ( ARR_R8 )
      END IF
!
      IER = 0
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SHC_WRITE !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SHC_INQ ( FILIN, DEG, SHC_FMT, M_TXT, L_TXT, C_TXT, &
     &                     NUM_SPH, MJD, TAI, DATA_OFFS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SHC_INQ 
! *                                                                      *
! *  ### 21-SEP-2012    SHC_INQ    v1.1 (c)  L. Petrov  20-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INTEGER*4  DEG, M_TXT, L_TXT, NUM_SPH, MJD, DATA_OFFS, IUER
      REAL*8     TAI
      CHARACTER  FILIN*(*), SHC_FMT*(*), C_TXT(M_TXT)*(*)
      INTEGER*8  SIZE_I8
      CHARACTER  STR*256, BUF(MALO__SHC_LTXT)*(MALO__SHC_LSTR), REG*4
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//CHAR(10) )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 ) 
      INTEGER*4  UNIX_DATE, LUN, J1, J2, J3, IP, IR, MAX_SIZE, LIND, &
     &           IND(2,MIND), IER
      INTEGER*8  IS
      INTEGER*8, EXTERNAL :: READ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, INDEX_I1
!
      DATA_OFFS = -1
      CALL CLRCH ( SHC_FMT )
      L_TXT = 0
!
      IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, &
     &                 SIZE_I8 )                                         
      IF ( IS .NE. 0 ) THEN                                              
           CALL GERROR ( STR )                                           
           CALL ERR_LOG ( 5321, IUER, 'SHC_INQ', 'Failure in an attempt '// &
     &         'to collect information about file '//FILIN(1:I_LEN(FILIN))// &
     &          ' '//STR )
           RETURN 
      END IF                                                             
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5322, IUER, 'SHC_INQ', 'Failure in an attempt '// &
     &         'to open input file '//FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), %REF(STR), %VAL(LEN(SPHE__LABEL)+1) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5323, IUER, 'SHC_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
         ELSE IF ( IS .NE. LEN(SPHE__LABEL)+1 ) THEN
           CALL ERR_LOG ( 5324, IUER, 'SHC_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- only a part of data was read' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
      IF ( STR(1:LEN(SPHE__LABEL)) == SPHE__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 5325, IUER, 'SHC_INQ', 'Unrecognzed format '// &
      &         'label in the input file '//FILIN(1:I_LEN(FILIN))// &
      &         ' -- '//STR(1:LEN(SPHE__LABEL))//' while '//SPHE__LABEL// &
      &         ' was expected' )
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), %REF(BUF), %VAL(MALO__SHC_LTXT*MALO__SHC_LSTR) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5326, IUER, 'SHC_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
        ELSE IF ( IS .NE. MALO__SHC_LTXT*MALO__SHC_LSTR ) THEN
           CALL ERR_LOG ( 5327, IUER, 'SHC_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- only a part of data was read' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
      CALL BINF_CLOSE ( LUN, IER )
!
      L_TXT = 0
      CALL CLRCH ( SHC_FMT )
      DO 410 J1=1,MALO__SHC_LTXT 
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DEGREE:'  ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) DEG
              IF ( IER .NE. 0 .OR. DEG < 0 ) THEN
                   CALL TRAN ( 13, STR, STR )
                   CALL ERR_LOG ( 5328, IUER, 'SHC_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key DEGREE: a non-zero '// &
     &                 'integer was expected, bug got '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FORMAT:'  ) THEN
              SHC_FMT = BUF(J1)(IND(1,2):IND(2,2))
              IF ( ( SHC_FMT .NE. 'REAL*8' .AND. &
     &               SHC_FMT .NE. 'REAL*4'       ) ) THEN
                   CALL ERR_LOG ( 5329, IUER, 'SHC_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key FORMAT: one of '// &
     &                 'REAL*8 or READL*4 was expected, bug got '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_SET:'  ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == '*' ) BUF(J1)(IND(1,2):IND(2,2)) = '2'
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) NUM_SPH
              IF ( NUM_SPH .NE. 1  .AND. NUM_SPH .NE. 2 ) THEN
                   CALL ERR_LOG ( 5330, IUER, 'SHC_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key NUM_SET FORMAT: one of '// &
     &                 '1 or 2 was expected, bug got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MJD:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) MJD
              IF ( IER .NE. 0 .OR. MJD < MALO__MJD_MIN .OR. MJD > MALO__MJD_MAX ) THEN
                   CALL TRAN ( 13, STR, STR )
                   CALL ERR_LOG ( 5331, IUER, 'SHC_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key MJD: a opsitive '// &
     &                 'integer in a specific range was expected, '// &
     &                 'bug got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TAI:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F7.1)', IOSTAT=IER ) TAI
              IF ( IER .NE. 0 .OR. TAI < -90000.0 .OR. TAI > 90000.0D0 ) THEN
                   CALL TRAN ( 13, STR, STR )
                   CALL ERR_LOG ( 5332, IUER, 'SHC_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key TAI: a float '// &
     &                 'number in ragem [-90000, 90000] was expected, '// &
     &                 ' bug got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'COMMENT:' ) THEN
              L_TXT = L_TXT + 1
              CALL CLRCH ( C_TXT(L_TXT) )
              C_TXT(L_TXT) = BUF(J1)(IND(1,2):IND(2,LIND))
         END IF
 410  CONTINUE 
 810  CONTINUE 
      DATA_OFFS = LEN(SPHE__LABEL) + 1 + MALO__SHC_LTXT*MALO__SHC_LSTR 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SHC_INQ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SHC_READ ( FILIN, DEG, SHC_FMT, NUM_SETS, DATA_OFFS, &
     &                      ORD_FLAG, SPH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SHC_READ 
! *                                                                      *
! *  ### 21-SEP-2012    SHC_READ   v2.0 (c)  L. Petrov  19-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER  FILIN*(*), SHC_FMT*(*)
      INTEGER*4  DEG, M_TXT, L_TXT, NUM_SETS, DATA_OFFS, ORD_FLAG, IUER
      REAL*8     SPH(2,0:DEG,0:DEG,NUM_SETS)
      INTEGER*8  SIZE_I8, IS
      CHARACTER  STR*256
      REAL*4,    ALLOCATABLE :: ARR_R4(:,:)
      REAL*8,    ALLOCATABLE :: ARR_R8(:,:)
      INTEGER*4  LUN, J1, J2, J3, SEEK_SET, OFFSET_RET, ARG_LN, IER
      INTEGER*8, EXTERNAL :: READ, LSEEK
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, INDEX_I1
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5341, IUER, 'SHC_READ', 'Failure in an attempt '// &
     &         'to open input file '//FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(DATA_OFFS), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. DATA_OFFS ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5342, IUER, 'SHC_READ', 'Failure in an attempt '// &
     &         'to seek for beginning the data section in the input file '// &
     &          FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      IF ( SHC_FMT == 'REAL*4' ) THEN
           ALLOCATE ( ARR_R4(2,0:DEG) )
         ELSE IF ( SHC_FMT == 'REAL*8' ) THEN
           ALLOCATE ( ARR_R8(2,0:DEG) )
         ELSE 
           CALL ERR_LOG ( 5343, IUER, 'SHC_READ', 'Unsupported '// &
     &         'spherical harmonics format string '//SHC_FMT )
           RETURN 
      END IF
!
      DO 410 J1=1,NUM_SETS
         DO 420 J2=0,DEG
            IF ( SHC_FMT == 'REAL*8' ) THEN
                 IS = READ ( %VAL(LUN), ARR_R8, %VAL(8*2*(J2+1)) )
                 IF ( IS .EQ. -1 ) THEN
                      CALL CLRCH  ( STR )
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 5344, IUER, 'SHC_READ', 'Failure in an attempt '// &
     &                    'to read a data record from the input file '// &
     &                     FILIN(1:I_LEN(FILIN))//' -- '//STR )
                      IER = 0
                      CALL BINF_CLOSE ( LUN, IER )
                      RETURN 
                   ELSE IF ( IS < 4*2*(J2+1) ) THEN
                      CALL CLRCH  ( STR )
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 5345, IUER, 'SHC_READ', 'Failure in an attempt '// &
     &                    'to read a data record from the input file '// &
     &                     FILIN(1:I_LEN(FILIN))//' -- not all the data '// &
     &                     'have been read' )
                      IER = 0
                      CALL BINF_CLOSE ( LUN, IER )
                      RETURN 
                 END IF
                 SPH(1:2,J2,0:J2,J1) = ARR_R8(1:2,0:J2) 
               ELSE IF ( SHC_FMT == 'REAL*4' ) THEN
                 IS = READ ( %VAL(LUN), ARR_R4, %VAL(4*2*(J2+1)) )
                 IF ( IS .EQ. -1 ) THEN
                      CALL CLRCH  ( STR )
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 5346, IUER, 'SHC_READ', 'Failure in an attempt '// &
     &                    'to read a data record from the input file '// &
     &                     FILIN(1:I_LEN(FILIN))//' -- '//STR )
                      IER = 0
                      CALL BINF_CLOSE ( LUN, IER )
                      RETURN 
                   ELSE IF ( IS < 4*2*(J2+1) ) THEN
                      CALL CLRCH  ( STR )
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 5347, IUER, 'SHC_READ', 'Failure in an attempt '// &
     &                    'to read a data record from the input file '// &
     &                     FILIN(1:I_LEN(FILIN))//' -- not all the data '// &
     &                     'have been read' )
                      IER = 0
                      CALL BINF_CLOSE ( LUN, IER )
                      write ( 6 ,* ) ' j2=',j2, ' j1= ',j1, ' is= ',is ! %%%
                      RETURN 
                 END IF
                 IF ( ORD_FLAG == MALO__DIR ) THEN
                      SPH(1:2,J2,0:J2,J1) = ARR_R4(1:2,0:J2) 
                    ELSE IF ( ORD_FLAG == MALO__TRA ) THEN
                      SPH(1:2,0:J2,J2,J1) = ARR_R4(1:2,0:J2) 
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
      CALL BINF_CLOSE ( LUN, IER )
      IF ( SHC_FMT == 'REAL*4' ) THEN
           DEALLOCATE ( ARR_R4 )
         ELSE IF ( SHC_FMT == 'REAL*8' ) THEN
           DEALLOCATE ( ARR_R8 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SHC_READ  !#!#
