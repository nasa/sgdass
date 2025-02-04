#include <mk5_preprocessor_directives.inc>
      FUNCTION  PAUSE ( STR )
! ************************************************************************
! *                                                                      *
! *   Function PAUSE prints a line STR in the screen and waits for       *
! *   a user hitting any key.                                            *
! *                                                                      *
! *  ### 17-JAN-1989     PAUSE     v1.0 (c)  L. Petrov  10-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4 PAUSE
      CHARACTER STR*(*)
      CHARACTER ASIM
      INTEGER*4 ISIM
      INTEGER*4, EXTERNAL :: INSIM
!
      CALL NEG
      CALL PRCH ( '---  PAUSE  ---' )
      CALL UN_NEG
      CALL CURR ( 2 )
      IF ( LOC(STR) .NE. 0 ) CALL PRCH ( STR )
      CALL CURR ( 2 )
      PAUSE = INSIM ( ASIM, ISIM )
      CALL CLSTR ()
      RETURN
      END  !#!  PAUSE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRCH ( STR )
      CHARACTER  STR*(*)
      INTEGER*4, EXTERNAL :: I_LEN
      WRITE ( 6, 110 ) STR(1:I_LEN(STR))
 110  FORMAT ( A,$ )
      CALL FLUSH ( 6 )
      RETURN
      END  !#!  PRCH  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRI_BUF ( BUF, NBUF, LSTR_I, ICAR_I )
      CHARACTER  BUF(NBUF)*(*)
      INTEGER*4  ICAR, LSTR
      INTEGER*4, EXTERNAL :: I_LEN
!
      ICAR=1
      IF ( LOC(ICAR_I) .NE. 0 ) ICAR=ICAR_I
      IF ( ICAR .NE. 0  .AND.  ICAR .NE. 1 ) ICAR=1
!
      LSTR=LEN(BUF(1))
      IF ( LOC(LSTR_I) .NE. 0 ) LSTR=LSTR_I
      DO 410 J1=1,NBUF
         LN=I_LEN(BUF(J1))
         IF ( LN .GT. LSTR ) LN=LSTR
         IF ( ICAR .EQ. 0 ) THEN
              CALL PRCH ( BUF(J1)(1:LN) )
           ELSE IF ( ICAR .EQ. 1 ) THEN
              CALL PRCH ( CHAR(10)//BUF(J1)(1:LN)//CHAR(13) )
         END IF
 410  CONTINUE
      RETURN
      END  !#!  PRI_BUF  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION HIT_CONT ( STR, NPAR )
! ************************************************************************
! *                                                                      *
! *   Function HIT_CONT
! *                                                                      *
! *  ### 13-MAR-1993   HUIT_CONT   v1.2 (c)  L. Petrov  11-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  HIT_CONT, ISIM, NPAR
      CHARACTER  STR*(*), ASIM*1
      LOGICAL*4  FL_OUT_TERM
      INTEGER*8  ADR_LOC
      INTEGER*4  ADR_LOC_I4(2)
      INTEGER*4, EXTERNAL :: INSIM
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
#else
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
#endif
      ADR_LOC = LOC(STR)
      CALL MEMCPY ( ADR_LOC_I4, ADR_LOC, %VAL(8) )
      IF ( ADR_LOC_I4(1) == 0 .AND. ADR_LOC_I4(2) == 0 ) THEN
           CALL PRCH ( 'Hit any key to proceed  '//CHAR(1) )
       ELSE
           CALL PRCH ( STR )
      END IF
#ifdef SUN
        FL_OUT_TERM = FUNC_ISATTY ( 1 ) ! Flag whether the unit 5 is a terminal
#else
#ifdef GNU
        FL_OUT_TERM = ISATTY      ( 5 ) ! Flag whether the unit 5 is a terminal
#else
        FL_OUT_TERM = FUNC_ISATTY ( 5 ) ! Flag whether the unit 5 is a terminal
#endif
#endif
      IF ( FL_OUT_TERM ) THEN
           HIT_CONT = INSIM ( ASIM, ISIM )
           CALL CLSTR ()
         ELSE
           HIT_CONT = 13
      END IF
      RETURN
      END  !#!  HIT_CONT  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE INSTR ( PROMPT, IBG, BEGIN, STR, IUER )
! ************************************************************************
! *                                                                      *
! *     ������������  INSTR  ������ ������ � ���������� SYS$INPUT.       *
! *     ���� ����� �������� PROMPT, �� � ����� ������ ��������� ������   *
! *     PROMPT ( � PROMPT ��������� ������� ������������ ). �����, ����  *
! *     BEGIN ������, ��������� IBG �������� ������-�������. ������      *
! *     ������� � ������ ������� ������� ������. �������. ����          *
! *     ��������� ����������� ����� SYS$INPUT  �������� ��� ���������,   *
! *     �� ��������� ������� ����� ������. ���� ����������� �� �������   *
! *     ������� "������� �������" ��� �� ���������� ���� ������ STR.     *
! *     ���� ���� �������������� � ��������� Microterm 5530 ( ���������� *
! *     ������ TERM_TYPE="43" ��� "44" ), �� ������� F20 �����������     *
! *     �������/LATIN ��������.                                          *
! *     ���� ��������� ����������� ����� SYS$INPUT �������� ��� �����,   *
! *     �� �������� ������ ��������� READ. � ���� ������ �������         *
! *     ������������� �� �����������.                                    *
! *                                                                      *
! * ________________________ ������� ���������: ________________________ *
! *                                                                      *
! *     PROMPT ( CHARACTER, OPT ) -- ������ ������������ ���������. ���  *
! *                                  ������ �� �������� ����������       *
! *                                  ��������� �������. ��� ��������     *
! *                                  ������ �� ����� ������� �� �������  *
! *                                  ���������.                          *
! *        IBG ( INTEGER*4, OPT ) -- ���������� �������� �               *
! *                                  ������-�������.                     *
! *      BEGIN ( CHARACTER, OPT ) -- ����������� ������-������. �������� *
! *                                  ������ ���������� ���� ���������   *
! *                                  �������: ��������� �������,         *
! *                                  ����������� �������� � �����        *
! *                                  ������� � �.�. ��� ����� �������    *
! *                                  CTRL/U ������ ���������.            *
! *                                  �� ��������� ������� ���. ��������  *
! *                                  ������ ����� ����������� ������.    *
! *                                                                      *
! * ________________________ �������� ���������: _______________________ *
! *                                                                      *
! *        STR ( CHARACTER ) -- �������� ������.                        *
! *                                                                      *
! * ___________________ �������������� ���������: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- �������� ������:                       *
! *             ������� ��������  --  ����� ��������� ������:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- ����������� ���� ������.                             *
! *      IUER=-1 -- ����������� ���� IUER=0 � ������ �����������         *
! *                 ���������� � ����� ���������������� ���������        *
! *                 � ������ ������������� ������.                       *
! *      IUER<-1 -- ����������� ���� IUER=0 � ������ �����������         *
! *                 ����������, ����� ���������������� ��������� �       *
! *                 ���������� ������ � ������ ������������� ������.     *
! *      ���� IUER ������, �� ������� �������� ����������� ������ -1     *
! *             �������� ��������  --  ��� ������ ( ���� IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             �������� ��� ������ ):                                   *
! *      IUER=0  --  ���������� ����������.                              *
! *                                                                      *
! *  ###  09-JUN-92     INSTR     V2.0  (c) ������ �.�.  20-MAR-95  ###  *
! *
! ************************************************************************
        IMPLICIT   NONE 
        CHARACTER  PROMPT*(*), BEGIN*(*), STR*(*)
        CHARACTER  ISTR*132, BEG*132, OSTR*132
        INTEGER*4  IBG, IUER
        INTEGER*4  IP, IZ, IBB, IBG_R, I1, LGT
        INTEGER*4, EXTERNAL :: ILEN, I_LEN, INTX
!
        CALL CLRCH ( ISTR )
        IF ( LOC(PROMPT) .NE. 0 ) THEN
             ISTR = PROMPT
!
! ---------- ���� ����� ������
!
             IF ( LEN(PROMPT) .GT. 0 ) THEN
                  CALL PRCH ( CHAR(10)//CHAR(13)//ISTR ) ! ����� �� �����
                  LGT=LEN(PROMPT)-ILEN(PROMPT)
                  IF ( LGT .GT. 0 ) THEN
                       CALL CLRCH ( OSTR )
                       IF ( LGT .GT. 131 ) LGT=131
                       OSTR(LGT+1:)=CHAR(1)
                       CALL PRCH ( OSTR )
                  END IF
             END IF
        END IF
        IF ( LOC(IBG) .NE. 0 ) THEN
             IBG_R=IBG
             IF ( IBG_R .GT.132 ) IBG_R = 132
             IF ( IBG_R .LT.0   ) IBG_R = 0
          ELSE
             IBG_R=0
        END IF
!
! ----- ����������� ������ BEGIN � BEG �� ���������� ������������������
!
        IF ( IBG_R.GT.0 ) THEN
             IF ( LOC(BEGIN) .NE. 0 ) THEN
                  BEG=BEGIN
               ELSE
                  CALL CLRCH ( BEG )
             END IF
             CALL PRCH ( BEG(1:IBG_R) ) ! ����� �� �����
             IBB = IBG_R
          ELSE
             IBB = 1
        END IF
!
! ----- ����� ����� ������ STR
!
        IP=LEN ( STR )
        IF ( IP.GT.132 ) IP=132
!
! ----- ���������� ���� ������ � ���������
!
        IZ=INTX  ( IBG_R, BEG(1:IBB), OSTR(1:IP) )
!#        IF ( IZ.EQ.18888 ) THEN
!
! ---------- ����������  SYS$INPUT  --  �� ��������. ������ � �����
!
!#             READ ( 5, FMT='(A)', IOSTAT=I1, ERR=710 ) OSTR
!#          ELSE IF ( IZ.NE.1 ) THEN
!#             IF ( IZ.EQ.0 ) IZ=1111
!#             CALL ERR_LOG ( IZ, IUER, 'INSTR', '������ ������ � '//
!#     $                      '���������' )
!#             RETURN
!#        END IF
!
! ----- ����������� ������ OSTR � STR �� ���������� ������������������
!
        STR=OSTR
        CALL ERR_LOG ( 0, IUER )
        RETURN
!
  710   CONTINUE
        CALL ERR_LOG ( I1, IUER, 'INSTR', 'Error in reading' )
        RETURN
        END  !#!  INSTR  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION INTX ( IB, BEG, ZAG )
! ************************************************************************
! *                                                                      *
! *     ������������  INTX  ��������� � ��������� ������ ZAG � ������    *
! *     ��������� ���������. ���� ����������� ���� ��� ������� �������   *
! *     <��>, ���� �� ���������� ���� ������. ���� ���� �������������� � *
! *     ��������� Microterm 5530 ( ���������� ������ TERM_TYPE="43",     *
! *     "44" , "33" ��� "34" ), �� ������� F20 ����������� �������/LATIN *
! *     ��������.                                                        *
! *     ������� <CTRL/U> �� ����� ����� ������� ������. �������� ������� *
! *     <DEL>.                                                           *
! *                                                                      *
! * ________________________ ������� ���������: ________________________ *
! *                                                                      *
! *      IB ( INTEGER*4 ) -- ����� ������-�������.                       *
! *     BEG ( CHARACTER ) -- ������-������.                              *
! *                                                                      *
! * ________________________ �������� ���������: _______________________ *
! *                                                                      *
! *     ZAG ( CHARACTER ) -- �������� ������.                            *
! *                                                                      *
! *  ###  18-APR-90      INTX     V3.0  (c) ������ �.�.  20-MAR-95  ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER ZAG*(*), BEG*(*)
        CHARACTER CC*1
!
!#        NA=NUM$ARG ( )
!#        IF ( NA.NE.3 ) CALL VER$ARG ( 3 )
!
        LIM=LEN(ZAG)
        CALL CLRCH ( ZAG )
        IF ( IB.GE.LIM ) THEN
             ZAG=BEG
             RETURN
        END IF
        IF ( IB.NE.0 ) ZAG(1:IB)=BEG(1:IB)
!
! ----- K  --  ���������� ��� ��������� ��������
!
        K=IB
        DO 410 J1=1,1024
  910      CONTINUE
           INTX=INSIM ( CC, ICC )  !  �������� ����� � ��������� ������A CC
!#           IF ( INTX.NE.1 ) RETURN
           IF ( ICC.EQ.10 .OR. ICC.EQ.13 ) GOTO 810   !  <��>
           IF ( ICC.EQ.21 ) THEN       !  CTRL/U
                IF ( K.EQ.0 ) GOTO 410
                CALL CURL   ( K )      !  ������ ����� �� K �������
                CALL CLRCH  ( ZAG )   !  ������� ������
                ZAG(K:K) =CHAR(1)
                CALL PRCH   ( ZAG )
                CALL CLRCH  ( ZAG )   !  ������� ������
                CALL ITTOUT ( ' ' )
                CALL CURL ( K )      !  ������ ����� �� K �������
                K=0
                GOTO 410
           END IF
           IF ( ICC.EQ.127 .OR. ICC.EQ.8 ) THEN
!
! ------------ ������� ������������ �� ����
! ------------ ������� 127  <DEL> ��� <BS> ("�����")
!
               IF ( K.EQ.0 ) GOTO 910
               CALL CURL(1)      ! ������ �����
               ZAG(K:K) = ' '
               CALL ITTOUT ( ZAG(K:K) )  ! �������� ���������� ����� �������
               CALL CURL(1)      ! ������ �����
               K=K-1             ! ��������� ���������� ��������� ��������
               GOTO 410
           END IF
           IF ( ICC.LT.32 .OR. ICC.GE.256 ) THEN
!
! -------------- ����� ������������ ������
!
                 CALL BELL(1)
                 GOTO 910
           END IF
!
! -------- ��������� ZAG � ������, ���� ��� ������ ������ ��������
!
           K=K+1
           ZAG(K:K)=CC
           IF ( CC.EQ.' ' ) CALL ITTOUT ( ' ' )  !  ����� �������
           CALL PRCH ( CC )      !  ����� �������
           IF ( K.EQ.LIM ) GOTO 810
  410   CONTINUE
  810   CONTINUE
        RETURN
        END  !#!  INTX  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LIB$WAIT ( RTIME )
      REAL*8     RTIME
      INTEGER*4  ITIME, IS
      ADDRESS__TYPE  ARR1(2), ARR2(2)
#if defined LINUX || defined DARWIN
      ARR1(1) = RTIME
      ARR1(2) = (RTIME - ARR1(1))*1.E9
      IS = NANOSLEEP ( ARR1, ARR2 )
#else
      ITIME = RTIME
      IF ( RTIME .LT. 1.0D0 ) ITIME = 1
      CALL FUNC_SLEEP ( ITIME )
#endif
      RETURN
      END  !#!  LIB$WAIT  #!#
