        FUNCTION IPG(C)
! ************************************************************************
! *                                                                      *
! *     ������������  IPG  ������� � ������� ������� �������             *
! *     ����������������� ������. ��� ������ �� 7238 ( IT=1 ��� IT=2 )   *
! *     ���� ��� C ������������� ������� ��������� �����, �� ���������   *
! *     ����������������� ������ �� ������ 0, ���� �� ���������          *
! *     ��������� �����, �� �� ����� 8. ��� ������ 5530 Microterm  �     *
! *     ����� ������ ��������� ������ �� ������ 0. ����  ���  C  ������  *
! *     32 , �� ������ �� ��������, � IPG  ���������� �������� 1 .       *
! *                                                                      *
! *   ###  ������������  IPG   �������  ������ �.�.  01-JUN-92    ###    *
! *                                                                      *
! ************************************************************************
        CHARACTER C*1
        INTEGER*1 B
        CHARACTER T1*3, T2*3, T3*3, STR*7
        CALL SHOW_IO ( IT, IG, IP, IR, %VAL(0) )
        CALL CLRCH( STR )
        IF ( IT.GE.1 .AND. IT.LE.5  ) THEN
             T1=CHAR(27)//'(0'
             T2=CHAR(27)//'(8'
             T3=CHAR(27)//'(B'
             IPG=0   !  ������� ����������� ����������
             B=ICHAR(C)
             IF ( B .LT. 32 ) IPG=1
             IF ( B.LE.96 .AND. B.GE.32 )  STR=T1//CHAR(B+32)//T3
             IF ( B.GT.96 .AND. IT.GE.1 .AND. IT.LE.2 ) STR=T2//C//T3
             IF ( B.GT.96 .AND. IT.GE.3 .AND. IT.LE.5 ) STR=T1//C//T3
             CALL PRCH( STR )
        END IF
        RETURN
        END  !#!  IPG  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION ICHER(N)
! ************************************************************************
! *                                                                      *
! *     ������������  ICHER  ������� �� ����� ��������� ��7238 � ������� *
! *     ������� ������� ������-����������� ��������. �������� ������ �   *
! *     ����������� ��  ��������  N  ���� �� 10-� �������.               *
! *                                                                      *
! *         ����������:                                                  *
! *         ~~~~~~~~~~~                                                  *
! *         ����   N=4  �������� ������ 3-�� �������;                    *
! *         ����   N=7  �������� ������ 8-�� �������.                    *
! *                                                                      *
! *         ICHER �������� �������� 0 � ������ �������� ������,          *
! *         1, ���� N>10 � 2, ���� N=0 ��� N<0.                          *
! *                                                                      *
! *         ����  ICHER �� ����� 0, �� ������-���������� �� ����� ��     *
! *         ���������.                                                   *
! *                                                                      *
! *   ###  ������������  ICHER  �������  ������ �.�.  01-JUN-92   ###    *
! *                                                                      *
! ************************************************************************
        CHARACTER T1*4, T2*5, T3*3, T4*3, T5*3, STR*10
        ICHER=0
        T1=CHAR(27)//'[4m'
        T2=CHAR(27)//'[24m'
        T3=CHAR(27)//'(8'
        T4=CHAR(27)//'(B'
        T5=CHAR(27)//'(0'
!
        IF(N.GT.10) ICHER=1
        IF(N.LE.0 ) ICHER=2
        CALL SHOW_IO ( IT, IG, IP, IR, %VAL(0) )
        CALL CLRCH( STR )
        IF ( IT.GE.1 .AND. IT.LE.5  ) THEN
             ICHER=0
             IF(N.EQ.1 ) STR=T1//' '//T2
             IF(N.EQ.2 ) STR=T5//'s'//T4
             IF(N.EQ.3 ) STR=T5//'r'//T4
             IF(N.EQ.4 ) STR=T5//'r'//T4
             IF(N.EQ.5 ) STR=T5//'q'//T4
             IF(N.EQ.6 ) STR='-'
             IF(N.EQ.7 ) STR=T5//'p'//T4
             IF(N.EQ.8 ) STR=T5//'p'//T4
             IF ( N.EQ.9 .AND. ( IT.EQ.1 .OR.  IT.EQ.2 ) ) STR='~'
             IF ( N.EQ.9 .AND. ( IT.GE.3 .AND. IT.LE.5 ) ) STR= &
     &                                             T5//'o'//T4
             IF(N.EQ.10) STR=T5//'o'//T4
!
             CALL PRCH( STR )
!
        END IF
        RETURN
        END  !#!  ICHER  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE LINHOR ( C_I, NCOLB_I, NCOLE_I )
! ************************************************************************
! *                                                                      *
! *     ������������ LINHOR  ������ ����������������� ������  � �����  C *
! *     �� �������  [NSTRB,NCOLB]  ��  �������  [NSTRB,NCOLE]            *
! *                                                                      *
! ************************************************************************
        CHARACTER C_I*(*), C*1
        CHARACTER TB*3, TE*3, STR*132
!!        LOGICAL   LR$STR
!
!!        IF ( .NOT. LR$STR ( C_I ) ) RETURN
        IST = 1
        C(1:1)=C_I(1:1)
        CALL SHOW_IO ( IT, IG, IP, IR, %VAL(0) )
        IF ( IST.EQ.1 ) THEN
             NCOLB=1
             NCOLE=80
          ELSE
             NCOLB=1
             NCOLE=132
        END IF
!        IF ( NUM$ARG().GE.3 ) THEN
!             NCOLB=NCOLB_I
!             NCOLE=NCOLE_I
!             IF ( NCOLE.GT.132 ) NCOLE=132
!        END IF
!
        CALL CLRCH ( STR )
        IF (  IT.GE.1 .AND. IT.LE.5 ) THEN
             IF ( ICHAR(C).LT.96 .OR. ( IT.GE.3 .AND. IT.LE.5 ) ) THEN
                  C=CHAR ( ICHAR(C) + 32 )
                  TB=CHAR(27)//'(0'
               ELSE
                  TB=CHAR(27)//'(8'
             END IF
             TE=CHAR(27)//'(B'
             LL=NCOLE-NCOLB+1
             IF(LL.LT.1) RETURN
             CALL CURL(132)
             CALL CURR(NCOLB-1)
             IF( NCOLE.GT.132 ) LL=132-NCOLB+1
             DO 410 J1=1,LL
                 STR(J1:J1)=C
  410        CONTINUE
!
             CALL PRCH ( TB )
             CALL PRCHDL ( STR(1:LL) )
             CALL PRCH ( TE )
          ELSE IF ( IT .EQ. 6  .OR.  IT .EQ. 7 ) THEN
             CALL CLRCH ( STR )
             CALL REPEAT ( '-', NCOLE_I-NCOLB_I+1, STR )
             CALL CURL ( 132 )
             CALL CURR ( NCOLB_I-1 )
             CALL PRCH ( STR )
        END IF
        RETURN
        END  !#!  LINHOR  #!#
