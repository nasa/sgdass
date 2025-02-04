      PROGRAM    MD_TEST
! ************************************************************************
! *                                                                      *
! *   MD_TEXT is a test program for Multi_DiaGI.                         *
! *                                                                      *
! *  ###  20-JUL-1999   MD_TEST   v1.3  (c)  L. Petrov  06-AUG-2016 ###  *
! *                                                                      *
! ************************************************************************
      INCLUDE    'diagi.i'
      PARAMETER  ( MAXP = 128, MAX_PAR = 256 )
      TYPE ( DIAGI_STRU ) ::  DIAGI_S(MAXP)
      REAL*8     X(MAX_PAR,MAXP), Y(MAX_PAR,MAXP), E(MAX_PAR,MAXP)
      REAL*8     SC
      PARAMETER  ( SC = 1200 )
      CHARACTER  TITLE*80, TITS(MAXP)*32, BUT(MAXP)*80, STR*80, BUT_LET(MAXP)*1
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV_XS, ICL1, ICL2, ICL3
      INTEGER*4  IBATCH
      PARAMETER  ( IBATCH = 0 )
!
      MPL = 14
      MPB = 8
      NC  = 4
      NR  = 4
!
!      MPL = 3
!      MPB = 8
!      NC  = 2
!      NR  = 2
!
      BUT(1)  = 'Action 1'
      BUT(2)  = 'Action 2'
      BUT(3)  = 'Action 3 | and something | more'
      BUT(4)  = 'Action 4'
      BUT(5)  = 'Action 5 | and \(0903) and \(0254)'
      BUT(6)  = 'Action 6'
      BUT(7)  = 'Atmosphere'
      BUT(8)  = 'Clock offset'
      BUT(9)  = 'EOP'
      BUT(10) = 'Clock | offset'
      BUT(11) = 'Atmosphere | path delay'
      BUT(12) = 'EXIT'
!
      BUT_LET(1)  = '1'
      BUT_LET(2)  = '2'
      BUT_LET(3)  = '3'
      BUT_LET(4)  = '4'
      BUT_LET(5)  = '5'
      BUT_LET(6)  = '6'
      BUT_LET(7)  = '7'
      BUT_LET(8)  = '8'
      BUT_LET(9)  = '9'
      BUT_LET(10) = 'A'
      BUT_LET(11) = 'B'
      BUT_LET(12) = 'C'
!
      IER = -1
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV_XS, STR, STR, &
     &                 ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           STOP 'MD_DEMO: error in intialization'
      END IF
!
      DO 410 J1=1,MPL
         DO 420 J2=1,MAX_PAR
            X(J2,J1) = (J1 + DFLOAT(J2-1)/DFLOAT(MAX_PAR-1))*SC
            Y(J2,J1) = SC*(SIN(X(J2,J1)/SC) + 0.2*COS(5.0*X(J2,J1)/SC))
            E(J2,J1) = DABS(Y(J2,J1) * 0.2)
 420     CONTINUE
!
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
         CALL CHASHL ( STR )
!
         TITS(J1) = 'Plot '//STR
         STR      = 'It is a long-long title for the plot '//STR
         IF ( J1 .EQ. 1 ) THEN
              TITS(J1) = '#1 8352.99MHz'
         END IF
!
         IF ( IBATCH .EQ. 0 ) THEN
              CONTINUE 
              DIAGI_S(J1)%IDEV = IDEV_XS
            ELSE
              DIAGI_S(J1)%IDEV = 7
         END IF
         DIAGI_S(J1)%NPOI(1)   = MAX_PAR
         DIAGI_S(J1)%ADR_X8(1) = LOC(X(1,J1))
         DIAGI_S(J1)%ADR_Y8(1) = LOC(Y(1,J1))
         DIAGI_S(J1)%ADR_E8(1) = LOC(E(1,J1))
         DIAGI_S(J1)%LER(1)    = .TRUE.
         DIAGI_S(J1)%ICOL(1)   = 1
         DIAGI_S(J1)%IBST(1)   = 4
         DIAGI_S(J1)%ILST(1)   = 1
         DIAGI_S(J1)%IOST(1)   = 1
         DIAGI_S(J1)%IPST(1)   = 4
         DIAGI_S(J1)%IWST(1)   = 1
!
         IF ( J1 .EQ. 1 ) THEN
              DIAGI_S(J1)%IPST(1) = 5
              DIAGI_S(J1)%IBST(1) = 6
         END IF
!
         DIAGI_S(J1)%NCLR      = 1
         DIAGI_S(J1)%ICLR      = 1
         DIAGI_S(J1)%ZAG       = STR
         DIAGI_S(J1)%NAME      = '/tmp/md.gif'
         DIAGI_S(J1)%ARG_UNITS = 'Arg'
         DIAGI_S(J1)%ITRM      = 0
         IF ( IBATCH .EQ. 0 ) THEN
              DIAGI_S(J1)%IBATCH = 0
            ELSE IF ( IBATCH .EQ. 1 ) THEN
              DIAGI_S(J1)%IBATCH = 1
         END IF
         DIAGI_S(J1)%XMIN      = 1.0
         DIAGI_S(J1)%XMAX      = 0.0
         DIAGI_S(J1)%YMIN      = 1.0
         DIAGI_S(J1)%YMAX      = 0.0
!
!         DIAGI_S(J1)%MD_IN     = 4
!         DIAGI_S(J1)%MD_OUT    = DIAGI__QUIT
!
         DIAGI_S(J1)%STATUS    = DIA__DEF
 410  CONTINUE
!
      CALL CLRCH ( TITLE )
      TITLE = 'Test of Multi_DiaGI'
      IUER  = -1
      CALL MULTI_DIAGI ( TITLE, MPL, NC, NR, TITS, MPB, BUT, BUT_LET, &
     &                  '/tmp/mumu_', DIAGI_S, ICODE, IUER )
!
      WRITE ( 6, * ) ' icode = ',icode,' iuer =',iuer  ! %%%%
      END  !#!   MD_TEST  #!#
