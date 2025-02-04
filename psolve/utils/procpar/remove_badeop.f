      PROGRAM    REMOVE_BADEOP
! ************************************************************************
! *                                                                      *
! *   Program REMOVE_BADEOP removes bad EOP and nutation from the        *
! *   EOP, EOB and NUT files.                                            *
! *                                                                      *
! *  ### 24-AUG-2000  REMOVE_BADEOP v2.2 (c) L. Petrov  14-JUL-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 8192 )
      CHARACTER  FILSOL*128, FILEOB*128, FILEOP*128, FILNUT*128
      CHARACTER  FILEOB_GOOD*128, FILEOB_BAD*128, &
     &           FILBEOP*128, FILEOP_GOOD*128, FILEOP_BAD*128, &
     &           FILBNUT*128, FILNUT_GOOD*128, FILNUT_BAD*128, STR*80, &
     &           C_BADEOP(MBUF)*10, C_BADNUT(MBUF)*10
      CHARACTER  BUF(MBUF)*512, SES_EXCEOP(MBUF)*10, SES_EXCNUT(MBUF)*10, &
     &           BUF_EXCEOP(MBUF)*512, BUF_EXCNUT(MBUF)*512
      LOGICAL*4  FL_HEADER
      INTEGER*4  NBUF, NUMARG, L_EXCEOP, L_EXCNUT, L_BADEOP, L_BADNUT, IP, &
     &           IBAD_EOP, IBAD_NUT, J1, J2, J3, J4, J5, IDAT, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      NUMARG = IARGC ()
!
      CALL CLRCH ( FILSOL )
      CALL CLRCH ( FILBEOP )
      CALL CLRCH ( FILBNUT )
!
      IF ( NUMARG .GE. 3 ) THEN
           CALL GETARG ( 1, FILSOL  )
           CALL GETARG ( 2, FILBEOP )
           CALL GETARG ( 3, FILBNUT )
         ELSE
           WRITE ( 6, '(A)' ) 'Usage: remove_badeop <solution_name> '// &
     &                        '<bad_eop_file> <bad_nutation_file>'
           CALL EXIT ( 1 )
      END IF
!
      FILEOB      = FILSOL(1:I_LEN(FILSOL))//'.eob'
      FILEOB_GOOD = FILSOL(1:I_LEN(FILSOL))//'.good.eob'
      FILEOP      = FILSOL(1:I_LEN(FILSOL))//'.eop'
      FILEOP_GOOD = FILSOL(1:I_LEN(FILSOL))//'.good.eop'
      FILNUT      = FILSOL(1:I_LEN(FILSOL))//'.nut'
      FILNUT_GOOD = FILSOL(1:I_LEN(FILSOL))//'.good.nut'
      FILEOB_BAD  = FILSOL(1:I_LEN(FILSOL))//'.bad.eob'
      FILEOP_BAD  = FILSOL(1:I_LEN(FILSOL))//'.bad.eop'
      FILNUT_BAD  = FILSOL(1:I_LEN(FILSOL))//'.bad.nut'
!
      L_BADEOP = 0
      L_BADNUT = 0
      WRITE ( 6, * ) 'Removing bad EOP ...'
!
! --- Create bad EOP list
!
      IUER = -1
      CALL RD_TEXT ( FILBEOP, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      DO 410 J1=1,NBUF
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '*' ) GOTO 410
         CALL CHASHL ( BUF(J1) )
         CALL CHIN ( BUF(J1)(1:8), IDAT )
         IF ( BUF(J1)(1:1) .NE. '$' ) THEN
              IF ( IDAT > 19700000 .AND. IDAT < 20700000 ) THEN
                   CONTINUE 
                 ELSE 
                   BUF(J1) = '$'//BUF(J1)
              END IF
         END IF
         L_BADEOP = L_BADEOP + 1
         C_BADEOP(L_BADEOP) = BUF(J1)
 410  CONTINUE
!
! --- Create bad NUT list
!
      IUER = -1
      CALL RD_TEXT ( FILBNUT, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      DO 420 J2=1,NBUF
         IF ( ILEN(BUF(J2)) .EQ.  0  ) GOTO 420
         IF ( BUF(J2)(1:1)  .EQ. '#' ) GOTO 420
         IF ( BUF(J2)(1:1)  .EQ. '*' ) GOTO 420
         CALL CHASHL ( BUF(J2) )
         CALL CHIN ( BUF(J2)(1:8), IDAT )
         IF ( BUF(J2)(1:1) .NE. '$' ) THEN
              IF ( IDAT > 19700000 .AND. IDAT < 20700000 ) THEN
                   CONTINUE 
                 ELSE 
                   BUF(J2) = '$'//BUF(J2)
              END IF
         END IF
         L_BADNUT = L_BADNUT + 1
         C_BADNUT(L_BADNUT) = BUF(J2)
 420  CONTINUE
!
      IUER = -1
      CALL RD_TEXT ( FILEOB, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 2 )
      OPEN ( UNIT=11, STATUS='UNKNOWN', FILE=FILEOB_GOOD )
!
      L_EXCNUT = 0
      L_EXCEOP = 0
      FL_HEADER = .TRUE.
      DO 430 J3=1,NBUF
         IF ( ILEN(BUF(J3))  .EQ.  0  ) GOTO 830
         IF ( BUF(J3)(1:1)   .EQ. '#' ) GOTO 830
!
! ------ Look for the bad list
!
         IBAD_EOP = LTM_DIF ( 1, L_BADEOP, C_BADEOP, BUF(J3)(16:25) )
         IBAD_NUT = LTM_DIF ( 1, L_BADNUT, C_BADNUT, BUF(J3)(16:25) )
!
         IF ( IBAD_EOP .GT. 0 ) THEN
              L_EXCEOP = L_EXCEOP + 1
              SES_EXCEOP(L_EXCEOP) = BUF(J3)(16:25)
              BUF_EXCEOP(L_EXCEOP) = BUF(J3)
!
! ----------- Remove pole coordinates, UT1 and their rates
!
              BUF(J3)(34:41)   = '-0            '
              BUF(J3)(43:50)   = '-0            '
              BUF(J3)(52:62)   = '-0            '
              BUF(J3)(82:90)   = '-0            '
              BUF(J3)(92:100)  = '-0            '
              BUF(J3)(102:108) = '-0            '
!
! ----------- Remove formal uncerainties of pole coordinates, UT1 and their rates
!
              BUF(J3)(110:117) = '-0            '
              BUF(J3)(119:126) = '-0            '
              BUF(J3)(128:136) = '-0            '
              BUF(J3)(154:162) = '-0            '
              BUF(J3)(164:172) = '-0            '
              BUF(J3)(174:180) = '-0            '
         END IF
!
         IF ( IBAD_NUT .GT. 0 ) THEN
!
! ----------- Remove nutation angles
!
              BUF(J3)(64:71)   = '-0            '
              BUF(J3)(73:80)   = '-0            '
!
! ----------- ... and their formal uncertainties
!
              BUF(J3)(138:144) = '-0            '
              BUF(J3)(146:152) = '-0            '
         END IF
!
         IF ( BUF(J3)(110:112) .EQ. '-0 ' .AND. BUF(J3)(138:140) .EQ. '-0 ' ) &
     &   THEN
!
! ----------- This session did not have neigher good EOP nor nutation
!
              GOTO 430
         END IF
 830     CONTINUE
!
         IF ( FL_HEADER  .AND.  BUF(J3)(1:1) .NE. '#' ) THEN
              CALL CLRCH ( STR )
              STR = '# Processed by remove_badeop at '//GET_CDATE()
              WRITE ( 11, FMT='(A)' ) STR(1:I_LEN(STR))
              STR = '# '
              WRITE ( 11, FMT='(A)' ) STR(1:I_LEN(STR))
              FL_HEADER = .FALSE.
         END IF
!
         WRITE ( 11, '(A)' ) BUF(J3)(1:I_LEN(BUF(J3)))
 430  CONTINUE
      CLOSE ( UNIT=11 )
!
      WRITE ( 6, * ) L_BADEOP,' bad sessions for EOP determination'
      WRITE ( 6, * ) L_EXCEOP,' sessions for EOP determination were excluded'
      WRITE ( 6, * ) 'Output file: '//FILEOB_GOOD(1:I_LEN(FILEOB_GOOD))
!
      IF ( L_EXCEOP .GT. 0 ) THEN
!
           CALL RD_TEXT ( FILEOP, MBUF, BUF, NBUF, -3 )
           OPEN ( UNIT=33, FILE=FILEOP_GOOD, STATUS='UNKNOWN' )
           OPEN ( UNIT=44, FILE=FILEOP_BAD,  STATUS='UNKNOWN' )
           DO 440 J4=1,NBUF
              IP = LTM_DIF ( 1, L_EXCEOP, SES_EXCEOP, BUF(J4)(11:20) )
              IF ( IP .LE. 0 ) THEN
                   WRITE ( 33, FMT='(A)' ) BUF(J4)(1:I_LEN(BUF(J4)))
                 ELSE
                   WRITE ( 44, FMT='(A)' ) BUF(J4)(1:I_LEN(BUF(J4)))
              END IF
 440       CONTINUE
           CLOSE ( UNIT=33 )
           CLOSE ( UNIT=44 )
           WRITE ( 6, * ) 'Output file: '//FILEOP_GOOD(1:I_LEN(FILEOP_GOOD))
           WRITE ( 6, * ) 'Output file: '//FILEOP_BAD(1:I_LEN(FILEOP_BAD))
      END IF
!
      CALL RD_TEXT ( FILNUT, MBUF, BUF, NBUF, -3 )
      OPEN ( UNIT=33, FILE=FILNUT_GOOD, STATUS='UNKNOWN' )
      OPEN ( UNIT=44, FILE=FILNUT_BAD,  STATUS='UNKNOWN' )
!
! --- NB: the number of sessions in EOB and NUT fiels maybe different
!
      L_EXCNUT = 0
      DO 450 J5=1,NBUF
         IBAD_NUT = LTM_DIF ( 1, L_BADNUT, C_BADNUT, BUF(J5)(11:20) )
         IF ( IBAD_NUT .LE. 0 ) THEN
              WRITE ( 33, FMT='(A)' ) BUF(J5)(1:I_LEN(BUF(J5)))
            ELSE
              L_EXCNUT = L_EXCNUT + 1
              SES_EXCNUT(L_EXCNUT) = BUF(J5)(11:20)
              BUF_EXCNUT(L_EXCNUT) = BUF(J5)
              WRITE ( 44, FMT='(A)' ) BUF(J5)(1:I_LEN(BUF(J5)))
         END IF
 450  CONTINUE
!
      CLOSE ( UNIT=33 )
      CLOSE ( UNIT=44 )
!
      WRITE ( 6, * ) L_BADNUT,' bad sessions for nutation determination'
      WRITE ( 6, * ) L_EXCNUT,' sessions for nutation determination were excluded'
      WRITE ( 6, * ) 'Output file: '//FILNUT_GOOD(1:I_LEN(FILNUT_GOOD))
      WRITE ( 6, * ) 'Output file: '//FILNUT_BAD(1:I_LEN(FILNUT_BAD))
      WRITE ( 6, * ) 'Bad EOP are removed'
!
      END  !#!  REMOVE_BADEOP  #!#
