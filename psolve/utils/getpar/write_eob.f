      SUBROUTINE WRITE_EOB ( FILEOB, N_HEAD, HEAD_BUF, NSES, FLAG, MJD_EOP, &
     &  MJD_NUT, DBNAME, &
     &  XPL_VAL, YPL_VAL, U1_VAL, DPSI_VAL, DEPS_VAL, XPR_VAL, YPR_VAL, UTR_VAL, &
     &  XPL_ERR, YPL_ERR, U1_ERR, DPSI_ERR, DEPS_ERR, XPR_ERR, YPR_ERR, UTR_ERR, &
     &  C_XY, C_XU, C_YU, C_PE, C_URX, C_URY, C_URU, DURA, WRMS, NOBS, SCODE, &
     &  C_NET, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine for writing the Earth Orientation Parameters in the file   *
! *   in EOB format.
! *                                                                      *
! *  ### 03-JUN-2002   WRITE_EOB   v1.0 (c)  L. Petrov  03-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INTEGER*4  NSES, N_HEAD, IUER
      CHARACTER  FILEOB*(*), HEAD_BUF(*)*(*), FLAG(NSES)*(*), DBNAME(NSES)*(*), &
     &           SCODE(NSES)*(*), C_NET(NSES)*(*)
      INTEGER*4  NOBS(NSES)
      REAL*8     MJD_EOP(NSES), MJD_NUT(NSES), XPL_VAL(NSES), YPL_VAL(NSES), &
     &           U1_VAL(NSES), &
     &           DPSI_VAL(NSES), DEPS_VAL(NSES), XPR_VAL(NSES), YPR_VAL(NSES), &
     &           UTR_VAL(NSES),  XPL_ERR(NSES), YPL_ERR(NSES), U1_ERR(NSES), &
     &           DPSI_ERR(NSES), DEPS_ERR(NSES), XPR_ERR(NSES), YPR_ERR(NSES), &
     &           UTR_ERR(NSES), C_XY(NSES), C_XU(NSES), C_YU(NSES), C_PE(NSES), &
     &           C_URX(NSES), C_URY(NSES), C_URU(NSES), DURA(NSES), WRMS(NSES)
      CHARACTER  STR*80
      INTEGER*4  LUN, IOS, J1, J2, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=FILEOB, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5011, IUER, 'WRITE_EOB', 'Error '//STR(1:I_LEN(STR))// &
     &         'in an attempt to open output file '//FILEOB )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) SIG_EOB
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 5012, IUER, 'WRITE_EOB', 'Error '//STR(1:I_LEN(STR))// &
     &         'in an attempt to write the first record to the output file '// &
     &          FILEOB )
           RETURN
      END IF
!
      IF ( N_HEAD .GT. 0 ) THEN
           DO 410 J1=1,N_HEAD
              WRITE ( LUN, '(A)' ) HEAD_BUF(J1)(1:I_LEN(HEAD_BUF(J1)))
 410       CONTINUE
      END IF
!
      DO 420 J2=1,NSES
         CALL ERR_PASS ( IUER, IER )
         CALL WRITE_EOB_REC ( LUN, FLAG(J2), MJD_EOP(J2), MJD_NUT(J2), &
     &        DBNAME(J2), XPL_VAL(J2), YPL_VAL(J2), U1_VAL(J2), DPSI_VAL(J2), &
     &        DEPS_VAL(J2), XPR_VAL(J2), YPR_VAL(J2), UTR_VAL(J2), &
     &        XPL_ERR(J2), YPL_ERR(J2), U1_ERR(J2), DPSI_ERR(J2), DEPS_ERR(J2), &
     &        XPR_ERR(J2), YPR_ERR(J2), UTR_ERR(J2), C_XY(J2), C_XU(J2), &
     &        C_YU(J2), C_PE(J2), C_URX(J2), C_URY(J2), C_URU(J2), &
     &        DURA(J2), WRMS(J2), NOBS(J2), SCODE(J2), C_NET(J2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5013, IUER, 'WRITE_EOB', 'Error in an attempt '// &
     &            'to write in the file '//FILEOB )
              RETURN
         END IF
 420  CONTINUE
!
      CLOSE ( UNIT=LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_EOB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_EOB_FILE ( FILEOB, NHEAD, HEAD_BUF, NSES, EOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRITE_EOB_FILE
! *                                                                      *
! * ### 18-JUN-2002  WRITE_EOB_FILE  v1.0 (c) L. Petrov 18-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INTEGER*4  NHEAD, NSES, IUER
      CHARACTER  FILEOB*(*), HEAD_BUF(NHEAD)*(*)
      TYPE ( EOP__STRU ) ::  EOP(NSES)
!
      CHARACTER  STR*128
      INTEGER*4  LUN, IOS, J1, J2, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=FILEOB, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5061, IUER, 'WRITE_EOB_FILE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open output file '// &
     &          FILEOB )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) SIG_EOB
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 5062, IUER, 'WRITE_EOB_FILE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the first '// &
     &         'record in the output file '//FILEOB )
           RETURN
      END IF
!
      IF ( NHEAD .GT. 0 ) THEN
           DO 410 J1=1,NHEAD
              WRITE ( LUN, '(A)' ) HEAD_BUF(J1)(1:I_LEN(HEAD_BUF(J1)))
 410       CONTINUE
      END IF
!
      DO 420 J2=1,NSES
         CALL ERR_PASS ( IUER, IER )
         CALL WRITE_EOB_REC ( LUN, EOP(J2)%FLAG, EOP(J2)%MJD_EOP, &
     &        EOP(J2)%MJD_NUT, EOP(J2)%DBNAME, EOP(J2)%XPL_V, &
     &        EOP(J2)%YPL_V, EOP(J2)%U1_V, EOP(J2)%DPSI_V, &
     &        EOP(J2)%DEPS_V, EOP(J2)%XPR_V, EOP(J2)%YPR_V, &
     &        EOP(J2)%UTR_V, EOP(J2)%XPL_E, EOP(J2)%YPL_E, &
     &        EOP(J2)%U1_E, EOP(J2)%DPSI_E, EOP(J2)%DEPS_E, &
     &        EOP(J2)%XPR_E, EOP(J2)%YPR_E, EOP(J2)%UTR_E, &
     &        EOP(J2)%C_XY, EOP(J2)%C_XU, EOP(J2)%C_YU, EOP(J2)%C_PE, &
     &        EOP(J2)%C_URX, EOP(J2)%C_URY, EOP(J2)%C_URU, EOP(J2)%DURA, &
     &        EOP(J2)%WRMS, EOP(J2)%NOBS, EOP(J2)%SCODE, EOP(J2)%C_NET, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5063, IUER, 'WRITE_EOB_FILE', 'Error in '// &
     &            'an attempt to write in the file '//FILEOB )
              RETURN
         END IF
 420  CONTINUE
!
      CLOSE ( UNIT=LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_EOB_FILE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_EOB_REC ( LUN, FLAG, MJD_EOP, MJD_NUT, DBNAME, &
     &  XPL_VAL, YPL_VAL, U1_VAL, DPSI_VAL, DEPS_VAL, XPR_VAL, YPR_VAL, UTR_VAL, &
     &  XPL_ERR, YPL_ERR, U1_ERR, DPSI_ERR, DEPS_ERR, XPR_ERR, YPR_ERR, UTR_ERR, &
     &  C_XY, C_XU, C_YU, C_PE, C_URX, C_URY, C_URU, DURA, WRMS, NOBS, SCODE, &
     &  C_NET, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine
! *                                                                      *
! *  ### 04-JUN-2002 WRITE_EOB_REC v1.2 (c)  L. Petrov  30-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      CHARACTER  FLAG*(*), DBNAME*(*), SCODE*(*), C_NET*(*)
      INTEGER*4  LUN, NOBS, IUER
      REAL*8     MJD_EOP, MJD_NUT, XPL_VAL, YPL_VAL, U1_VAL, DPSI_VAL, &
     &           DEPS_VAL, XPR_VAL, YPR_VAL, UTR_VAL,  XPL_ERR, YPL_ERR, &
     &           U1_ERR, DPSI_ERR, DEPS_ERR, XPR_ERR, YPR_ERR, UTR_ERR, &
     &           C_XY, C_XU, C_YU, C_PE, C_URX, C_URY, C_URU, DURA, WRMS
      TYPE ( EOB__CHAR ) ::  EOB
      CHARACTER  STR*80, STR_EOB*512
      INTEGER*4  IOS, LEN_EOB
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Fill EOB record with blanks
!
      LEN_EOB = SIZEOF(EOB)
      CALL CLRCH ( STR_EOB )
      CALL LIB$MOVC3 ( LEN_EOB, %REF(STR_EOB), EOB )
!
      EOB%FLAG = FLAG
      WRITE ( UNIT=EOB%MJD_EOP, FMT='(F12.6)' ) MJD_EOP
      WRITE ( UNIT=EOB%MJD_NUT, FMT='(F12.6)' ) MJD_NUT
      EOB%DBNAME = DBNAME
      EOB%SCODE  = SCODE
!
      WRITE ( UNIT=EOB%XPL_V, FMT='(F8.6)'  ) XPL_VAL*RAD__TO__MAS/1.D3
      WRITE ( UNIT=EOB%YPL_V, FMT='(F8.6)'  ) YPL_VAL*RAD__TO__MAS/1.D3
      WRITE ( UNIT=EOB%U1_V,  FMT='(F11.7)' ) U1_VAL*RAD__TO__MSEC/1.D3
      WRITE ( UNIT=EOB%DPSI_V, FMT='(F8.3)' ) DPSI_VAL*RAD__TO__MAS
      WRITE ( UNIT=EOB%DEPS_V, FMT='(F8.3)' ) DEPS_VAL*RAD__TO__MAS
      WRITE ( UNIT=EOB%XPR_V, FMT='(F9.6)'  ) XPR_VAL*RAD__TO__MAS* &
     &                                           86400.0D0/1.D3
      WRITE ( UNIT=EOB%YPR_V, FMT='(F9.6)'  ) YPR_VAL*RAD__TO__MAS* &
     &                                           86400.0D0/1.D3
      WRITE ( UNIT=EOB%UTR_V, FMT='(F7.4)'  ) UTR_VAL*RAD__TO__MSEC*86400.0D0
!
      WRITE ( UNIT=EOB%XPL_E, FMT='(F8.6)'  ) XPL_ERR*RAD__TO__MAS/1.D3
      WRITE ( UNIT=EOB%YPL_E, FMT='(F8.6)'  ) YPL_ERR*RAD__TO__MAS/1.D3
      WRITE ( UNIT=EOB%U1_E,  FMT='(F9.7)'  ) U1_ERR*RAD__TO__MSEC/1.D3
      WRITE ( UNIT=EOB%DPSI_E, FMT='(F7.3)' ) DPSI_ERR*RAD__TO__MAS
      WRITE ( UNIT=EOB%DEPS_E, FMT='(F7.3)' ) DEPS_ERR*RAD__TO__MAS
      WRITE ( UNIT=EOB%XPR_E, FMT='(F9.6)'  ) XPR_ERR*RAD__TO__MAS* &
     &                                           86400.0D0/1.D3
      WRITE ( UNIT=EOB%YPR_E, FMT='(F9.6)'  ) YPR_ERR*RAD__TO__MAS* &
     &                                           86400.0D0/1.D3
      WRITE ( UNIT=EOB%UTR_E, FMT='(F7.4)'  ) UTR_ERR*RAD__TO__MSEC*86400.0D0
!
      IF ( XPL_ERR .LT. GTP__EPS ) THEN
           EOB%XPL_V = '-0      '
           EOB%XPL_E = '-0      '
      END IF
!
      IF ( YPL_ERR.LT. GTP__EPS ) THEN
           EOB%YPL_V = '-0      '
           EOB%YPL_E = '-0      '
      END IF
!
      IF ( U1_ERR .LT. GTP__EPS ) THEN
           EOB%U1_V = '-0         '
           EOB%U1_E = '-0        '
      END IF
!
      IF ( DPSI_ERR .LT. GTP__EPS ) THEN
           EOB%DPSI_V = '-0      '
           EOB%DPSI_E = '-0     '
      END IF
!
      IF ( DEPS_ERR .LT. GTP__EPS ) THEN
           EOB%DEPS_V = '-0      '
           EOB%DEPS_E = '-0     '
      END IF
!
      IF ( XPR_ERR  .LT. GTP__EPS ) THEN
           EOB%XPR_V = '-0       '
           EOB%XPR_E = '-0       '
      END IF
!
      IF ( YPR_ERR  .LT. GTP__EPS ) THEN
           EOB%YPR_V = '-0       '
           EOB%YPR_E = '-0       '
      END IF
!
      IF ( UTR_ERR .LT. GTP__EPS ) THEN
           EOB%UTR_V = '-0      '
           EOB%UTR_E = '-0      '
      END IF
!
      WRITE ( UNIT=EOB%C_XY,  FMT='(F6.4)'  ) C_XY
      WRITE ( UNIT=EOB%C_XU,  FMT='(F6.4)'  ) C_XU
      WRITE ( UNIT=EOB%C_YU,  FMT='(F6.4)'  ) C_YU
      WRITE ( UNIT=EOB%C_PE,  FMT='(F6.4)'  ) C_PE
      WRITE ( UNIT=EOB%C_URX, FMT='(F6.4)'  ) C_URX
      WRITE ( UNIT=EOB%C_URY, FMT='(F6.4)'  ) C_URY
      WRITE ( UNIT=EOB%C_URU, FMT='(F6.4)'  ) C_URU
!
      WRITE ( UNIT=EOB%DURA,  FMT='(F5.2)'  ) DURA/3600.D0
      WRITE ( UNIT=EOB%WRMS,  FMT='(F7.2)'  ) WRMS*1.D12
      WRITE ( UNIT=EOB%NOBS,  FMT='(I6)'    ) NOBS
      EOB%C_NET = C_NET
!
      CALL LIB$MOVC3 ( LEN_EOB, EOB, %REF(STR_EOB) )
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR_EOB(1:LEN_EOB)
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5021, IUER, 'WRITE_EOB_REC', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing in EOB file' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_EOB_REC  #!#
