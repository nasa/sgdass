      SUBROUTINE WRITE_EOPS ( FILEOPS, N_HEAD, HEAD_BUF, NSES, EOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRITE_EOPS write the EOP file in the EOPS format.          *
! *                                                                      *
! *   pet  2003.07.21  Fixed the bug: the previous version used wrong    *
! *                    units transformation coefficient for LOD.         *
! *                                                                      *
! *  ### 04-JUN-2002   WRITE_EOPS  v1.6 (c)  L. Petrov  18-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INCLUDE   'ners.i'
      INTEGER*4  NSES, N_HEAD, IUER
      CHARACTER  FILEOPS*(*), HEAD_BUF(N_HEAD)*(*)
      TYPE ( EOP__STRU  ) ::  EOP(NSES)
      TYPE ( NERS__TYPE ) ::  NERS
      TYPE ( EOPS__CHAR ) ::  EOPS
!
      CHARACTER  STR*128, STR_EOPS*512
      REAL*8     UTC_VAL, UTC_M_TAI
      INTEGER*4  LUN, IOS, LEN_EOPS, LEN_EOP, MJD_VAL, J1, J2, IER
!@      LOGICAL*2  KBIT
      INTEGER*4, EXTERNAL ::  GET_UNIT, ILEN, I_LEN
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILEOPS, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5041, IUER, 'WRITE_EOPS', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open input file '// &
     &          FILEOPS )
           RETURN
      END IF
      WRITE ( UNIT=LUN, FMT='(A)' ) SIG_EOP
!
      DO 410 J1=1,N_HEAD
         IF ( HEAD_BUF(J1)(1:1) .EQ. '#' ) THEN
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) &
     &                                     HEAD_BUF(J1)(1:I_LEN(HEAD_BUF(J1)))
            ELSE
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '// &
     &                                     HEAD_BUF(J1)(1:I_LEN(HEAD_BUF(J1)))
         END IF
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 5042, IUER, 'WRITE_EOPS', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing in file '//FILEOPS )
              CLOSE ( UNIT = LUN )
              RETURN
         END IF
 410  CONTINUE
!
      LEN_EOPS = SIZEOF(EOPS)
      DO 420 J2=1,NSES
!
! ------ File the output string with blanks
!
         CALL CLRCH ( STR_EOPS )
         CALL LIB$MOVC3 ( LEN_EOPS, %REF(STR_EOPS), EOPS )
!
         WRITE ( UNIT=EOPS%MJD, FMT='(F12.6)' ) EOP(J2)%MJD_EOP
!
!@         IF ( KBIT ( EOP(J2)%STATUS, XPL__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, XPL__GTP ) ) THEN
              WRITE ( UNIT=EOPS%XPL_V, FMT='(F8.6)' ) EOP(J2)%XPL_V* &
     &                                                RAD__TO__MAS*1.D-3
            ELSE
              EOPS%XPL_V(1:3) = '-0 '
              EOPS%XPL_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, YPL__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, YPL__GTP ) ) THEN
              WRITE ( UNIT=EOPS%YPL_V, FMT='(F8.6)' ) EOP(J2)%YPL_V* &
     &                                                RAD__TO__MAS*1.D-3
            ELSE
              EOPS%YPL_V(1:3) = '-0 '
              EOPS%YPL_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, U1__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, U1__GTP ) ) THEN
!
! ----------- Get leap seconds values: velues of function UTC minus TAI
!
              CALL ERR_PASS ( IUER, IER )
              MJD_VAL = IDINT ( EOP(J2)%MJD_EOP )
              UTC_VAL = EOP(J2)%MJD_EOP - MJD_VAL
              CALL ERR_PASS ( IUER, IER )
              CALL GET_UTC_M_TAI ( NERS, MJD_VAL, UTC_VAL, UTC_M_TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5044, IUER, 'WRITE_EOPS', 'Error in an '// &
     &                 'attempt to learn the value of leap second' )
                   CLOSE ( UNIT = LUN )
                   RETURN
              END IF
!
              WRITE ( UNIT=EOPS%U1_V, FMT='(F10.7)' ) EOP(J2)%U1_V* &
     &                                                RAD__TO__MSEC*1.D-3 - &
     &                                                UTC_M_TAI
            ELSE
              EOPS%U1_V(1:3) = '-0 '
              EOPS%U1_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, DPSI__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, DPSI__GTP ) ) THEN
              WRITE ( UNIT=EOPS%DPSI_V, FMT='(F8.3)' ) EOP(J2)%DPSI_V* &
     &                                                 RAD__TO__MAS
            ELSE
              EOPS%DPSI_V(1:3) = '-0 '
              EOPS%DPSI_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, DEPS__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, DEPS__GTP ) ) THEN
              WRITE ( UNIT=EOPS%DEPS_V, FMT='(F8.3)' ) EOP(J2)%DEPS_V* &
     &                                                 RAD__TO__MAS
            ELSE
              EOPS%DEPS_V(1:3) = '-0 '
              EOPS%DEPS_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, XPR__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, XPR__GTP ) ) THEN
              WRITE ( UNIT=EOPS%XPR_V, FMT='(F9.6)' ) EOP(J2)%XPR_V* &
     &                                              RAD__TO__MAS*1.D-3*86400.0D0
            ELSE
              EOPS%XPR_V(1:3) = '-0 '
              EOPS%XPR_E(1:3) = '-0 '
         END IF
!
!@          IF ( KBIT ( EOP(J2)%STATUS, YPR__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, YPR__GTP ) ) THEN
              WRITE ( UNIT=EOPS%YPR_V, FMT='(F9.6)' ) EOP(J2)%YPR_V* &
     &                                              RAD__TO__MAS*1.D-3*86400.0D0
            ELSE
              EOPS%YPR_V(1:3) = '-0 '
              EOPS%YPR_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, UTR__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, UTR__GTP ) ) THEN
              WRITE ( UNIT=EOPS%LOD_V, FMT='(F10.7)' ) -EOP(J2)%UTR_V/OM__EAR* &
     &                                                  86400.0D0
            ELSE
              EOPS%LOD_V(1:3) = '-0 '
              EOPS%LOD_E(1:3) = '-0 '
         END IF
!@          IF ( KBIT ( EOP(J2)%STATUS, XPL__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, XPL__GTP ) ) THEN
              WRITE ( UNIT=EOPS%XPL_E, FMT='(F8.6)' ) EOP(J2)%XPL_E* &
     &                                                RAD__TO__MAS*1.D-3
            ELSE
              EOPS%XPL_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, YPL__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, YPL__GTP ) ) THEN
              WRITE ( UNIT=EOPS%YPL_E, FMT='(F8.6)' ) EOP(J2)%YPL_E* &
     &                                                RAD__TO__MAS*1.D-3
            ELSE
              EOPS%YPL_E(1:3) = '-0 '
         END IF
!
!@          IF ( KBIT ( EOP(J2)%STATUS, U1__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, U1__GTP ) ) THEN
              WRITE ( UNIT=EOPS%U1_E, FMT='(F9.7)' ) EOP(J2)%U1_E* &
     &                                               RAD__TO__MSEC*1.D-3
            ELSE
              EOPS%U1_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, DPSI__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, DPSI__GTP ) ) THEN
              WRITE ( UNIT=EOPS%DPSI_E, FMT='(F7.3)' ) EOP(J2)%DPSI_E* &
     &                                                 RAD__TO__MAS
            ELSE
              EOPS%DPSI_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, DEPS__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, DEPS__GTP ) ) THEN
              WRITE ( UNIT=EOPS%DEPS_E, FMT='(F7.3)' ) EOP(J2)%DEPS_E* &
     &                                                 RAD__TO__MAS
            ELSE
              EOPS%DEPS_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, XPR__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, XPR__GTP ) ) THEN
              WRITE ( UNIT=EOPS%XPR_E, FMT='(F9.6)' ) EOP(J2)%XPR_E* &
     &                                              RAD__TO__MAS*1.D-3*86400.0D0
            ELSE
              EOPS%XPR_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, YPR__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, YPR__GTP ) ) THEN
              WRITE ( UNIT=EOPS%YPR_E, FMT='(F9.6)' ) EOP(J2)%YPR_E* &
     &                                              RAD__TO__MAS*1.D-3*86400.0D0
            ELSE
              EOPS%YPR_E(1:3) = '-0 '
         END IF
!
!@         IF ( KBIT ( EOP(J2)%STATUS, UTR__GTP ) ) THEN
         IF ( BTEST ( EOP(J2)%STATUS, UTR__GTP ) ) THEN
              WRITE ( UNIT=EOPS%LOD_E, FMT='(F10.7)' )  EOP(J2)%UTR_E/OM__EAR* &
     &                                                  86400.0D0
            ELSE
              EOPS%LOD_E(1:3) = '-0 '
         END IF
!
         WRITE ( UNIT=EOPS%C_XY,  FMT='(F6.4)' ) EOP(J2)%C_XY
         WRITE ( UNIT=EOPS%C_XU,  FMT='(F6.4)' ) EOP(J2)%C_XU
         WRITE ( UNIT=EOPS%C_YU,  FMT='(F6.4)' ) EOP(J2)%C_YU
         WRITE ( UNIT=EOPS%C_PE,  FMT='(F6.4)' ) EOP(J2)%C_PE
!
         WRITE ( UNIT=EOPS%NOBS,  FMT='(I6)'   ) EOP(J2)%NOBS
         WRITE ( UNIT=EOPS%DURA,  FMT='(F5.2)' ) EOP(J2)%DURA/3600.0D0
         WRITE ( UNIT=EOPS%WRMS,  FMT='(F7.2)' ) EOP(J2)%WRMS*1.D12
!
         EOPS%SCODE  = EOP(J2)%SCODE
         EOPS%FILL_1 = '-0'
         EOPS%FILL_2 = '-0'
         EOPS%FILL_3 = '-0'
         EOPS%FILL_4 = '-0'
!
         EOPS%C_NET = EOP(J2)%C_NET
!@ if ( ilen(EOP(J2)%C_NET) > 0 ) write ( 6, * ) ' j2=',j2,' c_net(1:10) =', &
!@      &        eop(j2)%c_net(1:10), ' c_net(1) = ', ichar(eop(j2)%c_net(1:1)) ! %%%%%
!
         CALL LIB$MOVC3 ( LEN_EOPS, EOPS, %REF(STR_EOPS) )
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR_EOPS(1:LEN_EOPS)
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 5043, IUER, 'WRITE_EOPS', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing in file '//FILEOPS )
              CLOSE ( UNIT = LUN )
              RETURN
         END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      CLOSE ( UNIT = LUN )
      RETURN
      END  !#!  WRITE_EOPS   #!#
