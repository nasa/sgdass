      PROGRAM    SORT_EOB
! ************************************************************************
! *                                                                      *
! *   Program SORT_EOB  sorts file in IERS EOPB format. It adds          *
! *   character @  beafore each line with data.                          *
! *                                                                      *
! *  ### 22-SEP-2000    SORT_EOB   v2.2 (c)  L. Petrov  30-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'param.i'
      INCLUDE   'getpar.i'
      INCLUDE   'precm.i'
      INTEGER*4  MHEAD, MSES
      PARAMETER  ( MHEAD = 256, MSES = 8192 )
      CHARACTER  STR*256, FILIN*128, FILOUT*128
      CHARACTER  HEAD_BUF(MHEAD)*80, MASTER_DIR*128, FLAG*1
      INTEGER*4  MG_BUF, LG_BUF
      PARAMETER  ( MG_BUF = MAX_ARCS )
      CHARACTER  G_BUF(MG_BUF)*128, MARK3_NAME*10
      REAL*8     ARR1(MSES), ARR2(MSES)
      TYPE ( EOP__STRU ) ::  EOP(MSES)
      INTEGER*4  NUMARG, NSES, NHEAD, IOS, IP, J1, J2, J3, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      LOGICAL*4  FL_MARK
!
      NUMARG = IARGC ()
      IF ( NUMARG .LT. 3 ) THEN
           WRITE ( 6, '(A/A)' ) ' Usage: sort_eob <input_eob> <output_eob> '// &
     &                          '(-@ | -no@) '
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILOUT )
           CALL GETARG ( 3, STR    )
           IF ( STR(1:2) .EQ. '-@' ) THEN
                FL_MARK = .TRUE.
              ELSE IF ( STR(1:4) .EQ. '-no@' ) THEN
                FL_MARK = .FALSE.
              ELSE
                CALL ERR_LOG ( 4770, IUER, 'SORT_EOB', 'Wrong third '// &
     &                         'argument: '//STR(1:I_LEN(STR))// &
     &                         '  -@ or -no@ was expected' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      CALL GETENVAR ( 'SAVE_DIR', PRE_SAV_DIR )
      PRE_SV_LEN = I_LEN(PRE_SAV_DIR)
!
      WRITE ( 6, * ) 'Sorting ...'
      IUER = -1
      CALL READ_EOB ( FILIN, MHEAD, NHEAD, HEAD_BUF, MSES, NSES, EOP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, * ) 'Header had ', NHEAD, ' lines; body had ',NSES,' lines'
!
      DO 410 J1=1,NSES
         ARR1(J1) = EOP(J1)%MJD_EOP + &
     &              ( ICHAR( EOP(J1)%DBNAME(10:10) ) - 96)*1.D-8
         ARR2(J1) = J1 + 0.001D0
 410  CONTINUE
!
      CALL SORT8 ( NSES, ARR1, ARR2 )
!
      OPEN ( UNIT=11, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5711, IUER, 'SORT_EOB', 'Error '//STR(1:I_LEN(STR))// &
     &         'in an attempt to open output file '//FILOUT )
           CALL EXIT ( 2 )
      END IF
!
      WRITE ( UNIT=11, FMT='(A)', IOSTAT=IOS ) SIG_EOB
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, IUER, 'SORT_EOB', 'Error '//STR(1:I_LEN(STR))// &
     &         'in an attempt to write the first record to the output file '// &
     &          FILOUT )
           CALL EXIT ( 2 )
      END IF
!
      DO 420 J2=1,NHEAD
         WRITE ( UNIT=11, FMT='(A)' ) HEAD_BUF(J2)(1:I_LEN(HEAD_BUF(J2)))
 420  CONTINUE
      WRITE ( UNIT=11, FMT='(A)' ) '# Sorted by sort_eob 2.2 at '//GET_CDATE()
      WRITE ( UNIT=11, FMT='(A)' ) '# '
!
! --- Get directory for master files
!
      CALL GETENV ( 'MASTER_DIR', MASTER_DIR )
      IF ( ILEN(MASTER_DIR) .LE. 0 ) THEN
           MASTER_DIR = MASTER_DIR_DEF
      END IF
      IF ( MASTER_DIR(I_LEN(MASTER_DIR):I_LEN(MASTER_DIR)) .NE. '/' ) THEN
           MASTER_DIR = MASTER_DIR(1:I_LEN(MASTER_DIR))//'/'
      END IF
!
      LG_BUF = 0
      DO 430 J3=1,NSES
         IF ( FL_MARK ) THEN
              FLAG = '@'
            ELSE
              FLAG = ' '
         END IF
         IP = ARR2(J3)
!
         IUER = -1
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, XPL__GTP  ) ) EOP(IP)%XPL_V  = 0.0D0
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, YPL__GTP  ) ) EOP(IP)%YPL_V  = 0.0D0
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, U1__GTP   ) ) EOP(IP)%U1_V   = 0.0D0
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, DPSI__GTP ) ) EOP(IP)%DPSI_V = 0.0D0
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, DEPS__GTP ) ) EOP(IP)%DEPS_V = 0.0D0
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, XPR__GTP  ) ) EOP(IP)%XPR_V  = 0.0D0
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, YPR__GTP  ) ) EOP(IP)%YPR_V  = 0.0D0
         IF ( .NOT. BTEST ( EOP(IP)%STATUS, UTR__GTP  ) ) EOP(IP)%UTR_V  = 0.0D0
!
         IUER = 0
         CALL RESOLVE_DBNAME ( MASTER_DIR, EOP(IP)%DBNAME, EOP(IP)%SCODE, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = 0
!@              CALL GVF_TO_MARK3_NAME ( MG_BUF, LG_BUF, G_BUF, EOP(IP)%DBNAME, &
!@     &                                 MARK3_NAME, IUER )
              IF ( IUER == 0 ) THEN
                  CALL RESOLVE_DBNAME ( MASTER_DIR, MARK3_NAME, &
     &                                  EOP(IP)%SCODE, IUER )
              END IF
         END IF
         CALL TRAN ( 12, EOP(IP)%SCODE, EOP(IP)%SCODE )
         IF ( IUER .NE. 0 ) EOP(IP)%SCODE  = '??--??'
!
         IUER = -1
         CALL WRITE_EOB_REC ( 11, FLAG, EOP(IP)%MJD_EOP, EOP(IP)%MJD_NUT, &
     &        EOP(IP)%DBNAME, &
     &        EOP(IP)%XPL_V,  EOP(IP)%YPL_V,  EOP(IP)%U1_V,  EOP(IP)%DPSI_V, &
     &        EOP(IP)%DEPS_V, EOP(IP)%XPR_V,  EOP(IP)%YPR_V, EOP(IP)%UTR_V, &
     &        EOP(IP)%XPL_E,  EOP(IP)%YPL_E,  EOP(IP)%U1_E,  EOP(IP)%DPSI_E, &
     &        EOP(IP)%DEPS_E, EOP(IP)%XPR_E,  EOP(IP)%YPR_E, EOP(IP)%UTR_E, &
     &        EOP(IP)%C_XY, EOP(IP)%C_XU, EOP(IP)%C_YU, EOP(IP)%C_PE, &
     &        EOP(IP)%C_URX, EOP(IP)%C_URY, EOP(IP)%C_URU, &
     &        EOP(IP)%DURA, EOP(IP)%WRMS, EOP(IP)%NOBS, EOP(IP)%SCODE, &
     &        EOP(IP)%C_NET, IUER )
         IF ( IUER .NE. 0 ) CALL EXIT ( 3 )
 430  CONTINUE
      CLOSE ( UNIT = 11 )
      WRITE ( 6, * ) 'Sorted'
!
      END  !#!  SORT_EOB  #!#
