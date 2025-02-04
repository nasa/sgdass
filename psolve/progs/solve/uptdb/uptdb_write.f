      SUBROUTINE UPTDB_WRITE ( GVH, OPCODE, ENV_FINAM, GVH_DB_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  UPTDB_WRITE 
! *                                                                      *
! * ### 06-DEC-2005   UPTDB_WRITE   v1.3 (c)  L. Petrov 26-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) :: GVH
      CHARACTER  GVH_DB_DIR*(*)
      INTEGER*4  OPCODE, IUER
      CHARACTER  ENV_FINAM*(*)
      INTEGER*4  M_ENV
      PARAMETER  ( M_ENV = 32 )
      CHARACTER  BUF(M_ENV)*128
      CHARACTER  EXP_NAME*128, FINAM(M_ENV)*128, SESS_CODE*16, SUFFIX*1, &
     &           FINAM_ENV_OUT*128
      LOGICAL*4  FL_TH1, FL_SL1, FL_TH1_NEW, FL_SL1_NEW
      INTEGER*4  SEG_TH1, SEG_SL1
      INTEGER*4  J1, J2, J3, IB, IE, IV, IL, L_FIL, N_ENV, EXP_VERSION, &
     &           IVER, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      SEG_TH1 = 2 
      SEG_SL1 = 3
!
! --- Read the file with the database envelope
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( ENV_FINAM, M_ENV, BUF, N_ENV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6511, IUER, 'UPTDB_WRITE', 'Error in an attempt '// &
     &         'to read envelop file '//ENV_FINAM )
           RETURN 
      END IF
!
! --- Extract the expriment name and the envelope version form the 
! --- envelope filename
!
      IB = LINDEX ( ENV_FINAM, '/' ) + 1
      IE =  INDEX ( ENV_FINAM(IB:), '_' ) + IB-2
      IF ( IE .LE. IB ) IE = ILEN(ENV_FINAM)
      IV  = LINDEX ( ENV_FINAM, '_v' )
      EXP_NAME = ENV_FINAM(IB:IB+9)
      CALL CHIN ( ENV_FINAM(IV+2:IV+4), EXP_VERSION )
!
      FL_TH1 = .FALSE.
      FL_SL1 = .FALSE.
      L_FIL = 0
      DO 410 J1=1,N_ENV
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '!' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '$' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         L_FIL = L_FIL + 1
         SUFFIX    = BUF(J1)(21:21) 
         SESS_CODE = BUF(J1)(23:) 
!
         IF ( SEG_TH1 > 0  .AND.  BUF(J1)(9:11) == 'th1' ) THEN
              IF ( OPCODE == 2 )  THEN
!
! ---------------- Update the version counter for the theoreticals segment
!
                   CALL CHIN ( BUF(J1)(13:15), IVER )
                   IVER = IVER + 1
                   CALL INCH ( IVER, BUF(J1)(13:15) )
                   CALL CHASHR     ( BUF(J1)(13:15) )
              END IF
              FL_TH1 = .TRUE.
         END IF
!
         IF ( SEG_SL1 > 0  .AND.  BUF(J1)(9:11) == 'sl1' ) THEN
              IF ( OPCODE == 2 )  THEN
!
! ---------------- Update the version counter for the solution segment
!
                   CALL CHIN ( BUF(J1)(13:15), IVER )
                   IVER = IVER + 1
                   CALL INCH ( IVER, BUF(J1)(13:15) )
                   CALL CHASHR     ( BUF(J1)(13:15) )
              END IF
              FL_SL1 = .TRUE.
         END IF
!
         FINAM(L_FIL) = GVH_DB_DIR(1:I_LEN(GVH_DB_DIR))//'/'//ENV_FINAM(IB:IE)
!
! ------ Update the file name
!
         CALL BLANK_TO_ZERO ( BUF(J1)(13:15))
         FINAM(L_FIL) = FINAM(L_FIL)(1:I_LEN(FINAM(L_FIL)))//'_'// &
     &                  SUFFIX//'_'//TRIM(SESS_CODE)//'_'// &
     &                  BUF(J1)(9:11)//'_v'//BUF(J1)(13:15)//'.'// &
     &                  BUF(J1)(17:19)
         IF ( BUF(J1)(9:11) == 'th1' ) THEN
              GVH%FILENAME(SEG_TH1) = FINAM(L_FIL) 
         END IF
         IF ( BUF(J1)(9:11) == 'sl1' ) THEN
              GVH%FILENAME(SEG_SL1) = FINAM(L_FIL) 
         END IF
 410  CONTINUE 
      IF ( SEG_TH1 > 0  .AND.  .NOT. FL_TH1 ) THEN
           N_ENV = N_ENV + 1
           BUF(N_ENV)(1:20) = 'SYS OPT th1 001 bgv '
           BUF(N_ENV)(21:21) = SUFFIX
           BUF(N_ENV)(23:)  = SESS_CODE
           GVH%FILENAME(SEG_TH1) = GVH_DB_DIR(1:I_LEN(GVH_DB_DIR))//'/'// &
     &                             ENV_FINAM(IB:IE)//'_'//SUFFIX//'_'// &
     &                             SESS_CODE(1:I_LEN(SESS_CODE))// &
     &                             '_th1_v001.bgv'
      END IF
!
      IF ( SEG_SL1 > 0  .AND.  .NOT. FL_SL1 ) THEN
           N_ENV = N_ENV + 1
           BUF(N_ENV)(1:20)  = 'SYS OPT sl1 001 bgv '
           BUF(N_ENV)(21:21) = SUFFIX
           BUF(N_ENV)(23:)   = SESS_CODE
           GVH%FILENAME(SEG_SL1) = GVH_DB_DIR(1:I_LEN(GVH_DB_DIR))//'/'// &
     &                             ENV_FINAM(IB:IE)//'_'//SUFFIX//'_'// &
     &                             SESS_CODE(1:I_LEN(SESS_CODE))// &
     &                             '_sl1_v001.bgv'
      END IF
!
      IF ( SEG_TH1 > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_WRITE_BGV ( GVH, SEG_TH1, GVH__CRT, GVH%FILENAME(SEG_TH1), &
     &                          IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6512, IUER, 'UPTDB_WRITE', 'Error in '// &
     &             'attempt to update TH1 segment of the database '// &
     &              EXP_NAME )
                RETURN 
           END IF
      END IF
!
      IF ( SEG_SL1 > 0 ) THEN
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_WRITE_BGV ( GVH, SEG_SL1, GVH__CRT, GVH%FILENAME(SEG_SL1), &
     &                          IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6513, IUER, 'UPTDB_WRITE', 'Error in '// &
     &             'attempt to update SL1 segment of the database '// &
     &              EXP_NAME )
                RETURN 
           END IF
      END IF
!
      IF ( OPCODE == 2  .OR.  &
     &     SEG_TH1 > 0  .OR.  &
     &     SEG_SL1 > 0        ) THEN
!
           FINAM_ENV_OUT = ENV_FINAM
           IF ( OPCODE == 2 ) THEN
                IL = ILEN ( FINAM_ENV_OUT )
                CALL CHIN ( FINAM_ENV_OUT(IL-6:IL-4), IVER )
                IVER = IVER + 1
                CALL INCH    ( IVER, FINAM_ENV_OUT(IL-6:IL-4) )
                CALL CHASHR  (       FINAM_ENV_OUT(IL-6:IL-4) )
                CALL BLANK_TO_ZERO ( FINAM_ENV_OUT(IL-6:IL-4) )
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT ( N_ENV, BUF, FINAM_ENV_OUT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6514, IUER, 'UPTDB_WRITE', 'Error in '// &
     &             'writing updated version of the envelop file '// &
     &              FINAM_ENV_OUT )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  UPTDB_WRITE  !#!#
