      PROGRAM    CHECK_LISTING_GAPS
! ************************************************************************
! *                                                                      *
! *   Program CHECK_LISTING_GAPS
! *                                                                      *
! * ## 20-MAY-2018 CHECK_LISTING_GAPS v2.0 (c)  L. Petrov 14-MAR-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MF
      PARAMETER  ( MF = 512*1024 )
      CHARACTER  DIRNAM*128, FILNAM*128, FIL*128, BUF(MF)*128, &
     &           DATE_STR*21, STR*128
      INTEGER*4  J1, J2, MP, IL, IS, MJD, MJD_LAST, NF, LEV, ID1, ID2, IUER
      INTEGER*8  DIR_DESC(16)
      REAL*8     TIM, TIM_LAST, TIM_STEP, DT
      LOGICAL*1  FL_GAP 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, GET_FILE_FROM_DIR
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: check_listing_gaps dirname time_step_in_hours'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIRNAM )
           CALL GETARG ( 2, STR )
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.5)' ) TIM_STEP
           TIM_STEP = 3600.0D0*TIM_STEP
      END IF
!
      FL_GAP = .FALSE.
      LEV = 0
      NF  = 0
      DO 410 J1=1,MF
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRNAM, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6010, IUER, 'CHECK_LISTING_GAPS', 'Error in '// &
     &            'reading input directory '//DIRNAM(1:I_LEN(DIRNAM))// &
     &             '  '//FILNAM )
              CALL EXIT ( 2 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '~' ) .GT. 0 ) GOTO 410
         NF = NF + 1
         BUF(NF) = FILNAM
 410  CONTINUE 
 810  CONTINUE 
      IF ( NF == 0 ) CALL EXIT ( 1 )
!
      CALL SORT_FAST_CH ( NF, BUF )
      ID1 = LINDEX ( BUF(1),  '/'  )
      ID2 = LINDEX ( BUF(NF), '/'  )
      WRITE ( 6, 110 ) NF, TRIM(BUF(1)(ID1+1:)), TRIM(BUF(NF)(ID2+1:))
 110  FORMAT ( I6, 2X, A, 2X, A )
!
      DO 420 J2=1,NF
         IS = LINDEX ( BUF(J2), '.nc4' )
         IF ( IS > 0 ) THEN
              IL = ILEN(BUF(J2))
              DATE_STR = BUF(J2)(IS-17:IS-14)//'.'// &
     &                   BUF(J2)(IS-13:IS-12)//'.'// &
     &                   BUF(J2)(IS-11:IS-7)//':'// &
     &                   BUF(J2)(IS-6:IS-5)//':00.0'
            ELSE 
              IS = LINDEX ( BUF(J2), '_' )
              IL = ILEN(BUF(J2))
              DATE_STR = BUF(J2)(IS-8:IS-5)//'.'// &
     &                   BUF(J2)(IS-4:IS-3)//'.'// &
     &                   BUF(J2)(IS-2:IS+2)//':'// &
     &                   BUF(J2)(IS+3:IS+4)//':00.0'
         END IF
         IUER = -1
         CALL DATE_TO_TIME ( DATE_STR, MJD, TIM, IUER )
         IF ( J2 > 1 ) THEN
              DT = (MJD - MJD_LAST )*86400.0D0 + (TIM - TIM_LAST )
              IF ( DT > TIM_STEP + 2.0D0 ) THEN
                   WRITE ( 6, 120 ) DT/86400.D0, BUF(J2-1)(1:IL)
     &                              
 120               FORMAT ( 'Gap ',F10.3, ' days after ', A )
                   FL_GAP = .TRUE.
              END IF
         END IF
         MJD_LAST = MJD
         TIM_LAST = TIM
 420  CONTINUE 
!
      IF ( FL_GAP ) THEN
           CALL EXIT ( 1 )
         ELSE
           CALL EXIT ( 0 )
      END IF
      END  PROGRAM  CHECK_LISTING_GAPS  !#!#
