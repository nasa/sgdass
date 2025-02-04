       PROGRAM    GET_SOURCE_TABLE_FROM_FITS_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL GET_SOURCE_TABLE_FROM_FITS()
       END  PROGRAM  GET_SOURCE_TABLE_FROM_FITS_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE GET_SOURCE_TABLE_FROM_FITS()
! ************************************************************************
! *                                                                      *
! *   Program GET_SOURCE_TABLE_FROM_FITS
! *                                                                      *
! * ###  23-MAY-2011  GET_SOURCE_TABLE_FROM_FITS v1.0 (c) L. Petrov ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  M_SOU
      PARAMETER  ( M_SOU = 8192 )
      TYPE     ( PIMA__TYPE    ) :: PIM
      CHARACTER  FIL*128, C_SOU(M_SOU)*16, ALP_STR*13, DEC_STR*13
      REAL*8     ALP_SOU(M_SOU), DEC_SOU(M_SOU) 
      INTEGER*4  L_SOU, J1, IUER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
!@      FIL = '/d5/vera_fits/R08238B-NO1.FITS.1'
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: get_source_table_from_fits {fits_file}'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FIL )
      END IF
      CALL PIMA_INIT ( PIM )
!
      PIM%L_FIL = 1
      ALLOCATE ( PIM%FILE(1) )
      PIM%FILE(1)%NAME = FIL
!
      IUER = -1
      CALL GET_SRC_TAB ( PIM, M_SOU, L_SOU, C_SOU, ALP_SOU, DEC_SOU, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 410 J1=1,L_SOU
         CALL RH_TAT ( ALP_SOU(J1), 4, ALP_STR, -2 )
         CALL RG_TAT ( DEC_SOU(J1), 3, DEC_STR, -2 )
         IF ( DEC_STR(1:1) == ' ' ) DEC_STR(1:1) = '+'
         WRITE  ( 6, 110 ) FIL(1:I_LEN(FIL)), C_SOU(J1)(1:I_LEN(C_SOU(J1))), &
     &                     ALP_STR, DEC_STR
 110     FORMAT ( A, 2X, A, 2X, A, 2X, A )
 410  CONTINUE 
      END  SUBROUTINE  GET_SOURCE_TABLE_FROM_FITS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_SRC_TAB ( PIM, M_SOU, L_SOU, C_SOU, ALP_SOU, DEC_SOU, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_SRC_TAB 
! *                                                                      *
! *  ### 23-JAN-2009  GET_SRC_TAB  v1.0 (c)  L. Petrov  23-JAN-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  M_SOU, L_SOU, IUER
      CHARACTER  C_SOU(M_SOU)*(*)
      REAL*8     ALP_SOU(M_SOU), DEC_SOU(M_SOU) 
      CHARACTER  TABLE_NAME*32, STR*128
      REAL*8     R8_ARR(32*1024)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IND_SOU_NAM, IND_SOU_TAB, &
     &           IND_SOU_ALP, IND_SOU_DEC, IND_SOU_IND, IP, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFITS_OPEN ( PIM%FILE(1)%NAME, PIM%FILE(1)%FITS_DESC, 'OLD', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7811, IUER, 'GET_SRC_TAB', 'Error in an attempt '// &
     &         'to open FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      CALL ERR_PASS       ( IUER, IER )
      CALL FFITS_GET_KEYP ( PIM%FILE(1)%FITS_DESC, PIM__MHDR, PIM__MKWD, &
     &                      PIM%FILE(1)%M_KWD, PIM%FILE(1)%L_HDR, &
     &                      PIM%FILE(1)%L_KWD, PIM%FILE(1)%KEY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7812, IUER, 'GET_SRC_TAB', 'Error in an attempt '// &
     &         'to get keys from FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      CALL CLRCH ( PIM%GENERATOR )
      PIM%GENERATOR = 'Undefined'
      IER = 0
      CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'ORIGIN', STR, IER )
      IF ( IER .EQ. 0 ) THEN
           IF ( STR(1:4) == 'AIPS' ) THEN
                PIM%GENERATOR = 'AIPS'
              ELSE
                PIM%GENERATOR = STR
           END IF
         ELSE
           IER = 0
           CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'ARRNAM', STR, IER )
           PIM%GENERATOR = STR
      END IF
!
      IF ( PIM%GENERATOR == 'AIPS' ) THEN
           TABLE_NAME = 'AIPS SU '
         ELSE
           TABLE_NAME = 'SOURCE  '
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_I4 ( PIM, 1, TABLE_NAME, 'NAXIS2', L_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7813, IUER, 'GET_SRC_TAB', 'Failure to get '// &
     &         'the number of stations in FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
      DO 410 J1=1,PIM%FILE(1)%L_HDR
         DO 420 J2=1,PIM%FILE(1)%L_KWD(J1)
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:8) == 'EXTNAME ' ) THEN
                 IF ( PIM%FILE(1)%KEY(J2,J1)(11:20) == "'SOURCE  '" ) THEN
                      IND_SOU_TAB = J1
                 END IF
                 IF ( PIM%FILE(1)%KEY(J2,J1)(11:20) == "'AIPS SU '" ) THEN
                      IND_SOU_TAB = J1
                 END IF
            END IF
            IF ( IND_SOU_TAB == J1 ) THEN
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'SOURCE  '" ) > 0 .AND. &
     &                PIM%FILE(1)%KEY(J2,J1)(1:5) == 'TTYPE' ) THEN
                      IND_SOU_NAM = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'SOURCE          '" ) > 0 .AND. &
     &                PIM%FILE(1)%KEY(J2,J1)(1:5) == 'TTYPE' ) THEN
                      IND_SOU_NAM = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'RAEPO   '" ) > 0 .OR. &
     &                INDEX ( PIM%FILE(1)%KEY(J2,J1), "'RAEPO           '" ) > 0 ) THEN
                      IND_SOU_ALP = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'DECEPO  '" ) > 0 .OR. &
     &                INDEX ( PIM%FILE(1)%KEY(J2,J1), "'DECEPO          '" ) > 0 ) THEN
                      IND_SOU_DEC = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'ID_NO.  '" ) > 0 ) THEN
                      IND_SOU_IND = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'SOURCE_ID'" ) > 0 ) THEN
                      IND_SOU_IND = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'ID. NO.         '" ) > 0 ) THEN
                      IND_SOU_IND = J2
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,L_SOU
         CALL ERR_PASS ( IUER, IER )
         CALL FFITS_GETCH ( PIM%FILE(1)%FITS_DESC, IND_SOU_TAB, J3, &
     &                      PIM%FILE(1)%KEY(IND_SOU_NAM,IND_SOU_TAB), 1, &
     &                      C_SOU(J3), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL ERR_LOG ( 7814, IUER, 'GET_SRC_TAB', 'Error in '// &
     &               'reading the name of the '//STR(1:I_LEN(STR))//'-th '// &
     &               'source in the FITS-IDI file '//PIM%FILE(1)%NAME  )
                 RETURN
            END IF
!
! --------- Replace binary zeroes with blanks for further comparison
!
            IP = INDEX ( C_SOU(J3), CHAR(0) )
            IF ( IP > 0 ) CALL CLRCH ( C_SOU(J3)(IP:) )
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_SOU_TAB, J3, &
     &                         PIM%FILE(1)%KEY(IND_SOU_ALP,IND_SOU_TAB), 1, &
     &                         R8_ARR(1+(J3-1)*2), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL ERR_LOG ( 7815, IUER, 'GET_SRC_TAB', 'Error in '// &
     &               'getting right ascension of the '//STR(1:I_LEN(STR))// &
     &               '-th source in the FITS-IDI file '//PIM%FILE(1)%NAME  )
                 RETURN
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_SOU_TAB, J3, &
     &                         PIM%FILE(1)%KEY(IND_SOU_DEC,IND_SOU_TAB), 1, &
     &                         R8_ARR(2+(J3-1)*2), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL ERR_LOG ( 7816, IUER, 'GET_SRC_TAB', 'Error in '// &
     &               'getting declination of the '//STR(1:I_LEN(STR))// &
     &               '-th source in the FITS-IDI file '//PIM%FILE(1)%NAME  )
                 RETURN
            END IF
!
            ALP_SOU(J3) = R8_ARR(1+(J3-1)*2)*DEG__TO__RAD 
            DEC_SOU(J3) = R8_ARR(2+(J3-1)*2)*DEG__TO__RAD 
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_SRC_TAB  !#!#
