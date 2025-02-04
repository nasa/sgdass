       PROGRAM    GET_STATION_TABLE_FROM_FITS_MAIN
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
       CALL GET_STATION_TABLE_FROM_FITS()
       END  PROGRAM  GET_STATION_TABLE_FROM_FITS_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE GET_STATION_TABLE_FROM_FITS()
! ************************************************************************
! *                                                                      *
! *   Program GET_STATION_TABLE_FROM_FITS
! *                                                                      *
! * ### 11-JUN-2021 GET_STATION_TABLE_FROM_FITS  v1.0 (c) L. Petrov  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  M_STA
      PARAMETER  ( M_STA = 256 )
      TYPE     ( PIMA__TYPE    ) :: PIM
      CHARACTER  FIL*128, C_STA(M_STA)*8
      REAL*8     STA_COO(3,M_STA)
      INTEGER*4  L_STA, J1, IUER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: get_station_table_from_fits {fits_file}'
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
      CALL GET_STA_TAB ( PIM, M_STA, L_STA, C_STA, STA_COO, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 410 J1=1,L_STA
         WRITE  ( 6, 110 ) FIL(1:I_LEN(FIL)), TRIM(C_STA(J1)), &
     &                     STA_COO(1:3,J1)
 110     FORMAT ( A, 2X, A, 2X, 3(F13.3,1X) )
 410  CONTINUE 
      END  SUBROUTINE  GET_STATION_TABLE_FROM_FITS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_STA_TAB ( PIM, M_STA, L_STA, C_STA, STA_COO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_STA_TAB 
! *                                                                      *
! *  ### 11-JUN-2021  GET_STA_TAB  v1.0 (c)  L. Petrov  11-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  M_STA, L_STA, IUER
      CHARACTER  C_STA(M_STA)*(*)
      REAL*8     STA_COO(3,M_STA)
      CHARACTER  TABLE_NAME*32, STR*128
      INTEGER*4  J1, J2, J3, J4, IND_STA_TAB, IND_STA_NAM, IND_STA_COO, IP, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFITS_OPEN ( PIM%FILE(1)%NAME, PIM%FILE(1)%FITS_DESC, 'OLD', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7821, IUER, 'GET_STA_TAB', 'Error in an attempt '// &
     &         'to open FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      CALL ERR_PASS       ( IUER, IER )
      CALL FFITS_GET_KEYP ( PIM%FILE(1)%FITS_DESC, PIM__MHDR, PIM__MKWD, &
     &                      PIM%FILE(1)%M_KWD, PIM%FILE(1)%L_HDR, &
     &                      PIM%FILE(1)%L_KWD, PIM%FILE(1)%KEY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7822, IUER, 'GET_STA_TAB', 'Error in an attempt '// &
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
           TABLE_NAME = 'ARRAY_GEOMETRY'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_I4 ( PIM, 1, TABLE_NAME, 'NAXIS2', L_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7823, IUER, 'GET_STA_TAB', 'Failure to get '// &
     &         'the number of stations in FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
      DO 410 J1=1,PIM%FILE(1)%L_HDR
         DO 420 J2=1,PIM%FILE(1)%L_KWD(J1)
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:8) == 'EXTNAME ' ) THEN
                 IF ( PIM%FILE(1)%KEY(J2,J1)(11:26) == "'ARRAY_GEOMETRY'" ) THEN
                      IND_STA_TAB = J1
                 END IF
                 IF ( PIM%FILE(1)%KEY(J2,J1)(11:20) == "'AIPS SU '" ) THEN
                      IND_STA_TAB = J1
                 END IF
            END IF
            IF ( IND_STA_TAB == J1 ) THEN
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'ANNAME  '" ) > 0 .AND. &
     &                PIM%FILE(1)%KEY(J2,J1)(1:5) == 'TTYPE' ) THEN
                      IND_STA_NAM = J2
                 END IF
                 IF ( INDEX ( PIM%FILE(1)%KEY(J2,J1), "'STABXYZ '" ) > 0 .AND. &
     &                PIM%FILE(1)%KEY(J2,J1)(1:5) == 'TTYPE' ) THEN
                      IND_STA_COO = J2
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,L_STA
         CALL ERR_PASS ( IUER, IER )
         CALL FFITS_GETCH ( PIM%FILE(1)%FITS_DESC, IND_STA_TAB, J3, &
     &                      PIM%FILE(1)%KEY(IND_STA_NAM,IND_STA_TAB), 1, &
     &                      C_STA(J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 7824, IUER, 'GET_STA_TAB', 'Error in '// &
     &            'reading the name of the '//STR(1:I_LEN(STR))//'-th '// &
     &            'source in the FITS-IDI file '//PIM%FILE(1)%NAME  )
              RETURN
         END IF
!
! ------ Replace binary zeroes with blanks for further comparison
!
         IP = INDEX ( C_STA(J3), CHAR(0) )
         IF ( IP > 0 ) CALL CLRCH ( C_STA(J3)(IP:) )
!
         CALL ERR_PASS ( IUER, IER )
         CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_STA_TAB, J3, &
     &                      PIM%FILE(1)%KEY(IND_STA_COO,IND_STA_TAB), 3, &
     &                      STA_COO(1,J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 7825, IUER, 'GET_STA_TAB', 'Error in '// &
     &            'getting right ascension of the '//STR(1:I_LEN(STR))// &
     &            '-th source in the FITS-IDI file '//PIM%FILE(1)%NAME  )
              RETURN
         END IF
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_STA_TAB  !#!#
