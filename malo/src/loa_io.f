      SUBROUTINE LOA_WRI ( NLON, NLAT, MJD, TAI, DSP_ARR, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LOA_WRI
! *                                                                      *
! * ### 21-NOV-2012     LOA_WRI    v1.0 (c)  L. Petrov  21-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INTEGER*4  NLON, NLAT, MJD, IUER
      REAL*8     TAI, DSP_ARR(3,NLON,NLAT)
      REAL*4,    ALLOCATABLE :: DSP_R4(:,:,:)
      CHARACTER  FILOUT*(*)
      CHARACTER  STR*128, LOA_TXT(MALO__LOA_LTXT)*(MALO__LOA_LSTR)
      INTEGER*4  LUN, IER
      INTEGER*8  IS, IP, J1
      INTEGER*8, EXTERNAL :: WRITE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( UNIT=LOA_TXT(1), FMT='(I5)'   ) MJD
      LOA_TXT(1) = 'MJD:  '//LOA_TXT(1) 
      WRITE ( UNIT=LOA_TXT(2), FMT='(F7.1)' ) TAI
      LOA_TXT(2) = 'TAI:  '//LOA_TXT(2) 
      WRITE ( UNIT=LOA_TXT(3), FMT='(I5)'   ) NLON
      LOA_TXT(3) = 'NLON: '//LOA_TXT(3) 
      WRITE ( UNIT=LOA_TXT(4), FMT='(I5)'   ) NLAT
      LOA_TXT(4) = 'NLAT: '//LOA_TXT(4) 
      WRITE ( UNIT=LOA_TXT(5), FMT='(F10.5)' ) 0.0
      LOA_TXT(5) = 'LON_DEG_START: '//LOA_TXT(5)
      WRITE ( UNIT=LOA_TXT(6), FMT='(F10.5)' ) 360.0D0/NLON
      LOA_TXT(6) = 'LON_DEG_STEP:  '//LOA_TXT(6)
      WRITE ( UNIT=LOA_TXT(7), FMT='(F10.5)' ) -90.0
      LOA_TXT(7) = 'LAT_DEG_START: '//LOA_TXT(7)
      WRITE ( UNIT=LOA_TXT(8), FMT='(F10.5)' ) 180.0D0/NLAT
      LOA_TXT(8) = 'LAT_DEG_STEP:  '//LOA_TXT(8)
!
      DO 410 J1=1,MALO__LOA_LTXT
         IP = ILEN(LOA_TXT(J1))
         IF ( IP < MALO__LOA_LSTR ) THEN
              CALL CLRCH ( LOA_TXT(J1)(IP+1:) )
         END IF
         LOA_TXT(J1)(MALO__LOA_LSTR:MALO__LOA_LSTR) = CHAR(10) 
 410  CONTINUE 
!
      ALLOCATE ( DSP_R4(3,NLON,NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH  ( 4*3*NLON*NLAT, STR )
           CALL ERR_LOG ( 6151, IUER, 'LOA_WRI', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for termporary array DSP_R4' )
           RETURN 
      END IF
!
      DSP_R4(1:3,1:NLON,1:NLAT) = DSP_ARR(1:3,1:NLON,1:NLAT) 
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6152, IUER, 'LOA_WRI', 'Failure in an attempt '// &
     &         'to open the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- '//STR )
           RETURN 
      END IF
!
      IS = WRITE ( %VAL(LUN), %REF(LOA__LABEL//CHAR(10)), &
     &             %VAL(LEN(LOA__LABEL)+1) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6153, IUER, 'LOA_WRI', 'Failure in an attempt '// &
     &         'to write the 1st line in the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
        ELSE IF ( IS .NE. LEN(LOA__LABEL)+1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6154, IUER, 'LOA_WRI', 'Failure in an attempt '// &
     &         'to write the 1st line in the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- not all the data were written' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
!
      IS = WRITE ( %VAL(LUN), %REF(LOA_TXT), &
     &             %VAL(MALO__LOA_LTXT*MALO__LOA_LSTR) )
      IF ( IS .NE. MALO__LOA_LTXT*MALO__LOA_LSTR ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6155, IUER, 'LOA_WRI', 'Failure in an attempt '// &
     &         'to write the loading header in the the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
!
      IS = WRITE ( %VAL(LUN), DSP_R4, %VAL(4*3*NLON*NLAT) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6156, IUER, 'LOA_WRI', 'Failure in an attempt '// &
     &         'to write the loading header in the the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
        ELSE IF ( IS .NE. 4*3*NLON*NLAT ) THEN
           CALL ERR_LOG ( 6157, IUER, 'LOA_WRI', 'Failure in an attempt '// &
     &         'to write the loading header in the the output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- not all the data were written' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
!
      DEALLOCATE ( DSP_R4 )
      CALL BINF_CLOSE ( LUN, IER )
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LOA_WRI  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LOA_INQ ( FILIN, NLON, NLAT, MJD, TAI, M_TXT, L_TXT, C_TXT, &
     &                     DATA_OFFS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LOA_INQ 
! *                                                                      *
! *  ### 21-SEP-2012    LOA_INQ    v1.1 (c)  L. Petrov  20-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INTEGER*4  DEG, M_TXT, L_TXT, NLON, NLAT, MJD, DATA_OFFS, IUER
      REAL*8     TAI
      CHARACTER  FILIN*(*), C_TXT(M_TXT)*(*)
      INTEGER*8  SIZE_I8
      CHARACTER  STR*256, BUF(MALO__LOA_LTXT)*(MALO__LOA_LSTR), REG*4
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//CHAR(10) )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 ) 
      INTEGER*4  UNIX_DATE, LUN, J1, J2, J3, IP, IR, MAX_SIZE, LIND, &
     &           IND(2,MIND), IER
      INTEGER*8  IS
      INTEGER*8, EXTERNAL :: READ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, INDEX_I1
!
      DATA_OFFS = -1
      L_TXT = 0
!
      IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, &
     &                 SIZE_I8 )                                         
      IF ( IS .NE. 0 ) THEN                                              
           CALL GERROR ( STR )                                           
           CALL ERR_LOG ( 6171, IUER, 'LOA_INQ', 'Failure in an attempt '// &
     &         'to collect information about file '//FILIN(1:I_LEN(FILIN))// &
     &          ' '//STR )
           RETURN 
      END IF                                                             
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6172, IUER, 'LOAD_INQ', 'Failure in an attempt '// &
     &         'to open input file '//FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), %REF(STR), %VAL(LEN(LOA__LABEL)+1) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6173, IUER, 'LOA_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
         ELSE IF ( IS .NE. LEN(LOA__LABEL)+1 ) THEN
           CALL ERR_LOG ( 6174, IUER, 'LOA_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- only a part of data was read' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
      IF ( STR(1:LEN(LOA__LABEL)) == LOA__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 6175, IUER, 'LOA_INQ', 'Unrecognzed format '// &
      &         'label in the input file '//FILIN(1:I_LEN(FILIN))// &
      &         ' -- '//STR(1:LEN(LOA__LABEL))//' while '//LOA__LABEL// &
      &         ' was expected' )
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), %REF(BUF), %VAL(MALO__LOA_LSTR*MALO__LOA_LTXT) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6176, IUER, 'LOA_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
        ELSE IF ( IS .NE. MALO__LOA_LSTR*MALO__LOA_LTXT ) THEN
           CALL ERR_LOG ( 6177, IUER, 'LOA_INQ', 'Failure in an attempt '// &
     &         'to read in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- only a part of data was read' )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
      CALL BINF_CLOSE ( LUN, IER )
!
      L_TXT = 0
      DO 410 J1=1,MALO__LOA_LTXT 
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NLON:'  ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) NLON
              IF ( IER .NE. 0 .OR. NLON < 1 .OR. NLON > MALO__MDIM ) THEN
                   CALL TRAN ( 13, STR, STR )
                   CALL ERR_LOG ( 6178, IUER, 'LOA_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key NLON: a non-zero '// &
     &                 'integer was expected, bug got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NLAT:'  ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) NLAT
              IF ( IER .NE. 0 .OR. NLON < 1 .OR. NLAT > MALO__MDIM ) THEN
                   CALL TRAN ( 13, STR, STR )
                   CALL ERR_LOG ( 6179, IUER, 'LOA_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key NLAT: a non-zero '// &
     &                 'integer was expected, bug got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MJD:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) MJD
              IF ( IER .NE. 0 .OR. MJD < 50000 .OR. MJD > 60000 ) THEN
                   CALL TRAN ( 13, STR, STR )
                   CALL ERR_LOG ( 6180, IUER, 'LOA_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key MJD: a non-zero '// &
     &                 'integer was expected, bug got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TAI:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F7.1)', IOSTAT=IER ) TAI
              IF ( IER .NE. 0 .OR. TAI < -90000.0 .OR. TAI > 90000.0D0 ) THEN
                   CALL TRAN ( 13, STR, STR )
                   CALL ERR_LOG ( 6181, IUER, 'LOA_INQ', 'Wrong format of '// &
     &                 'the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' -- wrong value of key TAI: a float '// &
     &                 'number in range [-90000, 90000] was expected, '// &
     &                 ' bug got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'COMMENT:' ) THEN
              L_TXT = L_TXT + 1
              CALL CLRCH ( C_TXT(L_TXT) )
              C_TXT(L_TXT) = BUF(J1)(IND(1,2):IND(2,LIND))
         END IF
 410  CONTINUE 
 810  CONTINUE 
      DATA_OFFS = LEN(SPHE__LABEL) + 1 + MALO__LOA_LTXT*MALO__LOA_LSTR 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LOA_INQ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LOA_READ ( FILIN, NLON, NLAT, DATA_OFFS, DSP_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LOA_READ 
! *                                                                      *
! *  ### 21-NOV-2012    LOA_READ   v1.0 (c)  L. Petrov  21-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER  FILIN*(*)
      INTEGER*4  NLON, NLAT, DATA_OFFS, IUER
      REAL*4     DSP_ARR(NLON,NLAT,3)
      INTEGER*8  IS
      CHARACTER  STR*256
      INTEGER*4  LUN, J1, J2, J3, SEEK_SET, OFFSET_RET, ARG_LN, IER
      INTEGER*8, EXTERNAL :: READ, LSEEK
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, INDEX_I1
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6191, IUER, 'LOA_READ', 'Failure in an attempt '// &
     &         'to open input file '//FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(DATA_OFFS), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. DATA_OFFS ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6192, IUER, 'LOA_READ', 'Failure in an attempt '// &
     &         'to seek for beginning the data section in the input file '// &
     &          FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), DSP_ARR, %VAL(4*3*NLON*NLAT) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6193, IUER, 'LOA_READ', 'Failure in an attempt '// &
     &         'to read a data record from the intput file '// &
     &          FILIN(1:I_LEN(FILIN))//' -- '//STR )
           IER = 0
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
        ELSE IF ( IS < 4*3*NLON*NLAT ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6194, IUER, 'LOA_READ', 'Failure in an attempt '// &
     &         'to read a data record from the input file '// &
     &          FILIN(1:I_LEN(FILIN))//' -- not all the data '// &
     &          'have been read' )
           IER = 0
           WRITE ( 6, * ) ' NLON,NLAT= ', NLON, NLAT
           WRITE ( 6, * ) ' IS = ', IS, ' DATA_OFFS= ', DATA_OFFS
           CALL BINF_CLOSE ( LUN, IER )
           RETURN 
      END IF
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LOA_READ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_LOADING_NC ( NLON, NLAT, L_FRQ, IND_FRQ, &
     &                              MJD, TAI, DSP_ARR3, DSP_ARR, &
     &                              PRGNAM, FILREF, &
     &                              FILDSC, FILCOM, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine WRITE_LOADING_NC 
! *                                                                      *
! * ### 27-DEC-2012  WRITE_LOADING_NC v3.2 (c) L. Petrov 16-FEB-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'netcdf.inc'
      INTEGER*4  NLON, NLAT, MJD, L_FRQ, IND_FRQ(L_FRQ), IUER
      REAL*8     TAI, DSP_ARR3(3,NLON,NLAT,2,L_FRQ)
      REAL*4     DSP_ARR(NLON,NLAT,3,2,L_FRQ)
      CHARACTER  PRGNAM*(*), FILREF*(*), FILDSC*(*), FILCOM*(*), FILOUT*(*)
      CHARACTER  STR*128, LOA_TXT(MALO__LOA_LTXT)*(MALO__LOA_LSTR)
      CHARACTER  SYSNAME*128, HOSTNAME*128, HARDWARE*128
      LOGICAL*1  LEX, FL3
      INTEGER*4  NCID,       DIM_VEC(5), &
     &           ID_DIM_LAT, ID_DIM_LON, ID_DIM_FRQ, ID_DIM_CMP, ID_DIM_VEC, &
     &           ID_DIM_WAV, ID_DIM_VCL, ID_DIM_CML, &
     &           ID_VAR_LAT, ID_VAR_LON, ID_VAR_FRQ, ID_VAR_CMP, ID_VAR_VEC, &
     &           ID_VAR_WAV, ID_VAR_LOA, ID_VAR_PHS, NVEC, NCMP
      PARAMETER  ( NCMP = 2 )
      PARAMETER  ( NVEC = 3 )
      CHARACTER  C_WAV(MALO__MWAV)*4, C_CMP(NCMP)*3, C_VEC(NVEC)*5
      DATA       C_CMP / 'cos', 'sin' /
      DATA       C_VEC / 'Up   ', 'East ', 'North' /
      REAL*8     PHS_ARR_R8(MALO__MWAV), FRQ_ARR_R8(MALO__MWAV)
      INTEGER*2  RANGE_I2(2)
      CHARACTER, POINTER :: BUF(:)*1 => NULL()
      INTEGER*2, ALLOCATABLE :: DSP_I2(:,:,:,:,:)
      REAL*4     MALO_SCALE
      REAL*4,    ALLOCATABLE :: LAT(:), LON(:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           IS, LUN, LBUF, N_DIMS, L_CMP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, UNLINK, WRITE, ADD_CLIST, LTM_DIF
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
!      IF ( INT8(L_FRQ)*INT8(NCMP)*INT8(NVEC)*INT8(NLAT)*INT8(NLON) .GE. &
!     &     INT8(4)*INT8(1024)*INT8(1024)*INT8(1024)                     ) THEN
!           CALL ERR_LOG ( 6710, IUER, 'WRITE_LOADING_NC', 'Sorry, '// &
!     &         'your dataset exceeds 4Gib limit imposed on netcdf '// &
!     &        'library. You cannot write your file' )
!           RETURN 
!      END IF
!
      MALO_SCALE = 0.000020
!
      IF ( .NOT. ( L_FRQ == 1 .AND. IND_FRQ(1) == 0 ) ) THEN
           DO 410 J1=1,L_FRQ
              C_WAV(J1) = OTID_WAV(IND_FRQ(J1))
              PHS_ARR_R8(J1) = OTID_PHS(IND_FRQ(J1))
              FRQ_ARR_R8(J1) = OTID_FRQ(IND_FRQ(J1))
 410       CONTINUE 
      END IF
!
      CALL GETINFO_SYSTEM ( SYSNAME, HOSTNAME, HARDWARE )
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( LEX ) THEN
           IS = UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) ) 
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6711, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              'an attempt to remove the stale the output file '// &
     &              FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
               RETURN
           END IF
      END IF
!
! --- Open the new output file in netcdf format WRITE_LOADING_NC
!
      IS = NF_CREATE ( FILOUT, NF_NETCDF4, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6712, IUER, 'WRITE_LOADING_NC', 'Error in an '// &
     &         'attempt to create the output netcf file '// &
     &         FILOUT(1:I_LEN(FILOUT))//' NF_CREATE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Declare three dimensions: lon, lat, vec, cmp, frq
!
      IS = NF_DEF_DIM ( NCID, 'lon', NLON, ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6713, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new dimension lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'lat', NLAT, ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6714, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'vec', NVEC, ID_DIM_VEC )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6715, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'vec_len', LEN(C_VEC(1)), ID_DIM_VCL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6716, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new dimension vcl: '// &
     &     NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( .NOT. ( L_FRQ == 1 .AND. IND_FRQ(1) == 0 ) ) THEN
           IS = NF_DEF_DIM ( NCID, 'cmp', NCMP, ID_DIM_CMP )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6717, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new dimension cmp: '// &
     &               NF_STRERROR(IS) )
               RETURN
           END IF
!
           IS = NF_DEF_DIM ( NCID, 'frq', L_FRQ, ID_DIM_FRQ )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6718, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new dimension frq: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_DEF_DIM ( NCID, 'wave_len', LEN(C_WAV(1)), ID_DIM_WAV )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6719, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new dimension wave: '// &
     &          NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_DEF_DIM ( NCID, 'cmp_len', LEN(C_CMP(1)), ID_DIM_CML )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6720, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new dimension cml: '// &
     &               NF_STRERROR(IS) )
               RETURN
           END IF
      END IF
!
      DIM_VEC(1) = ID_DIM_LON 
      DIM_VEC(2) = ID_DIM_LAT
      DIM_VEC(3) = ID_DIM_VEC
      DIM_VEC(4) = ID_DIM_CMP
      DIM_VEC(5) = ID_DIM_FRQ
!
      IS = NF_DEF_VAR ( NCID, 'lon', NF_REAL, 1, DIM_VEC(1), ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6721, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new variable lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'lat', NF_REAL, 1, DIM_VEC(2), ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6722, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new variable lat: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      DIM_VEC(1) = ID_DIM_VCL
      DIM_VEC(2) = ID_DIM_VEC
      IS = NF_DEF_VAR ( NCID, 'vec', NF_CHAR, 2, DIM_VEC, ID_VAR_VEC )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6723, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &          ' an attempt to create new variable vec: '// &
     &           NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( .NOT. ( L_FRQ == 1 .AND. IND_FRQ(1) == 0 ) ) THEN
           DIM_VEC(1) = ID_DIM_CML
           DIM_VEC(2) = ID_DIM_CMP
           IS = NF_DEF_VAR ( NCID, 'cmp', NF_CHAR, 2, DIM_VEC, ID_VAR_CMP )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6724, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new variable cmp: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DIM_VEC(1) = ID_DIM_LON 
           DIM_VEC(2) = ID_DIM_LAT
           DIM_VEC(3) = ID_DIM_VEC
           DIM_VEC(4) = ID_DIM_CMP
           DIM_VEC(5) = ID_DIM_FRQ
!
           IS = NF_DEF_VAR ( NCID, 'frq', NF_DOUBLE, 1, DIM_VEC(5), ID_VAR_FRQ )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6725, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new variable frq: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_DEF_VAR ( NCID, 'phs', NF_DOUBLE, 1, DIM_VEC(5), ID_VAR_PHS )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6726, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new variable frq: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
!
           DIM_VEC(1) = ID_DIM_WAV
           DIM_VEC(2) = ID_DIM_FRQ
           IS = NF_DEF_VAR ( NCID, 'wave', NF_CHAR, 2, DIM_VEC, ID_VAR_WAV )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6727, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new variable wave: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
!
           DIM_VEC(1) = ID_DIM_LON 
           DIM_VEC(2) = ID_DIM_LAT
           DIM_VEC(3) = ID_DIM_VEC
           DIM_VEC(4) = ID_DIM_CMP
           DIM_VEC(5) = ID_DIM_FRQ
           N_DIMS = 5
         ELSE 
           DIM_VEC(1) = ID_DIM_LON 
           DIM_VEC(2) = ID_DIM_LAT
           DIM_VEC(3) = ID_DIM_VEC
           N_DIMS = 3
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'dspl', NF_INT2, N_DIMS, DIM_VEC, ID_VAR_LOA )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6728, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new variable dspl: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! === Latitude axis
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'long_name', &
     &                       LEN('latitude'), 'latitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6729, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'standard_name', &
     &                       LEN('latitude'), 'latitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6730, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'units', &
     &                       LEN('degrees_north'), 'degrees_north' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6731, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'axis', &
     &                       LEN('Y'), 'Y' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6732, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'description', &
     &                       LEN('Geodetic latitude'), 'Geodetic latitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6733, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! === Longitude axis
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LON, 'long_name', &
     &                       LEN('longitude'), 'longitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6734, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LON, 'standard_name', &
     &                       LEN('longitude'), 'longitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6735, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LON, 'units', &
     &                       LEN('degrees_east'), 'degrees_east' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6736, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LON, 'axis', &
     &                       LEN('Y'), 'X' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6737, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! === Vector component
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_VEC, 'standard_name', &
     &                       LEN('Dsplacement_Vector_Component'), &
     &                           'Dsplacement_Vector_Component'   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6738, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_VEC, 'units', &
     &                       LEN('dimensionless'), 'dimensionless' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6739, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &           NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_VEC, 'axis', LEN('C'), 'C' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6740, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_VEC, 'description', &
     &                       LEN('Component of vector displacement: 1 for Up towards local normal to the reference ellipsoid, 2 for East, and 3 for North'), &
     &                           'Component of vector displacement: 1 for Up towards local normal to the reference ellipsoid, 2 for East, and 3 for North'   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6741, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &           NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_VEC, 'axis', &
     &                       LEN('V'), 'V' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6742, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      IF ( .NOT. ( L_FRQ == 1 .AND. IND_FRQ(1) == 0 ) ) THEN
!
! ======== Component axis
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_CMP, 'standard_name', &
     &                            LEN('Harmomic component'), 'Harmonic component' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6743, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_CMP, 'units', &
     &                            LEN('dimensionless'), 'dimensionless' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6744, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_CMP, 'axis', LEN('H'), 'H' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6745, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_CMP, 'description', &
     &                       LEN('Component of harmonic variation: 1 for cosine, 2 for sine'), &
     &                           'Component of harmonic variation: 1 for cosine, 2 for sine'   )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6746, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
               RETURN
           END IF
!
! ======== Frequency axis
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_FRQ, 'standard_name', &
     &                            LEN('frequency'), 'Frequency' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6747, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_FRQ, 'units', &
     &                            LEN('rad/s'), 'rad/s' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6748, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_FRQ, 'axis', &
     &                            LEN('F'), 'F' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6749, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_FRQ, 'description', &
     &                            LEN('Frequency of the harmonic variation'), &
     &                                'Frequency of the harmonic variation' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6750, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
! ======== Phase variable
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_PHS, 'standard_name', &
     &                            LEN('Phase'), 'Phase' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6751, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_PHS, 'units', &
     &                            LEN('rad'), 'rad' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6752, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_PHS, 'description', &
     &                            LEN('Phase of the harmonic variation since epoch 2000.01.01_12:00:00 TAI'), &
     &                                'Phase of the harmonic variation since epoch 2000.01.01_12:00:00 TAI' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6753, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
! ======== Wave variable
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_WAV, 'standard_name', &
     &                            LEN('Wave_name'), 'Wave_name' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6754, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_WAV, 'units', &
     &                            LEN('n/a'), 'n/a' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6755, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_WAV, 'description', &
     &                            LEN('Historical name of the tidal wave in Darwin notation'), &
     &                                'Historical name of the tidal wave in Darwin notation'   )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6756, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
      END IF
!
! === Displacement variable
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LOA, 'long_name', &
     &                       LEN('Loading displacement vector'), &
     &                           'Loading displacement vector'   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6757, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LOA, 'units', &
     &                       LEN('meter'), 'meter' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6758, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LOA, 'add_offset', &
     &                    NF_REAL, 1, 0.0 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6759, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LOA, 'scale_factor', &
     &                       NF_REAL, 1, MALO_SCALE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6760, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_INT2 ( NCID, ID_VAR_LOA, '_FillValue', &
     &                       NF_INT2, 1, -32768 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6761, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      RANGE_I2(1) = -32000
      RANGE_I2(2) =  32000
!
      IS = NF_PUT_ATT_INT2 ( NCID, ID_VAR_LOA, 'valid_range', &
     &                       NF_INT2, 2, RANGE_I2 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6762, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'Conventions', &
     &                       LEN('CF-1.6'), 'CF-1.6' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6763, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'title', &
     &                       LEN('Crustal displacements caused by loading'), &
     &                           'Crustal displacements caused by loading'   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6764, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( HOSTNAME == 'astrogeo' .OR. HOSTNAME == 'earthrotation' .OR. &
     &     HOSTNAME == 'pethome' ) THEN
!
           IS = NF_PUT_ATT_TEXT ( NCID, 0, 'institution', &
     &                            LEN('Astrogeo Center'), 'Astrogeo Center'   )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6765, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
         ELSE
           IS = NF_PUT_ATT_TEXT ( NCID, 0, 'institution', &
     &                            LEN('NASA Goddard Space Flight Center'), 'NASA Goddard Space Flight Center'   )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6765, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &              NF_STRERROR(IS) )
                RETURN
           END IF
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'source', &
     &                       LEN('Computed by program '//PRGNAM(1:I_LEN(PRGNAM))), &
     &                           'Computed by program '//PRGNAM(1:I_LEN(PRGNAM))   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6766, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &       ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_INT ( NCID, 0, 'MJD', NF_INT, 1, MJD )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6767, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &       'an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_DOUBLE ( NCID, 0, 'TAI', NF_DOUBLE, 1, TAI )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6768, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &       'an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      STR = GET_CDATE()
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'history', &
     &                       LEN(STR(1:19)//' generated'), &
     &                           STR(1:19)//' generated'   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6769, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( ILEN(FILREF) == 0 .OR. FILREF(1:2) == 'NO' .OR. &
     &     FILREF(1:2) == 'no' ) THEN
!
           IS = NF_PUT_ATT_TEXT ( NCID, 0, 'reference', &
     &                            LEN('n/a'), 'n/a' )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6770, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
      END IF
!
      IF ( ILEN(FILDSC) > 0 .AND. FILDSC(1:2) .NE. 'NO' .AND. &
     &     FILDSC(1:2) .NE. 'no' ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL GET_FILE_BUFFER ( FILDSC, BUF, LBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6771, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              'in an attempt to read input file with loading '// &
     &              'description '//FILDSC )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, 0, 'source', SIZEOF(BUF), BUF )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6772, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to create new attribute: '// &
     &                NF_STRERROR(IS) )
                RETURN
           END IF
      END IF
!
      IF ( ILEN(FILCOM) > 0 .AND. FILCOM(1:2) .NE. 'NO' .AND. &
     &     FILCOM(1:2) .NE. 'no' ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL GET_FILE_BUFFER ( FILCOM, BUF, LBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6773, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' NF_ENDDEF for output file '//FILOUT(1:I_LEN(FILOUT))// &
     &              ' error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_ATT_TEXT ( NCID, 0, 'comment', SIZEOF(BUF), BUF )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6774, IUER, 'WRITE_LOADING_NC', 'Error in '// &
          &         ' an attempt to create new attribute: '// &
          &           NF_STRERROR(IS) )
                RETURN
           END IF
      END IF
!
! === End of variable defintions!
!
      IS = NF_ENDDEF ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6775, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' NF_ENDDEF for output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      ALLOCATE ( LAT(NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*NLAT, STR )
           CALL ERR_LOG ( 6776, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         ' dynamic memory' )
           RETURN
      END IF
      DO 430 J3=1,NLAT
         LAT(J3) = -90.0 + 180.0*(J3-1)/FLOAT(NLAT-1)
 430  CONTINUE 
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LAT, LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6777, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to write varabile LAT into the '// &
     &         'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
      DEALLOCATE ( LAT )
!
      ALLOCATE ( LON(NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*NLON, STR )
           CALL ERR_LOG ( 6778, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         ' dynamic memory' )
           RETURN
      END IF
      DO 440 J4=1,NLON
         LON(J4) = 360.0*(J4-1)/FLOAT(NLON)
 440  CONTINUE 
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LON, LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6779, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to write varabile LON into the '// &
     &         'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
      DEALLOCATE ( LON )
!
      IS = NF_PUT_VAR_TEXT ( NCID, ID_VAR_VEC, C_VEC )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6780, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to write varabile C_VEC into the '// &
     &         'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( .NOT. ( L_FRQ == 1 .AND. IND_FRQ(1) == 0 ) ) THEN
           IS = NF_PUT_VAR_TEXT ( NCID, ID_VAR_CMP, C_CMP )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6781, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to write varabile OCMP_I2 into the '// &
     &              'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &              ' -- error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           DO 450 J5=1,L_FRQ
              PHS_ARR_R8(J5) = OTID_PHS(IND_FRQ(J5))
              FRQ_ARR_R8(J5) = OTID_FRQ(IND_FRQ(J5))
 450       CONTINUE 
           IS = NF_PUT_VAR_DOUBLE ( NCID, ID_VAR_FRQ, FRQ_ARR_R8 )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6782, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to write varabile FRQ into the '// &
     &              'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &              ' -- error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_VAR_DOUBLE ( NCID, ID_VAR_PHS, PHS_ARR_R8 )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6783, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to write varabile PHS into the '// &
     &              'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &              ' -- error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_PUT_VAR_TEXT ( NCID, ID_VAR_WAV, C_WAV )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6784, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to write varabile WAV  into the '// &
     &              'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &              ' -- error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           ALLOCATE ( DSP_I2(NLON,NLAT,3,2,L_FRQ), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( 2*3*NLAT*NLON*L_FRQ*2, STR )
                CALL ERR_LOG ( 6785, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &              ' dynamic memory' )
                RETURN
           END IF
           L_CMP = 2
         ELSE 
           ALLOCATE ( DSP_I2(NLON,NLAT,3,1,1), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( 2*3*NLAT*NLON*L_FRQ*2, STR )
                CALL ERR_LOG ( 6786, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &              ' an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &              ' dynamic memory' )
                RETURN
           END IF
           L_CMP = 1
      END IF
      DSP_I2 = 0
!
      IF ( LOC(DSP_ARR3) == 0 ) THEN
           FL3 = .FALSE.
         ELSE
           FL3 = .TRUE.
      END IF
      DO 460 J6=1,L_FRQ
         DO 470 J7=1,NCMP
            IF ( J7 == 2  .AND.  L_FRQ == 1 .AND.  IND_FRQ(1) == 0 ) GOTO 470
            DO 480 J8=1,NVEC
                DO 490 J9=1,NLAT
                   DO 4100 J10=1,NLON
                      IF ( FL3 ) THEN
                           DSP_I2(J10,J9,J8,J7,J6) = DSP_ARR3(J8,J10,J9,J7,J6)/MALO_SCALE
                         ELSE
                           DSP_I2(J10,J9,J8,J7,J6) = DSP_ARR(J10,J9,J8,J7,J6)/MALO_SCALE
                      END IF
 4100              CONTINUE 
 490            CONTINUE 
 480        CONTINUE 
 470     CONTINUE 
 460  CONTINUE 
!
      IS = NF_PUT_VAR_INT2 ( NCID, ID_VAR_LOA, DSP_I2 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6787, IUER, 'WRITE_LOADING_NC', 'Error in '// &
     &         ' an attempt to write varabile DSPL into the '// &
     &         'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
      DEALLOCATE ( DSP_I2 )
!
! --- Uph! Close file and go home to drink hot tea (or cold bear)
!
      IS = NF_CLOSE ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6788, IUER, 'WRITE_LOADING_NC', 'Error in an '// &
     &         'attempt to close the output netcf file '// &
     &         FILOUT(1:I_LEN(FILOUT))//' NF_CLOSE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  WRITE_LOADING_NC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_FILE_BUFFER ( FILIN, BUF, LBUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_FILE_BUFFER
! *                                                                      *
! * ## 28-DEC-2012  GET_FILE_BUFFER v1.0 (c)  L. Petrov 28-DEC-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILIN*(*)
      CHARACTER, POINTER :: BUF(:)*(*)
      INTEGER*4  LBUF, IUER
      LOGICAL*1  FL_N
      CHARACTER  STR*128
      INTEGER*8  SIZE_I8
      INTEGER*4  IS, J1, UNIX_DATE, LIN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ, FILE_INFO
!
! --- Learn the file size
!
      IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6321, IUER, 'GET_FILE_BUFFER', 'Error '// &
     &         'an attempt to open to collect information about '// &
     &         'input file '//FILIN )
           RETURN 
      END IF
!
! --- Open input file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LIN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6322, IUER, 'GET_FILE_BUFFER', 'Error '// &
     &                    'an attempt to open for reading '// &
     &                    'input file '//FILIN )
           RETURN 
      END IF
!
! --- Allocate memory for a buffer
!
      IF ( ASSOCIATED ( BUF ) ) DEALLOCATE ( BUF )
      ALLOCATE ( BUF(SIZE_I8), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZE_I8, STR )
           CALL ERR_LOG ( 6323, IUER, 'GET_FILE_BUFFER', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for a temporary buffer' )
           RETURN 
      END IF
!
! --- Read the input file
!
      IS = READ ( %VAL(LIN), BUF, %VAL(SIZE_I8) )
      IF ( IS == -1 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6324, IUER, 'GET_FILE_BUFFER', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading the input file '//FILIN )
           DEALLOCATE ( BUF )
           RETURN
         ELSE IF ( IS .NE. SIZE_I8 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6325, IUER, 'GET_FILE_BUFFER', 'Not all '// &
     &         'the data have been read from the input file '//FILIN )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Close input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LIN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6326, IUER, 'GET_FILE_BUFFER', 'Error in an '// &
     &         'attempt to close input file '//FILIN )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      LBUF = 0
      DO 410 J1=1,SIZE_I8
         IF ( BUF(J1) == CHAR(10) ) LBUF = LBUF + 1
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_FILE_BUFFER  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_LOADING_NC ( FILIN, MEL, NLON, NLAT, IVEC, ICMP, &
     &                             IFRQ, MJD, TAI, DSP_ARR, WAV_NAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine READ_LOADING_NC reads the loading displacement in       *
! *   NetCDF format. The file compressed with bzip2 is supported.        *
! *   In that case the file must have extension bz2.                     *
! *                                                                      *
! *   When used at computeers terra or astrogeo, directory /f0/temp      *
! *   must exist.                                                        *
! *                                                                      *
! *   The 2D displacement fields is determined by                        *
! *   a) IVEC -- its Up-East-North component:                            *
! *              1 -- Up,                                                *
! *              2 -- East,                                              *
! *              3 -- North,                                             *
! *   2) ICMP -- its cos/sin harmonics comonent:                         *
! *              1 -- cosine                                             *
! *              2 -- sine                                               *
! *              3 -- amplitde                                           *
! *              4 -- phase                                              *
! *   3) IFRQ -- its frequency component.                                *
! *                                                                      *
! *   Displacement field may be in a form of 3D array for the specific   *
! *   epoch or in a form of 5D array of harminic variations of loading   *
! *   displacements. READ_LOADING_NC supports both cases. For the case   *
! *   of a 3D array parameters ICMP, IFRQ are igonred.                   *
! *                                                                      *
! * ### 27-DEC-2012  READ_LOADING_NC  v4.4 (c) L. Petrov 13-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'netcdf.inc'
      INTEGER*8  MEL
      INTEGER*4  NLON, NLAT, IVEC, ICMP, IFRQ, MJD, IUER
      CHARACTER  WAV_NAM*(*)
      REAL*8     TAI
      REAL*4     DSP_ARR(MEL)
      CHARACTER  FILIN*(*)
      INTEGER*4  M_WAV
      PARAMETER  ( M_WAV = 256 )
      CHARACTER  STR*128, STR1*128, C_WAV(M_WAV)*4, FILTMP*128, COM*512, &
     &           INTERNET_HOSTNAME*64, SYSNAME*128, HARDWARE*128, TMP_DIR*128
      INTEGER*4  NCID,       DIM_VEC(2), &
     &           ID_DIM_LAT, ID_DIM_LON, ID_DIM_VEC, ID_DIM_CMP, &
     &           ID_VAR_LAT, ID_VAR_LON, ID_VAR_LOA_UP, ID_VAR_LOA_EAST, &
     &           ID_VAR_LOA_NORTH, ID_VAR_LOA, ID_VAR_CMP, ID_VAR_WAV, &
     &           ID_VARS(3), ID_DIM_FRQ, NCMP, NFRQ, NVEC, NDSP
      REAL*4     ADD_OFFSET, SCALE_FACTOR
      INTEGER*2, ALLOCATABLE :: DSP_I2(:,:,:,:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IS, IS1, IS2, IS3, &
     &           LUN, LBUF, PID, NTHR, IER
      INTEGER*8  KEL
      LOGICAL*1  FL_BZIP2
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      REAL*4,    EXTERNAL :: ATAN_CS_R4
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, SYSTEM, OMP_GET_THREAD_NUM
!
      IF ( INDEX ( FILIN, '.nc.bz2' ) > 1 ) THEN
           CALL GETINFO_HOST ( INTERNET_HOSTNAME )
           IF ( INTERNET_HOSTNAME == 'localhost' ) THEN
                CALL GETINFO_SYSTEM ( SYSNAME, INTERNET_HOSTNAME, HARDWARE )
           END IF
           PID = GETPID()
           CALL INCH ( PID, FILTMP(1:8) )
           CALL CHASHR    ( FILTMP(1:8) )
           CALL BLANK_TO_ZERO ( FILTMP(1:8) )
           IF ( OMP_IN_PARALLEL() ) THEN
                FILTMP(9:9) = '_'
                CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
                CALL CHASHR    ( FILTMP(10:13) )
                CALL BLANK_TO_ZERO ( FILTMP(10:13) )
           END IF 
!          
           IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &          INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &          INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &          INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &          INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'        ) THEN
                TMP_DIR = '/dev/shm'
             ELSE IF ( INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a' ) THEN
                TMP_DIR = '/imls/oper_temp'
             ELSE 
                TMP_DIR = '/tmp'
           END IF
           FILTMP = TRIM(TMP_DIR)//'/'//FILTMP(1:I_LEN(FILTMP))//'.heb'
!
! -------- Honor environemnet variable OMP_NUM_THREADS.
! -------- We limit the number of threads for lbzip2
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL CHIN ( STR, NTHR )
                IF ( NTHR < 1 ) NTHR = 1
                CALL CLRCH ( STR ) 
                CALL INCH  ( NTHR, STR )
                STR = '-n '//STR
              ELSE
!
! ------------- ... or do not use any limit when the variable is not set up
!
                CALL CLRCH ( STR )
           END IF
           FL_BZIP2 = .TRUE.
           IF ( OMP_IN_PARALLEL() ) THEN
                COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
              ELSE 
                COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
           END IF
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6241, IUER, 'READ_HEB', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN 
            END IF
         ELSE
           FL_BZIP2 = .FALSE.
           FILTMP = FILIN
      END IF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILTMP, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6611, IUER, 'READ_LOADING_NC', 'Error in an '// &
     &         'attempt to open the netcf file with land/sea mask '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CREATE: '//NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Learn the ID of dimension: 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6612, IUER, 'READ_LOADING_NC', 'Error in '// &
     &         ' an attempt to read dimension lon: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'lat'
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6613, IUER, 'READ_LOADING_NC', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, NLON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6614, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &         'the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, NLAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6615, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &         'the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IF ( MEL < INT8(NLON)*INT8(NLAT) ) THEN
           WRITE ( 6, * ) 'NLON = ', NLON, ' NLAT = ', NLAT
           CALL CLRCH   ( STR )
           CALL INCH8   ( MEL,                   STR )
           CALL INCH8   ( INT8(NLON)*INT8(NLAT), STR1 )
           CALL ERR_LOG ( 6616, IUER, 'READ_LOADING_NC', 'Parameter MEL '// &
     &         'is too small: '//STR(1:I_LEN(STR))//' while at least '// &
     &          STR1(1:I_LEN(STR1))//' is needed'  )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "dspl_up"
!
      IS1 = NF_INQ_VARID ( NCID, 'dspl_up', ID_VAR_LOA_UP )
      IS2 = NF_INQ_VARID ( NCID, 'dspl',    ID_VAR_LOA )
      IS3 = NF_INQ_DIMID ( NCID, 'vec', ID_DIM_VEC )
      IF ( IS1 .EQ. 0 ) THEN
           NVEC = 3
           NCMP = 1
!
! -------- Learn the ID of the variable "dspl_east"
!
           IS = NF_INQ_VARID ( NCID, 'dspl_east', ID_VAR_LOA_EAST )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6617, IUER, 'READ_LOADING_NC', 'Variable "dspl_east" '// &
     &              'was not found in the input netcdf file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Learn the ID of the variable "dspl_north"
!
           IS = NF_INQ_VARID ( NCID, 'dspl_north', ID_VAR_LOA_NORTH )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6618, IUER, 'READ_LOADING_NC', 'Variable "dspl_north" '// &
     &              'was not found in the input netcdf file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           ALLOCATE ( DSP_I2(NLON,NLAT,NVEC,1,1), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH8  ( INT8(2)*INT8(NLON)*INT8(NLAT), STR )
                CALL ERR_LOG ( 6619, IUER, 'READ_LOADING_NC', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA_UP, 'add_offset',   ADD_OFFSET   )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6620, IUER, 'READ_LOADING_NC', 'Failure '// &
     &              'in attempt to retrieve attribute add_offset for '// &
     &              'the "dspl_up" variable from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA_UP, 'scale_factor', SCALE_FACTOR )
           IF ( IS .NE. 0 ) THEN
                IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA_UP, 'scale', SCALE_FACTOR )
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6621, IUER, 'READ_LOADING_NC', 'Failure '// &
     &              'in attempt to retrieve attribute scale_factor for '// &
     &              'the "dspl_up" variable from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Get the variable: up displacement
!
           IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_LOA_UP, DSP_I2(1,1,1,1,1) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6622, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &              'the values of the variable "dspl_up" from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Get the variable: east displacement
!
           IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_LOA_EAST, DSP_I2(1,1,2,1,1) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6623, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &              'the values of the variable "dspl_east" from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Get the variable: up displacement
!
           IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_LOA_NORTH, DSP_I2(1,1,3,1,1) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6624, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &              'the values of the variable "dspl_north" from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           KEL = 0
           DO 410 J1=1,NLAT
              DO 420 J2=1,NLON
                 KEL = KEL + 1
                 DSP_ARR(KEL) = SCALE_FACTOR*DSP_I2(J2,J1,IVEC,1,1) 
 420          CONTINUE 
 410       CONTINUE 
           DEALLOCATE ( DSP_I2 )
           CALL CLRCH ( WAV_NAM )
         ELSE IF ( IS2 == 0 ) THEN
           NVEC = 3
           ALLOCATE ( DSP_I2(NLON,NLAT,NVEC,1,1), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH8  ( INT8(2)*INT8(NLON)*INT8(NLAT)*INT8(NVEC), STR )
                CALL ERR_LOG ( 6625, IUER, 'READ_LOADING_NC', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA, 'add_offset',   ADD_OFFSET   )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6626, IUER, 'READ_LOADING_NC', 'Failure '// &
     &              'in attempt to retrieve attribute add_offset for '// &
     &              'the "dspl" variable from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA, 'scale_factor', SCALE_FACTOR )
           IF ( IS .NE. 0 ) THEN
                IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA, 'scale', SCALE_FACTOR )
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6627, IUER, 'READ_LOADING_NC', 'Failure '// &
     &              'in attempt to retrieve attribute scale_factor for '// &
     &              'the "dspl" variable from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Get the variable: up,east,north displacement
!
           IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_LOA, DSP_I2(1,1,1,1,1) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6628, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &              'the values of the variable "dspl" from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
           KEL = 0
           DO 430 J3=1,NVEC
              DO 440 J4=1,NLAT 
                 DO 450 J5=1,NLON
                    KEL = KEL + 1
                    DSP_ARR(KEL) = SCALE_FACTOR*DSP_I2(J5,J4,J3,1,1) 
 450             CONTINUE 
 440          CONTINUE 
 430       CONTINUE 
           DEALLOCATE ( DSP_I2 )
           CALL CLRCH ( WAV_NAM )
         ELSE IF ( IS3 == 0 ) THEN
!
! -------- Learn the ID of dimensions 'vec'
!
           IS = NF_INQ_DIMID ( NCID, 'vec', ID_DIM_VEC )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6629, IUER, 'READ_LOADING_NC', 'Error in '// &
     &              ' an attempt to read dimension vec: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Learn the ID of dimensions 'cmp'
!
           IS = NF_INQ_DIMID ( NCID, 'cmp', ID_DIM_CMP )
           IF ( IS .NE. 0 ) THEN
                ID_DIM_CMP = -1
                NCMP = 1
           END IF
!
! -------- Learn the ID of dimensions 'frq'
!
           IS = NF_INQ_DIMID ( NCID, 'frq', ID_DIM_FRQ )
           IF ( IS .NE. 0 ) THEN
                ID_DIM_FRQ = -1
                NFRQ = 1
           END IF
!
! -------- Get the length of the dimension "cmp"
!
           IF ( ID_DIM_CMP > 0 ) THEN
                IS = NF_INQ_DIMLEN ( NCID, ID_DIM_CMP, NCMP )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 6630, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &                   'the length of the dimension "cmp" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
                     IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                     RETURN
                END IF
           END IF
!
! -------- Get the length of the dimension "vec"
!
           IS = NF_INQ_DIMLEN ( NCID, ID_DIM_VEC, NVEC )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6631, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &              'the length of the dimension "vec" in file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Get the length of the dimension "frq"
!
           IF ( ID_DIM_FRQ > 0 ) THEN
                IS = NF_INQ_DIMLEN ( NCID, ID_DIM_FRQ, NFRQ )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 6632, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &                   'the length of the dimension "frq" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
                     IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                     RETURN
                END IF
           END IF
!
! -------- Learn the ID of the variable "dspl"
!
           IS = NF_INQ_VARID ( NCID, 'dspl', ID_VAR_LOA )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6633, IUER, 'READ_LOADING_NC', 'Variable "dspl" '// &
     &              'was not found in the input netcdf file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA, 'add_offset',   ADD_OFFSET   )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6634, IUER, 'READ_LOADING_NC', 'Failure '// &
     &              'in attempt to retrieve attribute add_offset for '// &
     &              'the "dspl" variable from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA, 'scale_factor', SCALE_FACTOR )
           IF ( IS .NE. 0 ) THEN
                IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LOA, 'scale', SCALE_FACTOR )
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6635, IUER, 'READ_LOADING_NC', 'Failure '// &
     &              'in attempt to retrieve attribute scale_factor for '// &
     &              'the "dspl" variable from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           ALLOCATE ( DSP_I2(NLON,NLAT,NVEC,NCMP,NFRQ), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH8  ( INT8(2)*INT8(NLON)*INT8(NLAT)*INT8(NVEC)*INT8(NCMP)*INT8(NFRQ), STR )
                CALL ERR_LOG ( 6636, IUER, 'READ_LOADING_NC', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
! -------- Get the variable: up displacement
!
           IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_LOA, DSP_I2 )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6637, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &              'the values of the variable "dspl" from the file '// &
     &               FILIN(1:I_LEN(FILIN))//' error: '// &
     &               NF_STRERROR(IS) )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN
           END IF
!
           KEL = 0
           DO 460 J6=1,NLAT
              DO 470 J7=1,NLON
                 KEL = KEL + 1
                 IF ( ICMP == 1 ) THEN
                      DSP_ARR(KEL) = SCALE_FACTOR*DSP_I2(J7,J6,IVEC,1,IFRQ) 
                    ELSE IF ( ICMP == 2 ) THEN
                      DSP_ARR(KEL) = SCALE_FACTOR*DSP_I2(J7,J6,IVEC,2,IFRQ) 
                    ELSE IF ( ICMP == 3 ) THEN
                      DSP_ARR(KEL) = SQRT( (SCALE_FACTOR*DSP_I2(J7,J6,IVEC,1,IFRQ))**2 + &
     &                                     (SCALE_FACTOR*DSP_I2(J7,J6,IVEC,2,IFRQ))**2   )
                    ELSE IF ( ICMP == 4 ) THEN
                      DSP_ARR(KEL) = ATAN_CS_R4 ( SCALE_FACTOR*DSP_I2(J7,J6,IVEC,1,IFRQ), &
     &                                            SCALE_FACTOR*DSP_I2(J7,J6,IVEC,2,IFRQ)  )
                 END IF
 470          CONTINUE 
 460       CONTINUE 
           DEALLOCATE ( DSP_I2 )
!
! -------- Learn the ID of the variable "wave"
!
           IS = NF_INQ_VARID ( NCID, 'wave', ID_VAR_WAV )
           IF ( IS > 0 ) THEN
!
! ------------- Get the variable: up displacement
!
                IS = NF_GET_VAR_TEXT ( NCID, ID_VAR_WAV, C_WAV )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 6638, IUER, 'READ_LOADING_NC', 'Error in getting '// &
     &                   'the values of the variable "wave" from the file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
                     IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                     RETURN
                END IF
                WAV_NAM = C_WAV(IFRQ)
             ELSE 
                CALL CLRCH ( C_WAV(1) )
                CALL CLRCH ( WAV_NAM )
           END IF
      END IF
!
      IS = NF_GET_ATT_INT ( NCID, 0, 'MJD', MJD )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6639, IUER, 'READ_LOADING_NC', 'Error in '// &
     &         'an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_DOUBLE ( NCID, 0, 'TAI', TAI )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6640, IUER, 'READ_LOADING_NC', 'Error in '// &
     &         'an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Uph! Close file and go home to drink hot tea (or cold bear)
!
      IS = NF_CLOSE ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6641, IUER, 'READ_LOADING_NC', 'Error in an '// &
     &         'attempt to close the input netcf file '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CLOSE: '//NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  READ_LOADING_NC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INQ_LOADING_NC ( FILIN, NLON, NLAT, MJD, TAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine INQ_LOADING_NC
! *                                                                      *
! * ### 11-MAR-2013  INQ_LOADING_NC  v1.4 (c) L. Petrov 13-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'netcdf.inc'
      INTEGER*4  NLON, NLAT, MJD, IUER
      REAL*8     TAI
      CHARACTER  FILIN*(*)
      CHARACTER  STR*128
      INTEGER*4  NCID,       DIM_VEC(2), &
     &           ID_DIM_LAT, ID_DIM_LON, &
     &           ID_VAR_LAT, ID_VAR_LON, &
     &           ID_VAR_LOA_UP, ID_VAR_LOA_EAST, ID_VAR_LOA_NORTH, &
     &           ID_VARS(3)
      LOGICAL*1  FL_BZIP2
      CHARACTER  FILTMP*128, INTERNET_HOSTNAME*128, SYSNAME*128, &
     &           HARDWARE*128, TMP_DIR*128, COM*512
      INTEGER*4  J1, J2, J3, PID, KEL, IS, LUN, LBUF, NTHR, IER
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, SYSTEM, OMP_GET_THREAD_NUM
!
      IF ( INDEX ( FILIN, '.nc.bz2' ) > 1 ) THEN
           CALL GETINFO_HOST ( INTERNET_HOSTNAME )
           IF ( INTERNET_HOSTNAME == 'localhost' ) THEN
                CALL GETINFO_SYSTEM ( SYSNAME, INTERNET_HOSTNAME, HARDWARE )
           END IF
           PID = GETPID()
           CALL INCH ( PID, FILTMP(1:8) )
           CALL CHASHR    ( FILTMP(1:8) )
           CALL BLANK_TO_ZERO ( FILTMP(1:8) )
           IF ( OMP_IN_PARALLEL() ) THEN
                FILTMP(9:9) = '_'
                CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
                CALL CHASHR    ( FILTMP(10:13) )
                CALL BLANK_TO_ZERO ( FILTMP(10:13) )
           END IF 
!          
           IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &          INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &          INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &          INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &          INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'        ) THEN
                TMP_DIR = '/dev/shm'
             ELSE IF ( INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a' ) THEN
                TMP_DIR = '/imls/oper_temp'
             ELSE 
                TMP_DIR = '/tmp'
           END IF
           FILTMP = TRIM(TMP_DIR)//'/'//FILTMP(1:I_LEN(FILTMP))//'.heb'
!
! -------- Honor environment variable OMP_NUM_THREADS.
! -------- We limit the number of threads for lbzip2
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL CHIN ( STR, NTHR )
                IF ( NTHR < 1 ) NTHR = 1
                CALL CLRCH ( STR ) 
                CALL INCH  ( NTHR, STR )
                STR = '-n '//STR
              ELSE
!
! ------------- ... or do not use any limit when the variable is not set up
!
                CALL CLRCH ( STR )
           END IF
           FL_BZIP2 = .TRUE.
           IF ( OMP_IN_PARALLEL() ) THEN
                COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
              ELSE 
                COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
           END IF
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6241, IUER, 'READ_HEB', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN 
            END IF
         ELSE
           FL_BZIP2 = .FALSE.
           FILTMP = FILIN
      END IF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILTMP, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6351, IUER, 'INQ_LOADING_INC', 'Error in an '// &
     &         'attempt to open the netcf file with loading displacements '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CREATE: '//NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Learn the ID of dimension: 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6352, IUER, 'INQ_LOADING_INC', 'Error in '// &
     &         ' an attempt to read dimension lon: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'lat'
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6353, IUER, 'INQ_LOADING_INC', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, NLON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6354, IUER, 'INQ_LOADING_INC', 'Error in getting '// &
     &         'the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, NLAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6355, IUER, 'INQ_LOADING_INC', 'Error in getting '// &
     &         'the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_INT ( NCID, 0, 'MJD', MJD )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6356, IUER, 'INQ_LOADING_INC', 'Error in '// &
     &       'an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_DOUBLE ( NCID, 0, 'TAI', TAI )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6357, IUER, 'INQ_LOADING_INC', 'Error in '// &
     &       'an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
! --- Close file 
!
      IS = NF_CLOSE ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6358, IUER, 'INQ_LOADING_INC', 'Error in an '// &
     &         'attempt to close the input netcf file '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CLOSE: '//NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  INQ_LOADING_NC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_LOADING_NC_INFO ( FILIN, TITLE, INSTITUTION, &
     &                                  MS, LS, SOURCE, HISTORY, REFERENCE, &
     &                                  MC, LC, COMMENT, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine READ_LOADING_NC_INFO reads auxilliary information from  *
! *   the loading displacement in NetCDF format. The file compressed     *
! *   with bzip2 is supported. In that case the file must have           *
! *   extension bz2.                                                     *
! *                                                                      *
! *   When used at computeers terra or astrogeo, directory /f0/temp      *
! *   must exist.                                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *                                                                      *
! * ## 06-JUN-2016 READ_LOADING_NC_INFO v1.3 (c) L. Petrov 13-JAN-2024 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'netcdf.inc'
      INTEGER*4  MS, LS, MC, LC, IUER 
      CHARACTER  FILIN*(*)
      CHARACTER  TITLE*(*), INSTITUTION*(*), HISTORY*(*), REFERENCE*(*), &
     &           SOURCE(MS)*(*), COMMENT(MC)*(*)
      CHARACTER  STR*128, FILTMP*128, COM*512, INTERNET_HOSTNAME*64, &
     &           SYSNAME*128, HARDWARE*128, TMP_DIR*128
      INTEGER*4  NCID,       DIM_VEC(2), &
     &           ID_DIM_LAT, ID_DIM_LON, ID_DIM_VEC, ID_DIM_CMP, &
     &           ID_VAR_LAT, ID_VAR_LON, ID_VAR_LOA_UP, ID_VAR_LOA_EAST, &
     &           ID_VAR_LOA_NORTH, ID_VAR_LOA, ID_VAR_CMP, ID_VAR_WAV, &
     &           ID_VARS(3), ID_DIM_FRQ, NCMP, NFRQ, NVEC, NDSP, NTHR
      REAL*4     ADD_OFFSET, SCALE_FACTOR
      INTEGER*4  MB
      PARAMETER  ( MB = 128*1024 )
      CHARACTER  BUF*(MB)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IS, IS1, IS2, IS3, &
     &           IC, XT, LT, LUN, LBUF, PID, IER
      INTEGER*8  KEL
      LOGICAL*1  FL_BZIP2
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      REAL*4,    EXTERNAL :: ATAN_CS_R4
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, SYSTEM, OMP_GET_THREAD_NUM
!
      IF ( INDEX ( FILIN, '.nc.bz2' ) > 1 ) THEN
           CALL GETINFO_HOST ( INTERNET_HOSTNAME )
           IF ( INTERNET_HOSTNAME == 'localhost' ) THEN
                CALL GETINFO_SYSTEM ( SYSNAME, INTERNET_HOSTNAME, HARDWARE )
           END IF
           PID = GETPID()
           CALL INCH ( PID, FILTMP(1:8) )
           CALL CHASHR    ( FILTMP(1:8) )
           CALL BLANK_TO_ZERO ( FILTMP(1:8) )
           IF ( OMP_IN_PARALLEL() ) THEN
                FILTMP(9:9) = '_'
                CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
                CALL CHASHR    ( FILTMP(10:13) )
                CALL BLANK_TO_ZERO ( FILTMP(10:13) )
           END IF 
!          
           IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &          INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &          INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &          INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &          INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'        ) THEN
                TMP_DIR = '/dev/shm'
             ELSE IF ( INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a' ) THEN
                TMP_DIR = '/imls/oper_temp'
             ELSE 
                TMP_DIR = '/tmp'
           END IF
           FILTMP = TRIM(TMP_DIR)//'/'//FILTMP(1:I_LEN(FILTMP))//'.heb'
!
! -------- Honor environemnet variable OMP_NUM_THREADS.
! -------- We limit the number of threads for lbzip2
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL CHIN ( STR, NTHR )
                IF ( NTHR < 1 ) NTHR = 1
                CALL CLRCH ( STR ) 
                CALL INCH  ( NTHR, STR )
                STR = '-n '//STR
              ELSE
!
! ------------- ... or do not use any limit when the variable is not set up
!
                CALL CLRCH ( STR )
           END IF
           FL_BZIP2 = .TRUE.
           IF ( OMP_IN_PARALLEL() ) THEN
                COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
              ELSE 
                COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
           END IF
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6811, IUER, 'READ_LOADING_NC_INFO', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN 
            END IF
         ELSE
           FL_BZIP2 = .FALSE.
           FILTMP = FILIN
      END IF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILTMP, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6812, IUER, 'READ_LOADING_NC_INFO', 'Error in an '// &
     &         'attempt to open the netcf file with land/sea mask '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CREATE: '//NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'title', TITLE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6813, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to read attribut title: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution', INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           INSTITUTION = 'NASA GSFC'
           IS = 0
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6814, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to read attribut institution: '//&
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'history', HISTORY )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6815, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to read attribut history: '//&
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'reference', REFERENCE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6816, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to read attribut reference: '//&
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_INQ_ATT ( NCID, 0, 'source', XT, LT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6817, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to inquire attribute source: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'source', BUF )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6818, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to read attribute source: '//&
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
      LS = 0
      IC = 1
      DO 410 J1=1,LT
         IF ( BUF(J1:J1) == CHAR(10) ) THEN
              LS = LS + 1
              CALL CLRCH  ( SOURCE(LS) )
              IF ( IC .LE. J1-1 ) THEN
                   SOURCE(LS) = BUF(IC:J1-1)
              END IF
              IC = J1 + 1
         END IF
 410  CONTINUE 
      IF ( BUF(XT:XT) .NE. CHAR(10) ) THEN
           LS = LS + 1
           CALL CLRCH  ( SOURCE(LS) )
           IF ( IC .LE. LT ) THEN
                SOURCE(LS) = BUF(IC:LT)
           END IF
      END IF
!
      IS = NF_INQ_ATT ( NCID, 0, 'comment', XT, LT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6819, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to inquire attribute comment: '// &
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'comment', BUF )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6820, IUER, 'READ_LOADING_NC_INFO', 'Error in '// &
     &         'an attempt to read attribute comment: '//&
     &          NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
      LC = 0
      IC = 1
      DO 420 J2=1,LT
         IF ( BUF(J2:J2) == CHAR(10) ) THEN
              LC = LC + 1
              CALL CLRCH  ( COMMENT(LC) )
              IF ( IC .LE. J2-1 ) THEN
                   COMMENT(LC) = BUF(IC:J2-1)
              END IF
              IC = J2 + 1
         END IF
 420  CONTINUE 
      IF ( BUF(XT:XT) .NE. CHAR(10) ) THEN
           LS = LS + 1
           CALL CLRCH  ( COMMENT(LS) )
           IF ( IC .LE. LT ) THEN
                COMMENT(LS) = BUF(IC:LT)
           END IF
      END IF
!
! --- Uph! Close file and go home to drink hot tea (or cold bear)
!
      IS = NF_CLOSE ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6821, IUER, 'READ_LOADING_NC_INFO', 'Error in an '// &
     &         'attempt to close the input netcf file '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CLOSE: '//NF_STRERROR(IS) )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN
      END IF
!
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  READ_LOADING_NC_INFO  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_HARPOS_WRITE ( L_STA, L_FRQ, IND_FRQ, NUM_CMP, &
     &                               DSPL_ARR, MALO, PRG_NAME, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_HARPOS_WRITE
! *                                                                      *
! * ### 19-JUL-2013 MALO_HARPOS_WRITE v2.0 (c) L. Petrov 07-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'harpos.i'
      TYPE      ( MALO__TYPE ) :: MALO
      TYPE ( HARPOS__A_RECORD ) ::  AREC
      TYPE ( HARPOS__H_RECORD ) ::  HREC
      TYPE ( HARPOS__S_RECORD ) ::  SREC
      TYPE ( HARPOS__D_RECORD ) ::  DREC
      INTEGER*4  L_STA, L_FRQ, IND_FRQ(L_FRQ), NUM_CMP(L_FRQ), IUER
      REAL*8     DSPL_ARR(3,L_STA,L_FRQ,2)
      CHARACTER  PRG_NAME*(*), FILOUT*(*)
      CHARACTER  BEG_COM_LINE*80, END_COM_LINE*80, &
     &           BEG_DSC_LINE*80, END_DSC_LINE*80, &
     &           BEG_FMT_LINE*80, END_FMT_LINE*80
      REAL*8     LAT_GCN, EPS
      PARAMETER  ( EPS = 1.D-9 )
!
      CHARACTER  STR*128, STR_DATE*32
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  SYSNAME*128, NODENAME*128, HARDWARE*128
      LOGICAL*4  LEX
      INTEGER*4  LUN, LUN_DSC, LUN_COM, LUN_FMT
      INTEGER*4  IOS, IER, IP, J1, J2, J3, J4, J5, J6, J7, J8, I_FRQ, I_CMP
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN
!
      BEG_DSC_LINE = '#============================ Beginning of description: ========================'
      END_DSC_LINE = '#============================ End of description: =============================='
      BEG_COM_LINE = '#============================ Beginning of comments: ==========================='
      END_COM_LINE = '#============================ End of comments: ================================='
      BEG_FMT_LINE = '#============================ Beginning of format description: ================='
      END_FMT_LINE = '#============================ End of format description: ======================='
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 2521, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &         'in an attempt to open the output file '//FILOUT ) 
           RETURN
      END IF
!
! --- write the format label in the file
!
      WRITE ( LUN, '(A)' ) HARPOS__LABEL
      WRITE ( LUN, '(A)' ) '#'
!
! --- Get information about user name and system name
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- Write inforamtion about user and system
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Created by '//PRG_NAME
      WRITE ( UNIT=LUN, FMT='(A)' ) '#       run by '// &
     &        USER_REALNAME(1:I_LEN(USER_REALNAME))//' ( '// &
     &        USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#           on '// &
     &        NODENAME(1:I_LEN(NODENAME))//' at '//GET_CDATE()//' local time'
      WRITE ( LUN, '(A)' ) '#'
!
      IF ( MALO%CONF%LOA_FINAM_DESCR(1:2) .EQ. '  ' .OR. MALO%CONF%LOA_FINAM_DESCR(1:2) .EQ. 'NO' .OR. &
     &     MALO%CONF%LOA_FINAM_DESCR(1:2) .EQ. 'No' .OR. MALO%CONF%LOA_FINAM_DESCR(1:2) .EQ. 'no'      ) THEN
           CONTINUE
         ELSE
!
! -------- Well, we have to put the file with description in the output file
!
           INQUIRE ( FILE=MALO%CONF%LOA_FINAM_DESCR, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 2522, IUER, 'MALO_HARPOS_WRITE', 'File with '// &
     &              'comments '//MALO%CONF%LOA_FINAM_DESCR(1:I_LEN(MALO%CONF%LOA_FINAM_DESCR))//' has not been '// &
     &              'found' )
                RETURN
           END IF
!
           LUN_DSC = GET_UNIT ()
!
! -------- Open file with comments
!
           OPEN ( UNIT=LUN_DSC, FILE=MALO%CONF%LOA_FINAM_DESCR, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 2523, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'comments '//MALO%CONF%LOA_FINAM_DESCR )
                RETURN
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) BEG_DSC_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           DO 410 J1=1,1024*1024
!
! ----------- Read  a line
!
              READ ( UNIT=LUN_DSC, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) GOTO 810
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2524, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &                  'with description '//MALO%CONF%LOA_FINAM_DESCR )
                   RETURN
              END IF
!
! ----------- Write a line
!
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2525, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 410       CONTINUE
 810       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) END_DSC_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           CLOSE ( UNIT=LUN_DSC )
      END IF
!
      IF ( MALO%CONF%LOA_FINAM_COMM(1:2) .EQ. '  ' .OR. MALO%CONF%LOA_FINAM_COMM(1:2) .EQ. 'NO' .OR. &
     &     MALO%CONF%LOA_FINAM_COMM(1:2) .EQ. 'No' .OR. MALO%CONF%LOA_FINAM_COMM(1:2) .EQ. 'no'      ) THEN
           CONTINUE
         ELSE
!
! -------- Well, we have to put the file with comments in the output file
!
           INQUIRE ( FILE=MALO%CONF%LOA_FINAM_COMM, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 2526, IUER, 'MALO_HARPOS_WRITE', 'File with '// &
     &              'comments '//MALO%CONF%LOA_FINAM_COMM(1:I_LEN(MALO%CONF%LOA_FINAM_COMM))//' has not been '// &
     &              'found' )
                RETURN
           END IF
!
           LUN_COM = GET_UNIT ()
!
! -------- Open file with comments
!
           OPEN ( UNIT=LUN_COM, FILE=MALO%CONF%LOA_FINAM_COMM, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 2527, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'comments '//MALO%CONF%LOA_FINAM_COMM )
                RETURN
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) BEG_COM_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           DO 420 J2=1,1024*1024
!
! ----------- Read  a line
!
              READ ( UNIT=LUN_COM, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) GOTO 820
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2528, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &                  'with comments '//MALO%CONF%LOA_FINAM_COMM )
                   RETURN
              END IF
!
! ----------- Write a line
!
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2529, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 420       CONTINUE
 820       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) END_COM_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           CLOSE ( UNIT=LUN_COM )
      END IF
!
      IF ( MALO%CONF%HARPOS_FINAM_FMT(1:2) .EQ. '  '  .OR.  MALO%CONF%HARPOS_FINAM_FMT(1:2) .EQ. 'NO'  .OR. &
     &     MALO%CONF%HARPOS_FINAM_FMT(1:2) .EQ. 'No'  .OR.  MALO%CONF%HARPOS_FINAM_FMT(1:2) .EQ. 'no'       ) THEN
           CONTINUE
         ELSE
!
! -------- Well, we have to put file with format description in the output file
!
           INQUIRE ( FILE=MALO%CONF%HARPOS_FINAM_FMT, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 2530, IUER, 'MALO_HARPOS_WRITE', 'File with '// &
     &              'format description '//MALO%CONF%HARPOS_FINAM_FMT(1:I_LEN(MALO%CONF%HARPOS_FINAM_FMT))//' has '// &
     &              'not been found' )
                RETURN
           END IF
!
           LUN_FMT = GET_UNIT ()
           OPEN ( UNIT=LUN_FMT, FILE=MALO%CONF%HARPOS_FINAM_FMT, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 2531, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &              'format description '//MALO%CONF%HARPOS_FINAM_FMT )
                RETURN
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) BEG_FMT_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           DO 430 J3=1,1024*1024
!
! ----------- Read line
!
              READ ( UNIT=LUN_FMT, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) GOTO 830
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2532, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &                  'with format description '//MALO%CONF%HARPOS_FINAM_FMT )
                   RETURN
              END IF
!
! ----------- Write line
!
              WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 2533, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in writing in the output '// &
     &                 'file '//FILOUT )
                   RETURN
              END IF
 430       CONTINUE
 830       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) END_FMT_LINE
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           CLOSE ( UNIT=LUN_FMT )
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Radius of validity area (in meters)'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      CALL CLRCH ( STR )
      CALL LIB$MOVC3 ( LEN__A_REC, STR, AREC )
      AREC%REC_ID = 'A'
      WRITE ( UNIT=AREC%AREA_RD, FMT='(F14.6)', IOSTAT=IOS ) MALO__RD_AREA
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 2534, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &         'in an attempt to encode the A-record' )
           RETURN
      END  IF
      CALL LIB$MOVC3 ( LEN__A_REC, AREC, STR )
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:80)
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Harmonic   Phase          Frequency           Acceleration'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
! --- Cycle over the waves
!
      DO 440 J4=1,L_FRQ
!
! ------ Prepare the H-record of the ouput file file with harmonic definition
!
         CALL CLRCH ( STR )
         CALL LIB$MOVC3 ( LEN__H_REC, %REF(STR), HREC )
         HREC%REC_ID = 'H'
         HREC%WAVE_ID = OTID_WAV(IND_FRQ(J4))
         WRITE ( UNIT=HREC%PHASE, FMT='(1PD13.6)'  ) OTID_PHS(IND_FRQ(J4))
         WRITE ( UNIT=HREC%FREQ,  FMT='(1PD19.12)' ) OTID_FRQ(IND_FRQ(J4))
         WRITE ( UNIT=HREC%ACCEL, FMT='(1PD10.3)'  ) OTID_ACC(IND_FRQ(J4))
         CALL LIB$MOVC3 ( LEN__H_REC, HREC, %REF(STR) )
!
! ------ Write down the H-record
!
         WRITE ( LUN, '(A)' ) STR(1:LEN__H_REC)
 440  CONTINUE
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Site ID    X-coord.      Y-coord.      Z-coord.     phi-geoc. longit.  height'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      DO 450 J5=1,L_STA ! Cycle over all stations
!
! ------ Prepare S-record
!
         CALL CLRCH ( STR )
         CALL LIB$MOVC3 ( LEN__S_REC, %REF(STR), SREC )
!
         SREC%REC_ID  = 'S'
         SREC%SITE_ID = MALO%STA(J5)%NAME
         IF ( MALO%STA(J5)%LON .LT. 0.0D0 ) MALO%STA(J5)%LON = MALO%STA(J5)%LON + PI2
         LAT_GCN = DATAN ( MALO%STA(J5)%COO(3)/(DSQRT ( MALO%STA(J5)%COO(1)**2 + &
     &                                                  MALO%STA(J5)%COO(2)**2   ) + EPS ) )
         WRITE ( UNIT=SREC%X_COORD,   FMT='(F13.4)' ) MALO%STA(J5)%COO(1)
         WRITE ( UNIT=SREC%Y_COORD,   FMT='(F13.4)' ) MALO%STA(J5)%COO(2)
         WRITE ( UNIT=SREC%Z_COORD,   FMT='(F13.4)' ) MALO%STA(J5)%COO(3)
         WRITE ( UNIT=SREC%GEOC_LAT,  FMT='(F8.4)'  ) LAT_GCN/DEG__TO__RAD
         WRITE ( UNIT=SREC%LONGITUDE, FMT='(F8.4)'  ) MALO%STA(J5)%LON/DEG__TO__RAD
         WRITE ( UNIT=SREC%HEIGHT,    FMT='(F6.1)'  ) MALO%STA(J5)%HEI_ELL
!
! ------ Specia trick for pseudo-stations like geocenter
!
         IF ( ( MALO%STA(J5)%COO(1)**2 + MALO%STA(J5)%COO(2)**2 + &
     &          MALO%STA(J5)%COO(3)**3 ) .LT. 1.D4 ) THEN
              SREC%HEIGHT = '   0.0'
         END IF
!
         CALL LIB$MOVC3 ( LEN__S_REC, SREC, %REF(STR) )
!
! ------ Write S-record
!
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR(1:LEN__S_REC)
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 2535, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing in the output file '// &
     &             FILOUT )
              RETURN
         END IF
 450  CONTINUE
!
      DO 460 J6=1,L_STA ! Cycle over all stations
         IF ( MALO%STA(J6)%LON .GT. PI__NUM ) MALO%STA(J6)%LON = MALO%STA(J6)%LON - PI2
         WRITE ( LUN, '(A)' ) '#'
         WRITE ( LUN, '(A)' ) '#  Wave ID   Site ID     Up-cos  East-cos North-cos   Up-sin  East-sin North-sin'
         WRITE ( LUN, '(A)' ) '#'
!
         DO 470 J7=1,L_FRQ ! Cycle over all waves
!
! --------- Prepare D-record
!
            CALL CLRCH ( STR )
            CALL LIB$MOVC3 ( LEN__D_REC, %REF(STR), DREC )
!
            DREC%REC_ID  = 'D'
            DREC%WAVE_ID = OTID_WAV(IND_FRQ(J7))
            DREC%SITE_ID = MALO%STA(J6)%NAME
!
            IF ( NUM_CMP(J7) == 1 ) THEN
                 WRITE ( UNIT=DREC%UP_COS,    FMT='(F8.5)' ) DSPL_ARR(1,J6,J7,1)
                 WRITE ( UNIT=DREC%EAST_COS,  FMT='(F8.5)' ) DSPL_ARR(2,J6,J7,1)
                 WRITE ( UNIT=DREC%NORTH_COS, FMT='(F8.5)' ) DSPL_ARR(3,J6,J7,1)
!
                 WRITE ( UNIT=DREC%UP_SIN,    FMT='(F8.5)' ) 0.0D0
                 WRITE ( UNIT=DREC%EAST_SIN,  FMT='(F8.5)' ) 0.0D0
                 WRITE ( UNIT=DREC%NORTH_SIN, FMT='(F8.5)' ) 0.0D0
               ELSE IF ( NUM_CMP(J7) == 2 ) THEN
                 WRITE ( UNIT=DREC%UP_COS,    FMT='(F8.5)' ) DSPL_ARR(1,J6,J7,1)
                 WRITE ( UNIT=DREC%EAST_COS,  FMT='(F8.5)' ) DSPL_ARR(2,J6,J7,1)
                 WRITE ( UNIT=DREC%NORTH_COS, FMT='(F8.5)' ) DSPL_ARR(3,J6,J7,1)
!
                 WRITE ( UNIT=DREC%UP_SIN,    FMT='(F8.5)' ) DSPL_ARR(1,J6,J7,2)
                 WRITE ( UNIT=DREC%EAST_SIN,  FMT='(F8.5)' ) DSPL_ARR(2,J6,J7,2)
                 WRITE ( UNIT=DREC%NORTH_SIN, FMT='(F8.5)' ) DSPL_ARR(3,J6,J7,2)
            END IF
!
            CALL LIB$MOVC3 ( LEN__D_REC, DREC, %REF(STR) )
!
! --------- Write D-record
!
            WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR(1:LEN__D_REC)
            IF ( IOS .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IOS, STR )
                 CALL ERR_LOG ( 2536, IUER, 'MALO_HARPOS_WRITE', 'Error '// &
     &                STR(1:I_LEN(STR))//' in writing in the output file '// &
     &                FILOUT )
                 RETURN
            END IF
 470     CONTINUE
 460  CONTINUE
!
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) HARPOS__LABEL
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_HARPOS_WRITE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_LOADING_HEB ( NLON, NLAT, MJD, TAI, DSP_ARR, &
     &                               PRGNAM, FILREF, FILDSC, FILCOM, &
     &                               FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine WRITE_LOADING_HEB
! *                                                                      *
! * ### 25-MAR-2016 WRITE_LOADING_HEB v1.0 (c) L. Petrov 25-MAR-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_DSPL
      INTEGER*4  NLON, NLAT, MJD, IUER
      REAL*8     TAI, DSP_ARR(3,NLON,NLAT)
      CHARACTER  PRGNAM*(*), FILREF*(*), FILDSC*(*), FILCOM*(*), FILOUT*(*)
      CHARACTER  STR*128
      CHARACTER  SYSNAME*128, HOSTNAME*128, HARDWARE*128
      LOGICAL*1  LEX, FL3
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           IS, LUN, LBUF, N_DIMS, L_CMP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, UNLINK, WRITE, ADD_CLIST, LTM_DIF
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      CALL GETINFO_SYSTEM ( SYSNAME, HOSTNAME, HARDWARE )
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( LEX ) THEN
           IS = UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) ) 
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6781, IUER, 'WRITE_LOADING_HEB', 'Error in '// &
     &              'an attempt to remove the stale the output file '// &
     &              FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
                RETURN
           END IF
      END IF
!    
      HEB_DSPL%DIMS(1) = NLON
      HEB_DSPL%DIMS(2) = NLAT 
      HEB_DSPL%DIMS(3) = 3
      HEB_DSPL%DIMS(4) = 1
      ALLOCATE ( HEB_DSPL%VAL(HEB_DSPL%DIMS(1),HEB_DSPL%DIMS(2),HEB_DSPL%DIMS(3),HEB_DSPL%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*NLON*NLAT, STR )
           CALL ERR_LOG ( 6782, IUER, 'WRITE_LOADING_HEB', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         'bytes dynamic memory for temporary arary HEB_DSPL%VAL' )
           RETURN
      END IF
!
      HEB_DSPL%MIN_VALUE = DSP_ARR(1,1,1)
      HEB_DSPL%MAX_VALUE = DSP_ARR(1,1,1)
      DO 410 J1=1,NLAT
         DO 420 J2=1,NLON
            DO 430 J3=1,3
               HEB_DSPL%VAL(J2,J1,J3,1) = DSP_ARR(J3,J2,J1)
               IF ( HEB_DSPL%VAL(J2,J1,J3,1) < HEB_DSPL%MIN_VALUE ) THEN
                    HEB_DSPL%MIN_VALUE = HEB_DSPL%VAL(J2,J1,J3,1) 
               END IF
               IF ( HEB_DSPL%VAL(J2,J1,J3,1) > HEB_DSPL%MAX_VALUE ) THEN
                    HEB_DSPL%MAX_VALUE = HEB_DSPL%VAL(J2,J1,J3,1) 
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                call plot_grid_r4 ( 1, 7, 0, 1, nlon, nlat, HEB_DSPL%VAL(1,1,3,1), 'East loading', 'm', &
!     &                              '/tmp/foo', IER )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      HEB_DSPL%MJD            = MJD
      HEB_DSPL%UTC            = TAI 
      HEB_DSPL%TAI            = TAI 
      HEB_DSPL%DATA_OFFSET    = HEB__HDS
      HEB_DSPL%ENDIAN         = HEB__LE
      HEB_DSPL%DATA_TRANSFORM = HEB__SCOF
      HEB_DSPL%FILL_VALUE     = 1.0E15
      HEB_DSPL%OFFSET         = 0.0
      HEB_DSPL%SCALE_FACTOR   = 2.0E-5
      HEB_DSPL%DATA_FORMAT    = HEB__I2
!      HEB_DSPL%OFFSET         = 0.0
!      HEB_DSPL%SCALE_FACTOR   = 1.0
!      HEB_DSPL%DATA_FORMAT    = HEB__R4
!
      HEB_DSPL%DATA_COMPRESSION = HEB__NONE
      HEB_DSPL%SDS_NAME       = 'Site displacement caused by mass loading'
      HEB_DSPL%UNITS          = 'meter'
      HEB_DSPL%VALID_RANGE(1) =  -0.096D0
      HEB_DSPL%VALID_RANGE(2) =   0.096D0
      HEB_DSPL%PROD_DATE_TIME = GET_CDATE()
!
      HEB_DSPL%FILE_NAME      = FILOUT
      HEB_DSPL%HISTORY        = ' '
      HEB_DSPL%SOURCE         = ' '
      HEB_DSPL%TITLE          = 'Mass loading displacements'
      HEB_DSPL%PROD_NAME      = 'Mass loading displacements'
      HEB_DSPL%INSTITUTION    = 'Astrogeo Center'
      HEB_DSPL%REFERENCES     = 'http://astrogeo.org/malo/'
      HEB_DSPL%VERSION_ID     = PRGNAM
      HEB_DSPL%COMMENT(1)     = FILREF
      HEB_DSPL%COMMENT(2)     = FILDSC
      HEB_DSPL%COMMENT(3)     = FILCOM
!
      CALL ERR_PASS  ( IUER, IER )
      CALL WRITE_HEB ( HEB_DSPL, HEB_DSPL%VAL, FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6783, IUER, 'WRITE_LOADING_HEB', 'Failed  '// &
     &         'to write loading displacements into the output '// &
     &         'file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      DEALLOCATE ( HEB_DSPL%VAL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE WRITE_LOADING_HEB  !#!  
