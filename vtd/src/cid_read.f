      SUBROUTINE CID_READ ( INTMOD_TYPE, UTC_MTAI, FINAM, L_MOD, &
     &                      K_MOD, CIH, CID, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CID_READ reads an input binary file with an               *
! *   inteferometric model in CODA (VERA) format, parses it, and returns *
! *   array K_MOD, as well as objects CIH and CID.                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * INTMODE_TYPE ( CHARACTER ) -- Model ID. Supported models:            *
! *                               CID__MOD_VERA1000                      *
! *                               CID__MOD_VERA2000                      *
! * UTC_MTAI     ( REAL*8    ) -- Function UTC minus TAI on the epoch    *
! *                               of an experiment.                      *
! * FINAM        ( CHARACTER ) -- File name with the interferometric     *
! *                               model.                                 *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * L_MOD ( INTEGER*4          ) -- The total number of interferometric  *
! *                                 model records.                       *
! * K_MOD ( INTEGER*4          ) -- Array with the number of             *
! *                                 interferometric model records        *
! *                                 per station. Dimension: CID__MSTA    *
! * CIH   ( CODA_IFMOD__HEADER ) -- Object with the header of the        *
! *                                 input file.                          *
! * CID   ( CODA_IFMOD__DATA   ) -- Array of objects with the contents   *
! *                                 of the input file with the           *
! *                                 interferometric model. Dimension:    *
! *                                 CID__MMOD.                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 27-FEB-2012     CID_READ    v3.0  (d) L. Petrov 30-JUN-2025  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'cid.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( CODA_IFMOD__HEADER ) :: CIH
      TYPE     ( CODA_IFMOD__DATA   ) :: CID
      REAL*8     UTC_MTAI
      CHARACTER  FINAM*(*), INTMOD_TYPE*(*)
      INTEGER*4  L_MOD, K_MOD(CID__MSTA), IUER
      REAL*8,    ALLOCATABLE :: CIM_DATA(:)
      CHARACTER  BUF*1024, STR*128
      INTEGER*8  SIZE_I8
      INTEGER*4  IS, UNIX_DATE, SEEK_CUR, LUN, LN, LEN_DATA, &
     &           J1, J2, J3, IL, IL1, IL2, IL3, IL4, IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, LSEEK, LTM_DIF, READ
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_CUR', SEEK_CUR, LN )
      IS = FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), UNIX_DATE, &
                       SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7471, IUER, 'CID_READ', &
     &         'Failure in attempt to learn size of file '// &
     &          FINAM(1:I_LEN(FINAM))//' -- '//STR )
           RETURN
      END IF
!
      IF ( SIZE_I8 < 512 ) THEN
           CALL ERR_LOG ( 7472, IUER, 'CID_READ', &
     &         'File '//FINAM(1:I_LEN(FINAM))//' is too short' )
           RETURN
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER < 0 ) THEN
           CALL ERR_LOG ( 7473, IUER, 'CID_READ', &
     &         'Failure in attempt to open CODA file '// &
     &          FINAM(1:I_LEN(FINAM))//' -- '//STR )
           RETURN
      END IF
!
      IF ( INTMOD_TYPE == CID__MOD_VERA2000 ) THEN
           IS = READ  ( %VAL(LUN), %REF(BUF), %VAL(400) )
           IF ( IS < 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7474, IUER, 'CID_READ', &
     &              'Failure in attempt to read CODA file '// &
     &               FINAM(1:I_LEN(FINAM))//' -- '//STR )
                RETURN
           END IF
           IF ( BUF(1:13) .NE. 'HEADDER_START' ) THEN
                CALL ERR_LOG ( 7475, IUER, 'CID_READ', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' does not '// &
     &              'conform CODA standard: '//BUF(1:13) )
                RETURN
           END IF
           IF ( BUF(389:399) .NE. 'HEADDER_END' ) THEN
                CALL ERR_LOG ( 7476, IUER, 'CID_READ', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has unexpected '// &
     &              'format' )
                RETURN
           END IF
!
           CIH%VERSION = BUF(40:42)//' '
           IF ( CIH%VERSION .NE. '1.0 ' ) THEN
                CALL CLRCH (  STR )
                CALL TRAN ( 13, BUF(40:42), STR(1:3) )
                CALL ERR_LOG ( 7477, IUER, 'CID_READ', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has unsupported '// &
     &              'version '//STR(1:3)//' while only version 1.0 '// &
     &              'is supported' )
                RETURN
           END IF
!
           IS = LSEEK ( %VAL(LUN), %VAL(1), %VAL(SEEK_CUR) )
           IS = READ  ( %VAL(LUN), CIH%EXP_NAME, %VAL(32) )
           IS = READ  ( %VAL(LUN), CIH%NPOLY, %VAL(4) )
           IS = READ  ( %VAL(LUN), CIH%VERS,  %VAL(4) )
           IS = READ  ( %VAL(LUN), CIH%NSS,   %VAL(4) )
           CALL ENDIAN_CNV_I4 ( CIH%NPOLY )
           CALL ENDIAN_CNV_I4 ( CIH%VERS  )
           CALL ENDIAN_CNV_I4 ( CIH%NSS   )
           CIH%NDAT = (SIZE_I8 - 445)/(61 + CIH%NSS*11*8)
           IF ( CIH%NDAT*(61 + CIH%NSS*11*8) .NE. (SIZE_I8 - 445) ) THEN
                WRITE ( 6, * ) 'CIH%NDAT*(61 + CIH%NSS*11*8) = ', CIH%NDAT*(61 + CIH%NSS*11*8)
                WRITE ( 6, * ) 'CIH%NSS*11*8 = ', CIH%NSS*11*8
                WRITE ( 6, * ) 'CIH%NPOLY= ', CIH%NPOLY, ' CIH%VERS= ', CIH%VERS, ' CIH%NSS= ', CIH%NSS
                WRITE ( 6, * ) 'CIH%NDAT= ', CIH%NDAT
                CALL ERR_LOG ( 7478, IUER, 'CID_READ', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has nonstadnard '// &
     &              'length' )
                RETURN
           END IF
           IS = LSEEK ( %VAL(LUN), %VAL(49), %VAL(SEEK_CUR) )
           IS = READ  ( %VAL(LUN), CIH%IND_ANT, %VAL(4) )
           CALL ENDIAN_CNV_I4    ( CIH%IND_ANT )
         ELSE IF ( INTMOD_TYPE == CID__MOD_VERA1000 ) THEN
           IS = LSEEK ( %VAL(LUN), %VAL(48), %VAL(SEEK_CUR) )
           IS = READ  ( %VAL(LUN), CIH%NSS, %VAL(4) )
           CALL ENDIAN_CNV_I4    ( CIH%NSS )
           CIH%NDAT = (SIZE_I8 - 56)/(72 + CIH%NSS*11*8)
           IF ( CIH%NDAT*(72 + CIH%NSS*11*8) .NE. (SIZE_I8 - 56) ) THEN
                WRITE ( 6, * ) 'CIH%NDAT*(61 + CIH%NSS*11*8) = ', CIH%NDAT*(61 + CIH%NSS*11*8)
                WRITE ( 6, * ) 'CIH%NSS*11*8 = ', CIH%NSS*11*8
                WRITE ( 6, * ) 'CIH%NDAT= ', CIH%NDAT
                CALL ERR_LOG ( 7479, IUER, 'CID_READ', &
     &              'File '//FINAM(1:I_LEN(FINAM))//' has nonstadnard '// &
     &              'length' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_CUR', SEEK_CUR, LN )
      ALLOCATE ( CIM_DATA(CIH%NSS*11) )
!
      L_MOD = 0
      K_MOD = 0
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7480, IUER, 'CID_READ', 'Failure in attempt '// &
     &         'to open CODA file '//FINAM )
           DEALLOCATE ( CIM_DATA )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      IF ( INTMOD_TYPE == CID__MOD_VERA2000 ) THEN
           IS = LSEEK ( %VAL(LUN), %VAL(445), %VAL(SEEK_CUR) )
           LEN_DATA = 445
         ELSE IF ( INTMOD_TYPE == CID__MOD_VERA1000 ) THEN
           IS = LSEEK ( %VAL(LUN), %VAL(52), %VAL(SEEK_CUR) )
           LEN_DATA = 52
      END IF
      IF ( IS .NE. LEN_DATA ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7481, IUER, 'CID_READ', &
     &         'Failure in attempt to seek the CODA file '// &
     &          FINAM(1:I_LEN(FINAM))//' -- '//STR )
           DEALLOCATE ( CIM_DATA )
           RETURN
      END IF
      DO 410 J1=1,CIH%NDAT
         IF ( INTMOD_TYPE == CID__MOD_VERA2000 ) THEN
              IS = LSEEK ( %VAL(LUN), %VAL(1), %VAL(SEEK_CUR) )
!
              IS = READ  ( %VAL(LUN), CID, %VAL(60) )
              IF ( IS .NE. 60 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7482, IUER, 'CID_READ', &
     &                 'Failure in attempt to read the CODA file '// &
     &                  FINAM(1:I_LEN(FINAM))//' -- '//STR )
                   DEALLOCATE ( CIM_DATA )
                   RETURN
              END IF
           ELSE IF ( INTMOD_TYPE == CID__MOD_VERA1000 ) THEN
              IS = LSEEK ( %VAL(LUN), %VAL(12), %VAL(SEEK_CUR) )
              IS = READ  ( %VAL(LUN), CID, %VAL(60) )
              IF ( IS .NE. 60 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7483, IUER, 'CID_READ', &
     &                 'Failure in attempt to read the CODA file '// &
     &                  FINAM(1:I_LEN(FINAM))//' -- '//STR )
                   DEALLOCATE ( CIM_DATA )
                   RETURN
              END IF
         END IF
!
         IS = READ  ( %VAL(LUN), CIM_DATA, %VAL(8*11*CIH%NSS) )
         IF ( IS .NE. 8*11*CIH%NSS ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7484, IUER, 'CID_READ', &
     &            'Failure in attempt to read the CODA file '// &
     &             FINAM(1:I_LEN(FINAM))//' -- '//STR )
              DEALLOCATE ( CIM_DATA )
              RETURN
         END IF
!
         L_MOD = L_MOD + 1
         IF ( L_MOD > CID__MMOD ) THEN
              CALL CLRCH  ( STR )
              CALL INCH   ( CID__MMOD, STR )
              CALL ERR_LOG ( 7485, IUER, 'CID_READ', 'Too many records in the '// &
     &            'input the CODA file '//TRIM(FINAM)//' more than CID__MMOD= '// &
     &             STR )
              DEALLOCATE ( CIM_DATA )
              RETURN
         END IF
!
         CID%PDELAY = CIM_DATA(1)
         CID%GDELAY = CIM_DATA(1+CIH%NSS)
         CID%PRATE  = CIM_DATA(1+2*CIH%NSS)
         CID%GRATE  = CIM_DATA(1+3*CIH%NSS)
         CID%DDELAY = CIM_DATA(1+4*CIH%NSS)
         CID%DRATE  = CIM_DATA(1+5*CIH%NSS)
         CID%P2ND   = CIM_DATA(1+6*CIH%NSS)
         CID%G2ND   = CIM_DATA(1+7*CIH%NSS)
         CID%P3RD   = CIM_DATA(1+8*CIH%NSS)
         CID%G3RD   = CIM_DATA(1+9*CIH%NSS)
         CID%P4TH   = CIM_DATA(1+10*CIH%NSS)
!
         CALL ENDIAN_CNV_R8 ( CID%MJD_R8  )
         CALL ENDIAN_CNV_R8 ( CID%PP      )
         CALL ENDIAN_CNV_I4 ( CID%ANT_NUM )
         CALL ENDIAN_CNV_R8 ( CID%FAR_ROT )
         CALL ENDIAN_CNV_R8 ( CID%PDELAY  )
         CALL ENDIAN_CNV_R8 ( CID%GDELAY  )
         CALL ENDIAN_CNV_R8 ( CID%PRATE   )
         CALL ENDIAN_CNV_R8 ( CID%GRATE   )
         CALL ENDIAN_CNV_R8 ( CID%DDELAY  )
         CALL ENDIAN_CNV_R8 ( CID%DRATE   )
         CALL ENDIAN_CNV_R8 ( CID%P2ND    )
         CALL ENDIAN_CNV_R8 ( CID%G2ND    )
         CALL ENDIAN_CNV_R8 ( CID%P3RD    )
         CALL ENDIAN_CNV_R8 ( CID%G3RD    )
         CALL ENDIAN_CNV_R8 ( CID%P4TH    )
         CID%MJD_R8 = CID%MJD_R8 - UTC_MTAI/86400.0D0
!        
! ------ Bypass "fill" values
!
         IF ( DABS(CID%GDELAY) > 1.0D0 ) GOTO 410
         IF ( CID%ANT_NUM .NE. CIH%IND_ANT ) THEN
              WRITE ( 6, * ) ' J1 ', INT2(J1), ' ANT_NUM = ', CID%ANT_NUM, &
     &                       ' IND_ANT = ', CIH%IND_ANT
              CALL ERR_LOG ( 7486, IUER, 'CID_READ', &
     &            'Trap of internal control: ANT_NUM is not IND_ANT '// &
     &            'in file '//FINAM )
              DEALLOCATE ( CIM_DATA )
              RETURN
         END IF
         K_MOD(CIH%STA_IND) = K_MOD(CIH%STA_IND) + 1
 410  CONTINUE
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7487, IUER, 'CID_READ', &
     &         'Failure in attempt to close CODA file '// &
     &          FINAM )
           DEALLOCATE ( CIM_DATA )
           RETURN
      END IF
      DEALLOCATE ( CIM_DATA )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CID_READ  !#!#
