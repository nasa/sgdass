      SUBROUTINE VTD_LOAD_STAECC ( VTD, MBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_ECCDAT  reads eccentricity file, parses it and gets   *
! *   appropriate values of eccentricity vectors for a list of requested *
! *   stations. Eccentricity vector is a vector from the monument        *
! *   reference point to the point of antenna axis intersection.         *
! *                                                                      *
! *   GET_ECCDAT also returns an array of monument names and monument    *
! *   types.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        MBUF ( INTEGER*4 ) -- The length of the buffer in lines.      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       BUF ( CHARACTER ) -- The text buffer. Dimension: MBUF.         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 14-OCT-1999  VTD_LOAD_STAECC  v2.4 (c) L. Petrov 06-JUL-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MBUF, IUER
      CHARACTER  BUF(MBUF)*(*)
!
      LOGICAL*4  LEX
      CHARACTER  DATE_STR*21, STR*80, STR2*80
      CHARACTER  STAECC__LABEL1*38
      PARAMETER  ( STAECC__LABEL1 = '# ECC-FORMAT V 1.0   ECCENTRICITY FILE' )
      REAL*8     SF, CF, SL, CL, PP, RD, PHI_GCN, LONGITUDE, MU, LAT_GDT, &
     &           UEN_TO_CFS(3,3), ECC_VEC(3), ECC_VEC_UEN(3)
      INTEGER*4  NBUF, IOS, I_STA, J1, J2, J3, NN, IER
      REAL*8,    EXTERNAL :: ATAN_CS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( VTD%CONF%FINAM_STAECC == 'NONE' ) THEN
           DO 410 J1=1,VTD%L_STA
              VTD%STA(J1)%N_ECC = 0
 410       CONTINUE 
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=VTD%CONF%FINAM_STAECC, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2171, IUER, 'VTD_LOAD_STAECC', 'File with '// &
     &         'station eccentricites '// &
     &          VTD%CONF%FINAM_STAECC(1:I_LEN(VTD%CONF%FINAM_STAECC))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Read the file with station coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_STAECC, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2172, IUER, 'VTD_LOAD_STAECC', 'Error in an '// &
     &         'attempt to read input file with station eccentricity file '// &
     &          VTD%CONF%FINAM_STAECC )
           RETURN 
      END IF
!
      IF ( BUF(1) .NE. STAECC__LABEL1 ) THEN
           CALL ERR_LOG ( 2173, IUER, 'VTD_LOAD_STAECC', 'Error in an '// &
     &         'attempt to parse the input file with eccentricity file '// &
     &          VTD%CONF%FINAM_STAECC(1:I_LEN(VTD%CONF%FINAM_STAECC))// &
     &         ' -- format of this file was not recognized' )
           RETURN 
      END IF
!
! --- Parse the buffer
!
      DO 420 J2=1,NBUF
         IF ( BUF(J2)(1:1) .EQ. '#' ) GOTO 420
         IF ( BUF(J2)(1:1) .EQ. '$' ) GOTO 420
         CALL VTD_NAME_REPAIR ( BUF(J2)(3:10) )
         I_STA = 0
         DO 430 J3=1,VTD%L_STA
            IF ( BUF(J2)(3:10) .EQ. VTD%STA(J3)%IVS_NAME ) THEN
                 I_STA = J3
            END IF
 430     CONTINUE 
         IF ( I_STA .EQ. 0 ) GOTO 420
!
! ------ Parse the start date
!
         DATE_STR = BUF(J2)(18:33)//':00.0'
         VTD%STA(I_STA)%N_ECC = VTD%STA(I_STA)%N_ECC + 1
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_STR, &
     &        VTD%STA(I_STA)%ECC_MJD_BEG(VTD%STA(I_STA)%N_ECC), &
     &        VTD%STA(I_STA)%ECC_TAI_BEG(VTD%STA(I_STA)%N_ECC), IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J2, STR ) 
              CALL ERR_LOG ( 2174, IUER, 'VTD_LOAD_STAECC', 'Error '// &
     &            'in parsing reference epoch on line '//STR(1:I_LEN(STR))// &
     &            ' -- '//DATE_STR//' in the file with station '// &
     &            'eccentricites '//VTD%CONF%FINAM_STAECC )
              RETURN 
         END IF
!
! ------ Parse the end date
!
         DATE_STR = BUF(J2)(36:51)//':00.0'
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_STR, &
     &        VTD%STA(I_STA)%ECC_MJD_END(VTD%STA(I_STA)%N_ECC), &
     &        VTD%STA(I_STA)%ECC_TAI_END(VTD%STA(I_STA)%N_ECC), IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J2, STR ) 
              CALL ERR_LOG ( 2175, IUER, 'VTD_LOAD_STAECC', 'Error '// &
     &            'in parsing reference epoch on line '//STR(1:I_LEN(STR))// &
     &            ' -- '//DATE_STR//' in the file with station '// &
     &            'eccentricites '//VTD%CONF%FINAM_STAECC )
              RETURN 
         END IF
!
! ------ Get station's CDP number
!
         CALL CHIN ( BUF(J2)(12:15), VTD%STA(I_STA)%CDP_NUMBER )
         IF ( VTD%STA(I_STA)%CDP_NUMBER .LE.    0  .OR. &
     &        VTD%STA(I_STA)%CDP_NUMBER .GT. 9999       ) THEN
              VTD%STA(I_STA)%CDP_NUMBER = 9999
         END IF
!
! ------ Read the eccentrivity vector
!
         READ ( UNIT=BUF(J2)(54:63), FMT='(F10.3)', IOSTAT=IOS ) ECC_VEC(1)
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J2, STR ) 
              CALL ERR_LOG ( 2176, IUER, 'VTD_LOAD_STAECC', 'Error '// &
     &            'in parsing parsing line '//STR(1:I_LEN(STR))// &
     &            ' of the file with station eccentricities '// &
     &            VTD%CONF%FINAM_STAECC )
              RETURN 
         END IF
!
         READ ( UNIT=BUF(J2)(65:74), FMT='(F10.3)', IOSTAT=IOS ) ECC_VEC(2)
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J2, STR ) 
              CALL ERR_LOG ( 2177, IUER, 'VTD_LOAD_STAECC', 'Error '// &
     &            'in parsing parsing line '//STR(1:I_LEN(STR))// &
     &            ' of the file with station eccentricities '// &
     &            VTD%CONF%FINAM_STAECC )
              RETURN 
         END IF
!
         READ ( UNIT=BUF(J2)(76:85), FMT='(F10.3)', IOSTAT=IOS ) ECC_VEC(3)
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J2, STR ) 
              CALL ERR_LOG ( 2178, IUER, 'VTD_LOAD_STAECC', 'Error '// &
     &            'in parsing parsing line '//STR(1:I_LEN(STR))// &
     &            ' of the file with station eccentricities '// &
     &            VTD%CONF%FINAM_STAECC )
              RETURN 
         END IF
!
         IF ( BUF(J2)(88:90) .EQ. 'XYZ' ) THEN
              CALL COPY_R8 ( 3, ECC_VEC, &
     &                       VTD%STA(I_STA)%ECC_TRS(1,VTD%STA(I_STA)%N_ECC) )
            ELSE IF ( BUF(J2)(88:90) .EQ. 'NEU' ) THEN
!
! ----------- Calculation longitude LONGITUDE and geocentric lattitude PHI_GCN of
! ----------- the station
!
              IF ( DABS ( VTD%STA(I_STA)%COO_TRS(1,1) ) .GT. 1.D-8 ) THEN
                   LONGITUDE = ATAN_CS ( VTD%STA(I_STA)%COO_TRS(1,1), &
     &                                   VTD%STA(I_STA)%COO_TRS(2,1)  )
                ELSE
                   LONGITUDE = P2I
              END IF
!
              IF ( LONGITUDE  .LT. 0.D0 ) LONGITUDE = PI2 + LONGITUDE
!
              PP  = DSQRT ( VTD%STA(I_STA)%COO_TRS(1,1)**2 + &
     &                      VTD%STA(I_STA)%COO_TRS(2,1)**2 )
              IF ( DABS(PP) .LT. 1.D-8 ) PP=1.D-8
              RD = DSQRT ( VTD%STA(I_STA)%COO_TRS(3,1)**2 + PP**2    )
!
              IF ( RD < VTD__HEIGHT_MIN .OR. &
     &             RD > VTD__HEIGHT_MAX      ) THEN
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR, FMT='(I12)' ) J2
                   CALL CHASHL ( STR  )
!
                   CALL CLRCH  ( STR2 )
                   WRITE ( UNIT=STR2, FMT='(3(F15.3,2X))' ) &
     &                   ( VTD%STA(I_STA)%COO_TRS(NN,1), NN=1,3)
                   CALL ERR_LOG ( 2179, IUER, 'VTD_LOAD_STAECC', 'Wrong '// &
     &                 'positions of the '//STR(1:ILEN(STR))//'-th '// &
     &                 'station '//BUF(J2)(3:10)//' -- '//STR2(1:ILEN(STR2))// &
     &                 ' -- they are not on the surface of our planet' )
                   RETURN
              END IF
!
              PHI_GCN = DATAN( VTD%STA(I_STA)%COO_TRS(3,1)/PP )
!
! ----------- Computation of geodetic latitude
!
              MU = DATAN ( VTD%STA(I_STA)%COO_TRS(3,1)/PP * &
     &                     ( (1.D0 - VTD__FE) + VTD__EXC_SQ*VTD__REA/RD ) )
!
              LAT_GDT = DATAN( ( (1.D0 - VTD__FE)*VTD%STA(I_STA)%COO_TRS(3,1) + &
     &                         VTD__EXC_SQ*VTD__REA*DSIN(MU)**3 ) / &
     &                         ( (1.D0 - VTD__FE)* &
     &                         ( PP  - VTD__EXC_SQ*VTD__REA*DCOS(MU)**3 )) )

!
! ----------- Calculation matrix of transformation from UEN (local topocentric,
! ----------- (Up,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
              CF = DCOS(LAT_GDT)
              SF = DSIN(LAT_GDT)
              CL = DCOS(LONGITUDE)
              SL = DSIN(LONGITUDE)
!
              UEN_TO_CFS(1,1) = CF*CL
              UEN_TO_CFS(2,1) = CF*SL
              UEN_TO_CFS(3,1) = SF
!
              UEN_TO_CFS(1,2) = -SL
              UEN_TO_CFS(2,2) =  CL
              UEN_TO_CFS(3,2) =  0.D0
!
              UEN_TO_CFS(1,3) = -SF*CL
              UEN_TO_CFS(2,3) = -SF*SL
              UEN_TO_CFS(3,3) =  CF
!
              ECC_VEC_UEN(1) = ECC_VEC(3)
              ECC_VEC_UEN(2) = ECC_VEC(2)
              ECC_VEC_UEN(3) = ECC_VEC(1)
!
! ----------- Transform the eccentricity vector from Up-East-North to
!
              CALL MUL_MV_IV_V ( 3, 3, UEN_TO_CFS, 3, ECC_VEC_UEN, &
     &             3, VTD%STA(I_STA)%ECC_TRS(1,VTD%STA(I_STA)%N_ECC), IER )
            ELSE 
              CALL CLRCH ( STR ) 
              CALL INCH  ( J2, STR ) 
              CALL ERR_LOG ( 2180, IUER, 'VTD_LOAD_STAECC', 'Error '// &
     &            'in parsing line '//STR(1:I_LEN(STR))//' of the file '// &
     &            'with station eccentricities '//VTD%CONF%FINAM_STAECC )
              RETURN 
         END IF
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_STAECC 
