      SUBROUTINE READ_HEO ( FILIN, M_HEO, L_HEO, HEO, NAME_HEO, &
     &                      HEO_EPOCH_SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_HEO  reads the file with harmonic Earth orientation  *
! *   parameter variations in HEO format, parses it, and puts its        *
! *   context into HEO data record.                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    FILIN ( CHARACTER )  -- Input file in HEO format.                 *
! *    M_HEO ( INTEGER*4 )  -- The maximal number of Harmonic Earth      *
! *                            Orientation variations constituents.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      L_HEO ( INTEGER*4 ) -- The number of Harmonic Earth Orientation *
! *                             variations constituents which were found *
! *                             in the file.                             *
! *        HEO ( RECORD    ) -- Array of records with the Harmonic Earth *
! *                             Orientation variations. Data structure   *
! *                             is defined $MK5_ROOT/include/heo.i       *
! *                             Dimension: L_HEO.                        *
! *   NAME_HEO ( CHARACTER ) -- Name of the mode of harmonic variations  *
! *                             in the Earth rotation.                   *
! * HEO_EPOCH_SEC ( REAL*8 ) -- Epoch of the Harmonic Earth Orientation  *
! *                             in HEO_EPOCH_JD. This epoch is used for  *
! *                             computing contribution of the rate of    *
! *                             changes of the amplitudes.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ### 01-OCT-2003    READ_HEO   v3.1 (c)  L. Petrov  08-JUN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heo.i'
      CHARACTER  FILIN*128, NAME_HEO*(*)
      INTEGER*4  M_HEO, L_HEO, IUER
      TYPE ( HEO__STRUC ) :: HEO(M_HEO)
      REAL*8     HEO_EPOCH_SEC
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 2*M__HEO )
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128, WAVE_A*8, WAVE_V*8, WAVE_S*8, WAVE_R*8, &
     &           REF_EPOCH_STR*21
      LOGICAL*4  LEX
      INTEGER*4  J2000__MJD
      PARAMETER  ( J2000__MJD  = 51544       ) ! 2000.01.01_00:00:00
      INTEGER*4  NBUF, IW, IOS1, IOS2, IOS3, IOS4, IOS5, IOS6, I_HEO, &
     &           MJD_EPH, LEN_HEO, J1, J2, J3, J4, J5, IER
      REAL*8     SEC_EPH
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! --- Initialization
!
      L_HEO = 0
#ifdef SUN
!
! --- An attempt to circumvent a bug in Sun compiler
!
      LEN_HEO = LOC(HEO(1)%LAST_FIELD) - LOC(HEO(1)) + SIZEOF(HEO(1)%LAST_FIELD) 
#else
      LEN_HEO = SIZEOF(HEO(1))
#endif
      IF ( LEN_HEO .EQ. 0 ) THEN
!
! -------- It may be needed for bypassing a bug in Sun compiler
!
      END IF
      CALL NOUT  ( M_HEO*LEN_HEO, HEO )
      CALL CLRCH ( NAME_HEO )
!
! --- Check: wether th efile with HEO exists
!
      INQUIRE ( FILE=FILIN, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3711, IUER, 'READ_HEO', 'File with Harmonic '// &
     &         'Earth Orientation variations '//FILIN(1:I_LEN(FILIN))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Alocate memory for the buffer where the file will be read
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( MBUF*128, STR )
           CALL ERR_LOG ( 3712, IUER, 'READ_HEO', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes for an internal '// &
     &         'buffer' ) 
           RETURN 
      END IF
!
! --- Read the file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3713, IUER, 'READ_HEO', 'Error in reading '// &
     &         'the file with Harmonic Earth Orientation variations '// &
     &          FILIN )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(HEO__LABEL))       .NE. HEO__LABEL       .AND. &
     &     BUF(1)(1:LEN(HEO__LABEL_NOERR)) .NE. HEO__LABEL_NOERR        ) THEN
!
           write ( 6, '(a)' ) ' bb ', buf(1)(1:LEN(HEO__LABEL_NOERR)) ! %%%
           write ( 6, '(a)' ) ' ll ', HEO__LABEL_NOERR               ! %%%
           CALL ERR_LOG ( 3714, IUER, 'READ_HEO', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//' is not in HEO format. The first '// &
     &         'line is "'//BUF(1)(1:I_LEN(BUF(1)))//'"  file the HEO '// &
     &         'format label "'//HEO__LABEL//'" was expected' )
           RETURN 
      END IF
!
      CALL CLRCH ( REF_EPOCH_STR )
      CALL CLRCH ( NAME_HEO      )
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
         IF ( J1 .EQ. NBUF  .AND.  BUF(J1) .NE. BUF(1) ) THEN
!          
              CALL ERR_LOG ( 3715, IUER, 'READ_HEO', 'Apparantly the input '// &
     &            'file '//FILIN(1:I_LEN(FILIN))//' was truncated: '// &
     &            'the footer line was not found' ) 
              RETURN 
         END IF
!         
         IF ( BUF(J1)(1:2) .EQ. 'N ' ) THEN
              NAME_HEO = BUF(J1)(4:80)
           ELSE IF ( BUF(J1)(1:2) .EQ. 'E ' ) THEN
              REF_EPOCH_STR = BUF(J1)(4:24)
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( REF_EPOCH_STR, MJD_EPH, SEC_EPH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 3716, IUER, 'READ_HEO', 'Wrong epoch: '// &
     &                  REF_EPOCH_STR//' in the input file with the '//     &
     &                 'Harmonic Earth Orientation variations '//FILIN )
                   RETURN 
              END IF
              HEO_EPOCH_SEC = (MJD_EPH - J2000__MJD)*86400.0D0 + SEC_EPH
           ELSE IF ( BUF(J1)(1:2) .EQ. 'H ' ) THEN
              L_HEO = L_HEO + 1
              IF ( L_HEO .GT. M_HEO ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( M_HEO, STR )
                   CALL ERR_LOG ( 3717, IUER, 'READ_HEO', 'Input file '//    &
     &                 'with the Harmonic Earth Orientation variations '//   &
     &                  FILIN(1:I_LEN(FILIN))//' has too many harmonics. '// &
     &                 'More than the limit '//STR ) 
                   RETURN 
              END IF
!              
              HEO(L_HEO)%WAVE = BUF(J1)(4:11)
              READ ( UNIT=BUF(J1)(14:25), FMT='(F11.9)',  &
     &               IOSTAT=IOS1 ) HEO(L_HEO)%PHAS
              READ ( UNIT=BUF(J1)(28:46), FMT='(D19.12)', &
     &               IOSTAT=IOS2 ) HEO(L_HEO)%FREQ
              READ ( UNIT=BUF(J1)(49:59), FMT='(D11.4)',  &
     &               IOSTAT=IOS3 ) HEO(L_HEO)%ACCL
              IF ( IOS1 .NE. 0  .OR. IOS2 .NE. 0  .OR. IOS3 .NE. 0 ) THEN
                   WRITE ( 6, * ) ' IOS1=',IOS1,' IOS2=',IOS2,' IOS3=',IOS3
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3718, IUER, 'READ_HEO', 'Error in '// &
     &                 'Decoding the H-record, line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' with Harmonic Earth Orientation variations' )
                   RETURN 
              END IF
              HEO(L_HEO)%USE_VEL = .FALSE. ! We set it by default
              HEO(L_HEO)%USE_ERR = .FALSE. ! We set it by default
            ELSE IF ( BUF(J1)(1:2) .EQ. 'A ' ) THEN
              WAVE_A = BUF(J1)(4:11)
              I_HEO = 0
              DO 420 J2=1,L_HEO
                 IF ( HEO(J2)%WAVE .EQ. WAVE_A ) THEN
                      IF ( I_HEO .NE. 0 ) THEN
                           CALL ERR_LOG ( 3719, IUER, 'READ_HEO', 'Harmonic '// &
     &                          WAVE_A//' was defined more than once in the '// &
     &                         'input file '//FILIN(1:I_LEN(FILIN))// &
     &                         ' with the Harmonic Earth Orientation '// &
     &                         'variations ' ) 
                           RETURN 
                      END IF
                      I_HEO = J2
                 END IF
 420          CONTINUE 
              IF ( I_HEO .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3720, IUER, 'READ_HEO', 'The input file '// &
     &                  FILIN(1:I_LEN(FILIN))//' with the Harmonic Earth '// &
     &                 'Orientation variations has the A-record with '// &
     &                 'harmonic '//WAVE_A//' which was not defined '// &
     &                 'in preceeding H-records' ) 
                   RETURN 
              END IF
!
! ----------- Decode HEO values
!
              READ ( UNIT=BUF(J1)(14:25), FMT='(F12.0)', IOSTAT=IOS1 ) &
     &               HEO(I_HEO)%ROTANG(HEO__PMC,HEO__ANG)
              READ ( UNIT=BUF(J1)(27:38), FMT='(F12.0)', IOSTAT=IOS2 ) &
     &               HEO(I_HEO)%ROTANG(HEO__PMS,HEO__ANG)
              READ ( UNIT=BUF(J1)(41:52), FMT='(F12.0)', IOSTAT=IOS3 ) &
     &               HEO(I_HEO)%ROTANG(HEO__E3C,HEO__ANG)
              READ ( UNIT=BUF(J1)(54:65), FMT='(F12.0)', IOSTAT=IOS4 ) &
     &               HEO(I_HEO)%ROTANG(HEO__E3S,HEO__ANG)
!
              IF ( IOS1 .NE. 0  .OR.  IOS2 .NE. 0  .OR.  &
     &             IOS3 .NE. 0  .OR.  IOS4 .NE. 0        ) THEN
!
                   WRITE ( 6, * ) ' IOS1=',IOS1, ' IOS2=',IOS2, &
     &                            ' IOS3=',IOS3, ' IOS4=',IOS4  
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3721, IUER, 'READ_HEO', 'Error in '// &
     &                 'Decoding the A-record, line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' with Harmonic Earth Orientation variations' )
                   RETURN 
              END IF
!
! ----------- Convert HEO values from prad to rad
!
              HEO(I_HEO)%ROTANG(HEO__PMC,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG(HEO__PMC,HEO__ANG)*1.D-12
              HEO(I_HEO)%ROTANG(HEO__PMS,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG(HEO__PMS,HEO__ANG)*1.D-12
              HEO(I_HEO)%ROTANG(HEO__E3C,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG(HEO__E3C,HEO__ANG)*1.D-12
              HEO(I_HEO)%ROTANG(HEO__E3S,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG(HEO__E3S,HEO__ANG)*1.D-12
            ELSE IF ( BUF(J1)(1:2) .EQ. 'V ' ) THEN
              WAVE_V = BUF(J1)(4:11)
              I_HEO = 0
              DO 430 J3=1,L_HEO
                 IF ( HEO(J3)%WAVE .EQ. WAVE_V ) THEN
                      IF ( I_HEO .NE. 0 ) THEN
                           CALL ERR_LOG ( 3722, IUER, 'READ_HEO', 'Harmonic '// &
     &                          WAVE_V//' was defined more than once in the '// &
     &                         'input file '//FILIN(1:I_LEN(FILIN))// &
     &                         ' with the Harmonic Earth Orientation '// &
     &                         'variations ' ) 
                           RETURN 
                      END IF
                      I_HEO = J3
                 END IF
 430          CONTINUE 
              IF ( I_HEO .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3723, IUER, 'READ_HEO', 'The input file '// &
     &                  FILIN(1:I_LEN(FILIN))//' with the Harmonic Earth '// &
     &                 'Orientation variations has the V-record with '// &
     &                 'harmonic '//WAVE_V//' which was not defined '// &
     &                 'in preceeding H-records' ) 
                   RETURN 
              END IF
              HEO(I_HEO)%USE_VEL = .TRUE.
!
! ----------- Decode values of HEO rates
!
              READ ( UNIT=BUF(J1)(14:25), FMT='(F12.0)', IOSTAT=IOS1 ) &
     &               HEO(I_HEO)%ROTANG(HEO__PMC,HEO__VEL)
              READ ( UNIT=BUF(J1)(27:38), FMT='(F12.0)', IOSTAT=IOS2 ) &
     &               HEO(I_HEO)%ROTANG(HEO__PMS,HEO__VEL)
              READ ( UNIT=BUF(J1)(41:52), FMT='(F12.0)', IOSTAT=IOS3 ) &
     &               HEO(I_HEO)%ROTANG(HEO__E3C,HEO__VEL)
              READ ( UNIT=BUF(J1)(54:65), FMT='(F12.0)', IOSTAT=IOS4 ) &
     &               HEO(I_HEO)%ROTANG(HEO__E3S,HEO__VEL)
!
              IF ( IOS1 .NE. 0  .OR.  IOS2 .NE. 0  .OR.  &
     &             IOS3 .NE. 0  .OR.  IOS4 .NE. 0        ) THEN
!
                   WRITE ( 6, * ) ' IOS1=',IOS1, ' IOS2=',IOS2, &
     &                            ' IOS3=',IOS3, ' IOS4=',IOS4  
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3724, IUER, 'READ_HEO', 'Error in '// &
     &                 'Decoding the V-record, line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' with Harmonic Earth Orientation variations' )
                   RETURN 
              END IF
!
! ----------- Convert HEO values from 1.0D-21 rad/sec to rad/sec
!
              HEO(I_HEO)%ROTANG(HEO__PMC,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG(HEO__PMC,HEO__VEL)*1.0D-21
              HEO(I_HEO)%ROTANG(HEO__PMS,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG(HEO__PMS,HEO__VEL)*1.0D-21
              HEO(I_HEO)%ROTANG(HEO__E3C,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG(HEO__E3C,HEO__VEL)*1.0D-21
              HEO(I_HEO)%ROTANG(HEO__E3S,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG(HEO__E3S,HEO__VEL)*1.0D-21
            ELSE IF ( BUF(J1)(1:2) .EQ. 'S ' ) THEN
              WAVE_S = BUF(J1)(4:11)
              I_HEO = 0
              DO 440 J4=1,L_HEO
                 IF ( HEO(J4)%WAVE .EQ. WAVE_S ) THEN
                      IF ( I_HEO .NE. 0 ) THEN
                           CALL ERR_LOG ( 3725, IUER, 'READ_HEO', 'Harmonic '// &
     &                          WAVE_S//' was defined more than once in the '// &
     &                         'input file '//FILIN(1:I_LEN(FILIN))// &
     &                         ' with the Harmonic Earth Orientation '// &
     &                         'variations ' ) 
                           RETURN 
                      END IF
                      I_HEO = J4
                 END IF
 440          CONTINUE 
              IF ( I_HEO .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3726, IUER, 'READ_HEO', 'The input file '// &
     &                  FILIN(1:I_LEN(FILIN))//' with the Harmonic Earth '// &
     &                 'Orientation variations has the S-record with '// &
     &                 'harmonic '//WAVE_S//' which was not defined '// &
     &                 'in preceeding H-records' ) 
                   RETURN 
              END IF
!
! ----------- Decode HEO values
!
              READ ( UNIT=BUF(J1)(15:26), FMT='(F12.1)', IOSTAT=IOS1 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__PMC,HEO__ANG)
              READ ( UNIT=BUF(J1)(28:39), FMT='(F12.1)', IOSTAT=IOS2 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__PMS,HEO__ANG)
              READ ( UNIT=BUF(J1)(42:53), FMT='(F12.1)', IOSTAT=IOS3 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__E3C,HEO__ANG)
              READ ( UNIT=BUF(J1)(55:66), FMT='(F12.1)', IOSTAT=IOS4 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__E3S,HEO__ANG)
!
              IF ( IOS1 .NE. 0  .OR.  IOS2 .NE. 0  .OR.  &
     &             IOS3 .NE. 0  .OR.  IOS4 .NE. 0        ) THEN
!
                   WRITE ( 6, * ) ' IOS1=',IOS1, ' IOS2=',IOS2, &
     &                            ' IOS3=',IOS3, ' IOS4=',IOS4  
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3727, IUER, 'READ_HEO', 'Error in '// &
     &                 'Decoding the S-record, line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' with Harmonic Earth Orientation variations' )
                   RETURN 
              END IF
!
! ----------- Convert HEO values from prad to rad
!
              HEO(I_HEO)%ROTANG_ERR(HEO__PMC,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG_ERR(HEO__PMC,HEO__ANG)*1.D-12
              HEO(I_HEO)%ROTANG_ERR(HEO__PMS,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG_ERR(HEO__PMS,HEO__ANG)*1.D-12
              HEO(I_HEO)%ROTANG_ERR(HEO__E3C,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG_ERR(HEO__E3C,HEO__ANG)*1.D-12
              HEO(I_HEO)%ROTANG_ERR(HEO__E3S,HEO__ANG) = &
    &                    HEO(I_HEO)%ROTANG_ERR(HEO__E3S,HEO__ANG)*1.D-12
              HEO(L_HEO)%USE_ERR = .TRUE.
            ELSE IF ( BUF(J1)(1:2) .EQ. 'R ' ) THEN
              WAVE_R = BUF(J1)(4:11)
              I_HEO = 0
              DO 450 J5=1,L_HEO
                 IF ( HEO(J5)%WAVE .EQ. WAVE_R ) THEN
                      IF ( I_HEO .NE. 0 ) THEN
                           CALL ERR_LOG ( 3728, IUER, 'READ_HEO', 'Harmonic '// &
     &                          WAVE_R//' was defined more than once in the '// &
     &                         'input file '//FILIN(1:I_LEN(FILIN))// &
     &                         ' with the Harmonic Earth Orientation '// &
     &                         'variations ' ) 
                           RETURN 
                      END IF
                      I_HEO = J5
                 END IF
 450          CONTINUE 
              IF ( I_HEO .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3729, IUER, 'READ_HEO', 'The input file '// &
     &                  FILIN(1:I_LEN(FILIN))//' with the Harmonic Earth '// &
     &                 'Orientation variations has the R-record with '// &
     &                 'harmonic '//WAVE_R//' which was not defined '// &
     &                 'in preceeding H-records' ) 
                   RETURN 
              END IF
              HEO(I_HEO)%USE_VEL = .TRUE.
              HEO(I_HEO)%USE_ERR = .TRUE.
!
! ----------- Decode values of HEO rates
!
              READ ( UNIT=BUF(J1)(14:25), FMT='(F12.0)', IOSTAT=IOS1 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__PMC,HEO__VEL)
              READ ( UNIT=BUF(J1)(27:38), FMT='(F12.0)', IOSTAT=IOS2 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__PMS,HEO__VEL)
              READ ( UNIT=BUF(J1)(41:52), FMT='(F12.0)', IOSTAT=IOS3 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__E3C,HEO__VEL)
              READ ( UNIT=BUF(J1)(54:65), FMT='(F12.0)', IOSTAT=IOS4 ) &
     &               HEO(I_HEO)%ROTANG_ERR(HEO__E3S,HEO__VEL)
!
              IF ( IOS1 .NE. 0  .OR.  IOS2 .NE. 0  .OR.  &
     &             IOS3 .NE. 0  .OR.  IOS4 .NE. 0        ) THEN
!
                   WRITE ( 6, * ) ' IOS1=',IOS1, ' IOS2=',IOS2, &
     &                            ' IOS3=',IOS3, ' IOS4=',IOS4  
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3730, IUER, 'READ_HEO', 'Error in '// &
     &                 'Decoding the R-record, line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                 ' with Harmonic Earth Orientation variations' )
                   RETURN 
              END IF
!
! ----------- Convert HEO values from 1.0D-21 rad/sec to rad/sec
!
              HEO(I_HEO)%ROTANG_ERR(HEO__PMC,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG_ERR(HEO__PMC,HEO__VEL)*1.0D-21
              HEO(I_HEO)%ROTANG_ERR(HEO__PMS,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG_ERR(HEO__PMS,HEO__VEL)*1.0D-21
              HEO(I_HEO)%ROTANG_ERR(HEO__E3C,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG_ERR(HEO__E3C,HEO__VEL)*1.0D-21
              HEO(I_HEO)%ROTANG_ERR(HEO__E3S,HEO__VEL) = &
     &                   HEO(I_HEO)%ROTANG_ERR(HEO__E3S,HEO__VEL)*1.0D-21
         END IF
 410  CONTINUE 
!
! --- Deallocate memory for the buffer
!
      DEALLOCATE ( BUF )
!
      IF ( L_HEO > 0 ) THEN
           IF ( ILEN(NAME_HEO) .EQ. 0 ) THEN
                CALL ERR_LOG ( 3731, IUER, 'READ_HEO', 'Error in decoding the '// &
     &              'input file '//FILIN(1:I_LEN(FILIN))//' with Harmonic '// &
     &              'Earth Orientation variations: name of the model was not '// &
     &              'found' ) 
                RETURN 
           END IF
!
           IF ( ILEN(REF_EPOCH_STR) .EQ. 0 ) THEN
                CALL ERR_LOG ( 3732, IUER, 'READ_HEO', 'Error in decoding the '// &
     &              'input file '//FILIN(1:I_LEN(FILIN))//' with Harmonic '// &
     &              'Earth Orientation variations: the reference epoch was not '// &
     &              'found' ) 
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_HEO   #!#
