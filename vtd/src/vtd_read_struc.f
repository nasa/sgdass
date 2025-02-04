      SUBROUTINE VTD_READ_STRUC ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_READ_STRUC  parses control file which defines         *
! *   source maps used for computing contribution of source structure    *
! *   in VLBI delay.                                                     *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
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
! * ## 27-FEB-2004  VTD_READ_STRUC   v2.1 (c)  L. Petrov  17-OCT-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      CHARACTER  STR*80, STRUC__LABEL*44
      PARAMETER  ( STRUC__LABEL = 'STRUC_CONTROL  Format version of 2007.03.18 ' )
      LOGICAL*4  LEX
      INTEGER*4  MIND, M_MAC 
      PARAMETER  ( MIND  =  32 )
      PARAMETER  ( M_MAC = 128 )
      INTEGER*4  NBUF, IOS, J1, J2, J3, J4, LIND, IND(2,MIND), IND_MAC, &
     &           IP, IL, L_MAC, IBND, IER
      CHARACTER  C_MAC(M_MAC)*256, V_MAC(M_MAC)*256, ENV_NAME*256
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN, LTM_DIF, ADD_CLIST
!
! --- Check whether the file really exists
!
      INQUIRE ( FILE=VTD%CONF%FINAM_STRUC, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2371, IUER, 'VTD_READ_STRUC', 'Control file '// &
     &         'for source structure definition '// &
     &         VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Allocate memory for parsing the file
!
      ALLOCATE ( BUF(VTD__M_STR), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 256*VTD__M_STR, STR )
           CALL ERR_LOG ( 2372, IUER, 'VTD_READ_STRUC', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory' )
           RETURN 
      END IF
!
! --- Read the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_STRUC, VTD__M_STR, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2373, IUER, 'VTD_READ_STRUC', 'Error in an '// &
     &         'attempt to read input file with source structure '// &
     &         'definition '//VTD%CONF%FINAM_STRUC )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the header and footer
!
      IF ( BUF(1)(1:LEN(STRUC__LABEL)) .NE. STRUC__LABEL ) THEN
           CALL ERR_LOG ( 2374, IUER, 'VTD_READ_STRUC', 'Format of the '// &
     &         'input file with source structure definition '// &
     &         VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))// &
     &         ' was not recognized: the first line does not have the '// &
     &         'label which was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(NBUF)(1:LEN(STRUC__LABEL)) .NE. STRUC__LABEL ) THEN
           WRITE ( 6, * ) ' NBUF = ', NBUF 
           CALL ERR_LOG ( 2375, IUER, 'VTD_READ_STRUC', 'Apparently '//    &
     &         'the input file with source structure definition '//        &
     &         VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))//       &
     &         ' was damaged or was not read to the end: the last line '// &
     &         'does not have the label which was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Initializtion
!
      VTD%L_STR = 0
!
! --- Now time came to start parsing this file
!
      DO 410 J1=2,NBUF-1
         IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
!
         IF ( BUF(J1)(1:3) == 'SET' ) THEN
!
! ----------- Parsing the macro definition record
!
              CALL EXWORD ( BUF(J1), MIND, LIND, IND, &
     &                      CHAR(0)//CHAR(32)//CHAR(9), IER )
              IF ( LIND < 3 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2376, IUER, 'VTD_READ_STRUC', 'Too few '// &
     &                 'words on line '//STR(1:I_LEN(STR))//' of the  '// &
     &                 'source structure defintion file: '// &
     &                 VTD%CONF%FINAM_STRUC )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              IND_MAC = ADD_CLIST ( M_MAC, L_MAC, C_MAC, &
     &                              BUF(J1)(IND(1,2):IND(2,2)), IER )
              V_MAC(IND_MAC) = BUF(J1)(IND(1,3):IND(2,3)) 
              IF ( V_MAC(IND_MAC)(1:1)  == '{'  .AND.  &
     &             ILEN(V_MAC(IND_MAC)) > 2            ) THEN
!
                   IF ( V_MAC(IND_MAC)(ILEN(V_MAC(IND_MAC)):ILEN(V_MAC(IND_MAC))) .NE. '}' ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 2377, IUER, 'VTD_READ_STRUC', 'The '// &
     &                      'last character is not } in the enviroment '// &
     &                      'defintion word on line '//STR(1:I_LEN(STR))// &
     &                      ' of the source structure defintion file: '// &
     &                       VTD%CONF%FINAM_STRUC )
                        DEALLOCATE ( BUF )
                        RETURN 
                   END IF
!
                   ENV_NAME = V_MAC(IND_MAC)(2:ILEN(V_MAC(IND_MAC))-1) 
                   CALL GETENVAR ( ENV_NAME, V_MAC(IND_MAC) )
                   IF ( ILEN(V_MAC(IND_MAC)) .LE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 2378, IUER, 'VTD_READ_STRUC', 'The '// &
     &                      'environment variable '// &
     &                       ENV_NAME(1:I_LEN(ENV_NAME))//' is NOT DEFINED. '// &
     &                      'This variable was declarad at line '// &
     &                       STR(1:I_LEN(STR))//' of the source structure '// &
     &                      'defintion file: '//VTD%CONF%FINAM_STRUC )
                        DEALLOCATE ( BUF )
                        RETURN 
                   END IF
              END IF
            ELSE IF ( BUF(J1)(1:3) == 'SOU' ) THEN
!
! ----------- Parsing the source definition record
!
             VTD%L_STR = VTD%L_STR + 1
             VTD%STRUC(VTD%L_STR)%SOU_NAME = BUF(J1)(6:15)
             VTD%STRUC(VTD%L_STR)%BAND = BUF(J1)(18:18)
!
             IF ( BUF(J1)(21:28) .EQ. 'MAP_FITS' ) THEN
                  VTD%STRUC(VTD%L_STR)%USAGE_CODE = STRUC__MAP 
                ELSE IF ( BUF(J1)(21:28) .EQ. 'DEL_COMP' ) THEN
                  VTD%STRUC(VTD%L_STR)%USAGE_CODE = STRUC__DEL
                ELSE IF ( BUF(J1)(21:28) .EQ. 'GAU_COMP' ) THEN
                  VTD%STRUC(VTD%L_STR)%USAGE_CODE = STRUC__GAU
                ELSE IF ( BUF(J1)(21:28) .EQ. 'NONE    ' ) THEN
                  VTD%STRUC(VTD%L_STR)%USAGE_CODE = STRUC__NON
                ELSE 
                  CALL CLRCH ( STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 2379, IUER, 'VTD_READ_STRUC', 'Error in '//  &
     &                'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                'file with source structure definition '//              &
     &                 VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))//   &
     &               ' -- unrecognized usage keyword' ) 
                  DEALLOCATE ( BUF )
                  RETURN 
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL DATE_TO_TIME ( BUF(J1)(31:49), VTD%STRUC(VTD%L_STR)%MJD_BEG, &
     &                           VTD%STRUC(VTD%L_STR)%SEC_END, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL CLRCH ( STR ) 
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 2380, IUER, 'VTD_READ_STRUC', 'Error in '//  &
     &                'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                'file with source structure definition '//              &
     &                 VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))//  &
     &                ' -- wrong format of the date_begin field' )
                  DEALLOCATE ( BUF )
                  RETURN 
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL DATE_TO_TIME ( BUF(J1)(51:69), VTD%STRUC(VTD%L_STR)%MJD_END, &
     &                           VTD%STRUC(VTD%L_STR)%SEC_END, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL CLRCH ( STR ) 
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 2381, IUER, 'VTD_READ_STRUC', 'Error in '// &
     &                'parsing line '//STR(1:I_LEN(STR))//' of the input '// &
     &                'file with source structure definition '//             &
     &                 VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))//  &
     &                ' -- wrong format of the date_end field' )
                  DEALLOCATE ( BUF )
                  RETURN 
             END IF
!
             READ  ( UNIT=BUF(J1)(72:79), FMT='(F8.2)', IOSTAT=IOS ) &
     &               VTD%STRUC(VTD%L_STR)%PHASE_CENTER_PIXEL(1)
             IF ( IOS .NE. 0 ) THEN
                  CALL CLRCH ( STR ) 
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 2382, IUER, 'VTD_READ_STRUC', 'Error in '//  &
     &                'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                'file with source structure definition '//              &
     &                 VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))//   &
     &                ' -- wrong format of the first pixel phase center field' )
                  DEALLOCATE ( BUF )
                  RETURN 
             END IF
!
             READ ( UNIT=BUF(J1)(81:88), FMT='(F8.2)', IOSTAT=IOS ) &
     &              VTD%STRUC(VTD%L_STR)%PHASE_CENTER_PIXEL(2)
             IF ( IOS .NE. 0 ) THEN
                  CALL CLRCH ( STR ) 
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 2383, IUER, 'VTD_READ_STRUC', 'Error in '// &
     &                'parsing line '//STR(1:I_LEN(STR))//' of the input '// &
     &                'file with source structure definition '//             &
     &                 VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))//  &
     &                ' -- wrong format of the second pixel phase center field' )
                  DEALLOCATE ( BUF )
                  RETURN 
             END IF
!       
             VTD%STRUC(VTD%L_STR)%MAP_FILE = BUF(J1)(91:)
             IF ( L_MAC > 0 ) THEN
                  DO 420 J2=1,L_MAC
                     IP = INDEX ( VTD%STRUC(VTD%L_STR)%MAP_FILE, &
     &                            C_MAC(J2)(1:I_LEN(C_MAC(J2))) )
                     IF ( IP > 1 ) THEN
                          IF ( VTD%STRUC(VTD%L_STR)%MAP_FILE(IP-1:IP-1) == '$' ) THEN
!
! ---------------------------- Make substitution
!
                               IF ( IP == 2 ) THEN
                                    VTD%STRUC(VTD%L_STR)%MAP_FILE = &
     &                                 V_MAC(J2)(1:I_LEN(V_MAC(J2)))// &
     &                                 VTD%STRUC(VTD%L_STR)%MAP_FILE(IP+ILEN(C_MAC(J2)):)
                                  ELSE 
                                    VTD%STRUC(VTD%L_STR)%MAP_FILE = &
     &                                 VTD%STRUC(VTD%L_STR)%MAP_FILE(1:IP-2)//&
     &                                 V_MAC(J2)(1:I_LEN(V_MAC(J2)))// &
     &                                 VTD%STRUC(VTD%L_STR)%MAP_FILE(IP+ILEN(C_MAC(J2)):)
                               END IF
                          END IF
                     END IF
!
                     IF ( IP > 1 ) THEN
                          IF ( VTD%STRUC(VTD%L_STR)%MAP_FILE(IP-1:IP-1) == '$' ) THEN
!
! ---------------------------- Make substitution
!
                               IF ( IP == 2 ) THEN
                                    VTD%STRUC(VTD%L_STR)%MAP_FILE = &
     &                                 V_MAC(J2)(1:I_LEN(V_MAC(J2)))// &
     &                                 VTD%STRUC(VTD%L_STR)%MAP_FILE(IP+ILEN(C_MAC(J2)):)
                                  ELSE 
                                    VTD%STRUC(VTD%L_STR)%MAP_FILE = &
     &                                 VTD%STRUC(VTD%L_STR)%MAP_FILE(1:IP-2)//&
     &                                 V_MAC(J2)(1:I_LEN(V_MAC(J2)))// &
     &                                 VTD%STRUC(VTD%L_STR)%MAP_FILE(IP+ILEN(C_MAC(J2)):)
                               END IF
                          END IF
                     END IF
 420              CONTINUE 
             END IF
!
! ---------- Check whether the file with source map really exists
!
             INQUIRE ( FILE=VTD%STRUC(VTD%L_STR)%MAP_FILE, EXIST=LEX ) 
             IF ( .NOT. LEX ) THEN
                  CALL CLRCH ( STR ) 
                  CALL INCH  ( J1, STR )
                  IL = ILEN(VTD%STRUC(VTD%L_STR)%MAP_FILE)
                  CALL ERR_LOG ( 2384, IUER, 'VTD_READ_STRUC', 'Cannot '// &
     &                'open file '//VTD%STRUC(VTD%L_STR)%MAP_FILE(1:IL)// &
     &                ' in line '//STR(1:I_LEN(STR))//' of the input '// &
     &                'file with source structure definition '//             &
     &                 VTD%CONF%FINAM_STRUC )
                  DEALLOCATE ( BUF )
                  RETURN 
             END IF
           ELSE 
             CALL CLRCH ( STR ) 
             CALL INCH  ( J1, STR )
             CALL ERR_LOG ( 2385, IUER, 'VTD_READ_STRUC', 'Unrecozngied '// &
     &           'record label '//BUF(J1)(1:3)//' on line '// &
     &            STR(1:I_LEN(STR))//' if the input file with source '// &
     &           'structure defintion '//VTD%CONF%FINAM_STRUC )
             RETURN 
         END IF
 410  CONTINUE 
      DEALLOCATE ( BUF )
!
! --- Now load maps
!
      DO 430 J3=1,VTD%L_SOU
         DO 440 J4=1,VTD%L_STR
            IF ( VTD%SOU(J3)%IVS_NAME == VTD%STRUC(J4)%SOU_NAME ) THEN
                 IF ( .NOT. VTD%SOU(J3)%FL_STRUC(1) ) THEN
                      IBND = 1
                    ELSE IF ( .NOT. VTD%SOU(J3)%FL_STRUC(2) ) THEN
                      IBND = 2
                    ELSE 
                      CALL ERR_LOG ( 2386, IUER, 'VTD_READ_STRUC', 'Attempt '// &
     &                    'to load maps for source '//VTD%SOU(J3)%IVS_NAME// &
     &                    'for more than two band. Maps for bands '// &
     &                    VTD%SOU(J3)%IMAGE_BAND(1)//' '// &
     &                    VTD%SOU(J3)%IMAGE_BAND(2)//' have already been '// &
     &                   'loaded' )
                      RETURN 
                 END IF
!
#ifdef VTD__NO_STRUC
                 VTD%SOU(J3)%IMAGE_BAND(IBND) = '?'
                 VTD%SOU(J3)%FL_STRUC(IBND)   = .FALSE.
                 VTD%SOU(J3)%MAP_USAGE_CODE(IBND) = VTD__NONE
#else

                 IF ( VTD%STRUC(J4)%USAGE_CODE .EQ. STRUC__NON ) THEN
!
! ------------------- No source maps will be used
!
                      VTD%SOU(J3)%IMAGE_BAND(IBND) = '?'
                      VTD%SOU(J3)%FL_STRUC(IBND)   = .FALSE.
                      VTD%SOU(J3)%MAP_USAGE_CODE(IBND) = VTD__NONE
                    ELSE 
!
! ------------------- Read the source map from the FITS file
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL READ_FITS_MAP ( VTD%STRUC(J4)%MAP_FILE, .TRUE., &
     &                                    .TRUE., VTD%SOU(J3)%MAP(IBND), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 2387, IUER, 'VTD_READ_STRUC', &
     &                         'Error '// &
     &                         'in an attempt to load map for source '// &
     &                          VTD%SOU(J3)%IVS_NAME )
                           RETURN 
                      END IF
                      VTD%SOU(J3)%IMAGE_BAND(IBND) = VTD%STRUC(J4)%BAND
                      VTD%SOU(J3)%FL_STRUC(IBND) = .TRUE.
                      VTD%SOU(J3)%MAP_USAGE_CODE(IBND) = &
     &                            VTD%STRUC(VTD%L_STR)%USAGE_CODE 
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL MAP_PROCESS ( VTD%SOU(J3)%MAP(IBND), 1, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 2388, IUER, 'VTD_READ_STRUC', &
     &                         'Error '// &
     &                         'in an attempt to process map for source '// &
     &                          VTD%SOU(J3)%IVS_NAME )
                           RETURN 
                      END IF
                 END IF
#endif
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_READ_STRUC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAP_PROCESS ( MAP, IPAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MAP_PROCESS
! *                                                                      *
! *  ### 21-AUG-2007  MAP_PROCESS  v1.0 (c)  L. Petrov  21-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      TYPE     ( SOUMAP__TYPE ) :: MAP_NEW
      INTEGER*4  IPAR, IUER
      REAL*8     XC, YC, ARG, BEAM_SIZE, NSIG_LEV, NMAX_LEV, BEAM_SCALE
      CHARACTER  STR*32
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, IMAGE_SCALE, &
     &           J11, J12, J13, J14, J15, J16, N_CMP, IPX, IPY, IER
      PARAMETER  ( NSIG_LEV    = 30.0D0 )
      PARAMETER  ( NMAX_LEV    = 0.3D0 )
      PARAMETER  ( BEAM_SCALE  = 1.0D0 )
      PARAMETER  ( IMAGE_SCALE = 1     )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!      
      IF ( IPAR == 1 ) THEN
          DO 4110 J11=1,MAP%NUM_CC
             IPX = MAP%COOR_CC(1,J11)/MAP%STEP_RA + MAP%DIM1/2
             IF ( IPX < 1        ) IPX = 1
             IF ( IPX > MAP%DIM1 ) IPX = MAP%DIM1
!
             IPY = MAP%COOR_CC(2,J11)/MAP%STEP_DL + MAP%DIM2/2
             IF ( IPY < 1        ) IPY = 1
             IF ( IPY > MAP%DIM2 ) IPY = MAP%DIM2
!
!!   write ( 6, * ) 'R1 J11= ', J11, ' flux = ', MAP%FLUX_CC(J11), 'ima= ', MAP%IMAGE(IPX,IPY), NSIG_LEV, MAP%NOISE, ' | ',  NMAX_LEV, MAP%FLUX_MAX ! %%%%%%%%%%%%%%%
!             IF ( MAP%IMAGE(IPX,IPY) < NSIG_LEV*MAP%NOISE    .AND. &
!     &            MAP%IMAGE(IPX,IPY) < NMAX_LEV*MAP%FLUX_MAX       ) THEN
!                  MAP%FLUX_CC(J11) = 0.0D0
!             END IF
!!   write ( 6, * ) 'R2 J11= ', J11, ' flux = ', MAP%FLUX_CC(J11) ! %%%%%%%%%%%%%%%
 4110     CONTINUE 
        ELSE IF ( IPAR == 11 ) THEN
!
! -------- This is test mode. Does not produce good results. Takes a looot of time
!
           MAP_NEW%DIM1     = IMAGE_SCALE*MAP%DIM1
           MAP_NEW%DIM2     = IMAGE_SCALE*MAP%DIM2    
           MAP_NEW%STEP_RA  = MAP%STEP_RA/IMAGE_SCALE
           MAP_NEW%STEP_DL  = MAP%STEP_DL/IMAGE_SCALE
           ALLOCATE ( MAP_NEW%IMAGE(MAP_NEW%DIM1,MAP_NEW%DIM2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*MAP_NEW%DIM1*MAP_NEW%DIM2, STR )
                CALL ERR_LOG ( 4621, IUER, 'MAP_PROCESS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for the image' )
                RETURN 
           END IF
!
           CALL NOUT_R4 ( MAP_NEW%DIM1*MAP_NEW%DIM2, MAP_NEW%IMAGE )
           BEAM_SIZE = BEAM_SCALE*MAP%BEAM_MIN
           DO 410 J1=1,MAP_NEW%DIM1
              XC = MAP_NEW%STEP_RA*(J1 - MAP_NEW%DIM1/2)
              DO 420 J2=1,MAP_NEW%DIM2 
                 YC = MAP_NEW%STEP_DL*(J2 - MAP_NEW%DIM2/2)
                 DO 430 J3=1,MAP%NUM_CC 
!@ IF ( MAP%FLUX_CC(J3) > 0.100 ) GOTO 430
!@ IF ( MAP%FLUX_CC(J3) < 0.080 ) GOTO 430
                    ARG = -(  (  XC - MAP%COOR_CC(1,J3)) &
     &                       -(  YC - MAP%COOR_CC(2,J3)) )**2/ &
     &                        (BEAM_SIZE/DSQRT(2.0D0))**2 &
     &                       -( (XC - MAP%COOR_CC(1,J3)) &
     &                         +(YC - MAP%COOR_CC(2,J3)) )**2/ &
     &                        (BEAM_SIZE/DSQRT(2.0D0))**2
                    IF ( ARG > -15.0D0 ) THEN
                         MAP_NEW%IMAGE(J1,J2) = MAP_NEW%IMAGE(J1,J2) + &
     &                                          MAP%FLUX_CC(J3)*EXP( ARG )
                    END IF
 430             CONTINUE 
 420          CONTINUE 
 410       CONTINUE 
!
           N_CMP = 0
           DO 440 J4=1,MAP_NEW%DIM1
              XC = MAP_NEW%STEP_RA*(J4 - MAP_NEW%DIM1/2)
              DO 450 J5=1,MAP_NEW%DIM2 
                 YC = MAP_NEW%STEP_DL*(J5 - MAP_NEW%DIM2/2)
                 IF ( MAP_NEW%IMAGE(J5,J4) > NSIG_LEV*MAP%NOISE ) THEN
                      N_CMP = N_CMP + 1
                 END IF
 450          CONTINUE 
 440       CONTINUE 
!           
           ALLOCATE ( MAP_NEW%FLUX_CC(N_CMP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*N_CMP, STR )
                CALL ERR_LOG ( 4622, IUER, 'MAP_PROCESS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for the array of new clean components' )
                RETURN 
           END IF
!           
           ALLOCATE ( MAP_NEW%COOR_CC(2,N_CMP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 2*4*N_CMP, STR )
                CALL ERR_LOG ( 4623, IUER, 'MAP_PROCESS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for the array of new coordinates of clean components' )
                RETURN 
           END IF
!
           MAP_NEW%MJD      = MAP%MJD     
           MAP_NEW%TAI      = MAP%TAI     
           MAP_NEW%ALPHA    = MAP%ALPHA   
           MAP_NEW%DELTA    = MAP%DELTA   
           MAP_NEW%FREQ     = MAP%FREQ    
           MAP_NEW%FLUX_MAX = MAP%FLUX_MAX 
           MAP_NEW%FLUX_INT = MAP%FLUX_INT 
           MAP_NEW%FLUX_SHR = MAP%FLUX_SHR 
           MAP_NEW%FLUX_UNR = MAP%FLUX_UNR 
           MAP_NEW%BEAM_MAJ = MAP%BEAM_MAJ 
           MAP_NEW%BEAM_MIN = MAP%BEAM_MIN 
           MAP_NEW%BEAM_POS_ANG = MAP%BEAM_POS_ANG 
           MAP_NEW%NOISE      = MAP%NOISE      
           MAP_NEW%STATUS_MAP = MAP%STATUS_MAP 
           MAP_NEW%STATUS_CC  = MAP%STATUS_CC  
           MAP_NEW%SOU_NAME   = MAP%SOU_NAME
           MAP_NEW%EXP_NAME   = MAP%EXP_NAME
           MAP_NEW%DATE_OBS   = MAP%DATE_OBS
           MAP_NEW%FINAM      = MAP%FINAM
!
           MAP_NEW%NUM_CC = 0
           DO 460 J6=1,MAP_NEW%DIM1
              XC = MAP_NEW%STEP_RA*(J6 - MAP_NEW%DIM1/2)
              DO 470 J7=1,MAP_NEW%DIM2 
                 YC = MAP_NEW%STEP_DL*(J7 - MAP_NEW%DIM2/2)
                 IF ( MAP_NEW%IMAGE(J6,J7) > NSIG_LEV*MAP%NOISE ) THEN
                      MAP_NEW%NUM_CC = MAP_NEW%NUM_CC + 1
                      MAP_NEW%COOR_CC(1,MAP_NEW%NUM_CC) = XC
                      MAP_NEW%COOR_CC(2,MAP_NEW%NUM_CC) = YC
                      MAP_NEW%FLUX_CC(MAP_NEW%NUM_CC)   = MAP_NEW%IMAGE(J6,J7)
                 END IF
 470          CONTINUE 
 460       CONTINUE 
!           
           DEALLOCATE ( MAP%COOR_CC )
           DEALLOCATE ( MAP%FLUX_CC )
           IF ( ASSOCIATED ( MAP%IMAGE ) ) DEALLOCATE ( MAP%IMAGE )
!
           MAP%NUM_CC   = MAP_NEW%NUM_CC 
           MAP%DIM1     = MAP_NEW%DIM1
           MAP%DIM2     = MAP_NEW%DIM2    
           MAP%STEP_RA  = MAP_NEW%STEP_RA
           MAP%STEP_DL  = MAP_NEW%STEP_DL
!           
           ALLOCATE ( MAP%IMAGE(MAP%DIM1,MAP%DIM2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*MAP%DIM1*MAP%DIM2, STR )
                CALL ERR_LOG ( 4624, IUER, 'MAP_PROCESS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for the image' )
                RETURN 
           END IF
!           
           ALLOCATE ( MAP%FLUX_CC(MAP%NUM_CC), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*N_CMP, STR )
                CALL ERR_LOG ( 4625, IUER, 'MAP_PROCESS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for the array of new clean components' )
                RETURN 
           END IF
!           
           ALLOCATE ( MAP%COOR_CC(2,MAP%NUM_CC), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 2*4*N_CMP, STR )
                CALL ERR_LOG ( 4626, IUER, 'MAP_PROCESS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for the array of new coordinates of clean components' )
                RETURN 
           END IF
!
           DO 480 J8=1,MAP%DIM1
              DO 490 J9=1,MAP%DIM2 
                 MAP%IMAGE(J8,J9) = MAP_NEW%IMAGE(J8,J9)
 490          CONTINUE 
 480       CONTINUE 
!
           DO 4100 J10=1,MAP%NUM_CC
              MAP%COOR_CC(1,J10) = MAP_NEW%COOR_CC(1,J10) 
              MAP%COOR_CC(2,J10) = MAP_NEW%COOR_CC(2,J10) 
              MAP%FLUX_CC(J10)   = MAP_NEW%FLUX_CC(J10) 
 4100      CONTINUE 
!
!           WRITE ( 6, * ) ' N_CMP = ', N_CMP, ' NUM_CC = ', MAP%NUM_CC ! %%%
!      WRITE ( 6, * ) ' NUM_CC = ', MAP%NUM_CC ! %%%
!
           DEALLOCATE ( MAP_NEW%COOR_CC )
           DEALLOCATE ( MAP_NEW%FLUX_CC )
           DEALLOCATE ( MAP_NEW%IMAGE   )
      END IF ! ipar
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   MAP_PROCESS   !#!  
