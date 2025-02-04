      SUBROUTINE VTD_LOAD_NZO ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD_NZO  
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
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
! *  ### 16-APR-2023   VTD_LOAD_NZO  v1.2 (c) L. Petrov  02-MAY-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      CHARACTER  STR*128, STR1*128, STR2*128, VTD_DIFX_COMPAT*3
      ADDRESS__TYPE :: DIR_DESC(16)
      CHARACTER  FILNAM*128, NZO_NAME*8, CENTER_NAME*32, REF_NAME*32, &
     &           OBJ_TYPE*8, OBJ_CODE*1, OBJ_USED*1, SOU_ANT_NAME*3
      REAL*8     TIM_ARR(VTD__MEPH), POS_ARR(VTD__MEPH,3), VEL_ARR(VTD__MEPH,3), &
     &           TIM_INT, DST, TIM_INT_INTRP
      INTEGER*4  TIM_CODE, COO_CODE, NZO_REF
      INTEGER*4  NZO_DEG, NUM_ANTENNAS
      PARAMETER  ( NZO_DEG = 3 )
      LOGICAL*1  FL_SMO_SPLINE, DO_PCO, IS_UNIQUE
      INTEGER*4  J1, J2, J3, J4, J5, J6, LEV, IS, MJD_ARR(VTD__MEPH), L_ARR, L_NOD, IER
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, I_LEN, ILEN, LTM_DIF
      CHARACTER*20, ANTENNA_TYPES(VTD__M_PCO)
!
! --- By default an interpolation spline is computed.
! --- If FL_SMO_SPLINE is equale YES, smoothing spline is computed
!
      FL_SMO_SPLINE = .FALSE.
      CALL GETENVAR ( 'VTD_NZO_SMO', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_SMO_SPLINE = .TRUE.
      END IF
      IF ( ILEN(VTD%CONF%DIR_NZO) == 0 ) THEN
           CALL ERR_LOG ( 2431, IUER, 'VTD_LOAD_NZO', 'Trap of internal '// &
     &         'control: DIR_NZO was specified as NONE in the VTD '// &
     &         'control file '//VTD%CONF%CONFIG_FINAM )
           RETURN 
      END IF
!
! --- A cycle over all files in the NZO directory
!
      DO_PCO = .FALSE.
      LEV = 0
      DO 410 J1=1,1024*1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, VTD%CONF%DIR_NZO, FILNAM )
         IF ( LEV == 0 ) GOTO 810
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 2432, IUER, 'VTD_LOAD_NZO', 'Error in '// &
     &            'reading directory '//TRIM(VTD%CONF%DIR_NZO)// &
     &            ' wit the ephemerides of near zone objects: '//FILNAM )
              RETURN 
         END IF
         IF ( INDEX ( FILNAM, '#' ) > 0    ) GOTO 410
         IF ( INDEX ( FILNAM, '~' ) > 0    ) GOTO 410
!
         CALL ERR_PASS     ( IUER, IER )
         CALL VTD_READ_NZO ( FILNAM, VTD__MEPH, L_ARR, MJD_ARR, &
     &                       TIM_ARR, POS_ARR, VEL_ARR, NZO_NAME, OBJ_TYPE, &
     &                       CENTER_NAME, REF_NAME, TIM_CODE, COO_CODE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2432, IUER, 'VTD_LOAD_NZO', 'Error in an '// &
     &            'attempt to read NZO data into VTD data structure' )
              CALL EXIT ( 1 ) 
         END IF
         IF ( OBJ_TYPE == VTD__GEO_SAT ) THEN
              OBJ_CODE = VTD__ES
              DO_PCO = .TRUE.
            ELSE IF ( OBJ_TYPE == VTD__MOO_ORB ) THEN
              OBJ_CODE = VTD__SS
            ELSE IF ( OBJ_TYPE == VTD__MOO_LND ) THEN
              OBJ_CODE = VTD__SS
            ELSE IF ( OBJ_TYPE == VTD__PLA_ORB ) THEN
              OBJ_CODE = VTD__SS
            ELSE IF ( OBJ_TYPE == VTD__PLA_LND ) THEN
              OBJ_CODE = VTD__SS
            ELSE IF ( OBJ_TYPE == VTD__SOL_ORB ) THEN
              OBJ_CODE = VTD__SS
         END IF
!
         TIM_INT = (MJD_ARR(L_ARR) - MJD_ARR(1))*86400.0D0 + (TIM_ARR(L_ARR) - TIM_ARR(1))
         IF ( FL_SMO_SPLINE ) THEN
              DST = DSQRT ( POS_ARR(1,1)**2 + POS_ARR(1,2)**2 + POS_ARR(1,3)**2 )
              IF ( DST < 7.D6 ) THEN
                   TIM_INT_INTRP = 50.0D0
                   OBJ_USED = VTD__ES
                 ELSE IF ( DST < 45.D6 ) THEN
                   TIM_INT_INTRP = 100.0D0
                   OBJ_USED = VTD__ES
                 ELSE IF ( DST < 430.D6 ) THEN
                   TIM_INT_INTRP = 500.0D0
                   OBJ_USED = VTD__SS
                 ELSE
                   TIM_INT_INTRP = 3000.0D0
                   OBJ_USED = VTD__SS
              END IF
              L_NOD = IDNINT( TIM_INT/TIM_INT_INTRP ) + 1
              IF ( L_NOD  < 8 ) L_NOD = 8
            ELSE
              L_NOD = L_ARR 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_LOAD_OBJ_NZO ( NZO_NAME, OBJ_CODE, OBJ_USED, NZO_REF, TIM_CODE, &
     &                           VTD, L_ARR, MJD_ARR, TIM_ARR, POS_ARR, L_NOD, &
     &                           NZO_DEG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2433, IUER, 'VTD_LOAD_NZO', 'Error in an '// &
     &            'attempt to load NZO data of object '//TRIM(NZO_NAME)// &
     &            ' from ephemeride file '//TRIM(FILNAM)//' into VTD '// &
     &            'data structure ' )
              RETURN
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
!
! --- If we have a GNSS satellite, we need to build PCO/PCV objects,
! --- to do this, we need to get the antenna types of stations and sources
!
      IF ( DO_PCO .EQV. .TRUE. ) THEN
           NUM_ANTENNAS = 0
           DO 420 J2=1,VTD%L_STA
!
! ----------- First get receiver antenna types
!
              IF ( VTD%STA(J2)%MOUNT_TYPE == 'GNSS' ) THEN
!
! ---------------- Extract the antenna types
!  
                   IS_UNIQUE = .TRUE.
                   DO 430 J3=1,NUM_ANTENNAS
                       IF ( TRIM(VTD%STA(J2)%ANTEX_TYPE) == TRIM(ANTENNA_TYPES(J3)) ) THEN
                            IS_UNIQUE = .FALSE.
                            GOTO 830
                       END IF
 430               CONTINUE
 830               CONTINUE
                   IF ( IS_UNIQUE .EQV. .TRUE. ) THEN
                        IF ( NUM_ANTENNAS == VTD__M_PCO ) THEN
                             CALL ERR_LOG ( 2434, IUER, 'VTD_LOAD_NZO', 'Error in an '// &
     &                           'attempt to load PCO/PCV data into VTD: '// &
     &                           'too many antenna types, either reduce '//  &
     &                           'number of antennas or increase VTD__M_PCO' )
                             RETURN
                        END IF
                        NUM_ANTENNAS = NUM_ANTENNAS + 1
                        ANTENNA_TYPES(NUM_ANTENNAS) = TRIM ( VTD%STA(J2)%ANTEX_TYPE )
                   END IF
              END IF
 420       CONTINUE
!
           DO 450 J5=1,VTD%L_NZO
!
! ----------- Now get source/transmitter antenna types
!
              IS_UNIQUE = .TRUE.
              SOU_ANT_NAME = VTD%NZO(J5)%NAME
              DO 460 J6=1,NUM_ANTENNAS
                 IF ( TRIM(SOU_ANT_NAME) == TRIM (ANTENNA_TYPES(J6)) ) THEN
                      IS_UNIQUE = .FALSE.
                      GOTO 860
                 END IF
 460          CONTINUE
 860          CONTINUE
              IF ( IS_UNIQUE .EQV. .TRUE. ) THEN
                   NUM_ANTENNAS = NUM_ANTENNAS + 1
                   ANTENNA_TYPES(NUM_ANTENNAS) = SOU_ANT_NAME
              END IF
 450       CONTINUE
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_READ_ANTEX ( VTD, ANTENNA_TYPES(1:NUM_ANTENNAS), NUM_ANTENNAS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2435, IUER, 'VTD_LOAD_NZO', 'Error in an '// &
     &              'attempt to load PCO/PCV data into VTD '// &
     &              'data structure' )
                RETURN
           END IF
      END IF
      VTD%L_ANT_TYPE = NUM_ANTENNAS
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_NZO  !#!#
