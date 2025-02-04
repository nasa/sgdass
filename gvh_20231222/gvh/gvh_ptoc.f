      SUBROUTINE GVH_PTOC ( GVH, LCODE, TYP, CLASS, DIM1, DIM2, DESCR, ISEG, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_PTOC  adds the definition of the new lcode to the     *
! *   table of contents of the ISEG-th segment.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    LCODE ( CHARACTER ) -- 8-characters long LCODE name.              *
! *      TYP ( INTEGER*4 ) -- LCODE type. The following types are        *
! *                           supported:                                 *
! *                           GVH__C1                                    *
! *                           GVH__I2                                    *
! *                           GVH__I4                                    *
! *                           GVH__R4                                    *
! *                           GVH__R8                                    *
! *    CLASS ( INTEGER*4 ) -- LCODE class. The following classes are     *
! *                           supported:                                 *
! *                           GVH__SES                                   *
! *                           GVH__SCA                                   *
! *                           GVH__STA                                   *
! *                           GVH__BAS                                   *
! *     DIM1 ( INTEGER*4 ) -- First dimension of the data array          *
! *                           associated with the array.                 *
! *     DIM2 ( INTEGER*4 ) -- Second dimension of the data array         *
! *                           associated with the array.                 *
! *     ISEG ( INTEGER*4 ) -- Index of the segment where the lcode       *
! *                           definition will be added. The index should *
! *                           be in the range of available segments.     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GVH ( GVH__STRU      ) -- Data structure which keeps internal    *
! *                               information related to the database of *
! *                               an astro/geo VLBI experiment.          *
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
! *  ### 21-NOV-2001     GVH_PTOC    v2.2 (c) L. Petrov 12-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE ( GVH__STRU ) ::  GVH
      CHARACTER  LCODE*(*), DESCR*(*)
      INTEGER*4  TYP, CLASS, DIM1, DIM2, ISEG, IUER
      CHARACTER  STR*32, STR1*32, LCODE_STR*8, DESCR_STR*80
      INTEGER*4  J1, J2, J3, J4, NT, I1, I2, I3, I4, IP, DIMS(2), IER
      ADDRESS__TYPE :: IADR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4041, IUER, 'GVH_PTOC', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( ISEG > GVH%NSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH%NSEG, STR1 )
           CALL ERR_LOG ( 4042, IUER, 'GVH_PTOC', 'The segment index '// &
     &         'parameter ISEG: '//STR(1:I_LEN(STR))//' execeeds the '// &
     &         'maximum number of segments speficied for this experiment: '// &
     &         STR1 )
           RETURN
      END IF
!
      IF ( ISEG .LE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4043, IUER, 'GVH_PTOC', 'Wrong segment index, '// &
     &         'parameter ISEG: '//STR )
           RETURN
      END IF
!
      IF ( GVH%TOCS(ISEG)%STATUS .NE. GVH__INITIALIZED ) THEN
           GVH%TOCS(ISEG)%LEN = GVH__MTOC*GVH__LCODE1_LEN
           IER = IUER
           CALL GVH_ALLOCATE ( GVH, GVH%TOCS(ISEG)%LEN, &
     &                              GVH%TOCS(ISEG)%ADR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4044, IUER, 'GVH_PTOC', 'The error in '// &
     &              'attempt to allocate dynamic memory for the table '// &
     &              'of contents' )
                RETURN
           END IF
           GVH%TOCS(ISEG)%STATUS = GVH__INITIALIZED
      END IF
!
      DO 410 J1=1,GVH%NSEG
         DO 420 J2=1,GVH%TOCS(J1)%NTOC
            CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(J1)%ADR), &
     &           J2, GVH__GET, LCODE_STR, DESCR_STR,   &
     &           I1, I2, DIMS, I3, IADR, IER )
            IF ( LCODE_STR .EQ. LCODE ) THEN
                 WRITE ( 6, * ) 'Segment: ', J1,' Lcode_index: ', J2
                 CALL ERR_LOG ( 4045, IUER, 'GVH_PTOC', 'Lcode '//LCODE// &
     &               ' is already defined' )
                 RETURN
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC + 1
      IF ( GVH%TOCS(ISEG)%NTOC .GT. GVH__MTOC ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( GVH__MTOC, STR )
           CALL ERR_LOG ( 4046, IUER, 'GVH_PTOC', 'The number of '// &
     &         'lcodes in the table of content exceeded the current limit '// &
     &         'GVH__MTOC: '//STR )
           RETURN
      END IF
      IF ( GVH%TOCS(ISEG)%NTOC*GVH__LCODE1_LEN .GT. GVH%TOCS(ISEG)%LEN ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( GVH__MTOC, STR )
           CALL ERR_LOG ( 4047, IUER, 'GVH_PTOC', 'Trap of inernal control: '// &
     &         'TOCS table size is too small and cannot be extended' )
           RETURN
      END IF
!
      IF ( GVH%TEMP_FLAG  .NE.  GVH__MANDATORY ) THEN
           IP = LTM_DIF ( 1, GVH__NMLCODE, GVH__MLCODE, LCODE )
           IF ( IP .GT. 0 ) THEN
                CALL ERR_LOG ( 4048, IUER, 'GVH_PTOC', 'Lcode '//LCODE// &
     &              ' is mandatory. You can put mandatory lcode into the '// &
     &              'database only by using GVH_PREPUT' )
                GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
                RETURN
           END IF
           IF ( ISEG == 1  .AND.  GVH%TOCS(ISEG)%NTOC .EQ. 1 ) THEN
!
! ------------- Now we reserve place for GVH__NMLCODE mandatory lcodes
!
                DIMS(1) = 0
                DIMS(2) = 0
                IADR = 0
                DO 430 J3=1,GVH__NMLCODE
                   CALL GVH_EXCH_LCODE1 ( .FALSE.,                         &
     &                                    %VAL(GVH%TOCS(ISEG)%ADR), J3, &
     &                                    GVH__PUT, GVH__RESERVED, ' ',    &
     &                                    GVH__UNDEFINED, GVH__UNDEFINED,  &
     &                                    DIMS, 0, IADR, IER )
 430            CONTINUE
                GVH%TOCS(ISEG)%NTOC = GVH__NMLCODE + 1 ! set tocs counter
           END IF
      END IF
!
      NT = GVH%TOCS(ISEG)%NTOC
      IF ( NT .GT. 0 ) THEN
           DO 440 J4=1,NT-1
              CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), &
     &                               J4, GVH__GET, LCODE_STR, DESCR_STR,   &
     &                               I1, I2, DIMS, I3, IADR, IER )
              IF ( LCODE_STR .EQ. LCODE ) THEN
                   WRITE ( 6, * ) ' J4',J4, ' NT=',NT, ' ISEG=', ISEG
                   CALL ERR_LOG ( 4049, IUER, 'GVH_PTOC', 'Lcode '//LCODE// &
     &                 ' is already defined' )
                   GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
                   RETURN
              END IF
 440       CONTINUE
      END IF
!
! --- Check arguments
!
      IF ( ILEN(LCODE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4050, IUER, 'GVH_PTOC', 'Attempt to insert empty '// &
     &                    'lcode' )
           GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
           RETURN
      END IF
!
      IF ( ILEN(DESCR) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4051, IUER, 'GVH_PTOC', 'Attempt to insert empty '// &
     &                    'description' )
           GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
           RETURN
      END IF
!
      IF ( .NOT. ( TYP .EQ. GVH__C1 .OR. &
     &             TYP .EQ. GVH__I2 .OR. &
     &             TYP .EQ. GVH__I4 .OR. &
     &             TYP .EQ. GVH__R4 .OR. &
     &             TYP .EQ. GVH__R8      ) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( TYP, STR )
           GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
           CALL ERR_LOG ( 4052, IUER, 'GVH_PTOC', 'GVH does not support type '// &
     &                    STR )
           RETURN
      END IF
!
      IF ( .NOT. ( CLASS .EQ. GVH__SES .OR. &
     &             CLASS .EQ. GVH__SCA .OR. &
     &             CLASS .EQ. GVH__STA .OR. &
     &             CLASS .EQ. GVH__BAS      ) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( TYP, STR )
           GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
           CALL ERR_LOG ( 4053, IUER, 'GVH_PTOC', 'GVH does not support '// &
     &                    'class '//STR )
           RETURN
      END IF
!
      IF ( DIM1 .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM1, STR )
           GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
           CALL ERR_LOG ( 4054, IUER, 'GVH_PTOC', 'Wrong value of argument '// &
     &                   'DIM1: '//STR )
           RETURN
      END IF
!
      IF ( DIM2 .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM2, STR )
           GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
           CALL ERR_LOG ( 4055, IUER, 'GVH_PTOC', 'Wrong value of argument '// &
     &                   'DIM2: '//STR )
           RETURN
      END IF
!
      IF ( DIM1 .LT. 0   .AND.  DIM2 .GT.0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM2, STR )
           CALL ERR_LOG ( 4056, IUER, 'GVH_PTOC', 'Wrong value of argument '// &
     &                   'DIM2: '//STR(1:I_LEN(STR))//' since DIM1 < 0, '// &
     &                   'DIM2 should be < 0 also' )
           RETURN
      END IF
!
      IF ( DIM2 .LT. 0  .AND.  DIM1 .GT. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM1, STR )
           GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC - 1
           CALL ERR_LOG ( 4057, IUER, 'GVH_PTOC', 'Wrong value of argument '// &
     &                   'DIM1: '//STR(1:I_LEN(STR))//' since DIM2 < 0, '// &
     &                   'DIM1 should be > 0 also' )
           RETURN
      END IF
!
! --- All checks are over, now let's put lcode
!
      DIMS(1) = DIM1
      DIMS(2) = DIM2
      IADR = 0
!
      IER = IUER
      CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), NT, &
     &                       GVH__PUT, LCODE, DESCR, CLASS, TYP, DIMS, &
     &                       0, IADR, IER )
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_PTOC  !#!#
