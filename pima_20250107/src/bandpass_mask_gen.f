      PROGRAM    BANDPASK_MASK_GEN
! ************************************************************************
! *                                                                      *
! *   Routine  BANDPASK_MASK_GEN
! *                                                                      *
! *  ### 05-FEB-2009               v1.0 (c)  L. Petrov  05-FEB-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE        ), POINTER :: PIM(:)
      TYPE     ( PIM_CONF__TYPE    )          :: CONF
      CHARACTER  STR*128
      CHARACTER  FIL_PIM_CNT*128, FIL_GEN*128, FIL_OUT*128, FIL_PIM*128
      CHARACTER  KEYWORD(PIM__MOPT)*80, VALUE(PIM__MOPT)*80
      INTEGER*4  IUER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( IARGC () < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: {pima_control} {mask_gen_control} {output_mask_file}'
           CALL EXIT ( 1 )
         ELSE 
            CALL GETARG ( 1, FIL_PIM_CNT ) 
            CALL GETARG ( 2, FIL_GEN ) 
            CALL GETARG ( 3, FIL_OUT ) 
      END IF
!
      ALLOCATE ( PIM(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(PIM(1)), STR )
           CALL ERR_LOG ( 4601, -2, 'BANDPASS_MASK_GEN', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for PIMA internal data structures. What is going on? '// &
     &         'Do you really have so few memory?' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(PIM), PIM )
!
! --- Read configuration file
!
      IUER = -1
      CALL PIMA_CONF ( FIL_PIM_CNT, PIM, 0, KEYWORD, VALUE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4602, -2, 'BANDPASS_MASK_GEN', 'Error '// &
     &         'in parsing pima control file '//FIL_PIM_CNT )
           CALL EXIT ( 1 ) 
      END IF
!
      CONF = PIM(1)%CONF  ! Save confgirutaton
      FIL_PIM = CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'//&
     &          CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.pim'
!
      IUER = -1
      CALL PIMA_READ_OBS ( FIL_PIM, CONF, PIM, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4603, -2, 'BANDPASS_MASK_GEN', 'Error '// &
     &         'in an attempt to read pima file '//FIL_PIM )
           CALL EXIT ( 1 ) 
      END IF
      PIM(1)%CONF = CONF ! Save confgirutaton
!
      IUER = -1
      CALL PARSE_BPS_GEN_CNT ( PIM, FIL_GEN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4604, -2, 'BANDPASS_MASK_GEN', 'Error '// &
     &         'in an attempt to parse bandpass generation file '//FIL_GEN )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL WRI_BANDPASS_MASK ( PIM, FIL_GEN, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4605, -2, 'BANDPASS_MASK_GEN', 'Error '// &
     &         'in an attempt to write bandpass mask file '//FIL_OUT )
           CALL EXIT ( 1 ) 
      END IF
!
      WRITE (6, '(A)' ) 'Output file: '//FIL_OUT(1:I_LEN(FIL_OUT))
!
      END  PROGRAM    BANDPASK_MASK_GEN  !#!#
