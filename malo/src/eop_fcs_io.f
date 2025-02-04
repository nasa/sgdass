      SUBROUTINE EOP_FCS_WRI ( NERS, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EOP_FCS_WRI
! *                                                                      *
! *  ### 01-APR-2016  EOP_FCS_WRI  v2.2 (c)  L. Petrov  04-DEC-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  FILOUT*(*)
      INTEGER*4  IUER 
      CHARACTER  STR*128
      INTEGER*4  LUN, IS, FCS_STATIC_SIZE, J1, J2, J3, J4, J5, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, WRITE
!
      IF ( NERS%FCS_STATUS .NE. NERS__COMP ) THEN
           CALL ERR_LOG ( 6711, IUER, 'EOP_FCS_WRI', 'Trap of internal '// & 
    &         'control: FCS status is not "computed"' ) 
           RETURN 
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6712, IUER, 'EOP_FCS_WRI', 'Error in opening '// &
     &         'output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write the format label
!
      IS = WRITE ( %VAL(LUN), NERS__BIN_FMT )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6713, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. LEN(NERS__BIN_FMT) ) THEN
           CALL ERR_LOG ( 6714, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
      NERS%FCS%NERS_STATUS = NERS__OK
!
! --- Write static variables
!
      FCS_STATIC_SIZE = LOC(NERS%FCS%NERS_STATUS) - LOC(NERS%FCS) + SIZEOF(NERS%FCS%NERS_STATUS)
      IS = WRITE ( %VAL(LUN), NERS%FCS, %VAL(FCS_STATIC_SIZE) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6715, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. FCS_STATIC_SIZE ) THEN
           CALL ERR_LOG ( 6716, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array ARG_12
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%ARG_12, %VAL(SIZEOF(NERS%FCS%ARG_12)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6717, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_12) ) THEN
           CALL ERR_LOG ( 6718, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array ARG_3
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%ARG_3, %VAL(SIZEOF(NERS%FCS%ARG_3)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6719, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_3) ) THEN
           CALL ERR_LOG ( 6720, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array ARG_C
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%ARG_C, %VAL(SIZEOF(NERS%FCS%ARG_C)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6721, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_C) ) THEN
           CALL ERR_LOG ( 6722, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array ARG_L
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%ARG_L, %VAL(SIZEOF(NERS%FCS%ARG_L)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6723, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_L) ) THEN
           CALL ERR_LOG ( 6724, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array ARG_UTC_M_TAI
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%ARG_UTC_M_TAI, %VAL(SIZEOF(NERS%FCS%ARG_UTC_M_TAI)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6725, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_UTC_M_TAI) ) THEN
           CALL ERR_LOG ( 6726, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array HEO_ARG
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%HEO_ARG, %VAL(SIZEOF(NERS%FCS%HEO_ARG)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6727, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEO_ARG) ) THEN
           CALL ERR_LOG ( 6728, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
!
! -------- Write array HEOR_ARG
!
           IS = WRITE ( %VAL(LUN), NERS%FCS%HEOR_ARG, %VAL(SIZEOF(NERS%FCS%HEOR_ARG)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6729, IUER, 'EOP_FCS_WRI', 'Error '// &
     &               STR(1:I_LEN(STR))//'in writing into binary output '// &
     &              'EOP file '//FILOUT )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEOR_ARG) ) THEN
                CALL ERR_LOG ( 6730, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &              'were written into the output binary EOP file '//FILOUT )
                RETURN 
           END IF
      END IF
!
! --- Write array BSPL_E12
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%BSPL_E12, %VAL(SIZEOF(NERS%FCS%BSPL_E12)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6731, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_E12) ) THEN
           CALL ERR_LOG ( 6732, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array BSPL_E3
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%BSPL_E3, %VAL(SIZEOF(NERS%FCS%BSPL_E3)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6733, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_E3) ) THEN
           CALL ERR_LOG ( 6734, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array BSPL_C
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%BSPL_C, %VAL(SIZEOF(NERS%FCS%BSPL_C)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6735, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_C) ) THEN
           CALL ERR_LOG ( 6736, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array BSPL_L
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%BSPL_L, %VAL(SIZEOF(NERS%FCS%BSPL_L)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6737, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_L) ) THEN
           CALL ERR_LOG ( 6738, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array BSPL_UTC_M_TAI
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%BSPL_UTC_M_TAI, %VAL(SIZEOF(NERS%FCS%BSPL_UTC_M_TAI)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6739, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_UTC_M_TAI) ) THEN
           CALL ERR_LOG ( 6740, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
!
! --- Write array HEO_AMP
!
      IS = WRITE ( %VAL(LUN), NERS%FCS%HEO_AMP, %VAL(SIZEOF(NERS%FCS%HEO_AMP)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6741, IUER, 'EOP_FCS_WRI', 'Error '// &
     &          STR(1:I_LEN(STR))//'in writing into binary output '// &
     &         'EOP file '//FILOUT )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEO_AMP) ) THEN
           CALL ERR_LOG ( 6742, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &         'were written into the output binary EOP file '//FILOUT )
           RETURN 
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
!
! -------- Write array HEOR_AMP
!
           IS = WRITE ( %VAL(LUN), NERS%FCS%HEOR_AMP, %VAL(SIZEOF(NERS%FCS%HEOR_AMP)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6743, IUER, 'EOP_FCS_WRI', 'Error '// &
     &               STR(1:I_LEN(STR))//'in writing into binary output '// &
     &              'EOP file '//FILOUT )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEOR_AMP) ) THEN
                CALL ERR_LOG ( 6744, IUER, 'EOP_FCS_WRI', 'Not all bytes '// &
     &              'were written into the output binary EOP file '//FILOUT )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6745, IUER, 'EOP_FCS_WRI', 'Error in closing '// &
     &         'the output binary EOP file '//FILOUT )
           RETURN 
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EOP_FCS_WRI  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOP_FCS_REA ( FILIN, NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EOP_FCS_REA
! *                                                                      *
! *  ### 01-APR-2016  EOP_FCS_REA  v2.0 (c)  L. Petrov  31-AUG-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  FILIN*(*)
      INTEGER*4  IUER 
      CHARACTER  STR*128
      LOGICAL*1  LEX
      INTEGER*4  LUN, IS, FCS_STATIC_SIZE, J1, J2, J3, J4, LPAR_12, LPAR_3, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6811, IUER, 'EOP_FCS_REA', 'Input binary EOP '// &
     &         'file '//FILIN(1:I_LEN(FILIN))//' was not found' )
           RETURN 
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6812, IUER, 'EOP_FCS_REA', 'Error in opening '// &
     &         'the input binary EOP file '//FILIN )
           RETURN 
      END IF
      IS = READ ( %VAL(LUN), STR(1:LEN(NERS__BIN_FMT)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6813, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//'in reading from binary intput '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. LEN(NERS__BIN_FMT) ) THEN
           CALL ERR_LOG ( 6814, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
      IF ( STR(1:LEN(NERS__BIN_FMT)) .EQ. NERS__BIN_FMT ) THEN
           CONTINUE 
         ELSE 
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 6815, IUER, 'EOP_FCS_REA', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//' has wrong format. The first '// &
     &         'line is '//STR(1:LEN(NERS__BIN_FMT))//' while '// &
     &          NERS__BIN_FMT//' was expected' )
           RETURN 
      END IF
!
! --- Read static variables
!
      FCS_STATIC_SIZE = LOC(NERS%FCS%NERS_STATUS) - LOC(NERS%FCS) + SIZEOF(NERS%FCS%NERS_STATUS)
      IS = READ ( %VAL(LUN), NERS%FCS, %VAL(FCS_STATIC_SIZE) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6816, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//'in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. FCS_STATIC_SIZE ) THEN
           CALL ERR_LOG ( 6817, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
      LPAR_12 =  NERS%FCS%NK_12 + MALO__MDEG - 1
      LPAR_3  =  NERS%FCS%NK_3  + MALO__MDEG - 1
!
! === Allocate memory for dynamic arrays
!
      ALLOCATE ( NERS%FCS%ARG_12(NERS%FCS%NK_12), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6818, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_12' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6819, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_3' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%ARG_C(NERS%FCS%NC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6820, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_C' )
           RETURN
      END IF
      ALLOCATE ( NERS%FCS%ARG_L(NERS%FCS%NL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6821, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_L' )
           RETURN 
      END IF
      ALLOCATE ( NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6821, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_UTC_M_TAI' )
           RETURN 
      END IF
      IF ( NERS%FCS%L_HEO > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEO_ARG(NERS%FCS%L_HEO,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6822, IUER, 'EOP_FCS_REA', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEO_ARG' ) 
                RETURN
           END IF
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEOR_ARG(NERS%FCS%L_HEOR,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6823, IUER, 'EOP_FCS_REA', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEOR_ARG' ) 
                RETURN
           END IF
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_E12(1-NERS__MDEG:NERS%FCS%NK_12-1,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6824, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_E12' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_E3(1-NERS__MDEG:NERS%FCS%NK_3-1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6825, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_E3' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6826, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_C' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_L(1-NERS__MDEG:NERS%FCS%NL-1,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6826, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_L' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_UTC_M_TAI(NERS%FCS%NJ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6827, IUER, 'EOP_FCS_REA', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_UTC_M_TAI' )
           RETURN 
      END IF
      IF ( NERS%FCS%L_HEO > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEO_AMP(NERS%FCS%L_HEO,2,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6828, IUER, 'EOP_FCS_REA', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEO_AMP' ) 
                RETURN
           END IF
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEOR_AMP(NERS%FCS%L_HEOR,2,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6829, IUER, 'EOP_FCS_REA', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEOR_AMP' ) 
                RETURN
           END IF
      END IF
!
! === Read dynamic arrays
!
! --- Read array ARG_12
!
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_12, %VAL(SIZEOF(NERS%FCS%ARG_12)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6830, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_12) ) THEN
           CALL ERR_LOG ( 6831, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
! --- Read array ARG_3
!
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_3, %VAL(SIZEOF(NERS%FCS%ARG_3)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6832, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_3) ) THEN
           CALL ERR_LOG ( 6833, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
! --- Read array ARG_C
!
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_C, %VAL(SIZEOF(NERS%FCS%ARG_C)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4434, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_C) ) THEN
           CALL ERR_LOG ( 4435, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
! --- Read array ARG_L
!
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_L, %VAL(SIZEOF(NERS%FCS%ARG_L)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4434, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_L) ) THEN
           CALL ERR_LOG ( 4435, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
! --- Read array ARG_UTC_M_TAI
!
      NERS%FCS%ARG_UTC_M_TAI = -1.0
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_UTC_M_TAI, %VAL(SIZEOF(NERS%FCS%ARG_UTC_M_TAI)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6834, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_UTC_M_TAI) ) THEN
           CALL ERR_LOG ( 6835, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
      IF ( NERS%FCS%L_HEO > 0 ) THEN
!
! -------- Read array HEO_ARG
!
           IS = READ ( %VAL(LUN), NERS%FCS%HEO_ARG, %VAL(SIZEOF(NERS%FCS%HEO_ARG)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6836, IUER, 'EOP_FCS_REA', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//FILIN )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEO_ARG) ) THEN
                CALL ERR_LOG ( 6837, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//FILIN )
                RETURN 
           END IF
      END IF
!
! --- Read array HEOR_ARG
!
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           IS = READ ( %VAL(LUN), NERS%FCS%HEOR_ARG, %VAL(SIZEOF(NERS%FCS%HEOR_ARG)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6838, IUER, 'EOP_FCS_REA', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//FILIN )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEOR_ARG) ) THEN
                CALL ERR_LOG ( 6839, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//FILIN )
                RETURN 
           END IF
      END IF
!
! --- Read array BSPL_E12
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_E12, %VAL(SIZEOF(NERS%FCS%BSPL_E12)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6840, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_E12) ) THEN
           CALL ERR_LOG ( 6841, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
! --- Read array BSPL_E3
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_E3, %VAL(SIZEOF(NERS%FCS%BSPL_E3)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6842, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_E3) ) THEN
           CALL ERR_LOG ( 6843, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
! --- Read array BSPL_C
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_C, %VAL(SIZEOF(NERS%FCS%BSPL_C)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6844, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_C) ) THEN
           CALL ERR_LOG ( 6845, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
! --- Read array BSPL_L
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_L, %VAL(SIZEOF(NERS%FCS%BSPL_L)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6844, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_L) ) THEN
           CALL ERR_LOG ( 6845, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
! --- Read array UTC_M_TAI
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_UTC_M_TAI, &
     &            %VAL(SIZEOF(NERS%FCS%BSPL_UTC_M_TAI)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6846, IUER, 'EOP_FCS_REA', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//FILIN )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_UTC_M_TAI) ) THEN
           CALL ERR_LOG ( 6847, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
      IF ( NERS%FCS%L_HEO > 0 ) THEN
!
! -------- Read array HEO_AMP
!
           IS = READ ( %VAL(LUN), NERS%FCS%HEO_AMP, %VAL(SIZEOF(NERS%FCS%HEO_AMP)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6848, IUER, 'EOP_FCS_REA', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//FILIN )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEO_AMP) ) THEN
                CALL ERR_LOG ( 6849, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//FILIN )
                RETURN 
           END IF
      END IF
!
! --- Read array HEOR_ARG
!
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
!
! -------- Read array HEOR_AMP
!
           IS = READ ( %VAL(LUN), NERS%FCS%HEOR_AMP, %VAL(SIZEOF(NERS%FCS%HEOR_AMP)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6850, IUER, 'EOP_FCS_REA', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//FILIN )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEOR_AMP) ) THEN
                CALL ERR_LOG ( 6851, IUER, 'EOP_FCS_REA', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//FILIN )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6852, IUER, 'EOP_FCS_REA', 'Error in closing '// &
     &         'the input binary EOP file '//FILIN )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EOP_FCS_REA  !#!  
