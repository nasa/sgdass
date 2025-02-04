      SUBROUTINE SINEX_PARSER ( STRING, FL_SINEX_MAKE, FL_OVERWRITE, &
     &                FL_SINEX_GLO, FL_SINEX_LOC, FL_SINEX_SEG,   &
     &                FL_SINEX_EST,  FL_SINEX_COV, FL_SINEX_CNS,  &
     &                FL_SINEX_DCM, SINEX_OUTFILE, SINEX_INCLUDE, &
     &                SINEX_EXCLUDE, SINEX_ACKFIL, SINEX_COMFIL,  &
     &                SINEX_VERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SINEX_PARSER parses keyword SINEX of the $OUTPUT section   *
! *   of a batch control file.                                           *
! *                                                                      *
! *  ### 27-MAR-2002  SINEX_PARSER  v1.2 (c) L. Petrov  12-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IUER
      CHARACTER  STRING*(*), SINEX_OUTFILE*(*), SINEX_INCLUDE*(*), &
     &           SINEX_EXCLUDE*(*), SINEX_ACKFIL*(*), SINEX_COMFIL*(*), &
     &           SINEX_VERS*(*)
      LOGICAL*2  FL_SINEX_MAKE, FL_OVERWRITE, FL_SINEX_GLO, FL_SINEX_LOC, &
     &           FL_SINEX_SEG,  FL_SINEX_EST, FL_SINEX_COV, FL_SINEX_CNS, &
     &           FL_SINEX_DCM
      CHARACTER  TOKEN*128
      INTEGER*4  M_QUA
      PARAMETER  ( M_QUA = 14 )
      CHARACTER  QUAL_WORD(M_QUA)*32, QUAL_VALUE(M_QUA)*128, OUT*1024
      INTEGER*4  QUAL_TYPE(M_QUA), ANS__TYP, IFL__TYP, OFL__TYP, STR__TYP
      PARAMETER  ( ANS__TYP = 2673 )
      PARAMETER  ( IFL__TYP = 6782 )
      PARAMETER  ( OFL__TYP = 7543 )
      PARAMETER  ( STR__TYP = 9832 )
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, IW, N1, IP
      DATA       ( QUAL_WORD(N1), QUAL_TYPE(N1), N1=1,M_QUA) &
     &           / &
     &              'ACKNOWLEDGMENTS_FILE            ', IFL__TYP, &
     &              'ALLOW_OVERWRITE                 ', ANS__TYP, &
     &              'COMMENTS_FILE                   ', IFL__TYP, &
     &              'CONSTRAINTS                     ', ANS__TYP, &
     &              'COVARIANCES                     ', ANS__TYP, &
     &              'DECOMPOSED_NORMAL_EQUATIONS     ', ANS__TYP, &
     &              'ESTIMATES                       ', ANS__TYP, &
     &              'EXCLUDE_PARAM                   ', IFL__TYP, &
     &              'FORMAT_VERSION                  ', STR__TYP, &
     &              'GLOBAL                          ', ANS__TYP, &
     &              'INCLUDE_PARAM                   ', IFL__TYP, &
     &              'LOCAL                           ', ANS__TYP, &
     &              'OUTPUT_FILE                     ', OFL__TYP, &
     &              'SEGMENTED                       ', ANS__TYP  &
     &           /
      LOGICAL*4  FL_QUAL(M_QUA), FL_ANS(M_QUA)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'YES' ) THEN
           FL_SINEX_MAKE = .TRUE.
         ELSE
           FL_SINEX_MAKE = .FALSE.
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      DO 410 J1=1,M_QUA
         FL_QUAL(J1) = .FALSE.
         FL_ANS(J1)  = .FALSE.
 410  CONTINUE
!
! --- Get arguments
!
      DO 420 J2=1,M_QUA
         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         IF ( ILEN(TOKEN) .EQ. 0 ) THEN
              WRITE ( 6, * ) ' Word ', J2
              CALL LIST_TO_LINE ( M_QUA, QUAL_WORD, ',', OUT )
              CALL ERR_LOG ( 8271, IUER, 'SINEX_PARSER', 'Not sufficient '// &
     &            'number of qualifiers of SINEX keyword were supploed. '// &
     &            'The list of supported qualifiers: '//OUT )
              RETURN
         END IF
!
         IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, TOKEN )
         IF ( IW .LE. 0 ) THEN
              CALL LIST_TO_LINE ( M_QUA, QUAL_WORD, ',', OUT )
              CALL ERR_LOG ( 8272, IUER, 'SINEX_PARSER', 'Wrong qualifier of '// &
     &            'SINEX keyword: '//TOKEN(1:I_LEN(TOKEN))//' . The list of '// &
     &            'supported qualifiers: '//OUT )
              RETURN
         END IF
!
         IF ( FL_QUAL(IW) ) THEN
              CALL ERR_LOG ( 8273, IUER, 'SINEX_PARSER', 'Attempt to use '// &
     &            'qualifier of SINEX keyword: '//TOKEN(1:I_LEN(TOKEN))// &
     &            ' twice' )
              RETURN
         END IF
         FL_QUAL(IW) = .NOT. FL_QUAL(IW)
         CALL SPLIT_STRING ( STRING, QUAL_VALUE(IW), STRING )
 420  CONTINUE
!
! --- Check values
!
      DO 430 J3=1,M_QUA
         IF ( QUAL_TYPE(J3) .EQ. ANS__TYP ) THEN
!
! ----------- Aga. It was YES/NO type
! ----------- Translate letters to upper register
!
              CALL TRAN ( 11, QUAL_VALUE(J3), QUAL_VALUE(J3) )
              IF ( QUAL_VALUE(J3) .EQ. 'YES' ) THEN
                   FL_ANS(J3) = .TRUE.
                 ELSE IF ( QUAL_VALUE(J3) .EQ. 'NO' ) THEN
                   FL_ANS(J3) = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 8274, IUER, 'SINEX_PARSER', 'Wrong value '// &
     &                 'of the qualifer "'// &
     &                  QUAL_WORD(J3)(1:I_LEN(QUAL_WORD(J3)))// &
     &                 '" -- '//QUAL_VALUE(J3)(1:I_LEN(QUAL_VALUE(J3)))// &
     &                 ' -- YES or NO expected' )
                   RETURN
              END IF
            ELSE IF ( QUAL_TYPE(J3) .EQ. IFL__TYP ) THEN
!
! ----------- Oh! IUt was "input file" type. Try to open it
!
              INQUIRE ( FILE=QUAL_VALUE(J3), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 8275, IUER, 'SINEX_PARSER', 'Wrong value '// &
     &                 'of the qualifer "'// &
     &                  QUAL_WORD(J3)(1:I_LEN(QUAL_WORD(J3)))// &
     &                 '" -- file '//QUAL_VALUE(J3)(1:I_LEN(QUAL_VALUE(J3)))// &
     &                 ' was not found' )
                   RETURN
              END IF
            ELSE IF ( QUAL_TYPE(J3) .EQ. OFL__TYP ) THEN
!
! ----------- Eh-eh. It is the output file type. Nothing to check
!
              CONTINUE
            ELSE IF ( QUAL_TYPE(J3) .EQ. STR__TYP ) THEN
!
! ----------- U-uuuu... It is the string type. Nothing to check
!
              CONTINUE
         END IF
 430  CONTINUE
!
! --- Finally copy extracted values to the output arguments
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'ACKNOWLEDGMENTS_FILE' )
      SINEX_ACKFIL = QUAL_VALUE(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'ALLOW_OVERWRITE' )
      FL_OVERWRITE = FL_ANS(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'COMMENTS_FILE' )
      SINEX_COMFIL = QUAL_VALUE(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'CONSTRAINTS' )
      FL_SINEX_CNS = FL_ANS(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'COVARIANCES' )
      FL_SINEX_COV = FL_ANS(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'DECOMPOSED_NORMAL_EQUATIONS' )
      FL_SINEX_DCM = FL_ANS(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'ESTIMATES' )
      FL_SINEX_EST = FL_ANS(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'EXCLUDE_PARAM' )
      SINEX_EXCLUDE = QUAL_VALUE(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'GLOBAL' )
      FL_SINEX_GLO = FL_ANS(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'INCLUDE_PARAM' )
      SINEX_INCLUDE = QUAL_VALUE(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'LOCAL' )
      FL_SINEX_LOC = FL_ANS(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'FORMAT_VERSION' )
      SINEX_VERS = QUAL_VALUE(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'OUTPUT_FILE' )
      SINEX_OUTFILE = QUAL_VALUE(IW)
!
      IW = LTM_DIF ( 0, M_QUA, QUAL_WORD, 'SEGMENTED' )
      FL_SINEX_SEG = FL_ANS(IW)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SINEX_PARSER  #!#
