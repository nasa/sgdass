      SUBROUTINE DO_SUPR_FILE ( N_PAR, C_PAR, SUPPRESS_FILE, NOR_MAT, &
     &                          NOR_VEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DO_SUPR_FILE  performs suppression of parameters listed    *
! *   in the SUPPRESS_FILE. This file is specified in the $SUPPRESSION   *
! *   section of the batch control file.                                 *
! *                                                                      *
! *   The supression file consists of comments records with characeter # *
! *   at the first postion and parameter deifinition records.            *
! *   Parameter definition record may contaain either 20 characters long *
! *   internal parameter name or, the parameter index.                   *
! *                                                                      *
! *   Routine DO_SUPRE_FILE writes zeroes in the raw and column of the   *
! *   supressed parameterr and after that writes there 1.0D0 .           *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   N_PAR ( INTEGER*4 ) -- The total number of parameters in this      *
! *                          solution.                                   *
! *   C_PAR ( CHARACTER ) -- The list of parameter names. Dimenstion:    *
! *                          N_PAR.                                      *
! * SUPRESS_FILE ( CHARACTER ) -- Name of the file with the list of      *
! *                               suppressed parameters.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * NOR_MAT ( REAL*8         ) -- Normal matrix in the upper triagonal   *
! *                               format.                                *
! * NOR_VEC ( REAL*8         ) -- Normal vector. Dimension: N_PAR.       *
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
! * ### 01-OCT-2007  DO_SUPR_FILE  v1.1 (c)  L. Petrov  14-FEB-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INTEGER*4  N_PAR, IUER 
      CHARACTER  C_PAR(N_PAR)*(*), SUPPRESS_FILE*(*)
      REAL*8     NOR_MAT(*), NOR_VEC(N_PAR)
      CHARACTER  BUF(M_GPA)*128, STR*80
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, J4, N_SUP, L_SUP, IND_PAR, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      INTEGER*8, EXTERNAL :: INDX8
!
      INQUIRE ( FILE = SUPPRESS_FILE, EXIST = LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8271, IUER, 'DO_SUPR_FILE', 'Cannot find '// &
     &         'suppression file '//SUPPRESS_FILE(1:I_LEN(SUPPRESS_FILE))// &
     &         ' specified in Batch control file in $SUPRESSION section' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SUPPRESS_FILE, M_GPA, BUF, N_SUP, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8272, IUER, 'DO_SUPR_FILE', 'Error in reading '// &
     &         'suppression file '//SUPPRESS_FILE(1:I_LEN(SUPPRESS_FILE))// &
     &         ' specified in Batch control file in $SUPRESSION section' )
           RETURN 
      END IF
!
      L_SUP = 0
      DO 410 J1=1,N_SUP
         IF ( ILEN(BUF(J1)) == 0    ) GOTO 410
         IF ( BUF(J1)(1:1)  == '#'  ) GOTO 410
         IF ( BUF(J1)(1:1)  == '"'                          .AND. &
     &        BUF(J1)(I_LEN(BUF(J1)):I_LEN(BUF(J1))) == '"'       ) THEN
!
              BUF(J1)(I_LEN(BUF(J1)):I_LEN(BUF(J1))) = ' '
              BUF(J1) = BUF(J1)(2:)
         END IF
!
         CALL CHIN ( BUF(J1), IND_PAR ) 
         IF ( IND_PAR .GE. 1  .AND.  IND_PAR .LE. N_PAR ) THEN
              CONTINUE 
            ELSE 
              IND_PAR = LTM_DIF ( 0, N_PAR, C_PAR, BUF(J1) )
         END IF
!
         IF ( IND_PAR .LE. 0   .OR. &
     &        IND_PAR > N_PAR       ) THEN
!
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8273, IUER, 'DO_SUPR_FILE', 'Cannot find '// &
     &            'parameter '//BUF(J1)(1:I_LEN(BUF(J1)))// &
     &            ' specified at the '//STR(1:I_LEN(STR))// &
     &            ' th line of the suppression file '// &
     &             SUPPRESS_FILE(1:I_LEN(SUPPRESS_FILE))// &
     &            ' specified in Batch control file in $SUPRESSION '// &
     &            'section' )
              RETURN 
         END IF
!
         DO 420 J2=1,N_PAR
            NOR_MAT(INDX8(J2,IND_PAR)) = 0.0D0
 420     CONTINUE 
         NOR_MAT(INDX8(IND_PAR,IND_PAR)) = 1.0D0
         NOR_VEC(IND_PAR) = 0.0D0
!
         L_SUP = L_SUP + 1
         WRITE ( 23, 110 ) L_SUP, IND_PAR, C_PAR(IND_PAR)
 110     FORMAT ( ' DO_SUPR_FILE: ',I5,') suppressed parameter ind: ', &
     &            I5, 2X, A )
 410  CONTINUE 
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DO_SUPR_FILE  !#!#
