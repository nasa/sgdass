      SUBROUTINE EHEO_CLS_CNST ( L_PAR, C_PAR, L_EHEO, EHEO, L_EHEC, EHEC, &
     &                           CNSTROBJ, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine  EHEO_CLS_CNST  imposes decorrelation constraints between  *
! *   constituents of the Harmonic variations in the Earth rotation with *
! *   closed frequencies. Considering the amplitude of the Earth         *
! *   rotation E1, E2 is expressed in complex notation E1 + i E2,        *
! *   the constraint equation is written as                              *
! *                                                                      *
! *      Pc1 + i Ps1     Ac1 + i Ac1                                     *
! *      -----------  =  -----------                                     *
! *      Pc2 + i Ps2     Ac2 + i As2                                     *
! *                                                                      *
! *   This results in two equations per constraint which bounds four     *
! *   parameters: one equation for the real part and another equation    *
! *   for the image part:                                                *
! *                                                                      *
! *   Ac2 * Pc1  -  As2 * Ps1  -  Ac1 * Pc2  +  Ac1 * Ps2 = 0            *
! *   As2 * Pc1  +  Ac2 * Ps1  -  As1 * Pc2  -  Ac1 * Ps2 = 0            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       L_PAR ( INTEGER*4  ) -- The total number of global parameter.  *
! *       C_PAR ( INTEGER*4  ) -- The list of global parameters.         *
! *      L_EHEO ( INTEGER*4  ) -- The number of frequencies for which    *
! *                               harmonic variations in Earth's         *
! *                               rotation are computed.                 *
! *        EHEO ( EHEO__TYPE ) -- Array Derived object defined in        *
! *                               $PSOLVE_ROOT/include/solve.i which     *
! *                               keeps information about estimation of  *
! *                               the harmonic variations in the Earth's *
! *                               orientation. Dimension: L_EHEO.        *
! *      L_EHEC ( INTEGER*4  ) -- The number of pairs of constituents    *
! *                               in harmonic variations in Earth's      *
! *                               rotation with close frequencies.       *
! *        EHEC ( EHEC__TYPE ) -- Array of derived objects defined in    *
! *                               $PSOLVE_ROOT/include/solve.i which     *
! *                               keeps information about constraints    *
! *                               imposed on harmonic variations in the  *
! *                               Earth's orientation. Dimension: L_EHEC.*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    CNSTROBJ  ( RECORD    ) -- Object which accumulates information   *
! *                               about constraints: names, coefficient  *
! *                               of constraint equations, etc.          *
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
! * ### 29-JUN-2006   EHEO_CLS_CNST  v1.1 (c) L. Petrov 24-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  L_EHEO, L_EHEC, L_PAR, IUER
      TYPE     ( EHEO__TYPE  ) :: EHEO(L_EHEO)
      TYPE     ( EHEC__TYPE  ) :: EHEC(L_EHEC)
      TYPE     ( CNSTR__STRU ) :: CNSTROBJ
      CHARACTER  C_PAR(L_PAR)*(*)
      CHARACTER  PAR_NAME(2,2)*20, CNS_ABR(2)*8, STR*80, &
     &           STR1*32, STR2*32, STR3*32
      REAL*8     NORM
      REAL*8     CNS_SIGMA
      PARAMETER  ( CNS_SIGMA = 3.D-11 )
      INTEGER*4  J1, J2, J3, J4, IND_PAR(2,2), IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF 
!
! --- Cycle over all frequencies
!
      CNS_ABR(1) = 'EHC_REAL'
      CNS_ABR(2) = 'EHC_IMAG'
      DO 410 J1=1,L_EHEC
         DO 420 J2=1,2 ! Over the first and the second station
            IF ( EHEC(J1)%IND(J2) < 1 .OR. EHEC(J1)%IND(J2) > L_EHEO ) THEN
                 CALL CLRCH ( STR )
                 CALL CLRCH ( STR1 )
                 CALL CLRCH ( STR2 )
                 CALL CLRCH ( STR3 )
                 CALL INCH  ( J1, STR  ) 
                 CALL INCH  ( J2, STR1 ) 
                 CALL INCH  ( EHEC(J1)%IND(J2), STR2 ) 
                 CALL INCH  ( L_EHEO, STR3 ) 
                 CALL ERR_LOG ( 8561, IUER, 'EHEO_CLS_CNST', 'Trap of '// &
     &               'internal control: EHEC('//STR(1:I_LEN(STR))//')%IND('// &
     &               STR1(1:I_LEN(STR1))//') = '//STR2(1:I_LEN(STR2))// &
     &               ', while it should be in the range [1, '//STR3 )
                 RETURN 
            END IF
!
            IF ( EHEC(J1)%HEO_TYPE == HEO__E1E2 ) THEN
!
                 PAR_NAME(HEO__COS,J2) = 'HEO C  EP '//EHEO(EHEC(J1)%IND(J2))%NAME(1:10)
                 PAR_NAME(HEO__SIN,J2) = 'HEO S  EP '//EHEO(EHEC(J1)%IND(J2))%NAME(1:10)
                 IND_PAR(HEO__COS,J2)  = LTM_DIF ( 0, L_PAR, C_PAR, &
     &                                             PAR_NAME(HEO__COS,J2) )
                 IND_PAR(HEO__SIN,J2)  = LTM_DIF ( 0, L_PAR, C_PAR, &
     &                                             PAR_NAME(HEO__SIN,J2) )
              ELSE IF ( EHEC(J1)%HEO_TYPE == HEO__E3 ) THEN
!
                 PAR_NAME(HEO__COS,J2) = 'HEO C  E3 '//EHEO(EHEC(J1)%IND(J2))%NAME(1:10)
                 PAR_NAME(HEO__SIN,J2) = 'HEO S  E3 '//EHEO(EHEC(J1)%IND(J2))%NAME(1:10)
                 IND_PAR(HEO__COS,J2)  = LTM_DIF ( 0, L_PAR, C_PAR, &
     &                                             PAR_NAME(HEO__COS,J2) )
                 IND_PAR(HEO__SIN,J2)  = LTM_DIF ( 0, L_PAR, C_PAR, &
     &                                             PAR_NAME(HEO__SIN,J2) )
            END IF
!
            IF ( IND_PAR(HEO__COS,J2) .LE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J1, STR  ) 
                 CALL ERR_LOG ( 8562, IUER, 'EHEO_CLS_CNST', 'Trap of '// &
     &               'internal control: cannot find parameter '// &
     &                PAR_NAME(HEO__COS,J2)//' for the '//STR(1:I_LEN(STR))// &
     &               ' condtraint on close constituents in harmonic '// &
     &               'variaions in the Earth rotation' )
                 RETURN 
            END IF
!
            IF ( IND_PAR(HEO__SIN,J2) .LE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J1, STR  ) 
                 CALL ERR_LOG ( 8563, IUER, 'EHEO_CLS_CNST', 'Trap of '// &
     &               'internal control: cannot find parameter '// &
     &                PAR_NAME(HEO__SIN,J2)//' for the '//STR(1:I_LEN(STR))// &
     &               ' condtraint on close constituents in harmonic '// &
     &               'variaions in the Earth rotation' )
                 RETURN 
            END IF
 420     CONTINUE 
!
         DO 430 J3=1,2 ! Over the real and image part
!
!---------- Define constraint for either real or image part
!
            CALL ERR_PASS ( IUER, IER )
            CALL ADDCNS_NAM ( CNS_ABR(J3), J1, 'EHEO_CLS_'//CNS_ABR(J3)(5:8), &
     &                        'rad', 0.0D0, CNS_SIGMA, .TRUE., CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8564, IUER, 'EHEO_CLS_CNST', 'Error in '// &
     &               'an attempt to put information about '// &
     &                CNS_ABR(J3)//' constraints into CNSTROBJ' )
                 RETURN
            END IF
 430     CONTINUE 
!
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ Compte th enorm of contraint equations.
!
         NORM = DSQRT ( EHEC(J1)%AMP(HEO__COS,1)**2 + &
     &                  EHEC(J1)%AMP(HEO__SIN,1)**2 + &
     &                  EHEC(J1)%AMP(HEO__COS,2)**2 + &
     &                  EHEC(J1)%AMP(HEO__SIN,2)**2   )/2.0D0
!
! ------ Insert four coefficients of constraint equations for the real part
!
!
! ------ 1st equation, real part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(1), J1, IND_PAR(HEO__COS,1), &
     &                     EHEC(J1)%AMP(HEO__COS,2)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8565, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the first coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &             STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
!
! ------ 2nd equation, real part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(1), J1, IND_PAR(HEO__SIN,1), &
     &                    -EHEC(J1)%AMP(HEO__SIN,2)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8566, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the second coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &            STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
!
! ------ 3rd equation, real part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(1), J1, IND_PAR(HEO__COS,2), &
     &                    -EHEC(J1)%AMP(HEO__COS,1)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8567, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the third coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &            STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
!
! ------ 4th equation, real part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(1), J1, IND_PAR(HEO__SIN,2), &
     &                     EHEC(J1)%AMP(HEO__SIN,1)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8568, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the second coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &            STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
!
! ------ Insert four coefficients of constraint equations for the image part
!
! ------ 1st equation, image part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(2), J1, IND_PAR(HEO__COS,1), &
     &                     EHEC(J1)%AMP(HEO__SIN,2)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8569, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the first coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &             STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
!
! ------ 2nd equation, image part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(2), J1, IND_PAR(HEO__SIN,1), &
     &                     EHEC(J1)%AMP(HEO__COS,2)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8570, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the second coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &            STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
!
! ------ 3rd equation, image part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(2), J1, IND_PAR(HEO__COS,2), &
     &                    -EHEC(J1)%AMP(HEO__SIN,1)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8571, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the third coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &            STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
!
! ------ 4th equation, real part
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( CNS_ABR(2), J1, IND_PAR(HEO__SIN,2), &
     &                    -EHEC(J1)%AMP(HEO__COS,1)/NORM, .TRUE., CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8572, IUER, 'EHEO_CLS_CNST', 'Failure in '// &
     &            'putting the second coefficient of constraint equation '// &
     &            'EHEO_CLS_'//CNS_ABR(1)(5:8)//' for the '// &
     &            STR(1:I_LEN(STR))//' constraint' )
              RETURN
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   EHEO_CLS_CNST  !#!#
