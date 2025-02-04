      PROGRAM    SUPPRESS_CGM
! ************************************************************************
! *                                                                      *
! *   Program SUPPRESS CGM reads the input CGM, removes parameters found *
! *   in the list supplied as the second argument and writes down the    *
! *   output CGM. The parameter list may contain regular experessions.   *
! *                                                                      *
! *  ### 10-APR-2020  SUPPRESS_CGM  v1.0 (c)  L. Petrov  10-APR-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      CHARACTER  FIL_CGM_INP*128, FIL_PAR_LIST*128, FIL_CGM_OUT*128, STR*128
      ADDRESS__TYPE :: IADR_ARR, IADR_NORMAT, IADR_NORVEC
      CHARACTER  CPAR(M_GPA)*20, EPAR(M_GPA)*20, NPAR(M_GPA)*20, PARAM_FIL*128
      LOGICAL*1  FL_MATCH, FL_DRY, FL_USE_PAR(M_GPA), LEX
      INTEGER*4  J1, J2, J3, J4, NE, NUMARG, LPAR, KPAR, IUER
#ifdef INTEL
      INTEGER*4, EXTERNAL ::  IARGC
#endif
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN
      LOGICAL*4, EXTERNAL ::  MATCH_REGEXP
!
      NUMARG = IARGC ()
      FL_DRY = .FALSE.
      IF ( NUMARG .GE. 3 ) THEN
           CALL GETARG ( 1, FIL_CGM_INP  )
           CALL GETARG ( 2, FIL_PAR_LIST )
           CALL GETARG ( 3, FIL_CGM_OUT  )
           IF ( NUMARG .GE. 4 ) THEN
                CALL GETARG ( 4, STR )
                IF ( STR(1:4) == '-dry' .OR. STR(2:5) == '-dry' ) THEN
                     FL_DRY = .TRUE.
                   ELSE 
                     IUER = -1
                     CALL ERR_LOG ( 5001, IUER, 'SUPPRESS_CGM', 'Unsupported 4th argument '// &
     &                    TRIM(STR)//' -- only -dry-run is suypported' )
                     CALL EXIT ( 1 )
                END IF
           END IF
         ELSE
           WRITE ( 6, * ) 'Usage: reduce_cgm inp_cgm par_list out_cgm [-dry-run]'
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FIL_CGM_INP, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 5002, IUER, 'SUPPRESS_CGM', 'Cannot find the '// &
     &                   'file with input CGM '//FIL_CGM_INP )
           CALL EXIT ( 1 )
      END IF
      INQUIRE ( FILE=FIL_CGM_OUT, EXIST=LEX )
      IF ( LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 5003, IUER, 'SUPPRESS_CGM', 'Output CGM file '// &
     &                    TRIM(FIL_CGM_OUT)//' exists. '// &
     &                   'Please, remove it first.' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_PAR_LIST, M_GPA, EPAR, NE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5004, IUER, 'SUPPRESS_CGM', 'Error in reading input '// &
     &         'CGM matrix from file '//FIL_CGM_INP )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GET_NORMAL ( FIL_CGM_INP, LPAR, IADR_ARR, CPAR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5005, IUER, 'SUPPRESS_CGM', 'Error in reading input '// &
     &         'CGM matrix from file '//FIL_CGM_INP )
           CALL EXIT ( 1 )
      END IF
      IADR_NORMAT = IADR_ARR + 8*3*M_GPA
      IADR_NORVEC = IADR_ARR + 8*2*M_GPA
      KPAR = 0
      DO 410 J1=1,LPAR
         FL_MATCH = .FALSE.
         DO 420 J2=1,NE
            IF ( ILEN(EPAR(J2)) == 0 ) GOTO 420
            IF ( MATCH_REGEXP ( CPAR(J1), TRIM(EPAR(J2)), IUER ) ) THEN
                 FL_MATCH = .TRUE.
            END IF
 420     CONTINUE 
         IF ( FL_MATCH ) THEN
              WRITE ( 6, '(A, 2X, A)' ) 'excluded ', CPAR(J1)
              FL_USE_PAR(J1) = .FALSE.
            ELSE
              WRITE ( 6, '(A, 2X, A)' ) 'INCLUDED ', CPAR(J1)
              KPAR = KPAR + 1
              FL_USE_PAR(J1) = .TRUE.
         END IF
 410  CONTINUE 
      IF ( FL_DRY ) THEN
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A, I5, A)' ) 'There will be ', KPAR, ' intact parameters in the output CGM' 
!
      IUER = -1
      CALL SUPPRESS_NORMAL ( LPAR, %VAL(IADR_NORMAT), %VAL(IADR_NORVEC), FL_USE_PAR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5006, IUER, 'SUPPRESS_CGM', 'Error in an attempt'// &
     &         'to suppress elelments of the CGM matrix from file '//FIL_CGM_INP )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL PUT_NORMAL ( FIL_CGM_OUT, LPAR, IADR_ARR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5007, IUER, 'SUPPRESS_CGM', 'Error in reading input '// &
     &         'CGM matrix from file '//FIL_CGM_OUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  SUPPRESS_CGM  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUPPRESS_NORMAL ( LPAR, NOR_MAT, NOR_VEC, FL_USE_PAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUPPRESS_NORMAL 
! *                                                                      *
! *  ### 10-APR-2020 SUPPRESS_NORMAL v1.0 (c) L. Petrov 10-APR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  LPAR, IUER
      LOGICAL*1  FL_USE_PAR(LPAR)
      REAL*8     NOR_MAT(*), NOR_VEC(*)
      REAL*8     SUP
      PARAMETER  ( SUP = 1.0D-9 )
      INTEGER*4  J1, J2
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      DO 410 J1=1,LPAR
         IF ( .NOT. FL_USE_PAR(J1) ) THEN
              DO 420 J2=1,LPAR
                 NOR_MAT(LOCS(J2,J1)) = SUP*NOR_MAT(LOCS(J2,J1))
 420          CONTINUE 
              NOR_MAT(LOCS(J1,J1)) = SUP*NOR_MAT(LOCS(J1,J1))
              NOR_VEC(J1) = SUP*NOR_VEC(J1) 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUPPRESS_NORMAL   !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_NORMAL ( FINAM, LPAR, IADR_ARR, CPAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_NORMAL
! *                                                                      *
! *  ### 14-FEB-2001  GET_NORMAL   v2.0 (c)  L. Petrov  17-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'prfil.i'
      INCLUDE   'plist.i'
!
      INTEGER*4  LPAR, IUER
      INTEGER*8  LEN_GET, MEM_LEN
      ADDRESS__TYPE :: IADR_ARR, MEM_ADR
      INTEGER*4  IER
      CHARACTER  FINAM*(*), CPAR(*)*20, STR*32, STR1*32
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Opening input CGM
!
      CALL ACS_CGMFIL ( FINAM, 'O' )
!
! --- Read CGM-dependent common blocks
!
      CALL USE_CGMF_COM ( 'R' )
      NPARAM = PARM_NUM
      LPAR = PARM_NUM
!
      LEN_GET = 8*( 4*INT8(M_GPA) + (INT8(LPAR)*INT8(LPAR+1))/2 ) + 256
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, MEM_ADR, MEM_LEN, 1, LEN_GET, IADR_ARR )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL IINCH ( LEN_GET, STR )
           CALL INCH  ( NPARAM, STR1 )
           CALL ERR_LOG ( 4954, IUER, 'GET_NORMAL', 'Error in attempt '// &
     &         'to grab '//STR(1:I_LEN(STR))//' bytes of dynamic mmeory for '// &
     &         'cgm sized for '//STR1(1:I_LEN(STR1))//' parameters' )
           RETURN
      END IF
      WRITE ( 6, * ) 'Number of parameters: ', LPAR
!
      CALL USE_CGMF_MAT ( %VAL(IADR_ARR), NPARAM, 'R' )
      CALL ACS_CGMFIL   ( FINAM, 'C' )
      CALL LIB$MOVC3 ( LPAR*20, CPARM_NAMES, CPAR )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_NORMAL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PUT_NORMAL ( FINAM, NPAR, IADR_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PUT_NORMAL
! *                                                                      *
! *  ### 14-FEB-2001  PUT_NORMAL   v2.0 (c)  L. Petrov  17-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'prfil.i'
      INCLUDE   'plist.i'
!
      INTEGER*4  NPAR, IUER
      INTEGER*8  IADR_ARR
      CHARACTER  FINAM*(*)
      CHARACTER  STR*32
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Opening input CGM
!
      STR = 'SAVE'
      CALL CREATE_CGMF ( STR, '??', NPAR, 'M', FINAM )
!
! --- Write CGM-dependent common blocks
!
      CALL USE_CGMF_COM ( 'W' )
      CALL USE_CGMF_MAT ( %VAL(IADR_ARR), NPAR, 'W' )
      CALL ACS_CGMFIL   ( FINAM, 'C' )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PUT_NORMAL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SEARCH_NAN ( L_PAR, NOR_MAT )
      IMPLICIT   NONE 
      INTEGER*4  L_PAR
      REAL*8     NOR_MAT(*)
      INTEGER*4  J1, J2, IP
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
!
      DO 410 J1=1,L_PAR
         DO 420 J2=J1,L_PAR
            IP = LOCS ( J1, J2 )
            IF ( IS_R8_NAN ( NOR_MAT(IP) ) ) THEN
                 WRITE ( 6, * ) 'NaN: J1=', INT2(J1), ' J2=',INT2(J2)
                 CALL PETUTIL_TRAP()
            END IF
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  !#!  
