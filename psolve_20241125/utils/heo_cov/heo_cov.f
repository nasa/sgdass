      PROGRAM    HEO_COV
! ************************************************************************
! *                                                                      *
! *   Program  HEO_COV
! *                                                                      *
! *  ### 31-MAR-2016    HEO_COV    v2.1 (c) L. Petrov  31-MAR-2016  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER  FILIN*128, FILOUT*128, CPAR(M_GPA)*20
      ADDRESS__TYPE :: IADR_ARR, IADR_NORMAT, IADR_NORVEC
      REAL*8     COV_VAL
      INTEGER*4  LPAR, J1, J2, J3, L_HEO, IND_HEO(M_GPA), NUMARG, LUN, IOS, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      CALL CLRCH ( FILIN  )
      CALL CLRCH ( FILOUT )
      NUMARG = IARGC ()
      IF ( NUMARG .GE. 2 ) THEN
           CALL GETARG ( 1, FILIN )
           CALL GETARG ( 2, FILOUT )
         ELSE
           WRITE ( 6, * ) 'Usage: heo_cov {COVF-file} {heo_output_cov}'
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GET_NORMAL ( FILIN, LPAR, IADR_ARR, CPAR, IUER )
      IADR_NORMAT = IADR_ARR + 8*3*M_GPA
      IADR_NORVEC = IADR_ARR + 8*2*M_GPA
      L_HEO = 0
      DO 410 J1=1,LPAR
         IF ( CPAR(J1)(1:3) == 'HEO' ) THEN
              L_HEO = L_HEO + 1
              IND_HEO(L_HEO) = J1
         END IF
 410  CONTINUE 
!
      LUN = 55
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)') 'Failure to open output file '// &
     &                        FILOUT(1:I_LEN(FILOUT))
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Solve covariance matrix. Format version of 2016.03.31' 
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A,I4)' ) '# The number of parameters: ', L_HEO
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      DO 420 J2=1,L_HEO
         DO 430 J3=J2,L_HEO
            CALL MEMCPY ( COV_VAL, %VAL(IADR_NORMAT + 8*(LOCS( IND_HEO(J2), IND_HEO(J3))-1)), &
     &                    %VAL(8) )
            WRITE ( UNIT=LUN, FMT=110 ) J2, J3, &
     &                                  CPAR(IND_HEO(J2)), CPAR(IND_HEO(J3)), &
     &                                  COV_VAL
 110        FORMAT ( I4, ' | ', I4, ' | ', A20, ' | ', A20, ' | ', 1PD20.12 )
 430     CONTINUE 
 420  CONTINUE 
      END  !#!  HEO_COV  #!#
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
      LEN_GET = 8*( 4*M_GPA + (INT8(LPAR)*INT8(LPAR+1))/2 ) + 256
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
!
      CALL USE_CGMF_MAT ( %VAL(IADR_ARR), NPARAM, 'R' )
      CALL ACS_CGMFIL   ( FINAM, 'C' )
!
      CALL LIB$MOVC3 ( LPAR*20, CPARM_NAMES, CPAR )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_NORMAL  #!#
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
