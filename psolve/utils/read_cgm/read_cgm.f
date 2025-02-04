      PROGRAM    READ_CGM_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  READ_CGM
! *                                                                      *
! *  ### 04-MAY-2001    READ_CGM   v3.0 (c) L. Petrov  23-OCT-2017  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*4  LPAR, IUER
      CHARACTER  FINAM*128
      ADDRESS__TYPE :: IADR_ARR, IADR_NORMAT, IADR_NORVEC
      CHARACTER  CPAR(M_GPA)*20, PARAM_FIL*128
      INTEGER*4  J1, NUMARG, IANS, IOS
#ifdef INTEL
      INTEGER*4, EXTERNAL ::  IARGC
#endif
!
      PARAM_FIL = '/tmp/param.fil'
!
      CALL CLRCH ( FINAM )
      NUMARG = IARGC ()
      IF ( NUMARG .GE. 1 ) THEN
           CALL GETARG ( 1, FINAM )
         ELSE
           WRITE ( 6, * ) 'Usage: read_cgm  <cgm_file_name>'
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GET_NORMAL ( FINAM, LPAR, IADR_ARR, CPAR, IUER )
      IADR_NORMAT = IADR_ARR + 8*3*M_GPA
      IADR_NORVEC = IADR_ARR + 8*2*M_GPA
      IF ( IUER .NE. 0 ) STOP 'Abnormal termination'
 910  CONTINUE
      WRITE ( 6, '(A/">> "$)' ) 'Do you want to see (1) parameter list, '// &
     &                          '(2) normal matrix, (3) normal vector, (0) exit ?'
      READ ( UNIT=5, IOSTAT=IOS, FMT='(I1)' ) IANS
      IF ( IOS .NE. 0 ) CALL EXIT ( 0 )
      IF ( IANS .EQ. 1 ) THEN
           OPEN ( UNIT=16, FILE=PARAM_FIL, STATUS='UNKNOWN' )
           WRITE (  6, * ) ' LPAR=',LPAR
           WRITE ( 16, * ) ' LPAR=',LPAR
           DO 410 J1=1,LPAR
              WRITE (  6, 120 ) J1, CPAR(J1)
              WRITE ( 16, 120 ) J1, CPAR(J1)
  120         FORMAT ( 1X,' ipar=',I5,'  >',A,'<  ' )
  410      CONTINUE
           WRITE (  6, * ) ' ---------------------------------'
           WRITE ( 16, * ) ' ---------------------------------'
           CLOSE ( UNIT=16 )
           GOTO 910
        ELSE IF ( IANS .EQ. 2 ) THEN
           CALL MATVIEW_2 ( LPAR, %VAL(IADR_NORMAT) )
           GOTO 910
        ELSE IF ( IANS .EQ. 3 ) THEN
           CALL MATVIEW_1 ( LPAR, 1, %VAL(IADR_NORVEC) )
           GOTO 910
        ELSE IF ( IANS .EQ. 4 ) THEN
           CALL SEARCH_NAN ( LPAR, %VAL(IADR_NORMAT) )
      END IF
!
      END  !#!  READ_CGM_MAIN  #!#
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
