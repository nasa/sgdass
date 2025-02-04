      SUBROUTINE PARSE_AAM ( FIL_AAM, MJD_MOM, TAI_MOM, IMOM_NOIB, IMOM_IB, &
     &                       HMOM, EXF_NOIB, EXF_IB, &
     &                       DATA_SOURCE, DATA_TITLE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_AAM 
! *                                                                      *
! *  ### 06-AUG-2015   PARSE_AAM   v2.0 (c)  L. Petrov  30-NOV-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER  FIL_AAM*(*), DATA_SOURCE*(*), DATA_TITLE*(*)
      INTEGER*4  MJD_MOM, IUER
      REAL*8     TAI_MOM, HMOM(3), IMOM_NOIB(3), IMOM_IB(3), &
     &                             EXF_NOIB(3),  EXF_IB(3)
      INTEGER*4  MB
      PARAMETER  ( MB = 64 )
      CHARACTER  BUF(MB)*256, STR*128
      INTEGER*4  NB, J1, J2, J3, IFMT, IER 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_AAM, MB, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5111, IUER, 'PARSE_AAM', 'Error in reading '// &
     &         'input file with AAM '//FIL_AAM )
           RETURN
      END IF
!
      IF ( BUF(1) .EQ. AAM__FMT ) THEN
           IFMT = 1
        ELSE IF ( BUF(1)(1:36) .EQ. '# AAM  Format  Version of 2015.03.06' ) THEN
           IFMT = 2
        ELSE IF ( BUF(1)(1:36) .EQ. '# AAM  Format  Version of 2015.08.05' ) THEN
           IFMT = 3
        ELSE
           CALL CLRCH ( STR )
           STR = BUF(1)
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 5112, IUER, 'PARSE_AAM', 'Wrong format of '// &
     &         'the AAM file '//FIL_AAM(1:I_LEN(FIL_AAM))//' the first '// &
     &         'line is '//STR(1:I_LEN(STR))//' while the label '// &
     &          AAM__FMT//' was expected' )
           RETURN
      END IF
!
      IF ( IFMT == 1 ) THEN
           DO 410 J1=1,NB
              IF ( BUF(J1)(1:12) == 'AAM date:' ) THEN
                   READ ( UNIT=BUF(J1)(49:53), FMT='(I5)'   ) MJD_MOM
                   READ ( UNIT=BUF(J1)(56:62), FMT='(F7.1)' ) TAI_MOM
                 ELSE IF ( BUF(J1)(1:22) == 'AAM matter part no IB:' ) THEN
                   READ ( UNIT=BUF(J1)(28:39), FMT='(E12.5)'   ) IMOM_NOIB(1)
                   READ ( UNIT=BUF(J1)(42:53), FMT='(E12.5)'   ) IMOM_NOIB(2)
                   READ ( UNIT=BUF(J1)(56:67), FMT='(E12.5)'   ) IMOM_NOIB(3)
                 ELSE IF ( BUF(J1)(1:22) == 'AAM matter part    IB:' ) THEN
                   READ ( UNIT=BUF(J1)(28:39), FMT='(E12.5)'   ) IMOM_IB(1)
                   READ ( UNIT=BUF(J1)(42:53), FMT='(E12.5)'   ) IMOM_IB(2)
                   READ ( UNIT=BUF(J1)(56:67), FMT='(E12.5)'   ) IMOM_IB(3)
                 ELSE IF ( BUF(J1)(1:16) == 'AAM motion part:' ) THEN
                   READ ( UNIT=BUF(J1)(28:39), FMT='(E12.5)'   ) HMOM(1)
                   READ ( UNIT=BUF(J1)(42:53), FMT='(E12.5)'   ) HMOM(2)
                   READ ( UNIT=BUF(J1)(56:67), FMT='(E12.5)'   ) HMOM(3)
                 ELSE IF ( BUF(J1)(1:26) == 'Excitation function no IB:' ) THEN
                   READ ( UNIT=BUF(J1)(28:39), FMT='(E12.5)'   ) EXF_NOIB(1)
                   READ ( UNIT=BUF(J1)(42:53), FMT='(E12.5)'   ) EXF_NOIB(2)
                   READ ( UNIT=BUF(J1)(56:67), FMT='(E12.5)'   ) EXF_NOIB(3)
                 ELSE IF ( BUF(J1)(1:26) == 'Excitation function    IB:' ) THEN
                   READ ( UNIT=BUF(J1)(28:39), FMT='(E12.5)'   ) EXF_IB(1)
                   READ ( UNIT=BUF(J1)(42:53), FMT='(E12.5)'   ) EXF_IB(2)
                   READ ( UNIT=BUF(J1)(56:67), FMT='(E12.5)'   ) EXF_IB(3)
                 ELSE IF ( BUF(J1)(1:23) == '# Original data source:' ) THEN
                   DATA_SOURCE = BUF(J1)(34:)
                 ELSE IF ( BUF(J1)(1:22) == '# Original data title:' ) THEN
                   DATA_TITLE  = BUF(J1)(34:)
              END IF
 410       CONTINUE 
        ELSE IF ( IFMT == 2 ) THEN
           DO 420 J2=1,NB
              IF ( BUF(J2)(1:12) == 'AAM date:' ) THEN
                   READ ( UNIT=BUF(J2)(43:47), FMT='(I5)'   ) MJD_MOM
                   READ ( UNIT=BUF(J2)(50:56), FMT='(F7.1)' ) TAI_MOM
                 ELSE IF ( BUF(J2)(1:16) == 'AAM matter part:' ) THEN
                   READ ( UNIT=BUF(J2)(22:33), FMT='(E12.5)'   ) IMOM_NOIB(1)
                   READ ( UNIT=BUF(J2)(36:47), FMT='(E12.5)'   ) IMOM_NOIB(2)
                   READ ( UNIT=BUF(J2)(50:61), FMT='(E12.5)'   ) IMOM_NOIB(3)
                 ELSE IF ( BUF(J2)(1:16) == 'AAM motion part:' ) THEN
                   READ ( UNIT=BUF(J2)(22:33), FMT='(E12.5)'   ) HMOM(1)
                   READ ( UNIT=BUF(J2)(36:47), FMT='(E12.5)'   ) HMOM(2)
                   READ ( UNIT=BUF(J2)(50:61), FMT='(E12.5)'   ) HMOM(3)
                 ELSE IF ( BUF(J2)(1:20) == 'Excitation function:' ) THEN
                   READ ( UNIT=BUF(J2)(22:33), FMT='(E12.5)'   ) EXF_NOIB(1)
                   READ ( UNIT=BUF(J2)(36:47), FMT='(E12.5)'   ) EXF_NOIB(2)
                   READ ( UNIT=BUF(J2)(50:61), FMT='(E12.5)'   ) EXF_NOIB(3)
                 ELSE IF ( BUF(J2)(1:23) == '# Original data source:' ) THEN
                   DATA_SOURCE = BUF(J2)(34:)
                 ELSE IF ( BUF(J2)(1:22) == '# Original data title:' ) THEN
                   DATA_TITLE  = BUF(J2)(34:)
              END IF
 420       CONTINUE 
           IMOM_IB = 0.0
           EXF_IB  = 0.0
        ELSE IF ( IFMT == 3 ) THEN
           DO 430 J3=1,NB
              IF ( BUF(J3)(1:1) == '#' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J3)(1:12) == 'AAM date:' ) THEN
                   READ ( UNIT=BUF(J3)(39:43), FMT='(I5)'   ) MJD_MOM
                   READ ( UNIT=BUF(J3)(46:52), FMT='(F7.1)' ) TAI_MOM
                 ELSE IF ( BUF(J3)(1:16) == 'AAM matter part:' ) THEN
                   READ ( UNIT=BUF(J3)(18:29), FMT='(E12.5)'   ) IMOM_NOIB(1)
                   READ ( UNIT=BUF(J3)(32:43), FMT='(E12.5)'   ) IMOM_NOIB(2)
                   READ ( UNIT=BUF(J3)(46:57), FMT='(E12.5)'   ) IMOM_NOIB(3)
                 ELSE IF ( BUF(J3)(1:16) == 'AAM motion part:' ) THEN
                   READ ( UNIT=BUF(J3)(18:29), FMT='(E12.5)'   ) HMOM(1)
                   READ ( UNIT=BUF(J3)(32:43), FMT='(E12.5)'   ) HMOM(2)
                   READ ( UNIT=BUF(J3)(46:57), FMT='(E12.5)'   ) HMOM(3)
              END IF
 430       CONTINUE 
           IMOM_IB  = 0.0
           EXF_IB   = 0.0
           EXF_IB   = 0.0
           EXF_NOIB = 0.0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_AAM  !#!  
