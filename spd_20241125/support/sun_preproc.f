      PROGRAM    SUN_PREPROC
! ************************************************************************
! *                                                                      *
! *   A simple program which replaces                                    *
! *   %REF( name ) with %VAL(LOC( name ))                                *
! *                                                                      *
! *   It is needed for SUn compiler Frotran 6.2, since it does not       *
! *   understand %REF intrinsic function.                                *
! *                                                                      *
! *  ### 19-MAY-2004  SUN_PREPROC  v1.0 (c)  L. Petrov  19-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  J1, J2, IL, IPR, IPP, IOS
      CHARACTER  STR*1024
      INTEGER*4  ILEN
!
      DO 410 J1=1,1024*1024*1024
         READ ( UNIT=5, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .NE. 0 ) GOTO 810
         IL  = ILEN(STR)
         IF ( IL .LE. 0 ) THEN
              WRITE ( 6, '(A)' ) ""
              GOTO 410
         END IF
         IPR = INDEX ( STR(1:IL), '%REF' )
         IF ( IPR .EQ. 0 ) IPR = INDEX ( STR(1:IL), '%ref' )
         IF ( IPR .LE. 0 ) THEN
              WRITE ( 6, '(A)' ) STR(1:IL)
            ELSE
              STR = STR(1:IPR-1)//'%VAL(LOC'//STR(IPR+4:)
              IL = IL + 4
              IPP = 1
              DO 420 J2=IPR+9,IL
                 IF ( STR(J2:J2) .EQ. '(' ) IPP = IPP + 1
                 IF ( STR(J2:J2) .EQ. ')' ) IPP = IPP - 1
                 IF ( IPP .EQ. 0 ) THEN
                      STR = STR(1:J2)//')'//STR(J2+1:IL)
                      IL = IL + 1
                      GOTO 820
                 END IF
 420          CONTINUE 
 820          CONTINUE 
              WRITE ( 6, '(A)' ) STR(1:IL)
         END IF
 410  CONTINUE 
 810  CONTINUE 
      END  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION ILEN ( STR )
! ************************************************************************
! *                                                                      *
! *   Function ILEN returns the position of the last character of the    *
! *   string STR which is not blank or binary zero. If the string        *
! *   contains only blanks and/or binary zeroes, then ILEN=0             *
! *                                                                      *
! *  ### 17-JAN-1989               v1.0 (c)  L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  ILEN
      CHARACTER STR*(*)
      INTEGER*4  J1
!
      ILEN=0
      DO 410 J1=LEN(STR),1,-1
         ILEN=J1
         IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) RETURN
 410  CONTINUE
      ILEN=0
      RETURN
      END  !#!  ILEN  #!#
