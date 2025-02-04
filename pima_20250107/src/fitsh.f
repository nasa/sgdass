       PROGRAM    FITSH_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL FITSH()
       END  PROGRAM  FITSH_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE FITSH()
! ************************************************************************
! *                                                                      *
! *   Program FITSH
! *                                                                      *
! *  ### 28-DEC-2005    FITSH      v2.0 (c)  L. Petrov  28-JUN-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FINAM*128
      INTEGER*4  MHDR, MKEY
      PARAMETER  ( MHDR =       32 )
      PARAMETER  ( MKEY = 256*1024 )
      INTEGER*4  LHDR, LKEY(MHDR), IUER
      INTEGER*8  FPTR
      CHARACTER  KEYS(MKEY,MHDR)*80, STR*128
      INTEGER*4  J1, J2, MAX_KEY
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() == 0 ) THEN
           WRITE ( 6, * ) 'Usage: fitsh {fits-idi_file}'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FINAM )
      END IF
!
      IUER = -1
      CALL FFITS_OPEN ( FINAM, FPTR, 'OLD', IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      IUER = -1
      CALL FFITS_GET_KEYS ( FPTR, MHDR, MKEY, LHDR, LKEY, KEYS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      IUER = -1
      CALL FFITS_CLOSE    ( FPTR, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      DO 410 J1=1,LHDR
         DO 420 J2=1,LKEY(J1)
            WRITE ( 6, 110 ) J1, J2, KEYS(J2,J1)(1:I_LEN(KEYS(J2,J1)))
 110        FORMAT ( 'HDR: ',I2,' Key: ',I4,'  ', A )
 420     CONTINUE 
         STR = '================================================================'// &
     &         '================================================================'
         WRITE ( 6, '(A)' ) STR(1:100)
 410  CONTINUE 
      END  SUBROUTINE  FITSH  !#!#
