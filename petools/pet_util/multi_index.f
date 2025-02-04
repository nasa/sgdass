      FUNCTION MULTI_INDEX ( NREP, STR, SUBSTR )
! ************************************************************************
! *                                                                      *
! *   Function  MULTI_INDEX returns an index of the NREP-th occurence    *
! *   of the substring STR in the string STR. It returns 0 if there      *
! *   were no occurence, -1 if NREP=0 and -2 if LEN(STR) = 0             *
! *                                                                      *
! *  ###  11-SEP-99   MULTI_INDEX  v1.0  (c)  L. Petrov  11-SEP-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MULTI_INDEX, NREP
      CHARACTER  STR*(*), SUBSTR*(*)
      INTEGER*4  J1, IB, IP, IL
!
      IF ( NREP .LE. 0 ) THEN
           MULTI_INDEX = -1
           RETURN
      END IF
!
      IL = LEN(STR)
      IF ( IL .LE. 0 ) THEN
           MULTI_INDEX = -2
           RETURN
      END IF
!
      MULTI_INDEX = 0
      IB = 1
      DO 410 J1=1,NREP
         IF ( IB .GT. IL ) RETURN
         IP = INDEX ( STR(IB:), SUBSTR ) + IB-1
         IF ( IP .LE. IB-1 ) RETURN
         IB=IP+1
 410  CONTINUE
      MULTI_INDEX = IP
!
      RETURN
      END  !#!  MULTI_INDEX  #!#
