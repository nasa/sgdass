      FUNCTION   SANITIZE_LOGVAR_L1 ( L1 )
! ************************************************************************
! *                                                                      *
! *   Program SANITIZE_LOGVAR_L1 solves incompatibility of               *
! *   represnetation of a logical variable between GNU fortran and       *
! *   INTEL complilers. It reads LOGICAL*1 variable that may not         *
! *   necessarily fit ABI of the current compiler and returns the        *
! *   value that conforms the ABI.                                       *
! *                                                                      *
! * ## 16-JAN-2011 SANITIZE_LOGVAR_L1 v1.0 (c) L. Petrov  16-JAN-2011 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*1  SANITIZE_LOGVAR_L1 
      INTEGER*1  L1
#ifdef INTEL
      IF ( L1 == 0 ) THEN
           SANITIZE_LOGVAR_L1 = L1
        ELSE IF ( L1 == -1 ) THEN
           SANITIZE_LOGVAR_L1 = L1
        ELSE 
           IF ( IBSET ( L1, 0 ) == 1 ) THEN
                SANITIZE_LOGVAR_L1 = -1
              ELSE 
                SANITIZE_LOGVAR_L1 = 0
           END IF
      END IF
#elif GNU
      IF ( L1 == 0 ) THEN
           SANITIZE_LOGVAR_L1 = L1
        ELSE IF ( L1 == 1 ) THEN
           SANITIZE_LOGVAR_L1 = L1
        ELSE 
           SANITIZE_LOGVAR_L1 =  1
      END IF
#else
      SANITIZE_LOGVAR_L1 = L1
#endif
      RETURN
      END  FUNCTION  SANITIZE_LOGVAR_L1  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   SANITIZE_LOGVAR_L2 ( L2 )
! ************************************************************************
! *                                                                      *
! *   Program SANITIZE_LOGVAR_L2 solves incompatibility of               *
! *   represnetation of a logical variable between GNU fortran and       *
! *   INTEL complilers. It reads LOGICAL*1 variable that may not         *
! *   necessarily fit ABI of the current compiler and returns the        *
! *   value that conforms the ABI.                                       *
! *                                                                      *
! * ## 16-JAN-2011 SANITIZE_LOGVAR_L2 v1.0 (c) L. Petrov  16-JAN-2011 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*2  SANITIZE_LOGVAR_L2 
      INTEGER*2  L2
#ifdef INTEL
      IF ( L2 == 0 ) THEN
           SANITIZE_LOGVAR_L2 = L2
        ELSE IF ( L2 == -1 ) THEN
           SANITIZE_LOGVAR_L2 = L2
        ELSE 
           IF ( IBSET ( L2, 0 ) == 1 ) THEN
                SANITIZE_LOGVAR_L2 = -1
              ELSE 
                SANITIZE_LOGVAR_L2 = 0
           END IF
      END IF
#elif GNU
      IF ( L2 == 0 ) THEN
           SANITIZE_LOGVAR_L2 = L2
        ELSE IF ( L2 == 1 ) THEN
           SANITIZE_LOGVAR_L2 = L2
        ELSE 
           SANITIZE_LOGVAR_L2 =  1
      END IF
#else
      SANITIZE_LOGVAR_L2 = L2
#endif
      RETURN
      END  FUNCTION  SANITIZE_LOGVAR_L2  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   SANITIZE_LOGVAR_L4 ( L4 )
! ************************************************************************
! *                                                                      *
! *   Program SANITIZE_LOGVAR_L4 solves incompatibility of               *
! *   represnetation of a logical variable between GNU fortran and       *
! *   INTEL complilers. It reads LOGICAL*1 variable that may not         *
! *   necessarily fit ABI of the current compiler and returns the        *
! *   value that conforms the ABI.                                       *
! *                                                                      *
! * ## 16-JAN-2011 SANITIZE_LOGVAR_L4 v1.0 (c) L. Petrov  16-JAN-2011 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  SANITIZE_LOGVAR_L4 
      INTEGER*4  L4
#ifdef INTEL
      IF ( L4 == 0 ) THEN
           SANITIZE_LOGVAR_L4 = L4
        ELSE IF ( L4 == -1 ) THEN
           SANITIZE_LOGVAR_L4 = L4
        ELSE 
           IF ( IBSET ( L4, 0 ) == 1 ) THEN
                SANITIZE_LOGVAR_L4 = -1
              ELSE 
                SANITIZE_LOGVAR_L4 = 0
           END IF
      END IF
#elif GNU
      IF ( L4 == 0 ) THEN
           SANITIZE_LOGVAR_L4 = L4
        ELSE IF ( L4 == 1 ) THEN
           SANITIZE_LOGVAR_L4 = L4
        ELSE 
           SANITIZE_LOGVAR_L4 =  1
      END IF
#else
      SANITIZE_LOGVAR_L4 = L4
#endif
      RETURN
      END  FUNCTION  SANITIZE_LOGVAR_L4  !#!  
