      SUBROUTINE GET_BAND_RANGE ( VEX, IND_STA, N_BND, N_IFS, IND_IFS, BAND_MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_BAND_RANGE
! *                                                                      *
! *  ### 07-JUL-2021  GET_BAND_RANGE v1.0 (c) L. Petrov  07-JUL-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vex.i'
      TYPE ( VEX_TYPE  ) :: VEX
      REAL*8     FRQ_ARR(VEX__MCHA), FRQ_MIN, FRQ_MAX, FRQ_EPS
      PARAMETER  ( FRQ_EPS = 1.0D0 )
      INTEGER*4  IND_STA, N_BND, N_IFS(4), IND_IFS(VEX__MCHA,4), IND_FRQ(VEX__MCHA), IUER
      CHARACTER  BAND_MODE*(*)
      INTEGER*4  J1, J2, J3, IER
!    
      N_IFS = 0
      IND_IFS = 0
!
      FRQ_MIN =  1.D20
      FRQ_MAX = -1.D20
      FRQ_ARR = VEX%FRQ(1)%SKY_FRQ
      CALL SORT_R8 ( VEX%FRQ(1)%N_CHA, FRQ_ARR )
      FRQ_MIN = FRQ_ARR(1)
      FRQ_MAX = FRQ_ARR(VEX%FRQ(1)%N_CHA)
      N_IFS = 0
      IF ( FRQ_MIN < 3.D9 .AND. FRQ_MAX > 8.1D9 ) THEN
           BAND_MODE = 'SX'
        ELSE IF ( FRQ_MIN < 5.D9 .AND. FRQ_MAX > 7.1D9 ) THEN
           BAND_MODE = 'CX'
        ELSE IF ( FRQ_MIN < 4.D9 .AND. FRQ_MAX > 10.0D9 ) THEN
           BAND_MODE = 'VGOS'
        ELSE
           BAND_MODE = 'ONE'
      END IF 
      DO 410 J1=1,VEX%FRQ(1)%N_CHA
         IND_FRQ = -1
         DO 420 J2=1,VEX%FRQ(1)%N_CHA
            IF ( DABS ( FRQ_ARR(J1) - VEX%FRQ(1)%SKY_FRQ(J2) ) < FRQ_EPS ) THEN
                 IND_FRQ(J1) = J2
            END IF
 420     CONTINUE 
         IF ( BAND_MODE == 'ONE' ) THEN
              N_BND = 1
              N_IFS(1) = N_IFS(1) + 1
              IND_IFS(N_IFS(1),1) = IND_FRQ(J1)
            ELSE IF ( BAND_MODE == 'SX' ) THEN
              N_BND = 2
              IF ( FRQ_ARR(J1) < 2.5D9 ) THEN
                   N_IFS(1) = N_IFS(1) + 1
                   IND_IFS(N_IFS(1),1) = IND_FRQ(J1)
                ELSE                    
                   N_IFS(2) = N_IFS(2) + 1
                   IND_IFS(N_IFS(2),2) = IND_FRQ(J1)
              END IF
            ELSE IF ( BAND_MODE == 'CX' ) THEN
              N_BND = 2
              IF ( FRQ_ARR(J1) < 5.0D9 ) THEN
                   N_IFS(1) = N_IFS(1) + 1
                   IND_IFS(N_IFS(1),1) = IND_FRQ(J1)
                ELSE                    
                   N_IFS(2) = N_IFS(2) + 1
                   IND_IFS(N_IFS(2),2) = IND_FRQ(J1)
              END IF
            ELSE IF ( BAND_MODE == 'VGOS' ) THEN
              N_BND = 4
              IF ( FRQ_ARR(J1) < 4.0D9 ) THEN
                   N_IFS(1) = N_IFS(1) + 1
                   IND_IFS(N_IFS(1),1) = IND_FRQ(J1)
                ELSE IF ( FRQ_ARR(J1) < 6.0D9 ) THEN
                   N_IFS(2) = N_IFS(2) + 1
                   IND_IFS(N_IFS(2),2) = IND_FRQ(J1)
                ELSE IF ( FRQ_ARR(J1) < 9.0D9 ) THEN
                   N_IFS(3) = N_IFS(3) + 1
                   IND_IFS(N_IFS(3),3) = IND_FRQ(J1)
                ELSE
                   N_IFS(4) = N_IFS(4) + 1
                   IND_IFS(N_IFS(4),4) = IND_FRQ(J1)
              END IF
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_BAND_RANGE  !#!#
