      SUBROUTINE HIST ( MP, VAL, MH, HST_MIN, HST_MAX, HST_ARG, HST_VAL, IC )
! ************************************************************************
! *                                                                      *
! *   Routine  HIST computes the histogram in a range [HST_MIN, HST_MAX] *
! *   of an array VAL dimesion MP.                                       *
! *                                                                      *
! *  ### 06-DEC-2010     HIST      v2.0 (c)  L. Petrov  16-OCT-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP, MH, IC
      REAL*8     VAL(MP), HST_MIN, HST_MAX, HST_ARG(MH), HST_VAL(MH), HST_INT
      INTEGER*4  J1, J2, J3, J4, IH
!
      DO 410 J1=1,MH
         HST_ARG(J1) = HST_MIN + (J1-0.5D0)*(HST_MAX-HST_MIN)/MH
         HST_VAL(J1) = 0.0D0
 410  CONTINUE 
!
      DO 420 J2=1,MP
         IH = IDINT ( (VAL(J2)-HST_MIN)/((HST_MAX-HST_MIN)/MH) ) + 1
         IF ( IH < 1  ) IH = 1
         IF ( IH > MH ) IH = MH
         HST_VAL(IH) = HST_VAL(IH) + 1.0D0
 420  CONTINUE 
!
      IF ( IC == 1 ) THEN
           HST_INT = 0.0D0
           DO 430 J3=2,MH
              HST_INT = HST_INT + (HST_VAL(J3) + HST_VAL(J3-1))* &
     &                            (HST_ARG(J3) - HST_ARG(J3-1))/2.0D0
 430       CONTINUE 
!
           DO 440 J4=1,MH
              HST_VAL(J4) = HST_VAL(J4)/HST_INT
 440       CONTINUE 
      END IF
!
      RETURN
      END  SUBROUTINE  HIST  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CUMUL_HIST ( MP, VAL, MH, HST_MIN, HST_MAX, HST_ARG, HST_VAL, IC )
! ************************************************************************
! *                                                                      *
! *   Routine  CUMUL_HIST 
! *                                                                      *
! * ###   30-SEP-2016  CUMUL_HIST   v1.1 (c) L. Petrov  27-OCT-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP, MH, IC
      REAL*8     VAL(MP), HST_MIN, HST_MAX, HST_ARG(MH), HST_VAL(MH), HST_INT
      INTEGER*4  J1, J2, J3, J4, IH
!
      DO 410 J1=1,MH
         HST_ARG(J1) = HST_MIN + (J1-1)*(HST_MAX-HST_MIN)/(MH-1)
         HST_VAL(J1) = 0.0D0
 410  CONTINUE 
!
      DO 420 J2=1,MP
         DO 430 J3=1,MH
            IF ( VAL(J2) .LE. HST_ARG(J3) ) THEN
                 HST_VAL(J3) = HST_VAL(J3) + 1.0D0
            END IF
 430     CONTINUE 
 420  CONTINUE 
!
      IF ( IC == 1 ) THEN
           DO 440 J4=1,MH
              HST_VAL(J4) = HST_VAL(J4)/MP
 440       CONTINUE 
      END IF
!
      RETURN
      END  SUBROUTINE  CUMUL_HIST  !#!  
