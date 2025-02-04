      FUNCTION   GET_GRPAMBSP ( NFRQ, FRQ, FRQ_WD, FRQ_CH )
! ************************************************************************
! *                                                                      *
! *   Routine GET_GRPAMBSP  computes group delay ambiguity spacings.     *
! *   It asssumes that frequencies are in the ascending order, except    *
! *   some frequencies may be zero. Zero frquencies are bypassed and     *
! *   not accounted for.                                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   NFRQ ( INTEGER*4 ) -- The number of frequencies.                   *
! *    FRQ ( REAL*8    ) -- Array of frequencies. Dimension: NFRQ.       *
! *                         Units: Hz.                                   *
! * FRQ_WD ( REAL*8    ) -- The width of each frequency channel.         *
! *                         Units: Hz.                                   *
! * FRQ_CH ( REAL*8    ) -- The width of each spectral channel.          *
! *                         Units: Hz.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <GET_GRPAMBSP> ( REAL*8 ) -- Group delay ambiguity spacing.          *
! *                              Units: sec.                             *
! *                                                                      *
! *  ### 11-JUL-2009  GET_GRPAMBSP  v4.0 (c) L. Petrov  01-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NFRQ
      REAL*8     FRQ(NFRQ), FRQ_WD, FRQ_CH
      REAL*8     GET_GRPAMBSP 
      REAL*8     FRQ_SCL, FRQ_MIN, GRA_SCL
      PARAMETER  ( FRQ_SCL = 1.D6   )
      PARAMETER  ( FRQ_MIN = 1.D3   )
      PARAMETER  ( GRA_SCL = 1.D-15 )
      INTEGER*4, ALLOCATABLE :: FRQ_I4(:)
      REAL*8     FRQ_BEG, FRQ_END, FRQ_LAST, FRQ_PREV, &
     &           FRQ_MIN_SEP, FRQ_MAX_MUL
      INTEGER*4  J1, J2, J3, J4, KFRQ, MFRQ, IFCT
!
! --- Find KFRQ -- tbe number of non-zero frequencies
! --- FRQ_BEG   -- the mininal frequency of the sequence
! --- FRQ_END   -- the maximal frequency of the sequence
!
      IFCT = 1
      KFRQ = 0
      FRQ_BEG = 0.0D0
      FRQ_END = 0.0D0
      DO 410 J1=1,NFRQ
         IF ( FRQ(J1) > FRQ_MIN ) THEN
              KFRQ = KFRQ + 1
              IF ( KFRQ == 1 ) THEN
                   FRQ_BEG = FRQ(1)
              END IF
              FRQ_END = FRQ(J1)
         END IF
 410  CONTINUE 
!
! --- Find the minimum separation between non-contagious frequency channels.
! --- The contagious frequency channels (i.e. those which follow each
! --- others without a gap) are not considered
!
      ALLOCATE ( FRQ_I4(KFRQ) )
      MFRQ = 0
      FRQ_MIN_SEP = FRQ_END - FRQ_BEG + FRQ_WD 
      FRQ_LAST = 0.0
      DO 420 J2=1,NFRQ
         IF ( FRQ(J2) > FRQ_MIN ) THEN
              IF ( (FRQ(J2) - FRQ_LAST) > FRQ_WD + FRQ_MIN ) THEN
                   MFRQ = MFRQ + 1
                   IF ( MFRQ > 1 ) THEN
                        FRQ_MIN_SEP = MIN ( FRQ_MIN_SEP, FRQ(J2) - FRQ_PREV )
                   END IF
                   FRQ_I4(MFRQ) = (FRQ(J2) - FRQ_BEG)/FRQ_SCL
                   FRQ_PREV = FRQ(J2)
              END IF
              FRQ_LAST = FRQ(J2)
         END IF
 420  CONTINUE 
      IF ( MFRQ .LE. 1 ) THEN
!
! -------- One effective channel
!
           GET_GRPAMBSP = 1.0D0/FRQ_CH
        ELSE IF ( MFRQ == 2  ) THEN
!
! -------- Two effective channels
!
           IF ( FRQ_MIN_SEP < FRQ_END - FRQ_BEG + FRQ_MIN ) THEN
                GET_GRPAMBSP = 1.0D0/FRQ_MIN_SEP 
              ELSE 
                GET_GRPAMBSP = 1.0D0/FRQ_CH
           END IF
        ELSE
!
! -------- Let is find the maximum multiple of FRQ_MIN_SEP
!
           FRQ_MAX_MUL = FRQ_SCL
           DO 430 J3=2,IDNINT(FRQ_MIN_SEP/FRQ_SCL)
              DO 440 J4=1,MFRQ
                 IF ( MOD(FRQ_I4(J4), J3) > 0 ) GOTO 430
 440          CONTINUE 
              FRQ_MAX_MUL = J3*FRQ_SCL
 430       CONTINUE 
 830       CONTINUE 
            GET_GRPAMBSP = 1.D0/FRQ_MAX_MUL
      END IF
!
! --- Rounding to a femtosecond
!
      GET_GRPAMBSP = GRA_SCL*NINT( GET_GRPAMBSP/GRA_SCL, KIND=8 ) ! NB: intermediate result
      DEALLOCATE ( FRQ_I4 )
      RETURN
      END  FUNCTION  GET_GRPAMBSP  !#!#
