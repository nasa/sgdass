      SUBROUTINE AMBIG_RESOLVE ( IALG, N, ARG, VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  AMBIG_RESOLVE  resolves 2*pi ambiguities in the array     *
! *   VAL  using algorithm IALG.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IALG ( INTEGER*4 ) -- Algorithm codes. Codes are defined in        *
! *                         phase_doctor.i                               *
! *                         Algorithm ARRA3__PHD is recommended.         *
! *      N ( INTEGER*4 ) -- Number of points in arrays T and X.          *
! *    ARG ( REAL*8    ) -- Array of arguments. dimension: N.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    VAL ( REAL*8    ) -- Array of values. Dimension: N.               *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  04-AUG-99  AMBIG_RESOLVE  v1.0 (c)  L. Petrov  24-AUG-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!!      INCLUDE    'phase_doctor.i'
      INCLUDE    'astro_constants.i'
      INCLUDE    'pima.i'
      INCLUDE    'trend_ite.i'
      INTEGER*4  IALG, N, IUER
      REAL*8     ARG(N), VAL(N)
      TYPE ( ITE_STRU ) ::  ITE_REC
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IAMB, IAMB1, IAMB2
      INTEGER*4  M_AM, M_SEG, M_BOX
      PARAMETER  ( M_AM = 4, M_SEG = 8192, M_BOX = 8 )
      INTEGER*4  KL_SEG(M_SEG), IFR_SEG(M_SEG), IAM_SEG(M_SEG), L_SEG, &
     &           IB, IL_MAX, KL_MAX, IL_IND
      REAL*8     COEF_SEG, COEF_BOX, DR_MAX, DR_SIG, RATE, VAL_MOD, VAL_EXT, &
     &           SH, DR, AV_SEG(M_SEG), SH_SEG(M_SEG), DR_SEG(M_SEG)
      LOGICAL*4  FL_SEG(M_SEG)
      PARAMETER  ( COEF_SEG = 0.45 )
      PARAMETER  ( COEF_BOX = 0.25 )
      PARAMETER  ( DR_SIG   = 2.0  )
      PARAMETER  ( DR_MAX   = 8.0  )
!!      PARAMETER  ( DR_SIG   = 200.0  )
!!      PARAMETER  ( DR_MAX   = 1000.0  )
!
!
      IF ( N .LE. 1 ) THEN
!
! -------- Too few points: Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( IALG .EQ. ARA1__PHD  .OR.  IALG .EQ. ARA2__PHD  ) THEN
!
! -------- The simplest algorithm: ambiguity are resolved on the basis of the
! -------- first differences.
! -------- Assumption: noise never exceed by maximum 1/4 ambiguity spacing.
!
           DO 410 J1=2,N
              IAMB = NINT( (VAL(J1) - VAL(J1-1))/PI2 )
              VAL(J1) = VAL(J1) - IAMB*PI2
 410       CONTINUE
      END IF
!
      IF ( IALG .EQ. ARA2__PHD  .AND.  N .GE. 3 ) THEN
!
! -------- Algorithm with linear interpolation. It is not robust in the presence
! -------- of noise.
!
           DO 420 J2=3,N
              RATE    = ( VAL(J2-1) - VAL(J2-2) )/( ARG(J2-1) - ARG(J2-2) )
              VAL_MOD = VAL(J2-1) + RATE*( ARG(J2) - ARG(J2-1) )
              IAMB    = NINT( (VAL(J2) - VAL_MOD)/PI2 )
              VAL(J2) = VAL(J2) - IAMB*PI2
 420       CONTINUE
      END IF
      IF ( IALG .EQ. ARA3__PHD  .AND.  N .GE. 2+M_AM ) THEN
!
! -------- The most powerfull algorithm which is actually in use.
!
! -------- I. Elimination of spikes. N-point spike is a group of N+2 points
! -------- which has a difference berween the first and the second point
! -------- resulting an ambiguity jump, then N-point with ambiguity jumps and
! -------- the difference between the last and the last but one point in the
! -------- group with ambiguity jump to be exactly opposite to the ambiguity
! -------- jump between the first and the second point of the group.
! -------- Example of 3-point spike:
!             ...
!            .   .
! -------- Ambiguties at the top of jump are eliminated to remove the spike
!
           DO 430 J3=1,M_AM
!
! ----------- Cycle on the number of poiint at the top of the spike
!
              DO 440 J4=2,N-J3
                 IAMB1 = NINT( (VAL(J4)    - VAL(J4-1)   )/PI2 )
                 IAMB2 = NINT( (VAL(J4+J3) - VAL(J4+J3-1))/PI2 )
                 IF ( IAMB1 .NE. 0   .AND.   IAMB1 .EQ. -IAMB2 ) THEN
                      DO 450 J5=1,J3
                         VAL(J4+J5-1) = VAL(J4+J5-1) - IAMB1*PI2
 450                  CONTINUE
                 END IF
 440          CONTINUE
 430       CONTINUE
!
! -------- II. Split the array of the point onto segments where we are able to
! -------- guarantee the lack of ambiguity jumps.
!
! -------- Initialization
!
           CALL NOUT_I4 ( M_SEG, IAM_SEG )
!
! -------- Open the first segment
!
           L_SEG = 1
           IFR_SEG(L_SEG) = 1
           KL_SEG(L_SEG)  = 1
           AV_SEG(L_SEG)  = VAL(1)
           CALL TREND_ITE ( 1, ARG(1), VAL(1), ITE_REC, SH_SEG(L_SEG), &
     &                      DR_SEG(L_SEG), -2 )
           IL_MAX = 1
           KL_MAX = 1
           IL_IND = 1
           DO 460 J6=2,N
!
! ----------- Check do we need the close the segment and to start a new one or
! ----------- we can add the J6-th point to the current segment.
! ----------- Segment is closed if
! ----------- a) the current point deviates from the average of the segment
! -----------    by more than a COEF_SEG fraction of the ambiguity OR
! ----------- b) the current point deviates from the previous point in the
! -----------    segment by more than a COEF_SEG fraction of the ambiguity
!
              IF ( DABS( VAL(J6) - AV_SEG(L_SEG) ) .GT. PI2*COEF_SEG .OR. &
     &             DABS( VAL(J6) - VAL(IL_IND)   ) .GT. PI2*COEF_SEG     ) THEN
!
! ---------------- We have to close segment. Check: is the segment good or
! ---------------- bad. Good segment has
! ---------------- a) more points than the limit;
! ---------------- b) rate of changes less by modulo than the limit
!
                   IF ( KL_SEG(L_SEG)       .GT. M_BOX  .AND. &
     &                  DABS(DR_SEG(L_SEG)) .LT. DR_MAX       ) THEN
                        FL_SEG(L_SEG) = .TRUE.
                      ELSE
                        FL_SEG(L_SEG) = .FALSE.
                   END IF
                   IF ( FL_SEG(L_SEG)  .AND.  KL_SEG(L_SEG) .GT. KL_MAX ) THEN
                        KL_MAX = KL_SEG(L_SEG)
                        IL_MAX = L_SEG
                   END IF
!
! ---------------- Open a new segment
!
                   L_SEG = L_SEG + 1
                   IFR_SEG(L_SEG) = J6
                   IL_IND         = J6
                   KL_SEG(L_SEG)  = 1
                   AV_SEG(L_SEG)  = VAL(J6)
                   CALL TREND_ITE ( KL_SEG(L_SEG), ARG(J6), VAL(J6), ITE_REC, &
     &                              SH_SEG(L_SEG), DR_SEG(L_SEG), -2 )
                 ELSE
!
! ---------------- Add the J6-th point to the previous segment
!
                   IL_IND        = J6
                   KL_SEG(L_SEG) = KL_SEG(L_SEG) + 1
!
! ---------------- Update the average
!
                   AV_SEG(L_SEG) = ( AV_SEG(L_SEG)*(KL_SEG(L_SEG)-1) + &
     &                               VAL(J6) )/KL_SEG(L_SEG)
!
! ---------------- Update the parameters of the linear trend: drift DR and
! ---------------- shift SH at the moment of the first poiint of the segment.
!
                   CALL TREND_ITE ( KL_SEG(L_SEG), ARG(J6), VAL(J6), ITE_REC, &
     &                              SH_SEG(L_SEG), DR_SEG(L_SEG), -2 )
              END IF
 460       CONTINUE
!
! -------- Close the last segment
!
           IF ( KL_SEG(L_SEG)       .GT. M_BOX  .AND. &
     &          DABS(DR_SEG(L_SEG)) .LT. DR_MAX       ) THEN
                FL_SEG(L_SEG) = .TRUE.
              ELSE
                FL_SEG(L_SEG) = .FALSE.
           END IF
           IF ( FL_SEG(L_SEG)  .AND.  KL_SEG(L_SEG) .GT. KL_MAX ) THEN
                KL_MAX = KL_SEG(L_SEG)
                IL_MAX = L_SEG
           END IF
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           type *,'>> l_seg =',l_seg,' il_max =',il_max,' kl_max=',kl_max  ! %%
!           do 570 j7=1,l_seg                                               ! %%
!              type 128, j7, ifr_seg(j7), kl_seg(j7), av_seg(j7),           ! %%
!     #             sh_seg(j7), dr_seg(j7), iam_seg(j7), fl_seg(j7)         ! %%
! 128          format ( 1x,i3,' ifr=',i4,' kl=',i4,' AV_SEG=',f7.3,         ! %%
!     #               ' | SH_SEG=',f7.3,' DR_SEG=',F7.4,' iam=',i3,' ',L1 ) ! %%
! 570       continue                                                        ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! -------- III. Now resolve ambiguties. We assume that the ambiguity are
! -------- resolved whithin the segment which is marked as good. We take the
! -------- segment with maximal number of points as the reference segment
!
           IF ( IFR_SEG(IL_MAX) .EQ. 1 .AND.  .NOT. FL_SEG(IL_MAX)  ) THEN
                WRITE ( 6, * ) ' l_seg = ',l_seg
                CALL ERR_LOG ( 5141, IUER, 'AMBIG_RESOLVE', 'No good '// &
     &              'segement was found. Ambiguity resolution process '// &
     &              'was not completed' )
                RETURN
           END IF
           IF ( L_SEG .GT. 1 ) THEN
                IF ( IFR_SEG(IL_MAX) .GT. 1 ) THEN
!
! ------------------ First resolve points before the first point of the
! ------------------ reference segment.
!
                     DO 470 J7=IFR_SEG(IL_MAX)-1,1,-1
!
! --------------------- Compute parameters of the linear trend: drift and
! --------------------- shift with respect to the first popint of the box.
! --------------------- The starting point of teh box is the earliest point
! --------------------- in the area with resolved ambiguities. The ending point
! --------------------- has the index of the starting starting point + M_BOX-1
! --------------------- Linear trend is computed with imposing constraints
! --------------------- on rate. Initial constraint is DR_SIG. But the rate
! --------------------- will appear larger by module that DR_MAX then the
! --------------------- sigma of constraint will be reduced iteratively by
! --------------------- factor 2 (maybe be more than once) to get rate less
! --------------------- by modulo than DR_MAX
!
                        IB = J7+1
                        CALL BOX_TREND ( M_BOX, DR_SIG, DR_MAX, &
     &                                   ARG(IB), VAL(IB), SH, DR )
!
! --------------------- Now extrapolate the linear trend to the point under
! --------------------- consideration.
!
                        VAL_EXT = SH + DR*(ARG(J7) - ARG(IB))
!
! --------------------- ... and resolve ambiguities
!
                        IAMB = NINT( (VAL(J7) - VAL_EXT)/PI2 )
                        VAL(J7) = VAL(J7) - IAMB*PI2
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                           type 131, j7, val_ext, val(j7), iamb, dr   ! %%%
! 131                       format ( 1x,' j7=',i3,' val_ext=',f7.3,    ! %%%
!     #                      ' val(j7)=',f7.3,' iam=',i3,' dr=',f7.3 ) ! %%%
!                 if ( dabs(dr) .gt. dr_max ) then                     ! %%%
!                      call diagi_1 ( m_box, arg(ib), val(ib), -1 )    ! %%%
!                 end if                                               ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 470                 CONTINUE
                   ELSE
                END IF
!
                IF ( IL_MAX .LT. L_SEG ) THEN
!
! ------------------ Then resolve points after the last poiunt of the
! ------------------ reference segment.
!
                     DO 480 J8=IFR_SEG(IL_MAX+1),N
!
! --------------------- Analogously compute parameters of linear trend of the
! --------------------- last MBOX points of the area with resolved ambiguities
!
                        IB = J8 - M_BOX
                        CALL BOX_TREND ( M_BOX, DR_SIG, DR_MAX, &
     &                                   ARG(IB), VAL(IB), SH, DR )
!
! --------------------- Extrapolation...
!
                        VAL_EXT = SH + DR*(ARG(J8) -ARG(IB))
!
! --------------------- and resolving ambiguities
!
                        IAMB = NINT( (VAL(J8) - VAL_EXT)/PI2 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                           type 132, j8, val_ext, val(j8), iamb, dr   ! %%%
! 132                       format ( 1x,' j8=',i3,' val_ext=',f7.3,    ! %%%
!     #                      ' val(j8)=',f7.3,' iam=',i3,' dr=',f7.3 ) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        VAL(J8) = VAL(J8) - IAMB*PI2
 480                 CONTINUE
                END IF
           END IF ! L_SEG>1
!
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  AMBIG_RESOLVE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SIMPLE_AMBIG_RESOLVE ( N, ARG, VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SIMPLE_AMBIG_RESOLVE  resolves 2*pi ambiguities in the array *
! *   VAL using a numpy-like strategy.                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      N ( INTEGER*4 ) -- Number of points in arrays T and X.          *
! *    ARG ( REAL*8    ) -- Array of arguments. dimension: N.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    VAL ( REAL*8    ) -- Array of values. Dimension: N.               *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *      11-JUL-22  SIMPLEAMBIG_RESOLVE  v1.0 (c)  L. Petrov  11-JUL-22  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!!      INCLUDE    'phase_doctor.i'
      INCLUDE    'astro_constants.i'
      INCLUDE    'pima.i'
      INTEGER*4  N, IUER
      REAL*8     ARG(N), VAL(N), DIFF(N-1), VAL_COPY(N), DIFF_MOD, &
    &            PHASE_CORR, TOTAL
      INTEGER*4  J1, J2
      IF ( N .LE. 1 ) THEN
!
! -------- Too few points: Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      DIFF = 0.0D0
      TOTAL = 0.0D0
      DO 310 J1 = 1,N-1
         DIFF(J1) = VAL(J1+1)-VAL(J1)
 310  CONTINUE
      
      DO 320 J2 = 1,N-1
         DIFF_MOD = MOD ( DIFF(J2) + PI__NUM, PI2 ) - PI__NUM
         IF ( DIFF_MOD == -PI__NUM .AND. DIFF(J2) > 0 ) THEN
              DIFF_MOD = PI__NUM
         ENDIF

         PHASE_CORR = DIFF_MOD - DIFF(J2)
         IF ( ABS ( PHASE_CORR ) < PI__NUM ) THEN
              PHASE_CORR = 0.0D0
         ENDIF

       !  IF ( PHASE_CORR .NE. 0.0D0 .AND. &
      ! &      ABS ( VAL(J2+1) + TOTAL + PHASE_CORR - VAL(J2) ) > PI__NUM ) THEN
       !       PHASE_CORR = 0.0D0
       !     ELSEIF ( PHASE_CORR == 0.0D0 .AND. &
      ! &      ABS ( VAL(J2+1) + TOTAL + PHASE_CORR - VAL(J2) ) > PI__NUM ) THEN
        !      PHASE_CORR = PI__NUM * NINT ( (VAL(J2+1) + TOTAL - VAL(J2)) / PI__NUM  )
        ! ENDIF
         IF ( ABS ( VAL(J2+1) + TOTAL + PHASE_CORR - VAL(J2) ) > PI__NUM ) THEN
              PHASE_CORR = -PI__NUM * NINT ( (VAL(J2+1) + TOTAL - VAL(J2)) / PI__NUM  )
         ENDIF
           
         TOTAL = TOTAL + PHASE_CORR
         
         VAL(J2+1) = VAL(J2+1) + TOTAL     
 320  CONTINUE

!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  AMBIG_RESOLVE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BOX_TREND ( N, DR_SIG, DR_MAX, T, X, SH, DR )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine computes parameters of lineat trend: rate of     *
! *   changes DR and shift -- vlaue of trend at the first point.         *
! *   Contraint on rate with initial sigma DR_SIG is imposed. If the     *
! *   rate exceeds by modulo the limit then sigma of the constraint is   *
! *   iteratively reduced by factfo 2 (maybe more than once) in order to *
! *   rate of change less by modulo than DR_MAX.                         *
! *                                                                      *
! *   Linear trend is TREND(t) = SH + DR*(t - T(1))                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      N ( INTEGER*4 ) -- Number of points in arrays T and X.          *
! * DR_SIG ( REAL*8    ) -- Unitial sigma of constrin on rate.           *
! * DR_MAX ( REAL*8    ) -- Maximal by modulo allowed rate of change.    *
! *      T ( REAL*8    ) -- Array of the arguments. Dimension: N.        *
! *      X ( REAL*8    ) -- Array of values. Dimension: N.               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     SH ( REAL*8    ) -- shift of the liner trend for the first point.*
! *     DR ( REAL*8    ) -- date of changes of linear trend.             *
! *                                                                      *
! *  ###  24-AUG-99   BOX_TREND    v1.0  (c)  L. Petrov  24-AUG-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N
      REAL*8     DR_SIG, DR_MAX, T(N), X(N), SH, DR
      REAL*8     TT, SW, SX, ST, SXT, STT, DET, SIG_CNS
      INTEGER*4  J1, J2
!
      SIG_CNS = DR_SIG
!
! --- Compute normal matrix
!
      SW  = 0.0
      SX  = 0.0
      ST  = 0.0
      SXT = 0.0
      STT = 1.D0/SIG_CNS**2
      DO 410 J1=1,N
         TT  = T(J1)-T(1)
         SW  = SW  + 1.0
         SX  = SX  + X(J1)
         ST  = ST  + TT
         SXT = SXT + X(J1)*TT
         STT = STT + TT**2
  410 CONTINUE
!
      DO 420 J2=1,1024
!
! ------ Compute the trend
!
         DET = SW*STT - ST*ST
         DR  = (SW*SXT - SX*ST)/DET
         SH  = (SX*STT - ST*SXT)/DET
!
! ------ Does rate of changes OK?
!
         IF ( DABS(DR) .GT. DR_MAX ) THEN
!
! ----------- Not. Reduce constrain and repeat computation of rate of change
!
              STT = STT - 1.D0/SIG_CNS**2
              SIG_CNS = SIG_CNS/2.0
              STT = STT + 1.D0/SIG_CNS**2
            ELSE
              GOTO 810
         END IF
 420  CONTINUE
 810  CONTINUE
!
      RETURN
      END  !#!  BOX_TREND  #!#
