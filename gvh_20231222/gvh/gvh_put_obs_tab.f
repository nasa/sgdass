      SUBROUTINE GVH_PUT_OBS_TAB ( FL_ENDIAN_SWAP, NUMOBS, NUMSTA, NOBS_STA, &
     &                             OBS_TAB_IN, OBS_TAB_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine GVH_PUT_OBS_TAB
! *                                                                      *
! *  ### 23-NOV-2001 GVH_PUT_OBS_TAB v1.0 (c) L. Petrov 23-NOV-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      LOGICAL*4  FL_ENDIAN_SWAP
      INTEGER*4  NUMOBS, NUMSTA, NOBS_STA(NUMSTA), OBS_TAB_IN(3,NUMOBS), &
     &           OBS_TAB_OUT(5,NUMOBS), IUER
      INTEGER*4  J1, J2, J3, J4, NOBS_COU(GVH__MSTA), LAST_SCA(GVH__MSTA), &
     &           LAST_IND(GVH__MSTA)
      CHARACTER  STR1*32, STR2*32, STR3*32
      INTEGER*4  I_LEN
!
      CALL NOUT_I4 ( NUMSTA, NOBS_COU )
      CALL NOUT_I4 ( NUMSTA, LAST_SCA )
!
! --- Count the number of observations at each station and put indices
!
      DO 410 J1=1,NUMOBS
         IF ( FL_ENDIAN_SWAP ) THEN
              CALL ENDIAN_CNV_I4 ( OBS_TAB_IN(1,J1) )
              CALL ENDIAN_CNV_I4 ( OBS_TAB_IN(2,J1) )
              CALL ENDIAN_CNV_I4 ( OBS_TAB_IN(3,J1) )
         END IF
!
         IF ( OBS_TAB_IN(2,J1) .LT. 1  .OR.  OBS_TAB_IN(2,J1) .GT. NUMSTA ) THEN
              CALL CLRCH ( STR1 )
              CALL CLRCH ( STR2 )
              CALL CLRCH ( STR3)
              CALL INCH  ( J1, STR1 )
              CALL INCH  ( OBS_TAB_IN(2,J1), STR2 )
              CALL INCH  ( NUMSTA, STR3 )
              CALL ERR_LOG ( 4131, IUER, 'GVH_PUT_OBS_TAB', 'Erroneous '// &
     &             'value of the (2,'//STR1(1:I_LEN(STR1))//'-th) element of '// &
     &             'the observation table: '//STR2(1:I_LEN(STR2))//' -- out '// &
     &             ' of range (1,'//STR3(1:I_LEN(STR3))//')' )
              RETURN
         END IF
!
         IF ( OBS_TAB_IN(3,J1) .LT. 1  .OR.  OBS_TAB_IN(3,J1) .GT. NUMSTA ) THEN
              CALL CLRCH ( STR1 )
              CALL CLRCH ( STR2 )
              CALL CLRCH ( STR3)
              CALL INCH  ( J1, STR1 )
              CALL INCH  ( OBS_TAB_IN(3,J1), STR2 )
              CALL INCH  ( NUMSTA, STR3 )
              CALL ERR_LOG ( 4132, IUER, 'GVH_PUT_OBS_TAB', 'Erroneous '// &
     &             'value of the (3,'//STR1(1:I_LEN(STR1))//'-th) element of '// &
     &             'the observation table: '//STR2(1:I_LEN(STR2))//' -- out '// &
     &             ' of range (1,'//STR3(1:I_LEN(STR3))//')' )
              RETURN
         END IF
!
         IF ( LAST_SCA(OBS_TAB_IN(2,J1)) .NE. OBS_TAB_IN(1,J1) ) THEN
              NOBS_COU(OBS_TAB_IN(2,J1)) = NOBS_COU(OBS_TAB_IN(2,J1)) + 1
              OBS_TAB_OUT(4,J1) = NOBS_COU(OBS_TAB_IN(2,J1))
              LAST_SCA(OBS_TAB_IN(2,J1)) = OBS_TAB_IN(1,J1)
         END IF
!
         IF ( LAST_SCA(OBS_TAB_IN(3,J1)) .NE. OBS_TAB_IN(1,J1) ) THEN
              NOBS_COU(OBS_TAB_IN(3,J1)) = NOBS_COU(OBS_TAB_IN(3,J1)) + 1
              OBS_TAB_OUT(5,J1) = NOBS_COU(OBS_TAB_IN(3,J1))
              LAST_SCA(OBS_TAB_IN(3,J1)) = OBS_TAB_IN(1,J1)
         END IF
 410  CONTINUE
!
      CALL NOUT_I4 ( NUMSTA, LAST_SCA )
      DO 420 J2=1,NUMOBS
         OBS_TAB_OUT(1,J2) = OBS_TAB_IN(1,J2)
         OBS_TAB_OUT(2,J2) = OBS_TAB_IN(2,J2)
         OBS_TAB_OUT(3,J2) = OBS_TAB_IN(3,J2)
         IF ( LAST_SCA(OBS_TAB_IN(2,J2)) .NE. OBS_TAB_OUT(1,J2) ) THEN
              IF ( OBS_TAB_IN(2,J2) .GT. 1 ) THEN
                   DO 430 J3=1,OBS_TAB_IN(2,J2)-1
                      OBS_TAB_OUT(4,J2) = OBS_TAB_OUT(4,J2) + NOBS_STA(J3)
 430               CONTINUE
              END IF
              LAST_SCA(OBS_TAB_IN(2,J2)) = OBS_TAB_OUT(1,J2)
              LAST_IND(OBS_TAB_IN(2,J2)) = OBS_TAB_OUT(4,J2)
            ELSE
              OBS_TAB_OUT(4,J2) = LAST_IND(OBS_TAB_IN(2,J2))
         END IF
!
         IF ( LAST_SCA(OBS_TAB_IN(3,J2)) .NE. OBS_TAB_OUT(1,J2) ) THEN
              IF ( OBS_TAB_IN(3,J2) .GT. 1 ) THEN
                   DO 440 J4=1,OBS_TAB_IN(3,J2)-1
                      OBS_TAB_OUT(5,J2) = OBS_TAB_OUT(5,J2) + NOBS_STA(J4)
 440               CONTINUE
              END IF
              LAST_SCA(OBS_TAB_IN(3,J2)) = OBS_TAB_OUT(1,J2)
              LAST_IND(OBS_TAB_IN(3,J2)) = OBS_TAB_OUT(5,J2)
            ELSE
              OBS_TAB_OUT(5,J2) = LAST_IND(OBS_TAB_IN(3,J2))
         END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  GVH_PUT_OBS_TAB  #!#
