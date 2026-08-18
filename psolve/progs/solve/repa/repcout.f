      SUBROUTINE REPCOUT ( SUPKEY, M, NIPQ, IPQ, YC, NP, ARR_Y4, GMB, &
     &                     T, Y, REC_OBS, FILE_NAME_1, FILE_NAME_2 )
!
! ************************************************************************
! *                                                                      *
! *   routine SUB_GROUP_SEARCH finds the points which are "outside"      *
! *   resp. "inside" the                                                 *
! *   curser area abs(y) > abs(YC). It stores the ARR_Y4 indices         *
! *   (good points) in the NIPQ elements of the array IPQ.               *
! *   It writes changed records in RESFxx and OBSFxx                     *
! *                                                                      *
! *   called subroutines and functions:                                  *
! *   REPOBRD, REPRERD, REPREWT, REPOBWT, RECV_OBS, SUPR_OBS             *
! *                                                                      *
! *   calling routines:                                                  *
! *   REPGRSU                                                            *
! *                                                                      *
! *  ### 10-SEP-2002         REPCOUT        V.Thorandt  10-SEP-2002 ###  *
! *  02-12-05 VT     - array for record #s added                         *
! *  02-12-12 GE     - added writing of changed records (RESFxx, OBSFxx) *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      INCLUDE    'diagi.i'                    ! DIAGI      - include
      INCLUDE    'solve.i'                    ! CALC/SOLVE - include
      INCLUDE    'oborg.i'                    ! OBSFxx     - include
      INCLUDE    'obors.i'                    ! OBSFxx     - include
      INCLUDE    'resfl.i'                    ! RESFxx     - include
      INCLUDE    'socom.i'                    ! COMMxx     - include
!
      CHARACTER  FILE_NAME_1*100              ! file name variable
      CHARACTER  FILE_NAME_2*100              ! file name variable
!
      INTEGER*2  REC_OBSF( JOBSREC_WORDS )    ! OBSFxx record (JOBSREC_WORDS s. solve.i)
      EQUIVALENCE ( REC_OBSF, FJD )
      LOGICAL*4  RECV_OBS                     ! recover observation
      LOGICAL*4  DO_RECV_OBS                  ! function: value of observation
!
      CHARACTER  SUPKEY*1                     ! suppression key
      INTEGER*4  M                            ! total # of points (G+M+B)
      INTEGER*4  NIPQ                         ! # of indices in IPQ(M)
      INTEGER*4  IPQ(*)                       ! array of point indices in ARR_Y4
      REAL*4     YC                           ! curser position coordinate (value)
      INTEGER*4  NP                           ! # of points in fields ARR_Y4
      REAL*4     ARR_Y4(*)                    ! value array
      CHARACTER  GMB(*)*1                     ! flag for good(G), man.down(M) or bad(B) observation
      INTEGER*4  REC_OBS(*)                   ! record #s of baseline observations
      REAL*8     T(*)                         ! argument array (all points)
      REAL*8     Y(*)                         ! value array (all points)
!
      INTEGER*4  J1, J2                       ! loop variables
      REAL*4     YC_A                         ! abs. value of YC
!
      YC_A = ABS( YC )
      NIPQ = 0
!
!C    write(6,*) 'REPCOUT: SUPKEY=',SUPKEY
!
! --- suppress points (G-->M)
      IF ( SUPKEY .EQ. '0' ) THEN
!
         DO J1 = 1, NP
            IF ( ABS( ARR_Y4( J1 ) ) .GT. YC_A ) THEN
               NIPQ = NIPQ + 1
! ------------ index of found point
               IPQ( NIPQ ) = J1
            END IF
         END DO
!
! ------ set user flag for found point
!
         DO J2 = 1, M
            IF ( GMB( J2 ) .EQ. 'G' .AND. ABS( Y( J2 ) ) .GT. YC_A ) THEN
!
               GMB( J2 ) = 'M'
!
!C             write (6,*) 'REPCOUT(M): REC_OBS(',J2,')=',REC_OBS(J2)
!
! ------------ rewrite changed records in RESFxx and OBSFxx
!
               CALL REPRERD ( FILE_NAME_1, REC_OBS(J2) )
               CALL REPOBRD ( FILE_NAME_2, REC_OBS(J2), REC_OBSF, JOBSREC_WORDS )
!C             write(6,*) 'REPPTSU: IDATYP= ',IDATYP
               SUPSTAT(1) = SUPSTAT_RES(1)
               SUPSTAT(2) = SUPSTAT_RES(2)
               UACSUP = UACSUP_RES
               CALL SUPR_OBS ( IDATYP, SUPSTAT, UACSUP )
               SUPSTAT_RES(1) = SUPSTAT(1)
               SUPSTAT_RES(2) = SUPSTAT(2)
               UACSUP_RES = UACSUP
               CALL REPREWT ( FILE_NAME_1, REC_OBS(J2) )
               CALL REPOBWT ( FILE_NAME_2, REC_OBS(J2), REC_OBSF, JOBSREC_WORDS )
!
            END IF
         END DO
!
! --- recover points (M-->G)
!
      ELSE IF ( SUPKEY .EQ. '1' ) THEN
!
         DO J1 = 1, NP
            IF ( ABS( ARR_Y4( J1 ) ) .LT. YC_A ) THEN
               NIPQ = NIPQ + 1
! ------------ index of found point
               IPQ( NIPQ ) = J1
            END IF
         END DO
!
! ------ set user flag for found point
!
         DO J2 = 1, M
            IF ( GMB(J2) .EQ. 'M' .AND. ABS( Y(J2) ) .LT. YC_A ) THEN
               GMB(J2) = 'G'
!
!C             write (6,*) 'REPCOUT(G): REC_OBS(',J2,')=',REC_OBS(J2)
!
! ------------ rewrite changed records in RESFxx and OBSFxx
!
               CALL REPRERD ( FILE_NAME_1, REC_OBS(J2) )
               CALL REPOBRD ( FILE_NAME_2, REC_OBS(J2), REC_OBSF, JOBSREC_WORDS )
               SUPSTAT(1) = SUPSTAT_RES(1)
               SUPSTAT(2) = SUPSTAT_RES(2)
               UACSUP = UACSUP_RES
!C             write(6,*) 'REPPTSU: IDATYP= ',IDATYP
               DO_RECV_OBS = RECV_OBS ( IDATYP, SUPSTAT, UACSUP )
!C             write(6,*) 'REPPTSU: DO_RECV_OBS= ',DO_RECV_OBS
               SUPSTAT_RES(1) = SUPSTAT(1)
               SUPSTAT_RES(2) = SUPSTAT(2)
               UACSUP_RES = UACSUP
               CALL REPREWT ( FILE_NAME_1, REC_OBS(J2) )
               CALL REPOBWT ( FILE_NAME_2, REC_OBS(J2), REC_OBSF, JOBSREC_WORDS )
!
            END IF
         END DO
!
      END IF
      RETURN
      END  !#!  REPCOUT  #!#
