      SUBROUTINE FIND_SUBNET ( L_BAS, LIS_BAS, L_STA, LIS_STA, LN_SNT, &
     &                         LIS_BASSNT, LIS_STASNT )
! ************************************************************************
! *                                                                      *
! *   Routine  FIND_SUBNET  scans the list of baselines and determines   *
! *   how many subnets have the net. Two baselines are considered as     *
! *   belonged to the different networks if there are no baselines which *
! *   would connect stations from these baselines.                       *
! *                                                                      *
! *   Number of subnetworks is counted. Arrays of stations and baselines *
! *   are formed which hold the indices of the subnetwork for the        *
! *   stations and baselines.                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      L_BAS ( INTEGER*4 ) -- Number of baselines.                     *
! *    LIS_BAS ( INTEGER*4 ) -- List of baselines. Each element contains *
! *                             a baseline code assigned by routine      *
! *                             NSTBA                                    *
! *      L_STA ( INTEGER*4 ) -- Number of stations.                      *
! *    LIS_STA ( INTEGER*4 ) -- List of stations. Each element contains  *
! *                             a station code assigned by SDBH          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     LN_STA ( INTEGER*4 ) -- Number of subnetworks.                   *
! * LIS_BASSNT ( INTEGER*4 ) -- Array  of subnetwork indices for         *
! *                             baselines. LIS_BASSNT(k) contains the    *
! *                             index of the subnetwork for the k-th     *
! *                             baseline.                                *
! * LIS_STASNT ( INTEGER*4 ) -- Array  of subnetwork indices for         *
! *                             stations. LIS_STASNT(k) contains the     *
! *                             index of the subnetwork for the k-th     *
! *                             station.                                 *
! *                                                                      *
! *  ###  08-OCT-98   FIND_SUBNET  v1.0  (c)  L. Petrov  08-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INTEGER*4  L_BAS, LIS_BAS(L_BAS), L_STA, LIS_STA(L_STA), LN_SNT, &
     &           LIS_BASSNT(L_BAS), LIS_STASNT(L_STA)
      INTEGER*4  LN_STA_OLD, LN_STA, LISN_STA(MO_STA), J1, J2, J3, J4, J5, &
     &           IST1, IST2, IPL_ST1, IPL_ST2, KST1, KST2, KPL_ST1, KPL_ST2, &
     &           K_BAS, MAX_SNT
      INTEGER*4  IFIND_PL
      CHARACTER  CBAST_NUM*9
!
! --- First: initialization
!
      LN_SNT = 0
      CALL NOUT_I4 ( L_BAS, LIS_BASSNT )
      CALL NOUT_I4 ( L_STA, LIS_STASNT )
      IF ( L_BAS .LE. 0 ) THEN
!
! -------- singular case: nothing to do
!
           RETURN
      END IF
      MAX_SNT = L_BAS
!
      DO 410 J1=1,MAX_SNT
!
! ------ Scan the list of subnetworks. We need to find a baseline which does
! ------ not belong to any network
!
         DO 420 J2=1,L_BAS
            IF ( LIS_BASSNT(J2) .LE. 0 ) THEN
!
! -------------- We found it!
!
                 LN_SNT = LN_SNT + 1
                 LIS_BASSNT(J2) = LN_SNT  ! assign the index of the network
!
! -------------- N_BAS -0 index of the initial baseline of the J1-th subnetwork
!
                 K_BAS = J2
                 GOTO 810    ! continue our bisiness
            END IF
 420     CONTINUE
!
! ------ We didn't find? Oh, it means that assign subnetwork index to all
! ------ baselines. Good bye!
!
         GOTO 820
 810     CONTINUE
!
! ------ Find: which station forms the baseline
!
         CALL NBAST ( LIS_BAS(K_BAS), IST1, IST2 )
         IPL_ST1 = IFIND_PL ( L_STA, LIS_STA, IST1 )
         IPL_ST2 = IFIND_PL ( L_STA, LIS_STA, IST2 )
         IF ( IPL_ST1 .GE. 1 ) LIS_STASNT(IPL_ST1) = LN_SNT
         IF ( IPL_ST2 .GE. 1 ) LIS_STASNT(IPL_ST2) = LN_SNT
!
! ------ Put ths station forming an intial baseline of the sunbetwork to the
! ------ lists.
!
         LN_STA = 0
         CALL ADD_LIS ( MO_STA, LN_STA, LISN_STA, IST1, -3 )
         CALL ADD_LIS ( MO_STA, LN_STA, LISN_STA, IST2, -3 )
!
! ------ Now scan many times all baselines and try to assign the baseline
! ------ current subnetwork.
!
         DO 430 J3=1,MO_STA
            LN_STA_OLD = LN_STA
            DO 440 J4=1,L_BAS
               IF ( LIS_BASSNT(J4) .LE. 0 ) THEN
!
! ----------------- Split the baseline to the stations
!
                    CALL NBAST ( LIS_BAS(J4), KST1, KST2 )
!
! ----------------- If at least one station of the baseline belongs to the
! ----------------- list of stations of the subnetwotk then this baseline is
! ----------------- considered as belonging to the subnetwork. Both stations
! ----------------- are added to the station list of the subnetwork
!
                    IF ( IFIND_PL ( LN_STA, LISN_STA, KST1 ) .GE. 1 .OR. &
     &                   IFIND_PL ( LN_STA, LISN_STA, KST2 ) .GE. 1      ) THEN
!
                         LIS_BASSNT(J4) = LN_SNT
!
! ---------------------- Set values of station subnetwork list.
!
                         KPL_ST1 = IFIND_PL ( L_STA, LIS_STA, KST1 )
                         KPL_ST2 = IFIND_PL ( L_STA, LIS_STA, KST2 )
                         IF ( KPL_ST1 .GE. 1 ) LIS_STASNT(KPL_ST1) = LN_SNT
                         IF ( KPL_ST2 .GE. 1 ) LIS_STASNT(KPL_ST2) = LN_SNT
!
                         CALL ADD_LIS ( MO_STA, LN_STA, LISN_STA, KST1, -3 )
                         CALL ADD_LIS ( MO_STA, LN_STA, LISN_STA, KST2, -3 )
                    END IF
               END IF
 440        CONTINUE
!
! --------- We look: is the number of baselines participated in the subnetwork
! --------- increased? If yes, we should repeat the process. The true is that
! --------- we assume that list of baselines is not sorted
!
            IF ( LN_STA .EQ. LN_STA_OLD ) GOTO 830
 430     CONTINUE
 830     CONTINUE
 410  CONTINUE
 820  CONTINUE
      DO 450 J5=1,L_BAS
         IF ( LIS_BASSNT(J5) .LE. 0 ) THEN
!
! ----------- It is impossible event. Make posthumous printout and die.
!
              WRITE ( 6, * ) ' l_sta = ',l_sta,' l_bas = ',l_bas,' ln_snt = ',ln_snt
              WRITE ( 6, * ) ' lis_sta = ', ( lis_sta(j1), j1=1,l_sta )
              WRITE ( 6, * ) ' lis_bas = ', ( cbast_num(lis_bas(j1)), j1=1,l_bas )
              WRITE ( 6, * ) ' lis_stasnt = ', (lis_stasnt(j1), j1=1,l_sta )
              WRITE ( 6, * ) ' lis_bassnt = ', (lis_bassnt(j1), j1=1,l_bas )
              WRITE ( 6, * ) ' ln_sta = ',ln_sta
              WRITE ( 6, * ) ' lisn_sta = ', ( lisn_sta(j1), j1=1,ln_sta )
              STOP 'FIND_SUBNET: Abnormal termination. Bug?'
         END IF
 450  CONTINUE
      RETURN
      END  !#!  FIND_SUBNET  #!#
