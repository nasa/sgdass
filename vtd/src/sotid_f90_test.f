      PROGRAM    SOTID_F90_TEST
! ************************************************************************
! *                                                                      *
! *   Program  SOTID_F90_TEST
! *                                                                      *
! * ### 06-JUL-2002  SOTID_F90_TEST  v1.0 (c) L. Petrov 10-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      USE        SOTID_MODULE
      IMPLICIT   NONE
      INCLUDE   'sotid_data.i'
      INTEGER*4    M_STA
      PARAMETER  ( M_STA = 5 )
      TYPE ( STATID__STRU ) ::  STATID(M_STA)
      TYPE ( TIMTID__STRU ) ::  TIMTID
      TYPE ( TIDCNF__STRU ) ::  TIDCNF
      CHARACTER  C_STA(M_STA)*(SOTID__NAM_LEN), STR*80
      INTEGER*4  IMODE(8), M_TST, NN, MJD, MJD_BEG, CNF_VAL, ID, IS, &
     &           J1, J2, IUER
      REAL*8     R_STA(3,M_STA), D_REN(3), DELTA_TAI, TAI, TAI_BEG, UT1_M_TAI
!
! --- Defining station coordinates and station names
!
      C_STA(1)   = 'Wz'  ! Wettzell, Germany
      R_STA(1,1) = 4075539.895D0
      R_STA(2,1) =  931735.270D0
      R_STA(3,1) = 4801629.355D0
!
      C_STA(2)   = 'Ny'  ! NyAlesund, Spizbergen
      R_STA(1,2) = 1202462.761D0
      R_STA(2,2) = 252734.404D0
      R_STA(3,2) = 6237766.013D0
!
      C_STA(3)   = 'Ap'  ! Algopark
      R_STA(1,3) =  +918034.750D0
      R_STA(2,3) = -4346132.269D0
      R_STA(3,3) = +4561971.156D0
!
      C_STA(4)   = 'Ts'  ! Tsukuba, Japan
      R_STA(1,4) = -3957408.308D0
      R_STA(2,4) =  3310229.259D0
      R_STA(3,4) =  3737494.482D0
!
      C_STA(5)   = 'Hh'  ! Hartebeesthoek, South Africa
      R_STA(1,5) =  5085442.796D0
      R_STA(2,5) =  2668263.498D0
      R_STA(3,5) = -2768697.043D0
!
! --- Set configuration for computing displacements due to Solid Earth tides.
! --- We tell SOTID how exactly we want to compute solid Earth tides.
! --- It is sufficient to call this routine only once
!
      IUER = -1
      CALL SOTID_SET ( SOTID__GEN_ALL, SOTID__MAT00, SOTID__2D_012ORD, &
     &                 SOTID__ZF_LOVE, SOTID__3D_MDG97, TIDCNF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Compute station-dependent variables needed for computation of the
! --- displacements. It is sufficient to call this routine only once
!
      IUER = -1
      CALL SOTID_PRE ( M_STA, C_STA, R_STA, TIDCNF, STATID, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      M_TST = 2
      MJD_BEG   = 51848   ! 2000.10.31
      TAI_BEG   = 65326.0
      DELTA_TAI = 1500.D0
      UT1_M_TAI = -31.855734D0
!
      WRITE ( 6, '(A)' ) ' Results of run sotid_f90_test '
      WRITE ( 6, '(A)' ) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
      WRITE ( 6, '(A)' ) ' '
!
! --- We inquiry the status of SOTID settings. This is done for an example.
!
      IUER = -1
      CNF_VAL = SOTID_INQ ( SOTID__REQ_MODEL_2D, TIDCNF, STR, IS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) ' '//STR(1:IS)
!
      IUER = -1
      CNF_VAL = SOTID_INQ ( SOTID__REQ_ORDER_2D, TIDCNF, STR, IS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) ' '//STR(1:IS)
!
      IUER = -1
      CNF_VAL = SOTID_INQ ( SOTID__REQ_MODEL_3D, TIDCNF, STR, IS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) ' '//STR(1:IS)
!
      IUER = -1
      CNF_VAL = SOTID_INQ ( SOTID__REQ_GEN_LOVE, TIDCNF, STR, IS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) ' '//STR(1:IS)
!
      IUER = -1
      CNF_VAL = SOTID_INQ ( SOTID__REQ_ZF_LOVE,  TIDCNF, STR, IS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) ' '//STR(1:IS)
!
      IUER = -1
      CNF_VAL = SOTID_INQ ( SOTID__REQ_NW_D2,  TIDCNF, STR, IS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) ' '//STR(1:IS)
!
      IUER = -1
      CNF_VAL = SOTID_INQ ( SOTID__REQ_NW_D3, TIDCNF, STR, IS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) ' '//STR(1:IS)
!
      WRITE ( 6, '(A)' ) '----------------------------------------------------|'
      WRITE ( 6, '(A)' ) '   Date        |    |        Displacement (m)       |'
      WRITE ( 6, '(A)' ) ' MJD   TAI (s) | St |    Up        East      North  |'
!
      DO 410 J1=1,M_TST
         TAI = TAI_BEG + DELTA_TAI*(J1-1)
         MJD = MJD_BEG
         ID  = INT ( TAI/86400.0D0 )
         TAI = TAI - ID*86400.0D0
         MJD = MJD + ID
!
! ------ Compute time-dependent intermediary quantities
!
         IUER = -1
         CALL SOTID_TIM ( MJD, TAI, UT1_M_TAI, TIDCNF, TIMTID, IUER )
         WRITE ( 6, '(A)' ) '---------------|----|-----------------------'// &
     &                      '--------|'
         DO 420 J2=1,M_STA
!
! --------- Compute vector of site displacement
!
            CALL SOTID_DSP ( TIDCNF, TIMTID, STATID(J2), D_REN )
            WRITE  ( 6, 110 ) MJD, TAI, C_STA(J2)(1:2), ( D_REN(NN), NN=1,3)
 110        FORMAT ( I6,1X, F7.1,1X,'|',1X, A,1X, '|',1X, 3(F8.5,'  '), '|' )
 420     CONTINUE
 410  CONTINUE
      WRITE ( 6, '(A)' ) '----------------------------------------------'// &
     &                   '-------'
      END  !#!  SOTID_F90_TEST  #!#
