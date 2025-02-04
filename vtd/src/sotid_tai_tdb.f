        SUBROUTINE SOTID_TAI_TDB ( MJD, TAI, TDB )
! ************************************************************************
! *                                                                      *
! *     Routine SOTID_TAI_TDB computes the argument TDB on the moment of *
! *   time TAI and integer modified Julian date MJD. Computation is done *
! *   according to the expression 2.222-1 in page 42 of "Explanatory     *
! *   Supplement to the Astronomical Almanac / edited by                 *
! *   P.K.Seidelmann / University Science Book, 1992.                    *
! *                                                                      *
! *   Accuracy of this expression is 20 microseconds of time.            *
! *                                                                      *
! * ________________________ Input parameters:  ________________________ *
! *                                                                      *
! *       MJD ( INTEGER*4 ) -- Integer fraction of the Modified Julian   *
! *                            Day -- MJD at the midnight of the         *
! *                            observations. It has the meanin the       *
! *                            INTEGER number of days elapsed from       *
! *                            0 hours of 01 January 2000).              *
! *       TAI ( REAL*8    ) -- Time at TAI scale of the mement under     *
! *                            considertion (in sec).                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       TDB ( REAL*8    ) -- Time at TDB scale of the mement under     *
! *                            considertion (in sec).                    *
! *                                                                      *
! * ### 05-OCT-1993  SOTID_TAI_TDB  v1.1 (c)  L. Petrov  11-JUL-2002 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT    NONE
        INTEGER*4   MJD
        INCLUDE    'sotid_data.i'
        REAL*8      TAI, TDB, GR_RAD, G, BT, DT
        PARAMETER ( GR_RAD=180.D0/SOTID__PI ) ! Transformation deg to rad
!
! ----- The number of days alapsed from the fundamenta lepoxh J2000.0
!
        DT=(MJD - SOTID__MJD_J2000) - 0.5D0 + TAI/86400.D0
!
! ----- G  --  Mean anomaly of the Earth's orbit
!
        G=( 357.53D0  +  0.9856003D0 *DT ) /GR_RAD
!
! ----- BT  --  Difference TDB-TDT
!
        BT=0.001658D0 *DSIN(G)   +   0.000014D0 *DSIN(2.D0*G)
        TDB=TAI + 32.184D0 + BT
!
        RETURN
        END  SUBROUTINE SOTID_TAI_TDB
