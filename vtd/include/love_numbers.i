!
! --- Last updated at 16-JAN-2002 22:11:52
!
!
! --- These interpolation coefficients were generated automatically by
! --- program LOVE written by L. Petrov
! --- They represents dependence of generalized Love numbers on frequency
! --- in the form  L(f) = L1 + L2*log(f) + L3*log**2(f)
! --- Coefficients were obtained on the basic of numeric values of
! --- generalized Love numbers taken from the paper
! --- P.M. Mathews, V. Dehant and J.M. Gipson "Tidal Station Displacements",
! --- Journal of Geophysical Research, 1997
!
      INTEGER*4    L_MDG97_LP
      PARAMETER  ( L_MDG97_LP = 3 )
      REAL*8         MDG97_LP_H0R(L_MDG97_LP)
      REAL*8         MDG97_LP_H0I(L_MDG97_LP)
      REAL*8         MDG97_LP_L0R(L_MDG97_LP)
      REAL*8         MDG97_LP_L0I(L_MDG97_LP)
!
      DATA           MDG97_LP_H0R &
     &           / &
     &               6.2467594D-01, &  ! 1
     &               4.3806506D-03, &  ! 2
     &               2.6754389D-04 &   ! 3
     &           /
      DATA           MDG97_LP_H0I &
     &           / &
     &              -7.9274743D-03, &  ! 1
     &              -1.1720154D-03, &  ! 2
     &              -6.7929872D-05 &   ! 3
     &           /
      DATA           MDG97_LP_L0R &
     &           / &
     &               9.1526120D-02, &  ! 1
     &               1.4563549D-03, &  ! 2
     &               8.5494813D-05 &   ! 3
     &           /
      DATA           MDG97_LP_L0I &
     &           / &
     &              -2.7596457D-03, &  ! 1
     &              -4.0880533D-04, &  ! 2
     &              -2.2394626D-05 &   ! 3
     &           /
!
! --- These interpolation coefficients were generated automatically by
! --- program LOVE written by L. Petrov
! --- They represents dependence of generalized Love numbers on frequency
! --- in the form  L(f) = L1 + L2*log(f) + L3*log**2(f)
! --- Coefficients were obtained on the basic of numeric values of
! --- generalized Love numbers taken from the paper
! --- V. Dehant, P. Defraigne and J.M. Wahr "Tides for a convective Earth",
! --- Journal of Geophysical Research, 1997
!
      INTEGER*4    L_DDW99_LP
      PARAMETER  ( L_DDW99_LP = 3 )
      REAL*8         DDW99_LP_H0R(L_DDW99_LP)
      REAL*8         DDW99_LP_L0R(L_DDW99_LP)
!
      DATA           DDW99_LP_H0R &
     &           / &
     &               6.2316934D-01, &  ! 1
     &               3.6496590D-03, &  ! 2
     &               2.3701503D-04 &   ! 3
     &           /
      DATA           DDW99_LP_L0R &
     &           / &
     &               8.8969912D-02, &  ! 1
     &               8.8789510D-04, &  ! 2
     &               5.7110724D-05 &   ! 3
     &           /
!
! --- These interpolation coefficients were generated automatically by
! --- program LOVE written by L. Petrov
! --- They represents dependence of generalized Love numbers on frequency
! --- in the form  L(f) = L0 + L1*(f*f_sc)**3 + L2/((f0 - f)*f_sc)
! --- Coefficients were obtained on the basic of numeric values of
! --- generalized Love numbers taken from the paper
! --- P.M. Mathews, V. Dehant and J.M. Gipson "Tidal Station Displacements",
! --- Journal of Geophysical Research, 1997
!
      INTEGER*4    L_MDG97_DP
      PARAMETER  ( L_MDG97_DP = 3 )
      REAL*8         MDG97_DP_NDFWE
      REAL*8         MDG97_DP_NDFWA
      REAL*8         MDG97_DP_FSC
!
      PARAMETER  ( MDG97_DP_NDFWE =   7.309004395D-05 )
      PARAMETER  ( MDG97_DP_NDFWA =   7.308957461D-05 )
      PARAMETER  ( MDG97_DP_FSC   =   1.375098708D+04 )
!
      REAL*8         MDG97_DP_HER(L_MDG97_DP)
      REAL*8         MDG97_DP_HE2(L_MDG97_DP)
      REAL*8         MDG97_DP_LER(L_MDG97_DP)
      REAL*8         MDG97_DP_LE1(L_MDG97_DP)
      REAL*8         MDG97_DP_LEP(L_MDG97_DP)
      REAL*8         MDG97_DP_HAR(L_MDG97_DP)
      REAL*8         MDG97_DP_HAI(L_MDG97_DP)
      REAL*8         MDG97_DP_LAR(L_MDG97_DP)
      REAL*8         MDG97_DP_LAI(L_MDG97_DP)
!
      DATA           MDG97_DP_HER &
     &           / &
     &               5.9881110D-01, &  ! 1
     &               1.6015490D-04, &  ! 2
     &              -1.9076883D-04 &   ! 3
     &           /
      DATA           MDG97_DP_HE2 &
     &           / &
     &              -3.6096508D-04, &  ! 1
     &              -3.0204187D-04, &  ! 2
     &              -2.3628422D-07 &   ! 3
     &           /
      DATA           MDG97_DP_LER &
     &           / &
     &               8.2774880D-02, &  ! 1
     &               7.2185300D-05, &  ! 2
     &               5.9978390D-06 &   ! 3
     &           /
      DATA           MDG97_DP_LE1 &
     &           / &
     &               1.0848122D-03, &  ! 1
     &               1.5180392D-04, &  ! 2
     &              -2.7009656D-07 &   ! 3
     &           /
      DATA           MDG97_DP_LEP &
     &           / &
     &              -1.0102050D-04, &  ! 1
     &              -1.2128431D-04, &  ! 2
     &              -1.3625924D-07 &   ! 3
     &           /
      DATA           MDG97_DP_HAR &
     &           / &
     &               6.0516799D-01, &  ! 1
     &              -6.7906990D-05, &  ! 2
     &              -1.8912633D-04 &   ! 3
     &           /
      DATA           MDG97_DP_HAI &
     &           / &
     &              -3.4031344D-03, &  ! 1
     &               1.1858048D-03, &  ! 2
     &              -1.6902620D-06 &   ! 3
     &           /
      DATA           MDG97_DP_LAR &
     &           / &
     &               8.4712401D-02, &  ! 1
     &               1.4903631D-05, &  ! 2
     &               5.7915343D-06 &   ! 3
     &           /
      DATA           MDG97_DP_LAI &
     &           / &
     &              -9.9522463D-04, &  ! 1
     &               2.5039685D-04, &  ! 2
     &               1.1087002D-07 &   ! 3
     &           /
!
! --- These interpolation coefficients were generated automatically by
! --- program LOVE written by L. Petrov
! --- They represents dependence of generalized Love numbers on frequency
! --- in the form  L(f) = L0 + L1*(f*f_sc)**3 + L2/((f0 - f)*f_sc)
! --- Coefficients were obtained on the basic of numeric values of
! --- generalized Love numbers taken from the paper
! --- V. Dehant, P. Defraigne and J.M. Wahr "Tides for a convective Earth",
! --- Journal of Geophysical Research, 1997
!
      INTEGER*4    L_DDW99_DP
      PARAMETER  ( L_DDW99_DP = 3 )
      REAL*8         DDW99_DP_H0E(L_DDW99_DP)
      REAL*8         DDW99_DP_H2E(L_DDW99_DP)
      REAL*8         DDW99_DP_L0E(L_DDW99_DP)
      REAL*8         DDW99_DP_L1E(L_DDW99_DP)
      REAL*8         DDW99_DP_L2E(L_DDW99_DP)
      REAL*8         DDW99_DP_LPE(L_DDW99_DP)
      REAL*8         DDW99_DP_H0I(L_DDW99_DP)
      REAL*8         DDW99_DP_H2I(L_DDW99_DP)
      REAL*8         DDW99_DP_L0I(L_DDW99_DP)
      REAL*8         DDW99_DP_L1I(L_DDW99_DP)
      REAL*8         DDW99_DP_L2I(L_DDW99_DP)
      REAL*8         DDW99_DP_LPI(L_DDW99_DP)
      REAL*8         DDW99_DP_NDFWE
      REAL*8         DDW99_DP_NDFWI
      REAL*8         DDW99_DP_FSC
!
      PARAMETER  ( DDW99_DP_NDFWE =   7.308036003D-05 )
      PARAMETER  ( DDW99_DP_NDFWI =   7.309020438D-05 )
      PARAMETER  ( DDW99_DP_FSC   =   1.375098708D+04 )
!
      DATA           DDW99_DP_H0E &
     &           / &
     &               5.9765562D-01, &  ! 1
     &               6.3194051D-04, &  ! 2
     &              -1.8420830D-04 &   ! 3
     &           /
      DATA           DDW99_DP_H2E &
     &           / &
     &              -4.7774903D-04, &  ! 1
     &               1.9609718D-06, &  ! 2
     &              -3.9181010D-07 &   ! 3
     &           /
      DATA           DDW99_DP_L0E &
     &           / &
     &               8.2873440D-02, &  ! 1
     &               5.9697268D-05, &  ! 2
     &               5.8393037D-06 &   ! 3
     &           /
      DATA           DDW99_DP_L1E &
     &           / &
     &               6.3030676D-04, &  ! 1
     &               1.1058257D-04, &  ! 2
     &              -3.0625365D-07 &   ! 3
     &           /
      DATA           DDW99_DP_L2E &
     &           / &
     &               7.4733200D-05, &  ! 1
     &               7.4213173D-06, &  ! 2
     &               6.8110738D-08 &   ! 3
     &           /
      DATA           DDW99_DP_LPE &
     &           / &
     &               2.4760844D-04, &  ! 1
     &              -5.8049980D-05, &  ! 2
     &              -8.2909172D-08 &   ! 3
     &           /
      DATA           DDW99_DP_H0I &
     &           / &
     &               6.0690147D-01, &  ! 1
     &               7.4542288D-04, &  ! 2
     &              -1.9804177D-04 &   ! 3
     &           /
      DATA           DDW99_DP_H2I &
     &           / &
     &              -4.9430129D-04, &  ! 1
     &               4.9482637D-06, &  ! 2
     &              -4.1694245D-07 &   ! 3
     &           /
      DATA           DDW99_DP_L0I &
     &           / &
     &               8.5284355D-02, &  ! 1
     &               5.2686623D-05, &  ! 2
     &               6.7391901D-06 &   ! 3
     &           /
      DATA           DDW99_DP_L1I &
     &           / &
     &               6.2493686D-04, &  ! 1
     &               1.2557929D-04, &  ! 2
     &              -3.2950970D-07 &   ! 3
     &           /
      DATA           DDW99_DP_L2I &
     &           / &
     &               8.1057453D-05, &  ! 1
     &              -4.0625186D-06, &  ! 2
     &               7.7205005D-08 &   ! 3
     &           /
      DATA           DDW99_DP_LPI &
     &           / &
     &               2.5594916D-04, &  ! 1
     &              -5.5076437D-05, &  ! 2
     &              -9.1104328D-08 &   ! 3
     &           /
!
! --- These interpolation coefficients were generated automatically by
! --- program LOVE written by L. Petrov
! --- They represents dependence of generalized Love numbers on frequency
! --- in the form
! --- L(f) = L0 + L1*(f*f_sc)**2 + L2*(f*f_sc)**3 + L3/((f0 - f)*f_sc)
! --- Coefficients were obtained on the basic of numeric values of
! --- generalized Love numbers taken from the paper
! --- IERS Conventions 2000, Chapter 7,
! --- http://maia.usno.navy.mil/conv2000.html
!
      INTEGER*4    L_MAT00_DP
      PARAMETER  ( L_MAT00_DP = 4 )
      REAL*8         MAT00_DP_FREQ_FCNR
      REAL*8         MAT00_DP_FREQ_FCNI
      REAL*8         MAT00_DP_FSC
!
      PARAMETER  ( MAT00_DP_FREQ_FCNR =   7.308973076D-05 )
      PARAMETER  ( MAT00_DP_FREQ_FCNI =   7.308973076D-05 )
      PARAMETER  ( MAT00_DP_FSC       =   1.371344092D+04 )
!
      REAL*8         MAT00_DP_H0R(L_MAT00_DP)
      REAL*8         MAT00_DP_H0I(L_MAT00_DP)
      REAL*8         MAT00_DP_H2R(L_MAT00_DP)
      REAL*8         MAT00_DP_L0R(L_MAT00_DP)
      REAL*8         MAT00_DP_L0I(L_MAT00_DP)
      REAL*8         MAT00_DP_L1R(L_MAT00_DP)
      REAL*8         MAT00_DP_LPR(L_MAT00_DP)
!
      DATA           MAT00_DP_H0R &
     &           / &
     &               8.4271506D-04, &  ! 1
     &              -9.8841116D-04, &  ! 2
     &               2.6670623D-04, &  ! 3
     &              -6.4494255D-06 &   ! 4
     &           /
      DATA           MAT00_DP_H0I &
     &           / &
     &              -5.5686700D-03, &  ! 1
     &               1.4993222D-02, &  ! 2
     &              -9.2148203D-03, &  ! 3
     &               2.7940955D-06 &   ! 4
     &           /
      DATA           MAT00_DP_H2R &
     &           / &
     &              -4.2917092D-05, &  ! 1
     &               1.8383782D-04, &  ! 2
     &              -1.2702684D-04, &  ! 3
     &               8.1627094D-09 &   ! 4
     &           /
      DATA           MAT00_DP_L0R &
     &           / &
     &              -7.9870603D-05, &  ! 1
     &              -5.3875626D-04, &  ! 2
     &               4.7932737D-04, &  ! 3
     &               1.7228939D-07 &   ! 4
     &           /
      DATA           MAT00_DP_L0I &
     &           / &
     &               8.7680898D-04, &  ! 1
     &              -1.9452911D-03, &  ! 2
     &               1.1817028D-03, &  ! 3
     &              -7.6218388D-08 &   ! 4
     &           /
      DATA           MAT00_DP_L1R &
     &           / &
     &              -4.0121695D-04, &  ! 1
     &               1.0836044D-03, &  ! 2
     &              -6.7007702D-04, &  ! 3
     &              -7.7634589D-09 &   ! 4
     &           /
      DATA           MAT00_DP_LPR &
     &           / &
     &               3.6110331D-04, &  ! 1
     &              -8.7551904D-04, &  ! 2
     &               5.1692059D-04, &  ! 3
     &              -6.0739961D-09 &   ! 4
     &           /
!
! --- These are the complex coefficients of the resonanse formula of
! --- P. Mathews
! --- Numerical values of the coefficeints are taken from the
! --- IERS Conventions 2000, version of 2001.10.10
!
      COMPLEX*16 FREQ_MAT00(3), H0_MAT00(0:3), H2_MAT00(0:3), L0_MAT00(0:3), &
     &           L1_MAT00(0:3), L2_MAT00(0:3), LP_MAT00(0:3)
      REAL*8     H20R_MAT00, H20I_MAT00, L20R_MAT00, L20I_MAT00, ALPAN_MAT00, &
     &           H2Z_MAT00, HPZ_MAT00, L2Z_MAT00, ANFRQ_MAT00, SIDFREQ_MAT00
      PARAMETER  ( SIDFREQ_MAT00 = 7.292115855306589D-5 )
      DATA       FREQ_MAT00 &
     &           / &
     &             (-0.0026010D0, -0.0001361D0 ), &
     &             ( 1.0023181D0,  0.0000250D0 ), &
     &             ( 0.9902600D0,  0.0007800D0 ) &
     &           /
!
      DATA       H0_MAT00 &
     &           / &
     &             (  0.60671D00, -0.2420D-2 ), &
     &             ( -0.15777D-2, -0.7630D-4 ), &
     &             (  0.18053D-3, -0.6292D-5 ), &
     &             ( -0.18616D-5,  0.1379D-6 ) &
     &           /
!
      DATA       H2_MAT00 &
     &           / &
     &             ( -0.615D-3, -0.122D-4 ), &
     &             (  0.160D-5,  0.116D-6 ), &
     &             (  0.201D-6,  0.279D-8 ), &
     &             ( -0.329D-7, -0.217D-8 ) &
     &           /
!
      DATA       L0_MAT00 &
     &           / &
     &             (  0.84963D-1, -0.7395D-3 ), &
     &             ( -0.22107D-3, -0.9646D-5 ), &
     &             ( -0.54710D-5, -0.2990D-6 ), &
     &             ( -0.29904D-7, -0.7717D-8 ) &
     &           /
!
      DATA       L1_MAT00 &
     &           / &
     &             (  0.121D-2,  0.136D-6 ), &
     &             ( -0.316D-5, -0.166D-6 ), &
     &             (  0.272D-6, -0.858D-8 ), &
     &             ( -0.545D-8,  0.827D-11) &
     &           /
!
      DATA       L2_MAT00 &
     &           / &
     &             (  0.19334D-3, -0.3819D-5 ), &
     &             ( -0.50331D-6,  0.1639D-7 ), &
     &             ( -0.66460D-8,  0.5076D-9 ), &
     &             (  0.10372D-7,  0.7511D-9 ) &
     &           /
!
      DATA       LP_MAT00 &
     &           / &
     &             ( -0.221D-3, -0.474D-7 ), &
     &             (  0.576D-6,  0.303D-7 ), &
     &             (  0.128D-6, -0.378D-8 ), &
     &             ( -0.655D-8, -0.291D-9 ) &
     &           /
      PARAMETER  ( H20R_MAT00  =  0.5998D0  ) ! zonal
      PARAMETER  ( L20R_MAT00  =  0.0831D0  ) ! zonal
      PARAMETER  ( H20I_MAT00  = -9.96D-4   ) ! zonal
      PARAMETER  ( L20I_MAT00  = -3.01D-4   ) ! zonal
      PARAMETER  ( ALPAN_MAT00 =  0.15D0    )
      PARAMETER  ( ANFRQ_MAT00 =  3.1415D-2 )
      PARAMETER  ( H2Z_MAT00   = -0.0006D0  ) ! zonal and semidiurnal
      PARAMETER  ( HPZ_MAT00   =  0.0001D0  ) ! zonal
      PARAMETER  ( L2Z_MAT00   =  0.0002D0  ) ! zonal and semidiurnal
!
      REAL*8     H22R_MAT00, H22I_MAT00, L22R_MAT00, L22I_MAT00, L221_MAT00
      PARAMETER  ( H22R_MAT00 =  0.6078D0 ) ! semidiurnal
      PARAMETER  ( H22I_MAT00 = -0.0022D0 ) ! semidiurnal
      PARAMETER  ( L22R_MAT00 =  0.0847D0 ) ! semidiurnal
      PARAMETER  ( L22I_MAT00 = -0.0007D0 ) ! semidiurnal
      PARAMETER  ( L221_MAT00 =  0.0024D0 ) ! semidiurnal
!
! --- These coefficeints were taken from the paper
! --- Mathews, P.M., Love numbers and gravimetric factor for diurnal tides,
! --- {\it Journal of the Geodetic Society of Japan}, {\it 47}(1),
! --- 231--236, 2001.
!
      COMPLEX*16 FREQ_MAT01(3), H0_MAT01(0:3), H2_MAT01(0:3), L0_MAT01(0:3), &
     &           L1_MAT01(0:3), L2_MAT01(0:3), LP_MAT01(0:3)
      REAL*8     H20R_MAT01, H20I_MAT01, L20R_MAT01, L20I_MAT01, ALPAN_MAT01, &
     &           H2Z_MAT01, HPZ_MAT01, L2Z_MAT01, ANFRQ_MAT01, SIDFREQ_MAT01
      PARAMETER  ( SIDFREQ_MAT01 = 7.292115855306589D-5 )
      DATA       FREQ_MAT01 &
     &           / &
     &             (-0.0025988D0, -0.0001443D0 ), &
     &             ( 1.0023181D0,  0.0000253D0 ), &
     &             ( 0.998935D0,   0.000758D0  ) &
     &           /
!
      DATA       H0_MAT01 &
     &           / &
     &             (  0.60671D0,  -0.2416D-2 ), &
     &             ( -0.15764D-2, -0.8125D-4 ), &
     &             (  0.18053D-3, -0.6429D-5 ), &
     &             ( -0.18715D-5,  0.1234D-6 ) &
     &           /
!
      DATA       H2_MAT01 &
     &           / &
     &             ( -0.615D-3, -0.123D-4 ), &
     &             (  0.160D-5,  0.121D-6 ), &
     &             (  0.201D-6,  0.250D-8 ), &
     &             ( -0.327D-7, -0.205D-8 ) &
     &           /
!
      DATA       L0_MAT01 &
     &           / &
     &             (  0.84963D-1, -0.7388D-3 ), &
     &             ( -0.22090D-3, -0.1034D-4 ), &
     &             ( -0.54720D-5, -0.2954D-6 ), &
     &             ( -0.29128D-7, -0.6847D-8 ) &
     &           /
!
      DATA       L1_MAT01 &
     &           / &
     &             (  0.121D-2,  0.145D-6 ), &
     &             (  0.315D-5, -0.175D-6 ), &
     &             (  0.272D-6, -0.880D-8 ), &
     &             ( -0.545D-8, -0.168D-11) &
     &           /
!
      DATA       L2_MAT01 &
     &           / &
     &             ( -0.31789D-3, -0.3894D-5 ), &
     &             (  0.82550D-6,  0.5594D-7 ), &
     &             ( -0.63927D-8,  0.5565D-9 ), &
     &             (  0.10307D-7,  0.7051D-9 ) &
     &           /
!
      DATA       LP_MAT01 &
     &           / &
     &             ( -0.221D-3, -0.500D-7 ), &
     &             (  0.576D-6,  0.321D-7 ), &
     &             (  0.128D-6, -0.390D-8 ), &
     &             ( -0.653D-8, -0.277D-9 ) &
     &           /
      PARAMETER  ( H20R_MAT01  =  0.5998D0  )
      PARAMETER  ( L20R_MAT01  =  0.0831D0  )
      PARAMETER  ( H20I_MAT01  = -9.96D-4   )
      PARAMETER  ( L20I_MAT01  = -3.01D-4   )
      PARAMETER  ( ALPAN_MAT01 =  0.15D0    )
      PARAMETER  ( ANFRQ_MAT01 =  3.1415D-2 )
      PARAMETER  ( H2Z_MAT01   = -0.0006D0  )
      PARAMETER  ( HPZ_MAT01   =  0.0001D0  )
      PARAMETER  ( L2Z_MAT01   =  0.0002D0  )
