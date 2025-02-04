      SUBROUTINE VTD_SOU_DEBIAS ( VTD, OBS_TYP, ISOU, DER_DEL, DER_RAT, &
     &                            DELAY_BIAS, RATE_BIAS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_SOU_DEBIAS
! *                                                                      *
! * ### 29-AUG-2022 VTD_SOU_DEBIAS v1.7 (c)  L. Petrov  05-FEB-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  ISOU, IUER
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), DELAY_BIAS, &
     &           RATE_BIAS
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4   NN, MD
      PARAMETER ( MD = 3 )
      REAL*8     ALPHA_BIAS, DELTA_BIAS, EFF_FRQ_SQ, EPS
      PARAMETER  ( EPS = 1.D-5 )
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8
!
!      
      INTEGER*4   N_MOD_850N
      PARAMETER ( N_MOD_850N = 15 )
      REAL*8     ARG_NOD_MOD_850N(1-MD:N_MOD_850N), SPL_NOD_MOD_850N(1-MD:N_MOD_850N)
      DATA      ( ARG_NOD_MOD_850N(NN), SPL_NOD_MOD_850N(NN), NN=1-MD,N_MOD_850N ) / &
     &         -0.78539816,   1.73875514D+11, & ! -2
     &         -0.78539816,   1.65847394D+11, & ! -1
     &         -0.78539816,   1.30883868D+11, & !  0
     &         -0.78539816,   1.70733904D+10, & !  1
     &         -0.61709856,  -3.71133704D+10, & !  2
     &         -0.44879895,  -4.52772190D+10, & !  3
     &         -0.28049934,  -2.47139909D+10, & !  4
     &         -0.11219974,  -7.94940204D+08, & !  5
     &          0.05609987,   2.40969160D+10, & !  6
     &          0.22439948,   2.40807758D+10, & !  7
     &          0.39269908,   4.73757758D+09, & !  8
     &          0.56099869,  -1.03765848D+10, & !  9
     &          0.72929829,  -3.08600048D+10, & ! 10
     &          0.89759790,  -4.34508431D+10, & ! 11
     &          1.06589751,  -3.58739185D+10, & ! 12
     &          1.23419711,  -2.52517168D+10, & ! 13
     &          1.40249672,  -2.33701352D+10, & ! 14
     &          1.57079633,  -2.33701352D+10  & ! 15
     & /
!
      INTEGER*4   N_MOD_750R
      PARAMETER ( N_MOD_750R = 15 )
      REAL*8     ARG_NOD_MOD_750R(1-MD:N_MOD_750R), SPL_NOD_MOD_750R(1-MD:N_MOD_750R)
      DATA      ( ARG_NOD_MOD_750R(NN), SPL_NOD_MOD_750R(NN), NN=1-MD,N_MOD_750R ) / &
     &         -1.57079633,   4.24595195D+10, & ! -2
     &         -1.57079633,   2.47923447D+10, & ! -1
     &         -1.57079633,   2.85383878D+10, & !  0
     &         -1.57079633,  -7.08688307D+09, & !  1
     &         -1.34639685,  -4.36441329D+10, & !  2
     &         -1.12199738,  -2.81352696D+10, & !  3
     &         -0.89759790,  -3.70507881D+10, & !  4
     &         -0.67319843,  -3.54870456D+10, & !  5
     &         -0.44879895,  -6.10130304D+10, & !  6
     &         -0.22439948,   9.67204604D+09, & !  7
     &          0.00000000,   4.50496483D+10, & !  8
     &          0.22439948,   3.75639995D+10, & !  9
     &          0.44879895,   1.07454791D+09, & ! 10
     &          0.67319843,  -2.24996583D+10, & ! 11
     &          0.89759790,  -1.35495759D+10, & ! 12
     &          1.12199738,  -5.14366841D+09, & ! 13
     &          1.34639685,   2.65909526D+09, & ! 14
     &          1.57079633,   2.65909526D+09  & ! 15
     & /
!
      INTEGER*4   N_MAP_780S
      PARAMETER ( N_MAP_780S = 15 )
      REAL*8     ARG_NOD_MAP_780S(1-MD:N_MAP_780S), SPL_NOD_MAP_780S(1-MD:N_MAP_780S)
      DATA      ( ARG_NOD_MAP_780S(NN), SPL_NOD_MAP_780S(NN), NN=1-MD,N_MAP_780S ) / &
     &         -1.57079633,   1.40311725D+10, & ! -2
     &         -1.57079633,   1.87025274D+10, & ! -1
     &         -1.57079633,   3.16843442D+10, & !  0
     &         -1.57079633,   1.71613880D+10, & !  1
     &         -1.34639685,  -1.83060562D+10, & !  2
     &         -1.12199738,  -4.61133171D+10, & !  3
     &         -0.89759790,  -6.31927478D+10, & !  4
     &         -0.67319843,  -4.27584566D+10, & !  5
     &         -0.44879895,  -1.79125048D+10, & !  6
     &         -0.22439948,  -3.29688643D+09, & !  7
     &          0.00000000,   3.82670388D+10, & !  8
     &          0.22439948,   9.85921339D+10, & !  9
     &          0.44879895,   2.51454459D+10, & ! 10
     &          0.67319843,  -1.93240441D+10, & ! 11
     &          0.89759790,  -1.33082221D+10, & ! 12
     &          1.12199738,   9.57087630D+10, & ! 13
     &          1.34639685,   8.44680399D+10, & ! 14
     &          1.57079633,   8.44680399D+10  & ! 15
     & /
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      INTEGER*4        N_MOD_850A
      PARAMETER ( N_MOD_850A = 17 )
      REAL*8     ARG_NOD_MOD_850A(1-MD:N_MOD_850A), SPL_NOD_MOD_850A(1-MD:N_MOD_850A)
      DATA      ( ARG_NOD_MOD_850A(NN), SPL_NOD_MOD_850A(NN), NN=1-MD,N_MOD_850A ) / &
     &         -1.57079633,  -8.01934105D+08, & ! -2
     &         -1.57079633,   3.27817286D+10, & ! -1
     &         -1.57079633,   2.23708891D+10, & !  0
     &         -1.57079633,   4.87093809D+10, & !  1
     &         -1.37444679,  -2.10936833D+10, & !  2
     &         -1.17809725,   1.44212399D+11, & !  3
     &         -0.98174770,   1.34623487D+11, & !  4
     &         -0.78539816,  -2.41221252D+10, & !  5
     &         -0.58904862,  -4.15454897D+10, & !  6
     &         -0.39269908,  -3.66288753D+10, & !  7
     &         -0.19634954,  -3.19614997D+09, & !  8
     &          0.00000000,   2.45592320D+10, & !  9
     &          0.19634954,   2.29761146D+10, & ! 10
     &          0.39269908,  -1.65946391D+09, & ! 11
     &          0.58904862,  -1.87815503D+10, & ! 12
     &          0.78539816,  -4.34966058D+10, & ! 13
     &          0.98174770,  -3.87556015D+10, & ! 14
     &          1.17809725,  -2.50892456D+10, & ! 15
     &          1.37444679,  -2.65746127D+10, & ! 16
     &          1.57079633,  -2.65746127D+10  & ! 17
     & /
!
      INTEGER*4   N_MOD_850S
      PARAMETER ( N_MOD_850S = 15 )
      REAL*8     ARG_NOD_MOD_850S(1-MD:N_MOD_850S), SPL_NOD_MOD_850S(1-MD:N_MOD_850S)
      DATA      ( ARG_NOD_MOD_850S(NN), SPL_NOD_MOD_850S(NN), NN=1-MD,N_MOD_850S ) / &
     &         -1.57079633,   1.40311725D+10, & ! -2
     &         -1.57079633,   1.87025274D+10, & ! -1
     &         -1.57079633,   3.16843442D+10, & !  0
     &         -1.57079633,   1.71613880D+10, & !  1
     &         -1.34639685,  -1.83060562D+10, & !  2
     &         -1.12199738,  -4.61133171D+10, & !  3
     &         -0.89759790,  -6.31927478D+10, & !  4
     &         -0.67319843,  -4.27584566D+10, & !  5
     &         -0.44879895,  -1.79125048D+10, & !  6
     &         -0.22439948,  -3.29688643D+09, & !  7
     &          0.00000000,   3.82670388D+10, & !  8
     &          0.22439948,   9.85921339D+10, & !  9
     &          0.44879895,   2.51454459D+10, & ! 10
     &          0.67319843,  -1.93240441D+10, & ! 11
     &          0.89759790,  -1.33082221D+10, & ! 12
     &          1.12199738,   9.57087630D+10, & ! 13
     &          1.34639685,   8.44680399D+10, & ! 14
     &          1.57079633,   8.44680399D+10  & ! 15
     & /
      INTEGER*4   N_MAP_780N
      PARAMETER ( N_MAP_780N = 13 )
      REAL*8     ARG_NOD_MAP_780N(1-MD:N_MAP_780N), SPL_NOD_MAP_780N(1-MD:N_MAP_780N)
      DATA      ( ARG_NOD_MAP_780N(NN), SPL_NOD_MAP_780N(NN), NN=1-MD,N_MAP_780N ) / &
     &         -0.78539816,   1.67122511D+11, & ! -2
     &         -0.78539816,   1.71358561D+11, & ! -1
     &         -0.78539816,   1.07926799D+11, & !  0
     &         -0.78539816,  -2.62051112D+10, & !  1
     &         -0.58904862,  -6.50103514D+10, & !  2
     &         -0.39269908,  -4.44948117D+10, & !  3
     &         -0.19634954,  -9.52930374D+09, & !  4
     &          0.00000000,   2.79546736D+10, & !  5
     &          0.19634954,   2.43393955D+10, & !  6
     &          0.39269908,  -3.81275486D+08, & !  7
     &          0.58904862,  -1.80906648D+10, & !  8
     &          0.78539816,  -4.15808865D+10, & !  9
     &          0.98174770,  -3.71458437D+10, & ! 10
     &          1.17809725,  -2.49472041D+10, & ! 11
     &          1.37444679,  -2.31808302D+10, & ! 12
     &          1.57079633,  -2.31808302D+10  & ! 13
     & /
!
      INTEGER*4   N_MAP_780A
      PARAMETER ( N_MAP_780A = 17 )
      REAL*8     ARG_NOD_MAP_780A(1-MD:N_MAP_780A), SPL_NOD_MAP_780A(1-MD:N_MAP_780A)
      DATA      ( ARG_NOD_MAP_780A(NN), SPL_NOD_MAP_780A(NN), NN=1-MD,N_MAP_780A ) / &
     &         -1.57079633,  -2.92952421D+09, & ! -2
     &         -1.57079633,   3.21688370D+10, & ! -1
     &         -1.57079633,   1.92587782D+10, & !  0
     &         -1.57079633,   4.53699024D+10, & !  1
     &         -1.37444679,  -2.57396940D+10, & !  2
     &         -1.17809725,   1.42740952D+11, & !  3
     &         -0.98174770,   1.33226508D+11, & !  4
     &         -0.78539816,  -3.84921291D+10, & !  5
     &         -0.58904862,  -5.83113776D+10, & !  6
     &         -0.39269908,  -4.78249137D+10, & !  7
     &         -0.19634954,  -7.64227360D+09, & !  8
     &          0.00000000,   2.68915670D+10, & !  9
     &          0.19634954,   2.49450156D+10, & ! 10
     &          0.39269908,  -7.20209128D+08, & ! 11
     &          0.58904862,  -1.78937331D+10, & ! 12
     &          0.78539816,  -4.16368439D+10, & ! 13
     &          0.98174770,  -3.72727787D+10, & ! 14
     &          1.17809725,  -2.43710093D+10, & ! 15
     &          1.37444679,  -2.57823988D+10, & ! 16
     &          1.57079633,  -2.57823988D+10  & ! 17
     & /
!
      INTEGER*4   N_MOD_900A
      PARAMETER ( N_MOD_900A = 24 )
      REAL*8     ARG_NOD_MOD_900A(1-MD:N_MOD_900A), &
     &           SPL_NOD_RA_900A(1-MD:N_MOD_900A),  &
     &           SPL_NOD_DEC_900A(1-MD:N_MOD_900A)
      DATA      ( ARG_NOD_MOD_900A(NN), &
     &            SPL_NOD_RA_900A(NN),  &
     &            SPL_NOD_DEC_900A(NN), &
     &           NN=1-MD,N_MOD_900A ) / &
     &        -1.57079633,   -1.8954025D+10,   1.3401914D+09,  & ! -2
     &        -1.57079633,   -1.7694489D+10,   1.6784144D+09,  & ! -1
     &        -1.57079633,   -8.3364835D+09,   2.3878035D+10,  & !  0
     &        -1.57079633,   -2.4081184D+10,   7.3137477D+10,  & !  1
     &        -1.39626340,   -9.4459907D+09,   1.5417454D+11,  & !  2
     &        -1.22173048,    1.4192537D+10,   2.1861459D+11,  & !  3
     &        -1.13446401,    4.9385310D+10,   2.6185971D+11,  & !  4
     &        -1.04719755,    7.0804600D+10,   2.7839756D+11,  & !  5
     &        -0.95993109,    7.7182332D+10,   2.8221704D+11,  & !  6
     &        -0.87266463,    7.0804916D+10,   2.7839775D+11,  & !  7
     &        -0.78539816,    4.9384712D+10,   2.6185935D+11,  & !  8
     &        -0.69813170,    1.4193462D+10,   2.1861515D+11,  & !  9
     &        -0.61086524,   -9.4474031D+09,   1.5417369D+11,  & ! 10
     &        -0.52359878,   -2.4078503D+10,   7.3139089D+10,  & ! 11
     &        -0.43633231,   -8.3420529D+09,   2.3874687D+10,  & ! 12
     &        -0.34906585,   -2.2361742D+10,  -9.4143328D+09,  & ! 13
     &        -0.17453293,   -1.5951998D+10,   2.1813653D+10,  & ! 14
     &         0.00000000,   -1.1389715D+10,   3.2729807D+10,  & ! 15
     &         0.17453293,    7.4650055D+09,   1.2102274D+10,  & ! 16
     &         0.34906585,    4.9533222D+09,   1.0200611D+10,  & ! 17
     &         0.52359878,    1.4404214D+10,  -3.7548474D+10,  & ! 18
     &         0.69813170,    8.6652416D+09,  -4.5050341D+10,  & ! 19
     &         0.87266463,    1.0177878D+10,  -4.9868828D+10,  & ! 20
     &         1.04719755,    8.8890839D+09,  -3.2318332D+10,  & ! 21
     &         1.22173048,   -4.0440635D+08,  -1.6405335D+10,  & ! 22
     &         1.39626340,    2.1571907D+10,   4.1125262D+10,  & ! 23
     &         1.57079633,    2.1571907D+10,   4.1125262D+10   & ! 24
     & /
!
      INTEGER*4   N_MOD_900R
      PARAMETER ( N_MOD_900R = 24 )
      REAL*8     ARG_NOD_MOD_900R(1-MD:N_MOD_900R), &
     &           SPL_NOD_RA_900R(1-MD:N_MOD_900R),  &
     &           SPL_NOD_DEC_900R(1-MD:N_MOD_900R)
      DATA      ( ARG_NOD_MOD_900R(NN), &
     &            SPL_NOD_RA_900R(NN),  &
     &            SPL_NOD_DEC_900R(NN), &
     &           NN=1-MD,N_MOD_900R ) / &
     &        -1.57079633,    1.8954025D+10,  -1.3401914D+09,  & ! -2
     &        -1.57079633,    1.7694489D+10,  -1.6784144D+09,  & ! -1
     &        -1.57079633,    8.3364835D+09,  -2.3878035D+10,  & !  0
     &        -1.57079633,    2.4081184D+10,  -7.3137477D+10,  & !  1
     &        -1.39626340,    9.4459907D+09,  -1.5417454D+11,  & !  2
     &        -1.22173048,   -1.4192537D+10,  -2.1861459D+11,  & !  3
     &        -1.13446401,   -4.9385310D+10,  -2.6185971D+11,  & !  4
     &        -1.04719755,   -7.0804600D+10,  -2.7839756D+11,  & !  5
     &        -0.95993109,   -7.7182332D+10,  -2.8221704D+11,  & !  6
     &        -0.87266463,   -7.0804916D+10,  -2.7839775D+11,  & !  7
     &        -0.78539816,   -4.9384712D+10,  -2.6185935D+11,  & !  8
     &        -0.69813170,   -1.4193462D+10,  -2.1861515D+11,  & !  9
     &        -0.61086524,    9.4474031D+09,  -1.5417369D+11,  & ! 10
     &        -0.52359878,    2.4078503D+10,  -7.3139089D+10,  & ! 11
     &        -0.43633231,    8.3420529D+09,  -2.3874687D+10,  & ! 12
     &        -0.34906585,    2.2361742D+10,   9.4143328D+09,  & ! 13
     &        -0.17453293,    1.5951998D+10,  -2.1813653D+10,  & ! 14
     &         0.00000000,    1.1389715D+10,  -3.2729807D+10,  & ! 15
     &         0.17453293,   -7.4650055D+09,  -1.2102274D+10,  & ! 16
     &         0.34906585,   -4.9533222D+09,  -1.0200611D+10,  & ! 17
     &         0.52359878,   -1.4404214D+10,   3.7548474D+10,  & ! 18
     &         0.69813170,   -8.6652416D+09,   4.5050341D+10,  & ! 19
     &         0.87266463,   -1.0177878D+10,   4.9868828D+10,  & ! 20
     &         1.04719755,   -8.8890839D+09,   3.2318332D+10,  & ! 21
     &         1.22173048,    4.0440635D+08,   1.6405335D+10,  & ! 22
     &         1.39626340,   -2.1571907D+10,  -4.1125262D+10,  & ! 23
     &         1.57079633,   -2.1571907D+10,  -4.1125262D+10   & ! 24
     & /
!
      DELAY_BIAS = 0.0D0
      RATE_BIAS  = 0.0D0
      IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'NO' .OR. &
     &     VTD%CONF%SOU_DEBIAS_MODEL == 'NONE'    ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Get the square of the effective frequnecy
!
      IF (        OBS_TYP%DELAY_TYPE == VTD__PL__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ = -OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ = -OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SL__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__ML__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__MH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLPH__DTP ) THEN
           EFF_FRQ_SQ = OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SLSH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__MLMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PHML__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PHMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLML__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
      END IF
!
      IF ( DABS(EFF_FRQ_SQ) < 1.0D0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MAP_780N' ) THEN
           IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MAP_780N(1)      + EPS .AND. &
     &          VTD%SOU(ISOU)%DELTA < ARG_NOD_MAP_780N(N_MAP_780N) - EPS       ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780N, MD, VTD%SOU(ISOU)%DELTA, &
     &                                      ARG_NOD_MAP_780N(1), SPL_NOD_MAP_780N )
              ELSE IF ( VTD%SOU(ISOU)%DELTA < ARG_NOD_MAP_780N(1) + EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780N, MD, ARG_NOD_MAP_780N(1) + EPS , &
     &                                      ARG_NOD_MAP_780N(1), SPL_NOD_MAP_780N )
              ELSE IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MAP_780N(N_MAP_780N) - EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780N, MD, ARG_NOD_MAP_780N(N_MAP_780N) - EPS, &
     &                                      ARG_NOD_MAP_780N(1), SPL_NOD_MAP_780N )
           END IF
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MAP_780S' ) THEN
           IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MAP_780S(1)       + EPS .AND. &
     &          VTD%SOU(ISOU)%DELTA < ARG_NOD_MAP_780S(N_MAP_780S) - EPS       ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780S, MD, VTD%SOU(ISOU)%DELTA, &
     &                                      ARG_NOD_MAP_780S(1), SPL_NOD_MAP_780S )
              ELSE IF ( VTD%SOU(ISOU)%DELTA < ARG_NOD_MAP_780S(1) + EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780S, MD, ARG_NOD_MAP_780S(1) + EPS, &
     &                                      ARG_NOD_MAP_780S(1), SPL_NOD_MAP_780S )
              ELSE IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MAP_780S(N_MAP_780S) - EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780S, MD, ARG_NOD_MAP_780S(N_MAP_780S) - EPS, &
     &                                      ARG_NOD_MAP_780S(1), SPL_NOD_MAP_780S )
           END IF
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MAP_780A' ) THEN
           IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MAP_780A(1)       + EPS .AND. &
     &          VTD%SOU(ISOU)%DELTA < ARG_NOD_MAP_780A(N_MAP_780A) - EPS       ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780A, MD, VTD%SOU(ISOU)%DELTA, &
     &                                      ARG_NOD_MAP_780A(1), SPL_NOD_MAP_780A )
              ELSE IF ( VTD%SOU(ISOU)%DELTA < ARG_NOD_MAP_780A(1) + EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780A, MD, ARG_NOD_MAP_780A(1) + EPS, &
     &                                      ARG_NOD_MAP_780A(1), SPL_NOD_MAP_780A )
              ELSE IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MAP_780A(N_MAP_780A) - EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MAP_780A, MD, ARG_NOD_MAP_780A(N_MAP_780A) - EPS, &
     &                                      ARG_NOD_MAP_780A(1), SPL_NOD_MAP_780A )
           END IF
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MOD_850N' ) THEN
           IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_850N(1)          + EPS .AND. &
     &          VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_850N(N_MOD_850N) - EPS       ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850N, MD, VTD%SOU(ISOU)%DELTA, &
     &                                      ARG_NOD_MOD_850N(1), SPL_NOD_MOD_850N )
              ELSE IF ( VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_850N(1) + EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850N, MD, ARG_NOD_MOD_850N(1) + EPS, &
     &                                      ARG_NOD_MOD_850N(1), SPL_NOD_MOD_850N )
              ELSE IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_850N(N_MOD_850N) - EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850N, MD, ARG_NOD_MOD_850N(N_MOD_850N) - EPS, &
     &                                      ARG_NOD_MOD_850N(1), SPL_NOD_MOD_850N )
           END IF
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MOD_850S' ) THEN
           IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_850S(1)          + EPS .AND. &
     &          VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_850S(N_MOD_850S) - EPS       ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850S, MD, VTD%SOU(ISOU)%DELTA, &
     &                                      ARG_NOD_MOD_850S(1), SPL_NOD_MOD_850S )
              ELSE IF ( VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_850S(1) + EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850S, MD, ARG_NOD_MOD_850S(1) + EPS, &
     &                                      ARG_NOD_MOD_850S(1), SPL_NOD_MOD_850S )
              ELSE IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_850S(N_MOD_850S) - EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850S, MD, ARG_NOD_MOD_850S(N_MOD_850S) - EPS, &
     &                                      ARG_NOD_MOD_850S(1), SPL_NOD_MOD_850S )
           END IF
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MOD_750R' ) THEN
           IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_750R(1)          + EPS .AND. &
     &          VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_750R(N_MOD_750R) - EPS       ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_750R, MD, VTD%SOU(ISOU)%DELTA, &
     &                                      ARG_NOD_MOD_750R(1), SPL_NOD_MOD_750R )
              ELSE IF ( VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_750R(1) + EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_750R, MD, ARG_NOD_MOD_750R(1) + EPS, &
     &                                      ARG_NOD_MOD_750R(1), SPL_NOD_MOD_750R )
              ELSE IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_750R(N_MOD_750R) - EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_750R, MD, ARG_NOD_MOD_750R(N_MOD_750R) - EPS, &
     &                                      ARG_NOD_MOD_750R(1), SPL_NOD_MOD_750R )
           END IF
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MOD_850A' ) THEN
           IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_850A(1)          + EPS .AND. &
     &          VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_850A(N_MOD_850A) - EPS       ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850A, MD, VTD%SOU(ISOU)%DELTA, &
     &                                      ARG_NOD_MOD_850A(1), SPL_NOD_MOD_850A )
              ELSE IF ( VTD%SOU(ISOU)%DELTA < ARG_NOD_MOD_850A(1) + EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850A, MD, ARG_NOD_MOD_850A(1) + EPS, &
     &                                      ARG_NOD_MOD_850A(1), SPL_NOD_MOD_850A )
              ELSE IF ( VTD%SOU(ISOU)%DELTA > ARG_NOD_MOD_850A(N_MOD_850A) - EPS ) THEN
                DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_850A, MD, ARG_NOD_MOD_850A(N_MOD_850A) - EPS, &
     &                                      ARG_NOD_MOD_850A(1), SPL_NOD_MOD_850A )
           END IF
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MOD_900A' ) THEN
           ALPHA_BIAS = EBSPL_VAL_R8 ( N_MOD_900A, MD, VTD%SOU(ISOU)%DELTA, &
     &                                 ARG_NOD_MOD_900A(1), SPL_NOD_RA_900A )
           DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_900A, MD, VTD%SOU(ISOU)%DELTA, &
     &                                 ARG_NOD_MOD_900A(1), SPL_NOD_DEC_900A )
           DELAY_BIAS = DER_DEL(VTD__DER_RA)*ALPHA_BIAS/EFF_FRQ_SQ + &
     &                  DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ 
           RATE_BIAS  = DER_RAT(VTD__DER_RA)*ALPHA_BIAS/EFF_FRQ_SQ + &
     &                  DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == 'MOD_900R' ) THEN
           ALPHA_BIAS = EBSPL_VAL_R8 ( N_MOD_900R, MD, VTD%SOU(ISOU)%DELTA, &
     &                                 ARG_NOD_MOD_900R(1), SPL_NOD_RA_900R )
           DELTA_BIAS = EBSPL_VAL_R8 ( N_MOD_900R, MD, VTD%SOU(ISOU)%DELTA, &
     &                                 ARG_NOD_MOD_900R(1), SPL_NOD_DEC_900R )
           DELAY_BIAS = DER_DEL(VTD__DER_RA)*ALPHA_BIAS/EFF_FRQ_SQ + &
     &                  DER_DEL(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ 
           RATE_BIAS  = DER_RAT(VTD__DER_RA)*ALPHA_BIAS/EFF_FRQ_SQ + &
     &                  DER_RAT(VTD__DER_DL)*DELTA_BIAS/EFF_FRQ_SQ
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == '1MAS' ) THEN
           DELTA_BIAS = 1.D0*MAS__TO__RAD
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS
        ELSE IF ( VTD%CONF%SOU_DEBIAS_MODEL == '10MAS' ) THEN
           DELTA_BIAS = 10.D0*MAS__TO__RAD
           DELAY_BIAS = DER_DEL(VTD__DER_DL)*DELTA_BIAS
           RATE_BIAS  = DER_RAT(VTD__DER_DL)*DELTA_BIAS
        ELSE
           CALL ERR_LOG ( 5621, IUER, 'VTD_SOU_DEBIAS', 'Source position debias '// &
     &         'model '//TRIM(VTD%CONF%SOU_DEBIAS_MODEL)//' is not supported. '// &
     &         'Supported models: NONE, MOD_850N, MOD_850S, MOD_850A, MOD_750R,'// &
     &         ' MAP_780N, MAP_780S, MAP_780A, MOD_850N, MOD_850S, MOD_750R,'// &
     &         ' MOD_850A, MOD_900A' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_SOU_DEBIAS  !#!#
