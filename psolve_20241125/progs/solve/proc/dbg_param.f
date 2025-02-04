      SUBROUTINE DBG_PARAM ( B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   This program is for pringing debugging information to the file     *
! *   /tmp/param.fil                                                     *
! *                                                                      *
! *  ### 28-JAN-1997   DBG_PARAM   v1.1 (c)  L. Petrov  12-NOV-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
!
      INTEGER*4  ILUN, IUER
      LOGICAL*4  LEX
      LOGICAL*2  KBIT, KSHORT, KGLOBAL
      CHARACTER  STR*80, STR1*80
!
      REAL*8     CLO_MAX, ATM_MAX, EOP_MAX
      CHARACTER  MIN_OBJECT*5
      INTEGER*4  L_STA, L_CLO, L_ATM, L_EOP, K_LOC, K_GLO, &
     &           NP_MIN, NP_MAX, NP_A, NP_C, NP_E
      COMMON   / MAP_DBG / CLO_MAX, ATM_MAX, EOP_MAX, &
     &                     L_STA, L_CLO, L_ATM, L_EOP, &
     &                     NP_MIN, NP_MAX, MIN_OBJECT, NP_A, NP_C, NP_E, &
     &                     K_LOC, K_GLO
!
      INTEGER*4  NPARAM2
      INTEGER*2  ILPAR_LEN2
      PARAMETER  ( ILPAR_LEN2 = 20 )
      CHARACTER  LPARM(M_GPA)*(ILPAR_LEN2), FINAM_PARAM*80
      INTEGER*4  LUN, ITP, J1, K1, K2
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!CCCC
!
      IF ( SOCOM_PLUS_FIRST .NE. SPL__DONE ) THEN
           CALL ERR_LOG ( 8341, IUER, 'DBG_PARAM', 'socom_plus has not '// &
     &         'been initialized. It is fatal error' )
           RETURN
      END IF
!
! --- Openning output file for debug information
!
      CALL CLRCH ( FINAM_PARAM )
      FINAM_PARAM = '/tmp/param.fil'  !  The name of debug file
!
      LUN = 64
      ITP = 6
      INQUIRE ( FILE = FINAM_PARAM, EXIST = LEX )
      IF ( LEX ) CALL UNLINK ( FINAM_PARAM(1:I_LEN(FINAM_PARAM))//CHAR(0) )
      OPEN ( UNIT=LUN, FILE=FINAM_PARAM(1:I_LEN(FINAM_PARAM)), STATUS='NEW', &
     &       IOSTAT=ILUN )
      IF ( ILUN .NE. 0 ) THEN
           WRITE ( 6, * ) ' Error in oipenning file '// &
     &              FINAM_PARAM(1:I_LEN(FINAM_PARAM))//'  iostat=',ilun
           WRITE ( 6, * ) ' LUN = ',LUN
           CALL PAUSE ( 'dbg_param' )
      END IF
!
! --- Get names of the parameters -- array LPARM
!
      KSHORT  = .TRUE.
      KGLOBAL = .FALSE.
      KGLOBONLY = .FALSE.
      CALL GET_NAMES ( LPARM, ILPAR_LEN2, M_GPA, NPARAM2, KSHORT, KGLOBAL )
!
! --- Printout of debug information
!
         write ( ITP, * ) ' ~~~~~~~~~~~~~~~~~~~~~~~~'
         write ( ITP, * ) ' Session '//b3dobj%dbname_mes
         write ( ITP, * ) ' '
         write ( ITP, * ) ' fast_mode= ',fast_mode,' fast_dbg=',fast_dbg, &
     &                  ' fast_cov= ', fast_cov, &
     &                  ' solve_emulation= ',solve_emulation
         write ( ITP, * ) ' clk_brk_stat = ',clk_brk_stat, &
     &                  ' old_clocks=',old_clocks,' unf_clo = ',unf_clo, &
     &                  ' unf_atm=',unf_atm,' unf_eop =',unf_eop
         write ( ITP, * ) ' M_GPA=',M_GPA, ' nparam=',nparam
         do 410 j1=1,numsta
            write ( ITP, 123 ) isitn_chr(j1), &
     &                       kbit(stabit_g,int2(j1)), kbit(stabit_p,int2(j1)), &
     &                       num_brk(j1), nsparm(j1)
 123        format ( 1x,' Station ',A,'   "',2L1,'"  num_brk = ',i2, &
     &                  '  |  nsparm = ',i5 )
 410     continue
         write ( ITP, * ) '========================='
!CCCCCC
         write ( lun, * ) ' ~~~~~~~~~~~~~~~~~~~~~~~~'
         write ( lun, * ) ' Session '//b3dobj%dbname_mes
         write ( lun, * ) ' '
         write ( lun, * ) ' fast_mode= ',fast_mode,' fast_dbg=',fast_dbg, &
     &                    ' fast_cov= ', fast_cov, &
     &                    ' solve_emulation= ',solve_emulation
         write ( lun, * ) ' clk_brk_stat = ',clk_brk_stat, &
     &                    ' old_clocks=',old_clocks,' unf_clo = ',unf_clo, &
     &                    ' unf_atm=',unf_atm,' unf_eop =',unf_eop
         write ( lun, * ) ' M_GPA=',M_GPA, ' nparam=',nparam
         do 510 k1=1,numsta
            write ( lun, 123 ) isitn_chr(k1), &
     &                        kbit(stabit_g,int2(k1)), kbit(stabit_p,int2(k1)), &
     &                        num_brk(k1), nsparm(k1)
 510     continue
         IF ( FAST_MODE  .NE. F__NONE ) THEN
            write ( lun, * ) ' clo_max = ', clo_max*24.d0, &
     &                       ' atm_max = ', atm_max*24.d0, &
     &                       ' eop_max = ', eop_max*24.d0, ' ( hours) '
            write ( lun, * ) ' M_GPA=',M_GPA, ' nparam=',nparam, &
     &                       ' l_sta=', l_sta,' l_clo=',l_clo,' l_atm=',l_atm, &
     &                       ' l_eop=', l_eop
            write ( lun, * ) ' num_eop = ',num_eop,' np_max=',np_max, &
     &                       ' np_min =',np_min,' min_object = ',min_object
            write ( lun, * ) ' b3dobj%nx_clo = ',b3dobj%nx_clo, &
     &                       ' b3dobj%nx_atm = ',b3dobj%nx_atm, &
     &                       ' b3dobj%nx_eop = ',b3dobj%nx_eop
            write ( lun, * ) ' b3dobj%n_glo=',b3dobj%n_glo, &
     &                       ' b3dobj%n_loc=',b3dobj%n_loc, &
     &                       ' b3dobj%n_sgm=',b3dobj%n_sgm
            write ( lun, * ) ' b3dobj%n_clo=',b3dobj%n_clo, &
     &                       ' b3dobj%n_atm=',b3dobj%n_atm, &
     &                       ' b3dobj%n_eop=',b3dobj%n_eop
            write ( lun, * ) ' np_a=', np_a, ' np_c=', np_c,' np_e=',np_e
            write ( lun, * ) ' b3dobj%k_clo=',b3dobj%k_clo, &
     &                       ' b3dobj%k_atm=',b3dobj%k_atm, &
     &                       ' b3dobj%k_eop=',b3dobj%k_eop
            write ( lun, * ) ' b3dobj%sb=', b3dobj%sb, &
     &                       ' b3dobj%sx=', b3dobj%sx, &
     &                       ' b3dobj%nbs=', b3dobj%nbs
            write ( lun, * ) ' clo_interval  (min) = ', clo_interval*1440.d0, &
     &                       ' atm_interval  (min) = ', atm_interval*1440.d0, &
     &                       ' eop_interval  (min) = ', eop_interval*1440.d0
            write ( lun, * ) ' b3dobj%n_par = ',b3dobj%n_par
            write ( lun, * ) ' k_loc = ',k_loc,' k_glo = ',k_glo
            write ( lun, * ) ' jdate_clo(1)  = ', jdate_clo(1)
            write ( lun, * ) ' jdate_atm(1)  = ', jdate_atm(1)
            write ( lun, * ) ' jdate_eop(1)  = ', jdate_eop(1)
         END IF
!
         write ( lun, * ) 'eerm_ovr =',eerm_ovr
         write ( lun, * ) '========================='
!
! ------ Writing array lparam at debug file
!
         do 520 k2 = 1,nparam2
            write ( LUN, 220) k2, lparm(k2), b3dobj%blo(k2), b3dobj%pl(k2), &
     &                         b3dobj%CURR(k2), b3dobj%NEXT(k2)
 220        format ( 1x,' i=',i5,' lparam >>',a,'<< blo=',i3,' pl=',i5, &
     &                  '  curr=',L1,'  next=',L1 )
 520     continue
         write ( lun, * ) '========================='
!
         close ( unit=lun )
         call system ( 'chmod o+rw,g+rw,u+rw '// &
     &        finam_param(1:i_len(finam_param))//char(0) )
!
      IF ( NPARAM2 .NE. NPARAM ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NPARAM2, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NPARAM, STR1 )
           CALL ERR_LOG ( 8343, IUER, 'DBG_PARAM', 'Parametr NPARAM2 ('// &
     &          STR(1:I_LEN(STR))//') is not equal to NPARAM: ('//STR1(1:I_LEN(STR1))// &
     &         ') . It is indication of some '// &
     &         'inconsistency PARCN and GET_NAMES.' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DBG_PARAM  #!#
