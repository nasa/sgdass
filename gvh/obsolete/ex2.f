      PROGRAM    EX2
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  IUER
      TYPE ( GVH__STRU ) ::  GVH
      CHARACTER  BUF(128)*64, SOUNAM*8, FILENAME*128, C_STA(10)*8, TITLE*80
      REAL*8     GRDEL(2), ARR_R8(1024)
      REAL*4     ARR_R4(2048)
      INTEGER*4  MLEN, ADIM1, ADIM2, BYTES_REMAINED, L_LIN, M_LIN, N_CHP, J1
      INTEGER*2  LO_FRQ(16,60)
      PARAMETER  ( M_LIN = 128 )
      INTEGER*4  I_LEN
!
      FILENAME = '/tmp/sample.agv'
!
      IUER = -1
      CALL GVH_INIT ( GVH, IUER )
      WRITE ( 6, * ) '0: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%%
!!
      IUER = -1
      FILENAME = '/tmp/sample.agv'
      CALL GVH_READ_AGV ( GVH, 0, FILENAME, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!!
!      IUER = -1
!      FILENAME = '/tmp/sample-2.agv'
!      CALL GVH_READ_AGV ( GVH, 1, FILENAME, IUER )
!      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!!!!!!
!!      FILENAME = '/tmp/sample.bgv_1'
!      FILENAME = '/tmp/sample.bgv'
!      CALL GVH_READ_BGV ( GVH, 1, FILENAME, BYTES_REMAINED, IUER )
!      WRITE ( 6, * ) '1a: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%%
!      WRITE ( 6, * ) '1a: BYTES_REMAINED = ', BYTES_REMAINED, ' IUER=' ,IUER ! %%
!
!      FILENAME = '/tmp/sample.bgv'
!      IUER = -1
!      CALL GVH_READ_BGV ( GVH, 0, FILENAME, BYTES_REMAINED, IUER )
!      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
      WRITE ( 6, * ) '1a: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%%
      WRITE ( 6, * ) '1a: BYTES_REMAINED = ', BYTES_REMAINED, ' IUER=' ,IUER ! %%
      WRITE ( 6, * ) ' GVH%NSEG = ', GVH%NSEG
!
      N_CHP =  1
      IUER  = -1
      CALL GVH_GTEXT_CHP ( GVH, 1, N_CHP, M_LIN, L_LIN, TITLE, BUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      WRITE ( 6, '(A)' ) 'TITLE: '//TITLE(1:I_LEN(TITLE))
      DO 410 J1=1,L_LIN
         WRITE ( 6, '(A)' ) BUF(J1)(1:I_LEN(BUF(J1)))
 410  CONTINUE 
!
!      IUER = -1
!      FILENAME = '/tmp/sample.bgv'
!      CALL GVH_READ_BGV ( GVH, 2, FILENAME, BYTES_REMAINED, IUER )
!!
!      FILENAME = '/tmp/sample.bgv_2'
!      CALL GVH_READ_BGV ( GVH, 1, FILENAME, BYTES_REMAINED, IUER )
!!
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
      WRITE ( 6, * ) '1b: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%%
      WRITE ( 6, * ) '1b: BYTES_REMAINED = ', BYTES_REMAINED, ' IUER=' ,IUER ! %%
!
!!!!!!
      IUER = -1
      CALL GVH_PREGET ( GVH, IUER )
      WRITE ( 6, * ) '2: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%%
!
      MLEN = 2*8
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'GROBSDEL', 11, 0, MLEN, ADIM1, ADIM2, GRDEL, &
     &                  IUER )
      WRITE ( 6, * ) ' GRDEL=',GRDEL,' ADIM1=',ADIM1,' ADIM2=',ADIM2 ! %%%%
!
      IUER= -1
      MLEN = 16*40*2
      CALL GVH_GLCODE ( GVH, 'LO_FREQ ', 4, 2, MLEN, ADIM1, ADIM2, LO_FRQ, &
     &                  IUER )
      WRITE ( 6, * ) ' lo_frq(1,1) = ',lo_frq(1,1),' adim1=',adim1,' adim2=',adim2
      WRITE ( 6, * ) ' lo_frq(2,1) = ',lo_frq(2,1)
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'PHASE_AP', 11, 0, 2048*4, ADIM1, ADIM2, &
     &                  ARR_R4, IUER )
      WRITE ( 6, * ) 'phase_ap, 11 adim1=',adim1,' adim2=',adim2, &
     &       ' arr_r4(1-2)=', arr_r4(1), arr_r4(2)
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'PHASE_AP', 18, 0, 2048*4, ADIM1, ADIM2, &
     &                  ARR_R4, IUER )
      WRITE ( 6, * ) 'phase_ap, 12 adim1=',adim1,' adim2=',adim2
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'GRIONFRQ', 6, 0, 16, 2, 1, ARR_R8, IUER )
      WRITE ( 6, * ) 'grionfrq = ', ARR_R8(1)
!
      FILENAME = '/tmp/new_sample_new.agv'
        write ( 6, * ) ' before gvh_write_agv' ! %%%%%%%
      IUER = -1
      CALL GVH_WRITE_AGV ( GVH, 1, GVH__CRT, FILENAME, IUER )
      WRITE ( 6, * ) ' output avg-file: '//FILENAME(1:I_LEN(FILENAME))
      IUER = -1
      CALL GVH_RELEASE ( GVH, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
      END  !#!  EX2  #!#
