      PROGRAM    SC_APPLY_LAUNCH  
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL SC_APPLY()
      END  PROGRAM  SC_APPLY_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SC_APPLY()
! ************************************************************************
! *                                                                      *
! *   Program  SC_APPLY
! *                                                                      *
! *  ### 05-FEB-2016   SC_APPLY    v4.0 (c)  L. Petrov  24-FEB-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ), POINTER :: MALO(:)
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_SC, HEB_PRS, HEB_MOD, HEB_OUT
      CHARACTER  FILIN*128, FILPRS*128, FILCNF*128, FILOUT*128, &
     &           STR*128, MODE_STR*8
      REAL*4,    ALLOCATABLE :: SPH_PRS(:,:,:)
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 ) 
      REAL*8     TIM_R8, TIM
      INTEGER*4  J1, J2, J3, J4, IVRB, DEG_SC, LIND, INDS(2,MIND), N_FRQ, &
     &           DIM_LUT, IND_PAR, IND_FRQ(2*MALO__MFRQ), NLON, NLAT, &
     &           IND3_SECT(2), IND4_SECT(2), IUER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      REAL*8,    EXTERNAL :: WALL_TIMER 
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
!     $MALO_DIR/bin/sc_apply ocean /s0/temp/load/toc_fes2012_d2699.heb       /s0/temp/load/fes2012_d2699.heb /s0/sc_load/mod44w_sc_21599_2699.heb /s0/temp/load/toc_fes2012_d2699_m3.heb
!
      IVRB = 1
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, * ) 'Usage: sc_apply mode filcnf filin_dspl filprs filout_dspl [ivrb]'
           CALL EXIT ( 1 ) 
         ELSE 
           CALL GETARG ( 1, MODE_STR ) 
           CALL GETARG ( 2, FILCNF   ) 
           CALL GETARG ( 3, FILIN    ) 
           CALL GETARG ( 4, FILPRS   ) 
           CALL GETARG ( 5, FILOUT   ) 
           IF ( IARGC() .LE. 6 ) THEN
                CALL GETARG ( 6, STR  ) 
                CALL CHIN ( STR, IVRB )
           END IF
      END IF
!
      IF ( MODE_STR == 'pres' ) THEN
           CONTINUE 
         ELSE IF ( MODE_STR == 'ocean' ) THEN
           CONTINUE 
         ELSE IF ( MODE_STR == 'ocean0' ) THEN
           CONTINUE 
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6801, IUER, 'SC_APPLY', 'Thge first argument mode '// &
     &         'should be pres, or ocean, or ocean0 while '//STR(1:I_LEN(STR))// &
     &         ' was specified' )
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate malo object
!
      ALLOCATE ( MALO(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6802, IUER, 'SC_APPLY', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize malo object
!
      IUER = -1
      CALL MALO_INIT ( MALO(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6803, IUER, 'SC_APPLY', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Parse malo config file
!
      IUER = -1
      CALL MALO_CONFIG ( FILCNF, MALO(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6804, IUER, 'SC_APPLY', 'Failure in parsing MALO '// &
     &         'configuration file '//FILCNF )
           CALL EXIT ( 1 )
      END IF
!
! --- Read input loading field
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEB_IN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6805, IUER, 'SC_APPLY', 'Failed to read input '// &
     &         'file with displacements '//FILIN )
           CALL EXIT ( 1 )
      END IF
      IF ( .NOT. MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
!
! -------- Read input pressure fields
!
           IUER = -1
           CALL READ_HEB ( FILPRS, HEB_PRS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6806, IUER, 'SC_APPLY', 'Failed to read input '// &
     &              'file with pressure '//FILPRS )
                CALL EXIT ( 1 )
           END IF
         ELSE IF ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_HAR_ONLY ) THEN
!
! -------- Read input prssrue model and put it in HEB_PRS!
!
           IUER = -1
           CALL READ_HEB ( MALO(1)%CONF%FINAM_MODEL, HEB_PRS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6807, IUER, 'SC_APPLY', 'Failed to read input '// &
     &              'file with pressure '//FILPRS )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Read input sampling correction file
!
      IUER = -1
      CALL READ_HEB ( MALO(1)%CONF%SC_FILE, HEB_SC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6808, IUER, 'SC_APPLY', 'Failed to read input '// &
     &         'file with samplinig correction '//MALO(1)%CONF%SC_FILE )
           CALL EXIT ( 1 )
      END IF
!
      IF ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT  .OR. &
     &     MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUB_NOHAR      ) THEN
!
! -------- Read the header ofthe surface pressure regression model
!
           IUER = -1
           CALL READ_HEB_HEADER ( MALO(1)%CONF%FINAM_MODEL, HEB_MOD, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6809, IUER, 'SC_APPLY', 'Error in '// &
     &             'an attempt to read heb-file with the model of surface '// &
     &             'pressure '//MALO(1)%CONF%FINAM_MODEL )
                CALL EXIT ( 1 )
           END IF   
      END IF
!
      IND4_SECT = 1
      DO 410 J1=1,MALO(1)%NMDC
         IF ( ( MALO(1)%MODC(J1)%TYP == MALO__CNST .OR. &
     &          MALO(1)%MODC(J1)%TYP == MALO__JMP       ) .AND. &
              ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT  .OR. &
     &          MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUB_NOHAR      ) ) THEN
     &          
!
! ----------- Get the J1-th component of the regression model
!
              IND3_SECT = J1
              IUER = -1
              CALL READ_HEB_SECT ( MALO(1)%CONF%FINAM_MODEL, &
     &                             HEB_MOD, IND3_SECT, IND4_SECT, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( J1, STR )
                   IUER = -1
                   CALL ERR_LOG ( 6810, IUER, 'SC_APPLY', 'Error in '// &
     &                 'reading the '//STR(1:I_LEN(STR))// &
     &                 ' component of the model file '//MALO(1)%CONF%FINAM_MODEL )
                   RETURN 
              END IF
         END IF
!
         IF ( MALO(1)%MODC(J1)%TYP == MALO__CNST ) THEN
              IF ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT  .OR. &
     &             MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUB_NOHAR      ) THEN
!
! ---------------- Remove the average surface pressure
!
                   HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) = &
     &                 HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) - &
     &                     HEB_MOD%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1)
              END IF
           ELSE IF ( MALO(1)%MODC(J1)%TYP == MALO__JMP ) THEN
              IF ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT .OR. &
     &             MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUB_NOHAR     ) THEN
                   IF ( TIM .GE. MALO(1)%MODC(J1)%TIM .AND. MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
!
! --------------------- Remove the jump
!
                        HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) = &
     &                      HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) - &
     &                          HEB_MOD%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1)
                    END IF
              END IF
         END IF
 410  CONTINUE 
!
      IF ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
           TIM = (HEB_PRS%MJD - MALO(1)%MJD_DEF)*86400.0D0 + &
     &           (HEB_PRS%TAI - MALO(1)%TAI_DEF)
           DO 420 J2=1,MALO_HFS(MALO(1)%CONF%MODEL_CODE)
              IF ( MALO(1)%MODC(J2)%TYP == MALO__COS .OR. &
     &             MALO(1)%MODC(J2)%TYP == MALO__SIN      ) THEN
!
! ---------------- Get the J2-th component of the regression model
!
                   IND3_SECT = J2
                   IUER = -1
                   CALL READ_HEB_SECT ( MALO(1)%CONF%FINAM_MODEL, &
     &                                  HEB_MOD, IND3_SECT, IND4_SECT, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( J2, STR )
                        IUER = -1
                        CALL ERR_LOG ( 6811, IUER, 'SC_APPLY', 'Error in '// &
     &                      'reading the '//STR(1:I_LEN(STR))// &
     &                      ' component of the model file '//MALO(1)%CONF%FINAM_MODEL )
                        RETURN 
                   END IF
              END IF
!
              IF ( MALO(1)%MODC(J2)%TYP == MALO__COS ) THEN
                   IF ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
!
! --------------------- Remove the contribution of the cos-part of the harmonic model 
! --------------------- in surface pressure
!
                        HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) = &
     &                      HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) - &
     &                          DCOS(MALO(1)%MODC(J2)%FRQ*TIM + MALO(1)%MODC(J2)%PHS)*  &
     &                               HEB_MOD%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1)
                   END IF 
                ELSE IF ( MALO(1)%MODC(J2)%TYP == MALO__SIN ) THEN
                   IF ( MALO(1)%CONF%MODEL_USE .EQ. MALO__MOD_SUBTRACT ) THEN
!
! --------------------- Remove the contribution of the sin-part of the harmonic model 
! --------------------- in surface pressure
!
                        HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) = &
     &                      HEB_PRS%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1) - &
     &                             DSIN(MALO(1)%MODC(J2)%FRQ*TIM + MALO(1)%MODC(J2)%PHS)*  &
     &                                  HEB_MOD%VAL(1:HEB_PRS%DIMS(1),1:HEB_PRS%DIMS(2),1,1)
                   END IF 
              END IF
 420       CONTINUE 
      END IF
      IF ( ASSOCIATED ( HEB_MOD%VAL ) ) THEN
           DEALLOCATE ( HEB_MOD%VAL )
      END IF
!
      TIM_R8 = WALL_TIMER ( %VAL(0) )
!
! --- Apply sampling correction
!
      IUER = -1
      CALL SC_CONTR ( MODE_STR, HEB_IN, HEB_PRS, HEB_SC, HEB_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6812, IUER, 'SC_APPLY', 'Failed to compute '// &
     &         'the contribuition of the sampling correction' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IVRB == 8 ) THEN
           nlon = heb_prs%dims(1)
           nlat = heb_prs%dims(2)
           call plot_grid_r4 ( 1, 7, 0,  1, nlon, nlat, heb_in%val, 'Up loading nosc', 'mm', '/tmp/foo', iuer )
           call plot_grid_r4 ( 1, 7, 0,  1, nlon, nlat, heb_prs%val, 'Pressure', 'Pa', '/tmp/foo', iuer )
           call plot_grid_r4 ( 1, 7, 45, 1, nlon, nlat, heb_sc%val, 'sc', 'm/pa', '/tmp/foo',  iuer )
           call plot_grid_r4 ( 1, 7, 0,  1, nlon, nlat, heb_out%val, 'Up loading sc', 'mm', '/tmp/foo', iuer )
      END IF
      TIM_R8 = WALL_TIMER ( %VAL(2) )
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, 220 ) TIM_R8
 220       FORMAT ( 'SC_CONTR took ', F8.2,' sec' )
      END IF
!
      CALL EXWORD ( HEB_SC%COMMENT(2), MIND, LIND, INDS, CHAR(0)//CHAR(32), IUER )
      HEB_OUT%COMMENT(4) = 'Sampling correction was applied up to degree '// &
     &                      HEB_SC%COMMENT(2)(INDS(1,6):INDS(2,6))
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6813, IUER, 'SC_APPLY', 'Failure in '// &
     &         'writing sampling correction into the output file '// &
     &          FILOUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, * ) 'Wrote output file '//FILOUT(1:I_LEN(FILOUT))
!      
      END  SUBROUTINE  SC_APPLY  !#!#
