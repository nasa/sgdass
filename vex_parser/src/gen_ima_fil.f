      PROGRAM    GEN_IMA_FIL
      IMPLICIT   NONE
      CHARACTER  FIL_VEX*128, FIL_HEA*128, FIL_IMA_DIR*128, FIL_SOU*128
      CHARACTER  FIL_OUT*128, BAND*2, STR*128
      INTEGER*8  STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8  ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0),      &
     &              %VAL(1) )
!
! --- Declare input variables
!
      FIL_VEX     = '/tmp/rv117.vex'
      FIL_SOU     = '/apr/sou/glo.src'
      FIL_HEA     = '/opt64/share/struc_header.txt'
      FIL_IMA_DIR = '/vlbi/imdb/images.dir'
      FIL_OUT     = '/tmp/rv117_ima.cnt'
      BAND        = 'XS'
!
      CALL GEN_IMA_FIL_ROUTINE ( FIL_VEX, FIL_SOU, FIL_HEA,             &
     &               FIL_IMA_DIR, BAND, FIL_OUT )
!
      END PROGRAM
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_IMA_FIL_ROUTINE ( FIL_VEX, FIL_SOU, FIL_HEA,       &
     &               FIL_IMA_DIR, BAND, FIL_OUT )
! ************************************************************************
! *                                                                      *
! *   Program  GEN_IMA_FIL
! *                                                                      *
! *  ### 20-NOV-2020  GEN_IMA_FIL  v1.0 (c)  L. Petrov  20-NOV-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vex.i'
      TYPE     ( VEX_TYPE    ) :: VEX
      INTEGER*4  MS, M_SOU
      PARAMETER  ( MS    = 2*VEX__MSOU + 512 )
      PARAMETER  ( M_SOU = 256*1024 )
      CHARACTER  FIL_VEX*(*), FIL_HEA*(*), FIL_IMA_DIR*(*), FIL_SOU*(*) 
      CHARACTER  FIL_OUT*128, BAND*2, JNAME*10
      CHARACTER  CAT_SOU(M_SOU)*128, BUF_IMA(M_SOU)*128, OUT(MS)*256
      CHARACTER  FIL_IMA*128
      REAL*8     TIM_DIF, TIM_DIF_MIN, SEC_IMA
      INTEGER*4  IVRB, NC_SOU, N_IMA,J1, J2, J3, J4, ID, MJD_IMA
      INTEGER*4  N_HEA, NOUT, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      IUER = -1
      CALL RD_TEXT ( FIL_SOU, M_SOU, CAT_SOU, NC_SOU, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1411, IUER, 'GEN_IMA_FIL',                    &
     &         'Error in reading source names file '//FIL_SOU )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_IMA_DIR, M_SOU, BUF_IMA, N_IMA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1412, IUER, 'GEN_IMA_FIL',                    &
     &         'Error in reading image dir file '//FIL_IMA_DIR )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_HEA, MS, OUT, N_HEA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1412, IUER, 'GEN_IMA_FIL',                    &
     &         'Error in reading Header file '//FIL_HEA )
           CALL EXIT ( 1 )
      END IF
!
! --- Parse VLBI schedule in vex format
!
      VEX%STATUS = 0
      IVRB = 1
      IUER = -1
      CALL VEX_PARSER ( VEX, FIL_VEX, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1413, IUER, 'GEN_IMA_FIL',                    &
     &         'Error in parsing vex files '//FIL_VEX )
           CALL EXIT ( 1 )
      END IF
!
      NOUT = N_HEA
      WRITE ( 6, * ) 'VEX%N_SOU = ', VEX%N_SOU, ' NC_SOU= ', NC_SOU
      DO 410 J1=1,VEX%N_SOU
         JNAME = '??????????'
         DO 420 J2=1,NC_SOU
            IF ( CAT_SOU(J2)(1:1)  == '#' ) GOTO 420
            IF ( ILEN(CAT_SOU(J2)) ==  0  ) GOTO 420
            IF ( CAT_SOU(J2)(4:11) == VEX%C_SOU(J1)(1:8) ) THEN
                 JNAME = CAT_SOU(J2)(14:23)
            END IF 
 420     CONTINUE 
         IF ( JNAME == '??????????' ) THEN
              IUER = -1
              CALL ERR_LOG ( 1413, IUER, 'GEN_IMA_FIL',                 &
     &            'Source '//VEX%C_SOU(J1)//'from vex file '            &
     &            //TRIM(FIL_VEX)//' was not found in the catalogue '   &
     &            //'file '//FIL_SOU )
              CALL EXIT ( 1 )
         END IF
!
         DO 430 J3=1,2
            IF ( BAND(J3:J3) == ' ' ) GOTO 430
            TIM_DIF_MIN = 1.0001D30
            CALL CLRCH ( FIL_IMA )
            DO 440 J4=1,N_IMA
               ID = LINDEX ( BUF_IMA, '/' )
               IF ( BUF_IMA(J4)(ID+1:ID+10) == JNAME .AND.              &
     &              BUF_IMA(J4)(ID+12:ID+12) == BAND(J3:J3) ) THEN
                  IUER = -1
                  CALL DATE_TO_TIME ( BUF_IMA(J4)(ID+14:ID+23),         &
     &                     MJD_IMA, SEC_IMA, IUER )
                  TIM_DIF = ABS ( MJD_IMA*86400.0D0 + SEC_IMA -         &
     &               ( (VEX%MJD_START + VEX%MJD_STOP)*86400.0D0/2.0D0 + &
     &                 (VEX%UTC_START + VEX%UTC_STOP)/2.0D0   ) )
                  IF ( TIM_DIF < TIM_DIF_MIN ) THEN
                     TIM_DIF_MIN = TIM_DIF
                     FIL_IMA = BUF_IMA(J4)
                  END IF
               END IF
 440        CONTINUE 
            IF ( ILEN(FIL_IMA) == 0 ) THEN
               WRITE ( 6, * ) 'Did not find image for source '          &
     &            //VEX%C_SOU(J1)//' band '//BAND(J3:J3)
            ELSE
               NOUT = NOUT + 1
               OUT(NOUT) = 'SOU  '//VEX%C_SOU(J1)(1:8)//'    '//        &
     &             BAND(J3:J3)//'  DEL_COMP  1970.01.01_00:00:00 '//    &
     &             '2020.01.01_00:00:00      0.00     0.00  '//         &
     &             TRIM(FIL_IMA)
               IF ( J1 == VEX%N_SOU ) THEN
                  NOUT = NOUT + 1
                  OUT(NOUT) = 'STRUC_CONTROL  Format version of '//     &
     &                '2007.03.18'
               END IF
            END IF
 430     CONTINUE 
 410  CONTINUE 
!
      IUER = -1
      CALL WR_TEXT ( NOUT, OUT, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1413, IUER, 'GEN_IMA_FIL',                    &
     &         'Error in writing the output file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, * ) 'Image control is written to '//TRIM(FIL_OUT)
!
      END  SUBROUTINE  !#!#!  GEN_IMA_FIL_ROUTINE
