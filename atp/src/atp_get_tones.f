      PROGRAM  MAIN_TESTER

      IMPLICIT   NONE
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
       CHARACTER  STR*128, ANC_DIR*128, BNC_FNAM*128, BNC_FIL*256
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
       INTEGER*4  IUER
       REAL*8     FRQ_DIF
!     
! ---- Set stacksize. Alternative is to set stacksize in shell:
! ---- commands limit stacksize 4000000 or limit -s 4000000
! ---- and set evironment variable GOMP_STACKSIZE
! ---- Program will crash in attempt to use default stacksize,
! ---- because fortran uses stack for storing variables
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8  ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0),     &
     &               %VAL(1) )
!



       ANC_DIR  = '/anc/vlbi/orig'
       BNC_FNAM = TRIM(ANC_DIR)//'/v23341k2_orig.bnc'
!
       PRINT *, "%%%% BNC_FNAM %%%%:", TRIM(BNC_FNAM)
       IUER = -1
       CALL BNC_PARSE ( BNC_FNAM, ANC, IUER )
       IF ( IUER .NE. 0 ) THEN
          CALL ERR_LOG ( 5035, IUER, 'ATP_GET_TONES',                   &
     &           'Failure in parsing binary file data from '//BNC_FNAM )
       END IF

       CALL ATP_GET_PCAL_FRQ_DIF ( ANC, FRQ_DIF )


       PRINT *, FRQ_DIF
      




      END PROGRAM 
      


      
      SUBROUTINE ATP_GET_PCAL_TONES ( ANC, FRQ_BNDWTH, FRQ_MID,         &
     &                                NTONES, IUER )
!
! ***************************************************************************************
! *                                                                                     *
! *    SUBROUTINE ATP_GET_PCAL_TONES                                                    *
! *                                                                                     *
! *    INPUT:                                                                           *
! *           ANC    = Antenna calibration file deried type                { }          *
! *                                                                                     *
! *    OUTPUT:                                                                          *
! *            FRQ_BNDWTH   = Frequency Bandwith                       { REAL*4 } [MHz] *
! *                                                                                     *
! *            FRQ_MID      = Centre Frequency                         { REAL*4 } [MHz] *
! *                                                                                     *
! *            NTONES       = Number of tones                          { INT*4 }        *
! *                                                                                     *
! *  ### 21-DEC-2023  ATP_GET_PCAL_TONES       v1.0 (c)    N. HABANA   21-DEC-2023 ###  *
! *                                                                                     *
! ***************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'atp.i'
!      INCLUDE    'pima.i'
      TYPE ( ANC__TYP   ) :: ANC
 !     TYPE ( PIMA__TYPE  ) :: PIM
      CHARACTER  STR*128
      INTEGER*4  NTONES, IUER
      REAL*8     FRQ_BNDWTH, FRQ_MID
      INTEGER*4  J1, J2, J3
!
! --- Find indices where the difference is greater than the modal 
!     freq diff. Those indices mark the end of a frequncy band.
! --- Check also for the polarization, to avoid repitionsg      
!     






      
 !@@!     DO 310 J1 = 1, ANC%NUM_PCS






         



!@@! 310  CONTINUE
      












      END SUBROUTINE ATP_GET_PCAL_TONES !#!1
!
! --------------------------------------------------------------------------------------------
!
      SUBROUTINE ATP_GET_PCAL_FRQ_DIF ( ANC, FRQ_DIF )
!
! ***************************************************************************************
! *                                                                                     *
! *    SUBROUTINE ATP_GET_PCAL_FRQ_DIF                                                  *
! *                                                                                     *
! *    INPUT:                                                                           *
! *           ANC    = Antenna calibration file derived type               { }          *
! *                                                                                     *
! *    OUTPUT:                                                                          *
! *            FRQ_DIF      = Bandwidth between IF's                   { REAL*4 } [MHz] *
! *                                                                                     *
! *  ### 02-JAN-2024  ATP_GET_PCAL_FRQ_DIF     v1.0 (c)    N. HABANA   02-JAN-2024 ###  *
! *                                                                                     *
! ***************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  IUER, N, J1, IER
      REAL*8     FRQ_DIF
      REAL*8,    ALLOCATABLE :: FRQ_DIF_ARR(:)
      REAL*8,    EXTERNAL :: MODE_R8
!     
! ---
!
      N = ANC%NUM_PCS - 1
! ---
      ALLOCATE ( FRQ_DIF_ARR( ANC%NUM_PCS ), STAT = IER ) 
      IF ( IER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 2401, IUER, 'ATP_GET_PCAL_FRQ_DIF',             &
     &           'Failed to allocate FRQ_DIF_ARR' )
      END IF
!     
! --- Generate the array of frequency differences
!
      DO 310 J1 = 2, ANC%NUM_PCS
         FRQ_DIF_ARR(J1-1) = DABS( ANC%PCS(J1)%SKY_FRQ -                &
     &                             ANC%PCS(J1-1)%SKY_FRQ )
 310  CONTINUE
!
! --- Sort the difference array
!
      CALL SORT_R8 ( N, FRQ_DIF_ARR )
!
! --- Compute the frequency difference by computing the mode.
!
      FRQ_DIF =  MODE_R8 ( N, FRQ_DIF_ARR )
!
      RETURN
      END SUBROUTINE
