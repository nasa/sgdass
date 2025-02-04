#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_OPAG ( PIM, VTD, SPD_URL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_OPAG 
! *                                                                      *
! *  ### 16-SEP-2014   PIMA_OPAG   v1.0 (c)  L. Petrov  17-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INCLUDE   'pima_local.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      CHARACTER  SPD_URL*(*)
      INTEGER*4  IUER 
      INTEGER*4  M_FIL, M_CNF, MIND
      PARAMETER  ( M_FIL = 2*1024*1024 )
      PARAMETER  ( M_CNF =         128 )
      PARAMETER  ( MIND  =          32 )
      INTEGER*2  MODE_MKDIR
      DATA       MODE_MKDIR / O'00755' /
      CHARACTER  FINAM*128, SOB_DIR*128, STR*128, COM_STR*512, DATE_STR*19, &
     &           STA*8, SPD_MOD*128,  URL*128
      CHARACTER, ALLOCATABLE :: C_FIL(:)*128
      INTEGER*8  DIR_DESC(16)
      REAL*8     EPS
      INTEGER*4  ME__EPC
      PARAMETER  ( EPS  = 120.0D0 )
      PARAMETER  ( ME__EPC = 3 ) !  extra epochs before and after 
      REAL*8     TAI_BEG, TAI_EPC, TAI_END
      INTEGER*4  LEV, ID, IL, IS, N_STA, LIND, IND(2,MIND), I_CNF, N_SOB, &
     &           MJD_BEG, MJD_EPC, MJD_END, IDAY, J1, J2, J3, J4, J5, J6, J7, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR 
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN, LINDEX, CLOSEDIR, MKDIR, SYSTEM
!
      ID = LINDEX ( SPD_URL, '/' )
      IF ( ID == ILEN(SPD_URL) ) THEN
           CALL CLRCH ( SPD_URL(ID:) )
           ID = LINDEX ( SPD_URL, '/' )
      END IF
      SPD_MOD = SPD_URL(ID+1:)
!
      SOB_DIR = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'// &
     &          PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_sob'
!
! --- Check whether the SOB_DIR exists
!
      DIR_DESC(1) = FUNC_OPENDIR ( SOB_DIR(1:I_LEN(SOB_DIR))//CHAR(0) )
      IF ( DIR_DESC(1) == 0 ) THEN
!
! -------- Does not exist? Let us create it
!
           IS = MKDIR ( SOB_DIR(1:I_LEN(SOB_DIR))//CHAR(0), &
     &                  %VAL(MODE_MKDIR) )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7325, IUER, 'PIMA_OPAG', 'Failure '// &
     &              'in attempt to create the directory for slant path delay '// &
     &              'and atmosphere opacity '//SOB_DIR(1:I_LEN(SOB_DIR))//' -- '// &
     &               STR )
                RETURN 
           END IF
         ELSE 
           IS = CLOSEDIR ( %VAL(DIR_DESC(1)) )
      END IF
!
      MJD_BEG = PIM%MJD_0
      TAI_BEG = PIM%TAI_0 - (ME__EPC-1)*PIMA__SPD_STP - EPS
      IDAY = TAI_BEG/86400.0D0
      MJD_BEG = MJD_BEG + IDAY
      TAI_BEG = TAI_BEG - IDAY*86400.0D0
      IF ( TAI_BEG < 0.0D0 ) THEN
           MJD_BEG = MJD_BEG - 1
           TAI_BEG = TAI_BEG + 86400.0D0
      END IF 
      TAI_BEG = PIMA__SPD_STP*IDINT(TAI_BEG/PIMA__SPD_STP)
!
      MJD_END = PIM%MJD_0
      TAI_END = PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) + ME__EPC*PIMA__SPD_STP + EPS
      IDAY = TAI_END/86400.0D0
      MJD_END = MJD_END + IDAY
      TAI_END = TAI_END - IDAY*86400.0D0
      TAI_END = PIMA__SPD_STP*(IDINT(TAI_END/PIMA__SPD_STP)+1)
      IF ( TAI_END > 86400.0D0 ) THEN
           MJD_END = MJD_END + 1
           TAI_END = TAI_END - 86400.0D0
      END IF
!
      N_SOB = IDNINT ( ((MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG))/PIMA__SPD_STP ) + 1
!
      DO 410 J1=1,N_SOB
         MJD_EPC = MJD_BEG
         TAI_EPC = TAI_BEG + (J1-1)*PIMA__SPD_STP 
         DATE_STR = MJDSEC_TO_DATE ( MJD_EPC, TAI_EPC, IER )
         URL = TRIM(SPD_URL)//'/opa_spd_'//TRIM(SPD_MOD)//'_'//DATE_STR(1:4)//DATE_STR(6:7)// &
     &         DATE_STR(9:10)//'_'//DATE_STR(12:13)//DATE_STR(15:16)//'.spd.bz2'
         COM_STR = 'cd '//TRIM(SOB_DIR)//'; '// &
     &             'wget -c -r --no-check-certificate --timeout=30 --tries=64 --retry-connrefused -nH --cut-dirs=12 '// &
     &              URL
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, * ) 'PIMA_OPAG: about to execute command '//TRIM(COM_STR)
         END IF
         IS = SYSTEM ( TRIM(COM_STR)//CHAR(0) )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 7311, IUER, 'PIMA_OPAG', 'Error in an attempt to download '// &
     &            'slant path delays with command '//COM_STR )
              RETURN 
         END IF
 410  CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, * ) 'PIMA_OPAG: Downloaded slant path delay and opacities to '//TRIM(SOB_DIR)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_OPAG  !#!#
