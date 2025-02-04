#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_FRIP_WRITE ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRIP_WRITE
! *                                                                      *
! * ### 28-DEC-2011  PIMA_FRIP_WRITE  v1.2 (c) L. Petrov 06-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER
      INTEGER*2  MODE_MKDIR
      DATA       MODE_MKDIR  / O'00755' /
      CHARACTER  FRIPDIR*128, FIL*128, STR*128
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, CLOSEDIR, WRITE
      ADDRESS__TYPE :: DIR_DESC, IP
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, SIZ_SCA, SIZ_OBS, IS, LUN, IER
      INTEGER*4, EXTERNAL     :: ILEN, I_LEN, MKDIR, UNLINK 
!
      FRIPDIR = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))// &
     &          '/'//PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &          '_frip'
!
! --- Check whether the FRIPDIR exists
!
      DIR_DESC = FUNC_OPENDIR ( FRIPDIR(1:I_LEN(FRIPDIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
!
! -------- Does not exist? Let us create it
!
           IS = MKDIR ( FRIPDIR(1:I_LEN(FRIPDIR))//CHAR(0), &
     &                  %VAL(MODE_MKDIR) )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 9141, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &              'in attempt to create the directory for output '// &
     &              'frip data '//FRIPDIR(1:I_LEN(FRIPDIR))//' -- '// &
     &              STR )
                RETURN 
           END IF
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
      CALL CLRCH  ( STR ) 
      CALL INCH   ( PIM%FRIP(PIMA__TAG)%IND_SCA, STR(1:4) )
      CALL CHASHR ( STR(1:4) )
      CALL BLANK_TO_ZERO ( STR(1:4) )
      FIL = FRIPDIR(1:I_LEN(FRIPDIR))//'/sca_'//STR(1:4)//'.frip'
      INQUIRE ( FILE=FIL, EXIST=LEX ) 
      IF ( LEX ) THEN
           IS = UNLINK ( FIL(1:I_LEN(FIL))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 9142, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &              'in attempt to remove the old version of the frip file '// &
     &               FIL(1:I_LEN(FIL))//' -- '//STR )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL BINF_OPEN ( FIL, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9143, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &         'in attempt to open for writing frip file '//FIL )
           RETURN 
      END IF
!
      DO 410 J1=1,2
         SIZ_SCA = ( LOC(PIM%FRIP(J1)%MAP_STATUS) - &
     &               LOC(PIM%FRIP(J1)%IND_SCA)      ) + &
     &            SIZEOF(PIM%FRIP(J1)%MAP_STATUS)
         IP = WRITE ( %VAL(LUN), PIM%FRIP(J1), %VAL(SIZ_SCA) )
         IF ( IP == -1 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 9144, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &            'in attempt to write into frip file '// &
     &             FIL(1:I_LEN(FIL))//' -- '//STR )
              RETURN 
            ELSE IF ( IP .NE. SIZ_SCA ) THEN
              CALL ERR_LOG ( 9145, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &            'in attempt to write into frip file '// &
     &             FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
              RETURN 
         END IF
 410  CONTINUE 
!
      IP = WRITE ( %VAL(LUN), PIM%FRIP(PIMA__CAL)%FRQ, &
     &             %VAL(8*PIM%FRIP(PIMA__CAL)%NFRQ) )
      IF ( IP == -1 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 9146, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &         'in attempt to write into frip file '// &
     &          FIL(1:I_LEN(FIL))//' -- '//STR )
           RETURN 
         ELSE IF ( IP .NE. 8*PIM%FRIP(PIMA__CAL)%NFRQ  ) THEN
           CALL ERR_LOG ( 9147, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &         'in attempt to write into frip file '// &
     &          FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
           RETURN 
      END IF
!
      IP = WRITE ( %VAL(LUN), PIM%FRIP(PIMA__CAL)%CFRQ_REF, &
     &             %VAL(8*PIM%FRIP(PIMA__CAL)%NFRQ) )
      IF ( IP == -1 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 9148, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &         'in attempt to write into frip file '// &
     &          FIL(1:I_LEN(FIL))//' -- '//STR )
           RETURN 
         ELSE IF ( IP .NE. 8*PIM%FRIP(PIMA__CAL)%NFRQ  ) THEN
           CALL ERR_LOG ( 9149, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &         'in attempt to write into frip file '// &
     &          FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
           RETURN 
      END IF
!
      DO 420 J2=1,2
         DO 430 J3=1,PIM%FRIP(J2)%NOBS
            SIZ_OBS = ( LOC(PIM%FRIP(J2)%OBS(J3)%VIS_STATUS) - &
     &                  LOC(PIM%FRIP(J2)%OBS(J3))       ) + &
     &               SIZEOF(PIM%FRIP(J2)%OBS(J3)%VIS_STATUS)
            IP = WRITE ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3), &
     &                   %VAL(SIZ_OBS) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9150, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- '//STR )
                 RETURN 
              ELSE IF ( IP .NE. SIZ_OBS ) THEN
                 CALL ERR_LOG ( 9151, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
                 RETURN 
            END IF
!
            IP = WRITE ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%TIM_AP, &
     &                              %VAL(8*PIM%FRIP(J2)%OBS(J3)%NAP) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9152, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 8*PIM%FRIP(J2)%OBS(J3)%NAP ) THEN
                 CALL ERR_LOG ( 9153, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
                 RETURN 
            END IF
!
            IP = WRITE ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%WEI, &
     &                   %VAL(4*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9154, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 4*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP ) THEN
                 CALL ERR_LOG ( 9155, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
                 RETURN 
            END IF
!
            IP = WRITE ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%UVW, &
     &                   %VAL(4*3*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9156, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 4*3*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ ) THEN
                 CALL ERR_LOG ( 9157, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
                 RETURN 
            END IF
!
            IP = WRITE ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%VIS, &
     &                   %VAL(4*2*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9158, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 4*2*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ ) THEN
                 CALL ERR_LOG ( 9159, IUER, 'PIMA_FRIP_WRITE', 'Failure '// &
     &               'in attempt to write into frip file '// &
     &                FIL(1:I_LEN(FIL))//' -- not all bytes have been written' )
                 RETURN 
            END IF
 430     CONTINUE 
 420  CONTINUE 
!
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRIP_WRITE   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_FRIP_READ ( PIM, FIL_FRIP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRIP_READ
! *                                                                      *
! * ### 28-DEC-2011  PIMA_FRIP_READ  v1.1 (c) L. Petrov 04-MAR-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER
      INTEGER*2  MODE_MKDIR
      DATA       MODE_MKDIR  / O'00755' /
      CHARACTER  FRIPDIR*128, FIL_FRIP*128, STR*128
      ADDRESS__TYPE, EXTERNAL :: READ
      ADDRESS__TYPE :: IP
      INTEGER*8  MEM_SIZE
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, IS, LUN, SIZ_SCA, SIZ_OBS, IER
      INTEGER*4, EXTERNAL     :: ILEN, I_LEN
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=FIL_FRIP, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 9171, IUER, 'PIMA_FRIP_READ', 'Cannot find '// &
     &              'frip file '//FIL_FRIP )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL BINF_OPEN ( FIL_FRIP, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9172, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &         'in attempt to open for reading frip file '//FIL_FRIP )
           RETURN 
      END IF
!
      MEM_SIZE = 0
      DO 410 J1=1,2
         SIZ_SCA = ( LOC(PIM%FRIP(J1)%MAP_STATUS) - &
     &               LOC(PIM%FRIP(J1)%IND_SCA)      ) + &
     &            SIZEOF(PIM%FRIP(J1)%MAP_STATUS)
         IP = READ ( %VAL(LUN), PIM%FRIP(J1), %VAL(SIZ_SCA) )
         IF ( IP == -1 ) THEN
              CALL GERROR ( STR )
              WRITE ( 6, * ) ' J1= ', J1
              CALL ERR_LOG ( 9173, IUER, 'PIMA_FRIP_READ', 'Failure '// &
        &         'in attempt to read from frip file '// &
        &          FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
              RETURN 
            ELSE IF ( IP .NE. SIZ_SCA ) THEN
              CALL ERR_LOG ( 9174, IUER, 'PIMA_FRIP_READ', 'Failure '// &
        &         'in attempt to read from file '// &
        &          FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- not all bytes have been read' )
              RETURN 
         END IF
!
         ALLOCATE ( PIM%FRIP(J1)%FRQ(PIM%FRIP(J1)%NFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%FRIP(J1)%NFRQ, STR )
              CALL ERR_LOG ( 9175, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &            'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'of dynamic memory for array PIM%FRIP(...)%FRQ' )
              RETURN 
         END IF
         MEM_SIZE = MEM_SIZE + 8*PIM%FRIP(J1)%NFRQ
!
         ALLOCATE ( PIM%FRIP(J1)%CFRQ_REF(PIM%NCHN,(PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1)), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%FRIP(J1)%NFRQ, STR )
              CALL ERR_LOG ( 9176, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &            'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'of dynamic memory for array PIM%FRIP(...)%CFRQ_REF' )
              RETURN 
         END IF
         MEM_SIZE = MEM_SIZE + 8*PIM%FRIP(J1)%NFRQ
!
         ALLOCATE ( PIM%FRIP(J1)%OBS(PIM%FRIP(J1)%NOBS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( PIM%FRIP(J1)%NOBS*SIZEOF(PIM%FRIP(J1)%OBS(1)), &
     &                     STR )
              CALL ERR_LOG ( 9177, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &            'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'of dynamic memory for array PIM%FRIP(...)%OBS' )
              RETURN 
         END IF
         MEM_SIZE = MEM_SIZE + PIM%FRIP(J1)%NOBS*SIZEOF(PIM%FRIP(J1)%OBS(1))
 410  CONTINUE 
!
      IP = READ ( %VAL(LUN), PIM%FRIP(PIMA__CAL)%FRQ, &
     &            %VAL(8*PIM%FRIP(PIMA__CAL)%NFRQ) )
      IF ( IP == -1 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 9178, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &         'in attempt to read from frip file '// &
     &          FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
           RETURN 
         ELSE IF ( IP .NE. 8*PIM%FRIP(PIMA__CAL)%NFRQ  ) THEN
           CALL ERR_LOG ( 9179, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &         'in attempt to read from frip file '// &
     &          FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- not all bytes have been read' )
           RETURN 
      END IF
!
      IP = READ ( %VAL(LUN), PIM%FRIP(PIMA__CAL)%CFRQ_REF, &
     &            %VAL(8*PIM%FRIP(PIMA__CAL)%NFRQ) )
      IF ( IP == -1 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 9180, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &         'in attempt to read from frip file '// &
     &          FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
           RETURN 
         ELSE IF ( IP .NE. 8*PIM%FRIP(PIMA__CAL)%NFRQ  ) THEN
           CALL ERR_LOG ( 9181, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &         'in attempt to read from frip file '// &
     &          FIL_FRIP(1:I_LEN(FIL_FRIP))// &
     &          ' -- not all bytes have been read' )
           RETURN 
      END IF
!
! --- Copy the frequency table from CAL to TAG
!
      PIM%FRIP(PIMA__TAG)%FRQ = PIM%FRIP(PIMA__CAL)%FRQ
      PIM%FRIP(PIMA__TAG)%CFRQ_REF = PIM%FRIP(PIMA__CAL)%CFRQ_REF
!
      DO 420 J2=1,2
         DO 430 J3=1,PIM%FRIP(J2)%NOBS
            SIZ_OBS = ( LOC(PIM%FRIP(J2)%OBS(J3)%VIS_STATUS) - &
     &                  LOC(PIM%FRIP(J2)%OBS(J3)) ) + &
     &               SIZEOF(PIM%FRIP(J2)%OBS(J3)%VIS_STATUS)
            IP = READ ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3), %VAL(SIZ_OBS) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9182, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. SIZ_OBS ) THEN
                 CALL ERR_LOG ( 9183, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- not all bytes '// &
     &               'have been read' )
                 RETURN 
            END IF
!
            ALLOCATE ( PIM%FRIP(J2)%OBS(J3)%TIM_AP(PIM%FRIP(J2)%OBS(J3)%NAP), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%FRIP(J2)%OBS(J3)%NAP, STR )
                 CALL ERR_LOG ( 9184, IUER, 'PIMA_FRIP_READ', 'Failure in an '// &
     &               'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dymanic memory for array %TIM_AP' )
                 RETURN 
            END IF
            MEM_SIZE = MEM_SIZE + 8*PIM%FRIP(J2)%OBS(J3)%NAP
!
            ALLOCATE ( PIM%FRIP(J2)%OBS(J3)%WEI(PIM%FRIP(J2)%NFRQ,PIM%FRIP(J2)%OBS(J3)%NAP), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 4*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP, STR )
                 CALL ERR_LOG ( 9185, IUER, 'PIMA_FRIP_READ', 'Failure in an '// &
     &               'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dymanic memory for array %WEI' )
                 RETURN 
            END IF
            MEM_SIZE = MEM_SIZE + 4*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP
!
            ALLOCATE ( PIM%FRIP(J2)%OBS(J3)%VIS(PIM%FRIP(J2)%NFRQ,PIM%FRIP(J2)%OBS(J3)%NAP), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP, STR )
                 CALL ERR_LOG ( 9186, IUER, 'PIMA_FRIP_READ', 'Failure in an '// &
     &               'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dymanic memory for array %VIS' )
                 RETURN 
            END IF
            MEM_SIZE = MEM_SIZE + 8*PIM%FRIP(J2)%NFRQ* &
     &                              PIM%FRIP(J2)%OBS(J3)%NAP
!
            ALLOCATE ( PIM%FRIP(J2)%OBS(J3)%UVW(3,PIM%FRIP(J2)%NFRQ,PIM%FRIP(J2)%OBS(J3)%NAP), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 12*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP, STR )
                 CALL ERR_LOG ( 9187, IUER, 'PIMA_FRIP_READ', 'Failure in an '// &
     &               'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dymanic memory for array %UWV' )
                 RETURN 
            END IF
            MEM_SIZE = MEM_SIZE + 12*PIM%FRIP(J2)%NFRQ* &
     &                               PIM%FRIP(J2)%OBS(J3)%NAP
!
            IP = READ ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%TIM_AP, &
     &                             %VAL(8*PIM%FRIP(J2)%OBS(J3)%NAP) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9188, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 8*PIM%FRIP(J2)%OBS(J3)%NAP ) THEN
                 CALL ERR_LOG ( 9189, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))// &
     &                ' -- not all bytes have been read' )
                 RETURN 
            END IF
!
            IP = READ ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%WEI, &
     &                  %VAL(4*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9190, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 4*PIM%FRIP(J2)%NFRQ*PIM%FRIP(J2)%OBS(J3)%NAP ) THEN
                 CALL ERR_LOG ( 9191, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))// &
     &                ' -- not all bytes have been read' )
                 RETURN 
            END IF
!
            IP = READ ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%UVW, &
     &                  %VAL(4*3*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9192, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 4*3*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ ) THEN
                 CALL ERR_LOG ( 9193, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))// &
     &               ' -- not all bytes have been read' )
                 RETURN 
            END IF
!
            IP = READ ( %VAL(LUN), PIM%FRIP(J2)%OBS(J3)%VIS, &
     &                  %VAL(4*2*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ) )
            IF ( IP == -1 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 9194, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))//' -- '//STR )
                 RETURN 
               ELSE IF ( IP .NE. 4*2*PIM%FRIP(J2)%OBS(J3)%NAP*PIM%FRIP(J2)%NFRQ ) THEN
                 CALL ERR_LOG ( 9195, IUER, 'PIMA_FRIP_READ', 'Failure '// &
     &               'in attempt to read from frip file '// &
     &                FIL_FRIP(1:I_LEN(FIL_FRIP))// &
     &                ' -- not all bytes have been read' )
                 RETURN 
            END IF
 430     CONTINUE 
 420  CONTINUE 
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( MEM_SIZE, STR )
           WRITE ( 6, 220 ) STR(1:I_LEN(STR)), PIM%FRIP(PIMA__TAG)%IND_SCA, &
     &                      PIM%FRIP(PIMA__CAL)%IND_SCA
 220       FORMAT ( 'PIMA_FRIP_READ: allocated ',A, ' bytes for tag/cal scans ', &
     &               I4, 1X, I4 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRIP_READ  !#!#
