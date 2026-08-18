      PROGRAM    GEN_ROT_ADM
! ************************************************************************
! *                                                                      *
! *   Program  GEN_ROT_ADM  computes the admittance matrix of the        *
! *   TRF rotation and rotation rate to right hand side of the           *
! *   nnr_pos/nnr_vel constraints and writes it in the output file.      *
! *   It uses results of 7 trial solutions.                              *
! *                                                                      *
! *   1) rot_1p -- a trail solution with right hand sides                *
! *                nnr_pos 0.1, 0.0, 0.0 and nnr_vel 0.0, 0.0, 0.0       *
! *   2) rot_2p -- a trail solution with right hand sides                *
! *                nnr_pos 0.0, 0.1, 0.0 and nnr_vel 0.0, 0.0, 0.0       *
! *   3) rot_3p -- a trail solution with right hand sides                *
! *                nnr_pos 0.0, 0.0, 0.1 and nnr_vel 0.0, 0.0, 0.0       *
! *   4) rot_1v -- a trail solution with right hand sides                *
! *                nnr_pos 0.0, 0.0, 0.0 and nnr_vel 0.1, 0.0, 0.0       *
! *   5) rot_2v -- a trail solution with right hand sides                *
! *                nnr_pos 0.0, 0.0, 0.0 and nnr_vel 0.0, 0.1, 0.0       *
! *   6) rot_3v -- a trail solution with right hand sides                *
! *                nnr_pos 0.0, 0.0, 0.0 and nnr_vel 0.0, 0.0, 0.1       *
! *   7) rot_3v -- a trail solution with right hand sides                *
! *                nnr_pos 0.0, 0.0, 0.0 and nnr_vel 0.0, 0.0, 0.0       *
! *                                                                      *
! *   nnt_pos and nnt_vel contraints are supposed to be imposed by       *
! *   eop_alignment and kept fixed for all these solition.               *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 29-JUN-2026  GEN_ROT_ADM  v1.1 (d)  L. Petrov  03-JUL-2026 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MS, MB
      PARAMETER  ( MS =   7 )
      PARAMETER  ( MB = 256 )
      CHARACTER  FIL_SOL*128, FIL_ROT*128, FILOUT*128, SUFS(MS)*12, BUF(MB)*128, LABEL*66
      PARAMETER  ( LABEL = '# Rotational admittance matrix for EOP. Format version: 2026.07.01' )
      DATA       SUFS / &
     &                  '_rot_1p.txt', & ! 1
     &                  '_rot_2p.txt', & ! 2
     &                  '_rot_3p.txt', & ! 3
     &                  '_rot_1v.txt', & ! 4
     &                  '_rot_2v.txt', & ! 5
     &                  '_rot_3v.txt', & ! 6
     &                  '_rot_0.txt'   & ! 7
     &                /
      LOGICAL*1  LEX
      REAL*8     ROT_VECS(6,MS), ROTMAT(6,6), APPLIED_ROT, DAT_BEG, DAT_END, DUR
      REAL*8     VEC_TEST(6), TEST_RES(6), VEC5(6), VEC0(6), VEC1(6), INVMAT(6,6), EPS
      PARAMETER  ( APPLIED_ROT = 0.1D0 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, N_SIT, NP, IP, NO, IER, IUER
      INTEGER*4, EXTERNAL :: LINDEX
      CHARACTER, EXTERNAL :: GET_CDATE*19, GET_VERSION*54
!
      INCLUDE   'gen_rot_adm_version.i'
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_rot_adm spool_file'
         ELSE 
           CALL GETARG ( 1, FIL_SOL )
      END IF
!
! --- Remove suffix in the spool file name
!
      IP = LINDEX ( FIL_SOL, '.' )
      IF ( IP > 0 ) THEN
           FIL_SOL = FIL_SOL(1:IP-1)
      END IF
!
! --- Cycle over 1p, 2p, 3p, 1v, 2v, 3v, 0
!
      DO 410 J1=1,MS
!
! ------ Build the rotation file name
!
         FIL_ROT = TRIM(FIL_SOL)//SUFS(J1)
!
! ------ Check whetehr the file with that name exists
!
         INQUIRE ( FILE=FIL_ROT, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              IUER = -1
              CALL ERR_LOG ( 6801, IUER, 'GEN_ROT_ADM', 'Did not find rotation '// &
     &            'file '//FIL_ROT )
              CALL EXIT ( 1 )
         END IF
!
! ------ Read thefile
!
         IUER = -1
         CALL RD_TEXT ( FIL_ROT, MB, BUF, NP, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6802, IUER, 'GEN_ROT_ADM', 'Error in reading '// &
     &            'rotation file '//FIL_ROT )
              CALL EXIT ( 1 )
         END IF
!
! ------ Parse rotation file
!
         N_SIT = 0
         DO 420 J2=1,NP
            IF ( BUF(J2)(1:14)  == '*       N_SIT=' ) THEN
                 CALL CHIN ( BUF(J2)(15:19), N_SIT )
            END IF
!
            IF ( 'Date_start:' == BUF(J2)(3:13) ) THEN
                 READ ( UNIT=BUF(J2)(28:37), FMT='(F10.5)' ) DAT_BEG
              ELSE IF ( 'Date_end:  ' == BUF(J2)(3:13) ) THEN
                 READ ( UNIT=BUF(J2)(28:37), FMT='(F10.5)' ) DAT_END
              ELSE IF ( 'Duration:  ' == BUF(J2)(3:13) ) THEN
                 READ ( UNIT=BUF(J2)(28:37), FMT='(F10.5)' ) DUR
              ELSE IF ( BUF(J2)(1:11) == '*       PHI'      .AND. &
     &           BUF(J2)(60:74) == '* phi in meters'       ) THEN
                 READ ( UNIT=BUF(J2)(15:27), FMT='(F13.10)' ) ROT_VECS(1,J1)
                 READ ( UNIT=BUF(J2)(30:42), FMT='(F13.10)' ) ROT_VECS(2,J1)
                 READ ( UNIT=BUF(J2)(45:57), FMT='(F13.10)' ) ROT_VECS(3,J1)
            END IF
            IF ( BUF(J2)(1:11)  == '*       OME'   .AND. &
     &           BUF(J2)(60:72) == '* ome in m/yr'       ) THEN
                 READ ( UNIT=BUF(J2)(15:27), FMT='(F13.10)' ) ROT_VECS(4,J1)
                 READ ( UNIT=BUF(J2)(30:42), FMT='(F13.10)' ) ROT_VECS(5,J1)
                 READ ( UNIT=BUF(J2)(45:57), FMT='(F13.10)' ) ROT_VECS(6,J1)
            END IF
 420     CONTINUE 
         IF ( N_SIT == 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6803, IUER, 'GEN_ROT_ADM', 'Did not find N_SIT field '// &
     &            'in the rotation file '//FIL_ROT )
              CALL EXIT ( 1 )
         END IF 
 410  CONTINUE 
!
! --- Build the admittance matrix
!
      DO 430 J3=1,6
         ROTMAT(1:6,J3) = (ROT_VECS(1:6,J3) - ROT_VECS(1:6,7))/APPLIED_ROT
 430  CONTINUE 
!
! --- Generate the header of the output file 
!
      NO = 0
      NO = NO + 1; BUF(NO) = LABEL
      NO = NO + 1; BUF(NO) = '#'
      NO = NO + 1; BUF(NO) = '# Reference solution: '//TRIM(FIL_SOL)
      NO = NO + 1; BUF(NO) = '# '
      NO = NO + 1; BUF(NO) = '# Processed with '//TRIM(GET_VERSION())
      NO = NO + 1; BUF(NO) = '# Processed on   '//GET_CDATE()
      NO = NO + 1; BUF(NO) = '# '
!
! --- Put the number of stations partiipiating in the nnr constriants in the
! --- output file
!
      NO = NO + 1; WRITE ( UNIT=BUF(NO), FMT=110 ) N_SIT
 110  FORMAT ( 'Nsta:     ', I4 )
!
      NO = NO + 1; BUF(NO) = '# '
      DO 440 J4=1,6
         NO = NO + 1; WRITE ( UNIT=BUF(NO), FMT=130 ) J4, ROTMAT(1:6,J4), 'meters and meters/year'
 130     FORMAT ( 'Adm_', I1, ':', 1X, 6(F9.6, 1X), 1X, A )
 440  CONTINUE 
!
! --- Write down the table with the rotation admittance matrix
!
      FILOUT = TRIM(FIL_SOL)//'.rot'
      IUER = -1
      CALL WR_TEXT ( NO, BUF, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6803, IUER, 'GEN_ROT_ADM', 'Error in writing rotation '// &
     &         'admittance matrices to '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, '(A)' ) 'Wrote the output file '//TRIM(FILOUT)
      END  PROGRAM  GEN_ROT_ADM  !#!#
