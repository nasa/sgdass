      SUBROUTINE PSOLVE_VERSION_CHECK ( IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PSOLVE_VERSION_CHECK 
! *                                                                      *
! * ## 15-AUG-2013 PSOLVE_VERSION_CHECK  v2.0 (c) L. Petrov 06-NOV-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners_local.i'
      INCLUDE   'vtd.i'
      INCLUDE   'gvh.i'
      INTEGER*4  IUER
      CHARACTER  ANS*128, SPD_DEFINED_VERS*8, GVH_DEFINED_VERS*8
!
      CALL NERS_VERSION ( 'NERS__LABEL', ANS )
      IF ( ANS .NE. NERS__LABEL ) THEN
           CALL ERR_LOG ( 8811, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve was compiled against '//NERS__LABEL// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled ners by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL SPD_CLIENT_VERSION( 'SPD_CLIENT__LABEL', ANS )
      SPD_DEFINED_VERS  = ANS(12:20)
      CALL SPD_CLIENT_VERSION( 'SPD_CLIENT_VERSION', ANS )
      IF ( ANS .NE. SPD_DEFINED_VERS  ) THEN
           CALL ERR_LOG ( 8812, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve was compiled against spd_client '//SPD_DEFINED_VERS// &
     &         ' version, but linked against '//TRIM(ANS)// &
     &         ' version. Please recompiled spd_client by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL VTD_VERSION ( 'VTD__LABEL', ANS ) 
      IF ( ANS .NE. VTD__LABEL ) THEN
           CALL ERR_LOG ( 8813, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve was compiled against '//VTD__LABEL// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled vtd by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL VTD_VERSION ( 'BSPPOS__LABEL', ANS ) 
      IF ( ANS .NE. BSPPOS__LABEL ) THEN
           CALL ERR_LOG ( 8814, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve was compiled against '//BSPPOS__LABEL// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled vtd by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL VTD_VERSION ( 'SOU_MAP__LABEL', ANS ) 
      IF ( ANS .NE. SOU_MAP__LABEL ) THEN
           CALL ERR_LOG ( 8815, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve was compiled against '//SOU_MAP__LABEL// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled vtd by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL VTD_VERSION ( 'VIONO__LABEL', ANS ) 
      IF ( ANS .NE. VIONO__LABEL ) THEN
           CALL ERR_LOG ( 8816, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve was compiled against '//VIONO__LABEL// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled vtd by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL VTD_VERSION ( 'SPD_3D_PROG__LABEL', ANS ) 
      IF ( ANS(1:16) .NE. SPD_3D_PROG__LABEL(1:16) ) THEN
           CALL ERR_LOG ( 8817, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve was compiled against '//SPD_3D_PROG__LABEL// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled vtd by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL VTD_VERSION ( 'DE440_EPH__LABEL', ANS ) 
      IF ( ANS .NE. DE440_EPH__LABEL ) THEN
           CALL ERR_LOG ( 8818, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve A was compiled against '//DE440_EPH__LABEL// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled vtd by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL GVH_VERSION ( 'GVH__LABEL', ANS )
      GVH_DEFINED_VERS  = ANS(5:12)
      CALL GVH_VERSION ( 'GVH_VERSION', ANS )
      IF ( ANS .NE. GVH_DEFINED_VERS  ) THEN
           CALL ERR_LOG ( 8819, IUER, 'PSOLVE_VERSION_CHECK', 'Trap of internal '// &
     &         'control: pSolve  was compiled against gvh '//GVH_DEFINED_VERS// &
     &         ', but linked against '//TRIM(ANS)// &
     &         ' Please recompiled gvh by running '// &
     &         'reconfigure; make clean; make ; make install' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PSOLVE_VERSION_CHECK  !#!#
