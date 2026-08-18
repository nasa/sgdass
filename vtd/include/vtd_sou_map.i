!
!  >>>> Include block vtd_sou_map.i
!  >>>> 2004.02.26  L. Petrov   v 2.0  02-MAR-2004 10:09:56
!
	INTEGER*4    SMP__MAX, SMP__M2
	PARAMETER  ( SMP__MAX = 512 ) 
	PARAMETER  ( SMP__M2  = SMP__MAX*SMP__MAX ) 
        TYPE      SOUMAP__TYPE !
            INTEGER*4  DIM1
            INTEGER*4  DIM2
	    REAL*8     STEP_RA
	    REAL*8     STEP_DL
	    REAL*8     FREQ                ! Frequnecy of teh map in rad/s
	    REAL*8     FLUX_MAX
	    REAL*8     BEAM_MAJ
	    REAL*8     BEAM_MIN
	    REAL*8     BEAM_POS_ANG
	    REAL*4,    POINTER :: IMAGE(:,:) 
	    COMPLEX*8, POINTER :: IMAGE_FF(:,:)
	    INTEGER*4  STATUS_IMAGE
	    INTEGER*4  STATUS_IMAGE_FF
	    CHARACTER  NAME*8
        END TYPE  SOUMAP__TYPE ! SOUMAP__TYPE !
!
! >>>>> end of block vtd_sou_map.i
