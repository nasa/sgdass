
! >>>>> Include block for the user program user_decimation
! >>>>> 2007.10.24 (c)  L. Petrov  v 0.01  2007.10.24_22:19:09
!
	INTEGER*4    DCM__M_OBJ
	PARAMETER  ( DCM__M_OBJ = 8192 )
	CHARACTER    DCM__SOU*3, DCM__STA*3, DCM__BAS*3, DCM__CRE*3
	PARAMETER  ( DCM__SOU = 'SOU' )
	PARAMETER  ( DCM__SOU = 'STA' )
	PARAMETER  ( DCM__SOU = 'BAS' )
	PARAMETER  ( DCM__SOU = 'CRE' )
	INTEGER*4    DCM__M_PAR
	PARAMETER  ( DCM__M_PAR = 7 )
	TYPE DCM__TYPE
	     CHARACTER  OPER*3
	     CHARACTER  FILLER*1
	     CHARACTER  DIR*128
	     CHARACTER  FIL_INC*128
	     CHARACTER  FIL_EXC*128
	     INTEGER*4  USE_CASE
	     INTEGER*4  TOT_CASE
	     INTEGER*4  L_INC
	     INTEGER*4  L_EXC
	     CHARACTER  NAM_INC(DCM__M_OBJ)*16
	     CHARACTER  NAM_EXC(DCM__M_OBJ)*16
        END TYPE DCM__TYPE
!
! >>>>> End of include block for package VTD
!
