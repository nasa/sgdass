      BLOCK DATA INIT_PVERS
! ************************************************************************
! *                                                                      *
! *   This program unit initializes pvers.i with program names.          *
! *                                                                      *
! *  ### 05-JUL-2003   PVERS_INIT  v1.0 (c)  L. Petrov  05-JUL-2003 ###  *
! *                                                                      *
! ************************************************************************
      INCLUDE   'solve.i'
      INCLUDE   'pvers.i'
      DATA PROG_NAMES &
     &  / &
     &  'ACCOR', &    !  1
     &  'ADDER', &    !  2
     &  'ADJST', &    !  3
     &  'ALEN ', &    !  4
     &  'ARCPE', &    !  5
     &  'BACK ', &    !  6
     &  'BASFE', &    !  7
     &  'BATCH', &    !  8
     &  'BCLOK', &    !  9
     &  'CHPAR', &    ! 10
     &  'CNPLT', &    ! 11
     &  'COREL', &    ! 12
     &  'CORRN', &    ! 13
     &  'COVP ', &    ! 14
     &  'CRES ', &    ! 15
     &  'ELIM ', &    ! 16
     &  'FLOPT', &    ! 17
     &  'GAMB ', &    ! 18
     &  'GLOBL', &    ! 19
     &  'GETDB', &    ! 20
     &  'GTSUP', &    ! 21
     &  'HAUSR', &    ! 22
     &  'IONO ', &    ! 23
     &  'MDLPL', &    ! 24
     &  'MKSUP', &    ! 25
     &  'NEWDB', &    ! 26
     &  'NORML', &    ! 27
     &  'OPTIN', &    ! 28
     &  'PAMB ', &    ! 29
     &  'PROC ', &    ! 30
     &  'QUIKD', &    ! 31
     &  'REWAY', &    ! 32
     &  'SDBH ', &    ! 33
     &  'SETFL', &    ! 34
     &  'SLEND', &    ! 35
     &  'SLVEB', &    ! 36
     &  'TRANS', &    ! 37
     &  'UPTDB', &    ! 38
!
     &  'U-CAL', &    ! 39
     &  'U-CON', &    ! 40
     &  'U-PAR', &    ! 41
     &  'liptn' &     ! 42
     &  /
      END !#!  BLOCK DATA  INIT_PVERS  #!#
