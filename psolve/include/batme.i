!@This is the start of file &BATME
!
! Last modified by L. Petrov on 2020.06.23_22:39:42
!
      INTEGER*2     NATMEX,  NGRADEX
      INTEGER*2     IATMEX(4,MAX_STA), IGRADEX(4,MAX_STA)
      CHARACTER     ATMEX_CHR(MAX_STA)*8, GRADEX_CHR(MAX_STA)*8
      EQUIVALENCE ( IATMEX,  ATMEX_CHR  )
      EQUIVALENCE ( IGRADEX, GRADEX_CHR )
      LOGICAL*2     KGRAD
!
      INTEGER*4  NUM_ATMOFF,  &
     &           NUM_ATMEXC, &
     &           NUM_GRADOFF, &
     &           NUM_STAEXC,  &  ! number of stations to be excluded
     &           NUM_STAOFF,  &  ! number of stations with positions not estimated
     &           NUM_STAINC,  &  ! number of stations to be included
     &           NUM_STAON,   &  ! number of stations with positions estimated
     &           NUM_SOUOFF,  &
     &           NUM_SOUEXC 
!
      CHARACTER  LIST_ATMOFF(MAX_ARC_STA)*8,  &
     &           LIST_ATMEXC(MAX_ARC_STA)*8,  &
     &           LIST_GRADOFF(MAX_ARC_STA)*8, &
     &           LIST_STAEXC(MAX_ARC_STA)*8,  & ! List of stations to be excluded
     &           LIST_STAOFF(MAX_ARC_STA)*8,  & ! List of station with positions not to estimated
     &           LIST_STAINC(MAX_ARC_STA)*8,  & ! List of stations to be included
     &           LIST_STAON(MAX_ARC_STA)*8,   & ! List of stations with position estimated
     &           LIST_SOUEXC(MAX_ARC_SRC)*8,  &
     &           LIST_SOUOFF(MAX_ARC_SRC)*8
!
      CHARACTER  CLOCK_MD_TYP*1,   & 
     &           CLOCK_INTV_TYP*1, &
     &           CLOCK_CNS_TYP*1,  &
     &           GRAD_INTV_TYP*1,  &
     &           ATM_INTV_TYP*1,   &
     &           ATM_CNS_TYP*1,    &
     &           GRAD_CNS_TYP*1
!
      INTEGER*4  CLOCK_MD_VAL
!
      REAL*8     CLOCK_INT_VAL,    &
     &           CLOCK_CNS_VAL,    &
     &           ATM_INT_VAL,      &
     &           ATM_CNS_VAL,      &
     &           GRAD_INT_VAL,     &
     &           GRAD_CNS_OFFS,    &
     &           GRAD_CNS_RATE     
!!
      COMMON / BATME / NATMEX, IATMEX, NGRADEX, IGRADEX, KGRAD, &
     &           NUM_ATMOFF,       &
     &           NUM_ATMEXC,      &
     &           NUM_GRADOFF,       &
     &           NUM_STAEXC,       &
     &           NUM_STAOFF,       &
     &           NUM_STAINC,       &
     &           NUM_STAON,        &
     &           NUM_SOUOFF,       &
     &           NUM_SOUEXC,       & 
!
     &           LIST_ATMOFF,      &
     &           LIST_ATMEXC,      &
     &           LIST_GRADOFF,     &
     &           LIST_STAEXC,      & 
     &           LIST_STAOFF,      &
     &           LIST_STAINC,      & 
     &           LIST_STAON,       &
     &           LIST_SOUEXC,      &
     &           LIST_SOUOFF,      &
!
     &           CLOCK_MD_TYP,     & 
     &           CLOCK_INTV_TYP,   &
     &           CLOCK_CNS_TYP,    &
     &           GRAD_INTV_TYP,    &
     &           ATM_INTV_TYP,     &
     &           ATM_CNS_TYP,      &
     &           GRAD_CNS_TYP,     &
!
     &           CLOCK_MD_VAL,     &
!
     &           CLOCK_INT_VAL,    &
     &           CLOCK_CNS_VAL,    &
     &           ATM_INT_VAL,      &
     &           ATM_CNS_VAL,      &
     &           GRAD_INT_VAL,     &
     &           GRAD_CNS_OFFS,    &
     &           GRAD_CNS_RATE     
