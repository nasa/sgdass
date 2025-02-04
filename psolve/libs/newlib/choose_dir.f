      SUBROUTINE CHOOSE_DIR(DEFAULT_DIR,CHOSEN_DIR,ICHOSEN_LEN)
!
!     CHOOSE A DIRECTORY.
!
!     restrictions: The user is expected to specify a valid directory.
!                   Any errors resulting from the choice (such as the
!                   specification of a non-existent directory) must be
!                   handled by the caller.
!
!
      IMPLICIT NONE
!
!     input:
!
!     default_dir - directory to be offered as a default choice OR
!                   + in character 1 if no default directory is allowed.
!
      character*(*) default_dir
!
!     output:
!
!     chosen_dir - directory selected by user
!     ichosen_len - length of selected directory's name
!
      character*(*) chosen_dir
      integer*2 ichosen_len
!
!     local variables
!
      integer*2 iluout,iluin,trimlen,idlen
      logical*2 def_allowed
      character*1 type_choice,choice1,choice2,uchoice1,uchoice2
      character*80 temp_choice
!
!     history
!     KDB 97.07.15 - created.
!
      DATA ILUOUT /6/, ILUIN /5/
!
      if (default_dir(1:1).eq.'+') then
        def_allowed = .false.
      else
        def_allowed = .true.
      endif
      idlen = trimlen(default_dir)
!
      type_choice = ' '
      DO WHILE (type_choice.NE.'D'.AND.type_choice.NE.'P' &
     &          .AND. type_choice.NE.'Q')
!       Loop until the user chooses:
!             D - default directory
!             P - valid, user specified path
!             Q - quit
        if (def_allowed) then
          write(iluout,'(" (D)efault (return chooses): ",A)') &
     &      default_dir(1:idlen)
          write(iluout,"(' OR ',$)")
        endif
        write(iluout,'(" absolute_dir OR relative_dir")')
        if (def_allowed) then
          write(iluout,'( &
     &        "    (!d and !q for subdirectories d and q)")')
        else
          write(iluout,'("    (!q for subdirectory q) ")')
        endif
        write(iluout,'(" OR (Q) or :: to quit ")')
        write(iluout,'("? ",$)')
        temp_choice = '*'
        read(iluin,"(A)") temp_choice
!       Get first character to test for alphanumeric later
        choice1 = temp_choice(1:1)
        choice2 = temp_choice(2:2)
        uchoice1 = choice1
        uchoice2 = choice2
        call casefold(uchoice1)
        call casefold(uchoice2)
        if (choice1.eq.'/') then
!         Absolute path
          type_choice = 'P'
        else if (choice1.eq.'.') then
!         Potential relative directory of the form . or ..
          type_choice = 'P'
        else IF (TRIMLEN(temp_choice).EQ.0) THEN
!         User hit return (wants the default path)
          if (def_allowed) then
            type_choice = 'D'
          else
            type_choice = ' '
          endif
        ELSE IF (choice1.eq.'!') THEN
!         Check for special subdirectory names d, q, D, Q.
!         (These are also single character options so they must be handled
!          specially, as an escape character is used for a meta character)
          if (uchoice2.eq.'D'.or.uchoice2.eq.'Q') then
!           special subdirectory name
!           (also allow !d<other chars>.)
            temp_choice = temp_choice(2:)
            type_choice = 'P'
          else
!           ! is only intended as an "escape character" for the option names.
!           ! followed by anything else is not allowed.
            type_choice = ' '
          endif
        else if ((lge(uchoice1,'A').and.lle(uchoice1,'Z')) .or. &
     &        (lge(uchoice1,'0').and.lle(uchoice1,'9'))) then
!         alphanumeric - check to see if the user wants a subdirectory
!         or a single character option
          if (trimlen(temp_choice).eq.1) then
!           Check for single character options
            if (uchoice1.eq.'D') then
              if (def_allowed) then
                type_choice = 'D'
              else
                type_choice = ' '
              endif
            else if (uchoice1.eq.'Q') then
              type_choice = 'Q'
            else
!             Assume the user wants a subdirectory
              type_choice = 'P'
            endif
          else
!           For a multicharacter choice starting with an alphanumeric
!              character, assume the user wants a subdirectory
            type_choice = 'P'
          endif
        ELSE IF (TEMP_CHOICE(1:2).EQ.'::') THEN
          type_choice = 'Q'
        ELSE
!         begins with invalid symbol (%,# etc.)
          type_choice = ' '
        ENDIF
        if (type_choice.eq.' ') &
     &    write(iluout,"('***bad choice - try again**')")
      END DO
!
      IF (type_choice.EQ.'Q') then
        ichosen_len = 0
        chosen_dir = '::'
      ELSE IF (type_choice.EQ.'D') then
        ichosen_len = idlen
        chosen_dir = default_dir(1:idlen)
      ELSE IF (type_choice.EQ.'P') then
        ichosen_len = trimlen(temp_choice)
        chosen_dir = temp_choice(1:ichosen_len)
      endif
!
      RETURN
      END
