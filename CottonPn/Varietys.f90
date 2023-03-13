    SUBROUTINE VARIETYS

    use common_block

    CHARACTER VDSCRIP*51
    Character InStringVar*132
120 FORMAT(A8)
    
    Open(40,file = VarietyFile, status = 'old',ERR=200)
    15       Read (40,'(A132)') InStringVar
          if (InStringVar(1:14).ne.'[Phenology]') goto 15
    
    READ(40,120,ERR=200,END=300) VTYNAM
    READ(40,*,ERR=200,END=300) CALBRT 
    VARITY(IVARTY) = VTYNAM  
    CLOSE(40)
    RETURN
    
200 CONTINUE
    PRINTBUF=' error on read in variety file'
    CALL WRITERR
    CLOSE(40)
    ABEND = .TRUE.
    RETURN
300 CONTINUE
    PRINTBUF=' end-of-file while reading variety file'
    CALL WRITERR
    CLOSE(40)
    ABEND = .TRUE.
    RETURN
    END