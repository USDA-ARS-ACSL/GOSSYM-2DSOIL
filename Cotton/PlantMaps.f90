
    SUBROUTINE PLANTMAPS

    ! *** Original PLANTMAPS subroutine with DNBaker's code ***
    ! *** MAX # Of: VBranch = 3, FBranch = 30, Fruiting Sites = 5

    use common_block

    INTEGER*2 IPLTHT
    CHARACTER FILNAM*13,DSCRIP*51
    INTEGER*4 POINTR

1000 FORMAT(I2)

    DO 80 I=1,30
        NPLANT(I)=0
        PLTHT(I)=0.
        MSATYP(I)=0
        ANGBOLL(I)=0
        DO 80 J=1,450
            AVGPLT(I,J)=0.
80  CONTINUE
    IF(PMAFIL.NE.'             ') THEN
        OPEN(11,FILE=PMAFIL,FORM=FILFRM,STATUS='OLD',ERR=340)
        I12 = 0
90      CONTINUE

        I=0
        JPDAY=0
100     CONTINUE
        IF((RUNMODE.EQ.'pmap').OR.(RUNMODE.EQ.'PMAP')) THEN
            CALL MEMPLANT
            IF((PMAFIL.EQ.'iamdone').OR.(PMAFIL.EQ.'IAMDONE')) &
                GO TO 300
            PROFLE=PMAFIL
            IPLTHT=NFRQ
        ELSEIF((OPSYS.EQ.'dos').OR.(OPSYS.EQ.'DOS')) THEN
            READ(11,END=300,ERR=200) FILNAM,DSCRIP,MSDATE,IPLTHT, &
                NDMSMS,NDMSPF,NDV1MS,NDV1PF,NDV2MS, &
                NDV2PF,(PLTMAP(J),J=1,450),POINTR
            IF(I12.GT.0) READ(12,1000,END=120,ERR=120) NGBOLLMS
        ELSE
            READ(11,*,END=300,ERR=200) FILNAM,DSCRIP,MSDATE,IPLTHT,	&
                NDMSMS,NDMSPF,NDV1MS,NDV1PF,NDV2MS,	&
                NDV2PF,(PLTMAP(J),J=1,450),POINTR
            IF(I12.GT.0) READ(12,1000,END=120,ERR=120) NGBOLLMS
        ENDIF
120     CONTINUE
        CALL CALTOJULAN(MSDATE,IDUM1,IDUM2,KDAY,J2DAY)
        IF(J2DAY.NE.JPDAY) THEN
            IF(I.GT.0) THEN
                NPLANT(I)=KNT
                PLTHT(I)=PLTHT(I)/KNT
                MSANODE(I)=MSANODE(I)/KNT
                IF((MSATYP(I).GT.0).AND.(KNT.GT.1)) THEN
                    ANGBOLL(I)=ANGBOLL(I)/KNT
                    DO J=1,450
                        AVGPLT(I,J)=AVGPLT(I,J)/KNT
                    ENDDO
                ENDIF
            ENDIF
            I=I+1
            KNT=1
            JPDAY=J2DAY
            MSADTE(I)=J2DAY
            PLTHT(I)=IPLTHT
            MSANODE(I)=NDMSMS
            MSATYP(I)=0
            NODPMAP(I,1)=NDMSMS
            NODPMAP(I,2)=NDMSPF
            NODPMAP(I,3)=NDV1MS
            NODPMAP(I,4)=NDV1PF
            NODPMAP(I,5)=NDV2MS
            NODPMAP(I,6)=NDV2PF
            ANGBOLL(I)=NGBOLLMS
            IF(NDMSMS.GT.0) MSATYP(I)=1
            IF(I12.GT.0) MSATYP(I)=2
            DO 140 J=1,450
                IF((PLTMAP(J).EQ.'b').OR.(PLTMAP(J).EQ.'B').OR. &
                    (PLTMAP(J).EQ.'s').OR.(PLTMAP(J).EQ.'S').OR. &
                    (PLTMAP(J).EQ.'g').OR.(PLTMAP(J).EQ.'G')) THEN
                    AVGPLT(I,J)=1
                ELSE
                    AVGPLT(I,J)=0
                ENDIF
140         CONTINUE
        ELSE
            KNT=KNT+1
            PLTHT(I)=PLTHT(I)+IPLTHT
            MSANODE(I)=MSANODE(I)+NDMSMS
            IF(NODPMAP(I,1).LT.NDMSMS) NODPMAP(I,1)=NDMSMS
            IF(NODPMAP(I,2).LT.NDMSPF) NODPMAP(I,2)=NDMSPF
            IF(NODPMAP(I,3).LT.NDV1MS) NODPMAP(I,3)=NDV1MS
            IF(NODPMAP(I,4).LT.NDV1PF) NODPMAP(I,4)=NDV1PF
            IF(NODPMAP(I,5).LT.NDV2MS) NODPMAP(I,5)=NDV2MS
            IF(NODPMAP(I,6).LT.NDV2PF) NODPMAP(I,6)=NDV2PF
            ANGBOLL(I)=ANGBOLL(I)+NGBOLLMS
            DO J=1,450
                IF((PLTMAP(J).EQ.'b').OR.(PLTMAP(J).EQ.'B').OR. &
                    (PLTMAP(J).EQ.'s').OR.(PLTMAP(J).EQ.'S').OR. &
                    (PLTMAP(J).EQ.'g').OR.(PLTMAP(J).EQ.'G')) THEN
                    AVGPLT(I,J)=AVGPLT(I,J)+1
                ENDIF
            ENDDO
        ENDIF
        GO TO 100
200     CONTINUE
        PRINTBUF=' error on read in plant map adj. file'
        CALL WRITERR
        ABEND= .TRUE.
        CLOSE(11)
        RETURN
300     CONTINUE
        IF(I.GT.0) THEN
            NPLANT(I)=KNT
            PLTHT(I)=PLTHT(I)/KNT
            MSANODE(I)=MSANODE(I)/KNT
            IF((MSATYP(I).GT.0).AND.(KNT.GT.1)) THEN
                ANGBOLL(I)=ANGBOLL(I)/KNT
                DO J=1,450
                    AVGPLT(I,J)=AVGPLT(I,J)/KNT
                ENDDO
            ENDIF
        ENDIF
        CLOSE(11)
    ENDIF
340 CONTINUE
    RETURN
    END

    SUBROUTINE MEMPLANT

    use common_block

    PMAFIL='IAMDONE'
    ABEND=.TRUE.
    RETURN
    END
