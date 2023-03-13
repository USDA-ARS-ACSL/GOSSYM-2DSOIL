    ! Daily simulation of the crop from Emerging day to end day


    SUBROUTINE GosLoop

    use common_block
    KDAY=JDAY-EMERGINGDAY+1    !LSC JDAY=DAYNUM
    
         IF((PIXDAY(1).GT.0).AND.(DAYNUM.GE.PIXDAY(1))) CALL PIX
            IF((DEFBGN.GT.0).AND.(DAYNUM.GE.DEFBGN)) call defoliat
    CALL PNET !Calculate gross photosynthate
    CALL GROWTH
    CALL PLTMAPS
    CALL ABCISE
    DO 320 I=1,15
        IPLTNO=I
        IF(DAYNUM.EQ.MSADTE(I)) CALL MDSEA(IPLTNO) !related to plant map

320 CONTINUE
    CALL MATBAL    !Calculate the total plant weight

    CALL OUTPUT

    RETURN
    END