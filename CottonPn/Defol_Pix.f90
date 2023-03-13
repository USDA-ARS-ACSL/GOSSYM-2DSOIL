
    SUBROUTINE DEFOLIAT
    ! *************************************************************************
    ! *                                                                        *
    ! *      DEFOLIAT SUBROUTINE.  SIMULATES THE EFFECTS OF COMMONLY USED      *
    ! *        DEFOLIATES AND PREP ON COTTON.                                  *
    ! **************************************************************************

    use common_block

    ! IF TODAY IS DATE OF PREP APPLICATION THEN WE MUST CALCULATE % INTERCEPTED

    DO I=1,5
         IF(DAYNUM.EQ.PRPDATE(I)) THEN                                  !PRP date
            IF(PRPMTH(I).EQ.0) THEN                                     !Banded
                PRPKGH=PRPKGH+PRPPPA(I)*.95*1.12085*.75
            ELSE
                PRPKGH=PRPKGH+PRPPPA(I)*INT*1.12085*.75                 !if sprinkeler or broadcast
            ENDIF
            TDFKGH=DEFKGH+PRPKGH
        ENDIF

        IF(DAYNUM.EQ.DEFDATE(I)) THEN                                   !DEF Date
            IF(DEFMTH(I).EQ.0) THEN
                DEFKGH=DEFKGH+DEFPPA(I)*.95*1.12085*.75
            ELSE
                DEFKGH=DEFKGH+DEFPPA(I)*INT*1.12085*.75
            ENDIF
            TDFKGH=DEFKGH+PRPKGH
        ENDIF
    ENDDO
     IF((DEFBGN.GT.0).AND.(DAYNUM.EQ.DEFBGN)) THEN
        LFATDF = 0
        DO 70 I=1,LEFCNT
            IDUM = LEFSRT(I)
            IF(IDUM.GT.0) THEN
                K = IDUM/1000
                L = IDUM/10-K*100
                M = IDUM-IDUM/10*10
                IF(LEAFW(K,L,M).LT..0001) THEN
                    LEFSRT(I) = 0
                ELSE
                    LFATDF = LFATDF+1
                ENDIF
            ENDIF
70      CONTINUE
    ENDIF

    IF((PRPDAY.GT.0).AND.(DAYNUM.GT.PRPDAY))	 &
        AVGTSP=(AVGTSP*(DAYNUM-(PRPDAY+1))+TAVG)/(DAYNUM-PRPDAY)

    ! *** The constant 4. in the following logic is from DNB,VRR,FDW
    ! *** field obs. at MITCHENERS 1989
    !
    IF((DEFBGN.GT.0).AND.(DAYNUM.GT.DEFBGN)) THEN
        AVGTSD=(AVGTSD*(DAYNUM-(DEFBGN+1))+TAVG)/(DAYNUM-DEFBGN)
        DUM = PSILD*10.
        PERDEF = -35.2076+.5255*AVGTSD+7.0586*TDFKGH+				 &
            1.7510*(DAYNUM-DEFBGN)-2.476*DUM-.03744*DUM**2+	 &
            .0004198*AVGTSD*TDFKGH*(DAYNUM-DEFBGN)*DUM
        IF((DEFDAY.GT.0).AND.(DAYNUM.GT.DEFDAY)) THEN
            PERDEF = PERDEF*4.
        ELSE
            PERDEF = PERDEF*1.5
        ENDIF
        IF(PERDEF.LT.0.) PERDEF = 0.
        IF(PERDEF.GT.100.) PERDEF = 100.
    ENDIF

    RETURN
    END


    SUBROUTINE PIX
    !  *************************************************************************
    !  *                                                                       *
    !  *              KRREDDY's PIX  SUBROUTINE                  KIT 5/12/99   *
    !  *************************************************************************
    !  *   THIS SUBROUTINE CALCULATES THE EFFECTS OF MEPIQUAT CHLORIDE         *
    !  * ON DIFFERENT PHYSIOLOGICAL PROCESSES OF COTTON INCLUDING GROWTH AND   *
    !  * DEVELOPMENT.  THE DATABASE WAS DEVELOPED USING BASF CORPORATION'S     *
    !  * GROWTH REGULATOR PIX AND USDA/ARS SOIL PLANT ATMOSPHERIC RESEARCH     *
    !  * UNITS AT MISS.STATE.  THE DATABASE WAS DEVELOPED USING                *
    !  * 20 G PIX AI/ACRE AND  AT A RANGE OF TEMPERATURE TREATMENTS.           *
    !  *************************************************************************

    use common_block

    ! IF AN APPLICATION OF PIX HAS BEEN MADE, CALCULATE INTERCEPTED PORTION PIX
    ! AND CONVERT INTO SI UNITS (G). TAKE THE AMOUNT PIX APPLIED AND ADD TO THE
    ! EXISTING CONCENTRATION OF PIX IN THE PLANT.

    PIXLOS=DEAD2DAY*PIXCON                  ! gC lossed due to abscission 
    PIXPLT=PIXPLT-PIXLOS                    !Pix remaining today
    IF(Daynum.EQ.PIXDAY(IPIX)) THEN
        IF(PIXMTH(IPIX).EQ.0) THEN                                      !if banded
            PIXPLT = PIXPLT + (19068.0 * PIXPPA(IPIX)*.90) / POPPLT     !pixppa is in lb/acre     
        ELSE                                                            !if sprinkler or broadcast
            PIXPLT = PIXPLT + (19068.0 * PIXPPA(IPIX)*INT) / POPPLT     !Int : FRACTION OF SOLAR RADIATION INTERCEPTED
        ENDIF
        IPIX = IPIX + 1
    ENDIF

    ! *** BASED ON FALLEN LEAF WEIGHT,SQUAR WEIGHT, BOLL WEIGHT AND DEAD
    ! *** ROOT WEIGHT PIXLOS IS CALCULATED AND PIX CONCENTRATION IN THE
    ! *** PLANT IS REDUCED EVERDAY IN OTHER SUBROUTINES OF THE PROGRAM.
    ! *** CALCULATE PIX CONCENTRATION IN COTTON PLANT ON A DRYWEIGHT BASIS
    ! *** EVERDAY. ASSUME PIX CONCENTRATION IS SAME IN ALL PLANT PARTS.
    ! *** PIX IS SYSTEMIC IN NATURE AND EASILY MOVABLE WITH IN THE PLANT.

    if(plantw.gt.0.0) then
        pixcon = pixplt / plantw                                    !pix concentration
    else
        pixcon = 0.0
    endif

    ! *** CALCULATE STRESS PARAMETERS FOR HEIGHT,NODES AND LEAFAREA AS A
    ! *** FUNCTION OF DAILY AVERAGE TEMPERATURE AND PIX CONC. IN PLANT TISSUE.

    conc = pixcon
    if(conc.gt.0.035 ) conc = 0.035
    pixdz = 1.0 - 18.619 * conc                         !Stress for plant height
    pixda = 1.0 - 7.774 * conc                          !Stress for leaf area
    if(conc.gt.0.02 ) conc = 0.02
    pixdpn = 1.0 - 25.167*conc + 590.286*conc*conc      !Stress for nodes

    IF(PIXDZ.GE.1.0)PIXDZ=1.0
    IF(PIXDZ.LE.0.45)PIXDZ=0.45
    IF(PIXDA.GE.1.0)PIXDA=1.0
    IF(PIXDA.LE.0.7)PIXDA=0.7
    IF(PIXDPN.GE.1.0)PIXDPN=1.0
    IF(PIXDPN.LE.0.7)PIXDPN=0.7

    RETURN
    END