    SUBROUTINE GROWTH
    ! ************************************************************
    ! *                                                          *
    ! *                  GROWTH SUBROUTINE                       *
    ! *                                            kit 5/11/99   *
    ! ************************************************************

    use common_block

    DAYTYM = DAYLNG / 24. !Daytym and NYttym are the fraction of the 24 hours period in day and night
    NYTTYM = 1. - DAYTYM 
    IF(TDAY.LT.13.5)TDAY=13.5 !Average day time temp.
    IF(TNYT.LT.13.5)TNYT=13.5 !Average night time temp.
    SPDWSQ = 0.0 !SUM OF POTENTIAL WEIGHT GAINS BY SQUARES
    SPDWBO = 0.0 !TOTAL POTENTIAL CHANGE IN WEIGHT OF BOLLS
    SPDWLD = 0. !TOTAL POTENTIAL LEAF GROWTH DURING THE Day
    SPDWLN = 0. !TOTAL POTENTIAL LEAF GROWTH DURING THE NIGHT
    PFAREA = 0. !PfAREA: AREA OF LEAf AT PREFRUITING NODE
    AREA = 0.   !Total leaf area
    
  
    !All the variables are for 1 plant or per plant
    !Number of plants in the soil slab = Popslab
    !Slab (in plan) = (row width cm * 1cm) if plant is in the middle
    !Slab (in plan) = (rows width cm * 1cm* 0.5) if plant is in the edge
    !to convert form slab to plant divide by popslab
    !to convert from plant to slab multiply by popslab
    
    if (ROOTWT.le.0.2) ROOTWT=0.2
    
    !! *** Water stress factor for stem growth
    call stem_growth_water_stress           !uses Psild and variety parameter

    ! *** Temperature and water stress factors affecting leaf and fruit growth
    call leaf_growth_water_stress           !uses Psild and variety parameter
    call fruit_growth_water_stress          !uses average water potential of root zone (bars), Tday, Tnyt
    
    ! *** Calculate the potential growth of the plant parts
    ! *** Calculate potential growth of prefruiting leaves
    call PfLeafArea                         !use age of leaf, LeafCN

    ! *** Calculate potential square (PDWSQ) and boll (PDWBOL) growth
    !FCODE - IK,L,M,) FRUIT CODE (l=SOUARE, 2=GREEN BOLL,     
    !=OPEN BOLL, 4=ABSCISED, 5=SQUARE MARKED FOR ABSCISION,  
    !6   =BOLL MARKED FOR ABSCISION, 7= BOLL OF AGE SUSCEPTIBLE  
    !TO ABSCISSION 
    
    IF(FCODE(1,1,1).gt.0) then
        FRATIO = GBOLWT/PLANTW              !fratio-ratio of green bolls weight to total plant weight
        DO K=1,NVBRCH                       !NVBRCH:number of vegetative branches
            NBRCH = NFBR(K)                 !nfbr(k):number of fruiting branches on the vegetative branch
            DO L=1,NBRCH                    !nbrch: number of fruiting branch
                NNID = NNOD(K,L)            !nnod(m): number of nodes on the fruiting branch
                DO M=1,NNID                 !nnid: node number on fruiting branch
                    call pot_sqr_growth(k,l,m)  !running average temperature of each node
                    call pot_bol_growth(k,l,m)
                enddo
            enddo
        enddo

        ! *** Calculate potential main stem leaf growth. Assume that main
        ! *** stem leaf is initiated at the same time as fruiting branch leaf.

        call MSLeafArea !main stem
        call FBLeafArea !fruiting branch

    endif

    ! *** CALCULATE POTENTIAL STEM GROWTH

    call pot_stm_growth

    ! CALL RUTGRO(0) ! This was a routine in GOSSYM Rhizos for calculating the potential root growth

    ! The output from RutGro(0) is the SPDWRT, which is the sum of the potential delta weight of roots over all cells in gms.This considers root growth reduction due to soil impedance and other root growth stress factors
    ! The same is currently calculated in the Carbon_Partitioning.for. The proportional reduction of root growth due to mechanical resistance, partial pressure of soil oxygen, soil temperature, and other physical causes are taken in to consideration
 
    SPDWRT = potenRootWgt/popslab     !potenRootWgt- SUM OF POTENTIAL DELTA WEIGHT OF ROOTS OVER ALL CELLS, GMS.
    
    SPDWRD = SPDWRT*DAYTYM      !SPDWRD- TOTAL POTENTIAL ROOT GROWTH DURING THE DAY
    SPDWRN = SPDWRT*NYTTYM      !SPDWRN- TOTAL POTENTIAL ROOT GROWTH DURING THE NIGHT


    ! *** Calculate root/shoot correction factors for potential root,
    ! *** stem amd leaf growth . Source: AVI BEN-PORATH PHD THESIS.
      call rootshootratio         !SPDWRN and SPDWRD are modified based on the root shoot ratio
 
    ! *** Calculate carbohydrate demand of each organ and the whole plant
 
    CDSTEM = PDSTMD + PDSTMN    !Cdstem [dryweight (ch20) in g/plant]    
    CDROOT = SPDWRD + SPDWRN    !modified for day and night for the root shoot ratio
    CDLEAF = SPDWLD + SPDWLN 
    CDSQAR = SPDWSQ
    CDBOLL = SPDWBO
        !Total carbohydrate demand 
    CD = CDSTEM + CDLEAF + CDROOT + CDSQAR + CDBOLL 
    CPOOL = PN + RESC           ! Total available pool of carbohydrate from todays increment of photosynthate production + reserve carbon from earlier days


    ! *** Calculate carbohydrate stress (CSTRES)
    call Carbon_stress

    ! *** Reduce the potential weight of each organ before calculating the nitrogen demand and nitrogen stress
    PDSTEM = CDSTEM * CSTRES    ![dryweight (ch20) in g/plant] 
    PDLEAF = CDLEAF * CSTRES
    PDROOT = CDROOT * CSTRES    !total demand* carbon stress
    PDBOLL = CDBOLL * CSTRES
    PDSQ   = CDSQAR * CSTRES

    IF(PDSTEM.LT.0.) PDSTEM=0.
    IF(PDLEAF.LT.0.) PDLEAF=0.
    IF(PDROOT.LT.0.) PDROOT=0.
    IF(PDBOLL.LT.0.) PDBOLL=0.
    IF(PDSQ.LT.0.) PDSQ=0.

    ! *** Call NITRO to get today's N stress
    CALL NITRO
 
                  
    IF(LEAFWT.gt.0.0) then

        ! *** Calculate excess carbohydrate created by N stress
        ! if there is no enough translocatable nitrogen availabe in the plant for assimilation of the avilable carbohydrate, then the excess carbohydrate is added to XTRAC  
        CH2OX = (CDSTEM+CDLEAF)*CSTRES*(1.-NV) +			&
            (CDSQAR+CDBOLL) *CSTRES*(1.-NF)  +			&
            (CDROOT*CSTRES *(1.-NR))
        XTRAC = XTRAC + CH2OX !permenanatly added to the root system

        ! *** Initialize LEAFWT to 0.2 (Initial leaf weight).
        ! *** Assume cotyledons fall off at first square.

        IF(ISQ.GT.0) THEN !switch turn on by the first square
            LEAFWT = 0.
        ELSE
            LEAFWT = 0.2
        ENDIF
      
        ! *** Calculate nutrogen stress
        ! Combine the nitrogen and carbohydrate stresses in to single parameter
        VSTRES = NV * CSTRES
        FSTRES = NF * CSTRES
        RSTRES = NR * CSTRES   

        SDWSQR = 0.0
        SDWBOL = 0.0
        SDWLEF = 0.0

        ! *** Calculate actual growth of organs

        IF(FCODE(1,1,1).gt.0) then
            SQWT = 0.0
            GBOLWT = 0.0
            DO K = 1,NVBRCH
                NBRCH = NFBR(K)
                DO L = 1,NBRCH
                    NNID = NNOD(K,L)
                    DO M = 1,NNID

                        ! *** Calculate actual square growth

                        call Actual_square_growth(k,l,m)

                        ! *** Calculate actual boll growth

                        call Actual_boll_growth(k,l,m)

                        ! *** Calculate actual main stem and fruiting branches leaf growth

                        call Actual_leaf_growth(k,l,m)

                        ! *** Calculate actual main stem and fruiting branches leaf areas

                        call Actual_leaf_area(k,l,m)
100                 enddo
101             enddo
102         enddo
        endif

        ! *** CALCULATE prefruiting LEAF GROWTH AND NEW LEAF AREA

26      DO J=1,NUMPFN
            call Actual_prefruitingleaf_growth(j)
            call Actual_prefruitingleaf_area(j)
28      enddo

        !PfAREA- AREA OF LEAf AT PREFRUITING NODE
      
        AREA = AREA + PFAREA            !total leaf area[dm2], unit conversion from cm2 to dm2 for the area is done in pfleafarea routine 
        LAI = AREA/POPFAC               !POFAC [dm2 ground area per plant]
! *** CALCULATE stem and root GROWTH
        SDWSTM = CDSTEM * VSTRES
        RCH2O1  = CDROOT * RSTRES       !Root carbohydrate supply per plant [gm of c/plant]
        
       
        !Check if PCRS=RCH20   
        !Pcrq=pcrq+ (pcrs-rch20)
        
        !PCRQ:Maximum carbon provided to grow roots [g of C/day/slab]
        !pcrl:minimum carbon needed to grow roots [g of c/day/slab] 

        !Sending PCRQ and PCRL to the 2D soil carbon partitioning
        
        
        PCRQ=RCH2O1*popslab!  X popslab:  per plant to per slab
        PCRL=RCH2O1*popslab
        RCH2O=PCRSD/popslab  ! /popslab: per slab to per plant   !RCH2O is [g of C/plant]
        
        !rch201:root carbohydrate supply per plant in g of c/plant
        !rch20: total carbon used ot grow roots in g of c/plant
        !pcrsd: total carbon used to grow roots g of c/day/slab 
                    
        ! *** SLEAF, SBOLL, SQUAR, SSTEM ARE THE CUMMULATIVE AMOUNT
        ! *** OF CH0 ALLOCATED TO THE DIFFERENT SINKS.
        
        SLEAF = SLEAF + SDWLEF
        SBOLL = SBOLL + SDWBOL
        SQUAR = SQUAR + SDWSQR
        SSTEM = SSTEM + SDWSTM
        SROOT = SROOT + RCH2O 

        STEMWT = STEMWT + SDWSTM
        STMWT(KDAY) = STEMWT
        
        ! *** NOTE THAT STORED CH2O IS USED IN CALCULATION OF TOTAL
        ! *** PLANT WEIGHT. Xstor
        !CALCULATE VERTICAL GROWTH
      
40      IF (PLANTW.LE.1.0) VSTRES = 1.

        CALL PLANTHEIGHT

510     FORMAT(1X,I5,10F7.2)
    endif

    RETURN
    END

    
    
    
     subroutine stem_growth_water_stress
    ! ************************************************************
    ! *   Based on Marani et.al., 1985 Effects of water stress   *
    ! * on canopy senescence and carbon exchange rates in cotton *
    ! * Crop Sci. 25:798-802.                                    *
    ! *                                            kit 12/14/99  *
    ! ************************************************************

    use common_block

    PSILIN = CALBRT(16) !!psilin: minimum leafwater potential for the day in well watered soil: From variety file
    WSTST1 = 13.16 + 9.007*PSILIN + 1.452*PSILIN*PSILIN 
    WSTST2 = 13.16 + 9.007*PSIL_G + 1.452*PSIL_G*PSIL_G    !Psild: Leaf water potential
    IF(WSTST2.LE.0.1) WSTST2=0.1                        !
    WSTRST = WSTST2/WSTST1                              
    IF(WSTRST.GT.1.05) WSTRST = 1.05
    IF(WSTRST.LT.0.00) WSTRST = 0.00  !stem growth water stress
    RETURN
    END


    subroutine leaf_growth_water_stress
    ! ************************************************************
    ! *   Based on Marani et.al., 1985 Effects of water stress   *
    ! * on canopy senescence and carbon exchange rates in cotton *
    ! * Crop Sci. 25:798-802. Leaf growth is affected by water   *                                     *
    ! * stress at <= -1.0 bar. 	rsquare = 0.73						       *
    ! *                                            kit 12/14/99  *
    ! ************************************************************

    use common_block

    PSILIN = CALBRT(33)
    WSTRL1 = 44.89+33.98*PSILIN+6.38*PSILIN*PSILIN
    WSTRL2 = 44.89+33.98*PSIL_G+6.38*PSIL_G*PSIL_G
    IF(WSTRL2.LE.0.1) WSTRL2=0.1
    WSTRLF = WSTRL2/WSTRL1
    IF(WSTRLF.GT.1.05) WSTRLF = 1.05
    IF(WSTRLF.LT.0.00) WSTRLF = 0.00
    day_lfstress = DAYTYM * WSTRLF * PIXDA * calbrt(32)
    eve_lfstress = NYTTYM * WSTRLF * PIXDA * calbrt(32)
    RETURN
    END


    subroutine fruit_growth_water_stress
    ! *************************************************************
    ! *   Based on Marani and Baker, 1981 Science Report to US    *
    ! * Israel Binational Foundation. Development of a Predictive *
    ! * Dynamic Simulation Model of growth and yield in Acala.    *
    ! *                                            kit 12/14/99   *
    ! *************************************************************

    use common_block
    !WSTRSD: WATER STRESS DAY. fRACTION OF DAY TIME PERIOD DURING
    !WHICH LEAF IS TURGID ENOUGH (ABOVE -7 BARS) FOR GROWTH
    !WSTRSN: WATER STRESS NIGHT. SAME AS ABOVE
    !PSIM: AVERAGE WATER POTENTIAL Of ROOT ZONE, IN BARS.
   ! PSIAVG1/timecount
   ! WSTRSD = -2.5/(PSISM - 1.6)+(0.0005*PSISM*TDAY) - 0.001*RN
   ! WSTRSN = -2.5/(PSISM - 1.6)+(0.0005*PSISM*TNYT)
    WSTRSD = -2.5/(PSIAVG - 1.6)+(0.0005*PSIAVG*TDAY) - 0.001* (RI/41868*.71536-26.)
    WSTRSN = -2.5/(PSIAVG- 1.6)+(0.0005*PSIAVG*TNYT)
  
    IF(WSTRSD.LT.0.0) WSTRSD = 0.0
    IF(WSTRSD.GT.1.0) WSTRSD = 1.0
    IF(WSTRSN.LT.0.0) WSTRSN = 0.0
    IF(WSTRSN.GT.1.0) WSTRSN = 1.0
    WSTRS = (WSTRSD*DAYTYM) + (WSTRSN*NYTTYM)
    WFDAY1 = DAYTYM * CALBRT(52) * AMIN1(1.0,0.5+WSTRSD)
    WFNYT1 = NYTTYM * CALBRT(52) * AMIN1(1.0,0.5+WSTRSN)
    WFDAY2 = DAYTYM * CALBRT(57) * AMIN1(1.0,0.3+WSTRSD)
    WFNYT2 = NYTTYM * CALBRT(57) * AMIN1(1.0,0.3+WSTRSN)
    eve_water_index = wfnyt1
    day_water_index = wfday1

    ! *** Calculate water and temperature stress factors for fruit growth

    day_expfac = AMAX1(0.,(0.0160791*TDAY-0.2120865) * WFDAY2)
    eve_expfac = AMAX1(0.,(0.0160791*TNYT-0.2120865) * WFNYT2)
    day_hitemp = AMAX1(0.,(2.7328500-0.082857*TDAY)  * WFDAY1)
    eve_hitemp = AMAX1(0.,(2.7328500-0.082857*TNYT)  * WFNYT1)
    day_lotemp = AMAX1(0.,(0.0312500*TDAY-0.508125) * WFDAY1)
    eve_lotemp = AMAX1(0.,(0.0312500*TNYT-0.508125) * WFNYT1)
    RETURN
    END
    
    
    
    SUBROUTINE PfLeafArea

    use common_block

    do i=1,numpfn !number of prefruting nodes

        ! ***	Calculate the leaf age

        PfLfAge(i)=PfLfAge(i)+Duration_leaf_expansion(tavg)*calbrt(40)
        if(PfLfAge(i).lt.1.0) then
            agepflf(i) = agepflf(i) + 1.0
            cntpflfcn(i) = cntpflfcn(i) + 1.0
            sumpflfcn(i) = sumpflfcn(i) + leafcn
            !Leafcn: Nitrogen concentration in the leaf

            ! *** Initiated once the node is produced (ADDPFNOD routine) except
            ! *** for the first node.  Calculate the initial leaf area at unfolding

            if(PfLfArea(i).le.0.001) then

                ! *** Calculate running average of leaf nitrogen (max leafcn = 0.035)

                if(i.eq.1) then
                    avelfcn = 0.035
                else
                    avelfcn = sumpflfcn(i-1) / cntpflfcn(i-1)
                endif

                ! *** Convert leaf area, cm2 to dm2 and accumulate the daily increment
                ! *** in leaf area to determine the total current leaf area
                ! *** PfAL is the initial leaf area at unfolding = PfLfArea

                call Potential_PreFruiting_Leaf_Area(i)
                PfAL(i) = PfLfArea(i)*CNReduction_to_Leaf_Area(i,avelfcn)  &
                    * calbrt(41)
                raday = 1.0
                ranyt = 1.0
            else

                ! *** Calculate the current rate of leaf expansion during the day and night

                raday = (Rate_Leaf_expansion(tday) + Rate_of_reduction(tday)*agepflf(i))
                ranyt = (Rate_Leaf_expansion(tnyt) + Rate_of_reduction(tnyt)*agepflf(i))
                if(raday.LT.0.003) raday = 0.003
                if(ranyt.LT.0.003) ranyt = 0.003
            endif

            ! *** Calculate the current leaf area

            raday = raday * PfAL(i)
            ranyt = ranyt * PfAL(i)

            ! *** Calculate the corresponding daily potential change in leaf weight

            PFDALD(i) = raday * day_lfstress * calbrt(34)	 &
                * CNReduction_to_Leaf_Area(i,leafcn)
            PFDALN(i) = ranyt * eve_lfstress * calbrt(34)	 &
                * CNReduction_to_Leaf_Area(i,leafcn)
            PFDWLD(i) = PFDALD(i) * Sp_leaf_weight(tday) * CALBRT(36)
            PFDWLN(i) = PFDALN(i) * Sp_leaf_weight(tnyt) * CALBRT(36)
            SPDWLD    = SPDWLD + PFDWLD(i)
            SPDWLN    = SPDWLN + PFDWLN(i)
        else
            PFDALD(i) = 0.0
            PFDALN(i) = 0.0
            PFDWLD(i) = 0.0
            PFDWLN(i) = 0.0
        endif
    enddo
    return
    end
    
   
     subroutine pot_sqr_growth(k,l,m)
    ! ************************************************************
    ! * CALCULATE POTENTIAL SQUARE GROWTH(PDWSQ); rsquare = 0.98 *
    ! ************************************************************

    use common_block
    !FCODE - IK,L,M,) FRUIT CODE (l=SOUARE, 2=GREEN BOLL,     
    !=OPEN BOLL, 4=ABSCISED, 5=SQUARE MARKED FOR ABSCISION,  
    !6   =BOLL MARKED FOR ABSCISION, 7= BOLL OF AGE SUSCEPTIBLE  
    !TO ABSCISSION                                           



    IF(FCODE(K,L,M).EQ.1) THEN

        ! *** Raja Reddy's new square growth equation      5/17/2000 kit
        !avgt - (k,l,m) - running average temperature of each node
        if(avgt(k,l,m).lt.17.0) avgt(k,l,m) = 17.0
        if(avgt(k,l,m).gt.35.0) avgt(k,l,m) = 35.0
        PDWSQ(k,l,m) = -0.01017 + avgt(k,l,m) 				   &
            *(0.0012532-0.000019484*avgt(k,l,m))
        ! pdwsq: potential square growth
        ! spdwsq: sum of potential square growth
        
        SPDWSQ = SPDWSQ + PDWSQ(K,L,M) * calbrt(17)
    ENDIF
    RETURN
    END

    
     subroutine pot_bol_growth(k,l,m)
    ! ************************************************************
    ! *     CALCULATE POTENTIAL BOLL GROWTH(PDWBOL) Original     *
    ! ************************************************************

    use common_block

    ! *** CALCULATE EXPONENTIAL BOLL GROWTH(PDWBOL)
    !FCODE - IK,L,M,) FRUIT CODE (l=SOUARE, 2=GREEN BOLL,     
    !=OPEN BOLL, 4=ABSCISED, 5=SQUARE MARKED FOR ABSCISION,  
    !6   =BOLL MARKED FOR ABSCISION, 7= BOLL OF AGE SUSCEPTIBLE  
    !TO ABSCISSION
    
    IF((FCODE(K,L,M).EQ.2).OR.(FCODE(K,L,M).EQ.7)) THEN
        PHASE1=DEHISS(K,L,M)*0.15
        IF(AGEBOL(K,L,M).LE.PHASE1) THEN
            PDWBOD(K,L,M) = BOLWGT(K,L,M)*day_expfac*FFRUT(K,L,M)
            PDWBON(K,L,M) = BOLWGT(K,L,M)*eve_expfac*FFRUT(K,L,M)
        ELSE

            ! *** D(LN(W))/DT = B = (1/W)(DW/DT) = F(TAVG) LINEAR BOLL GROWTH PHASE.
            ! *** ACTIVE BOLL GROWTH PHASE.

            C00=1.0
            IF((VARITY(IVARTY).EQ.'PIMA').OR.(VARITY(IVARTY).EQ.'pima')) &
                C00=.75
            IF(TDAY.GT.28.5) THEN
                PDWBOD(K,L,M) = day_hitemp*FFRUT(K,L,M)*C00
                PDWBON(K,L,M) = eve_hitemp*FFRUT(K,L,M)*C00
            ELSE
                PDWBOD(K,L,M) = day_lotemp*FFRUT(K,L,M)*C00
                PDWBON(K,L,M) = eve_lotemp*FFRUT(K,L,M)*C00
            ENDIF
        ENDIF
        IF(FFRUT(K,L,M).GT.0.001)THEN
            BSIZE(K,L,M) = BOLWGT(K,L,M)/FFRUT(K,L,M)
        ELSE
            BSIZE(K,L,M) = 0.0
        ENDIF
        IF(BSIZE(K,L,M).GT.CALBRT(12)) THEN
            PDWBOD(K,L,M) = PDWBOD(K,L,M) * 0.25
            PDWBON(K,L,M) = PDWBON(K,L,M) * 0.25
        ENDIF
        SPDWBO = SPDWBO + PDWBOD(K,L,M) + PDWBON(K,L,M)
    ENDIF
    RETURN
    END


    subroutine pot_bol_growthN(k,l,m)
    ! ************************************************************
    ! *          CALCULATE POTENTIAL BOLL GROWTH(PDWBOL)         *
    ! ************************************************************

    use common_block

    IF((FCODE(K,L,M).EQ.2).OR.(FCODE(K,L,M).EQ.7)) THEN

        ! *** Raja Reddy's new boll growth equation      5/17/2000 kit

        if(tday.lt.17.0) tday = 17.0
        if(tday.gt.35.0) tday = 35.0
        if(tnyt.lt.17.0) tnyt = 17.0
        if(tnyt.gt.35.0) tnyt = 35.0
        pdwbod(k,l,m) = (-0.23303 + tday*(0.02797-0.0005852*tday))	 &
            * FFRUT(K,L,M) * daytym * CALBRT(18)
        !	.                     * FFRUT(K,L,M) * day_water_index * CALBRT(18)

        pdwbon(k,l,m) = (-0.23303 + tnyt*(0.02797-0.0005852*tnyt)) 	 &
            * FFRUT(K,L,M) * nyttym * CALBRT(18)
        !	.                     * FFRUT(K,L,M) * eve_water_index * CALBRT(18)
        IF(FFRUT(K,L,M).GT.0.001)THEN
            BSIZE(K,L,M) = BOLWGT(K,L,M)/FFRUT(K,L,M)
        ELSE
            BSIZE(K,L,M) = 0.0
        ENDIF
        IF(BSIZE(K,L,M).GT.CALBRT(12)) THEN
            PDWBOD(K,L,M) = PDWBOD(K,L,M) * 0.25
            PDWBON(K,L,M) = PDWBON(K,L,M) * 0.25
        ENDIF
        SPDWBO = SPDWBO + PDWBOD(K,L,M) + PDWBON(K,L,M)
    ENDIF
    RETURN
    END

 SUBROUTINE MSLeafArea

    use common_block

    do k=1,nvbrch
        nbrch = nfbr(k)
        do l=1,nbrch

            ! ***	Calculate the leaf age

            XMSLfAge(k,l) = XMSLfAge(k,l) + Duration_leaf_expansion(tavg)*calbrt(40)
            if(XMSLfAge(k,l).lt.1.0) then
                agemslf(k,l) = agemslf(k,l) + 1.0
                nodecount = numpfn + l
                cntmslfcn(nodecount) = cntmslfcn(nodecount) + 1.0
                summslfcn(nodecount) = summslfcn(nodecount)+leafcn

                ! *** Initiated once new node is produced (ADDMSNOD routine)

                if(XMSLfArea(k,l).le.0.001) then
                    call Potential_MainStem_Leaf_Area(k,l)
                    if(cntmslfcn(nodecount-1).le.0.0) cntmslfcn(nodecount-1) =1.0
                    avelfcn = summslfcn(nodecount-1)/cntmslfcn(nodecount-1)
                    MLArea(k,l) = XMSLfArea(k,l) * calbrt(42)				  &
                        * CNReduction_to_Leaf_Area(nodecount,avelfcn)
                    raday = 1.0
                    ranyt = 1.0
                else

                    ! *** Calculate the current rate of leaf expansion during the day and night

                    raday = (Rate_Leaf_expansion(tday) 			  &
                        + Rate_of_reduction(tday)*agemslf(k,l))
                    ranyt = (Rate_Leaf_expansion(tnyt) 			  &
                        + Rate_of_reduction(tnyt)*agemslf(k,l))
                    if(raday.LT.0.003) raday = 0.003
                    if(ranyt.LT.0.003) ranyt = 0.003
                endif

                ! *** Calculate the current leaf area

                raday = raday * MLArea(k,l)
                ranyt = ranyt * MLArea(k,l)

                ! *** Calculate the corresponding daily potential change in leaf weight

                PDAMLD(K,L) = raday * day_lfstress * calbrt(35)		  &
                    * CNReduction_to_Leaf_Area(nodecount,leafcn)
                PDAMLN(K,L) = ranyt * eve_lfstress * calbrt(35)		  &
                    * CNReduction_to_Leaf_Area(nodecount,leafcn)
                PDWMLD(K,L) = PDAMLD(K,L) * Sp_leaf_weight(tday)		  &
                    * CALBRT(36)
                PDWMLN(K,L) = PDAMLN(K,L) * Sp_leaf_weight(tnyt)		  &
                    * CALBRT(36)
                SPDWLD = SPDWLD + PDWMLD(K,L)
                SPDWLN = SPDWLN + PDWMLN(K,L)
            else
                PDAMLD(K,L) = 0.0
                PDAMLN(K,L) = 0.0
                PDWMLD(K,L) = 0.0
                PDWMLN(K,L) = 0.0
            endif
        enddo
    enddo
    return
    end


    SUBROUTINE FBLeafArea

    use common_block

    do k=1,nvbrch
        nbrch = nfbr(k)
        do l=1,nbrch
            nnid = nnod(k,l)
            do m=1,nnid

                ! ***	Calculate the leaf age

                FRLfAge(k,l,m) = FRLfAge(k,l,m)						   &
                    +Duration_leaf_expansion(tavg)*calbrt(40)
                if(FRLfAge(k,l,m).lt.1.0) then
                    agefrlf(k,l,m) = agefrlf(k,l,m) + 1.0
                    nodecount = numpfn + l
                    cntmslfcn(nodecount) = cntmslfcn(nodecount) + 1.0
                    summslfcn(nodecount) = summslfcn(nodecount) + leafcn

                    ! *** Initiated once the new node is produced (ADDMSNOD routine)

                    if(FRLfArea(k,l,m).le.0.001) then
                        call Potential_Fruiting_Branch_Leaf_Area(k,l,m)
                        avelfcn=summslfcn(nodecount-1)/cntmslfcn(nodecount-1)
                        LArea(k,l,m) = FRLfArea(k,l,m) * calbrt(43)		    &
                            * CNReduction_to_Leaf_Area(nodecount,avelfcn)
                        raday = 1.0
                        ranyt = 1.0
                    else

                        ! *** Calculate the current rate of leaf expansion during the day and night

                        raday = (Rate_Leaf_expansion(tday) 		  &
                            + Rate_of_reduction(tday)			  &
                            * agefrlf(k,l,m))
                        ranyt = (Rate_Leaf_expansion(tnyt) 		  &
                            + Rate_of_reduction(tnyt)			  &
                            * agefrlf(k,l,m))
                        if(raday.LT.0.003) raday = 0.003
                        if(ranyt.LT.0.003) ranyt = 0.003
                    endif

                    ! *** Calculate the current leaf area

                    raday = raday * LArea(k,l,m)
                    ranyt = ranyt * LArea(k,l,m)

                    ! *** Calculate the corresponding daily potential change in leaf weight

                    PDADAY(K,L,M) = raday * day_lfstress * calbrt(35)	   &
                        * CNReduction_to_Leaf_Area(nodecount,leafcn)
                    PDANYT(K,L,M) = ranyt * eve_lfstress * calbrt(35)	   &
                        * CNReduction_to_Leaf_Area(nodecount,leafcn)
                    PDWFLD(K,L,M) = PDADAY(K,L,M) * Sp_leaf_weight(tday) &
                        * CALBRT(36)
                    PDWFLN(K,L,M) = PDANYT(K,L,M) * Sp_leaf_weight(tnyt) &
                        * CALBRT(36)
                    SPDWLD = SPDWLD + PDWFLD(K,L,M)
                    SPDWLN = SPDWLN + PDWFLN(K,L,M)
                else
                    PDADAY(K,L,M) = 0.0
                    PDANYT(K,L,M) = 0.0
                    PDWFLD(K,L,M) = 0.0
                    PDWFLN(K,L,M) = 0.0
                endif
            enddo
        enddo
    enddo
    return
    end

 subroutine pot_stm_growth
    ! ************************************************************
    ! *         CALCULATE POTENTIAL STEM GROWTH(PDSTMD/N)        *
    ! ************************************************************

    use common_block

    IF(KDAY.LE.CALBRT(13)) THEN  !KDay start from 1 from the date of Emerging day
        !         PDSTMD = (0.1 + 0.02 * KDAY) * DAYTYM * CALBRT(14)
        !         PDSTMN = (0.1 + 0.02 * KDAY) * NYTTYM * CALBRT(14)
        FACDAY = DAYTYM *AMIN1(1.0,0.2+WSTRST) * CALBRT(14)
        FACNYT = NYTTYM *AMIN1(1.0,0.2+WSTRST) * CALBRT(14)
        PDSTMD = (0.1 + 0.02 * KDAY) * FACDAY
        PDSTMN = (0.1 + 0.02 * KDAY) * FACNYT

    ELSE
        !         PDSTMD=(0.2 + 0.06 * (STEMWT-STMWT(KDAY-32)))*DAYTYM*CALBRT(15)
        !         PDSTMN=(0.2 + 0.06 * (STEMWT-STMWT(KDAY-32)))*NYTTYM*CALBRT(15)
        FACDAY = DAYTYM *AMIN1(1.0,0.2+WSTRST) * CALBRT(15)
        FACNYT = NYTTYM *AMIN1(1.0,0.2+WSTRST) * CALBRT(15)
                        !PDSTMD=(0.2 + 0.06 * (STEMWT-STMWT(KDAY-32))) * FACDAY
                        !PDSTMN=(0.2 + 0.06 * (STEMWT-STMWT(KDAY-32))) * FACNYT
        PDSTMD=(0.2 + 0.06 * (STEMWT-STMWT(KDAY-32))) * FACDAY
        PDSTMN=(0.2 + 0.06 * (STEMWT-STMWT(KDAY-32))) * FACNYT
    ENDIF
    
    RETURN
    END

     
    subroutine rootshootratio
    ! ************************************************************
    ! * Calculate correction factors to potential stem, leaf and *
    ! * root growth based on the root/shoot ratio                *
    ! ************************************************************

    use common_block

    REAL NRUT

    ! *** RATIO IS ROOTWT/(STEMWT+LEAFWT) RATIO UNDER NO WATER STRESS
    ! *** CONDITIONS. DATA FOR THE EQUATION BELOW COMES FROM AVI
    ! *** BEN-PORATH PHD THESIS.

    RATIO  = 0.2072+0.606506*EXP(-0.13*(STEMWT+LEAFWT))

    ! *** RATIO (DURING THE DAY AND NIGHT) IS INCREASED AS A FUNCTION OF
    ! *** WATER STRESS. IT COULD BE INCREASED AS MUCH AS 25% IF WSTRS IS 0.

    RATIOD = RATIO * (1.00 + 0.25*(1.0-WSTRSD)) * CALBRT(22)
    RATION = RATIO * (1.00 + 0.25*(1.0-WSTRSN)) * CALBRT(22)

    ! *** RUTSUT IS THE SIMULATED ROOT/SHOOT RATIO.

    RUTSUT = ROOTWT/(STEMWT+LEAFWT)
    DTOP = 1. !DTOP- MOISTURE STRESS ADJUSTMENT FOR POTENTIAL DAY TIME TOP GROWTH
    NTOP = 1. !NTOP -MOISTURE ADJUSTMENT fOR POTENTIAL NIGHT TIME TOP GROWTH
    DRUT = 1. !DROOT- MOISTURE ADJUSTMENT FOR POTENTIAL DAY TIME ROOT GROWTH
    NRUT = 1. !DROOT- MOISTURE ADJUSTMENT FOR POTENTIAL NIGHT TIME ROOT GROWTH

    ! *** IF ROOT/SHOOT (RUTSUT) RATIO IS BELOW THE TARGET (RATIO)
    ! *** INCREASED ROOT GROWTH POTENTIAL AND DECREASE STEM AND
    ! *** LEAF GROWTH POTENTIAL.
 
    IF(RUTSUT.LT.RATIOD) THEN
        DRUT = RATIOD/RUTSUT ! INCREASE
        DTOP = RUTSUT/RATIOD ! DECREASE
        SPDWRD = SPDWRD * DRUT !TOTAL POTENTIAL ROOT GROWTH DURING THE DAY
        PDSTMD = PDSTMD * DTOP !POTENTIAL STEM GROWTH DURING THE DAY
        SPDWLD = SPDWLD * DTOP !POTENTIAL LEAF GROWTH DURING THE DAY
    ENDIF
    IF(RUTSUT.LT.RATION) THEN
        NRUT = RATION/RUTSUT 
        NTOP = RUTSUT/RATION
        SPDWRN = SPDWRN * NRUT
        PDSTMN = PDSTMN * NTOP
        SPDWLN = SPDWLN * NTOP
    ENDIF

    ! *** IF ROOT/SHOOT RATIO IS ABOVE THE TARGET RATIO DECREASE ROOT
    ! *** GROWTH POTENTIAL AND INCREASE STEM AND LEAVES GROWTH POTENTIAL.

    IF(RUTSUT.GT.RATIOD) THEN
        DRUT = RATIOD/RUTSUT !DECREASE
        DTOP = RUTSUT/RATIOD !INCREASE
        SPDWRD = SPDWRD * DRUT !TOTAL POTENTIAL ROOT GROWTH DURING THE DAY
        PDSTMD = PDSTMD * DTOP !POTENTIAL STEM GROWTH DURING THE DAY
        SPDWLD = SPDWLD * DTOP !POTENTIAL LEAF GROWTH DURING THE DAY
    ENDIF
    IF(RUTSUT.GT.RATION) THEN
        NRUT = RATION/RUTSUT 
        NTOP = RUTSUT/RATION
        SPDWRN = SPDWRN * NRUT
        PDSTMN = PDSTMN * NTOP
        SPDWLN = SPDWLN * NTOP
    ENDIF
    
    RETURN
    END


    SUBROUTINE Carbon_stress
    ! ************************************************************
    ! ***         CALCULATE CARBOHYDRATE stress (CSTRES)		   *
    ! ************************************************************

    use common_block

    CSTRES = CPOOL / CD
   
    IF(CSTRES.LE.1.) THEN  !if cpool <cd
        RESC = 0.
    ELSE
        CSTRES = 1.
        RESC = CPOOL - CD !RESC: Reserve carbohydrate
        DUMY11 = 0.3*LEAFWT
        IF(RESC.GT.DUMY11) THEN !Reserve can be upto 30% of leaf weight
            XTRAC  =  XTRAC + RESC - DUMY11 !XTRAC: Extra crbohydrate beyond an increase in 30% leaf weight: this is permenanlty used for the root at the base of the plant
            
            RESC   = DUMY11 !whats reserved for the 30% of leaf weight
            CSTORE = 1.0
        ELSE
            CSTORE=RESC/DUMY11
        ENDIF
    ENDIF

    RETURN
    END

      SUBROUTINE NITRO
    !  ************************************************************
    !  *                                                          *
    !  *                   NITRO SUBROUTINE                       *
    !  *                                                          *
    !  ************************************************************
    !  *  IN THIS SUBROUTINE, THE MAXIMUM AND MINIMUM N           *
    !  *  CONCENTRATIONS FOR THE VARIOUS ORGANS ARE AS REPORTED   *
    !  *  BY JONES ET. AL. (1974) DEVELOPMENT OF A NITROGEN       *
    !  *  BALANCE FOR COTTON GROWTH MODELS: A FIRST APPROXIMATION *
    !  *  CROP SCI. 14:541-546.                                   *
    !  ************************************************************
    !
    use common_block
    real npoolava,npoolreq
    NF = 1.
    SEEDR1 = 0.!SEED NITROGEN REQUIREMENT FOR GROWTH
    BURADD = 0.
    !buradd: burri + burmin
    !burmin: burr nitrogen requirement for growth (minimum)
    !burr: potential nitrogen storage in burrs
    !burri: burr nitrogen requirement for growth
    SEDADD = 0.
    PLANTN = SLEAFN + ROOTN + STEMN + SEEDN + BURRN !Calculate total plant nitrogen [g/plant]
    !plantn: total plant nitrogen
    !sleafn: total leaf nitrogen
 
    NV = 1.!- factor for limiting vegetative growth in response nitrogen stress.
    NR = 1. 
    
    ! F2 = RESERVE NITROGEN AVAILABILITY COEFFICIENT.
    ! THE MIN N CONC. IN LEAVES IS .015, IN STEMS IS .009, IN STEMS & ROOTS
    ! IS .009, IN BURRS IS .006 ACCORD. TO JONES & HESKETH 73
    !Calculate nitrogen reserves for today in each organ from minimum allowable
    !concentration in that class of organ and the amount of N there now. F2 is a
    !rate of mobilization coefficient. Its value. assigned in BLOCK DATA is 0.5.
    
    LEAFRS = (SLEAFN- .015 * LEAFWT) * F2 !Calculate the nitrogen reserve
    STEMRS = (STEMN - .009 * STEMWT) * F2
    ROOTRS = (ROOTN - .009 * ROOTWT) * F2
    
    ! resn is reserve nitrogen. units are g/plant
    ! npool is total nitrogen available for growth.

    RESN =LEAFRS + STEMRS + ROOTRS !g/plant !reserved nitrogen
    IF(RESN.LT.0.) RESN = 0.
    
    !npool = (supno3*0.2258) + (supnh4*0.6364) + resn !from the soil and what is reserved in the plant
    !conversion factor from no3 to n is 0.2258; nh4 to n is 0.6364
    
    !npool1 from soil2d is in [gram/day/slab], it should be converted to [gram/day/plant] 
    !npool=(npool1/popslab)+resn
    NPOOL=(NPOOL1/popslab)+RESN    !npool1/popslab: is the nitrogen supply [g n/day/plant] !no3 to n and nh4 to n is already done in 2dsoil
  
    npoolava=npool 
    !the following represets potential growth requirement
    !calculate the nitrogen required for new growth in each class of vegetative
    !organs and in the total vegetative structure. the coefficients are the
    !minimum n concentration associated with actively growing tissue.

    LEAFR1 = 0.035 * PDLEAF ! leaf nitrogen requirement for growth (PDleaf accounts for carbon stress)
    STEMR1 = 0.020 * PDSTEM ! stem nitrogen requirement for growth
    ROOTR1 = 0.020 * PDROOT ! root nitrogen requirement for growth
    
    NPOOL = NPOOL - ROOTR1  ! nitrogen pool requirment for the stem and leaf= (Nitrogen pool available - root nitrogen requirment)

    IF(NPOOL.LE.0.0) THEN  !if nitrogen pool available for the stem and leaf < or = 0
        NR = (NPOOL+ROOTR1)/ROOTR1 ! total pool of nitrogen/ root nitrogen requirment for growth
        NPOOL = 0.0
    ENDIF

    REQV   = LEAFR1 + STEMR1 !REQV- (NITROGEN) REQUIREMENT fOR VEGETATIVE GROWTH

    !  IF THERE IS A GREEN BOLL ON PLANT, CALCULATE SEED & BURR REQS
    !burmin: burr nitrogen requirement for growth (minimum)
    !burr: potential nitrogen storage in burrs
    !burri: burr nitrogen requirement for growth

    IF(GBOLWT.GT.0.) THEN
        SEEDR1 = PDBOLL * .045 * .416
        BURMIN = PDBOLL * .006 * .278
        BURR1  = PDBOLL * .014 * .278
        BOLL1  = BURMIN + SEEDR1
        REQ1   = BOLL1  + BURR1    !nitrogen required for boll and burr
        BURADD = BURR1  + BURMIN
        SEDADD = SEEDR1
    ENDIF
      
    !multiplying with popslab coverts [g N/pant] to [g N/ slab]
    !multipying with 1.0e-6 convert from [g] to [microg]
    !Nitrodemand (nitrogen) is converted to Nitrate in Solute uptake routine in soil2d
    NitroDemand=(LEAFR1+STEMR1+ROOTR1+REQ1)*popslab*1.0e6  
    
    IF((REQV+REQ1).GT.NPOOL) THEN    ! at this state Npool= the avaialable nitrogen for plant parts other than root
        IF(GBOLWT.GT.0.) THEN
            IF((REQV+REQ1-BURR1).LE.NPOOL) THEN
                BURADD = NPOOL-REQV-REQ1+BURR1
            ELSE
                IF(BOLL1.LE.NPOOL) THEN
                    NV = (NPOOL-BOLL1) / REQV
                    IF(NV.GT.1.) NV = 1.
                    BURADD = BURMIN
                ELSE
                    NV = 0.
                    BURADD = NPOOL*BURMIN / (BURMIN+SEEDR1)
                    SEDADD = NPOOL*SEEDR1 / (BURMIN+SEEDR1)
                    NF = NPOOL/BOLL1
                    IF(NF.GT.1.) NF = 1.
                ENDIF
            ENDIF
        ELSE
            NV = NPOOL / REQV
            IF(NV.GT.1.) NV = 1.
            IF(NV.lT.0.1) NV = 0.1
        ENDIF
    ENDIF
   Nratio=npoolava-REQ1 / REQV+ROOTR1-REQ1  !required N and Available N for all plant parts except ReQ1
   
   ! VEGETATIVE GROWTH SECTION
    SLEAFN = SLEAFN + LEAFR1 * NV   !this is the nitrogen  added to each class of vegetative organ
    STEMN  = STEMN  + STEMR1 * NV
    ROOTN  = ROOTN  + ROOTR1 * NR
    ! BOLL GROWTH SECTION

    BURRN = BURRN + BURADD
    SEEDN = SEEDN + SEDADD
   
    !Total nitrogen to be added to all parts of the plant: PLTN 
    PLTN  = SLEAFN + STEMN + ROOTN + BURRN + SEEDN
    
    XTRAN = (NPOOL1/popslab) - (PLTN - PLANTN)      ! [gram N/plant]
    
    !  xtran= pool of nitrogen (g/plwant/day from 2dsoil) - (what is required by the plant - what is avaialable in the plant)
    !  the part of xtran supplied by resn is (pltn-plantn)-(supno3 + supnh4)
    !  if this  is +, then some came from resn. xtran must be negative
    !  if (pltn-plantn) -(supno3+supnh4) is - , then all came from supno3
    !  supnh4 & xtran must be +.
   
      
      VEGWT  = LEAFWT + STEMWT + ROOTWT  !Calculate the dry weight of the vegetative organs
    
    !inorder to Allocate the excess nitrogen to the various vegetative structures in
    !proportion to their dry weight.
    IF(XTRAN.GT.0.) THEN
        SLEAFN = SLEAFN + XTRAN * (LEAFWT/VEGWT)
        STEMN  = STEMN  + XTRAN * (STEMWT/VEGWT)
        ROOTN  = ROOTN  + XTRAN * (ROOTWT/VEGWT)
    ELSE
        IF(RESN.LE.0.) THEN
            XTRAN = 0.0
        ELSE
            SLEAFN = SLEAFN + XTRAN * (LEAFRS/RESN)
            STEMN  = STEMN  + XTRAN * (STEMRS/RESN)
            ROOTN  = ROOTN  + XTRAN * (ROOTRS/RESN)
        ENDIF
    ENDIF
    XTRAN = 0.
    STEMCN = STEMN / STEMWT

    IF(LEAFWT.GT.0.0) THEN
        LEAFCN = SLEAFN / LEAFWT        !SLEAFN: TOTAL LEAF NITROGEN
        ROOTCN = ROOTN / ROOTWT         !ROOTN: TOTAL ROOT NITROGEN
        IF(GBOLWT.GT.0.) THEN
            XXWT = COTXX + GBOLWT
            SEEDCN = SEEDN / (XXWT*.416)
            BURCN  = BURRN / (XXWT*.278)
        ENDIF
        TNO3UP = TNO3UP + SUPNO3
        TNH4UP = TNH4UP + SUPNH4
    ENDIF
    RETURN
    END

    
       SUBROUTINE Actual_square_growth(k,l,m)
    ! ************************************************************
    ! ***             CALCULATE ACTUAL SQUARE GROWTH	    	   *
    ! ************************************************************

    use common_block

    IF(FCODE(K,L,M).EQ.1) THEN
        DWSQ = PDWSQ(K,L,M)*FSTRES
        SQRWT(K,L,M) = SQRWT(K,L,M) + DWSQ
        SDWSQR = SDWSQR + DWSQ
        SQWT = SQWT + SQRWT(K,L,M)
    ENDIF
    RETURN
    END


    SUBROUTINE Actual_boll_growth(k,l,m)
    ! ************************************************************
    ! ***             CALCULATE ACTUAL BOLL GROWTH	    	   *
    ! ************************************************************

    use common_block

    IF(FCODE(K,L,M).EQ.2.OR.FCODE(K,L,M).EQ.7) THEN
        DWBOLL = (PDWBOD(K,L,M)+PDWBON(K,L,M))*FSTRES
        BOLWGT(K,L,M) = BOLWGT(K,L,M) + DWBOLL
        SDWBOL = SDWBOL + DWBOLL
        GBOLWT = GBOLWT + BOLWGT(K,L,M)
    ENDIF
    RETURN
    END


    SUBROUTINE Actual_leaf_growth(k,l,m)
    ! ************************************************************
    ! ***             CALCULATE ACTUAL LEAF GROWTH	    	   *
    ! ************************************************************

    use common_block

    ! *** Calculate actual main stem leaf growth

    IF(M.EQ.1) THEN
        DWL = (PDWMLD(K,L)*DTOP+PDWMLN(K,L)*NTOP)*VSTRES
        SDWLEF = SDWLEF + DWL
        MLEAFW(K,L) = MLEAFW(K,L) + DWL
        LEAFWT = LEAFWT + MLEAFW(K,L)
    ENDIF

    ! *** Calculate actual fruiting branches leaf growth

    DWL  = (PDWFLD(K,L,M)*DTOP+PDWFLN(K,L,M)*NTOP)*VSTRES
    SDWLEF = SDWLEF + DWL
    LEAFW(K,L,M) = LEAFW(K,L,M) + DWL
    LEAFWT = LEAFWT + LEAFW(K,L,M)
    RETURN
    END


    SUBROUTINE Actual_leaf_area(k,l,m)
    ! ************************************************************
    ! ***             CALCULATE ACTUAL LEAF AREA  	    	   *
    ! ************************************************************

    use common_block

    ! *** Calculate actual main stem leaf area

    IF(M.EQ.1) THEN
        DAL = (PDAMLD(K,L)*DTOP+PDAMLN(K,L)*NTOP)*VSTRES
        MLAREA(K,L) = MLAREA(K,L) + DAL
        AREA = AREA + MLAREA(K,L)
    ENDIF

    ! *** Calculate actual fruiting branches leaf area

    DAL = (PDADAY(K,L,M)*DTOP+PDANYT(K,L,M)*NTOP)*VSTRES
    LAREA(K,L,M) = LAREA(K,L,M) + DAL
    AREA = AREA + LAREA(K,L,M)
    RETURN
    END


    SUBROUTINE Actual_prefruitingleaf_growth(j)
    ! ************************************************************
    ! ***          CALCULATE PREFRUITING LEAF GROWTH	    	   *
    ! ************************************************************

    use common_block

    PFDWL = (PFDWLD(J)*DTOP + PFDWLN(J)*NTOP) * VSTRES
    PFWL(J) = PFWL(J) + PFDWL
    SDWLEF = SDWLEF + PFDWL
    LEAFWT = LEAFWT + PFWL(J)
    RETURN
    END


    SUBROUTINE Actual_prefruitingleaf_area(j)
    ! ************************************************************
    ! ***          CALCULATE PREFRUITING LEAF AREA	    	   *
    ! ************************************************************

    use common_block

    PFDAL = (PFDALD(J)*DTOP + PFDALN(J)*NTOP) * VSTRES
    PFAL(J) = PFAL(J) + PFDAL
    PFAREA = PFAREA + PFAL(J)
    RETURN
    END


    SUBROUTINE PLANTHEIGHT
    ! ************************************************************
    ! *   Based on Reddy, et. al., 1997.  Modeling temperature   *
    ! * effects on cotton internode and leaf growth.  Crop Sci.  *
    ! *                                            kit 5/11/99   *
    ! ************************************************************

    use common_block

    DZ = 0.
    if(tavg.lt.17.0) tavg = 17.0
    if(tavg.gt.36.0) tavg = 36.0

    !*** rsquare = 0.96 for internode elongation duration equation

    DURATION = -0.04312+0.007383*TAVG-0.0001046*TAVG**2
    IF(DURATION.LT.0.) DURATION = 0.

    ! *** Calculate reduction to internode length due to defoliants

    IF((PRPDAY.GT.0).AND.(DAYNUM.GT.PRPDAY)) THEN
        DUM = DAYNUM-PRPDAY
        ! IF((PRPDAY.GT.0).AND.(JDAY.GT.PRPDAY)) THEN
        !DUM = JDAY-PRPDAY
        DZCON = .07151+.008617*TAVG-.01844*DUM+.001176*TAVG*DUM
        DZPREP = -.4685+.04319*TAVG-.005356*DUM+.3196*			&
            PRPKGH-.1063*PRPKGH**2-.0006625*TAVG**2
        DZPREP = DZPREP/DZCON
        IF(DZPREP.GT.1.5) DZPREP=1.5
        IF(DZPREP.LT..001) DZPREP=.001
    ELSE
        DZPREP = 1.
    ENDIF

    ! *** Reduction in plant height due to water stress from SPAR data
    ! *** collected by K. R. Reddy in 1991; 4 cm is the unstressed height
    ! *** rsquare = 0.77 for the equation


    if(PSIL_G.le.-2.5) PSIL_G = -2.5
       H2OIndex = (4.2904 - PSIL_G*(0.6491+0.9737*PSIL_G))/4.
    IF(H2OIndex.LT.0.15) H2OIndex = 0.15
    IF(H2OIndex.GT.1.0) H2OIndex = 1.0
   
    N00 = NUMPFN + NFBR(1)
    !NOO= NUM OF PREFRUTING NODES+number of nodes in the frist vegitative branch
    !NFBR - (K) - NUMBER OF fRUITING BRANCHES ON THE VEGETATIVE BRANCH
    DO I=1,N00
        XMNODAGE(I) = XMNODAGE(I)+DURATION * CALBRT(44)
        IF(XMNODAGE(I).LT.1.0) THEN
            xnodage(i) = xnodage(i) + 1.0
            cntlfcn(i) = cntlfcn(i) + 1.0
            sumlfcn(i) = sumlfcn(i) + leafcn

            ! *** Calculate initial internode length

            IF(XMNODLTH(I).LT.0.0001) THEN

                ! *** Calculate reduction to initial internode length due to temperature
                ! *** Assume that maximum initial internode length at 27oC = 0.75 cm.

                rednodlth = (-0.6853 + tavg*(0.1077-0.002031*tavg))/0.75

                ! *** rsquare = 0.11 for above equation

                if (rednodlth.lt.0.001) rednodlth = 0.001
                if (rednodlth.gt.1.0) rednodlth = 1.0
                IF(i.lt.14) THEN
                    XMNODLTH(I)=(0.05738+0.05605*I)*rednodlth*CALBRT(47)

                    ! *** rsquare = 0.93 for above equation

                    IF(XMNODLTH(I).LT.0.1) XMNODLTH(I) = 0.1
                ELSE
                    XMNODLTH(I)=(1.3589-0.0407*I)*rednodlth * CALBRT(48)

                    ! *** rsquare = 0.91 for above equation

                    IF(XMNODLTH(I).LT.0.3) XMNODLTH(I) = 0.3
                ENDIF

                ! *** Calculate reduction to initial internode length due to nitrogen

                if(i.gt.1) then
                    avelfcn = sumlfcn(i-1) / cntlfcn(i-1)
                    faclfn = avelfcn / 0.035
                    if(faclfn.gt.1.0) faclfn = 1.0
                    redlfcn = -2.9953 + faclfn*(8.0622-4.0669*faclfn)
                    if(redlfcn.lt.0.2) redlfcn = 0.2
                    if(redlfcn.gt.1.0) redlfcn = 1.0
                else
                    redlfcn = 1.0
                endif
                stress_index = H2OIndex * redlfcn * pixdz * dzprep
                if(stress_index.lt.0.15) stress_index = 0.15
                if(stress_index.GT.1.) stress_index = 1.0
                DZ = DZ+XMNODLTH(I) * stress_index  * CALBRT(45)
            ELSE
                faclfn = leafcn/0.035   !LEAF NITROGEN CONCENTRATION 
                if(faclfn.gt.1.0) faclfn = 1.0
                redlfcn = -2.9953 + faclfn*(8.0622-4.0669*faclfn)
                if(redlfcn.lt.0.2) redlfcn = 0.2
                if(redlfcn.gt.1.0) redlfcn = 1.0
                stress_index = H2OIndex * redlfcn * pixdz * dzprep
                if(stress_index.lt.0.15) stress_index = 0.15
                if(stress_index.GT.1.) stress_index = 1.0

                ! *** Calculate the daily increment of internode length elongation
                ! *** Calculate the relative rate of elongation (yintrcpt)
                ! *** and rate of reduction (slope).      rsquare = 0.97

                YINTRCPT = -0.001427 + 0.0166*TAVG
                SLOPE = 0.02479 - 0.001994*TAVG

                ! *** Calculate the current rate of internode length elongation

                Stem_Elongation_Rate = YINTRCPT + SLOPE*XNODAGE(I)
                if(Stem_Elongation_Rate.le.0.0)Stem_Elongation_Rate = 0.0

                ! *** Calculate the current internode length

                Potential_Height = Stem_Elongation_Rate * XMNODLTH(I)

                GROWFAC = Potential_Height * stress_index * CALBRT(49)
                IF(GROWFAC.gt.0.6) GROWFAC = 0.6
                DZ = DZ + GROWFAC
                XMNODLTH(I) = XMNODLTH(I) + GROWFAC
            ENDIF
        ENDIF
         
    ENDDO

    Z = Z + DZ
    RETURN
    END

    
    FUNCTION TempFact_leaf(tmp)
    ! ************************************************************
    ! *** THIS FUNCTION Calculate the temperature reduction 	   *
    ! *** factor (assume max area = 12.65 sq cm at 27oC)		   *
    ! ************************************************************

    if(tmp.lt.17.0) tmp = 17.0
    if(tmp.gt.36.0) tmp = 36.0
    tempfact_leaf = (-18.599 + tmp*(2.186-0.0381*tmp))/12.65
    if(tempfact_leaf.lt.0.01) tempfact_leaf = 0.01
    if(tempfact_leaf.gt.1.0) tempfact_leaf = 1.0
    RETURN
    END


    FUNCTION Duration_leaf_expansion(tmp)
    ! ************************************************************
    ! *** THIS FUNCTION Calculates the daily increment of leaf   *
    ! *** expansion duration			rsquare = 0.95  		   *
    ! ************************************************************

    if(tmp.lt.17.0) tmp = 17.0
    if(tmp.gt.36.0) tmp = 36.0
    Duration_leaf_expansion = -0.09365+tmp*(0.01070-0.0001697*tmp)
    if(Duration_leaf_expansion.lt.0.0) Duration_leaf_expansion = 0.0
    if(Duration_leaf_expansion.gt.1.0) Duration_leaf_expansion = 1.0
    RETURN
    END


    FUNCTION Rate_Leaf_expansion(tmp)
    ! ************************************************************
    ! *** THIS FUNCTION Calculate the daily increment of leaf    *
    ! *** area expansion (RLER)	    	rsquare = 0.95 		   *
    ! ************************************************************

    if(tmp.lt.17.0) tmp = 17.0
    if(tmp.gt.36.0) tmp = 36.0
    Rate_Leaf_expansion = -0.03390 + 0.02041*tmp
    RETURN
    END


    FUNCTION Rate_of_reduction(tmp)
    ! ************************************************************
    ! *** THIS FUNCTION Calculate the daily rate of reduction    *
    ! *** of leaf area expansion (ror)      	rsquare = 0.98	   *
    ! ************************************************************

    if(tmp.lt.17.0) tmp = 17.0
    if(tmp.gt.36.0) tmp = 36.0
    Rate_of_reduction = 0.01341 - 0.001879*tmp
    RETURN
    END


    FUNCTION Sp_leaf_weight(tmp)
    ! ************************************************************
    ! *               CALCULATE Specific leaf weight             *
    ! ************************************************************

    if(tmp.lt.17.0) tmp = 17.0
    if(tmp.gt.36.0) tmp = 36.0
    Sp_leaf_weight = 0.509 + 313.122 * exp(-0.377 * tmp)

    ! *** Original GOSSYM function

    !      if(tmp.le.13.5) tmp = 13.5
    !      Sp_leaf_weight = 1.0/(-0.62142855 + tmp*(0.1093651
    !     .                 - tmp*0.00137566))
    RETURN
    END


    SUBROUTINE  Potential_PreFruiting_Leaf_Area(i)
    ! ************************************************************
    ! * CALCULATE potential prefruiting leaf area; rsquare =0.91 *
    ! ************************************************************

    use common_block

    if(tavg.lt.17.0) tavg = 17.0
    if(tavg.gt.36.0) tavg = 36.0
    PfLfArea(i) = (6.061+1.8069*i) * TempFact_leaf(tavg)/100.0
    if(PfLfArea(i).LT.0.045) PfLfArea(i) = 0.045
    RETURN
    END


    SUBROUTINE  Potential_MainStem_Leaf_Area(k,l)
    ! ************************************************************
    ! *  CALCULATE potential main stem leaf area; rsquare = 0.95 *
    ! ************************************************************

    use common_block

    if(tavg.lt.17.0) tavg = 17.0
    if(tavg.gt.36.0) tavg = 36.0
    XMSLfArea(k,l) = (18.3812-0.523*l) * TempFact_leaf(tavg)/100.0
    if(XMSLfArea(k,l).LT.0.03) XMSLfArea(k,l) = 0.03
    RETURN
    END


    SUBROUTINE  Potential_Fruiting_Branch_Leaf_Area(k,l,m)
    ! ************************************************************
    ! * CALCULATE potential fruiting branch leaf area; r2 = 0.98 *
    ! ************************************************************

    use common_block

    if(tavg.lt.17.0) tavg = 17.0
    if(tavg.gt.36.0) tavg = 36.0
    FRLfArea(k,l,m) = (13.457-1.179*m) * TempFact_leaf(tavg)/100.0
    if(FRLfArea(k,l,m).LT.0.045)FRLfArea(k,l,m) = 0.045
    RETURN
    END


    FUNCTION CNReduction_to_Leaf_Area(nodenumber,avelfcn)
    ! ************************************************************
    ! *** THIS FUNCTION Calculate the N-reduction to leaf area   *
    ! ************************************************************

    faclfn = avelfcn / 0.035
    if(faclfn.gt.1.0) faclfn = 1.0
    redlfcn = -3.5288 + faclfn * (8.0488-3.5200*faclfn)
    if(redlfcn.lt.0.2) redlfcn = 0.2
    if(redlfcn.gt.1.0) redlfcn = 1.0
    if(nodenumber.eq.1) then
        CNReduction_to_Leaf_Area = 1.0
    else
        CNReduction_to_Leaf_Area = redlfcn
    endif
    RETURN
    END

    
    subroutine rtswrf(l,k,efac1,efacl,efacr,efacd,srwp)

    ! *****************************************************************
    ! *** calculate the combined effect of soil impedance and moisture
    ! *** content on root growth
    ! *****************************************************************

    use common_block

    STR1 = (104.6 - 3.53*RTIMPD(L,Kmod)/1.0216)*.01
    IF(STR1.GT.1.)  STR1 = 1.
    IF(STR1.LT.0.)  STR1 = 0.
    STRL = (104.6 - 3.53*RTIMPD(L,KL1)/1.0216)*.01
    IF(STRL.GT.1.)  STRL = 1.
    IF(STRL.LT.0.)  STRL = 0.
    STRR = (104.6 - 3.53*RTIMPD(L,KR1)/1.0216)*.01
    IF(STRR.GT.1.)  STRR = 1.
    IF(STRR.LT.0.)  STRR = 0.
    STRD = (104.6 - 3.53*RTIMPD(LD1,Kmod)/1.0216)*.01
    IF(STRD.GT.1.)  STRD = 1.
    IF(STRD.LT.0.)  STRD = 0.

    SWFAC = (1./PSIS(L,Kmod)**3) + (ILC/PSIS(L,KL1)**3) +		 &
        (IRC/PSIS(L,KR1)**3) + (LDC/PSIS(LD1,Kmod)**3)
    WEFAC1 = (1.0/PSIS(L,Kmod)  **3)/SWFAC
    WEFAC1 = (1.0/PSIS(L,K)  **3)/SWFAC
    WEFACL = (ILC/PSIS(L,KL1)**3)/SWFAC
    WEFACR = (IRC/PSIS(L,KR1)**3)/SWFAC
    WEFACD = (LDC/PSIS(LD1,Kmod)**3)/SWFAC
    SRIMPD = STR1 + STRL + STRR + STRD
    IF(SRIMPD.GT.0.0) THEN
        RIMP1 = STR1/SRIMPD
        RIMPL = STRL/SRIMPD
        RIMPR = STRR/SRIMPD
        RIMPD = STRD/SRIMPD
    ELSE
        RIMP1 = 1.
        RIMPL = 1.
        RIMPR = 1.
        RIMPD = 1.
    ENDIF

    EFAC1 = WEFAC1 * RIMP1
    EFACL = WEFACL * RIMPL
    EFACR = WEFACR * RIMPR
    EFACD = WEFACD * RIMPD
    SRWP = EFAC1 + EFACL + EFACR + EFACD
    return
    end
    
    
        SUBROUTINE RIMPED
    ! ********************************************************************
    ! *    THIS SUBROUTINE CALCULATES ROOT IMPEDENCE BASED UPON THE BULK *
    ! * DENSITY AND WATER CONTENT. THIS IS BASED UPON DATA FROM ARTICLES *
    ! * BY R.B. CAMPBELL, D.C.REICOSKY AND C.W.DOTY J.OF SOIL AND WATER  *
    ! * CONS. 29:220-224,1974 AND H.M.TAYLOR AND H.R.GARDNER. SOIL SCI.  *
    ! * SOIL SCI.96:153-156,1963.                                        *
    ! *    A LINEAR TABLE LOOK-UP PROCEDURE IS USED. ASSUME ALL          *
    ! * CURVES ARE READ AT THE SAME BD.                                  *
    ! ********************************************************************

    use common_block

    DO 99 L = 1,NL
        J = LYRDPH(L)
        JJ = 1
26      IF(BD(J).GT.TSTBD(1,JJ)) THEN
            JJ = JJ+1
            IF(JJ.LT.INRIM) GO TO 26
        ENDIF
        IF(JJ.GT.INRIM) JJ=INRIM
        DO 98 K = 1,NK
            TEST1=VH2OC(L,K)/BD(J)
            IK = 1
32          IF(TEST1.GT.GH2OC(IK)) THEN
                IK = IK+1
                IF(IK.LT.NCURVE) GO TO 32
            ENDIF

            ! *** SOIL CELL H2O LESS THAN TEST H2O

            IF(JJ.EQ.1) THEN
                IF((IK.EQ.1).OR.(TEST1.EQ.GH2OC(IK))) THEN
                    RTIMPD(L,K)=TSTIMP(IK,JJ)
                    GO TO 98
                ENDIF
            ELSE
                IF((IK.EQ.1).OR.(TEST1.EQ.GH2OC(IK))) THEN
                    RTIMPD(L,K)=TSTIMP(IK,JJ-1)-(TSTIMP(IK,JJ-1)-		   &
                        TSTIMP(IK,JJ))*((TSTBD(IK,JJ-1)-BD(J))/  &
                        (TSTBD(IK,JJ-1)-TSTBD(IK,JJ)))
                    GO TO 98
                ENDIF
            ENDIF

            ! *** CALCULATE SOIL STRENGTH FOR VALUES OF BD LESS THAN TABLE VALUES

            IF(JJ.EQ.1) THEN
                RTIMPD(L,K)=TSTIMP(IK-1,JJ)-(TSTIMP(IK-1,JJ) 		   &
                    -TSTIMP(IK,JJ))*((TEST1-GH2OC(IK-1))		   &
                    /(GH2OC(IK)-GH2OC(IK-1)))
            ELSE

                ! *** FOR VALUES OF BD AND H2O BETWEEN TABLE VALUES

                TEMP1R=TSTIMP(IK,JJ-1)-(TSTIMP(IK,JJ-1)-TSTIMP(IK,JJ))*		&
                    ((TSTBD(IK,JJ-1)-BD(J))/(TSTBD(IK,JJ-1)-TSTBD(IK,JJ)))
                TEMP2=TSTIMP(IK-1,JJ-1)-(TSTIMP(IK-1,JJ-1)-TSTIMP(IK-1,JJ))*	&
                    ((TSTBD(IK-1,JJ-1)-BD(J))/(TSTBD(IK-1,JJ-1)-TSTBD(IK-1,JJ)))
                RTIMPD(L,K)=TEMP2+(TEMP1R-TEMP2)*((TEST1-GH2OC(IK-1))/		&
                    (GH2OC(IK)-GH2OC(IK-1)))
            ENDIF
98      CONTINUE
99  CONTINUE
    JJ = 1
126 IF(BDI.GT.TSTBD(1,JJ)) THEN
        JJ = JJ + 1
        IF(JJ.LT.INRIM) GO TO 126
    ENDIF
    IF(JJ.GT.INRIM) JJ=INRIM
    DO 198 K=1,3
        TEST1 = VH2OC(1,K)/BDI
        IK = 1
132     IF(TEST1.GT.GH2OC(IK)) THEN
            IK = IK + 1
            IF(IK.LT.NCURVE) GO TO 132
        ENDIF
        IF(IK.GT.NCURVE) IK=NCURVE
        IF(JJ.LE.1) THEN
            IF((IK.EQ.1).OR.(TEST1.EQ.GH2OC(IK))) THEN
                RTIMPD(1,K) = TSTIMP(IK,JJ)
                GO TO 198
            ENDIF
        ELSE
            IF((IK.EQ.1).OR.(TEST1.EQ.GH2OC(IK))) THEN
                RTIMPD(1,K) = TSTIMP(IK,JJ-1) - (TSTIMP(IK,JJ-1)-	  &
                    TSTIMP(IK,JJ)) * ((TSTBD(IK,JJ-1)-BDI)/	      &
                    (TSTBD(IK,JJ-1)-TSTBD(IK,JJ)))
                GO TO 198
            ENDIF
        ENDIF
        IF(JJ.LE.1) THEN
            RTIMPD(1,K) = TSTIMP(IK-1,JJ) - (TSTIMP(IK-1,JJ)-			  &
                TSTIMP(IK,JJ))*((TEST1-GH2OC(IK-1))/(GH2OC(IK)-GH2OC(IK-1)))
        ELSE
            TEMP1R = TSTIMP(IK,JJ-1) - (TSTIMP(IK,JJ-1)-TSTIMP(IK,JJ)) *	 &
                ((TSTBD(IK,JJ-1)-BDI)/(TSTBD(IK,JJ-1)-TSTBD(IK,JJ)))
            TEMP2 = TSTIMP(IK-1,JJ-1)-(TSTIMP(IK-1,JJ-1)-TSTIMP(IK-1,JJ))*	 &
                ((TSTBD(IK-1,JJ-1)-BDI)/(TSTBD(IK-1,JJ-1)-TSTBD(IK-1,JJ)))
            RTIMPD(1,K) = TEMP2 + (TEMP1R-TEMP2) * ((TEST1-GH2OC(IK-1))/	 &
                (GH2OC(IK)-GH2OC(IK-1)))
        ENDIF
198 CONTINUE
    RETURN
    END
    
    
    SUBROUTINE SORTIT(N,SORT,INDX)

    ! *** SORTS THE 2-D ARRAY SORT IN DESCENDING ORDER BY THE 3RD COL
    ! *** ALGORITHM FOR THE HEAP SORT USED IS IN NUMERICAL RECIPES,
    ! *** BY PRESS, FLANNERY, TEUKOLSKY, VETTERLING,  PP. 231

    REAL SORT(800,3)
    INTEGER INDX(800)

    L = N/2+1
    IR = N
10  CONTINUE
    IF (L .GT. 1) THEN
        L=L-1
        SORT3 = SORT(L,3)
        SORT2 = SORT(L,2)
        SORT1 = SORT(L,1)
        INDX1 = INDX(L)
    ELSE
        SORT3 = SORT(IR,3)
        SORT2 = SORT(IR,2)
        SORT1 = SORT(IR,1)
        INDX1 = INDX(IR)
        SORT(IR,3) = SORT(1,3)
        SORT(IR,2) = SORT(1,2)
        SORT(IR,1) = SORT(1,1)
        INDX(IR) = INDX(1)
        IR = IR-1
        IF (IR .EQ. 1) THEN
            SORT(IR,3) = SORT3
            SORT(IR,2) = SORT2
            SORT(IR,1) = SORT1
            INDX(IR) = INDX1
            RETURN
        ENDIF
    ENDIF
    I = L
    J = L + L
20  IF (J .LE. IR) THEN
        IF (J .LT. IR) THEN
            IF (SORT(J,3) .GT. SORT(J+1,3)) J = J + 1
        ENDIF
        IF (SORT3 .GT. SORT(J,3)) THEN
            SORT(I,3) = SORT(J,3)
            SORT(I,2) = SORT(J,2)
            SORT(I,1) = SORT(J,1)
            INDX(I) = INDX(J)
            I = J
            J = J + J
        ELSE
            J = IR + 1
        ENDIF
        GO TO 20
    ENDIF
    SORT(I,3) = SORT3
    SORT(I,2) = SORT2
    SORT(I,1) = SORT1
    INDX(I) = INDX1

    GO TO 10

    END


    subroutine N_and_C_Delay
    ! ****************************************************************
    ! *** Calculate morphogenetic delays due to N and C stresses   ***
    ! *** Well watered experiments (AAA), Bruce and Romkens (1965) ***
    ! ****************************************************************

    use common_block

    NDLAY = CALBRT(19) * (1.0 - NV)
    IF (NDLAY.GT.1.0) NDLAY = 1.0
    IF (NDLAY.LT.0.0) NDLAY = 0.0
    CDLAYF =  CALBRT(1) +FSTRES *(CALBRT(2) +CALBRT(3) *FSTRES) !DELAY IN FRUITING NODE FORMATION DUE TO CHO STRESS
    IF (CDLAYF.GT.1.0) CDLAYF = 1.0
    IF (CDLAYF.LT.0.0) CDLAYF = 0.0
    CDLAYV = 1.0 +FSTRES * CALBRT(4) + FSTRES**2 * CALBRT(5)!CARBOHYDRATE DELAYS IN VEGETATIVE BRANCHES
    IF (CDLAYV.GT.1.0) CDLAYV = 1.0
    IF (CDLAYV.LT.0.0) CDLAYV = 0.0
     RETURN
    END


    subroutine Time_to_First_Square
    ! **************************************************************
    ! *** Calculate the time from emergence to first square, TSQ ***
    ! *** THE NEW TSQ EQN IS FROM REDDY'S 1988 SPAR EXPERIMENT.  ***
    ! **************************************************************

    use common_block

    IF(AVTEMP.LT.17.0) AVTEMP = 17.0
    IF((VARITY(IVARTY).EQ.'PIMA').OR.(VARITY(IVARTY).EQ.'pima')) THEN
        IF(AVTEMP.GT.32.0) AVTEMP = 32.0
        TSQ = (247.722-16.484*AVTEMP+0.306*AVTEMP**2)*CALBRT(29)
    ELSE
        IF(AVTEMP.GT.35.0) AVTEMP = 35.0
        TSQ = (190.338-11.372*AVTEMP+0.194*AVTEMP**2)*CALBRT(29) !TSQ: TIme of first square
    ENDIF
    RETURN
    END


    subroutine PreFruiting_Node_Time_Interval
    ! ************************************************************
    ! *** Calculate the time interval between prefruiting node ***
    ! ************************************************************

    use common_block

    AT = AVTPFN(NUMPFN)
    IF(AT.lT.17.0) AT = 17.0
    IF((VARITY(IVARTY).EQ.'PIMA').OR.(VARITY(IVARTY).EQ.'pima')) THEN
        IF(AT.GT.33.0) AT = 33.0
        pfti = (57.993 - 3.5871*AT+0.058894*AT**2)*CALBRT(26)
    ELSE
        IF(AT.GT.32.0) AT = 32.
        pfti = (41.205-2.67219*AT+0.0459705*AT**2)*CALBRT(26)
    ENDIF
    RETURN
    END


    subroutine Vegetative_Branch_Time_Interval
    ! ************************************************************
    ! *** Calculate the time interval between prefruiting node ***
    ! ************************************************************

    use common_block

    AT = (AVGT(NVBRCH,1,1)*AGE(NVBRCH,1,1)+TAVG)/(AGE(NVBRCH,1,1)+1)
    IF(AT.LT.17.0) AT = 17.0
    IF((VARITY(IVARTY).EQ.'PIMA').OR.(VARITY(IVARTY).EQ.'pima')) THEN
        IF(AT.GT.37.0) AT = 37.0
        vbti = 17.365 - 0.87003*AT + 0.012265*AT**2
    ELSE
        IF(AT.GT.32.0) AT = 32.0
        vbti = 41.205 - 2.67219*AT + 0.0459705*AT**2
    ENDIF
    RETURN
    END


    SUBROUTINE Add_A_Vegetative_Branch
    ! *****************************************************************
    ! *** Add other vegetative branches mimicking that of main stem ***
    ! *****************************************************************

    use common_block

    NVBRCH = NVBRCH + 1
    FFRUT(NVBRCH,1,1) = 1.
    FCODE(NVBRCH,1,1) = 1
    IF(PINHED.GE.1.) THEN
        PINHED = PINHED - 1.
        MCODE(NVBRCH,1,1) = 6
    ENDIF

    ! *** Calculate the age, weight and fruiting branch leaf area at unfolding

    FRLfAge(NVBRCH,1,1) = FRLfAge(NVBRCH,1,1)						  &
        + Duration_leaf_expansion(tavg)*calbrt(40)
    if(FRLfAge(NVBRCH,1,1).lt.1.0) 								  &
        agefrlf(NVBRCH,1,1) = agefrlf(NVBRCH,1,1) + 1.0

    ! *** Assume that at node initiation, the leaf area is negligible
    !	Larea(NVBRCH,1,1) = 0.04
    !	LA_WT_CF=(Sp_leaf_weight(tday)+Sp_leaf_weight(tnyt))*CALBRT(36)/2.0
    !	LEAFW(NVBRCH,1,1) = 0.04 * Lf_WT_CF
    ! *** The above 3 statements needs further refinements

    LEAFW(NVBRCH,1,1) = 0.001
    STEMWT = STEMWT -  LEAFW(NVBRCH,1,1)
    SLEAFN = SLEAFN + (LEAFW(NVBRCH,1,1) * 0.035)
    STEMN  = STEMN  - (LEAFW(NVBRCH,1,1) * 0.035)

    ! *** Calculate the age, weight and leaf area on main stem node at unfolding

    XMSLfAge(NVBRCH,1) = XMSLfAge(NVBRCH,1) 						   &
        + Duration_leaf_expansion(tavg)*calbrt(40)
    if(XMSLfAge(NVBRCH,1).lt.1.0) 								   &
        agemslf(NVBRCH,1) = agemslf(NVBRCH,1) + 1.0

    ! *** Assume that at node initiation, the leaf area is negligible
    !	MLarea(NVBRCH,1) = 0.04
    !	LA_WT_CF=(Sp_leaf_weight(tday)+Sp_leaf_weight(tnyt))*CALBRT(36)/2.0
    !	MLEAFW(NVBRCH,1) = 0.04 * Lf_WT_CF
    ! *** The above 3 statements needs further refinements

    MLEAFW(NVBRCH,1) = 0.001
    STEMWT = STEMWT -  MLEAFW(NVBRCH,1)
    SLEAFN = SLEAFN + (MLEAFW(NVBRCH,1) * 0.035)
    STEMN  = STEMN  - (MLEAFW(NVBRCH,1) * 0.035)
    NFBR(NVBRCH)     = 1
    NNOD(NVBRCH,1)   = 1
    LEFCNT=LEFCNT+1
    LEFSRT(LEFCNT)=NVBRCH*1000+10+1

    RETURN
    END


    subroutine Main_Stem_Node_Time_Interval(k,nbrch)
    ! ************************************************************
    ! *** Calculate the time interval between prefruiting node ***
    ! ************************************************************

    use common_block

    ! *** THE DATA FOR THIS ESTIMATE OF THE EFFECT OF TEMPERATURE ON TIME
    ! *** BETWEEN NODES ON A FRUITING BRANCH ARE PUBLISHED IN HESKETH, J.D.,
    ! *** D.N. BAKER, AND W.G. DUNCAN (1972) THE SIMULATION OF GROWTH AND
    ! *** YIELD IN COTTON II. ENVIRONMENTAL CONTROL OF MORPHOGENESIS. CROP
    ! *** SCI. 12:436-439 - EXCEPT FOR THE FACTOR .51. THIS FACTOR IS
    ! *** REQUIRED TO MATCH THE PHYTOTRON GROWTH RATES TO THE FIELD DATA OF
    ! *** BRUCE, R.R. AND ROMKENS. (1965) FRUITING AND GROWTH IN RELATION
    ! *** TO SOIL MOISTURE TENSION.AGRON. J. 57:135-140.

    AT = (AVGT(K,NBRCH,1)*AGE(K,NBRCH,1)+TAVG)/(AGE(K,NBRCH,1)+1)
    IF(AT.LT.17.0) AT=17.0
    IF(K.EQ.1) THEN
        IF(AT.GT.35.0) AT=35.0
        xmsti = (35.919 - 2.252*AT + 0.0375*AT**2)*CALBRT(28)
    ELSE
        IF(AT.GT.32.) AT=32.
        xmsti = (61.416 - 3.833*AT + 0.0652*AT**2)*CALBRT(27)
    ENDIF

    ! *** THE ABOVE EQUATIONS PROVIDES THE SAME MORPHOGENETIC RATE ON
    ! *** SECONDARY VEGETATIVE BRANCHES AS ON FRUITING BRANCHES.

    RETURN
    END


    subroutine Fruiting_Branch_Node_Time_Interval(k,l,nnid)
    ! ****************************************************************
    ! *** Calculate the time interval between fruiting branch node ***
    ! ****************************************************************

    use common_block

    AT = (AVGT(K,L,NNID)*AGE(K,L,NNID)+TAVG)/(AGE(K,L,NNID)+1)
    IF(AT.LT.17.) AT=17.
    IF(AT.GT.32.) AT=32.
    fbnti = (61.416 - 3.833*AT + 0.0652*AT**2)*CALBRT(27)
    RETURN
    END


    subroutine Boll_Temperature(k,l,m)
    ! **************************************************************
    ! *** Calculate boll temperature                             ***
    ! **************************************************************

    use common_block

    IF(BOLWGT(K,L,M).GT.0.0) THEN
        DUM=TAVG
        !         DUM=TAVG + Canopy_Temp_Differential(psild)
        IF((LAI.LE.CALBRT(37)).AND.(KDAY.GT.100)) 			   &
            DUM = DUM * (1.129-0.043*LAI)
        BOLTMP(K,L,M) = (BOLTMP(K,L,M) * AGEBOL(K,L,M)+DUM)/  &
            (AGEBOL(K,L,M)+1)
        AGEBOL(K,L,M) = AGEBOL(K,L,M) + 1.0
    ENDIF
    RETURN
    END


    FUNCTION Canopy_Temp_Differential(psil_)
    ! ************************************************************
    ! *** Calculates the canopy temperature differential as a    *
    ! *** function of midday leaf water potential  KR Reddy 1997 *
    ! ***		                  rsquare = 0.51			       *
    ! ************************************************************

    Canopy_Temp_Differential = -4.7739 - 4.2762*psil_
    if(Canopy_Temp_Differential.lt.-5.0) Canopy_Temp_Differential=-5.0
    if(Canopy_Temp_Differential.gt.8.0) Canopy_Temp_Differential = 8.0
    RETURN
    END


    subroutine Time_to_Bloom(avtnod)
    ! **************************************************************
    ! *** Calculate the time from first square to bloom, BLOOM   ***
    ! **************************************************************

    use common_block

    AT = AVTNOD
    IF(AT.LT.17.0) AT = 17.0
    IF(AT.GT.32.0) AT = 32.0
    IF((VARITY(IVARTY).EQ.'PIMA').OR.(VARITY(IVARTY).EQ.'pima')) THEN
        BLOOM=(188.261-10.704*AT+0.1784*AT**2)*CALBRT(30)
    ELSE
        BLOOM=(252.591-15.321*AT+0.2531*AT**2)*CALBRT(30)
    ENDIF
    RETURN
    END


    subroutine First_Bloom_Today
    ! *****************************************************
    ! *** Set the date for first bloom                  ***
    ! *****************************************************

    use common_block

    ! *** Set the date counter for the calculation of ave. temp to FBL

    FBLOOM = KDAY
     iflday = daynum
    !iflday = jday
    ave_fbl_tavg = sum_fbl_tavg / float(kday)
    sum_fob_tavg = sum_fbl_tavg
    ifbldae = kday
    ifob = kday-1
    numfob = 1
    RETURN
    END


    subroutine Update_Boll_and_Square_Weight(k,l,m)
    ! **************************************************************
    ! *** Estimate boll weight and update square weight          ***
    ! **************************************************************

    use common_block

    BLUM(KDAY) = BLUM(KDAY) + FFRUT(K,L,M)
    BOLWGT(K,L,M) = .31 * SQRWT(K,L,M)
    PQFLR = PQFLR + .69 * SQRWT(K,L,M)
    GBOLWT = GBOLWT + BOLWGT(K,L,M)
    SQWT = SQWT - SQRWT(K,L,M)
    SQRWT(K,L,M) = 0.0
    BOLTMP(K,L,M) = TAVG
    AGEBOL(K,L,M) = 1.0
    FCODE(K,L,M) = 7
    RETURN
    END


    subroutine Loss_due_Pollination(k,l,m)
    ! **************************************************************
    ! *** Estimate the number of blooms affected by pollination  ***
    ! **************************************************************

    use common_block

    IF(POLYNA.EQ.0) THEN
        MCODE(K,L,M) = 8
        POLOSS = POLOSS + FFRUT(K,L,M)*.25
    ENDIF
    RETURN
    END


    subroutine Boll_Safe_Age(k,l,m)
    ! **************************************************************
    ! *** Calculate the boll safe time from abscission           ***
    ! **************************************************************

    use common_block

    FSTAVG(K,L,M) = (FSTAVG(K,L,M)*AGEBOL(K,L,M)+FSTRES)/		&
        (AGEBOL(K,L,M)+1)
    IF((PRPDAY.GT.0).AND.(DAYNUM.GT.PRPDAY)) THEN
    !IF((PRPDAY.GT.0).AND.(JDAY.GT.PRPDAY)) THEN
        
        AGEABZ(K,L,M) = 12.82-3.229*PRPKGH-.4987*PRPKGH**2+	&
            1.723*AVGTSP-.03181*AVGTSP**2+	&
            0.09911*AVGTSP*PRPKGH
    ELSE
        AGEABZ(K,L,M) = CALBRT(11)+15.0-15.0*FSTAVG(K,L,M)
    ENDIF

    IF(AGEBOL(K,L,M).GE.AGEABZ(K,L,M)) FCODE(K,L,M) = 2
    if(iobday.eq.0.and.ifob.lt.kday) then
        sum_fob_tavg = sum_fob_tavg + tavg
        ifob = kday
        numfob = numfob + 1
    endif

    RETURN
    END


    subroutine Time_to_Open_Bolln(k,l,m)
    ! **************************************************************
    ! *** Calculate the time from emergence to open boll, DEHISS ***
    ! **************************************************************

    use common_block

    atn = boltmp(k,l,m)
    if(atn.lt.17.0) atn = 17.0
    if(atn.gt.35.0) atn = 35.0
    if(prpkgh.gt.0.01) then
        dehiss(k,l,m) = (446.51 - 27.65*atn + 0.4807*atn**2-127.03*	&
            prpkgh+10.28*atn*prpkgh-0.2105*prpkgh*atn**2)
    else
        dehiss(k,l,m) = (1.0/(-0.0058298+0.0009947*atn))*calbrt(31)
    endif
    if(dehiss(k,l,m).gt.70.) dehiss(k,l,m) = 70.

    if(iobday.eq.0.and.ifob.lt.kday) then
        sum_fob_tavg = sum_fob_tavg + tavg
        ifob = kday
        numfob = numfob + 1
    endif

    return
    end


    subroutine Time_to_Open_Boll(k,l,m)
    ! **************************************************************
    ! *** Calculate the time from emergence to open boll, DEHISS ***
    ! **************************************************************

    use common_block

    ATN=BOLTMP(K,L,M)
    IF(ATN.GT.35.0) ATN = 35.0
    IF(ATN.LT.17.0) ATN = 17.0
    IF(PRPKGH.GT.0.01) THEN
        DEHISS(K,L,M)=(446.51-27.65*ATN+.4807*ATN**2-127.03*		  &
            PRPKGH+10.28*ATN*PRPKGH-.2105*PRPKGH*ATN**2)
    ELSEIF((VARITY(IVARTY).EQ.'PIMA').OR.							  &
        (VARITY(IVARTY).EQ.'pima')) THEN
        DEHISS(K,L,M)=(358.264-19.294*ATN+0.291*ATN**2)*CALBRT(31)
    ELSE
        DEHISS(K,L,M)=(327.396-17.251*ATN+0.255*ATN**2)*CALBRT(31)
    ENDIF
    IF(DEHISS(K,L,M).GT.70.) DEHISS(K,L,M)=70.

    if(iobday.eq.0.and.ifob.lt.kday) then
        sum_fob_tavg = sum_fob_tavg + tavg
        ifob = kday
        numfob = numfob + 1
    endif

    RETURN
    END


    subroutine Seed_and_Burr_Ncontent(k,l,m)
    ! ****************************************************
    ! *** Calculate the N content of the burr and seed ***
    ! ****************************************************

    use common_block

    FCODE(K,L,M) = 3
    IF(iobday.eq.0)then
         iobday = daynum
        !   iobday = JDAY
        ifob = kday
        ifobdae = kday
        ave_fob_tavg = sum_fob_tavg / float(kday)
    endif

    IF(GBOLWT.GT.0.0) THEN
        SEEDN = SEEDN - BOLWGT(K,L,M)*0.416*SEEDCN
        BURRN = BURRN - BOLWGT(K,L,M)*0.278*BURCN
        NLOSS = NLOSS + BOLWGT(K,L,M)*0.416*SEEDCN
        NLOSS = NLOSS + BOLWGT(K,L,M)*0.278*BURCN
    ENDIF
    RETURN
    END


    subroutine Fiber_Length_and_Strength(avtnod)
    ! **************************************************************
    ! *** Calculate the time from first square to bloom, BLOOM   ***
    ! **************************************************************

    use common_block

    ! *** FS=FIBER STRENGTH : (G / TEX * 1/8 INCH)
    ! *** FL IS FIBER LENGTH ( INCHES, 2.5% SPUN )

    FSX = 56.603 + AVTNOD *(-2.921+0.059*AVTNOD)
    IF(NOPEN.GT.0.01) FS=(FSX+FS)/NOPEN
    FLX=1.219-.0065*AVTNOD
    IF(NOPEN.GT.0.01) FL=(FL+FLX)/NOPEN
    RETURN
    END


    SUBROUTINE ADDPFNOD
    ! ***************************************************************
    ! *** Increment prefruiting node count and initiate leaf area ***
    ! ***************************************************************

    use common_block

    NUMPFN = NUMPFN + 1
    AVTPFN(NUMPFN) = TAVG
    AGEPFN(NUMPFN) = 1

    ! *** Calculate running average of leaf nitrogen

    cntpflfcn(NUMPFN) = cntpflfcn(NUMPFN) + 1.0
    sumpflfcn(NUMPFN) = sumpflfcn(NUMPFN) + leafcn
    !	avelfcn = sumpflfcn(NUMPFN-1) / cntpflfcn(NUMPFN-1)

    ! *** Calculate the age, weight and prefruiting leaf area at unfolding

    PfLfAge(numpfn) = PfLfAge(numpfn) 							 &
        + Duration_leaf_expansion(tavg)*calbrt(40)
    if(PfLfAge(numpfn).lt.1.0)agepflf(numpfn) = agepflf(numpfn) + 1.0

    ! *** Assume that at node initiation, the leaf area is negligible

    PFWL(NUMPFN) = 0.001
    STEMWT = STEMWT - PFWL(NUMPFN)
    SLEAFN = SLEAFN + (PFWL(NUMPFN)* 0.035)
    STEMN  = STEMN  - (PFWL(NUMPFN)* 0.035)

    RETURN
    END


    SUBROUTINE ADDMSNOD(K)
    ! ************************************************************
    ! *** Increment mainstem node count and initiate leaf area ***
    ! *** NFBR(1) is set to 1 at first square                  ***
    ! ************************************************************

    use common_block

    IF((K.GT.1).OR.(NFBR(K).GT.0)) THEN
        NFBR(K) = NFBR(K) + 1
        nodeknt = numpfn + nfbr(k)
        NEWBR = NFBR(K)
        NNOD(K,NEWBR) = 1
        FFRUT(K,NEWBR,1) = 1.
        FCODE(K,NEWBR,1) = 1
        IF(PINHED.GE.1.) THEN
            PINHED = PINHED - 1.
            MCODE(K,NEWBR,1) = 6
        ENDIF

        ! *** Calculate running average of leaf nitrogen

        cntmslfcn(nodeknt) = cntmslfcn(nodeknt) + 1.0
        summslfcn(nodeknt) = summslfcn(nodeknt) + leafcn
        !	  avelfcn = summslfcn(NEWBR-1) / cntmslfcn(NEWBR-1)

        ! *** Calculate the age, weight and fruiting branch leaf area at unfolding

        FRLfAge(k,NEWBR,1) = FRLfAge(k,NEWBR,1)						  &
            + Duration_leaf_expansion(tavg)*calbrt(40)
        if(FRLfAge(k,NEWBR,1).lt.1.0) 								  &
            agefrlf(k,NEWBR,1) = agefrlf(k,NEWBR,1) + 1.0

        ! *** Assume that at node initiation, the leaf area is negligible

        LEAFW(K,NEWBR,1) = 0.001
        STEMWT = STEMWT -  LEAFW(K,NEWBR,1)
        SLEAFN = SLEAFN + (LEAFW(K,NEWBR,1) * 0.035)
        STEMN  = STEMN  - (LEAFW(K,NEWBR,1) * 0.035)

        ! *** Calculate the age, weight and leaf area on main stem node at unfolding

        XMSLfAge(k,NEWBR) = XMSLfAge(k,NEWBR) 						  &
            + Duration_leaf_expansion(tavg)*calbrt(40)
        if(XMSLfAge(k,NEWBR).lt.1.0) 								  &
            agemslf(k,NEWBR) = agemslf(k,NEWBR) + 1.0

        ! *** Assume that at node initiation, the leaf area is negligible

        MLEAFW(K,NEWBR) = 0.001
        STEMWT = STEMWT -  MLEAFW(K,NEWBR)
        SLEAFN = SLEAFN + (MLEAFW(K,NEWBR) * 0.035)
        STEMN  = STEMN  - (MLEAFW(K,NEWBR) * 0.035)
        NBRCH = NFBR(K)
        VDELAY(K) = 0.
        LEFCNT=LEFCNT+1
        LEFSRT(LEFCNT)=K*1000+NEWBR*10+1
    ELSE
        CALL FSTSQTODAY
    ENDIF

    RETURN
    END


    subroutine ADDFBNOD(k,l,nnid)
    ! *******************************************************************
    ! *** Increment fruiting branch node count and initiate leaf area ***
    ! *******************************************************************

    use common_block

    NEWNOD = NNID + 1
    NNOD(K,L) = NNOD(K,L) + 1
    FFRUT(K,L,NEWNOD) = 1.
    FCODE(K,L,NEWNOD) = 1
    IF(PINHED.GE.1.) THEN
        MCODE(K,L,NEWNOD) = 6
        PINHED = PINHED - 1.
    endif
    nodeknt = numpfn + nfbr(k)

    ! *** Calculate running average of leaf nitrogen

    cntmslfcn(nodeknt) = cntmslfcn(nodeknt) + 1.0
    summslfcn(nodeknt) = summslfcn(nodeknt) + leafcn
    !	  avelfcn = summslfcn(nodeknt-1) / cntmslfcn(nodeknt-1)

    ! *** Calculate the age, weight and fruiting branch leaf area at unfolding

    FRLfAge(k,L,NEWNOD) = FRLfAge(k,L,NEWNOD)							&
        + Duration_leaf_expansion(tavg)*calbrt(40)
    if(FRLfAge(k,L,NEWNOD).lt.1.0) 									&
        agefrlf(k,L,NEWNOD) = agefrlf(k,L,NEWNOD) + 1.0

    ! *** Assume that at node initiation, the leaf area is negligible

    LEAFW(K,L,NEWNOD) = 0.001
    STEMWT = STEMWT -  LEAFW(K,L,NEWNOD)
    SLEAFN = SLEAFN + (LEAFW(K,L,NEWNOD) * 0.035)
    STEMN  = STEMN  - (LEAFW(K,L,NEWNOD) * 0.035)
    NNID = NNOD(K,L)
    DELAY(K,L) = 0.
    LEFCNT=LEFCNT+1
    LEFSRT(LEFCNT)=K*1000+L*10+NEWNOD
    RETURN
    END


    SUBROUTINE FSTSQTODAY

    use common_block

    ISQ=KDAY
    FCODE(1,1,1) = 1
    FFRUT(1,1,1) = 1.

    ! *** Calculate running average of leaf nitrogen

    cntmslfcn(numpfn) = cntmslfcn(numpfn) + 1.0
    summslfcn(numpfn) = summslfcn(numpfn) + leafcn
    !	avelfcn = summslfcn(numpfn-1) / cntmslfcn(numpfn-1)

    ! *** Calculate the age, weight and fruiting branch leaf area at unfolding

    FRLfAge(1,1,1) = FRLfAge(1,1,1)									&
        + Duration_leaf_expansion(tavg)*calbrt(40)
    if(FRLfAge(1,1,1).lt.1.0) 										&
        agefrlf(1,1,1) = agefrlf(1,1,1) + 1.0

    ! *** Assume that at node initiation, the leaf area is negligible

    LEAFW(1,1,1) = 0.001
    STEMWT = STEMWT -  LEAFW(1,1,1)
    SLEAFN = SLEAFN + (LEAFW(1,1,1) * 0.035)
    STEMN  = STEMN  - (LEAFW(1,1,1) * 0.035)
    NFBR(1) = 1
    NNOD(1,1) = 1
    FSTRES = 1.0
    LEFCNT = LEFCNT+1
    LEFSRT(LEFCNT)=1000+10+1

    ! *** DROP COTYLEDONS AT TIME OF FIRST SQUARE. 0.2 IS THE INITIAL LEAFW

    daelfwt = 0.2
    LEFABS = LEFABS + daelfwt
    leafwt = leafwt - lefabs
    SLEAFN = SLEAFN - daelfwt*0.01
    NLOSS  = NLOSS + (daelfwt*0.01)
    PIXLOS = daelfwt * PIXCON
    PIXPLT = PIXPLT - PIXLOS

    ! *** Set the date counter for the calculation of ave. temp to FSQ

    isqday = daynum
   ! isqday = jday
    ifsqdae = kday
    ave_fsq_tavg = sum_fsq_tavg / float(isq)
    sum_fbl_tavg = sum_fsq_tavg
    ifbl = kday - 1

    RETURN
    END


    SUBROUTINE ABCISE
    ! *************************************************************
    ! *                                                           *
    ! *                    ABSCISE SUBROUTINE                     *
    ! *                                                           *
    ! *************************************************************

    use common_block

    BOLLOS = 0.
    GBZ2   = 0
    ABZ    = 0.
    SUMSQR = 0.
    SUMBOL = 0.
    SQRZ   = 0.
    SQRLOS = 0.
    SUMFRU = 0.
    FDROPS = 0.
    FDROPB = 0.
    SQABZ  = 0.
    BOLABZ = 0.
    DPSMX  = CALBRT(6)
    DPBMX  = CALBRT(7)
    DUM = LAI
    IF(DUM.GT.3.5) DUM = 3.5
    DROPLF = (90.0 - (5.0 * DUM)) * CALBRT(39)

    ! *** ABSCISE ALL LEAVES OLDER THAN DROPLF PHYSIOLOGICAL DAYS AND
    ! *** CALCULATE THE AMOUNT OF N REMOVED FROM THE PLANT SYSTEM BY THE
    ! *** ABSCISED LEAF. IT IS ASSUMED THAT DEAD LEAVES CONTAIN 1 % N.
    ! *** ABSCISE PRE-FRUTING LEAVES
    !
    
    DO 4 J=1,NUMPFN
        IF((AGEPFN(J).GE.DROPLF).AND.(PFAL(J).GT.0.)			&
            .AND.(LAI.GT.0.3)) THEN
            AREA = AREA - PFAL(J)
            LAI = AREA/POPFAC
            PFAL(J) = 0.
            LEFABS = LEFABS + PFWL(J)
            LEAFWT = LEAFWT - PFWL(J)
            PIXLOS =  PFWL(J) * PIXCON
            PIXPLT = PIXPLT - PIXLOS
            SLEAFN = SLEAFN - PFWL(J) * 0.01
            NLOSS  = NLOSS + (PFWL(J) * 0.01)
            PFWL(J) = 0.
            LVSLOS = LVSLOS+1
            IF((DEFBGN.GT.0).AND.(DAYNUM.GT.DEFBGN))			 &
          !                  IF((DEFBGN.GT.0).AND.(JDAY.GT.DEFBGN))			 &

                LVSLOS = LVSLOS+1
        ENDIF
4   CONTINUE

    ! *** ABSCISE MAIN STEM LEAVES

    DO 52 K=1,NVBRCH
        NBRCH = NFBR(K)
        DO 52 L=1,NBRCH
            NNID = NNOD(K,L)
            DO 52 M=1,NNID
                IF((M.EQ.1).AND.(LAGE(K,L,M).GE.DROPLF).AND.	 &
                    (MLAREA(K,L).GT.0.).AND.(LAI.GT.0.3)) THEN
                    LEFABS = LEFABS + MLEAFW(K,L)
                    LEAFWT = LEAFWT - MLEAFW(K,L)
                    PIXLOS = MLEAFW(K,L) * PIXCON
                    PIXPLT = PIXPLT - PIXLOS
                    SLEAFN = SLEAFN - MLEAFW(K,L) * 0.01
                    NLOSS  = NLOSS + (MLEAFW(K,L) * 0.01)
                    AREA = AREA - MLAREA(K,L)
                    LAI = AREA/POPFAC
                    MLAREA(K,L) = 0.
                    MLEAFW(K,L) = 0.
                    LVSLOS = LVSLOS+1
                     IF((DEFBGN.GT.0).AND.(DAYNUM.GT.DEFBGN))		 &
                    !         IF((DEFBGN.GT.0).AND.(JDAY.GT.DEFBGN))		 &
                        LVSLOS = LVSLOS+1
                ENDIF

                ! *** ABSCISE FRUITING LEAVES

                IF((LAGE(K,L,M).GE.DROPLF).AND.(LAREA(K,L,M).GT.0.)    &
                    .AND.(LAI.GT.0.3)) THEN
                    LEFABS = LEFABS + LEAFW(K,L,M)
                    LEAFWT = LEAFWT - LEAFW(K,L,M)
                    PIXLOS = LEAFW(K,L,M) * PIXCON
                    PIXPLT = PIXPLT - PIXLOS
                    SLEAFN = SLEAFN - LEAFW(K,L,M) * 0.01
                    NLOSS  = NLOSS + (LEAFW(K,L,M) * 0.01)
                    AREA = AREA - LAREA(K,L,M)
                    LAI = AREA/POPFAC
                    LAREA(K,L,M) = 0.
                    LEAFW(K,L,M) = 0.
                    LVSLOS = LVSLOS+1
                    IF((DEFBGN.GT.0).AND.(DAYNUM.GT.DEFBGN))			   &
            !        IF((DEFBGN.GT.0).AND.(JDAY.GT.DEFBGN))			   &
                        LVSLOS = LVSLOS+1
                ENDIF
52  CONTINUE

    IF((DEFBGN.GT.0).AND.(DAYNUM.GT.DEFBGN)) THEN
   ! IF((DEFBGN.GT.0).AND.(JDAY.GT.DEFBGN)) THEN
        KNT = LFATDF*PERDEF/100.-LVSLOS
        I = 0
60      CONTINUE
        I = I+1
        IF((KNT.GT.0).AND.(I.LE.LEFCNT)) THEN
            IDUM = LEFSRT(I)
            IF(IDUM.GT.0) THEN
                K = IDUM/1000
                L = IDUM/10-K*100
                M = IDUM-IDUM/10*10
                IF(LEAFW(K,L,M).LE.0.0001) THEN
                    IDUM = 0
                    LEFSRT(I) = 0
                ENDIF
            ENDIF
            IF(IDUM.GT.0) THEN
                LEFABS = LEFABS + LEAFW(K,L,M)
                LEAFWT = LEAFWT - LEAFW(K,L,M)
                PIXLOS = LEAFW(K,L,M) * PIXCON
                PIXPLT = PIXPLT - PIXLOS
                SLEAFN = SLEAFN - LEAFW(K,L,M) * 0.01
                NLOSS  = NLOSS + (LEAFW(K,L,M) * 0.01)
                AREA = AREA - LAREA(K,L,M)
                LAREA(K,L,M) = 0.
                LEAFW(K,L,M) = 0.
                KNT = KNT-1
                LVSLOS = LVSLOS+1
                IF((M.EQ.1).AND.(MLAREA(K,L).GT.0.)) THEN
                    LEFABS = LEFABS + MLEAFW(K,L)
                    LEAFWT = LEAFWT - MLEAFW(K,L)
                    PIXLOS = MLEAFW(K,L) * PIXCON
                    PIXPLT = PIXPLT - PIXLOS
                    SLEAFN = SLEAFN - MLEAFW(K,L) * 0.01
                    NLOSS  = NLOSS + (MLEAFW(K,L) * 0.01)
                    AREA = AREA - MLAREA(K,L)
                    MLAREA(K,L) = 0.
                    MLEAFW(K,L) = 0.
                    KNT=KNT-1
                    LVSLOS=LVSLOS+1
                ENDIF
                LEFSRT(I) = 0
            ENDIF
            GO TO 60
        ENDIF
        LAI = AREA/POPFAC
    ENDIF

    ! *** FRUIT ABSCISION

    IF(FCODE(1,1,1).gt.0) then


        IF(FRATIO.LT.CALBRT(8)) THEN                        !RATIO Of GREEN BOLLS WEIGHT TO TOTAL PLANT WEIGHT
            FLOSS= (CALBRT(9)-3.607*FSTRES+1.605*FSTRES**2)*CALBRT(54)
        ELSE
            FLOSS=(CALBRT(10)-3.607*FSTRES+1.605*FSTRES**2)*CALBRT(55)
        ENDIF
        IF(FLOSS.LT.0.0) FLOSS = 0.0

        ! *** FCODES   1=SQ  2=GB  3=OB  4  5  6  or 7=YGB
        ! *** Count the total number of squares/bolls susceptible to abscission

        DO K=1,NVBRCH
            NBRCH = NFBR(K)
            DO L=1,NBRCH
                NNID = NNOD(K,L)
                DO M=1,NNID
                    IF(FCODE(K,L,M).EQ.1) THEN
                        IF(AGE(K,L,M).GT.2.0) THEN
                            SUMSQR=SUMSQR+FFRUT(K,L,M)
                        ENDIF
                    ELSEIF(FCODE(K,L,M).EQ.7) THEN
                        IF(AGEBOL(K,L,M).LT.AGEABZ(K,L,M)) THEN
                            SUMBOL = SUMBOL + FFRUT(K,L,M)
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO
        ENDDO
        susceptible_bolls = sumbol
        SUMFRU = SUMBOL + SUMSQR

        ! *** DO CALCULATION PERTAINING TO YOUNG GREEN BOLLS. REMOVE BOLLS WITH
        ! *** A SMALL FRACTION REMAINING, SET FCODE TO 4 AND REDUCE ITS WEIGHT AND
        ! *** NITROGEN CONTENT

        IF(SUMBOL.GT.0.001) THEN
            call Running_Ave_Temp
            call TempFact_Boll

            ! *** Increment the number of bolls marked for abcission due to
            ! *** nutritional and heat stress         Kit's original code  04/27/00
            ! *** Count the number of bolls marked for abcission due to heat stress

            HeatLoss = HeatIndex * (SUMBOL/SUMFRU)

            ! *** Count the number of bolls marked for abcission due to nutritional stress

            BOLLOS = FLOSS * (SUMBOL/SUMFRU)

            bollos = HeatLoss + Bollos
            if(bollos.gt.sumbol) bollos = sumbol

            BOLLOSMX = SUMBOL * DPBMX
            IF(POLOSS.GT.BOLLOSMX) THEN
                FDROPB = 0.0
                BOLLOS = POLOSS
            ELSEIF((BOLLOS + POLOSS).GT.BOLLOSMX) THEN
                FDROPB = (BOLLOSMX - POLOSS) / SUMBOL
                BOLLOS = DPBMX * SUMBOL
            ELSE
                FDROPB = (BOLLOS - POLOSS) / SUMBOL
                IF(FDROPB.LT.0.) FDROPB = 0.
                BOLLOS = FDROPB * SUMBOL + POLOSS
            ENDIF
            SUMBOL = SUMBOL - SUMBOL*FDROPB
        ENDIF

        IF(SUMSQR.GT.0.00001) THEN
            SQRLOS = FLOSS - BOLLOS
            IF(SQRLOS.LT.0.0) SQRLOS = 0.0
            FDROPS = SQRLOS / SUMSQR
            IF(FDROPS.GT.DPSMX) THEN
                FDROPS = DPSMX
                SQRLOS = SUMSQR * DPSMX
            ENDIF
            SUMSQR = SUMSQR - SQRLOS
        ENDIF

        DO K=1,NVBRCH
            NBRCH = NFBR(K)
            DO L=1,NBRCH
                NNID = NNOD(K,L)
                DO M=1,NNID
                    WTLOS = 0.0
                    IF(FCODE(K,L,M).EQ.1) THEN
                        IF(AGE(K,L,M).GT.2.0) THEN
                            WTLOS = SQRWT(K,L,M) * FDROPS
                            SQRWT(K,L,M) = SQRWT(K,L,M) - WTLOS
                            PQFLR = PQFLR + WTLOS
                            SQWT = SQWT - WTLOS
                            SQABZ = SQABZ + FFRUT(K,L,M)*FDROPS
                            FFRUT(K,L,M) = FFRUT(K,L,M)-FFRUT(K,L,M)*FDROPS
                            IF(FFRUT(K,L,M).LE.0.001) THEN
                                FFRUT(K,L,M) = 0.0
                                PQFLR = PQFLR + SQRWT(K,L,M)
                                SQWT = SQWT - SQRWT(K,L,M)
                                SQRWT(K,L,M) = 0.0
                            ENDIF
                            SQRZ = SQRZ + FFRUT(K,L,M)
                        ELSEIF(AGE(K,L,M).EQ.2) THEN
                            PINDROP = .045
                            WTLOS = SQRWT(K,L,M) * PINDROP
                            SQRWT(K,L,M) = SQRWT(K,L,M) - WTLOS
                            PQFLR = PQFLR + WTLOS
                            SQWT = SQWT - WTLOS
                            SQABZ = SQABZ + FFRUT(K,L,M) * PINDROP
                            FFRUT(K,L,M)=FFRUT(K,L,M)-FFRUT(K,L,M)*PINDROP
                            SQRZ = SQRZ + FFRUT(K,L,M)
                        ELSE
                            SQRZ = SQRZ + FFRUT(K,L,M)
                        ENDIF
                    ENDIF
                    IF(FCODE(K,L,M).EQ.2) THEN
                        GBZ2 = GBZ2 + FFRUT(K,L,M)
                    ENDIF
                    IF(FCODE(K,L,M).EQ.7) THEN
                        IF(MCODE(K,L,M).EQ.8) THEN
                            FFRUT(K,L,M) = FFRUT(K,L,M)*.75
                            MCODE(K,L,M) = 7
                        ELSE
                            CALL ABCISBOL(BOLABZ,K,L,M,FDROPB)
                        ENDIF
                        IF(FFRUT(K,L,M).LE.0.001) THEN
                            SEEDN = SEEDN - BOLWGT(K,L,M)*0.416*SEEDCN
                            BURRN = BURRN - BOLWGT(K,L,M)*0.278*BURCN
                            NLOSS = NLOSS + BOLWGT(K,L,M)*0.416*SEEDCN
                            NLOSS = NLOSS + BOLWGT(K,L,M)*0.278*BURCN
                            GBLOS = GBLOS + BOLWGT(K,L,M)
                            GBOLWT = GBOLWT - BOLWGT(K,L,M)
                            PIXLOS = BOLWGT(K,L,M) * PIXCON
                            PIXPLT = PIXPLT - PIXLOS
                            BOLWGT(K,L,M) = 0.0
                            BOLABZ = BOLABZ + FFRUT(K,L,M)
                            FFRUT(K,L,M) = 0.0
                        ENDIF
                        GBZ2 = GBZ2 + FFRUT(K,L,M)
                    ENDIF
                ENDDO
            ENDDO
        ENDDO
        !SQLOSS(JDAY) = SQABZ
        !BOLOSS(JDAY) = BOLABZ
        SQLOSS(DAYNUM) = SQABZ
        BOLOSS(DAYNUM) = BOLABZ
        BOLABZT=BOLABZT+BOLABZ
        SQABZT=SQABZT+SQABZ
        ABZ = SQABZ + BOLABZ

    endif

    RETURN
    END


    Subroutine Running_Ave_Temp
    ! ***********************************************************
    ! *** CALCULATE RUNNING AVERAGE OF TAVG FOR THE LAST FIVE ***
    ! *** DAYS. THIS IS DONE TO SMOOTH THE SHARP FLUCTUATIONS ***
    ! *** IN DAILY TAVG.MOVE ALL VALUES DOWN ONE SLOT, PUT  	***
    ! *** NEW VALUE IN FIRST SLOT, AVERAGE 					***
    ! ***********************************************************

    use common_block

    do j = 2,7
        Bloom_tavg(j) = Bloom_tavg(daynum-j+1)
     !   Bloom_tavg(j) = Bloom_tavg(jday-j+1)

    enddo
    Bloom_tavg(1) = Bloom_tavg(daynum)
   ! Bloom_tavg(1) = Bloom_tavg(jday)

    !  Used only when considering the current effect of canopy
    !  temperature differential                      06/04/03  Kit
    !      Bloom_tavg(1) = tavg + Canopy_Temp_Differential(psild)

    Bloom_tavg(1) = tavg
    Boll_tavg = 0.
    do J =1,7
        Boll_tavg = Boll_tavg + Bloom_tavg(J)
    enddo
    Boll_tavg = Boll_tavg/7.0
    return
    end


    Subroutine TempFact_Boll
    ! ********************************************************
    ! *** Calculates heat stress index for boll abscission ***
    ! *** based on percent fruit retention vs temperature. ***
    ! *** TempFact_Node is a multiplier to increase boll   ***
    ! *** loss. Based temperature of 26.0 oC.  HeatIndex   ***
    ! *** is the number of bolls to be dropped today.      ***
    ! *** (Source:  K.R. Reddy)             01/18/2000	 ***
    ! ********************************************************


    use common_block

    stdtemp = 26.0
    templimit = 33.5
    if(Boll_tavg.lt.stdtemp) Boll_tavg = stdtemp
    if(Boll_tavg.gt.templimit) Boll_tavg = templimit
    dum1 = Boll_tavg/30.7978
    dum2 = stdtemp/30.7978
    tempnum = -7.2531 + (92.2099/(1.0 + dum1**28.5048))
    tempden = -7.2531 + (92.2099/(1.0 + dum2**28.5048))
    ratio = tempnum/tempden
    if(ratio.gt.1.0) ratio = 1.0
    if(ratio.lt.0.0) ratio = 0.0
    HeatIndex = (1.0 - ratio)

    !  Number of bolls lost due to heat injury
    HeatIndex0 = (1.0 - ratio) * susceptible_bolls * calbrt(50)

    return
    end


    SUBROUTINE ABCISBOL(BOLABZ,K,L,M,FDROPB)

    use common_block

    FDUM = FDROPB
    IF(FFRUT(K,L,M)-FFRUT(K,L,M)*FDUM.GT.1.) THEN
        FDUM = (FFRUT(K,L,M)-1.0)/FFRUT(K,L,M)
    ENDIF
    WTLOS = BOLWGT(K,L,M) * FDUM
    SEEDN = SEEDN - WTLOS*0.416*SEEDCN
    BURRN = BURRN - WTLOS*0.278*BURCN
    NLOSS = NLOSS + WTLOS*0.416*SEEDCN
    NLOSS = NLOSS + WTLOS*0.278*BURCN
    GBLOS = GBLOS + WTLOS
    GBOLWT = GBOLWT - WTLOS
    PIXLOS = WTLOS * PIXCON
    PIXPLT = PIXPLT - PIXLOS
    BOLWGT(K,L,M) = BOLWGT(K,L,M) - WTLOS
    BOLABZ = BOLABZ + FFRUT(K,L,M)*FDUM
    FFRUT(K,L,M) = FFRUT(K,L,M)-FFRUT(K,L,M)*FDUM

    RETURN
    END


    SUBROUTINE PMAPS
    ! *************************************************************
    ! *                                                           *
    ! *                PLANT MAPS SUBROUTINE                      *
    ! *                                                           *
    ! *************************************************************
    ! * SETS MCODES TO DISTRIBUTE ABSCISSION OVER ENTIRE PLANT.   *
    ! * A FRUIT WILL BE INCLUDED IN THE AVERAGE PLANT MAP IF THE  *
    ! * FRACTION OF FRUIT REMAINING AT EACH SITE IS EQUAL OR      *
    ! * LARGER TO THE AVERAGE SIZE FOR EACH FRUIT CLASS.          *
    ! * THIS SUBROUTINE ALSO CALCULATES SUMMARIES OF FRUIT PRESENT*
    ! * AT EACH SITE AND CALLS COTPLT TO PRINT PLANT MAPS         *
    ! *************************************************************
    ! *                                                           *
    ! * FCODE                          MCODE                      *
    ! *                                                           *
    ! * 1 SQUARE                       1 SQUARE                   *
    ! * 2 GREEN BOLL                   2 GREEN BOLL               *
    ! * 3 MATURE BOLL                  3 MATURE BOLL              *
    ! * 4 FRUIT ABSCISED AS A SQUARE   4 FRUIT ABZ AS A SQUARE    *
    ! * 5 FRUIT ABSCISED AS A BOLL     5 FRUIT ABZ AS A BOLL      *
    ! * 6 NOT USED                     6 ABZ AS PINHEAD OR POLYNA *
    ! * 7 YOUNG GREEN BOLL             7 YOUNG GREEN BOLL         *
    ! *************************************************************

    use common_block

    SUMFRU1 = 0.0
    SUMFRU2 = 0.0
    SUMFRU3 = 0.0
    SUMFRU7 = 0.0
    FRU1NO  = 0.0
    FRU2NO  = 0.0
    FRU3NO  = 0.0
    FRU7NO  = 0.0

    !         DO 23 K = 1,450

    DO 23 K = 1,mxfruts
        IF(FCODES(K).NE.0) THEN
            IF(FCODES(K).EQ.1) THEN
                SUMFRU1 = SUMFRU1 + FRUITS(K)
                FRU1NO  = FRU1NO  + 1.0
            ENDIF
            IF(FCODES(K).EQ.2) THEN
                SUMFRU2 = SUMFRU2 + FRUITS(K)
                FRU2NO  = FRU2NO  + 1.0
            ENDIF
            IF(FCODES(K).EQ.3) THEN
                SUMFRU3 = SUMFRU3 + FRUITS(K)
                FRU3NO  = FRU3NO  + 1.0
            ENDIF
            IF(FCODES(K).EQ.7) THEN
                SUMFRU7 = SUMFRU7 + FRUITS(K)
                FRU7NO  = FRU7NO  + 1.0
            ENDIF
        ENDIF
23  CONTINUE
    IF(FRU1NO.GT.0.0) AVGFRU1 = SUMFRU1/FRU1NO
    IF(FRU2NO.GT.0.0) AVGFRU2 = SUMFRU2/FRU2NO
    IF(FRU3NO.GT.0.0) AVGFRU3 = SUMFRU3/FRU3NO
    IF(FRU7NO.GT.0.0) AVGFRU7 = SUMFRU7/FRU7NO

    !         DO 24 K = 1,450

    DO 24 K = 1,mxfruts
        IF(FCODES(K).NE.0) THEN
            IF(FCODES(K).EQ.1) THEN
                IF(FRUITS(K).GE.AVGFRU1) THEN
                    MCODES(K) = 1
                ELSE
                    MCODES(K) = 4
                ENDIF
            ENDIF
            IF(FCODES(K).EQ.2) THEN
                IF(FRUITS(K).GE.AVGFRU2) THEN
                    MCODES(K) = 2
                ELSE
                    MCODES(K) = 5
                ENDIF
            ENDIF
            IF(FCODES(K).EQ.3) THEN
                IF(FRUITS(K).GE.AVGFRU3) THEN
                    MCODES(K) = 3
                ELSE
                    MCODES(K) = 5
                ENDIF
            ENDIF
            IF(FCODES(K).EQ.7) THEN
                IF(FRUITS(K).GE.AVGFRU7) THEN
                    MCODES(K) = 7
                ELSE
                    MCODES(K) = 5
                ENDIF
            ENDIF
            IF(FCODES(K).EQ.4) MCODES(K) = 4
            IF(FCODES(K).EQ.5) MCODES(K) = 5
        ENDIF
24  CONTINUE

    ! *** DISPLAYS OUTPUT OF BOLL AGE AND WEIGHT. **NOTE** FRACTION
    ! *** OF FRUTP AND BSIZE ARE TRUNKATED IN COTPLT

    DO 100 K=1,NVBRCH
        NBRCH = NFBR(K)
        DO 100 L=1,NBRCH
            NNID = NNOD(K,L)
            DO 100 M=1,NNID
                IF(FCODE(K,L,M).EQ.2.OR.FCODE(K,L,M).EQ.3.OR.	  &
                    FCODE(K,L,M).EQ.7) THEN
                    IF(FFRUT(K,L,M).GT.0.001) THEN
                        BSIZE(K,L,M) = BOLWGT(K,L,M)/FFRUT(K,L,M)
                    ELSE
                        BSIZE(K,L,M) = 0.0
                    ENDIF
                    IF(BSIZE(K,L,M).GT.CALBRT(12))BSIZE(K,L,M)=CALBRT(12)
                ENDIF
                IF(FCODE(K,L,M).EQ.4.OR.FCODE(K,L,M).EQ.5) THEN
                    FRUTP(K,L,M) = 11.
                    BSIZE(K,L,M) = 11.
                    MATURE(K,L,M)= 11
                ELSE
                    FRUTP(K,L,M) = FFRUT(K,L,M)*10.0
                    MATURE(K,L,M)= (DEHISS(K,L,M)-AGEBOL(K,L,M))/10.
                    IF(FRUTP(K,L,M).LT.1.)FRUTP(K,L,M)  = 10.
                    IF(BSIZE(K,L,M).LT.1.)BSIZE(K,L,M)  = 10.
                    IF(MATURE(K,L,M).LT.1)MATURE(K,L,M) = 10
                    IF(FCODE(K,L,M).EQ.1) BSIZE(K,L,M)  = 13.
                    IF(FCODE(K,L,M).EQ.1) MATURE(K,L,M) = 13
                    IF(FCODE(K,L,M).EQ.3) MATURE(K,L,M) = 12
                ENDIF
100 CONTINUE

    RETURN
    END

    
    subroutine fiberQualityParameters()       !FQ

    use common_block
    !BOLWGT(K,L,M)! weight of each boll
    !COTXX !Total boll weight
    !FQS_Plant= plant level quality index

    WRITE(lstng,1700) "VegB","FB","FBN",           &
        "Temp",  "LeafN (g/kg)", "LWP (Mpa)", "Bwt[lb/acre]"
1700 FORMAT (A10,A10,A10,A10, 3(A15))

    DO K = 1,NVBRCH                                                             !Veg
        NBRCH = NFBR(K)
        DO L=1,NBRCH                                                            !Fruting Branch
            NNID = NNOD(K,L)
            DO M=1,NNID                                                         !Node

                !  if (AVGLWP(K,L,M).gt.-1.7 ) AVGLWP(K,L,M)=-1.7                  ! Limits for running avg lwp
                !  if (AVGLWP(K,L,M).lt.-2.5 ) AVGLWP(K,L,M)=-2.5

                !  if (AVGT(K,L,M).lt.18)AVGT(K,L,M)=18.                           ! Limits for running avg temp
                !  if (AVGT(K,L,M).gt.30)AVGT(K,L,M)=30.

                !  if (AVGLEAFN(K,L,M)*1000.lt. 25.) AVGLEAFN(K,L,M)=25./1000.     !AVGLEAFN(K,L,M)*1000 is in gN/kg leafweight
                !  if (AVGLEAFN(K,L,M)*1000.gt. 47.) AVGLEAFN(K,L,M)=47./1000.

                write(lstng,1701)K,L,M, AVGT(K,L,M), AVGLEAFN(K,L,M)*1000,&
                    AVGLWP(K,L,M), OpenBollYield(K,L,M)
1701            FORMAT(6X,I2,10X,I2,7X,I2,4(4x,f9.2) )


                Red_Fac_Strength_LWP=    (1.50 + 0.309* AVGLWP(K,L,M))              !  Reduction factor = f(AVGLWP(K,L,M)/10.0) [MPA]
                Red_Fac_Strength_LeafN =(0.87 + (0.0021*AVGLEAFN(K,L,M)*1000.0))    !  Reduction factor = f(AVGLEAFN(K,L,M)*1000.0) [gN/kg leaf weight]

                if (Red_Fac_Strength_LWP.gt. 1.) Red_Fac_Strength_LWP=1
                if (Red_Fac_Strength_LeafN.gt. 1.) Red_Fac_Strength_LeafN=1
                if (Red_Fac_Strength_LWP.lt. 0.) Red_Fac_Strength_LWP=0
                if (Red_Fac_Strength_LeafN.lt. 0.) Red_Fac_Strength_LeafN=0

                FQS(K,L,M)=                                     &                   ! Fiber strength
                    (21.817 + 0.341* AVGT(K,L,M))*              &                   !  Potential value = f(temp)
                    Red_Fac_Strength_LWP*                      &
                    Red_Fac_Strength_LeafN

                Red_Fac_Length_LWP=    (1.35  + 0.220*AVGLWP(K,L,M))                !  Reduction factor = f(AVGLWP(K,L,M)/10.0) [MPA]
                Red_Fac_Length_LeafN = (0.928 + 0.001*AVGLEAFN(K,L,M)*1000.0)       !  Reduction factor = f(AVGLEAFN(K,L,M)*1000.0) [gN/kg leaf weight]

                if (Red_Fac_Length_LWP.gt. 1.) Red_Fac_Length_LWP=1
                if (Red_Fac_Length_LeafN.gt. 1.) Red_Fac_Length_LeafN=1
                if (Red_Fac_Length_LWP.lt. 0.) Red_Fac_Length_LWP=0
                if (Red_Fac_Length_LeafN.lt. 0.) Red_Fac_Length_LeafN=0

                FQL(K,L,M)=                                     &                   ! Fiber length
                    (11.49 + 1.75*AVGT(K,L,M)                   &
                    -0.04*AVGT(K,L,M)*AVGT(K,L,M))*             &
                    Red_Fac_Length_LWP*                         &
                    Red_Fac_Length_LeafN

                Red_Fac_Micro_LWP=    (0.44-0.203*AVGLWP(K,L,M))                    !  Reduction factor = f(AVGLWP(K,L,M)/10.0) [MPA]
                Red_Fac_Micro_LeafN =(1.157-0.007*AVGLEAFN(K,L,M)*1000.0)           !  Reduction factor = f(AVGLEAFN(K,L,M)*1000.0) [gN/kg leaf weight]

                if (Red_Fac_Micro_LWP.gt. 1.) Red_Fac_Micro_LWP=1
                if (Red_Fac_Micro_LeafN.gt. 1.) Red_Fac_Micro_LeafN=1
                if (Red_Fac_Micro_LWP.lt. 0.) Red_Fac_Micro_LWP=0
                if (Red_Fac_Micro_LeafN.lt. 0.) Red_Fac_Micro_LeafN=0

                FQM(K,L,M)=                                     &                   ! Micronaire
                    (-6.88 + 0.843*AVGT(K,L,M)                  &
                    -0.017* AVGT(K,L,M)*AVGT(K,L,M))*           &
                    Red_Fac_Micro_LWP*                          &
                    Red_Fac_Micro_LeafN

                Red_Fac_Uni_LWP=   (1.16 +  0.098*AVGLWP(K,L,M))                    !  Reduction factor = f(AVGLWP(K,L,M)/10.0) [MPA]
                Red_Fac_Uni_LeafN =(1.005-0.0002*AVGLEAFN(K,L,M)*1000.0)            !  Reduction factor = f(AVGLEAFN(K,L,M)*1000.0) [gN/kg leaf weight]

                if (Red_Fac_Uni_LWP.gt. 1.) Red_Fac_Uni_LWP=1
                if (Red_Fac_Uni_LeafN.gt. 1.) Red_Fac_Uni_LeafN=1
                if (Red_Fac_Uni_LWP.lt. 0.) Red_Fac_Uni_LWP=0
                if (Red_Fac_Uni_LeafN.lt. 0.) Red_Fac_Uni_LeafN=0
                FQU(K,L,M)=                                     &                   ! Uniformity (%)
                    (55.04 + 2.368*AVGT(K,L,M)                  &
                    -0.047 *AVGT(K,L,M)*AVGT(K,L,M))*           &
                    Red_Fac_Uni_LWP*                            &
                    Red_Fac_Uni_LeafN

                FQS_Plant=FQS_Plant+FQS(K,L,M)* OpenBollYield(K,L,M)                !Multiplying by the open boll weight, for doing the mass weighted avg
                FQL_Plant=FQL_Plant+FQL(K,L,M)* OpenBollYield(K,L,M)
                FQM_Plant=FQM_Plant+FQM(K,L,M)* OpenBollYield(K,L,M)
                FQU_Plant=FQU_Plant+FQU(K,L,M)* OpenBollYield(K,L,M)


            end do
        end do
    end do

    !mas of the boll weighted average is estimated for the entire plant
    if (sum(OpenBollYield).gt.0.) then
        FQS_Plant=FQS_Plant/sum(OpenBollYield)                                               !COTXX is the total open boll weight
        FQL_Plant=FQL_Plant/sum(OpenBollYield)
        FQM_Plant=FQM_Plant/sum(OpenBollYield)
        FQU_Plant=FQU_Plant/sum(OpenBollYield)

        Write(lstng,*) 'Fiber_Quality Summary'
        Write(lstng,*) 'Plant_FQ_strength_(g/tex) =', FQS_Plant
        Write(lstng,*) 'Plant_FQ_length_(mm)      =', FQL_Plant
        Write(lstng,*) 'Plant_FQ_Micronare_(-)    =', FQM_Plant
        Write(lstng,*) 'Plant_FQ_Uniformity(%)    =', FQU_Plant

    else
        Write(lstng,*) 'Fiber Quality Summary'
        Write(lstng,*) 'There are no open bolls in the plant'
    end if
    return
    end
