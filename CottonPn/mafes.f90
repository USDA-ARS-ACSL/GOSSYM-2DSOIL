

    subroutine crop()
    use common_block


    !dec$ attributes dllexport:: crop
    !*****************************************************************************
    !
    !mafes version 2004- updated for incorporating 2DSOIL and Gas exchange module
    !
    !           *********************  warning  **********************
    !temperatures must be between 13 and 37oc--will not extrapolate beyond limits
    !           *********************  warning  **********************
    !
    !             this is the application version of mafes for guics
    !
    !*****************************************************************************
   
 
    !***initialise/setting the crop routine----------
    if (linput.eq.1) then
        NumMod=NumMod+1
        ModNum=NumMod
        nPool1=0
        totalDM=0
        potenRootWgt=0
        pcrsd=0
        waterUptake=0
        sumPNet=0
        sumPNetGross=0
        iperd=24
        tCount=1
        transSum=0
        psil_g=-0.8
        idays=0
        RunFlag=1
        totalNitrogenUptake=0
        NDemandError = 0
		CumulativeNDemandError = 0
        NitrogenUptake=0
        startCropSim=Sowing
        stopCropSim=Ending
        EMERG=Sowing
        emergingDay=sowingDay
    !***-------------------------------------------
    !***GOSSYM is simulated from emerging day till end day
        activeCropDays=endDay-emergingDay                       !Total number of cropdays, this includes emerging day also     
        j=1
        do i=emergingDay, endDay
            cropActive(j)=dble(i)+0.9500                        !For calling crop routine at the end of the day
            j=j+1
        end do
        tNext(ModNum)=cropActive(1)                             !Emerging day- first day
        CropRun=1
        
    !***Manage output files from GOSSYM----------
        runMode = 'gossym'                                      
        opsys = ' '
        binFle = 'runtime.g00' 
        errFle = 'runtime.err'  
   !***---------------------------------------------
        
   !*** open main output files from GOSSYM simulations    
        open(21,file=binFle,form='unformatted',status='unknown')!open runtime.g00
        open(77,file=PlantGraphics,status='unknown')            !open.g01 file
        open(22,file=sumryfle,status='unknown')                 !open.sum file
        open(44,file=LeafGraphics,status='unknown')             !open.g02 file to output plant stresses
        call initialize                                         !For initialising variables, profile details,read pgr herbicide routine
    end if
    !***end intialise/setting the crop routine
    
    !***Gasexchange input info-----------------------
    !***The following variables are passed from 2dsoil to gas exchange
      
      cdayofYear = dayofYear                        !day of the year
      ciTime = iTime                                !hour
      ciPerd = 24                                   !24
      cWattsm(ciTime) = wattsm(iTime)               ![watts/m2] !cwattsm: wattsm in each hour
      cPar(ciTime) = par(iTime)                     ![watts/m2]!par: photosynthetically active radiation in each hour
      cTair(ciTime) = tair(iTime)                   !tair:air temperature in each hour
      cco2 = co2
      cVpd(ciTime) = vpd(iTime)                     !cvpd:water vapour pressure deficit
      cWind = wind                                  ![km/hour] cwind: wind for the day
      cPsil_ = psilC                                ![Mpa] cpsil_minimum leaf water potential for the day 
      cLatude = latude
      cLareat = area*100                            !area is in [dm2], clareat: leaf area is in [cm2]
      cLai=lai                                      !leaf area index

    !***-----------switch for nshoot--------------------
    if ((nShoot .eq. 0) .and. abs(time-(emergingDay)).lt.0.001)then
        nShoot=1                                    !Crop starts=emergence day.
        isEmerged=1
    end if

    !***---------------------------------------------------
    !from gas exchange we get transpiration [mmolh2o m-2 ground s-1] and pgr gross photosynthesis [mg co2/m2 leafarea/sec] 
    if (nShoot.eq.1) call GasExchange
    !*** get minimum of PSIL for the day-------
    if (nShoot.eq.1) then
        IF ( iTime.eq.tCount) then
            Psilh(iTime)=PsilC                      !PsilC: Leaf water potential calcualated with the same concept in the old GOSSYM
            canopTemp(iTime)=temperature            !Leaf temperature form Gas Exchange
            StomCond(iTime)=stomConduc              !Stomatal conductance: mmol H2o m-2 s-1 ( for a day , need to average)
            photoFluxDen(iTime)=par(iTime) * 4.55   !conversion from PAR in W m-2 to umol s-1 m-2.Photosynthetic flux density: umol/m2/sec
            call Add_output1                        !For writing additional output to cotton.sum file
            tcount=tcount+1
            if (tcount.gt.ciPerd)  tcount=1
        end if
    end if
   
    
    !***----------if plant is emerged, the following variables from 2dsoil should be processed over a day and then use in cotton which runs on a daily interval
    if (nShoot.eq.1) then
        !pgr [mg co2/m2 /sec]
        !adding the pnet to sum for a day
        !/100 : convert from m2 to dm2 leaf area
        !*86400 convert step from day to sec
        !/lai converts from dm2 of leaf area to dm2 of ground area
        sumPNet=sumPNet+(pgr*86400*step/(lai*100))                      !sumPNet [mgco2/dm2 ground area/day]
        sumPNetGross=sumPNetGross+(pgr*86400*step)                      !sumPNetGross [mgco2/m2/day]
        
        !transpiration from gas ex [mmolh2o m-2 ground s-1]
        !0.018 converts mmol h20 to g
        !/10000 converts per m2 to cm2
        !*86400 converts /sec to /day
        Et_Demand=Transpiration*0.018*86400/10000                       !g/cm2/day 
        
        !npool: total nitrogen available for the growth (from 2d soil) [g of nitrogen/plant]
        !sincrsink mass unit is [micro g of nitrogen], npool is in [g of nitrogen/plant]
        !sincrsink in solupt_y1.for is sum on all cells in a time step (is already *step)
        !conversion from nitrate to nitrogen is done already in solute_y1.for
        !npool1 [gram/slab/day]
        nPool1=nPool1+(SIncrSink*1.0e-6)                                !*1.0e-6 [micro g to g]

        !spdwr: sum of potential delta weight of roots over all cells, [gms]
        !potenRootWgt:sum of spdwr over a day [grams/slab/day]
        potenRootWgt=potenRootWgt+spdwr*step

        !dailycarboused(pcrsd):dailycarboused(pcrsd)+pcrs*step
        pcrsd=pcrsd+pcrs*step                                           ![gram/slab/day]

        !actual water uptake accumulated for a day                      !This is not used in GOSSYM Crop routine
        waterUptake = waterUptake + awups*step                          ![gram/slab/day]
        totalNitrogenUptake=totalNitrogenUptake+(SIncrSink*1.0e-6)      !gram/slab Total over the crop duration
        NitrogenUptake=nPool1                                           !For the current day gram/slab 
       

    end if
    !---------------------------------------------
    !-----------daily loop  growth routine from emerging day till end day
    if(abs(time -tNext(ModNum)).lt.0.001*step)  then
            psil_g=minval(psilh)                                        !minimum psil of the day
            if(psil_g.lt.-3.5) psil_g=-3.5
            if(psil_g.ge.-0.8) psil_g = -0.8
            avgCanopTemp=sum(canopTemp)/ciPerd 
            if (nShoot.eq.1)then                                        
            DayNum = emerge+iDay                                        !julian day
            iDay=iDay+1                                                 !starts at 1 on the day of emergence
            psiAvg=amax1(psiAvg1,-7.0)
            rootWt=totalRootWeight/popslab                                          ![gram/plant/day] rootwt: dry weightof all the living roots per plant
            CurrentNUptakeError=(NitrogeNUptake/popslab)- (NitroDemand/1.0e6/popslab)                !g/plant (nitrogen uptake is only for a day)
            CumulativeNUptakeError = CumulativeNUptakeError + CurrentNUptakeError                    !g/plant (unit is confusing (how is it used in plant regulation)
       
            !-----plant growth simulations-----------
            call clymat                                                 !Effect of rain on polination 
            call gosloop
            !----------------------------------------
            potenRootWgt=0                                              !potential weight of roots is accumulated for a day
            nPool1=0
            pcrsd=0                                                     !pcrsd accumulates pcrs for each times steps to a day
            psiAvg=0
            psil_g=0
            sumPNet=0
            sumPNetGross=0
            waterUptake=0
            NitrogenUptake=0
            NDemandError = CurrentNUptakeError
            CumulativeNDemandError = CumulativeNUptakeError
        end if

        CropRun=CropRun+1
        if (CropRun.gt.(activeCropDays+1))  RunFlag=0.  !Maturity concept in cotton check
        
        if(CropRun.gt.(activeCropDays+1))then
            tNext(ModNum)=1.e+32                                        !this stopes the daily cotton loop
            nShoot=0
            call printout                                               !6 output table print, print summary (GOSSYM title), and pre-print (final short summary)
            close(1)
            close(21,status='delete')                                   !Temporary file 
            close(22)                                                   !.sum summary file
            close(77)                                                   !.g01
            close(lstng)                                                !.out output file
        else
            tNext(ModNum)=cropActive(CropRun)                         !takes to the next day
        end if

    end if
    !-----------daily loop from emerging day to crop end day ends here------------------------

    return
    end





