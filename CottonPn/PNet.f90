   SUBROUTINE PNET
    ! ************************************************************
    ! *                                                          *
    ! *                    PNET  SUBROUTINE                      *
    ! *                                     Reddy's new PNET     *
    ! ************************************************************

    use common_block
    
    ! *** Nitrogen effect on PNET from unpublished data collected in
    ! *** 1992 from pad experiment by K.R. Reddy (PIMA S-6)  3/10/2000
    ! Gas exchange routine does not account for the nitrogen stress on the photosynthesis. Therefore
    ! the same routine present in the original GOSSYM is added here to calculate Nstress on photosynthesis. 

    LEAFCN = leafcn * 100.0
    if(leafcn.ge.4.5)leafcn = 4.5
    XNIRED = -0.4029+0.5954*LEAFCN-0.0630*LEAFCN**2
    IF(XNIRED.GT.1.0) XNIRED = 1.0
    IF(XNIRED.LT.0.2) XNIRED = 0.2

    stress_index = amax1(0.0,xnired)
    IF(stress_index.lt.0.15) stress_index = 0.15
    IF(stress_index.GT.1.) stress_index = 1.0

    !PSTAND: GROSS DAILY PHOTOSYNTHATE PRODUCTION (MG CO2/dm**2/DAY)
    !PPLANT = PSTAND * INT *POPFAC *stress_index *0.001*PNETCOR (unit is gramsco2 /plant)
    !INT=INTERCEPTION (FRACTION OF INCIDENT RS)
    !POPFAC: POPULATION FACTOR
    PSTAND=sumPNet                                                      !sumpnet: gross photosynthesis for a day [mg co2/dm2/day]
    PPLANT= PSTAND * INT *POPFAC *0.001*stress_index                                 !(unit is grams co2 /plant)                            
    !VALUES BASED ON DATA OF HARPER ET. AL. (1973) CARBON DIOXIDE AND
    !THE PHOTOSYNTHESIS OF FIELD CROPS.  A METERED CARBON DIOXIDE
    !RELEASE IN COTTON UNDER FIELD CONDITIONS.  AGRON. JOUR. 65:7-11.
    !AND ON BAKER (1965)  EFFECTS OF CERTAIN ENVIRONMENTAL FACTORS
    !ON NET ASSIMILATION IN COTTON.  CROP SCI. 5:53-56. FIG 5.

    
    IF(CO2.GE.0)PPLANT=PPLANT*1.405 !check this, In both old and new GOSSSYM this is kept as it is. 
 
    ! CO2 IS A FERTILIZATION TRIGGER.WHEN CO2 IS EQUAL TO 1,PPLANT IS
    ! INCREASED 40.5 % DUE TO  500 PPM CO2 CONCENTRATION.
    ! This part is not clear. CO2 is a input in ppm everywhere else, 6/1/06, SK

    RSUBL=0.0032125+0.0066875*TDAY          !Light respiration coeff- is a function of temperature 
    LYTRES = RSUBL*PPLANT                   !Light respiration- is propotiosnal to the photosynthate production
    BMAIN=(PLANTW-COTXX)*RSUBO              !Maintanance respiration is propotional to the green biomass
    PTS=PPLANT-LYTRES-BMAIN                 !g co2 per plant
    
     
    IF(PTS.LE..01)PTS=.01
    PN= (PTS/(1.+GSUBR) * 0.68182) * PIXDPN !convert to dry matter
    SPN = SPN + PN                          !  0.68182 CONVERTS CO2 TO CH2O
         
    RETURN
    END