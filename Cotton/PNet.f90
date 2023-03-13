   SUBROUTINE PNET
    ! ************************************************************
    ! *                                                          *
    ! *                    PNET  SUBROUTINE                      *
    ! *                                     Reddy's new PNET     *
    ! ************************************************************

    use common_block


    !check if the follwing 2 lines are required
     IF(PSIL_G.LT.-3.5) PSIL_G=-3.5
     IF(PSIL_G.GE.-0.8) PSIL_G = -0.8
    

    !  DATA LEADING TO THIS PTSRED ARE FROM CONTROLLED ENVIRONMENT UNITS
    !  (SPAR) THE EFFECT OF WATER STRESS ON CANOPY SENESCENCE AND
    !  APPARENT PHOTOSYNTHESIS IN COTTON (MARINI,BAKER,REDDY,AND McKINION
    !  CROP SCI 25:798-802 (1985). PSTAND, RSUBL, RSUBO, GSUBR FROM
    !  BAKER ET. AL. (1972). SIMULATION OF GROWTH AND YIELD IN COTTON:
    !  I. GROSS PHOTOSYNTHESIS, RESPIRATION AND GROWTH. CROP SCI.12:431-435.

    ! *** Raja's new equation on the effect of water stress; r2 = 0.63   4/20/2001

    stdpsild = -0.8
    PnIndex = (4.7929 - 0.1318*stdpsild - 0.3319*stdpsild**2)!Well watered state
    PnCurrent =  (4.7929 - 0.1318*psil_G - 0.3319*psil_G**2)!Based on actual leaf water pot.
    ptsred = PnCurrent/PnIndex
    ptsred = PnCurrent/PnIndex * calbrt(23)
    IF(PTSRED.GT.1.0) PTSRED = 1.0 !PTSRED: Photosynthesis reduction term for water stress
    IF(PTSRED.LT.0.2) PTSRED = 0.2

    ! *** Nitrogen effect on PNET from unpublished data collected in
    ! *** 1992 from pad experiment by K.R. Reddy (PIMA S-6)  3/10/2000

    LEAFCN = leafcn * 100.0   !LeafCN: Leaf nitrogen concentration
    if(leafcn.ge.4.5)leafcn = 4.5
    XNIRED = -0.4029+0.5954*LEAFCN-0.0630*LEAFCN**2
    IF(XNIRED.GT.1.0) XNIRED = 1.0
    IF(XNIRED.LT.0.2) XNIRED = 0.2
            
    stress_index = amax1(0.0,ptsred*xnired)
    IF(stress_index.lt.0.15) stress_index = 0.15
    IF(stress_index.GT.1.) stress_index = 1.0

    !  PSTAND: GROSS DAILY PHOTOSYNTHATE PRODUCTION (MG CO2/dm**2/DAY)
    !  PPLANT: GROSS PHOTOSYNTHATE PRODUCED/PLANT TODAY
    !  POPFAC: POPULATION FACTOR
    !  PNETCOR: CARBONDIOXIDE CORRECTION FACTOR FOR PHOTOSYNTHESIS
    !  WATTSM: Incident radiation in watts/SQm  
    PSTAND =(2.3908 + WATTSMc*(1.37379 - WATTSMc*0.00054136))*calbrt(24)
    PPLANT = PSTAND * INT *POPFAC *stress_index *0.001 *PNETCOR    !(unit is grams/plant)
      !(unit is grams/plant)

    !  VALUES BASED ON DATA OF HARPER ET. AL. (1973) CARBON DIOXIDE AND
    !  THE PHOTOSYNTHESIS OF FIELD CROPS.  A METERED CARBON DIOXIDE
    !  RELEASE IN COTTON UNDER FIELD CONDITIONS.  AGRON. JOUR. 65:7-11.
    !  AND ON BAKER (1965)  EFFECTS OF CERTAIN ENVIRONMENTAL FACTORS
    !  ON NET ASSIMILATION IN COTTON.  CROP SCI. 5:53-56. FIG 5.

    IF(CO2.EQ.1)PPLANT=PPLANT*1.405 !check this

    ! CO2 IS A FERTILIZATION TRIGGER.WHEN CO2 IS EQUAL TO 1,PPLANT IS
    ! INCREASED 40.5 % DUE TO  500 PPM CO2 CONCENTRATION.
    ! This part is not clear. CO2 is a input in ppm everywhere else, 6/1/06, SK

    RSUBL=0.0032125+0.0066875*TDAY !Light reapiration coeff
    LYTRES = RSUBL*PPLANT   !Light respiration
    BMAIN=(PLANTW-COTXX)*RSUBO !Maintanance respiration
    PTS=PPLANT-LYTRES-BMAIN 
    !pts= unit(LANGLEYS/DAY)
    
    IF(PTS.LE..01)PTS=.01
    PN= (PTS/(1.+GSUBR) * 0.68182) * PIXDPN !convert to dry matter
    SPN = SPN + PN
    
    !  0.68182 CONVERTS CO2 TO CH2O

    RETURN
    END