    SUBROUTINE NUMCHR(FILNAM,NUMBR)
    CHARACTER FILNAM*13
    I=0
80  CONTINUE
    I=I+1
    IF(FILNAM(I:I).EQ.CHAR(0)) THEN
        DO 90 J=I,13
            FILNAM(J:J)=' '
90      CONTINUE
    ENDIF
    IF(I.LT.13) GO TO 80
    I=0
    NUMBR=1
100 CONTINUE
    I=I+1
    IF((FILNAM(I+1:I+1).EQ.'.').OR.(FILNAM(I+1:I+1).EQ.' ')) THEN
        NUMBR=I
    ELSE
        IF(I.LT.9) GO TO 100
        NUMBR=8
    ENDIF
    RETURN
    END



    SUBROUTINE PrintSummary
    !To write the summary at the end of .out and .sum output files

    use common_block


1040 FORMAT(/	&
        4X,'Emergence Date..... ',I3,'/',I2,'/',I4,/	&
        4X,'Harvest Date....... ',I3,'/',I2,'/',I4,/	& 
        4X,'Variety............ ',A14,/)
1120 FORMAT('1',25X,'MAFES-GOSSYM (TM)  SUMMARY', /	&
        30X,'COPYRIGHT (C)  2003' / &
        14X,'Mississippi Agriculture & Forestry Experiment Station' /	&
        18X,'Contact:  Dr. K.R. Reddy and David W. Brand' /	&
        32X,'(662) 325-9462'/)
1140 FORMAT(/													 &
        '                            PLANT           LIGHT', &
        '         GREEN   OPEN' /								 &
        '    EVENT         DATE DAE HEIGHT NODES LAI  INT.', &
        ' SQARS  BOLLS  BOLLS YIELD' /							 &
        '                            (in)             (%) ', &
        ' (-- 1000''S/ACRE --) (b/a)' / )
1150 FORMAT(25x,'Soil Dispersion Trigger   ',i5,/)

   ! OPEN(22,FILE=sumryfle,STATUS='UNKNOWN')
    WRITE(22,1120)

    ! ***	get the emergence,start and stop dates

    CALL JULANTOCAL(MO,DAZE,IYEAR,emerge)
    IMoemg = MO
    IDyemg= DAZE
    iyremg= iyear
    CALL JULANTOCAL(MO,DAZE,IYEAR,jdstrs)
    IMostr = MO
    IDystr = DAZE
    iyrstr = iyear
    CALL JULANTOCAL(MO,DAZE,IYEAR,jdstps)
    IMostp = MO
    IDystp = DAZE
    iyrstp = iyear
    WRITE(22,1040) imoEMG,idyemg,iyremg, &
        imostp,idystp,iyrstp, &
        VARITY(IVARTY)

    WRITE(22,1140)
    WRITE(lstng,1040) imoEMG,idyemg,iyremg, &
        imostp,idystp,iyrstp, &
        VARITY(IVARTY)
    WRITE(lstng,1140)
    RETURN
    END


    SUBROUTINE WRITERR
    !*** for writing error msg in runtime.err file
    use common_block

    CHARACTER*1 DUMBO

1000 FORMAT(A80)
    OPEN(23,FILE=ERRFLE,STATUS='UNKNOWN')
100 CONTINUE
    READ(23,*,END=200,ERR=100) DUMBO                
    GO TO 100
200 CONTINUE                                        !Read previous lines
    BACKSPACE(23)
    WRITE(23,1000) PRINTBUF                         !Error command
    CLOSE(23)
    RETURN
    END


    SUBROUTINE COUTPUT
    !                          O U T P U T
    !  ****************************************************************
    !  *   COLLECTION OF WRITE STATEMENTS FOR OUTPUT OF MODEL RESULTS *
    !  ****************************************************************

    use common_block
integer  TimeHour
Real RIO
Character*15 remarks
1000 FORMAT(' ** FIRST SQUARE ON ',I2.2,'/',I2.2,' **')
1020 FORMAT(11X,' YIELD BALES/ACRE   YIELD LBS/ACRE')
1030 FORMAT(17X,F4.2,13X,F7.0)
1040 FORMAT(' ** FIRST BLOOM ON ',I2.2,'/',I2.2,' **')
1050 FORMAT(1X,I3,1X,I2,'/',I2.2,F7.1,F6.2,I7,2X,I8,1X,3I8,F7.2,I8)
1060 FORMAT('1',19X,'GENERAL OUTPUT IN ENGLISH UNITS')
1065 FORMAT(17X,'PER/PLANT',27X,'PER/ACRE')
1070 FORMAT(12X,18('-'),5X,45('-'))
1080 FORMAT(' DAE  DATE  HEIGHT  LAI  NODES     SITES',		 &
        '  SQUARES   GREEN   OPEN   YIELD   FRUIT')
1090 FORMAT(13X,'(IN)',35X,'BOLLS   BOLLS',10X,' SHED')
1100 FORMAT(' ')
1110 FORMAT(' **',F6.1,' LBS OF NITROGEN APPLIED ON ',			 &
        I2.2,'/',I2.2,' **')
1120 FORMAT(' **',F6.2,' INCHES OF IRRIGATION APPLIED ON ',	 &
        I2.2,'/',I2.2,' **')
1140 FORMAT(' FIRST SQUARE  ',2X,I2.2,'/',I2.2,I5,F7.1,I7,		 &
        2F6.1,3I7,F6.2)
1180 FORMAT(' ** LAST ACTUAL WEATHER ',I2.2,'/',I2.2,'**')
1200 FORMAT(' LAST ACT WTHER',2X,I2.2,'/',I2.2,I5,F7.1,I7,		 &
        2F6.1,3I7,F6.2)
1220 FORMAT(' FIRST BLOOM   ',2X,I2.2,'/',I2.2,I5,F7.1,I7,		 &
        2F6.1,3I7,F6.2)
1300 FORMAT(' ** PREP APPLIED ON ',I2.2,'/',I2.2,'**')
1320 FORMAT(' ** DEFOLIANT APPLIED ON ',I2.2,'/',I2.2,'**')
1340 FORMAT(' ** ',F6.1,' OZS PIX APPLIED ON ',I2.2,'/',I2.2,' **')
1360 FORMAT(' ** PLANT MAP ADJUSTMENT ',I2.2,'/',I2.2,			 &
        ' USED',I5,' PLANTS **')
1380 FORMAT(' ** PLANT HT. ADJUSTMENT ',I2.2,'/',I2.2,			 &
        ' USED',I5,' PLANTS **')
1400 FORMAT(' ** SIMULATED CROP MAY BE IMMATURE. ',              &
        'YOU MAY WANT TO TRY A LATER STOP DATE. **')
1420 FORMAT(' ** TEMPERATURE FELL BELOW 30 F.  IT IS OVER. **')
1440 FORMAT(' ** TEMPERATURE FELL BELOW 32 F. **')
1460 FORMAT(' ** Oh Boy - Leaf Weight Went To Zero. **')


 
 !For classim output .G01============= 
      if (daynum.eq.emerge) then
            
         WRITE(77,1924) "jday", "date", "time", "PlantH", "LAI", "LInt",& 
        "Nodes", "Sites", "N_Squares", "N_GB", "N_OB",& 
        "NLvsLoss", "NSqLoss", "NBollsLoss", "NFruitShed",&
        "PetShd_DM", "GB_lossDM", "Lf_lossDM", "Rt_lossDM",&
        "Dd_WtDM", "SquareDM", "GB_DM", "OB_DM", "LeafDM",&
        "StemDM", "RootDM", "ResC", "PlantDM", "R_S",& 
        "Yield", "Temp", "L_Temp", "Rain", "SRad", "PFD",&
        "RH", "LeafN", "StemN", "SeedN", "BurrN", "RootN",&
        "Nloss", "PlantN", "N_uptake", "S_Psi", "L_Psi",&
        "LArea", "VPD", "StCond", "Pnet", "PGross",&
        "L_Res", "Main_Res", "Resp", "SPnet", "C_Bal",&
        "Nstress_Pn", "Note"
         
1924     FORMAT (A10, 57(",",A12))

!For classim output .G02===========

         WRITE(44,1969) "jday","date","time","W_stress","N_Veg_Str",           &
             "N_Fru_Str",  "N_Rt_Str","C_Stress"
1969     FORMAT (A10, 7(",",A15))

     end if 
     
    TimeHour=23
    C00 = Z / 2.54   ! change the units from inches to cm
    C01 = INT * 100.
    ! C02 = PIXCON * 1000000
    C02 = PIXCON * 1000
    !C03 = PSIL_G / .101325                         !  leaf water potential [atm] (bar = atm * 1.01325)
    C03 = PSIL_G                                    !  leaf water potential [atm] (bar = atm * 1.01325)
    N00 = NUMPFN + NFBR(1)
    I00=SQRZ*POPPLT/1000+.5
    I01=GBZ2*POPPLT/1000+.5
    I02=NOPEN*POPPLT/1000+.5
    AVAILN = AVAILN/.0448
    DAILYN=SUPNO3+SUPNH4+(LEFABS-PLEFABS)*.005
    PLEFABS=LEFABS
    FRTNUSE=.045*.416*SDWBOL+.006*.278*SDWBOL
    VEGNUSE=.009*SDWSTM+.015*SDWLEF+.019*RCH2O
    DYNEXS=(POPPLT/454)*(DAILYN-FRTNUSE-VEGNUSE)
    kKday=kday-1
   ! PSIM=PSIAVG

    WRITE(21,ERR=300) IDAY,KDAY,YIELD,NF,WSTRSD,C00,RI,TMAX,TMIN,SQRZ  &
        ,RAIN,FERN,GBZ2,C03,NV,LAI,SITEZ,NOPEN,FLOSS,ABZ			   &
        ,CSTRES,PSIAVG,TAVG,TDAY,TNYT,C01,WIND,ES,EP				   &
        ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW	       &
        ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT		   &
        ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,C02,NUMPFN			       &
        ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4			   &
        ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
300     CONTINUE

        !***All the information that is printed in the g01 file***---    
        
!jday	Julian day	Day starting from 1 on day 1 of the year        [daynum]
!date	Date	Calender date                                       [MO,DAZE,IYEAR]
!time
!PlantH	Plant height	cm                                          [A01]
!LAI	Leaf area index	_                                           [LAI]
!LInt	Canopy light interception	_                               [INT]
!Nodes	Number of main stem nodes	_                               [N00]
!Sites	Number of fruiting sites	_                               [m01]
!N_Squares	Number of squares	_                                   [m02]
!N_GB	Number of green bolls	_                                   [m03]
!N_OB	Number of open bolls	_                                   [m04]
!NLvsLoss	Number of leaves lost	_                               [LVSLOS]
!NSqLoss	Number of squares lost	_                               [SQABZ]
!NBollsLoss	Number of bolls lost	_                               [BOLABZ]
!NFruitShed	Total number of abscissed fruits	_                   [ABZ]
!PetalShed_DM	Petal shed after blooming	gC/plant                [pqflr]
!GB_lossDM	Green bolls lost	gC/plant                            [GBLOS]
!Leaf_lossDM	Weight of leaves abscissed	gC/plant                [LEFABS]
!Root_lossDM	Weight of root lost	gC/plant                        [RUTOFF]
!Dead_WtDM	Total weight of dead tissue lost	gC/plant            [DEADWT]
!SquareDM	Squares dry matter	gC/plant                            [SQWT]
!GB_DM	Green boll dry matter	gC/plant                            [gbolwt]
!OB_DM	Open boll dry matter	gC/plant                            [cotxx]
!LeafDM	Leaf dry matter	gC/plant                                    [leafwt]
!StemDM	Stem dry matter	gC/plant                                    [stemwt]
!RootDM	Root dry matter	gC/plant                                    [rootwt]
!ResC	Reserved dry matter	gC/plant                                [Resc]
!PlantDM	Total plant dry matter	gC/plant                        [PlantW]
!Root_Shoot	Root shoot ratio	_                                   [ROOTWT/(STEMWT+LEAFWT)]
!Yield	Total yield	lb/acre                                         [Yield_lbs_ac]
!Temp	Average temperature	DegreeC                                 [tavg]
!L_Temp	Average leaf temperature	DegreeC                         [avgCanopTemp]
!Rain	Rain+irrigation	mm/day                                      [RAIN]
!SRad	Solar radiation	W/m2                                        [RIO]
!PFD	Photosynthetic flux density	mol photons/day/m2              [sum(photoFluxDen)*3600/1000000]
!RH	Relative humidity	%                                           [RH_D*100]                         
!LeafN	Leaf nitrogen	gN/plant                                    [SLEAFN]
!StemN	Stem nitrogen	gN/plant                                    [STEMN]
!SeedN	seed nitrogen	gN/plant                                    [SEEDN]
!BurrN	Burr nitrogen	gN/plant                                    [BURRN]
!RootN	Root nitrogen	gN/plant                                    [ROOTN]   
!Nloss	Nitrogen lost abscission 	gN/plant                        [NLOSS]
!PlantN	Total plant nitrogen	gN/plant                            [PLANTN]
!N_uptake	Nitrogen uptake	gN/plant                                [NitrogeNUptake/popslab]
!S_Psi	Average soil water potential in the root zone	bar         [psiavg]
!L_Psi	Leaf water potential	bar                                 [psil_g]
!LArea	Leaf area 	cm2                                             [area*100]
!VPD	Vapour pressure deficit	Kpa                                 [sum(vpd)/ciPerd]
!StCond	Stomatal conductance	micro-mol/m2-s                      [sum(StomCond)/ciPerd]
!Pnet	Net photosynthesis	gC/plant                                [Pn]                               
!PGross	Gross photosythesis	gC/plant                                [PPLANT]
!L_Res	Light respiration	gC/plant                                [LYTRES]
!Main_Res	Maintanance respiration	gC/plant                        [BMAIN]
!Resp	Total respiration 	gC/plant                                [LYTRES+BMAIN]
!SPnet	Cumulative net photosynthesis	gC/plant                    [spn]
!C_Bal	Plant C balance	gC/plant                                    [CHOBAL]
!Nstress_Pnet	Nitrogen stress on the photosynthesis	_           [stress_index]
!Note	Note on first day of square, bloom and open boll	_       [remarks]
        
        
        A01=Z   !Z is the plant height in cm
        N00=NUMPFN+NFBR(1) !Total number of prefrutingnodesand fruting branches on the vegitative branch
        !numpfn: NUMBER OF PREfRUITING NODES
        !NFBR(K):  NUMBER OF fRUITING BRANCHES ON THE VEGETATIVE BRANCH
        !Sitez, Sqrz, GBz2, Nopen etc are per plant
        m01=IFIX(SITEZ)  !Per plant
        m02=IFIX(SQRZ)
        m03=IFIX(GBZ2)
   
       
        m04=IFIX(NOPEN)
        m05=IFIX(ABZ)
        m051=IFIX(LVSLOS)
        m052=IFIX(SQABZT)
        m053=IFIX(BOLABZT)
        m054=IFIX(BOLABZT)+IFIX(SQABZT)
        
        !yield: yield of 500 lb. bales/acre
        Yield_lbs_ac=YIELD*500.  !Total yield in lb/acre
        !TAVG=TAVG/5.*9.+32. !Celcius to fahrenheit
        RAIN=RAIN*10  !rain is in cm/day, here its converting to mm/day
        RIO= RI/41868   !ly/day   !in soil 2d Ri is in Joule/m2 so we need to / by 41868 to get in W/m2
        SUPNO3= NPOOL1/popslab !Total Nitrate-N g/day/plant  !This is the sum of both ammonium N and Nitrate N
        SUPNH4=0.
        
        CALL JULANTOCAL(MO,DAZE,IYEAR,Daynum)
        if (Daynum.eq.ISQday) then
            remarks="1stSquare"
        else if (Daynum.eq.iflday) then
            remarks="1stBloom"
        else if (Daynum.eq. iobday) then
            remarks="1stOpenBoll"
        else 
            remarks="None"
        end if


          WRITE(77,1925) daynum,	MO,DAZE,IYEAR,TimeHour,	A01,LAI,INT,N00,m01,	m02,	&
            m03,	m04,	m051, m052, m053, m054,	pqflr,	GBLOS,	&
            LEFABS,	RUTOFF,	DEADWT,	SQWT,	gbolwt,	cotxx,	leafwt,	stemwt,	rootwt,	&
            Resc,	PlantW,	ROOTWT/(STEMWT+LEAFWT),	Yield_lbs_ac*1.12085105,	tavg,&	
            avgCanopTemp,	RAIN,	RIO,	sum(photoFluxDen)*3600/1000000,	&
            RH_D*100,	SLEAFN,	STEMN,	SEEDN,	BURRN,	ROOTN,	NLOSS,	PLANTN,	&
            totalNitrogenUptake/popslab,	psiavg,	psil_g*10,	area*100,	sum(vpd)/ciPerd,&	
            sum(StomCond)/ciPerd,	Pn,	PPLANT,	LYTRES,	BMAIN,	LYTRES+BMAIN,&	
            spn,	CHOBAL,	stress_index,	remarks

1925        FORMAT(1X,I8,",",1X,I2,'/',I2.2,'/',I4.4,(",",I8),3(",",f9.2),5(",",I8),&
            4(",",I8), 42(",",f9.2),",",A20)        
            
!For classim output G02, all stresses are added here
        WRITE(44,1970) daynum,MO,DAZE,IYEAR,TimeHour, WSTRSD,  nv, nf, nr,           &
            cstres
1970    FORMAT(1X,I8,",",1X,I2,'/',I2.2,'/',I4.4,(",",I8),5(",",F10.3) )
        !Multiply these with population per acre to get the count in acre
        N01=IFIX(SITEZ*POPPLT)    !total fruting sites, POpplt: plants/acre
        N02=IFIX(SQRZ*POPPLT)     !Total number of squares
        N03=IFIX(GBZ2*POPPLT)     !GREEN BOLLS PAST ABSCISSION AGE
        N04=IFIX(NOPEN*POPPLT)    !Number of open bolla
        N05=IFIX(ABZ*POPPLT)      !Total abscissed fruit/acre
        m06=IFIX(abzb*POPPLT)
            
            
!-----------For debug 
              
        !WRITE(77,1925) daynum,MO,DAZE,IYEAR,A01,LAI,N00,m01,m02,m03,m04,    &
        !    SQWT,gbolwt,cotxx,leafwt,stemwt,rootwt,Yield_lbs_ac,                  &
        !    tavg,rain,rio,wstrsd,supno3,supnh4,psiavg,psil_g,               &
        !    cstres, nv, nf, nr, WSTRSD,area/100,leafcn, Pn,Spn,remarks,     &
        !    LVSLOS,SQLOSS(DAYNUM), BOLOSS(DAYNUM), pixcon,pixlos,pixplt,    &
        !    pixdz,pixda,pixdpn,tday, tnyt,daytym,tavg,int 
        ! 

        !SUPNO3 and SUPNH4 we dont consider it seperatly. Hence This should be Total N/plant passed from soil to plant
        !A01=   !Z is the plant height in inch
        !Sqwt: total weight of squares on a plant
        !gbolwt: green boll weight in a plant
        !Cotxx: weight of open bolls
        !Leafwt: leaf weight in grams per plant
        !Stemwt: Weight of stem 
        !RI: incident solar radiation  in cal/cm2/day  !!!!Joules/m2 (as it is passed from 2Dsoil)
        !WSTRSD: WATER STRESS DAY. fRACTION OF DAY TIME PERIOD DURING 
        !WHICH LEAF IS TURGID ENOUGH (ABOVE -7 BARS) FOR GROWTH
        !SUPno3: nitrate taken up from the soil cell
        !leafcn: SLEAFN / LEAFWT  gN/g leaf weight
        !Pn: Net photsyn gC/plant
        !area: dm2/100 gives m2
        !remarks: Comment
        
        
        
        !1925    FORMAT(1X,I8,",",1X,I2,'/',I2.2,'/',I4.4,2(",",F6.2),5(",",I4),6(",",F7.2),&
        !        (",",F9.2),2(",",F8.2),(",",I5),14(",",f10.3),",",2x,A15,14(",",f10.3))

        ! TEST BEGIN
        IF((IDAY.EQ.1).OR.(IDAY/NFRQ*NFRQ.EQ.IDAY)) THEN
            IF(FCODE(1,1,1).NE.0.AND.NPC.GT.0) CALL COTPLT(1)
            IF(NPN.GT.0) CALL OUT(VNO3C,TNNO3,1)
            IF(NPN.GT.0) CALL OUT(VNH4C,TNNH4,1)
            IF(NPW.GT.0) CALL OUT(VH2OC,TH2O,2)
            IF(NPR.GT.0) CALL OUT(ROOTSV,ROOTWT,3)
            IF(NPP.GT.0) CALL OUT(PSIS,PSIAVG,4)
            IF(NPT.GT.0) CALL OUT(SOILT,TSOLAV(2),5)
        ENDIF
        ! TEST END


        LINE=LINE+1

    IF(DAYNUM.GE.JDSTPS) SEND=.TRUE.
    IF(LEAFWT.LE.0.) SEND=.TRUE.
    IF((CLIMAT(DAYNUM,3).LT.30.).AND.(DAYNUM.GT.EMERGE)) SEND=.TRUE.

    RETURN
    END


    SUBROUTINE COTPLT(MTYPE)
    !  ************************************************************
    !  *                                                          *
    !  *  THIS SUBROUTINE PLOTS THE COTTON PLANT WITH THE         *
    !  *  CORRESPONDING SYMBOLS IN THE CORRECT LOCATIONS.         *
    !  *  SQUARE-X             OLDER GREEN BOLL-*    OPEN BOLL-$  *
    !  *  YOUNG GREEN BOLL-B   ABSCISED-O                         *
    !  *                                       Original Code      *
    !  ************************************************************
    !
    use common_block
    !
95  FORMAT(12X,'AVERAGE PLANT FRUIT CODE ',I2.2,'/',I2.2				 &
        /,5X,'*  X  SQUARE          B  BLOOM OR YOUNG GREEN BOLL*'/	 &
        5X,'*  *  SET GREEN BOLL  $  MATURE BOLL              *'/	 &
        5X,'*  A  ABSCISED FRUIT  I  MONOPODIAL NODE          *'/	 &
        5X,'***************************************************'/)
96  FORMAT(19X,'FRUIT CODE MAP ',I2.2,'/',I2.2						 &
        /,5X,'*  X  SQUARE          B  BLOOM OR YOUNG GREEN BOLL*'/	 &
        5X,'*  *  SET GREEN BOLL  $  MATURE BOLL              *'/	 &
        5X,'*  A  ABSCISED FRUIT  I  MONOPODIAL NODE          *'/	 &
        5X,'***************************************************'/)
97  FORMAT(10X,'PERCENTAGE OF FRUIT BY POSITION ',I2.2,'/',I2.2		 &
        /,5X,'*      A -->  ABSCISED                            *'/	 &
        5X,'*      0 -->  0 -  9 %      1 --> 10 -  19 %      *'/	 &
        5X,'*      2 --> 20 - 29 %      3 --> 30 -  39 %      *'/	 &
        5X,'*      4 --> 40 - 49 %      5 --> 50 -  59 %      *'/	 &
        5X,'*      6 --> 60 - 69 %      7 --> 70 -  79 %      *'/	 &
        5X,'*      8 --> 80 - 89 %      9 --> 90 - 100 %      *'/	 &
        5X,'***************************************************'/	 &
        '  ' / '  ')
98  FORMAT(14X,'BOLL WEIGHT BY POSITION ',I2.2,'/',I2.2				 &
        /,5X,'*      A -->  ABSCISED      X -->  SQUARE         *'/	 &
        5X,'*      0 -->  0.0 - 0.9 g   1 -->  1.0 - 1.9 g    *'/	 &
        5X,'*      2 -->  2.0 - 2.9 g   3 -->  3.0 - 3.9 g    *'/	 &
        5X,'*      4 -->  4.0 - 4.9 g   5 -->  5.0 - 5.9 g    *'/	 &
        5X,'*      6 -->  6.0 - 6.9 g   7 -->  7.0 - 7.9 g    *'/	 &
        5X,'***************************************************'/)
99  FORMAT(11X,'DAYS TO MATURITY BY POSITION ',I2.2,'/',I2.2			 &
        /,5X,'*         A -->  ABSCISED    X --> SQUARE         *'/	 &
        5X,'*         $ -->  MATURE      0 -->   0 -  9       *'/	 &
        5X,'*         1 -->  10 - 19     2 -->  20 - 29       *'/	 &
        5X,'*         3 -->  30 - 39     4 -->  40 - 49       *'/	 &
        5X,'*         5 -->  50 - 59     6 -->  60 - 69       *'/	 &
        5X,'***************************************************'/)
    !
100 FORMAT(14X,A1,6A2,2(13X,A1,6A2))
101 FORMAT(2X,6A2,A1,2(13X,6A2,A1))
102 FORMAT(14X,A1,25X,A1,25X,A1/14X,A1,25X,A1,25X,A1/					 &
        14X,A1,25X,A1,25X,A1)
103 FORMAT(//)
    !
    WRITE(24,*)'FROM COTPLT SUB'
    WRITE(24,103)

    DO 1 K=1,NVBRCH
        NBRCH=NFBR(K)
        DO 2 L=1,NBRCH,2
            NNID  =NNOD(K,L)
            DO 3 M=1,NNID
                IF(FCODE(K,L,M).EQ.0)GO TO 2
                IF(MTYPE.EQ.1) THEN
                    ICODE = MCODE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M) = CHAR1(ICODE)
                ENDIF
                IF(MTYPE.EQ.2) THEN
                    ICODE = FCODE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M) = CHAR1(ICODE)
                ENDIF
                IF(MTYPE.EQ.3) THEN
                    ICODE = FRUTP(K,L,M)
                    IF((FRUTP(K,L,M).GE.10.).AND.(FRUTP(K,L,M).LT.10.01))		 &
                        ICODE=9
                    IF(ICODE.GT.0) PRT(K,L,M) = CHAR3(ICODE)
                ENDIF
                IF(MTYPE.EQ.4) THEN
                    ICODE = BSIZE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M) = CHAR3(ICODE)
                ENDIF
                IF(MTYPE.EQ.5) THEN
                    ICODE = MATURE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M) = CHAR3(ICODE)
                ENDIF
                PRI(K,L) = CHARI
3           CONTINUE
2       CONTINUE
        DO 4 L=2,NBRCH,2
            NNID = NNOD(K,L)
            DO 5 M=1,NNID
                M2 = 7-M
                !            M2 = 6-M
                IF(FCODE(K,L,M).EQ.0) GO TO 4
                IF(MTYPE.EQ.1) THEN
                    ICODE = MCODE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M2) = CHAR2(ICODE)
                ENDIF
                IF(MTYPE.EQ.2) THEN
                    ICODE = FCODE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M2) = CHAR2(ICODE)
                ENDIF
                IF(MTYPE.EQ.3) THEN
                    ICODE = FRUTP(K,L,M)
                    IF((FRUTP(K,L,M).GE.10.).AND.(FRUTP(K,L,M).LT.10.01))	   &
                        ICODE=9
                    IF(ICODE.GT.0) PRT(K,L,M2) = CHAR4(ICODE)
                ENDIF
                IF(MTYPE.EQ.4) THEN
                    ICODE = BSIZE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M2) = CHAR4(ICODE)
                ENDIF
                IF(MTYPE.EQ.5) THEN
                    ICODE = MATURE(K,L,M)
                    IF(ICODE.GT.0) PRT(K,L,M2) = CHAR4(ICODE)
                ENDIF
                PRI(K,L) = CHARI
5           CONTINUE
4       CONTINUE
1   CONTINUE
    IF(MTYPE.EQ.1) WRITE(24,95)MO,DAZE
    IF(MTYPE.EQ.2) WRITE(24,96)MO,DAZE
    IF(MTYPE.EQ.3) WRITE(24,97)MO,DAZE
    IF(MTYPE.EQ.4) WRITE(24,98)MO,DAZE
    IF(MTYPE.EQ.5) WRITE(24,99)MO,DAZE
    NBRCH = NFBR(1)
    IF(NBRCH.EQ.(NBRCH/2*2)) GO TO 20
    DO 10 L=1,NBRCH,2
        LX = NBRCH +1 - L
        LX1 = LX-1
        WRITE(24,100) PRI(1,LX),(PRT(1,LX,M),M=1,6),PRI(2,LX),	 &
            (PRT(2,LX,M),M=1,6),PRI(3,LX),(PRT(3,LX,M),M=1,6)
        IF(LX1.EQ.0) GO TO 6
        WRITE(24,101) (PRT(1,LX1,M),M=1,6),PRI(1,LX1),		 &
            (PRT(2,LX1,M),M=1,6),PRI(2,LX1),(PRT(3,LX1,M),M=1,6),  &
            PRI(3,LX1)
10  CONTINUE
    GO TO 6
20  DO 21 L=1,NBRCH,2
        LX = NBRCH + 1 - L
        LX1 = LX - 1
        WRITE(24,101) (PRT(1,LX,M),M=1,6),PRI(1,LX),				 &
            (PRT(2,LX,M),M=1,6),PRI(2,LX),(PRT(3,LX,M),M=1,6),		 &
            PRI(3,LX)
        IF(LX1.EQ.0) GO TO 6
        WRITE(24,100) PRI(1,LX1),(PRT(1,LX1,M),M=1,6),		 &
            PRI(2,LX1),(PRT(2,LX1,M),M=1,6),PRI(3,LX1),			 &
            (PRT(3,LX1,M),M=1,6)
21  CONTINUE
6   CONTINUE
    WRITE(24,102) PRI(1,1),PRI(2,1),PRI(3,1),PRI(1,1),PRI(2,1),		 &
        PRI(3,1),PRI(1,1),PRI(2,1),PRI(3,1)
    WRITE(24,103)
    RETURN
    END


    SUBROUTINE OUT(ARRAY,TOTAL,IGO)
    !  ************************************************************
    !  *                                                          *
    !  *  THIS SUBROUTINE PLOTS THE SOIL SLAB AND THE DENSITIES   *
    !  *  OF THE ARRAY ELEMENTS IN EACH CELL.                     *
    !  *                                                          *
    !  ************************************************************

    use common_block

    DIMENSION CAPSCA(11), PSISCA(11), VNOSCA(11),						&
        ROOSCA(11), RANGE(11), ARRAY(40,20),TEMPSCA(11)

    CHARACTER TTL1*40, TTL3*40, TTL4*40, TTL5*40,TTL8*40,				&
        TTL7*40, UNITST*16, VNOUNI*24, VH2UNI*24, PSIUNI*24,	&
        NITUNT*16, TTL1R*40, TTL2R*40, UNITS*24, UNITSR*16,		&
        UNTS*24, UNTST*16, TL1*40, TL2*40, TEMPUNT*16,TTL9*40,	&
        TUNITS*24, UNITPS*16

    DATA ROOSCA/1.0E-25,.0001,.0005,.005,.01,.015,.02,.025,.03,		&
        .035,.04/
    DATA PSISCA/-15.,-10.,-6.,-3.,-1.5,-1.,-.6,-.4,-.2,-.1,0./
    DATA VNOSCA/1.0E-25,.01,.02,.03,.04,.05,.06,.07,.08,.09,.1/
    DATA CAPSCA/0.0,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5/
    DATA TEMPSCA/0.00,5.00,10.00,15.00,20.00,25.00,30.00,35.00,		&
        40.00,45.00,50.00/
    DATA TTL1R  /'ROOTS IN EACH CELL, TOTAL               '/
    DATA TTL2R  /'                                        '/
    DATA TTL1   /'VOLUMETRIC WATER CONTENT OF SOIL        '/
    DATA TTL3   /'                                        '/
    DATA TTL4   /'SOIL WATER POTENTIAL FOR          '/
    DATA TTL5   /'VOLUMETRIC NITRATE CONTENT OF SOIL      '/
    DATA TTL7   /'VOLUMETRIC AMMONIA CONTENT OF SOIL      '/
    DATA TTL8   /'AVERAGE SOIL TEMPERATURE                '/
    DATA TTL9   /'AT THE END OF THE DAY                   '/
    DATA UNITS  /'G/CM**3 SOIL            '/
    DATA UNITPS /' BARS           '/
    DATA PSIUNI /' BARS IN ROOT ZONE      '/
    DATA VNOUNI /' MG/N PER CM**3         '/
    DATA VH2UNI /' CM**3/CM**3 SOIL       '/
    DATA TUNITS /' DEGREES CELSIUS        '/
    DATA UNITSR /' GM. DRY WEIGHT '/
    DATA UNITST /' MM WATER       '/
    DATA NITUNT /' LBS N PER ACRE '/
    DATA TEMPUNT/' DEGREES CELSIUS'/


100 FORMAT(///6X,A40,15X,'DAY ',I2.2,'/',I2.2/6X,A40/					 &
        6X,'UNITS - ',A24,23X,'LEGEND'//24X,'1 1 1 1 1 1 1 1 1 1 2'/	 &
        6X,'1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0',18X,A1,			 &
        '  <= ',F8.4//53X,F8.4,' < ',A1,' <= ',F8.4)
102 FORMAT(3(1X,I2,3X,20A2/),1X,I2,3X,20A2,7X,F8.4,' < ',				 &
        A1,' <= ',F8.4)
104 FORMAT(3(1X,I2,3X,20A2/),1X,I2,3X,20A2,7X,F8.4,' < ',A1//			 &
        6X,'TOTAL = ',F11.2,1X,A16)
200 FORMAT(///6X,A40,15X,'DAY ',I2.2,'/',I2.2/6X,A40/					 &
        6X,'UNITS - ',A24,23X,'LEGEND'//24X,'1 1 1 1 1 1 1 1 1 1 2'/	 &
        6X,'1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0',18X,A1,			 &
        '  < ',E8.2//53X,E8.2,' < ',A1,' < ',E8.2)
202 FORMAT(3(1X,I2,3X,20A2/),1X,I2,3X,20A2,7X,E8.2,' < ',				 &
        A1,' < ',E8.2)
204 FORMAT(3(1X,I2,3X,20A2/),1X,I2,3X,20A2,7X,E8.2,' < ',A1//			 &
        6X,'TOTAL = ',F11.2,1X,A16)
    IF(IGO.EQ.1) THEN
        IF(TOTAL.EQ.TNNH4) THEN
            TL1=TTL7
        ELSE
            TL1=TTL5
        ENDIF
        TL2=TTL3
        UNTS=VNOUNI
        UNTST=NITUNT
        DO 20 I=1,11
            RANGE(I)=VNOSCA(I)
20      CONTINUE
    ENDIF
    IF(IGO.EQ.2) THEN
        TL1=TTL1
        TL2=TTL3
        UNTS=VH2UNI
        UNTST=UNITST
        DO 40 I=1,11
            RANGE(I)=CAPSCA(I)
40      CONTINUE
    ENDIF
    IF(IGO.EQ.3) THEN
        TL1=TTL1R
        TL2=TTL2R
        UNTS=UNITS
        UNTST=UNITSR
        DO 60 I=1,11
            RANGE(I)=ROOSCA(I)
60      CONTINUE
    ENDIF
    IF(IGO.EQ.4) THEN
        TL1=TTL4
        TL2=TTL3
        UNTS=PSIUNI
        UNTST=UNITPS
        DO 80 I=1,11
            RANGE(I)=PSISCA(I)
80      CONTINUE
    ENDIF
    IF (IGO.EQ.5) THEN
        TL1 = TTL8
        TL2 = TTL9
        UNTS = TUNITS
        UNTST=TEMPUNT
        DO 70 I=1,11
            RANGE(I)=TEMPSCA(I)
70      CONTINUE
    ENDIF
    DO 1 K=1, 20
        DO 1 L=1, 40
            ARAYLK = ARRAY(L,K)
            DO 2 I=1, 11
                RANGE1 = RANGE(I)
                IF(ARAYLK.LE.RANGE1) GO TO 1
2           CONTINUE
            I = 12
1   KHAR(L,K) = KA(I)
    IF(IGO.EQ.2) GO TO 15
    RANGE1 = RANGE(1)
    WRITE(24,100) TL1,MO,DAZE,TL2,UNTS,KA(1),RANGE1,RANGE1,KA(2),		 &
        RANGE(2)
    INDX=0
    DO 14 L=1, 33, 4
        INDX=INDX+1
        L1 = L+1
        L2=L+2
        L3=L+3
14  WRITE(24,102)L,(KHAR(L,K),K=1,20),L1,(KHAR(L+1,K),K=1,20),		 &
        L2,(KHAR(L+2,K),K=1,20),L3,(KHAR(L+3,K),K=1,20),		 &
        RANGE(INDX+1),KA(INDX+2),RANGE(INDX+2)
    L37=37
    L38=38
    L39=39
    L40=40
    WRITE(24,104) L37,(KHAR(37,K),K=1,20),L38,(KHAR(38,K),K=1,20),	 &
        L39,(KHAR(39,K),K=1,20),L40,(KHAR(40,K),K=1,20),	 &
        RANGE(11),KA(12),TOTAL,UNTST
    RETURN
15  RANGE1 = RANGE(1)
    WRITE(24,200) TL1,MO,DAZE,TL2,UNTS,KA(1),RANGE1,RANGE1,KA(2),		 &
        RANGE(2)
    INDX=0
    DO 16 L=1, 33, 4
        L1 = L+1
        L2=L+2
        L3=L+3
        INDX=INDX+1
16  WRITE(24,202)L,(KHAR(L,K),K=1,20),L1,(KHAR(L+1,K),K=1,20),		 &
        L2,(KHAR(L+2,K),K=1,20),L3,(KHAR(L+3,K),K=1,20),		 &
        RANGE(INDX+1),KA(INDX+2),RANGE(INDX+2)
    L37 = 37
    L38=38
    L39=39
    L40=40
    WRITE(24,204) L37,(KHAR(L37,K),K=1,20),L38,(KHAR(L38,K),K=1,20),	  &
        L39,(KHAR(L39,K),K=1,20),L40,(KHAR(L40,K),K=1,20),	  &
        RANGE(11),KA(12),TOTAL,UNTST
    RETURN
    END


    SUBROUTINE MDSEA(I)

    use common_block

    CHARACTER PMAPFLE*12,FILEPATH*80
    DIMENSION IDUMCDE(40,15)

1000 FORMAT(A16,F6.1,2X,F4.2)
1020 FORMAT(15I5)
1040 FORMAT(I2,1X,I2,1X,I2)
1060 FORMAT(2F10.1,3I10)
1080 FORMAT(6I10)

    IF(KULKNT.EQ.1) THEN
        CALL NUMCHR(PRONAM,J)
        IF(I.GT.1) THEN
            J = J+2
            IF(J.GT.8) J = 8
            K = I
            L = I/10
            IF(I.GT.9) K = I - L*10
            PMAPFLE(1:1) = CHAR(L+48)
            PMAPFLE(2:2) = CHAR(K+48)
            PMAPFLE(3:J) = PRONAM(1:J-2)
        ELSE
            PMAPFLE(1:J) = PRONAM(1:J)
        ENDIF
        PMAPFLE(J+1:J+4) = '.MAP'
        FILEPATH(1:1) = CHAR(92)
        FILEPATH(2:5) = 'PMAP'
        FILEPATH(6:6) = CHAR(92)
        FILEPATH(7:J+10) = PMAPFLE(1:J+4)
        OPEN(11,FILE=FILEPATH,STATUS='UNKNOWN',ERR=120)
        DUM = 0.0
        DUM0 = 3.0
        WRITE(11,1000) PRONAM,DUM,DUM0
        WRITE(11,1040) MO,DAZE,IYEAR
        DO II=1,50
            DO KK=1,mxvbrch
                IF(FCODE(KK,1,1).GT.0) THEN
                    DO L=1,mxfbrch
                        DO M=1,mxfsite
                            IDUMCDE(L,M) = FCODE(KK,L,M)
                            IF(IDUMCDE(L,M).EQ.2) IDUMCDE(L,M) = 3
                            IF(IDUMCDE(L,M).EQ.7) IDUMCDE(L,M) = 2
                            IF((IDUMCDE(L,M).GT.0).AND.						 &
                                (FFRUT(KK,L,M)*100..LT.II*2-1)) 				 &
                                IDUMCDE(L,M) = 4
                        ENDDO
                    ENDDO
                    WRITE(11,1000) 'REP 1           '
                    DUM0 = II+(KK-1)/10.
                    IF(KK.GT.1) THEN
                        IDUM1 = 0
                    ELSE
                        IDUM1 = NUMPFN
                    ENDIF
                    IF(NFBR(3).GT.0) THEN
                        IDUM2 = 2
                    ELSEIF(NFBR(2).GT.0) THEN
                        IDUM2 = 1
                    ELSE
                        IDUM2 = 0
                    ENDIF
                    DUM1 = Z/2.54
                    WRITE(11,1060) DUM0,DUM1,NFBR(KK),IDUM1,IDUM2
                    IDUM1 = 0
                    WRITE(11,1080) IDUM1,IDUM1,IDUM1,IDUM1,IDUM1
                    WRITE(11,1080) IDUM1,IDUM1,IDUM1,IDUM1,IDUM1,IDUM1
                    WRITE(11,1080) IDUM1,IDUM1,IDUM1,IDUM1,IDUM1
                    WRITE(11,1020) (IDUMCDE(1,M),M=1,5),IDUM1,					&
                        (IDUMCDE(2,M),M=1,5),IDUM1,(IDUMCDE(3,M),M=1,3)
                    WRITE(11,1020) (IDUMCDE(3,M),M=4,5),IDUM1,					&
                        (IDUMCDE(4,M),M=1,5),(IDUMCDE(5,M),M=1,5),				&
                        (IDUMCDE(6,M),M=1,2)
                    WRITE(11,1020) (IDUMCDE(6,M),M=3,5),(IDUMCDE(7,M),M=1,5),	&
                        (IDUMCDE(8,M),M=1,5),(IDUMCDE(9,M),M=1,2)
                    WRITE(11,1020) (IDUMCDE(9,M),M=3,4),(IDUMCDE(10,M),M=1,4),	&
                        (IDUMCDE(11,M),M=1,4),(IDUMCDE(12,M),M=1,4),				&
                        IDUMCDE(13,1)
                    WRITE(11,1020) (IDUMCDE(13,M),M=2,4),(IDUMCDE(14,M),M=1,3),	&
                        (IDUMCDE(15,M),M=1,3),(IDUMCDE(16,M),M=1,3),				&
                        (IDUMCDE(17,M),M=1,3)
                    WRITE(11,1020) (IDUMCDE(18,M),M=1,3),(IDUMCDE(19,M),M=1,2),	&
                        (IDUMCDE(20,M),M=1,2),(IDUMCDE(21,M),M=1,2),				&
                        (IDUMCDE(22,M),M=1,2),(IDUMCDE(23,M),M=1,2),				&
                        IDUMCDE(24,1),IDUMCDE(25,1)
                ENDIF
            ENDDO
        ENDDO
        WRITE(11,1000) 'END             '
        CLOSE(11)
    ENDIF
120 CONTINUE
    Z = PLTHT(I)*2.54
    IF(MSATYP(I).GT.0) THEN
        DO 300 K=1,mxvbrch
            DO 301 L=1,mxfbrch
                DO 302 M=1,mxfsite
                    IF(FCODE(K,L,M).GT.0) THEN
                        J=K*150-L*5+M
                        FFRUT(K,L,M)=AVGPLT(I,J)
                    ENDIF
302             continue
301         continue
300     CONTINUE
    ENDIF
    IF((RUNMODE.EQ.'pmap').OR.(RUNMODE.EQ.'PMAP')) THEN
        DO 320 K=1,mxvbrch
            DO 320 L=1,mxfbrch
                DO 320 M=1,mxfsite
                    FRUTP(K,L,M)=FFRUT(K,L,M)*10.+.001
                    IF(FRUTP(K,L,M).LT.1.) FRUTP(K,L,M)=10.1
320     CONTINUE
        CALL COTPLT(3)
    ELSE
        CALL PMAPS
        !        IF(IOUTFG(13).GT.0) THEN  ' BY GR
        CALL COTPLT(2)
        CALL COTPLT(3)
        !       ENDIF  ' BY GR
    ENDIF
    RETURN
    END


    SUBROUTINE PREPRINT

    use common_block

    INTEGER*2 IDUM

1140 FORMAT(' FIRST SQUARE  ',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1200 FORMAT(' LAST ACT WTHER',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1220 FORMAT(' FIRST BLOOM   ',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1700 FORMAT(A80)
1760 FORMAT(' 1ST OPN BOLL  ',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1780 FORMAT(' 60% OPN BOLL  ',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1800 FORMAT(' MAX YIELD-100#',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1820 FORMAT(' MAX YIELD- 50#',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1840 FORMAT(' MAX YIELD     ',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1860 FORMAT(' NITR. STRESS *',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1880 FORMAT(' WATER STRESS *',2X,I2.2,'/',I2.2,I4,F5.1,I6,		 &
        F5.1,F6.1,I6,2I7,F6.2)
1900 FORMAT(/ '* First simulated water or nitrogen stress after ',	   &
        'last day of actual weather.' //)

    IF(ABEND) RETURN
    IFG1=0
    IFG2=0
    IFG3=0
    IFG4=0
    IFG5=0
    IFG6=0
    IFG7=0
    YMX100=YIELD*500.-100.      !*500 converts bales/acre to lb/acre- 100 lb/acre
    YMX50=YIELD*500.-50.        !*500 converts bales/acre to lb/acre- 50 lb/acre
    YLDMAX=YIELD                !Bales/acre
    REWIND(21)
400 CONTINUE
    READ(21,END=420,ERR=500) IDAY,KDAY,YIELD,NF,WSTRSD,Z,RI,TMAX			&
        ,TMIN,SQRZ,RAIN,FERN,GBZ2,PSILD,NV,LAI,SITEZ,NOPEN,FLOSS		&
        ,ABZ,CSTRES,PSIAVG,TAVG,TDAY,TNYT,C01,WIND,ES,EP				&
        ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW		    &
        ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT			&
        ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,PTSRED,NUMPFN				&
        ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4				&
        ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
    I00=SQRZ*POPPLT/1000+.5
    I01=GBZ2*POPPLT/1000+.5
    I02=NOPEN*POPPLT/1000+.5
    IDUM=KDAY+EMERGE-1

    IF((KDAY.GT.0).AND.(KDAY.EQ.ISQ)) THEN
        WRITE(22,1140) MO,DAZE,KDAY, Z,N00,LAI,C01,I00,I01,I02,YIELD
        WRITE(LSTNG,1140) MO,DAZE,KDAY,Z,N00,LAI,C01,I00,I01,I02,YIELD
    endif
    IF((KDAY.GT.0).AND.(KDAY.EQ.FBLOOM)) THEN
        WRITE(22,1220) MO,DAZE,KDAY,Z,N00,LAI,C01,I00,I01,I02,YIELD
        WRITE(LSTNG,1220) MO,DAZE,KDAY,Z,N00,LAI,C01,I00,I01,I02,YIELD
    endif
    IF(DAYNUM.EQ.LDAYAW) THEN
        WRITE(22,1200) MO,DAZE,KDAY,Z,N00,LAI,C01,I00,I01,I02,YIELD
        WRITE(LSTNG,1200) MO,DAZE,KDAY,Z,N00,LAI,C01,I00,I01,I02,YIELD
    endif

    IF(IDUM.GT.LDAYAW) THEN
        IF((IFG1.EQ.0).AND.(PSIAVG.LE.PSICMX)) THEN
            WRITE(22,1880,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
                I00,I01,I02,YIELD
            IFG1=1
            WRITE(lstng,1880,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,		   &
                I00,I01,I02,YIELD
        ENDIF


        IF((GBZ2+NOPEN).GT.0.) THEN
            IF((IFG2.EQ.0).AND.(NF.LT.1.)) THEN
                WRITE(22,1860,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,		   &
                    I00,I01,I02,YIELD
                IFG2=1
                WRITE(lstng,1860,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,	   &
                    I00,I01,I02,YIELD
            ENDIF
        ELSE
            IF((IFG2.EQ.0).AND.(NV.LT.1.).AND.(KDAY.GT.0)) THEN
                WRITE(22,1860,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,		   &
                    I00,I01,I02,YIELD
                IFG2=1
                WRITE(lstng,1860,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,	   &
                    I00,I01,I02,YIELD
            ENDIF
        ENDIF
    ENDIF
    IF((IFG3.EQ.0).AND.(NOPEN.GT..1)) THEN
        WRITE(22,1760,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
        IFG3=1
        WRITE(lstng,1760,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
    ENDIF
    SIXPCT=(NOPEN+GBZ2)*.6
    IF((IFG4.EQ.0).AND.(NOPEN.GE.SIXPCT).AND.(NOPEN.GT.0.)) THEN
        WRITE(22,1780,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
        IFG4=1
        WRITE(lstng,1780,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
    ENDIF
    IF((IFG5.EQ.0).AND.(YMX100.LE.YIELD*500.).AND.					   &
        (YIELD.GT.0.)) THEN
        WRITE(22,1800,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
        IFG5=1
        WRITE(lstng,1800,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
    ENDIF
    IF((IFG6.EQ.0).AND.(YMX50.LE.YIELD*500).AND.					   &
        (YIELD.GT.0.)) THEN
        WRITE(22,1820,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
        IFG6=1
        WRITE(lstng,1820,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
    ENDIF
    IF((IFG7.EQ.0).AND.(YIELD.GE.YLDMAX).AND.						   &
        (YIELD.GT.0.)) THEN
        WRITE(22,1840,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
        IFG7=1
        WRITE(lstng,1840,ERR=520) MO,DAZE,KDAY,Z,N00,LAI,C01,			   &
            I00,I01,I02,YIELD
    ENDIF
    GO TO 400
420 CONTINUE
   ! WRITE(22,1900,ERR=520)
   ! WRITE(LSTNG,1900)
    RETURN
500 CONTINUE
    PRINTBUF=' Error reading binary output file in preprint.'
    CALL WRITERR
    RETURN
520 CONTINUE
    PRINTBUF=' Error writing summary file.'
    CALL WRITERR
    RETURN
    END


    SUBROUTINE PRINTOUT
    !  ****************************************************************
    !  *   COLLECTION OF WRITE STATEMENTS FOR OUTPUT OF MODEL RESULTS *
    !  ****************************************************************

    use common_block

1000 FORMAT(' ** FIRST SQUARE ON ',I2.2,'/',I2.2,' **')
1020 FORMAT(11X,' YIELD BALES/ACRE   YIELD LBS/ACRE')
1030 FORMAT(17X,F4.2,13X,F7.0)
1040 FORMAT(' ************ FIRST BLOOM ON DAY ',I3,' *************')
1200 FORMAT('TABLE 1:',17X,'GENERAL OUTPUT IN ENGLISH UNITS')
1220 FORMAT(17X,'PER/PLANT',27X,'PER/ACRE')
1240 FORMAT(12X,18('-'),4X,43('-'))
1260 FORMAT(' DAE  DATE  HEIGHT  LAI  NODES   SITES',			 &
        '  SQUARES   GREEN    OPEN  YIELD  FRUIT')
1280 FORMAT(13X,'(IN)',33X,'BOLLS   BOLLS',9X,' SHED')
1290 FORMAT(1X,I3,1X,I2,'/',I2.2,F7.1,F6.2,I5,2X,I8,1X,3I8,F6.2,I8)
1300 FORMAT('TABLE 2:',22X,'STRESS FACTORS OUTPUT')
1320 FORMAT('          %                         LEAF         ',	 &
        '           LEAF    SOIL')
1340 FORMAT('  DATE LIGHT  CSTRES  NITR-STRESS    N    WATER ',	 &
        '    PIXCON   WATER   WATER')
1360 FORMAT('        INT           FRUIT   VEG  CONC  STRESS ',	 &
        '    (ppm)    POT     POT')
1380 FORMAT(1X,I2.2,'/',I2.2,F6.1,F7.3,2F7.2,F6.3,F8.3,F8.3,	 &
        2F8.3)
1400 FORMAT('TABLE 5:',15X,'WEATHER VARIABLES IN METRIC UNITS')
1420 FORMAT(12X,'TEMPERATURE DEGREES (C)')
1440 FORMAT('                    DAILY  DAY  NIGHT (LNLYS)   (MM)',  &
        '   (MM)     (MM)')
1460 FORMAT(12X,'VARIETY PARAMETERS FOR ',A9)
1480 FORMAT(5X,5E12.4)
1500 FORMAT('TABLE 5:',15X,'WEATHER VARIABLES IN ENGLISH UNITS')
1520 FORMAT(12X,'TEMPERATURE DEGREES (F)')
1540 FORMAT(9X,28('-'))
1560 FORMAT('  DATE   MAX   MIN   AVG   AVG   AVG   SOLAR    RAIN',  &
        '    IRR    RUNOFF  WIND')
1580 FORMAT('                    DAILY  DAY  NIGHT (LNLYS)   (IN)',  &
        '   (IN)     (IN)')
1590 FORMAT(1X,I2.2,'/',I2.2,5F6.1,5F8.2)
1600 FORMAT('TABLE 4:',21X,'WATER AND ET VARIABLES')
1620 FORMAT('  DATE    ES    EP     RN     PN  CUMES  CUMEP',	  &
        ' CUMSOK  CUMRAN    TH2O  H2OBAL')
1640 FORMAT(1X,I2.2,'/',I2.2,2F6.2,F7.1,F7.3,3F7.1,3F8.1)
1700 FORMAT('TABLE 3:',21X,'WEIGHTS IN METRIC UNITS')
1720 FORMAT('  DATE    SPN PLANTW CO2BL  LFWT STMWT RUTWT',		  &
        ' SQRWT  COTXX GBOLWT DEADWT XTRAC')
1740 FORMAT(1X,I2.2,'/',I2.2,2F7.2,F6.2,3F6.2,F6.2,3F7.2,F6.1)
1800 FORMAT(' ')
1820 FORMAT('1')
1840 FORMAT(12X,'SOIL PARAMETERS FOR ',A13)
1860 FORMAT(1X,I5,5E12.3)
1880 FORMAT(6X,3E12.3,2I12)
    
     write(22,*)' '
     write(22,*)'Total N uptake and yield'
     write(22,*)'N-Uptake kg/ha= ', TotalNitrogenUptake*(.001/(RowSp*Eomult*.0001*.0001)) !g to kg, slab(RowSp*Eomult*.0001cm2)to ha
     write(22,*)'Total yield (kg/ha)=', Yield_lbs_ac*1.12085       !lb/acre to kg/ha 1.12
     write(22,*)' '
     write(22,*)' '
     
    IF(IOUTFG(1).GT.0) THEN
        WRITE(lstng,1800)
        WRITE(lstng,1800)
        WRITE(lstng,1200)
        WRITE(lstng,1220)
        WRITE(lstng,1240)
        WRITE(lstng,1260)
        WRITE(lstng,1280)
        WRITE(lstng,1800)
        REWIND(21)
180     CONTINUE
        READ(21,END=200,ERR=200) IDAY,KDAY,YIELD,NF,WSTRSD,Z,RI,TMAX	 &
            ,TMIN,SQRZ,RAIN,FERN,GBZ2,PSILD,NV,LAI,SITEZ,NOPEN,FLOSS	 &
            ,ABZ,CSTRES,PSIAVG,TAVG,TDAY,TNYT,C01,WIND,ES,EP			 &
            ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW	     &
            ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT		 &
            ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,PIXCON,NUMPFN			 &
            ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4			 &
            ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
        IF(KDAY.GT.0) THEN
            !IF(KDAY.EQ.ISQ) WRITE(lstng,1000) MO,DAZE
            N01=IFIX(SITEZ*POPPLT)                                      !Total fruiting sites POPPLT: population per acre
            N02=IFIX(SQRZ*POPPLT)                                       !Number of squares
            N03=IFIX(GBZ2*POPPLT)                                       !GREEN BOLLS PAST ABSCISSION AGE
            N04=IFIX(NOPEN*POPPLT)                                      !Number of open bolls
            N05=IFIX(ABZ*POPPLT)                                        !Total abscised fruit
            n06=IFIX(ABZ0*POPPLT)
            WRITE(lstng,1290) KDAY,MO,DAZE,Z,LAI,N00,					 &
                N01,N02,N03,N04,YIELD,N05                               !Yield: Bales/acre
        ENDIF
        GO TO 180
200     CONTINUE
        Yield_lbs_ac=YIELD*500.
        WRITE(lstng,1800)
        WRITE(lstng,1020)
        WRITE(lstng,1030) YIELD,Yield_lbs_ac                                  !Lb/ACRE                            !
        WRITE(lstng,1800)
        WRITE(lstng,1800)
    endif

    IF(IOUTFG(2).GT.0) THEN
        WRITE(lstng,1300)
        WRITE(lstng,1320)
        WRITE(lstng,1340)
        WRITE(lstng,1360)
        WRITE(lstng,1800)
        REWIND(21)
220     CONTINUE
        READ(21,END=240,ERR=240) IDAY,KDAY,YIELD,NF,WSTRSD,Z,RI,TMAX	  &
            ,TMIN,SQRZ,RAIN,FERN,GBZ2,PSILD,NV,LAI,SITEZ,NOPEN,FLOSS	  &
            ,ABZ,CSTRES,PSIavg,TAVG,TDAY,TNYT,C01,WIND,ES,EP			  &
            ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW	      &
            ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT		  &
            ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,PIXCON,NUMPFN			  &
            ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4			  &
            ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
        IF(KDAY.GT.0) THEN
            XDUM=(ES+EP)/25.4
            !IF(KDAY.EQ.ISQ) WRITE(lstng,1000) MO,DAZE
            WRITE(lstng,1380) MO,DAZE,C01,CSTRES,NF,NV,LEAFCN,			  &
                WSTRSD, PIXCON,PSILD,PSIavg
             !WRITE(lstng,1380) MO,DAZE,C01,CSTRES,NF,NV,LEAFCN,			  &
             !   WSTRSD,XDUM,PIXCON,PSILD,PSIM           
        ENDIF
        GO TO 220
240     CONTINUE
        Yield_lbs_ac=YIELD*500.
        WRITE(lstng,1800)
        WRITE(lstng,1020)
        WRITE(lstng,1030) YIELD,Yield_lbs_ac
        WRITE(lstng,1800)
        WRITE(lstng,1800)
    endif

    IF(IOUTFG(3).GT.0) THEN
        WRITE(lstng,1700)
        WRITE(lstng,1720)
        WRITE(lstng,1800)
        REWIND(21)
260     CONTINUE
        READ(21,END=280,ERR=280) IDAY,KDAY,YIELD,NF,WSTRSD,Z,RI,TMAX	  &
            ,TMIN,SQRZ,RAIN,FERN,GBZ2,PSILD,NV,LAI,SITEZ,NOPEN,FLOSS	  &
            ,ABZ,CSTRES,PSIAVG,TAVG,TDAY,TNYT,C01,WIND,ES,EP			  &
            ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW	      &
            ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT		  &
            ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,PIXCON,NUMPFN			  &
            ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4			  &
            ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
        IF(KDAY.GT.0) THEN
           ! IF(KDAY.EQ.ISQ) WRITE(lstng,1000) MO,DAZE
            WRITE(lstng,1740) MO,DAZE,SPN,PLANTW,CHOBAL,LEAFWT,			  &
                STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT,XTRAC
        ENDIF
        GO TO 260
280     CONTINUE
        Yield_lbs_ac=YIELD*500.
        WRITE(lstng,1800)
        WRITE(lstng,1020)
        WRITE(lstng,1030) YIELD,Yield_lbs_ac
        WRITE(lstng,1800)
        WRITE(lstng,1800)
    endif

    IF(IOUTFG(4).GT.0) THEN
        WRITE(lstng,1600)
        WRITE(lstng,1620)
        WRITE(lstng,1800)
        REWIND(21)
300     CONTINUE
        READ(21,END=320,ERR=320) IDAY,KDAY,YIELD,NF,WSTRSD,Z,RI,TMAX	  &
            ,TMIN,SQRZ,RAIN,FERN,GBZ2,PSILD,NV,LAI,SITEZ,NOPEN,FLOSS	  &
            ,ABZ,CSTRES,PSIAVG,TAVG,TDAY,TNYT,C01,WIND,ES,EP			  &
            ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW	      &
            ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT		  &
            ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,PIXCON,NUMPFN			  &
            ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4			  &
            ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
        IF(KDAY.GT.0) THEN
            IF(KDAY.EQ.ISQ) WRITE(lstng,1000) MO,DAZE
            WRITE(lstng,1640) MO,DAZE,ES,EP,RN,PN,CUMES,CUMEP,			  &
                CUMSOK,CUMRAN,TH2O,H2OBAL
        ENDIF
        GO TO 300
320     CONTINUE
        Yield_lbs_ac=YIELD*500.
        WRITE(lstng,1800)
        WRITE(lstng,1020)
        WRITE(lstng,1030) YIELD,Yield_lbs_ac
        WRITE(lstng,1800)
        WRITE(lstng,1800)
    endif

    IF(IOUTFG(5).GT.0) THEN
        WRITE(lstng,1400)
        WRITE(lstng,1420)
        WRITE(lstng,1540)
        WRITE(lstng,1560)
        WRITE(lstng,1440)
        WRITE(lstng,1800)
        REWIND(21)
340     CONTINUE
        READ(21,END=360,ERR=360) IDAY,KDAY,YIELD,NF,WSTRSD,Z,RI,TMAX	 &
            ,TMIN,SQRZ,RAIN,FERN,GBZ2,PSILD,NV,LAI,SITEZ,NOPEN,FLOSS	 &
            ,ABZ,CSTRES,PSIavg,TAVG,TDAY,TNYT,C01,WIND,ES,EP			 &
            ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW	     &
            ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT		 &
            ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,PIXCON,NUMPFN			 &
            ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4			 &
            ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
        IF(KDAY.GT.0) THEN
            IF(KDAY.EQ.ISQ) WRITE(lstng,1000) MO,DAZE
            WIND=WIND*1.609344
            C00=RAIN+RUNOFF(DAYNUM)-AMTIRR(DAYNUM)*25.4
            C01=AMTIRR(DAYNUM)*25.4
            WRITE(lstng,1590) MO,DAZE,TMAX,TMIN,TAVG,TDAY,TNYT,			 &
                RI,C00,C01,RUNOFF(DAYNUM),WIND
        ENDIF
        GO TO 340
360     CONTINUE
        Yield_lbs_ac=YIELD*500.
        WRITE(lstng,1800)
        WRITE(lstng,1020)
        WRITE(lstng,1030) YIELD,Yield_lbs_ac
        WRITE(lstng,1800)
    endif

    IF(IOUTFG(6).GT.0) THEN
        WRITE(lstng,1500)
        WRITE(lstng,1520)
        WRITE(lstng,1540)
        WRITE(lstng,1560)
        WRITE(lstng,1580)
        WRITE(lstng,1800)
        REWIND(21)
380     CONTINUE
        READ(21,END=400,ERR=400) IDAY,KDAY,YIELD,NF,WSTRSD,Z,RI,TMAX	 &
            ,TMIN,SQRZ,RAIN,FERN,GBZ2,PSILD,NV,LAI,SITEZ,NOPEN,FLOSS	 &
            ,ABZ,CSTRES,PSIavg,TAVG,TDAY,TNYT,C01,WIND,ES,EP			 &
            ,RN,PN,CUMES,CUMEP,CUMSOK,CUMRAN,TH2O,H2OBAL,SPN,PLANTW		 &
            ,CHOBAL,LEAFWT,STEMWT,ROOTWT,SQWT,COTXX,GBOLWT,DEADWT		 &
            ,XTRAC,POPPLT,LEAFCN,ISQ,DAZE,MO,N00,PIXCON,NUMPFN			 &
            ,SDWSTM,SDWLEF,RCH2O,SDWBOL,AVAILN,SUPNO3,SUPNH4			 &
            ,LEFABS,RUNOFF(DAYNUM),AMTIRR(DAYNUM)
        IF(KDAY.GT.0) THEN
            IF(KDAY.EQ.ISQ) WRITE(lstng,1000) MO,DAZE
            TMAX=TMAX/5.*9.+32.
            TMIN=TMIN/5.*9.+32.
            TAVG=TAVG/5.*9.+32.
            TDAY=TDAY/5.*9.+32.
            TNYT=TNYT/5.*9.+32.
            RAIN=RAIN/25.4
            RUNOFF(DAYNUM)=RUNOFF(DAYNUM)/25.4
            C00=RAIN+RUNOFF(DAYNUM)-AMTIRR(DAYNUM)
            WRITE(lstng,1590) MO,DAZE,TMAX,TMIN,TAVG,TDAY,TNYT,			 &
                RI,C00,AMTIRR(DAYNUM),RUNOFF(DAYNUM),WIND
        ENDIF
        GO TO 380
400     CONTINUE
        Yield_lbs_ac=YIELD*500.
        WRITE(lstng,1800)
        WRITE(lstng,1020)
        WRITE(lstng,1030) YIELD,Yield_lbs_ac
        WRITE(lstng,1800)
    ENDIF
!    write(lstng,*) "SPN  PN   ES"
!    write(lstng,*) SPN, PN, ES
!    write(lstng,*)
!    WRITE(lstng,1840) hydfle
!    WRITE(lstng,1800)
!    DO 375 I=1,LYRSOL
!        WRITE(lstng,1860) LDEPTH(I),DIFF0(I),THETA0(I),BETA(I),		 &
!            THETAS(I),FCININ(I)
!        WRITE(lstng,1880) THETAR(I),AIRDR(I),BD(I),IPSAND(I),			 &
!            IPCLAY(I)
!375 CONTINUE
    WRITE(lstng,1800)
    WRITE(lstng,1800)

    Call fiberQualityParameters () !FQ
    call PrintSummary
    call preprint

500 CONTINUE
    return
    end

    
    subroutine Add_output1
    use common_block
    write(22,1971) dayofYear , tCount, wattsm(iTime),par(iTime),tair(iTime) ,vpd(iTime) ,Lai, Psilh(iTime)
1971 FORMAT(I3,5X,I2,6(4x,f11.4) )
    return
    end

    subroutine Add_output2
    use common_block
#    write(22,1972) dayofYear , tCount, wattsm(iTime),par(iTime),tair(iTime) ,vpd(iTime) ,Lai, Psilh(iTime)
#1972 FORMAT(I3,5X,I2,6(4x,f9.2) )
    return
    end

    
    
    
