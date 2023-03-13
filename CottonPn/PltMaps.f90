   SUBROUTINE PLTMAPS
    !  *************************************************************
    !  *                                                           *
    !  *                     PLTMAP SUBROUTINE                     *
    !  *                                                           *
    !  *************************************************************
    !  *                                                           *
    !  * FCODE                          MCODE                      *
    !  *                                                           *
    !  * 1 SQUARE                       1 SQUARE                   *
    !  * 2 GREEN BOLL                   2 GREEN BOLL               *
    !  * 3 MATURE BOLL                  3 MATURE BOLL              *
    !  * 4 FRUIT ABCISED AS A SQUARE    4 FRUIT ABZ AS A SQUARE    *
    !  * 5 FRUIT ABCISED AS A BOLL      5 FRUIT ABZ AS A BOLL      *
    !  * 6 NOT USED                     6 ABZ AS PINHEAD OR POLYNA *
    !  * 7 YOUNG GREEN BOLL             7 YOUNG GREEN BOLL         *
    !  *************************************************************

    use common_block

    POLOSS = 0.
    AGEADD = 0.
    DSITES = SITEZ-SITES   !diference in sites= Total fruting site- ???
    SITES  = SITEZ
    SITEZ  = 0
    PINHED = 0.  !pinhead square

    ! *** Calculate morphogenetic delays due to N- and C-stress

    call N_and_C_Delay

    ! *** Update age of pre-fruiting node

    DO J=1,NUMPFN                                   !number of pre fruting nodes
        AGEPFN(J) = AGEPFN(J) + 1                   ! AGEPFN: age of prefruting node
        AVTPFN(J) = (AVTPFN(J)*(AGEPFN(J)-1)+TAVG) / AGEPFN(J)
    ENDDO
    AVTEMP = ((KDAY-1)*AVTEMP + TAVG)/ KDAY
   

    ! *** If first square has not occurred, calculate the time from emergence
    ! *** to first square, TSQ

    if(isq.le.0)then  !ISq=1: First square had happend -------------------------------------------------------------
        call Time_to_First_Square   !Get TSQ: time of first square
        sum_fsq_tavg = sum_fsq_tavg + tavg

        ! *** If first square has not occurred, decide whether to add a PFruit node

        IF(KDAY.LT.IFIX(TSQ)) THEN                                      ! TSQ: Time of first square

            IF(AGEPFN(NUMPFN).LE.66.) THEN

                ! *** Increment the age of each node and calculate its running ave.temp.

                IF(NUMPFN.LT.9) THEN
                    call PreFruiting_Node_Time_Interval
                    IF(AGEPFN(NUMPFN).GE.(pfti+CDLAYV+NDLAY))CALL ADDPFNOD
                endif
            endif
            return
        endif

        ! *** HAVE FIRST SQUARE, INITIALIZE THAT NODE AREA, AVGT, AND LEAF.

        CALL FSTSQTODAY
        idum0 = 0
    ENDIF!-----------------------------------------------------------------------------------------------------------

    PIN = 0.0457*DSITES
    PINHED = PIN

    ! *** DECIDE WHETHER TO ADD A VEGETATIVE BRANCH.

    IF((NVBRCH.LT.mxvbrch).AND.(INT.LT.0.9)) THEN
        call Vegetative_Branch_Time_Interval
        call Time_to_First_Square
        dum = tsq
        IF(NVBRCH.EQ.2) DUM=0.0
        IF(AGE(NVBRCH,1,1).GE.(vbti+CDLAYV+NDLAY+DUM))	   &
            call Add_A_Vegetative_Branch
    ENDIF

    DO 30 K = 1,NVBRCH    !NVBRCH: Number of vegitative branch

        ! *** DECIDE WHETHER TO ADD A FRUITING BRANCH TO THIS VEGETATIVE BRANCH.

        NBRCH = NFBR(K)
        IF(NBRCH.LT.mxfbrch) THEN
            VDELAY(K) = VDELAY(K) + ((CDLAYV  + NDLAY )/ PIXDN)
            call Main_Stem_Node_Time_Interval(k,nbrch)
            IF(AGE(K,NBRCH,1).GE.(xmsti+VDELAY(K))) CALL ADDMSNOD(K)
        ENDIF

        ! *** DECIDE WHETHER TO ADD A NODE TO THIS FRUITING BRANCH OF THIS
        ! *** VEGETATIVE BRANCH.

        NBRCH = NFBR(K)
        DO L=1,NBRCH !Number of fruting branch
            NNID = NNOD(K,L)
            IF(NNID.LT.mxfsite) THEN
                DELAY(K,L) = DELAY(K,L) + ((CDLAYF+NDLAY) / PIXDN)
                call Fruiting_Branch_Node_Time_Interval(k,l,nnid)
                  
                IF(AGE(K,L,NNID).GE.(fbnti+DELAY(K,L)))				&

                    ! *** Add A fruiting branch.

                    call ADDFBNOD(k,l,nnid)
            ENDIF

            ! *** AGE ALL EXISTING NODES AND UPDATE AVERAGE TEMPERATURE OF EACH.
            ! *** AVGT(K,L,M) IS BOLL RUNNING AVERAGE TEMPERATURE	SINCE BOLLSET

            DO 50 M=1,NNID   !Node number on fruting branch
                AGE(K,L,M) = AGE(K,L,M) + 1.0
                AVGT(K,L,M) = (AVGT(K,L,M)*(AGE(K,L,M)-1)+TAVG)/AGE(K,L,M)
! FQ Calculate running average leaf water potential    !AVGLWP(10,40,15)
! FQ Calculate running average LeafN   !AVGLEAFN(10,40,15)
! FQ Calculate running average K   !AVGK(10,40,15)
! FQ Calculate running average P   !AVGP(10,40,15)		   
               AVGLWP(K,L,M) = (AVGLWP(K,L,M)*(AGE(K,L,M)-1)+PSIL_G)/AGE(K,L,M)         !Leaf water potential
		       AVGLEAFN(K,L,M) = (AVGLEAFN(K,L,M)*(AGE(K,L,M)-1)+leafcn)/AGE(K,L,M)     !LeafN
		       AVGK(K,L,M) = (AVGK(K,L,M)*(AGE(K,L,M)-1)+SLEAFK)/AGE(K,L,M)             !AvgK
		       AVGP(K,L,M) = (AVGP(K,L,M)*(AGE(K,L,M)-1)+SLEAFP)/AGE(K,L,M)             !AvgP                
                
                AVTNOD = AVGT(K,L,M)
                AGENOD = AGE(K,L,M)

                ! *** Set temperature used in BLOOM calculation to include canopy temperature
                ! *** differential; used in the calculation of heatindex.

                 Bloom_tavg(daynum) = avtnod
                !Bloom_tavg(JDAY) = avtnod

                ! *** AGE ALL LEAVES

                AGEFAC = AMAX1((1.0-WSTRS),(1.0-NV))*CALBRT(38)
                LAGE(K,L,M)=LAGE(K,L,M) + 1.0 + AGEFAC

                ! *** CALCULATE BOLL TEMP

                call Boll_Temperature(k,l,m)

                ! *** FCODE =          1  2  3  4  5  6  7

                IF(FCODE(K,L,M).GT.0) THEN

                    ! *** Squares are present

                    IF(FCODE(K,L,M).EQ.1) THEN
                        call Time_to_Bloom(avtnod)
                        if((k.eq.1).and.(l.eq.1).and.(m.eq.1)) then
                            if(iflday.eq.0.and.ifbl.lt.kday) 		   &
                                sum_fbl_tavg = sum_fbl_tavg + tavg
                        endif

                        ! *** IF SQUARE IS OLD ENOUGH, MAKE IT A Young GREEN BOLL (FCODE=7).

                        IF(AGENOD.GE.BLOOM) THEN

                            ! *** Estimate boll weight and update square weight

                            call Update_Boll_and_Square_Weight(k,l,m)

                            IF(GBOLWT.GT.0.0.AND.FBLOOM.LE.1.) 		&
                                call First_Bloom_Today

                            ! *** Estimate bloom loss due to a rain event during pollination

                            call Loss_due_Pollination(k,l,m)
                        ENDIF

                        ! *** Young green bolls are present

                    ELSEIF(FCODE(K,L,M).EQ.7) THEN

                        ! *** AGEABZ IS THE AGE AFTER WHICH GREEN BOLLS CANNOT BE ABSCISED

                        call Boll_Safe_Age(k,l,m)

                        ! *** Green bolls are present

                    ELSEIF(FCODE(K,L,M).EQ.2) THEN
                        call Time_to_Open_Boll(k,l,m)

                        ! *** IF GREEN BOLL IS OLD ENOUGH, MAKE IT AN OPEN BOLL AND SET FCODE TO 3.

                        IF(AGEBOL(K,L,M).GE.DEHISS(K,L,M)) THEN
                            call Seed_and_Burr_Ncontent(k,l,m)
                            COTXX  = COTXX + BOLWGT(K,L,M)  ! Cotxx: is the weight of the open bolls
                            GBOLWT = GBOLWT - BOLWGT(K,L,M)

                            GINP = (50.54 - .6755*BOLTMP(K,L,M)) / 100.0

                            ! *** NOPEN IS NUMBER OF OPEN BOLLS

                            NOPEN=NOPEN+FFRUT(K,L,M)
                            IF(NOPEN.GT.0.0) GIN=(GINP+GIN)/NOPEN

                            ! *** YIELD=500 LB. BALES/ACRE OF LINT. K=(453.6 G/LB)*(500 LB./BALE)

                            if(bolwgt(k,l,m).le.0.0001) then
                                yield = yield
                            else
                                YIELD = YIELD + (GINP * (BOLWGT(K,L,M)*0.75)	   &
                                    * POPPLT/226800.)
                                OpenBollYield(K,L,M)=GINP * (BOLWGT(K,L,M)*0.75)*  &
                                    POPPLT/226800*500                                   !Lb/acre
                            endif

                            ! *** Estimate fiber length and stength

	          !     call Fiber_Length_and_Strength()        !FQ

                        ENDIF
                    ENDIF
                    SITEZ = SITEZ + 1
                ENDIF
50          CONTINUE
40      enddo                               !loop of number of fruting branch
30  CONTINUE                                ! Loop of number of vergitativ ebranch
 
    RETURN
    END