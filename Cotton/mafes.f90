 

    SUBROUTINE Crop()
    !DEC$ ATTRIBUTES DLLEXPORT:: Crop
    USE MSFLIB
    USE MSFWIN

    !*****************************************************************************
    !
    !                            MAFES Version 2004
    !
    !           *********************  WARNING  **********************
    !Temperatures must be between 13 and 37oC--will not extrapolate beyond limits
    !           *********************  WARNING  **********************
    !
    !             This is the application version of MAFES for GUICS
    !
    !*****************************************************************************

    use common_block

    CHARACTER XFILE*150,DUMBO*1
    
!***Initialise/Setting the crop routine----------
  
    if (lInput.eq.1) then
!-------Initialise variables in crop routine
        nummod=nummod+1
        modnum=nummod
        npool1=0
        tspdwr=0
        pcrsd=0
        wateruptake=0
        !rch2o: root carbohydrate supply per plant[gm /plant]
        !multiplying by popslab gives the data over the slab
        initialrootcarbo= rch2o*popslab ![gram/slab]
!-------------------------------------------
        
!-------Cotton is simulated from sowing day till end day
        activecropdays=endday-sowingday
        j=1
        do i=sowingday, endday
            cropactive(j)=i
            j=j+1
        end do
        tnext(modnum)=cropactive(1)
        croprun=1
!----------------------------------------------
        
        
1020 FORMAT(/// '    THE FOLLOWING MESSAGES ARE FOR PROFILE ',A15)

    iDanoflg = 0
    ipdays = 0
    co2 = 0
    runmode = 'gossym'
    opsys = ' '
    errfle = 'runtime.err'
    binfle = 'runtime.g00'
    insfle = ''
    fngfle = ''
    loc0 = index(plotfle,'.g01')
    plotfle2 = plotfle(1:loc0) // 'g02'
    OPEN(24,FILE='plot.tmp', status='unknown')
    	loc6 = index(pgrhrb,'\', BACK=.true.)+1
 	    pgrfle = pgrhrb(loc6:)
    loc7 = index(pmafil,'\', BACK=.true.)+1
    pmafle = pmafil(loc7:)

    profle = profile((index(profile,'\', BACK=.true.)+1):)
    OPEN(21,FILE=binfle,FORM='UNFORMATTED',STATUS='unknown')
    OPEN(23,FILE=ERRFLE,STATUS='UNKNOWN')
    open(77,file=plotfle,status='unknown')  !.g01
    open(88,file=plotfle2,status='unknown') !.go2

140 CONTINUE
    READ(23,*,END=160,ERR=160) DUMBO
    GO TO 140
160 CONTINUE
    backspace(23)
    WRITE(23,1020) profle
    CALL INITIALIZE
    end if
!***End intialise/Setting the crop routine
 

!---------- Average soil water potential needs to be calculated from a day prior to sowing day       
            if (nsow1.eq. 0) psiavg1=0 !nsow1: is a switch related to sowing date
            if ((nsow1.eq. 0) .and. abs(timec-(sowingday-1) ).lt.0.001) then!a day before sowing day
                nsow1=1 ! this is used as a switch to calculate the PSIAVG in Carbon_Partitioning()
            end if
            if ((nsow1.eq. 1) .and. (navg1.gt.0)) then
                psiavg =psiavg+psiavg1   ! the average psi in the root zone is summed over a day
           end if
!-------------------------------------------------- 
            
!-----------Switch for Nshoot. Nshoot =1 if plant is emerged
            if ((nshoot .eq. 0) .and. abs(timec-(emergingday)).lt.0.001) then
                nshoot=1
                isemerged=1
            end if
!---------------------------------------------------
            
!----------If plant is emerged, the following variables from 2Dsoil should be processed over a day and then use in cotton which runs on a daily interval 
            if (nshoot .gt. 0) then
                !npool: total nitrogen available for the growth (from 2d soil) [g of nitrogen/plant]
                !sincrsink mass unit is [micro g of nitrogen], npool is in [g of nitrogen/plant]
                !sincrsink in solupt_y1.for is sum on all cells in a time step (is already *step)
                !conversion from nitrate to nitrogen is done already in solute_y1.for
                !npool1 [gram/slab/day]
                npool1=npool1+(sincrsink*1.0e-6)            !*1.0e-6 [micro g to g]

                !spdwr: sum of potential delta weight of roots over all cells, gms
                !tspdwr:Sum of spdwr over a day [grams/slab/day]
                tspdwr=tspdwr+spdwr*step

                !dailycarboused(pcrsd):dailycarboused(pcrsd)+pcrs*step
                pcrsd=pcrsd+pcrs*step                       ![gram/slab/day]

                !actual water uptake accumulated for a day
                wateruptake = wateruptake + awups*step      ![gram/slab/day]

                !minimum psil of the day
                psil_g= min(psil_,psil_g)                   ![bar]
            end if
!---------------------------------------------

!-----------Daily loop from sowing day to crop end day. The Growth routine will function only from emerging day till end day, whereas ET and Clymat routine functions on all days from sowing till end day            
            
            if(abs(timec-tnext(modnum)).lt.0.001*step)  then

                !if timec is from sowing till emergingday-1
                if ((nsow.eq. 0).and.abs(timec-(sowingday)).lt.0.001) nsow=1  !Nsow=1, if the timeC is between [sowing day to emerging day]
                if (timeC.ge.emergingday) nsow=0
                if (nsow.eq.1) then
                    psiavg=psiavg/24  !Average soil water potential [bar]
                    call clymat
                    call et
                    psiavg=0
                end if

                !if timec is from emergingday to end day
                if (nshoot.eq.1)then
                    jday =tnext(modnum)                 !5 digit day representation
                    daynum = emerge+iday                !Julian day
                    iday=iday+1                         !Starts at 1 on the day of emergence  
                    psiavg=psiavg/24
                    rootwt=totalrootweight/popslab      ![gram/plant/day] rootwt: dry weightof all the living roots per plant
                    call clymat
                    call et
                    call gosloop
                    tspdwr=0                            !potential weight of roots is accumulated for a day
                    npool1=0
                    pcrsd=0                             !pcrsd accumulates pcrs for each times steps to a day
                    wateruptake=0
                    psiavg=0
                    psil_g=0
                end if

                croprun=croprun+1
                if(croprun.gt.(activecropdays+1))then
                    tnext(modnum)=1.e+32  !This stopes the daily cotton loop
                    nshoot=0
                    close(1)
                    close(21,status='delete')
                    close(22)
                    close(23)
                    close(75)
                    close(77)   !.g01
                    close(88)   !.g02
                    close(lstng)
                else
                    tnext(modnum)=cropactive(croprun) !takes to the next day
                end if

            end if
!-----------Daily loop from sowing day to crop end day ends here-            

        200 CONTINUE

    
    !MS$IF DEFINED(WINDOWS)
    return
    !MS$ENDIF
    end


    SUBROUTINE MEMPLANT

    use common_block

    PMAFIL='IAMDONE'
    ABEND=.TRUE.
    RETURN
    END


    SUBROUTINE PHLPLT(PROFLE)
    CHARACTER*13 PROFLE
    PROFLE=PROFLE
    RETURN
    END


    FUNCTION CKUSER(I)
    INTEGER*2 CKUSER,I
    JDUM=I
    CKUSER=0
    RETURN
    END





