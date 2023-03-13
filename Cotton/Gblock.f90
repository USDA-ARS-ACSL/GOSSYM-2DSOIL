
    SUBROUTINE GBLOCK

    ! ******** SIMULATED BLOCK DATA FOR GOSSYM **** DATE NOVEMBER 9, 1989 ********
    ! ******** CONVERTED BY WENDELL LADNER AND SUSAN BRIDGES FOR C ***************

    use common_block

    ABEND=.FALSE.
    ABZ=0.
    abz0=0.

    ! *** added (2) variables to differentiate rain from irrigation  gtheseira

    actirrg = 0.
    actrain = 0.
    add60 = 0.0

    ADDEDN=0.
    AGETOP=0.
    AIRDRI=0.
    AIRDRW=0.
    ALPHA=3.5
    APRES=0.0
    AREA=0.
    AT=.2
    AVAILN=0.
    AVGTSD=0.
    AVGTSP=0.
    AVTEMP=20.

    BDI=0.
    BDRATO=0.
    BDSLOP=0.
    BDW=0.
    BETAI=0.
    BETAW=0.
    BOLL1=0.
    BURCN=0.
    BURMIN=0.
    BURR1=0.
    BURRN=0.

    CD=0.0
    CDBOLL=0.
    CDLEAF=0.
    CDROOT=0.
    CDSQAR=0.
    CDSTEM=0.
    CHARI = 'I'
    CMXIRR = 0.

    ! *** fertigation variables for FRTLIZ (mg N/cu. cm of irrigation water)

    conamm = 0.
    connit = 0.
    conura = 0.

    COTXX=0.

    !     CO2 = 0

    CPOOL=0.
    CSTRES=1.
    CSTORE=1.0
    CUMEP=0.
    CUMES=0.
    CUMRAN=0.
    CUMSOK=0.
    CONVR=0.5
    DCELL=5.
    DAY1PN=0.
    DAY1SN=0.
    DEAD2DAY=0.
    DEFBGN=0
    DEFDAY=0
    DEFKGH=0.
    DAYLNG=13.
    DAYNUM=1
    DAYWTF=0.
    DAZE=0.0
    DIFF0I=0.
    DIFF0W=0.
    DZ=0.0

    dzprep = 1.0
    duration = 0.0
    daysq=0.
    dayfb=0.
    daymt=0.
    daysnf=0.
    daysnv=0.
    dd60 = 0.0

    EMERGE=0.
    EP=0.
    ES=0.

    F2=.5
    FBLOOM=0.
    FCINIC=.4
    FCFCTI=0.
    FCINIW=.2
    FERN=0.
    FILFRM='formatted'
    IF((OPSYS.EQ.'dos').OR.(OPSYS.EQ.'DOS')) FILFRM='binary'
    FL = 0.
    FLOSS=0.0
    FLNMIN=1.0E-6
    FNH4=0.
    FNO3=1.
    FRATIO=0.
    FS = 0.
    FSQ=0.
    FSTRES=0.

    frstbl = 0
    frstob = 0
    frstsq = 0
    fsqfra=0.0

    GEOTR=10.
    GAMMA=0.653
    GBLOS=0.
    GBOLWT=0.
    GBZ2=0.
    GIN=0.
    GINP=0.0
    GSUBR=.375
    growfac = 0.0

    ! *** TH2OADD replacements separates water infiltration on the left
    ! *** from right of the plant

    h2oaddl = 0.
    h2oaddr = 0.
    height=0.
    
    IDAY=0
    IFGIRR=0
    IFGRAIN=0
    INRIM=0
    INT=0.
    IPIX=1
    ISCRN=6
    ISQ=0

    isqday=0
    iflday=0
    igcmflg = 0.

    iobday=0
    imtday=0
    !	  ipdays=0
    irrflag=0

    KRAIN=0

    ! *** variables KULCLF and KULCRT are location ids of cultivated
    ! *** cells under regular and skiprow configuration

    kulclf = 7
    kulcrt = 14
    KWIDTH=0

    !JDAYC=1

    LAI=0.001
    LAMDAC=0.23
    LAMDAS=0.10
    LATUDE=35
    LDAYAW=0
    LDAYIR=0
    LDAYFW=0
    LDAYPW=0
    LEAFCN=.037
    LEAFR1=0
    LEAFRS=0
    leafwt=0.2
    !      leafwt=0.02
    LEFABS=0  !WEIGHT Of LEAVES ABSCISSED
    LEFCNT=0
    LINE=51
    LMAX=0
    LR=5
    LTYPE=0
    LVSLOS=0
    LYTRES=0.

    MH2O=0
    MMUPN1=0.
    MMUPN2=0.
    MMUPN3=0.
    MO=1

    !	mxfbrch = 30
    !	mxvbrch = 3
    !	mxfsite = 5
    !	mxfruts	= 450

    mxfbrch = 40
    mxvbrch = 5
    mxfsite = 6
    mxfruts	= 1200
    matday = 0.0

    NAPS=0
    NDLAY=0.
    NEWEP=0.0
    NEWES=0
    NF=0
    NFRQ=1
    NK=20
    NL=40
    NLOSS=0.
    NOITR=5
    NOPEN=0.
    NPOOL=0
    NUMPFN=1
    NR=0.
    NV=0.
    NVBRCH=1
    NYTTYM=0.
    NYTWTF=0
    NAVG=0
    n06=0
    Nsow=0
    ORGN=0.

    PDBOLL = 0.
    PDLEAF = 0.0001
    PDROOT = .0001
    PDSQ = 0.
    PDSTEM = .0001
    PI = 3.14159
    PLANTN = 0.
    PFAREA = 0.0
    PFDAL = 0.
    PFDWL = 0.
    PIN = 0.0
    PLANTW = 0.
    PLEFABS = 0.
    PIXCON = 0.
    PIXLOS = 0.
    PIXPLT = 0.
    PIXDA = 1.
    PIXDN = 1.
    PIXDZ = 1.
    PIXDPN=1.0
    PLTN = 0.
    PN = 0.
    POLYNA = 0
    POPFAC = 0.
    POPPLT = 41000.
    PQFLR = 0.
    PRPDAY = 0
    PRPKGH = 0.
    PSIM = -.175
    PSIAVG = -.175
    PSIAVG1 = -.175
    PSICMX = -0.5
    PSILD = -0.8
    PSILN = -0.8
    PSIMAX = 0.0
    PSINUM =  0.
    PTSRED = 1.
    PCRSD=0.
    PSIL_G=-0.8  ! minimum psil in a day obtained from soil 2d

    punits=0.0

    RAIN = 0.
    RCH2O = .0002 !ROOT CARBOHYDRATE SUPPLY PER PLANT, IN GM/PLANT
   ! RCH2O = 0.0!ROOT CARBOHYDRATE SUPPLY PER PLANT, IN GM/PLANT
    REQ1 = 0.
    RESC = .06
    RESN = 0.
    RI = 0.
    RN = 0.
    ROOTCN = .037  !AVERAGE NITROGEN CONCENTRATION IN ROOTS
    ROOTN = .00450 !Total root nitrogen
    ROOTR1 = 0.!Root nitrogen requirment for growth
    ROOTRS = 0.!Root reserves
    ROOTS = 0.!DRY WEIGHT OF ALL LIVING ROOTS IN PROFILE, IN GRAMS.
    ROOTWT = .200 !DRY WEIGHT OF ALL LIVING ROOTS PER PLANT
    !ROWSP = 101.6
    RSUBO = .0032
    RTP1 = .3
    RTP2 = .1
    RUTOFF = 0.

    redlfcn = 1.0
    rnfactor = 1.0

    SBOLL = 0.
    SDWBOL = 0.
    SDWLEF = 0.
    SDWSQR = 0.
    SDWSTM = 0.
    SEEDCN = 0.
    SEEDN = 0.
    SEEDR1 = 0.
    SEND = .FALSE.
    SESI = 0.
    SESII = 0.
    SITES = 0.
    SITEZ = 0.
    SKPFLG =  .FALSE.
    SLEAF = 0.
    SLEAFN = .0074
    SLF = .02
    SPDWBO = 0.
    SPDWLD = 0.
    SPDWLN = 0.
    SPDWRT = 0.
    SPDWSQ = 0.
    SPN = 0.
    SQUAR = 0.
    SQRZ = 0.
    SQWT = 0.
    SROOT = 0.
    SSTEM = 0.
    STEMCN = .037
    STEMN  = .0074
    STEMRS = 0.
    SUMEP = 0.0
    SUMES = 0.0
    SUMSTRS = 0.0
    SUPNO3 = 0.
    SUPNH4 = 0.
    SUMSUB = 0.
    SUBIRR = 0.
    SUPF = 0.


    strdays = 1
    stemwt=0.2
    !      stemwt=0.02
    str01=0.0
    str02=0.0
    str03=0.0
    str04=0.0
    str05=0.0
    str06=0.0
    sumxirr = 0.0
    sum_fsq_tavg = 0.0
    sum_fbl_tavg = 0.0
    sum_fob_tavg = 0.0
    ave_fsq_tavg = 0.0
    ave_fbl_tavg = 0.0
    ave_fob_tavg = 0.0

    T = 0.
    TAVG = 0.
    TCELL = 1.
    TD = 0.
    TEMP1C = 0.
    TEMP1G = 0.
    TEMP1R = 0.
    TH2O = 0.
    TDAY = 0.
    TDFKGH = 0.
    THETAI = 0.
    THRLN = 0.3E-3
    THTA0I = 0.
    THTA0W = 0.
    THTARC = 0.
    THTARI = 0.
    THTARW = 0.
    THTASC = 0.
    THTASI = 0.
    THTASW = 0.
    TIH2OC = 0.
    TMAX = 0.
    TMIN = 0.
    TNNH4 = 0.
    TNNO3 = 0.
    TNH4UP = 0.
    TNO3UP = 0.
    TNYT = 0.

    UPNH4 = 0.
    UPNO3 = 0.

    V = 0.
    VARNAME = 'MISSING'
    VERSION = '06/20/03'
    VSTRES = 0.

    WCELL = 5.
    WATTBL = 200.
    WATTSM = 0.
    WIND = 88.
    WSTRSD = 1.
    WSTRSN = 1.
    WTSLFD = 0.
    WSTRS = 1.

    XTRAC = 0.
    XTRAN = 0.

    YIELD = 0.
    IYEAR = 74.

    Z = 3.0
  !  zdum=0.
    
    !      Z = 0.1
    ZPIXD = 0.

    ! *** variable for leaf area development

    areapflf = 0.0
    areamslf = 0.0
    areafblf = 0.0

    ! ********************************************************************
    !    INITIALIZATION OF ARRAYS IS LISTED IN ASCENDING ORDER BY THE
    !    FIRST SUBSCRIPT ON THE ARRAY.  WITHIN AN INITIALIZATION OF
    !    ARRAYS OF THE SAME SIZE, THEY ARE LISTED ALPHABETICALLY.
    ! ********************************************************************

    ! *** LEAF AREA & LEAF WEIGHT INITIALIZED ACCORDING TO COTYLEDON
    ! *** DATA FOR 'M-8' COTTON OF CHRISTIANSEN, M. N. (1962) A METHOD
    ! *** OF MEASURING AND EXPRESSING EPIGNEOUS SEEDLING GROWTH RATE.
    ! *** CROP SCI. 2:487-488.


    ! ***********************(3),(3,30),(3,30,5)**************************
    DO 100 I=1,mxvbrch
        VDELAY(I) = 0.
        NFBR(I)=0
        DO 100 J=1,mxfbrch
            agemslf(i,j) = 0.0
            xmslfage(i,j) = 0.0
            xmslfarea(i,j) = 0.0
            DELAY(I,J)=0.
            MLAREA(I,J)=.04
            MLEAFW(I,J)=0.
            NNOD(I,J)=0
            PDAMLD(I,J) = 0.
            PDAMLN(I,J) = 0.
            PDWMLD(I,J) = 0.
            PDWMLN(I,J) = 0
            PRI(I,J) = '  '
            DO 100 K=1,mxfsite
                AGE(I,J,K)=0.
                AGEABZ(I,J,K)=0.0
                AGEBOL(I,J,K)=0.0
                AVGT(I,J,K)=0.
                BOLTMP(I,J,K)=0.
                BOLWGT(I,J,K)=0.
                BSIZE(I,J,K)=0.0
                DEHISS(I,J,K)=45.
                FCODE(I,J,K)=0.
                FFRUT(I,J,K)=0.
                FRUTP(I,J,K)=0.0
                FSTAVG(I,J,K)=0.5
                LAGE(I,J,K)=0.
                LAREA(I,J,K)=0.04
                LEAFW(I,J,K)=0
                MATURE(I,J,K)=0
                MCODE(I,J,K)=0
                PDADAY(I,J,K)=0.
                PDANYT(I,J,K) = 0.
                PDWBOD(I,J,K) = 0.
                PDWBON(I,J,K) = 0.
                PDWFLD(I,J,K) = 0.
                PDWFLN(I,J,K) = 0.
                PDWSQ(I,J,K) = 0.
                SQRWT(I,J,K) = 0.
                PRT(I,J,K) = '  '
                agefrlf(i,j,k) = 0.0
                frlfage(i,j,k) = 0.0
                frlfarea(i,j,k) = 0.0
100 CONTINUE

    ! *******************************(5)**********************************
    DO 110 I=1,5
        DEFDATE(I)=0
        DEFMTH(I)=0
        DEFPPA(I)=0.
        EPAVG(I)=.15
        KULDAY(I)=0
        FMTHOD(I)='      '
        HMTHOD(I)='      '
        IMTHOD(I)='      '
        PRPDATE(I)=0
        PRPMTH(I)=0
        PRPPPA(I)=0.
        gcm(i) = 0.0
110 CONTINUE
    FMTHOD(6)='      '
    FMTHOD(7)='      '
    HMTHOD(6)='      '
    HMTHOD(7)='      '
    IMTHOD(6)='      '
    IMTHOD(7)='      '


    FMTHOD(1)='BDCAST'
    FMTHOD(2)='SDRESS'
    FMTHOD(3)='FOLIAR'

    ! *** added (2) methods of fertilizer application:
    ! ***  1) broadcast/incorporated and 2) fertigation	  gtheseira

    FMTHOD(4)='bcstnc'
    FMTHOD(5)='frtgtn'

    HMTHOD(1)='BANDED'
    HMTHOD(2)='SPKLER'
    HMTHOD(3)='BDCAST'

    IMTHOD(1)='SPKLER'
    IMTHOD(2)='FURROW'
    IMTHOD(3)=' DRIP '

    ! ***  added (2) methods of irrigation application:
    ! ***  1) alternate drip and 2) alternate furrow   gtheseira

    IMTHOD(4)='altfur'
    IMTHOD(5)='altdrp'


    ! *******************************(7)**********************************
    DO 120 I=1,7
        DTAVG(I)=20.
        PGRUNT(I)='      '
120 CONTINUE
    PGRUNT(1)=' pts/a'
    PGRUNT(2)=' gal/a'
    PGRUNT(3)=' ozs/a'
    PGRUNT(4)=' lbs/a'
    PGRUNT(5)=' a/lb '
    PGRUNT(6)=' a/gal'

    CHAR1(1) = '-X'
    CHAR1(2) = '-*'
    CHAR1(3) = '-$'
    CHAR1(4) = '-A'
    CHAR1(5) = '-A'
    CHAR1(6) = '-A'
    CHAR1(7) = '-B'

    CHAR2(1) = 'X-'
    CHAR2(2) = '*-'
    CHAR2(3) = '$-'
    CHAR2(4) = 'A-'
    CHAR2(5) = 'A-'
    CHAR2(6) = 'A-'
    CHAR2(7) = 'B-'

    ! **************************(9)(9,40)******************************
    C1(1) = 0.3964D-0
    C1(2) = 0.3631D+1
    C1(3) = 0.3838D-1
    C1(4) = 0.7659D-1
    C1(5) = 0.0000D+0
    C1(6) = -0.2297D+2
    C1(7) = -0.3885D+0
    C1(8) = -0.1587D-0
    C1(9) = -0.01021D-1

    DO 130 I=1,9
        AIRDR(I)=0.
        BD(I)=0.
        BETA(I)=0.
        DIFF0(I)=0.
        FCININ(I)=.0
        FLXMAX(I)=0.
        FLXMIN(I)=0.
        GH2OC(I)=0.
        IPCLAY(I) = 30
        IPSAND(I) = 35
        LDEPTH(I) = 0
        THETA0(I) = 0.
        THETAR(I) = 0.
        THETAS(I) = 0.
        DO 130 J = 1,40
            TSTBD(I,J) = 0.
            TSTIMP(I,J) = 0.
130 CONTINUE


    ! *******************************(10)*********************************
    DO 140 I=1,10
        AGEPFN(I)=0.
        AVTPFN(I)=0.
        PIXDAY(I) = 0
        PIXMTH(I) = 0
        PIXPPA(I) = 0.
        PFAL(I) = 0.04
        PFDALD(I) = 0.
        PFDALN(I) = 0.
        PFDWLD(I) = 0.
        PFDWLN(I) = 0.
        PFWL(I) = 0.
140 CONTINUE

    DO I=1,10
        agepflf(i) = 0.0
        pflfage(i) = 0.0
        pflfarea(i) = 0.0
        cntpflfcn(i) = 0.0
        sumpflfcn(i) = 0.0
    enddo

    ! *******************************(12)*********************************

    KA(1) = ' '
    KA(2) = '0'
    KA(3) = '1'
    KA(4) = '2'
    KA(5) = '3'
    KA(6) = '4'
    KA(7) = '5'
    KA(8) = '6'
    KA(9) = '7'
    KA(10) = '8'
    KA(11) = '9'
    KA(12) = '*'

    ! *******************************(13)*********************************
    CHAR3(1) = '-1'
    CHAR3(2) = '-2'
    CHAR3(3) = '-3'
    CHAR3(4) = '-4'
    CHAR3(5) = '-5'
    CHAR3(6) = '-6'
    CHAR3(7) = '-7'
    CHAR3(8) = '-8'
    CHAR3(9) = '-9'
    CHAR3(10) = '-0'
    CHAR3(11) = '-A'
    CHAR3(12) = '-$'
    CHAR3(13) = '-X'
    CHAR4(1) = '1-'
    CHAR4(2) = '2-'
    CHAR4(3) = '3-'
    CHAR4(4) = '4-'
    CHAR4(5) = '5-'
    CHAR4(6) = '6-'
    CHAR4(7) = '7-'
    CHAR4(8) = '8-'
    CHAR4(9) = '9-'
    CHAR4(10) = '0-'
    CHAR4(11) = 'A-'
    CHAR4(12) = '$-'
    CHAR4(13) = 'X-'

    ! *******************************(14)*********************************
    DO 150 I=1,14
        H2OINT(I)=100.
150 CONTINUE

    OMA(1)=1.
    RNNH4(1) = 50.
    RNNO3(1) = 10.
    DO 160 I=2,14
        OMA(I)=0.
        RNNH4(I) = 0.
        RNNO3(I) = 0.
160 CONTINUE

    ! *******************************(15)*********************************
    DO 170 I = 1,15
        MSADTE(I) = 0
        DO 165 J=1,6
            NODPMAP(I,J)=0
165     CONTINUE
        TAIR(I) = 0.
170 CONTINUE

    ! *******************************(20)*********************************
    DO 180 I=1,20
        TSOLAV(I) = 0.
        PFNODAGE(I) = 0.
        PFNODLTH(I) = 0.
180 CONTINUE

    BETAK(1)=1.1429E-4
    BETAK(2)=1.1429E-4
    BETAK(3)=1.1429E-4
    BETAK(4)=0.8601E-4
    BETAK(5)=0.8601E-4
    BETAK(6)=0.8601E-4
    BETAK(7)=0.6534E-4
    BETAK(8)=0.6534E-4
    BETAK(9)=0.6534E-4

    CONSK(1)=7.7699E-4
    CONSK(2)=7.7699E-4
    CONSK(3)=7.7699E-4
    CONSK(4)=5.7685E-4
    CONSK(5)=5.7685E-4
    CONSK(6)=5.7685E-4
    CONSK(7)=4.1618E-4
    CONSK(8)=4.1618E-4
    CONSK(9)=4.1618E-4

    DO 190 I=10,20
        BETAK(I)=0.4853E-4
        CONSK(I)=3.2446E-4
190 CONTINUE

    ! *******************************(30)*********************************
    DO I=1,mxfbrch
        XMNODAGE(I) = 0.
        XMNODLTH(I) = 0.
        xnodage(i) = 0.0
        cntlfcn(i) = 0.0
        sumlfcn(i) = 0.0
    ENDDO

    ! ********************(40)(40,20)(40,20,3)(40,21)*********************
    DO 260 I=1,40
        BDL(I)=1.
        FC(I)=.267
        RTEXNT(I) = .FALSE.
        TSMN(I) = 25.
        TSMX(I) = 25.
        TSOILD(I) = 25.
        TSOILN(I) = 25.
        THTS(I) = 0.
        THTR(I) = 0.
        THAD(I) = 0.
        cntmslfcn(i) = 0.0
        summslfcn(i) = 0.0
        DO 270 J=1,20
            DIFF(I,J)=258.3
            KHAR(I,J) = ' '
            PSIS(I,J) =-.175
            ROOTSV(I,J) = 0.
            RTIMPD(I,J) = 0.
            RTWTCU(I,J) = 0.
            UPF(I,J) =0.
            VH2OC(I,J) = .267
            VNC(I,J) = 0.
            VNH4C(I,J) = 0.
            VNO3C(I,J) = 0.
270     CONTINUE
        DO 290 J=1,21
            FNL(I,J)=0.
            TUPF(I,J) = .TRUE.
            TTUPF(I,J) = .TRUE.
290     CONTINUE
260 CONTINUE

    ! **************************(41)(41,10)(41,20)(41,21)*******************
    DO 440 I=1,41
        DO 450 J=1,10
            FWU(I,J)=0.0
450     CONTINUE
        DO 460 J = 1,21
            PUPF(I,J) = 0.
460     CONTINUE
        DO 465 J = 1,20
            FNU(I,J)=0.
465     CONTINUE
440 CONTINUE

    ! **************************(365)(365,7)(365,5)***********************
    DO 530 I=1,365
        AMTIRR(I) = 0.
        MTHIRR(I) = 0
        RUNOFF(I) = 0.
        Bloom_tavg(i) = 0.0
        DO 540 J=1,7
            NFERT(I,J)=0
            CLIMAT(I,J)=0.
540     CONTINUE
530 CONTINUE

    ! ********************************(366)*******************************
    DO 610 I = 1, 366
        BOLOSS(I)=0.
        BLUM(I)=0.
        SQLOSS(I) = 0.
        STMWT(I) =  0.
610 CONTINUE
    
    
            
    RETURN
    END


