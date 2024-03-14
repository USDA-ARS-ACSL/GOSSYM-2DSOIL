    SUBROUTINE PROFILES

    use common_block

    INTEGER*2 ISKWTH,IROWSP,NACRES !processed in SOIL2D
    

1000 FORMAT('1',28X,'MAFES-GOSSYM (TM)', /	&
        29X,'COPYRIGHT (C)  2003' / &
        12X,'Mississippi Agriculture & Forestry Experiment Station' /	&
        16X,'Contact:  Dr. K.R. Reddy and David W. Brand' /	&
        32X,'(662) 325-9462'/	&
        29X,'Version - ',A8 /)
1020 FORMAT(A80)
1040 FORMAT(// 4X,'Program: ',A13 / 4X,'Description: ',A51)
1060 FORMAT(/	&
        4X,'Emergence.......... ',I3,'/',I2,'/',I4,/	&
        4X,'Stop Simulation.... ',I3,'/',I2,'/',I4,/	& 
        4X,'Variety............ ',A14,/  &
        4X,'Season Length...... ',I11 /				&
        4X,'Row Spacing (in)... ',f11.2,/,4X,'Plants Per Row-Ft.. ',F11.2/ &
        4X,'Skip Width (in).... ',f11.2,/,4X,'Plants Per Acre.... ',I11)
1080 Format(/ 4X,'Plant Maps......... ',A14,5X,'PGR & Herbicides... ',A15/)

    !OPEN(11,FILE=profile,FORM=FILFRM,STATUS='OLD',ERR=570)
    !READ(11,*,ERR=580,END=600) PRFNAM,PDESCP,rundate,(ioutfg(i),i=1,6)          !GOSSYM profile name, description, run date, output flag
    PRFNAM="GOSSYM-2022"
    PDESCP="GOSSYM-2DSOIL-GasExchange"
    ioutfg(1)=1
    ioutfg(2)=1
    ioutfg(3)=1
    ioutfg(4)=0
    ioutfg(5)=0
    ioutfg(6)=0

    ioutfg(1)=1
    CLOSE(11)
    ivarty = 0
    LSTNG=7
    i=1
    OPEN(LSTNG,FILE=listfle,STATUS='UNKNOWN')                                   !.out file
    WRITE(LSTNG,1000) VERSION
    !WRITE(LSTNG,1040) PRFNAM,PDESCP

    CALL CALTOJULAN(EMERG,IDUM1,IDUM2,IYEAR,EMERGE)                             !Emerg: Date format, Emerge: Julian day format
    CALL CALTOJULAN(startCropSim,IDUM1,IDUM2,IDUM3,JDSTRS)                            !Crop start
    CALL CALTOJULAN(stopCropSim,IDUM1,IDUM2,IDUM3,JDSTPS)                            !Crop end
    
    ! ***	end of planting date routine
    IF((JDSTRS.LE.0).OR.(JDSTPS.LE.0)) THEN
        PRINTBUF=' error on reading date in profile'
        CALL WRITERR
        ABEND=.TRUE.
        RETURN
    ELSE
        IF((JDSTRS.GT.EMERGE).AND.(EMERGE.GT.0)) THEN
            JDSTRS=EMERGE
            startCropSim=EMERG
        ELSEIF(EMERGE.LE.0) THEN
            EMERGE = 366
        ENDIF
        IVARTY = IVARTY+1
        !SKIPWD = ISKWTH*2.54
        !ROWSP = IROWSP*2.54
        !ROWSAVE = ROWSP
        !IF(SKIPWD.GT.1.0) ROWSP = (ROWSP+SKIPWD)/2.0

        !POPPLT = PLTPFT/ROWSP*1327709.!This is population per acre
        !poparea: Population per m2
        !poprow: Population per m row
        !POPSLAB: Plant population per soil slab
        POPSLAB=(ROWSP*Eomult*poparea)/10000 
        !pltpft: Population per ft row
        POPPLT=POPAREA/0.000247105
        PLTPFT=POPROW/3.28084


        !IF(.NOT.ABEND) CALL VARIETYS
        CALL VARIETYS
        J0=POPPLT+.5
        J1=JDSTPS-JDSTRS+1

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
        
        WRITE(LSTNG,1060) imoemg,idyemg,iyremg, &
            imostp,idystp,iyrstp,VARITY(IVARTY), &
            J1,ROWSP/2.54,PLTPFT,SKIPWD/2.54,J0
 
    ENDIF
    RETURN
570 CONTINUE
    PRINTBUF=' error on open of profile file'
    CALL WRITERR
    ABEND=.TRUE.
    RETURN
580 CONTINUE
    PRINTBUF=' error on read in profile'
    CALL WRITERR
    ABEND=.TRUE.
    CLOSE(11)
    RETURN
600 CONTINUE
    PRINTBUF=' end-of-file while reading profile'
    CALL WRITERR
    ABEND=.TRUE.
    CLOSE(11)
    RETURN
    END

    
    

    SUBROUTINE CALTOJULAN(MODYYR,MONTH,IDAY,IYEAR,JDAYC)
    !  ************************************************************
    !  *                                                          *
    !  *   DATE SUBROUTINE.  CONVERTS CALENDAR DAY TO JULIAN      *
    !  *    DAY AND ALLOWS FOR LEAP YEARS.                        *
    !  *                                                          *
    !  ************************************************************

    CHARACTER*10 MODYYR
    DIMENSION I0(12)

    I0(1) = 0
    I0(2) = 31
    I0(3) = 59
    I0(4) = 90
    I0(5) = 120
    I0(6) = 151
    I0(7) = 181
    I0(8) = 212
    I0(9) = 243
    I0(10) = 273
    I0(11) = 304
    I0(12) = 334

    IF(MODYYR.NE.' ') THEN
        MONTH = (ICHAR(MODYYR(1:1))-48)*10 + (ICHAR(MODYYR(2:2))-48)
        IDAY = (ICHAR(MODYYR(4:4))-48)*10 + (ICHAR(MODYYR(5:5))-48)

        !        IYEAR = (ICHAR(MODYYR(7:7))-48)*10+(ICHAR(MODYYR(8:8))-48)
        ! *** Changes to make the model read 4 digit year

        IYEAR = (ICHAR(MODYYR(7:7))-48)*1000+(ICHAR(MODYYR(8:8))-48)*100 &
            + (ICHAR(MODYYR(9:9))-48)*10 + (ICHAR(MODYYR(10:10))-48)
    ENDIF

    IF((MONTH.LE.0).OR.(IDAY.LE.0)) THEN
        JDAYC=0
    ELSE
        if((mod(iyear,4).eq.0).and.(mod(iyear,100).ne.0)) then
            do I=3,12
                I0(I) = I0(I) + 1
            enddo
        endif
        if((mod(iyear,100).eq.0).and.(mod(iyear,400).eq.0)) then
            do I=3,12
                I0(I) = I0(I) + 1
            enddo
        endif

        !        IF(IYEAR/4*4.EQ.IYEAR) THEN
        !          DO 100 I=3,12
        !             I0(I) = I0(I) + 1
        !  100     CONTINUE
        !        ENDIF

        IF(MONTH.GT.12) MONTH=12
        JDAYC = I0(MONTH) + IDAY
    ENDIF
    RETURN
    END


    SUBROUTINE JULANTOCAL(MONTH,IDAY,IYEAR,JULIAN)
    !  ************************************************************
    !  *                                                          *
    !  *   DATE SUBROUTINE.  CONVERTS JULIAN TO CALENDAR AND      *
    !  *    ALLOWS FOR LEAP YEARS.                                *
    !  *                                                          *
    !  ************************************************************

    INTEGER DACNT(12)

    DACNT(1) = 31
    DACNT(2) = 28
    if((mod(iyear,4).eq.0).and.(mod(iyear,100).ne.0)) DACNT(2) = 29
    if((mod(iyear,100).eq.0).and.(mod(iyear,400).eq.0)) DACNT(2) = 29

    !      IF(IYEAR/4*4.EQ.IYEAR) DACNT(2) = 29

    DACNT(3) = 31
    DACNT(4) = 30
    DACNT(5) = 31
    DACNT(6) = 30
    DACNT(7) = 31
    DACNT(8) = 31
    DACNT(9) = 30
    DACNT(10) = 31
    DACNT(11) = 30
    DACNT(12) = 31

    MONTH = 1
    IDAY = JULIAN
    DO 100 I=1,12
        IF(IDAY.LE.DACNT(I)) GO TO 200
        MONTH = MONTH + 1
        IDAY = IDAY - DACNT(I)
100 CONTINUE
200 CONTINUE
    RETURN
    END
    SUBROUTINE PGRHRBCDE

    use common_block

    INTEGER*2 PGRMTH,PGRBDW,PGUNIT,Pgr_time,Npgr
    REAL RTEPGR
    Character InStringpgr*132
    CHARACTER PGRBRD*14,PGRNAM*13,DESCRP*51,PGRDTE*10

1000 FORMAT(///  &
        '   DATE      BRAND       METHOD     BAND WTH      COST  ', &
        '   RATE      UNITS' /  &
        '             NAME                     (in)       ($/a)' /)
1020 FORMAT(1X,A10,2X,A8,4X,A6,5X,I5,6X,F7.2,2X,F7.2,5X,A6)
1040 FORMAT(13X,     A8,4X,A6,5X,I5,6X,F7.2,2X,F7.2,5X,A6)

      Pgr_time=0

      Open(40,file=ManagementFile,ERR=200)
     150       Read (40,'(A132)', END=380) InStringpgr
          if (InStringpgr(1:14).ne.'[PGR]') goto 150

      
        IPX = 0
        IPRP = 0
        IDEF = 0
        I = 0
        Read(40,*,Err=200)                  !Read text
        READ(40,*,ERR=200,END=300) Pgr_time               
        Read(40,*,Err=200)                  !Read text
        if (Pgr_time .ne.0.)then
            I=I+1
        Do npgr=1,Pgr_time 
            j = 1
            READ(40,*,ERR=200,END=300) PGRDTE,PGRBRD,PGRMTH,	&
                PGRBDW,RTEPGR,PGUNIT
            !1= PGR date
            !2= PGR brand,
            !   			'PIX'
			!               'PREP'
           	!		        'DEF'
            !        		'DROPP'
            !       		'HARVADE'
            !               'GRAMOXON'
            !3= PGR method:Code: 0 = Banded
            !                 1 = Sprinkler
            !                 2 = Broadcast
            !4 = Band width of application, in.
            !5 = Rate of plant growth regulator application
            !6 = Units of applied plant growth regulator

            CALL CALTOJULAN(PGRDTE,IDUM1,IDUM2,IDUM3,jdpgr)
            if((jdpgr.ge.JDSTRS).and.(jdpgr.le.JDSTPS))then
                IF(I.EQ.1) THEN
                    WRITE(lstng,1000)
                    i = 0
                ENDIF

                CSTPGR = 0.0
                L=2
120             CONTINUE
                IF((PGRBRD(L:L).NE.'.').AND.(PGRBRD(L:L).NE.' ') &
                    .AND.(PGRBRD(L:L).NE.CHAR(0)).AND.(L.LT.14)) THEN
                    L=L+1
                    GO TO 120
                ENDIF
                DO 140 K=L,14
                    PGRBRD(K:K)=' '
140             CONTINUE
                IF(((PGRBRD(1:3).EQ.'Pix').OR.(PGRBRD(1:3).EQ.'PIX')) &         
                    .AND.(RTEPGR.GT.0.01)) THEN
                    IPX=IPX+1                                                   !Number of pix application count
                    CALL CALTOJULAN(PGRDTE,IDUM1,IDUM2,IDUM3,PIXDAY(IPX))
                    DUMRTE=RTEPGR
                    IF(PGUNIT.EQ.1) DUMRTE=RTEPGR*8
                    IF(PGUNIT.EQ.2) DUMRTE=RTEPGR/16.
                    IF(PGUNIT.EQ.4) DUMRTE=1./RTEPGR
                    IF(PGUNIT.EQ.5) DUMRTE=8./RTEPGR                            !Checking the units of the rate of application
                    PIXPPA(IPX)=DUMRTE                                          !Pix rate
                    PIXMTH(IPX)=PGRMTH                                          !Pix method
                ELSEIF(((PGRBRD(1:4).EQ.'Prep').OR.	&
                    (PGRBRD(1:4).EQ.'PREP')).AND.(RTEPGR.GT.0.01)) THEN
                    IPRP = IPRP + 1                                             !Number of prep applicaiton count
                    CALL CALTOJULAN(PGRDTE,IDUM1,IDUM2,IDUM3,PRPDATE(IPRP))
                    DUMRTE=RTEPGR
                    IF(PGUNIT.EQ.1) DUMRTE=RTEPGR*8                             ! all units are converted to lb/acre
                    IF(PGUNIT.EQ.2) DUMRTE=RTEPGR/16.
                    IF(PGUNIT.EQ.4) DUMRTE=1./RTEPGR
                    IF(PGUNIT.EQ.5) DUMRTE=8./RTEPGR
                    PRPPPA(IPRP) = DUMRTE                                       !Prep application rate( lb/acre)
                    PRPMTH(IPRP) = PGRMTH                                       !Prep application method
                ELSEIF(((PGRBRD(1:3).EQ.'DEF').OR. &
                    (PGRBRD(1:5).EQ.'DROPP').OR. &
                    (PGRBRD(1:7).EQ.'HARVADE').OR. &
                    (PGRBRD(1:8).EQ.'GRAMOXON').OR.	&
                    (PGRBRD(1:3).EQ.'Def').OR. &
                    (PGRBRD(1:5).EQ.'Dropp').OR. &
                    (PGRBRD(1:7).EQ.'Darvade').OR. &
                    (PGRBRD(1:8).EQ.'Dramoxon')).AND. &
                    (RTEPGR.GT.0.01)) THEN
                    IDEF = IDEF + 1                                             !Rest all the methods are considered as Def
                    CALL CALTOJULAN(PGRDTE,IDUM1,IDUM2,IDUM3,DEFDATE(IDEF))
                    DUMRTE=RTEPGR
                    IF(PGUNIT.EQ.1) DUMRTE=RTEPGR*8
                    IF(PGUNIT.EQ.2) DUMRTE=RTEPGR/16.
                    IF(PGUNIT.EQ.4) DUMRTE=1./RTEPGR
                    IF(PGUNIT.EQ.5) DUMRTE=8./RTEPGR
                    DEFPPA(IDEF)=DUMRTE                                         !Def application rate (lb/acre)
                    DEFMTH(IDEF)=PGRMTH                                         !Def method
                ENDIF

                IF(PGRBRD(1:1).NE.CHAR(0)) THEN
                    K=PGRMTH+1
                    L=PGUNIT+1
                    IF(J.EQ.1) THEN
                        WRITE(lstng,1020) PGRDTE,PGRBRD(1:8),HMTHOD(K), &
                            PGRBDW,CSTPGR,RTEPGR,PGRUNT(L)
                    ELSE
                        WRITE(lstng,1040) PGRBRD(1:8),HMTHOD(K), &
                            PGRBDW,CSTPGR,RTEPGR,PGRUNT(L)
                    ENDIF
                ENDIF
            endif
            if (npgr.eq.Pgr_time ) goto 300
            End do
200         CONTINUE
             Stop 'Error on read in PGR & Herbicide file'
300         CONTINUE
            IF(IDEF.GT.0) THEN
                IF((DEFDATE(1).LT.DEFBGN).OR.(DEFBGN.LE.0)) &
                    DEFBGN = DEFDATE(1)                         !First date of defoliation
                DEFDAY = DEFDATE(1)                             !First date of defoliation
            ENDIF
            IF(IPRP.GT.0) THEN
                IF((PRPDATE(1).LT.DEFBGN).OR.(DEFBGN.LE.0)) &
                    DEFBGN = PRPDATE(1)                         !First day of prep or DEF
                PRPDAY = PRPDATE(1)                             !First day of prep
            ENDIF
        end if
380         CONTINUE
            close(40)
        RETURN
    END
