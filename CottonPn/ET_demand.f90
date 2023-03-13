      SUBROUTINE ET
!  ************************************************************
!  *                                                          *
!  *           EVAPOTRANSPIRATION SUBROUTINE                  *
!  *                                                          *
!  ************************************************************
!  *** SUBROUTINE TAKEN ALMOST ENTIRELY FROM RITCHIE, A MODEL
!  *** FOR PREDICTING EVAPORATION FROM A ROW CROP WITH INCOMPLETE
!  *** COVER. WATER RESOURCES RESEARCH VOL. 8:1204.

      use common_block
     Real(4) LAMDACT, LAMDAST, GAMMAT
     LAMDACT=0.23
     LAMDAST=0.10
     GAMMAT=0.653
      U=6.
      P = RAIN*10   !unit P=mm/day 
      
!      RS = RI*.0169491525 ! ly/day   
      RS = RI/41868*.0169491525 ! ly/day   !in soil 2d Ri is in Joule/m2 so we need to / by 41868 to get in ly/day

! *** RS = SOLAR RADIATION IN MM H2O/DAY.
     
      TAVM1 = TAVG-1.
      DEL_ET = VP(TAVG) - VP(TAVM1)

! *** DEL=SLOPE OF SATURATION VAPOR PRESSURE CURVE AT MEAN AIR TEMP.

      LAMDA = INT*LAMDACT + (1.-INT)*LAMDAST

! *** LAMDAC & LAMDAS = ALBEDOS OF CROP & SOIL.
! *** INT=INTERCEPTION (FRACTION OF INCIDENT RS)
! *** RNO=NET RADIATION ABOVE CANOPY (MM/DAY)

      RNO=(RS-LAMDA*RS)

! *** TDRY & TWET = DRY AND WET BULB TEMPERATURES.
       IF(TMIN.LE.0) TMIN = 1.0
      TDRY = TAVG
      VPO = VP(TDRY)
      TWET = TMIN !temperature in degree C

! *** eot=POTENTIAL EVAPORATION RATE ABOVE CANOPY (MM/DAY)
! *** MODIFIED PENMAN EQ.
! *** WIND = WINDSPEED AT 2 METERS (MILES/DAY)
! *** GAMMA=PSYCHROMETER CONSTANT

      VPA = VP(TWET)
!      EO1=(RNO*DEL/GAMMA+.262*(1.+0.0061*WIND)*(VPO-VPA))/(DEL/GAMMA+1.)
       EOT=(RNO*DEL_ET/GAMMAT+.262*(1.+0.0061*WIND*14.9)*(VPO-VPA))/(DEL_ET/GAMMAT+1.)  !multiply 14.9 convert to km/hour to miles/day

! *** THE FOLLOWING CALCULATES esot(POTENTIAL EVAP. RATE AT SOIL SURFACE1
! *** RNS=NET RADIATION AT SOIL SURFACE BELOW CANOPY

      RNS=((1.-INT)-(1.-INT)*LAMDAST)*RS
      esot=DEL_ET*RNS/(DEL_ET+GAMMAT)

! *** STAGE I DRYING
! *** SESI=CUMULATIVE STAGE ONE EVAPORATION FROM SOIL SURFACE
! *** U=UPPER LIMIT OF SESI

      IF(SESI.GT.U)GOTO 100

! *** P=RAINFALL

      IF(P.GE.SESI)GOTO 101
      SESI=SESI-P
 99   SESI=SESI+esot
      IF(SESI.GE.U)GOTO 102
      ES=esot
      GOTO 110
 102  ES=esot-.4*(SESI-U)
      SESII=.6*(SESI-U)
      DUMY01 = SESII / ALPHA
      T = DUMY01 * DUMY01
      GO TO 110
 101  SESI=0.
      GO TO 99

! *** STAGE II DRYING

 100  IF(P.GE.SESII)GO TO 103
      T=T+1.
      ES = ALPHA * (SQRT(T)-SQRT(T-1.))
      IF(P.GT.0.)GO TO 104
      IF(ES.GT.esot)GO TO 105
 106  SESII=SESII+ES-P
      DUMY02 = SESII / ALPHA
      T = DUMY02 * DUMY02
      GO TO 110
 105  ES=esot
      GO TO 106
 104  ESX=0.8*P
      IF(ESX.LT.ES)GO TO 107
 111  IF(ESX.GT.esot)GO TO 108
 109  ES=ESX
      GO TO 106
 108  ESX=esot
      GO TO 109
 107  ESX=ES+P
      GO TO 111
 103  P=P-SESII
      SESI=U-P
      IF(P.GT.U)GO TO 101
      GO TO 99

! *** TRANSPIRATION IS PROPORTIONAL TO LIGHT INTERCEPTION (INT).
! *** THIS REPRESENTS A MODIFICATION TO RITCHIE'S MODEL.

 110  EP = INT * eot
      IF(EP.GT.(eot-ES)) EP=eot-ES

!       E = ES + EP
       AVGPSI = -1. * (PSIAVG)   !Average soil water potential (bars, positive value); equal to PSIAVG but opposite in sign.
      IF(AVGPSI.LT.-7.0) AVGPSI = -7.0
      IF(AVGPSI.GT.0.8) AVGPSI = 0.8
      !RN = RI*.71536-26.
      RNT =  RI/41868*.71536-26.

! *** RFEP = REDUCTION FACTOR FOR EVAPORATION FROM PLANT.  BASED ON
! *** UNPUBLISHED DATA OF BAKER & HESKETH. 1969.

      RFEPN = 749.5831405 + 0.9659065*RNT - 54.6600986*TAVG              &
       - 194.6508431*AVGPSI - 0.0010226*RNT*RNT + 1.0153007*TAVG*TAVG +	&
       29.775978*AVGPSI*AVGPSI + 0.0293687*RNT*TAVG						&
       - 4.206856*TAVG*AVGPSI
      RFEPD = 749.5831405 + 0.9659065*RNT					&
       - 54.6600986*TAVG - 19.46508431 - 0.0010226*RNT*RNT +	&
       1.0153007*TAVG*TAVG + .29775978 + 0.0293687*RNT*TAVG	&
       - .4206856*TAVG
      RFEP = RFEPN/RFEPD*1!CALBRT(25)
      IF(RFEP.LT.(0.2*LAI/2.0)) RFEP = 0.2 * LAI/2.0
      IF(RFEP.LT.0.2) RFEP = 0.2
      IF(RFEP.GT.1.0) RFEP = 1.0
      EP = EP * RFEP    !(mm/cm2/day)
                 !   Et_demand= (Ep*rowsp*eomult)/10   !mm/cm2/day to gram/slab/day
             !     Write(*,*) ES,EP
      ES_demand=ES
        
      RETURN
      END 

 
      FUNCTION VP(TMP)
 
! *** THIS FUNCTION CALCULATES SATURATION VAPOR PRESSURE
! *** FOR AIR TEMPERATURE.
 
      VP = EXP(1.8282+TMP*(0.07046136-TMP*0.000215743))
      RETURN
      END