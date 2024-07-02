
    ! ************************************************************
    ! *                  CLIMATE SUBROUTINE 
    ! *Switch for polyna
    ! *calculation of INT: Fraction of incident lght interceted
    ! *by the plant canopy                                       *
    ! ************************************************************
    SUBROUTINE CLYMAT
    use common_block
    
    IF((RAIN*10).GT.12.7) THEN                           !if rain> 0.5 inch (mm/day)
        POLYNA = 0
           ELSE
        POLYNA = 1
    ENDIF
    
    ! *** LTYPE=1 OKRA LEAF.   LTYPE=0 NORMAL LEAF
    ! *** TEST OF CHANGE IN CANOPY LIGHT INTERCEPTION DUE TO PIX   06/29/90

    ZDUM = Z + ZPIXD * .6  !effect of Pix on plant height
    IF(LTYPE.EQ.1) THEN
        INT=(-2.05595+1.64301*ZDUM+(-.00648851*(ZDUM**2)))/100.
    ELSE
        INT = 1.0756*ZDUM/ROWSP
    ENDIF
    IF(INT.LT.0.) INT = 0.
    IF(INT.GE.0.95) INT = 0.95
    
    CALL JULANTOCAL(MO,DAZE,IYEAR,DAYNUM)
    ! *** INT = FRACTION OF INCIDENT LIGHT INTERCEPTED BY PLANT CANOPY.
    ! *** BAKER ET. AL. CANOPY ARCHITECTURE IN RELATION TO YIELD.
    ! *** CHAPTER 3 IN 'CROP PHYSIOLOGY' ED. V. S. GUPTO.

    IF(LAI.GT.LMAX) LMAX = LAI
    IF(LAI.LT.LMAX.AND.LAI.LT.3.1) THEN
        CCLAI = 0.0
        IF(Z.LT.ROWSP) THEN
            CCLAI = 3.1 * Z/ROWSP
            IF(LAI.LE.CCLAI) INT = INT * LAI / CCLAI
        ELSE
            INT=INT*LAI/3.1
        ENDIF
    ENDIF

    IF(INT.LT.0.) INT = 0.
    IF(INT.GE.0.95) INT = 0.95
  
    Cover= 1.0 - exp (-0.79*(area*100*poparea/10000.0))  !Portion of soil covered by crop
    shade=cover* ROWSP*Eomult  !Width of shaded strip of soil  surface
    height=min(shade, rowsp)
    
   
    RETURN
    END