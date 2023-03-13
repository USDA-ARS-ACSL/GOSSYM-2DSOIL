    ! Obtain the input details specific to Crop
      
    
    SUBROUTINE INITIALIZE
    use common_block
   
    CALL GBLOCK     !initialise variables and parameters
    CALL PROFILES   !passing basic crop features and read variety
    CALL PGRHRBCDE
    CALL PLANTMAPS  !Crop feature
    CALL INITIAL    !Calcuate POPFAC, PNETCOR
    
    RETURN
    END
    
    
        SUBROUTINE INITIAL
    !                    I N I T I A L I Z E
    ! ************************************************************
    ! *   INITIAL SETUP CALCULATIONS FOR THE MODEL               *
    ! *   Mostly soil parameters and CO2 function  Kit 1/20/2000 *
    ! ************************************************************

    use common_block

    !WCELL = ROWSP/20.0
    !ACELLDW = DCELL * WCELL
    !VCELL = DCELL * WCELL * TCELL
    !LPLOW = 20 / DCELL
    !THRLN = THRLN*(ACELLDW/25.)
    POPFAC = 404685.6/POPPLT


    if(co2.eq.0.0) then
        IF(IYEAR.GT.1900) INDXCO2 = IYEAR - 1959
        IF(INDXCO2.LT.1) INDXCO2 = 1
        PNETCOR = 1.0208594 + 0.0021710*INDXCO2 + 0.0000717*INDXCO2**2
    else

        ! *** New Carbon vs CO2 function. Assume base CO2=320 ppm  Reddy 11/99

        stdco2 = 320.0
        co2num = (0.0179*co2*5.7194)/(0.0179*co2 + 5.7194)
        co2den = (0.0179*stdco2*5.7194)/(0.0179*stdco2 + 5.7194)
        pnetcor = co2num/co2den
    endif

    RETURN
    END