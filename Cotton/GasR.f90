
    
    subroutine GasExchange
    use common_block
    if (nratio.lt.0.1) nratio=0.1
    Call GasExchanger(CDayofYear,NRATIO)
    return
    end subroutine