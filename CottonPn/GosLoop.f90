    ! Daily simulation of the crop from Emerging day to end day


    SUBROUTINE GosLoop

    use common_block
    KDAY=JDAY-EMERGINGDAY+1                                         !JDAY=DAYNUM, KDAY starts from 1 on emergence day  
   
    
    !Pix takes energy away from leaf and stem development and 
    !directs it toward boll development and retention.
    !Pixday(1) is the first day pf Pix
    IF((PIXDAY(1).GT.0).AND.(DAYNUM.GE.PIXDAY(1))) CALL PIX         
    
    !Defoliation force cotton leaves to drop from the plant
    !allowing harvest of the crop in a timely manner
    !DEFBGN is the first day of Prep or DEF
    IF((DEFBGN.GT.0).AND.(DAYNUM.GE.DEFBGN)) call defoliat          
    CALL PNET                                                       !Calculate gross photosynthate and net photo.
    CALL GROWTH                                                     !C allocation, water stress, N stress, potential growth and actual growth
    CALL PLTMAPS                                                    !Addition of new nodes, leaves, square, green boll etc
    CALL ABCISE                                                     !Abscision of different plant parts
    CALL MATBAL                                                     !Calculate the total plant weight
    CALL COUTPUT                                                    !Write output to .g01 file and stores some info in the binary format to be accessed later 
    
    RETURN
    END