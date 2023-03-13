* This is for tillage processes
* The input for the tillage processes will be read and processed on the day of tillage
* Tillage date: tillDate
* Tillage depth: tillDepth
* Switch for tillage: iTill
      
      Subroutine Tillage()
      Include 'public.ins'
      
      Integer tillApplied
      Integer iTill
      character*10 till_Date
      Character*132 InString
      Real till_Depth
      
      Common/Till/ModNum,iTill
      
*Read tillage input from management file      
      If (lInput.eq.1) then
          iTill=0
      Open(40,file=ManagementFile,err=20)
      
15       Read (40,'(A132)',end=200) InString
          if (InString(1:14).ne.'[Tillage]') goto 15
          
          Read(40,*,err=20) iTill                 !Itill =1, tillage is ON, iTill=0, No till (Default)
        
          if (iTill.eq.1)then
             read(40,*,err=20)
             Read(40,*,err=20) till_Date, till_Depth
             Write(*,*) 'Soil tilled on: ',till_Date
         else
             Write(*,*) 'No Tillage'
         end if 
         
      end if
 
200   Continue 
      close(40)
      Return
20    Stop 'Tillage data error'        
      end