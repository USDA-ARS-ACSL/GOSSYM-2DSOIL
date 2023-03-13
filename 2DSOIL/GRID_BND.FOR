*||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*
CDT Nov, 2007 added a drainage boundary. It is input similar to seepage. The difference is
C*     that drainage always takes place from a drainage boundary - it is usually horizontal
C      seepage faces are usually used in ditches.
      Subroutine Get_Grid_and_Boundary()
      Include 'public.ins'
      Include 'nitvar.ins'
      Include 'PuSurface.ins'
*
      Integer e
      Logical lConst
      Dimension idummy(4)
      
*
cccz some parameter (constant) in the Nitvar.ins      
      NO3mass_2_Nmass=14.0067D0/62.0049D0
      NH4mass_2_Nmass=14.0067D0/18.039D0
      
      im=20
      il=0
*
!     KAT: if axisimmetrical or planar
!     NumNP: Number of nodal points
!     NumEl: Number of elements
!     NumBP: Number of boundary nodes
!     IJ: Maximum number of points on traverse lines
!     NumMat: number of soil material

      Open(40,file=GeometryFile,status='old',ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10) KAT,NumNP,NumEl,NumBP,IJ, NMat
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
C
C Reading nodal information
!     n: Nodal number
!     x(n): x cordinate of n
!     y(n): Y cordinate of n 
!     MatNumN(n): soil material number at node n
      Do i=1,NumNP
        il=il+1
        Read(40,*,ERR=10) n,x(n),y(n),MatNumN(n)
      Enddo
*  Reading element information
!     KX: nodal number of the element corner nodes
!     MatNuME: Material number of the element
      Num=0
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      Do e=1,NumEL
        il=il+1
        Read(40,*,ERR=10) Num,(KX(Num,i),i=1,4),MatNuME(Num)
cdt 10/21/2002 added this line to allow input of 3 node elements
        if(KX(Num,4).eq.0) KX(Num,4)=KX(Num,3)
      Enddo
C
C  Reading boundary information
C
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      Do i=1,NumBP
        il=il+1
        Read(40,*,ERR=10) n,(idummy(j),j=1,4),Width(i)
	KXB(i)=n
        CodeW(n)=idummy(1)                !reads the codes for water, solute, temperature and gas for each of the BP
        CodeS(n)=idummy(2)
        CodeT(n)=idummy(3)
        CodeG(n)=idummy(4)
        Do j=1,4
          If(idummy(j).ne.0) Movers(j)=1  !Active Movers  
        Enddo
      Enddo
C
C  Reading seepage face information
C
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10) NSeep
      If (NSeep.ne.0) then
        im=im+1
        il=il+1
        Read(40,*,ERR=10)
        im=im+1
        il=il+1
        Read(40,*,ERR=10) (NSP(i),i=1,NSeep)          ! Number of seepage face expected to develop
        im=im+1
        il=il+1
        Read(40,*,ERR=10)
        im=im+1
        Do i=1,NSeep
          il=il+1
          Read(40,*,ERR=10) (NP(i,j),j=1,NSP(i))
        Enddo
      EndIf
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10) NDrain
      If (NDrain.ne.0) then
        im=im+1
        il=il+1
        Read(40,*,ERR=10)
        im=im+1
        il=il+1
        Read(40,*,ERR=10) (NDR(i),i=1,NDrain)
        im=im+1
        il=il+1
        Read(40,*,ERR=10)
        im=im+1
        Do i=1,NDrain
          il=il+1
          Read(40,*,ERR=10) (ND(i,j),j=1,NDR(i))
        Enddo
      EndIf
      Close(40)
C
C
C       
      Do n=1,NumNP
        ListN(n)=1
        ListNE(n)=0
        nodeArea(n)=0.0D0
      Enddo
C
C  Area of elements and bandwidth MBand
C
      lOrt=.false.
      lConst=.true.
      MBand=1                 
      xMul=1.                             !xMul Modifying factor to transform equations of planar flow to equations of axisymmetric flow, cm
      Do e=1,NumEl
        Area(e)=0.
        NUS=4
        If(KX(e,3).eq.KX(e,4)) NUS=3
        Do k=1,NUS-2
          MB=1
          i=KX(e,1)
          j=KX(e,k+1)
          l=KX(e,k+2)
          CJ=x(i)-x(l)
          CK=x(j)-x(i)
          BJ=y(l)-y(i)
          BK=y(i)-y(j)
          AE=(CK*BJ-CJ*BK)/2.
          If(KAT.eq.1) xMul=2.*3.1416*(x(i)+x(j)+x(l))/3.
          Area(e)=Area(e)+xMul*AE
          ListNE(i)=ListNE(i)+1
          ListNE(j)=ListNE(j)+1
          ListNE(l)=ListNE(l)+1
          If(abs(i-j).gt.MB) MB=abs(i-j)
          If(abs(i-l).gt.MB) MB=abs(i-l)
          If(abs(j-l).gt.MB) MB=abs(j-l)
          If(MB.gt.MBand) MBand=MB
          If(e.eq.1.and.k.eq.1) then
            MB1=MB
          Else
            If(MB1.ne.MB) lConst=.false.
          Endif
        Enddo
      Enddo
cccz do the computaiton of nodeArea here, moved from the carbon_partitioning      
      Do e=1,NumEl
          If(KX(e,3).eq.KX(e,4)) then
              i=KX(e,1)
              j=KX(e,k+1)
              l=KX(e,k+2)
              nodeArea(i)=nodeArea(i)+Area(e)/3.0D0
              nodeArea(j)=nodeArea(j)+Area(e)/3.0D0
              nodeArea(l)=nodeArea(l)+Area(e)/3.0D0
          Else
              i=KX(e,1)
              j=KX(e,2)
              l=KX(e,3)
              k=KX(e,4)
              nodeArea(i)=nodeArea(i)+Area(e)/4.0D0
              nodeArea(j)=nodeArea(j)+Area(e)/4.0D0
              nodeArea(l)=nodeArea(l)+Area(e)/4.0D0        
              nodeArea(k)=nodeArea(k)+Area(e)/4.0D0 
          Endif
      Enddo
      
      
      MBand=MBand+1
      if(MBand.gt.MBandD.or.(2*MBand-1.gt.MBandD)) lOrt=.true.!
	
      If(.not.lConst) IJ=NumNP
CDT note SWMS_2D used 200   (lChem.and. -> this went before 2 and after .or)
      if(MBand.gt.10.or.NumNP.gt.200) lOrt=.true.
C
C  Numbers of boundary nodes where boundary conditions are not constant
C
      Do j=1,NumBP
        i=KXB(j)
        k=abs(CodeW(i))
        If(k.eq.3.or.k.eq.6) NVarBW=NVarBW+1
        If(k.eq.4) NSurf = NSurf +1
        k=abs(CodeS(i))
        If(k.eq.3.or.k.eq.6) NVarBS=NVarBS+1
        If(abs(CodeT(i)).eq.3) NVarBT=NvarBT+1
        If(abs(CodeG(i)).eq.3) NVarBG=NvarBG+1
      Enddo

        NumSol=1
        im=450
        il=0
      Open(41,File=NodeGeomFile,ERR=10)
        im=im+1
        il=il+1
        Read(41,*)
        im=im+1
        Read(41,*)
        im=im+1
        
cccz ------------------ The Carbon/Nitrogen Component ---------------------------------
c
c     Nh(NumNP)   [ug/g soil]     ELEMENTAL N in soil humus
c     Nl(NumNP)   [ug/g soil]     ELEMENTAL N in soil litter
c     Nm(NumNP)   [ug/g soil]     ELEMENTAL N in soil organic fertilizer
c     NNH4(NumNP) [ug/g soil]     Mineral N pool in the form of ammonium
c     NNO3(NumNP) [ug/g soil]     Mineral N pool in the form of nitrate
c
c     CL(NumNP)   [ug/g soil]     ELEMENTAL C in soil organic matter    
cccz ----------------------------------------------------------------------------------

        Do j=1,NumNP
          il=il+1
          Read(41,*,ERR=10)i,Nh(i),Ch(i),NL(i),CL(i),Nm(i),Cm(i),NNH4(i)
     &      ,(Conc(i,jjj),jjj=1,NumSol),Tmpr(i),hNew(i)
          Ch_old(i)=Ch(i)
          Nh_old(i)=Nh(i)
          CL_old(i)=CL(i)
          NL_old(i)=NL(i)
          Cm_old(i)=Cm(i)
          Nm_old(i)=Nm(i)
C     right now conc is ug NO3 per g soil. further conversions will be done in the solute mover initialization routine when we have water contents.
c          NNO3_old(i)=Conc(i,1)  
C   ammonia is input as ppm or ug /g soil here we convert it to ug/cm3 or mg per liter of soil volume
c          NNH4_old(i) =  NNH4(i)*blkdn(MatNumN(i))

cccz --------------------------------------------------------------------------------------------------------------------
cccz update the assignment
cccz the unit of Conc(i,1) is "ug NO3 per g soil", will need to updated to "ug NO3 per cm^3 soil water" in solmov
cccz the unit of NNO3_old(i) should be "ug N per g soil", converted from Conc(i,1), i.e.,
          NNO3_old(i)=Conc(i,1)*NO3mass_2_Nmass
cccz the unit of NNH4(i) and NNH4_OLD(i) should be "ug N per g soil", conversion needed, i.e.,
          NNH4(i)=NNH4(i)*NH4mass_2_Nmass
          NNH4_old(i)=NNH4(i)
cccz --------------------------------------------------------------------------------------------------------------------
          
          ThOld(i)=ThNew(i)
          TmprOld(i)=Tmpr(i)
          Denit(i)=0.0D0
          Denit_old(i)=0.0D0
        Enddo
      close(41)
      
! READING FROM Nodal_N.dat alternative file.  BY GR EDN      
      Write(*,*) 'Reading Grid Done'
      Return
10    Call errmes(im,il)
      End
