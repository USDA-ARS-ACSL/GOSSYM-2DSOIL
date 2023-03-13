      subroutine Root_Mover()
      Include 'public.ins'
      Include 'puplant.ins'
      Double precision A,B,C,P,Sum
      Character InString*132
      Integer newjjj,RootAge
      Real YRW, MRW,class1,class2,class3

C  AD added RMass, RMassM, MRL into Public.ins 2011-08-01 
C AD 12-2-2011 this code from MaizSim10.11 is based on Yakov Pachepsky's diffusion code
      
      Dimension A(MBandD,NumNPD),B(NumNPD),F(NumNPD),DS(NumNPD),
     &            Gc(NumNPD),Sc(NumNPD),Fc(NumNPD),
     &            MRL(NumNPD),RDenTotal(NumNPD)
      Common /RootM/  DMolx,DMolz,Vel,Ac(NumNPD),
     !                NLevel,dt,epsi,CourMax,lUpW,
     !                S(3,3),Wz(3),Wx(3),Ri(3),Ci(3),Bi(3),List(3),
c     !         VxROld(NumNPD),VzROld(NumNPD),
c     !         VxR(NumNPD), VzR(NumNPD), hOld(NumNPD),tOld(NumNPD),
c     !         VxRH(NumNPD),VzRH(NumNPD),WeTab(3,2*NumElD),
     !                Dispzz(NumNPD),Dispxx(NumNPD),Dispxz(NumNPD),
     !                RMassOld(NumNPD),RDenTotal
      If (lInput.eq.0) goto 11
*      open(109,file='class')
      Open(40,file = VarietyFile, status = 'old',ERR = 10)

      im=70
      il=0
55       Read (40,'(A132)') InString
          if (InString(1:14).ne.'[RootDiff]') goto 55
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10)
      im=im+1
      il=il+1
      Read(40,*,ERR=10) epsi,lUpW,CourMax
      Nlevel=1
      if(epsi.lt.0.999) NLevel=2  
      im=im+1
      il=il+1
      Read(40,*,ERR=10)     
      im=im+1
      il=il+1
      Read(40,*,ERR=10) DMolx,DMolz,Vel
      close(40)

      Do n=1,NumNP
        RMassOld(n)=RMassYC(n)
c        VxR(n)=0.0D0
c        VxROld(n)=0.0D0
c        VzROld(n)=VzR(n)
      Enddo
      Movers(4)=1
      Return
C	 
C  Routine calculations
      
      
       
       
11    Continue 

      
C  No Plant = No Root Activity
      If(NShoot.eq.0) Return
      
      
      t=Time
      dt = Step
      xMul=1.0D0
      alf=1.0D0-epsi
      newjjj=MBand
c  	 
      Do 13 n=1,NumNP
       DS(n)=0.0D0
       Gc(n)=0.0D0
       Sc(n)=0.0D0
       B(n) =0.0D0
       Fc(n)=0.0D0
c          Ac(i)=0.
         If(lOrt) B1(n)=RMassY(n)
         
          If(epsi.lt.0.001D0) then
            if(lOrt) newjjj = IADD(n)
            A(newjjj,n)=0.0D0
          Else
          
          Do 12 j=1,MBandD
              A(j,n)=0.0D0
12          Continue
          Endif
13      Continue
c
c        A loop to calculate geotropic velocity
CDT     we are going to not use this as a first approximation.
c
c        Do n=1,NumNP
c           VzR(n)=Vel*f1(hNew(n))*f2(Tmpr(n))
c     &         *f3(RDenY(n)+RDenM(n))
C          VzR(i)=Vel*RGCF(i) 
C        Enddo
C
C  assign fluxes of root growth
        Do n=1,NumNP
          Gc(n)=ADWR(n)/nodeArea(n)
        Enddo
C
C Assembling matrixes
C
        Do 21 Level=1,NLevel
         If(lUpW.eq.1) then
!          If(Level.eq.NLevel) then
!            Do n=1,NumNP
!cccz test there is no convection
!                if ((abs(VxR(n))+abs(VzR(n))).gt.0.0D0) then
!                 aaaa=1
!                endif
!cccz
!             VxRH(n)=VxR(n)
!             VzRH(n)=VzR(n)
!            Enddo
!          Else
!	      Do n=1,NumNP
!cccz test there is no convection
!             if ((abs(VxROld(n))+abs(VzROld(n))).gt.0.0D0) then
!                 aaaa=1
!             endif
!cccz
!             VxRH(n)=VxROld(n)
!             VzRH(n)=VzROld(n)
!            Enddo
!          Endif
          
          Call RDisper(NumNP,Dispxx,Dispzz,Dispxz,DMolx,DMolz,
     &             hNew,Tmpr,RDenTotal)
!          Call RWeFact(NumNP,NumEl,NumElD,x,y,KX,WeTab,VxRH,VzRH,
!     &             Dispxx,Dispzz,Dispxz)
         Endif
         If(Level.eq.NLevel) then
          do 14 n=1,NumNP
           Ac(n)=-1
14        continue
         Endif
         Do 15 n=1,NumNP
            F(n)=0.
            If(Level.eq.NLevel) DS(n)=0.
15        Continue
          NumSEl=0
          Do 19 n=1,NumEl
            NUS=4
            If(KX(n,3).eq.KX(n,4)) NUS=3
            Do 18 k=1,NUS-2
              NumSEl=NumSEl+1
              i=KX(n,1)
              j=KX(n,k+1)
              l=KX(n,k+2)
              List(1)=i
              List(2)=j
              List(3)=l
              Ci(1)=x(l)-x(j)                    ! cccz the order: ijl is counter-clockwise   
              Ci(2)=x(i)-x(l)
              Ci(3)=x(j)-x(i)
              Bi(1)=y(j)-y(l)
              Bi(2)=y(l)-y(i)
              Bi(3)=y(i)-y(j)
              AE=(Ci(3)*Bi(2)-Ci(2)*Bi(3))/2.
              If(KAT.eq.1) xMul=2.*3.1416*(x(i)+x(j)+x(l))/3.
              FMul=xMul*AE/4.
              GcE=(Gc(i)+Gc(j)+Gc(l))/3.
              Ec1=(Dispxx(i)+Dispxx(j)+Dispxx(l))/3.
              Ec2=(Dispxz(i)+Dispxz(j)+Dispxz(l))/3.
              Ec3=(Dispzz(i)+Dispzz(j)+Dispzz(l))/3.
              AcE=(Ac(i)+Ac(j)+Ac(l))/3.
!              FcE=(Fc(i)+Fc(j)+Fc(l))/3.
!              VxE=(VxR(i)+VxR(j)+VxR(l))/3.
!              VzE=(VzR(i)+VzR(j)+VzR(l))/3.
              SMul1=-1./AE/4.*xMul
!              SMul2=AE/20.*xMul
!cccz test there is no convection
!               if ((abs(VxE)+abs(VzE)).gt.0.0D0) then
!                 aaaa=1
!               endif
!cccz
!              If(lUpW.eq.1) then
!                NS=NumSEl
!                W1=WeTab(1,NS)
!                W2=WeTab(2,NS)
!                W3=WeTab(3,NS)
!                Wx(1)=
!     &          2.*VxRH(i)*(W2-W3)+VxRH(j)*(W2-2.*W3)+VxRH(l)*(2.*W2-W3)
!                Wx(2)=
!     &          VxRH(i)*(2.*W3-W1)+2.*VxRH(j)*(W3-W1)+VxRH(l)*(W3-2.*W1)
!                Wx(3)=
!     &          VxRH(i)*(W1-2.*W2)+VxRH(j)*(2.*W1-W2)+2.*VxRH(l)*(W1-W2)
!                Wz(1)=
!     &          2.*VzRH(i)*(W2-W3)+VzRH(j)*(W2-2.*W3)+VzRH(l)*(2.*W2-W3)
!                Wz(2)=
!     &          VzRH(i)*(2.*W3-W1)+2.*VzRH(j)*(W3-W1)+VzRH(l)*(W3-2.*W1)
!                Wz(3)=
!     &          VzRH(i)*(W1-2.*W2)+VzRH(j)*(2.*W1-W2)+2.*VzRH(l)*(W1-W2)
!cccz cccz test there is no convection
!             if ((abs(VxRH(i))+abs(VxRH(j))+abs(VxRH(k))).gt.0.0D0) then
!                 aaaa=1
!              endif
!             if ((abs(VzRH(i))+abs(VzRH(j))+abs(VzRH(k))).gt.0.0D0) then
!                 aaaa=1
!         endif
!              Endif
            do 17 j1=1,3
              i1=List(j1)
              F(i1)=F(i1)+FMul*(GcE+Gc(i1)/3.)
              if(Level.eq.NLevel) DS(i1)=DS(i1)+FMul*(AcE+Ac(i1)/3.)
              do 16 j2=1,3
                i2=List(j2) 
                S(j1,j2)=SMul1*(Ec1*Bi(j1)*Bi(j2)+Ec3*Ci(j1)*Ci(j2)+
     !                         Ec2*(Bi(j1)*Ci(j2)+Ci(j1)*Bi(j2)))
!                S(j1,j2)=S(j1,j2)-(Bi(j2)/8.*(VxE+VxR(i1)/3.)+
!     !                            Ci(j2)/8.*(VzE+VzR(i1)/3.))*xMul
!                if(lUpW.eq.1) S(j1,j2)=S(j1,j2)-xMul*
!     !                            (Bi(j2)/40.*Wx(j1)+Ci(j2)/40.*Wz(j1))
!                ic=1
!                if(i1.eq.i2) ic=2
!                S(j1,j2)=S(j1,j2)+SMul2*ic*(FcE+(Fc(i1)+Fc(i2))/3.)
                if(Level.ne.NLevel) then
                  B(i1)=B(i1)-alf*S(j1,j2)*RMassY(i2)
                else
                  If (lOrt) Then
                    call Find(i1,i2,kk,NumNP,MBandD,IAD,IADN)
                    ib=kk
                  Else
                     iB=MBand+i2-i1
                  Endif
                     A(iB,i1)=A(iB,i1)+epsi*S(j1,j2)
                endif
16            continue
17          continue
18          Continue
19        Continue
          Do 20 i=1,NumNP
            If(Level.ne.NLevel) then
              B(i)=B(i)-alf*F(i)
            Else
              if (lOrt) newjjj=IADD(i)
              A(newjjj,i)=A(newjjj,i)+DS(i)/dt
c               A(MBand,i)=A(MBand,i)+DS(i)/dt
              B(i)=B(i)+DS(i)/dt*RMassY(i)-epsi*F(i)
            Endif
20        Continue
21      Continue
C
C  Solve the global matrix equation for transport
C
        If(epsi.lt.0.001) then
          Do 22 i=1,NumNP
          if (lOrt) newjjj = IADD(i)
             B(i)=B(i)/A(newjjj,i)
c            B(i)=B(i)/A(MBand,i)
22        Continue
        Else if(lOrt) then
cccz             
c             print*,time,',','Root Diff'
	       call ILU(A,NumNP,MBandD,IAD,IADN,IADD,A1)
             call OrthoMin(A,B1,B,NumNP,MBandD,NumNPD,IAD,
     !	            IADN,IADD,A1,VRV,
     !                RES,RQI,RQ,QQ,QI,RQIDOT,ECNVRG,RCNVRG,ACNVRG,4,
     !                MNorth,MaxItO)
        Else
C
C  Solve the system of linear equations
C
          N1=NumNP-1
          Do 212 k=1,N1
            P=1./A(MBand,k)
            kk=k+1
            kc=MBand
            Do 211 i=kk,NumNP
              kc=kc-1
              If(kc.le.0) goto 212
              C=-P*A(kc,i)
              A(kc,i)=C
              ii=kc+1
              L=kc+MBand-1
              Do 211 j=ii,L
                jj=j+MBand-kc
                A(j,i)=A(j,i)+C*A(jj,k)
211         Continue
212       Continue
          Do  214 i=2,NumNP
            jj=MBand+1-i
            ii=1
            If(jj.le.0) then
              jj=1
              ii=i-MBand+1
            Endif
            Sum=0.
            Do 213 j=jj,MBand-1
              Sum=Sum+A(j,i)*B(ii)
              ii=ii+1
213         Continue
            B(i)=B(i)+Sum
214       Continue              
          B(NumNP)=B(NumNP)/A(MBand,NumNP)
          Do 216 k=1,N1
            i=NumNP-k
            jj=i
            m=min0(2*MBand-1,MBand+k)
            Sum=0.
            Do 215 j=MBand+1,m
              jj=jj+1
              Sum=Sum+A(j,i)*B(jj)
215         Continue
            B(i)=(B(i)-Sum)/A(MBand,i)
216       Continue  
        Endif
C
        Do i=1,NumNP
          if(lOrt) B(i) = B1(i)
          if (B(i).lt.0) then
                 B(i)=0.0
                 iii=1
            endif
CDT 10/10/11 using the max statement here was causing root system to lose mass. I changed it back.           
          RMassY(i)=sngl(B(i))!new roots in each cell
        Enddo	 
*

     
c calculate mature root mass, total root mass and rool lengths
c set up for next step     
!      Do i=1,NumNPD 
!        RMean=(RMassY(i)+RMassOld(i))/2.0d0
!        RMassM(i)=RMassM(i)+ALPY*RMassY(i)*Step  !Alpy:Potential relative young root elongation rate (/day): APLY[1/day]*Rmassy[g/cm2]*step[day]= [g/cm2]
!        RMassY(i)=RMassY(i)*(1.0-ALPY*Step)  !(1-alpy) of young roots as new young roots
!        RDenM(i)=RMassM(i)/RTWL   ! new cm of roots /cm2 of area
!        RDenY(i)=RMassY(i)/RTWL
!        RDenTotal(i)=RDenM(i)+RDenY(i)
!        
!        RMassOld(i)=RMassY(i) !for the next time step assign the new roots now as the old root
!        
!c        VxROld(i)=VxR(i)
!c        VzROld(i)=VzR(i)
!      Enddo
     
      
       RootAge=floor(time)-int(sowingDay)+1                              !day from emergence 
        Do i=1,NumNPD
	        RMean=(RMassY(i)+RMassOld(i))/2.0d0
              RMassYc(i)=RMassY(i)                                    !RMassYc: Mass of young root based on the class
	        if (RootAge.ge.5)then                                   !moving from young roots to class 2 roots and adjusting young roots
		        RMassM(i)=RMassM(i)+ALPY*RMassY(i)*Step             !Alpy:Potential relative young root elongation rate (/day), this is rtp1 in the rhizos model: Partitioning coefficient for moving root material from age class 1 to age class 2
                                                                      !RmassM[g/cm2]=APLY[1/day]*Rmassy[g/cm2]*step[day]
		        RootClass(i,2)=RMassM(i)
		        RMassY(i)=RMassY(i)*(1.0-ALPY*Step)                 !(1-alpy) of young roots as new young roots
		        RootClass(i,1)=RMassY(i)
		        if (RootAge.gt.15)then                              !moving from class 2 to class 3 and adjusting class 2
			        RootClass(i,3)=RootClass(i,3)+0.1* RMassM(i)*Step   !RTP2=0.1 in Rhizos 0.1 is Partitioning coefficient for moving root material from age class 2 to age class 3
			        RMassM(i)=RMassM(i)*(1-0.1*Step)
			        RootClass(i,2)=RMassM(i)
		        end if
              end if
              
              !RmassY and RmassM have been modified to move to three classes
              !RmassYc and RmassMc [g/cm2]are the mass of young and matured roots based  on classes
	        
              RMassYc(i)=RootClass(i,1)+RootClass(i,2)
              RMassMc(i)=RootClass(i,3)
              
              RDenM(i)=RMassMc(i)/RTWL                                !new cm of roots /cm2 of area
	        RDenY(i)=RMassYc(i)/RTWL                                !young roots in rhizos that grow are roots in class 1 and class 2
	        RDenTotal(i)=RDenM(i)+RDenY(i)
	        RMassOld(i)=RMassYc(i)                                  !for the next time step assign the new roots now as the old root

c        VxROld(i)=VxR(i)
c        VzROld(i)=VzR(i)
        Enddo
      
      YRW=0.0
      MRW=0.0
      class1=0
      class1=0
      class2=0
      class3=0
      do n=1,NumNP
          YRW=YRW+RMassYc(n)*nodeArea(n)                              !Total mass of young roots in g/ slab
          MRW=MRW+RMassMc(n)*nodeArea(n)                              !Total mass of old roots
          class1=class1+RootClass(n,1)*nodeArea(n)
          class2=class2+RootClass(n,2)*nodeArea(n)
          class3=class3+RootClass(n,3)*nodeArea(n)
      enddo
      
      
       
      
      TotalRootWeight=0.0D0
      do n=1,NumNP
          TotalRootWeight=TotalRootWeight
     &      +nodeArea(n)*(RMassMc(n)+RMassYc(n))
      enddo
*
*        write(109,1973)Time, TotalRootWeight/popslab, class1/popslab, 
*     !    class2/popslab,class3/popslab
 
1973  FORMAT(f9.2,4X,4(4x,f9.2) )
      Return
10    Call errmes(im,il)
      Return
      End
* 

      Subroutine RDisper(NumNP,Dispxx,Dispzz,Dispxz,DMolx,DMolz,
     !   hN,Tm,RD)
C      Real xDist, yDist, Dist
CDT 11/2018 we probably don't need this subroutine. All control of the root movement will be 
C  done via carbon allocation, we won't be changing the disp coefficiencts based on soil props.
C basically, all this does now is assign values Dispxx and Dispyy
C we can keep it for now if we do want to adjust the values later in the future.
C I have cleaned it up though.
      Dimension 
     !  Dispxx(NumNP),Dispzz(NumNP),Dispxz(NumNP),
     !  hN(NumNP),Tm(NumNP),RD(NumNP)

      Do 11 i=1,NumNP
          Adjust=1.0
c          Alp=RD(i)-3.5+3.5*exp(-RD(i)/3.6)
          Alp=1.0D0
          Dispxx(i)=DMolx*Adjust*Alp*min(f1(hN(i)),f2(Tm(i)))
          Dispzz(i)=DMolz*Adjust*Alp*min(f1(hN(i)),f2(Tm(i)))
          Dispxz(i)=0.
11    continue
      Return
      End
*
!      subroutine RWeFact(NumNP,NumEl,NumElD,x,y,KX,WeTab,VxH,VzH,Dispxx,
!     !                  Dispzz,Dispxz)
!      Dimension x(NumNP),y(NumNP),KX(NumElD,4),VxH(NumNP),VzH(NumNP),
!     !          Dispxx(NumNP),Dispzz(NumNP),Dispxz(NumNP),
!     !          WeTab(3,2*NumElD),Beta(3),List(3)
!      Integer e
!
!C AD 10/12/11 commented out the following line, because it causes Vel (geotrophic velocity) to make the denominator very small
!C      TanH(z)=(exp(z)-exp(-z))/(exp(z)+exp(-z))
!      NumSEl=0
!      Do 13 e=1,NumEl
!        NCorn=4
!        If(KX(e,3).eq.KX(e,4)) NCorn=3
!        Do 12 n=1,NCorn-2
!          NumSEl=NumSEl+1
!          M1=KX(e,1)
!          M2=KX(e,n+1)
!          M3=KX(e,n+2)
!          A=y(M2)-y(M1)
!          B=x(M2)-x(M1)
!          Beta(1)=atan2(A,B)
!          A=y(M3)-y(M2)
!          B=x(M3)-x(M2)
!          Beta(2)=atan2(A,B)
!          A=y(M1)-y(M3)
!          B=x(M1)-x(M3)
!          Beta(3)=atan2(A,B)
!          List(1)=M1
!          List(2)=M2
!          List(3)=M3
!          Do 11 j=1,3
!            k=j-1
!            If(k.eq.0) k=3
!            WeTab(k,NumSEl)=0.
!            M1=List(j)
!            jp1=j+1
!            If(j.eq.3) jp1=1
!            M2=List(jp1)
!            Vxx=(VxH(M1)+VxH(M2))/2.
!            Vzz=(VzH(M1)+VzH(M2))/2.
!            If(abs(Vxx).lt.1.e-30.and.abs(Vzz).lt.1.e-30) goto 11
!            BetaV=atan2(Vzz,Vxx)
!            Delta=abs(BetaV-Beta(j))
!            If(Delta.gt.0.314.and.abs(Delta-3.1416).gt.0.314) goto 11
!            ALeng=sqrt((x(M2)-x(M1))**2+(y(M2)-y(M1))**2)
!            CBeta=cos(Beta(j))
!            SBeta=sin(Beta(j))
!            Val=Vxx*CBeta+Vzz*SBeta      ! velocity along the edge
!            VV=sqrt(Vxx*Vxx+Vzz*Vzz)
!            DLL=(Dispxx(M1)+Dispxx(M2))/2.
!            DLT=(Dispxz(M1)+Dispxz(M2))/2.
!            DTT=(Dispzz(M1)+Dispzz(M2))/2.
!            DAL=abs(DLL*CBeta*CBeta+2.0*CBeta*SBeta*DLT+DTT*SBeta*SBeta)
!            Vel=VAL*ALeng
!            Disp=2.0*DAL
!            aa=11.
!            If(Disp.gt.0.) aa=abs(Vel/Disp)
!            If(Disp.lt.1.e-30.or.abs(Vel).lt.0.001*VV.or.abs(aa).gt.10.)
!     !            then
!              If(abs(Vel).lt.0.001*VV) WeTab(k,NumSEl)=0.0
!              If(Vel.gt.0.001*VV) WeTab(k,NumSEl)=1.0
!              If(Vel.lt.-0.001*VV) WeTab(k,NumSEl)=-1.0
!            Else
!              WeTab(k,NumSEl)=1.0/TanH(Vel/Disp)-Disp/Vel
!            Endif
!11        Continue
!12      Continue
!13    Continue
!      Return
!      End

