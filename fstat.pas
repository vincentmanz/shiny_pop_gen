
procedure sumlinkg(namedat:string);
type
loc2ptr = array of array of real;

var
obsg       :loc2ptr;
obsgp       :array of loc2ptr;
il1,il2,ip,iu,iv,nall_loc,i  :word;
ninpop      :ptr_to_popptr;
presall     :array of boolean;
id_all      :array of word;
trans,nbgenot       :array of array of word;
pinpop      :array of array of array of word;
presgenot,nbcells   :array of array of array of boolean;
nbperms     :word;
countip     :array[1..npmax] of word;
maxip,irep,dum,maxall       :word;
tempg,tempgt     :real;
dummyfile,df,dfp,dumfile        :text;
   ProgressBar1: TProgressBar;
pass1:boolean;
sdg,sdgp,dgp,dg            :real;
ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8,ss9        :string;
nbdt,accnbdt                                      :word;
(**********************************)
procedure linkg(var g,dg,dgp:real;var ts:word; ip,il1,il2:word);
type
tables= array of array of word;
var
i,iu,ng1,ng2,ig1,ig2,al11,al12,al21,al22  :word;
table           :tables;
d               :array of array of real;
tempcells       :word;
nbgam,nbgam1,nbgam2           :word;
thisd,w,accdp,accd,f1,f2,c1,c2,ct,rij,dpij,deltaij    :real;
begin
     ng1:=kall[il1]*(kall[il1]+1);
     ng2:=kall[il2]*(kall[il2]+1);
     setlength(table,ng1+1,ng2+1);

     g:=0.0;
     for ig1:=1 to ng1 do
     for ig2:=1 to ng2 do
         table[ig1][ig2]:=0;

     i:=0;
     while (i<maxip) do
     begin
          i:=i+1;
          table[pinpop[il1][ip][i]][pinpop[il2][ip][i]]:=table[pinpop[il1][ip][i]][pinpop[il2][ip][i]]+1;
     end;

     if pass1 then
     begin
          for ig1:=1 to ng1 do table[ig1][0]:=0;
          for ig2:=1 to ng2 do table[0][ig2]:=0;

          for ig1:=1 to ng1 do
          for ig2:=1 to ng2 do
               table[ig1][0]:=table[ig1][0]+table[ig1][ig2];
          for ig2:=1 to ng2 do
          for ig1:=1 to ng1 do
              table[0][ig2]:=table[0][ig2]+table[ig1][ig2];

          nbgam1:=0;
          for ig1:=1 to ng1 do nbgam1:=nbgam1+table[ig1][0]*2;
          nbgam2:=0;
          for ig2:=1 to ng2 do nbgam2:=nbgam2+table[0][ig2]*2;
          if (nbgam1<>nbgam2) then messagedlg('Counts of gametes for 1st and second locus different!',mtError,[mbOK],0);
          nbgam:=nbgam1;
          tempcells:=0;
          for ig1:=1 to  ng1 do
          for ig2:=1 to ng2 do
           if table[ig1][ig2]>0 then   tempcells:=tempcells+1;
          if tempcells>1 then nbcells[ip][il1][il2]:=true;
          tempcells:=0;
          for ig1:=1 to ng1 do
           if table[ig1][0]>0 then tempcells:=tempcells+1;
          if tempcells<=1 then nbcells[ip][il1][il2]:=False;

          tempcells:=0;
          for ig2:=1 to ng2 do
           if table[0][ig2]>0 then tempcells:=tempcells+1;
          if tempcells<=1 then nbcells[ip][il1][il2]:=False;

          if nbcells[ip][il1][il2] then
          if ffstat.CB_savetables.Checked then
          begin
               writeln(dummyfile,'Sample ',popname[ip],' for ',locname[il1],' X ',locname[il2]);
               writeln(dummyfile);
               write(dummyfile,cht);
               for al21:=1 to kall[il2] do
               for al22:=al21 to kall[il2] do
               begin
                    ig2:=al21*kall[il2]+al22;
                    if (presgenot[ip][il2][ig2])
                    then
                    begin
                         if trans[il2][al21]<trans[il2][al22]
                         then write(dummyfile,cht,trans[il2][al21],trans[il2][al22])
                         else write(dummyfile,cht,trans[il2][al22],trans[il2][al21]);
                    end;
               end;
               writeln(dummyfile);
               for al11:=1 to kall[il1] do
               for al12:=al11 to kall[il1] do
               begin
                    ig1:=al11*kall[il1]+al12;
                    if (presgenot[ip][il1][ig1]) then
                    begin
                         if trans[il1][al11]<trans[il1][al12]
                         then write(dummyfile,cht,trans[il1][al11],trans[il1,al12])
                         else write(dummyfile,cht,trans[il1][al12],trans[il1,al11]);
                         for al21:=1 to kall[il2] do
                         for al22:=al21 to kall[il2] do
                         begin
                              ig2:=al21*kall[il2]+al22;
                              if (presgenot[ip][il2][ig2])
                              then write(dummyfile,cht,table[ig1][ig2]:4);
                         end;
                         if presgenot[ip][il1][ig1] then writeln(dummyfile);
                    end;
               end;
               writeln(dummyfile);
          end;
        accdp:=0.0;accd:=0.0;
        setlength(d,kall[il1]+1,kall[il2]+1);

        if nbcells[ip][il1][il2] then
        for al11:=1 to kall[il1] do d[al11][0]:=0;
        for al11:=1 to kall[il1] do
        begin
             for al12:=al11 to kall[il1] do
             if (al12=al11)
             then d[al11][0]:=d[al11][0]+2*table[al11*kall[il1]+al12][0]
             else
             begin
                  d[al11][0]:=d[al11][0]+table[al11*kall[il1]+al12][0];
                  d[al12][0]:=d[al12][0]+table[al11*kall[il1]+al12][0];
             end;
        end;

        if nbcells[ip][il1][il2] then
        for al21:=1 to kall[il2] do d[0][al21]:=0;
        for al21:=1 to kall[il2] do
        begin
             for al22:=al21 to kall[il2] do
             if (al22=al21)
             then d[0][al21]:=d[0][al21]+2*table[0][al21*kall[il2]+al22]
             else
             begin
                  d[0][al21]:=d[0][al21]+table[0][al21*kall[il2]+al22];
                  d[0][al22]:=d[0][al22]+table[0][al21*kall[il2]+al22];
             end;
        end;

        if not(nbcells[ip][il1][il2])
        then
        begin
             dg:=-9.99;
             dgp:=-9.99;
             exit;
        end
        else
        for al11:=1 to kall[il1] do
        if (d[al11][0]>0) then
        begin
             for al21:=1 to kall[il2] do
             if (d[0][al21]>0)then
             begin
                  d[al11][al21]:=0;
                  for al12:=1 to kall[il1] do
                  if (d[al12][0]>0) then
                  begin
                       ig1:=min(al11,al12)*kall[il1]+max(al11,al12);
                       if presgenot[ip][il1][ig1] then
                       for al22:=1 to kall[il2] do
                       if (d[0][al22]>0) then
                       begin
                            ig2:=min(al21,al22)*kall[il2]+max(al21,al22);
                            if presgenot[ip][il2][ig2] then
                            begin
                                 if (al12=al11) then
                                 begin
                                      if (al22=al21) then
                                      begin
                                           d[al11][al21]:=d[al11][al21]+2*table[ig1][ig2];
                                      end
                                      else
                                      begin
                                           d[al11][al21]:=d[al11][al21]+table[ig1][ig2];
                                      end;
                                 end
                                 else
                                 begin
                                      if (al22=al21) then
                                      begin
                                           d[al11][al21]:=d[al11][al21]+table[ig1][ig2];
                                      end
                                      else
                                      begin
                                           d[al11][al21]:=d[al11][al21]+table[ig1][ig2]/2;
                                      end;
                                 end;
                            end;
                       end;
                  end;
                  f1:=d[al11][0]/nbgam;
                  f2:=d[0][al21]/nbgam;
                  deltaij:=d[al11][al21]*2/nbgam-2*f1*f2;           //deltaij, see weir 1996 p126 delta_AB
       //c1, c2 & ct to allow for non random mating, see weir 1996 pp 137
                  c1:=table[al11*kall[il1]+al11][0]*2/nbgam;
                  c1:=f1*(1-f1)+c1-f1*f1;        //p_A(1-p_A)+D_A (D_A=homAA-p_A^2, weir p95, 3.1)
                  c2:=table[0][al21*kall[il2]+al21]*2/nbgam;
                  c2:=f2*(1-f2)+c2-f2*f2;
                  ct:=c1*c2;
                  if deltaij<0
                  then w:=min(f1*f2,(1-f1)*(1-f2))
                  else w:=min(f1*(1-f2),(1-f1)*f2);
                  if (abs(ct)>zermach) then
                  begin

                       rij:=deltaij/sqrt(ct);
                       accd:=accd+rij*rij*f1*f2; // same weighting as D'
                  end;
{
                  else
                  begin
                       str(ip,ss1);
                       str(il1,ss2);
                       str(il2,ss3);
                       str(trans[il1][al11],ss4);
                       str(trans[il2][al21],ss5);
                       str(ct:10:6,ss6);
                       messagedlg('sample: '+ss1+#10#13+'loc1: '+ss2+'; allele: '+ss4+#10#13+'loc2: '+ss3+'; allele: '+ss5+#10#13+'CT: '+ss6,mtError,[mbOK],0);

                  end;
}
                  if (w>zermach) then
                  begin
                       dpij:=deltaij/2/w;
                       accdp:=accdp+abs(dpij)*f1*f2;    //following e.g Zapata et al, Ann Hum Genet (2001) 65, 395-406
                  end;


                  if ffstat.CB_details.Checked
                  then writeln(dumfile,popname[ip],cht,locname[il1],'(',trans[il1][al11],')X',
                               locname[il2],'(',trans[il2][al21],')',cht,nbgam div 2:8,cht,
                               f1:8:5,cht,f2:8:5,cht,c1:8:5,cht,c2:8:5,cht,
                               d[al11][al21]:8:5,cht,deltaij*(nbgam/2/(nbgam/2-1)):8:5,cht,
                               rij:8:5,cht,dpij:8:5)

             end;
        end;
        dg:=accd;
        dgp:=accdp;
        finalize(d);
        ts:=nbgam div 2;
     end;
     for al11:=1 to kall[il1] do
     for al12:=al11 to kall[il1] do
     for al21:=1 to kall[il2] do
     for al22:=al21 to kall[il2] do
     begin
          ig1:=al11*kall[il1]+al12;
          ig2:=al21*kall[il2]+al22;
          if ((presgenot[ip][il1][ig1]) and (presgenot[ip][il2][ig2])) then
          if (table[ig1][ig2]>1)
          then  g:=g+table[ig1][ig2]*ln(table[ig1][ig2]);
     end;

     //the only thing that changes in G...

     finalize(table);
end;
(********************************************)
begin
     maxall:=0;
     for il1:=1 to nl do if kall[il1]>maxall then maxall:=kall[il1];
     setlength(trans,nl+1,maxall+1);
     for il1:=0 to nl do new(ninpop[il1]);
     for il1:=1 to nl do
     begin
          setlength(presall,nu+1);
          setlength(id_all,nu+1);
          for iu:=1 to nu do
          begin
               presall[iu]:=False;
               id_all[iu]:=0;
          end;
          nall_loc:=0;
          for i:=1 to toti do
          begin
               iu:=inpop[il1]^[i] div modulo;
               iv:=inpop[il1]^[i] mod modulo;
               if ((iu<>0) and (iv<>0)) then
               begin
                    if not presall[iu] then
                    begin
                         nall_loc:=nall_loc+1;
                         trans[il1][nall_loc]:=iu;
                         id_all[iu]:=nall_loc;
                         presall[iu]:=True;
                    end;
                    if not presall[iv] then
                    begin
                         nall_loc:=nall_loc+1;
                         trans[il1][nall_loc]:=iv;
                         id_all[iv]:=nall_loc;
                         presall[iv]:=True;
                    end;
                    ninpop[il1]^[i]:=min(id_all[iu],id_all[iv])*kall[il1]+max(id_all[iu],id_all[iv]);
               end else ninpop[il1]^[i]:=0;
               if il1=1 then ninpop[0]^[i]:=inpop[0]^[i];
          end;
          finalize(presall);
          finalize(id_all);
     end;

     //each sample stored in its own array

     maxip:=n[1];
     for ip:=2 to np do if (n[ip]-n[ip-1])>maxip then maxip:=(n[ip]-n[ip-1]);
     setlength(pinpop,nl+1,np+1,maxip+1);
     setlength(presgenot,np+1,nl+1);
     setlength(nbcells,np+1,nl+1,nl+1);
     for ip:=1 to np do
     for il1:=1 to nl do
     begin
          setlength(presgenot[ip][il1],kall[il1]*(kall[il1]+1)+1);
          for il2:=1 to nl do nbcells[ip][il1][il2]:=False;
     end;
     for ip:=1 to np do
     begin
          countip[ip]:=0;
          for il1:=1 to nl do
          for i:=1 to (kall[il1]*(kall[il1]+1)) do
           presgenot[ip][il1][i]:=False;
     end;
     setlength(nbgenot,np+1,nl+1);
     for ip:=1 to np do
     for il1:=1 to nl do nbgenot[ip][il1]:=0;
     for i:=1 to toti do
     begin
          ip:=ninpop[0]^[i];
          countip[ip]:=countip[ip]+1;
          for il1:=1 to nl do
          begin
               pinpop[il1][ip][countip[ip]]:=ninpop[il1]^[i];
               if ninpop[il1]^[i]<>0 then
               if not presgenot[ip][il1][ninpop[il1]^[i]]
               then
               begin
                    presgenot[ip][il1][ninpop[il1]^[i]]:=True;
                    nbgenot[ip][il1]:=nbgenot[ip][il1]+1;
               end;
          end;
     end;
     for il1:=1 to nl do dispose(ninpop[il1]);


     //estimate G on observed data set
     pass1:=true;
     if ffstat.CB_Savetables.checked then
     begin
          assignfile(dummyfile,namedat+'-tables.ld');
          rewrite(dummyfile);
     end;
     if ffstat.CB_dp.Checked then
     begin
          assignfile(dfp,namedat+'-dp.cd');
          rewrite(dfp);
     end;
     if ffstat.CB_r.Checked then
     begin
          assignfile(df,namedat+'-r2.cd');
          rewrite(df);
     end;
     if ffstat.CB_details.checked then
     begin
          assignfile(dumfile,namedat+'-details.cd');
          rewrite(dumfile);
          writeln(dumfile,'   pop',cht,'  loc1(al)X  loc2(al)',cht,'       N',cht,
                          '      pA',cht,'      pB',cht,'      cA',cht,'      cB',cht,
                          '     nAB',cht,'   Delta',cht,'     RAB',cht,'  Delta''');
     end;


     setlength(obsg,nl+1,nl+1);
     setlength(obsgp,np+1,nl+1,nl+1);
     for il1:=1 to nl-1 do
     if kall[il1]>1 then
     for il2:=il1+1 to nl do
     if kall[il2]>1 then
     begin
          obsg[il1][il2]:=0.0;
          obsg[il2][il1]:=0.0;
          sdg:=0.0;sdgp:=0.0;
          if ffstat.CB_dp.Checked then write(dfp,locname[il1]+' X '+locname[il2],cht);
          if ffstat.CB_r.Checked then write(df,locname[il1]+' X '+locname[il2],cht);
          accnbdt:=0;
          for ip:=1 to np do
          begin
               if ((nbgenot[ip][il1]>1) and (nbgenot[ip][il2]>1)) then
               begin
                    linkg(obsgp[ip][il1][il2],dg,dgp,nbdt,ip,il1,il2);
                    obsgp[ip][il2][il1]:=0.0;
                    if nbcells[ip][il1][il2] then
                    begin
                         obsg[il1][il2]:=obsg[il1][il2]+obsgp[ip][il1][il2];
                         if ffstat.CB_dp.Checked then write(dfp,cht,dgp:8:5);
                         if ffstat.CB_r.Checked then write(df,cht,dg:8:5);
                         sdgp:=sdgp+dgp*nbdt;
                         sdg:=sdg+dg*nbdt;
                         accnbdt:=accnbdt+nbdt;
                    end
                    else
                    begin
                         obsgp[ip][il1][il2]:=0.0;
                         if ffstat.CB_dp.Checked then write(dfp,cht,'      NA');
                         if ffstat.CB_r.Checked then write(df,cht,'      NA');
                    end;
               end
               else
               begin
                    if ffstat.CB_dp.Checked then write(dfp,cht,'      NA');
                    if ffstat.CB_r.Checked then write(df,cht,'      NA');
               end;
          end;

          if ffstat.CB_dp.Checked
          then if abs(accnbdt)>0
               then writeln(dfp,cht,sdgp/accnbdt:8:5)
               else writeln(dfp,cht,'      NA');

          if ffstat.CB_r.Checked
          then if abs(accnbdt)>0
               then writeln(df,cht,sdg/accnbdt:8:5)
               else writeln(df,cht,'      NA');
     end;
    if ffstat.CB_dp.Checked then closefile(dfp);
    if ffstat.CB_r.Checked then closefile(df);

    if ffstat.CB_details.Checked then closefile(dumfile);

     if ffstat.CB_Savetables.checked then closefile(dummyfile);

     //   now the shuffling of loci within pops. Tricky!
     pass1:=False;

     nbperms:=nbperm[8];

  If nbperms>0 then
  begin
  ProgressBar1 := TProgressBar.Create(fFstat);
  try
    ProgressBar1.Parent := fFstat;
    ProgressBar1.Align := AlTop;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := Nbperms;
    ProgressBar1.Step := 1; // the amount to move with the StepIt method
     for irep:=1 to (nbperms-1) do
     begin
      ProgressBar1.Stepit; // Move one Step amount
      application.processmessages;

          for il1:=1 to nl-1 do
          if kall[il1]>1 then
          for il2:=il1+1 to nl do
          if kall[il2]>1 then
          begin
               tempgt:=0.0;
               for ip:=1 to np do
               begin
                    if (nbcells[ip][il1][il2] and ((nbgenot[ip][il1]>1) and (nbgenot[ip][il2]>1)))
                    then
                    begin
                         i:=maxip;
                         repeat
                               if ((pinpop[il1][ip][i]>0) and (pinpop[il2][ip][i]>0)) then
                               begin
                                    dum:=grandom(i)+1;
                                    if ((pinpop[il1][ip][dum]=0) or (pinpop[il2][ip][dum]=0)) then
                                    begin
                                         repeat
                                               dum:=dum-1;
                                         until (((pinpop[il1][ip][dum]>0) and (pinpop[il2][ip][dum]>0)) or (dum=0));
                                    end;
                                    pinpop[il2][ip][0]:=pinpop[il2][ip][dum];
                                    pinpop[il2][ip][dum]:=pinpop[il2][ip][i];
                                    pinpop[il2][ip][i]:=pinpop[il2][ip][0];
                               end;
                               i:=i-1;
                         until (i=1);
                         linkg(tempg,dg,dgp,nbdt,ip,il1,il2);
                         if tempg>=obsgp[ip][il1][il2]
                         then obsgp[ip][il2][il1]:=obsgp[ip][il2][il1]+1;
                         tempgt:=tempgt+tempg;
                    end
                    else obsgp[ip][il2][il1]:=-1;
               end;
               if tempgt>=obsg[il1][il2] then obsg[il2][il1]:=obsg[il2][il1]+1;
//               write(df,cht,tempgt:8:2);
          end;
//          writeln(df);
     end;
     for il1:=1 to nl-1 do
     for il2:=il1+1 to nl do
     begin
          nbcells[0][il1][il2]:=False;
          for ip:=1 to np do
          begin
               if nbcells[ip][il1][il2]
               then
               begin
                    obsgp[ip][il2][il1]:=(obsgp[ip][il2][il1]+1.0)/nbperms;
                    nbcells[0][il1][il2]:=True;
               end;
          end;
          obsg[il2][il1]:=(obsg[il2][il1]+1.0)/nbperms
     end;
  finally
    ProgressBar1.Free;
//   write stuff to file


     append(fileout1);writeln(fileout1);
     writeln(fileout1,'*******************************************************');
     writeln(fileout1,cht,'P-value for genotypic disequilibrium');
     writeln(fileout1,cht,'based on ',nbperms:10,' permutations.');
     case ffstat.RG_MTG.itemindex of
          0: writeln(fileout1,cht,'Adjusted P-value for 5% nominal level is : ',1/nbperms:14:10);
          1:begin
             writeln(fileout1,cht,'Adjusted P-value for 5% nominal level is : ',5/nbperms:14:10);
             writeln(fileout1,cht,'Adjusted P-value for 1% nominal level is : ',1/nbperms:14:10);
            end;
          2:begin
             writeln(fileout1,cht,'Adjusted P-value for 5% nominal level is : ',50/nbperms:14:10);
             writeln(fileout1,cht,'Adjusted P-value for 1% nominal level is : ',10/nbperms:14:10);
             writeln(fileout1,cht,'Adjusted P-value for 0.1% nominal level is : ',1/nbperms:14:10);
            end;

     end;

     writeln(fileout1);
     write(fileout1,cht);

     if (ffstat.RG_GDT.itemindex=2)
     then for ip:=1 to np do write(fileout1,cht,popname[ip]:6);

     writeln(fileout1,cht,'   All');
     for il1:=1 to nl-1 do
     for il2:=il1+1 to nl do
     begin
          write(fileout1,locname[il1],' X ',locname[il2],cht);
          if ((kall[il1]=1) or (kall[il2]=1)) then nbcells[0][il1][il2]:=False;

          if (ffstat.RG_GDT.itemindex=2) then
          for ip:=1 to np do
             if nbcells[ip][il1][il2]
             then write(fileout1,cht,obsgp[ip][il2][il1]:8:5)
             else write(fileout1,cht,'      NA');

          if nbcells[0][il1][il2]
          then writeln(fileout1,cht,obsg[il2][il1]:8:5)
          else writeln(fileout1,cht,'      NA');
     end;

     writeln(fileout1);
     closefile(fileout1);
  end;      //if nbperms>0
     finalize(nbgenot);
     finalize(nbcells);
     finalize(obsgp);
     finalize(obsg);
     finalize(presgenot);
     finalize(pinpop);
     finalize(trans);
  end;
end;