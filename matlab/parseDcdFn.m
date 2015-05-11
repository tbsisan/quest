function [ dcdFnData ] = parseDcdFn( dcdf )
% Parsing the data from the name of the dcd file
% TODO: still a workin in progress

display(sprintf('\nGRABBING DATA FROM DCD FILENAME\n'));
display(sprintf('\tCalled with %s',dcdf));
Fcell=regexp(dcdf,'_g([-0-9.]*)','tokens');
if (length(Fcell)>1)
    FpnCell=Fcell{1};
    FpnStr=FpnCell{1}
elseif (length(Fcell)>0)
    FpnStr=Fcell{1}{1};
else
    FpnStr='0';
end
if (isempty(FpnStr))
    FpnStr='0';
end
dcdFnData.FpnStr=FpnStr;
dcdFnData.Fpn=str2num(FpnStr);

ensCell=regexp(dcdf,'_ens([-0-9]*)','tokens');
if (length(ensCell)>1)
    ensStr=ensCell{1};
elseif (length(ensCell)>0)
    ensStr=ensCell{1}{1};
else
    ensStr='0';
end
dcdFnData.ensStr=ensStr;
dcdFnData.ens=str2num(ensStr);

fluidModelCell=regexp(dcdf,'_nm[0-9]{2,4}_([a-z0-9]*[a-z])[0-9]+_','tokens');
dcdFnData.fluidModelStr=fluidModelCell{1}{1};

thermostatCell=regexp(dcdf,'([a-z]*)([.0-9_]*)([a-z]*).dcd','tokens');
thermostatname=thermostatCell{1}{1};
thermostatStr=[thermostatname(1:3) thermostatCell{1}{2}];
etaStr=thermostatCell{1}{2};
dcdFnData.thermostatStr=thermostatStr;
dcdFnData.etaStr=etaStr;
dcdFnData.eta=str2num(etaStr);

runid='';
runid=regexp(dcdf,'run([a-zA-Z0-9.]*)','tokens');
if (size(runid,1)>0)
    runid=runid{1}{1};
end
dcdFnData.runidStr=runid;

runnumberStr='';
runnumberCell=regexp(dcdf,'run[a-z]*([0-9.]*)','tokens');
if (size(runnumberCell,1)>0)   
    runnumberStr=runnumberCell{1}{1};
end
dcdFnData.runnumberStr=runnumberStr;
dcdFnData.runnumber=str2num(runnumberStr);

%Ccell=regexp(dcdf,'run([0-9.]*)','tokens');
%whateverThisIs=str2num(Ccell{1}{1});

Xicell{1}{1}='0';
Xicell=regexp(dcdf,'[0-9]p([0-9]*)','tokens');
if (length(Xicell)==0)
Xicell=regexp(dcdf,'spe([0-9]*)','tokens');
end
if (length(Xicell)==0)
Xicell=regexp(dcdf,'lj([0-9]*)','tokens');
end
fluidQuantityStr=Xicell{1}{1};
waterCell=regexp(dcdf,'_(cgp[0-9]g|lj|spe|h2o|tbs[0-9]p|ts26[ab]|tip3[a-z]{0,}|tip42005p{0,})([0-9]*)','tokens');
if (length(waterCell)>1)
    fluidQuantityStr=waterCell{1}{2};
elseif (length(waterCell)>0)
    disp('length waterCell is 1 (or more, but can"t be more) and this is unexpected');
    fluidQuantityStr=waterCell{1};
else
    fluidQuantityStr='0';
end
%fluidQuantityStr
%fluidQuantityStr{1}

% Assuming that rigid carbons are unbonded and fluid is water
if (fluidQuantityStr=='0')
    [stat,str]=system(['sed -n "/[0-9] RIGID BONDS/"p <' outp]);
    if (~stat)
    watstr=regexp(str,'[0-9]+','match')
    h2os=str2num(watstr{1})/3;
    fluidQuantityStr=num2str(h2os);
    end
end
dcdFnData.fluidQuantityStr=fluidQuantityStr;
dcdFnData.fluidQuantity=str2num(fluidQuantityStr);

% o1=cnts*4*str2num(nm(1:end/2))+1;
% oe=o1+h2os*ospace-1;

%cntLstr=regexp(dcdf,'^.nt([0-9.]*)','tokens');
cnts={'0'};
[tokenstring]=regexp(dcdf,'^[bcs]nt([0-9.]*)','tokens');
cnts=(tokenstring{1});
[tokenstringn]=regexp(dcdf,'^[bcs]nt([0-9.]*)(n)','tokens');
if (~isempty(tokenstringn))
if (length(tokenstringn{1})==2)
    cnts={'0'};
end
end
cntLStr=(cell2mat(cnts));
dcdFnData.cntLStr=cntLStr;
dcdFnData.cntL=str2num(cntLStr);

nmCells=regexp(dcdf,'_nm([0-9]*)','tokens');
nmCell=(nmCells{1});
nmStr=cell2mat(nmCell);
dcdFnData.nmStr=nmStr;
dcdFnData.nm=str2num(nmStr);
dcdFnData.nm1=str2num(nmStr(1));

[tempCell]=regexp(dcdf,'_T([0-9.]*)','tokens');
if (~isempty(tempCell))
    temp=(tempCell{1});
    tempStr=cell2mat(temp);
else
    tempStr='298';
end
dcdFnData.tempStr=tempStr;
dcdFnData.temp=str2num(tempStr);

poreR='';
poreRc=regexp(dcdf,'r([0-9.])p','tokens');
if (size(poreRc,1)>0)
    poreR=poreRc{1}{1};
    dcdFnData.poreRStr=poreR;
    dcdFnData.poreR=str2num(poreR);
end

myFields = fieldnames(dcdFnData);
emptyFieldInds=structfun(@isempty,dcdFnData);
emptyFields=myFields(emptyFieldInds);
nonEmptyFieldNum=length(myFields)-sum(emptyFieldInds);
display(sprintf('\t %d fields retrieved from dcdf',nonEmptyFieldNum));
display(sprintf('\t Empty fields are:'));
display( sprintf('\t    { %s }',strjoin(emptyFields')) );

