
runlist{10}='';
parfor i=1:10
    mystruct=returnStructWithStr(i);
    runlist{i}=mystruct.b;
end
