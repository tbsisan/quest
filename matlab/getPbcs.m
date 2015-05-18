function [ pbcx, pbcy, pbcz ] = getPbcs( xscf )
%UNTITLED5 Summary of this function goes here
%   Detailed explanation goes here

display(sprintf('\nGETTING PBCS from file: %s\n',xscf));
pbcz=0; pbcx=0; pbcy=0;

[stat,str]=system(['awk "/^[0-9.]/ {printf \"%f\", \$10; exit}" <' xscf]);
pbcz=str2num(str);

if stat
    xscf=regexprep(xscf,'xsc$','restart.xsc');
    [stat,str]=system(['awk "/^[0-9.]/ {printf \"%f\", \$10; exit}" <' xscf]);
    pbcz=str2num(str);
end

if stat
    reply = input('what is pbcz?: ', 's');
    pbcz=str2num(reply);
else
    [stat,str]=system(['awk "/^[0-9.]/ {printf \"%f\", \$2; exit}" <' xscf]);
    pbcx=str2num(str);
    [stat,str]=system(['awk "/^[0-9.]/ {printf \"%f\", \$6; exit}" <' xscf]);
    pbcy=str2num(str);
end
pbca=[pbcx, pbcy, pbcz];
display(sprintf('\t (%f, %f, %f)',pbcx,pbcy,pbcz));
if any(pbca==0)
   display(sprintf('\n\t WARNING ONE OR MORE DIMENSIONS IS ZERO'));
   pause(2.5);
   [~,xscleaf,~]=fileparts(xscf);
   warndlg(sprintf('ONE OR MORE DIMENSIONS WAS ZERO\n\n %s \n\nCONTINUING',xscleaf));
end

end

