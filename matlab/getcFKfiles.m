function [ cFKfiles ]    = getcFKfiles( cFKfullFile );
cFKfiles.x      = cFKfullFile;
cFKfiles.log    = regexprep(cFKfullFile,'\/x\.','/log.');
cFKfiles.log    = regexprep(cFKfiles.log,'\.dat','.ascii.dat');
cFKfiles.Ux     = regexprep(cFKfullFile,'\/x\.','/Ux.');
cFKfiles.vx     = regexprep(cFKfullFile,'\/x\.','/vx.');
