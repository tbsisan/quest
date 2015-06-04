function [ cFKfiles ]    = getcFKfiles( cFKfullFile );
cFKfiles.x      = cFKfullFile;
cFKfiles.log    = regexprep(cFKfullFile,'\/x\.','/log.');
cFKfiles.Ux     = regexprep(cFKfullFile,'\/x\.','/Ux.');
cFKfiles.vx     = regexprep(cFKfullFile,'\/x\.','/vx.');
