function [ fort, ts, sol, solo, fullFile ] = readFortran( dataFile, lambda )
% Read in Fortran binary data written with unformatted output type.
% Column 1 contains the time step, the rest of columns correspond to each particle.  

[ projDir,~,~ ] = fileparts(dataFile);
fileInfo = dir(dataFile);
fullFile = [projDir '/' fileInfo(1).name];
disp(sprintf('about to open %s',fullFile));
fid = fopen(fullFile, 'rb');

% In fortran unformatted, "rows" of data are referred to as records
% Records starts with an int32 with length of the records, 
% and the records end with an int32 of the same value
rowLength=fread(fid, 1, 'int32'); % length of row in bytes, the filepointer is now after the first int32
timeSteps=fileInfo(1).bytes/(rowLength+8); % the bytes for a row is rowLength+2*size(int32)

%frewind(fid); %set file pointer back to start of file
numbersPerRow=round(rowLength/4)+2;
fort=zeros(numbersPerRow,timeSteps);

% Read from number 2 on current record up to and including the first number on the next record
fort=fread(fid,[rowLength/4+2,timeSteps],'float32'); 
fclose(fid);

ts=fort(1,:);
% NOTE: since we started off reading one int32, the next read puts the
% first data of the next record at the end of the read, so that my fort
% variable ends with two int32 record length indicators
fort=fort(1:end-2,:); % Throw out the 2 int32's enclosing each record
fort=fort(2:end,:); %strip off time data
fort=fort'; % return so each row is a time step

solSteps = round(linspace(1,size(fort,1),100));
solo=mod(fort(solSteps,:),lambda);
sol=unbreakSol(solo,lambda);
end

