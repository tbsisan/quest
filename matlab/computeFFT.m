function [ data ] = computeFFT( settings, flags, sampleTimes, varargin )
% Compute the FFT for a group of data in varargin (see matNotes TODO)
% For example, to compute the fft of dataSet_1 as well as the fft of dataSet_2
% then you would call the function like:
% [dataResults] = computeFFT( settings, flags, sampleTs, dataSet1, dataSet2)
% but all data sets must have the same length and correspond to the time values in sampleTimes.
% dataResults would be an array of length 2 of structures with the FFT in the field dataResults.power
% The settings variable is a structure.
% The falgs variable is a cell array.
% Taken from my fftSpectrum.m in PMFscripts folder


numTimePoints=length(sampleTimes);
numDataArrays=length(varargin);
display(sprintf('\tCOMPUTING FFTs: number of data arrays passed to computeFFT: %i',numDataArrays));
% TODO: delete below comments.
% Initialize a struct array with dimension equal to number of input arrays.
% Changed to struct of structs, so no longer needed.
% timeArrayZeros=zeros(1,numTimePoints);
% data=struct('power',timeArrayZeros, ...
%             'smoothPower',timeArrayZeros, ...
%             'freqs',timeArrayZeros, ...
%             'dataNum',num2cell(1:numDataArrays)); % makes a 0-initialized structure array of size numDataArrays, with fields as listed

for datai=1:numDataArrays
    dataName = inputname(datai+3);
    display(sprintf('\t\tname of data: %s',dataName));
    ydata=squeeze(varargin{datai});
    numDataPoints=length(ydata);
    if (numDataPoints ~= numTimePoints)
        display(sprintf('\t\tERROR in computeFFT: sample time array (%i) has different length than data array (%i) for %s',numTimePoints,numDataPoints,dataName));
        continue;
    end
    signal = ydata - mean( ydata );
    sampleTime_ns=sampleTimes(2)-sampleTimes(1);
    sampleFreq=1/sampleTime_ns;
    NFFT = 2^nextpow2(numDataPoints);
    Y = fft(signal,NFFT)/numDataPoints;
    data.(dataName).freqs = sampleFreq/2*linspace(0,1,NFFT/2+1);
    fftLength=length(data.(dataName).freqs);
    power=2*abs(Y(1:NFFT/2+1));
    powern=2*abs(Y(end:-1:NFFT/2+1));
    powern(end+1)=0;
    data.(dataName).power=(power+powern)/2;
    [sgP,~] = savitzy( 2, settings.smoothingWindow, data.(dataName).power, 1 );
    data.(dataName).name=dataName; % TODO: This is probably not necessary any more
    data.(dataName).smoothPower=sgP;
    data.(dataName).smoothPower(end+1:fftLength)=0;
    if (flags == 'plotOn')
        figure;
        display(sprintf('\t\tPlotting data: freq size: %i, power size: %i, smooth power size: %i',fftLength,length(data.(dataName).power),length(data.(dataName).smoothPower)));
        plot(   data.(dataName).freqs/1000, data.(dataName).power,'b', ...
                data.(dataName).freqs/1000, data.(dataName).smoothPower,'g');
        xlabel('freq (1/ps)');
        ylabel('power');
        legend('raw data','smoothed data');
        title(dataName);
    end
end
