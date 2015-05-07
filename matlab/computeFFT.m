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
data(numDataArrays)=struct();
data.power=zeros(1,numTimePoints);
data.smoothPower=zeros(1,numTimePoints);
data.freqs=zeros(1,numTimePoints);

for datai=1:numDataArrays
    dataName = inputname(datai+2);
    ydata=varargin{datai};
    numDataPoints=length(ydata);
    if (numDataPoints ~= numTimePoints)
        display(sprintf('ERROR: sample time array (%i) has different length than data array (%i) for %s',numTimePoints,numDataPoints,dataName));
        continue;
    end
    signal = ydata - mean( ydata );
    sampleTime_ns=sampleTimes(2)-sampleTimes(1);
    sampleFreq=1/sampleTime_ns;
    NFFT = 2^nextpow2(numDataPoints);
    Y = fft(signal,NFFT)/numDataPoints;
    data(datai).freqs = sampleFreq/2*linspace(0,1,NFFT/2+1);
    power=2*abs(Y(1:NFFT/2+1));
    powern=2*abs(Y(end:-1:NFFT/2+1));
    powern(end+1)=0;
    data(datai).power=(power+powern)/2;
    [sgP,~] = savitzy( 2, settings.smoothingWindow, data(datai).power, 1 );
    data(datai).smoothPower=sgP;
    data(datai).smoothPower(end+1:numTimePoints)=0;
    data(datai).name=dataName;
    if (flags == 'plotOn')
        figure;
        plot(   data(datai).freqs/1000, data(datai).power,'b', ...
                data(datai).freqs/1000, data(datai).smoothPower,'g');
        xlabel('freq (1/ps)');
        ylabel('power');
        legend('raw data','smoothed data');
        title(dataName);
    end
end
