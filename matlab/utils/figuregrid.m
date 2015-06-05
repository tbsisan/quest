function fighandle=figuregrid(datalength1,varargin)

gh=0.05; gw=0.03; um=0.05; lom=0.02; lm=0.05; rm=0.01;
gh=0.09; gw=0.05; um=0.08; lom=0.09; lm=0.10; rm=0.06;

if length(varargin)>0
    datalength2=varargin{1};
    hx=figure;
    % fighandle=tight_subplot(datalength1,datalength2,gh,um,lm,0);
    fighandle=tight_subplot(datalength1,datalength2,[gh gw],[lom um],[lm rm],0);
else
    subplots=datalength1;
    mingrid=floor(sqrt(subplots));
    if (mingrid*mingrid==subplots)
        wpanels=mingrid;
        hpanels=mingrid;
    elseif (mingrid*(mingrid+1)>=subplots)
        wpanels=mingrid;
        hpanels=mingrid+1;
    else
        wpanels=mingrid+1;
        hpanels=mingrid+1;
    end
    
    hx=figure;
    fighandle=tight_subplot(hpanels,wpanels,[gh gw],[lm um],[lm rm],0);
end
