function horizontalLine(y,spec,color,varargin)
ax=axis;
hold on
for bari=1:length(y)
    yi=y(bari);
    %col=cplot([ax(1) ax(2)],[yi yi]);
    col=plot([ax(1) ax(2)],[yi yi],spec,'Color',color);
end
if ~isempty(varargin)
    y=varargin{1}*ax(4);
    txt=varargin{2};
    if (strcmp(varargin{3},'r'))
        text(x,y,{['\leftarrow',txt],num2str(x)},'BackgroundColor',1-(1-col)*0.5,'HorizontalAlignment','left','Margin',1e-6);
    else
        text(x,y,{[txt,'\rightarrow'],num2str(x)},'BackgroundColor',1-(1-col)*0.5,'HorizontalAlignment','right','Margin',1e-6);
    end
    
end
