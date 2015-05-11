function [dcdPruned] = pruneDcdList(dcds,patterns)
dcdPruned={};
for dcdi=1:length(dcds)
    if (any( regexp( dcds(dcdi).name, [ '(' strjoin( patterns,'|' ) ')' ] )));
        continue;
    end
    dcdPruned{end+1}=dcds(dcdi).name;
end
end
