function [dcdPruned] = pruneDcdList(dcds,patterns)
dcdPruned={};
for dcdi=1:length(dcds)
    if (any( regexp( dcdf, [ '(' strjoin( patterns,'|' ) ')' ] )));
        continue;
    end
    dcdPruned{end+1}=dcds(dcdi).name;
end
end
