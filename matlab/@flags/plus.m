function C = plus (A, B)
  C.cell = cellfun (@plus, A.cell, B.cell, 'UniformOutput', false);
  C = class (C, 'flags'); 
end 
