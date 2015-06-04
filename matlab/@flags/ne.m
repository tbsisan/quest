function C = ne(A,B)
  % Define an inequality operation on cell arrays.  B should have a single value.
  % the ~= operator can hereby be used on cell arrays
  % and returns true if B is nowhere in A
  Acell=A;
  Bcell=B;
  if strcmp( class(A), 'flags' )
      Acell=A.cell;
  end
  if strcmp( class(B), 'flags' )
      Bcell=B.cell;
  end
  C = ~any(strcmp(Acell,Bcell));  % Apply equality comparison cell-wise
end
