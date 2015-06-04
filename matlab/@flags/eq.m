function C = eq(A,B)
  % Define an equality operation on cell arrays.  B should have a single value.
  % the == operator can hereby be used on cell arrays
  % and returns true if B is anywhere in A
  Acell=A;
  Bcell=B;
  if strcmp( class(A), 'flags' )
      Acell=A.cell;
  end
  if strcmp( class(B), 'flags' )
      Bcell=B.cell;
  end
  C = any(strcmp(Acell,Bcell));  % Apply equality comparison cell-wise
end
