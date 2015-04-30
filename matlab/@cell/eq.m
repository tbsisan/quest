function C = eq(A,B)
  % Define an equality operation on cell arrays.  B should have a single value.
  % the == operator can hereby be used on cell arrays
  % and returns true if B is anywhere in A
  C = any(strcmp(A,B));  %# Apply equality comparison cell-wise
end
