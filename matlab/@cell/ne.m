function C = eq(A,B)
  % Define an inequality operation on cell arrays.  B should have a single value.
  % the ~= operator can hereby be used on cell arrays
  % and returns true if B is nowhere in A
  C = ~any(strcmp(A,B));  % Apply equality comparison cell-wise
end
