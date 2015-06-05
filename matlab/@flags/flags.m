function obj = flags (varargin)
  if (nargin == 1 && isa (varargin{1}, 'flags'))
    obj = varargin{1};   % Copy constructor
  else
    s.cell = varargin;
    obj = class (s, 'flags');
  end
end
