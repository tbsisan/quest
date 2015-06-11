% Process cFK results for a seties of spring constants, k.
% One figure is produced for each k value.
% For each value of the spring constant, there will be multiple runs at the same parameter set.
% Data from these runs are simultaneously plotted on a few different axes in one figure.

klist={'20','25','30','40','50','60','80','12'}
klist={'30'}
close all;

for ki=1:length(klist)

    % For all cFK simulations matching the following pattern,
    cFKpattern=['x.0*eqt8*Ti600*L*50*N*00*k0.' klist{ki} 'E*h0.15*F0.00*dat'];

    % Read data and make figure:
    cFKprocessor;

    % Save figure as image.
    % savefigs('/home/tbs246/Dropbox/LichterGroup/meetings/6.4.2015/', [cFKpruned{cFKi} '_'], 1);
    % close(fh);
end
