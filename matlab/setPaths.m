paths.home        = '/home/tbsisan';
paths.projectName = 'XiEffects/changeh/projScience';
paths.projectName = 'cFK/polymer';
paths.projectHome = [ paths.home '/' paths.projectName ];
paths.scriptPath  = [ paths.home '/md' ];
paths.storage     = '/projects/p20200';
paths.projectStor = [ paths.storage '/' paths.projectName ];
paths.remoteStor  = [ 'tbs246@quest.it.northwestern.edu:' paths.projectStor ];
paths.remoteStor  = '';
paths.dcdPath     = [ paths.projectStor '/namdState' ];
paths.movieStor   = [ paths.projectStor '/movies' ];
addpath(genpath([paths.home paths.scriptPath]));
paths
