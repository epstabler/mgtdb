%   File   : mg0.pl
%   Author : E Stabler
%   Updated: Mar 2012
% ?- lexBuild(LexT), recognize(LexT,[the,king,prefers,the,beer).
% ?- lexBuild(LexT), recognize(LexT,[the,queen,prefers,the,wine]).
% ?- lexBuild(LexT), recognize(LexT,[the,king,knows,which,wine,the,queen,prefers]).
% ?- lexBuild(LexT), recognize(LexT,['Sue',knows,the,king,knows,which,beer,'John',prefers]).

:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features

[]::[='V','C'].               []::[='V',+wh,'C'].
[drinks]::[='D',='D','V'].    [prefers]::[='D',='D','V'].
[knows]::[='C',='D','V'].     [says]::[='C',='D','V'].
[the]::[='N','D'].            [which]::[='N','D',-wh].
[king]::['N'].                [queen]::['N'].
[wine]::['N'].                [beer]::['N'].
['Sue']::['D'].               ['John']::['D'].

startCategory('C').
