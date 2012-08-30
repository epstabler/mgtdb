%   File   : mgxx.pl
%   Author : E Stabler
%   Updated: Mar 00
%     grammar for the copy language {xx| x\in{a,b}*}

[]::['T'].
[]::['T',-r,-l].            []::[='T',+r,+l,'T'].
[a]::[='T',+r,'A',-r].      [b]::[='T',+r,'B',-r].
[a]::[='A',+l,'T',-l].      [b]::[='B',+l,'T',-l].

startCategory('T').
