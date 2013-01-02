%   File   : g-ne.pl - naive english
%   Author : E Stabler
%   Updated: Mar 00

[]::[=i,c].                []::[=i,+wh,c].
['-s']::[=pred,+v,+k,i].   []::[=vt,+k,=d,pred].    []::[=v,pred].
[praise]::[=d,vt,-v].      [laugh]::[=d,v,-v].
[lavinia]::[d,-k].         [titus]::[d,-k].         [who]::[d,-k,-wh].

startCategory(c).
