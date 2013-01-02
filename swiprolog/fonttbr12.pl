/* fonttbr12SWI.pl
 *
 * Font width tables for tk_tree.pl and latex_tree.pl
 *
 * font_width(+FontName, +String, ?PixelWidth) iff the width of String
 *  in FontName is PixelWidth.
 */

:- module(fonttbr12,[	label_size/3,
			tk_font_name/1,
			tk_symfont_name/1,
			tk_geometry/1
			]).
%:- use_module(library(charsio), [format_to_chars/3]).

tk_geometry('wm geometry . 380x428-5+40').
tk_symfont_name('-adobe-symbol-medium-r-normal--12-120-*').
tk_font_name('-*-times-bold-r-normal--12-120-*').

label_size(Label, PixWidth, 12) :- 
%	format_to_chars("~p", [Label], LabelChars),
	sformat(LabelString,'~p',[Label]), string_to_list(LabelString,LabelChars),
	chars_width(LabelChars, 0, Width0, tbr12),
	PixWidth is Width0/100000.

chars_width([], Width, Width, _).
chars_width([C|Cs], Width0, Width, Font) :-
	char_width(Font, C, Width1),
	Width2 is Width0+Width1,
	chars_width(Cs, Width2, Width, Font).

char_width(tbr12, C, Width) :- tbr12(C, Width0), Width is Width0*100000.

%name:  -adobe-times-bold-r-normal--12-120-75-75-p-67-iso8859-1
% bounds:		width left  right  asc  desc   attr   keysym
%	min		   3    -1     0    -2    -7  0x0000
%	max		  13     1    12    12     3  0x0000
%  obtained using xlsfonts -lll -fn -*-times-bold-r-normal--*-120-* > tbr12.pl
%  and then grepping and editing the results
tbr12(32,  3).
tbr12(33,  4).
tbr12(34,  5).
tbr12(35,  6).
tbr12(36,  6).
tbr12(37, 12).
tbr12(38, 10).
tbr12(39,  4).
tbr12(40,  4).
tbr12(41,  4).
tbr12(42,  6).
tbr12(43,  7).
tbr12(44,  4).
tbr12(45,  7).
tbr12(46,  4).
tbr12(47,  4).
tbr12(48,  6).
tbr12(49,  6).
tbr12(50,  6).
tbr12(51,  6).
tbr12(52,  6).
tbr12(53,  6).
tbr12(54,  6).
tbr12(55,  6).
tbr12(56,  6).
tbr12(57,  6).
tbr12(58,  4).
tbr12(59,  4).
tbr12(60,  7).
tbr12(61,  7).
tbr12(62,  8).
tbr12(63,  7).
tbr12(64, 12).
tbr12(65,  9).
tbr12(66,  9).
tbr12(67,  8).
tbr12(68,  9).
tbr12(69,  8).
tbr12(70,  8).
tbr12(71,  9).
tbr12(72, 10).
tbr12(73,  5).
tbr12(74,  7).
tbr12(75, 10).
tbr12(76,  8).
tbr12(77, 11).
tbr12(78,  9).
tbr12(79,  9).
tbr12(80,  8).
tbr12(81,  9).
tbr12(82,  9).
tbr12(83,  7).
tbr12(84,  9).
tbr12(85,  9).
tbr12(86,  9).
tbr12(87, 12).
tbr12(88,  9).
tbr12(89,  9).
tbr12(90,  8).
tbr12(91,  4).
tbr12(92,  3).
tbr12(93,  4).
tbr12(94,  7).
tbr12(95,  6).
tbr12(96,  4).
tbr12(97,  7).
tbr12(98,  6).
tbr12(99,  6).
tbr12(100,  6).
tbr12(101,  7).
tbr12(102,  4).
tbr12(103,  6).
tbr12(104,  6).
tbr12(105,  3).
tbr12(106,  3).
tbr12(107,  7).
tbr12(108,  3).
tbr12(109,  9).
tbr12(110,  6).
tbr12(111,  7).
tbr12(112,  6).
tbr12(113,  6).
tbr12(114,  5).
tbr12(115,  6).
tbr12(116,  4).
tbr12(117,  6).
tbr12(118,  6).
tbr12(119,  9).
tbr12(120,  6).
tbr12(121,  6).
tbr12(122,  6).
tbr12(123,  5).
tbr12(124,  3).
tbr12(125,  5).
tbr12(126,  7).
tbr12(127,  0).
tbr12(128,  0).
tbr12(129,  0).
tbr12(130,  0).
tbr12(131,  0).
tbr12(132,  0).
tbr12(133,  0).
tbr12(134,  0).
tbr12(135,  0).
tbr12(136,  0).
tbr12(137,  0).
tbr12(138,  0).
tbr12(139,  0).
tbr12(140,  0).
tbr12(141,  0).
tbr12(142,  0).
tbr12(143,  0).
tbr12(144,  0).
tbr12(145,  0).
tbr12(146,  0).
tbr12(147,  0).
tbr12(148,  0).
tbr12(149,  0).
tbr12(150,  0).
tbr12(151,  0).
tbr12(152,  0).
tbr12(153,  0).
tbr12(154,  0).
tbr12(155,  0).
tbr12(156,  0).
tbr12(157,  0).
tbr12(158,  0).
tbr12(159,  0).
tbr12(160,  3).
tbr12(161,  4).
tbr12(162,  6).
tbr12(163,  6).
tbr12(164,  7).
tbr12(165,  8).
tbr12(166,  3).
tbr12(167,  6).
tbr12(168,  4).
tbr12(169, 11).
tbr12(170,  4).
tbr12(171,  8).
tbr12(172,  8).
tbr12(173,  4).
tbr12(174, 11).
tbr12(175,  4).
tbr12(176,  5).
tbr12(177,  7).
tbr12(178,  4).
tbr12(179,  4).
tbr12(180,  4).
tbr12(181,  6).
tbr12(182,  8).
tbr12(183,  3).
tbr12(184,  4).
tbr12(185,  4).
tbr12(186,  4).
tbr12(187,  8).
tbr12(188,  9).
tbr12(189,  9).
tbr12(190,  9).
tbr12(191,  7).
tbr12(192,  9).
tbr12(193,  9).
tbr12(194,  9).
tbr12(195,  9).
tbr12(196,  9).
tbr12(197,  9).
tbr12(198, 13).
tbr12(199,  8).
tbr12(200,  8).
tbr12(201,  8).
tbr12(202,  8).
tbr12(203,  8).
tbr12(204,  5).
tbr12(205,  5).
tbr12(206,  5).
tbr12(207,  5).
tbr12(208,  9).
tbr12(209,  9).
tbr12(210,  9).
tbr12(211,  9).
tbr12(212,  9).
tbr12(213,  9).
tbr12(214,  9).
tbr12(215,  7).
tbr12(216,  9).
tbr12(217,  9).
tbr12(218,  9).
tbr12(219,  9).
tbr12(220,  9).
tbr12(221,  9).
tbr12(222,  8).
tbr12(223,  8).
tbr12(224,  7).
tbr12(225,  7).
tbr12(226,  7).
tbr12(227,  7).
tbr12(228,  7).
tbr12(229,  7).
tbr12(230,  8).
tbr12(231,  6).
tbr12(232,  7).
tbr12(233,  7).
tbr12(234,  7).
tbr12(235,  7).
tbr12(236,  3).
tbr12(237,  3).
tbr12(238,  3).
tbr12(239,  3).
tbr12(240,  7).
tbr12(241,  6).
tbr12(242,  7).
tbr12(243,  7).
tbr12(244,  7).
tbr12(245,  7).
tbr12(246,  7).
tbr12(247,  7).
tbr12(248,  7).
tbr12(249,  6).
tbr12(250,  6).
tbr12(251,  6).
tbr12(252,  6).
tbr12(253,  6).
tbr12(254,  6).
tbr12(255,  6).
