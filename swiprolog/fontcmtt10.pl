/* fontcmtt10.pl
 *
 * Font width tables for tk_tree.pl and latex_tree.pl
 *
 * font_width(+FontName, +String, ?PixelWidth) iff the width of String
 *  in FontName is PixelWidth.
 */

:- module(fontcmtt10,[	label_size/3,
			tk_font_name/1,
			tk_symfont_name/1,
			tk_geometry/1
			]).
%:- use_module(library(charsio), [format_to_chars/3]).

tk_symfont_name('-adobe-symbol-medium-r-normal--10-100-*').
tk_font_name('-*-courier-bold-r-normal--10-100-*').
tk_geometry('wm geometry . 380x428-5+40').

label_size(Label, PixWidth, 10) :- 
%	format_to_chars("~p", [Label], LabelChars),
	sformat(LabelString,'~p',[Label]), string_to_list(LabelString,LabelChars),
	chars_width(LabelChars, 0, Width0, cmtt10),
	PixWidth is Width0/100000.

% chars_width(Chars, Width, Font) returns the width in pixels of Chars

chars_width([], Width, Width, _).
chars_width([C|Cs], Width0, Width, Font) :-
	char_width(Font, C, Width1),
	Width2 is Width0+Width1,
	chars_width(Cs, Width2, Width, Font).

char_width(cmtt10, _, 524996).
