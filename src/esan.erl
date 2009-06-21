-module(esan).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xs, [xslapply/2]).

default_valid_tags() ->
	[a, b, big, br, center, dd, dl, dt, em, embed, h1, h2, h3, h4, h5, h6, i, img, li, menu, object, ol, p, small, strike, strong, table, td, th, tr, tt, u, ul].

default_valid_attributes() ->
	[align, allowfullscreen, alt, height, href, hspace, src, type, vspace, width].

sanitize(Html) ->
	sanitize(Html, default_valid_tags(), default_valid_attributes()).

sanitize(Html, ValidTags, ValidAttributes) ->
	try
		put(valid_tags, ValidTags),
		put(valid_attributes, ValidAttributes),

		% Html may not be inside a single element, thus we put it inside one
		WithRoot = "<root>" ++ Html ++ "</root>",

		{E, _Rest} = xmerl_scan:string(WithRoot),
		Result = xslapply(fun sanitize_xml/1, E),
		{ok, Result}
	catch
		_ : Reason -> {error, Reason}
	end.

sanitize_xml(E = #xmlElement{name = root}) ->
	xslapply(fun sanitize_xml/1, E);

sanitize_xml(E = #xmlElement{}) ->
	ValidTags = get(valid_tags),
	Name = E#xmlElement.name,
	case lists:member(Name, ValidTags) of
		true ->
			case E#xmlElement.content of
				[] ->
					case E#xmlElement.attributes of
						[]         -> ["<", atom_to_list(Name),                                       " />"];
						Attributes -> ["<", atom_to_list(Name), " ", sanitize_attributes(Attributes), " />"]
					end;

				_ ->
					OpenTag = case E#xmlElement.attributes of
						[]         -> ["<", atom_to_list(Name),                                       ">"];
						Attributes -> ["<", atom_to_list(Name), " ", sanitize_attributes(Attributes), ">"]
					end,
					CloseTag = ["</", atom_to_list(Name), ">"],
					[OpenTag, xslapply(fun sanitize_xml/1, E), CloseTag]
			end;

		false -> ""
	end;

sanitize_xml(T = #xmlText{}) ->
	T#xmlText.value.

sanitize_attributes(Attributes) ->
	ValidAttributes = get(valid_attributes),
	Strings = lists:foldr(
		fun(A, Acc) ->
			Name = A#xmlAttribute.name,
			case lists:member(Name, ValidAttributes) of
				true ->
					Value = A#xmlAttribute.value,
					[atom_to_list(Name) ++ "=\"" ++ Value ++ "\"" | Acc];

				false ->
					Acc
			end
		end,
		[],
		Attributes
	),
	string:join(Strings, " ").
