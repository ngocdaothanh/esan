-module(esan).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xs, [xslapply/2]).

default_acceptable_tags() ->
    [a, abbr, acronym, address, area, b, big, blockquote, br, caption, center,
    cite, code, col, colgroup, dd, del, dfn, dir, 'div', dl, dt, em, embed,
    font, h1, h2, h3, h4, h5, h6, hr, i, img, ins, kbd, label, legend, li,
    menu, object, ol, p, pre, q, s, samp, small, span, strike, strong, sub, sup,
    table, tbody, td, tfoot, th, thead, tr, tt, u, ul].

default_acceptable_attributes() ->
    [abbr, align, allowfullscreen, alt, axis, border, cellpadding, cellspacing,
    cols, colspan, dir, height, href, hreflang, hspace, label, nohref, noshade,
    nowrap, prompt, rel, rev, rows, rowspan, rules, size, span, src, title,
    type, valign, vspace, width].
    
sanitize(Html) ->
    sanitize(Html, default_acceptable_tags(), default_acceptable_attributes()).

sanitize(Html, AcceptableTags, AcceptableAttributes) ->
    try
        put(acceptable_tags, AcceptableTags),
        put(acceptable_attributes, AcceptableAttributes),

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
    AcceptableTags = get(acceptable_tags),
    Name = E#xmlElement.name,
    case lists:member(Name, AcceptableTags) of
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
    AcceptableAttributes = get(acceptable_attributes),
    Strings = lists:foldr(
        fun(A, Acc) ->
            Name = A#xmlAttribute.name,
            case lists:member(Name, AcceptableAttributes) of
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
