module Multipart exposing
    ( Multipart, ContentType, Boundary, boundary, Header, header
    , mixed, alternative, subtype
    , addStringPart, addMultipart
    , string
    )

{-|

@docs Multipart, ContentType, Boundary, boundary, Header, header


## Empty initial values

@docs mixed, alternative, subtype


## Adding parts inside

@docs addStringPart, addMultipart


## Return a useful value

@docs string

-}

import Array exposing (Array)


{-| use the [`string`](#string) function to convert a `Multipart` value into a `String`
-}
type Multipart
    = Multipart SubType Boundary (Array Part)


type SubType
    = Mixed
    | Alternative
    | SubType String


stringFromSubType : SubType -> String
stringFromSubType partType =
    case partType of
        Mixed ->
            "mixed"

        Alternative ->
            "alternative"

        SubType s ->
            s


{-| 7-bit ASCII like e.g. `z84d86635fa691af3d9d7d9436cc5d44018a7bc8`
-}
type Boundary
    = Boundary String


{-| clamps given `String` to 7-bit ASCII
-}
boundary : String -> Boundary
boundary s =
    String.toList s
        |> List.map (Char.toCode >> clamp 32 126 >> Char.fromCode)
        |> String.fromList
        |> Boundary


{-| e.g. `text/html; charset="utf-8"`
-}
type alias ContentType =
    String


{-| An empty `Content-Type: multipart/mixed` parts container.

_The primary subtype for multipart, "mixed", is intended for use when the body parts
are independent and intended to be displayed serially. Any multipart subtypes that
an implementation does not recognize should be treated as being of subtype "mixed"._
<https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html>

-}
mixed : Boundary -> Multipart
mixed b =
    Multipart Mixed b Array.empty


{-| An empty `Content-Type: multipart/alternative` parts container.

_each of the parts is an "alternative" version of the same information. User agents
should recognize that the content of the various parts are interchangeable. The user
agent should either choose the "best" type based on the user's environment and
preferences, or offer the user the available alternatives. In general, choosing
the best type means displaying only the LAST part that can be displayed._
<https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html>

In the following multipart, the "best" type is `text/html`

    Multipart.alternative (Multipart.boundary "boundary-string")
        |> Multipart.addStringPart "text/plain; charset=\"utf-8\"" textHeaders textContent
        |> Multipart.addStringPart "text/html; charset=\"utf-8\"" htmlHeaders htmlContent
        |> Multipart.string

-}
alternative : Boundary -> Multipart
alternative b =
    Multipart Alternative b Array.empty


{-| An empty `Content-Type: multipart/{{subtype}}` parts container. Where `{{subtype}}`
is any `String` you supply

See <https://en.wikipedia.org/wiki/MIME#Multipart_subtypes>

    Multipart.subtype "related" (Multipart.boundary "boundary-string")
        |> Multipart.addStringPart "text/css; charset=\"utf-8\"" cssHeaders cssContent
        |> Multipart.addStringPart "text/html; charset=\"utf-8\"" htmlHeaders htmlContent
        |> Multipart.string

-}
subtype : String -> Boundary -> Multipart
subtype subType b =
    Multipart (SubType subType) b Array.empty


{-| -}
type Header
    = Header String String


{-|

    header "Content-Type" "application/json"

-}
header : String -> String -> Header
header k v =
    Header k v


type Part
    = Part (List Header) String
    | Nested Multipart


{-| Add a section to a `Multipart` value

    Multipart.mixed (Multipart.boundary "boundary-string")
        |> Multipart.addStringPart "text/plain" [] "hello world!"
        |> Multipart.string
    --> "Content-Type: text/plain\r\n\r\nhello world!"

A `Multipart` with more than 1 part carries `Content-Type` header of `multipart/alternative`

    Multipart.alternative (Multipart.boundary "boundary-string")
        |> Multipart.addStringPart "text/plain" [] "hello world!"
        |> Multipart.addStringPart "text/html" [] "<p>hello world!</p>"
        |> Multipart.string
    --> "Content-Type: multipart/alternative; boundary=\"boundary-string\"\r\nContent-Transfer-Encoding: 7bit\r\n\r\n--boundary-string\r\nContent-Type: text/plain\r\n\r\nhello world!\r\n\r\n--boundary-string\r\nContent-Type: text/html\r\n\r\n<p>hello world!</p>\r\n\r\n--boundary-string--"

-}
addStringPart : ContentType -> List Header -> String -> Multipart -> Multipart
addStringPart contentType headers content (Multipart type_ b parts) =
    parts
        |> Array.push (Part (header "Content-Type" contentType :: headers) content)
        |> Multipart type_ b


{-| Embeds another multipart into an outer multipart. The

    Multipart.mixed (Multipart.boundary "boundary-string111")
        |> Multipart.addStringPart "text/plain" [] "hello world!"
        |> Multipart.addMultipart
            (Multipart.alternative (Multipart.boundary "boundary-string222")
                |> Multipart.addStringPart "text/plain" [] "hello world!"
                |> Multipart.addStringPart "text/html" [] "<p>hello world!</p>"
            )
        |> Multipart.string
    --> "Content-Type: multipart/mixed; boundary=\"boundary-string111\"\r\nContent-Transfer-Encoding: 7bit\r\n\r\n--boundary-string111\r\nContent-Type: text/plain\r\n\r\nhello world!\r\n\r\n--boundary-string111\r\nContent-Type: multipart/alternative; boundary=\"boundary-string222\"\r\nContent-Transfer-Encoding: 7bit\r\n\r\n--boundary-string222\r\nContent-Type: text/plain\r\n\r\nhello world!\r\n\r\n--boundary-string222\r\nContent-Type: text/html\r\n\r\n<p>hello world!</p>\r\n\r\n--boundary-string222--\r\n\r\n--boundary-string111--"

-}
addMultipart : Multipart -> Multipart -> Multipart
addMultipart child (Multipart type_ b parts) =
    parts
        |> Array.push (Nested child)
        |> Multipart type_ b


{-| Returns a `Multipart` value as `String` so you can use it, e.g. as an email body
-}
string : Multipart -> String
string (Multipart type_ (Boundary bs) parts) =
    case Array.toList parts of
        [] ->
            ""

        part :: [] ->
            partString part

        list ->
            let
                -- Note that the encapsulation boundary must occur at the beginning of a line,
                -- i.e., following a CRLF, and that that initial CRLF is considered to be part
                -- of the encapsulation boundary rather than part of the preceding part. The
                -- boundary must be followed immediately either by another CRLF and the header
                -- fields for the next part, or by two CRLFs, in which case there are no header
                -- fields for the next part (and it is therefore assumed to be of Content-Type
                -- text/plain).
                --
                -- https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
                boundarySeparator =
                    crlf ++ crlf ++ "--" ++ bs ++ crlf

                output =
                    List.map partString list
                        |> String.join boundarySeparator
            in
            String.join crlf
                (List.map headerString
                    [ header "Content-Type" ("multipart/" ++ stringFromSubType type_ ++ "; boundary=\"" ++ bs ++ "\"")
                    , header "Content-Transfer-Encoding" "7bit"
                    ]
                )
                ++ boundarySeparator
                ++ output
                ++ String.trimRight boundarySeparator
                -- The encapsulation boundary following the last body part is a distinguished
                -- delimiter that indicates that no further body parts will follow. Such a
                -- delimiter is identical to the previous delimiters, with the addition of two
                -- more hyphens at the end of the line
                --
                -- https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
                ++ "--"


partString : Part -> String
partString part =
    case part of
        Part headers content ->
            List.map headerString headers
                ++ [ "", content ]
                |> String.join crlf

        Nested multipart ->
            string multipart


headerString : Header -> String
headerString (Header k v) =
    k ++ ": " ++ v


crlf : String
crlf =
    "\u{000D}\n"
