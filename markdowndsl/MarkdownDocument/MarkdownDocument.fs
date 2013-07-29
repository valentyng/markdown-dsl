namespace MarkdownDocument


type MarkdownDocument = list<MarkdownBlock>

and MarkdownBlock = 
    | Heading of int * MarkdownSpans
    | Paragraph of MarkdownSpans
    | CodeBlock of list<string>

and MarkdownSpans = list<MarkdownSpan>

and MarkdownSpan = 
    | Literal of string
    | InlineCode of string
    | String of MarkdownSpans
    | Emphasis of MarkdownSpans
    | HyperLink of MarkdownSpans * string


