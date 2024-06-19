-- Unused (for now)
module Drasil.Fake.Markdown where

type Markdown = [MarkdownFragment]

data HL = One | Two | Three | Four | Five | Six

data LT = Ordered | Unordered

data MarkdownFragment = Heading HL MarkdownText
                      | Paragraph MarkdownText
                      | List LT [MarkdownFragment]
                      | Quote [MarkdownText]
                      | CodeBlock String [MarkdownText]
                      | Figure String String

type URL = String

data MarkdownText = Plain String 
                  | (:+:) MarkdownFragment MarkdownFragment
                  | Italic String
                  | Bold String
                  | ItalicBold String
                  | Link MarkdownText URL
