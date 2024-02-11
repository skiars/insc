# insc

ins is a simple markup language that can be used to manage LLM instruction datasets. The syntax of this markup language is somewhat similar to XML, and XML syntax highlighting can be used directly:
``` xml
<s>
system prompt
<ins>instruction</ins>
assistant output
</s>
```
As above, ins is actually a very simple markup language: a `<s>... </s>` block represents a chat, the text before the first `<ins>` is an optional system prompt, and `<ins>...</ins>` is instruction followed by assistant output.

This is pretty much all the details of the ins:
- Escape characters are not supported (for now).
- Whitespace characters (spaces, line breaks, etc.) before and after a text node are removed.
- Only one space inside the text is retained.
- A maximum of two newlines will be retained inside the text.

## Usage

### `*.ins` file

An ins file is a text file with an `.ins` suffix, in which there may be multiple `<s>... </s>` blocks:
``` xml
<s>
system prompt
<ins>instruction 1</ins>
assistant output 1
<ins>instruction 2</ins>
assistant output 2
</s>

<s> ... </s>
...
```

### `insc` commands

- `insc PATH`：Collect all `*.ins` files in the `PATH` directory and output a JSON dataset (ChatML format)
- `insc -d=TYPE PATH`
- `insc -c PATH`
