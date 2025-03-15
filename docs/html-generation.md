# HTML Generation

When debugging or adding functionality, it can be useful to know which code is responsible for which part of the generated output.
For this reason, we support HTML generation, which is an annotated version of the regular Coq output:
each generated token has associated with it a list of OCaml code locations, which more or less represents a "stack trace".

To generate this HTML, create an extra file with extension `.template.html`, e.g., `base.template.html`.
Add the following:

```html
<!DOCTYPE html>
<html>
    <head>
        <style>
            body {
                font-family: monospace, monospace;
            }

            .tooltipped {
                position: relative;
            }

            .tooltip {
                background-color: white;
                visibility: hidden;
                border: black 1px solid;
                position: absolute;
                z-index: 1;
                padding: 1em;
                top: 100%;
                left: 0;
            }

            .tooltipped:hover {
                border: 1px solid black;
            }
            
            .tooltipped:hover > .tooltip {
                visibility: visible;
            }
        </style>
    </head>
    <body>
(*<
    (generate (base-html-translation))
>*)
    </body>
</html>
```

Next, in the `configuration.lisp` file, add an extra line

```lisp
(template "base.template.html")
```

See the end to end tests (`tests/coq-tests`) for examples.

**Note**
The generated HTMLs have at some point grown to such a size (>500MB) that Chrome could not open them.
Some steps towards reducing the number of annotations have been taken, but perhaps we should
consider removing even more of them to keep the HTMLs to a manageable size.
