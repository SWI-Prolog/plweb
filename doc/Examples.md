# Adding examples to the SWI-Prolog website

## An Example is a markdown file

----------------------------------------------------------------
# Title

Bla bla

```
Code
```

```
?- query
```

@see p/1, p/2, library(x)
----------------------------------------------------------------

## Indexing

  - If @see, link to the predicate indicators and libraries listed
  - Else, scan the code and find it

## Display

  - Code rendered using htmlsrc when possible.
  - Extend to link goals.

## Include in the website

  - Show shortest if < N lines
  - Show max M others as carousel
  - _Show all all_ to fill carousel

Show only fragment containing predicate from a more extensive example
with _view all_?

Embedded SWISH like LPN?



## Import/export to SWISH

  - Sequence of markdown and code blocks.
