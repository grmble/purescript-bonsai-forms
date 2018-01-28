# Purescript Bonsai Forms

A DSL for html form generation.

Making good looking forms is all about structuring your
html to match your css library's (pure css, bootstrap, ...)
expectations and using the correct css classes.

Making correct forms with Bonsai is all about avoiding state
that is not in the model.  This means you need a model for
every form, and you can't just use your record types either -
e.g. for a number input, you want to store even the users
non-number input and let him correct it on his own.

All this amounts to a lot of boilerplate.

This library attempts to provide

* a DSL that describes a form
* a reference implementation that tranforms
  this DSL into html markup for [Pure CSS](https://purecss.io/)
* automatic form generation for generic data types:
  the compiler has enough information about your data types
  to generate simple but working forms

That's the idea anyway.  Right now, there is a first shot at\
the DSL and an interpreter for Pure CSS aligned forms.

* Support for textarea and all input types except hidden and file.
* customMarkup/customControl can be used to add missing functionality,
  e.g. html selects (example at https://github.com/grmble/purescript-bonsai-forms-demo)
