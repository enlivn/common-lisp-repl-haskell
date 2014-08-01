###Common Lisp REPL written in Haskell

####Features

* Interpreter supporting lambdas, closures, many Common Lisp functions (see complete list below)
* Can be run in REPL or single-command mode (see details in 'Running' section below)

Supported Common Lisp Type | |
------ | ----
`Symbols` | `Lists` (and Dotted Lists) |
`Functions` | `Streams` |
`Strings` | `Numbers` (integers only)

Supported Common Lisp Functions/Macros |
------ |
**Flow control functions** |
`if` |
`case` |
`cond` |
**Lambda functions** |
`apply` |
`funcall` |
`lambda` (with `&optional` and `&rest`) |
`defun` |
`eval` |
**List functions** |
`cons` |
`length` |
`subseq` |
`reverse`<sup>*</sup> |
`member`<sup>*</sup> |
`assoc`<sup>*</sup> |
`mapcar`<sup>*</sup> |
`remove`, `remove-if`, `remove-if-not` |
`car`, `cdr` |
`caar`, `cadr`, `cdar`, `cddr`, `caaar`, `caadr`, `cadar`, `caddr`, `cdaar`, `cdadr`, `cddar`, `cdddr`, `caaadrr`, `caaaarr`, `caadarr`, `cadaarr`, `caaddrr`, `cadadrr`, `caddarr`, `cdaaarr`, `cdaadrr`, `cadddrr`, `cdadarr`, `cdaddrr`, `cddaarr`, `cddadrr`, `cdddarr`, `cddddrr` <sup>*</sup> |
`function` |
**Number functions** |
`+`, `-`, `1+`, `1-`, `*`, `/` |
`/=`, `=`, `<`, `>`, `<=`, `>=` |
`plusp`, `minusp`, `evenp`, `oddp` <sup>*</sup>|
`mod`, `rem` |
**String functions** |
`string=`, `string-equal`, `string/=`, `string<`, `string-lessp`, `string>`, `string-greaterp`, `string<=`, `string-not-greaterp`, `string>=`, `string-not-lesserp` |
`string-downcase`, `string-upcase` (both with `:start` and `:end`)|
**IO functions** |
`read` |
`load` |
`open` (with `:direction`, `:input`, `:output`, `:io` |
`close` |
`prin1` |
`print` |
`write-string` |
**Other functions** |
`setq` |
`eql` |
`weakEqual` |
`atom` |
`quoted` (') |
`not`, `and`, `or` |
<sup>*</sup> indicates functions defined in `CommonLib.lisp`. It is only available in REPL mode. See the 'Running' section below on how to make these functions available in the REPL |


####Running
You will need GHCI installed.

    $> git clone https://github.com/neerajrao/haskell-common-lisp-interpreter.git
    $> cd haskell-common-lisp-interpreter

You can now try one of the following

Compiles source, creates the clisp executable and runs it

    $> make run


Compiles source, creates the clisp executable. Does NOT run it.

    $> make

Run in REPL mode

    $> clisp

Run in single-command mode

    $> clisp "your common lisp expression"

If you want the functions defined in `CommonLib.lisp` (the ones marked by <sup>*</sup> in the list of supported functions above) to be available, you must load it after the REPL starts

    clisp>> (load "src/CommonLib.lisp")

####Details
This project closely follows the excellent wikibook [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) by Jonathan Tang.

There are some important differences:

* Support for Lisp-2 namespaces (distinct variable and function namespaces) (instead of Scheme's Lisp-1 namespaces)
* Support for Common Lisp functions (instead of Scheme functions)

