# cl-webkit - a binding to WebKitGtk+ for Common Lisp
This is alpha software. Do not use.

## Building
cl-webkit currently requires working installations of

* [WebKitGtk+](http://webkitgtk.org/)
* [CFFI]

Use [Quicklisp] to install any missing Lisp dependencies.

Then, you should be able to load cl-webkit thus

    $ cd cl-webkit && sbcl
    * (progn
        (load "cl-webkit.asd")
        (asdf:load-system :cl-webkit))

[Quicklisp]: http://www.quicklisp.org/
[CFFI]: http://common-lisp.net/projects/cffi/

## Compatibility
cl-webkit should be compatible with all Common Lisp implementations that
support [CFFI].

## Contributing
Send patches via email or as pull requests on [GitHub].
Feel free to append your name to the list of
[CONTRIBUTORS](../master/CONTRIBUTORS) (anything above fixing single-letter
typos warrants a mention).

[GitHub]: https://github.com/joachifm/cl-webkit

## Resources
* [WebKitGTK+ API reference](http://webkitgtk.org/reference/index.html)

## Copying
cl-webkit is distributed under a 2-clause BSD license,
see [COPYING](../master/COPYING).

Please note that this licence only covers the binding itself.
An image containing .so files from third-party libraries may be covered
by additional terms and is most likely not redistributable without also
providing full source code or a mechanism for re-linking.
Refer to the distribution terms of the third-party dependencies for
details.
