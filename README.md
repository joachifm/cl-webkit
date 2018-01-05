# cl-webkit - a binding to WebKitGTK+ for Common Lisp

## About
cl-webkit is a binding to [WebKitGTK+] for Common Lisp, currently
targeting WebKit version 2.
The WebKitGTK+ library adds web browsing capabilities to an application,
leveraging the full power of the WebKit browsing engine.

[WebKitGTK+]: http://webkitgtk.org/

## API overview
The cl-webkit API closely follows the WebKit2 API, with the exception that
class based interfaces are preferred over functional interfaces in cases
where both are provided.
That is, `*_{new,get,set}` methods are excluded in favour of `make-instance`
and slot accessors wherever possible.

For documentation on how to use the WebKit2 API, please refer to the C API
documentation.

Backwards compatibility (with older WebKit2 API versions) is not a priority.
The binding targets the latest version of Webkit2Gtk+ available on mainstream
distributions.

## Status

The binding currently covers most of the WebKit 2.4 API.

## Dependencies
- A working installation of WebKit2GTK+ (whichever
  package provides libwebkit2gtk-3.0.so, e.g.,
  libwebkit2gtk on Debian).
- A [CFFI] compatible Common Lisp implementation.
  The package is developed using [SBCL] but should work on any
  implementation that supports CFFI.
- A recent checkout of [cl-cffi-gtk]

To run the test-suite you also need a checkout of [lisp-unit].

[CFFI]: http://common-lisp.net/project/cffi
[cl-cffi-gtk]: https://github.com/crategus/cl-cffi-gtk.git
[lisp-unit]: https://github.com/OdonataResearchLLC/lisp-unit.git
[SBCL]: http://sbcl.org

## Installation

Assuming you have [Quicklisp] installed, do

    $ git clone https://github.com/joachifm/cl-webkit.git ~/common-lisp/quicklisp/local-projects/cl-webkit
    $ lisp
    * (ql:quickload :cl-webkit2)

[Quicklisp]: http://quicklisp.org/

## Contributing
Send patches via email or as pull requests on [GitHub].
Feel free to append your name to the list of
[CONTRIBUTORS](https://github.com/joachifm/cl-webkit/blob/master/CONTRIBUTORS).

A few things to keep in mind

- When adding a new binding, maintain a consistent mapping to the
  C API, so that users can find what they need and easily cross-reference
  the C API documentation
- Each major section of the C API should be covered by a separate file under
  `webkit2/`
- Document the binding not the C API
- Contributing implies an agreement to redistribute the work under the
  project's license (see below); please make sure that you own the rights
  to the work you contribute

[GitHub]: https://github.com/joachifm/cl-webkit

## Resources
* [WebKit2GTK+ Stable API reference](http://webkitgtk.org/reference/webkit2gtk/stable/index.html)
* [CFFI User Manual](http://common-lisp.net/project/cffi/manual/html_node/index.html)

## See also
* [lispkit](https://github.com/AeroNotix/lispkit), a lisp browser using WebKit

## Copying
cl-webkit is distributed under the MIT license
(see [COPYING](https://github.com/joachifm/cl-webkit/blob/master/COPYING)).
Please note that this licence only covers the binding itself.
Refer to the distribution terms of the third-party dependencies for
details.
