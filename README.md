# cl-webkit - a binding to WebKitGTK+ for Common Lisp

## About
cl-webkit is a FFI binding to [WebKitGTK+], currently targeting
WebKit version 2.

[WebKitGTK+]: http://webkitgtk.org/

## Stability
This is alpha software. Do not use.

## Additional dependencies
- [CFFI](http://common-lisp.net/project/cffi)

## Contributing
Send patches via email or as pull requests on [GitHub].
Feel free to append your name to the list of
[CONTRIBUTORS](../master/CONTRIBUTORS) (anything above fixing single-letter
typos warrants a mention).

A few things to keep in mind

- When adding a new binding, maintain a consistent mapping to the
  C API, so that users can find what they need and easily cross-reference
  the C API documentation
- Each major section of the C API should be covered by a separate file under
  `webkit2/`
- Document the binding not the C API

Currently, the goal is not to cover the entire C API, but to grow the binding
by demand.

[GitHub]: https://github.com/joachifm/cl-webkit

## Resources
* [WebKit2GTK+ Stable API reference](http://webkitgtk.org/reference/webkit2gtk/stable/index.html)
* [CFFI User Manual](http://common-lisp.net/project/cffi/manual/html_node/index.html)

## Copying
cl-webkit is distributed under the MIT license (see [COPYING](../master/COPYING)).
Please note that this licence only covers the binding itself.
Refer to the distribution terms of the third-party dependencies for
details.
