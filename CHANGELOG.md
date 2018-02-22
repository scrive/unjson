# unjson-0.15.0.0 (2018-02-22)
* Breaking change: Unjson definitions now require the underlying types
  to have Typeable instances ([#6](https://github.com/scrive/unjson/pull/6)).
* Added a way to automatically derive Unjson definitions for
  parameterless sum types (`enumUnjsonDef`) ([#7](https://github.com/scrive/unjson/pull/7)).
