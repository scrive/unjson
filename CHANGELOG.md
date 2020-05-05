# unjson-0.15.3 (2020-05-05)
* GHC 8.8 and 8.10 support

# unjson-0.15.2.1 (2018-11-19)
* GHC 8.6 support ([#12](https://github.com/scrive/unjson/pull/12)).
* Documentation fixes
  ([#11](https://github.com/scrive/unjson/issues/11),
  [#13](https://github.com/scrive/unjson/issues/13),
  [#14](https://github.com/scrive/unjson/issues/14)).

# unjson-0.15.2.0 (2018-03-18)
* GHC 8.4 support.

# unjson-0.15.1.0 (2018-03-05)
* -Wall police.
* API addition: arrayWithModeOf'.

# unjson-0.15.0.0 (2018-02-22)
* Breaking change: Unjson definitions now require the underlying types
  to have Typeable instances ([#6](https://github.com/scrive/unjson/pull/6)).
* Added a way to automatically derive Unjson definitions for
  parameterless sum types (`enumUnjsonDef`) ([#7](https://github.com/scrive/unjson/pull/7)).
* Dropped support for GHC < 7.8.

# unjson-0.14.1.3 (2017-04-24)
* Bumped the dependency on aeson
* Fixed the test suite on GHC 7.10.3

# unjson-0.14.1.2 (2017-04-11)
* Bumped the dependency on aeson

# unjson-0.14.1.1 (2017-02-28)
* Adjusting tests

# unjson-0.14.1.0 (2017-02-28)
* Changed (>>=) to prevent error unpacking

# unjson-0.14.0.1 (2016-09-21)
* add README.md to extra-source-files
* relax the constraint on aeson

# unjson-0.14 (2016-09-21)
* initial release
* fix compilation with aeson-1.0
