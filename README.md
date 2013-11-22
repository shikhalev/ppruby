# ppRuby

<img src="https://raw.github.com/shikhalev/ppruby/new/img/logo.png" align="right">

**Ruby binding for Free Pascal and Lazarus**

**Author:** [Ivan Shikhalev](https://github.com/shikhalev).

**License:** [GNU GPL](http://www.gnu.org/copyleft/gpl.html).

**Homepage:** [this](https://github.com/shikhalev/ppruby), welcome
to [Wiki](https://github.com/shikhalev/ppruby/wiki)
and [Issue tracker](https://github.com/shikhalev/ppruby/issues).

**Version:** 1.0 public beta.

## Overview

This is a lazarus-package for use Ruby in Pascal projects. It is solving two task:
* Add a Ruby scripting engine in your project;
* or write extension libraries for Ruby using Free Pascal.

### Package

This package contains the next parts:
* API bindings in `ruby.pas`. This file may be compiled in two different modes
  (by setting conditional defines): static and dynamic. If `RUBY_STATIC`
  defined, your program will linked with selected version of `libruby`. It is
  the only right way for creation ruby extension and efficient but not flexible
  way for add Ruby in pascal programs. In dynamic way you select library version
  at run-time.
* Units `RbTools` and `RbObjects` provided some utilities for comfortable
  conversions between Ruby and Pascal data types. Pascal objects convert to Ruby
  objects with autodefinitions of published properties. And other properties and
  methods may defined by hands.
* Other units provide definitions some public members of common classes. This
  part of package will be extend from version to version.

Also this repo contains the demo project for demonstrate the basic features.
The subdir `demo/rb` contains some ruby-scripts usable for it.

### Compatibility

The package was testing in FPC 2.7.1 and Lazarus 1.3 with Ruby 1.9.3 and 2.0.
Ruby 1.8 is unsupported.

It's not fully tested but must work in Windows, Linux and Mac OS X under
32- and 64-bit architecture.

## RoadMap

* Define methods for most standard classes:
  * unit `Forms`;
  * unit `Controls`;
  * unit `Graphics`;
  * unit `Dialogs`;
  * And other...

* Writing documentation.
* Ruby 2.1 compatibility.





