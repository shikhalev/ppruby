# The ppRuby package

<img src="https://raw.github.com/shikhalev/ppruby/master/img/logo.png" align="right">

The `ppRuby` package provides access to Ruby API for Free Pascal. 
It is possible to use for two main tasks:

* Writing binary extensions for Ruby;
* Embedding Ruby as scriptiong engine to FPC/Lazarus applications.

In first case it's needed the `ruby.pas` compiled with static mode
(see below) only. In second case it's recommended to use other 
units from package which provide some utils for ease and object
conversion technic with automatic translation of `published` 
properties.

## Intro

### Files

* The directory `demo` contains the small demo-project.
  * subdir `rb` — scripts (`rb/*.rb`) for executing in demo-app.
    This subdir will be expanded in future.
  * files `aboutform.{lfm,pas}`, `demo.{ico,lpi,lpr}`, 
    `mainform.{lfm,pas}` — the source files of demo-app.
* The directory `doc` and `doc/*.md` files — this documentation.
* The directory `img` and `img/logo.*` files — the logo of project.
* The directory `src` — the source of package.
  * `rbopts.inc` — include file with compiler directives. It is
    included in all units of package.
  * `rbdefs.inc` — include file with conditional defines.
  * `ruby.pas` — Ruby API declarations and some small routines
    needed for dynamic library loading. Some parts is exctracted 
    in separate files:
    * `rbtypes.inc` — API types;
    * `rbdynintf.inc` — dynamic API interface;
    * `rbstatintf.inc` — static API interface;
    * `rbmacrointf.inc` — interface for inline-functions;
    * `rbdynimpl.inc` — implementation for dynamic loading;
    * `rbmacroimpl.inc` — implementation of inline-functions;
    * `rbdyninit.inc` — initialization of dynamic loading subsystem.
  * `rbtools.pas` — base utilities unit.
  * `rbobjects.pas` — objects conversion routines.
  * `rbclasses.pas`, `rbdialogs.pas`, `rbforms.pas` — define public
    methods for standard classes (uncomplete).
  * `rubyconnection.pas` — contains non-visual LCL-component 
    `TRubyConnection` for manage the connection to Ruby API 
    in design-time.
    * `rubyconnection_icon.lrs` — icon for `TRubyConnection`.
  * `pprubydsgn.pas` — registration components in IDE.
  * `ppruby.lpk` and `ppruby.pas` — package source.

