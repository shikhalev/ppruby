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

## Preamble

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

### Conditional defines

* `RUBY_STATIC` — static mode. Use early binding in compile-time.
  In this mode also the next defines have meaning:
  * `RUBY19`, `RUBY20` and (in a nearest future) `RUBY21` — Ruby
    library version;
  * `RUBY_LIB` — Ruby library name macro. This is setted automatocally
    in dependence of version above and OS defines (`DARWIN`, `UNIX` 
    except `DARWIN`, `WINDOWS`).

    Autoname for `UNIX` correspond to Debian distribution, but it 
    may be changed to Gentoo one by define `GENTOO`. If you use any
    other distro with another Ruby library name, tell me please.
    
  The correct way to change defines above — in `rbdefs.inc` or 
  in package settings.
  
* `RUBY_DYNAMIC` — dynamic mode. Use late binding at run-time.
  Library name and version will selected as described below in
  unit `ruby.pas` description. In this mode some API constants
  are the variables because they have different values in different
  Ruby versions...

It is possible to use the package without any differences between
static and dynamic modes, Ruby version and OS. Don't add unneeded
dependencies in your utils, all diffs must be in compilation and
initialization stages.

## Usage

### Writing extensions

The creation of Ruby binary extensions was not a main task of
the package. But it is possible throught API functions. For
API description see 
[the Ruby docs](http://rubydoc.info/stdlib/core/file/README.EXT).
Specific things for FPC and `ppRuby` are follow:

* Extensions must be compiled in static mode because in this 
  case loading is managed by Ruby side.
* The conversion utilites from `RbTools` and `RbObjects` are
  applicable, but it is necessary to keep in mind than life-time
  of objects also managed by Ruby.
* It is very impotant to provide an absence of intersection
  Ruby and Pascal exceptions handling (see below more detailed
  conversation about exceptions).

### Embedded scripting

This package is designed for maximal simplification of addition
Ruby as scripting engine to Lazarus projects. It is enough to
set a global variable with some object and than call the script
evaluation. As example, to enumerate components of a form we can:

```Pascal
uses
  ..., RbTools, RbObjects, RbClasses;

...
rb_gv_set('form', Obj2Val(frmMain));
pp_eval(script);
```
where `script` contains something like:

```Ruby
$form.each do |cmp|
  if cmp.respond_to? :caption
    cmp.caption = cmp.name
  end
end
```

When we use the `RbForms` unit, we have the “common access point” —
`Pascal::Forms.application`. Enumeration of forms can be this:

```Ruby
# the ‘application’ method is defined as ‘module_function’
# we can mixin the module for reduce writing
extend Pascal::Forms

application.each do |form|
 form.caption = "Form: #{form.name}"
end
```

Also the output handling is possible throught setting the global
variable `$stdout` (and, may be, `$stderr`) with special value
converted from interface `IOutput` defined in `RbObject`. The 
`TRubyConnection` implements this interface and redirect output
in selected component. With same substitution we can use in scripts
usual `puts`, `p` or `printf` methods.





    

