# The ppRuby package

The ppRuby package provides access to Ruby API for Free Pascal. 
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


