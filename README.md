
# ppRuby

Ruby bindings for Free Pascal / Lazarus.

* Author: [Ivan Shikhalev](https://github.com/shikhalev)
* License: [GNU GPL](http://www.gnu.org/copyleft/gpl.html)
* [Project](https://github.com/shikhalev/ppruby) @ [GitHub.com](https://github.com/)

## Simple usage

### At FPC side

Create a binding object.

````Delphi
var
  rb : TRuby;

. . . . .
rb := TRuby.Auto([TRuby20, TRuby19, TRuby18]);
// or
rb := TRuby<ver>.Create(libFile);

rb[<global>] := rb.Obj2Val(Application); // or any other object
rb.EvalString(<any ruby code>);
````

### At Ruby side

You have the module <code>Pascal</code> with nested submodules for Pascal units
and classes in their for Pascal classes.

You have access for published properties of objects and some public properties
and methods. Also subcomponents may be accessed by hash-like syntax with
<code>Symbol</code> names.

## More complex usage

Documentation and examples coming soon.
