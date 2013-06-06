
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
rb[<global var>] := rb.Obj2Val(Application); // or any other object
rb.EvalString(<any ruby code>);
````

### At Ruby side

You have the module |Pascal| with nested submodules for Pascal units and classes
in their for Pascal classes.

