{$if defined(RUBY_DYNAMIC)}
 {$undef RUBY_STATIC}
{$elseif not defined(RUBY_STATIC)}
 {$define RUBY_DYNAMIC}
{$endif}

{$if defined(RUBY_STATIC)}
 {$if defined(RUBY20) or defined(RUBY21)}
  {$define RUBY2}
  {$undef RUBY19}
 {$elseif defined(RUBY2)}
  {$if not defined(RUBY21)}
   {$define RUBY20}
  {$endif}
  {$undef RUBY19}
 {$else}
  {$define RUBY19}
 {$endif}
 {$if not defined(RUBY_LIB)}
  {$if defined(RUBY20)}
   {$define RUBY_LIB:='libruby20.so'}
  {$elseif defined(RUBY19)}
   {$define RUBY_LIB:='libruby19.so'}
  {$endif}
 {$endif}
{$else}
 {$undef RUBY19}
 {$undef RUBY20}
 {$undef RUBY21}
 {$undef RUBY2}
{$endif}

