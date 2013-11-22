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
{$else}
 {$undef RUBY19}
 {$undef RUBY20}
 {$undef RUBY21}
 {$undef RUBY2}
{$endif}

{$if defined(RUBY_STATIC) and not defined(RUBY_LIB)}

 {$if defined(DARWIN)}

  {$if defined(RUBY19)}
   {$define RUBY_LIB:='/System/Library/Frameworks/Ruby.framework/Versions/1.9/Ruby'}
  {$elseif defined(RUBY20)}
   {$define RUBY_LIB:='/System/Library/Frameworks/Ruby.framework/Versions/2.0/Ruby'}
  {$endif}

 {$elseif defined(GENTOO)}

  {$if defined(RUBY19)}
   {$define RUBY_LIB:='libruby19.so'}
  {$elseif defined(RUBY20)}
   {$define RUBY_LIB:='libruby20.so'}
  {$endif}

 {$elseif defined(UNIX)}

  {$if defined(RUBY19)}
   {$define RUBY_LIB:='libruby-1.9.1.so.1.9.1'}
  {$elseif defined(RUBY20)}
   {$define RUBY_LIB:='libruby-2.0.so.2.0'}
  {$endif}

 {$elseif defined(WINDOWS)}

  {$if defined(RUBY19)}
   {$define RUBY_LIB:='msvcrt-ruby191.dll'}
  {$elseif defined(RUBY20)}
   {$define RUBY_LIB:='msvcrt-ruby200.dll'}
  {$endif}

 {$endif}

{$endif}

