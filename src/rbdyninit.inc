{$if defined(DARWIN)}
pp_reg_ruby_version([1,9], Rb19,
               ['/System/Library/Frameworks/Ruby.framework/Versions/1.9/Ruby']);
pp_reg_ruby_version([2,0], Rb20,
               ['/System/Library/Frameworks/Ruby.framework/Versions/2.0/Ruby']);
{$elseif defined(UNIX)}
pp_reg_ruby_version([1,9], Rb19, ['libruby19.so', 'libruby-1.9.1.so.1.9.1']);
pp_reg_ruby_version([2,0], Rb20, ['libruby20.so', 'libruby-2.0.so.2.0']);
{$elseif defined(WINDOWS)}
pp_reg_ruby_version([1,9], Rb19, ['msvcrt-ruby191.dll']);
pp_reg_ruby_version([2,0], Rb20, ['msvcrt-ruby200.dll']);
{$endif}
