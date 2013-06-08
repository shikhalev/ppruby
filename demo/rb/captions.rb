application[:frmMain].each do |component|
  if component.respond_to? :caption
    puts "#{component.name} => \"#{component.caption}\""
  end
end
