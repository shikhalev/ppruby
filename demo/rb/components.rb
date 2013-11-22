
extend Pascal::Forms

form = application.frmMain

form.each do |component|
  if component.respond_to? :caption
    puts "#{component.name} => #{component.caption.inspect}"
  else
    puts "#{component.name}"
  end
end

