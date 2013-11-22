
extend Pascal::Forms
extend Pascal::Dialogs

form = application.frmMain

dlg = form.dlgOpen

if dlg.execute
  showmessage File.read(dlg.filename)
end

