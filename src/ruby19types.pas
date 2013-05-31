unit Ruby19Types;

{$mode objfpc}{$H+}
{$packrecords C}

interface

uses
  ctypes, RubyTypes;

type
  rb_vm_struct = record
    // TODO
  end;
  rb_vm_t = rb_vm_struct;

  rb_control_frame_t = record
    // TODO
  end;

  rb_block_struct = record
    // TODO
  end;
  rb_block_t = rb_block_struct;

  rb_method_entry_struct = record
    // TODO
  end;
  rb_method_entry_t = rb_method_entry_struct;

  rb_thread_struct = record
    _self : VALUE;
    vm : ^rb_vm_t;
    stack : ^VALUE;
    stack_size : culong;
    cfp : ^rb_control_frame_t;
    safe_level : cint;
    raised_flag : cint;
    last_status : VALUE;
    state : cint;
    waiting_fd : cint;
    passed_block : ^rb_block_t;
    passed_me : ^rb_method_entry_t;
    top_self : VALUE;
    top_wrapper : VALUE;
    base_block : ^rb_block_t;
    local_lfp : ^VALUE;
    local_svar : VALUE;
    // TODO
  end;

implementation

end.

