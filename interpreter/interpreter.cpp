/*
 * Copyright (c) 1997, 2011, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/bytecodeInterpreter.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateTable.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/arrayOop.hpp"
#include "oops/methodDataOop.hpp"
#include "oops/methodOop.hpp"
#include "oops/oop.inline.hpp"
#include "prims/forte.hpp"
#include "prims/jvmtiExport.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/timer.hpp"

# define __ _masm->


//------------------------------------------------------------------------------------------------------------------------
// Implementation of InterpreterCodelet

void InterpreterCodelet::initialize(const char* description, Bytecodes::Code bytecode) {
  _description       = description;
  _bytecode          = bytecode;
}


void InterpreterCodelet::verify() {
}


void InterpreterCodelet::print_on(outputStream* st) const {
  if (PrintInterpreter) {
    st->cr();
    st->print_cr("----------------------------------------------------------------------");
  }

  if (description() != NULL) st->print("%s  ", description());
  if (bytecode()    >= 0   ) st->print("%d %s  ", bytecode(), Bytecodes::name(bytecode()));
  st->print_cr("[" INTPTR_FORMAT ", " INTPTR_FORMAT "]  %d bytes",
                code_begin(), code_end(), code_size());

  if (PrintInterpreter) {
    st->cr();
    Disassembler::decode(code_begin(), code_end(), st);
  }
}


//------------------------------------------------------------------------------------------------------------------------
// Implementation of  platform independent aspects of Interpreter

void AbstractInterpreter::initialize() {
  if (_code != NULL) return;

  // make sure 'imported' classes are initialized
  if (CountBytecodes || TraceBytecodes || StopInterpreterAt) BytecodeCounter::reset();
  if (PrintBytecodeHistogram)                                BytecodeHistogram::reset();
  if (PrintBytecodePairHistogram)                            BytecodePairHistogram::reset();

  InvocationCounter::reinitialize(DelayCompilationDuringStartup);

}

void AbstractInterpreter::print() {
  tty->cr();
  tty->print_cr("----------------------------------------------------------------------");
  tty->print_cr("Interpreter");
  tty->cr();
  tty->print_cr("code size        = %6dK bytes", (int)_code->used_space()/1024);
  tty->print_cr("total space      = %6dK bytes", (int)_code->total_space()/1024);
  tty->print_cr("wasted space     = %6dK bytes", (int)_code->available_space()/1024);
  tty->cr();
  tty->print_cr("# of codelets    = %6d"      , _code->number_of_stubs());
  tty->print_cr("avg codelet size = %6d bytes", _code->used_space() / _code->number_of_stubs());
  tty->cr();
  _code->print();
  tty->print_cr("----------------------------------------------------------------------");
  tty->cr();
}


void interpreter_init() {
  Interpreter::initialize();
#ifndef PRODUCT
  if (TraceBytecodes) BytecodeTracer::set_closure(BytecodeTracer::std_closure());
#endif // PRODUCT
  // need to hit every safepoint in order to call zapping routine
  // register the interpreter
  Forte::register_stub(
    "Interpreter",
    AbstractInterpreter::code()->code_start(),
    AbstractInterpreter::code()->code_end()
  );

  // notify JVMTI profiler
  if (JvmtiExport::should_post_dynamic_code_generated()) {
    JvmtiExport::post_dynamic_code_generated("Interpreter",
                                             AbstractInterpreter::code()->code_start(),
                                             AbstractInterpreter::code()->code_end());
  }
}

//------------------------------------------------------------------------------------------------------------------------
// Implementation of interpreter

StubQueue* AbstractInterpreter::_code                                       = NULL;
bool       AbstractInterpreter::_notice_safepoints                          = false;
address    AbstractInterpreter::_rethrow_exception_entry                    = NULL;

address    AbstractInterpreter::_native_entry_begin                         = NULL;
address    AbstractInterpreter::_native_entry_end                           = NULL;
address    AbstractInterpreter::_slow_signature_handler;
address    AbstractInterpreter::_entry_table            [AbstractInterpreter::number_of_method_entries];
address    AbstractInterpreter::_native_abi_to_tosca    [AbstractInterpreter::number_of_result_handlers];

//------------------------------------------------------------------------------------------------------------------------
// Generation of complete interpreter

AbstractInterpreterGenerator::AbstractInterpreterGenerator(StubQueue* _code) {
  _masm                      = NULL;
}


static const BasicType types[Interpreter::number_of_result_handlers] = {
  T_BOOLEAN,
  T_CHAR   ,
  T_BYTE   ,
  T_SHORT  ,
  T_INT    ,
  T_LONG   ,
  T_VOID   ,
  T_FLOAT  ,
  T_DOUBLE ,
  T_OBJECT
};

void AbstractInterpreterGenerator::generate_all() {


  { CodeletMark cm(_masm, "slow signature handler");
    Interpreter::_slow_signature_handler = generate_slow_signature_handler();
  }

}

//------------------------------------------------------------------------------------------------------------------------
// Entry points

AbstractInterpreter::MethodKind AbstractInterpreter::method_kind(methodHandle m) {
  // Abstract method?
  if (m->is_abstract()) return abstract;

  // Invoker for method handles?
  if (m->is_method_handle_invoke())  return method_handle;

  // Native method?
  // Note: This test must come _before_ the test for intrinsic
  //       methods. See also comments below.
  if (m->is_native()) {
    assert(!m->is_method_handle_invoke(), "overlapping bits here, watch out");
    return m->is_synchronized() ? native_synchronized : native;
  }

  // Synchronized?
  if (m->is_synchronized()) {
    return zerolocals_synchronized;
  }

  if (RegisterFinalizersAtInit && m->code_size() == 1 &&
      m->intrinsic_id() == vmIntrinsics::_Object_init) {
    // We need to execute the special return bytecode to check for
    // finalizer registration so create a normal frame.
    return zerolocals;
  }

  // Empty method?
  if (m->is_empty_method()) {
    return empty;
  }

  // Special intrinsic method?
  // Note: This test must come _after_ the test for native methods,
  //       otherwise we will run into problems with JDK 1.2, see also
  //       AbstractInterpreterGenerator::generate_method_entry() for
  //       for details.
  switch (m->intrinsic_id()) {
    case vmIntrinsics::_dsin  : return java_lang_math_sin  ;
    case vmIntrinsics::_dcos  : return java_lang_math_cos  ;
    case vmIntrinsics::_dtan  : return java_lang_math_tan  ;
    case vmIntrinsics::_dabs  : return java_lang_math_abs  ;
    case vmIntrinsics::_dsqrt : return java_lang_math_sqrt ;
    case vmIntrinsics::_dlog  : return java_lang_math_log  ;
    case vmIntrinsics::_dlog10: return java_lang_math_log10;

    case vmIntrinsics::_Reference_get:
                                return java_lang_ref_reference_get;
  }

  // Accessor method?
  if (m->is_accessor()) {
    assert(m->size_of_parameters() == 1, "fast code for accessors assumes parameter size = 1");
    return accessor;
  }

  // Note: for now: zero locals for all non-empty methods
  return zerolocals;
}


// Return true if the interpreter can prove that the given bytecode has
// not yet been executed (in Java semantics, not in actual operation).
bool AbstractInterpreter::is_not_reached(methodHandle method, int bci) {
  Bytecodes::Code code = method()->code_at(bci);

  if (!Bytecodes::must_rewrite(code)) {
    // might have been reached
    return false;
  }

  // the bytecode might not be rewritten if the method is an accessor, etc.
  address ientry = method->interpreter_entry();
  if (ientry != entry_for_kind(AbstractInterpreter::zerolocals) &&
      ientry != entry_for_kind(AbstractInterpreter::zerolocals_synchronized))
    return false;  // interpreter does not run this method!

  // otherwise, we can be sure this bytecode has never been executed
  return true;
}


#ifndef PRODUCT
void AbstractInterpreter::print_method_kind(MethodKind kind) {
  switch (kind) {
    case zerolocals             : tty->print("zerolocals"             ); break;
    case zerolocals_synchronized: tty->print("zerolocals_synchronized"); break;
    case native                 : tty->print("native"                 ); break;
    case native_synchronized    : tty->print("native_synchronized"    ); break;
    case empty                  : tty->print("empty"                  ); break;
    case accessor               : tty->print("accessor"               ); break;
    case abstract               : tty->print("abstract"               ); break;
    case method_handle          : tty->print("method_handle"          ); break;
    case java_lang_math_sin     : tty->print("java_lang_math_sin"     ); break;
    case java_lang_math_cos     : tty->print("java_lang_math_cos"     ); break;
    case java_lang_math_tan     : tty->print("java_lang_math_tan"     ); break;
    case java_lang_math_abs     : tty->print("java_lang_math_abs"     ); break;
    case java_lang_math_sqrt    : tty->print("java_lang_math_sqrt"    ); break;
    case java_lang_math_log     : tty->print("java_lang_math_log"     ); break;
    case java_lang_math_log10   : tty->print("java_lang_math_log10"   ); break;
    default                     : ShouldNotReachHere();
  }
}
#endif // PRODUCT


//------------------------------------------------------------------------------------------------------------------------
// Deoptimization support

// If deoptimization happens, this function returns the point of next bytecode to continue execution
address AbstractInterpreter::deopt_continue_after_entry(methodOop method, address bcp, int callee_parameters, bool is_top_frame) {
  assert(method->contains(bcp), "just checkin'");
  Bytecodes::Code code   = Bytecodes::java_code_at(method, bcp);
  assert(!Interpreter::bytecode_should_reexecute(code), "should not reexecute");
  int             bci    = method->bci_from(bcp);
  int             length = -1; // initial value for debugging
  // compute continuation length
  length = Bytecodes::length_at(method, bcp);
  // compute result type
  BasicType type = T_ILLEGAL;

  switch (code) {
    case Bytecodes::_invokevirtual  :
    case Bytecodes::_invokespecial  :
    case Bytecodes::_invokestatic   :
    case Bytecodes::_invokeinterface: {
      Thread *thread = Thread::current();
      ResourceMark rm(thread);
      methodHandle mh(thread, method);
      type = Bytecode_invoke(mh, bci).result_type();
      // since the cache entry might not be initialized:
      // (NOT needed for the old calling convension)
      if (!is_top_frame) {
        int index = Bytes::get_native_u2(bcp+1);
        method->constants()->cache()->entry_at(index)->set_parameter_size(callee_parameters);
      }
      break;
    }

   case Bytecodes::_invokedynamic: {
      Thread *thread = Thread::current();
      ResourceMark rm(thread);
      methodHandle mh(thread, method);
      type = Bytecode_invoke(mh, bci).result_type();
      // since the cache entry might not be initialized:
      // (NOT needed for the old calling convension)
      if (!is_top_frame) {
        int index = Bytes::get_native_u4(bcp+1);
        method->constants()->cache()->secondary_entry_at(index)->set_parameter_size(callee_parameters);
      }
      break;
    }

    case Bytecodes::_ldc   :
    case Bytecodes::_ldc_w : // fall through
    case Bytecodes::_ldc2_w:
      {
        Thread *thread = Thread::current();
        ResourceMark rm(thread);
        methodHandle mh(thread, method);
        type = Bytecode_loadconstant(mh, bci).result_type();
        break;
      }

    default:
      type = Bytecodes::result_type(code);
      break;
  }

  // return entry point for computed continuation state & bytecode length
  return
    is_top_frame
    ? Interpreter::deopt_entry (as_TosState(type), length)
    : Interpreter::return_entry(as_TosState(type), length);
}

// If deoptimization happens, this function returns the point where the interpreter reexecutes
// the bytecode.
// Note: Bytecodes::_athrow is a special case in that it does not return
//       Interpreter::deopt_entry(vtos, 0) like others
address AbstractInterpreter::deopt_reexecute_entry(methodOop method, address bcp) {
  assert(method->contains(bcp), "just checkin'");
  Bytecodes::Code code   = Bytecodes::java_code_at(method, bcp);
#ifdef COMPILER1
  if(code == Bytecodes::_athrow ) {
    return Interpreter::rethrow_exception_entry();
  }
#endif /* COMPILER1 */
  return Interpreter::deopt_entry(vtos, 0);
}

// If deoptimization happens, the interpreter should reexecute these bytecodes.
// This function mainly helps the compilers to set up the reexecute bit.
bool AbstractInterpreter::bytecode_should_reexecute(Bytecodes::Code code) {
  switch (code) {
    case Bytecodes::_lookupswitch:
    case Bytecodes::_tableswitch:
    case Bytecodes::_fast_binaryswitch:
    case Bytecodes::_fast_linearswitch:
    // recompute condtional expression folded into _if<cond>
    case Bytecodes::_lcmp      :
    case Bytecodes::_fcmpl     :
    case Bytecodes::_fcmpg     :
    case Bytecodes::_dcmpl     :
    case Bytecodes::_dcmpg     :
    case Bytecodes::_ifnull    :
    case Bytecodes::_ifnonnull :
    case Bytecodes::_goto      :
    case Bytecodes::_goto_w    :
    case Bytecodes::_ifeq      :
    case Bytecodes::_ifne      :
    case Bytecodes::_iflt      :
    case Bytecodes::_ifge      :
    case Bytecodes::_ifgt      :
    case Bytecodes::_ifle      :
    case Bytecodes::_if_icmpeq :
    case Bytecodes::_if_icmpne :
    case Bytecodes::_if_icmplt :
    case Bytecodes::_if_icmpge :
    case Bytecodes::_if_icmpgt :
    case Bytecodes::_if_icmple :
    case Bytecodes::_if_acmpeq :
    case Bytecodes::_if_acmpne :
    // special cases
    case Bytecodes::_getfield  :
    case Bytecodes::_putfield  :
    case Bytecodes::_getstatic :
    case Bytecodes::_putstatic :
    case Bytecodes::_aastore   :
#ifdef COMPILER1
    //special case of reexecution
    case Bytecodes::_athrow    :
#endif
      return true;

    default:
      return false;
  }
}

void AbstractInterpreterGenerator::bang_stack_shadow_pages(bool native_call) {
  // Quick & dirty stack overflow checking: bang the stack & handle trap.
  // Note that we do the banging after the frame is setup, since the exception
  // handling code expects to find a valid interpreter frame on the stack.
  // Doing the banging earlier fails if the caller frame is not an interpreter
  // frame.
  // (Also, the exception throwing code expects to unlock any synchronized
  // method receiever, so do the banging after locking the receiver.)

  // Bang each page in the shadow zone. We can't assume it's been done for
  // an interpreter frame with greater than a page of locals, so each page
  // needs to be checked.  Only true for non-native.
  if (UseStackBanging) {
    const int start_page = native_call ? StackShadowPages : 1;
    const int page_size = os::vm_page_size();
    for (int pages = start_page; pages <= StackShadowPages ; pages++) {
      __ bang_stack_with_offset(pages*page_size);
    }
  }
}

#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterGenerator.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateTable.hpp"
#include "oops/arrayOop.hpp"
#include "oops/methodDataOop.hpp"
#include "oops/methodOop.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "prims/jvmtiThreadState.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/arguments.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/timer.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/debug.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif

#define __ _masm->

//------------------------------------------------------------------------------------------------------------------------

address AbstractInterpreterGenerator::generate_slow_signature_handler() {
  address entry = __ pc();
  // rbx,: method
  // rcx: temporary
  // rdi: pointer to locals
  // rsp: end of copied parameters area
  __ mov(rcx, rsp);
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::slow_signature_handler), rbx, rdi, rcx);
  __ ret(0);
  return entry;
}


//
// Various method entries (that c++ and asm interpreter agree upon)
//------------------------------------------------------------------------------------------------------------------------
//
//

// Empty method, generate a very fast return.

address InterpreterGenerator::generate_empty_entry(void) {

  // rbx,: methodOop
  // rcx: receiver (unused)
  // rsi: previous interpreter state (C++ interpreter) must preserve
  // rsi: sender sp must set sp to this value on return

  if (!UseFastEmptyMethods) return NULL;

  address entry_point = __ pc();

  // If we need a safepoint check, generate full interpreter entry.
  Label slow_path;
  ExternalAddress state(SafepointSynchronize::address_of_state());
  __ cmp32(ExternalAddress(SafepointSynchronize::address_of_state()),
           SafepointSynchronize::_not_synchronized);
  __ jcc(Assembler::notEqual, slow_path);

  // do nothing for empty methods (do not even increment invocation counter)
  // Code: _return
  // _return
  // return w/o popping parameters
  __ pop(rax);
  __ mov(rsp, rsi);
  __ jmp(rax);

  __ bind(slow_path);
  (void) generate_normal_entry(false);
  return entry_point;
}

address InterpreterGenerator::generate_math_entry(AbstractInterpreter::MethodKind kind) {

  // rbx,: methodOop
  // rcx: scratrch
  // rsi: sender sp

  if (!InlineIntrinsics) return NULL; // Generate a vanilla entry

  address entry_point = __ pc();

  // These don't need a safepoint check because they aren't virtually
  // callable. We won't enter these intrinsics from compiled code.
  // If in the future we added an intrinsic which was virtually callable
  // we'd have to worry about how to safepoint so that this code is used.

  // mathematical functions inlined by compiler
  // (interpreter must provide identical implementation
  // in order to avoid monotonicity bugs when switching
  // from interpreter to compiler in the middle of some
  // computation)
  //
  // stack: [ ret adr ] <-- rsp
  //        [ lo(arg) ]
  //        [ hi(arg) ]
  //

  // Note: For JDK 1.2 StrictMath doesn't exist and Math.sin/cos/sqrt are
  //       native methods. Interpreter::method_kind(...) does a check for
  //       native methods first before checking for intrinsic methods and
  //       thus will never select this entry point. Make sure it is not
  //       called accidentally since the SharedRuntime entry points will
  //       not work for JDK 1.2.
  //
  // We no longer need to check for JDK 1.2 since it's EOL'ed.
  // The following check existed in pre 1.6 implementation,
  //    if (Universe::is_jdk12x_version()) {
  //      __ should_not_reach_here();
  //    }
  // Universe::is_jdk12x_version() always returns false since
  // the JDK version is not yet determined when this method is called.
  // This method is called during interpreter_init() whereas
  // JDK version is only determined when universe2_init() is called.

  // Note: For JDK 1.3 StrictMath exists and Math.sin/cos/sqrt are
  //       java methods.  Interpreter::method_kind(...) will select
  //       this entry point for the corresponding methods in JDK 1.3.
  // get argument
  __ fld_d(Address(rsp, 1*wordSize));
  switch (kind) {
    case Interpreter::java_lang_math_sin :
        __ trigfunc('s');
        break;
    case Interpreter::java_lang_math_cos :
        __ trigfunc('c');
        break;
    case Interpreter::java_lang_math_tan :
        __ trigfunc('t');
        break;
    case Interpreter::java_lang_math_sqrt:
        __ fsqrt();
        break;
    case Interpreter::java_lang_math_abs:
        __ fabs();
        break;
    case Interpreter::java_lang_math_log:
        __ flog();
        // Store to stack to convert 80bit precision back to 64bits
        __ push_fTOS();
        __ pop_fTOS();
        break;
    case Interpreter::java_lang_math_log10:
        __ flog10();
        // Store to stack to convert 80bit precision back to 64bits
        __ push_fTOS();
        __ pop_fTOS();
        break;
    default                              :
        ShouldNotReachHere();
  }

  // return double result in xmm0 for interpreter and compilers.
  if (UseSSE >= 2) {
    __ subptr(rsp, 2*wordSize);
    __ fstp_d(Address(rsp, 0));
    __ movdbl(xmm0, Address(rsp, 0));
    __ addptr(rsp, 2*wordSize);
  }

  // done, result in FPU ST(0) or XMM0
  __ pop(rdi);                               // get return address
  __ mov(rsp, rsi);                          // set sp to sender sp
  __ jmp(rdi);

  return entry_point;
}


// Abstract method entry
// Attempt to execute abstract method. Throw exception
address InterpreterGenerator::generate_abstract_entry(void) {

  // rbx,: methodOop
  // rcx: receiver (unused)
  // rsi: previous interpreter state (C++ interpreter) must preserve

  // rsi: sender SP

  address entry_point = __ pc();

  // abstract method entry

  //  pop return address, reset last_sp to NULL
  __ empty_expression_stack();
  __ restore_bcp();      // rsi must be correct for exception handler   (was destroyed)
  __ restore_locals();   // make sure locals pointer is correct as well (was destroyed)

  // throw exception
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_AbstractMethodError));
  // the call_VM checks for exception, so we should never return here.
  __ should_not_reach_here();

  return entry_point;
}


// Method handle invoker
// Dispatch a method of the form java.lang.invoke.MethodHandles::invoke(...)
address InterpreterGenerator::generate_method_handle_entry(void) {
  if (!EnableInvokeDynamic) {
    return generate_abstract_entry();
  }

  address entry_point = MethodHandles::generate_method_handle_interpreter_entry(_masm);

  return entry_point;
}

void Deoptimization::unwind_callee_save_values(frame* f, vframeArray* vframe_array) {

  // This code is sort of the equivalent of C2IAdapter::setup_stack_frame back in
  // the days we had adapter frames. When we deoptimize a situation where a
  // compiled caller calls a compiled caller will have registers it expects
  // to survive the call to the callee. If we deoptimize the callee the only
  // way we can restore these registers is to have the oldest interpreter
  // frame that we create restore these values. That is what this routine
  // will accomplish.

  // At the moment we have modified c2 to not have any callee save registers
  // so this problem does not exist and this routine is just a place holder.

  assert(f->is_interpreted_frame(), "must be interpreted");
}
