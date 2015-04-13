/*
 * Copyright (c) 2002, 2011, Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_VM_RUNTIME_JAVAFRAMEANCHOR_HPP
#define SHARE_VM_RUNTIME_JAVAFRAMEANCHOR_HPP

#include "utilities/globalDefinitions.hpp"
#ifdef TARGET_OS_ARCH_linux_x86
# include "orderAccess_linux_x86.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_linux_sparc
# include "orderAccess_linux_sparc.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_linux_zero
# include "orderAccess_linux_zero.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_solaris_x86
# include "orderAccess_solaris_x86.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_solaris_sparc
# include "orderAccess_solaris_sparc.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_windows_x86
# include "orderAccess_windows_x86.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_linux_arm
# include "orderAccess_linux_arm.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_linux_ppc
# include "orderAccess_linux_ppc.inline.hpp"
#endif
//
// An object for encapsulating the machine/os dependent part of a JavaThread frame state
//
class JavaThread;

class JavaFrameAnchor VALUE_OBJ_CLASS_SPEC {
// Too many friends...
friend class CallNativeDirectNode;
friend class OptoRuntime;
friend class Runtime1;
friend class StubAssembler;
friend class CallRuntimeDirectNode;
friend class MacroAssembler;
friend class InterpreterGenerator;
friend class LIR_Assembler;
friend class GraphKit;
friend class StubGenerator;
friend class JavaThread;
friend class frame;
friend class VMStructs;
friend class BytecodeInterpreter;
friend class JavaCallWrapper;

 private:
  //
  // Whenever _last_Java_sp != NULL other anchor fields MUST be valid!
  // The stack may not be walkable [check with walkable() ] but the values must be valid.
  // The profiler apparently depends on this.
  //
  intptr_t* volatile _last_Java_sp;

  // Whenever we call from Java to native we can not be assured that the return
  // address that composes the last_Java_frame will be in an accessible location
  // so calls from Java to native store that pc (or one good enough to locate
  // the oopmap) in the frame anchor. Since the frames that call from Java to
  // native are never deoptimized we never need to patch the pc and so this
  // is acceptable.
  volatile  address _last_Java_pc;

  // tells whether the last Java frame is set
  // It is important that when last_Java_sp != NULL that the rest of the frame
  // anchor (including platform specific) all be valid.

  bool has_last_Java_frame() const                   { return _last_Java_sp != NULL; }
  // This is very dangerous unless sp == NULL
  // Invalidate the anchor so that has_last_frame is false
  // and no one should look at the other fields.
  void zap(void)                                     { _last_Java_sp = NULL; }

#ifdef TARGET_ARCH_x86
//# include "javaFrameAnchor_x86.hpp"
private:

  // FP value associated with _last_Java_sp:
  intptr_t* volatile        _last_Java_fp;           // pointer is volatile not what it points to

public:
  // Each arch must define reset, save, restore
  // These are used by objects that only care about:
  //  1 - initializing a new state (thread creation, javaCalls)
  //  2 - saving a current state (javaCalls)
  //  3 - restoring an old state (javaCalls)

  void clear(void) {
    // clearing _last_Java_sp must be first
    _last_Java_sp = NULL;
    // fence?
    _last_Java_fp = NULL;
    _last_Java_pc = NULL;
  }

  void copy(JavaFrameAnchor* src) {
    // In order to make sure the transition state is valid for "this"
    // We must clear _last_Java_sp before copying the rest of the new data
    //
    // Hack Alert: Temporary bugfix for 4717480/4721647
    // To act like previous version (pd_cache_state) don't NULL _last_Java_sp
    // unless the value is changing
    //
    if (_last_Java_sp != src->_last_Java_sp)
      _last_Java_sp = NULL;

    _last_Java_fp = src->_last_Java_fp;
    _last_Java_pc = src->_last_Java_pc;
    // Must be last so profiler will always see valid frame if has_last_frame() is true
    _last_Java_sp = src->_last_Java_sp;
  }

  // Always walkable
  bool walkable(void) { return true; }
  // Never any thing to do since we are always walkable and can find address of return addresses
  void make_walkable(JavaThread* thread) { }

  intptr_t* last_Java_sp(void) const             { return _last_Java_sp; }

  address last_Java_pc(void)                     { return _last_Java_pc; }

private:

  static ByteSize last_Java_fp_offset()          { return byte_offset_of(JavaFrameAnchor, _last_Java_fp); }

public:

  void set_last_Java_sp(intptr_t* sp)            { _last_Java_sp = sp; }

  intptr_t*   last_Java_fp(void)                     { return _last_Java_fp; }
  // Assert (last_Java_sp == NULL || fp == NULL)
  void set_last_Java_fp(intptr_t* fp)                { _last_Java_fp = fp; }

#endif
#ifdef TARGET_ARCH_sparc
# include "javaFrameAnchor_sparc.hpp"
#endif
#ifdef TARGET_ARCH_zero
# include "javaFrameAnchor_zero.hpp"
#endif
#ifdef TARGET_ARCH_arm
# include "javaFrameAnchor_arm.hpp"
#endif
#ifdef TARGET_ARCH_ppc
# include "javaFrameAnchor_ppc.hpp"
#endif


public:
  JavaFrameAnchor()                              { clear(); }
  JavaFrameAnchor(JavaFrameAnchor *src)          { copy(src); }

  void set_last_Java_pc(address pc)              { _last_Java_pc = pc; }

  // Assembly stub generation helpers

  static ByteSize last_Java_sp_offset()          { return byte_offset_of(JavaFrameAnchor, _last_Java_sp); }
  static ByteSize last_Java_pc_offset()          { return byte_offset_of(JavaFrameAnchor, _last_Java_pc); }

};

#endif // SHARE_VM_RUNTIME_JAVAFRAMEANCHOR_HPP
