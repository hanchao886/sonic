//go:build arm64
// +build arm64

/*
 * Copyright 2022 ByteDance Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package abi

import (
	"fmt"
	"reflect"
	"unsafe"

	arm64 "github.com/bytedance/sonic/loader/internal/iasm/arm64"
)

// ARM64 Register definitions
// Based on ARM64 calling convention (AAPCS64)
type Register = arm64.Register
type Register64 = arm64.Register64
type VRegister = arm64.VRegister
type Program = arm64.Program
type MemoryOperand = arm64.MemoryOperand
type Label = arm64.Label

var (
	Ptr         = arm64.Ptr
	DefaultArch = arm64.DefaultArch
	CreateLabel = arm64.CreateLabel

	intType = reflect.TypeOf(0)
	ptrType = reflect.TypeOf(unsafe.Pointer(nil))
)

// ARM64 general purpose registers
const (
	X0  = arm64.X0
	X1  = arm64.X1
	X2  = arm64.X2
	X3  = arm64.X3
	X4  = arm64.X4
	X5  = arm64.X5
	X6  = arm64.X6
	X7  = arm64.X7
	X8  = arm64.X8
	X9  = arm64.X9
	X10 = arm64.X10
	X11 = arm64.X11
	X12 = arm64.X12
	X13 = arm64.X13
	X14 = arm64.X14
	X15 = arm64.X15
	X16 = arm64.X16
	X17 = arm64.X17
	X18 = arm64.X18
	X19 = arm64.X19
	X20 = arm64.X20
	X21 = arm64.X21
	X22 = arm64.X22
	X23 = arm64.X23
	X24 = arm64.X24
	X25 = arm64.X25
	X26 = arm64.X26
	X27 = arm64.X27
	X28 = arm64.X28
	X29 = arm64.X29
	X30 = arm64.X30
	SP  = arm64.SP
	XZR = arm64.XZR
)

// ARM64 NEON/FP registers
const (
	V0  = arm64.V0
	V1  = arm64.V1
	V2  = arm64.V2
	V3  = arm64.V3
	V4  = arm64.V4
	V5  = arm64.V5
	V6  = arm64.V6
	V7  = arm64.V7
	V8  = arm64.V8
	V9  = arm64.V9
	V10 = arm64.V10
	V11 = arm64.V11
	V12 = arm64.V12
	V13 = arm64.V13
	V14 = arm64.V14
	V15 = arm64.V15
	V16 = arm64.V16
	V17 = arm64.V17
	V18 = arm64.V18
	V19 = arm64.V19
	V20 = arm64.V20
	V21 = arm64.V21
	V22 = arm64.V22
	V23 = arm64.V23
	V24 = arm64.V24
	V25 = arm64.V25
	V26 = arm64.V26
	V27 = arm64.V27
	V28 = arm64.V28
	V29 = arm64.V29
	V30 = arm64.V30
	V31 = arm64.V31
)

// Aliases
const (
	FP = arm64.FP
	LR = arm64.LR
)

const (
	PtrSize  = 8 // pointer size on ARM64
	PtrAlign = 8 // pointer alignment on ARM64
)

// ARM64 C calling convention (AAPCS64)
// Arguments are passed in X0-X7 for integers/pointers
// V0-V7 for floating point
var iregOrderC = []Register64{
	X0, X1, X2, X3, X4, X5, X6, X7,
}

var vregOrderC = []VRegister{
	V0, V1, V2, V3, V4, V5, V6, V7,
}

// ARM64 Go calling convention (same as AAPCS64 for ARM64)
// Arguments are passed in X0-X7 for integers/pointers
// V0-V7 for floating point
var iregOrderGo = []Register64{
	X0, X1, X2, X3, X4, X5, X6, X7,
}

var vregOrderGo = []VRegister{
	V0, V1, V2, V3, V4, V5, V6, V7,
}

type stackAlloc struct {
	s uint32
	i int
	x int
}

func (self *stackAlloc) reset() {
	self.i, self.x = 0, 0
}

func (self *stackAlloc) ireg(vt reflect.Type) (p Parameter) {
	p = mkIReg(vt, iregOrderGo[self.i])
	self.i++
	return
}

func (self *stackAlloc) xreg(vt reflect.Type) (p Parameter) {
	p = mkVReg(vt, vregOrderGo[self.x])
	self.x++
	return
}

func (self *stackAlloc) stack(vt reflect.Type) (p Parameter) {
	p = mkStack(vt, self.s)
	self.s += uint32(vt.Size())
	return
}

func (self *stackAlloc) spill(n uint32, a int) uint32 {
	self.s = alignUp(self.s, a) + n
	return self.s
}

func (self *stackAlloc) alloc(p []Parameter, vt reflect.Type) []Parameter {
	nb := vt.Size()
	vk := vt.Kind()

	/* zero-sized objects are allocated on stack */
	if nb == 0 {
		return append(p, mkStack(intType, self.s))
	}

	/* check for value type */
	switch vk {
	case reflect.Bool:
		return self.valloc(p, reflect.TypeOf(false))
	case reflect.Int:
		return self.valloc(p, intType)
	case reflect.Int8:
		return self.valloc(p, reflect.TypeOf(int8(0)))
	case reflect.Int16:
		return self.valloc(p, reflect.TypeOf(int16(0)))
	case reflect.Int32:
		return self.valloc(p, reflect.TypeOf(int32(0)))
	case reflect.Int64:
		return self.valloc(p, reflect.TypeOf(int64(0)))
	case reflect.Uint:
		return self.valloc(p, reflect.TypeOf(uint(0)))
	case reflect.Uint8:
		return self.valloc(p, reflect.TypeOf(uint8(0)))
	case reflect.Uint16:
		return self.valloc(p, reflect.TypeOf(uint16(0)))
	case reflect.Uint32:
		return self.valloc(p, reflect.TypeOf(uint32(0)))
	case reflect.Uint64:
		return self.valloc(p, reflect.TypeOf(uint64(0)))
	case reflect.Uintptr:
		return self.valloc(p, reflect.TypeOf(uintptr(0)))
	case reflect.Float32:
		return self.valloc(p, reflect.TypeOf(float32(0)))
	case reflect.Float64:
		return self.valloc(p, reflect.TypeOf(float64(0)))
	case reflect.Complex64:
		panic("abi: arm64: not implemented: complex64")
	case reflect.Complex128:
		panic("abi: arm64: not implemented: complex128")
	case reflect.Array:
		panic("abi: arm64: not implemented: arrays")
	case reflect.Chan:
		return self.valloc(p, reflect.TypeOf((chan int)(nil)))
	case reflect.Func:
		return self.valloc(p, reflect.TypeOf((func())(nil)))
	case reflect.Map:
		return self.valloc(p, reflect.TypeOf((map[int]int)(nil)))
	case reflect.Ptr:
		return self.valloc(p, reflect.TypeOf((*int)(nil)))
	case reflect.UnsafePointer:
		return self.valloc(p, ptrType)
	case reflect.Interface:
		return self.valloc(p, ptrType, ptrType)
	case reflect.Slice:
		return self.valloc(p, ptrType, intType, intType)
	case reflect.String:
		return self.valloc(p, ptrType, intType)
	case reflect.Struct:
		panic("abi: arm64: not implemented: structs")
	default:
		panic("abi: invalid value type")
	}
}

func (self *stackAlloc) valloc(p []Parameter, vts ...reflect.Type) []Parameter {
	for _, vt := range vts {
		enum := isFloat(vt)
		if enum != notFloatKind && self.x < len(vregOrderGo) {
			p = append(p, self.xreg(vt))
		} else if enum == notFloatKind && self.i < len(iregOrderGo) {
			p = append(p, self.ireg(vt))
		} else {
			p = append(p, self.stack(vt))
		}
	}
	return p
}

func NewFunctionLayout(ft reflect.Type) FunctionLayout {
	var sa stackAlloc
	var fn FunctionLayout

	/* assign every arguments */
	for i := 0; i < ft.NumIn(); i++ {
		fn.Args = sa.alloc(fn.Args, ft.In(i))
	}

	/* reset the register counter, and add a pointer alignment field */
	sa.reset()

	/* assign every return value */
	for i := 0; i < ft.NumOut(); i++ {
		fn.Rets = sa.alloc(fn.Rets, ft.Out(i))
	}

	sa.spill(0, PtrAlign)

	/* assign spill slots */
	for i := 0; i < len(fn.Args); i++ {
		if fn.Args[i].InRegister {
			fn.Args[i].Mem = sa.spill(PtrSize, PtrAlign) - PtrSize
		}
	}

	/* add the final pointer alignment field */
	fn.FP = sa.spill(0, PtrAlign)
	return fn
}

func (self *Frame) argv(i int) *MemoryOperand {
	return Ptr(SP, int32(self.Prev()+self.desc.Args[i].Mem))
}

// spillv is used for growstack spill registers
func (self *Frame) spillv(i int) *MemoryOperand {
	// remain one slot for caller return pc
	return Ptr(SP, PtrSize+int32(self.desc.Args[i].Mem))
}

func (self *Frame) retv(i int) *MemoryOperand {
	return Ptr(SP, int32(self.Prev()+self.desc.Rets[i].Mem))
}

func (self *Frame) resv(i int) *MemoryOperand {
	return Ptr(SP, int32(self.Offs()-uint32((i+1)*PtrSize)))
}

func (self *Frame) emitGrowStack(p *Program, entry *Label) {
	// Spill all register arguments to their spill slots
	for i, v := range self.desc.Args {
		if v.InRegister {
			if v.IsFloat == floatKind64 {
				// STR Dreg, [SP, #offset]
				p.STR(arm64.DRegister(v.Reg.(VRegister)), self.spillv(i))
			} else if v.IsFloat == floatKind32 {
				// STR Sreg, [SP, #offset]
				p.STR(arm64.SRegister(v.Reg.(VRegister)), self.spillv(i))
			} else {
				// STR Xreg, [SP, #offset]
				p.STR(v.Reg.(Register64), self.spillv(i))
			}
		}
	}

	// Call runtime.morestack_noctxt
	// Load function address into X16 and call
	p.MOVZ(X16, uint16(F_morestack_noctxt&0xffff), 0)
	p.MOVK(X16, uint16((F_morestack_noctxt>>16)&0xffff), 16)
	p.MOVK(X16, uint16((F_morestack_noctxt>>32)&0xffff), 32)
	p.MOVK(X16, uint16((F_morestack_noctxt>>48)&0xffff), 48)
	p.BLR(X16)

	// Reload all register arguments from spill slots
	for i, v := range self.desc.Args {
		if v.InRegister {
			if v.IsFloat == floatKind64 {
				p.LDR(arm64.DRegister(v.Reg.(VRegister)), self.spillv(i))
			} else if v.IsFloat == floatKind32 {
				p.LDR(arm64.SRegister(v.Reg.(VRegister)), self.spillv(i))
			} else {
				p.LDR(v.Reg.(Register64), self.spillv(i))
			}
		}
	}

	// Jump back to the function entry
	p.B(entry)
}

func (self *Frame) GrowStackTextSize() uint32 {
	// TODO(ARM64): This should generate actual code and measure size like AMD64
	// For now, use estimation to avoid breaking existing functionality
	// Estimate: spills + MOVZ/MOVK sequence + BLR + loads + B
	// Each spill/load is 4 bytes, MOVZ+3*MOVK = 16 bytes, BLR = 4, B = 4
	spills := 0
	for _, v := range self.desc.Args {
		if v.InRegister {
			spills += 2 // one for spill, one for reload
		}
	}
	return uint32(spills*4 + 16 + 4 + 4)
}

func (self *Frame) emitPrologue(p *Program) {
	// ARM64 prologue:
	// SUB SP, SP, #frameSize
	// STP X29, X30, [SP, #offset]
	// ADD X29, SP, #offset

	frameSize := self.Size()

	// SUB SP, SP, #frameSize
	p.SUB(SP, SP, int(frameSize))

	// STP X29, X30, [SP, #offs]
	offs := int32(self.Offs())
	p.STP(X29, X30, Ptr(SP, offs))

	// ADD X29, SP, #offs (set up frame pointer)
	p.ADD(X29, SP, int(offs))
}

func (self *Frame) emitEpilogue(p *Program) {
	// ARM64 epilogue:
	// LDP X29, X30, [SP, #offset]
	// ADD SP, SP, #frameSize
	// RET

	// LDP X29, X30, [SP, #offs]
	offs := int32(self.Offs())
	p.LDP(X29, X30, Ptr(SP, offs))

	// ADD SP, SP, #frameSize
	frameSize := self.Size()
	p.ADD(SP, SP, int(frameSize))

	// RET
	p.RET()
}

// ReservedRegs returns the list of registers that need to be preserved
// For ARM64 Go ABI, we need to preserve X28 (current goroutine)
// When calling C code, we also need to preserve callee-saved registers (X19-X28, V8-V15)
func ReservedRegs(callc bool) []Register {
	if callc {
		// For C calls, we don't need to preserve additional registers beyond what Go ABI requires
		// as we're wrapping the call properly
		return nil
	}
	return []Register{
		X28, // current goroutine (g)
	}
}

func (self *Frame) emitReserveRegs(p *Program) {
	// Spill reserved registers to stack
	for i, r := range ReservedRegs(self.ccall) {
		switch r := r.(type) {
		case Register64:
			// STR Xreg, [SP, #offset]
			p.STR(r, self.resv(i))
		case VRegister:
			// STR Vreg, [SP, #offset] (would need D register for 64-bit FP)
			p.STR(arm64.DRegister(r), self.resv(i))
		default:
			panic(fmt.Sprintf("unsupported register type %T to reserve", r))
		}
	}
}

func (self *Frame) emitRestoreRegs(p *Program) {
	// Restore reserved registers from stack
	for i, r := range ReservedRegs(self.ccall) {
		switch r := r.(type) {
		case Register64:
			// LDR Xreg, [SP, #offset]
			p.LDR(r, self.resv(i))
		case VRegister:
			// LDR Vreg, [SP, #offset]
			p.LDR(arm64.DRegister(r), self.resv(i))
		default:
			panic(fmt.Sprintf("unsupported register type %T to restore", r))
		}
	}
}

func (self *Frame) emitStackCheck(p *Program, to *Label, maxStack uintptr) {
	// ARM64 stack check: compare SP - frameSize - maxStack with g.stackguard0
	// g is in X28, stackguard0 is at offset 16 in g struct
	// Use X9 as temporary register to calculate SP - frameSize - maxStack
	p.SUB(X9, SP, int(self.Size()+uint32(maxStack)))
	p.LDR(X10, Ptr(X28, 16)) // load g.stackguard0
	p.SUBS(XZR, X9, X10)     // compare calculated SP with stackguard0
	p.BLO(to)                // branch if calculated SP < stackguard0 (unsigned less than)
}

func (self *Frame) emitSpillPtrs(p *Program) {
	// Spill pointer argument registers to stack for GC
	for i, r := range self.desc.Args {
		if r.InRegister && r.IsPointer {
			// STR Xreg, [SP, #offset]
			p.STR(r.Reg.(Register64), self.argv(i))
		}
	}
}

func (self *Frame) emitClearPtrs(p *Program) {
	// Clear pointer argument registers on stack
	for i, r := range self.desc.Args {
		if r.InRegister && r.IsPointer {
			// STR XZR, [SP, #offset]
			p.STR(XZR, self.argv(i))
		}
	}
}

func (self *Frame) emitExchangeArgs(p *Program) {
	// For ARM64, Go and C use the same calling convention (AAPCS64)
	// So no register exchange is needed for arguments
	// Arguments are already in the correct registers (X0-X7, V0-V7)
}

func (self *Frame) emitExchangeRets(p *Program) {
	// For ARM64, return values are in X0, V0, etc., same for Go and C
	// No exchange needed, but we may need to store to stack if required
	if len(self.desc.Rets) > 0 && !self.desc.Rets[0].InRegister {
		if self.desc.Rets[0].IsFloat == floatKind64 {
			p.STR(arm64.DRegister(V0), self.retv(0))
		} else if self.desc.Rets[0].IsFloat == floatKind32 {
			p.STR(arm64.SRegister(V0), self.retv(0))
		} else {
			p.STR(X0, self.retv(0))
		}
	}
}

func (self *Frame) emitCallC(p *Program, addr uintptr) {
	// Load C function address into X16 and call it
	// Using MOVZ + MOVK sequence to load 64-bit address
	p.MOVZ(X16, uint16(addr&0xffff), 0)
	p.MOVK(X16, uint16((addr>>16)&0xffff), 16)
	p.MOVK(X16, uint16((addr>>32)&0xffff), 32)
	p.MOVK(X16, uint16((addr>>48)&0xffff), 48)
	// BLR X16 (branch with link to register)
	p.BLR(X16)
}

type floatKind uint8

const (
	notFloatKind floatKind = iota
	floatKind32
	floatKind64
)

type Parameter struct {
	InRegister bool
	IsPointer  bool
	IsFloat    floatKind
	Reg        Register
	Mem        uint32
	Type       reflect.Type
}

func mkIReg(vt reflect.Type, reg Register64) (p Parameter) {
	p.Reg = reg
	p.Type = vt
	p.InRegister = true
	p.IsPointer = isPointer(vt)
	return
}

func isFloat(vt reflect.Type) floatKind {
	switch vt.Kind() {
	case reflect.Float32:
		return floatKind32
	case reflect.Float64:
		return floatKind64
	default:
		return notFloatKind
	}
}

func mkVReg(vt reflect.Type, reg VRegister) (p Parameter) {
	p.Reg = reg
	p.Type = vt
	p.InRegister = true
	p.IsFloat = isFloat(vt)
	return
}

func mkStack(vt reflect.Type, mem uint32) (p Parameter) {
	p.Mem = mem
	p.Type = vt
	p.InRegister = false
	p.IsPointer = isPointer(vt)
	p.IsFloat = isFloat(vt)
	return
}

func (self Parameter) String() string {
	if self.InRegister {
		return fmt.Sprintf("[%s, Pointer(%v), Float(%v)]", self.Reg, self.IsPointer, self.IsFloat)
	} else {
		return fmt.Sprintf("[%d(SP), Pointer(%v), Float(%v)]", self.Mem, self.IsPointer, self.IsFloat)
	}
}

func CallC(addr uintptr, fr Frame, maxStack uintptr) []byte {
	// Generate ARM64 machine code that wraps a C function call
	p := DefaultArch.CreateProgram()

	stack := CreateLabel("_stack_grow")
	entry := CreateLabel("_entry")

	// Entry point
	p.Link(entry)
	fr.emitStackCheck(p, stack, maxStack)
	fr.emitPrologue(p)
	fr.emitReserveRegs(p)
	fr.emitSpillPtrs(p)
	fr.emitExchangeArgs(p)
	fr.emitCallC(p, addr)
	fr.emitExchangeRets(p)
	fr.emitRestoreRegs(p)
	fr.emitEpilogue(p)

	// Stack growth path
	p.Link(stack)
	fr.emitGrowStack(p, entry)

	return p.Assemble(0)
}

func (self *Frame) StackCheckTextSize() uint32 {
	p := DefaultArch.CreateProgram()
	p.SUB(X9, SP, int(self.Size()))
	p.LDR(X10, Ptr(X28, 16)) // load g.stackguard0
	p.SUBS(XZR, X9, X10)     // compare calculated SP with stackguard0
	to := CreateLabel("")
	p.Link(to)
	p.BLO(to) // branch if calculated SP < stackguard0 (unsigned less than)
	return uint32(len(p.Assemble(0)))
}
