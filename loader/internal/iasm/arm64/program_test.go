//
// Copyright 2024 CloudWeGo Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

package arm64

import (
	"encoding/hex"
	"testing"
)

func TestRegisterString(t *testing.T) {
	tests := []struct {
		reg  Register
		want string
	}{
		{X0, "x0"},
		{X15, "x15"},
		{X30, "x30"},
		{SP, "sp"},
		{XZR, "xzr"},
		{W0, "w0"},
		{W15, "w15"},
		{WZR, "wzr"},
		{V0, "v0"},
		{V31, "v31"},
		{D0, "d0"},
		{D31, "d31"},
		{S0, "s0"},
		{S31, "s31"},
	}

	for _, tt := range tests {
		if got := tt.reg.String(); got != tt.want {
			t.Errorf("%T.String() = %v, want %v", tt.reg, got, tt.want)
		}
	}
}

func TestMemoryOperand(t *testing.T) {
	tests := []struct {
		mem  *MemoryOperand
		want string
	}{
		{Ptr(SP, 0), "[sp]"},
		{Ptr(SP, 16), "[sp, #16]"},
		{Ptr(X0, -8), "[x0, #-8]"},
		{PtrIndex(SP, X1), "[sp, x1]"},
		{PtrIndexScale(X0, X1, 3), "[x0, x1, lsl #3]"},
	}

	for _, tt := range tests {
		if got := tt.mem.String(); got != tt.want {
			t.Errorf("MemoryOperand.String() = %v, want %v", got, tt.want)
		}
	}
}

func TestCondition(t *testing.T) {
	tests := []struct {
		cond Condition
		want string
	}{
		{CondEQ, "eq"},
		{CondNE, "ne"},
		{CondLT, "lt"},
		{CondGE, "ge"},
		{CondAL, "al"},
	}

	for _, tt := range tests {
		if got := tt.cond.String(); got != tt.want {
			t.Errorf("Condition.String() = %v, want %v", got, tt.want)
		}
	}
}

func TestSimpleInstructions(t *testing.T) {
	p := NewProgram()

	// Test NOP
	p.NOP()
	if len(p.insns) != 1 || p.insns[0].enc != 0xd503201f {
		t.Errorf("NOP encoding failed: got 0x%08x", p.insns[0].enc)
	}

	// Test RET
	p = NewProgram()
	p.RET()
	if len(p.insns) != 1 || p.insns[0].enc != 0xd65f03c0 {
		t.Errorf("RET encoding failed: got 0x%08x", p.insns[0].enc)
	}
}

func TestADD(t *testing.T) {
	p := NewProgram()
	p.ADD(X0, X1, 42)

	// ADD X0, X1, #42 should encode to: 0x91 0x0a 0x80 0x20
	// Format: sf=1, op=00, S=0, shift=00, imm12=0x02a (42), Rn=1, Rd=0
	expected := uint32(0x9100a820)
	if p.insns[0].enc != expected {
		t.Errorf("ADD encoding failed: got 0x%08x, want 0x%08x", p.insns[0].enc, expected)
	}
}

func TestSUB(t *testing.T) {
	p := NewProgram()
	p.SUB(X0, X1, 16)

	// SUB X0, X1, #16
	// Format: sf=1, op=10, S=0, shift=00, imm12=0x010 (16), Rn=1, Rd=0
	expected := uint32(0xd1004020)
	if p.insns[0].enc != expected {
		t.Errorf("SUB encoding failed: got 0x%08x, want 0x%08x", p.insns[0].enc, expected)
	}
}

func TestMOV(t *testing.T) {
	p := NewProgram()
	p.MOV(X0, X1)

	// MOV X0, X1 (encoded as ORR X0, XZR, X1)
	// Should have X1 in Rm field and XZR in Rn field
	if p.insns[0].enc&0x1f != 0 { // Check Rd is X0
		t.Errorf("MOV encoding failed: Rd field incorrect")
	}
}

func TestLDR_STR(t *testing.T) {
	p := NewProgram()

	// LDR X0, [SP, #16]
	p.LDR(X0, Ptr(SP, 16))

	// STR X1, [SP, #8]
	p.STR(X1, Ptr(SP, 8))

	if len(p.insns) != 2 {
		t.Errorf("Expected 2 instructions, got %d", len(p.insns))
	}
}

func TestSTP_LDP(t *testing.T) {
	p := NewProgram()

	// STP X29, X30, [SP, #-16]!
	mem := Ptr(SP, -16)
	mem.Mode = AddrModePreIndex
	p.STP(X29, X30, mem)

	// LDP X29, X30, [SP], #16
	mem2 := Ptr(SP, 16)
	mem2.Mode = AddrModePostIndex
	p.LDP(X29, X30, mem2)

	if len(p.insns) != 2 {
		t.Errorf("Expected 2 instructions, got %d", len(p.insns))
	}
}

func TestBranches(t *testing.T) {
	p := NewProgram()

	label := CreateLabel("test")

	// B label
	p.B(label)

	// BL label
	p.BL(label)

	// B.EQ label
	p.BEQ(label)

	// Link the label
	p.Link(label)
	p.NOP()

	if len(p.insns) != 4 {
		t.Errorf("Expected 4 instructions, got %d", len(p.insns))
	}
}

func TestBR_BLR(t *testing.T) {
	p := NewProgram()

	// BR X16
	p.BR(X16)
	expected := uint32(0xd61f0200) // BR X16
	if p.insns[0].enc != expected {
		t.Errorf("BR encoding failed: got 0x%08x, want 0x%08x", p.insns[0].enc, expected)
	}

	// BLR X16
	p.BLR(X16)
	expected = uint32(0xd63f0200) // BLR X16
	if p.insns[1].enc != expected {
		t.Errorf("BLR encoding failed: got 0x%08x, want 0x%08x", p.insns[1].enc, expected)
	}
}

func TestFloatingPoint(t *testing.T) {
	p := NewProgram()

	// FADD D0, D1, D2
	p.FADD(D0, D1, D2)

	// FSUB S0, S1, S2
	p.FSUB(S0, S1, S2)

	// FMUL D0, D1, D2
	p.FMUL(D0, D1, D2)

	// FDIV D0, D1, D2
	p.FDIV(D0, D1, D2)

	if len(p.insns) != 4 {
		t.Errorf("Expected 4 FP instructions, got %d", len(p.insns))
	}
}

func TestCMP(t *testing.T) {
	p := NewProgram()

	// CMP X0, #10
	p.CMP(X0, 10)

	// CMP is encoded as SUBS XZR, X0, #10
	// Check that Rd field is XZR (31)
	if p.insns[0].enc&0x1f != 31 {
		t.Errorf("CMP encoding failed: Rd should be XZR")
	}
}

func TestAssemble(t *testing.T) {
	p := NewProgram()

	// Simple program: add, sub, ret
	p.ADD(X0, X0, 1)
	p.SUB(X0, X0, 1)
	p.RET()

	// Assemble
	code := p.Assemble(0)

	// Should be 3 instructions × 4 bytes = 12 bytes
	if len(code) != 12 {
		t.Errorf("Expected 12 bytes, got %d", len(code))
	}

	// Print hex for debugging
	t.Logf("Assembled code: %s", hex.EncodeToString(code))
}

func TestProgramWithLabels(t *testing.T) {
	p := NewProgram()

	entry := CreateLabel("entry")
	loop := CreateLabel("loop")

	p.Link(entry)
	p.MOV(X0, XZR)

	p.Link(loop)
	p.ADD(X0, X0, 1)
	p.CMP(X0, 10)
	p.BNE(loop)
	p.RET()

	// Should have 5 instructions
	if len(p.insns) != 5 {
		t.Errorf("Expected 5 instructions, got %d", len(p.insns))
	}

	// Check labels are linked
	if entry.Pos != 0 {
		t.Errorf("Entry label pos = %d, want 0", entry.Pos)
	}
	if loop.Pos != 1 {
		t.Errorf("Loop label pos = %d, want 1", loop.Pos)
	}
}

func TestMUL(t *testing.T) {
	p := NewProgram()

	// MUL X0, X1, X2
	p.MUL(X0, X1, X2)

	// MUL is encoded as MADD with Ra=XZR
	if len(p.insns) != 1 {
		t.Errorf("Expected 1 instruction, got %d", len(p.insns))
	}
}

func TestShifts(t *testing.T) {
	p := NewProgram()

	// LSL X0, X1, #3
	p.LSL(X0, X1, 3)

	// LSR X0, X1, #3
	p.LSR(X0, X1, 3)

	// ASR X0, X1, #3
	p.ASR(X0, X1, 3)

	if len(p.insns) != 3 {
		t.Errorf("Expected 3 shift instructions, got %d", len(p.insns))
	}
}

func TestSUBS(t *testing.T) {
	p := NewProgram()

	// SUBS XZR, X16, X17
	// This is used for comparison in stack check
	p.SUBS(XZR, X16, X17)

	if len(p.insns) != 1 {
		t.Errorf("Expected 1 instruction, got %d", len(p.insns))
	}

	// SUBS (shifted register): sf 11 01011 shift(00) 0 Rm imm6(000000) Rn Rd
	// sf=1 (64-bit), opcode=0x6B, shift=0, Rm=X17(17), imm6=0, Rn=X16(16), Rd=XZR(31)
	// Expected: 1_11_01011_00_0_10001_000000_10000_11111
	//         = 1110_1011_0001_0001_0000_0010_0001_1111
	//         = 0xEB11_021F
	expected := uint32(0xEB11021F)
	if p.insns[0].enc != expected {
		t.Errorf("SUBS XZR, X16, X17 encoding failed: got 0x%08x, want 0x%08x", p.insns[0].enc, expected)
	}

	// Test 32-bit version: SUBS WZR, W0, W1
	p = NewProgram()
	p.SUBS(WZR, W0, W1)

	// sf=0, Rm=W1(1), Rn=W0(0), Rd=WZR(31)
	// Expected: 0_11_01011_00_0_00001_000000_00000_11111
	//         = 0110_1011_0000_0001_0000_0000_0001_1111
	//         = 0x6B01_001F
	expected = uint32(0x6B01001F)
	if p.insns[0].enc != expected {
		t.Errorf("SUBS WZR, W0, W1 encoding failed: got 0x%08x, want 0x%08x", p.insns[0].enc, expected)
	}
}
